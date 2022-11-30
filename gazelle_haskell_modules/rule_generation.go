package gazelle_haskell_modules

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io/fs"
	"log"
	"path"
	"path/filepath"
	"regexp"

	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
	"github.com/bazelbuild/buildtools/build"
	"github.com/bazelbuild/rules_go/go/tools/bazel"

	"os/exec"
	"strings"
)

// Extracts the source files from Haskell rules and creates
// haskell_module rules to build them.
//
// For existing haskell_module rules, it also sets some private attributes as a side effect
// which are needed when indexing the rule.
func rulesToRuleInfos(pkgRoot string, rules []*rule.Rule, repo string, pkg string) []*RuleInfo {
	ruleInfoss0, originatingRules := nonHaskellModuleRulesToRuleInfos(pkgRoot, rules, repo, pkg)
	ruleInfoss1 := haskellModuleRulesToRuleInfos(pkgRoot, rules, repo, pkg, originatingRules)
	return concatRuleInfos(append(ruleInfoss0, ruleInfoss1...))
}

const PRIVATE_ATTR_MODULE_LABELS = "module_labels"
const PRIVATE_ATTR_DEP_LABELS = "dep_labels"
const PRIVATE_ATTR_MODULE_NAME = "module_name"
const PRIVATE_ATTR_ORIGINATING_RULE = "originating_rule"
const PRIVATE_FIND_MODULES_DIRECTIVE = "gazelle_haskell_modules:srcs:"
const PRIVATE_ATTR_IS_BOOT = "is_source_importation"
const BOOT_EXTENSION = ".hs-boot"

var PRIVATE_STRIP_FIND_MODULES_DIRECTIVE = regexp.MustCompile(fmt.Sprintf(`#\s*%s(.*)`, PRIVATE_FIND_MODULES_DIRECTIVE))

// Yields the rule infos and a map of module labels to the rules that
// enclose the modules.
func nonHaskellModuleRulesToRuleInfos(
	pkgRoot string,
	rules []*rule.Rule,
	repo string,
	pkg string,
) ([][]*RuleInfo, map[label.Label][]*rule.Rule) {
	ruleInfoss := make([][]*RuleInfo, 0, 100)
	originatingRules := make(map[label.Label][]*rule.Rule, 100)
	// Analyze non-haskell_module rules
	for _, r := range rules {
		if !isNonHaskellModule(r.Kind()) || !shouldKeep(r) {
			continue
		}
		srcs, err := getSrcs(pkgRoot, r)
		handleRuleError(err, r, "srcs")

		modules, err := depsFromRule(r.Attr("modules"), repo, pkg)
		handleRuleError(err, r, "modules")

		modDatas := haskellModulesToModuleData(srcs)
		ruleInfos := make([]*RuleInfo, len(modDatas))
		moduleLabels := make(map[label.Label]bool, len(modules)+len(srcs))
		for i, modData := range modDatas {
			ruleInfos[i] = &RuleInfo{
				OriginatingRules: []*rule.Rule{r},
				ModuleData:       modData,
			}
			modLabel := label.New(repo, pkg, ruleNameFromRuleInfo(ruleInfos[i]))
			addOriginatingRule(originatingRules, &modLabel, r)
			moduleLabels[modLabel] = true
		}
		ruleInfoss = append(ruleInfoss, ruleInfos)

		for mod, _ := range modules {
			addOriginatingRule(originatingRules, &mod, r)
			moduleLabels[mod] = true
		}

		r.SetPrivateAttr(PRIVATE_ATTR_MODULE_LABELS, moduleLabels)
	}
	return ruleInfoss, originatingRules
}

// Adds a rule to a map at the given label.
func addOriginatingRule(originatingRules map[label.Label][]*rule.Rule, mod *label.Label, r *rule.Rule) {
	oRules := originatingRules[*mod]
	if oRules == nil {
		originatingRules[*mod] = []*rule.Rule{r}
	} else {
		originatingRules[*mod] = append(oRules, r)
	}
}

// originatingRules is used to determine which rule is originating a haskell_module
// rule, which is used in turn to determine which modules from the same originating
// rule are meant in imports.
func haskellModuleRulesToRuleInfos(
	pkgRoot string,
	rules []*rule.Rule,
	repo string,
	pkg string,
	originatingRules map[label.Label][]*rule.Rule,
) [][]*RuleInfo {
	ruleInfoss := make([][]*RuleInfo, 0, 100)
	// Analyze haskell_module rules
	for _, r := range rules {
		if r.Kind() != "haskell_module" {
			continue
		}

		src := path.Join(pkgRoot, r.AttrString("src"))

		rLabel := label.New(repo, pkg, r.Name())
		oRules, ok := originatingRules[rLabel]
		if !ok {
			continue
		}

		modDatas := haskellModulesToModuleData([]string{src})
		if len(modDatas) > 0 {
			ruleInfo := RuleInfo{
				OriginatingRules: oRules,
				ModuleData:       modDatas[0],
			}

			ruleInfoss = append(ruleInfoss, []*RuleInfo{&ruleInfo})

			r.SetPrivateAttr(PRIVATE_ATTR_IS_BOOT, ruleInfo.ModuleData.IsBoot)
			r.SetPrivateAttr(PRIVATE_ATTR_MODULE_NAME, ruleInfo.ModuleData.ModuleName)
			r.SetPrivateAttr(PRIVATE_ATTR_ORIGINATING_RULE, ruleInfo.OriginatingRules)
		} else {
			log.Printf("warning: found no module source for haskell_module rule %s: %q\n", r.Name(), src)
		}
	}
	return ruleInfoss
}

const HIMPORTSCAN_PATH = "himportscan/himportscan"

// Collects the imported modules from every sourcefile
//
// Module file paths must be absolute.
//
// If a source file is missing we don't return ModuleData for it.
func haskellModulesToModuleData(moduleFiles []string) []*ModuleData {
	himportscan, err := bazel.Runfile(HIMPORTSCAN_PATH)
	if err != nil {
		log.Fatal(err)
	}
	cmd := exec.Command(himportscan)

	cmd.Stdin = strings.NewReader(strings.Join(moduleFiles, "\n"))

	var stdout bytes.Buffer
	cmd.Stdout = &stdout

	var stderr strings.Builder
	cmd.Stderr = &stderr

	err = cmd.Start()
	if err != nil {
		log.Fatalf("himportscan failed to start %v", err)
	}

	err = cmd.Wait()
	stdoutBytes := stdout.Bytes()
	stderrString := stderr.String()

	if err != nil {
		fmt.Printf("%s\n", stdout.Bytes())
		fmt.Printf("%s\n", stderrString)
		log.Fatalf("himportscan exited unsuccessfully %v\n", err)
	}

	// print stderr so if himportscan generates some stderr but doesn't fail the user might see useful output
	if stderrString != "" {
		log.Printf("%s\n", stderrString)
	}

	var modDatas []*ModuleData
	err = json.Unmarshal(stdoutBytes, &modDatas)
	if err != nil {
		log.Printf("Incorrect json: %s\n", stdoutBytes)
		log.Fatal(err)
	}
	return modDatas
}

func infoToRules(pkgRoot string, ruleInfos []*RuleInfo) language.GenerateResult {

	theRules := make([]*rule.Rule, len(ruleInfos))
	theImports := make([]interface{}, len(ruleInfos))
	for i, ruleInfo := range ruleInfos {
		ruleName := ruleNameFromRuleInfo(ruleInfo)
		r := rule.NewRule("haskell_module", ruleName)
		r.SetPrivateAttr(PRIVATE_ATTR_IS_BOOT, ruleInfo.ModuleData.IsBoot)
		r.SetPrivateAttr(PRIVATE_ATTR_MODULE_NAME, ruleInfo.ModuleData.ModuleName)
		r.SetPrivateAttr(PRIVATE_ATTR_ORIGINATING_RULE, ruleInfo.OriginatingRules)
		file, _ := filepath.Rel(pkgRoot, ruleInfo.ModuleData.FilePath)
		r.SetAttr("src", file)
		stripPrefix := srcStripPrefix(file, ruleInfo.ModuleData.ModuleName)
		r.SetAttr("src_strip_prefix", stripPrefix)
		idealModFile := fmt.Sprintf("%s/%s%s", stripPrefix, strings.Replace(ruleInfo.ModuleData.ModuleName, ".", "/", -1), filepath.Ext(file))
		if file != idealModFile {
			r.SetAttr("module_name", ruleInfo.ModuleData.ModuleName)
		}
		r.AddComment("# rule generated by gazelle_haskell_modules")

		theRules[i] = r
		theImports[i] = &HModuleImportData{
			ImportedModules: ruleInfo.ModuleData.ImportedModules,
			UsesTH:          ruleInfo.ModuleData.UsesTH,
		}
	}

	return language.GenerateResult{
		Gen:     theRules,
		Imports: theImports,
	}
}

func addNonHaskellModuleRules(
	pkgRoot string,
	repo string,
	pkg string,
	gen language.GenerateResult,
	rules []*rule.Rule,
) language.GenerateResult {
	haskellRules := make([]*rule.Rule, 0, len(rules))
	imports := make([]interface{}, 0, len(rules))
	for _, r := range rules {
		if !shouldKeep(r) {
			continue
		}
		if isNonHaskellModule(r.Kind()) {
			newr := rule.NewRule(r.Kind(), r.Name())
			for _, k := range r.AttrKeys() {
				if k != "srcs" && k != "modules" && k != "deps" && k != "narrowed_deps" && k != "main_file" {
					newr.SetAttr(k, r.Attr(k))
				}
			}

			srcs, err := getSrcs(pkgRoot, r)
			handleRuleError(err, r, "srcs")
			modules, err := depsFromRule(r.Attr("modules"), repo, pkg)
			handleRuleError(err, r, "modules")
			deps, err := depsFromRule(r.Attr("deps"), repo, pkg)
			handleRuleError(err, r, "deps")
			narrowedDeps, err := depsFromRule(r.Attr("narrowed_deps"), repo, pkg)
			handleRuleError(err, r, "narrowed_deps")
			appendLabelMaps(deps, narrowedDeps)
			imports = append(imports, &HRuleImportData{
				Deps:    deps,
				Modules: modules,
				Srcs:    srcs,
			})
			haskellRules = append(haskellRules, newr)

			r.SetPrivateAttr(PRIVATE_ATTR_DEP_LABELS, deps)
			newr.SetPrivateAttr(PRIVATE_ATTR_DEP_LABELS, deps)
			newr.SetPrivateAttr(PRIVATE_ATTR_MODULE_LABELS, r.PrivateAttr(PRIVATE_ATTR_MODULE_LABELS))
		}
	}
	return language.GenerateResult{
		Gen:     append(gen.Gen, haskellRules...),
		Imports: append(gen.Imports, imports...),
	}
}

func appendLabelMaps(a map[label.Label]bool, b map[label.Label]bool) {
	for k, v := range b {
		a[k] = v
	}
}

func handleRuleError(err error, r *rule.Rule, attr string) {
	if err != nil {
		fmt.Println("Error when analyzing target", r.Name())
		if expr := r.Attr(attr); expr != nil {
			fmt.Println(attr, "=", build.FormatString(expr))
		} else {
			fmt.Printf("%s doesn't exist in rule %s\n", attr, r.Name())
		}
		log.Fatal(err)
	}
}

func concatRuleInfos(xs [][]*RuleInfo) []*RuleInfo {
	s := 0
	for _, x := range xs {
		s += len(x)
	}
	ys := make([]*RuleInfo, s)
	i := 0
	for _, x := range xs {
		for _, y := range x {
			ys[i] = y
			i++
		}
	}
	return ys
}

// Find and append together
// 0. The files listed in the srcs attribute of a rule
// 1. All the files (recursively) in directories specified in a "gazelle_haskell_modules:srcs:"
func getSrcs(pkgRoot string, r *rule.Rule) ([]string, error) {
	sourcesFromRule, err := srcsFromRule(pkgRoot, r)
	if err != nil {
		return nil, fmt.Errorf("getSrcs: %w", err)
	}

	autodiscoveredSrcs, err := autodiscoverSrcs(pkgRoot, r)
	if err != nil {
		return nil, fmt.Errorf("getSrcs: %w", err)
	}

	// reuse the set we got form srcsFromRule to deduplicate autodiscovered and explicit files
	for _, v := range autodiscoveredSrcs {
		sourcesFromRule[v] = true
	}

	srcs := make([]string, len(sourcesFromRule))
	i := 0
	for file := range sourcesFromRule {
		srcs[i] = file
		i++
	}
	return srcs, nil
}

func srcsFromRule(pkgRoot string, r *rule.Rule) (map[string]bool, error) {
	if expr := r.Attr("srcs"); expr != nil {
		files, err := getStringList(expr)
		if err != nil {
			return nil, fmt.Errorf("srcsFromRule: %w", err)
		}

		sourceMap := make(map[string]bool, len(files))
		for _, file := range files {
			sourceMap[path.Join(pkgRoot, file)] = true
		}

		return sourceMap, nil
	}
	return map[string]bool{}, nil
}

func autodiscoverSrcs(pkgRoot string, r *rule.Rule) ([]string, error) {

	srcDirs, err := getSrcDirsFromComments(r.Comments())
	if err != nil {
		return nil, fmt.Errorf("autodiscoverSrcs: %w", err)
	}

	sourcesFromDirective, err := getSourcesRecursivelyFromDirs(pkgRoot, srcDirs)
	if err != nil {
		return nil, fmt.Errorf("autodiscoverSrcs:: %w", err)
	}

	return sourcesFromDirective, err
}

// Collects the dependencies referenced in the given expression
func depsFromRule(expr build.Expr, repo string, pkg string) (map[label.Label]bool, error) {
	deps, err := getLabelsFromListExpr(expr)
	if err != nil {
		return nil, err
	}

	xs := make(map[label.Label]bool, len(deps))
	for lbl, _ := range deps {
		xs[abs(lbl, repo, pkg)] = true
	}

	return xs, nil
}

func setVisibilities(f *rule.File, rules []*rule.Rule) {
	if f == nil || !f.HasDefaultVisibility() {
		for _, r := range rules {
			r.SetAttr("visibility", []string{"//visibility:public"})
		}
	}
}

type ModuleData struct {
	ModuleName      string
	FilePath        string
	ImportedModules []ModuleImport
	UsesTH          bool
	IsBoot          bool
}

type ModuleImport struct {
	IsSourceImported bool
	PackageName      string
	ModuleName       string
}

type RuleInfo struct {
	OriginatingRules []*rule.Rule
	ModuleData       *ModuleData
}

type HModuleImportData struct {
	ImportedModules []ModuleImport
	UsesTH          bool
}

type HRuleImportData struct {
	Deps    map[label.Label]bool // Absolute labels of deps of the library/binary/test
	Modules map[label.Label]bool // Absolute labels of the modules in the library, empty if not a library
	Srcs    []string
}

func getLabelsFromListExpr(expr build.Expr) (map[label.Label]bool, error) {
	switch expr.(type) {
	case nil:
		return map[label.Label]bool{}, nil
	case *build.ListExpr:
		exprList := expr.(*build.ListExpr).List
		xs := make(map[label.Label]bool, len(exprList))
		for _, e := range exprList {
			switch e.(type) {
			case *build.StringExpr:
				lbl, err := ParseLabel(e.(*build.StringExpr).Value)
				if err != nil {
					return nil, err
				}
				xs[lbl] = true
			default:
				return nil, fmt.Errorf("Unhandled expression type %T (expected a string)", e)
			}
		}
		return xs, nil
	default:
		return nil, fmt.Errorf("Unhandled expression type %T (expected a list)", expr)
	}
}

// We use a patched parsing for labels, as the parser from gazelle can't understand
// labels of the form "@repo", supposed to mean "@repo//:repo".
func ParseLabel(v string) (label.Label, error) {
	if strings.HasPrefix(v, "@") && !strings.Contains(v, "//") && !strings.Contains(v, ":") {
		v = fmt.Sprintf("%s//:%s", v, v[1:])
	}
	return label.Parse(v)
}

func getSourcesRecursivelyFromDirs(pkgRoot string, dirs []string) ([]string, error) {
	var srcs []string
	for _, dir := range dirs {
		err := filepath.WalkDir(path.Join(pkgRoot, dir), func(path string, d fs.DirEntry, err error) error {
			if err != nil {
				return err
			}
			// TODO what about hsc and -boot files?
			if strings.HasSuffix(path, ".hs") || strings.HasSuffix(path, ".lhs") {
				srcs = append(srcs, path)
			}
			return nil
		})
		if err != nil {
			return nil, fmt.Errorf("getSourcesRecursivelyFromDirs: %w", err)
		}
	}
	return srcs, nil
}

// Similar to (*Rule) AttrStrings(key string) []string
// but yields an empty list if the attribute isn't set, and
// gives an error if the attribute is set to something that
// isn't a list.
func getStringList(expr build.Expr) ([]string, error) {
	switch expr.(type) {
	case nil:
		return []string{}, nil
	case *build.ListExpr:
		exprList := expr.(*build.ListExpr).List
		xs := make([]string, 0, len(exprList))
		for _, e := range exprList {
			switch e.(type) {
			case *build.StringExpr:
				estr := e.(*build.StringExpr)
				xs = append(xs, estr.Value)
			default:
				return nil, fmt.Errorf("Unhandled expression type %T (expected a string)", e)
			}
		}
		return xs, nil
	default:
		return nil, fmt.Errorf("Unhandled expression type %T (expected a list)", expr)
	}
}

func isNonHaskellModule(kind string) bool {
	return kind == "haskell_library" ||
		kind == "haskell_binary" ||
		kind == "haskell_test"
}

// Computes the prefix of file that doesn't correspond with
// the module hierarchy.
//
// srcStripPrefix("/a/B/C", "B.C") == "/a"
//
// Actually, it doesn't check that the components of
// the module name match the path, so
//
// srcStripPrefix("/a/B/C", "D.E") == "/a"
//
func srcStripPrefix(file, modName string) string {
	numComponents := strings.Count(modName, ".") + 1
	dir := file
	for i := 0; i < numComponents; i++ {
		dir = filepath.Dir(dir)
	}
	return dir
}

func ruleNameFromRuleInfo(ruleInfo *RuleInfo) string {
	suffix := bootSuffixIf(ruleInfo.ModuleData.IsBoot)
	return ruleInfo.OriginatingRules[0].Name() + "." + ruleInfo.ModuleData.ModuleName + suffix
}

// Check for the "usual" keep directive, along with our own custom "# gazelle_haskell_modules:keep".
func shouldKeep(r *rule.Rule) bool {
	if r.ShouldKeep() {
		return false
	}
	for _, c := range r.Comments() {
		if strings.Trim(c, "# ") == "gazelle_haskell_modules:keep" {
			return false
		}
	}
	return true
}

// Detect srcs specifications from comments attached to a rule.
//
// # gazelle_haskell_modules:srcs: src/ pirin/
// ->
// [src/ pirin/]
func getSrcDirsFromComments(cs []string) ([]string, error) {

	srcsComment := ""
	for _, comment := range cs {
		if strings.Contains(comment, PRIVATE_FIND_MODULES_DIRECTIVE) {
			if srcsComment != "" {
				return nil, fmt.Errorf("found more than one srcs directive")
			}
			srcsComment = comment
		}
	}
	if srcsComment == "" {
		return []string{}, nil
	}
	stripped, err := stripDirective(srcsComment)

	if err != nil {
		return nil, err
	}

	return strings.Fields(stripped), nil
}

func stripDirective(c string) (string, error) {
	matches := PRIVATE_STRIP_FIND_MODULES_DIRECTIVE.FindStringSubmatch(c)
	if len(matches) != 2 {
		return "", nil
	}
	return matches[1], nil
}
