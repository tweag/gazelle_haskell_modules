package gazelle_haskell_modules

import (
	"encoding/json"
	"fmt"
	"log"
	"path"
	"path/filepath"

    "github.com/bazelbuild/buildtools/build"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/rule"
	"github.com/bazelbuild/rules_go/go/tools/bazel"

	"os/exec"
	"strings"
)

// Extracts the source files from Haskell rules and creates
// haskell_module rules to build them.
//
// For existing haskell_module rules, it sets the indexing_mod_name
// private attribute as a side effect!
// indexing_mod_name is needed when indexing the rule.
func rulesToRuleInfos(pkgRoot string, rules []*rule.Rule, repo string, pkg string) []*RuleInfo {
	ruleInfoss0, reverseDeps := nonHaskellModuleRulesToRuleInfos(pkgRoot, rules, repo, pkg)
	ruleInfoss1 := haskellModuleRulesToRuleInfos(pkgRoot, rules, repo, pkg, reverseDeps)
	return concatRuleInfos(append(ruleInfoss0, ruleInfoss1...))
}

// Yields the rule infos and a map of dependency labels to the rule that
// has that dependency. If multiple rules have the same dependency only one
// of them ends up in the entry of the dependency.
//
func nonHaskellModuleRulesToRuleInfos(
	pkgRoot string,
	rules []*rule.Rule,
	repo string,
	pkg string,
) ([][]*RuleInfo, map[label.Label]*rule.Rule) {
	ruleInfoss := make([][]*RuleInfo, 0, 100)
	reverseDeps := make(map[label.Label]*rule.Rule, 100)
	// Analyze non-haskell_module rules
	for _, r := range rules {
		if !isNonHaskellModule(r.Kind()) {
			continue
		}
		srcs, err := srcsFromRuleExceptKeep(pkgRoot, r.Attr("srcs"))
		handleRuleError(err, r, "srcs")

		deps, err := depsFromRule(r.Attr("deps"), repo, pkg)
		handleRuleError(err, r, "deps")

		modules, err := depsFromRule(r.Attr("modules"), repo, pkg)
		handleRuleError(err, r, "modules")

		// Allow imports of modules on the same library, binary or test
		deps[label.New(repo, pkg, r.Name())] = true

		modDatas := haskellModulesToModuleData(srcs)
		ruleInfos := make([]*RuleInfo, len(modDatas))
		for i, modData := range modDatas {
			ruleInfos[i] = &RuleInfo {
				OriginatingRule: r,
				ModuleData: modData,
				Deps: deps,
				Modules: modules,
			}
		}
		ruleInfoss = append(ruleInfoss, ruleInfos)

		for dep, _ := range modules {
			reverseDeps[dep] = r
		}
		for dep, _ := range deps {
			reverseDeps[dep] = r
		}
	}
	return ruleInfoss, reverseDeps
}

// reverseDeps is used to determine which rule is originating a haskell_module
// rule, which is used in turn to determine from which rule the `haskell_module`
// rule should "inherit" attributes.
func haskellModuleRulesToRuleInfos(
	pkgRoot string,
	rules []*rule.Rule,
	repo string,
	pkg string,
    reverseDeps map[label.Label]*rule.Rule,
) [][]*RuleInfo {
	ruleInfoss := make([][]*RuleInfo, 0, 100)
	// Analyze haskell_module rules
	for _, r := range rules {
		if r.Kind() != "haskell_module" {
			continue
		}

		src := path.Join(pkgRoot, r.AttrString("src"))

		var originatingRule *rule.Rule
		rLabel := label.New(repo, pkg, r.Name())
		if v, ok := reverseDeps[rLabel]; ok {
			originatingRule = v
		} else {
			originatingRule = r
		}

		deps, err := depsFromRule(r.Attr("deps"), repo, pkg)
		handleRuleError(err, r, "deps")

		if originatingRule != r {
			// In contrast to non haskell_module rules, take into acount
			// the dependencies coming both from the originating and the
			// existing rule.
			depsOrigin, err := depsFromRule(originatingRule.Attr("deps"), repo, pkg)
			handleRuleError(err, originatingRule, "deps")

			// Allow imports of modules on the same library, binary or test
			deps[label.New(repo, pkg, originatingRule.Name())] = true

			for k, v := range depsOrigin {
				deps[k] = v
			}
		}

		modDatas := haskellModulesToModuleData([]string{src})
		ruleInfo := RuleInfo {
			OriginatingRule: originatingRule,
			ModuleData: modDatas[0],
			Deps: deps,
			Modules: map[label.Label]bool{},
		}

		ruleInfoss = append(ruleInfoss, []*RuleInfo{&ruleInfo})

		r.SetPrivateAttr("indexing_mod_name", ruleInfo.ModuleData.ModuleName)
	}
	return ruleInfoss
}

const HIMPORTSCAN_PATH = "himportscan/himportscan"

// Collects the imported modules from every sourcefile
//
// Module file paths must be absolute.
func haskellModulesToModuleData(moduleFiles []string) []*ModuleData {
	himportscan, err := bazel.Runfile(HIMPORTSCAN_PATH)
	if err != nil {
		log.Fatal(err)
	}
	cmd := exec.Command(himportscan)

	cmd.Stdin = strings.NewReader(strings.Join(moduleFiles, "\n"))
	out, err := cmd.CombinedOutput()
	if err != nil {
		log.Printf("%s", out)
		log.Fatal(err)
	}
	var modDatas []*ModuleData
	err = json.Unmarshal(out, &modDatas)
	if err != nil {
		log.Printf("Incorrect json: %s\n", out)
		log.Fatal(err)
	}
	return modDatas
}

func infoToRules(pkgRoot string, ruleInfos []*RuleInfo) language.GenerateResult {

	theRules := make([]*rule.Rule, len(ruleInfos))
	theImports := make([]interface{}, len(ruleInfos))
	for i, ruleInfo := range ruleInfos {
		ruleName := ruleInfo.OriginatingRule.Name()
		if ruleInfo.OriginatingRule.Kind() != "haskell_module" {
			ruleName = ruleInfo.OriginatingRule.Name() + "." + ruleInfo.ModuleData.ModuleName
		}
		r := rule.NewRule("haskell_module", ruleName)
		r.SetPrivateAttr("indexing_mod_name", ruleInfo.ModuleData.ModuleName)
		file, _ := filepath.Rel(pkgRoot, ruleInfo.ModuleData.FilePath)
		r.SetAttr("src", file)
		r.SetAttr("src_strip_prefix", srcStripPrefix(file, ruleInfo.ModuleData.ModuleName))
		if ruleInfo.OriginatingRule.Attr("ghcopts") != nil {
			SetArrayAttrExpr(r, "ghcopts", ruleInfo.OriginatingRule.Attr("ghcopts"))
		} else {
			SetArrayAttrExpr(r, "ghcopts", ruleInfo.OriginatingRule.Attr("compiler_flags"))
		}
		SetArrayAttrExpr(r, "tools", ruleInfo.OriginatingRule.Attr("tools"))
		SetArrayAttrExpr(r, "plugins", ruleInfo.OriginatingRule.Attr("plugins"))
		SetArrayAttrExpr(r, "extra_srcs", ruleInfo.OriginatingRule.Attr("extra_srcs"))
		r.AddComment("# rule generated by gazelle_haskell_modules")

		theRules[i] = r
		theImports[i] = &HModuleImportData {
			Deps: ruleInfo.Deps,
			ImportedModules: ruleInfo.ModuleData.ImportedModules,
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
		if isNonHaskellModule(r.Kind()) {
			newr := rule.NewRule(r.Kind(), r.Name())
			for _, k := range r.AttrKeys() {
				// Empty lists in attributes crash gazelle, so we remove them here.
				if k != "srcs" && k != "deps" && k != "modules" && !isEmptyListExpr(r.Attr(k)) {
					newr.SetAttr(k, r.Attr(k))
				}
			}

			srcs, err := srcsFromRuleExceptKeep(pkgRoot, r.Attr("srcs"))
			handleRuleError(err, r, "srcs")
			deps, err := depsFromRule(r.Attr("deps"), repo, pkg)
			handleRuleError(err, r, "deps")
			modules, err := depsFromRule(r.Attr("modules"), repo, pkg)
			handleRuleError(err, r, "modules")
			imports = append(imports, &HRuleImportData {
				Deps: deps,
				Modules: modules,
				Srcs: srcs,
			})
			haskellRules = append(haskellRules, newr)
		}
	}
	return language.GenerateResult{
		Gen:     append(gen.Gen, haskellRules...),
		Imports: append(gen.Imports, imports...),
	}
}

func handleRuleError(err error, r *rule.Rule, attr string) {
	if err != nil {
		fmt.Println("Error when analyzing target", r.Name())
		fmt.Println(attr, "=", build.FormatString(r.Attr(attr)))
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

func NotShouldKeep(expr build.Expr) bool {
	return !rule.ShouldKeep(expr)
}

func ConstTrue(expr build.Expr) bool {
	return true
}

// Collects the source files referenced in the given expression
func srcsFromRuleExceptKeep(pkgRoot string, expr build.Expr) ([]string, error) {
	srcs, err := getSources(expr, NotShouldKeep)
	if err != nil {
		return nil, err
	}

	xs := make([]string, len(srcs))
	i := 0
	for f, _ := range srcs {
		xs[i] = path.Join(pkgRoot, f)
		i++
	}

	return xs, nil
}

// Collects the dependencies referenced in the given expression
func depsFromRule(expr build.Expr, repo string, pkg string) (map[label.Label]bool, error) {
	deps, err := getLabelsFromListExpr(expr)
	if err != nil {
		return nil, err
	}

	xs := make(map[label.Label]bool, len(deps))
	for lbl, _ := range deps {
		xs[lbl.Abs(repo, pkg)] = true
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
	ModuleName string
	FilePath  string
	ImportedModules []string
}

type RuleInfo struct {
	OriginatingRule *rule.Rule
	Deps map[label.Label]bool // Absolute labels of the dependencies
	Modules map[label.Label]bool // Absolute labels of the modules in the library, empty if not a library
	ModuleData *ModuleData
}

type HModuleImportData struct {
	Deps map[label.Label]bool // Absolute labels of the dependencies
	ImportedModules []string
}

type HRuleImportData struct {
	Deps map[label.Label]bool // Absolute labels of the dependencies
	Modules map[label.Label]bool // Absolute labels of the modules in the library, empty if not a library
	Srcs []string
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
		v = fmt.Sprintf("%s//:%s",v, v[1:])
	}
	return label.Parse(v)
}

func getSources(expr build.Expr, p func(build.Expr) bool) (map[string]bool, error) {
	xs, err := getStringListP(expr, p)
	if err != nil {
		return nil, err
	}
	sourceMap := make(map[string]bool, len(xs))
	for _, x := range xs {
		sourceMap[x] = true
	}
	return sourceMap, nil
}

// Similar to (*Rule) AttrStrings(key string) []string
// but yields an empty list if the attribute isn't set, and
// gives an error if the attribute is set to something that
// isn't a list.
//
// Yields only the values satisfying the given predicate.
func getStringListP(expr build.Expr, p func(build.Expr) bool) ([]string, error) {
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
				if p(estr) {
					xs = append(xs, estr.Value)
				}
			default:
				return nil, fmt.Errorf("Unhandled expression type %T (expected a string)", e)
			}
		}
		return xs, nil
	default:
		return nil, fmt.Errorf("Unhandled expression type %T (expected a list)", expr)
	}
}

func getStringList(expr build.Expr) ([]string, error) {
	return getStringListP(expr, ConstTrue)
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

func isEmptyListExpr(expr build.Expr) bool {
	switch expr.(type) {
	case *build.ListExpr:
		return len(expr.(*build.ListExpr).List) == 0
	default:
		return false
	}
}

func SetArrayAttrExpr(r *rule.Rule, attrName string, expr build.Expr) {
    if expr != nil && !isEmptyListExpr(expr) {
        r.SetAttr(attrName, expr)
    }
}
