// An extension for gazelle to generate haskell_module rules from haskell rules
package gazelle_haskell_modules

import (
	"flag"
	"fmt"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/repo"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"

	gazelle_cabal "github.com/tweag/gazelle_cabal/gazelle_cabal"

	"log"
	"path"
	"path/filepath"
)

////////////////////////////////////////////////////
// gazelle callbacks
////////////////////////////////////////////////////

const gazelleHaskellModulesName = "gazelle_haskell_modules"

type gazelleHaskellModulesLang struct{}

func NewLanguage() language.Language {
	return &gazelleHaskellModulesLang{}
}

func (*gazelleHaskellModulesLang) Name() string { return gazelleHaskellModulesName }

func (*gazelleHaskellModulesLang) RegisterFlags(fs *flag.FlagSet, cmd string, c *config.Config) {}

func (*gazelleHaskellModulesLang) CheckFlags(fs *flag.FlagSet, c *config.Config) error { return nil }

func (*gazelleHaskellModulesLang) KnownDirectives() []string {
	return []string{}
}

type Config struct {
}

func (*gazelleHaskellModulesLang) Configure(c *config.Config, rel string, f *rule.File) {
	if f == nil {
		return
	}

	m, ok := c.Exts[gazelleHaskellModulesName]
	var extraConfig Config
	if ok {
		extraConfig = m.(Config)
	} else {
		extraConfig = Config{}
	}

	for _, directive := range f.Directives {
		switch directive.Key {
		}
	}
	c.Exts[gazelleHaskellModulesName] = extraConfig
}

var haskellAttrInfo = rule.KindInfo{
	MatchAttrs:    []string{},
	NonEmptyAttrs: map[string]bool{},
	ResolveAttrs: map[string]bool{
		"modules":       true,
		"deps":          true,
		"narrowed_deps": true,
		"srcs":          true,
	},
}

var haskellModuleAttrInfo = rule.KindInfo{
	MatchAttrs:    []string{},
	NonEmptyAttrs: map[string]bool{},
	ResolveAttrs: map[string]bool{
		"deps":               true,
		"module_name":        true,
		"cross_library_deps": true,
		"enable_th":          true,
	},
}

var kinds = map[string]rule.KindInfo{
	"haskell_library": haskellAttrInfo,
	"haskell_binary":  haskellAttrInfo,
	"haskell_test":    haskellAttrInfo,
	"haskell_module":  haskellModuleAttrInfo,
}

func (*gazelleHaskellModulesLang) Kinds() map[string]rule.KindInfo {
	return kinds
}

func (*gazelleHaskellModulesLang) Loads() []rule.LoadInfo {
	return []rule.LoadInfo{
		{
			Name:    "@rules_haskell//haskell/experimental:defs.bzl",
			Symbols: []string{"haskell_module"},
		},
	}
}

func (*gazelleHaskellModulesLang) Imports(c *config.Config, r *rule.Rule, f *rule.File) []resolve.ImportSpec {
	if r.Kind() == "haskell_module" {
		originatingRules := getOriginatingRules(r)
		moduleSpecs := make([]resolve.ImportSpec, len(originatingRules), 2*len(originatingRules)+1)
		for i, originatingRule := range originatingRules {
			moduleSpecs[i] = moduleByFilepathSpec(f.Pkg, originatingRule.Name(), getSrcFromRule(c.RepoRoot, f.Path, r))
		}
		for _, originatingRule := range originatingRules {
			if originatingRule.Kind() == "haskell_library" {
				moduleSpecs = append(
					moduleSpecs,
					moduleByModuleImportSpec(&ModuleImport{getIsBootFromRule(r), getPackageNameFromRule(originatingRule), getModuleNameFromRule(r)}),
				)
			}
		}
		if len(originatingRules) > 0 {
			moduleSpecs = append(
				moduleSpecs,
				moduleByModuleImportSpec(&ModuleImport{getIsBootFromRule(r), "", getModuleNameFromRule(r)}))
		}
		return moduleSpecs
	} else if isNonHaskellModule(r.Kind()) {
		modules := r.PrivateAttr(PRIVATE_ATTR_MODULE_LABELS)
		moduleLabels := map[label.Label]bool{}
		if modules != nil {
			moduleLabels = modules.(map[label.Label]bool)
		}
		libraryDeps := r.PrivateAttr(PRIVATE_ATTR_DEP_LABELS)
		libraryDepLabels := map[label.Label]bool{}
		if libraryDeps != nil {
			libraryDepLabels = libraryDeps.(map[label.Label]bool)
		}

		var usesModules int
		if shouldKeep(r) {
			usesModules = 1
		} else {
			usesModules = 0
		}
		moduleSpecs := make([]resolve.ImportSpec, len(moduleLabels)+len(libraryDepLabels)+usesModules)
		i := 0
		for moduleLabel := range moduleLabels {
			moduleSpecs[i] = libraryOfModuleSpec(moduleLabel)
			i++
		}
		i = 0
		for libLabel := range libraryDepLabels {
			moduleSpecs[len(moduleLabels)+i] = isDepOfLibrarySpec(libLabel, f.Pkg, r.Name())
			i++
		}
		if usesModules > 0 {
			moduleSpecs[len(moduleSpecs)-1] = libraryUsesModulesSpec(label.New(c.RepoName, f.Pkg, r.Name()))
		}
		return append(moduleSpecs, gazelle_cabal.RunImports(r, gazelleHaskellModulesName)...)
	} else {
		return []resolve.ImportSpec{}
	}
}

func (*gazelleHaskellModulesLang) Embeds(r *rule.Rule, from label.Label) []label.Label { return nil }

// This enricher tries to catch information that were not added to the ruleIndex,
// because they require to have access to the 'imports' field which is only accessible in the 'Resolve' phase.
var ixEnricher = make(map[label.Label][]label.Label)

// We store the private attributes of each rule,
// in order to access this information when treating future rules.
// This information is useful, but cannot be accessed during the 'Import' phase,
// since it requires to access the imports associated to a rule and that is only possible during the 'Resolve' phase.
// TODO: This works, but relies strongly on the fact that the rules generated by gazelle_cabal (if any) are resolved before the ones generated by gazelle_haskell_modules.
func enrichIx(r *rule.Rule, from label.Label) {
	var l []label.Label
	deps := r.PrivateAttr(PRIVATE_ATTR_DEP_LABELS).(map[label.Label]bool)
	if deps != nil {
		for x := range deps {
			l = append(l, x)
		}
		ixEnricher[label.New(from.Repo, from.Pkg, r.Name())] = l
	}
}

func (*gazelleHaskellModulesLang) Resolve(c *config.Config, ix *resolve.RuleIndex, rc *repo.RemoteCache, r *rule.Rule, imports interface{}, from label.Label) {
	hmc := c.Exts[gazelleHaskellModulesName].(Config)
	switch expr := imports.(type) {
	// This happens only when one run gazelle_cabal and gazelle_haskell_modules in one pass as the the gazelle_binary.
	case gazelle_cabal.ImportData:
		// We fully generate the rule that gazelle_cabal would have produced.
		gazelle_cabal.RunResolve(c, ix, r, expr, from, gazelleHaskellModulesName)
		// We then create an HRuleImportData from the generated rule.
		pkgRoot := path.Join(c.RepoRoot, from.Pkg)
		newr, newImports := addOneNonHaskellModuleRule(pkgRoot, from.Repo, from.Pkg, r)
		// The new version of the rule is used to clean the attributes of r.
		for _, a := range r.AttrKeys() {
			if newr.Attr(a) == nil {
				r.DelAttr(a)
			}
		}
		enrichIx(r, from)
		// After this cleaning, the new deps are set.
		setNonHaskellModuleDeps(&hmc, c.RepoRoot, ix, r, newImports, from)
	default:
		if isNonHaskellModule(r.Kind()) {
			setNonHaskellModuleDeps(&hmc, c.RepoRoot, ix, r, imports.(*HRuleImportData), from)
		} else {
			setHaskellModuleDeps(ix, r, imports.(*HModuleImportData), from)
		}
	}
}

func (*gazelleHaskellModulesLang) GenerateRules(args language.GenerateArgs) language.GenerateResult {
	// If there is no BUILD.bazel file and no rules generated for a soon to be BUILD.bazel file,
	// then there are no reasons to go further.
	if args.File == nil && args.OtherGen == nil {
		return language.GenerateResult{
			Gen:     []*rule.Rule{},
			Imports: []interface{}{},
		}
	}

	var fileRules []*rule.Rule
	if args.File != nil {
		// If there were no BUILD.bazel file, then there are no rules pre-existing this invocation of gazelle.
		fileRules = args.File.Rules
	}
	// The list of rules should contain both the rules already present in the build file (fileRules)
	// and the one that previous extensions of gazelle generated in this run (args.OtherGen...).
	// This second argument contains the rule generated by gazelle_cabal when both extensions are ran in the same gazelle_binary.
	rulesList := append(fileRules, args.OtherGen...)
	ruleInfos := rulesToRuleInfos(args.Dir, rulesList, args.Config.RepoName, args.Rel)
	generateResult := infoToRules(args.Dir, ruleInfos)

	setVisibilities(args.File, generateResult.Gen)

	finalGenRes := addNonHaskellModuleRules(args.Dir, args.Config.RepoName, args.Rel, generateResult, fileRules)

	return finalGenRes
}

func (*gazelleHaskellModulesLang) Fix(c *config.Config, f *rule.File) {
	if !c.ShouldFix || f == nil {
		return
	}

	ruleInfos := rulesToRuleInfos(path.Dir(f.Path), f.Rules, c.RepoName, f.Pkg)

	ruleNameSet := make(map[string]bool, len(ruleInfos))
	for _, info := range ruleInfos {
		rName := ruleNameFromRuleInfo(info)
		ruleNameSet[rName] = true
	}

	deleted := make(map[string]bool)

	for _, r := range f.Rules {
		if !r.ShouldKeep() && r.Kind() == "haskell_module" {
			if _, ok := ruleNameSet[r.Name()]; !ok {
				r.Delete()
				deleted[r.Name()] = true
			}
		}
	}

	for _, r := range f.Rules {
		if shouldKeep(r) && isNonHaskellModule(r.Kind()) {
			cleanupHiddenModulesList(r, deleted)
			cleanupModulesList(r, deleted)
		}
	}

	f.Sync()
}

func cleanupModulesList(r *rule.Rule, deleted map[string]bool) {
	// TODO: use labels instead of manually stripping away the ':' ?
	shouldKeepModule := func(module string) bool {
		if len(module) > 0 && module[0] == byte(':') {
			return !deleted[module[1:]]
		} else {
			return true
		}
	}
	cleanupModulesLists(r, "modules", shouldKeepModule)
}

// Removes from the hidden_modules attribute the modules in the map deleted
// which are also mentioned in the modules attribute.
//
// This function depends on haskell_module rule names following the
// convention <libname>.<modulename>, where <modulename> is the name
// of the module that is/was defined by the rule.
func cleanupHiddenModulesList(r *rule.Rule, deleted map[string]bool) {
	ruleName := r.Name()
	modules := r.AttrStrings("modules")
	if modules != nil {
		modulesSet := make(map[string]bool, len(modules))
		for _, module := range modules {
			modulesSet[module] = true
		}

		// TODO: use something better?
		shouldKeepModule := func(module string) bool {
			moduleRuleName := fmt.Sprintf("%s.%s", ruleName, module)
			return !modulesSet[":"+moduleRuleName] || !deleted[moduleRuleName]
		}
		cleanupModulesLists(r, "hidden_modules", shouldKeepModule)
	}
}

// Leaves only those modules from r that satisfy the given predicate shouldKeepModule.
//
// modulesFieldName is expected to be "modules" or "hidden_modules".
//
// The field is removed if all the modules/labels are dropped from the list.
func cleanupModulesLists(r *rule.Rule, modulesFieldName string, shouldKeepModule func(string) bool) {
	modules := r.AttrStrings(modulesFieldName)
	if modules != nil {
		nonDeletedModules := make([]string, 0, len(modules))
		for _, module := range modules {
			if shouldKeepModule(module) {
				nonDeletedModules = append(nonDeletedModules, module)
			}
		}
		if len(nonDeletedModules) > 0 {
			r.SetAttr(modulesFieldName, nonDeletedModules)
		} else {
			r.DelAttr(modulesFieldName)
		}
	}
}

////////////////////////////////
// Indexing
////////////////////////////////

func getIsBootFromRule(r *rule.Rule) bool {
	if r.PrivateAttr(PRIVATE_ATTR_IS_BOOT) == nil {
		log.Fatal("Error reading boot status of " + r.Name())
	}
	return r.PrivateAttr(PRIVATE_ATTR_IS_BOOT).(bool)
}

func getModuleNameFromRule(r *rule.Rule) string {
	if r.PrivateAttr(PRIVATE_ATTR_MODULE_NAME) == nil {
		log.Fatal("Error reading module name of " + r.Name())
	}
	return r.PrivateAttr(PRIVATE_ATTR_MODULE_NAME).(string)
}

func getPackageNameFromRule(r *rule.Rule) string {
	pkgName := r.AttrString("package_name")
	if pkgName != "" {
		return pkgName
	} else {
		return r.Name()
	}
}

func getSrcFromRule(repoRoot string, buildFilePath string, r *rule.Rule) string {
	if "" == r.AttrString("src") {
		log.Fatal("Couldn't read src from rule: " + r.Name())
	}
	src, err := filepath.Rel(repoRoot, path.Join(path.Dir(buildFilePath), r.AttrString("src")))
	if err != nil {
		log.Fatal("Reading src of "+r.Name(), err)
	}
	return src
}

func getOriginatingRules(r *rule.Rule) []*rule.Rule {
	v := r.PrivateAttr(PRIVATE_ATTR_ORIGINATING_RULE)
	if v != nil {
		return v.([]*rule.Rule)
	}
	return nil
}
