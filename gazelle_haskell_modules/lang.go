// An extension for gazelle to generate haskell_module rules from haskell rules
package gazelle_haskell_modules

import (
	"flag"
	"fmt"
	"log"
	"path"
	"path/filepath"

	"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/language"
	"github.com/bazelbuild/bazel-gazelle/repo"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"
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

type Config struct{}

func (*gazelleHaskellModulesLang) Configure(c *config.Config, rel string, f *rule.File) {}

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
			Name:    "@rules_haskell//haskell:defs.bzl",
			Symbols: []string{"haskell_binary", "haskell_library", "haskell_test"},
		},
		{
			Name:    "@rules_haskell//haskell/experimental:defs.bzl",
			Symbols: []string{"haskell_module"},
		},
	}
}

func (*gazelleHaskellModulesLang) Imports(c *config.Config, r *rule.Rule, f *rule.File) []resolve.ImportSpec {
	if r.Kind() == "haskell_module" {
		// For each haskell module:
		originatingRules := getOriginatingRules(r)
		moduleSpecs := make([]resolve.ImportSpec, len(originatingRules), 2*len(originatingRules)+1)
		// A 'moduleByFileSpec' entry is added to the RuleIndex for each rule using this module.
		// These entries are used by 'SetNonHaskellModuleDeps' to list the modules in the libraries/binaries/tests.
		for i, originatingRule := range originatingRules {
			moduleSpecs[i] = moduleByFilepathSpec(f.Pkg, originatingRule.Name(), getSrcFromRule(c.RepoRoot, f.Path, r))
		}
		// A 'moduleByModuleImportSpec' entry is added to the RuleIndex for each rule using this module.
		// These entries are used by 'SetHaskellModuleDeps' to list the dependencies between 'haskell_module' rules.
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
		// For each haskell library/library/test:
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
		// A 'libraryOfModuleSpec' entry is added to the RuleIndex for each module of the rule.
		// These entries are used by 'SetHaskellModuleDeps' to find the library using a module.
		i := 0
		for moduleLabel := range moduleLabels {
			moduleSpecs[i] = libraryOfModuleSpec(moduleLabel)
			i++
		}
		// A 'isDepOfLibrarySpec' entry is added to the RuleIndex for each dependency of the rule.
		// These entries are used by 'SetHaskellModuleDeps' to find the dependencies between modules in 'narrowed_deps'.
		i = 0
		for libLabel := range libraryDepLabels {
			moduleSpecs[len(moduleLabels)+i] = isDepOfLibrarySpec(libLabel, f.Pkg, r.Name())
			i++
		}
		// A 'libraryUsesModulesSpec' entry is added to the RuleIndex.
		// These entries are used by 'SetNonHaskellModuleDeps' to know if the dependency to the current lib should be stored as a 'narrowed_deps'.
		if usesModules > 0 {
			moduleSpecs[len(moduleSpecs)-1] = libraryUsesModulesSpec(label.New(c.RepoName, f.Pkg, r.Name()))
		}
		return moduleSpecs
	} else {
		return []resolve.ImportSpec{}
	}
}

func (*gazelleHaskellModulesLang) Embeds(r *rule.Rule, from label.Label) []label.Label { return nil }

func (*gazelleHaskellModulesLang) Resolve(c *config.Config, ix *resolve.RuleIndex, rc *repo.RemoteCache, r *rule.Rule, imports interface{}, from label.Label) {
	if isNonHaskellModule(r.Kind()) {
		setNonHaskellModuleDeps(c.RepoRoot, ix, r, imports.(*HRuleImportData), from)
	} else {
		setHaskellModuleDeps(ix, r, imports.(*HModuleImportData), from)
	}
}

func (*gazelleHaskellModulesLang) GenerateRules(args language.GenerateArgs) language.GenerateResult {
	if args.File == nil {
		return language.GenerateResult{
			Gen:     []*rule.Rule{},
			Imports: []interface{}{},
		}
	}

	ruleInfos := rulesToRuleInfos(args.Dir, args.File.Rules, args.Config.RepoName, args.File.Pkg)
	generateResult := infoToRules(args.Dir, ruleInfos)

	setVisibilities(args.File, generateResult.Gen)

	return addNonHaskellModuleRules(args.Dir, args.Config.RepoName, args.File.Pkg, generateResult, args.File.Rules)
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
