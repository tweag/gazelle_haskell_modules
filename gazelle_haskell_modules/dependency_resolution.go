// Functions used for dependency resolution
package gazelle_haskell_modules

import (
	"fmt"

	//"github.com/bazelbuild/bazel-gazelle/config"
	"github.com/bazelbuild/bazel-gazelle/label"
	//"github.com/bazelbuild/bazel-gazelle/language"
	//golang "github.com/bazelbuild/bazel-gazelle/language/go"
	//"github.com/bazelbuild/bazel-gazelle/language/proto"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"
	//"github.com/bazelbuild/bazel-gazelle/walk"

	"log"
	"path/filepath"
	"sort"
)

// This is to be called only on haskell_library, haskell_binary,
// or haskell_test rules.
//
// Adds to the modules attribute the labels of all haskell_module
// rules originated from this rule.
func setNonHaskellModuleDeps(
	c *Config,
	repoRoot string,
	ix *resolve.RuleIndex,
	r *rule.Rule,
	importData *HRuleImportData,
	from label.Label,
) {
	modules := importData.Modules
	for _, f := range importData.Srcs {
		mod, err := findModuleLabelByModuleFilePath(repoRoot, ix, f, r.Name(), from)
		if err != nil {
			log.Fatal("On rule ", label.New(from.Repo, from.Pkg, r.Name()), ": ", err)
		}
		if mod == nil {
			log.Fatal("On rule ", label.New(from.Repo, from.Pkg, r.Name()), ": couldn't find haskell_module rule for source ", f)
		}
		modules[*mod] = true
	}
	moduleStrings := make([]string, len(modules))
	i := 0
	for lbl, _ := range modules {
		moduleStrings[i] = rel(lbl, from).String()
		i++
	}
	sort.Strings(moduleStrings)

	r.SetAttr("modules", moduleStrings)
}

// Sets as deps the labels of all imported modules.
// If the origin of an imported module can't be determined, it
// is ignored.
func setHaskellModuleDeps(
	ix *resolve.RuleIndex,
	r *rule.Rule,
	importData *HModuleImportData,
	from label.Label,
) {
	originalComponentName := importData.OriginatingRule.Name()
	depsCapacity := len(importData.ImportedModules)
	deps := make([]string, 0, depsCapacity)
	for _, mod := range importData.ImportedModules {
		dep, err := findModuleLabelByModuleName(ix, mod, originalComponentName, from)
		if err != nil {
			log.Fatal("On rule ", r.Name(), ": ", err)
		}
		if dep == nil {
			continue
		}
		deps = append(deps, rel(*dep, from).String())
	}

	if len(deps) > 0 {
		r.SetAttr("deps", deps)
	}
}

// Yields the label of a module with the given name.
//
// If the importing module comes from the same component (originalComponentName)
// as the given moduleName, the rule defining the module for the given component is
// chosen.
//
// If multiple rules define the module, an error is returned.
//
// If no rule defines the module, nil is returned.
//
func findModuleLabelByModuleName(
	ix *resolve.RuleIndex,
	moduleName string,
	originalComponentName string,
	from label.Label,
) (*label.Label, error) {
	spec := resolve.ImportSpec{gazelleHaskellModulesName, fmt.Sprintf("module_name:%s:%s:%s", from.Pkg, originalComponentName, moduleName)}
	res := ix.FindRulesByImport(spec, gazelleHaskellModulesName)

	switch len(res) {
	case 1:
		lbl := rel(res[0].Label, from)
		return &lbl, nil
	case 0:
		return nil, nil
	default:
		lbls := make([]label.Label, len(res))
		for i, r := range res {
			lbls[i] = r.Label
		}
		return nil, fmt.Errorf("Multiple rules define %q in %v: %v ", moduleName, label.New(from.Repo, from.Pkg, originalComponentName), lbls)
	}
}

func findModuleLabelByModuleFilePath(
	repoRoot string,
	ix *resolve.RuleIndex,
	moduleFilePath string,
	componentName string,
	from label.Label,
) (*label.Label, error) {
	relModuleFilePath, err := filepath.Rel(repoRoot, moduleFilePath)
	if err != nil {
		return nil, fmt.Errorf("Can't make src relative: %q: %v", moduleFilePath, err)
	}

	spec := resolve.ImportSpec{gazelleHaskellModulesName, fmt.Sprintf("filepath:%s:%s:%s", from.Pkg, componentName, relModuleFilePath)}
	res := ix.FindRulesByImport(spec, gazelleHaskellModulesName)

	switch len(res) {
	case 1:
		lbl := rel(res[0].Label, from)
		return &lbl, nil
	case 0:
		return nil, nil
	default:
		lbls := make([]label.Label, len(res))
		for i, r := range res {
			lbls[i] = r.Label
		}
		return nil, fmt.Errorf("Multiple rules define %q in %v: %v ", moduleFilePath, label.New(from.Repo, from.Pkg, componentName), lbls)
	}
}

func rel(lbl label.Label, from label.Label) label.Label {
	return lbl.Rel(from.Repo, from.Pkg)
}

// "//package".Abs(repo, pkg) leaves the label unchanged when we
// would need "@repo//package"
func abs(lbl label.Label, repo string, pkg string) label.Label {
	if lbl.Repo == "" {
		if lbl.Pkg == "" {
			return label.New(repo, pkg, lbl.Name)
		} else {
			return label.New(repo, lbl.Pkg, lbl.Name)
		}
	} else {
		lbl.Relative = false
		return lbl
	}
}
