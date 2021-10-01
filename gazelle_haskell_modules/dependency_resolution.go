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
	"strings"
)


// This is to be called only on haskell_library, haskell_binary,
// or haskell_test rules.
//
// Adds to the deps attribute the labels of all haskell_module
// rules originated from this rule.
//
// Removes dependencies defined in the same repo. haskell_module rules
// will depend on the modules of those dependencies instead.
func setNonHaskellModuleDepsAttribute(
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
			log.Fatal("On rule ", r.Name(), ": ", err)
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

	// Skip dependencies defined in the same repo. Modules will depend directly on those.
	deps := make([]string, 0, len(importData.Deps))
	for dep, _ := range importData.Deps {
	    if !isIndexedNonHaskellModuleRule(ix, dep) {
			deps = append(deps, rel(dep, from).String())
		}
	}
	if r.Kind() == "haskell_library" {
		SetArrayAttr(r, "deps", deps)
		SetArrayAttr(r, "modules", moduleStrings)
	} else {
		SetArrayAttr(r, "deps", append(moduleStrings, deps...))
	}
}

// Sets as deps the labels of all imported modules.
// If the origin of an imported module can't be determined, it
// is ignored.
//
// Dependencies of the originating haskell_library or haskell_binary
// rule are copied, excluding those dependencies that are defined in
// the same repo and not imported in the module source.
func setHaskellModuleDepsAttribute(
	ix *resolve.RuleIndex,
	r *rule.Rule,
	importData *HModuleImportData,
	from label.Label,
) {
	depsCapacity := len(importData.ImportedModules) + len(importData.Deps)
	deps := make([]string, 0, depsCapacity)
	for _, mod := range importData.ImportedModules {
		dep, err := findModuleLabelByModuleName(ix, importData.Deps, mod, from)
		if err != nil {
			log.Fatal("On rule ", r.Name(), ": ", err)
		}
		if dep == nil {
			continue
		}
		deps = append(deps, rel(*dep, from).String())
	}

	for dep, _ := range importData.Deps {
		if !isIndexedNonHaskellModuleRule(ix, dep) && !isIndexedHaskellModuleRule(ix, dep) {
			deps = append(deps, rel(dep, from).String())
		}
	}

	SetArrayAttr(r, "deps", deps)
}

// Yields the label of a module with the given name.
//
// The label is chosen according to the first of the following
// criteria that is met:
//
// 1. If mapDep contains a dependency of the form <pkg>.<the_module_name>,
// it is chosen.
//
// 2. If multiple libraries in mapDep contain the same module,
// one of them is chosen.
//
// 3. If there is only one rule defining the module in the repo,
// it is chosen.
//
// 4. If multiple rules define the module, an error is returned.
//
// 5. If no rule defines the module, nil is returned.
//
func findModuleLabelByModuleName(
	ix *resolve.RuleIndex,
	mapDep map[label.Label]bool,
	moduleName string,
	from label.Label,
) (*label.Label, error) {
	spec := resolve.ImportSpec{gazelleHaskellModulesName, "module_name:" + moduleName}
	res := ix.FindRulesByImport(spec, gazelleHaskellModulesName)

	for _, r := range res {
		if _, ok := mapDep[r.Label]; ok {
			lbl := rel(r.Label, from)
			return &lbl, nil
		}
	}
	for _, r := range res {
		pkgName := strings.SplitN(r.Label.Name, ".", 2)[0]
		pkgLabel := label.New(from.Repo, from.Pkg, pkgName)
		if _, ok := mapDep[pkgLabel]; ok {
			lbl := rel(r.Label, from)
			return &lbl, nil
		}
	}
	if len(res) == 1 {
		lbl := rel(res[0].Label, from)
		return &lbl, nil
	}
	if len(res) > 1 {
		labels := make([]label.Label, len(res))
		for i, r := range res {
			labels[i] = rel(r.Label, from)
		}
		return nil, fmt.Errorf("Multiple rules define %s: %v", moduleName, labels)
	}
	return nil, nil
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

	spec := resolve.ImportSpec{gazelleHaskellModulesName, "filepath:" + relModuleFilePath}
	res := ix.FindRulesByImport(spec, gazelleHaskellModulesName)

	for _, r := range res {
		rComponentName := strings.SplitN(r.Label.Name, ".", 2)[0]
		if componentName == rComponentName {
			lbl := rel(r.Label, from)
			return &lbl, nil
		}
	}
	if len(res) > 1 {
		labels := make([]label.Label, len(res))
		for i, r := range res {
			labels[i] = rel(r.Label, from)
		}
		return nil, fmt.Errorf("Multiple rules define %q: %v", moduleFilePath, labels)
	} else if len(res) == 1 {
		lbl := rel(res[0].Label, from)
		return &lbl, nil
	} else {
		return nil, nil
	}
}

// dep must be an absolute Label
func isIndexedNonHaskellModuleRule(ix *resolve.RuleIndex, dep label.Label) bool {
	spec := resolve.ImportSpec{gazelleHaskellModulesName, "label:" + dep.String()}
	res := ix.FindRulesByImport(spec, gazelleHaskellModulesName)

	return len(res) > 0
}

// dep must be an absolute Label
func isIndexedHaskellModuleRule(ix *resolve.RuleIndex, dep label.Label) bool {
	spec := resolve.ImportSpec{gazelleHaskellModulesName, "haskell_module:" + dep.String()}
	res := ix.FindRulesByImport(spec, gazelleHaskellModulesName)

	return len(res) > 0
}

func rel(lbl label.Label, from label.Label) label.Label {
	return lbl.Rel(from.Repo, from.Pkg)
}

func SetArrayAttr(r *rule.Rule, attrName string, arr []string) {
    if len(arr) > 0 {
        r.SetAttr(attrName, arr)
    }
}
