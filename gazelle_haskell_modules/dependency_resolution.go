// Functions used for dependency resolution
package gazelle_haskell_modules

import (
	"fmt"
	"strings"

	"github.com/bazelbuild/bazel-gazelle/label"
	"github.com/bazelbuild/bazel-gazelle/resolve"
	"github.com/bazelbuild/bazel-gazelle/rule"

	"log"
	"path/filepath"
	"sort"
)

// This is to be called only on haskell_library, haskell_binary,
// or haskell_test rules.
//
// Adds to the modules attribute the labels of all haskell_module
// rules originated from this rule.
//
// Also splits deps into deps and narrowed_deps. Any dependency
// from deps that uses the modules attribute is moved to narrowed_deps.
//
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

	depStrings := make([]string, 0, len(importData.Deps))
	narrowedDepStrings := make([]string, 0, len(importData.Deps))
	for dep := range importData.Deps {
		if doesLibraryUseModules(ix, dep) {
			narrowedDepStrings = append(narrowedDepStrings, rel(dep, from).String())
		} else {
			depStrings = append(depStrings, rel(dep, from).String())
		}
	}

	if len(depStrings) > 0 {
		r.SetAttr("deps", depStrings)
	}
	if len(narrowedDepStrings) > 0 {
		r.SetAttr("narrowed_deps", narrowedDepStrings)
	}
	if len(moduleStrings) > 0 {
		r.SetAttr("modules", moduleStrings)
	}
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
	originalLibs := librariesOfModule(ix, label.New(from.Repo, from.Pkg, r.Name()))
	depsCapacity := len(importData.ImportedModules)
	deps := make([]string, 0, depsCapacity)
	crossLibraryDeps := make([]string, 0, depsCapacity)
	for _, mod := range importData.ImportedModules {
		dep, err := findModuleLabelByModuleName(ix, &mod, originalLibs)
		if err != nil {
			log.Fatal("On rule ", r.Name(), ": ", err)
		}
		if dep != nil {
			deps = append(deps, rel(*dep, from).String())
			continue
		}

		dep, err = findCrossLibraryModuleLabelByModuleName(ix, &mod, originalLibs)
		if err != nil {
			log.Fatal("On rule ", r.Name(), ": ", err)
		}
		if dep != nil {
			crossLibraryDeps = append(crossLibraryDeps, rel(*dep, from).String())
		}
	}

	if importData.UsesTH || optionsEnableTH(r.AttrStrings("ghcopts")) {
		r.SetAttr("enable_th", true)
	}
	if len(deps) > 0 {
		r.SetAttr("deps", deps)
	}
	if len(crossLibraryDeps) > 0 {
		r.SetAttr("cross_library_deps", crossLibraryDeps)
	}
}

func optionsEnableTH(opts []string) bool {
	for _, o := range opts {
		if o == "-XTemplateHaskell" || o == "-XQuasiQuotes" {
			return true
		}
	}
	return false
}

// Yields the label of a haskell_module rule for a module with the
// given name coming from any library in libs.
//
// If multiple such rules define the module, an error is returned.
//
// If no rule defines the module, nil is returned.
//
func findModuleLabelByModuleName(
	ix *resolve.RuleIndex,
	moduleImport *ModuleImport,
	libs []label.Label,
) (*label.Label, error) {
	spec := moduleByModuleImportSpec(moduleImport)
	isBootDep := false
	// If the module we are considering comes from an `hs-boot` file,
	// we store this in the `isBootDep` variable and then
	// behave as if we were considering the standard version of ther module.
	if isBoot(spec.Imp) {
		isBootDep = true
		spec.Imp = strings.TrimSuffix(spec.Imp, PRIVATE_ATTR_BOOT_EXTENSION)
	}
	res := ix.FindRulesByImport(spec, gazelleHaskellModulesName)

	var foundLabel label.Label
	for _, r := range res {
		// When looking for the label, we ignore the `hs-boot` version of the file,
		// since it would lead to collision when investigating for the library associated to this module name.
		if !isBoot(r.Label.Name) {
			intersection := intersectLabelArrays(librariesOfModule(ix, r.Label), libs)
			if len(intersection) > 0 {
				if foundLabel.Name != "" {
					return nil, fmt.Errorf(
						"Multiple rules define %q:%s in %v: %v and %v",
						moduleImport.PackageName,
						moduleImport.ModuleName,
						intersection,
						foundLabel,
						r.Label,
					)
				} else {
					foundLabel = r.Label
				}
			}
		}
	}
	if foundLabel.Name != "" {
		// Since those names were ignored in the first computation,
		// we need to add the suffix, whenever we are studying a source import.
		foundLabel.Name = foundLabel.Name + bootSuffixIf(isBootDep)
		return &foundLabel, nil
	} else {
		return nil, nil
	}
}

// Yields the label of a haskell_module rule for a module with the
// given name coming from some library that is a dependency of a
// library in libs.
//
// If multiple such rules define the module, an error is returned.
//
// If no rule defines the module, nil is returned.
//
func findCrossLibraryModuleLabelByModuleName(
	ix *resolve.RuleIndex,
	moduleImport *ModuleImport,
	libs []label.Label,
) (*label.Label, error) {
	spec := moduleByModuleImportSpec(moduleImport)
	res := ix.FindRulesByImport(spec, gazelleHaskellModulesName)

	var foundLabel label.Label
	var foundNarrowedLibLabel label.Label
	var foundOriginalLibLabel label.Label
	for _, r := range res {
		narrowedLib, originalLib := isDepOfAnyLibrary(ix, librariesOfModule(ix, r.Label), libs)
		if narrowedLib != nil {
			if foundLabel.Name != "" {
				lbls := make([]label.Label, len(res))
				for i, r1 := range res {
					lbls[i] = r1.Label
				}
				return nil, fmt.Errorf("Multiple rules define %q:%s in narrowed deps of %v and %v: %v and %v with narrowed_deps %v and %v",
					moduleImport.PackageName,
					moduleImport.ModuleName,
					foundOriginalLibLabel,
					originalLib,
					foundLabel,
					r.Label,
					foundNarrowedLibLabel,
					narrowedLib,
				)
			} else {
				foundLabel = r.Label
				foundNarrowedLibLabel = *narrowedLib
				foundOriginalLibLabel = *originalLib
			}
		}
	}
	if foundLabel.Name != "" {
		return &foundLabel, nil
	} else {
		return nil, nil
	}
}

func intersectLabelArrays(a []label.Label, b []label.Label) []label.Label {
	res := make([]label.Label, 0, len(a))
	for _, lbla := range a {
		for _, lblb := range b {
			if lbla == lblb {
				res = append(res, lbla)
				break
			}
		}
	}
	return res
}

func librariesOfModule(ix *resolve.RuleIndex, moduleLabel label.Label) []label.Label {
	spec := libraryOfModuleSpec(moduleLabel)
	res := ix.FindRulesByImport(spec, gazelleHaskellModulesName)

	libs := make([]label.Label, len(res))
	for i, r := range res {
		libs[i] = r.Label
	}
	return libs
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

	spec := moduleByFilepathSpec(from.Pkg, componentName, relModuleFilePath)
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

func doesLibraryUseModules(ix *resolve.RuleIndex, libraryLabel label.Label) bool {
	spec := libraryUsesModulesSpec(libraryLabel)
	res := ix.FindRulesByImport(spec, gazelleHaskellModulesName)

	return len(res) > 0
}

func libraryUsesModulesSpec(libLabel label.Label) resolve.ImportSpec {
	return resolve.ImportSpec{
		gazelleHaskellModulesName,
		fmt.Sprintf("library_uses_modules:%s", libLabel.String()),
	}
}

func libraryOfModuleSpec(moduleLabel label.Label) resolve.ImportSpec {
	return resolve.ImportSpec{
		gazelleHaskellModulesName,
		fmt.Sprintf("library_of_module:%s", moduleLabel.String()),
	}
}

func moduleByModuleImportSpec(moduleImport *ModuleImport) resolve.ImportSpec {
	return resolve.ImportSpec{
		gazelleHaskellModulesName,
		fmt.Sprintf("module_name:%s:%s", moduleImport.PackageName, moduleImport.ModuleName),
	}
}

func moduleByPackageImportSpec(pkgName string, moduleName string) resolve.ImportSpec {
	return resolve.ImportSpec{
		gazelleHaskellModulesName,
		fmt.Sprintf("module_name:%s:%s", pkgName, moduleName),
	}
}

func moduleByFilepathSpec(pkg string, componentName string, relativeModuleFilePath string) resolve.ImportSpec {
	return resolve.ImportSpec{
		gazelleHaskellModulesName,
		fmt.Sprintf("filepath:%s:%s:%s", pkg, componentName, relativeModuleFilePath),
	}
}

func isDepOfAnyLibrary(ix *resolve.RuleIndex, depLabels []label.Label, libs []label.Label) (*label.Label, *label.Label) {

	for _, depLabel := range depLabels {
		for _, lib := range libs {
			spec := isDepOfLibrarySpec(depLabel, lib.Pkg, lib.Name)
			res := ix.FindRulesByImport(spec, gazelleHaskellModulesName)

			if len(res) > 0 {
				return &depLabel, &lib
			}
		}
	}

	return nil, nil
}

func isDepOfLibrarySpec(dep label.Label, pkg string, libName string) resolve.ImportSpec {
	return resolve.ImportSpec{
		gazelleHaskellModulesName,
		fmt.Sprintf("is_dep_of:%s:%s:%s", dep.String(), pkg, libName),
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

func isBoot(s string) bool {
	return strings.HasSuffix(s, PRIVATE_ATTR_BOOT_EXTENSION)
}

func bootSuffixIf(b bool) string {
	res := ""
	if b {
		res = PRIVATE_ATTR_BOOT_EXTENSION
	}
	return res
}
