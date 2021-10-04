# gazelle\_haskell\_modules

This is a [gazelle][gazelle] extension that generates `haskell_module`
rules from `haskell_library`, `haskell_binary`, and `haskell_test` as
defined in [Haskell rules][rules_haskell] for [Bazel][bazel].

For each `haskell_library` rule, `haskell_module` rules are generated
in the same `BUILD` for all modules listed in the `srcs` attribute.

This [example repo][example] shows it in action.

Right now, this is work in progress. The features that are implemented
include a scanner of imports in Haskell modules, and the generation of
some `haskell_module` rules. See the **What's next** section below.

## Configuration

Firstly, setup [gazelle][gazelle] and [rules_haskell][rules_haskell].
Then import `gazelle_haskell_modules`.

```python
http_archive(
	name = "io_tweag_gazelle_haskell_modules",
	strip_prefix = "gazelle_haskell_modules-main",
	url = "https://github.com/tweag/gazelle_haskell_modules/archive/main.zip",
)
```

Additionally, some Haskell packages are needed to build
`gazelle_haskell_modules`. The simplest way to bring them is to use the
`stack_snapshot` rule in the `WORKSPACE` file as follows.

```python
load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")
load("@io_tweag_gazelle_haskell_modules//:defs.bzl", "gazelle_haskell_modules_dependencies")
gazelle_haskell_modules_dependencies()

stack_snapshot(
	name = "stackage",
	packages = [
		"aeson", # keep
		"parsec", # keep
	],
	# Most snapshots of your choice might do
	snapshot = "lts-18.1",
)
```
Should Haskell packages need to be grabbed from elsewhere, alternative
labels can be provided to [gazelle_haskell_modules_dependencies][gazelle_haskell_modules_dependencies].

You can generate or update build rules by adding the following to
one of your `BUILD` files.

```python
load(
	"@bazel_gazelle//:def.bzl",
	"DEFAULT_LANGUAGES",
	"gazelle",
	"gazelle_binary",
)

gazelle(
	name = "gazelle",
	data = ["@io_tweag_gazelle_haskell_modules//himportscan"],
	gazelle = ":gazelle_binary",
)

gazelle_binary(
	name = "gazelle_binary",
	languages = DEFAULT_LANGUAGES + ["@io_tweag_gazelle_haskell_modules//gazelle_haskell_modules"],
)
```

## Running

Build and run gazelle with
```bash
bazel run //:gazelle
```

Gazelle's [fix command][fix-command] can be used to delete rules when
components are removed from the cabal file.

## Rule generation

Each module listed in the `srcs` attribute of a Haskell rule originates
a `haskell_module` rule with name `<pkg>.<module>`. Some attributes
are copied from the originating rule to the `haskell_module` rule, like
`ghcopts`, `tools`, and `extra_srcs`.

The `deps` attribute is managed in a special way though. Each dependency
is considered for addition to the `deps` attribute of a `haskell_module`
rule. If the dependency is defined in the current repository, and it is
defined with a Haskell rule, then the dependency is dropped since the
`haskell_module` rule is expected to depend directly on the modules of
that dependency if at all.

If the dependency isn't defined in the same repo, or isn't defined with
Haskell rules, then the dependcy is added since the source module might
depend on it without `gazelle_haskell_modules` being able to determine it.

### Dependencies of non-haskell\_module rules

The `srcs` attributes of `haskell_library`, `haskell_binary`, and
`haskell_test` are cleared. The `deps` attribute is augmented with
the labels of the corresponding `haskell_module` rules.

Additionally, `gazelle_haskell_modules` removes from `deps` the
dependencies defined with other `haskell_library` rules. These
dependencies are unnecessary because the `haskell_module` rules will
depend on modules from them if needed.

### Updating dependencies of haskell\_module rules

When the imports in a module are changed, the corresponding
`haskell_module` rule might need to be updated. Removed imports are
removed from the `deps` attribute, and added imports might originate new
dependencies. Adding an import to a module that is defined in the current
repo, will add that module to the dependencies as long as the name of the
module uniquely identifies a rule that defines it. If two different rules
defined the same module name, `gazelle_haskell_modules` will produce an
error.

Note that `gazelle_haskell_modules` will always chose the right module
when generating new `haskell_module` rules because the list of
dependencies in the originating rule allows to determine from which
library imports are coming from. Some of these dependencies are erased
later on, though, so they aren't available when the `haskell_module`
rules need to be updated.

## Implementation

`gazelle_haskell_modules` extracts module imports from Haskell modules
using [himportscan][himportscan], a command line tool written in Haskell,
which presents the extracted data in json format to the `go` part.

The [go part][go-part] consists of a set of functions written in the
[go][go] programming language, which `gazelle` will invoke to
generate `haskell_module` rules. The most important functions are:

* `GenerateRules`: calls the `himportscan` command-line tool and
  produces rules that contain no information about dependencies.

* `Imports`: indexes the rules for dependency resolution.

* `Resolve`: adds to the previously generated rules all the
  information about dependencies (`deps`, `plugins`, and `tools`).

## Limitations

### CPP support

CPP directives in source files are ignored when scanning for imports.
That is, `himportscan` would always pick up both imports in the next example.

```Haskell
#if COND
import SomeModule
#else
import SomeOtherModule
#endif
```

### Preprocessor support

Imports may not be possible to extract in files that need preprocessors which
generate those same imports (e.g. `tasty-discover`). In these cases, generation
of `haskell_module` rules can be avoided by using `# keep` comments on the
given files, and any sources that import them.

```python
# The contents of Spec.hs are generated by tasty-discover
haskell_test(
	name = "tests"
	srcs = [
		"tests/Main.hs", # keep
		"tests/Spec.hs", # keep
		"tests/Other.hs"
	]
	deps = ...,
	tools = ["@stackage-exe//tasty-discover"],
)
```

In the above case, only a `haskell_module` rule for `tests/Other.hs` would
be produced.

At the moment, this only works on `haskell_binary` and `haskell_test` rules.
If used in a `haskell_library`, rules for modules in other targets that
import the preprocessed source files would miss them in the dependencies.

### export and reexported\_modules

`haskell_library` has attributes `export` and `reexported_modules` which
affect the dependencies of rules that depend on the Haskell library.
`gazelle_haskell_modules` makes no effort to honor those attributes when
generating rules or resolving imports.

### Package imports

`gazelle_haskell_modules` ignores package imports as implemented in GHC
with `-XPackageImports`.

## What's next

- [X] Have `haskell_module` rules depend on each other.
- [X] Investigate how to index external repositories, which would be needed
  to recognize the provenance of modules coming from external dependencies.
- [X] Have Haskell rules depend on `haskell_module` rules
- [X] Copy `extra_srcs` from the originating rule
- [X] Copy `tools` from the originating rule
- [X] Copy `plugins` from the originating rule
- [X] Skip CPP directives when scanning for imports
- [X] Have a story for preprocessed modules like those needing tasty-discover
- [X] Update `haskell_module` rules when imports change
- [X] Solve interferences with `gazelle_cabal` (removes keep comments on srcs, libsodium label is not understood, deps attribute is reset)
- [X] Lift restriction to name only `haskell_module` rules with dots.
- [X] Lift restriction to name custom `haskell_module` rules as `<package>.<module>`
- [X] Propagate compiler flags and other attributes of libraries, tests, and binaries, to `haskell_module` rules listed in the dependencies.
- [X] Use the `modules` attribute of `haskell_library` instead of `deps`
- [X] Address feedback from code review session.
- [ ] Check if linker options need special treatment
- [ ] Copy `data` from the originating rule (?)
- [X] Document how `gazelle_haskell_modules` works
- [X] Support the fix command to remove outdated `haskell_module` rules
- [X] See whether we can run `gazelle_haskell_modules` and `gazelle_cabal` in one pass
- [ ] Implement tests
- [ ] Setup CI

## Sponsors

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![Symbiont](https://imgur.com/KPV3lTY.png)](https://symbiont.io)
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
[![Tweag I/O](https://i.imgur.com/KPEy44T.png)](http://tweag.io)

`gazelle_haskell_modules` was funded by [Symbiont](https://www.symbiont.io/)
and is maintained by [Tweag I/O](http://tweag.io/).

Have questions? Need help? Tweet at
[@tweagio](http://twitter.com/tweagio).

[bazel]: https://bazel.build
[himportscan]: himportscan/exe/Main.hs
[gazelle_haskell_module_dependencies]: defs.bzl
[example]: example
[fix-command]: https://github.com/bazelbuild/bazel-gazelle#fix-and-update
[gazelle]: https://github.com/bazelbuild/bazel-gazelle
[go-part]: gazelle_haskell_modules/lang.go
[go]: https://golang.org
[rules_haskell]: https://github.com/tweag/rules_haskell
