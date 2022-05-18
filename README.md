# gazelle\_haskell\_modules

![Build status](https://github.com/tweag/gazelle_haskell_modules/actions/workflows/workflow.yaml/badge.svg?branch=main)

This is a [gazelle][gazelle] extension that generates `haskell_module`
rules from `haskell_library`, `haskell_binary`, and `haskell_test` as
defined in [Haskell rules][rules_haskell] for [Bazel][bazel]. Moreover,
it updates the dependencies of the generated rules whenever the import
declarions are changed in the source files.

For each `haskell_library`, `haskell_binary`, and `haskell_test` rule,
`haskell_module` rules are generated in the same `BUILD` file for all
modules listed in the `srcs` attribute. For instance,
```python
haskell_library(
    name = "lib",
    srcs = [
        "src/A/B.hs",
        "src/C/D.hs",
    ],
    deps = [":base"],
    ghcopts = ["-threaded"],
)
```
is updated to
```python
haskell_library(
    name = "lib",
    modules = [
        "lib.A.B",
        "lib.C.D",
    ],
    deps = [":base"],
    ghcopts = ["-threaded"],
)

haskell_module(
    name = "lib.A.B",
    src = "src/A/B.hs",
    src_strip_prefix = "src",
    deps = ["lib.C.D"],
)

haskell_module(
    name = "lib.C.D",
    src = "src/C/D.hs",
    src_strip_prefix = "src",
)
```

This [example project][example] shows it in action.

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
        "aeson",
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

Gazelle's [fix command][fix-command] can be used to delete
`haskell_module` rules when they have no enclosing library, binary, or
test. At the moment, the `fix` command only looks for enclosing
rules in the same `BUILD` file containing the `haskell_module` rule.

Additionally, fix will also remove `haskell_module`s whose src files have been deleted, as well as
removing those modules from places where they are referenced.

## Rule generation

### Finding source files

There are two ways for `gazelle_haskell_modules` to find source files containing modules.

### Autodetect directive

By attaching the `gazelle_haskell_modules:srcs: <folders..>` directive to a rule,
`gazelle_haskell_modules` will recursively search `<folders..>` to find Haskell source files
to generate `haskell_module`s with. After finding files, `gazelle_haskell_modules` will proceed
as if the files were manually specified, as documented [below](#specified-in-srcs).

Example:
```bazel
# gazelle_haskell_modules:srcs: src/
haskell_library(
    name = "package-c",
    ...
)
```

More examples of this usage can be found in [`package-c`][directive-examples]

If `srcs` are also explicitly specified in the rule, the results of the directive and the files
listed in the explicit srcs are combined.

[directive-examples]: example/package-c/BUILD.bazel

### Specified in `srcs`

Each module listed in the `srcs` attribute of a Haskell rule originates
a `haskell_module` rule with name `<pkg>.<module>`. The dependencies
of the `haskell_module` rule are populated with labels corresponding
to other `haskell_module` rules originating from the same library,
binary, or test.

### Dependencies of non-haskell\_module rules

The `srcs` attributes of `haskell_library`, `haskell_binary`, and
`haskell_test` are cleared. The `modules` attribute is populated with
the labels of the corresponding `haskell_module` rules.

### Updating dependencies of haskell\_module rules

When the imports in a module are changed, the corresponding
`haskell_module` rule might need to be updated. Removed imports are
removed from the `deps` attribute, and added imports might originate new
dependencies. Adding an import to a module that is defined in the current
repo, will add that module to the dependencies if the importer and the
imported come from the same library, binary, or test.

If no enclosing library, binary, or test can be found for a
`haskell_module` rule, then it won't be updated.

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

### Support for hidden_modules

Currently, the generator allows modules to depend on hidden modules
of dependencies. This is something that should be changed eventually
so the generator fails in these cases.

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
of `haskell_module` rules can be avoided by using `# gazelle_haskell_modules:keep`
comments on the given rule.

```python
# The contents of Spec.hs are generated by tasty-discover
# gazelle_haskell_modules:keep
haskell_test(
    name = "tests"
    srcs = [
        "tests/Main.hs",
        "tests/Spec.hs",
        "tests/Other.hs"
    ]
    deps = ...,
    tools = ["@stackage-exe//tasty-discover"],
)
```

In the above case, the rule won't originate any `haskell_module` rules.

### export and reexported\_modules

`haskell_library` has attributes `export` and `reexported_modules` which
affect the dependencies of rules that depend on the Haskell library.
`gazelle_haskell_modules` makes no effort to honor those attributes when
generating rules or resolving imports.

### Detection of TemplateHaskell

`gazelle_haskell_modules` detects modules that use `TemplateHaskell` by
looking at the `LANGUAGE` pragmas and the `ghcopts` attribute of the
`haskell_module` rule. But the internal or external interpreter could be
activated by using `ANN` pragmas in the module source, or by using
`-XTemplateHaskell` in the `ghcopts` attribute of the enclosing library.
In these cases, `enable_th` won't be set on the `haskell_module` rule
and complains about missing libraries or object files will ensue.

To workaround this, you could set `enable_th = True` manually on
the `haskell_module` rule and use a `#keep` comment.

```
haskell_module(
    name = "...",
    ...
    #keep
    enable_th = True,
    ...
)
```

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
[gazelle_haskell_modules_dependencies]: defs.bzl
[example]: example
[fix-command]: https://github.com/bazelbuild/bazel-gazelle#fix-and-update
[gazelle]: https://github.com/bazelbuild/bazel-gazelle
[go-part]: gazelle_haskell_modules/lang.go
[go]: https://golang.org
[PackageImports]: https://downloads.haskell.org/~ghc/9.0.1/docs/html/users_guide/exts/package_qualified_imports.html
[rules_haskell]: https://github.com/tweag/rules_haskell
