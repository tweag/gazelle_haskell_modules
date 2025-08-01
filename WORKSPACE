workspace(name = "io_tweag_gazelle_haskell_modules")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

##########################
# rules_proto preamble
##########################

http_archive(
    name = "rules_proto",
    sha256 = "6fb6767d1bef535310547e03247f7518b03487740c11b6c6adb7952033fe1295",
    strip_prefix = "rules_proto-6.0.2",
    url = "https://github.com/bazelbuild/rules_proto/releases/download/6.0.2/rules_proto-6.0.2.tar.gz",
)

load("@rules_proto//proto:repositories.bzl", "rules_proto_dependencies")

rules_proto_dependencies()

load("@rules_proto//proto:setup.bzl", "rules_proto_setup")

rules_proto_setup()

load("@rules_proto//proto:toolchains.bzl", "rules_proto_toolchains")

rules_proto_toolchains()

##########################
# rules_nixpkgs preamble
##########################

http_archive(
    name = "io_tweag_rules_nixpkgs",
    sha256 = "30271f7bd380e4e20e4d7132c324946c4fdbc31ebe0bbb6638a0f61a37e74397",
    strip_prefix = "rules_nixpkgs-0.13.0",
    urls = ["https://github.com/tweag/rules_nixpkgs/releases/download/v0.13.0/rules_nixpkgs-0.13.0.tar.gz"],
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:repositories.bzl",
    "rules_nixpkgs_dependencies",
)

rules_nixpkgs_dependencies(toolchains = [
    "go",
    "python",
    "posix",
])

##########################
# rules_haskell preamble
##########################

http_archive(
    name = "rules_haskell",
    sha256 = "4cae22bc84f327bf3cb7605021c3663160ff6bc8a0b7b6266062366bcbd19e79",
    strip_prefix = "rules_haskell-1.0",
    url = "https://github.com/tweag/rules_haskell/releases/download/v1.0/rules_haskell-1.0.tar.gz",
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load(
    "@rules_nixpkgs_core//:nixpkgs.bzl",
    "nixpkgs_local_repository",
    "nixpkgs_package",
)
load(
    "@rules_nixpkgs_python//:python.bzl",
    "nixpkgs_python_configure",
)

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//:nixpkgs.nix",
)

nixpkgs_python_configure(repository = "@nixpkgs")

nixpkgs_package(
    name = "stack",
    attribute_path = "stack",
    repository = "@nixpkgs",
)

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot", "use_stack")

use_stack("@stack//:bin/stack")

######################################
# Haskell dependencies and toolchain
######################################

load("//:config_settings/setup.bzl", "config_settings")

config_settings(name = "config_settings")

load("@config_settings//:info.bzl", "ghc_version")
load("@io_tweag_gazelle_haskell_modules//:defs.bzl", "gazelle_haskell_modules_dependencies")

gazelle_haskell_modules_dependencies()

stack_snapshot(
    name = "stackage",
    components =
        {
            "tasty-discover": [
                "lib",
                "exe:tasty-discover",
            ],
        },
    components_dependencies =
        None if ghc_version == "8.10.7" else {
            "attoparsec": """{"lib:attoparsec": ["lib:attoparsec-internal"]}""",
        },
    local_snapshot = "//:snapshot-" + ghc_version + ".yaml",
    packages = [
        "Cabal",
        "hspec",
        "json",
        "string-qq",
        "tasty",
        "tasty-discover",
        "tasty-hspec",
    ],
    setup_deps = {
        "transformers-compat": ["@stackage//:Cabal"],
        "hspec-discover": ["@stackage//:Cabal"],
        "call-stack": ["@stackage//:Cabal"],
        "HUnit": ["@stackage//:Cabal"],
        "quickcheck": ["@stackage//:Cabal"],
        "hspec-expectations": ["@stackage//:Cabal"],
        "quickcheck-io": ["@stackage//:Cabal"],
        "tasty-discover": ["@stackage//:Cabal"],
        "hspec-core": ["@stackage//:Cabal"],
        "bifunctors": ["@stackage//:Cabal"],
        "hspec": ["@stackage//:Cabal"],
    },
    stack_snapshot_json = "//:snapshot-" + ghc_version + ".json",
)

load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")

haskell_register_ghc_nixpkgs(
    attribute_path =
        "haskell.compiler.ghc" + ghc_version.replace(".", ""),
    ghcopts = [
        "-Werror",
        "-Wall",
        "-Wcompat",
        "-Wincomplete-record-updates",
        "-Wredundant-constraints",
    ],
    repository = "@nixpkgs",
    version = ghc_version,
)

###############
# Go preamble
###############

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "9d72f7b8904128afb98d46bbef82ad7223ec9ff3718d419afb355fddd9f9484a",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.55.1/rules_go-v0.55.1.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.55.1/rules_go-v0.55.1.zip",
    ],
)

load(
    "@rules_nixpkgs_go//:go.bzl",
    "nixpkgs_go_configure",
)

nixpkgs_go_configure(
    repository = "@nixpkgs",
)

load("@io_bazel_rules_go//go:deps.bzl", "go_rules_dependencies")

go_rules_dependencies()

####################
# Gazelle preamble
####################

http_archive(
    name = "bazel_gazelle",
    sha256 = "49b14c691ceec841f445f8642d28336e99457d1db162092fd5082351ea302f1d",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.44.0/bazel-gazelle-v0.44.0.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.44.0/bazel-gazelle-v0.44.0.tar.gz",
    ],
)

load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")

gazelle_dependencies()

#######################
# Buildifier preamble
#######################

nixpkgs_package(
    name = "buildifier",
    attribute_path = "buildifier",
    build_file_content = """\
filegroup(
  name = "buildifier",
  srcs = ["bin/buildifier"],
  visibility = ["//visibility:public"]
)""",
    repository = "@nixpkgs",
)
