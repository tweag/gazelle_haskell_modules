workspace(name = "io_tweag_gazelle_haskell_modules")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

##########################
# rules_haskell preamble
##########################

http_archive(
    name = "rules_haskell",
	sha256 = "707537ea32edd7101ab0e68d5583b805c81cc9f8695a59017d9884edf165a238",
    strip_prefix = "rules_haskell-7a2cdc17fc6b3401b168e33760d4d52d2c33a105",
    urls = ["https://github.com/tweag/rules_haskell/archive/7a2cdc17fc6b3401b168e33760d4d52d2c33a105.zip"],
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:nixpkgs.bzl",
    "nixpkgs_local_repository",
    "nixpkgs_python_configure",
)

nixpkgs_local_repository(
    name = "nixpkgs",
    nix_file = "//:nixpkgs.nix",
)

nixpkgs_python_configure(repository = "@nixpkgs")

load("@rules_haskell//haskell:cabal.bzl", "stack_snapshot")

######################################
# Haskell dependencies and toolchain
######################################

load("@io_tweag_gazelle_haskell_modules//:defs.bzl", "gazelle_haskell_modules_dependencies")

gazelle_haskell_modules_dependencies()

stack_snapshot(
    name = "stackage",
    components = {
        "tasty-discover": [
            "lib",
            "exe:tasty-discover",
        ],
    },
    packages = [
        "aeson",
        "hspec",
        "parsec",
        "path",
        "path-io",
        "tasty",
        "tasty-discover",
        "tasty-hspec",
    ],
    snapshot = "lts-18.1",
)

load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")

haskell_register_ghc_nixpkgs(
    attribute_path = "haskell.compiler.ghc8104",
    compiler_flags = [
        "-Werror",
        "-Wall",
        "-Wcompat",
        "-Wincomplete-record-updates",
        "-Wredundant-constraints",
    ],
    repositories = {"nixpkgs": "@nixpkgs"},
    version = "8.10.4",
)

###############
# Go preamble
###############

http_archive(
    name = "io_bazel_rules_go",
    sha256 = "69de5c704a05ff37862f7e0f5534d4f479418afc21806c887db544a316f3cb6b",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.27.0/rules_go-v0.27.0.tar.gz",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.27.0/rules_go-v0.27.0.tar.gz",
    ],
)

load(
    "@io_tweag_rules_nixpkgs//nixpkgs:toolchains/go.bzl",
    "nixpkgs_go_configure",
)

nixpkgs_go_configure(repository = "@nixpkgs")

load("@io_bazel_rules_go//go:deps.bzl", "go_rules_dependencies")

go_rules_dependencies()

####################
# Gazelle preamble
####################

http_archive(
    name = "bazel_gazelle",
    sha256 = "62ca106be173579c0a167deb23358fdfe71ffa1e4cfdddf5582af26520f1c66f",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.23.0/bazel-gazelle-v0.23.0.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.23.0/bazel-gazelle-v0.23.0.tar.gz",
    ],
)

load("@bazel_gazelle//:deps.bzl", "gazelle_dependencies")

gazelle_dependencies()

#######################
# Buildifier preamble
#######################

http_archive(
    name = "com_github_bazelbuild_buildtools",
    sha256 = "5b7fe9aa131ab64a51de4da3668005cf58418c967438ce129aad24fd3e6dfaa9",
    strip_prefix = "buildtools-4890966c38b910fd5bd1ad78a3dd88538d09854f",
    url = "https://github.com/bazelbuild/buildtools/archive/4890966c38b910fd5bd1ad78a3dd88538d09854f.zip",
)
