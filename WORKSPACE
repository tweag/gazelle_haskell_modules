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

# build_bazel_rules_nodejs is only needed when using bazelisk due
# to a rules_haskell issue: https://github.com/tweag/rules_haskell/issues/1635
load("@bazel_tools//tools/build_defs/repo:utils.bzl", "maybe")
maybe(
    http_archive,
    name = "build_bazel_rules_nodejs",
    sha256 = "0fa2d443571c9e02fcb7363a74ae591bdcce2dd76af8677a95965edf329d778a",
    urls = ["https://github.com/bazelbuild/rules_nodejs/releases/download/3.6.0/rules_nodejs-3.6.0.tar.gz"],
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
    sha256 = "2b1641428dff9018f9e85c0384f03ec6c10660d935b750e3fa1492a281a53b0f",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.29.0/rules_go-v0.29.0.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.29.0/rules_go-v0.29.0.zip",
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
    sha256 = "de69a09dc70417580aabf20a28619bb3ef60d038470c7cf8442fafcf627c21cb",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.24.0/bazel-gazelle-v0.24.0.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.24.0/bazel-gazelle-v0.24.0.tar.gz",
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
