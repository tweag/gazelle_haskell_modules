workspace(name = "io_tweag_gazelle_haskell_modules")

load("@bazel_tools//tools/build_defs/repo:http.bzl", "http_archive")

##########################
# rules_haskell preamble
##########################

http_archive(
    name = "rules_haskell",
    sha256 = "57e55ca74c9dd2710da852c6c9a70fc0274f038ff37216b6c48fd9389bbfbce7",
    strip_prefix = "rules_haskell-b8ac6c18d26c0011a2464200762e45302a70bbf6",
    urls = ["https://github.com/tweag/rules_haskell/archive/b8ac6c18d26c0011a2464200762e45302a70bbf6.zip"],
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
)

load("@rules_haskell//haskell:nixpkgs.bzl", "haskell_register_ghc_nixpkgs")

haskell_register_ghc_nixpkgs(
    attribute_path =
        "haskell.compiler.ghc" + ghc_version.replace(".", ""),
    compiler_flags = [
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

####################
# Gazelle Cabal
####################

http_archive(
    name = "io_tweag_gazelle_cabal",
    sha256 = "9a85c591a06a9d8a18d14dbbc2743eba72883d0424f56cdcd25a60cecdd9903c",
    strip_prefix = "gazelle_cabal-76b2e64dfcfdbbf480b4364d9526f68f62933740",
    urls = [
        "https://github.com/tweag/gazelle_cabal/archive/76b2e64dfcfdbbf480b4364d9526f68f62933740.zip",
    ],
)

load("@io_tweag_gazelle_cabal//:defs.bzl", "gazelle_cabal_dependencies")

gazelle_cabal_dependencies()

#######################
# Buildifier preamble
#######################

http_archive(
    name = "com_github_bazelbuild_buildtools",
    sha256 = "5b7fe9aa131ab64a51de4da3668005cf58418c967438ce129aad24fd3e6dfaa9",
    strip_prefix = "buildtools-4890966c38b910fd5bd1ad78a3dd88538d09854f",
    url = "https://github.com/bazelbuild/buildtools/archive/4890966c38b910fd5bd1ad78a3dd88538d09854f.zip",
)
