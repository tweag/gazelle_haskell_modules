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
    sha256 = "1adb04dc0416915fef427757f4272c4f7dacefeceeefc50f683aec7f7e9b787a",
    strip_prefix = "rules_nixpkgs-0.12.0",
    urls = ["https://github.com/tweag/rules_nixpkgs/releases/download/v0.12.0/rules_nixpkgs-0.12.0.tar.gz"],
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
    sha256 = "34742848a8882d94a0437b3b1917dea6f58c82fe5762afe8d249d3a36e51935d",
    strip_prefix = "rules_haskell-0.19",
    url = "https://github.com/tweag/rules_haskell/releases/download/v0.19/rules_haskell-0.19.tar.gz",
)

load("@rules_haskell//haskell:repositories.bzl", "rules_haskell_dependencies")

rules_haskell_dependencies()

load(
    "@rules_nixpkgs_core//:nixpkgs.bzl",
    "nixpkgs_local_repository",
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
    sha256 = "f4a9314518ca6acfa16cc4ab43b0b8ce1e4ea64b81c38d8a3772883f153346b8",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/rules_go/releases/download/v0.50.1/rules_go-v0.50.1.zip",
        "https://github.com/bazelbuild/rules_go/releases/download/v0.50.1/rules_go-v0.50.1.zip",
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
    sha256 = "8ad77552825b078a10ad960bec6ef77d2ff8ec70faef2fd038db713f410f5d87",
    urls = [
        "https://mirror.bazel.build/github.com/bazelbuild/bazel-gazelle/releases/download/v0.38.0/bazel-gazelle-v0.38.0.tar.gz",
        "https://github.com/bazelbuild/bazel-gazelle/releases/download/v0.38.0/bazel-gazelle-v0.38.0.tar.gz",
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
