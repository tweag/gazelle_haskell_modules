This is an example project using the [gazelle_haskell_modules][gazelle_haskell_modules] extension.
It tests using other repository names than `stackage` from which to get dependencies.

Run the following to generate the build configuration from the BUILD files.
```bazel
echo "build --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host" > .bazelrc.local
nix-shell --pure --run "bazel run //:gazelle"
nix-shell --pure --run "bazel run //:gazelle-update-repos"
```
Now you can build and test with
```bazel
nix-shell --pure --run "bazel test //..."
```

[gazelle_haskell_modules]: https://github.com/tweag/gazelle_haskell_modules
