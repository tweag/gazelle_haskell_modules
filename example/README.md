This is an example project using the [gazelle_haskell_modules][gazelle_haskell_modules] extension.

Run the following to generate the `haskell_module` rules from the other
Haskell rules.
```bazel
echo "build --host_platform=@io_tweag_rules_nixpkgs//nixpkgs/platforms:host" > .bazelrc.local
nix-shell --pure --run "bazel run //:gazelle"
```
Then you can check the generated rules in `example/package-a/BUILD.bazel`
and `example/package-b/BUILD.bazel`.

Now you should be able to build and test with
```bazel
nix-shell --pure --run "bazel test //..."
```

[gazelle_haskell_modules]: https://github.com/tweag/gazelle_haskell_modules
