""" Implementation of gazelle_haskell_modules_dependencies """

def _gazelle_haskell_modules_dependencies_impl(repository_ctx):
    repository_ctx.file(
        "BUILD",
        content = '''
package(default_visibility = ["//visibility:public"])

alias(name="aeson", actual="{aeson}")
        '''.format(
            aeson = repository_ctx.attr.aeson,
        ),
        executable = False,
    )

_gazelle_haskell_modules_dependencies = repository_rule(
    implementation = _gazelle_haskell_modules_dependencies_impl,
    local = True,
    attrs = {
        "aeson": attr.label(default = "@stackage//:aeson"),
    },
)

def gazelle_haskell_modules_dependencies(**kargs):
    """
    Produces a repository with the dependencies of himportscan.

    The main purpose of it is to offer a convenient way to override
    the dependencies that himportscan uses. By default, all dependencies
    are assumed to come from a @stackage repository.

    Example:

      ```bzl
      # Dependencies taken from @stackage
      gazelle_haskell_modules_dependencies()

      # Dependencies overriden
      gazelle_haskell_modules_dependencies(
          aeson = "@someother//:some-other-aeson",
      )

      ```

    """
    _gazelle_haskell_modules_dependencies(name = "io_tweag_gazelle_haskell_modules_deps", **kargs)
