{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "argonaut-core"
  , "arrays"
  , "bifunctors"
  , "console"
  , "datetime"
  , "effect"
  , "either"
  , "enums"
  , "error"
  , "exceptions"
  , "fetch"
  , "fetch-core"
  , "foldable-traversable"
  , "foreign-object"
  , "free"
  , "maybe"
  , "node-buffer"
  , "node-fs-aff"
  , "node-process"
  , "partial"
  , "prelude"
  , "strings"
  , "test-unit"
  , "transformers"
  , "tuples"
  , "unicode"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
