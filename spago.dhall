{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "concur-react"
, dependencies =
  [ "arrays"
  , "concur-core"
  , "console"
  , "foldable-traversable"
  , "halogen-vdom"
  , "nonempty"
  , "profunctor-lenses"
  , "tailrec"
  , "web-dom"
  , "web-html"
  ]
, license = "MIT"
, repository = "https://github.com/purescript-concur/purescript-concur-react"
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
