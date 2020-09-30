{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "apiary"
, dependencies = [ "affjax", "media-types", "simple-json" ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
