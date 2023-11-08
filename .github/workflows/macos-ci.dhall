let ci = ./ci.dhall

in  ci
  with jobs.build.runs-on = "macos-latest"
  with name = "MacOS Haskell CI"
