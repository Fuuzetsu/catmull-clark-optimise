{ ghc }:

let pkgs = import <nixpkgs> {};
in pkgs.haskell.lib.buildStackProject {
  inherit ghc;
  name = "catmull-clark-optimise-env";
  buildInputs = [];
}
