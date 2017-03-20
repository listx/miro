with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "miro";
  ghc = haskell.packages.ghc802.ghc;
  buildInputs = [
    zlib # for 'diagrams' package
  ];
}

