{
  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };
  outputs = { self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (
      system: let
        pkgs = import nixpkgs { inherit system; };
      in
        {
          devShell = pkgs.haskell.packages.ghc8104.shellFor {
            buildInputs = with pkgs; [
              rnix-lsp
              cabal-install
              haskell-language-server
            ];
            packages = ps: with ps; [
              vector
              req
            ];
          };
        }
    );
}
