{
  description = "Inspect the size and padding of C structs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.05";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        lib = pkgs.lib;
        stdenv = pkgs.stdenv;
        packageName = "struct-inspector";
        src = pkgs.nix-gitignore.gitignoreSource [] ./.;
      in

      rec {
        packages.${packageName} = pkgs.haskellPackages.callCabal2nix "struct-inspector" src {};
        defaultPackage = packages.${packageName};
      });
}
