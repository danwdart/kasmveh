{
  nixpkgs ? import <nixpkgs> {},
  haskell-tools ? import (builtins.fetchTarball "https://github.com/danwdart/haskell-tools/archive/master.tar.gz") {
    nixpkgs = nixpkgs;
    compiler = compiler;
  },
  compiler ? "ghc910"
}:
let
  gitignore = nixpkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];
  tools = haskell-tools compiler;
  lib = nixpkgs.pkgs.haskell.lib;
  myHaskellPackages = nixpkgs.pkgs.haskell.packages.${compiler}.override {
    overrides = self: super: rec {
      # Don't check necessarily until we can include non-Haskell stuff for tests
      kasmveh = lib.doBenchmark (lib.dontHaddock (self.callCabal2nix "kasmveh" (gitignore ./.) {}));
    };
  };
  shell = myHaskellPackages.shellFor {
    packages = p: [
      p.kasmveh
    ];
    shellHook = ''
      gen-hie > hie.yaml
      for i in $(find -type f | grep -v dist-newstyle); do krank $i; done
    '';
    buildInputs = tools.defaultBuildTools ++ [ nixpkgs.gettext nixpkgs.nodejs_20 nixpkgs.php82 nixpkgs.tinycc ];
    nativeBuildInputs = tools.defaultBuildTools ++ [ nixpkgs.gettext nixpkgs.nodejs_20 nixpkgs.php82 nixpkgs.tinycc ];
    withHoogle = false;
  };
  in
{
  inherit shell;
  kasmveh = lib.justStaticExecutables (myHaskellPackages.kasmveh);
}
