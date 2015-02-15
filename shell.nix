with (import <nixpkgs> {}).pkgs;
let pkg = haskellngPackages.callPackage
            (import ./.) {};
in
  pkg.env
