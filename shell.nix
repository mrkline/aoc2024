(import <nixos> {}).pkgs.haskellPackages.ghcWithPackages (p: with p; [
    bytestring
    mtl
    text
    vector
])
