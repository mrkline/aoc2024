(import <nixos> {}).pkgs.haskellPackages.ghcWithPackages (p: with p; [
    bytestring
    text
    vector
])
