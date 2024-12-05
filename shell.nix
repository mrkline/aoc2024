let pkgs = (import <nixos> {});
in pkgs.mkShell {
  packages = [
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [
        bytestring
        mtl
        split
        text
        vector
    ]))
  ];
}
