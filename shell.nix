let pkgs = (import <nixos> {});
in pkgs.mkShell {
  packages = [
    (pkgs.haskellPackages.ghcWithPackages (p: with p; [
        bytestring
        deque
        mtl
        linear
        parallel
        split
        text
        vector
    ]))
  ];
}
