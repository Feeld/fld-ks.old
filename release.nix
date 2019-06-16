let pkgs = import ./nix {};
in {
  inherit (pkgs.myHaskellPackages)
    fld-jwt
    fld-pg-typed
    fld-prelude
    ;
  }
