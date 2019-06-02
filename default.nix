{ pkgsPath ? null
, compiler ? "ghc864"
, fullyStatic ? true
, overlay ? (_:_:{})
, haskellOverlay ? (_:_:_:{})
}:
let
  # We pin the nixpkgs version here to ensure build reproducibility
  pinnedNixpkgs =
    import ./fetch-nixpkgs.nix
      { # Latest HEAD of the master branch as of 2019-03-31
        rev = "c637aeff19b57b620a27029188060c76c6022dc";
        # This sha256 can be obtained with:
        # `$ nix-prefetch-url https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz`
        sha256 = "189fqxp7v72nak6y8qnqw8vzmv51wmbz4xj081kd2jf7d4icjjvz";
        # This one with:
        # `$ nix-prefetch-url --unpack https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz`
        outputSha256 = "08vij68df530xfnid700my1f9pj0k8iyb1zzplv2yidls204k70b";
      };

  # Use pkgsPath if provided, else the pinned checkout
  realPkgsPath =
    if pkgsPath == null then pinnedNixpkgs else pkgsPath;

  # Musl is not supported on osx so don't attempt to link fully statically there
  linkStatically = fullyStatic && !pkgsGlibc.stdenv.isDarwin;

  # This overlay extends the nixpkgs' package set with our stuff
  nixOverlay = self: super:
    let pkgs = if linkStatically then self.pkgsMusl else pkgsGlibc;
        inherit (pkgs.lib) foldl composeExtensions;
        haskellOverrides = self: old:
          foldl composeExtensions (old.overrides or (_:_:{}))
          [ (import ./overlays/ghcPackages.nix self)
            (haskellOverlay self)
          ];
    in
    {
      inherit pkgsGlibc linkStatically;

      # This is an overlayed haskell package set. See
      # 'nix/overlays/ghcPackages.nix' for the overrides
      myHaskellPackages =
        pkgs.haskell.packages."${compiler}".override (old : {
          overrides = haskellOverrides pkgs old;
          });

      myGlibcHaskellPackages =
        pkgsGlibc.haskell.packages."${compiler}".override (old : {
          overrides = haskellOverrides pkgsGlibc old;
          });

    };

  pkgsGlibc = import realPkgsPath
    {
      overlays =
        [ 
          overlay
          nixOverlay
          (import ./overlays/utilities.nix)
          (import ./overlays/muslPkgs.nix)
          (import ./overlays/docker-static.nix)
          (import ./overlays/kubenix.nix)
        ];
      config.allowUnfree = true;
    };
in pkgsGlibc
