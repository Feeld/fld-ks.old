self: super:
if !super.stdenv.hostPlatform.isMusl then {} else
{
  # Build tools which we don't need to build with musl
  inherit (self.pkgsGlibc)
    python pythonPackages
    python2 python2Packages
    python3 python3Packages
    cabal2nix
    ;
  iproute = super.iproute.overrideAttrs (old: {
    # We need this fix which hasn't yet made it to nixpkgs to build with musl
    patches = old.patches or [] ++ [
      (super.fetchurl {
        url = "https://git.kernel.org/pub/scm/network/iproute2/iproute2.git/patch/?id=28747146622a49c3e7b5c5b36dc02c6a64124770";
        sha256 = "09pi4mdazks1r02wsw7qap43alh4nx0fnnh72g5b9qn410jrzcp3";
      })
      ];
    });

  xorg = super.xorg // {
    inherit (self.pkgsGlibc.xorg)
      lndir;
    };
}
