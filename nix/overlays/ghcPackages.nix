# This overlay extends the nixpkgs' haskell package set with our stuff
pkgs: self: super:
with pkgs.haskell.lib;
{
  dbmigrations =
    let
      src = pkgs.fetchFromGitHub
        { owner = "jtdaugherty";
          repo = "dbmigrations";
          rev = "80336a736ac394a2d38c65661b249b2fae142b64";
          sha256 = "0v5vq1yjwdgc90bz05hs0i6rqzx1zghm4r1c3b8s6civ460jx1kc";
        };
    in dontCheck (self.callCabal2nix "dbmigrations" src {});
  dbmigrations-postgresql = dontCheck (self.callHackage "dbmigrations-postgresql" "2.0.0" {});

  # This is used by ekg server. We don't need openssl support (it's used for
  # TLS/SSL and nginx will do TLS termination).
  # Supporting it adds weight to the docker image and incurs in a runtime dep
  # (even though the executable is linked statically) to plugin dynamic
  # libraries. Disabling dso's in openssl requires a patch the the makefile so
  # disabling it altogether is cleaner.
  snap-server = (appendConfigureFlag super.snap-server "-f-openssl").override {
    HsOpenSSL = null;
    openssl-streams = null;
  };


  # We use the un-released version in Github instead of the one at Hackage
  # because we want this commit https://github.com/dylex/postgresql-typed/commit/964c7ec8dfb781a1607ef4a230ebfda39fdd2295
  # to avoid warnings/errors with incomplete-uni-patterns.
  # Also to disable HDBC support which we don't need
  postgresql-typed =
    let
      src = pkgs.fetchFromGitHub
        { owner = "Dylex";
          repo = "postgresql-typed";
          rev = "e0903ae6bdb397e9b2ebffe4a7fa1d5ae11f1141";
          sha256 = "1jwyfd53654a9c9vl8g1ha13vfmzkpd9jibx3igwqgr0a4nkpjzs";
        };
      drv = self.callCabal2nix "postgresql-typed" src { HDBC=null; };
    in pkgs.lib.withPostgres pkgs.pkgsGlibc.postgresql
       (appendConfigureFlag drv "-f-hdbc");

  servant-sns-webhook =
    let
      src = pkgs.fetchFromGitHub
        { owner = "Feeld";
          repo = "servant-sns-webhook";
          rev = "ff2d50279a3c3ae9c11e3125e89aab6c1e45bf91";
          sha256 = "0s3illb2c3p922sb7a2yy7ysf93ppny01d0vjk4i8skidpg3a9sr";
        };
    in doJailbreak (self.callCabal2nix "servant-sns-webhook" src {});

  ses-sns-notification =
    let
      src = pkgs.fetchFromGitHub
        { owner = "Feeld";
          repo = "ses-sns-notification";
          rev = "499f3805c2fb3e434d832a6e75ab85fda812e90b";
          sha256 = "1mw26f50s2hzfyc5a18yfvfzckkk85g9pmpjxxxk3qp6v9p2xx99";
        };
    in self.callCabal2nix "ses-sns-notification" src {};

  s3-sns-notification =
    let
      src = pkgs.fetchFromGitHub
        { owner = "Feeld";
          repo = "s3-sns-notification";
          rev = "587514f2677fceae66a71b0a9931acd99fd57352";
          sha256 = "1jy5bqaq53qy321q3j038fjcxyz18mcf741h944hxnw6lwppmrpw";
        };
    in self.callCabal2nix "s3-sns-notification" src {};

  fb =
    let
      src = pkgs.fetchFromGitHub
        { owner = "psibi";
          repo = "fb";
          rev = "f80eca8b1183b58fa704d667a2d414d8d34c0036";
          sha256 = "1f917hyx1g8v2jil67xvjqkw9w35ymrbywk8pc6d7izw71r3bbkr";
        };
    in dontCheck( self.callCabal2nix "fb" src {});

  fld-jwt =
    self.callCabal2nix "fld-jwt" (pkgs.lib.cleanSource ../../fld-jwt) {};

  fld-pg-typed =
    self.callCabal2nix "fld-pg-typed" (pkgs.lib.cleanSource ../../fld-pg-typed) {};

  fld-prelude =
    self.callCabal2nix "fld-prelude" (pkgs.lib.cleanSource ../../fld-prelude) {};

  # This hack provides a configured hoogle instance inside nix-shell
  ghcWithPackages = super.ghcWithHoogle;
}
