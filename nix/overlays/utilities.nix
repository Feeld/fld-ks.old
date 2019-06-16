let
  # bash magic so we can set several traps without removin any existing one.
  # Brought to you by https://stackoverflow.com/questions/16115144/save-and-restore-trap-state-easy-way-to-manage-multiple-handlers-for-traps/16115145
  trapMagic = ''
    trap_stack_name() {
      local sig=''${1//[^a-zA-Z0-9]/_}
      echo "__trap_stack_$sig"
    }

    extract_trap() {
      echo ''${@:3:$(($#-3))}
    }

    get_trap() {
      eval echo $(extract_trap `trap -p $1`)
    }

    trap_push() {
      local new_trap=$1
      shift
      local sigs=$*
      for sig in $sigs; do
        local stack_name=`trap_stack_name "$sig"`
        local old_trap=$(get_trap $sig)
        eval "''${stack_name}"'[''${#'"''${stack_name}"'[@]}]=$old_trap'
        trap "''${new_trap}" "$sig"
      done
    }

    trap_prepend() {
      local new_trap=$1
      shift
      local sigs=$*
      for sig in $sigs; do
        if [[ -z $(get_trap $sig) ]]; then
          trap_push "$new_trap" "$sig"
        else
          trap_push "$new_trap ; $(get_trap $sig)" "$sig"
        fi
      done
    }
    '';

in self: super: {

  yamlUtils =
    let
      hsPackages = self.myHaskellPackages // {
        yaml = self.haskell.lib.appendConfigureFlag self.myHaskellPackages.yaml "-f-no-exe";
      };
    in self.pkgsMusl.haskell.lib.justReallyStaticExecutables hsPackages "yaml";

  lib = super.lib //
    {
      # This function provides an ephemeral postgresql instance for development in
      # the shellHook and at build/test time of the package it wraps
      withPostgres = pg: drv:
        let functions = ''
            ${trapMagic}

            function initPG() {
              ${super.lib.optionalString (self.stdenv.hostPlatform.isMusl) "export LOCALE_ARCHIVES=${self.pkgsGlibc.glibcLocales}/lib/locale/locale-archive"}
              ${super.lib.optionalString self.stdenv.isDarwin "export TMPDIR=/tmp"}
              ${super.lib.optionalString (self.stdenv.hostPlatform.isMusl) "export LANG=C.UTF-8"}
              ${super.lib.optionalString (self.stdenv.hostPlatform.isMusl) "export LC_ALL=C.UTF-8"}
              ${super.lib.optionalString (self.stdenv.hostPlatform.isMusl) "export LC_CTYPE=C.UTF-8"}
              export TZ='UTC'
              export PGHOST=$(mktemp -d)
              export PGDATA=$PGHOST/db
              export PGSOCK=$PGHOST/.s.PGSQL.5432
              export PGDATABASE=templatepg
              export DBM_DATABASE="dbname=templatepg host=$PGHOST"
              export PGUSER=templatepg
              # We set these environment variables so postgresql-typed knows how
              # to connect to the database at compile-time to make sure all SQL
              # queries are well typed and well formed
              export TPG_SOCK=$PGSOCK
              export TPG_DB=$PGDATABASE
              export TPG_USER=$PGUSER
              #
              ${pg}/bin/initdb -E UTF8 $PGDATA
              ${pg}/bin/postgres -D $PGDATA -k $PGHOST -c listen_addresses="" &
              export pgpid=$!
              echo -n "Waiting for database to start up..."
              while [[ ! -e $PGSOCK ]]; do sleep 0.1; done
              ${pg}/bin/createuser -h $PGHOST -U $(id -u --name) -s $PGUSER
              ${pg}/bin/createdb -h $PGHOST -O $PGUSER $PGDATABASE

              echo "Initializing database..."
              if [[ -e ./initializeDatabase.sh ]]; then
                PATH=${self.myGlibcHaskellPackages.dbmigrations-postgresql}/bin:$PATH ${self.stdenv.shell} ./initializeDatabase.sh
              fi

              echo "Created database PGDATABASE=$PGDATABASE at PGHOST=$PGHOST."
              echo "Call killPG to stop and delete it. Call initPG to re-create it"
            }
            function killPG() {
              echo "Killing postgres database at $PGHOST"
              kill $pgpid || true
              echo "Waiting for postgres database to die ..."
              while [[ -e $PGSOCK ]]; do sleep 0.1; done
              echo "Postgres is dead, deleting its data dir"
              rm -rf $PGHOST
            }
            function reinitPG {
              killPG && initPG
            }
            # export the functions so they're available in the development nix-shell
            # so the database can be re-created easly
            export -f initPG
            export -f killPG
            export -f reinitPG

            trap_prepend "killPG" EXIT
            '';

        in self.haskell.lib.overrideCabal drv (old: {
          buildDepends = (old.buildDepends or []) ++ [ pg ];
          preBuild = ''
            ${old.preBuild or ""}
            ${functions}
            initPG
          '';
          shellHook = ''
            ${old.shellHook or ""}
            ${functions}
            initPG
            '';
          postInstall = ''
            killPG
            ${old.postInstall or ""}
            '';
        });


      # This function provides an ephemeral redis instance for development in
      # the shellHook and at build/test time of the package it wraps
      withRedis = redis: drv:
        let functions = ''
            ${trapMagic}

            function initRedis() {
              ${super.lib.optionalString super.stdenv.isDarwin "export TMPDIR=/tmp"}
              export REDIS_HOST=$(mktemp -d)
              export REDIS_SOCKET=$REDIS_HOST/redis.sock
              ${redis}/bin/redis-server --unixsocket $REDIS_SOCKET \
                                        --unixsocketperm 700 \
                                        --always-show-logo no \
                                        --port 0 &
              export redispid=$!
              echo -n "Waiting for redis to start up..."
              while [[ ! -e $REDIS_SOCKET ]]; do sleep 0.1; done
              echo "Call killRedis to stop and delete it. Call initRedis to re-create it"
            }
            function killRedis() {
              echo "Killing and deleting redis database at $REDIS_HOST"
              kill $redispid || true
              echo "Waiting for Redis to die.."
              while [[ -e $REDIS_SOCKET ]]; do sleep 0.1; done
              echo "Redis is dead, deleting its data dir"
              rm -rf $REDIS_HOST
            }
            function reinitRedis {
              killRedis && initRedis
            }
            # export the functions so they're available in the development nix-shell
            # so the database can be re-created easly
            export -f initRedis
            export -f killRedis
            export -f reinitRedis

            trap_prepend "killRedis" EXIT
            '';

        in self.haskell.lib.overrideCabal drv (old: {
          buildDepends = (old.buildDepends or []) ++ [ redis ];
          preBuild = ''
            ${old.preBuild or ""}
            ${functions}
            initRedis
          '';
          shellHook = ''
            ${old.shellHook or ""}
            ${functions}
            initRedis
            '';
          postInstall = ''
            killRedis
            ${old.postInstall or ""}
            '';
        });

      # This function filters out stuff we don't want to consider part of the source
      # when building with nix. Any change in one of these files would cause a
      # re-build otherwise
      cleanSource =
        let
          fldSourceFilter = name: type: let baseName = baseNameOf (toString name); in ! (
            # Filter out Subversion and CVS directories.
            (type == "directory" &&
              ( baseName == ".git" ||
                baseName == ".circleci" ||
                baseName == ".nix-cache" ||
                baseName == ".cache" ||
                baseName == "deploy" ||
                baseName == "kubernetes" ||
                baseName == "secrets" ||
                baseName == "nix" ||
                baseName == "dist" ||
                baseName == "dist-newstyle"
              )
            ) ||
            # Filter out editor backup / swap files.
            self.lib.hasSuffix "~" baseName ||
            builtins.match "^\\.sw[a-z]$" baseName != null ||
            builtins.match "^\\..*\\.sw[a-z]$" baseName != null ||

            # filter out .ghc.environment
            builtins.match "^\\.ghc.environment.*" baseName != null ||

            # Filter out nix-build result symlinks
            (type == "symlink" && self.lib.hasPrefix "result" baseName) ||

            # Filter other random crap we have lying around for development
            # which we don't need to properly build
            (baseName == "develop.sh") ||
            (baseName == "Setup") ||
            (baseName == "Setup.o") ||
            (baseName == "Setup.hi") ||
            (baseName == ".bash_history") ||
            (baseName == "README.md")
          );
        in builtins.filterSource fldSourceFilter;

      # This function compresses every executable the original derivation left in
      # $out/bin with upx
      upxExecutables = drv:
        self.stdenv.mkDerivation {
          name = "${drv.name}-upx";
          nativeBuildInputs = [ self.upx ];
          phases = ["installPhase"];
          passthru = {
            uncompressed = drv;
          };
          installPhase = ''
            mkdir -p $out/bin
            for f in ${drv}/bin/*; do
              cp $f $out/bin
              chmod u+wx $out/bin/$(basename $f)
              upx -9 $out/bin/$(basename $f)
            done
          '';
        };
    };

  haskell = super.haskell // {
    lib = super.haskell.lib // {
      justReallyStaticExecutables = pkgSet: name:
        let
          drv = pkgSet.${name};
          inherit (self.haskell.lib)
            appendConfigureFlags justStaticExecutables
            disableLibraryProfiling overrideCabal dontHaddock;
          # This version of Cabal supports --enable-executable-static.
          Cabal_head =
            let
              src =
                self.fetchFromGitHub {
                  owner = "haskell";
                  repo = "cabal";
                  rev = "7b8e6e5aa7d851d4caa132140673b17fe85970e3";
                  sha256 = "0p8mk7ry31ykk9q68mvif6yva36cyamfbhqffsalrzxkqbv9j20m";
                };
              drv = (pkgSet.callCabal2nix "Cabal" "${src}/Cabal" {}).overrideDerivation (old: {
                # Fixes the parsing of pkgId which now contains more tha one space
                # between "id:" and the package id
                installPhase = ''
                  ./Setup copy
                  local packageConfDir="$out/lib/${pkgSet.ghc.name}/package.conf.d"
                  local packageConfFile="$packageConfDir/${old.pname}-${old.version}.conf"
                  mkdir -p "$packageConfDir"
                  ./Setup register --gen-pkg-config=$packageConfFile
                  if [ -d "$packageConfFile" ]; then
                    mv "$packageConfFile/"* "$packageConfDir"
                    rmdir "$packageConfFile"
                  fi
                  find $packageConfDir
                  for packageConfFile in "$packageConfDir/"*; do
                    local pkgId=$( ${self.gnused}/bin/sed -n -e 's|^id: *||p' $packageConfFile )
                    mv $packageConfFile $packageConfDir/$pkgId.conf
                  done

                  # delete confdir if there are no libraries
                  find $packageConfDir -maxdepth 0 -empty -delete;
                  '';
              });

            in disableLibraryProfiling (dontHaddock drv);
          enableStatic = drv: drv.overrideAttrs (old: {
            dontDisableStatic = true;
            configureFlags = (old.configureFlags or []) ++ [
              "--enable-static"
              ];
            });
        in overrideCabal (justStaticExecutables drv) (old: {
          enableSharedLibraries = true;
          enableSharedExecutables = false;
          setupHaskellDepends = (old.setupHaskellDepends or []) ++ [ Cabal_head ];
          passthru = (old.passthru or {}) // { inherit Cabal_head; };
          configureFlags = (old.configureFlags or []) ++ [
            "--enable-executable-static"
            # Pass lib location of common libraries with static .a library
            # available
            "--extra-lib-dirs=${self.pkgsMusl.gmp6.override { withStatic = true;}}/lib"
            "--extra-lib-dirs=${self.pkgsMusl.zlib.static}/lib"
            "--extra-lib-dirs=${self.pkgsMusl.bzip2.override { linkStatic=true;}}/lib"
            "--extra-lib-dirs=${self.pkgsMusl.ncurses.override { enableStatic=true;}}/lib"
            "--extra-lib-dirs=${(enableStatic self.pkgsMusl.xz).out}/lib"
            "--extra-lib-dirs=${enableStatic self.pkgsMusl.libffi}/lib"
          ];
      });
    };
  };

  nix-cache-s3 = self.stdenv.mkDerivation {
    name = "nix-cache-s3";
    phases = ["installPhase" "fixupPhase"];
    installPhase = ''
      mkdir -p $out/bin
      cp ${../scripts/cache} $out/bin/nix-cache-s3
    '';
  };
}
