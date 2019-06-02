let
  pkgs = import ./. {} ;
  # A simple derivation that just creates a file with the names of all of its inputs.  If built, it will have a runtime dependency on all of the given build inputs.
  pinBuildInputs = name: args: pkgs.runCommand name (args // {
    buildCommand = ''
      mkdir "$out"
      echo "$propagatedBuildInputs $buildInputs $nativeBuildInputs $propagatedNativeBuildInputs" > "$out/deps"
    '';
    inherit name;
  }) "";
in

pinBuildInputs "binary-cache" {
  buildInputs = with pkgs.myHaskellPackages; [
    pkgs.awscli
    pkgs.yamlUtils
    pkgs.cabal-install
    pkgs.myGlibcHaskellPackages.dbmigrations-postgresql
    ghcid
    hpack
    hoogle
    stylish-haskell
  ];
}
