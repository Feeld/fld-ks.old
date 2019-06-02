self: super:
{
  docker-cli-static = self.callPackage ({stdenv, fetchurl}: 
    stdenv.mkDerivation rec {
      version =  "18.09.4";
      pname = "docker-cli-static";
      src = fetchurl {
        url = "https://download.docker.com/linux/static/stable/x86_64/docker-${version}.tgz";
        sha256 = "09y8ql7mb7j5j8cpnx22jvfpx2b5wbcf052ifjrqccjhkl53ibvv";
      };
      phases = [ "unpackPhase" "installPhase" ];
      installPhase = ''
        install -Dm755 ./docker $out/bin/docker
        '';
      }
    ) {};
}
