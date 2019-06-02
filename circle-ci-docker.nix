{
  # The uid of the dev user.
  # You can create youself a docker image that uses a different uid to avoid
  # messing up with the uids in the mounted /src volume
  devUserUid ? 1000
, pkgs ? import ./. {}
}:
let
  nixConf = pkgs.writeText "nix.conf" ''
    substituters = file:///src/.nix-cache https://feeld-nixcache.s3.amazonaws.com/ https://cache.nixos.org/
    trusted-public-keys = feeld-circleci:Rgj3MboxJuJerz4vLK8srQS4uJ41cesRwq1dF5SPXho= alberto-valverde-1:A+NbXRfx+Uo0tQNZ8hlip+1zru2P32l7/skPDeaZnxU= cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= 
    sandbox = false # Cannot use sandbox inside an unprivileged container
    '';
  suExec = pkgs.pkgsMusl.su-exec.overrideAttrs(o:{CFLAGS="--static";});

  busybox-static = pkgs.pkgsMusl.busybox.override { enableStatic=true; };

  # The entryPoint starts up a nix-daemon in the background and su-execs to the
  # build user. This is to enable builds in multi-user mode so we don't need to
  # run nix commands as root
  entryPoint = pkgs.writeScript "entry-point.sh" ''
    #!${pkgs.stdenv.shell}
    (nix-daemon > /dev/shm/nix-daemon.log 2>&1) &
    export NIX_REMOTE=daemon
    su-exec dev:dev /bin/bash "$@"
    '';
in pkgs.dockerTools.buildImage {
  name = "avalverde/feeld-circleci-nix";
  tag = "latest";
  created = "now";

  contents = with pkgs; [
    awscli
    busybox-static
    bashInteractive
    google-cloud-sdk
    cacert
    curl
    nix
    suExec
    git-crypt
    skopeo
    yamlUtils
    jq
    git
    nix-cache-s3
  ];

  runAsRoot = ''
    #!${pkgs.stdenv.shell}
    ${pkgs.dockerTools.shadowSetup}
    mkdir -p /etc/nix /tmp /var/tmp /usr/bin
    chmod a=rwx,o+t /tmp /var/tmp
    groupadd nixbld -g 30000
    for i in {1..10}; do
      useradd -c "Nix build user $i" \
        -d /var/empty -g nixbld -G nixbld -M -N -r -s "/bin/nologin" nixbld$i || true
    done
    useradd \
      -c "Dev user" \
      -d /src \
      -s "/bin/bash" \
      -u ${toString devUserUid} \
      -U \
      dev
    cp ${nixConf} /etc/nix/nix.conf
    ln -s /bin/env /usr/bin/env
  '';
  config = {
    Cmd = [ entryPoint ];
    WorkingDir = "/src";
    Env = [
      "PATH=/bin"
      "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
    ];
  };
}
