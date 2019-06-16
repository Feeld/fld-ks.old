self: super: {
  kubenix =
    let
      src = self.fetchFromGitHub
        { owner = "xtruder";
          repo = "kubenix";
          rev = "db5ee88274abfbc98ff98046bb6a96ee2f9c1e95";
          sha256 = "0yzhhz4p6g2xy96vfdzp47x24y4x8h9z5d9v2mqcpfax7w0czy4b";
        };
    in self.callPackage src {};

  # We need gnupg for git-crypt in the docker image. We strip it down of
  # unneeded functionality to reduce image size
  gnupg = super.gnupg.override {
    guiSupport = false;
    pinentry = null;
    adns = null;
    gnutls = null;
    libusb = null;
    openldap = null;
    readline = null;
  };

  # Strip it down of unneeded functionality to reduce image size
  skopeo = (super.skopeo.overrideAttrs(old: {
    outputs = ["bin" "out"];
    buildPhase = ''
      go build \
        -tags "containers_image_ostree_stub exclude_graphdriver_devicemapper exclude_graphdriver_btrfs containers_image_openpgp" \
        -o skopeo ./go/src/${super.skopeo.goPackagePath}/cmd/skopeo
      install -d -m 755 $bin/bin
      install -D -m 755 skopeo $bin/bin/skopeo
      install -d -m 755 $bin/etc
      install -D -m 644 ${super.skopeo.src}/default-policy.json $bin/etc/containers/policy.json
      '';
  })).override { stdenv = self.stdenv // { isLinux=false;};};
}
