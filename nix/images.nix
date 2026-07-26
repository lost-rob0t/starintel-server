{ pkgs, starServer }:

let
  couchdbVersion = "3.5.2";
  clouseauVersion = "3.3.0";

  couchdbBase = pkgs.dockerTools.pullImage {
    imageName = "couchdb";
    imageDigest = "sha256:09c35f535e4df3c4dc789aca8ce0b4cfe6e56619edb873a041d35dec68df9e18";
    hash = "sha256-9ZAkDiaUE1xBwjn48KUkQqxDjEQyuhv/ghJzyVEXR7U=";
    finalImageName = "couchdb";
    finalImageTag = couchdbVersion;
  };

  clouseauDist = pkgs.stdenvNoCC.mkDerivation {
    pname = "clouseau-dist";
    version = clouseauVersion;

    src = pkgs.fetchurl {
      url = "https://github.com/cloudant-labs/clouseau/releases/download/${clouseauVersion}/clouseau-${clouseauVersion}-dist.zip";
      hash = "sha256-M3AP4qygVajhcFJluSw7+Kad1e9qqWwIymH/4DMVKK8=";
    };

    nativeBuildInputs = [ pkgs.unzip ];
    dontUnpack = true;

    installPhase = ''
      runHook preInstall
      unzip -q "$src"
      install -Dm644 clouseau_2.13.16_${clouseauVersion}.jar \
        "$out/share/clouseau/clouseau.jar"
      runHook postInstall
    '';
  };

  clouseauRoot = pkgs.runCommand "clouseau-image-root" { } ''
    install -Dm755 ${../docker/clouseau-entrypoint.sh} \
      "$out/bin/clouseau-entrypoint"
    install -Dm644 ${../docker/clouseau.conf} \
      "$out/etc/clouseau/app.conf"
  '';

  clouseauImageRoot = pkgs.buildEnv {
    name = "clouseau-image-environment";
    paths = [
      clouseauDist
      clouseauRoot
      pkgs.beamMinimalPackages.erlang
      pkgs.busybox
      pkgs.jdk21_headless
      pkgs.su-exec
    ];
    pathsToLink = [ "/bin" "/etc" "/share" ];
  };

  serverRoot = pkgs.runCommand "star-server-image-root" { } ''
    install -Dm755 ${../docker/star-server-entrypoint.sh} \
      "$out/bin/star-server-entrypoint"
    install -Dm644 ${../docker/star-server-init.lisp} \
      "$out/etc/starintel/init.lisp"
  '';

  serverImageRoot = pkgs.buildEnv {
    name = "star-server-image-environment";
    paths = [
      starServer
      serverRoot
      pkgs.busybox
      pkgs.cacert
      pkgs.curl
      pkgs.su-exec
    ];
    pathsToLink = [ "/bin" "/etc" ];
  };

  couchdbImage = pkgs.dockerTools.buildImage {
    name = "starintel/couchdb";
    tag = couchdbVersion;
    fromImage = couchdbBase;

    extraCommands = ''
      install -Dm755 ${../docker/couchdb-entrypoint.sh} \
        usr/local/bin/starintel-couchdb-entrypoint
      install -Dm644 ${../docker/couchdb-search.ini} \
        opt/couchdb/etc/local.d/starintel-search.ini
    '';

    config = {
      Entrypoint = [ "/usr/local/bin/starintel-couchdb-entrypoint" ];
      Cmd = [ "couchdb" ];
      ExposedPorts = {
        "4369/tcp" = { };
        "5984/tcp" = { };
        "9100/tcp" = { };
      };
      Volumes."/opt/couchdb/data" = { };
      WorkingDir = "/opt/couchdb";
    };
  };

  clouseauImage = pkgs.dockerTools.buildLayeredImage {
    name = "starintel/clouseau";
    tag = clouseauVersion;
    contents = clouseauImageRoot;

    extraCommands = ''
      mkdir -p tmp var/lib/clouseau
      chmod 1777 tmp
      chmod 0770 var/lib/clouseau
    '';

    fakeRootCommands = ''
      chown 65532:65532 var/lib/clouseau
    '';

    config = {
      Entrypoint = [ "/bin/clouseau-entrypoint" ];
      Env = [
        "CLOUSEAU_CONFIG=/etc/clouseau/app.conf"
        "CLOUSEAU_COOKIE_FILE=/run/secrets/erlang_cookie"
        "HOME=/tmp"
      ];
      ExposedPorts."4369/tcp" = { };
      Volumes."/var/lib/clouseau" = { };
      WorkingDir = "/var/lib/clouseau";
    };
  };

  serverImage = pkgs.dockerTools.buildLayeredImage {
    name = "starintel/server";
    tag = "0.1.0";
    contents = serverImageRoot;

    extraCommands = ''
      mkdir -p tmp
      chmod 1777 tmp
    '';

    config = {
      Entrypoint = [ "/bin/star-server-entrypoint" ];
      Env = [
        "HOME=/tmp"
        "HTTP_API_LISTEN_ADDRESS=0.0.0.0"
        "SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt"
        "XDG_CACHE_HOME=/tmp/.cache"
        "XDG_CONFIG_HOME=/tmp/.config"
      ];
      ExposedPorts."5000/tcp" = { };
      WorkingDir = "/tmp";
    };
  };

  allImages = pkgs.dockerTools.mergeImages [
    serverImage
    couchdbImage
    clouseauImage
  ];

  loadImages = pkgs.writeShellApplication {
    name = "load-starintel-images";
    runtimeInputs = [ pkgs.docker-client ];
    text = ''
      docker load < ${allImages}
    '';
  };
in
{
  inherit
    allImages
    clouseauImage
    couchdbImage
    loadImages
    serverImage
    ;
}
