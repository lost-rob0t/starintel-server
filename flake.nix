{
  description = "Starintel API server";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    star-cl = {
      url = "github:lost-rob0t/star-cl";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, star-cl }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      runtimeLibs = with pkgs; [
        lmdb openssl rabbitmq-c libffi sqlite
      ];

      starintel = star-cl.packages.${system}.starintel;
      cms-ulid  = star-cl.packages.${system}.cms-ulid;

      cl-couch = pkgs.sbcl.buildASDFSystem rec {
        pname = "cl-couch";
        version = "latest";
        src = pkgs.fetchFromGitHub {
          owner = "lost-rob0t";
          repo  = "cl-couch";
          rev   = "a37150193b81fad16d24b42102353fcad9c649ad";
          hash  = "sha256-Niq9klZi9iKOwNb5u7FUalz82FlP5XIGxnnEjN/Jg9A=";
        };
        lispLibs = with pkgs.sbclPackages; [ dexador jsown serapeum ];
      };

      # cl-gserver repo's real ASDF system is "sento".
      # Some of your code (or old deps) may still depend on "cl-gserver".
      # So we generate a tiny ASDF alias system "cl-gserver" -> depends on "sento".
      sentoPkg = pkgs.sbcl.buildASDFSystem rec {
        pname = "sento";
        version = "latest";
        src = pkgs.fetchFromGitHub {
          owner = "mdbergmann";
          repo  = "cl-gserver";
          rev   = "6a510c5b58469e72e6363bd3a6059d80b9a5c320";
          hash  = "sha256-eoFh4AusY1T00jWo4C2ID+0uJoTDIhdNf67WGVrSKPA=";
        };

        postPatch = ''
          cat > cl-gserver.asd <<'EOF'
;;;; Compatibility shim: provide ASDF system "cl-gserver" (real system is "sento")
(asdf:defsystem "cl-gserver"
  :description "Compatibility alias for the cl-gserver repository; real system is SENTO."
  :depends-on ("sento")
  :components ())
EOF
        '';

        systems = [ "sento" "cl-gserver" ];

        lispLibs = with pkgs.sbclPackages; [
          alexandria bordeaux-threads lparallel atomics str
          log4cl blackbird cl-speedy-queue binding-arrows timer-wheel
          local-time-duration clack-handler-hunchentoot
        ];
      };

      cl-rabbit = pkgs.sbcl.buildASDFSystem rec {
        pname = "cl-rabbit";
        version = "latest";
        src = pkgs.fetchFromGitHub {
          owner = "lokedhs";
          repo  = "cl-rabbit";
          rev   = "9603204715bb13f09243dc286c5ad4bd51b4fd7b";
          hash  = "sha256-SkbXB6+4SuVg+urQMPEF2WAIZTXVA3mmMnn1jTuGNeA=";
        };
        lispLibs = with pkgs.sbclPackages; [ cffi cffi-grovel cffi-libffi babel cl-ppcre ];
        nativeLibs = with pkgs; [ rabbitmq-c libffi ];
      };

      nhooks = pkgs.sbcl.buildASDFSystem rec {
        pname = "nhooks";
        version = "latest";
        src = pkgs.fetchFromGitHub {
          owner = "atlas-engineer";
          repo  = "nhooks";
          rev   = "3847bc749a6f6eb1103bc21f8ef3b4f6b301e822";
          hash  = "sha256-h7zbXow3uzLxnodPRcqHDtFyQ+wiUNk1wg9+I9VlNMw=";
        };
        lispLibs = with pkgs.sbclPackages; [ serapeum bordeaux-threads closer-mop ];
      };

      lack-middleware-accesslog = pkgs.sbcl.buildASDFSystem rec {
        pname = "lack-middleware-accesslog";
        version = "latest";
        src = pkgs.sbclPackages.lack.src;
        lispLibs = with pkgs.sbclPackages; [ lack local-time ];
        systems = [
          "lack-middleware-accesslog"
          "lack/middleware/accesslog"
        ];
      };

      sbcl' = pkgs.sbcl.withOverrides (self: super: {
        inherit starintel cms-ulid cl-couch cl-rabbit nhooks lack-middleware-accesslog;

        # expose both names to the package set
        sento     = sentoPkg;
        cl-gserver = sentoPkg;
      });

      starintel-gserver = sbcl'.buildASDFSystem rec {
        pname = "starintel-gserver";
        version = "0.1.0";
        src = ./.;

        nativeLibs = runtimeLibs;

        lispLibs = with sbcl'.pkgs; [
          starintel
          cl-couch
          serapeum
          alexandria
          cl-rabbit
          sento
          babel
          uuid
          anypool
          clack
          ningle
          clingon
          slynk
          nhooks
          lparallel
          cl-stream
          cl-ppcre
          cms-ulid
          bordeaux-threads
          usocket
          cl_plus_ssl
          jsown
          closer-mop
          ironclad
          xmls
          lack
          lack-middleware-accesslog
        ];

        systems = [ "starintel-gserver" ];
        asdFilesToKeep = [ "starintel-gserver.asd" ];
        dontStrip = true;
      };

      starintel-gserver-tests = sbcl'.buildASDFSystem {
        pname = "starintel-gserver-tests";
        version = "0.1.0";
        src = ./.;

        lispLibs = with sbcl'.pkgs; [
          starintel-gserver
          starintel-gserver-client
          star-cli-lib
          star-ui-lib
          star-migrations-lib
          fiveam
          bordeaux-threads
          jsown
          lack
        ];

        systems = [ "starintel-gserver-tests" ];
        asdFilesToKeep = [
          "starintel-gserver-tests.asd"
          "starintel-gserver-integration-tests.asd"
        ];
        dontStrip = true;
      };

      starintel-gserver-integration-tests = sbcl'.buildASDFSystem {
        pname = "starintel-gserver-integration-tests";
        version = "0.1.0";
        src = ./.;

        lispLibs = with sbcl'.pkgs; [
          starintel-gserver-tests
        ];

        systems = [ "starintel-gserver-integration-tests" ];
        dontStrip = true;
      };

      starintel-gserver-client = sbcl'.buildASDFSystem {
        pname = "starintel-gserver-client";
        version = "0.1.0";
        src = ./cli;

        lispLibs = with sbcl'.pkgs; [
          starintel
          jsown
          uuid
          dexador
          quri
        ];

        systems = [ "starintel-gserver-client" ];
        dontStrip = true;
      };

      star-cli-lib = sbcl'.buildASDFSystem {
        pname = "star-cli";
        version = "0.1.0";
        src = ./cli;

        nativeLibs = runtimeLibs;

        lispLibs = with sbcl'.pkgs; [
          starintel-gserver-client
          clingon
          dexador
          jsown
          quri
        ];

        systems = [ "star-cli" ];
        dontStrip = true;
      };

      star-ui-lib = sbcl'.buildASDFSystem {
        pname = "star-ui";
        version = "0.1.0";
        src = ./ui;

        lispLibs = with sbcl'.pkgs; [
          ningle
          clack
          clack-handler-hunchentoot
          lack
          lack-middleware-accesslog
          jsown
          babel
          alexandria
          dexador
          log4cl
        ];

        systems = [ "star-ui" ];
        dontStrip = true;
      };

      star-migrations-lib = sbcl'.buildASDFSystem {
        pname = "star-migrations";
        version = "0.1.0";
        src = ./source/migrations;

        lispLibs = with sbcl'.pkgs; [
          lparallel
          dexador
          cl-couch
        ];

        systems = [ "star-migrations" ];
        dontStrip = true;
      };

      sbcl-wrapped = sbcl'.withPackages (ps: with ps; [ starintel-gserver ]);
      sbcl-test-wrapped = sbcl'.withPackages (ps: with ps; [ starintel-gserver-tests ]);
      sbcl-integration-test-wrapped = sbcl'.withPackages
        (ps: with ps; [ starintel-gserver-integration-tests ]);
      sbcl-cli-wrapped = sbcl'.withPackages (ps: with ps; [ star-cli-lib ]);

      make-test-runner = name: wrapped: asdfSystem: extraRuntimeInputs: prelude:
        pkgs.writeShellApplication {
          inherit name;
          runtimeInputs = runtimeLibs ++ extraRuntimeInputs;
          text = ''
            test_home="$(mktemp -d)"
            export HOME="$test_home"
            export XDG_CACHE_HOME="$HOME/.cache"
            export TMPDIR="/tmp"
            export TMP="/tmp"
            export TEMP="/tmp"
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath runtimeLibs}"
            export STARINTEL_SOURCE_ROOT="${./.}"

            ${prelude}

            ${wrapped}/bin/sbcl --non-interactive --no-userinit --no-sysinit \
              --eval "(require :asdf)" \
              --eval "(handler-case
                        (progn
                          (asdf:test-system :${asdfSystem})
                          (uiop:quit 0))
                        (error (condition)
                          (format *error-output*
                                  \"~&Test system ${asdfSystem} failed: ~a~%\"
                                  condition)
                          (uiop:quit 1)))"
          '';
        };

      unit-test-runner = make-test-runner
        "star-unit-tests"
        sbcl-test-wrapped
        "starintel-gserver-tests"
        []
        "";

      integration-test-runner = make-test-runner
        "star-integration-tests"
        sbcl-integration-test-wrapped
        "starintel-gserver-integration-tests"
        [ pkgs.valkey pkgs.openssl pkgs.python3 ]
        ''
          service_root="$(mktemp -d)"
          plain_pid=""
          tls_pid=""
          stop_valkey() {
            local pid="$1"
            if [ -z "$pid" ] || ! kill -0 "$pid" 2>/dev/null; then return; fi
            kill "$pid" 2>/dev/null || true
            for _ in $(seq 1 50); do
              if ! kill -0 "$pid" 2>/dev/null; then
                wait "$pid" 2>/dev/null || true
                return
              fi
              sleep 0.02
            done
            kill -KILL "$pid" 2>/dev/null || true
            wait "$pid" 2>/dev/null || true
          }
          cleanup_valkey() {
            stop_valkey "$tls_pid"
            stop_valkey "$plain_pid"
            rm -rf "$service_root"
          }
          trap cleanup_valkey EXIT

          mapfile -t valkey_ports < <(python - <<'PY'
import socket
for _ in range(3):
    sock = socket.socket()
    sock.bind(("127.0.0.1", 0))
    print(sock.getsockname()[1])
    sock.close()
PY
          )
          plain_port="''${valkey_ports[0]}"
          tls_port="''${valkey_ports[1]}"
          unused_port="''${valkey_ports[2]}"
          password_file="$service_root/password"
          bad_password_file="$service_root/bad-password"
          printf '%s\n' 'valkey-integration-secret' > "$password_file"
          printf '%s\n' 'wrong-valkey-password' > "$bad_password_file"
          chmod 600 "$password_file" "$bad_password_file"

          mkdir -p "$service_root/plain" "$service_root/tls"
          password_hash="$(printf %s "$(cat "$password_file")" | sha256sum | cut -d ' ' -f 1)"
          printf 'user default on #%s ~* &* +@all\n' "$password_hash" \
            > "$service_root/plain/users.acl"
          printf 'user default on #%s ~* &* +@all\n' "$password_hash" \
            > "$service_root/tls/users.acl"
          unset password_hash
          chmod 600 "$service_root/plain/users.acl" "$service_root/tls/users.acl"
          openssl req -x509 -newkey rsa:2048 -nodes \
            -keyout "$service_root/ca.key" -out "$service_root/ca.crt" \
            -days 1 -subj '/CN=StarIntel Valkey Test CA' >/dev/null 2>&1
          openssl req -newkey rsa:2048 -nodes \
            -keyout "$service_root/server.key" -out "$service_root/server.csr" \
            -subj '/CN=localhost' \
            -addext 'subjectAltName=DNS:localhost,IP:127.0.0.1' >/dev/null 2>&1
          openssl x509 -req -in "$service_root/server.csr" \
            -CA "$service_root/ca.crt" -CAkey "$service_root/ca.key" \
            -CAcreateserial -out "$service_root/server.crt" -days 1 \
            -copy_extensions copy >/dev/null 2>&1
          openssl req -x509 -newkey rsa:2048 -nodes \
            -keyout "$service_root/wrong-ca.key" \
            -out "$service_root/wrong-ca.crt" -days 1 \
            -subj '/CN=Wrong Test CA' >/dev/null 2>&1

          valkey-server \
            --bind 127.0.0.1 --protected-mode yes --port "$plain_port" \
            --aclfile "$service_root/plain/users.acl" --appendonly yes \
            --dir "$service_root/plain" --logfile "$service_root/plain.log" \
            </dev/null >"$service_root/plain.stdout.log" 2>&1 &
          plain_pid="$!"
          valkey-server \
            --bind 127.0.0.1 --protected-mode yes --port 0 \
            --tls-port "$tls_port" --tls-cert-file "$service_root/server.crt" \
            --tls-key-file "$service_root/server.key" \
            --tls-ca-cert-file "$service_root/ca.crt" --tls-auth-clients no \
            --aclfile "$service_root/tls/users.acl" --appendonly yes \
            --dir "$service_root/tls" --logfile "$service_root/tls.log" \
            </dev/null >"$service_root/tls.stdout.log" 2>&1 &
          tls_pid="$!"

          for _ in $(seq 1 100); do
            if VALKEYCLI_AUTH='valkey-integration-secret' \
              valkey-cli -h 127.0.0.1 -p "$plain_port" ping \
              2>/dev/null | grep -q PONG; then
              break
            fi
            sleep 0.05
          done
          VALKEYCLI_AUTH='valkey-integration-secret' \
            valkey-cli -h 127.0.0.1 -p "$plain_port" ping \
            2>/dev/null | grep -q PONG
          for _ in $(seq 1 100); do
            if VALKEYCLI_AUTH='valkey-integration-secret' \
              valkey-cli --tls --cacert "$service_root/ca.crt" \
              -h localhost -p "$tls_port" ping 2>/dev/null | grep -q PONG; then
              break
            fi
            sleep 0.05
          done
          VALKEYCLI_AUTH='valkey-integration-secret' \
            valkey-cli --tls --cacert "$service_root/ca.crt" \
            -h localhost -p "$tls_port" ping 2>/dev/null | grep -q PONG

          export STAR_TEST_VALKEY_HOST=localhost
          export STAR_TEST_VALKEY_PORT="$plain_port"
          export STAR_TEST_VALKEY_TLS_PORT="$tls_port"
          export STAR_TEST_VALKEY_UNUSED_PORT="$unused_port"
          export STAR_TEST_VALKEY_PASSWORD_FILE="$password_file"
          export STAR_TEST_VALKEY_BAD_PASSWORD_FILE="$bad_password_file"
          export STAR_TEST_VALKEY_CA_FILE="$service_root/ca.crt"
          export STAR_TEST_VALKEY_WRONG_CA_FILE="$service_root/wrong-ca.crt"
          export VALKEY_HOST=localhost
          export VALKEY_PORT="$plain_port"
          export VALKEY_PASSWORD_FILE="$password_file"
        '';

      star-server-bin = pkgs.stdenv.mkDerivation {
        pname = "star-server";
        version = "0.1.0";

        dontUnpack = true;
        dontStrip = true;
        nativeBuildInputs = [ pkgs.makeWrapper ];

        buildPhase = ''
          ${sbcl-wrapped}/bin/sbcl --non-interactive --no-userinit --no-sysinit \
            --eval "(require :asdf)" \
            --eval "(asdf:load-system :starintel-gserver)" \
            --eval "(sb-ext:save-lisp-and-die \"star-server\" :toplevel 'star::main :executable t :compression t)"
        '';

        installPhase = ''
          mkdir -p $out/bin
          cp star-server $out/bin/

          wrapProgram $out/bin/star-server \
            --set TMPDIR /tmp \
            --set TMP /tmp \
            --set TEMP /tmp \
            --run 'export HOME=''${HOME:-$(mktemp -d)}' \
            --run 'export XDG_CONFIG_HOME=''${XDG_CONFIG_HOME:-$HOME/.config}' \
            --run 'export XDG_CACHE_HOME=''${XDG_CACHE_HOME:-$HOME/.cache}' \
            --run 'mkdir -p "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME"' \
            --prefix LD_LIBRARY_PATH : "${pkgs.lib.makeLibraryPath runtimeLibs}" \
        '';
      };

      containerImages = import ./nix/images.nix {
        inherit pkgs;
        starServer = star-server-bin;
      };

    in {
      packages.${system} = {
        default = star-server-bin;

        star-unit-tests = unit-test-runner;
        star-smoke = unit-test-runner;
        star-integration-tests = integration-test-runner;
        star-server-image = containerImages.serverImage;
        couchdb-image = containerImages.couchdbImage;
        clouseau-image = containerImages.clouseauImage;
        rabbitmq-image = containerImages.rabbitmqImage;
        valkey-image = containerImages.valkeyImage;
        container-images = containerImages.allImages;
        load-images = containerImages.loadImages;

        star-cli = pkgs.stdenv.mkDerivation {
          pname = "star-cli";
          version = "0.1.0";

          dontUnpack = true;
          dontStrip = true;
          nativeBuildInputs = [ pkgs.makeWrapper ];

          buildPhase = ''
            ${sbcl-cli-wrapped}/bin/sbcl --non-interactive --no-userinit --no-sysinit \
              --eval "(require :asdf)" \
              --eval "(asdf:load-system :star-cli)" \
              --eval "(sb-ext:save-lisp-and-die \"star-cli\" :toplevel 'star-cli:main :executable t :compression t)"
          '';

          installPhase = ''
            mkdir -p $out/bin
            cp star-cli $out/bin/

            wrapProgram $out/bin/star-cli \
              --set TMPDIR /tmp \
              --set TMP /tmp \
              --set TEMP /tmp \
              --prefix LD_LIBRARY_PATH : "${pkgs.lib.makeLibraryPath runtimeLibs}"
          '';
        };

        starintel-gserver = starintel-gserver;
        starintel-gserver-tests = starintel-gserver-tests;
        starintel-gserver-integration-tests =
          starintel-gserver-integration-tests;
        starintel-gserver-client = starintel-gserver-client;
        star-cli-lib = star-cli-lib;
        star-ui-lib = star-ui-lib;
        star-migrations-lib = star-migrations-lib;
      };

      apps.${system}.load-images = {
        type = "app";
        program = "${containerImages.loadImages}/bin/load-starintel-images";
      };

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [ sbcl-wrapped pkg-config ] ++ runtimeLibs;

        shellHook = ''
          export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath runtimeLibs}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
          unset LD_PRELOAD
          export TMPDIR="/tmp"
          export TMP="/tmp"
          export TEMP="/tmp"
          export XDG_CONFIG_HOME="''${XDG_CONFIG_HOME:-$HOME/.config}"
          export XDG_CACHE_HOME="''${XDG_CACHE_HOME:-$HOME/.cache}"
          mkdir -p "$XDG_CONFIG_HOME" "$XDG_CACHE_HOME"
          echo "Starintel Gserver dev environment ready"
        '';
      };
    };
}
