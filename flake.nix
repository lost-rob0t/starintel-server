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

      tools = with pkgs; [
        nmap
        httpx
      ];

      starintel = star-cl.packages.${system}.starintel;
      cms-ulid  = star-cl.packages.${system}.cms-ulid;

      cl-couch = pkgs.sbcl.buildASDFSystem rec {
        pname = "cl-couch";
        version = "latest";
        src = pkgs.fetchFromGitHub {
          owner = "lost-rob0t";
          repo  = "cl-couch";
          rev   = "9085da82d3b26c82a44be1d830b0f9bf374b5368";
          hash  = "sha256-jtHNWK7ekKqoxMp+VJvLU4hF74fPnddrx/yeXvCPZVM=";
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
        buildInputs = tools;

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
          xmls
          lack
          lack-middleware-accesslog
        ];

        systems = [ "starintel-gserver" ];
        asdFilesToKeep = [ "starintel-gserver.asd" "starintel-gserver-tests.asd" ];
        dontStrip = true;
      };

      starintel-gserver-tests = sbcl'.buildASDFSystem {
        pname = "starintel-gserver-tests";
        version = "0.1.0";
        src = ./.;

        lispLibs = with sbcl'.pkgs; [
          starintel-gserver
          fiveam
          bordeaux-threads
          jsown
          lack
        ];

        systems = [ "starintel-gserver-tests" ];
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
          cl-csv
          data-table
          cl-csv-data-table
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

      sbcl-wrapped = sbcl'.withPackages (ps: with ps; [ starintel-gserver ]);
      sbcl-test-wrapped = sbcl'.withPackages (ps: with ps; [ starintel-gserver-tests ]);
      sbcl-cli-wrapped = sbcl'.withPackages (ps: with ps; [ star-cli-lib ]);

    in {
      packages.${system} = {
        default = pkgs.stdenv.mkDerivation {
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
              --prefix PATH : "${pkgs.lib.makeBinPath tools}"
          '';
        };

        star-smoke = pkgs.writeScriptBin "star-smoke" ''
          #!/usr/bin/env bash
          set -e
          export HOME="$(mktemp -d)"
          export XDG_CACHE_HOME="$HOME/.cache"
          export TMPDIR="/tmp"
          export TMP="/tmp"
          export TEMP="/tmp"
          export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath runtimeLibs}"

          echo "=========================================="
          echo "  StarIntel Gserver Smoke Tests"
          echo "=========================================="
          echo ""

          ${sbcl-test-wrapped}/bin/sbcl --non-interactive --no-userinit --no-sysinit \
            --eval "(require :asdf)" \
            --eval "(asdf:load-system :starintel-gserver-tests)" \
            --eval "(in-package :star-server-tests)" \
            --eval "(handler-case
                      (progn
                        (run-all-gserver-tests)
                        (format t \"~%~%✓ Smoke tests completed~%\")
                        (uiop:quit 0))
                      (error (e)
                        (format t \"~%~%✗ Smoke tests failed: ~a~%\" e)
                        (uiop:quit 1)))"
        '';

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
        starintel-gserver-client = starintel-gserver-client;
        star-cli-lib = star-cli-lib;
      };

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [ sbcl-wrapped pkg-config ] ++ runtimeLibs ++ tools;

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



