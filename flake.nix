{
  description = "Starintel API server";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs { inherit system; };

      runtimeLibs = with pkgs; [
        lmdb openssl rabbitmq-c libffi sqlite
      ];

      # Define custom lisp dependencies using buildASDFSystem
      star-cl = pkgs.sbcl.buildASDFSystem rec {
        pname = "starintel";
        version = "latest";
        src = let
          repo = pkgs.fetchFromGitHub {
            owner = "lost-rob0t";
            repo = "star-cl";
            rev = "b169e549740144b1ce55a5b40d18af61c72bf55f";
            sha256 = "sha256-fefc6eFhiAgZosovfWFgO6kybzi1jzVCZ1LoK7T+P20=";
          };
        in pkgs.runCommand "star-cl-src" {} ''
          mkdir -p $out
          cp -r ${repo}/src/* $out/
          # Fix type conflicts - remove :type declarations that conflict with :initform values
        '';
        lispLibs = with pkgs.sbclPackages; [
          jsown ironclad local-time cms-ulid
        ];
      };

      cl-couch = pkgs.sbcl.buildASDFSystem rec {
        pname = "cl-couch";
        version = "latest";
        src = pkgs.fetchFromGitHub {
          owner = "lost-rob0t";
          repo = "cl-couch";
          rev = "e3c8e7d1548c7814e49528869c098e5e03ccbe80";
          hash = "sha256-7UYNz+0eHnllHTfM46XgANG202058HcFZpC7NCd5hN4=";
        };
        lispLibs = with pkgs.sbclPackages; [ dexador jsown serapeum ];
      };

      cl-gserver = pkgs.sbcl.buildASDFSystem rec {
        pname = "sento";
        version = "latest";
        src = pkgs.fetchFromGitHub {
          owner = "mdbergmann";
          repo = "cl-gserver";
          rev = "6a510c5b58469e72e6363bd3a6059d80b9a5c320";
          hash = "sha256-eoFh4AusY1T00jWo4C2ID+0uJoTDIhdNf67WGVrSKPA=";
        };
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
          repo = "cl-rabbit";
          rev = "9603204715bb13f09243dc286c5ad4bd51b4fd7b";
          hash = "sha256-SkbXB6+4SuVg+urQMPEF2WAIZTXVA3mmMnn1jTuGNeA=";
        };
        lispLibs = with pkgs.sbclPackages; [ cffi cffi-grovel cffi-libffi babel cl-ppcre ];
        nativeLibs = with pkgs; [ rabbitmq-c libffi ];
      };

      nhooks = pkgs.sbcl.buildASDFSystem rec {
        pname = "nhooks";
        version = "latest";
        src = pkgs.fetchFromGitHub {
          owner = "atlas-engineer";
          repo = "nhooks";
          rev = "3847bc749a6f6eb1103bc21f8ef3b4f6b301e822";
          hash = "sha256-h7zbXow3uzLxnodPRcqHDtFyQ+wiUNk1wg9+I9VlNMw=";
        };
        lispLibs = with pkgs.sbclPackages; [ serapeum bordeaux-threads closer-mop ];
      };

      cms-ulid = pkgs.sbcl.buildASDFSystem rec {
        pname = "cms-ulid";
        version = "latest";
        src = pkgs.fetchgit {
          url = "https://gitlab.com/colinstrickland/cms-ulid.git";
          rev = "fff84302dee5db42fb90aafd834af3ffbfd6c2bb";
          hash = "sha256-B5rekME60bWHk47kDepQpOr9drjgXjZBiRpA+Ob1CuU=";
        };
        lispLibs = with pkgs.sbclPackages; [ local-time ironclad bit-smasher serapeum ];
      };

      lack-middleware-accesslog = pkgs.sbcl.buildASDFSystem rec {
        pname = "lack-middleware-accesslog";
        version = "latest";
        src = pkgs.sbclPackages.lack.src;
        lispLibs = with pkgs.sbclPackages; [ lack local-time ];
      };

      # Create an SBCL with all our custom packages
      sbcl' = pkgs.sbcl.withOverrides (self: super: {
        inherit star-cl cl-couch cl-gserver cl-rabbit nhooks cms-ulid lack-middleware-accesslog;
        starintel = star-cl;
        sento = cl-gserver;
      });

      # Build the final executable
      starintel-gserver = sbcl'.buildASDFSystem rec {
        pname = "starintel-gserver";
        version = "0.1.0";
        src = ./.;

        nativeLibs = runtimeLibs;

        lispLibs = with sbcl'.pkgs; [
          starintel cl-couch serapeum alexandria cl-rabbit
          sento babel uuid anypool clack ningle clingon
          slynk nhooks lparallel cl-stream cl-ppcre
          cms-ulid bordeaux-threads xmls lack lack-middleware-accesslog
        ];

        systems = [ "starintel-gserver" ];

        # Keep test asd file so tests can be built
        asdFilesToKeep = [ "starintel-gserver.asd" "starintel-gserver-tests.asd" ];

        dontStrip = true;
      };

      # Build the test system
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

      # Build the API client library
      starintel-gserver-client = sbcl'.buildASDFSystem {
        pname = "starintel-gserver-client";
        version = "0.1.0";
        src = ./.;

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

      # Build the CLI client
      star-cli = sbcl'.buildASDFSystem {
        pname = "star-cli";
        version = "0.1.0";
        src = ./.;

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

      # Create wrapper with all dependencies
      sbcl-wrapped = sbcl'.withPackages (ps: with ps; [
        starintel-gserver
      ]);

      # Create wrapper for tests
      sbcl-test-wrapped = sbcl'.withPackages (ps: with ps; [
        starintel-gserver-tests
      ]);

      # Create wrapper for CLI
      sbcl-cli-wrapped = sbcl'.withPackages (ps: with ps; [
        star-cli
      ]);

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
              --prefix LD_LIBRARY_PATH : "${pkgs.lib.makeLibraryPath runtimeLibs}"
          '';
        };

        # Smoke test script
        star-smoke = pkgs.writeScriptBin "star-smoke" ''
          #!/usr/bin/env bash
          set -e

          export HOME="$(mktemp -d)"
          export XDG_CACHE_HOME="$HOME/.cache"
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

        # CLI client binary
        cli = pkgs.stdenv.mkDerivation {
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
              --prefix LD_LIBRARY_PATH : "${pkgs.lib.makeLibraryPath runtimeLibs}"
          '';
        };

        starintel-gserver = starintel-gserver;
        starintel-gserver-tests = starintel-gserver-tests;
        starintel-gserver-client = starintel-gserver-client;
        star-cli = star-cli;
      };

      # Add test checks
      checks.${system} = {
        starintel-gserver-tests = pkgs.stdenv.mkDerivation {
          name = "starintel-gserver-tests-check";
          src = ./.;

          nativeBuildInputs = [ sbcl-test-wrapped ];
          buildInputs = runtimeLibs;

          buildPhase = ''
            export HOME=$TMPDIR
            export XDG_CACHE_HOME="$HOME/.cache"
            export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath runtimeLibs}"

            # Copy source to writable location
            cp -r $src $TMPDIR/source
            chmod -R u+w $TMPDIR/source
            cd $TMPDIR/source

            echo "=========================================="
            echo "  Running StarIntel Gserver Tests"
            echo "=========================================="
            echo ""

            ${sbcl-test-wrapped}/bin/sbcl --non-interactive --no-userinit --no-sysinit \
              --eval "(require :asdf)" \
              --eval "(push (truename \".\") asdf:*central-registry*)" \
              --eval "(asdf:load-system :starintel-gserver-tests)" \
              --eval "(in-package :star-server-tests)" \
              --eval "(handler-case
                        (progn
                          (run-all-gserver-tests)
                          (uiop:quit 0))
                        (error (e)
                          (format t \"~%~%========================================~%\")
                          (format t \"  Test Error~%\")
                          (format t \"========================================~%\")
                          (format t \"~%Error: ~a~%~%\" e)
                          (uiop:quit 1)))" \
              2>&1 | tee $TMPDIR/test-output.log

            TEST_EXIT_CODE=''${PIPESTATUS[0]}

            if [ $TEST_EXIT_CODE -eq 0 ]; then
              echo ""
              echo "✓ Test check passed"
            else
              echo ""
              echo "✗ Test check failed with exit code $TEST_EXIT_CODE"
              exit $TEST_EXIT_CODE
            fi
          '';

          installPhase = ''
            mkdir -p $out
            cp $TMPDIR/test-output.log $out/test-results.log
            echo "Test results saved to $out/test-results.log"
          '';
        };
      };

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = with pkgs; [
          sbcl-wrapped pkg-config
        ] ++ runtimeLibs;

        shellHook = ''
          export LD_LIBRARY_PATH="${pkgs.lib.makeLibraryPath runtimeLibs}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}"
          unset LD_PRELOAD
          echo "Starintel Gserver dev environment ready"
          echo "Use: sbcl to start SBCL with all dependencies"
        '';
      };
    };
}

