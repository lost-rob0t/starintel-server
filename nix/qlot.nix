(final: prev: {
            qlot = prev.sbclPackages.qlot-cli.overrideAttrs (_: {
              version = "1.6.0";
              src = prev.fetchFromGitHub {
                owner = "fukamachi";
                repo = "qlot";
                rev = "refs/tags/1.6.0";
                hash = "sha256-j9iT25Yz9Z6llCKwwiHlVNKLqwuKvY194LrAzXuljsE=";
              };
            });
          })
