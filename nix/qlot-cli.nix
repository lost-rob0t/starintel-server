(self: super: {
            sbclPackages = super.sbclPackages // {
              qlot-cli = super.sbclPackages.qlot-cli.overrideAttrs (_: {
                version = "1.6.0";
                src = super.fetchFromGitHub {
                  owner = "fukamachi";
                  repo = "qlot";
                  rev = "refs/tags/1.6.0";
                  hash = "sha256-yHRnacUXc8RYTyWWuFyzH17TVSSI9Rm8zIi5WentVMM=";
                };
              });
            };
          })
