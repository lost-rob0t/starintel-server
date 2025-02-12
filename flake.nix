{
  description = "Starintel API server that routes the data through msg queues.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      newerQlot = self: super: pkgs.sbclPackages.qlot-cli.overrideAttrs (finalAttrs: previousAttrs: {
          src = pkgs.fetchFromGitHub {
            owner = "fukamachi";
            repo = "qlot";
            rev = "refs/tags/1.6.0";
            hash = "sha256-j9iT25Yz9Z6llCKwwiHlVNKLqwuKvY194LrAzXuljsE=";
    };
      });
    in
    {
      devShell.x86_64-linux =
        pkgs.mkShell {
          buildInputs = with pkgs; [
            pkg-config
            sbcl
            openssl
            rabbitmq-c
            libffi
            sbclPackages.qlot-cli

          ];
          shellHook = ''
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath([pkgs.lmdb.out])}:${pkgs.lib.makeLibraryPath([pkgs.openssl pkgs.rabbitmq-c pkgs.libffi pkgs.sqlite])}
          '';
        };
    };
}
