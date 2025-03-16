{
  description = "Starintel API server that routes the data through msg queues.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs }:
    let
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
          (import ./nix/qlot-cli.nix)
        ];
      };
    in {
      devShell.${system} = pkgs.mkShell {
        buildInputs = with pkgs; [
          pkg-config
          nix
          sbcl
          openssl
          rabbitmq-c
          libffi
          sbclPackages.qlot-cli  # Uses the overridden version
        ];
          LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
            pkgs.lmdb
            pkgs.openssl
            pkgs.rabbitmq-c
            pkgs.libffi
            pkgs.sqlite
          ];
      };
    };
}
