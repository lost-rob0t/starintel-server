{
  description = "Starintel API server that routes the data through msg queues.";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
  };

  outputs = { self, nixpkgs }:
    let
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
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
            sqlite
            pre-commit
          ];
          shellHook = ''
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath([pkgs.lmdb.out])}:${pkgs.lib.makeLibraryPath([pkgs.openssl pkgs.rabbitmq-c pkgs.libffi pkgs.sqlite])}
          '';
        };
    };
}
