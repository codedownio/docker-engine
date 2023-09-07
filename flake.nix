{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
        {
          apps.generate = let
            generateScript = pkgs.writeShellScriptBin "generate.sh" ''
              ${pkgs.openapi-generator-cli}/bin/openapi-generator-cli generate --generator-name haskell-http-client
            '';
          in {
            type = "app";
            program = "${generateScript}/bin/generate.sh";
          };

          packages = {
            default = pkgs.hello;
          };
        }
    );
}
