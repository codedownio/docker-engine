{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
        {
          apps = {
            generate = let
              script = pkgs.writeShellScriptBin "generate.sh" ''
                ${pkgs.openapi-generator-cli}/bin/openapi-generator-cli generate --generator-name haskell-http-client -i ./swagger.yaml
              '';
            in {
              type = "app";
              program = "${script}/bin/generate.sh";
            };

            openapi-version = let
              script = pkgs.writeShellScriptBin "version.sh" ''
                ${pkgs.openapi-generator-cli}/bin/openapi-generator-cli version
              '';
            in {
              type = "app";
              program = "${script}/bin/version.sh";
            };
          };

          packages = {
            default = pkgs.hello;
          };
        }
    );
}
