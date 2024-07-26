{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        api_1_36 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.44.yaml";
          hash = pkgs.lib.fakeHash;
        };
        api_1_37 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.44.yaml";
          hash = pkgs.lib.fakeHash;
        };
        api_1_38 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.44.yaml";
          hash = pkgs.lib.fakeHash;
        };
        api_1_39 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.44.yaml";
          hash = pkgs.lib.fakeHash;
        };
        api_1_40 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.44.yaml";
          hash = pkgs.lib.fakeHash;
        };
        api_1_41 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.44.yaml";
          hash = pkgs.lib.fakeHash;
        };
        api_1_42 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.44.yaml";
          hash = pkgs.lib.fakeHash;
        };
        api_1_43 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.44.yaml";
          hash = pkgs.lib.fakeHash;
        };
        api_1_44 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.44.yaml";
          hash = "sha256-GfZnFirciPMRuBrUqjIydfpZ7yw/L26tNBjMxId4NLg=";
        };

        mkGenerateScript = apiYaml: dir: pkgs.writeShellScriptBin "generate.sh" ''
          mkdir -p "${dir}"
          ${pkgs.openapi-generator-cli}/bin/openapi-generator-cli generate --generator-name haskell-http-client -i ${apiYaml} -o "${dir}"
        '';

      in
        {
          packages = {
            inherit (pkgs) openapi-generator-cli;

            generate1_36 = mkGenerateScript api_1_36 "v1.36";
            generate1_37 = mkGenerateScript api_1_37 "v1.37";
            generate1_38 = mkGenerateScript api_1_38 "v1.38";
            generate1_39 = mkGenerateScript api_1_39 "v1.39";
            generate1_40 = mkGenerateScript api_1_40 "v1.40";
            generate1_41 = mkGenerateScript api_1_41 "v1.41";
            generate1_42 = mkGenerateScript api_1_42 "v1.42";
            generate1_43 = mkGenerateScript api_1_43 "v1.43";
            generate1_44 = mkGenerateScript api_1_44 "v1.44";
          };
        }
    );
}
