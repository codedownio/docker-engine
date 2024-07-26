{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        versionSuffix = 0;

        mkApiYaml = { src, patchPhase ? true }: pkgs.stdenv.mkDerivation {
          name = "docker-api-1.39.${toString versionSuffix}.yaml";
          inherit src;
          unpackPhase = ''
            cp $src api.yaml
          '';
          inherit patchPhase;
          buildInputs = [pkgs.yq-go];
          dontConfigure = true;
          dontBuild = true;
          installPhase = ''
            cp api.yaml "$out"
          '';
        };

        api_1_36 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.36.yaml";
          hash = "sha256-6kS2MJunowLqAEhdCqi+lXLHsGb9dr2M51fuG+ENX0Q=";
        };
        api_1_37 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.37.yaml";
          hash = "sha256-TSOJs7T7EDkehQIqRa7U59miFdxH72YIn8ynBx2uUOI=";
        };
        api_1_38 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.38.yaml";
          hash = "sha256-5eHhNFiO4YXVhl045OldlL8Mry72LybHzuAtJT1dfMc=";
        };
        api_1_39 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/engine/v1.39.yaml";
            hash = "sha256-Oswl1SJb2MCVpTQ/P9Cj+l1gM8d7E7IXxzffmeavhFM=";
          };
          patchPhase = ''
            yq e -i '.definitions.ContainerSummary.type = "object"' api.yaml
            yq e -i '.definitions.ContainerSummary.properties = .definitions.ContainerSummary.items.properties' api.yaml
            yq e -i 'del(.definitions.ContainerSummary.items)' api.yaml
          '';
        };
        api_1_40 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.40.yaml";
          hash = "sha256-7AOKrQhc1wzFNnMEIk8grt0DK+KtWLTkrrqwAqiKlQo=";
        };
        api_1_41 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.41.yaml";
          hash = "sha256-bTE0P7dTdIILMxuPy0lm07fB6azn42SxkrLFhramEjE=";
        };
        api_1_42 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.42.yaml";
          hash = "sha256-qaILCCvjwXoPf4R7SHEhsTmronF4h7BtsLChP3pHJBI=";
        };
        api_1_43 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.43.yaml";
          hash = "sha256-R29jmbUjGsOOJl7uITl2vgcifGWhKUmuK4p32Xz+Vbc=";
        };
        api_1_44 = pkgs.fetchurl {
          url = "https://docs.docker.com/reference/engine/v1.44.yaml";
          hash = "sha256-GfZnFirciPMRuBrUqjIydfpZ7yw/L26tNBjMxId4NLg=";
        };

        mkGenerateScript = apiYaml: dir: pkgs.writeShellScriptBin "generate.sh" ''
          mkdir -p "${dir}"
          ${pkgs.openapi-generator-cli}/bin/openapi-generator-cli generate \
            --generator-name haskell-http-client \
            -i ${apiYaml} \
            -o "${dir}"

          # Fill in the package version
          PACKAGE_VERSION="${builtins.replaceStrings ["_"] ["."] (builtins.substring 1 (-1) dir)}.${toString versionSuffix}"
          ${pkgs.gnused}/bin/sed -i "s/^version:\s*\(.*\)/version:        $PACKAGE_VERSION/" "${dir}/docker-engine.cabal"

          # Fill in license
          # https://docs.docker.com/engine/#licensing
          ${pkgs.gnused}/bin/sed -i "s/^license:\s*\(.*\)/license:        Apache-2.0/" "${dir}/docker-engine.cabal"

          # Fill in other metadata
          ${pkgs.gnused}/bin/sed -i "s/^author:\s*\(.*\)/author:         Tom McLaughlin <tom@codedown.io>/" "${dir}/docker-engine.cabal"
          ${pkgs.gnused}/bin/sed -i "s/^maintainer:\s*\(.*\)/maintainer:     Tom McLaughlin <tom@codedown.io>/" "${dir}/docker-engine.cabal"
          ${pkgs.gnused}/bin/sed -i "s/^homepage:\s*\(.*\)/homepage:       https:\/\/github.com\/codedownio\/docker-engine/" "${dir}/docker-engine.cabal"
          ${pkgs.gnused}/bin/sed -i '/copyright:/d' "${dir}/docker-engine.cabal"

          # Remove some unnecessary files
          rm "${dir}/openapi.yaml"
          rm "${dir}/.travis.yml"
          rm "${dir}/stack.yaml"

          # Delete openapi.yaml from the extra-source-files
          ${pkgs.gnused}/bin/sed -i '/^\s*openapi\.yaml$/d' "${dir}/docker-engine.cabal"

          # Patch up some problems
          echo "type Map = A.Object" >> "${dir}/lib/DockerEngine/Model.hs"
        '';

      in
        {
          packages = {
            inherit (pkgs) openapi-generator-cli;

            inherit
              api_1_36
              api_1_37
              api_1_38
              api_1_39
              api_1_40
              api_1_41
              api_1_42
              api_1_43
              api_1_44;

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
