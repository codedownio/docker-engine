{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.05";

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        majorVersion = 0;
        minorVersion = 0;

        # Fix a spec issue where the ContainerSummary definition is wrongly specified as an array,
        # rather than an object
        fixContainerSummaryDefinition = ''
          yq e -i '.definitions.ContainerSummary.type = "object"' api.yaml
          yq e -i '.definitions.ContainerSummary.properties = .definitions.ContainerSummary.items.properties' api.yaml
          yq e -i 'del(.definitions.ContainerSummary.items)' api.yaml
        '';

        # Docker may emit an empty string for HostConfig.Isolation
        fixHostConfigIsolation = ''
          yq e -i '.definitions.HostConfig.allOf[1].properties.Isolation.enum += [""]' api.yaml
        '';

        mkApiYaml = { src, fixes ? [] }: pkgs.stdenv.mkDerivation {
          name = "docker-api.yaml";
          inherit src;
          unpackPhase = ''
            cp $src api.yaml
          '';
          patchPhase = pkgs.lib.concatStringsSep "\n" fixes;
          buildInputs = [pkgs.yq-go];
          dontConfigure = true;
          dontBuild = true;
          installPhase = ''
            cp api.yaml "$out"
          '';
        };

        api_1_36 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/engine/v1.36.yaml";
            hash = "sha256-6kS2MJunowLqAEhdCqi+lXLHsGb9dr2M51fuG+ENX0Q=";
          };
          fixes = [
            fixContainerSummaryDefinition
            fixHostConfigIsolation
          ];
        };
        api_1_37 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/engine/v1.37.yaml";
            hash = "sha256-TSOJs7T7EDkehQIqRa7U59miFdxH72YIn8ynBx2uUOI=";
          };
          fixes = [
            fixContainerSummaryDefinition
            fixHostConfigIsolation
          ];
        };
        api_1_38 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/engine/v1.38.yaml";
            hash = "sha256-5eHhNFiO4YXVhl045OldlL8Mry72LybHzuAtJT1dfMc=";
          };
          fixes = [
            fixContainerSummaryDefinition
            fixHostConfigIsolation
          ];
        };
        api_1_39 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/engine/v1.39.yaml";
            hash = "sha256-Oswl1SJb2MCVpTQ/P9Cj+l1gM8d7E7IXxzffmeavhFM=";
          };
          fixes = [
            fixContainerSummaryDefinition
          ];
        };
        api_1_40 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/engine/v1.40.yaml";
            hash = "sha256-7AOKrQhc1wzFNnMEIk8grt0DK+KtWLTkrrqwAqiKlQo=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_41 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/engine/v1.41.yaml";
            hash = "sha256-bTE0P7dTdIILMxuPy0lm07fB6azn42SxkrLFhramEjE=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_42 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/engine/v1.42.yaml";
            hash = "sha256-qaILCCvjwXoPf4R7SHEhsTmronF4h7BtsLChP3pHJBI=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_43 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/engine/v1.43.yaml";
            hash = "sha256-R29jmbUjGsOOJl7uITl2vgcifGWhKUmuK4p32Xz+Vbc=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_44 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/engine/v1.44.yaml";
            hash = "sha256-GfZnFirciPMRuBrUqjIydfpZ7yw/L26tNBjMxId4NLg=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_45 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/engine/v1.45.yaml";
            hash = "sha256-PiGCCkbT/dHiSE4J+/rAX4MWfb1JR6FHkPui8DTFP7o=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_46 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/engine/v1.46.yaml";
            hash = "sha256-bJr6mFqSvuFcN49NblO9pehFVCz7PSesBUweCicqRig=";
          };
          fixes = [fixHostConfigIsolation];
        };

        mkGenerateScript = apiYaml: dir: pkgs.writeShellScriptBin "generate.sh" ''
          mkdir -p "${dir}"

          # Would be nice to use this to deal with enum problems, but it produces crazy output:
          # --additional-properties=enumUnknownDefaultCase=true \

          ${pkgs.openapi-generator-cli}/bin/openapi-generator-cli generate \
            --generator-name haskell-http-client \
            -i ${apiYaml} \
            -o "${dir}"

          # Fill in the package version
          PACKAGE_VERSION="${toString majorVersion}.${builtins.replaceStrings ["."] [""] (builtins.substring 1 (-1) dir)}.${toString minorVersion}"
          ${pkgs.gnused}/bin/sed -i "s/^version:\s*\(.*\)/version:        $PACKAGE_VERSION/" "${dir}/docker-engine.cabal"

          # Fill in license
          # https://docs.docker.com/engine/#licensing
          ${pkgs.gnused}/bin/sed -i "s/^license:\s*\(.*\)/license:        Apache-2.0/" "${dir}/docker-engine.cabal"

          # Fill in other metadata
          ${pkgs.gnused}/bin/sed -i "s/^author:\s*\(.*\)/author:         Tom McLaughlin <tom@codedown.io>/" "${dir}/docker-engine.cabal"
          ${pkgs.gnused}/bin/sed -i "s/^maintainer:\s*\(.*\)/maintainer:     Tom McLaughlin <tom@codedown.io>/" "${dir}/docker-engine.cabal"
          ${pkgs.gnused}/bin/sed -i "s/^homepage:\s*\(.*\)/homepage:       https:\/\/github.com\/codedownio\/docker-engine/" "${dir}/docker-engine.cabal"
          ${pkgs.gnused}/bin/sed -i '/copyright:/d' "${dir}/docker-engine.cabal"

          # Bump the bound on http-api-data
          ${pkgs.gnused}/bin/sed -i "s/^[^,]*,\shttp-api-data.*$/    , http-api-data >= 0.3.4 \&\& <0.7/" "${dir}/docker-engine.cabal"

          # Remove some unnecessary files
          rm "${dir}/openapi.yaml"
          rm "${dir}/.travis.yml"
          rm "${dir}/stack.yaml"

          # Delete openapi.yaml from the extra-source-files
          ${pkgs.gnused}/bin/sed -i '/^\s*openapi\.yaml$/d' "${dir}/docker-engine.cabal"

          # Patch up some problems.
          # This "Map" type is emitted for the "Topology" definition. Not sure how to correct
          # the spec, so let's just provide a type alias here.
          echo "type Map = HM.HashMap String String" >> "${dir}/lib/DockerEngine/Model.hs"
          # Similarly patch up the test instances
          ${pkgs.gnused}/bin/sed -i '1i{-# LANGUAGE FlexibleInstances #-}' "${dir}/tests/Instances.hs"
          echo 'instance Arbitrary (HM.HashMap String String) where arbitrary = HM.fromList <$> arbitrary' >> "${dir}/tests/Instances.hs"
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
              api_1_44
              api_1_45
              api_1_46;

            generate1_36 = mkGenerateScript api_1_36 "v1.36";
            generate1_37 = mkGenerateScript api_1_37 "v1.37";
            generate1_38 = mkGenerateScript api_1_38 "v1.38";
            generate1_39 = mkGenerateScript api_1_39 "v1.39";
            generate1_40 = mkGenerateScript api_1_40 "v1.40";
            generate1_41 = mkGenerateScript api_1_41 "v1.41";
            generate1_42 = mkGenerateScript api_1_42 "v1.42";
            generate1_43 = mkGenerateScript api_1_43 "v1.43";
            generate1_44 = mkGenerateScript api_1_44 "v1.44";
            generate1_45 = mkGenerateScript api_1_45 "v1.45";
            generate1_46 = mkGenerateScript api_1_46 "v1.46";
          };
        }
    );
}
