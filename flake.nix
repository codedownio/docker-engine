{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-24.11";

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

        # Docker may emit an empty string for HostConfig.Isolation
        fixGraphDriverDataNullable = ''
          yq e -i '.definitions.GraphDriverData.properties.Data.x-nullable = true' api.yaml
          yq e -i '.definitions.GraphDriverData.required = ["Name"]' api.yaml
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
            url = "https://docs.docker.com/reference/api/engine/version/v1.36.yaml";
            hash = "sha256-WKRpvs5xRgmKrxYyroTWyJuq5nO6h4Pl+dk0nMleKEU=";
          };
          fixes = [
            fixContainerSummaryDefinition
            fixHostConfigIsolation
          ];
        };
        api_1_37 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.37.yaml";
            hash = "sha256-xunRRt5+Mtk6+YkgVqfBShgXEMspz0F700dYdg3LFTo=";
          };
          fixes = [
            fixContainerSummaryDefinition
            fixHostConfigIsolation
          ];
        };
        api_1_38 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.38.yaml";
            hash = "sha256-UoMRl4uw2FUIGdC4NemurkyvNURPIIs310MHmkFzRPc=";
          };
          fixes = [
            fixContainerSummaryDefinition
            fixHostConfigIsolation
          ];
        };
        api_1_39 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.39.yaml";
            hash = "sha256-9UfEzxsUf6Qy0uge9BZBecJh4OU2f/1P8KdcE/Y4VIA=";
          };
          fixes = [
            fixContainerSummaryDefinition
          ];
        };
        api_1_40 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.40.yaml";
            hash = "sha256-tqNsoXVwRJFeTYivGzkZtLx83E8nHxfIYJq8MGThPKg=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_41 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.41.yaml";
            hash = "sha256-G7m+s1MgQWOoFLs/ZGHjBhdOH6cd9Q2OpSPk2EZ5EYM=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_42 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.42.yaml";
            hash = "sha256-ebY+WeB7qZBgInV0GsU4gB16N+nkqaniuo/W+dMYIB0=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_43 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.43.yaml";
            hash = "sha256-Y391vkcYJ5ncX33ts4lWlGvtfvr7C2kJbJ+lquh71sc=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_44 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.44.yaml";
            hash = "sha256-gUHSS+/MSBRnapas1CKibX5zUkCmU9QmJymJ9A/pk1A=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_45 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.45.yaml";
            hash = "sha256-dB5waDT6HeK3y5cnBzMXKGAfAyZfbryd91WkUa2IHNU=";
          };
          fixes = [
            fixHostConfigIsolation
            fixGraphDriverDataNullable
          ];
        };
        api_1_46 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.46.yaml";
            hash = "sha256-u44GlkR1oZarTDZqrCPoycR594NTsvPws0ezoHqKvqw=";
          };
          fixes = [
            fixHostConfigIsolation
            fixGraphDriverDataNullable
          ];
        };
        api_1_47 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.47.yaml";
            hash = "sha256-5kVUKhYir7kO9o7DJ2WaaP2nhqiux0Hv/vBIJPsUsQc=";
          };
          fixes = [
            fixHostConfigIsolation
            fixGraphDriverDataNullable
          ];
        };
        api_1_48 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.48.yaml";
            hash = "sha256-a+x/1B48/kJjMPX//trTKsZXIbMHlBQIOIbEPuZqV50=";
          };
          fixes = [
            fixHostConfigIsolation
            fixGraphDriverDataNullable
          ];
        };
        api_1_49 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.49.yaml";
            hash = "sha256-a6onACRpydAab2ofRtgMjhytLuF+rTNiM2hjgudaVsU=";
          };
          fixes = [
            fixHostConfigIsolation
            fixGraphDriverDataNullable
          ];
        };
        api_1_50 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.50.yaml";
            hash = "sha256-opyyvlxHVYX15t/Qd2LgVdSyXnPVr9wxvHa645foDUI=";
          };
          fixes = [
            fixHostConfigIsolation
            fixGraphDriverDataNullable
          ];
        };
        api_1_51 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.51.yaml";
            hash = "sha256-EwSc8DLG+qLaEexeckotLzZngGqOP1NzM5kmnaURcu0=";
          };
          fixes = [
            fixHostConfigIsolation
            fixGraphDriverDataNullable
          ];
        };
        api_1_52 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.52.yaml";
            hash = "sha256-af4Szixu8eQqMX5cPWDJ4NvFv08w5WRyq5R5ddmm7ts=";
          };
          fixes = [
            fixHostConfigIsolation
            fixGraphDriverDataNullable
          ];
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
          PACKAGE_VERSION="${builtins.replaceStrings ["."] [""] (builtins.substring 1 (-1) dir)}.${toString majorVersion}.${toString minorVersion}"
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
              api_1_46
              api_1_47
              api_1_48
              api_1_49
              api_1_50
              api_1_51
              api_1_52
            ;

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
            generate1_47 = mkGenerateScript api_1_47 "v1.47";
            generate1_48 = mkGenerateScript api_1_48 "v1.48";
            generate1_49 = mkGenerateScript api_1_49 "v1.49";
            generate1_50 = mkGenerateScript api_1_50 "v1.50";
            generate1_51 = mkGenerateScript api_1_51 "v1.51";
            generate1_52 = mkGenerateScript api_1_52 "v1.52";
          };
        }
    );
}
