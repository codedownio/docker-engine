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
            url = "https://docs.docker.com/reference/api/engine/version/v1.36.yaml";
            hash = "sha256-mEyDAyzmk8C8iCHrl2Xr5mLUhVswaMfrWsbtNNl2iJc=";
          };
          fixes = [
            fixContainerSummaryDefinition
            fixHostConfigIsolation
          ];
        };
        api_1_37 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.37.yaml";
            hash = "sha256-UhhA7OI4NHOskRdl+Sik2hy4PE3ROJUltWlhqUoGNU4=";
          };
          fixes = [
            fixContainerSummaryDefinition
            fixHostConfigIsolation
          ];
        };
        api_1_38 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.38.yaml";
            hash = "sha256-j/rJFwRCeu4eHCnqhm+zRP4GYr03Lj26ATVeSVxX1cE=";
          };
          fixes = [
            fixContainerSummaryDefinition
            fixHostConfigIsolation
          ];
        };
        api_1_39 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.39.yaml";
            hash = "sha256-uupt1p8Ly1Qj24f1G2RzV3Q+dDp7oTicBx+pszHPpeQ=";
          };
          fixes = [
            fixContainerSummaryDefinition
          ];
        };
        api_1_40 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.40.yaml";
            hash = "sha256-ridNju3jtGyB7hQkJijrGfMx5qbKxjyfzyCTxf9GX2A=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_41 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.41.yaml";
            hash = "sha256-d2NppRZqVfOfm9Xy0+iXdifLzBJ32rhvqFEHguh6UVQ=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_42 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.42.yaml";
            hash = "sha256-3Ef1qUE5v8bYdCP4DlF3SUQdAiBj18nWhmyeJniDVCY=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_43 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.43.yaml";
            hash = "sha256-Al2ECUOYYkfo76k/1TNlo3r+DuVJZ+nPYiy35/mPVCI=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_44 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.44.yaml";
            hash = "sha256-vhPxelA6stBzLXVf/ycsqsPdABYpRcWuElIRaP1GsOs=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_45 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.45.yaml";
            hash = "sha256-7SvF0rSQhu5/Uve+R+n808z2Cg0aZdgckUBnS2F76mQ=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_46 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.46.yaml";
            hash = "sha256-kRSv3IAXOzAmXSgE7wLAJlfFdTCR87/vwYzF/fUev4c";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_47 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.47.yaml";
            hash = "sha256-yMchI2trdWQCT35roUey+zWm59KTHh9H0h6q6Qk9e30=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_48 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.48.yaml";
            hash = "sha256-nJ8yX9fZ/obJT4fO9QwRHDj+K9orkXhfu86tiND8wlw=";
          };
          fixes = [fixHostConfigIsolation];
        };
        api_1_49 = mkApiYaml {
          src = pkgs.fetchurl {
            url = "https://docs.docker.com/reference/api/engine/version/v1.49.yaml";
            hash = "sha256-9W8AOk6XkOXmRQlsH6j5fRKfQjEcIuWd9MR/sKnHqm0=";
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
          };
        }
    );
}
