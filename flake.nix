{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
    flake-utils.url = "github:numtide/flake-utils";
    clj-nix.url = "github:jlesquembre/clj-nix";
    snowfall-drift = {
      url = "github:snowfallorg/drift";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      clj-nix,
      snowfall-drift,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            clj-nix.overlays.default
            snowfall-drift.overlays.default
            self.overlays."${system}"
          ];
        };

        versions = import ./pkgs/versions.nix { inherit pkgs; };
      in
      {
        overlays =
          final: prev:
          versions
          // {
            datomic-pro-container = pkgs.callPackage ./pkgs/datomic-pro-container-image.nix {
              imageTag = final.datomic-pro.version;
            };
            datomic-pro-container-unstable = pkgs.callPackage ./pkgs/datomic-pro-container-image.nix {
              imageTag = "unstable";
            };
            datomic-generate-properties = pkgs.callPackage ./pkgs/datomic-generate-properties.nix { };
          };
        packages = {
          default = self.packages.${system}.datomic-pro;
          datomic-pro-container = pkgs.datomic-pro-container;
          datomic-pro-container-unstable = pkgs.datomic-pro-container-unstable;
          datomic-generate-properties = pkgs.datomic-generate-properties;
        }
        // builtins.mapAttrs (name: _: pkgs.${name}) versions;
        nixosModules = {
          datomic-pro = import ./nixos-modules/datomic-pro.nix;
          datomic-console = import ./nixos-modules/datomic-console.nix;
        };
        checks =
          let
            datomicVersions = pkgs.lib.filterAttrs (
              name: value: pkgs.lib.hasPrefix "datomic-pro_" name && !pkgs.lib.hasPrefix "datomic-pro-peer_" name
            ) versions;
          in
          builtins.mapAttrs (
            name: value:
            # a nixos module test vm for each datomic-pro version
            import ./tests/nixos-module.nix {
              inherit
                system
                pkgs
                nixpkgs
                self
                ;
              datomic-pro = value;
              datomic-pro-peer = versions."datomic-pro-peer_${pkgs.lib.removePrefix "datomic-pro_" name}";
            }
          ) datomicVersions
          // {
            # A test of the container image that runs in a VM
            containerImageTest = import ./tests/container-image.nix {
              inherit
                system
                pkgs
                nixpkgs
                self
                ;
            };
          };
        devShells.default = pkgs.mkShell {
          buildInputs = [
            snowfall-drift.packages.${system}.drift
            pkgs.skopeo
            pkgs.babashka
            pkgs.dive
            pkgs.gnumake
          ];
        };
      }
    );
}
