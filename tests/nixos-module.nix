{
  self,
  system,
  pkgs,
  nixpkgs,
  datomic-pro,
  datomic-pro-peer,
}:

let
  inherit
    (import (nixpkgs + "/nixos/lib/testing-python.nix") {
      inherit system pkgs;
    })
    makeTest
    ;
in
makeTest {
  name = "datomic-pro module test";
  nodes = {
    client =
      {
        config,
        pkgs,
        lib,
        ...
      }:
      let
        version = datomic-pro.version;
      in
      {
        nixpkgs.overlays = [ self.overlays."${system}" ];
        virtualisation.memorySize = 2048;
        imports = [
          self.nixosModules.${system}.datomic-pro
          self.nixosModules.${system}.datomic-console
        ];
        environment.etc."datomic-pro/do-not-do-this.properties" = {
          text = ''
            storage-admin-password=do-not-do-it-this-way-in-prod
            storage-datomic-password=do-not-do-it-this-way-in-prod-peer
          '';
          mode = "0600";
        };
        services.datomic-pro = {
          enable = true;
          package = datomic-pro;
          secretsFile = "/etc/datomic-pro/do-not-do-this.properties";
          settings = {
            host = "localhost";
            port = 4334;
            protocol = "dev";
            storage-access = "remote";
            # the follow memory tweaks are only for the test env
            memory-index-max = "64m";
            memory-index-threshold = "16m";
            object-cache-max = "64m";
          };
        };

        environment.etc."datomic-console/do-not-do-this" = {
          text = "datomic:dev://localhost:4334/?password=do-not-do-it-this-way-in-prod-peer";
          mode = "0600";
        };
        services.datomic-console = {
          enable = true;
          alias = "dev";
          port = 8080;
          dbUriFile = "/etc/datomic-console/do-not-do-this";
        };
        environment.systemPackages = [
          pkgs.clojure
          pkgs.vim
          pkgs.bash
          datomic-pro
          datomic-pro-peer
          (pkgs.writeShellScriptBin "run-datomic-test" ''
            ${pkgs.jdk}/bin/java -Ddatomic.uri=datomic:dev://localhost:4334/test-db?password=do-not-do-it-this-way-in-prod-peer \
                 -cp ".:${datomic-pro-peer}/share/java/*" \
                 clojure.main -m hello
          '')
        ];
        users.users.root = {
          hashedPassword = lib.mkForce null;
          hashedPasswordFile = lib.mkForce null;
          initialPassword = lib.mkForce null;
          password = lib.mkForce "root";
        };
        # I thought we could just use the clojure cli took, but it always reaches out to maven unfortunately to get itself
        #environment.etc."datomic-test/deps.edn" = {
        #  mode = "0600";
        #  text = ''
        #    {:paths   ["."]
        #     :extra-paths ["${datomic-pro-peer}/share/java/*"]
        #     :aliases {:run {:jvm-opts  ["-Ddatomic.uri=datomic:dev://localhost:4334/test-db?password=do-not-do-it-this-way-in-prod-peer"]
        #                     :main-opts ["-m" "hello"]}}}
        #  '';
        #};
        environment.etc."datomic-test/hello.clj" = {
          mode = "0600";
          text = builtins.readFile ./fixtures/hello.clj;
        };
      };
  };

  testScript = ''
    start_all()
    machine.wait_for_unit("datomic-pro.service")
    machine.wait_for_open_port(4334)
    machine.wait_until_succeeds("journalctl -u datomic-pro -o cat | grep -q 'System started'")

    machine.succeed("cd /etc/datomic-test && run-datomic-test")

    machine.wait_for_unit("datomic-console.service")
    machine.wait_for_open_port(8080)

    machine.succeed("curl --fail http://localhost:8080/browse")

    print(machine.succeed("datomic-run -m datomic.integrity 'datomic:dev://localhost:4334/test-db?password=do-not-do-it-this-way-in-prod-peer'"))

  '';
}
