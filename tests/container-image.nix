{
  self,
  system,
  pkgs,
  nixpkgs,
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
  name = "datomic-pro dev-mode container test";
  nodes = {
    docker =
      { pkgs, lib, ... }:
      {
        nixpkgs.overlays = [ self.overlays."${system}" ];
        virtualisation = {
          diskSize = 8192;
          memorySize = 2048;
          docker.enable = true;
        };
        environment.systemPackages = [
          pkgs.jq
          pkgs.clojure
          pkgs.datomic-pro
          pkgs.bash
          pkgs.vim
          pkgs.sqlite-interactive
          (pkgs.writeShellScriptBin "run-datomic-test" ''
            ${pkgs.jdk}/bin/java "-Ddatomic.uri=datomic:sql://app?jdbc:sqlite:/var/lib/datomic-docker/data/datomic-sqlite.db" \
                 -cp ".:${pkgs.sqlite-jdbc}/share/java/sqlite-jdbc-${pkgs.sqlite-jdbc.version}.jar:${pkgs.datomic-pro-peer}/share/java/*" \
                 clojure.main -m hello
          '')
        ];
        environment.etc."datomic-docker/logback.xml".text = ''
          <configuration>
            <!-- prevent per-message overhead for jul logging calls, e.g. Hornet -->
            <contextListener class="ch.qos.logback.classic.jul.LevelChangePropagator">
              <resetJUL>true</resetJUL>
            </contextListener>

            <appender name="STDOUT" class="ch.qos.logback.core.ConsoleAppender">
              <encoder>
                <pattern>%d{yyyy-MM-dd HH:mm:ss.SSS} %-5level %-10contextName %logger{36} - %msg%n</pattern>
              </encoder>
            </appender>

            <logger name="datomic.cast2slf4j" level="DEBUG"/>

            <!-- uncomment to log storage access -->
            <!-- <logger name="datomic.kv-cluster" level="DEBUG"/> -->

            <!-- uncomment to log transactor heartbeat -->
            <!-- <logger name="datomic.lifecycle" level="DEBUG"/> -->

            <!-- uncomment to log transactions (transactor side) -->
            <!-- <logger name="datomic.transaction" level="DEBUG"/> -->

            <!-- uncomment to log transactions (peer side) -->
            <!-- <logger name="datomic.peer" level="DEBUG"/> -->

            <!-- uncomment to log the transactor log -->
            <!-- <logger name="datomic.log" level="DEBUG"/> -->

            <!-- uncomment to log peer connection to transactor -->
            <!-- <logger name="datomic.connector" level="DEBUG"/> -->

            <!-- uncomment to log storage gc -->
            <!-- <logger name="datomic.garbage" level="DEBUG"/> -->

            <!-- uncomment to log indexing jobs -->
            <!-- <logger name="datomic.index" level="DEBUG"/> -->

            <!-- these namespsaces create a ton of log noise -->
            <logger name="org.apache.activemq.audit" level="WARN"/>
            <logger name="httpclient" level="INFO"/>
            <logger name="org.apache.commons.httpclient" level="INFO"/>
            <logger name="org.apache.http" level="INFO"/>
            <logger name="org.jets3t" level="INFO"/>
            <logger name="com.amazonaws" level="INFO"/>
            <logger name="com.amazonaws.request" level="WARN"/>
            <logger name="sun.rmi" level="INFO"/>
            <logger name="datomic.spy.memcached" level="INFO"/>
            <logger name="com.couchbase.client" level="INFO"/>
            <logger name="com.ning.http.client.providers.netty" level="INFO"/>
            <logger name="org.eclipse.jetty" level="INFO"/>
            <logger name="org.hornetq.core.client.impl" level="INFO"/>
            <logger name="org.apache.tomcat.jdbc.pool" level="INFO"/>

            <logger name="datomic.cast2slf4j" level="DEBUG"/>

            <root level="info">
              <appender-ref ref="STDOUT"/>
            </root>
          </configuration>
        '';
        users.users.root = {
          hashedPassword = lib.mkForce null;
          hashedPasswordFile = lib.mkForce null;
          initialPassword = lib.mkForce null;
          password = lib.mkForce "root";
        };
        environment.etc."datomic-docker/docker-compose.yml".text = builtins.readFile ./fixtures/docker-compose-sqlite.yml;

        #environment.etc."datomic-docker/deps.edn".text = ''
        #  {:paths ["."]
        #   :extra-paths ["${pkgs.datomic-pro-peer}/share/java/*" "${pkgs.sqlite-jdbc}/share/java/sqlite-jdbc-${pkgs.sqlite-jdbc.version}.jar"]
        #   :aliases {:run {:jvm-opts  ["-Ddatomic.uri=datomic:sql://app?jdbc:sqlite:/var/lib/datomic-docker/data/datomic-sqlite.db"]
        #                   :main-opts ["-m" "hello"]}}}
        #'';
        environment.etc."datomic-docker/hello.clj".text = builtins.readFile ./fixtures/hello.clj;
        environment.etc."datomic-docker/.env".text = "IMAGE=ghcr.io/outskirtslabs/datomic-pro:${pkgs.datomic-pro.version}";
      };
  };

  testScript = ''
    start_all()
    docker.wait_for_unit("sockets.target")
    docker.succeed("mkdir -p /var/lib/datomic-docker/data")
    docker.succeed("mkdir -p /var/lib/datomic-docker/config")
    docker.succeed("""
      echo "Creating SQLite database and schema..." && \
      sqlite3 /var/lib/datomic-docker/data/datomic-sqlite.db "
      CREATE TABLE IF NOT EXISTS datomic_kvs (
        id TEXT NOT NULL PRIMARY KEY,
        rev INTEGER,
        map TEXT,
        val BLOB
      );" && \
      echo "Database initialization complete."
    """)
    docker.wait_for_file("/var/lib/datomic-docker/data/datomic-sqlite.db")
    docker.succeed("echo Loading docker image && docker load --input='${pkgs.datomic-pro-container}'")
    docker.succeed("cd /etc/datomic-docker && docker compose up -d")
    docker.wait_for_open_port(4334)

    docker.wait_until_succeeds("cd /etc/datomic-docker && docker compose logs datomic-transactor | grep -q 'System started'")

    docker.wait_for_open_port(8081)

    machine.succeed("cd /etc/datomic-docker && run-datomic-test")
  '';
}
