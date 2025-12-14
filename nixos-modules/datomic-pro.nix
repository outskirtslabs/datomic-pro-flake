{
  config,
  lib,
  pkgs,
  ...
}:

let
  inherit (config.system) stateVersion;
  cfg = config.services.datomic-pro;
  settingsFormat = pkgs.formats.javaProperties { };
  stateDir = "/var/lib/${cfg.stateDirectoryName}";
  runtimePropertiesPath = "${stateDir}/transactor.properties";
  settingsDefault = {
    host = "127.0.0.1";
    port = 4334;
    data-dir = "${stateDir}/data";
  };
  propertiesFile = settingsFormat.generate "transactor.properties" (settingsDefault // cfg.settings);
  logbackConfigFile = pkgs.writeText "logback.xml" cfg.logbackConfig;
  extraJavaOptions =
    cfg.extraJavaOptions
    ++ lib.optional (cfg.logbackConfig != "") "-Dlogback.configurationFile=${logbackConfigFile}";
  extraClasspath = lib.concatStringsSep ":" cfg.extraClasspathEntries;
in
{
  options = {
    services.datomic-pro = {
      enable = lib.mkEnableOption "Datomic Pro";
      package = lib.mkOption {
        type = lib.types.package;
        description = "Which datomic-pro package to use.";
        relatedPackages = [
          "datomic-pro_1_0_7394"
          "datomic-pro_1_0_7387"
          "datomic-pro_1_0_7364"
        ];
      };
      secretsFile = lib.mkOption {
        type = lib.types.nullOr lib.types.path;
        default = null;
        description = ''
          Secret configuration concatenated to the transactor properties at runtime.

          Should be owned by root and have 0600 permissions.
        '';
      };
      extraJavaOptions = lib.mkOption {
        description = "Extra command line options for Java.";
        default = [ ];
        type = lib.types.listOf lib.types.str;
        example = [
          "-Dfoo=bar"
          "-Xbaz=bar"
        ];
      };
      extraClasspathEntries = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = ''
          Extra entries added to the Java classpath when running Datomic Pro.
        '';
        example = [
          "/path/to/my.jer"
          "/path/to/folder/of/jars/*"
        ];
      };
      logbackConfig = lib.mkOption {
        type = lib.types.lines;
        default = ''
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
        description = ''
          XML logback configuration for the datomic-pro transactor.
        '';
      };

      stateDirectoryName = lib.mkOption {
        type = lib.types.str;
        default = "datomic-pro";
        description = "The name of the directory under /var/lib that will be used as the state directory for datomic.";
      };

      settings = lib.mkOption {
        type = lib.types.submodule { freeformType = settingsFormat.type; };
        default = settingsDefault;
        description = ''
          Configuration written to `transactor.properties`.

          The default configuration will run the transactor in dev mode.

          ::: {.note}
          Do not specify secrets, password, etc in this config block as it is written to the globally readable nix store. For those properties use `secretSettingsFile`.
        '';
      };
    };
  };
  config = lib.mkIf cfg.enable {
    assertions = [
      {
        assertion = lib.attrsets.hasAttr "protocol" cfg.settings;
        message = ''
          You must define your storage procotol with the `protocol` key in <option>services.datomic-pro.settings</option> , refer to the Datomic Pro documentation.
          Some possible values are `"dev"`, `"sql"`, etc. Each protocol will have additional required settings that are not validated by this NixOS module.
        '';
      }
      {
        assertion = lib.strings.hasInfix "/" cfg.stateDirectoryName == false;
        message = ''
          <option>services.datomic-pro.stateDirectoryName</option> must be a single directory name, not a path with /.
        '';
      }

      {
        assertion = !(lib.attrsets.hasAttr "log-dir" cfg.settings);
        message = ''<option>services.datomic-pro.settings</option> must not contain the `log-dir` key, use <option>services.datomic-pro.logbackConfig</option> instead.'';
        # Ok intrepid spelunker, why can we not use log-dir? Because as of 2024-10, the log-dir is specially handled by datomic code and hardcodes a lookup for
        # logback.xml in the `bin/` dir of the datomic tarball. This obviously doesn't work on NixOS.
        # The solution is to NOT define log-dir, but instead just define your own logback configuration, we include a variation of the default that logs to stdout and ends up in systemd's journal.
      }
    ];
    services.datomic-pro.package =
      let
        mkThrow = ver: throw "datomic-pro_${ver} was removed, please upgrade your datomic-pro version.";
        mkWarn =
          ver:
          lib.warn ''
            The datomic-pro package is not pinned and selected automatically by
            `system.stateVersion`. Right now this is `pkgs.datomic-pro_${ver}`, the
            oldest datomic-pro version available.
          '';
        base =
          if lib.versionAtLeast config.system.stateVersion "26.05" then
            pkgs.datomic-pro_1_0_7394
          else if lib.versionAtLeast config.system.stateVersion "25.11" then
            pkgs.datomic-pro_1_0_7387
          else if lib.versionAtLeast config.system.stateVersion "24.11" then
            pkgs.datomic-pro_1_0_7364
          else
            mkWarn pkgs.datomic-pro_1_0_7277;
      in
      lib.mkDefault base;

    systemd.services.datomic-pro = {
      description = "Datomic Pro";
      wantedBy = [ "multi-user.target" ];
      preStart = ''
        cat ${propertiesFile} > ${runtimePropertiesPath}
        chmod 0600 ${runtimePropertiesPath}
        ${lib.optionalString (cfg.secretsFile != null) ''
          cat $CREDENTIALS_DIRECTORY/datomic-pro-secrets >> ${runtimePropertiesPath}
        ''}
        mkdir -p ${stateDir}/data ${stateDir}/log
      '';
      script = ''
        ${cfg.package}/bin/datomic-transactor ${runtimePropertiesPath}
      '';
      environment = {
        DATOMIC_JAVA_OPTS = toString extraJavaOptions;
      }
      // lib.optionalAttrs (cfg.extraClasspathEntries != [ ]) {
        CLASSPATH = extraClasspath;
      };
      serviceConfig = {
        Type = "simple";
        DynamicUser = true;
        StateDirectory = cfg.stateDirectoryName;
        LoadCredential = lib.mkIf (cfg.secretsFile != null) [ "datomic-pro-secrets:${cfg.secretsFile}" ];
        Restart = "always";
        MemoryDenyWriteExecute = false; # required for the jvm
        NoNewPrivileges = true;
        PrivateTmp = true;
        PrivateDevices = true;
        ProtectSystem = "strict";
        ProtectHome = true;
        ProtectControlGroups = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectKernelLogs = true;
        RestrictAddressFamilies = [
          "AF_UNIX"
          "AF_INET"
          "AF_INET6"
          "AF_NETLINK"
        ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        LockPersonality = true;
        SystemCallArchitectures = "native";
      };
    };
  };
}
