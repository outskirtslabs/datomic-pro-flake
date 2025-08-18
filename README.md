# datomic-pro-flake

[![FlakeHub](https://img.shields.io/endpoint?url=https://flakehub.com/f/ramblurr/datomic-pro/badge)](https://flakehub.com/flake/ramblurr/datomic-pro)
[![GitHub License](https://img.shields.io/github/license/outskirtslabs/datomic-pro-flake)](./LICENSE)
[![GitHub Actions Workflow Status](https://img.shields.io/github/actions/workflow/status/outskirtslabs/datomic-pro-flake/ci.yml)](https://github.com/outskirtslabs/datomic-pro-flake/actions/workflows/ci.yml)

️ This flake exposes:

* A `datomic-pro` nix package (and `console`, and `peer`)
* ❄ NixOS modules for running Datomic Pro on NixOS
* 🐋 A container image that you can use to run Datomic Pro (no nix required!)

All of the above are [end-to-end tested](./tests) by the CI suite in this repo!

**Project status:** Experimental but ready for testing. Breaking changes may occur until version 1.0. The 1.0 release will be considered production-ready.

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [datomic-pro-flake](#datomic-pro-flake)
  - [Usage - NixOS Module](#usage---nixos-module)
    - [`flake.nix`](#flakenix)
    - [`/etc/datomic-pro/secrets.properties`](#etcdatomic-prosecretsproperties)
    - [`configuration.nix`](#configurationnix)
  - [🐋 Usage - Docker (OCI) Container Image](#-usage---docker-oci-container-image)
    - [Transactor Mode](#transactor-mode)
      - [Env vars](#env-vars)
    - [Console Mode](#console-mode)
      - [Env vars](#env-vars-1)
    - [Example Compose](#example-compose)
      - [Datomic Pro with Local Storage](#datomic-pro-with-local-storage)
      - [Datomic Pro with SQLite Storage](#datomic-pro-with-sqlite-storage)
      - [Datomic Pro with Postgres Storage and memcached](#datomic-pro-with-postgres-storage-and-memcached)
    - [Discussion](#discussion)
  - [License](#license)

<!-- markdown-toc end -->


## Usage - Nix Package

`pkgs.datomic-pro` will always be the latest release, but the following specific versions are also available:

-  `pkgs.datomic-pro_1_0_7387` (latest)
-  `pkgs.datomic-pro_1_0_7364`
-  `pkgs.datomic-pro_1_0_7277`

And for peer:

-  `pkgs.datomic-pro-peer_1_0_7387` (latest)
-  `pkgs.datomic-pro-peer_1_0_7364`
-  `pkgs.datomic-pro-peer_1_0_7277`

## Usage - NixOS Module

### `flake.nix`

```nix
{
  inputs.
   inputs = {
        nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
        # Check https://github.com/outskirtslabs/datomic-pro-flake/releases for the latest release tag
        datomic-pro.url = "https://flakehub.com/f/Ramblurr/datomic-pro/0.7.0.tar.gz";
        datomic-pro.nixpkgs = "nixpkgs";
    };
    outputs = { nixpkgs, datomic-pro, ... }@attrs: {
        nixosConfigurations.machine = nixpksg.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
                ./configuration.nix
                datomic-pro.nixosModules.${system}.datomic-pro
            ];
        };
    };
}

```

### `/etc/datomic-pro/secrets.properties`

I wouldn't really do this in production I would use a tool like
[agenix](https://github.com/ryantm/agenix) or
[sops-nix](https://github.com/Mic92/sops-nix).


> [!IMPORTANT]
> Whatever you do, do not use
> [`environment.etc`](https://search.nixos.org/options?channel=24.05&show=environment.etc&from=0&size=50&sort=relevance&type=packages&query=environment.etc)
> to create the secret files! That will write the secrets into the globally
> readable nix store, and could end up on a nix cache somewhere. Bad news!

``` java-properties
# in `/etc/datomic-pro/secrets.properties`
storage-admin-password=changeme
storage-datomic-password=changeme
```

``` shell
# set permissions carefully, it just needs to be root owned
# even though datomic doesn't run as root (thanks systemd!)
chown root:root /etc/datomic-pro/secrets.properties
chmod 0600 /etc/datomic-pro/secrets.properties
```

### `configuration.nix`

A basic dev-mode datomic that stores its state in `/var/lib/datomic-pro`:

``` nix
{
    services.datomic-pro = {
        enable = true;
        package = pkgs.datomic-pro_1_0_7387;
        secretsFile = "/etc/datomic-pro/secrets.properties";
        settings = {
            # no secrets in here!
            enable = true;
            host = "localhost";
            port = 4334;
            memory-index-max = "256m";
            memory-index-threshold = "32m";
            object-cache-max = "128m";
            protocol = "dev";
            storage-access = "remote";
        };
    }
    # optionally add the console

    services.datomic-console = {
        enable = true;
        port = 8080;
        dbUriFile = ..path to secret file containing something like datomic:dev://localhost:4334/.....
    };

    # ... the rest of your owl ...
}
```

## 🐋 Usage - Docker (OCI) Container Image

This flake also provides a container image for Datomic Pro that can be driven entirely with environment variables or `_FILE` style secrets.

If you don't want to build the container image yourself with nix, you can get the latest image with:

``` shell
docker/podman pull ghcr.io/outskirtslabs/datomic-pro:1.0.7387
```

The available tags you can find here: https://github.com/orgs/outskirtslabs/packages/datomic-pro-flake/package/datomic-pro

### Transactor Mode

Runs a Datomic Transactor. This is the default mode when the container is run with no command.

* The default port is `4334`.
* A rw volume of `/config` is required.
* Configure with env vars (see below) or add `/config/transactor.properties` to supply a config to the transactor.
* A rw volume of `/data` is optional (for use in H2 mode).
* Postgresql, MySQL, and sqlite JDBC drivers are included by default.

#### Env vars

> [!IMPORTANT]
> All env vars can be passed with `_FILE` to read the value from a file
> (e.g, when using secrets). Example: `DATOMIC_STORAGE_ADMIN_PASSWORD` can be
> passed as `DATOMIC_STORAGE_ADMIN_PASSWORD_FILE=/run/secrets/admin-password` and
> the value from that file will be used as the admin password.

* `DATOMIC_TRANSACTOR_PROPERTIES_PATH` - The path to the properties file for the transactor. Defaults to `/config/transactor.properties`

The following environment vars configure the properties, refer to the Datomic documentation for more information:

* `DATOMIC_ALT_HOST` - `alt-host`
* `DATOMIC_DATA_DIR` - `data-dir` (default: /data)
* `DATOMIC_ENCRYPT_CHANNEL` - `encrypt-channel`
* `DATOMIC_HEARTBEAT_INTERVAL_MSEC` - `heartbeat-interval-msec`
* `DATOMIC_HOST` - `host` (default: 0.0.0.0)
* `DATOMIC_MEMCACHED` - `memcached`
* `DATOMIC_MEMCACHED_AUTO_DISCOVERY` - `memcached-auto-discovery`
* `DATOMIC_MEMCACHED_CONFIG_TIMEOUT_MSEC` - `memcached-config-timeout-msec`
* `DATOMIC_MEMCACHED_PASSWORD` - `memcached-password`
* `DATOMIC_MEMCACHED_USERNAME` - `memcached-username`
* `DATOMIC_MEMORY_INDEX_MAX` - `memory-index-max` (default: 256m)
* `DATOMIC_MEMORY_INDEX_THRESHOLD` - `memory-index-threshold` (default: 32m)
* `DATOMIC_OBJECT_CACHE_MAX` - `object-cache-max` (default: 128m)
* `DATOMIC_PID_FILE` - `pid-file`
* `DATOMIC_HEALTHCHECK_CONCURRENCY` - `ping-concurrency`
* `DATOMIC_HEALTHCHECK_HOST` - `ping-host`
* `DATOMIC_HEALTHCHECK_PORT` - `ping-port`
* `DATOMIC_PORT` - `port` (default: 4334)
* `DATOMIC_PROTOCOL` - `protocol` (default: dev)
* `DATOMIC_READ_CONCURRENCY` - `read-concurrency`
* `DATOMIC_SQL_DRIVER_CLASS` - `sql-driver-class`
* `DATOMIC_SQL_URL` - `sql-url`
* `DATOMIC_STORAGE_ACCESS` - `storage-access` (default: remote)
* `DATOMIC_STORAGE_ADMIN_PASSWORD` - `storage-admin-password`
* `DATOMIC_STORAGE_DATOMIC_PASSWORD` - `storage-datomic-password`
* `DATOMIC_VALCACHE_MAX_GB` - `valcache-max-gb`
* `DATOMIC_VALCACHE_PATH` - `valcache-path`
* `DATOMIC_WRITE_CONCURRENCY` - `write-concurrency`

If you want to provide your own `transactor.properties`, you can opt out of all of the above by:

1. Placing your properties file into a location such that `/config/transactor.properties` will exist when the container runs.
2. Set the env variable `DOCKER_DATOMIC_GENERATE_PROPERTIES_SKIP` to anything except an empty string.

### Console Mode

Runs the Datomic Console.

* Run this mode by passing the `console` as the first and only argument to the container.
* The default port is `8080`.

#### Env vars

* `DB_URI` - the database connection URI that console uses to connect to datomic
* `DB_URI_FILE` - will read the connection URI from the file specified by this env var

### Example Compose

#### Datomic Pro with Local Storage

Be sure to `mkdir data/ config/` before running this.

``` yaml
---
services:
  datomic-transactor:
    image: ghcr.io/outskirtslabs/datomic-pro:1.0.7387
    environment:
      DATOMIC_STORAGE_ADMIN_PASSWORD: unsafe
      DATOMIC_STORAGE_DATOMIC_PASSWORD: unsafe
    volumes:
      - ./data:/data
    ports:
      - 127.0.0.1:4334:4334
    #user: 1000:1000 # if using rootful containers uncomment this

  datomic-console:
    image: ghcr.io/outskirtslabs/datomic-pro:1.0.7387
    command: console
    environment:
      DB_URI: datomic:dev://datomic-transactor:4334/?password=unsafe
    ports:
      - 127.0.0.1:8081:8080
    #user: 1000:1000 # if using rootful containers uncomment this
```

#### Datomic Pro with SQLite Storage

1. `mkdir data config`
2. Prepare an empty SQLite database:

  ``` shell
  mkdir -p data/ config/
  sqlite3 data/datomic-sqlite.db "
  -- Tuning for SQLite in production - same as Rails 8.0
  PRAGMA foreign_keys = ON;
  PRAGMA journal_mode = WAL;
  PRAGMA synchronous = NORMAL;
  PRAGMA mmap_size = 134217728; -- 128 megabytes
  PRAGMA journal_size_limit = 67108864; -- 64 megabytes
  PRAGMA cache_size = 2000;
  -- Datomic's Schema
  CREATE TABLE datomic_kvs (
      id TEXT NOT NULL,
      rev INTEGER,
      map TEXT,
      val BYTEA,
      CONSTRAINT pk_id PRIMARY KEY (id)
  );"
  ```
  
3. Use this compose (or pick and choose what you need)

``` yaml
---
services:
  datomic-transactor:
    image: ghcr.io/outskirtslabs/datomic-pro:unstable
    environment:
      DATOMIC_PROTOCOL: sql
      DATOMIC_SQL_URL: jdbc:sqlite:/data/datomic-sqlite.db
      DATOMIC_SQL_DRIVER_CLASS: org.sqlite.JDBC
      DATOMIC_JAVA_OPTS: -Dlogback.configurationFile=/config/logback.xml
      DATOMIC_HOST: datomic-transactor # this value is so sibling compose containers can connect by DNS name
      DATOMIC_ALT_HOST: "127.0.0.1" # this value is so apps running on the container's host
    volumes:
      - "./data:/data:z"
      - "./config:/config:z"
    ports:
      - 127.0.0.1:4334:4334

  datomic-console:
    image: ghcr.io/outskirtslabs/datomic-pro:unstable
    command: console
    environment:
      # you don’t specify the db name in the uri (because console can access all dbs)
      DB_URI: "datomic:sql://?jdbc:sqlite:/data/datomic-sqlite.db"
    volumes:
      - "./data:/data:z"
    ports:
      - 127.0.0.1:8081:8080
```

#### Datomic Pro with Postgres Storage and memcached

This compose file is near-production ready. But you shouldn't manage the
lifecycle of the postgres schema this way. How you do it depends on your
environment.

Be sure to `mkdir data/ config/` before running this.

``` yaml
---
services:
  datomic-memcached:
    image: docker.io/memcached:latest
    command: memcached -m 1024
    ports:
      - 127.0.0.1:11211:11211
    restart: always
    healthcheck:
      test:
        [
          "CMD-SHELL",
          'bash -c ''echo "version" | (exec 3<>/dev/tcp/localhost/11211; cat >&3; timeout 0.1 cat <&3; exec 3<&-)''',
        ]
      interval: 5s
      retries: 60

  datomic-storage:
    image: docker.io/library/postgres:latest
    environment:
      POSTGRES_PASSWORD: unsafe
    command: postgres -c 'max_connections=1024'
    volumes:
      - ./data:/var/lib/postgresql/data
    ports:
      - 127.0.0.1:5432:5432
    restart: always
    healthcheck:
      test: ["CMD-SHELL", "pg_isready -U postgres"]
      interval: 5s
      timeout: 3s
      retries: 30

  datomic-storage-migrator:
    image: ghcr.io/outskirtslabs/datomic-pro:1.0.7387
    environment:
      PGUSER: postgres
      PGPASSWORD: unsafe
    volumes:
      - "./postgres-migrations:/migrations"
    entrypoint: /bin/sh
    command: >
      -c '(psql -h datomic-storage -lqt | cut -d \| -f 1 | grep -qw "datomic" || psql -h datomic-storage -f /opt/datomic-pro/bin/sql/postgres-db.sql) &&
             (psql -h datomic-storage -d datomic -c "\dt" | grep -q "datomic_kvs" || psql -h datomic-storage -d datomic -f /opt/datomic-pro/bin/sql/postgres-table.sql) &&
             (psql -h datomic-storage -d datomic -c "\du" | cut -d \| -f 1 | grep -qw "datomic" || psql -h datomic-storage -d datomic -f /opt/datomic-pro/bin/sql/postgres-user.sql)'
    depends_on:
      datomic-storage:
        condition: service_healthy

  datomic-transactor:
    image: ghcr.io/outskirtslabs/datomic-pro:1.0.7387
    environment:
      DATOMIC_STORAGE_ADMIN_PASSWORD: unsafe
      DATOMIC_STORAGE_DATOMIC_PASSWORD: unsafe
      DATOMIC_PROTOCOL: sql
      DATOMIC_SQL_URL: jdbc:postgresql://datomic-storage:5432/datomic?user=datomic&password=datomic
      DATOMIC_HEALTHCHECK_HOST: 127.0.0.1
      DATOMIC_HEALTHCHECK_PORT: 9999
      DATOMIC_MEMCACHED: datomic-memcached:11211
    ports:
      - 127.0.0.1:4334:4334
    #user: 1000:1000 # if using rootful containers uncomment this
    restart: always
    healthcheck:
      test:
        [
          "CMD-SHELL",
          'if [[ $(curl -s -o /dev/null -w "%{http_code}" -X GET http://127.0.0.1:9999/health)  = "200" ]]; then echo 0; else echo 1; fi',
        ]
      interval: 10s
      timeout: 3s
      retries: 30
    depends_on:
      datomic-storage:
        condition: service_healthy
      datomic-memcached:
        condition: service_healthy
      datomic-storage-migrator:
        condition: service_completed_successfully

  datomic-console:
    image: ghcr.io/outskirtslabs/datomic-pro:1.0.7387
    command: console
    environment:
      DB_URI: datomic:sql://?jdbc:postgresql://datomic-storage:5432/datomic?user=datomic&password=datomic
    ports:
      - 127.0.0.1:8081:8080
    #user: 1000:1000 # if using rootful containers uncomment this
```


### Discussion

Feel free to [open an issue](https://github.com/outskirtslabs/datomic-pro-flake/issues/new) or reach out to me on the Clojurians slack ([@Ramblurr](https://clojurians.slack.com/team/U70QFSCG2)).

## Security

See [here][sec] for security advisories or to report a security vulnerability.

[sec]: https://github.com/outskirtslabs/datomic-pro-flake/security/advisories

## License: Apache License 2.0

Copyright © 2025 Casey Link <casey@outskirtslabs.com>

Distributed under the [Apache-2.0](https://spdx.org/licenses/Apache-2.0.html), just like Datomic Pro itself.
