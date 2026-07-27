# Datomic Pro Packaging

This context covers how `datomic-pro-flake` names upstream Datomic Pro runtime roles, supported versions, and published distribution artifacts. It keeps an upstream Datomic Pro release distinct from a release of this flake.

## Datomic runtime

**Datomic Pro**:
The upstream on-premises Datomic distribution packaged by this project.
_Avoid_: Datomic Cloud, Datomic Local, bare Datomic when the edition matters

**Datomic Pro release**:
An upstream Datomic Pro version that provides matching Transactor and Peer artifacts.
_Avoid_: Flake release, package release, bare release

**Transactor**:
The Datomic Pro process that accepts transactions and coordinates indexing and durable storage.
_Avoid_: Datomic server, database server

**Peer**:
The application-side Datomic runtime that works with database values and submits transactions to a Transactor.
_Avoid_: Client, peer server

**Datomic Console**:
The browser-based administration interface for inspecting Datomic databases through a configured database URI.
_Avoid_: Web console, admin console

**Storage protocol**:
The Datomic configuration choice that identifies how a Transactor uses storage, such as `dev` or `sql`.
_Avoid_: Storage backend, storage mode

## Package distribution

**Supported Datomic release**:
A Datomic Pro release for which this project provides matching versioned Transactor and Peer packages.
_Avoid_: Flake release, current release

**Current Datomic release**:
The newest supported Datomic release. Unversioned package aliases point to it, and its version names the versioned OCI image tag.
_Avoid_: Latest release, current flake release

**Transactor package**:
The `datomic-pro` Nix package for a supported Datomic release. It contains the Transactor and the accompanying programs from the upstream distribution, including Datomic Console.
_Avoid_: Datomic package, server package

**Peer package**:
The `datomic-pro-peer` Nix package for a supported Datomic release. It supplies the Peer library and its runtime dependencies for applications.
_Avoid_: Client package

**Versioned package**:
A Transactor package or Peer package whose attribute name pins one supported Datomic release.
_Avoid_: Latest package, unversioned package

**Unversioned package alias**:
The moving `datomic-pro` or `datomic-pro-peer` package name that selects the current Datomic release.
_Avoid_: Versioned package, pinned package

## Container distribution

**OCI image**:
The published container form of the Transactor package. It can run in Transactor mode or Console mode.
_Avoid_: Docker image, container package

**Transactor mode**:
The OCI image's default role, which runs the Transactor.
_Avoid_: Server mode, default mode

**Console mode**:
The OCI image role that runs Datomic Console.
_Avoid_: Web mode, admin mode

**Versioned image tag**:
An OCI image tag named for a supported Datomic release and containing that release.
_Avoid_: Stable tag, latest tag

**Unstable image tag**:
The `unstable` OCI image tag that follows the repository's `main` branch.
_Avoid_: Latest tag, versioned tag

## Project releases

**Flake release**:
A versioned release of `datomic-pro-flake`. It can change packaging independently of the upstream Datomic Pro release cadence.
_Avoid_: Datomic Pro release, package release, bare release

**Version bump release**:
A Flake release whose principal change adds a supported Datomic release and makes it current.
_Avoid_: Datomic release, upstream release
