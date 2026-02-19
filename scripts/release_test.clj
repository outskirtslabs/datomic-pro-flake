(ns release-test
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [release]))

(deftest version->attr-test
  (testing "converts version string to nix attribute format"
    (is (= "1_0_7482" (release/version->attr "1.0.7482")))
    (is (= "1_0_7469" (release/version->attr "1.0.7469")))))

(deftest datomic-pro-entry-test
  (testing "generates datomic-pro nix entry"
    (let [entry (release/datomic-pro-entry "1.0.7500" "sha256-abc123=")]
      (is (str/includes? entry "datomic-pro_1_0_7500"))
      (is (str/includes? entry "version = \"1.0.7500\""))
      (is (str/includes? entry "hash = \"sha256-abc123=\"")))))

(deftest datomic-peer-entry-test
  (testing "generates datomic-pro-peer nix entry"
    (let [entry (release/datomic-peer-entry "1.0.7500" "sha256-mvn=" "sha256-zip=")]
      (is (str/includes? entry "datomic-pro-peer_1_0_7500"))
      (is (str/includes? entry "version = \"1.0.7500\""))
      (is (str/includes? entry "mvnHash = \"sha256-mvn=\""))
      (is (str/includes? entry "zipHash = \"sha256-zip=\"")))))

(deftest extract-versions-test
  (testing "extracts versions from versions.nix content"
    (let [content "{ pkgs, ... }:
rec {
  datomic-pro_1_0_7482 = pkgs.callPackage ./datomic-pro.nix {
    version = \"1.0.7482\";
  };
  datomic-pro_1_0_7469 = pkgs.callPackage ./datomic-pro.nix {
    version = \"1.0.7469\";
  };
  datomic-pro = datomic-pro_1_0_7482;
}"
          versions (release/extract-versions content)]
      (is (= ["1.0.7482" "1.0.7469"] versions)))))

(deftest extract-peer-versions-test
  (testing "extracts peer versions from versions.nix content"
    (let [content "{ pkgs, ... }:
rec {
  datomic-pro-peer_1_0_7482 = pkgs.callPackage ./datomic-pro-peer.nix {};
  datomic-pro-peer_1_0_7469 = pkgs.callPackage ./datomic-pro-peer.nix {};
  datomic-pro-peer = datomic-pro-peer_1_0_7482;
}"
          versions (release/extract-peer-versions content)]
      (is (= ["1.0.7482" "1.0.7469"] versions)))))

(deftest next-flake-version-test
  (testing "increments minor version"
    (is (= "v0.11.0" (release/next-flake-version "== [UNRELEASED]\n\n== v0.10.0 (2026-02-03)")))
    (is (= "v0.10.0" (release/next-flake-version "== [UNRELEASED]\n\n== v0.9.0 (2025-12-14)")))
    (is (= "v1.1.0" (release/next-flake-version "== [UNRELEASED]\n\n== v1.0.0 (2026-01-01)")))))

(deftest generate-changelog-entry-test
  (testing "generates proper changelog entry"
    (let [entry (release/generate-changelog-entry "v0.11.0" "1.0.7500"
                                                  ["1.0.7482" "1.0.7469"]
                                                  ["1.0.7482" "1.0.7469"]
                                                  "2026-02-03")]
      (is (str/includes? entry "== v0.11.0 (2026-02-03)"))
      (is (str/includes? entry "https://docs.datomic.com/changes/pro.html#1.0.7500[version 1.0.7500]"))
      (is (str/includes? entry "datomic-pro_1_0_7500` (latest)"))
      (is (str/includes? entry "datomic-pro_1_0_7482`"))
      (is (str/includes? entry "datomic-pro-peer_1_0_7500` (latest)"))
      (is (str/includes? entry "datomic-pro-peer_1_0_7482`")))))

(deftest update-changelog-content-test
  (testing "inserts new entry after UNRELEASED"
    (let [original "= Changelog

== [UNRELEASED]

== v0.10.0 (2026-02-03)

Previous content here."
          updated (release/update-changelog-content original "v0.11.0" "1.0.7500"
                                                    ["1.0.7482"] ["1.0.7482"] "2026-02-04")]
      (is (str/includes? updated "== [UNRELEASED]"))
      (is (str/includes? updated "== v0.11.0 (2026-02-04)"))
      (is (str/includes? updated "== v0.10.0 (2026-02-03)"))
      (is (< (.indexOf updated "UNRELEASED")
             (.indexOf updated "v0.11.0")
             (.indexOf updated "v0.10.0"))))))

(deftest update-versions-content-test
  (testing "adds new version entries and updates aliases"
    (let [original "{ pkgs, ... }:
rec {
  # Note: the latest version must be the first one in this file
  datomic-pro_1_0_7482 = pkgs.callPackage ./datomic-pro.nix {
    version = \"1.0.7482\";
    hash = \"sha256-old=\";
  };
  datomic-pro = datomic-pro_1_0_7482;
  datomic-pro-peer_1_0_7482 = pkgs.callPackage ./datomic-pro-peer.nix {
    version = \"1.0.7482\";
    mvnHash = \"sha256-mvn=\";
    zipHash = \"sha256-old=\";
  };
  datomic-pro-peer = datomic-pro-peer_1_0_7482;
}"
          updated (release/update-versions-content original "1.0.7500" "sha256-new=" "sha256-newmvn=")]
      (is (str/includes? updated "datomic-pro_1_0_7500"))
      (is (str/includes? updated "datomic-pro = datomic-pro_1_0_7500;"))
      (is (str/includes? updated "datomic-pro-peer_1_0_7500"))
      (is (str/includes? updated "datomic-pro-peer = datomic-pro-peer_1_0_7500;"))
      (is (< (.indexOf updated "datomic-pro_1_0_7500")
             (.indexOf updated "datomic-pro_1_0_7482"))))))

(deftest update-readme-content-test
  (testing "updates version lists and docker tags"
    (let [original "`pkgs.datomic-pro` will always be the latest release, but the following specific versions are also available:

-  `pkgs.datomic-pro_1_0_7482` (latest)
-  `pkgs.datomic-pro_1_0_7469`

And for peer:

-  `pkgs.datomic-pro-peer_1_0_7482` (latest)
-  `pkgs.datomic-pro-peer_1_0_7469`

docker pull ghcr.io/outskirtslabs/datomic-pro:1.0.7482

package = pkgs.datomic-pro_1_0_7482;"
          updated (release/update-readme-content original
                                                 ["1.0.7482" "1.0.7469"]
                                                 ["1.0.7482" "1.0.7469"]
                                                 "1.0.7500")]
      (is (str/includes? updated "datomic-pro_1_0_7500` (latest)"))
      (is (str/includes? updated "datomic-pro-peer_1_0_7500` (latest)"))
      (is (str/includes? updated "ghcr.io/outskirtslabs/datomic-pro:1.0.7500"))
      (is (str/includes? updated "package = pkgs.datomic-pro_1_0_7500;")))))

(deftest update-readme-content-nixos-module-format-test
  (testing "updates versions and package pin in the split nixos module page format"
    (let [original "Specific versions are also exposed, for example:

* `pkgs.datomic-pro_1_0_7482`
* `pkgs.datomic-pro_1_0_7469`
* `pkgs.datomic-pro-peer_1_0_7482`
* `pkgs.datomic-pro-peer_1_0_7469`

New upstream Datomic releases are typically added within 24 hours.

package = pkgs.datomic-pro_1_0_7482;

docker pull ghcr.io/outskirtslabs/datomic-pro:1.0.7482"
          updated (release/update-readme-content original
                                                 ["1.0.7482" "1.0.7469"]
                                                 ["1.0.7482" "1.0.7469"]
                                                 "1.0.7500")]
      (is (str/includes? updated "datomic-pro_1_0_7500` (latest)"))
      (is (str/includes? updated "And for peer:"))
      (is (str/includes? updated "datomic-pro-peer_1_0_7500` (latest)"))
      (is (str/includes? updated "package = pkgs.datomic-pro_1_0_7500;"))
      (is (str/includes? updated "ghcr.io/outskirtslabs/datomic-pro:1.0.7500")))))
