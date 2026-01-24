#!/usr/bin/env bb

(ns release-test
  (:require [clojure.test :refer [deftest is testing run-tests]]
            [clojure.string :as str]))

(load-file "bb/release.bb")

(deftest version->attr-test
  (is (= "1_0_7482" (release/version->attr "1.0.7482")))
  (is (= "1_0_7469" (release/version->attr "1.0.7469"))))

(deftest make-download-url-test
  (is (= "https://datomic-pro-downloads.s3.amazonaws.com/1.0.7482/datomic-pro-1.0.7482.zip"
         (release/make-download-url "1.0.7482"))))

(deftest datomic-pro-entry-test
  (let [entry (release/datomic-pro-entry "1.0.9999" "sha256-abc123")]
    (is (str/includes? entry "datomic-pro_1_0_9999"))
    (is (str/includes? entry "version = \"1.0.9999\""))
    (is (str/includes? entry "hash = \"sha256-abc123\""))))

(deftest datomic-peer-entry-test
  (let [entry (release/datomic-peer-entry "1.0.9999" "sha256-xyz789")]
    (is (str/includes? entry "datomic-pro-peer_1_0_9999"))
    (is (str/includes? entry "version = \"1.0.9999\""))
    (is (str/includes? entry "zipHash = \"sha256-xyz789\""))
    (is (str/includes? entry "mvnHash = \"\""))))

(def sample-versions-nix
  "{ pkgs, ... }:
rec {
  # Note: the latest version must be the first one in this file
  #       because the ci pipeline detects the \"current\" version that way
  datomic-pro_1_0_7469 = pkgs.callPackage ./datomic-pro.nix {
    version = \"1.0.7469\";
    hash = \"sha256-OgUuDc1sFpAP4Spx/ca2u8LsrrQK2X4cwB0ve+lQcBg=\";
  };
  datomic-pro = datomic-pro_1_0_7469;
  datomic-pro-peer_1_0_7469 = pkgs.callPackage ./datomic-pro-peer.nix {
    version = \"1.0.7469\";
    mvnHash = \"sha256-zoRBD41qnaV/XP9qwEYxFdn2JH6LR9udDCCTsYacY74=\";
    zipHash = \"sha256-OgUuDc1sFpAP4Spx/ca2u8LsrrQK2X4cwB0ve+lQcBg=\";
  };
  datomic-pro-peer = datomic-pro-peer_1_0_7469;
}")

(deftest update-versions-content-test
  (let [updated (release/update-versions-content sample-versions-nix "1.0.9999" "sha256-newhash")]
    
    (testing "inserts new datomic-pro entry at top"
      (is (str/includes? updated "datomic-pro_1_0_9999"))
      (is (< (str/index-of updated "datomic-pro_1_0_9999")
             (str/index-of updated "datomic-pro_1_0_7469"))))
    
    (testing "updates datomic-pro alias"
      (is (str/includes? updated "datomic-pro = datomic-pro_1_0_9999;"))
      (is (not (re-find #"datomic-pro = datomic-pro_1_0_7469;" updated))))
    
    (testing "inserts new peer entry"
      (is (str/includes? updated "datomic-pro-peer_1_0_9999")))
    
    (testing "updates datomic-pro-peer alias"
      (is (str/includes? updated "datomic-pro-peer = datomic-pro-peer_1_0_9999;"))
      (is (not (re-find #"datomic-pro-peer = datomic-pro-peer_1_0_7469;" updated))))
    
    (testing "preserves old entries"
      (is (str/includes? updated "datomic-pro_1_0_7469"))
      (is (str/includes? updated "datomic-pro-peer_1_0_7469")))))

(deftest insert-pro-entry-test
  (let [result (release/insert-pro-entry sample-versions-nix "1.0.9999" "sha256-test")]
    (testing "new entry appears after the ci comment"
      (let [comment-pos (str/index-of result "ci pipeline detects")
            entry-pos (str/index-of result "datomic-pro_1_0_9999")]
        (is (some? entry-pos))
        (is (< comment-pos entry-pos))))))

(deftest update-pro-alias-test
  (let [result (release/update-pro-alias sample-versions-nix "1.0.9999")]
    (is (str/includes? result "datomic-pro = datomic-pro_1_0_9999;"))))

(deftest update-peer-alias-test
  (let [result (release/update-peer-alias sample-versions-nix "1.0.9999")]
    (is (str/includes? result "datomic-pro-peer = datomic-pro-peer_1_0_9999;"))))

(when (= *file* (System/getProperty "babashka.file"))
  (run-tests 'release-test))
