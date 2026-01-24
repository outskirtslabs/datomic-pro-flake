#!/usr/bin/env bb

(ns release
  (:require [babashka.process :refer [shell sh]]
            [clojure.string :as str]))

;; Version fetching

(defn fetch-latest-version
  "Fetch latest version from Maven Central"
  []
  (-> (sh "curl" "-s" "https://repo.maven.apache.org/maven2/com/datomic/peer/maven-metadata.xml")
      :out
      (->> (re-find #"<latest>([^<]+)</latest>"))
      second))

(defn current-version
  "Get current version from flake"
  []
  (-> (sh "nix" "eval" "--raw" ".#datomic-pro.version")
      :out
      str/trim))

;; Hash computation

(defn make-download-url [version]
  (format "https://datomic-pro-downloads.s3.amazonaws.com/%s/datomic-pro-%s.zip" version version))

(defn nix-prefetch [url]
  (-> (sh "nix-prefetch-url" "--unpack" url)
      :out
      str/trim))

(defn nix32->sri [nix32-hash]
  (-> (sh "nix" "hash" "convert" "--hash-algo" "sha256" "--to" "sri" nix32-hash)
      :out
      str/trim))

(defn prefetch-hash [version]
  (-> version make-download-url nix-prefetch nix32->sri))

;; Nix entry generation

(defn version->attr [v]
  (str/replace v "." "_"))

(defn datomic-pro-entry [version hash]
  (format "  datomic-pro_%s = pkgs.callPackage ./datomic-pro.nix {
    version = \"%s\";
    hash = \"%s\";
  };"
          (version->attr version) version hash))

(defn datomic-peer-entry [version zip-hash]
  (format "  datomic-pro-peer_%s = pkgs.callPackage ./datomic-pro-peer.nix {
    version = \"%s\";
    mvnHash = \"\";
    zipHash = \"%s\";
  };"
          (version->attr version) version zip-hash))

;; File updates

(defn insert-pro-entry [content version hash]
  (str/replace content
               #"(#\s+because the ci pipeline detects.*\n)"
               (str "$1" (datomic-pro-entry version hash) "\n")))

(defn update-pro-alias [content version]
  (str/replace content
               #"datomic-pro = datomic-pro_[0-9_]+;"
               (format "datomic-pro = datomic-pro_%s;" (version->attr version))))

(defn insert-peer-entry [content version hash]
  (str/replace content
               #"(datomic-pro = datomic-pro_[0-9_]+;)\n"
               (str "$1\n" (datomic-peer-entry version hash) "\n")))

(defn update-peer-alias [content version]
  (str/replace content
               #"datomic-pro-peer = datomic-pro-peer_[0-9_]+;"
               (format "datomic-pro-peer = datomic-pro-peer_%s;" (version->attr version))))

(defn update-versions-content [content version hash]
  (-> content
      (insert-pro-entry version hash)
      (update-pro-alias version)
      (insert-peer-entry version hash)
      (update-peer-alias version)))

(defn update-versions-file! [version hash]
  (let [file "pkgs/versions.nix"]
    (->> (slurp file)
         (update-versions-content version hash)
         (spit file))))

;; Build verification

(defn build-pro! []
  (println "Building datomic-pro...")
  (shell "nix build .#datomic-pro --no-link"))

(defn build-peer! []
  (println "Building datomic-pro-peer...")
  (println "(If mvnHash fails, check error for expected hash)")
  (shell "nix build .#datomic-pro-peer --no-link"))

;; Main

(defn release!
  "Check for new version and update flake"
  [{:keys [build?]}]
  (let [latest (fetch-latest-version)
        current (current-version)]
    (println "Latest version: " latest)
    (println "Current version:" current)
    (if (= latest current)
      (do (println "Already up to date.") false)
      (do
        (println "\nNew version available! Fetching hash...")
        (let [hash (prefetch-hash latest)]
          (println "Hash:" hash)
          (println "\nUpdating pkgs/versions.nix...")
          (update-versions-file! latest hash)
          (println "Done!")
          (when build?
            (build-pro!)
            (build-peer!))
          true)))))

(when (= *file* (System/getProperty "babashka.file"))
  (release! {:build? (contains? (set *command-line-args*) "--build")}))
