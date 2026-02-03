(ns release
  (:require [babashka.process :refer [shell]]
            [clojure.string :as str]))

(defn fetch-latest-version
  []
  (let [{:keys [out exit]}
        (shell {:out :string :err :string}
               "curl" "-s"
               "https://repo.maven.apache.org/maven2/com/datomic/peer/maven-metadata.xml")]
    (when (zero? exit)
      (second (re-find #"<latest>([^<]+)</latest>" out)))))

(defn current-version
  []
  (let [{:keys [out exit]}
        (shell {:out :string :err :string}
               "nix" "eval" "--raw" ".#datomic-pro.version")]
    (when (zero? exit)
      (str/trim out))))

(defn prefetch-hash
  [version]
  (let [url (format "https://datomic-pro-downloads.s3.amazonaws.com/%s/datomic-pro-%s.zip"
                    version version)
        {:keys [out exit]}
        (shell {:out :string :err :string}
               "nix-prefetch-url" "--unpack" "--type" "sha256" url)]
    (when (zero? exit)
      (let [base32-hash (str/trim out)
            {:keys [out exit]}
            (shell {:out :string :err :string}
                   "nix" "hash" "convert" "--hash-algo" "sha256" "--to" "sri" base32-hash)]
        (when (zero? exit)
          (str/trim out))))))

(defn version->attr
  [version]
  (str/replace version "." "_"))

(defn datomic-pro-entry
  [version hash]
  (let [attr (version->attr version)]
    (format "  datomic-pro_%s = pkgs.callPackage ./datomic-pro.nix {
    version = \"%s\";
    hash = \"%s\";
  };" attr version hash)))

(defn datomic-peer-entry
  [version mvn-hash zip-hash]
  (let [attr (version->attr version)]
    (format "  datomic-pro-peer_%s = pkgs.callPackage ./datomic-pro-peer.nix {
    version = \"%s\";
    mvnHash = \"%s\";
    zipHash = \"%s\";
  };" attr version mvn-hash zip-hash)))

(defn extract-versions
  [content]
  (let [matches (re-seq #"datomic-pro_(\d+_\d+_\d+)\s*=" content)]
    (->> matches
         (map second)
         distinct
         (map #(str/replace % "_" "."))
         vec)))

(defn extract-peer-versions
  [content]
  (let [matches (re-seq #"datomic-pro-peer_(\d+_\d+_\d+)\s*=" content)]
    (->> matches
         (map second)
         distinct
         (map #(str/replace % "_" "."))
         vec)))

(defn update-versions-content
  [content new-version zip-hash mvn-hash]
  (let [attr (version->attr new-version)
        new-pro-entry (datomic-pro-entry new-version zip-hash)
        new-peer-entry (datomic-peer-entry new-version mvn-hash zip-hash)
        lines (str/split-lines content)
        header-end (inc (.indexOf lines "rec {"))
        comment-end (loop [i header-end]
                      (if (str/starts-with? (str/trim (nth lines i "")) "#")
                        (recur (inc i))
                        i))
        before-entries (subvec (vec lines) 0 comment-end)
        rest-content (str/join "\n" (subvec (vec lines) comment-end))
        pro-alias-pattern #"(\s*datomic-pro\s*=\s*)datomic-pro_\d+_\d+_\d+;"
        peer-alias-pattern #"(\s*datomic-pro-peer\s*=\s*)datomic-pro-peer_\d+_\d+_\d+;"
        first-peer-pattern #"(?m)^(\s*datomic-pro-peer_\d+_\d+_\d+\s*=)"
        updated-rest (-> rest-content
                         (str/replace pro-alias-pattern
                                      (str "$1datomic-pro_" attr ";"))
                         (str/replace peer-alias-pattern
                                      (str "$1datomic-pro-peer_" attr ";"))
                         (str/replace-first first-peer-pattern
                                            (str new-peer-entry "\n$1")))]
    (str (str/join "\n" before-entries)
         "\n"
         new-pro-entry
         "\n"
         updated-rest)))

(defn update-readme-content
  [content versions peer-versions new-version]
  (let [attr (version->attr new-version)
        all-versions (distinct (cons new-version versions))
        all-peer-versions (distinct (cons new-version peer-versions))
        pro-list (->> all-versions
                      (map-indexed (fn [i v]
                                     (if (zero? i)
                                       (format "-  `pkgs.datomic-pro_%s` (latest)" (version->attr v))
                                       (format "-  `pkgs.datomic-pro_%s`" (version->attr v)))))
                      (str/join "\n"))
        peer-list (->> all-peer-versions
                       (map-indexed (fn [i v]
                                      (if (zero? i)
                                        (format "-  `pkgs.datomic-pro-peer_%s` (latest)" (version->attr v))
                                        (format "-  `pkgs.datomic-pro-peer_%s`" (version->attr v)))))
                       (str/join "\n"))
        pro-section-pattern #"(?s)(`pkgs\.datomic-pro` will always be the latest release, but the following specific versions are also available:\n\n)(-  `pkgs\.datomic-pro_[^`]+`[^\n]*\n)+"
        peer-section-pattern #"(?s)(And for peer:\n\n)(-  `pkgs\.datomic-pro-peer_[^`]+`[^\n]*\n)+"
        docker-tag-pattern #"ghcr\.io/outskirtslabs/datomic-pro:\d+\.\d+\.\d+"
        config-pkg-pattern #"package = pkgs\.datomic-pro_\d+_\d+_\d+;"]
    (-> content
        (str/replace pro-section-pattern (str "$1" pro-list "\n"))
        (str/replace peer-section-pattern (str "$1" peer-list "\n"))
        (str/replace docker-tag-pattern (str "ghcr.io/outskirtslabs/datomic-pro:" new-version))
        (str/replace config-pkg-pattern (str "package = pkgs.datomic-pro_" attr ";")))))

(defn next-flake-version
  [changelog-content]
  (let [version-pattern #"## v(\d+)\.(\d+)\.(\d+)"
        match (re-find version-pattern changelog-content)]
    (when match
      (let [major (parse-long (nth match 1))
            minor (parse-long (nth match 2))]
        (format "v%d.%d.0" major (inc minor))))))

(defn generate-changelog-entry
  [flake-version datomic-version versions peer-versions date]
  (let [all-versions (distinct (cons datomic-version versions))
        all-peer-versions (distinct (cons datomic-version peer-versions))
        pro-list (->> all-versions
                      (map-indexed (fn [i v]
                                     (if (zero? i)
                                       (format "-  `pkgs.datomic-pro_%s` (latest)" (version->attr v))
                                       (format "-  `pkgs.datomic-pro_%s`" (version->attr v)))))
                      (str/join "\n"))
        peer-list (->> all-peer-versions
                       (map-indexed (fn [i v]
                                      (if (zero? i)
                                        (format "-  `pkgs.datomic-pro-peer_%s` (latest)" (version->attr v))
                                        (format "-  `pkgs.datomic-pro-peer_%s`" (version->attr v)))))
                       (str/join "\n"))]
    (format "## %s (%s)

This is a version bump release:

- Added package versions for [version %s](https://docs.datomic.com/changes/pro.html#%s)

`pkgs.datomic-pro` will always be the latest release, but the following specific versions are also available:

%s

And for peer:

%s"
            flake-version date datomic-version datomic-version pro-list peer-list)))

(defn has-unreleased-content?
  [content]
  (let [lines (str/split-lines content)
        unreleased-idx (->> lines
                            (map-indexed vector)
                            (filter #(str/starts-with? (second %) "## [UNRELEASED]"))
                            first
                            first)
        next-version-idx (->> lines
                              (map-indexed vector)
                              (filter #(and (> (first %) (or unreleased-idx -1))
                                            (re-matches #"## v\d+\.\d+\.\d+.*" (second %))))
                              first
                              first)]
    (when (and unreleased-idx next-version-idx)
      (let [between (subvec (vec lines) (inc unreleased-idx) next-version-idx)]
        (some #(not (str/blank? %)) between)))))

(defn update-changelog-content
  [content flake-version datomic-version versions peer-versions date]
  (when (has-unreleased-content? content)
    (throw (ex-info "CHANGELOG.md has unreleased notes. Please release those manually first."
                    {:type :unreleased-content})))
  (let [entry (generate-changelog-entry flake-version datomic-version versions peer-versions date)
        before-version-pattern #"(?m)^(## v\d+\.\d+\.\d+)"]
    (str/replace-first content before-version-pattern
                       (str entry "\n\n$1"))))

(defn today-date
  []
  (let [now (java.time.LocalDate/now)
        formatter (java.time.format.DateTimeFormatter/ofPattern "yyyy-MM-dd")]
    (.format now formatter)))

(defn update-nixos-module-content
  [content versions]
  (let [latest-5 (take 5 versions)
        related-packages (->> latest-5
                              (map #(str "          \"datomic-pro_" (version->attr %) "\""))
                              (str/join "\n"))
        related-pattern #"(?s)(relatedPackages = \[\n)(.*?)(\n\s*\];)"]
    (str/replace content related-pattern (str "$1" related-packages "$3"))))

(defn get-mvn-hash
  [version zip-hash]
  (println "Building peer package to get mvnHash (this will fail and show the correct hash)...")
  (let [{:keys [err exit]}
        (shell {:out :string :err :string :continue true}
               "nix" "build" (str ".#datomic-pro-peer_" (version->attr version)))]
    (if (zero? exit)
      (do
        (println "Build succeeded unexpectedly - mvnHash may not have changed")
        nil)
      (let [hash-match (re-find #"got:\s+(sha256-[A-Za-z0-9+/=]+)" err)]
        (when hash-match
          (second hash-match))))))

(defn release!
  [{:keys [build?]}]
  (println "Checking for new Datomic version...")
  (let [latest (fetch-latest-version)
        current (current-version)]
    (println "Latest:" latest)
    (println "Current:" current)
    (if (= latest current)
      (println "Already up to date!")
      (do
        (println "New version available:" latest)
        (println "Fetching zip hash...")
        (let [zip-hash (prefetch-hash latest)]
          (if-not zip-hash
            (println "Failed to fetch zip hash!")
            (do
              (println "Zip hash:" zip-hash)
              (println "Updating versions.nix...")
              (let [versions-path "pkgs/versions.nix"
                    versions-content (slurp versions-path)
                    existing-versions (extract-versions versions-content)
                    existing-peer-versions (extract-peer-versions versions-content)
                    mvn-hash (or (get-mvn-hash latest zip-hash)
                                 (do (println "Using previous mvnHash (likely unchanged)")
                                     "sha256-zoRBD41qnaV/XP9qwEYxFdn2JH6LR9udDCCTsYacY74="))
                    updated-versions (update-versions-content versions-content latest zip-hash mvn-hash)]
                (spit versions-path updated-versions)
                (println "Updated versions.nix")

                (println "Updating README.md...")
                (let [readme-path "README.md"
                      readme-content (slurp readme-path)
                      updated-readme (update-readme-content readme-content existing-versions existing-peer-versions latest)]
                  (spit readme-path updated-readme)
                  (println "Updated README.md"))

                (println "Updating CHANGELOG.md...")
                (let [changelog-path "CHANGELOG.md"
                      changelog-content (slurp changelog-path)
                      flake-version (next-flake-version changelog-content)
                      updated-changelog (update-changelog-content changelog-content
                                                                  flake-version
                                                                  latest
                                                                  existing-versions
                                                                  existing-peer-versions
                                                                  (today-date))]
                  (spit changelog-path updated-changelog)
                  (println "Updated CHANGELOG.md"))

                (println "Updating nixos-modules/datomic-pro.nix...")
                (let [module-path "nixos-modules/datomic-pro.nix"
                      module-content (slurp module-path)
                      all-versions (distinct (cons latest existing-versions))
                      updated-module (update-nixos-module-content module-content all-versions)]
                  (spit module-path updated-module)
                  (println "Updated nixos-modules/datomic-pro.nix"))

                (when build?
                  (println "Building packages to verify...")
                  (shell "nix" "build" ".#datomic-pro")
                  (println "datomic-pro built successfully")
                  (shell "nix" "build" ".#datomic-pro-peer")
                  (println "datomic-pro-peer built successfully")
                  (shell "nix" "build" ".#datomic-pro-container" "-o" "container")
                  (println "Container image built successfully"))

                (println "Done!")))))))))
