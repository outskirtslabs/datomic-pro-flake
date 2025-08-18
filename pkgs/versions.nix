{ pkgs, ... }:
rec {
  # Note: the latest version must be the first one in this file
  #       because the ci pipeline detects the "current" version that way
  datomic-pro_1_0_7394 = pkgs.callPackage ./datomic-pro.nix {
    version = "1.0.7394";
    hash = "sha256-KoW6OqoQHgaGQytglHOaUdC5iqnCRKp+Klv5Ti+UAfI=";
  };
  datomic-pro_1_0_7387 = pkgs.callPackage ./datomic-pro.nix {
    version = "1.0.7387";
    hash = "sha256-xHMQeLmLLk1a/5SyIselMf3SwJYyY7JYEV1/F200Hdo=";
  };
  datomic-pro_1_0_7364 = pkgs.callPackage ./datomic-pro.nix {
    version = "1.0.7364";
    hash = "sha256-y9jSA4s0aEn9QheZ3YBDtx8wTqOv+9XJXwzyW9R5T4w=";
  };
  datomic-pro_1_0_7277 = pkgs.callPackage ./datomic-pro.nix {
    version = "1.0.7277";
    hash = "sha256-fqmw+MOUWPCAhHMROjP48BwWCcRknk+KECM3WvF/Ml4=";
  };
  datomic-pro = datomic-pro_1_0_7394;
  datomic-pro-peer_1_0_7394 = pkgs.callPackage ./datomic-pro-peer.nix {
    version = "1.0.7394";
    mvnHash = "sha256-zoRBD41qnaV/XP9qwEYxFdn2JH6LR9udDCCTsYacY74=";
    zipHash = "sha256-KoW6OqoQHgaGQytglHOaUdC5iqnCRKp+Klv5Ti+UAfI=";
  };
  datomic-pro-peer_1_0_7387 = pkgs.callPackage ./datomic-pro-peer.nix {
    version = "1.0.7387";
    mvnHash = "sha256-zoRBD41qnaV/XP9qwEYxFdn2JH6LR9udDCCTsYacY74=";
    zipHash = "sha256-xHMQeLmLLk1a/5SyIselMf3SwJYyY7JYEV1/F200Hdo=";
  };
  datomic-pro-peer_1_0_7364 = pkgs.callPackage ./datomic-pro-peer.nix {
    version = "1.0.7364";
    mvnHash = "sha256-5QpAlC20mo0IZHoRjiCS3zOCTbM7xM8gHc6n+S42iu0=";
    zipHash = "sha256-y9jSA4s0aEn9QheZ3YBDtx8wTqOv+9XJXwzyW9R5T4w=";
  };
  datomic-pro-peer_1_0_7277 = pkgs.callPackage ./datomic-pro-peer.nix {
    version = "1.0.7277";
    mvnHash = "sha256-09AKaahc4MSc0d/gWJyMpB60O7WZOauj7vS1X4rtPjI=";
    zipHash = "sha256-fqmw+MOUWPCAhHMROjP48BwWCcRknk+KECM3WvF/Ml4=";
  };
  datomic-pro-peer = datomic-pro-peer_1_0_7394;
}
