let
  moz_overlay = import (
    builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz
  );
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  beta = nixpkgs.rustChannelOf { channel = "beta"; };
  extensions = [
      "clippy-preview"
      "rls-preview"
      "rustfmt-preview"
      "rust-analysis"
      "rust-std"
      "rust-src"
  ];
  rustbeta = (beta.rust.override {
    extensions = extensions;
  });
in
  with nixpkgs;
  mkShell {
    name = "rust";
    buildInputs = [
      autoconf
      automake
      cmake
      libtool
      openssl
      pkgconfig
      protobuf
      flatbuffers
      rustbeta
      zlib
      kcov
    ];

    PROTOC = "${protobuf}/bin/protoc";
  }
