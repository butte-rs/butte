let
  moz_overlay = import (
    builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz
  );
  nixpkgs = import <nixpkgs> { overlays = [ moz_overlay ]; };
  stable = nixpkgs.rustChannelOf { channel = "stable"; };
  extensions = [
      "clippy-preview"
      "rls-preview"
      "rustfmt-preview"
      "rust-analysis"
      "rust-std"
      "rust-src"
  ];
  rust-stable = stable.rust.override { extensions = extensions; };
in with nixpkgs;
  mkShell {
    name = "rust-stable";
    buildInputs = [ rust-stable protobuf flatbuffers ];
    PROTOC = "${protobuf}/bin/protoc";
  }
