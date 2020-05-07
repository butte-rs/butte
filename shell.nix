let
  moz_overlay = import (
    builtins.fetchTarball https://github.com/mozilla/nixpkgs-mozilla/archive/master.tar.gz
  );
  pkgs = import <nixpkgs> {
    overlays = [ moz_overlay ];
  };
  extensions = [
    "clippy-preview"
    "rls-preview"
    "rustfmt-preview"
    "rust-analysis"
    "rust-std"
    "rust-src"
  ];
  mkRustShell = channel: {
    ${channel} = pkgs.mkShell {
      name = "rust-${channel}-shell";
      buildInputs = [
        ((pkgs.rustChannelOf { inherit channel; }).rust.override { inherit extensions; })
        pkgs.cargo-audit
      ];
    };
  };
  channels = [
    "stable"
    "beta"
    "nightly"
  ];
in
pkgs.lib.fold (left: right: left // right) {} (builtins.map mkRustShell channels)
