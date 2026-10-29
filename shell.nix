{
  pkgs ? import <nixpkgs> { },
}:
pkgs.mkShell {
  buildInputs = with pkgs; [
    stdenv.cc
    rustc
    cargo
    rustfmt
    clippy
    rust-analyzer
    biome
    tinymist
    openssl
    pkg-config
  ];
}
