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
    openssl
    pkg-config
  ];
}
