{
  pkgs ? import <nixpkgs> { },
}:
pkgs.rustPlatform.buildRustPackage (finalAttrs: {
  pname = "space-typst";
  version = "0.1.0";

  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

  buildInputs = [ pkgs.openssl ];

  nativeBuildInputs = [
    pkgs.pkg-config
  ];

  cargoHash = "sha256-I4juhWhUkyuYgtQzDfo3Tp1Yh2hh//QFJ3CyKM+kPvI=";

})
