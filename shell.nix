{
  pkgs ? import <nixpkgs> { },
}:

(pkgs.buildFHSEnv {
  name = "blog-env";
  targetPkgs =
    pkgs:
    (with pkgs; [
      stdenv.cc
      (python3.withPackages (
        python-pkgs: with python-pkgs; [
          beautifulsoup4
        ]
      ))
    ]);
  runScript = "zsh";
}).env
