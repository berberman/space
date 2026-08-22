import Lake

open Lake DSL

package «verso-blog» where
  version := v!"0.1.0"

  leanOptions := #[
    ⟨`linter.unusedVariables, false⟩
  ]

require verso from git
  "https://github.com/leanprover/verso.git" @ "v4.31.0"


target typstRenderer pkg : System.FilePath := do
  let mainRs ←
    inputTextFile <|
      pkg.dir / "typst" / "src" / "main.rs"

  let cargoToml ←
    inputTextFile <|
      pkg.dir / "typst" / "Cargo.toml"

  let cargoLock ←
    inputTextFile <|
      pkg.dir / "typst" / "Cargo.lock"

  let binary :=
    pkg.dir
      / "typst"
      / "target"
      / "release"
      / "space-typst"

  buildFileAfterDep binary
      (.collectList [mainRs, cargoToml, cargoLock]) fun _ => do
    proc {
      cmd := "cargo"
      args := #[
        "build",
        "--release",
        "--locked"
      ]
      cwd := some (pkg.dir / "typst")
    }

@[default_target]
lean_lib Blog where
  needs := #[typstRenderer]

@[default_target]
lean_exe «generate-blog» where
  root := `Main
