import Lean.Data.PersistentHashMap
import SubVerso.Module

open Lean SubVerso.Module

namespace Blog.LeanRecall

structure ModuleCacheKey where
  projectDir : String
  toolchain : String
  mod : String
deriving BEq, Hashable

abbrev ModuleCache := PersistentHashMap ModuleCacheKey (Array ModuleItem)

initialize moduleCacheExt : EnvExtension (IO.Ref ModuleCache) ←
  registerEnvExtension (IO.mkRef {})

open System in
def projectDirAndToolchain (leanProject : FilePath) : IO (FilePath × String) := do
  let projectDir := ((← IO.currentDir) / leanProject).normalize
  let lakefile := projectDir / "lakefile.lean"
  let lakefile' := projectDir / "lakefile.toml"
  if !(← lakefile.pathExists) && !(← lakefile'.pathExists) then
    throw <| .userError s!"Neither {lakefile} nor {lakefile'} exist, couldn't load project"
  let toolchainfile := projectDir / "lean-toolchain"
  if !(← toolchainfile.pathExists) then
    throw <| .userError s!"File {toolchainfile} doesn't exist, couldn't load project"
  pure (projectDir, (← IO.FS.readFile toolchainfile).trimAscii.copy)

open System in
def leanPrefix (projectDir : FilePath) (toolchain : String) : IO FilePath := do
  let args := #["run", "--install", toolchain, "lean", "--print-prefix"]
  let res ← IO.Process.output { cmd := "elan", args, cwd := projectDir }
  if res.exitCode != 0 then
    throw <| .userError s!"Failed to locate Lean toolchain source.\nstderr:\n{res.stderr}"
  pure ⟨res.stdout.trimAscii.copy⟩

open System in
def loadModuleContentUncached (projectDir : FilePath) (toolchain mod : String) : IO (Array ModuleItem) := do
  let coreSrc := (← leanPrefix projectDir toolchain) / "src" / "lean"
  let lakeVars :=
    #[("LAKE", none), ("LAKE_HOME", none), ("LAKE_PKG_URL_MAP", none),
      ("LEAN_SYSROOT", none), ("LEAN_AR", none), ("LEAN_PATH", none),
      ("LEAN_SRC_PATH", some coreSrc.toString), ("LEAN_GITHASH", none),
      ("ELAN_TOOLCHAIN", none), ("DYLD_LIBRARY_PATH", none), ("LD_LIBRARY_PATH", none)]
  IO.FS.withTempFile fun h f => do
    let args := #["run", "--install", toolchain, "lake", "exe", "subverso-extract-mod", mod, f.toString]
    let res ← IO.Process.output { cmd := "elan", args, cwd := projectDir, env := lakeVars }
    if res.exitCode != 0 then
      throw <| .userError <|
        s!"Build process failed.\nCWD: {projectDir}\nCommand: elan\nArgs: {repr args}\nExit code: {res.exitCode}\nstdout:\n{res.stdout}\nstderr:\n{res.stderr}"
    h.rewind
    let .ok json := Json.parse (← h.readToEnd)
      | throw <| IO.userError "Expected JSON"
    match Module.fromJson? json with
    | .ok val => pure val.items
    | .error err => throw <| IO.userError s!"Couldn't parse JSON from output file: {err}\nIn:\n{json}"

def isCoreModule (mod : String) : Bool :=
  mod == "Lean" || mod.startsWith "Lean." || mod == "Init" || mod.startsWith "Init." || mod == "Std" || mod.startsWith "Std."

def sanitizePathPart (part : String) : String :=
  String.ofList <| part.toList.map fun c =>
    if c.isAlphanum || c == '-' || c == '_' then c else '_'

open System in
def diskCachePath (projectDir : FilePath) (toolchain mod : String) : FilePath :=
  projectDir / ".lake" / "lean-recall" / "module-cache" / "v1" /
    s!"{sanitizePathPart toolchain}--{sanitizePathPart mod}.json"

open System in
def loadDiskCache? (projectDir : FilePath) (toolchain mod : String) : IO (Option (Array ModuleItem)) := do
  if !isCoreModule mod then
    return none
  let path := diskCachePath projectDir toolchain mod
  if !(← path.pathExists) then
    return none
  try
    let .ok json := Json.parse (← IO.FS.readFile path)
      | return none
    unless (json.getObjValAs? Nat "version").toOption == some 1 do
      return none
    unless (json.getObjValAs? String "toolchain").toOption == some toolchain do
      return none
    unless (json.getObjValAs? String "module").toOption == some mod do
      return none
    let some payload := (json.getObjVal? "payload").toOption
      | return none
    match Module.fromJson? payload with
    | .ok module => return some module.items
    | .error _ => return none
  catch _ =>
    return none

open System in
def saveDiskCache (projectDir : FilePath) (toolchain mod : String) (items : Array ModuleItem) : IO Unit := do
  if !isCoreModule mod then
    return
  try
    let path := diskCachePath projectDir toolchain mod
    if let some parent := path.parent then
      IO.FS.createDirAll parent
    let json := Json.mkObj [
      ("version", ToJson.toJson (1 : Nat)),
      ("toolchain", ToJson.toJson toolchain),
      ("module", ToJson.toJson mod),
      ("payload", ToJson.toJson ({ items } : SubVerso.Module.Module))
    ]
    IO.FS.writeFile path json.compress
  catch _ =>
    pure ()

def loadModuleContentCached (env : Environment) (leanProject : System.FilePath) (mod : String) : IO (Array ModuleItem) := do
  let (projectDir, toolchain) ← projectDirAndToolchain leanProject
  let key : ModuleCacheKey := { projectDir := projectDir.toString, toolchain, mod }
  let fallback ← IO.mkRef ({} : ModuleCache)
  let _ : Inhabited (IO.Ref ModuleCache) := ⟨fallback⟩
  let cacheRef := moduleCacheExt.getState env
  let cache ← cacheRef.get
  if let some items := cache.find? key then
    pure items
  else
    let items ← match ← loadDiskCache? projectDir toolchain mod with
      | some items => pure items
      | none =>
        let items ← loadModuleContentUncached projectDir toolchain mod
        saveDiskCache projectDir toolchain mod items
        pure items
    cacheRef.modify (·.insert key items)
    pure items

end Blog.LeanRecall
