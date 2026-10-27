import VersoBlog

open Lean Meta Elab Verso Doc Elab ArgParse
open Verso.Genre.Blog
open Lean.Doc.Syntax
open SubVerso.Highlighting

namespace Blog.InlineLean

private structure Result where
  type : Std.Format
  highlighting : Highlighted
  messages : MessageLog
  env : Environment

private def elaborateTerm (config : LeanInlineConfig) (str : StrLit) : DocElabM Result :=
  Verso.withoutAsync do
    let (commandState, _) ← match exampleContextExt.getState (← getEnv) |>.contexts.find? config.exampleContext.getId with
      | some (.inline commandState state) => pure (commandState, state)
      | some (.module ..) => throwErrorAt config.exampleContext "Expected an example context for inline Lean, but found a module"
      | none => throwErrorAt config.exampleContext "Can't find example context"
    let {env, scopes, ..} := commandState
    let leveller {α} : TermElabM α → TermElabM α :=
      if let some us := config.universes then
        Term.withLevelNames (us.getString.splitOn " " |>.filterMap fun s =>
          if s.isEmpty then none else some s.toName)
      else id
    let oldEnv ← getEnv
    try
      setEnv env
      let initMsgs ← Core.getMessageLog
      let origTrees ← getResetInfoTrees
      let (newMsgs, type, tree, stx, termEnv) ← try
        Core.resetMessageLog
        let stx ← Verso.SyntaxUtils.parseStrLitAsCategory `term str
        let (tree, type, termEnv) ← runWithOpenDecls scopes <| runWithVariables scopes fun _ => do
          let expectedType ← config.type.mapM fun (s : StrLit) => do
            match Parser.runParserCategory (← getEnv) `term s.getString (← getFileName) with
            | .error e => throwErrorAt s e
            | .ok typeStx => withEnableInfoTree false do
              let t ← leveller <| Term.elabType typeStx
              Term.synthesizeSyntheticMVarsNoPostponing
              let t ← instantiateMVars t
              if t.hasExprMVar || t.hasLevelMVar then
                throwErrorAt s "Type contains metavariables: {t}"
              pure t
          let e ← leveller <| Term.elabTerm (catchExPostpone := true) stx expectedType
          -- We want to allow stuck instances
          Term.synthesizeSyntheticMVarsNoPostponing (ignoreStuckTC := true)
          let e ← Term.levelMVarToParam (← instantiateMVars e)
          let t ← inferType e >>= instantiateMVars >>= (ppExpr ·)
          let t := Format.group ((← ppExpr e) ++ (" :" ++ .line) ++ t)
          let ctx := .commandCtx {
            env := ← getEnv, fileMap := ← getFileMap, mctx := ← getMCtx, currNamespace := ← getCurrNamespace,
            openDecls := ← getOpenDecls, options := ← getOptions, ngen := ← getNGen
          }
          let innerTrees ← getResetInfoTrees
          pure (InfoTree.context ctx (.node (.ofCommandInfo ⟨`Blog.InlineLean, str⟩) innerTrees), t, ← getEnv)
        pure (← Core.getMessageLog, type, tree, stx, termEnv)
      finally
        Core.setMessageLog initMsgs
        for tree in origTrees do pushInfoTree tree
      pushInfoTree tree
      let hls ← highlight stx #[] (PersistentArray.empty.push tree)
      return ⟨type, hls, newMsgs, termEnv⟩
    finally
      setEnv oldEnv

private def reportMessages (str : StrLit) (messages : MessageLog) (error : Bool) : DocElabM Unit := do
  if error && !messages.hasErrors then
    throwErrorAt str "Error expected in Lean code, but none occurred"
  for msg in (if error then messages.errorsToWarnings else messages).toArray do
    logMessage {msg with isSilent := msg.isSilent || error || msg.severity != .error}

structure TermConfig extends LeanInlineConfig where
  «show» : Bool := true
  name : Option Name := none
  error : Bool := false
  showProofStates : Bool := true

instance : FromArgs TermConfig DocElabM where
  fromArgs := TermConfig.mk <$> FromArgs.fromArgs <*>
    .flag `show true "Include in rendered page?" <*>
    .named `name .name true <*>
    .flag `error false "Error expected in term?" <*>
    .flag `showProofStates true "Show proof states in rendered page?"

private def oneCode (elts : TSyntaxArray `inline) : DocElabM StrLit := do
  let #[code] := elts
    | throwError "Expected precisely one code element"
  let `(inline|code( $str:str )) := code
    | throwErrorAt code "Expected an inline code element"
  pure str

private def saveMessages (name : Option Name) (env : Environment) (messages : MessageLog) : DocElabM Unit := do
  if let some name := name then
    modifyEnv fun e => messageContextExt.modifyState e fun s => { s with
      messages := s.messages.insert name (env, messages)
    }

private def checkTerm (config : TermConfig) (str : StrLit) : DocElabM Result := do
  let result ← elaborateTerm config.toLeanInlineConfig str
  reportMessages str result.messages config.error
  saveMessages config.name result.env result.messages
  pure result

private def elaborateCommands (config : LeanBlockConfig) (str : StrLit) : DocElabM Highlighted :=
  Verso.withoutAsync do
    let x := config.exampleContext
    let (commandState, state) ← match exampleContextExt.getState (← getEnv) |>.contexts.find? x.getId with
      | some (.inline commandState state) => pure (commandState, state)
      | some (.module ..) => throwErrorAt x "Expected an example context for inline Lean, but found a module"
      | none => throwErrorAt x "Can't find example context"
    let (context, startPos) ← Verso.SyntaxUtils.strLitInputContext str.raw (← getFileName)
    let s ← IO.processCommands context { state with pos := startPos, hasLeading := false }
      { commandState with messages.unreported := {} }
    for tree in s.commandState.infoState.trees do
      pushInfoTree tree
    reportMessages str s.commandState.messages config.error
    if config.keep && !config.error then
      modifyEnv fun env => exampleContextExt.modifyState env fun st => { st with
        contexts := st.contexts.insert x.getId (.inline { s.commandState with messages := {} } s.parserState)
      }
    saveMessages config.name s.commandState.env s.commandState.messages
    let infoSt ← getInfoState
    let env ← getEnv
    try
      setInfoState s.commandState.infoState
      setEnv s.commandState.env
      let mut hls := Highlighted.empty
      let msgs := s.commandState.messages.toArray.filter (!·.isSilent)
      for cmd in s.commands do
        hls := hls ++ (← highlight cmd msgs s.commandState.infoState.trees)
      return hls
    finally
      setInfoState infoSt
      setEnv env

private def renderCode (inline showCode : Bool) (opts : Verso.Genre.Blog.CodeOpts)
    (str : StrLit) (hls : Highlighted) : DocElabM Term := do
  if inline then
    if showCode then
      `(Verso.Doc.Inline.other (Verso.Genre.Blog.InlineExt.highlightedCode
          { contextName := $(quote opts.contextName), showProofStates := $(quote opts.showProofStates) }
          $(quote hls))
        #[Verso.Doc.Inline.code $(quote str.getString)])
    else
      `(Verso.Doc.Inline.concat #[])
  else
    if showCode then
      `(Verso.Doc.Block.other (Verso.Genre.Blog.BlockExt.highlightedCode
          { contextName := $(quote opts.contextName), showProofStates := $(quote opts.showProofStates) }
          $(quote hls))
        #[Verso.Doc.Block.code $(quote str.getString)])
    else
      `(Verso.Doc.Block.concat #[])

@[code_block]
meta def lean : CodeBlockExpanderOf LeanBlockConfig
  | config, str => do
    let hls ← elaborateCommands config str
    renderCode false config.show ⟨config.exampleContext.getId, config.showProofStates⟩ str hls

@[code_block]
meta def leanTerm : CodeBlockExpanderOf TermConfig
  | config, str => do
    let result ← checkTerm config str
    renderCode false config.show ⟨config.exampleContext.getId, config.showProofStates⟩ str result.highlighting

@[role lean]
meta def leanInline : RoleExpanderOf TermConfig
  | config, elts => do
    let str ← oneCode elts
    let result ← checkTerm config str
    if let `(inline|role{%$s $f $_*}%$e[$_*]) ← getRef then
      Verso.Hover.addCustomHover (mkNullNode #[s, e]) result.type
      Verso.Hover.addCustomHover f result.type
    renderCode true config.show ⟨config.exampleContext.getId, config.showProofStates⟩ str result.highlighting

end Blog.InlineLean
