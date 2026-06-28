import VersoBlog

namespace Blog.Html

def xmlEscape (text : String) : String := Id.run do
  let mut out := ""
  for char in text.toList do
    out := out ++ match char with
      | '&' => "&amp;"
      | '<' => "&lt;"
      | '>' => "&gt;"
      | '"' => "&quot;"
      | '\'' => "&apos;"
      | c => c.toString
  out

def attr? (name : String) (attrs : Array (String × String)) : Option String := do
  let attr ← attrs.find? (·.1 == name)
  return attr.2

def classContains (needle : String) (attrs : Array (String × String)) : Bool :=
  match attr? "class" attrs with
  | none => false
  | some classes => classes.splitOn " " |>.contains needle

def normalizeSpaces (text : String) : String :=
  let (_, out) := text.foldl (fun (pendingSpace, out) char =>
    if char.isWhitespace then
      (true, out)
    else if pendingSpace && !out.isEmpty then
      (false, out.push ' ' |>.push char)
    else
      (false, out.push char)) (false, "")
  out

end Blog.Html
