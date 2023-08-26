// For more information see https://aka.ms/fsharp-console-apps
open FSharp.Data
open System
type LevelType = XmlProvider<"sample.xml">

let args = Environment.GetCommandLineArgs()
let level = LevelType.Load(args[1])
type Dir = Up | Right | Down | Left
let finder r c (restrict: LevelType.Restrict) =
  [
    if restrict.Col = c && restrict.Row = r then
      restrict.Dir
  ]
let divs =
  [
    for r = 1 to level.Height do
      for c = 1 to level.Width do
        let f = finder r c
        let dirs =
          Seq.map f level.Restrictions
          |> Seq.collect id
          |> Set.ofSeq
          |> String.concat " "
        $"""<div class="cell %s{dirs}"></div>"""
  ]
  |> String.concat Environment.NewLine
let s = $"""
<!doctype html>
<html>
  <head>
    <style>
      body {{
        display: inline-block;
      }}
      .container {{
        display: grid;
        grid-template-columns: repeat(%d{level.Width}, 0fr);
        border: 4px solid gray;
      }}
      .cell {{
        width: 20px;
        height: 20px;
        margin: -2px;
      }}
      .up {{
        border-top: 4px solid black;
      }}
      .right {{
        border-right: 4px solid black;
      }}
      .down {{
        border-bottom: 4px solid black;
      }}
      .left {{
        border-left: 4px solid black;
      }}
    </style>
    <script>
      setTimeout(() => document.location.reload(), 3000);
    </script>
  </head>
<body>
  <div class="container">
  %s{divs}
  </div>
</body>
</html>
"""
System.IO.File.WriteAllText(args[2], s)
let f (r: LevelType.Restrict) =
  match r.Dir with
  | "up" -> 1
  | "right" -> 2
  | "down" -> 4
  | "left" -> 8
  | _ -> 0
let extendedRestrictions =
  [
    for r in level.Restrictions do
      r
      if r.Dir = "up" then LevelType.Restrict(r.Col, r.Row - 1, "down")
      if r.Dir = "down" then LevelType.Restrict(r.Col, r.Row + 1, "up")
      if r.Dir = "left" then LevelType.Restrict(r.Col - 1, r.Row, "right")
      if r.Dir = "right" then LevelType.Restrict(r.Col + 1, r.Row, "left")
  ]
let lev =
  [
    $"%d{level.Theseus.Col} %d{level.Theseus.Row}\n"
    $"%d{level.Minotaur.Col} %d{level.Minotaur.Row}\n"
    $"%d{level.Goal.Col} %d{level.Goal.Row}\n"
    for r = 1 to level.Height do
      for c = 1 to level.Width do
        let restrictions =
          Seq.where (fun (x: LevelType.Restrict) -> x.Col = c && x.Row = r) extendedRestrictions
          |> List.ofSeq
        if restrictions.Length > 0 then
          let s =
            restrictions
            |> Seq.map f
            |> Seq.reduce (fun l r -> l ||| r)
          $"%x{s}"
        else
          "0"
      "\n"
  ]
  |> String.concat ""

System.IO.File.WriteAllText(args[3], lev)
eprintfn "--- done ---"
