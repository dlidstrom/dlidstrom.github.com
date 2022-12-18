open System
open System.IO
open System.Text.RegularExpressions

let parse (s: string) =
    try
        int (s.Trim())
    with
    | e -> failwithf $"%A{s}"
let sumGroup (g: string) =
    let s = Regex.Split(g, @"\s+")
    s
    |> Seq.where (fun s -> s <> "")
    |> Seq.map parse
    |> Seq.sum

let content = File.ReadAllText("advent1.txt")
let groups = content.Split(Environment.NewLine + Environment.NewLine)
//groups[0].Split(' ') |> Seq.map int
let m =
  groups
  |> Seq.sortByDescending sumGroup
  |> Seq.take 3
  |> Seq.map sumGroup
  |> Seq.sum
printfn $"%d{m}"
