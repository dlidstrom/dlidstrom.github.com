module Knockout

open Argu
open System.Text.Json
open System.IO

type Arguments =
  | Brackets_filename of string
  | Participants_file of string
  interface IArgParserTemplate with
    member this.Usage =
      match this with
      | Brackets_filename _ -> "Path to the brackets file."
      | Participants_file _ -> "Path to the participants file."

type Match = {
  TeamA: string
  TeamB: string
}
type Bracket = Match list list
type BracketStage = {
  Stage: string
  Bracket: Bracket
}
type Participant = {
  Name: string
  GroupPoints: int
  SelectedWinner: string
}

let simulateBracket (initialBracket: Bracket) : Map<string, int> =
  let r = System.Random()
  let allTeams = System.Collections.Generic.HashSet<string>()
  let teamStages = System.Collections.Generic.Dictionary<string, int>()

  // Flatten the first round
  let mutable current: Match list = initialBracket |> List.head
  let mutable roundNum = 0

  while current.Length > 1 do
    let winners =
      current
      |> List.map (fun m ->
          allTeams.Add m.TeamA |> ignore
          allTeams.Add m.TeamB |> ignore
          let winner = if r.NextDouble() < 0.5 then m.TeamA else m.TeamB
          teamStages[winner] <- roundNum + 1
          winner)

    if winners.Length % 2 <> 0 then
      failwithf "Odd number of winners in round %d: %A" roundNum winners

    current <-
      winners
      |> List.chunkBySize 2
      |> List.map (function
        | [a; b] -> { TeamA = a; TeamB = b }
        | _ -> failwith "Unexpected chunking error")

    roundNum <- roundNum + 1

  // Assign scores
  allTeams
  |> Seq.map (fun team ->
      let stage = if teamStages.ContainsKey team then teamStages[team] else 0
      let score =
        match stage with
        | s when s = roundNum + 1 -> 7  // winner
        | s when s = roundNum -> 3     // finalist
        | s when s > 0 -> 1            // semifinalist
        | _ -> 0
      team, score)
  |> Map.ofSeq

let simulateTop3Generic
  (participants: Participant list)
  (bracket: Bracket)
  (numSimulations: int) =

  let stats =
    participants
    |> List.map (fun p -> p.Name, (ref 0, ref 0, ref 0, ref 0))
    |> Map.ofList

  for _ in 1 .. numSimulations do
    let bonusMap = simulateBracket bracket

    let scores =
      participants
      |> List.map (fun p ->
          let bonus = Map.tryFind p.SelectedWinner bonusMap |> Option.defaultValue 0
          let total = p.GroupPoints + bonus
          p.Name, total)
      |> List.sortByDescending snd

    let grouped =
      scores
      |> List.groupBy snd
      |> List.sortByDescending fst
      |> List.map (fun (_, group) -> group |> List.map fst)

    let top3 = grouped |> List.truncate 3

    for i, group in List.indexed top3 do
      for name in group do
        let first, second, third, top3ref = stats[name]
        match i with
        | 0 -> first.Value <- first.Value + 1
        | 1 -> second.Value <- second.Value + 1
        | 2 -> third.Value <- third.Value + 1
        | _ -> ()
        top3ref.Value <- top3ref.Value + 1

  stats
  |> Map.toList
  |> List.map (fun (name, (f, s, t, top3)) ->
      let total = float numSimulations
      name,
      float f.Value / total,
      float s.Value / total,
      float t.Value / total,
      float top3.Value / total)
  |> List.sortByDescending (fun (_, f, _, _, _) -> f)

let simulateEvolution (participants: Participant list) (brackets: BracketStage list) (samples: int) =
  brackets
  |> List.map (fun bracketStage ->
      let results = simulateTop3Generic participants bracketStage.Bracket samples
      bracketStage.Stage, results)

let structureByParticipant (data: (string * (string * float * float * float * float) list) list) =
  data
  |> List.collect (fun (stage, results) ->
      results |> List.map (fun (name, f, s, t, top3) ->
        name, (stage, f, s, t, top3)))
  |> Seq.groupBy fst
  |> Seq.map (fun (name, rows) ->
      name, rows |> Seq.map snd |> Seq.toList)
  |> Map.ofSeq

let run (results: ParseResults<Arguments>) =
  let filename = results.GetResult Brackets_filename
  // Here you would read the file and parse the matches
  // For now, we will just print the filename
  printfn "Running knockout analysis with file: %s" filename
  let participantsFile = results.GetResult Participants_file
  let participants =
    System.IO.File.ReadAllText participantsFile
    |> JsonSerializer.Deserialize<Participant list>

  let bracketsFile = results.GetResult Brackets_filename
  let brackets =
    System.IO.File.ReadAllText bracketsFile
    |> JsonSerializer.Deserialize<BracketStage list>

  let evolution = simulateEvolution participants brackets 10000
  for ev in evolution do
    printfn "Stage: %s" (fst ev)
    for name, f, s, t, top3 in snd ev do
      printfn "  %s: First: %-6.2f%%, Second: %-6.2f%%, Third: %-6.2f%%, Top 3: %-6.2f%%" name (100.0 * f) (100.0 * s) (100.0 * t) (100.0 * top3)
  printfn "ev: %A" evolution

  let structure = structureByParticipant evolution
  printfn "Structure by participant:"
  structure
  |> Map.toList
  |> List.sortByDescending (fun (_, lst) -> List.last lst |> fun (_, e, f, g, _) -> e, f, g)
  |> List.iter (fun (name, stages) ->
      printfn "Participant: %s" name
      stages
      |> List.iter (fun (stage, f, s, t, top3) ->
        printfn "  Stage: %s, First: %.2f, Second: %.2f, Third: %.2f, Top 3: %.2f" stage f s t top3)
  )
