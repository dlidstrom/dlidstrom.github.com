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
  GroupPoints: float
  SelectedWinner: string
}

let simulateBracket (initialBracket: Bracket) : Map<string, float> =
  let r = System.Random()
  let allTeams = System.Collections.Generic.HashSet<string>()
  let teamStages = System.Collections.Generic.Dictionary<string, int>()

  // Start with the first round (usually round of 8 or 16)
  let mutable current: Match list = initialBracket |> List.head
  let mutable roundNum = 0

  // Simulate until only one match remains — the final
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

  // ✅ Simulate the final match
  match current with
  | [finalMatch] ->
      allTeams.Add finalMatch.TeamA |> ignore
      allTeams.Add finalMatch.TeamB |> ignore
      let winner = if r.NextDouble() < 0.5 then finalMatch.TeamA else finalMatch.TeamB
      teamStages[winner] <- roundNum + 1
  | _ -> failwithf "Invalid final state: %A" current

  // Compute scores based on stage
  allTeams
  |> Seq.map (fun team ->
      let stage = if teamStages.ContainsKey team then teamStages[team] else 0
      let score = [1 .. stage] |> List.sumBy (fun r -> pown 2.0 r)
      team, score)
  |> Map.ofSeq

let simulateTop3Generic
  (participants: Participant list)
  (bracket: Bracket)
  (numSimulations: int) =

  let stats =
    participants
    |> List.map (fun p -> p.Name, (ref 0.0, ref 0.0, ref 0.0, ref 0.0))
    |> Map.ofList

  for _ in 1 .. numSimulations do
    let bonusMap = simulateBracket bracket

    // Step 1: Calculate each participant's total score (groupPoints + bonus)
    let scored =
      participants
      |> List.map (fun p ->
          let bonus = Map.tryFind p.SelectedWinner bonusMap |> Option.defaultValue 0.0
          let score = p.GroupPoints + bonus
          p.Name, score)

    // Step 2: Sort by score, then groupPoints, then name
    let ranked =
      scored
      |> List.sortByDescending (fun (name, score) ->
          let gp = participants |> List.find (fun p -> p.Name = name) |> fun p -> p.GroupPoints
          score, gp, name)

    // Step 3: Group by combined score + groupPoints to define true ties
    let grouped =
      ranked
      |> List.groupBy (fun (name, score) ->
          let gp = participants |> List.find (fun p -> p.Name = name) |> fun p -> p.GroupPoints
          score + gp)
      |> List.map (fun (_, group) -> group |> List.map fst)

    // Step 4: Assign podium places
    let mutable place = 0

    for group in grouped do
      match place with
      | 0 ->
          for name in group do
            let first, _, _, top3 = stats[name]
            first.Value <- first.Value + 1.0
            top3.Value <- top3.Value + 1.0
          place <- if group.Length = 1 then 1 elif group.Length = 2 then 2 else 3

      | 1 ->
          for name in group do
            let _, second, _, top3 = stats[name]
            second.Value <- second.Value + 1.0
            top3.Value <- top3.Value + 1.0
          place <- if group.Length = 1 then 2 else 3

      | 2 ->
          for name in group do
            let _, _, third, top3 = stats[name]
            third.Value <- third.Value + 1.0
            top3.Value <- top3.Value + 1.0
          place <- 3

      | _ -> () // ignore any more places

  // Step 5: Convert to percentages
  stats
  |> Map.toList
  |> List.map (fun (name, (f, s, t, top3)) ->
      let total = float numSimulations
      name,
      f.Value / total,
      s.Value / total,
      t.Value / total,
      top3.Value / total)
  |> List.sortByDescending (fun (_, f, _, _, _) -> f)

let simulateEvolution (participants: Participant list) (brackets: BracketStage list) (samples: int) =
  brackets
  |> List.map (fun bracketStage ->
    let results = simulateTop3Generic participants bracketStage.Bracket samples
    bracketStage.Stage, results)

let countryToFlag =
  Map [
    "Sverige", "🇸🇪"
    "England", "🇬🇧"
    "Frankrike", "🇫🇷"
    "Tyskland", "🇩🇪"
    "Spanien", "🇪🇸"
    "Italien", "🇮🇹"
    "Nederländerna", "🇳🇱"
    "Portugal", "🇵🇹"
    "Schweiz", "🇨🇭"
    "Turkiet", "🇹🇷"
    "Kroatien", "🇭🇷"
    "Belgien", "🇧🇪"
    "Danmark", "🇩🇰"
    "Polen", "🇵🇱"
    "Tjeckien", "🇨🇿"
    "Serbien", "🇷🇸"
    "Skottland", "🏴󠁧󠁢󠁳󠁣󠁴󠁿"
    "Norge", "🇳🇴"
    "Finland", "🇫🇮"
    "Island", "🇮🇸"
    "Ukraina", "🇺🇦"
    "Ryssland", "🇷🇺"
    "Österrike", "🇦🇹"
    "Ungern", "🇭🇺"
    "Rumänien", "🇷🇴"
    "Slovakien", "🇸🇰"
    "Slovenien", "🇸🇮"
    "Grekland", "🇬🇷"
    "Irland", "🇮🇪"
    "Wales", "🏴󠁧󠁢󠁷󠁬󠁳󠁿"
    "Albanien", "🇦🇱"
    "Bulgarien", "🇧🇬"
    "Bosnien", "🇧🇦"
    "Nordmakedonien", "🇲🇰"
    "Luxemburg", "🇱🇺"
    "Georgien", "🇬🇪"
    "Kosovo", "🇽🇰"
    "Estland", "🇪🇪"
    "Lettland", "🇱🇻"
    "Litauen", "🇱🇹"
    "Montenegro", "🇲🇪"
    "Armenien", "🇦🇲"
    "Azerbajdzjan", "🇦🇿"
    "Israel", "🇮🇱"
    "Malta", "🇲🇹"
    "Cypern", "🇨🇾"
    "Kazakstan", "🇰🇿"
    "Liechtenstein", "🇱🇮"
    "Andorra", "🇦🇩"
    "San Marino", "🇸🇲"
    "Färöarna", "🇫🇴"
    "Gibraltar", "🇬🇮"
    "Moldavien", "🇲🇩"
    "Vitryssland", "🇧🇾"
    "USA", "🇺🇸"
    "Brasilien", "🇧🇷"
    "Argentina", "🇦🇷"
    "Mexiko", "🇲🇽"
    "Japan", "🇯🇵"
    "Sydkorea", "🇰🇷"
    "Australien", "🇦🇺"
    "Kanada", "🇨🇦"
    "Kina", "🇨🇳"
    // Lägg till fler vid behov
  ]

let run (results: ParseResults<Arguments>) =
  let participantsFile = results.GetResult Participants_file
  let participants =
    File.ReadAllText participantsFile
    |> JsonSerializer.Deserialize<Participant list>

  let bracketsFile = results.GetResult Brackets_filename
  let brackets =
    File.ReadAllText bracketsFile
    |> JsonSerializer.Deserialize<BracketStage list>

  let stage, ranks = simulateEvolution participants brackets 50000 |> List.last
  let countryByName =
    participants
    |> List.map (fun p -> p.Name, p.SelectedWinner)
    |> Map.ofList
  printfn "%s: Chans att placera sig i topp 3:" stage
  for ev in ranks |> Seq.sortByDescending (fun (_, e, f, g, _) -> e, f, g) do
    let name, f, s, t, top3 = ev
    printfn $"%s{name} %s{countryToFlag[countryByName[name]]}"
    printfn
      "  Etta 🏅: %s"
      (if f = 0.0 then "  -" else $"%3.0f{100.0 * f}%%")
    printfn
      "  Tvåa 🥈: %s"
      (if s = 0.0 then "  -" else $"%3.0f{100.0 * s}%%")
    printfn
      "  Trea 🥉: %s"
      (if t = 0.0 then "  -" else $"%3.0f{100.0 * t}%%")
    printfn
      " Åka ut  ፡ %s"
      $"%3.0f{100.0 * (1.0 - top3)}%%"
