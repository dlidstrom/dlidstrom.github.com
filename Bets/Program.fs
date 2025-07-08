open System
open System.IO
open System.Text.RegularExpressions
open MathNet.Numerics.LinearAlgebra.Double

type Team = {
  Id: int
  Name: string
}

type Pair = {
  Home: int
  Away: int
}

type Match = {
  Pair: Pair
  Win: float
  Draw: float
  Loss: float
}

type Probs = {
  WinP: float
  DrawP: float
  LossP: float
}

let parseMatchesFile (path: string) =
  let lines = File.ReadAllLines path
  let oddsRegex = Regex @"^(\S.*?)\s*-\s*(\S.*?)\s+([\d\.]+)\s+([\d\.]+)\s+([\d\.]+)$"
  let matchRegex = Regex @"^(\S.*?)\s*-\s*(\S.*?)\s*$"

  let oddsMatches, unknownMatches =
    lines
    |> Array.filter (fun line -> not (String.IsNullOrWhiteSpace line))
    |> Array.partition (fun line -> oddsRegex.IsMatch line)

  let known =
    oddsMatches
    |> Array.map (fun line ->
      let m = oddsRegex.Match(line)
      let home = m.Groups.[1].Value.Trim()
      let away = m.Groups.[2].Value.Trim()
      let win = float m.Groups.[3].Value
      let draw = float m.Groups.[4].Value
      let loss = float m.Groups.[5].Value
      home, away, win, draw, loss
    )
    |> Array.toList

  let unknown =
    unknownMatches
    |> Array.choose (fun line ->
      let m = matchRegex.Match line
      if m.Success then
        let home = m.Groups.[1].Value.Trim()
        let away = m.Groups.[2].Value.Trim()
        Some (home, away)
      else None
    )
    |> Array.toList

  // Collect all unique teams
  let teams =
    (known |> List.collect (fun (h, a, _, _, _) -> [h; a]))
    @ (unknown |> List.collect (fun (h, a) -> [h; a]))
    |> List.distinct

  teams, known, unknown

let teamsList, knownMatches, unknownMatches = parseMatchesFile "matches.txt"

let teams =
  teamsList
  |> List.mapi (fun i name -> (i + 1, name))
  |> Map.ofList

let nameToId =
  teams
  |> Seq.map (fun kv -> kv.Value, kv.Key)
  |> Map.ofSeq

let toId (name: string) =
  match nameToId.TryFind name with
  | Some id -> id
  | None -> failwithf "Unknown team: %s" name

let matches =
  knownMatches
  |> List.map (fun (h, a, w, d, l) ->
    {
      Pair = { Home = toId h; Away = toId a }
      Win = w
      Draw = d
      Loss = l
    })

let unknowns =
  unknownMatches
  |> List.map (fun (h, a) -> { Home = toId h; Away = toId a })

let estimateTeamStrengthsRidge (matches: Match list) (lambda: float) =
  // 1. Collect all unique teams
  let teams =
    matches
    |> List.collect (fun m -> [m.Pair.Home; m.Pair.Away])
    |> Set.ofList
    |> Set.toList

  let anchorTeam = teams.Head // Fix first team to strength 0
  let otherTeams = teams |> List.filter ((<>) anchorTeam)
  let teamIndex = otherTeams |> List.mapi (fun i t -> t, i) |> dict

  // 2. Build matrix A (excluding anchor team column) and vector b
  let rows = matches.Length
  let cols = otherTeams.Length

  let A = DenseMatrix.Create(rows, cols, 0.0)
  let b = DenseVector.Create(rows, 0.0)

  for i, mtch in List.indexed matches do
    let d = log (mtch.Loss / mtch.Win)
    b.[i] <- d

    match mtch.Pair.Home, mtch.Pair.Away with
    | h, a when h = anchorTeam && a = anchorTeam -> () // shouldn't happen
    | h, a when h = anchorTeam ->
      let aIdx = teamIndex[a]
      A.[i, aIdx] <- -1.0
    | h, a when a = anchorTeam ->
      let hIdx = teamIndex[h]
      A.[i, hIdx] <- 1.0
    | h, a ->
      let hIdx = teamIndex[h]
      let aIdx = teamIndex[a]
      A.[i, hIdx] <- 1.0
      A.[i, aIdx] <- -1.0

  // Ridge regularization
  let I = DenseMatrix.Create(cols, cols, 0.0)
  for i in 0 .. cols-1 do I.[i,i] <- sqrt lambda

  let Areg =
    DenseMatrix.OfRows(
      A.RowCount + I.RowCount,
      A.ColumnCount,
      Seq.append (A.EnumerateRows() |> Seq.map (fun v -> v :> seq<float>))
                (I.EnumerateRows() |> Seq.map (fun v -> v :> seq<float>))
    )
  let breg = DenseVector.Build.DenseOfEnumerable (Seq.append (b :> seq<_>) (Seq.init cols (fun _ -> 0.0)))

  let solved = Areg.QR().Solve breg

  let result =
    [ anchorTeam, 0.0 ] @
    (otherTeams |> List.mapi (fun i t -> t, solved.[i]))
  result |> dict

let strengths = estimateTeamStrengthsRidge matches 0.1

printfn "Estimated Team Strengths:"
for KeyValue(team, s) in strengths do
  printfn $"%s{teams[team]}: %.3f{s}"

let drawBias = -0.3

let allMatches =
  let knownPairs = matches |> List.map (fun m -> m.Pair.Home, m.Pair.Away)
  let unknownPairs = unknowns |> List.map (fun m -> m.Home, m.Away)
  // Combine and remove duplicates, preserving order: known first, then unknowns not already in known
  let combinedPairs =
    knownPairs @ (unknownPairs |> List.filter (fun x -> not (List.contains x knownPairs)))
  combinedPairs
  |> List.map (fun (h, a) -> { Home = h; Away = a })

let probs =
  allMatches
  |> List.map (fun mtch ->
    let homeStrength = strengths[mtch.Home]
    let awayStrength = strengths[mtch.Away]
    let diff = homeStrength - awayStrength
    let winE = Math.Exp diff
    let drawE = Math.Exp drawBias
    let lossE = Math.Exp -diff
    let softmax = winE + drawE + lossE
    let winProb = winE / softmax
    let drawProb = drawE / softmax
    let lossProb = lossE / softmax
    (mtch.Home, mtch.Away), { WinP = winProb; DrawP = drawProb; LossP = lossProb })
  |> Map

printfn "\nPredicted odds for unknown matches:"
for mtch in unknowns do
  let p = probs[(mtch.Home, mtch.Away)]
  printfn
    "%-16s - %-16s | Win: %3s%%  Draw: %3s%%  Loss: %3s%%"
    teams[mtch.Home]
    teams[mtch.Away]
    $"%.0f{100.0 * p.WinP}"
    $"%.0f{100.0 * p.DrawP}"
    $"%.0f{100.0 * p.LossP}"

let r = Random()
let pickOutcome (p: Probs) =
  let n = r.NextDouble()
  if n < p.WinP then '1'
  elif n < p.WinP + p.DrawP then 'X'
  else '2'

let riskOfRow (row: ((int * int) * char) list) (probs: Map<(int * int), Probs>) =
  row
  |> List.sumBy (fun ((h, a), pick) ->
    let p = probs[(h, a)]
    let selectedProb =
      match pick with
      | '1' -> p.WinP
      | 'X' -> p.DrawP
      | '2' -> p.LossP
      | _ -> failwith "Invalid pick"
    -log selectedProb
  )

let generateRow (r: Random) (probs: Map<(int * int), Probs>) =
  probs
  |> Seq.map (fun kv ->
    let (h, a), p = kv.Key, kv.Value
    let outcome = pickOutcome p
    (h, a), outcome)
  |> Seq.toList

let simulateRows count (probs: Map<(int * int), Probs>) =
  let r = Random()
  [ for _ in 1..count ->
    let row = generateRow r probs
    let risk = riskOfRow row probs
    risk, row ]

// Example: simulate 10,000 rows
let simulated = simulateRows 10_000 probs

// Sort by risk (lowest to highest)
let sorted = simulated |> List.sortBy fst

// Print top 10 safest and 10 riskiest
let printRow (risk, row) =
  printf "Risk-%.2f" risk
  let rowMap = row |> Map.ofList
  for mtch in allMatches do
    let pick = rowMap[(mtch.Home, mtch.Away)]
    printf " %c" pick
  printfn ""

printfn "\nTop 10 Safest Rows:"
sorted |> List.take 10 |> List.iter printRow

printfn "\nTop 10 Riskiest Rows:"
sorted |> List.rev |> List.take 10 |> List.iter printRow

let hammingDistance (a: ((int * int) * char) list) (b: ((int * int) * char) list) =
  List.zip a b
  |> List.sumBy (fun (((_, _), p1), ((_, _), p2)) -> if p1 = p2 then 0 else 1)

let averageDiversity (row: ((int * int) * char) list) (others: (((int * int) * char) list) list) =
  match others with
  | [] -> float row.Length // max possible diversity
  | _ ->
    others
    |> List.averageBy (hammingDistance row >> float)

let selectMostDiverseRows
  (candidates: ((int * int) * char) list list)
  (count: int)
  : ((int * int) * char) list list =

  let rec loop acc remaining =
    match List.length acc, remaining with
    | n, _ when n >= count -> acc
    | _, [] -> acc
    | _, _ ->
      // For each candidate, compute its average diversity from acc
      let scored =
        remaining
        |> List.map (fun row -> row, averageDiversity row acc)
      // Pick the row with the highest diversity
      let next, _ = scored |> List.maxBy snd
      let rest = remaining |> List.filter ((<>) next)
      loop (acc @ [next]) rest

  // Start with a random row, then greedily add the most diverse
  match candidates with
  | [] -> []
  | first::rest -> loop [first] rest

let allSimulatedRows =
  simulateRows 10000 probs
  |> List.map snd // take only the row, ignore risk
  |> List.distinct // avoid exact duplicates
  |> List.randomShuffle

let diverse10 = selectMostDiverseRows allSimulatedRows 10

// Display the results
printfn "\nMost Diverse Rows (10) - these seem to do good, pick from them:"
for i, row in List.indexed diverse10 do
  let risk = riskOfRow row probs
  printRow (risk, row)

// Pick 10 rows around the median risk
let midCount = 10
let total = List.length sorted
let startIdx = max 0 (total / 2 - midCount / 2)
let middleRows = sorted |> List.skip startIdx |> List.take midCount

printfn "\n10 Middle-Risk Rows:"
middleRows |> List.iter printRow
