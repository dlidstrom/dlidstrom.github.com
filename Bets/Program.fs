open System
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra

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

(**
 * Update with known results.
 *)
let teams =
  [
    "Spanien"
    "Belgien"
    "Portugal"
    "Italien"
    "Tyskland"
    "Danmark"
    "Polen"
    "Sverige"
    "England"
    "Nederländerna"
    "Frankrike"
    "Wales"
    "Finland"
    "Schweiz"
    "Norge"
    "Island"
  ]
  |> List.mapi (fun i name -> { Id = i + 1; Name = name })
  |> Seq.map (fun m -> m.Id, m.Name)
  |> Map

let toId (name: string) =
  match teams |> Seq.tryFind (fun kv -> kv.Value = name) with
  | Some (KeyValue(id, _)) -> id
  | None -> failwithf "Unknown team: %s" name

(**
 * Update with odds for known matches.
Island - Finland
Schweiz - Norge
Belgien - Italien
Spanien - Portugal
Danmark - Sverige
Tyskland - Polen
Wales - Nederländerna
Frankrike - England
Norge - Finland
Schweiz - Island
Spaniel - Belgien
Portugal - Italien
Tyskland - Danmark
Polen - Sverige
England - Nederländerna
Frankrike - Wales
Finland - Schweiz
Norge - Island
Italien - Spanien
Portugal - Belgien
Sverige - Tyskland
Polen - Danmark
Nederländerna - Frankrike
England - Wales
 *)
let matches =
  [
    "Island", "Finland", 1.0 / 0.52, 1.0 / 0.27, 1.0 / 0.21
    "Schweiz", "Norge", 1.0 / 0.26, 1.0 / 0.28, 1.0 / 0.46
    "Belgien", "Italien", 1.0 / 0.22, 1.0 / 0.25, 1.0 / 0.53
    "Spanien", "Portugal", 1.0 / 0.87, 1.0 / 0.9, 1.0 / 0.4
    "Danmark", "Sverige", 1.0 / 0.16, 1.0 / 0.23, 1.0 / 0.60
    "Tyskland", "Polen", 1.0 / 0.88, 1.0 / 0.9, 1.0 / 0.3
    "Wales", "Nederländerna", 1.0 / 0.6, 1.0 / 0.14, 1.0 / 0.80
    "Frankrike", "England", 1.0 / 0.31, 1.0 / 0.30, 1.0 / 0.40
  ]
  |> List.map (fun (h, a, w, d, l) ->
    {
      Pair = {
        Home = toId h
        Away = toId a
      }
      Win = w
      Draw = d
      Loss = l
    })

(**
 * Update with unknown matches.
 *)
let unknowns =
  [
    "Norge", "Finland"
    "Schweiz", "Island"
    "Spanien", "Belgien"
    "Portugal", "Italien"
    "Tyskland", "Danmark"
    "Polen", "Sverige"
    "England", "Nederländerna"
    "Frankrike", "Wales"
    "Finland", "Schweiz"
    "Norge", "Island"
    "Italien", "Spanien"
    "Portugal", "Belgien"
    "Sverige", "Tyskland"
    "Polen", "Danmark"
    "Nederländerna", "Frankrike"
    "England", "Wales"
  ]
  |> List.map (fun (h, a) -> {
    Home = toId h
    Away = toId a
  })


/// Solve team strengths by fixing one team's strength to 0
let estimateTeamStrengths (matches: Match list) =
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

    // 3. Solve for non-anchor team strengths
    let solved = A.QR().Solve b

    // 4. Combine all team strengths (anchor = 0)
    let result =
        [ anchorTeam, 0.0 ] @
        (otherTeams
         |> List.mapi (fun i t -> t, solved.[i]))

    result |> dict


/// Ridge regression version
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

    //let Areg = A.Append I
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

//let strengths = estimateTeamStrengths matches
let strengths = estimateTeamStrengthsRidge matches 0.1
for KeyValue(team, s) in strengths do
    printfn "%s: %.3f" teams[team] s

let drawBias = -0.3
let probs =
  unknowns
  |> List.map (fun mtch ->
    let homeStrength = strengths[mtch.Home]
    let awayStrength = strengths[mtch.Away]
    let diff = homeStrength - awayStrength
    let winE = Math.Exp diff
    let drawE = Math.Exp drawBias
    let lossE = Math.Exp -diff
    let softmax = winE + drawE + lossE
    let winProb = winE / softmax
    let drawProb = 1.0 / softmax
    let lossProb = lossE / softmax
    (mtch.Home, mtch.Away), { WinP = winProb; DrawP = drawProb; LossP = lossProb })
  |> Map

let r = Random()
let pickOutcome (p: Probs) (r: Random) =
  let n = r.NextDouble()
  if n < p.WinP then '1'
  elif n < p.WinP + p.DrawP then 'X'
  else '2'

let row = [
  for KeyValue((h, a), p) in probs -> (h, a), pickOutcome p r
]

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

row |>
Seq.iter (fun ((h, a), p) ->
  printfn "%8s - %-8s - %c" teams[h] teams[a] p
)
for (h, a), pick in row do
  let p = probs[(h, a)]
  let prob =
    match pick with '1' -> p.WinP | 'X' -> p.DrawP | '2' -> p.LossP | _ -> 0.0
  printfn "%s-%s %c (%.2f%%)" teams[h] teams[a] pick (prob * 100.0)

let risk = riskOfRow row probs
printfn "Total risk: %.3f" risk


let generateRow (r: Random) (probs: Map<(int * int), Probs>) =
  probs
  |> Seq.map (fun kv ->
      let (h, a), p = kv.Key, kv.Value
      let outcome = pickOutcome p r
      (h, a), outcome)
  |> Seq.toList

let simulateRows count (probs: Map<(int * int), Probs>) =
  let r = Random()
  [ for _ in 1..count ->
      let row = generateRow r probs
      let risk = riskOfRow row probs
      risk, row ]

// Example: simulate 10,000 rows
let simulated = simulateRows 10000 probs

// Sort by risk (lowest to highest)
let sorted = simulated |> List.sortBy fst

// Print top 10 safest and 10 riskiest
let printRow (risk, row) =
  printfn "Risk: %.2f" risk
  for (h, a), pick in row do
    printfn "  %8s - %-8s - %c" teams[h] teams[a] pick
  printfn ""

printfn "Top 10 Safest Rows:\n"
sorted |> List.take 10 |> List.iter printRow

printfn "\nTop 10 Riskiest Rows:\n"
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

let candidates = simulateRows 10000 probs |> List.sortBy fst

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
printfn "Most Diverse Rows (10):\n"
for i, row in List.indexed diverse10 do
  let risk = riskOfRow row probs
  printfn $"Row %d{i + 1} (Risk: %.3f{risk}):"
  // Create a lookup for this row
  let rowMap = row |> Map.ofList
  for mtch in unknowns do
    let pick = rowMap[(mtch.Home, mtch.Away)]
    printfn $"  %8s{teams[mtch.Home]} - %-8s{teams[mtch.Away]} - %c{pick}"
  printfn ""
