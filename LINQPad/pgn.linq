<Query Kind="FSharpProgram">
  <NuGetReference>FsToolkit.ErrorHandling</NuGetReference>
  <Namespace>FsToolkit.ErrorHandling</Namespace>
</Query>

open System
open System.IO
open System.Text.RegularExpressions

type ParserState =
  | Meta
  | Moves

let createTagPattern tagName f =
  let regex = Regex($@"\[{tagName} ""([^""]+)""\]", RegexOptions.Compiled)
  function
  | (input: string) when regex.IsMatch(input) ->
      Some (f (regex.Match(input).Groups.[1].Value))
  | _ -> None

type Termination =
  | Normal
  | TimeForfeit
  | Abandoned
with
  static member parse (s: string) =
    match s with
    | "Normal" -> Normal
    | "Time forfeit" -> TimeForfeit
    | "Abandoned" -> Abandoned
    | s -> failwith $"Unknown termination: %s{s}"

let (|Event|_|) = createTagPattern "Event" id
let (|Site|_|) = createTagPattern "Site" id
let (|White|_|) = createTagPattern "White" id
let (|Black|_|) = createTagPattern "Black" id
let (|Result|_|) = createTagPattern "Result" id
let (|UTCDate|_|) = createTagPattern "UTCDate" id
let (|UTCTime|_|) = createTagPattern "UTCTime" id
let (|WhiteElo|_|) = createTagPattern "WhiteElo" int
let (|BlackElo|_|) = createTagPattern "BlackElo" int
let (|WhiteRatingDiff|_|) = createTagPattern "WhiteRatingDiff" int
let (|BlackRatingDiff|_|) = createTagPattern "BlackRatingDiff" int
let (|ECO|_|) = createTagPattern "ECO" id
let (|Opening|_|) = createTagPattern "Opening" id
let (|TimeControl|_|) = createTagPattern "TimeControl" id
let (|Termination|_|) = createTagPattern "Termination" Termination.parse

type ChessGame = {
  Event: string option
  Site: string option
  White: string option
  Black: string option
  Result: string option
  UTCDate: string option
  UTCTime: string option
  WhiteElo: int option
  BlackElo: int option
  WhiteRatingDiff: int option
  BlackRatingDiff: int option
  ECO: string option
  Opening: string option
  TimeControl: string option
  Termination: Termination option
}
with
  static member Empty = {
    Event = None
    Site = None
    White = None
    Black = None
    Result = None
    UTCDate = None
    UTCTime = None
    WhiteElo = None
    BlackElo = None
    WhiteRatingDiff = None
    BlackRatingDiff = None
    ECO = None
    Opening = None
    TimeControl = None
    Termination = None
  }
  static member parse (str: string) =
    let lines = str.Split('\n', StringSplitOptions.RemoveEmptyEntries) |> List.ofArray
    let game = ChessGame.Empty
    let rec parseLines (lines: string list) game =
      match lines with
      | ln :: lns ->
        let game' =
          match ln with
          | Event e -> { game with Event = Some e }
          | White e -> { game with White = Some e }
          | Black e -> { game with Black = Some e }
          | Result e -> { game with Result = Some e }
          | UTCDate e -> { game with UTCDate = Some e }
          | UTCTime e -> { game with UTCTime = Some e }
          | WhiteElo e -> { game with WhiteElo = Some e }
          | BlackElo e -> { game with BlackElo = Some e }
          | WhiteRatingDiff e -> { game with WhiteRatingDiff = Some e }
          | BlackRatingDiff e -> { game with BlackRatingDiff = Some e }
          | ECO e -> { game with ECO = Some e }
          | Opening e -> { game with Opening = Some e }
          | TimeControl e -> { game with TimeControl = Some e }
          | Termination e -> { game with Termination = Some e }
          | _ -> game
        parseLines lns game'
      | [] -> game
    parseLines lines game

type ValidationError =
  | Event
let validateGeneric = function
| Some s -> Ok s
| None -> Error [Event]

type ValidatedChessGame = {
  Event: string
}
with
  static member validate (game: ChessGame): Result<ValidatedChessGame, ValidationError list> = result {
    let! event = validateGeneric game.Event
    return {
      Event = event
    }
  }
    
let extractGames (linesReader: StreamReader): seq<ChessGame> =
  let rec parseLines state currentGame = seq {
    match linesReader.ReadLine() with
    | null ->
      if not (String.IsNullOrWhiteSpace currentGame) then
        currentGame
    | line ->
      let updatedGame =
        if String.IsNullOrEmpty currentGame then
          line
        else
          currentGame + "\n" + line
      match state with
      | Meta when String.IsNullOrWhiteSpace line ->
        yield! parseLines Moves updatedGame
      | Moves when String.IsNullOrWhiteSpace line ->
        updatedGame
        yield! parseLines state updatedGame
      | _ ->
        yield! parseLines state updatedGame
    }
  let games = parseLines Meta "" |> Seq.map ChessGame.parse
  games

let filename = "/Users/daniel/programming/frost/lichess_db_standard_rated_2016-02.pgn"
let stream = File.OpenRead filename
let linesReader = new StreamReader(stream)
extractGames linesReader
|> Seq.where (fun g -> Option.get g.Termination = Normal && (Option.get g.WhiteElo > 1500 || Option.get g.BlackElo > 1500) )
|> Seq.take 10
|> Seq.iter (printfn "%A")
