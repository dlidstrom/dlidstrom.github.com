open Argu

type Arguments =
  | [<CliPrefix(CliPrefix.None)>] Odds of ParseResults<Odds.Arguments>
  | [<CliPrefix(CliPrefix.None)>] Knockout of ParseResults<Knockout.Arguments>
  interface IArgParserTemplate with
    member this.Usage =
      match this with
      | Odds _ -> "Run the odds analysis."
      | Knockout _ -> "Run the knockout analysis."

[<EntryPoint>]
let main argv =
  try
    let parser = ArgumentParser.Create<Arguments>()
    let results = parser.ParseCommandLine argv
    match results.GetSubCommand() with
    | Odds oddsResults ->
        Odds.run oddsResults
        0
    | Knockout knockoutResults ->
      Knockout.run knockoutResults
      0
  with
  | :? ArguParseException as ex ->
      printfn "Error: %s" ex.Message
      1
