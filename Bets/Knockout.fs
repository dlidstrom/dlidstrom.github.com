module Knockout

open Argu

type Arguments =
  | Filename of string
  interface IArgParserTemplate with
    member this.Usage =
      match this with
      | Filename _ -> "Path to the knockout matches file."
