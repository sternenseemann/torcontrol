#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () =
  Pkg.describe "torcontrol" @@ fun c ->
  Ok [ Pkg.mllib "src/torcontrol.mllib" ]
