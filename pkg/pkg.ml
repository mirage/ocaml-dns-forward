#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let metas = [
  Pkg.meta_file ~install:false "pkg/META";
]

let opams =
  let lint_deps_excluding = None in
  let install = false in
  [
    Pkg.opam_file "opam" ~lint_deps_excluding ~install;
  ]

let () =
  Pkg.describe ~opams ~metas "dns-forward" @@ fun c ->
  match Conf.pkg_name c with
  | "dns-forward" -> Ok [
      Pkg.lib   "pkg/META";
      Pkg.lib   "opam";
      Pkg.mllib ~api:["Dns_forward"] "lib/dns-forward.mllib";
      Pkg.mllib "lib/dns-forward-lwt-unix.mllib";
      Pkg.bin   "bin/main" ~dst:"dns-forwarder";
      Pkg.test  "lib_test/test" ~args:(Cmd.v "-q");
    ]
  | other -> R.error_msgf "unknown package name: %s" other
