(***********************************************************************)
(*                                                                     *)
(*                             ocamlbuild                              *)
(*                                                                     *)
(*  Nicolas Pouillard, Berke Durak, projet Gallium, INRIA Rocquencourt *)
(*                                                                     *)
(*  Copyright 2007 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)


(* Original author: Romain Bardou *)

open My_std
open My_unix
open Command

type command_spec = Command.spec

type error =
  | Dependency_not_found of string * string (* package, dependency *)
  | Package_not_found of string
  | Package_loop of string

exception Findlib_error of error

let error x = raise (Findlib_error x)

let string_of_error = function
  | Dependency_not_found(p, d) ->
      Printf.sprintf
        "Ocamlfind returned \"%s\" as a dependency for package \"%s\" but does \
not know this dependency." d p
  | Package_not_found p ->
      Printf.sprintf "Findlib package not found: \"%s\"." p
  | Package_loop p ->
      Printf.sprintf "The following Findlib package is required by itself: \"%s\"." p

let report_error e =
  prerr_endline (string_of_error e);
  exit 2

let () = Findlib.init ()

type package = {
  name: string;
  description: string;
  version: string;
  archives_byte: string;
  archives_native: string;
  link_options: string;
  location: string;
  dependencies: package list;
}

let packages = Hashtbl.create 42

let run_and_parse lexer command =
  Printf.ksprintf
    (fun command -> lexer & Lexing.from_string & run_and_read command)
    command

let run_and_read command =
  Printf.ksprintf run_and_read command

let package_property_or_emp preds name prop =
  try Findlib.package_property preds name prop with
    Not_found -> ""

let rec query name =
  try
    Hashtbl.find packages name
  with Not_found ->
    try
      let d = package_property_or_emp [] name "description" in
      let v = package_property_or_emp [] name "version" in
      let lo = package_property_or_emp [] name "linkopts" in
      let l = Findlib.package_directory name in
      let a_byte =
        try
          Pathname.concat l (Findlib.package_property ["byte"] name "archive")
        with Not_found -> ""
      in
      let a_native =
        try
          Pathname.concat l (Findlib.package_property ["native"] name "archive")
        with Not_found -> ""
      in
      let deps = Findlib.package_deep_ancestors [] [name] in
      let deps = List.filter ((<>) name) deps in
      let deps =
        try
          List.map query deps
        with Findlib_error (Package_not_found dep_name) ->
          (* Ocamlfind cannot find a package which it returned as a dependency.
             This should not happen. *)
          error (Dependency_not_found (name, dep_name))
      in
      let package = {
        name;
        description = d;
        version = v;
        archives_byte = a_byte;
        archives_native = a_native;
        link_options = lo;
        location = l;
        dependencies = deps;
      } in
      Hashtbl.add packages name package;
      package
    with
      | Findlib.No_such_package (name, _) -> error (Package_not_found name)
      | Findlib.Package_loop name -> error (Package_loop name)

let split_nl s =
  let x = ref [] in
  let rec go s =
    let pos = String.index s '\n' in
    x := (String.before s pos)::!x;
    go (String.after s (pos + 1))
  in
  try
    go s
  with Not_found -> !x

let before_space s =
  try
    String.before s (String.index s ' ')
  with Not_found -> s

let list () =
  Fl_package_base.list_packages ()
  |> List.sort compare

(* The closure algorithm is easy because the dependencies are already closed
and sorted for each package. We only have to make the union. We could also
make another findlib query such as:
  Findlib.package_deep_ancestors [] [package1; package2; ...] *)
let topological_closure l =
  let add l x = if List.mem x l then l else x :: l in
  let l = List.fold_left begin fun acc p ->
    add (List.fold_left add acc p.dependencies) p
  end [] l in
  List.rev l

module SSet = Set.Make(String)

let add_atom a l = match a, l with
  | A "", _ -> l
  | _ -> a :: l

let include_flags l =
  let pkgs = topological_closure l in
  let locations = List.fold_left begin fun acc p ->
    SSet.add p.location acc
  end SSet.empty pkgs in
  let flags = [] in
  (* includes *)
  let flags =
    List.fold_left begin fun acc l ->
      add_atom (P l) (add_atom (A "-I") acc)
    end flags (SSet.elements locations)
  in
  S (List.rev flags)
let compile_flags_byte = include_flags
let compile_flags_native = include_flags

let link_flags f l =
  let pkgs = topological_closure l in
  let locations = List.fold_left begin fun acc p ->
    SSet.add p.location acc
  end SSet.empty pkgs in
  let flags = [] in
  (* includes *)
  let flags =
    List.fold_left begin fun acc l ->
      add_atom (P l) (add_atom (A "-I") acc)
    end flags (SSet.elements locations)
  in
  (* special link options *)
  let flags =
    List.fold_left begin fun acc x ->
      add_atom (A x.link_options) acc
    end flags pkgs
  in
  (* archives *)
  let flags =
    List.fold_left begin fun acc x ->
      add_atom (A (f x)) acc
    end flags pkgs
  in
  S (List.rev flags)
let link_flags_byte = link_flags (fun x -> x.archives_byte)
let link_flags_native = link_flags (fun x -> x.archives_native)
