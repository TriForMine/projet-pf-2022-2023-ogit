(** fichier ogit.ml : le coeur de l'exécutable: parse la ligne de commande et appelle
    les commandes correspondantes (cf https://v2.ocaml.org/api/Arg.html) **)

let usage_msg = "ogit <command> [<args>]"
let command = ref ""
let args = ref []
let need_anonymous = ref true

let print_version () = print_endline "ogit version 0.1"; need_anonymous := false

let speclist = [
    ("-v", Arg.Unit print_version, "Display version");
    ("--version", Arg.Unit print_version, "Display version");
    ("init", Arg.Unit (fun () -> ()), "Initialize a new, empty repository");
    ("commit <description>", Arg.Unit (fun () -> ()), "Record changes to the repository");
    ("log", Arg.Unit (fun () -> ()), "Show commit logs");
    ("checkout <hash>", Arg.Unit (fun () -> ()), "Switch branches or restore working tree files");
    ("merge <hash>", Arg.Unit (fun () -> ()), "Join two or more development histories together");
]
    let anon_fun filename = if !command = "" then command := filename else args := filename :: !args
    
    let () = Arg.parse speclist anon_fun usage_msg;
    if !need_anonymous then
        let command = !command in
        let args = List.rev !args in
        match command with
        | "init" -> Ogitlib.Commands.ogit_init ()
        | "commit" -> Ogitlib.Commands.ogit_commit (String.concat " " args)
        | "log" -> Ogitlib.Commands.ogit_log ()
        | "checkout" -> Ogitlib.Commands.ogit_checkout (String.concat " " args)
        | "merge" -> Ogitlib.Commands.ogit_merge (String.concat " " args)
        | _ -> Arg.usage speclist usage_msg
    