(** fichier commands.ml **)
(** fonctions représentant les commandes ogit **)


(** initialise un dépot ogit. Crée un premier commit à l'état actuel du dépot.
    Une exception est levée si répertoire .ogit existe déjà **) 
let ogit_init () = Sys.mkdir ".ogit" 0o777; 
                  Sys.mkdir ".ogit/objects" 0o777;
                  Sys.mkdir ".ogit/logs" 0o777;
                  let commit = Logs.init_commit () in
                  let commit_hash = Logs.store_commit commit in
                  Logs.set_head [commit_hash]

let ogit_commit _msg = failwith "TODO"

let ogit_checkout _hash = failwith "TODO"

let ogit_log () = failwith "TODO"

let ogit_merge _hash = failwith "TODO"