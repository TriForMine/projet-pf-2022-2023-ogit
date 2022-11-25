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

let ogit_commit _msg = 
    let commit = Logs.make_commit _msg (Objects.store_work_directory ()) in
    let commit_hash = Logs.store_commit commit in
    Logs.set_head [commit_hash]

let ogit_checkout _hash = let commit_hash = Digest.from_hex _hash in 
    let _commit = Logs.read_commit commit_hash in
    Objects.clean_work_directory (); Objects.restore_work_directory (Objects.read_directory_object _commit.content)
    

let ogit_log () = failwith "TODO"

let ogit_merge _hash = let commit_hash = Digest.from_hex _hash in 
    let _commit = Logs.read_commit commit_hash in 
    if Objects.merge_work_directory_I (Objects.read_directory_object _commit.content) then
        ogit_commit "merge"