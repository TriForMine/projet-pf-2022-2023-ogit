(** fichier commands.ml **)
(** fonctions représentant les commandes ogit **)

(* fonction qui vérifie si un conflit est présent *)
let has_conflict () = Sys.readdir (Sys.getcwd ()) |> Array.to_list |> List.exists (fun x -> ((String.ends_with ~suffix:"..cl" x) || (String.ends_with ~suffix:"..cr" x)))

(** initialise un dépot ogit. Crée un premier commit à l'état actuel du dépot.
    Une exception est levée si répertoire .ogit existe déjà **)
let ogit_init () = Sys.mkdir ".ogit" 0o777; 
    Sys.mkdir ".ogit/objects" 0o777;
    Sys.mkdir ".ogit/logs" 0o777;
    let commit = Logs.init_commit () in
    let commit_hash = Logs.store_commit commit in
    Logs.set_head [commit_hash] 

let ogit_commit _msg = 
    if has_conflict () then raise (Failure "Conflit détecté")
    else
        let commit = Logs.make_commit _msg (Objects.store_work_directory ()) in
        let commit_hash = Logs.store_commit commit in
        Logs.set_head [commit_hash]

let ogit_checkout _hash = if has_conflict () then raise (Failure "Conflit détecté")
    else
        let commit_hash = Digest.from_hex _hash in 
        let _commit = Logs.read_commit commit_hash in
        let obj = Objects.read_directory_object _commit.content in
            begin
                Objects.clean_work_directory () ; Objects.restore_work_directory obj
            end
        
let ogit_log () = let files = Sys.readdir ".ogit/logs" in
    let rec print_log _files = match _files with
    | [] -> ()
    | h::t -> begin
        let commit = Logs.read_commit (Digest.from_hex h) in
        print_string (h ^ " : " ^ commit.message ^ "\n" ); print_log t
    end in print_log (Array.to_list files)

let ogit_merge _hash = let commit_hash = Digest.from_hex _hash in 
    let _commit = Logs.read_commit commit_hash in 
    if Objects.merge_work_directory_I (Objects.read_directory_object _commit.content) then
        begin
            Printf.printf "CONFLICT (object): Merge conflict in %s\nAutomatic merge failed; fix conflicts and then commit the result" _hash;
            Logs.set_head (commit_hash :: Logs.get_head ()) 
        end
        else
        ogit_commit "merge"