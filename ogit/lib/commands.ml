(** fichier commands.ml **)
(** fonctions représentant les commandes ogit **)

let has_conflict () = Sys.readdir (Sys.getcwd ()) |>
    Array.to_list |>
    List.exists (fun x -> ((String.ends_with ~suffix:"..cl" x) || (String.ends_with ~suffix:"..cr" x)))

let find_hash hash = if String.length hash < 4 then raise (Failure "Hash invalide")
    else
        let files = Array.append (Sys.readdir (Sys.getcwd () ^ "/.ogit/objects")) (Sys.readdir (Sys.getcwd () ^ "/.ogit/logs")) in
        let rec find_hash_aux hash files =
            match files with
            | [] -> raise (Failure "Hash invalide")
            | f::fs -> if f = hash || String.starts_with ~prefix:hash f then
                    f
                else find_hash_aux hash fs
        in find_hash_aux hash (Array.to_list files)

(** initialise un dépot ogit. Crée un premier commit à l'état actuel du dépot.
    Une exception est levée si répertoire .ogit existe déjà **)
let ogit_init () = if Sys.file_exists ".ogit" then raise (Failure "Dépot déjà initialisé") else begin
        Sys.mkdir ".ogit" 0o777;
        Sys.mkdir ".ogit/objects" 0o777;
        Sys.mkdir ".ogit/logs" 0o777;
        let commit_hash = (Logs.init_commit () |> Logs.store_commit) in
        Logs.set_head [commit_hash]
    end

let ogit_commit _msg =
    if has_conflict () then raise (Failure "Conflit détecté")
    else
        let commit_hash = (Objects.store_work_directory () |> Logs.make_commit _msg |> Logs.store_commit) in
        Logs.set_head [commit_hash]

let ogit_checkout _hash = if has_conflict () then raise (Failure "Conflit détecté")
    else
        let _commit = (_hash |> find_hash |> Digest.from_hex |> Logs.read_commit) in
        let obj = Objects.read_directory_object _commit.content in
            begin
                Objects.clean_work_directory () ; Objects.restore_work_directory obj
            end

(** Get all logs from .ogit/logs, without any duplicate, and ordered by date **)
let get_logs () = let a = Sys.readdir (Sys.getcwd () ^ "/.ogit/logs") |>
        Array.map(fun x -> Digest.from_hex x) |>
        Array.map (fun x -> (x, x |> Logs.read_commit)) in
        let _ = Array.sort (fun (_, c1) (_, c2) -> compare c1 c2) a in
        a |> Array.map (fun x -> fst x) |>
        Array.to_list

(** same as git log **)
let ogit_log () = let rec aux = function
    | [] -> ()
    | h::t -> let commit = Logs.read_commit h in
        Printf.printf "\027[31mcommit %s\027[0m\n" (Digest.to_hex h);
        print_endline ("Date: "^ (Logs.date_fm commit.date));
        print_endline "";
        print_endline commit.message;
        print_endline ""; aux t
    in aux (get_logs ())

(** same as git status **)

let ogit_merge _hash = let commit_hash = _hash |> find_hash |> Digest.from_hex in
    let _commit = Logs.read_commit commit_hash in
    if Objects.merge_work_directory_I (Objects.read_directory_object _commit.content) then
        begin
            Printf.printf "CONFLICT (object): Merge conflict in %s\nAutomatic merge failed; fix conflicts and then commit the result" _hash;
            Logs.set_head (commit_hash :: Logs.get_head ())
        end
        else
        ogit_commit ("Merge " ^ _hash)
