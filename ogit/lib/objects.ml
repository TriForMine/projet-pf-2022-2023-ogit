type t =
| Text of string 
| Directory of (string * bool * Digest.t * t) list

let obj_to_list _obj = match _obj with
| Text s -> [s]
| Directory dir ->
    let rec loop acc = function
      | [] -> acc
      | (name, is_dir, digest, _) :: rest ->
        loop ((name ^ ";" ^ (if is_dir then "d" else "t") ^ ";" ^ (Digest.to_hex digest)) :: acc)
          rest
    in List.rev (loop [] dir)

let hash _obj = 
      Digest.string (String.concat "\n" (obj_to_list _obj))

let is_known _h = Sys.file_exists (Sys.getcwd () ^ "/.ogit/objects/" ^ (Digest.to_hex _h))

let read_text_object _h = In_channel.with_open_text (Sys.getcwd () ^ "/.ogit/objects/" ^ (Digest.to_hex _h)) (fun file -> In_channel.input_all file)

let store_object _obj = let list = obj_to_list _obj in
  let digest = hash _obj in
    Out_channel.with_open_gen [Open_creat; Open_trunc; Open_text; Open_wronly] 0o777 (Sys.getcwd () ^ "/.ogit/objects/" ^ (Digest.to_hex digest)) (fun file -> Out_channel.output_string file (String.concat "\n" list)); digest

let store_object_helper _obj dir = let tmp = Sys.getcwd () in
  Sys.chdir dir;
  let digest = store_object _obj in
    Sys.chdir tmp;
    digest

let rec work_directory_to_obj basedir =
  let dir = Sys.readdir (Sys.getcwd ()) in
  let rec loop acc = function
    | [] -> Directory (acc)
    | file :: rest -> if file = ".ogit" then loop acc rest else
      match Sys.is_directory file with
      | true -> Sys.chdir file; let newdir = work_directory_to_obj basedir in Sys.chdir ".."; loop ((file, true, store_object_helper newdir basedir, newdir) :: acc) rest
      | false -> let content = Text (In_channel.with_open_text (Sys.getcwd () ^ "/" ^ file) (fun f -> In_channel.input_all f))
        in loop ((file, false, store_object_helper content basedir, content) :: acc) rest
  in loop [] (Array.to_list dir)

let store_work_directory () = let basedir = Sys.getcwd () in store_object (work_directory_to_obj basedir)

let rec read_directory_object _h = let obj = read_text_object _h in
  let rec loop acc = function
    | [] -> acc
    | line :: rest -> let (name, is_dir, digest) = match String.split_on_char ';' line with
      | [name; "d"; digest] -> (name, true, digest)
      | [name; "t"; digest] -> (name, false, digest)
      | _ -> failwith "Invalid object"
      in loop ((name, is_dir, Digest.from_hex digest, if is_dir then read_directory_object (Digest.from_hex digest) else Text (read_text_object (Digest.from_hex digest))) :: acc) rest
  in Directory (List.rev (loop [] (String.split_on_char '\n' obj)))
  
let rec clean_work_directory () = let dir = Sys.readdir (Sys.getcwd ()) in
  let rec loop = function
    | [] -> ()
    | file :: rest -> if String.starts_with ~prefix:"." file then loop rest else
      match Sys.is_directory file with
      | true -> Sys.chdir file; clean_work_directory (); Sys.chdir ".."; Sys.rmdir file; loop rest
      | false -> Sys.remove file; loop rest
  in loop (Array.to_list dir)

let rec restore_work_directory _obj = let rec loop = function
    | [] -> ()
    | (name, is_dir, _, obj) :: rest -> if is_dir then
      begin
        Sys.mkdir name 0o777;
        Sys.chdir name;
        restore_work_directory obj;
        Sys.chdir ".."
      end
      else
        Out_channel.with_open_gen [Open_creat; Open_trunc; Open_text; Open_wronly] 0o777 name (fun file -> Out_channel.output_string file (match obj with Text s -> s | _ -> failwith "Invalid object")); loop rest
  in loop ((match _obj with Directory dir -> dir | _ -> failwith "Invalid object"))


(* If file exist in commit but doesn't exist in current work directory, add it
   If file exist in commit and in current file, with same content do nothing
   If conflict create 2 files "filename..cl" and "filename..cr" *)
let rec merge_work_directory_I _obj = let rec loop (has_conflict: bool) = function
    | [] -> has_conflict
    | (name, is_dir, _, obj) :: rest -> if is_dir then
      begin
        if not (Sys.file_exists name) then Sys.mkdir name 0o777;
        Sys.chdir name;
        ignore (merge_work_directory_I obj);
        Sys.chdir "..";
        loop has_conflict rest
      end
      else
        if not (Sys.file_exists name) then
          begin
            Out_channel.with_open_gen [Open_creat; Open_trunc; Open_text; Open_wronly] 0o777 name (fun file -> Out_channel.output_string file (match obj with Text s -> s | _ -> failwith "Invalid object"));
            loop has_conflict rest
          end
        else
          let content = In_channel.with_open_text name (fun f -> In_channel.input_all f) in
            if content <> (match obj with Text s -> s | _ -> failwith "Invalid object") then
              begin
                Out_channel.with_open_gen [Open_creat; Open_trunc; Open_text; Open_wronly] 0o777 (name ^ "..cl") (fun file -> Out_channel.output_string file content);
                Out_channel.with_open_gen [Open_creat; Open_trunc; Open_text; Open_wronly] 0o777 (name ^ "..cr") (fun file -> Out_channel.output_string file (match obj with Text s -> s | _ -> failwith "Invalid object"));
                loop true rest
              end
            else
              loop has_conflict rest
  in loop false ((match _obj with Directory dir -> dir | _ -> failwith "Invalid object"))