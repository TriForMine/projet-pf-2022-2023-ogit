type commit = {
    parents : Digest.t list;
    date : float;
    message : string;
    content : Digest.t
}
    
let date_fm _d = let d = Unix.gmtime _d in
    Printf.sprintf "%02d:%02d:%02d-%02d/%02d/%04d" (d.Unix.tm_hour + 1) d.Unix.tm_min d.Unix.tm_sec d.Unix.tm_mday (d.Unix.tm_mon + 1) (d.Unix.tm_year + 1900)

let date_fm_to_float s = let d = Scanf.sscanf s "%d:%d:%d-%d/%d/%d" (fun h m s d mo y -> (Unix.mktime { 
    Unix.tm_hour = h; 
    Unix.tm_min = m; 
    Unix.tm_sec = s;
    Unix.tm_mday = d; 
    Unix.tm_mon = mo - 1; 
    Unix.tm_year = y - 1900;
    Unix.tm_wday = 0; Unix.tm_yday = 0; Unix.tm_isdst = false
  })) in
  fst d

let set_head _l = Out_channel.with_open_gen [Open_creat; Open_trunc; Open_text; Open_wronly] 0o777 (Sys.getcwd () ^ "/.ogit/HEAD") (fun file -> Out_channel.output_string file (String.concat ";" (List.map Digest.to_hex  _l)))

let get_head () = In_channel.with_open_text (Sys.getcwd () ^ "/.ogit/HEAD") (fun file ->  List.map Digest.from_hex (String.split_on_char ';' (In_channel.input_all file)))

let make_commit _s  _h = let date = Unix.time () in
    let parents = get_head () in
    { parents; date; message = _s; content = _h }

let init_commit () = let date = Unix.time () in
    let parents = [] in
    { parents; date; message = "init commit"; content = Objects.store_work_directory () }

let store_commit _c = let file_content = Printf.sprintf "%s\n%s\n%s\n%s" (String.concat ";" (List.map Digest.to_hex _c.parents)) (date_fm _c.date) _c.message (Digest.to_hex _c.content) in
    let hash = Digest.string file_content in
    let file = Sys.getcwd () ^ "/.ogit/logs/" ^ Digest.to_hex hash in
    Out_channel.with_open_gen [Open_creat; Open_trunc; Open_text; Open_wronly] 0o777 file (fun file -> Out_channel.output_string file file_content);
    hash

let read_commit _h = let path = Sys.getcwd () ^ "/.ogit/logs/" ^ (Digest.to_hex _h) in
    In_channel.with_open_text path (fun file -> let content = In_channel.input_all file in
        let lines = String.split_on_char '\n' content in
        let parents = List.map Digest.from_hex (String.split_on_char ';' (List.nth lines 0)) in
        let date = date_fm_to_float (List.nth lines 1) in
        let message = List.nth lines 2 in
        let content = Digest.from_hex (List.nth lines 3) in
        { parents; date; message; content })