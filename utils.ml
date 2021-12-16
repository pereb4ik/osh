(* Extra get *)
let eget (a, _) = match a with
| Some(s) -> s
| None -> ""

let tilde ?(name="") () =
  let open Unix in
  match name with
  | "" -> getenv "HOME"
  | name -> (getpwnam(name)).pw_dir