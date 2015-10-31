open Ocsipersist

let keyring_table = (open_table "keyring":string table)

let get_keyring_list () = fold_table (fun s _ l -> Lwt.return (s::l)) keyring_table []

let get_keyring_data keyring_data = "chop"

(* register the appropriate permissions *)
let _ = iter_table (fun s _ -> User.register_permission s) keyring_table

let _ = add keyring_table "blah" "blah blah blah"

{client{

(* site * username * password *)
type keyring_entry = string * string * string

let decrypt_data data password = data

}}
