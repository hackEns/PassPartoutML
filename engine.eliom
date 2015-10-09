let get_keyring_list () = ["test1"; "test2"; "test3"]

let get_keyring_data keyring_data = "chop"

{client{

(* site * username * password *)
type keyring_entry = string * string * string

let decrypt_data data password = data

}}
