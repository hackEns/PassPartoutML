open Ocsipersist

exception Keyring_exist
exception Keyring_bad_format

let keyring_table = (open_table "keyring":string table)

let get_keyring_list () = fold_table (fun s _ l -> Lwt.return (s::l)) keyring_table []

let get_keyring_data keyring_data = "chop"

(* register the appropriate permissions *)
let _ = iter_table (fun s _ -> User.register_permission s) keyring_table
let _ = User.register_permission "create keyring"

let new_keyring name =
	let check_good_name name =
		for i = 0 to (String.length name) - 1 do
			if name.[i] < 'A' || name.[i] > 'z' then
				raise Keyring_bad_format
		done;
	in
	check_good_name name;
	try_lwt
		lwt _ = find keyring_table name in
		raise_lwt Keyring_exist
	with
	| Not_found -> add keyring_table name ""

{client{

(* site * username * password *)
type keyring_entry = string * string * string

let decrypt_data data password = data

}}
