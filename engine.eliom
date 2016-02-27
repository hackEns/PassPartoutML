open Ocsipersist

exception Keyring_exist
exception Keyring_bad_format

let keyring_table = (open_table "keyring":string table)

let get_keyring_list () = fold_table (fun s _ l -> Lwt.return (s::l)) keyring_table []

let get_keyring_data keyring_data = find keyring_table keyring_data

(* register the appropriate permissions *)
let _ = iter_table (fun s _ -> User.register_permission s) keyring_table
let _ = User.register_permission "create keyring"

let new_keyring name data =
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
	| Not_found -> (add keyring_table name data; User.register_permission name)

{client{

	(* site * username * password *)
	type keyring_entry = string * string * string


	let _ = 
		Js.Unsafe.eval_string "sjcl.random.startCollectors()"
	
	let replace input output content =
		(Js.Unsafe.coerce content)##replace(input, output)

	exception WrongPassword
	let decipher key data : string =
		try
			Js.to_string ((Js.Unsafe.js_expr "sjcl")##decrypt(Js.string key, Js.string data))
		with
		| _ -> raise WrongPassword


	let cipher key data : string =
		Js.to_string ((Js.Unsafe.js_expr "sjcl")##encrypt(Js.string key, Js.string data))

}}
