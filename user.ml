open Ocsipersist
open Lwt

let user_id_ref = 
	Eliom_reference.Volatile.eref
    	~scope:Eliom_common.default_session_scope
		None

let permission_table = (open_table "permissions":string list table)

let require role success failure = match role with
	| "" -> success ()
	| role ->
		match Eliom_reference.Volatile.get user_id_ref with
		| None -> failure()
		| Some login -> 
			try_lwt
				lwt user_permissions = find permission_table login in
				let _ = List.find (fun c -> c = role) user_permissions in
				success ()
			with
			| Not_found -> failure()


(* Save the login in the session variables, load permissions, create them if needed, etc. *)
let perform_login login =
	Eliom_reference.Volatile.set user_id_ref (Some login);
	try_lwt
		lwt user_permissions = find permission_table login in
		let _ = List.find (fun c -> c = "logged") user_permissions
		in return (); 
	with
	| Not_found -> add permission_table login ["logged"]

let get_login () =
	let Some login = Eliom_reference.Volatile.get user_id_ref in
	login
	
