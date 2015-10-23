open Ocsipersist
open Lwt

open Eliom_content
open Html5.D
open Html5.F
open Eliom_tools.F

exception Not_logged_in
exception Not_allowed

let user_id_ref = 
	Eliom_reference.Volatile.eref
    	~scope:Eliom_common.default_session_scope
		None

let permission_table = (open_table "permissions":string list table)

let list_users () = fold_table (fun s p (q, l) -> return (q, ((s, p)::l))) permission_table (["perm"], [])

let display_auths_mechanism services =
	let auths_list = List.map (fun (service, name) -> (a service [pcdata name] ())) services in
	return (html ~title:"login needed" (body [ p auths_list ]))

let require services role success = match role with
	| "" -> success ()
	| role ->
		match Eliom_reference.Volatile.get user_id_ref with
		| None -> display_auths_mechanism services
		| Some login -> 
			try_lwt
				lwt user_permissions = find permission_table login in
				let _ = List.find (fun c -> c = role) user_permissions in
				success ()
			with
			| Not_found -> display_auths_mechanism services
				
				
let ensure_role = function
	| "" -> Lwt.return ()
	| role ->
		match Eliom_reference.Volatile.get user_id_ref with
		| None -> raise Not_allowed
		| Some login -> 
			try_lwt
				lwt user_permissions = find permission_table login in
				let _ = List.find (fun c -> c = role) user_permissions in
				Lwt.return ()
			with
			| Not_found -> raise Not_allowed



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
	match Eliom_reference.Volatile.get user_id_ref with
	| Some login -> login
	| _ -> raise Not_logged_in

let ensure_login () = let _ = get_login () in return ()

