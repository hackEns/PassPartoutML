open Ocsipersist
open Lwt

open Eliom_content
open Html5.D
open Html5.F
open Eliom_tools.F

open User

let display_logged_page url () =
	Template.make_page_redirect url [ p [pcdata "logged in"] ]


let display_auths_mechanism services =
	let auths_list = List.map (fun (service, name) -> (a service [pcdata name] ())) services in
	return (Template.make_page [ p auths_list ])

let require services role success = match role with
	| "" -> success ()
	| role ->
		match Eliom_reference.Volatile.get user_id_ref with
		| None -> display_auths_mechanism services
		| Some login -> 
			try%lwt
				let%lwt user_permissions = find permission_table login in
				let _ = List.find (fun c -> c = role) user_permissions in
				success ()
			with
			| Not_found -> display_auths_mechanism services
				

				
