open Ocsipersist
open Lwt

open Eliom_content
open Html5.D
open Html5.F
open Eliom_tools.F

open User

let display_logged_page url () =
	Template.make_page_redirect url [ p [pcdata "logged in"] ]


let get_page_url () =
	Eliom_request_info.(
	(if get_ssl () then "https://" else "http://") ^ (get_hostname ()) ^ ":" ^ (string_of_int @@ get_server_port ()) ^ "/" ^(Eliom_request_info.get_full_url ()))
	

let display_auths_mechanism services =
	let auths_list = List.map (fun (service, name) -> (a (service (display_logged_page (get_page_url ()))) [pcdata name] ())) services in
	return (Template.make_page [ p auths_list ])

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
				

				
