{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
(*  open Html5.F*)
open Dom
open Dom_html
}}
open Config
open Lwt
open Ocsigen_messages

module Userdemo_app =
  Eliom_registration.App (
    struct
      let application_name = "passpartout"
    end)

module CasModule = Cas.Cas(Userdemo_app)
module DumbPasswordModule = Dumb_password.DumbPassword(Userdemo_app)

open Eliom_tools.D

let f (a:string) = console (fun() -> a)

(*let data_debug_login = "<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'> <cas:authenticationFailure code='INVALID_TICKET'> ticket &#039;ST-6447-Bd3L7XbK14clqffUdp2l-cas&#039; not recognized </cas:authenticationFailure> </cas:serviceResponse>"*)

(*let _ = cas_xml_is_successful_debug f  data_debug_login*)

(* let _ = f (cas_xml_get_login data_debug_login) *)

let require = Auth.require [CasModule.main_service, "cas"; DumbPasswordModule.main_service, "password"]


let service_stub param func =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:param
    (fun () p -> User.ensure_login () >>= fun () -> Lwt.return p >>= func)

let get_keyring_service = service_stub (Eliom_parameter.string "keyring_name") (fun keyring_name ->
	User.ensure_role keyring_name >>=
	(fun () -> Engine.get_keyring_data keyring_name))

let write_keyring_service = service_stub Eliom_parameter.(string "keyring_name" ** string "content") (fun (keyring_name, content) ->
	User.ensure_role keyring_name >>=
	(fun () -> Engine.set_keyring_data keyring_name content))


let keyring_list_service = service_stub (Eliom_parameter.unit) (fun () -> Engine.get_keyring_list ())
	

let user_list_service = service_stub (Eliom_parameter.unit) (fun () ->
	User.ensure_role "" >>= User.list_users)

(* FIXME: check permissions here *)
let user_set_permission_service = service_stub (Eliom_parameter.((string "user") ** (string "permission_name") ** (bool "value"))) (fun (user, (perm, value)) ->
	User.ensure_role "" >>= fun () -> User.set_permission user perm value)

let keyring_create_new_service = service_stub (Eliom_parameter.(string "keyring_name" ** (string "data" ** (opt (file "data_file"))))) (fun (keyring_name, (data, data_file)) ->
	User.ensure_role "create keyring" >>= fun () ->
		match data_file with
		| None -> Engine.new_keyring keyring_name data
		| Some f -> lwt file_data = Server_ext.get_file_data f in
			Engine.new_keyring keyring_name file_data
		)


{client{


	let clear elt = 
		let child = list_of_nodeList(elt##childNodes) in
		List.iter (fun c -> let _ = (elt##removeChild(c)) in ()) child

	let loading_count = ref 0

	let update_loading_status () =
		let loading_p = getElementById "loading-p" in
		clear loading_p;
		if !loading_count > 0 then
			appendChild loading_p (Widgets.label "loading…")

	let start_loading () =
		incr loading_count; update_loading_status ()
	
	let end_loading () =
		decr loading_count; update_loading_status (); Lwt.return ()

	let create_keyring_item s =
		let item_li = createLi document in
		let a = createA document in
		a##href <- Js.string "#main-frame-wrapper";
		appendChild item_li a;
		appendChild a (document##createTextNode (Js.string s));
		item_li

	let main_frame () = getElementById "main-frame"

	let clear_main_frame () = clear (main_frame ())
	
	let get_from_server service param = Eliom_client.call_ocaml_service ~service:service () param
	
	let load_keyring keyring _ _ = 
		start_loading (); clear_main_frame ();
		
		try_lwt
			lwt keyring_data = get_from_server %get_keyring_service keyring in
			let entry = Widgets.form Widgets.((string_password "password")) "decrypt" (fun password ->
				try
					begin
					let data = (Engine.decipher password keyring_data) in
					let keyring_data = ref (Engine.load_data data) in
					let () = clear_main_frame () in
					let () = appendChild (main_frame ()) (document##createTextNode (Js.string data)) in
					let new_password = Widgets.form Widgets.(string "name" ** string "user" ** string_password "password") "add" (fun (name, (user, site_password)) ->
						keyring_data := (name, user, password)::(!keyring_data);
						lwt _ = get_from_server %write_keyring_service (keyring, Engine.cipher_data password !keyring_data) in
						Lwt.return ()

					) in
					let grid = Widgets.grid
						Widgets.(grid_string (grid_string (grid_string grid_header)))
						(List.map Widgets.(fun (a, b, c) -> TextCell(a)::TextCell(b)::TextCell(c)::[]) !keyring_data) [] in
					let () = appendChild (main_frame ()) grid in
					let () = appendChild (main_frame ()) new_password in
					Lwt.return ()
					end
				with
				| Engine.WrongPassword ->
					let () = appendChild (main_frame ()) (document##createTextNode (Js.string "wrong_password")) in Lwt.return ()
				) in
			let () = appendChild (main_frame ()) entry in
				
			end_loading ()
		with
		| Exception_on_server (s) ->
			let () = appendChild (main_frame()) (document##createTextNode (Js.string ("server error: " ^ s))) in
			end_loading ()
	
	let load_keyrings keyring_list_ul =
		start_loading ();
		lwt keyring_list = get_from_server %keyring_list_service () in
		let _ = List.iter (
			fun s ->
				let item_li = create_keyring_item s in
				Lwt_js_events.(
					async (fun () -> 
						mousedowns item_li (load_keyring s)
				));
				appendChild keyring_list_ul item_li
			) (keyring_list) in
		end_loading ()
	
	let menu = ref None

	
	let rec load_user_list () =
			start_loading ();
			clear_main_frame ();

			lwt (permission_list, user_list) = get_from_server %user_list_service () in
			let table_type = List.fold_right (fun p table_type ->
				Widgets.grid_editable_boolean (fun s whole_line ->
					match whole_line with
					| Widgets.TextCell(user)::q -> begin
						lwt _ = get_from_server %user_set_permission_service (user, (p, s)) in
						Lwt.return ()
					end
					| _ -> failwith "no id"
					) table_type) permission_list Widgets.grid_header in
			let table_type = Widgets.grid_string table_type in
			let user_list = List.map (fun (user, perm) ->
				(Widgets.TextCell(user)) :: List.map (fun p -> Widgets.BoolCell(List.mem p perm)) permission_list
			) user_list in
			let permission_header = List.map (fun s -> Widgets.TextCell s) permission_list in
			appendChild (main_frame ()) (Widgets.grid table_type user_list permission_header);
			end_loading ()
	
	let rec update_main_list () =
		match !menu with
		| Some main_list ->
			let _ = clear main_list in
			let _ = load_keyrings main_list in
			let _ = add_other_links main_list in
			()
		| None -> failwith "no main list to update"
	and widget_new_keyring () =
		let item_li = createP document in
		let form = Widgets.form Widgets.((string "keyring") ** ((string_password "password for a new keyring") ** (file "or an existing keyring file"))) ~autocomplete:false "create new keyring" (fun (keyring, (password, old_data)) ->
			start_loading ();
			clear_main_frame ();
			try_lwt
				lwt _ = get_from_server %keyring_create_new_service (keyring, ((Engine.cipher_data password Engine.empty_keyring), old_data)) in
				let () = appendChild (main_frame()) (document##createTextNode (Js.string (keyring ^ " added"))) in
				let _ = update_main_list () in
				end_loading ()
			with
			| Exception_on_server(s) ->
				let () = appendChild (main_frame()) (document##createTextNode (Js.string ("error adding the keyring, already exists? bad format? " ^ s))) in
				end_loading ()
			) in
		appendChild item_li form;
		item_li
	and add_other_links keyring_list_ul =
		let item_li = create_keyring_item "users" in
		appendChild keyring_list_ul item_li;
		Lwt_js_events.(
			async (fun () ->
				mousedowns item_li (fun _ _ -> load_user_list ())
		));
		let item_li = create_keyring_item "new" in
		appendChild keyring_list_ul item_li;
		Lwt_js_events.mousedowns item_li (fun _ _ ->
			start_loading ();
			clear_main_frame ();
			appendChild (main_frame ()) (widget_new_keyring ());
			end_loading ();
		)
}}


let _ = 
	Userdemo_app.register_service
		~path:[]
		~get_params: Eliom_parameter.unit
		(fun () () ->
			require
			"logged"
			(fun () ->
				let keyring_list = ul ~a:[a_id "main-menu"] [] in
				let _ =  {unit{
					Url.Current.set_fragment "main";
					menu :=  Some (Eliom_content.Html5.To_dom.of_ul %keyring_list);
					update_main_list ()
					}} in

				return (Template.make_page [div ~a:[a_id "main"] [keyring_list];
											div ~a:[a_id "main-frame-wrapper"] 
												[Raw.a ~a:[a_href (Raw.uri_of_string "#main")] [];
												div ~a:[a_id "main-frame-wrapper2"]
													[div ~a:[a_id "main-frame"] []]]])
			)
		)
