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

let data_debug_login = "<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'> <cas:authenticationFailure code='INVALID_TICKET'> ticket &#039;ST-6447-Bd3L7XbK14clqffUdp2l-cas&#039; not recognized </cas:authenticationFailure> </cas:serviceResponse>"

(*let _ = cas_xml_is_successful_debug f  data_debug_login*)

(* let _ = f (cas_xml_get_login data_debug_login) *)

let require = User.require [CasModule.main_service, "CAS"; DumbPasswordModule.main_service, "Password"]


let service_stub param func =
  Eliom_registration.Ocaml.register_post_coservice'
    ~post_params:param
    (fun () p -> User.ensure_login () >>= fun () -> Lwt.return p >>= func)

let get_keyring_service = service_stub (Eliom_parameter.string "keyring_name") (fun keyring_name ->
	User.ensure_role keyring_name >>=
	(fun () -> Lwt.return (Engine.get_keyring_data keyring_name)))


let keyring_list_service = service_stub (Eliom_parameter.unit) (fun () -> Lwt.return (Engine.get_keyring_list ()))


{client{
	let create_keyring_item s =
		let item_li = createLi document in
		appendChild item_li (document##createTextNode (Js.string s));
		item_li

	let main_frame () = getElementById "main-frame"
	
	let load_keyrings keyring_list_ul =
		lwt keyring_list = Eliom_client.call_ocaml_service ~service:%keyring_list_service () () in
		Lwt.return (List.iter (fun s ->
			let item_li = create_keyring_item s in
			Lwt_js_events.mousedowns item_li (fun _ _ ->
				let child = list_of_nodeList((main_frame())##childNodes) in
				List.map (fun c -> ((main_frame ())##removeChild(c))) child;
				Lwt.return (appendChild (main_frame()) (document##createTextNode (Js.string "clicked"))));
				appendChild keyring_list_ul item_li
			) (keyring_list))

}}


let _ = 
	Userdemo_app.register_service
		~path:[]
		~get_params: Eliom_parameter.unit
		(fun () () ->
			require
			"logged"
			(fun () ->
				let keyring_list = ul [] in
				let _ =  {unit{ let _  = load_keyrings (Eliom_content.Html5.To_dom.of_ul %keyring_list) in () }} in

				return (Template.make_page [keyring_list; div ~a:[a_id "main-frame"] []])
			)
		)
