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

{client{
	let create_keyring_item s =
		let item_li = createLi document in
		appendChild item_li (document##createTextNode (Js.string s));
		item_li
	
	let load_keyrings keyring_list =
		List.iter (fun s ->
			let item_li = create_keyring_item s in
			appendChild keyring_list item_li;
			) (Engine.get_keyring_list ())

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
				let _ =  {unit{ load_keyrings (Eliom_content.Html5.To_dom.of_ul %keyring_list) }} in

				return (Template.make_page [keyring_list])
			)
		)
