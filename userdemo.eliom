{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Html5.F
}}
open Config
open Lwt
open Ocsigen_messages

module Userdemo_app =
  Eliom_registration.App (
    struct
      let application_name = "userdemo"
    end)

module CasModule = Cas.Cas(Userdemo_app)
open CasModule


open Eliom_tools.F

let f (a:string) = console (fun() -> a)

let data_debug_login = "<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'> <cas:authenticationFailure code='INVALID_TICKET'> ticket &#039;ST-6447-Bd3L7XbK14clqffUdp2l-cas&#039; not recognized </cas:authenticationFailure> </cas:serviceResponse>"

(*let _ = cas_xml_is_successful_debug f  data_debug_login*)

(* let _ = f (cas_xml_get_login data_debug_login) *)

let _ = User.register_login_service CasModule.service_url

let _ = 
	Userdemo_app.register_service
		~path:["restricted_area"]
		~get_params: Eliom_parameter.unit
		(fun () () ->
			User.require
			"logged"
			(fun () -> return (html ~title:"restricted area" (body [ h2 [pcdata (User.get_login ())] ])))
		)
