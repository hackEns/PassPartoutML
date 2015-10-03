{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Html5.F
}}
open Config
open Cas
open Lwt
open Ocsigen_messages

module Userdemo_app =
  Eliom_registration.App (
    struct
      let application_name = "userdemo"
    end)

open Eliom_tools.F

let send_error str =
	Ocsigen_messages.errlog str;
	Lwt.return
        (html ~title:"error" (body [pcdata ("Error: " ^ str)]))

let f (a:string) = console (fun() -> a)

let data_debug_login = "<cas:serviceResponse xmlns:cas='http://www.yale.edu/tp/cas'> <cas:authenticationFailure code='INVALID_TICKET'> ticket &#039;ST-6447-Bd3L7XbK14clqffUdp2l-cas&#039; not recognized </cas:authenticationFailure> </cas:serviceResponse>"

(*let _ = cas_xml_is_successful_debug f  data_debug_login*)

(* let _ = f (cas_xml_get_login data_debug_login) *)

let _ =
	Userdemo_app.register_service 
		~path:[]
		~get_params:Eliom_parameter.(string "ticket")
		(fun ticket () ->
			try_lwt
				let cas_url = cas_server ^ "/serviceValidate?ticket=" ^ ticket ^ "&service=" ^ cas_service in
				lwt cas_data = download_data cas_url in 
				
				(* for now every user is logged in *)
				Eliom_state.set_volatile_data_session_group
					~scope:Eliom_common.default_session_scope
					"logged in";
			
				Lwt.return (html
					~title:"userdemo"
					~css:[["css";"userdemo.css"]]
					(body [
						 h2 [pcdata cas_data];
				]))
			with
			| CASConnectionError(error) -> send_error ("Could not connect to the CAS to check the authentification: " ^ error)
		)
