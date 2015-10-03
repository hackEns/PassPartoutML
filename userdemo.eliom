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

let user_id_ref = 
	Eliom_reference.Volatile.eref
    	~scope:Eliom_common.default_session_scope
		None

let _ =
	Userdemo_app.register_service 
		~path:[]
		~get_params:Eliom_parameter.(opt (string "ticket"))
		(fun ticket () ->
			match ticket with
			| Some ticket ->
				begin
				try_lwt
					let cas_url = cas_server ^ "/serviceValidate?ticket=" ^ ticket ^ "&service=" ^ cas_service in
					lwt cas_data = download_data cas_url in 
					let user_id = cas_xml_get_login cas_data in
					Eliom_reference.Volatile.set user_id_ref (Some user_id);
					
					(* for now every user is logged in *)
	(*				Eliom_state.set_volatile_data_session_group
						~scope:Eliom_common.default_session_scope
						"logged in";*)
				
					Lwt.return (html
						~title:"userdemo"
						~css:[["css";"userdemo.css"]]
						(body [
							 h2 [pcdata cas_data];
					]))
				with
				| CASConnectionError(error) -> send_error ("Could not connect to the CAS to check the authentification: " ^ error)
				| CASDataError(error) -> send_error ("CAS data not recognized: " ^ error)
				end
			| None -> match Eliom_reference.Volatile.get user_id_ref with
				| None -> return (html ~title:"" (body [ h2 [pcdata "none"] ]))
				| Some logi -> return (html ~title:"" (body [ h2 [pcdata logi] ]))
		)
