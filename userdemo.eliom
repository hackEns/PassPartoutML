{shared{
  open Eliom_lib
  open Eliom_content
  open Html5.D
  open Html5.F
}}
open Config
open Cas

exception CASConnectionError

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


let _ =
	Userdemo_app.register_service 
		~path:[]
		~get_params:Eliom_parameter.(string "ticket")
		(fun ticket () ->
			try
			begin
				let cas_url = cas_server ^ "/serviceValidate?ticket=" ^ ticket ^ "&service=" ^ cas_service in
				(download_data cas_url >>= fun (cas_data) ->
				
					Lwt.return (html
						~title:"userdemo"
						~css:[["css";"userdemo.css"]]
						(body [
							 h2 [pcdata cas_data];
					]))
					)
			end
			with
			| CASConnectionError -> send_error "Could not connect to the CAS to check the authentification."
		)
