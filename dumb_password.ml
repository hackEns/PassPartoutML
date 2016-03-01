module DumbPassword (App: Eliom_registration.ELIOM_APPL) = struct

open Lwt
open Config
open Ocsigen_messages
open Eliom_lib
open Eliom_content
open Html5.D
open Html5.F
open Eliom_tools.F

open Ocsipersist


exception BadPassword

let send_error str =
	Ocsigen_messages.errlog str;
	Lwt.return
        (html ~title:"error" (body [pcdata ("Error: " ^ str)]))

let service_path = ["login"; "dumb_password"]

let password_table = (open_table "users":string table)

(*let _ = add password_table "root" "root"*)

let main_service logged_callback =
    let get_service = Eliom_service.App.service
        ~path:service_path
        ~get_params:Eliom_parameter.(unit) () in
	let post_service = Eliom_service.App.post_service
		~fallback:get_service
        ~post_params:Eliom_parameter.((string "user") ** (string "password")) ()
	in
	let _ = App.register post_service
        (fun () (user,password) ->
			try_lwt
				lwt real_password = find password_table user in
				if real_password = password then
					lwt () = User.perform_login user in
					return (logged_callback ())
				else raise BadPassword
			with
			| Not_found | BadPassword -> send_error "bad password"
		) in
	let _ = App.register get_service
        (fun () () ->
			let login_form = Html5.F.post_form ~service:post_service (fun (user, password) ->
				[p [ pcdata "User";
					string_input ~input_type:`Text ~name:user ();
					pcdata "Password";
					string_input ~input_type:`Password ~name:password ();
					string_input ~input_type:`Submit ()
				]]) () in
            return (Template.make_page [login_form]))
    in
	get_service


end
