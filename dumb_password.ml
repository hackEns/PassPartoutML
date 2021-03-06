module DumbPassword (App: App.APP) = struct

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
	Eliom_registration.Html5.send
        (html ~title:"error" (body [pcdata ("Error: " ^ str)]))

let service_path = ["login"; "dumb_password"]

let password_table = (open_table "users":string table)

(*let _ = add password_table "root" "root"*)

let main_service =
    let get_service = Eliom_service.App.service
        ~path:service_path
        ~get_params:Eliom_parameter.(unit) () in
	let post_service = Eliom_service.App.post_service
		~fallback:get_service
        ~post_params:Eliom_parameter.((string "user") ** (string "password")) ()
	in
	let _ = Eliom_registration.Any.register post_service
        (fun () (user,password) ->
			try%lwt
				let%lwt real_password = find password_table user in
				if real_password = password then
					let%lwt () = User.perform_login user in
					Eliom_registration.Redirection.send App.welcome_service
				else raise BadPassword
			with
			| Not_found | BadPassword -> send_error "bad password"
		) in
	let _ = App.register get_service
        (fun () () ->
			let login_form = Form.post_form ~service:post_service (fun (user, password) ->
				[p [ pcdata "User";
					Form.input ~input_type:`Text ~name:user Form.string;
					pcdata "Password";
					Form.input ~input_type:`Password ~name:password Form.string;
					Form.input ~input_type:`Submit Form.string;
				]]) () in
            return (Template.make_page [login_form]))
    in
	get_service


end
