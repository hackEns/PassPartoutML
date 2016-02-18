open Eliom_lib
open Eliom_content
open Html5.D
open Eliom_tools.D

let loading_part () =
	div ~a:[a_id "loading-wrapper"] [ span ~a:[a_id "loading-p"] []]

let top_wrapper () =
	div ~a:[a_id "header-wrapper"] [ span ~a:[a_id "appname"] [ pcdata "PassPartout"]; 
		(try 
			span ~a:[a_id "welcome-msg"] [pcdata ("Hello " ^ User.get_login () ^ "!")]
		with 
		| User.Not_logged_in ->	span [ pcdata "Not logged in"]);
	loading_part ()]

let make_page l = html
				 ~title:"restricted area"
				 ~js:[["js";"sjcl.js"]]
				 ~css:[["css";"main.css"]]
				 ~other_head:[meta ~a:[a_name "viewport";
				                      a_content "user-scalable=no, initial-scale=1, maximum-scale=1, minimum-scale=1, width=device-width, height=device-height, target-densitydpi=device-dpi"]
									                () ]
				 (body (top_wrapper ()::l)
				)

