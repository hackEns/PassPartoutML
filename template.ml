open Eliom_lib
open Eliom_content
open Html5.D
open Eliom_tools.D

let loading_part () =
	div ~a:[a_id "loading-wrapper"] [ span ~a:[a_id "loading-p"] []]

let top_wrapper () =
	try 
		div ~a:[a_id "header-wrapper"] [ span ~a:[a_id "appname"] [ pcdata "PassPartout"]; span ~a:[a_id "welcome-msg"] [pcdata ("Hello " ^ User.get_login () ^ "!")]; loading_part() ]
	with 
	| User.Not_logged_in ->
		div [ p [ pcdata "Not logged in"]]

let make_page l = html
				 ~title:"restricted area"
				 ~js:[["js";"sjcl.js"]]
				 ~css:[["css";"main.css"]]
				 ~other_head:[meta ~a:[a_name "viewport";
				                      a_content "user-scalable=no, initial-scale=1, maximum-scale=1, minimum-scale=1, width=device-width, height=device-height, target-densitydpi=device-dpi"]
									                () ]
				 (body (top_wrapper ()::l)
				)

