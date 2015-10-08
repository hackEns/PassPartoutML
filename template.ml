open Eliom_lib
open Eliom_content
open Html5.D
open Eliom_tools.D

let top_wrapper () =
	try 
		div [ p [ pcdata "PassPartout | "; pcdata ("Hello " ^ User.get_login ()) ] ]
	with 
	| User.Not_logged_in ->
		div [ p [ pcdata "Not logged in"]]

let make_page l = html
				 ~title:"restricted area"
				 ~js:[["js";"sjcl.js"]]
				 (body (top_wrapper ()::l)
				)

