open Lwt
open Ocsigen_messages

exception CASConnectionError of string
exception CASDataError of string
exception XMLDataError

(* Download the data at the url specified by url, via a Lwt.
 * Throws CASConnectionError as Lwt exceptions. *)
let download_data url =
	try_lwt
		lwt http_frame = Ocsigen_http_client.get_url url in
		match Ocsigen_http_frame.(http_frame.frame_content) with
		| Some stream ->
			begin
			let real_stream = Ocsigen_stream.get stream in
			let str = Ocsigen_stream.string_of_stream 10000 real_stream in
			Ocsigen_stream.finalize stream `Success >>= fun () -> 
				str
			end
		| None -> fail (CASConnectionError "Ocsigen did not provide a valid http stream.")
	with
	| Ocsigen_lib.Ip_address.No_such_host -> fail (CASConnectionError "no such host") 
	| CASConnectionError(e) -> fail (CASConnectionError(e))
	| ex -> fail (CASConnectionError ("unknown error " ^ Printexc.to_string ex))

type xml_tree_checker =
	| Node of string * xml_tree_checker
	| Attribute of string * string
	| InlineData


let rec get_xml_value xml tree = match (xml, tree) with
	| Simplexmlparser.Element(tag, _, children), Node(tag2, child_tree) when tag = tag2 ->
		begin
		let rec iter_children = function
		| [] -> raise XMLDataError
		| t::q ->
			try
				get_xml_value t child_tree
			with
			| XMLDataError -> iter_children q
		in
		iter_children children
		end
	| Simplexmlparser.Element(tag, attributes, _), Attribute(tag2, attribute) when tag = tag2->
		begin
		try
			snd (List.find (fun (name, value) -> name = attribute) attributes)
		with
		| Not_found -> raise XMLDataError
		end
	| Simplexmlparser.PCData(s), InlineData -> s
	| _ -> raise XMLDataError

(* Read the xml CAS data and return the user login.
 * Throws a CASDataError if anything wrong occurs. *)
let cas_xml_get_login data =
	let xml::[] = Simplexmlparser.xmlparser_string data in
	get_xml_value xml (Node("cas:serviceResponse", Attribute("cas:authenticationSuccess", "user")))

		
let cas_xml_is_successful_debug func data =
	let xml::[] = Simplexmlparser.xmlparser_string data in
	match xml with
	| Element(a, t, children) -> List.map (fun (a, b) -> func a; func b) t

