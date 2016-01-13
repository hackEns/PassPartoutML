open Eliom_lib
open Eliom_content
open Html5.D
open Dom
open Dom_html

type grid_cell =
	| BoolCell of bool
	| TextCell of string


type grid_column =
	| End
	| BoolCol of grid_column * (bool -> grid_cell list -> Dom_html.element Js.t)
	| TextCol of grid_column * (string -> Dom_html.element Js.t)

exception BadDataForModel

let label s =
	let span = createSpan document in
	let text = document##createTextNode (Js.string s) in
	appendChild span text;
	span

let text_entry s cb =
	let form = createForm document in
	let input = createInput document in
	appendChild form input;
	(*Lwt_js_events.submit form (fun _ _ ->  cb input##value);*)
	match s with
	| Some s -> (input##value <- (Js.string s); input)
	| None -> input

	
let clear elt = 
	let child = list_of_nodeList(elt##childNodes) in
	List.iter (fun c -> let _ = (elt##removeChild(c)) in ()) child


let change_label l s =
	clear l;
	let text = document##createTextNode (Js.string s) in
	appendChild l text
	

let custom_div children =
	let div = createDiv document in
	List.iter (appendChild div) children;
	div


let grid_header = End

let grid_editable_boolean callback next = BoolCol(next, fun b whole_line -> (
		let current_val = ref b in
		let display () = if !current_val then "true" else "false" in
		let l = label (display ()) in
		Lwt_js_events.(
		       async (fun () ->
				mousedowns l (fun _ _ ->
			current_val := not !current_val;
			callback !current_val whole_line >>= (fun () -> Lwt.return (change_label l (display ()) ))
		)));
		l
	))

let grid_string next = TextCol(next, fun s -> label s)

let grid table_type content header_content =
	let rec create_elements whole_line = function
		| (End, []) -> []
		| (BoolCol(next_table_type, printer), (BoolCell(data))::q) -> (printer data whole_line)::(create_elements whole_line (next_table_type, q))
		| (TextCol(next_table_type, printer), (TextCell(data))::q) -> (printer data)::(create_elements whole_line (next_table_type, q))
		| _ -> failwith "Invalid table"
	in

	custom_div List.(concat ( map (fun c -> (create_elements c (table_type, c)) ) content))

