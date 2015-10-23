open Eliom_lib
open Eliom_content
open Html5.D
open Dom
open Dom_html

type grid_column =
	| End
	| BoolCol of grid_column * (bool -> Dom_html.element Js.t)
	| TextCol of grid_column * (string -> Dom_html.element Js.t)

type grid_cell =
	| BoolCell of bool
	| TextCell of string

exception BadDataForModel

let label s =
	let span = createSpan document in
	let text = document##createTextNode (Js.string s) in
	appendChild span text;
	span

let custom_div children =
	let div = createDiv document in
	List.map (appendChild div) children;
	div


let grid_header = End

let grid_editable_boolean callback next = BoolCol(next, fun b -> (if b then label "true" else label "false"))

let grid_string next = TextCol(next, fun s -> label s)

let grid table_type content header_content =
	let rec create_elements = function
		| (End, []) -> []
		| (BoolCol(next_table_type, printer), (BoolCell(data))::q) -> (printer data)::(create_elements (next_table_type, q))
		| (TextCol(next_table_type, printer), (TextCell(data))::q) -> (printer data)::(create_elements (next_table_type, q))
	in

	custom_div List.(concat ( map (fun c -> (create_elements (table_type, c)) ) content))

