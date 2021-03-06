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
	| CopiableTextCol of grid_column * (string -> Dom_html.element Js.t)

(* a small copy of eliom type system, which seems to be private atm *)
type +'a param_name = string

type file_dom = File.file Js.t

type _ atom =
  | TInt			: int atom
  | TBool    	  	: bool atom
  | TString   		: string atom
  | TStringPassword	: string atom
  | TFile           : (file_dom option) atom

type (_,_) params_type =
  | TProd : ( ('a, 'an) params_type * ('b, 'bn) params_type) -> (('a * 'b), 'an * 'bn) params_type
  | TAtom : (string * 'a atom) -> ('a, [`One of 'a] param_name) params_type
 
let int (n : string) = TAtom (n,TInt)

let bool (n : string) = TAtom (n,TBool)

let string (n : string) = TAtom (n,TString)
let string_password (n : string) = TAtom (n,TStringPassword)

let file (n : string) = TAtom(n, TFile)

let prod t1 t2 = TProd (t1,t2)

let ( ** ) = prod

let form :type a c. ?autocomplete:bool -> (a, c) params_type -> string -> (a -> 'e) -> 'd = fun ?autocomplete:(autocomplete=true) param send callback ->
	let div = createDiv document in
	div##.classList##add (Js.string "text-entry");
	let form = createForm document in
	form##setAttribute (Js.string "autocomplete") (Js.string (if autocomplete then "on" else "off"));
	let rec build_form : type a c. (a, c) params_type -> (unit -> a) =
		function
		| TProd(t1, t2) -> let f1 = build_form t1 in
			let f2 = build_form t2 in
			fun () -> ((f1 (), f2 ()) )
		| TAtom(n, TInt) ->
			let input = createInput ~_type:(Js.string "text") document in
			let () = appendChild form input in
			fun () -> int_of_string (Js.to_string input##.value)
		| TAtom(n, TBool) ->
			let checkbox = createLabel document in
			let span = createSpan document in
			let input = createInput ~_type:(Js.string "checkbox") document in
			let () = appendChild checkbox input in
			let () = appendChild checkbox span in
			let () = appendChild span (document##createTextNode (Js.string n)) in
			let () = appendChild form checkbox in
			fun () -> Js.to_bool input##.checked
		| TAtom(n, TString) ->
			let input = createInput ~_type:(Js.string "text") document in
			input##.placeholder := Js.string n;
			let () = appendChild form input in
			fun () -> Js.to_string input##.value
		| TAtom(n, TFile) ->
			let file = createLabel document in
			let input = createInput ~_type:(Js.string "file") document in
			let span = createSpan document in
			let () = appendChild span (document##createTextNode (Js.string n)) in
			let () = appendChild file span in
			let () = appendChild file input in
			let () = appendChild form file in
			fun () -> Js.Optdef.case (input##.files) (fun () -> None) (fun f -> Js.Opt.to_option (f##item(0)))
		| TAtom(n, TStringPassword) ->
			let input = createInput ~_type:(Js.string "password") document in
			input##.placeholder := Js.string n;
			let () = appendChild form input in
			fun () -> Js.to_string input##.value
	in
	let submit = createInput ~_type:(Js.string "submit") document in
	submit##.value := Js.string send;
	let f = build_form param in
	let _ =	Lwt_js_events.submits form (fun e _ -> Dom.preventDefault e;  callback (f () )) in
	appendChild form submit;
	appendChild div form;
	div

exception BadDataForModel

let label s =
	let span = createSpan document in
	let text = document##createTextNode (Js.string s) in
	appendChild span text;
	span

let clear elt = 
	let child = list_of_nodeList(elt##.childNodes) in
	List.iter (fun c -> let _ = (removeChild elt c) in ()) child


let change_label l s =
	clear l;
	let text = document##createTextNode (Js.string s) in
	appendChild l text
	

let custom_div children =
	let div = createDiv document in
	List.iter (appendChild div) children;
	div

let wrap_td elt =
	let td = createTd document in
	appendChild td elt; td

let line_to_tr line =
	let tr = createTr document in
	let tds = List.map wrap_td line in
	match tds with
	| [] -> tr
	| t::q -> (List.iter (fun e -> e |> appendChild tr) tds;
			   Lwt_js_events.clicks t (fun e _ -> Dom.preventDefault e; List.iter (fun e -> let classname = Js.string "widgets-table-visible-td" in
			   		 if e##.className = classname then 
					 	e##.className := Js.string ""
					 else e##.className := classname) tds; Lwt.return ());
			   tr)
	
let line_to_th line =
	let tr = createTh document in
	let tds = List.map wrap_td line in
	match tds with
	| [] -> tr
	| t::q -> (List.iter (fun e -> e |> appendChild tr) tds;
			   Lwt_js_events.clicks t (fun e _ -> Dom.preventDefault e; List.iter (fun e -> let classname = Js.string "widgets-table-visible-td" in
			   		 if e##.className = classname then 
					 	e##.className := Js.string ""
					 else e##.className := classname) tds; Lwt.return ());
			   tr)
	


let lines_to_table lines =
	let table = createTable document in
	table##.className := Js.string "widgets-table";
	match lines with
	| [] -> table
	| t::q -> (t |> line_to_th |> appendChild table; List.iter (fun e -> e |> line_to_tr |> appendChild table) q; table)



let grid_header = End

let grid_editable_boolean name callback next = BoolCol(next, fun b whole_line -> (
		let current_val = ref b in
		let display () = if !current_val then Format.sprintf "%s: yes" name else Format.sprintf "%s: no" name in
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
let grid_copiable_string next = CopiableTextCol(next, fun s ->
	let span = createSpan document in
	span##.className := Js.string "copiable-cell";
	let form = createForm document in
	let input = createInput ~_type:(Js.string "text") document in
	input##.value := Js.string s;
	input##.readOnly := Js.bool true;
	appendChild form input;
	appendChild span form;
	let a = createA document in
	appendChild a (document##createTextNode (Js.string "copy"));
	appendChild span a;
	let _ =	Lwt_js_events.clicks a (fun e _ -> Dom.preventDefault e; input##select; document##execCommand (Js.string "copy") (Js.bool false) Js.null; Lwt.return ()) in
	span
	)

let grid table_type content header_content =
	let rec create_elements whole_line = function
		| (End, []) -> []
		| (BoolCol(next_table_type, printer), (BoolCell(data))::q) -> (printer data whole_line)::(create_elements whole_line (next_table_type, q))
		| (TextCol(next_table_type, printer), (TextCell(data))::q) -> (printer data)::(create_elements whole_line (next_table_type, q))
		| (CopiableTextCol(next_table_type, printer), (TextCell(data))::q) -> (printer data)::(create_elements whole_line (next_table_type, q))
		| _ -> failwith "Invalid table"
	in

	lines_to_table @@ (List.map (fun (TextCell(s)) -> label s) header_content)::(List.map (fun c -> create_elements c (table_type, c)) content)

