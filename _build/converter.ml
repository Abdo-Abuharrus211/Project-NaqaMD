(*	This is the Converter module. It's responsible for converting MD to HTML. *)

open Md_types

(* Convert MD heading to HTML. If if lvl < 0 or lvl > 6 .*)
let heading_converter lvl txt =
	let lvl_str = (string_of_int lvl) in
		match lvl with
		| x when (x >= 1) && (x <= 6) -> "<h" ^ lvl_str ^ ">" ^ txt ^ "</h" ^ lvl_str ^ ">"
		| _ ->failwith "Error converting Heading"
		
		
(* Function that converts URLs into anchor tags *)
let anchor_converter txt url = 
    "<a href=\"" ^ url ^ "\">" ^ (String.to_seq txt |> Seq.filter (fun c -> c <> '\\') |> String.of_seq) ^  "</a>"

	
(* Convert a single BulletList item to an HTML <li> tag *)
let bullet_converter item =
  "<li>" ^ item ^ "</li>"

(* Convert a single NumberedList item to an HTML <li> tag *)
let numb_converter item =
  "<li>" ^ item ^ "</li>"

(* Group consecutive BulletList items into a single <ul> block *)
let rec group_bullet_items elements acc =
  match elements with
  | BulletList text :: rest -> group_bullet_items rest (acc ^ bullet_converter text)
  | _ -> "<ul>" ^ acc ^ "</ul>", elements

(* Group consecutive NumberedList items into a single <ol> block *)
let rec group_numbered_items elements acc =
  match elements with
  | NumberedList text :: rest -> group_numbered_items rest (acc ^ numb_converter text)
  | _ -> "<ol>" ^ acc ^ "</ol>", elements

(*	General converter for md_element to HTML.
	Calls the appropriate function to convert the element. *)
let md_html_converter md_ele =
  match md_ele with
  | Heading (lvl, txt) -> heading_converter lvl txt
  | Paragraph txt -> "<p>" ^ txt ^ "</p>"
  | Link (txt, url) -> anchor_converter txt url
  | BulletList item -> bullet_converter item
  | NumberedList item -> numb_converter item

(* Bundle all converted elements into a single string *)
let rec bundle_html elements acc =
  match elements with
  | [] -> acc
  | BulletList _ :: _ ->
    let ul_block, remaining = group_bullet_items elements "" in
    bundle_html remaining (acc ^ ul_block)
  | NumberedList _ :: _ ->
    let ol_block, remaining = group_numbered_items elements "" in
    bundle_html remaining (acc ^ ol_block)
  | ele :: rest -> bundle_html rest (acc ^ md_html_converter ele)

(* Add HTML boilerplate *)
let add_boilerplate body =
  "<!DOCTYPE html>\n<html>\n<head>\n<title>Converted Markdown</title>\n</head>\n<body>\n" ^ body ^ "\n</body>\n</html>"
