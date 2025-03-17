(*
	Abdulqadir Abuharrus
	November 2024
	
	This is the "Naqa" MD project module.
	Naqa MD, is a simple Markdown to HTML converter that utilizes OCaml's pattern matching and functional 
	features to process MD notation into HTML.*)

open Converter
open Md_types


(*  Read a file and store it's contents in list of String.
	  Each line is a separate string in the list.
	  Fail if a non MD file is given.*)
let read_file src =
	if String.ends_with ".md" src then 
		let ic = open_in src in 
			let rec read_file_line acc = 
				try
					let line = input_line ic in read_file_line (line :: acc)
				with End_of_file -> close_in ic; List.rev acc
				in read_file_line []
		else failwith "Not a Markdown file"
		

(*	Identify and parse a heading, and returns a variant `Heading` type that contains heading size and text.
	  Uses the the number of '#' elements to get sub-string of text.*)
let parse_heading line =
	let rec count_hashes idx =
    	if idx < String.length line && line.[idx] = '#' then count_hashes (idx + 1)
    	else idx in
	let hash_count = count_hashes 0 in 
  		if hash_count > 0 && hash_count <= 6 then
  			Some (Heading (hash_count, String.trim (String.sub line hash_count (String.length line - hash_count))))
		else
    		None

(*  Parse bulleted lists (both the `-` and `*` variety) and returns a 'BulletList` variant.*)
let parse_bullet_list line =
  (*Determining if a line is a bullet-point*)
	if String.length line > 2 && (String.sub line 0 2 = "- " || String.sub line 0 2 = "* ") then
		Some (BulletList (String.trim (String.sub line 2 (String.length line - 2))))
	else
		None
	
(*  Parse numbered lists (both the `-` and `*` variety) and returns a 'NumberedList` variant.*)
let parse_numbered_list line =
    (*Determining if a line is a numbered-list point.*)
	if String.length line > 2 && (String.sub line 0 2 = ". ") then
    	Some (NumberedList (String.trim (String.sub line 2 (String.length line - 2))))
  	else
    	None

(* Parse URL elements, extracting the text and the URL.*)
let parse_url line =
  try
    (* Find positions of the markers -> `[text](url)`*)
    let open_bracket = String.index line '[' in
    let close_bracket = String.index line ']' in
    let open_paren = String.index line '(' in
    let close_paren = String.index line ')' in

    (* Ensure correct order and boundaries.
    This mess simply finds the substrings from the line using the defined indices... *)
    if open_bracket < close_bracket && close_bracket < open_paren && open_paren < close_paren then
      let text = String.sub line (open_bracket + 1) (close_bracket - open_bracket - 1) in
      let url = String.sub line (open_paren + 1) (close_paren - open_paren - 1) in
      Some (Link (text, url))
    else
      None
  with
  | Not_found -> None  (* Handles cases where any of the markers are missing *)

  
(*  Generic parser to process a line and classify it into one of our defined Md_element types.*)
let line_parser line =
  match parse_heading line with
  | Some heading -> heading
  | None ->
    (match parse_bullet_list line with
    | Some bullet -> bullet
    | None ->
      (match parse_numbered_list line with
      | Some num_list -> num_list
      | None ->
        (match parse_url line with
        | Some link -> link
        | None -> Paragraph line)))
        
        
(* Write the converted document to a an HTML file*)
let write_to_file content output_path =
  let oc = open_out output_path in
    output_string oc content;
  close_out oc
      
let process_file src output_path =
  (* Read and parse the Markdown file *)
  let lines = read_file src in
  let md_elements = List.map line_parser lines in
  (* Convert parsed elements to HTML *)
  let body = Converter.bundle_html md_elements "" in
  let full_html = Converter.add_boilerplate body in
  (* Write the final HTML to the output file *)
  write_to_file full_html output_path


(*  "Main" function to run when executed, this is for later when compile into .exe

let () =
  if Array.length Sys.argv <> 3 then
    Printf.eprintf "Usage: %s <input.md> <output.html>\n" Sys.argv.(0)
  else
    let input_path = Sys.argv.(1) in
    let output_path = Sys.argv.(2) in
    try
      process_file input_path output_path;
      Printf.printf "File successfully converted and saved to %s\n" output_path
    with
    | Failure msg -> Printf.eprintf "Error: %s\n" msg
    | e -> Printf.eprintf "An unexpected error occurred: %s\n" (Printexc.to_string e)
*)
