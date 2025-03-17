(* Define a type for Markdown elements *)
type md_element =
  | Heading of int * string
  | BulletList of string
  | NumberedList of string
  | Paragraph of string
  | Link of string * string

