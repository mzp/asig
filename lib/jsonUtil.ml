open Tiny_json.Json

let rec string_of_json = function
  | String s -> "\"" ^s^ "\""
  | Number x -> Printf.sprintf "%s" x
  | Object fs ->
    "{"
    ^String.concat "," (List.map (fun (k,v) -> "\"" ^k ^ "\" :" ^ string_of_json v) fs)
    ^"}"
  | Array xs ->
    "["
    ^String.concat "," (List.map string_of_json xs)
    ^"]"
  | Bool true -> "true"
  | Bool false -> "false"
  | Null -> "null"
