open Angstrom

type status_type = Cleared | Uncleared

let regular_parse p s =
  match parse_string ~consume:Prefix p s with
  | Ok v -> v
  | Error msg -> failwith msg

let year = take 4 <* string "-"
let month = take 2 <* string "-"
let day = take 2

let date =
  lift3 (fun a b c -> (a, b, c)) year month day <?> "date" >>| fun (a, b, c) ->
  int_of_string (a ^ b ^ c)

let status =
  string " " *> take_while1 (function '!' -> true | '*' -> true | _ -> false)
  <?> "status"
  >>| function
  | "!" -> Uncleared
  | "*" -> Cleared
  | _ -> Uncleared

let vendor = string " [" *> take_till (function ']' -> true | _ -> false)
let desc = string "] " *> end_of_line

let transaction_line_p =
  lift4 (fun a b c d -> (a, b, c, d)) date status vendor desc

let account =
  end_of_line *> string "  " *> take_till (function ' ' -> true | _ -> false)

let ammount = take_till (function ' ' -> true | _ -> false) *> end_of_line
let first_entry = lift2 (fun a b -> (a, b)) account ammount
let trasaction = lift2 (fun a b -> (a, b)) transaction_line_p first_entry;;

regular_parse trasaction
  "2022-05-02 * [aa] this is some description!\n  acc $22"
