open Angstrom

type status_type = Cleared | Uncleared

(* define the parsers for date
   format: "2022-05-02 "
   output: 20220502
*)
let year_p = take 4 <* string "-"
let month_p = take 2 <* string "-"
let day_p = take 2 <* string " "

let date =
  lift3 (fun a b c -> (a, b, c)) year_p month_p day_p <?> "date"
  >>| fun (a, b, c) -> int_of_string (a ^ b ^ c)

(* define parser for status
   format: "! " or "* "
   output: Cleared | Uncleared
*)
let status =
  take_while1 (function '*' -> true | '!' -> true | _ -> false)
  <* string " " <?> "status"
  >>| function
  | "*" -> Cleared
  | "!" -> Uncleared
  | _ -> Uncleared

(* define parser for vendor
   format: "[vendor] "
   output: "vendor"
*)
let vendor =
  string "[" *> take_till (function ']' -> true | _ -> false) <* string "] "

(* define parser for description
   format: " description\n"
   output: "description"
*)
let desc = take_till (function '\n' -> true | _ -> false) <* end_of_line

(* define parser for transaction_top
   format: "2022-05-02 * [vendor] some description"
   output: (20220502, Cleared, "vendor", "some description")
*)
let transaction_top_p =
  lift4 (fun a b c d -> (a, b, c, d)) date status vendor desc

let account =
  end_of_line *> string "  " *> take_till (function ' ' -> true | _ -> false)

let ammount = take_till (function ' ' -> true | _ -> false) *> end_of_line
let first_entry = lift2 (fun a b -> (a, b)) account ammount
let trasaction = lift2 (fun a b -> (a, b)) transaction_top_p first_entry

let regular_parse p s =
  match parse_string ~consume:Prefix p s with
  | Ok v -> v
  | Error msg ->
      failwith msg regular_parse trasaction
        {|2022-05-02 * [aa] this is some description!
  acc $22
|}
