open Util

let nibble_to_hex_char n = match n with
  | n when n >= 0 && n <= 10 -> Char.chr (0x30 + n)
  | n when n >= 10 && n < 16 -> Char.chr (0x61 + (n - 10))
  | _ -> raise (Invalid_argument "Nibbles must be within the range of 0x0 and 0xf")

let hexdig_quote s =
  let char_to_hex_str c =
    let i  = Char.code c in
    let n1 = (i land 0xf0) lsr 4 in
    let n2 = i land 0x0f in
    let initfun = function
      | 0 -> nibble_to_hex_char n1
      | 1 -> nibble_to_hex_char n2
      | _ -> raise (Invalid_argument "Only indices between 0 and 1 are allowed")
    in String.init 2 initfun
  in String.concat "" (String.map_to_list char_to_hex_str s)

let quote_string s =
  let char_to_quoted_str c =
    match c with
      | '\n' -> "\\n"
      | '\\' -> "\\\\"
      | c    -> String.make 1 c
  in String.concat "" (String.map_to_list char_to_quoted_str s)

let quote_value (a, b) = (a, quote_string b)
