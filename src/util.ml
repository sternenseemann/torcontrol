open Nocrypto.Hash

module String = struct
  include String
  let map_to_list (f : char -> 'b) (s :string) =
    let rec map_tail_reverse f s i acc =
      if i >= 0
    then map_tail_reverse f s (i - 1) (f s.[i] :: acc)
    else acc
    in map_tail_reverse f s (String.length s - 1) []
end

let auth_hmac d =
  mac `SHA256
    (Cstruct.of_string "Tor safe cookie authentication server-to-controller hash") d

let id x = x

let string_of_opt_def val_f = function
  | (a, Some b) -> a ^ "=" ^ val_f b
  | (a, None) -> a

let list_of_option = function
  | Some x -> [ x; ]
  | None   -> []
