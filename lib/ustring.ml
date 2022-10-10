let length = Uuseg_string.fold_utf_8 `Grapheme_cluster (fun x _ -> x + 1) 0
let compare ?locale a b = (-1)

let codepoint_to_utf8 codepoint =
  if codepoint < 0 then Error "there are no negative unicode code points"
  else if 0x0 <= codepoint && codepoint <= 0x7F then
    Ok (String.of_char (Char.of_int_exn codepoint))
  else if 0x80 <= codepoint && codepoint <= 0x7FF then
    Ok
      (String.of_char_list
         [
           Char.of_int_exn (0b1100_0000 lor ((codepoint lsr 6) land 0b00011111));
           Char.of_int_exn (0b1000_0000 lor (codepoint land 0b00111111));
         ])
  else if 0x800 <= codepoint && codepoint <= 0xFFFF then
    Ok
      (String.of_char_list
         [
           Char.of_int_exn (0b1110_0000 lor ((codepoint lsr 12) land 0b00001111));
           Char.of_int_exn (0b1000_0000 lor ((codepoint lsr 6) land 0b00111111));
           Char.of_int_exn (0b1000_0000 lor (codepoint land 0b00111111));
         ])
  else if 0x10000 <= codepoint && codepoint <= 0x10FFFF then
    Ok
      (String.of_char_list
         [
           Char.of_int_exn (0b1111_0000 lor ((codepoint lsr 18) land 0b00000111));
           Char.of_int_exn (0b1000_0000 lor ((codepoint lsr 12) land 0b00111111));
           Char.of_int_exn (0b1000_0000 lor ((codepoint lsr 6) land 0b00111111));
           Char.of_int_exn (0b1000_0000 lor (codepoint land 0b00111111));
         ])
  else Error "not a valid unicode code point"

