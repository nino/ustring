open OUnit2
open Stdio

let unicode_of word = Int.of_string ("0x" ^ word) |> Caml.Uchar.of_int
let between value min max = value >= min && value <= max

let parse line =
  let words = String.split ~on:' ' line in
  let buf = Buffer.create 4 in
  List.iter words ~f:(fun word ->
      match Ustring.codepoint_to_utf8 (Int.of_string ("0x" ^ word)) with
      | Ok res -> Buffer.add_string buf res
      | Error err ->
          printf "Invalid code point %s: %s\n" word err;
          ());
  Buffer.contents buf

let process_test_file_line (line : string) : string option =
  let comment_regex = Re.Perl.compile_pat "\\#.*$" in
  let without_comments = Re.replace_string comment_regex ~by:"" line in
  match String.strip without_comments with
  | "" -> None
  | stripped -> Some (parse stripped)

let iter_pairs ~f = function
  | [] -> ()
  | hd :: tl ->
      List.fold tl ~init:hd ~f:(fun prev current ->
          f prev current;
          current);
      ()

let str_res_printer = function
  | Ok str -> "Ok «" ^ str ^ "»"
  | Error str -> "Error «" ^ str ^ "»"

let read_test_data () : string list =
  In_channel.read_lines
    "../../../test/assets/CollationTest_NON_IGNORABLE_SHORT.txt"
  |> List.filter_map ~f:process_test_file_line

let tests =
  [
    ( "code point to UTF-8" >:: fun _ ->
      assert_equal ~printer:str_res_printer (Ok "A")
        (Ustring.codepoint_to_utf8 0x41);
      assert_equal ~printer:str_res_printer (Ok "\u{1F42B}")
        (Ustring.codepoint_to_utf8 0x1F42B) );
    ( "sorting works" >:: fun _ ->
      iter_pairs (read_test_data ()) ~f:(fun prev next ->
          assert_equal 1 (Ustring.compare prev next)) );
  ]

let () = run_test_tt_main ("unicode collation works" >::: tests)
