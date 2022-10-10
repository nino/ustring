val length : string -> int
(** Get the number of unicode grapheme clusters in a UTF-8 encoded string. *)

val compare : ?locale:string -> string -> string -> int
(** Super-duper WIP! (Basically, I haven't even started implementation yet! I'm
    still setting up the tests!) Compare two UTF-8 encoded strings using the
    unicode collation algorithm. Locale defaults to en-us. *)

val codepoint_to_utf8 : int -> (string, string) Result.t
(** [codepoint_to_utf8 codepoint] converts the unicode code point
    [codepoint] to its equivalent UTF-8 string. *)
