signature LOG = 
sig
  (* current level of logging *)
  val out_level_ref : int Unsynchronized.ref;

  val writeln_to_stream : TextIO.outstream -> string -> unit; 
  val stdout : string -> unit; 
  val stderr : string -> unit; 

  (* send this string std-out if log-level is above the int. *)
  val log_stdout : int -> string -> unit;
  (* send this string std-err if log-level is above the int. *)
  val log_stderr : int -> string -> unit;

  val print : 'a -> unit;
end;

(* handy little library for writing logs specific to a bit of code. *)
functor Log(val level_ref : int Unsynchronized.ref)
: LOG
= struct
  val out_level_ref = Unsynchronized.ref 0;

  fun writeln_to_stream stream s = 
      (TextIO.output (stream, s); TextIO.flushOut stream);
  
  fun stdout s = writeln_to_stream TextIO.stdOut (s ^ "\n");
  fun stderr s = writeln_to_stream TextIO.stdErr (s ^ "\n");

  fun log_stdout level s = if (!out_level_ref) > level then stdout s else ();
  fun log_stderr level s = if (!out_level_ref) > level then stderr s else ();

  fun print x = (PolyML.print x; ());
end;

