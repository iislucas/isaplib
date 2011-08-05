(*
  Code for working with a PolyML project. 

  Starting a project: 
    PolyML.Project.set_state_unchanged;
    PolyML.Project.depends_on_files [ FILE_NAMES ]; 
  where FILE_NAMES is all files that are not called with PolyML.Project.make.

  For a file that loads other files: 
    PolyML.Project.use_root "PATH/ROOT.ML";

  For a file that compiles lots of ML stuff: 
    PolyML.Project.make "PATH/ROOT.ML";

  Once you have compiled everything you want (e.g. at the end of the project)
    PolyML.Project.save_project_child_state "PATH/PRJECT_NAME.polyml-heap";

  You can call:
    PolyML.Project.add_state_edittime TIME;
  for other dependencies, using the timestampt that is relevant to them.

  ASSUMES: all values are defined in files which are called by 
  PolyML.Project.make; (no values are defined in ROOT.ML files).
 
  What it does: in the directory of each "FILENAME.ML" file creates a
  sub-directory called ".polysave". A heap file called "FILENAME.ML.save" is
  created. This is a child heap of the last heap-file make. The heap-file is 
  the PolyML state before the file was used. When the .save file is newer than
  the last edit, the file is not used (because the next .save file will these
  edits already done).

  When using an IDE, you don't need to recompile everything, you can start from
  the save-file. Also means that re-compilation is faster, and only stuff that
  needs to be recompiled is recompiled (assuming dependent files are specified).  
*)
signature ML_PROJECT = sig
  structure Log : LOG 

  (* changes to the dir of the file and calls PolyML.use; this
     doesn't change the state to being changed. Thus it can be used for loading
     ROOT.ML files, which do nothing other than load other files. *)
  val use_root : string -> unit;
  
  (* changes to the dir of the file and calls PolyML.use; sets project to 
     having changed, and updated last heap to NONE. *)
  val basic_use : string -> unit;  

  (* checks time-stamps, if edited after the saved ".polysave/FILENAME.ML.save"
     file, then saves a .save file and changes to file's dir and uses it; else,
     does nothing more call depend_on_file. *)
  val make : string -> unit; 

  (* save a child heap state to this file-name.  *)
  val save_project_child_state : string -> unit;
  (* Given a filename, get the pre-save and post-save filenames for it *)
  val pre_savepath_of : string -> string;
  val post_savepath_of : string -> string;

  (* Returns true is new file is saved. 
     False if heap file exists and is newer than any edits. *)
  val heap_is_newer_than_edits : string -> bool;

  (* the current file being compiled: for introspection/debugging. *)
  val set_current_filename : string option -> unit;
  val get_current_filename : unit -> string option;
  val string_of_current_filename : unit -> string;

  (* name of subdirectory in which .save files are stored. *)
  val save_subdir_name_ref : string Unsynchronized.ref;

  (* true when state is unchanged or at an edit-time before the given time
     argument: i.e. if time is heap-timestamp, we can load the heap rather than
     recompile the file that made the heap. *)
  val state_edited_before : Time.time -> bool; 

  (* change the state *)
  val set_state_changed : unit -> unit;
  val set_state_unchanged : unit -> unit;
  val set_state_edittime : Time.time -> unit;
  (* updates time or unchanged state *)
  val add_state_edittime : Time.time -> unit;

  (* Declare that the current project depends on the file named in the string
     argument. Calls add_edit_time with file's time-stamp. *)
  val depend_on_file : string -> unit; 
  val depend_on_files : string list -> unit; 

  val state_is_Changed : unit -> bool; 
  val state_is_Unchanged : unit -> bool; 
  val state_is_Edited_sometime : unit -> bool; 

  (* latest heap saved. In case we need to load it, e.g. edit half way through
     loading old stuff: we then load up the latest heap before compiling the 
     new edit. *)
  val latest_heappath_ref : string option Unsynchronized.ref;
  val set_latest_heappath : string option -> unit;
  val get_latest_heappath : unit -> string option;
  (* loads latest heap, if it not NONE. *)
  val ensure_at_latest_heap : unit -> unit; 

  (* the on-load function: resets global vars when state is loaded. *)
  val onload : ((unit -> unit) -> unit);
end;


structure PolyML = 
struct

open PolyML;
structure Project 
: ML_PROJECT
= struct

  structure Log = Log(val level_ref = Unsynchronized.ref 3); 
  val log = Log.log;
  val log_err = Log.log ~2;

  (*** Global project refs ***)

  (* When save_states_ref is true, then for each file that make is called on, a 
     heap file is saved. *)
  val save_states_ref = Unsynchronized.ref false;

  (* The project filename currently being compiled. *)
  val current_filename_ref = Unsynchronized.ref (NONE : string option);
  fun set_current_filename n_opt = current_filename_ref := n_opt; 
  fun get_current_filename () = !current_filename_ref; 
  fun string_of_current_filename () = (case get_current_filename () 
        of NONE => "NONE" | SOME s => "SOME ("^s^")");  

  (* latest heap saved. In case we need to load it, e.g. edit half way through
     loading old stuff: we then load up the latest heap before compiling the 
     new edit. *)
  val latest_heappath_ref = Unsynchronized.ref (NONE : string option);
  fun set_latest_heappath s_opt = (latest_heappath_ref := s_opt); 
  fun get_latest_heappath () = (!latest_heappath_ref); 

  (* check if there is a latest heap state, and that the old state had not
     changed, but now it has. if so load it. For when we find
     outselves trying to load a file, but didn't load an of the earlier files.
  *)
  fun ensure_at_latest_heap () =
      (case get_latest_heappath () of NONE => ()
          | SOME s => 
            (log 2 ("ensure_at_latest_heap caused loading of heap:" ^ s);
             PolyML.SaveState.loadState s));

  (* the directory in which save_sub_dir_ref is created and in which .save files 
     are stored. *)
  val save_subdir_name_ref = Unsynchronized.ref ".polysave";
  fun set_save_subdir_name s = (save_subdir_name_ref := s);
  fun get_save_subdir_name () = !save_subdir_name_ref;

  (*** Basic filesystem ***)

  (* check if a file or directory exists. *)
  fun exists_in_filesys s = (OS.FileSys.fileId s; true) 
      handle OS.SysErr _ => false;

  (* apply function from the given directory. Changes back after application, 
     and if there is an error. *)
  fun apply_from_dir dir f x =
      if dir = "" then f x 
      else 
        let val old_dir = OS.FileSys.getDir(); in 
          let val _ = OS.FileSys.chDir dir;
              val result = f x;
              val _ = OS.FileSys.chDir old_dir;
          in result end handle e => (OS.FileSys.chDir old_dir; raise e)
        end;

  fun apply_from_file path_to_file f x = 
      let val {dir, file} = OS.Path.splitDirFile path_to_file;
          val _ = set_current_filename (SOME path_to_file);
          val result = apply_from_dir dir f file;
          val _ = set_current_filename NONE;
      in result end;

  (* create directories in path, as needed. *)
  fun create_dirs path =
    if path = "" orelse (OS.FileSys.isDir path handle OS.SysErr _ => false)
    then ()
    else (create_dirs (OS.Path.dir path); OS.FileSys.mkDir path);



  (*** Saving state ***)

  (* gives path to savedirectory for the filename with the additional postfix 
     added as a file-extension to the existing filename. *)
  fun savepath_of_filepath save_postfix filepath = 
      let
        val {file,dir} = OS.Path.splitDirFile filepath
        val save_dir = OS.Path.concat (dir, get_save_subdir_name ());
        val save_file = 
            OS.Path.joinBaseExt { base = file, ext = SOME save_postfix };
      in OS.Path.joinDirFile { dir = save_dir, file = save_file } end;

  val pre_savepath_of = savepath_of_filepath "save";
  val post_savepath_of = savepath_of_filepath "save2";

  fun string_of_save_hierarchy () = 
      let 
        val l = PolyML.SaveState.showHierarchy();
        val size = length l;
        val l' = if size >= 5 
                 then (List.take (l,2)) @ ["..."] @ (List.drop (l,size - 2))
                 else l
      in (List.foldr (fn (x,y) => " * "^x^"\n"^y) (" Total heapsize: " ^ (Int.toString size)) l') end;
  
  (* saves the child state in the given path, returns true if it saved a new 
     state; returns false is it wasn't needed (state newer than last edit 
     was found.) *)
  fun save_project_child_state save_path = 
      let (* make .polysave dir for preuse saved heaps. *)
        val _ = log 2 ("save_project_child_state " ^ (save_path));
        val {file,dir} = OS.Path.splitDirFile save_path
        val _ = create_dirs dir;
        val full_save_path = 
            OS.Path.joinDirFile {dir = OS.FileSys.fullPath dir, file = file};
        val _ = log 2 ("full_save_path: " ^ full_save_path);
        val depth = List.length(PolyML.SaveState.showHierarchy());
      in 
         (log 2 ("saving state...");
          log 3 ("previous heaps: ");
          log 3 (string_of_save_hierarchy ());
          (* save child heap; note save path is full, needed so later save
             paths can find this one. TODO: think about this, better (relative) 
             way? *)
          set_latest_heappath (SOME full_save_path);
          PolyML.SaveState.saveChild (full_save_path,
            if depth > 200 then 0 else depth); (* don't maintain a heirarchy more than 200 deep *)
          (* this is the latest heap path *)
          log 2 ("saved: " ^ full_save_path))
       end;

  (*** project state ***)

  (* This is to record when files need to be recompiled. *)
  (* IMPROVE: make this a proper graph-like structure: whenever any file that
     you depend on has changed, you need to recompile from that file.  *)
  (* TODO: use this! *)
  (* Change management *)
  datatype state = 
    (* project has changed, must recomiple *)
    Changed of unit
    (* when this time is after a save heap timestamp, we recompile. Otherwise 
       we load the save heap and skip the re-compilation. *)
  | EditAtTime of Time.time
    (* no changes known yet, try to use heap files if possible *)
  | Unchanged of unit;

  fun string_of_state (Changed ()) = "Changed()"
    | string_of_state (Unchanged ()) = "Unchanged()"
    | string_of_state (EditAtTime t) = "EditAtTime(" ^ (Time.toString t) ^ ")";

  val state_ref = Unsynchronized.ref (Changed ());
  fun get_state () = !state_ref;
  fun set_state_changed () = (state_ref := Changed ());
  fun set_state_unchanged () = (state_ref := Unchanged ());
  fun set_state_edittime t = (state_ref := EditAtTime t);
  fun add_state_edittime t2 = 
      (case !state_ref of Changed () => ()
       | Unchanged () => set_state_edittime t2
       | EditAtTime t => 
         (case Time.compare (t2,t) of LESS => () | EQUAL => () 
          | GREATER => set_state_edittime t2));
  fun depend_on_file s = add_state_edittime (OS.FileSys.modTime s);
  fun depend_on_files l = List.foldr (fn (s,()) => depend_on_file s) () l;

  fun state_edited_before t2 =
      (case !state_ref of Changed () => false
       | Unchanged () => true
       | EditAtTime t => 
         (case Time.compare (t2,t) of LESS => false | EQUAL => false
           | GREATER => true));

  (* checks if this heap is after last edit. *)
  fun heap_is_newer_than_edits heap_path =
      (exists_in_filesys heap_path
       andalso state_edited_before (OS.FileSys.modTime heap_path));

  fun state_is_Changed () = 
      case get_state() of Changed() => true | _ => false; 
  fun state_is_Unchanged () = 
      case get_state() of Unchanged() => true | _ => false; 
  fun state_is_Edited_sometime () = 
      case get_state() of EditAtTime _ => true | _ => false; 


  (* simple PolyML.use, but changes to files directory first, notes edit
     time for poject. *)
  fun use_root path_to_file = 
      let val {dir, file} = OS.Path.splitDirFile path_to_file;
      in 
        if state_is_Changed () 
        then save_project_child_state (pre_savepath_of path_to_file)
        else ();
        depend_on_file path_to_file;
        apply_from_file path_to_file PolyML.use file
      end;

  (* simple PolyML.use, but changes to files directory first, notes edit
     time for poject. *)
  fun basic_use path_to_file = 
      let val {dir, file} = OS.Path.splitDirFile path_to_file;
      in set_state_changed ();
         set_latest_heappath NONE;
         apply_from_file path_to_file PolyML.use file
      end;

  (* *)
  fun make path_to_file =
  let
    val oldstate = get_state(); 
    val _ = depend_on_file path_to_file;
    (* setup paths *)
    val preuse_save_path = pre_savepath_of path_to_file
    val postuse_save_path = post_savepath_of path_to_file
    (* logging for debugging *)
    val _ = log 2 ("path_to_file: " ^ path_to_file);
    val _ = log 2 ("old state: " ^ (string_of_state oldstate));
    val _ = log 2 ("preuse_save_path: " ^ preuse_save_path);
    val _ = log 2 ("postuse_save_path: " ^ postuse_save_path);
  in
    (* If file is unedited since last compilation then... *)
    if heap_is_newer_than_edits preuse_save_path
       andalso heap_is_newer_than_edits postuse_save_path
    then (* we skip compilation. *) 
      (set_latest_heappath (SOME (OS.FileSys.fullPath postuse_save_path));
          log 2 ("unmodified."); ())
    else
      ((* else if old state was also Changed, then no need to load heap as ML is already at the latest state (we compiled the last file). But otherwise, nothing has been loaded yet, so we need to make sure that we have the latest heap loaded before using compiling this file... *)
       if state_is_Changed () then () else ensure_at_latest_heap ();
       save_project_child_state preuse_save_path;
       log 2 ("using file...");
       basic_use path_to_file; 
       log 2 ("file used.");
       save_project_child_state postuse_save_path;
       log 2 ("completed.");
       ())
    (* handle IO.Io d => raise IO.Io d; *)
  end;

  fun onload load = 
    let val s = !save_subdir_name_ref;
        val st = !state_ref;
        val n = !current_filename_ref;
    in (load(); 
        state_ref := st;
        save_subdir_name_ref := s; 
        current_filename_ref := n;
       ()) 
    end;
  
end; (* Project struct *)
end; (* PolyML struct *)

(* make sure Project refs are not re-set by heap loading *)
PolyML.onLoad PolyML.Project.onload;
