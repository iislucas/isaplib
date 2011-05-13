(*
  Title:    project.ML
            A modified version of the "use" function, called "make" 
            which saves state
  Author:   David Matthews and Lucas Dixon
  Copyright   David Matthews 2009

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Lesser General Public
  License as published by the Free Software Foundation; either
  version 2.1 of the License, or (at your option) any later version.
  
  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.
  
  You should have received a copy of the GNU Lesser General Public
  License along with this library; if not, write to the Free Software
  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
*)

(*
  This is a structure for Project/IDE specific features that gets added to the 
  PolyML structure. 
*)


structure PolyML = 
struct

open PolyML;
structure Project 
= struct

  structure Log = Log(val level_ref = Unsynchronized.ref 3); 
  val log2 = Log.log_stdout 2;

  val ideModeRef = Unsynchronized.ref false;

  fun apply_from_dir dir f x =
      if dir = "" then f x 
      else 
        let val old_dir = OS.FileSys.getDir(); in 
          let val _ = OS.FileSys.chDir dir;
              val y = f x;
              val _ = OS.FileSys.chDir old_dir;
          in y end handle e => (OS.FileSys.chDir old_dir; raise e)
        end;

  (* simple PolyML.use, but changes to files directory first *)
  fun basicUse path_to_file = 
      let val {dir, file} = OS.Path.splitDirFile path_to_file;
      in apply_from_dir dir PolyML.use file end;

  val projectDirRef = Unsynchronized.ref "";
  val saveSubDirRef = Unsynchronized.ref ".polysave";

  fun setProjectDir s = (OS.FileSys.chDir s; projectDirRef := s);
  fun setSaveSubDir s = (saveSubDirRef := s);

  fun fileOrDirExists s = (OS.FileSys.fileId s; true) 
      handle OS.SysErr _ => false;

  fun createDirs path =
    if path = "" orelse (OS.FileSys.isDir path handle OS.SysErr _ => false)
    then ()
    else (createDirs (OS.Path.dir path); OS.FileSys.mkDir path);

  (*
    This is a "use" function that the IDE calls in the prelude before a build.
    It takes a directory name and returns a "use"
    function that saves the state and dependencies in ".save" and ".deps" files
    within that directory.
    
    It is called by over-riding use e.g.
      val use = IDE.projectUse ".polysave" "/home/ldixon/myproject"
    to define a version of "use" for the rest of the compilation.
  *)
  fun makeProject saveSubDir projectDir =
  let
    (* The root directory is the directory that is assumed to be the root of 
       the project. For each source file within this directory with path a/b/c.ML
       there will be a corresponding saved state file projectDir/saveSubDir/a/b/c.ML .
       If "use" is called on a file that is not within the root directory no
       information will be saved for that file. *)
    val rootPath = OS.FileSys.fullPath projectDir;
    
    (* Get the root directory and save directory (typically .polysave).  
       Assumes root directory is the parent of the save directory. *)
    val fullSaveDirPath = 
      OS.Path.joinDirFile {dir = rootPath, file = saveSubDir};
    
    val _ = log2 ("projectUse1: saveSubDir: " ^ saveSubDir );    
    val _ = log2 ("projectUse2: rootPath: " ^ rootPath );
    val _ = log2 ("projectUse3: fullSaveDirPath: " ^ fullSaveDirPath );
    
    fun preUse fileName =
    let
      (* Create a directory hierarchy. *)
      (* Compute the full path to the actual file taking account of any
         change of directory then make it relative to the root. *)
      val fullFileName = OS.FileSys.fullPath fileName;
      val pathFromRoot = OS.Path.mkRelative { path = fullFileName, 
                                              relativeTo = rootPath };
      val _ = log2 ("projectUse4: fullFileName: " ^ fullFileName);
      val _ = log2 ("projectUse5: pathFromRoot: " ^ pathFromRoot);
      
      val filePathRelativeToRoot =
          (* Is the file in the root directory or a sub-directory or is it in
             some other directory? *)
          (case #arcs (OS.Path.fromString pathFromRoot) of
            topArc :: _ =>
              (* If the first part of the path is ".." then it's in some other
                 directory. *)
              if topArc = OS.Path.parentArc then NONE else SOME pathFromRoot
          |   _ => NONE) (* No path at all? *)
            handle Path => NONE 
                (* Different volumes: can't make relative path. *)
                 | OS.SysErr _ => NONE (* If fileName doesn't actually exist. *)
      val _ = log2 ("\nprojectUse6: filePathRelativeToRoot: " ^ 
      (case filePathRelativeToRoot of NONE => "NONE" | SOME s => s));
    in
      (case filePathRelativeToRoot of
        NONE => NONE (* Do nothing: we can't save it. *)
      | SOME fileName =>
        let
          val baseName = OS.Path.joinDirFile 
              { dir = fullSaveDirPath, file = fileName }

          val saveDirForThisFile = OS.Path.dir baseName;
          
          (* name of saved heap before file is used. *)
          val preUse_SaveFile =
              OS.Path.mkCanonical (OS.Path.joinBaseExt
                { base = baseName, ext = SOME "save" })
              
          (* name of saved heap after file is used. *)
          val postUse_SaveFile =
              OS.Path.mkCanonical (OS.Path.joinBaseExt
                { base = baseName, ext = SOME "save2" })
          
          val _ = log2 ("projectUse7: baseName: " ^ baseName );
          val _ = log2 ("projectUse8: preUse_SaveFile: " 
                         ^ preUse_SaveFile );
          
          (* Reset the save directory before we save so that it isn't set 
             in the saved state.  That means that "use" won't save the state
             unless it's explicitly asked to. *)
         in
          (* Create any containing directories. *)
          log2 ("projectUse8.1: creating directories...");
          createDirs saveDirForThisFile;
          log2 ("projectUse8.2: saving state...: " ^ preUse_SaveFile);
          map (fn s => print (s )) (PolyML.SaveState.showHierarchy());
          (* TODO: only save if we have something new to save *)
          (* Save the state. *)
          PolyML.SaveState.saveChild (preUse_SaveFile,
            List.length(PolyML.SaveState.showHierarchy()));
          (* Restore the ref. *)
          log2 ("projectUse8.3: state saved! \n");
          SOME (preUse_SaveFile, postUse_SaveFile)
        end handle (ex as OS.SysErr args) =>
          (TextIO.print (String.concat ["Exception SysErr(", 
           PolyML.makestring args, ") raised for ", fileName, "\n"]);
           raise ex))
    end;
  in
    fn originalName =>
    let
      (* Find the actual file name by following the suffixes.  
         This mirrors what "use" will do. *)
      (* use "f" first tries to open "f" but if that fails it tries "f.ML",
         "f.sml" etc. *)
      fun trySuffixes [] =
        (* Not found - attempt to open the original and pass back the
           exception. *)
        (TextIO.openIn originalName, originalName)
       |  trySuffixes (s::l) =
        (TextIO.openIn (originalName ^ s), originalName ^ s)
          handle IO.Io _ => trySuffixes l;
      (* First in list is the name with no suffix. *)
      val (inStream, fileName) = trySuffixes("" :: ! PolyML.suffixes);
      val fullFilename = OS.FileSys.fullPath fileName;
      val _ = log2 ("projectUse9: fullFilename: " ^ fullFilename );
    in
      case preUse fullFilename of NONE => basicUse fullFilename
      | SOME (preUse_SaveFile,postUse_SaveFile) =>
        (* check timestamp on filename and saved file, *)
        if (fileOrDirExists postUse_SaveFile) 
        andalso false (* TODO: enable: need timestampt for chained deps. e.g.    
                         earlier file may have changed, therefore need to
                         recompile. *) 
        andalso (OS.FileSys.modTime fullFilename) 
                < (OS.FileSys.modTime postUse_SaveFile) 
        then (* use the saved heap, avoids re-compilation *)
          PolyML.SaveState.loadState postUse_SaveFile
        else basicUse fullFilename
        (* handle IO.Io d => raise IO.Io d; *)
    end
  end;
  
  (* the make checks the ref var to see if we are in IDE mode. *)
  fun make n = 
      if !ideModeRef then makeProject (!saveSubDirRef) (! projectDirRef ) n
      else basicUse n;

  fun onload load = 
    let val p = !projectDirRef;
        val s = !saveSubDirRef;
        val i = !ideModeRef;
    in (load(); 
        projectDirRef := p; 
        saveSubDirRef := s; 
        ideModeRef := i) 
    end;
  
end; (* Project struct *)
end; (* PolyML struct *)

(* make sure Project refs are not re-set by heap loading *)
PolyML.onLoad PolyML.Project.onload;

