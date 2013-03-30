signature NAME_FUNCTION =
sig
  structure DomSet : NAME_SET
  structure CodSet : NAME_SET
  
  type dom = DomSet.name
  type cod = CodSet.name
  
  type rep
  type T = (dom,cod,rep) Map.amap
  
  (* forward lookup *)
  val get : T -> dom -> cod
  val get_opt : T -> dom -> cod option
  (*val img : T -> dom -> CodSet.T*)
  val img_of_set : T -> DomSet.T -> CodSet.T
  
  (* inverse lookup *)
  (*val inv_get : T -> cod -> dom*)
  (*val inv_get_opt : T -> cod -> dom option*)
  val inv_img : T -> cod -> DomSet.T
  val inv_img_of_set : T -> CodSet.T -> DomSet.T
  
  (* relation lookup *)
  val is_mapped : T -> dom * cod -> bool
end

functor NameFunction(
  structure NSet : NAME_SET
  structure Namer : NAMER
  sharing type Namer.name = NSet.name
) =
struct

  

end

