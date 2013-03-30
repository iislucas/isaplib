signature NAME_FUNCTION =
sig
  include NAME_RELATION
end

functor NameFunction(
  structure NSet : NAME_SET
  structure Namer : NAMER
  sharing type Namer.name = NSet.name
) =
struct

  

end

