(* positions in a datatype *)
structure DataPos
= struct

datatype subloc = AtAnds of (En.name * En.NSet.T)
                | AtOrs of (Cn.name * Cn.NSet.T);

datatype T = DPos of Dn.name * subloc;

(* it is assumed that we only compare subloc when they are from the
same data - that way we only need to consider the ename *)

fun subloc_ord (AtAnds _, AtOrs _) = LESS
  | subloc_ord (AtOrs _, AtAnds _) = GREATER
  | subloc_ord (AtOrs (c1,_), AtOrs (c2,_)) = 
    Cn.name_ord (c1,c2)
  | subloc_ord (AtAnds (e1,_), AtAnds (e2,_)) = 
    En.name_ord (e1,e2);

fun ord (DPos a, DPos b) = 
    Library.prod_ord Dn.name_ord subloc_ord (a,b);

structure Tab = Table(type key = T val ord = ord);
structure Set = CollectionFUN(type key = T val ord = ord);

fun mk_atands (n, (en,eset)) = DPos (n, AtAnds(en,eset));
fun mk_ators (n, (cn,cset)) = DPos (n, AtOrs(cn,cset));

fun next_left (DPos (n,AtAnds (e,etab))) = 
    En.NSet.next_smaller etab e
  | next_left (DPos (n,AtOrs _)) = NONE;

fun next_right (DPos (n,AtAnds (e,etab))) = 
    En.NSet.next_bigger etab e
  | next_right (DPos (n,AtOrs _)) = NONE;

end;


