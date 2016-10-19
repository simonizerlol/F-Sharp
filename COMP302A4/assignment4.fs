(* Assignment 4 *) (* Do not edit this line. *)
(* Student name: Simon Hsu, Id Number: 260610820 *) (* Edit this line. *)

type typExp =
  | TypInt
  | TypVar of char
  | Arrow of typExp * typExp
  | Lst of typExp

type substitution = (char * typExp) list

let rec occurCheck (v: char) (tau: typExp) : bool = 
  match tau with
  | TypInt -> false
  | TypVar x -> v = x
  | Arrow (x, y) -> (occurCheck v x) || (occurCheck v y) 
  | Lst x -> occurCheck v x 
(* check if a variable occurs in a term *)

let rec substitute (tau1 : typExp) (v : char) (tau2 : typExp) : typExp =
    match tau2 with
    | TypInt -> TypInt
    | TypVar(a) -> if (occurCheck v tau1) then TypVar(a) else tau1
    | Arrow(a,b) -> Arrow(substitute tau1 v a, substitute tau1 v b)
    | Lst(a) -> Lst(substitute tau1 v a)

let applySubst (sigma: substitution) (tau: typExp) : typExp =
    List.fold (fun result (x, y) -> substitute y x result) tau sigma

let rec unify (tau1: typExp) (tau2:typExp) : substitution =
    match tau1 with
    | TypInt -> match tau2 with
                | TypInt -> []
                | TypVar y -> [(y,TypInt)]
                | Lst l2 -> failwith "Not unifiable"
                | Arrow (c,d)-> failwith "Not unifiable"
    | TypVar x -> match tau2 with
                  | TypInt -> [(x, TypInt)]
                  | TypVar y -> if y = x then [] else [(x, TypVar y)]
                  | Arrow (y,z) -> if (occurCheck x y) || (occurCheck x z) then failwith "Failed occurs check" else [(x, Arrow(y,z))]
                  | Lst y -> if occurCheck x y then failwith "Failed occurs check" else [(x, Lst y)]
    | Arrow (x,y) -> match tau2 with
                     | TypInt -> failwith "Not unifiable"
                     | TypVar z -> if ((occurCheck z x) || (occurCheck z y)) then failwith "Failed occurs check" else [(z, Arrow(x,y))]
                     | Arrow (z,w) -> (unify x z) @ unify (applySubst (unify x z) y) (applySubst (unify x z) w)  
                     | Lst z -> failwith "Clash in principal type constructor"
    | Lst x -> match tau2 with
                | TypInt -> failwith "Not unifiable"
                | TypVar y -> if (occurCheck y x) then failwith "Failed occurs check" else [(y, Lst x)]
                | Lst y -> unify x y
                | _ -> failwith "Clash in principal type constructor"
(* Use the following signals if unification is not possible:
 failwith "Clash in principal type constructor"
 failwith "Failed occurs check"
 failwith "Not unifiable"
*)
let te4 = Arrow (TypInt, Arrow(TypVar 'c', TypVar 'a'));;
let te3 = Arrow (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'));;
(*
> let te4 = Arrow (TypInt, Arrow(TypVar 'c', TypVar 'a'));;
val te4 : typExp = Arrow (TypInt,Arrow (TypVar 'c',TypVar 'a'))

> let te3 = Arrow (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'));;
val te3 : typExp = Arrow (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'))

> unify te3 te4;;
val it : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]

> let result = it;;
val result : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]

> applySubst result te3;;
val it : typExp = Arrow (TypInt,Arrow (TypInt,TypInt))

> applySubst result te4;;
val it : typExp = Arrow (TypInt,Arrow (TypInt,TypInt))
*)

(* Use the following signals if unification is not possible:
 failwith "Clash in principal type constructor"
 failwith "Failed occurs check"
 failwith "Not unifiable"
*)

(*
> let te4 = Arrow (TypInt, Arrow(TypVar 'c', TypVar 'a'));;
val te4 : typExp = Arrow (TypInt,Arrow (TypVar 'c',TypVar 'a'))

> let te3 = Arrow (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'));;
val te3 : typExp = Arrow (TypVar 'a',Arrow (TypVar 'b',TypVar 'c'))

> unify te3 te4;;
val it : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]

> let result = it;;
val result : substitution = [('c', TypInt); ('b', TypVar 'c'); ('a', TypInt)]

> applySubst it te3;;
val it : typExp = Arrow (TypInt,Arrow (TypInt,TypInt))

> applySubst it te4;;
val it : typExp = Arrow (TypInt,Arrow (TypInt,TypInt))
*)
