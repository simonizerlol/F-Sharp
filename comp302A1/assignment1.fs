(* Assignment 1 *) (* Do not edit this line. *)
(* Student name: Simon Hsu, Id Number: 260610820*) (* Edit this line. *)

(* In the template below we have written the names of the functions that
you need to define.  You MUST use these names.  If you introduce auxiliary
functions you can name them as you like except that your names should not
clash with any of the names we are using.  We have also shown the types
that you should have.  Your code must compile and must not go into infinite
loops.  *)

(* Question 1 *) (* Do not edit this line. *)

(* val sumlist : l:float list -> float *)
let rec sumlist (l: float list) =  (* remove failwith and replace with your code *)
    match l with
    | [] -> 0.0
    | h::t -> (sumlist t) + h   
    (*:: creates a list.The element on the left side is appended to the list on the right side.*)
    (*-> delimits arguments and return values. Yields an expression (in sequence expressions); equivalent to the yield keyword.*)

(* val squarelist : l:float list -> float list *)
let rec squarelist (l: float list) =  (* remove failwith and replace with your code *)
    match l with
    | [] -> []
    | h::t -> (h*h)::(squarelist t)

(* val mean : l:float list -> float *)
let mean l =(sumlist l)/(float)l.Length (* remove failwith and replace with your code *)

(* val mean_diffs : l:float list -> float list *)
let mean_diffs l =  (* remove failwith and replace with your code *)
    let rec helper l m =
        match l with
        |[] -> []
        | h::t -> (h-m):: (helper t m)
    in helper l (mean l)

(* val variance : l:float list -> float *)
let variance l = (sumlist (squarelist(mean_diffs l))) / (float)l.Length (* remove failwith and replace with your code *)

(* End of question 1 *) (* Do not edit this line. *)

(* Question 2 *) (* Do not edit this line. *)

(* val memberof : 'a * 'a list -> bool when 'a : equality *)
let rec memberof ((i:'a),(l:'a list)) =  (* remove failwith and replace with your code *)
    match l with
    | h::t -> if h = i 
              then true 
              else memberof(i,t)
    | [] -> false

(* val remove : 'a * 'a list -> 'a list when 'a : equality *)
let rec remove ((i:'a),(l:'a list)) =  (* remove failwith and replace with your code *)
    match l with
    | h::t when h = i -> t
    | h::t -> h::(remove(i,t))
    | [] -> []
(* End of question 2 *) (* Do not edit this line *)

(* Question 3 *) (* Do not edit this line *)

(* val isolate : l:'a list -> 'a list when 'a : equality *)
let rec isolate l = (* remove failwith and replace with your code *)
    match l with
    |[] -> []
    |h::t -> if memberof(h,t) = false 
             then h::(isolate(t)) 
             else isolate(t)
(* End of question 3 *) (* Do not edit this line *)

(* Question 4 *) (* Do not edit this line *)

(* val common : 'a list * 'a list -> 'a list when 'a : equality *)
let rec common (a:'a list,b:'a list) =  (* remove failwith and replace with your code *)
    match a with
    |[] -> match b with
           | [] -> []
           | h::t -> h::t
    |h::t -> if memberof(h,b) = true
              then h::(common(remove(h,b),t))
              else common (b, t)
(* End of question 4 *) (* Do not edit this line *)

(* Question 5 *) (* Do not edit this line *)

(* val split : l:'a list -> 'a list * 'a list *)
let split l =  (* remove failwith and replace with your code *)
    let rec helper l a b =
        match l with
        | [] -> (a,b) (* match only when the input list is empty *)
        | [x] -> (x::a,b) (* match when there's only one item *)
        | x::y::tail -> (* match all the other patterns *)
            helper tail (x::a) (y::b)
    helper l [] []
(* val merge : 'a list * 'a list -> 'a list when 'a : comparison *)
let rec merge l =  (* remove failwith and replace with your code *)
    match l with
    | ([], b) -> b
    | (a, []) -> a
    | (x::xs, y::ys) -> if x < y then x :: merge (xs, y::ys)
                        else y :: merge (x::xs, ys)
(* val mergesort : l:'a list -> 'a list when 'a : comparison *)
let rec mergesort l =  (* remove failwith and replace with your code *)
    match l with
    | [] -> []
    | [x] -> [x]
    | xs -> let (a, b) = split xs
            merge(mergesort a, mergesort b)
(* End of question 5 *) (* Do not edit this line *)


