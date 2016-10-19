let mutable tempstore = 0
  
type exptree = Var of char | Expr of char * exptree * exptree

let codegen (e: exptree) = 
  let rec helper (e: exptree, tag: char) =
    match e with
      | Var c ->
        if (tag = '=') then
          printfn "LOAD  %c" c
        elif (tag = '+') then
          printfn "ADD  %c" c
        else // tag = '*'
        printfn "MUL  %c" c
      | Expr(op,l,r) -> 
         if (tag = '=') then
           helper (l,'=')
           helper (r, op)
         else
           tempstore <- tempstore + 1
           printfn "STORE %i" tempstore
           helper(l,'=')
           helper(r,op)
           if (tag = '+') then printfn "ADD %i" tempstore
           else printfn "MUL %i" tempstore
           tempstore <- tempstore - 1
  helper(e,'=')

           

(*
val it : exptree =
  Expr ('+',Var 'a',Expr ('*',Var 'b',Expr ('+',Var 'c',Var 'd')))

val helper : e:exptree * tag:char -> unit

> helper(tree,'=');;
LOAD  a
STORE 1
LOAD  b
STORE 2
LOAD  c
ADD  d
MUL 2
ADD 1
val it : unit = ()

*)




    
    

    
