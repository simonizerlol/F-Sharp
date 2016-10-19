type exptree = Var of char | Expr of char * exptree * exptree

let charSet = ['a' .. 'z']

let example = "(a+(b+(c*d)+e)*f)*g"

let isin (x: char) L = List.exists (fun y -> x = y) L

let parse (inputexp: string): exptree = 
  let sym = ref inputexp.[0]
  let cursor = ref 0

  let getsym () =
    cursor := !cursor + 1
    sym := inputexp.[!cursor]

  let rec expr (): exptree =
    let result = term ()
    if !sym = '+' then
      Expr('+',result, (getsym();expr()))
    else
      result
  and term (): exptree =
    let result = primary()
    if !sym = '*' then
      Expr('*',result, (getsym();term()))
    else
      result
  and primary (): exptree = 
    if !sym = '(' then
      getsym()
      let result = expr ()
      if not (!sym = ')') then 
        failwith "Mismatched parens"
      else 
        if (!cursor = inputexp.Length - 1) 
        then 
          result
        else 
          getsym()
          result
    elif (isin !sym charSet) then 
      if (!cursor = inputexp.Length - 1) 
      then 
        (Var !sym) 
      else 
        let result = Var !sym in (getsym(); result)
    else
      printfn "sym is : %c." !sym
      failwith "In primary"
  expr()


