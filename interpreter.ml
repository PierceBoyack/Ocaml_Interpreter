type stackValue = BOOL of bool | INT of int | ERROR | UNIT | STRING of string | NAME of string
type command = ADD | SUB | NUL | DIV | PUSH of stackValue | POP | MUL 
| REM | NEG | SWAP | TOSTRING | PRINTLN | QUIT | CAT | AND | OR | NOT
| EQUAL | LESSTHAN | BIND | IF | LET | END

let rec interpreter ((input : string), (output : string )) : unit =
  let ic = open_in input in
  let oc = open_out output in
  let rec loop_read acc =
    try 
        let l = String.trim(input_line ic) in loop_read (l::acc)
    with
    | End_of_file -> List.rev acc in
  let ls_str = loop_read [] in

  let file_write command = Printf.fprintf oc "%s\n" command in

  let digits = ['1';'2';'3';'4';'5';'6';'7';'8';'9';'0'] in

  let choosePush (params: string list) : command =
    match params with
    | [] -> PUSH ERROR
    | head :: tail -> (
      (* Check for decimals *)
      if (String.contains head '.') then
        PUSH ERROR 

      (* Check for integers or negative integers *)
      else if head.[0] == '-' || List.mem head.[0] digits then
        PUSH (INT (int_of_string head))
      
      (* Check for strings. Might have to concat a split string *)
      else if head.[0] = '\"' then
        let concated = String.concat " " (head::tail) in
        let removeQuotes = String.sub concated 1 (String.length concated-2) in
        let pushString = STRING removeQuotes in 
        PUSH pushString;

      (* Check for bool, error, or unit *)
      else if head.[0] = ':' then
        if head.[1] = 't' then PUSH (BOOL true)
        else if head.[1] = 'f' then PUSH (BOOL false)
        else if head.[1] = 'e' then PUSH ERROR
        else if head.[1] = 'u' then PUSH UNIT
        else PUSH ERROR
      else PUSH (NAME head)
    )
  in
      

  let strToCommand (str: string) (params: string list) =
    match str with
    | "add" -> ADD
    | "sub" -> SUB
    | "mul" -> MUL
    | "div" -> DIV
    | "rem" -> REM
    | "neg" -> NEG
    | "push" -> choosePush params
    | "swap" -> SWAP
    | "pop" -> POP
    | "toString" -> TOSTRING
    | "println" -> PRINTLN
    | "nul" -> NUL
    | "quit" -> QUIT
    | "cat" -> CAT
    | "and" -> AND
    | "or" -> OR
    | "not" -> NOT
    | "equal" -> EQUAL
    | "lessThan" -> LESSTHAN
    | "bind" -> BIND
    | "if" -> IF
    | "let" -> LET
    | "end" -> END
    | _ -> NUL
  in

  let rec commandList (lineList: string list) : command list =
    match lineList with
    | [] -> []
    | line :: remainingProgram ->
      let args: string list = String.split_on_char ' ' line in
      (
        match args with
        | [] -> []
        | action :: params -> 
          let com = strToCommand action params in
          com :: commandList remainingProgram
      )
  in

  let rec searchMemFrame (mem: (stackValue*stackValue) list) (lookup: stackValue) (unchangedMem: (stackValue*stackValue) list) : stackValue =
    match mem, lookup with
    | [], _ -> ERROR;
    | ((NAME(key), INT(value))::restOfMem), NAME(lookupVal) -> (
        if key = lookupVal then INT(value)
        else searchMemFrame restOfMem (NAME(lookupVal)) unchangedMem;
    )
    | ((NAME(key), UNIT)::restOfMem), NAME(lookupVal) -> (
      if key = lookupVal then UNIT
      else searchMemFrame restOfMem (NAME(lookupVal)) unchangedMem
    )
    | ((NAME(key), STRING(value))::restOfMem), NAME(lookupVal) -> (
      if key = lookupVal then STRING(value)
      else searchMemFrame restOfMem (NAME(lookupVal)) unchangedMem
    )
    | ((NAME(key), NAME(value))::restOfMem), NAME(lookupVal) -> (
      if key = lookupVal then(
        searchMemFrame unchangedMem (NAME(value))) unchangedMem
    else
        searchMemFrame restOfMem (NAME(lookupVal)) unchangedMem
    )
    | (NAME(key), BOOL(value))::restOfMem , NAME(lookupVal) -> (
        if key = lookupVal then BOOL(value)
        else searchMemFrame restOfMem (NAME(lookupVal)) unchangedMem;
    )
    | _,_ -> ERROR
      in
  
  let rec searchMemory (mem: (stackValue*stackValue) list list) (lookup: stackValue): stackValue =
    match mem with
    | [] -> ERROR
    | memFrame :: restOfMem -> (
        let searchResult = searchMemFrame memFrame lookup memFrame in
        match searchResult with
        | ERROR -> (
            match restOfMem with
            | [] -> ERROR
            | nextFrame :: tail -> searchMemory ((memFrame @ nextFrame)::tail) lookup
          )
        | _ -> searchResult
      )
  in
  
  let rec updateName (mem: (stackValue*stackValue) list) (updateVal: (stackValue*stackValue)) : (stackValue*stackValue) list =
    match mem, updateVal with
    | [], _ -> [updateVal]
    | (key, value)::restOfMem, (upKey, upVal) -> (
        if key = upKey then (key, upVal)::restOfMem
        else (key,value)::(updateName restOfMem updateVal)
     )
      in
  

  let updateMemory (mem: (stackValue*stackValue) list list) (updateVal: (stackValue*stackValue)) (unchangedMem: (stackValue*stackValue) list list) : (stackValue*stackValue) list list =

    let rec updateNameOnFrame (mem: (stackValue*stackValue) list list) (updateVal: (stackValue*stackValue)) (unchangedMem: (stackValue*stackValue) list list) : (stackValue*stackValue) list list =
      match mem, updateVal, unchangedMem with
      | [], _, [] -> []
      | memFrame :: restOfMem, (upKey, upVal), unchangedMem -> (updateName memFrame updateVal)::restOfMem
      | _,_,_ -> []
    in
    updateNameOnFrame mem updateVal mem
  in 
    

  let svToSTRING (sv: stackValue) : stackValue = 
    match sv with
    | BOOL (boolVal) -> STRING (":" ^ string_of_bool boolVal ^ ":") (*USED FOR STRING STACKVALUE*)
    | INT (intValue) -> STRING (string_of_int intValue)
    | ERROR -> STRING (":error:")
    | UNIT -> STRING (":unit:")
    | STRING (stringVal) -> STRING (stringVal)
    | NAME (stringVal) -> STRING (stringVal) 
  in

  

  let myComs = commandList(ls_str) in
  (* let myStrComs = commandListToString(myComs) in *)

  let rec processor (commandList: command list) (stack: stackValue list list) (memory: (stackValue*stackValue) list list) =
    match (commandList, stack, memory) with
    | (PUSH (pushVal) :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((pushVal :: stackFrame) :: ss) memory
    | (POP :: restOfCommands, (e1 :: stackFrame)::ss, memory) -> processor restOfCommands (stackFrame::ss) memory
    | (POP :: restOfCommands, stackFrame :: ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (ADD :: restOfCommands, (INT(a) :: INT(b) :: stackFrame)::ss, memory) -> processor restOfCommands ((INT (a+b) :: stackFrame)::ss) memory
    | (ADD :: restOfCommands, (NAME(a) :: INT(b) :: stackFrame)::ss, memory) -> (
        let bindVal = searchMemory memory (NAME(a)) in
        match bindVal with
        | INT(a) -> processor restOfCommands ((INT (a+b) :: stackFrame)::ss) memory
        | _ -> processor restOfCommands ((ERROR::NAME(a)::INT(b)::stackFrame)::ss) memory
      )
    | (ADD :: restOfCommands, (INT(a) :: NAME(b) :: stackFrame)::ss, memory) -> (
        let bindVal = searchMemory memory (NAME(b)) in
        match bindVal with
        | INT(b) -> processor restOfCommands ((INT (a+b) :: stackFrame)::ss) memory
        | _ -> processor restOfCommands ((ERROR::INT(a)::NAME(b)::stackFrame)::ss) memory
      )
      | (ADD :: restOfCommands, (NAME(a) :: NAME(b) :: stackFrame)::ss, memory) -> (
        let bindVal1 = searchMemory memory (NAME(a)) in
        let bindVal2 = searchMemory memory (NAME(b)) in 
        match bindVal1, bindVal2 with
        | INT(a), INT(b) -> processor restOfCommands ((INT (a+b) :: stackFrame)::ss) memory
        | _,_ -> processor restOfCommands ((ERROR::NAME(a)::NAME(b)::stackFrame)::ss) memory
      )
    | (ADD :: restOfCommands, stackFrame :: ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (SUB :: restOfCommands, (INT(a) :: INT(b) :: stackFrame)::ss, memory) -> processor restOfCommands ((INT (b-a) :: stackFrame)::ss) memory
    | (SUB :: restOfCommands, (a :: b :: stackFrame)::ss, memory) -> (
        match a, b with
        | NAME(a), INT(b) -> (
            let bindVal = searchMemory memory (NAME(a)) in
            match bindVal with
            | INT(x) -> processor restOfCommands ((INT (b-x) :: stackFrame)::ss) memory
            | _ -> processor restOfCommands ((ERROR::NAME(a)::INT(b)::stackFrame)::ss) memory
          )
        | INT(a), NAME(b) -> (
            let bindVal = searchMemory memory (NAME(b)) in
            match bindVal with
            | INT(y) -> processor restOfCommands ((INT (y-a) :: stackFrame)::ss) memory
            | _ -> processor restOfCommands ((ERROR::INT(a)::NAME(b)::stackFrame)::ss) memory
          )
        | NAME(a), NAME(b) -> (
            let bindVal1 = searchMemory memory (NAME(a)) in
            let bindVal2 = searchMemory memory (NAME(b)) in
            match bindVal1, bindVal2 with
            | INT(y), INT(x) -> processor restOfCommands ((INT (x-y) :: stackFrame)::ss) memory
            | _ -> processor restOfCommands ((ERROR::NAME(a)::NAME(b)::stackFrame)::ss) memory
        )
        | _, _ -> processor restOfCommands ((ERROR::a::b::stackFrame)::ss) memory
      )
    | (SUB :: restOfCommands, stackFrame :: ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (MUL :: restOfCommands, (INT(a) :: INT(b) :: stackFrame)::ss, memory) -> processor restOfCommands ((INT (a*b) :: stackFrame)::ss) memory
    | (MUL :: restOfCommands, (a :: b :: stackFrame)::ss, memory) -> (
        match a, b with
        | NAME(a), INT(b) -> (
            let bindVal = searchMemory memory (NAME(a)) in
            match bindVal with
            | INT(a) -> processor restOfCommands ((INT (b*a) :: stackFrame)::ss) memory
            | _ -> processor restOfCommands ((ERROR::NAME(a)::INT(b)::stackFrame)::ss) memory
          )
        | INT(a), NAME(b) -> (
            let bindVal = searchMemory memory (NAME(b)) in
            match bindVal with
            | INT(b) -> processor restOfCommands ((INT (b*a) :: stackFrame)::ss) memory
            | _ -> processor restOfCommands ((ERROR::INT(a)::NAME(b)::stackFrame)::ss) memory
          )
        | NAME(a), NAME(b) -> (
            let bindVal1 = searchMemory memory (NAME(a)) in
            let bindVal2 = searchMemory memory (NAME(b)) in
            match bindVal1, bindVal2 with
            | INT(a), INT(b) -> processor restOfCommands ((INT (b*a) :: stackFrame)::ss) memory
            | _ -> processor restOfCommands ((ERROR::NAME(a)::NAME(b)::stackFrame)::ss) memory
        )
        | _, _ -> processor restOfCommands ((ERROR::a::b::stackFrame)::ss) memory
      )
    | (MUL :: restOfCommands, stackFrame :: ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (DIV :: restOfCommands, (INT(y) :: INT(x) :: stackFrame)::ss, memory) -> (
        if y = 0 then processor restOfCommands ((ERROR::INT(y)::INT(x)::stackFrame)::ss) memory
        else processor restOfCommands ((INT (x/y) :: stackFrame)::ss) memory
      )
    | (DIV :: restOfCommands, (y :: x :: stackFrame)::ss, memory) -> (
        match y, x with
        | NAME(a), INT(x) -> (
            let bindVal = searchMemory memory (NAME(a)) in
            match bindVal with
            | INT(y) -> (
              if y = 0 then processor restOfCommands ((ERROR::NAME(a)::INT(x)::stackFrame)::ss) memory
              else processor restOfCommands ((INT (x/y) :: stackFrame)::ss) memory
            )
            | _ -> processor restOfCommands ((ERROR::NAME(a)::INT(x)::stackFrame)::ss) memory
          )
        | INT(y), NAME(b) -> (
            let bindVal = searchMemory memory (NAME(b)) in
            match bindVal with
            | INT(x) -> (
              if y = 0 then processor restOfCommands ((ERROR::INT(y)::NAME(b)::stackFrame)::ss) memory
              else processor restOfCommands ((INT (x/y) :: stackFrame)::ss) memory
            )
            | _ -> processor restOfCommands ((ERROR::INT(y)::NAME(b)::stackFrame)::ss) memory
          )
        | NAME(b), NAME(a) -> (
            let bindVal1 = searchMemory memory (NAME(b)) in
            let bindVal2 = searchMemory memory (NAME(a)) in
            match bindVal1, bindVal2 with
            | INT(y), INT(x) -> (
              if y = 0 then processor restOfCommands ((ERROR::NAME(b)::NAME(a)::stackFrame)::ss) memory
              else processor restOfCommands ((INT (x/y) :: stackFrame)::ss) memory
            )
            | _ -> processor restOfCommands ((ERROR::NAME(b)::NAME(a)::stackFrame)::ss) memory
        )
        | _, _ -> processor restOfCommands ((ERROR::y::x::stackFrame)::ss) memory
      )
    | (DIV :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (REM :: restOfCommands, (INT(y) :: INT(x) :: stackFrame)::ss, memory) -> (
        if y = 0 then processor restOfCommands ((ERROR::INT(y)::INT(x)::stackFrame)::ss) memory
        else processor restOfCommands ((INT (x mod y) :: stackFrame)::ss) memory
      )
    | (REM :: restOfCommands, (y :: x :: stackFrame)::ss, memory) -> (
      match y, x with
      | NAME(b), INT(x) -> (
          let bindVal = searchMemory memory (NAME(b)) in
          match bindVal with
          | INT(y) -> (
            if y = 0 then processor restOfCommands ((ERROR::NAME(b)::INT(x)::stackFrame)::ss) memory
            else processor restOfCommands ((INT (x mod y) :: stackFrame)::ss) memory
          )
          | _ -> processor restOfCommands ((ERROR::NAME(b)::INT(x)::stackFrame)::ss) memory
        )
      | INT(y), NAME(a) -> (
          let bindVal = searchMemory memory (NAME(a)) in
          match bindVal with
          | INT(x) -> (
            if y = 0 then processor restOfCommands ((ERROR::INT(y)::NAME(a)::stackFrame)::ss) memory
            else processor restOfCommands ((INT (x mod y) :: stackFrame)::ss) memory
          )
          | _ -> processor restOfCommands ((ERROR::INT(y)::NAME(a)::stackFrame)::ss) memory
        )
      | NAME(b), NAME(a) -> (
          let bindVal1 = searchMemory memory (NAME(b)) in
          let bindVal2 = searchMemory memory (NAME(a)) in
          match bindVal1, bindVal2 with
          | INT(y), INT(x) -> (
            if y = 0 then processor restOfCommands ((ERROR::NAME(b)::NAME(a)::stackFrame)::ss) memory
            else processor restOfCommands ((INT (x mod y) :: stackFrame)::ss) memory
          )
          | _ -> processor restOfCommands ((ERROR::NAME(b)::NAME(a)::stackFrame)::ss) memory
      )
      | _, _ -> processor restOfCommands ((ERROR::y::x::stackFrame)::ss) memory
    )
    | (REM :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (NEG :: restOfCommands, (INT(a) :: stackFrame)::ss, memory) -> (
        if a = 0 then processor restOfCommands ((INT (0) :: stackFrame)::ss) memory
        else processor restOfCommands ((INT (a*(-1)) :: stackFrame)::ss) memory
      )
    | (NEG :: restOfCommands, (NAME(a)::stackFrame)::ss, memory) -> (
        let bindVal = searchMemory memory (NAME(a)) in
        match bindVal with
        | INT(b) -> processor restOfCommands ((INT(b*(-1))::stackFrame)::ss) memory
        | _ -> processor restOfCommands ((ERROR::NAME(a)::stackFrame)::ss) memory
      )
    | (NEG :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (SWAP :: restOfCommands, (e1 :: e2 :: stackFrame)::ss, memory) -> processor restOfCommands ((e2 :: e1 :: stackFrame)::ss) memory
    | (SWAP :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (TOSTRING :: restOfCommands, (e1 :: stackFrame)::ss, memory) -> processor restOfCommands (((svToSTRING e1) :: stackFrame)::ss) memory
    | (TOSTRING :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (PRINTLN :: restOfCommands, (STRING(str) :: stackFrame)::ss, memory) -> file_write str; processor restOfCommands (stackFrame::ss) memory
    | (PRINTLN :: restOfCommands, (NAME(a) :: stackFrame)::ss, memory) -> (
        let bindVal = searchMemory memory (NAME(a)) in
        match bindVal with
        | STRING(a) -> file_write a; processor restOfCommands (stackFrame::ss) memory
        | _ -> processor restOfCommands ((ERROR::NAME(a)::stackFrame)::ss) memory
    )
    | (PRINTLN :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (QUIT :: restOfCommands, s, memory) -> ()
    | (CAT :: restOfCommands, (STRING(x) :: STRING(y) :: stackFrame)::ss, memory) -> processor restOfCommands ((STRING(y^x) :: stackFrame)::ss) memory
    | (CAT :: restOfCommands, (x :: y :: stackFrame)::ss, memory) -> (
        match y, x with
        | NAME(y), NAME(x) -> (
            let bindVal1 = searchMemory memory (NAME(y)) in
            let bindVal2 = searchMemory memory (NAME(x)) in
            match bindVal1, bindVal2 with
            | STRING(y), STRING(x) -> processor restOfCommands ((STRING(y^x) :: stackFrame)::ss) memory
            | _, _ -> processor restOfCommands ((ERROR :: NAME(y) :: NAME(x) :: stackFrame)::ss) memory
          )
        | NAME(y), STRING(x) -> (
            let bindVal = searchMemory memory (NAME(y)) in
            match bindVal with
            | STRING(y) -> processor restOfCommands ((STRING(y^x) :: stackFrame)::ss) memory
            | _ -> processor restOfCommands ((ERROR :: NAME(y) :: NAME(x) :: stackFrame)::ss) memory
          )
        | STRING(y), NAME(x) -> (
            let bindVal = searchMemory memory (NAME(x)) in
            match bindVal with
            | STRING(x) -> processor restOfCommands ((STRING(y^x) :: stackFrame)::ss) memory
            | _ -> processor restOfCommands ((ERROR :: NAME(y) :: NAME(x) :: stackFrame)::ss) memory
        )
        | _, _-> processor restOfCommands ((ERROR :: y :: x :: stackFrame)::ss) memory
      )
    | (CAT :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (AND :: restOfCommands, (BOOL(p) :: BOOL(q) :: stackFrame)::ss, memory) -> processor restOfCommands ((BOOL(p && q) :: stackFrame)::ss) memory
    | (AND :: restOfCommands, (p :: q :: stackFrame)::ss, memory) -> (
        match p, q with
        | NAME(p), NAME(q) -> (
            let bindVal1 = searchMemory memory (NAME(p)) in
            let bindVal2 = searchMemory memory (NAME(q)) in
            match bindVal1, bindVal2 with
            | BOOL(p), BOOL(q) -> processor restOfCommands ((BOOL(p && q) :: stackFrame)::ss) memory
            | _, _ -> processor restOfCommands ((ERROR:: NAME(p) :: NAME(q) ::stackFrame)::ss) memory
          )
        | NAME(p), BOOL(q) -> (
            let bindVal = searchMemory memory (NAME(p)) in
            match bindVal with
            | BOOL(p) -> processor restOfCommands ((BOOL(p && q) :: stackFrame)::ss) memory
            | _ -> processor restOfCommands ((ERROR:: NAME(p) :: BOOL(q) ::stackFrame)::ss) memory
          )
        | BOOL(p), NAME(q) -> (
            let bindVal = searchMemory memory (NAME(q)) in
            match bindVal with
            | BOOL(q) -> processor restOfCommands ((BOOL(p && q) :: stackFrame)::ss) memory
            | _ -> processor restOfCommands ((ERROR:: BOOL(p) :: NAME(q) ::stackFrame)::ss) memory
          )
        | _, _ -> processor restOfCommands ((ERROR::p::q::stackFrame)::ss) memory
      )
    | (AND :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (OR :: restOfCommands, (BOOL(p) :: BOOL(q) :: stackFrame)::ss, memory) -> processor restOfCommands ((BOOL(p || q) :: stackFrame)::ss) memory
    | (OR :: restOfCommands, (p :: q :: stackFrame)::ss, memory) -> (
        match p, q with
        | NAME(p), NAME(q) -> (
            let bindVal1 = searchMemory memory (NAME(p)) in
            let bindVal2 = searchMemory memory (NAME(q)) in
            match bindVal1, bindVal2 with
            | BOOL(p), BOOL(q) -> processor restOfCommands ((BOOL(p || q) :: stackFrame)::ss) memory
            | _, _ -> processor restOfCommands ((ERROR:: NAME(p) :: NAME(q) ::stackFrame)::ss) memory
          )
        | NAME(p), BOOL(q) -> (
            let bindVal = searchMemory memory (NAME(p)) in
            match bindVal with
            | BOOL(p) -> processor restOfCommands ((BOOL(p || q) :: stackFrame)::ss) memory
            | _ -> processor restOfCommands ((ERROR:: NAME(p) :: BOOL(q) ::stackFrame)::ss) memory
          )
        | BOOL(p), NAME(q) -> (
            let bindVal = searchMemory memory (NAME(q)) in
            match bindVal with
            | BOOL(q) -> processor restOfCommands ((BOOL(p || q) :: stackFrame)::ss) memory
            | _ -> processor restOfCommands ((ERROR:: BOOL(p) :: NAME(q) ::stackFrame)::ss) memory
          )
        | _, _ -> processor restOfCommands ((ERROR::p::q::stackFrame)::ss) memory
      )
    | (OR :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (NOT :: restOfCommands, (BOOL(p) :: stackFrame)::ss, memory) -> (
        if p then processor restOfCommands ((BOOL(false) :: stackFrame)::ss) memory
        else processor restOfCommands ((BOOL(true) :: stackFrame)::ss) memory
      )
    | (NOT :: restOfCommands, (NAME(a) :: stackFrame)::ss, memory) -> (
        let bindVal = searchMemory memory (NAME(a)) in
        match bindVal with
        | BOOL(p) -> (
            if p then processor restOfCommands ((BOOL(false) :: stackFrame)::ss) (updateMemory memory (NAME(a), BOOL(false)) memory)
            else processor restOfCommands ((BOOL(true) :: stackFrame)::ss) (updateMemory memory (NAME(a), BOOL(true)) memory)
          )
        | _ -> processor restOfCommands ((ERROR::NAME(a)::stackFrame)::ss) memory
     )
    | (NOT :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (EQUAL :: restOfCommands, (INT(a) :: INT (b) :: stackFrame)::ss, memory) -> (
        if a = b then processor restOfCommands ((BOOL(true) :: stackFrame)::ss) memory
        else processor restOfCommands ((BOOL(false) :: stackFrame)::ss) memory
      )
    | (EQUAL :: restOfCommands, (y :: x :: stackFrame)::ss, memory) -> (
        match y, x with
        | NAME(y), INT(x) -> (
            let bindVal = searchMemory memory (NAME(y)) in
            match bindVal with
            | INT(y) -> (
                if y = x then processor restOfCommands ((BOOL(true) :: stackFrame)::ss) memory
                else processor restOfCommands ((BOOL(false) :: stackFrame)::ss) memory
              )
            | _ -> processor restOfCommands ((ERROR::NAME(y)::INT(x)::stackFrame)::ss) memory
          )
        | INT(y), NAME(x) -> (
            let bindVal = searchMemory memory (NAME(x)) in
            match bindVal with
            | INT(x) -> (
                if y = x then processor restOfCommands ((BOOL(true) :: stackFrame)::ss) memory
                else processor restOfCommands ((BOOL(false) :: stackFrame)::ss) memory
              )
            | _ -> processor restOfCommands ((ERROR::INT(y)::NAME(x)::stackFrame)::ss) memory
          )
        | NAME(y), NAME(x) -> (
            let bindVal1 = searchMemory memory (NAME(y)) in
            let bindVal2 = searchMemory memory (NAME(x)) in
            match bindVal1, bindVal2 with
            | INT(y), INT(x) -> (
                if y = x then processor restOfCommands ((BOOL(true) :: stackFrame)::ss) memory
                else processor restOfCommands ((BOOL(false) :: stackFrame)::ss) memory
              )
            | _, _ -> processor restOfCommands ((ERROR::NAME(y)::NAME(x)::stackFrame)::ss) memory
        )
        | _, _ ->  processor restOfCommands ((ERROR::y::x::stackFrame)::ss) memory
    )
    | (EQUAL :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (LESSTHAN :: restOfCommands, (INT(b) :: INT (a) :: stackFrame)::ss, memory) -> (
        if a < b then processor restOfCommands ((BOOL(true) :: stackFrame)::ss) memory
        else processor restOfCommands ((BOOL(false) :: stackFrame)::ss) memory
      )
    | (LESSTHAN :: restOfCommands, (b :: a :: stackFrame)::ss, memory) -> (
        match b, a with
        | NAME(b), NAME(a) -> (
            let bindVal1 = searchMemory memory (NAME(b)) in
            let bindVal2 = searchMemory memory (NAME(a)) in
            match bindVal1, bindVal2 with
            | INT(b), INT(a) -> (
                if a < b then processor restOfCommands ((BOOL(true) :: stackFrame)::ss) memory
                else processor restOfCommands ((BOOL(false) :: stackFrame)::ss) memory
              )
            | _, _ -> processor restOfCommands ((ERROR::NAME(b)::NAME(a)::stackFrame)::ss) memory
          )
        | NAME(b), INT(a) -> (
            let bindVal = searchMemory memory (NAME(b)) in
            match bindVal with
            | INT(b) -> (
                if a < b then processor restOfCommands ((BOOL(true) :: stackFrame)::ss) memory
                else processor restOfCommands ((BOOL(false) :: stackFrame)::ss) memory
              )
            | _ -> processor restOfCommands ((ERROR::NAME(b)::INT(a)::stackFrame)::ss) memory
          )
        | INT(b), NAME(a) -> (
            let bindVal = searchMemory memory (NAME(a)) in
            match bindVal with
            | INT(a) -> (
                if a < b then processor restOfCommands ((BOOL(true) :: stackFrame)::ss) memory
                else processor restOfCommands ((BOOL(false) :: stackFrame)::ss) memory
              )
            | _ -> processor restOfCommands ((ERROR::INT(b)::NAME(a)::stackFrame)::ss) memory
          ) 
        | _, _ -> ()
    )
    | (LESSTHAN :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (BIND :: restOfCommands, (ERROR :: e2 :: stackFrame)::ss, memory) -> processor restOfCommands ((ERROR::ERROR::e2::stackFrame)::ss) memory
    | (BIND :: restOfCommands, (NAME(bindVal) :: NAME(bindName) :: stackFrame)::ss, memory) -> (  (*search for unbound name*)
        if (searchMemory memory (NAME(bindVal))) = ERROR then
          processor restOfCommands ((ERROR::NAME(bindVal)::NAME(bindName)::stackFrame)::ss) memory
        else  processor restOfCommands ((UNIT::stackFrame)::ss) (updateMemory memory (NAME(bindName), NAME(bindVal)) memory)  
      )
    | (BIND :: restOfCommands, (bindVal :: NAME(bindName) :: stackFrame)::ss, memory) -> processor restOfCommands ((UNIT::stackFrame)::ss) (updateMemory memory (NAME(bindName), bindVal) memory)
    | (BIND :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (IF :: restOfCommands, (x::y::z::stackFrame)::ss, memory) -> (
        match z with
        | BOOL(z) -> (
            if z then processor restOfCommands ((x::stackFrame)::ss) memory
            else processor restOfCommands ((y::stackFrame)::ss) memory
          )
        | NAME(z) -> (
            let bindVal = searchMemory memory (NAME(z)) in
            match bindVal with
            | BOOL(z) -> (
                if z then processor restOfCommands ((x::stackFrame)::ss) memory
                else processor restOfCommands ((y::stackFrame)::ss) memory
              )
            | _ -> processor restOfCommands ((ERROR::x::y::NAME(z)::stackFrame)::ss) memory
          )
        | _ -> processor restOfCommands ((ERROR::x::y::z::stackFrame)::ss) memory
    )
    | (IF :: restOfCommands, stackFrame::ss, memory) -> processor restOfCommands ((ERROR::stackFrame)::ss) memory
    | (LET :: restOfCommands, ss, memory) -> processor restOfCommands ([]::ss) ([]::memory)
    | (END :: restOfCommands, stackFrame2::stackFrame1::ss, memFrame::restOfMem) -> (
        match stackFrame2 with
        | [] -> processor restOfCommands (stackFrame1::ss) restOfMem
        | head :: tail -> processor restOfCommands ((head::stackFrame1)::ss) restOfMem
    )
    | (_, stack, memory) -> ()
  in

  processor myComs [[]] [[]];;

(* interpreter ("input.txt", "output.txt");; *)