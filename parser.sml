(* Special thanks to Graham Hutton, University of Utrecht *)
(* http://www.cs.nott.ac.uk/~gmh/parsing.pdf *)

structure Parser = 
struct
  
  (* Ex:  PARSER (fn cs => SOME (v, cs'))   *)
  datatype 'a parser = PARSER of (char list -> ('a * char list) option);
  
  (* cs: char list *)
  fun parse (PARSER p) cs = p cs;
  
  (* s: string *)
  fun debug (PARSER p) s = 
    case (p (String.explode s)) of
      NONE => NONE
    | SOME (v, cs) => SOME (v, String.implode cs);
  
  
  (* INFIXES *)
  
  
  infix 3 >>>;
  infix 3 >>=;
  infix 4 ooo;
  infix 3 +++;
  infix 3 xxx;
  
  (* p >>> p': if p succeeds then continue with p' *)
  fun p >>> p' = PARSER (fn cs => case parse p cs of 
      NONE => NONE
    | SOME (v, cs') => parse p' cs');
  
  (* p >>= f: if p succeeds pass v to f returning p' *)
  (* f: 'a -> parser *)
  fun p >>= f = PARSER (fn cs => case parse p cs of 
      NONE => NONE
    | SOME (v, cs') => parse (f v) cs'); 

  (* p ooo p': if p fails retry with p' *)
  fun p ooo p' = PARSER (fn cs => case parse p cs of 
      NONE => parse p' cs
    | some => some);
  
  (* p +++ p': if p succeeds retry with p' *)
  fun p +++ p' = PARSER (fn cs => case parse p cs of
      NONE => NONE
    | some => parse p' cs)
  
  (* p xxx p': if p fails retry with p' *)
  fun p xxx p' = PARSER (fn cs => case parse p cs of
      NONE => parse p' cs
    | some => NONE);
        
  
  (* PARSERS *)
  
  
  (* parser: always fails *)
  val fail = PARSER (fn _ => NONE);
  
  (* parser: returns v without consuming cs *)
  fun return v = PARSER (fn cs => SOME (v, cs));
  
  (* parser: returns the next c *)
  local
    fun next nil = NONE
      | next (c::cs) = SOME (c, cs);
  in
    val item = PARSER next;
  end;
  
  (* parser: returns c if test passes *)
  (* test: char -> bool *)
  fun satisfy test = item >>= (fn c => 
    if test c then return c else fail);
 
  (* parser: returns c if match *)
  fun ch c = satisfy (fn c' => c = c');
  
  (* parser: returns c in cs *)
  fun chs cs = List.foldl (op ooo) fail (List.map ch cs);
  
  (* parser: returns array of 0..n vs *)
  fun any p = (many p) ooo (return nil)
  
  (* parser: returns array of 1..n vs *)
  and many p = p >>= (fn v =>
    (any p) >>= (fn vs => return (v::vs)));
  
  (* parser: returns matched string s *)
  fun str s = List.foldr (op >>>) (return s) (List.map ch (String.explode s));

  (* parser: returns array of p' returned vs separated by p *)
  fun sep p p' = p' >>= (fn v => any (p >>> p') >>= (fn vs => return (v::vs)));
  
  (* parser: returns p' value discarding p values *)
  fun wrap p p' = p >>> p' >>= (fn v => p >>> (return v));
  
  (* parser: turns a parser ref into a parser *)
  fun mutable r = PARSER (fn cs => parse (!r) cs);
  
  (* parser: ensures there is no remaining cs *)
  fun eof p = PARSER (fn cs => case parse p cs of
    SOME (v, nil) => SOME (v, nil)
  | _ => NONE);
end;