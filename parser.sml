(* Special thanks to Graham Hutton, University of Utrecht *)
(* http://www.cs.nott.ac.uk/~gmh/parsing.pdf *)

structure Parser = 
struct
  
  (* Ex:  Parser (fn cs => SOME (v, cs'))   *)
  datatype 'a parser = Parser of (char list -> ('a * char list) option);
  
  (* cs: char list *)
  fun parse (Parser p) cs = p cs;
  
  (* s: string *)
  fun debug (Parser p) s = 
    case (p (String.explode s)) of
      NONE => NONE
    | SOME (v, cs) => SOME (v, String.implode cs);
  
  
  (* INFIXES *)
  
  
  (* p >>> p': if p succeeds then continue with p' *)
  infix 3 >>>;
  fun p >>> p' = Parser (fn cs => case parse p cs of 
      NONE => NONE
    | SOME (v, cs') => parse p' cs');
  
  (* p >>= f: if p succeeds pass v to f returning p' *)
  infix 3 >>=;
  (* f: 'a -> parser *)
  fun p >>= f = Parser (fn cs => case parse p cs of 
      NONE => NONE
    | SOME (v, cs') => parse (f v) cs'); 

  (* p ooo p': if p fails retry with p' *)
  infix 4 ooo;
  fun p ooo p' = Parser (fn cs => case parse p cs of 
      NONE => parse p' cs
    | some => some);
  
  (* p +++ p': if p succeeds retry with p' *)
  infix 3 +++;
  fun p +++ p' = Parser (fn cs => case parse p cs of
      NONE => NONE
    | some => parse p' cs)
  
  (* p xxx p': if p fails retry with p' *)
  infix 3 xxx;
  fun p xxx p' = Parser (fn cs => case parse p cs of
      NONE => parse p' cs
    | some => NONE);
    
  
  (* PARSERS *)
  
  
  (* parser: always fails *)
  val fail = Parser (fn _ => NONE);
  
  (* parser: returns v without consuming cs *)
  fun return v = Parser (fn cs => SOME (v, cs));
  
  (* parser: returns the next c *)
  local
    fun next nil = NONE
      | next (c::cs) = SOME (c, cs);
  in
    val item = Parser next;
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

end;