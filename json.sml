structure JSON =
struct
  
  (* RFC 4627 JSON Specification *)
  datatype value = NULL 
    | STRING of string 
    | BOOLEAN of bool
    | NUMBER of real
    | ARRAY of value list
    | OBJECT of (string * value) list
  ;
  
  local
    val join = String.concatWith ",";
    fun quote s = "\"" ^ s ^ "\"";
  in
    fun dump json =
      case json of
        NULL => "null"
      | (STRING v) => quote v
      | (BOOLEAN v) => Bool.toString v
      | (NUMBER v) => Real.toString v
      | (ARRAY vs) => "[" ^ (join (List.map dump vs)) ^ "]"
      | (OBJECT vs) => let
        fun pair (k, v) = (quote k) ^ ":" ^ (dump v);
      in
        "{" ^ (join (List.map pair vs)) ^ "}"
      end
  end;
  
  local
    open Parser;

    infix 3 >>>;
    infix 3 >>=;
    infix 4 ooo;
    infix 3 +++;
    infix 3 xxx;
    
    val whitespace = any (chs [ #" ", #"\t", #"\n", #"\r" ]);
    fun token c = wrap whitespace (ch c);
  
    (* NULL *)
    val null = str "null" >>> return NULL;
  
    (* BOOLEAN *)
    local
      val true' = str "true" >>> return (BOOLEAN true);
      val false' = str "false" >>> return (BOOLEAN false);
    in
      val boolean = true' ooo false'
    end;
    
    (* NUMBER *)
    local
      val digit = chs (String.explode "0123456789");
      val integer = many digit;
    in
      val number = integer >>= (fn cs => 
        case (Real.fromString (String.implode cs)) of
          NONE => fail
        | (SOME r) => return (NUMBER r));
    end; 
  
    (* STRING *)
    local
      val escaped_quote = ch #"\\" >>> ch #"\"" >>> return #"\"";
      val not_quote = ch #"\"" xxx item;
    in
      val string = ch #"\"" >>> many (escaped_quote ooo not_quote) >>= (fn cs =>
        ch #"\"" >>> return (STRING (String.implode cs)))
    end;
  
    (* JSON *)
    val jsonref = ref (null ooo boolean ooo number ooo string);
    val json = mutable jsonref;
  
    (* ARRAY *)    
    val array = ch #"[" >>> sep (token #",") json >>= (fn vs => 
      ch #"]" >>> return (ARRAY vs));
  
    (* OBJECT *)
    local
      val pair = string >>= (fn (STRING k) => 
        (token #":") >>> json >>= (fn v => return (k, v)));
    in
      val object = token #"{" >>> sep (token #",") pair >>= (fn vs =>
        token #"}" >>> return (OBJECT vs));
    end;

    val _ = jsonref := (!jsonref) ooo array ooo object;
  in
    fun load s = #1(Option.valOf (parse (eof json) (String.explode s)))
  end;
end;