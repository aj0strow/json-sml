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
  
  val join = String.concatWith ",";
  fun quote s = "\"" ^ s ^ "\"";
  
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
    end;
  
  val null = str "null" >>> return NULL;
  
  val true' = str "true" >>> return (BOOLEAN true);
  val false' = str "false" >>> return (BOOLEAN false);
  val boolean = true' ooo false';
  
  fun load s = ();
  
  
end;