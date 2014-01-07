use "parser.sml";
use "json.sml";

print "\n\n\n\n";

val tests = [ "arrays", "numbers", "objects" ];
val files = List.map (fn s => "test/" ^ s ^ ".json") tests;

local
  fun loop io = case TextIO.inputLine io of
    SOME s => s :: loop io
  | NONE => nil;
in
  fun read file = let
    val io = TextIO.openIn file;
  in
    List.foldr op ^ "" (loop io before TextIO.closeIn io)
  end
end;

val raws = List.map read files;

val jsons = List.map JSON.load raws;

val _ = List.map (fn json => print ((JSON.dump json) ^ "\n\n")) jsons;