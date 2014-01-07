use "parser.sml";
use "json.sml";
use "io.sml";

print "\n\n\n\n";

val tests = [ "arrays", "numbers", "objects" ];

val files = List.map (fn s => "test/" ^ s ^ ".json") tests;

val raws = List.map IO.read files;

val jsons = List.map JSON.load raws;

val _ = List.map (fn json => print ((JSON.dump json) ^ "\n\n")) jsons;