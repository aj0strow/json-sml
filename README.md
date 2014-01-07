# json-sml

Thought it would be fun to write a json implementation in Standard ML. It was!

### JSON

It parses all of the json files in the `test` folder if you run the `test.sml` file. Here's a quick example though:

```sml
use "parser.sml";
use "json.sml";

val data = JSON.load "{ \"author\" : \"AJ\" }";
val _ = print ((JSON.dump data) ^ "\n");
(* {"author":"AJ"} *)
```

It's likely slow, but at ~200 LoC its tiny compared to a fast C lexer like https://github.com/lloyd/yajl. 

### Parser

The Parser structure is adapted from a McGill University Comp 302 assignment. Building up parsers in a functional way certainly leads to amazingly legible parsers. 

One gotcha was that infixes do not get included in the context when you open a structure. That means the infixes declarations must be copied. 