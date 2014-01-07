# json-sml

Thought it would be fun to write a json implementation in Standard ML. It was!

### JSON

The interface is based on the ruby `MultiJson` gem. `MultiJson.load` converts a string to json, and `MultiJson.dump` converts json to the string representation. 

```sml
use "parser.sml";
use "json.sml";

val data = JSON.load "{ \"author\" : \"AJ\" }";
val _ = print ((JSON.dump data) ^ "\n");
(* {"author":"AJ"} *)
```

### Parser

The Parser structure is adapted from a McGill University Comp 302 assignment. Building up parsers through combinations leads to amazingly legible parsers. The code is in `parser.sml` and `json.sml`.

One gotcha was that infixes do not get included in the context when you open a structure, so they need to be re-declared. 

### Stats

At ~ 200 LoC its tiny compared to a self-described tiny C lexer like https://github.com/lloyd/yajl which is ~ 2700 LoC. Yajl has a superset of functionaily though so it may be an unfair comparison. 

For a performance comparison I compiled a 3 line program `squish.sml` that loads, dumps, and prints json passed through a pipe (effectively removing unecessary whitespace). I also wrote the equiavlent Node.js program using the built-in `JSON.parse` and `JSON.stringify` methods. 

```
$ ruby perf/benchmark.rb
sml
  0.010000   0.130000   1.390000 (  1.287413)
js
  0.020000   0.130000   8.430000 (  8.362186)
```

The Standard ML program is ~ 6 times faster. The `perf/benchmark.json` came out of twitter using the chrome inspector for a real-life example. Ran it several times with similar results, but looking forward to a second opinion cause it seems a bit absurd. 

---

AJ Ostrow, January 2014