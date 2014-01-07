# json-sml

So far the following works:

```sml
use "parser.sml";
use "json.sml";

val raw = "{\"key\":[true,false,null,[\"hello\"]],\"nested\":{\"off\":null}}";
print (JSON.dump (JSON.load raw) ^ "\n");
````
