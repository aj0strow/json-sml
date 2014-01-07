structure IO = 
struct
  
  fun loop io = case TextIO.inputLine io of
    SOME s => s :: loop io
  | NONE => nil;
  
  fun readin io = List.foldr op ^ "" (loop io before TextIO.closeIn io);
  
  fun read file = readin (TextIO.openIn file);
  
end;