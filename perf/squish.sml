val raw = IO.readin TextIO.stdIn;

val json = JSON.load raw;

val _ = print (JSON.dump json);