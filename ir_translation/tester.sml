CM.make "sources.cm";

val args = CommandLine.arguments();

if (List.length args = 0) then print "ERROR: no test file specified" else (

print "STARTING TEST\n";

Main.compile (List.hd args); ()
);

print "FINISHING TEST\n";

val _ = OS.Process.exit(OS.Process.success)