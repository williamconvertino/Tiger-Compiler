# 553-compiler

## Fixes:

Parser:

Cases where Parser failed=>
1) empty let <br />
   FIXED at typechecking/tiger.grm:135 
2) empty assignment <br />
    FIXED at typechecking/tiger.grm:99
3) all AND and ORs seem to be not accepted <br />
    FIXED at typechecking/tiger.grm:154 and typechecking/tiger.grm:156

Semant Analysis:

Cases where Typechecker failed=>
1) Allowed duplicate names in mutually recursive function/type group <br />
   FIXED ir_translation/semant.sml:80
2)  Allowed variables to take on type nil <br />
   FIXED ir_translation/semant.sml:99



