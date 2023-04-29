## Compiling

```
CM.make "sources.cm";
Main.compile "../testcases/merge.tig";
```

## Extra Credit

### Coalescing / Freeze / Spilling

Based on [this post](https://edstem.org/us/courses/34991/discussion/2954214) on Ed we think we qualify for extra credit for implementing coalescing, freezing, and spilling in our compiler. These implementations can be found in [regalloc.sml](./regalloc.sml).

A simple (but rather convoluted) test case demonstrating this functionality can be seen by compiling [../testcases/spill.tig](../testcases/spill.tig). This will cause all of the callee-saved regs to be spilled to the frame as well as additional temporaries as they are defined since the number of variables exceeds the number of colorable registers.


## Partial Credit Fixes

### Lexing

No issues in lexing stage

### Parsing

* Empty let
* Empty assignment
* And/Ors not accepted

All of these are fixed and can be confirmed by compiling the example program [fixes-parser.tig](../testcases/fixes-parser.tig) which contains all three previously failing cases.
```
let 
in 
    let 
        type rectype={}
        var a:=5
    in 
    end;
    if (2 = 3 | (5 < 10 & 7 > 2)) then 4 else 7
end
```


### Semantic Analysis

* Allowed duplicate names in mutually recursive function/type groups 
* Allowed variables to take on type nil

All of these are fixed and can be confirmed by compiling the example program [fixes-sa.tig](../testcases/fixes-sa.tig) which contains the two previously failing cases.
```
let 
    /* should raise duplicate mutual recursion error */
    type a = int
    type b = a
    type a = string

    /* should raise duplicate mutual recursion error */
    function foo () = 6
    function bar () = 3
    function foo () = 5

    /* should raise nil assignment error */
    var x := nil
in 
    7
end
```

Note: We did allow for loop var to be reassigned and implicit upcasting of expressions to unit.

### Intermediate Representation

Sakai doesn't currently show a grade for us for IR. We had initially been marked off on a couple things but after discussing with Filip I believe the conclusion was that our IR had no issues. (Apologies again for the pre-emptive spilling that made this more difficult to grade at the time.)

### Instruction Selection

No issues in instruction selection

### Liveness and Register Allocation


