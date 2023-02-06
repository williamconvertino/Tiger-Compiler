type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end
val strText = ref ""
val strStart = ref 0
val strActive = ref false
val commentDepth = ref 0

fun eof() = 
    let val pos = hd(!linePos)
    in 
        (
            case (!commentDepth) of
                0 => () | _ => (commentDepth := 0; ErrorMsg.error pos ("unmatched comment"));
	        case  (!strActive) of
                false => () | true => (strActive := false; ErrorMsg.error pos ("unclosed string"));
            Tokens.EOF(pos,pos)
        )
    end

fun stringToDec str = 
    let val dec = valOf(Int.fromString(String.extract (str, 1, NONE)))
        val pos = hd(!linePos)
    in
        case ((dec < 256)) of
            true => (strText := !strText ^ Char.toString (chr dec)) |
            false => ErrorMsg.error pos ("invalid ascii character: " ^ Int.toString(dec))
    end
    


%%
%s STRING COMMENT;

alpha=[A-Za-z];
digit=[0-9];
whitespace = [\ \t];

%%

<INITIAL,COMMENT>\n	    => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<INITIAL> {whitespace}  => (continue());

<INITIAL,COMMENT>"/*"                    
                        => (commentDepth := !commentDepth+1; YYBEGIN COMMENT; continue());
<COMMENT>"*/"           => (commentDepth := !commentDepth-1; case (!commentDepth) of 0 => YYBEGIN INITIAL | _ => (); continue());
<COMMENT>.              => (continue());

<INITIAL>"type"         => (Tokens.TYPE (yypos, yypos + size(yytext)));
<INITIAL>"var"          => (Tokens.VAR (yypos, yypos + size(yytext)));
<INITIAL>"function"     => (Tokens.FUNCTION (yypos, yypos + size(yytext)));
<INITIAL>"break"        => (Tokens.BREAK (yypos, yypos + size(yytext)));
<INITIAL>"of"           => (Tokens.OF (yypos, yypos + size(yytext)));
<INITIAL>"end"          => (Tokens.END (yypos, yypos + size(yytext)));
<INITIAL>"in"           => (Tokens.IN (yypos, yypos + size(yytext)));
<INITIAL>"nil"          => (Tokens.NIL (yypos, yypos + size(yytext)));
<INITIAL>"let"          => (Tokens.LET (yypos, yypos + size(yytext)));
<INITIAL>"do"           => (Tokens.DO (yypos, yypos + size(yytext)));
<INITIAL>"to"           => (Tokens.TO (yypos, yypos + size(yytext)));
<INITIAL>"for"          => (Tokens.FOR (yypos, yypos + size(yytext)));
<INITIAL>"while"        => (Tokens.WHILE (yypos, yypos + size(yytext)));
<INITIAL>"else"         => (Tokens.ELSE (yypos, yypos + size(yytext)));
<INITIAL>"then"         => (Tokens.THEN (yypos, yypos + size(yytext)));
<INITIAL>"if"           => (Tokens.IF (yypos, yypos + size(yytext)));
<INITIAL>"array"        => (Tokens.ARRAY (yypos, yypos + size(yytext)));

<INITIAL>":="           => (Tokens.ASSIGN (yypos, yypos + size(yytext)));
<INITIAL>"|"            => (Tokens.OR (yypos, yypos + size(yytext)));
<INITIAL>"<>"           => (Tokens.NEQ (yypos, yypos + size(yytext)));
<INITIAL>"="            => (Tokens.EQ (yypos, yypos + size(yytext)));
<INITIAL>"&"            => (Tokens.AND (yypos, yypos + size(yytext)));
<INITIAL>">="           => (Tokens.GE (yypos, yypos + size(yytext)));
<INITIAL>">"            => (Tokens.GT (yypos, yypos + size(yytext)));
<INITIAL>">="           => (Tokens.GE (yypos, yypos + size(yytext)));
<INITIAL>"<="           => (Tokens.LE (yypos, yypos + size(yytext)));
<INITIAL>"<"            => (Tokens.LT (yypos, yypos + size(yytext)));
<INITIAL>"/"            => (Tokens.DIVIDE (yypos, yypos + size(yytext)));
<INITIAL>"*"            => (Tokens.TIMES (yypos, yypos + size(yytext)));
<INITIAL>"-"            => (Tokens.MINUS (yypos, yypos + size(yytext)));
<INITIAL>"+"            => (Tokens.PLUS (yypos, yypos + size(yytext)));
<INITIAL>"."            => (Tokens.DOT (yypos, yypos + size(yytext)));

<INITIAL>"}"            => (Tokens.RBRACE (yypos, yypos + size(yytext)));
<INITIAL>"{"            => (Tokens.LBRACE (yypos, yypos + size(yytext)));
<INITIAL>"]"            => (Tokens.RBRACK (yypos, yypos + size(yytext)));
<INITIAL>"["            => (Tokens.LBRACK (yypos, yypos + size(yytext)));
<INITIAL>")"            => (Tokens.RPAREN (yypos, yypos + size(yytext)));
<INITIAL>"("            => (Tokens.LPAREN (yypos, yypos + size(yytext)));
<INITIAL>";"            => (Tokens.SEMICOLON (yypos, yypos + size(yytext)));
<INITIAL>":"            => (Tokens.COLON (yypos, yypos + size(yytext)));
<INITIAL>","            => (Tokens.COMMA (yypos, yypos + size(yytext)));


<INITIAL>{digit}+       => (Tokens.INT (valOf(Int.fromString yytext),yypos, yypos + size(yytext)));

<INITIAL>{alpha}({digit}|{alpha}|"_")*
                        => (Tokens.ID (yytext,yypos, yypos + size(yytext)));

<STRING>\\[\t\n\f ]+\\  => (continue());

<STRING>\\n             => (strText := !strText ^ "\n"; continue());
<STRING>\\t             => (strText := !strText ^ "\t"; continue());
<STRING>\\{digit}{3}    => (stringToDec yytext; continue());
<STRING>\\\"            => (strText := !strText ^ "\""; continue());
<STRING>\\\\            => (strText := !strText ^ "\\"; continue());


<STRING>\n	            => (strText := !strText ^ yytext; lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<STRING>\\.             => (ErrorMsg.error yypos ("illegal character " ^ yytext); strText := !strText ^ yytext; continue());

<INITIAL>\"             => (strActive := true; YYBEGIN STRING; strText := ""; strStart := yypos; continue());
<STRING>\"              => (strActive := false; YYBEGIN INITIAL; Tokens.STRING (!strText, !strStart, yypos));
<STRING>.               => (strText := !strText ^ yytext; continue());

<INITIAL>.              => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
