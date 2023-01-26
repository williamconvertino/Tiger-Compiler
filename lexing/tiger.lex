type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end

val commentDepth = ref 0

%%

%s COMMENT;

alpha=[A-Za-z];
digit=[0-9];
ws = [\ \t];

%%

\n	                => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
"/*"                => (commentDepth := !commentDepth+1; YYBEGIN COMMENT; continue());
<INITIAL>{ws}       => (continue());
<INITIAL>"array"    => (Tokens.ARRAY (yypos, yypos + size(yytext)));
<INITIAL>":="       => (Tokens.ASSIGN (yypos, yypos + size(yytext)));
<INITIAL>"|"        => (Tokens.OR (yypos, yypos + size(yytext)));
<INITIAL>"<>"       => (Tokens.NEQ (yypos, yypos + size(yytext)));
<INITIAL>"="        => (Tokens.EQ (yypos, yypos + size(yytext)));
<INITIAL>"&"        => (Tokens.AND (yypos, yypos + size(yytext)));
<INITIAL>">="       => (Tokens.GE (yypos, yypos + size(yytext)));
<INITIAL>">"        => (Tokens.GT (yypos, yypos + size(yytext)));
<INITIAL>">="       => (Tokens.GE (yypos, yypos + size(yytext)));
<INITIAL>"<="       => (Tokens.LE (yypos, yypos + size(yytext)));
<INITIAL>"<"        => (Tokens.LT (yypos, yypos + size(yytext)));
<INITIAL>"/"        => (Tokens.DIVIDE (yypos, yypos + size(yytext)));
<INITIAL>"*"        => (Tokens.TIMES (yypos, yypos + size(yytext)));
<INITIAL>"-"        => (Tokens.MINUS (yypos, yypos + size(yytext)));
<INITIAL>"+"        => (Tokens.PLUS (yypos, yypos + size(yytext)));
<INITIAL>"."        => (Tokens.DOT (yypos, yypos + size(yytext)));
<INITIAL>.          => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
<COMMENT>"*/"       => (commentDepth := !commentDepth-1; case (!commentDepth) of 0 => YYBEGIN INITIAL | _ => (); continue());
<COMMENT>.          => (continue());

