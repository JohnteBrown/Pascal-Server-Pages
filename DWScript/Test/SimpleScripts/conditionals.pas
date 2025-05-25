{$IFDEF TEST}
PrintLn('Bug 1');
{$ENDIF junk}
{$IFNDEF TEST}
PrintLn('Hello');
{$ENDIF}
{$DEFINE TEST}
{$IFDEF TEST}
PrintLn('World');
{$ENDIF}
{$IFNDEF TEST}
PrintLn('Bug 2');
{$ENDIF}
{$UNDEF TEST}
{$IFDEF TEST}
PrintLn('Bug 3');
{$ENDIF}
{$IFNDEF TEST}
PrintLn('!')
{$ENDIF}
