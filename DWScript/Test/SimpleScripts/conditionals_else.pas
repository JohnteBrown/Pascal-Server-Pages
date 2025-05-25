{$IFDEF TEST}
PrintLn('Bug 1');
{$ELSE}
PrintLn('Hello');
{$ENDIF}
{$DEFINE test}
{$IFDEF TEST}
PrintLn('World');
{$ELSE}
PrintLn('Bug 2');
{$ENDIF}
