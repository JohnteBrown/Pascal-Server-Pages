{$IFDEF CompilerVersion}
{$IF CompilerVersion = 1}
PrintLn(1);
{$ENDIF}
{$ENDIF}
{$IFDEF CompilerVersion}
{$IF CompilerVersion <> 1}
PrintLn(2);
{$ENDIF}
{$ENDIF}
{$IFNDEF CompilerVersion}
{$IF CompilerVersion = 1}
PrintLn(3);
{$ENDIF}
{$ENDIF}
{$IFNDEF CompilerVersion}
{$IF CompilerVersion <> 1}
PrintLn(4);
{$ENDIF}
{$ENDIF}
