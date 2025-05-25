procedure Test({$IFDEF TEST}{$ENDIF}s: String);
begin
  PrintLn(s);
end;

{$DEFINE TEST}

type
  TDummy = class
{$IFNDEF TEST}
      bug here
{$ENDIF}
  end;

Test(TDummy.ClassName);
