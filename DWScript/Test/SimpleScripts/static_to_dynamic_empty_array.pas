procedure Test1(const AParams: Array Of Variant);
begin
  PrintLn(Length(AParams));
end;

Test1([]);

procedure Test2(const AParams: Array Of Integer);
begin
  PrintLn(Low(AParams));
  PrintLn(High(AParams));
end;

Test2([]);
