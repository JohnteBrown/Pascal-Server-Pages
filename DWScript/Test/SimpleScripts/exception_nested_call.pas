type
  Exception1 = class(Exception)
  end;

type
  Exception2 = class(Exception)
  end;

procedure Baz(i: Integer);
begin
  if i = 0 then
    raise new Exception1('Error message 1')
  else
    raise new Exception2('Error message 2');
end;

procedure Bar(i: Integer);
begin
  Baz(i);
end;

procedure Foo;
var
  i: Integer;
begin
  for i := 0 to 2 do
  begin
    try
      Bar(i);
    except
      on E: Exception1 do
        PrintLn(E.ClassName + ' caught');
    end;
  end;
end;

Foo;
