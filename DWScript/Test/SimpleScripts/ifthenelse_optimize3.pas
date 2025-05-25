//
// New Script
//
const
  B = False;

if B then
begin
  var
  a := 1;
  PrintLn(a);
end
else
begin
  var
  B := 2;
  PrintLn(B);
end;

if not B then
begin
  var
  a := 1;
  PrintLn(a);
end
else
begin
  var
  B := 2;
  PrintLn(B);
end;

if B then
begin
  var
  c := 3;
  PrintLn(c);
  if B then
  begin
    var
    d := 4;
    PrintLn(d);
  end
  else
  begin
    var
    e := 5;
    PrintLn(e);
  end;
end
else
begin
  var
  f := 6;
  PrintLn(f);
  if B then
  begin
    var
    g := 7;
    PrintLn(g);
  end
  else
  begin
    var
    h := 8;
    PrintLn(h);
  end;
end;

if B then
begin
  var
  c := 3;
  PrintLn(c);
  if not B then
  begin
    var
    d := 4;
    PrintLn(d);
  end
  else
  begin
    var
    e := 5;
    PrintLn(e);
  end;
end
else
begin
  var
  f := 6;
  PrintLn(f);
  if not B then
  begin
    var
    g := 7;
    PrintLn(g);
  end
  else
  begin
    var
    h := 8;
    PrintLn(h);
  end;
end;

if not B then
begin
  var
  c := 3;
  PrintLn(c);
  if B then
  begin
    var
    d := 4;
    PrintLn(d);
  end
  else
  begin
    var
    e := 5;
    PrintLn(e);
  end;
end
else
begin
  var
  f := 6;
  PrintLn(f);
  if B then
  begin
    var
    g := 7;
    PrintLn(g);
  end
  else
  begin
    var
    h := 8;
    PrintLn(h);
  end;
end;

if not B then
begin
  var
  c := 3;
  PrintLn(c);
  if not B then
  begin
    var
    d := 4;
    PrintLn(d);
  end
  else
  begin
    var
    e := 5;
    PrintLn(e);
  end;
end
else
begin
  var
  f := 6;
  PrintLn(f);
  if not B then
  begin
    var
    g := 7;
    PrintLn(g);
  end
  else
  begin
    var
    h := 8;
    PrintLn(h);
  end;
end;
