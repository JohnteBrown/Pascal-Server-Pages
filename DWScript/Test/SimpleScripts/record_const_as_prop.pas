type
  TBase = record
    Field: Boolean;
    class

  const
    c1 = 1;

  const
    c2 = 2;
    property p1: Integer read c1;
    property p2: Integer read c2;
  end;

var
  o: TBase;

PrintLn(TBase.p1);
PrintLn(TBase.p2);

PrintLn(o.p1);
PrintLn(o.p2);
