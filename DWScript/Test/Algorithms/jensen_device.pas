function sum(var i : Integer; lo, hi : Integer; lazy term : Float) : Float;
begin
   i:=lo;
   while i<=hi do begin
      Result += term;
      Inc(i);
   end;
end;

var i : Integer;

PrintLn(Format("%.3f", [sum(i, 1, 100, 1.0/i)]));
