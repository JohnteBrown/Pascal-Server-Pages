function IsMersennePrime(p : Integer) : Boolean;
var
   i, s, m_p : Integer;
begin
   if p=2 then
      Result:=True
   else begin
      m_p := (1 shl p)-1;
      s := 4;
      for i:=3 to p do
         s:=(s*s-2) mod m_p;
      Result:=(s=0);
   end;
end;

const upperBound = Floor(Log2(High(Integer))/2) - 1;

PrintLn('Finding Mersenne primes in M[2..' + IntToStr(upperBound) + ']: ');
Print('M2');

var p : Integer;
for p:=3 to upperBound step 2 do begin
   if IsMersennePrime(p) then
      Print(' M'+IntToStr(p));
end;
PrintLn('');
