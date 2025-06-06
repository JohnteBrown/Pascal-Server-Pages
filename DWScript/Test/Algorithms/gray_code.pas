function Encode(v : Integer) : Integer;
begin
   Result := v xor (v shr 1);
end;
 
function Decode(v : Integer) : Integer;
begin
   Result := 0;
   while v>0 do begin
      Result := Result xor v;
	  v := v shr 1;
   end;
end;

PrintLn('decimal  binary   gray    decoded');

var i : Integer;
for i:=0 to 31 do begin
   var g := Encode(i);
   var d := Decode(g);
   PrintLn(Format('  %2d     %s   %s   %s  %2d',
                  [i, IntToBin(i, 5), IntToBin(g, 5), IntToBin(d, 5), d]));
end;
