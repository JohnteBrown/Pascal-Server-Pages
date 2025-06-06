type TFunc = function (x : Float) : Float;

function f(x : Float) : Float;
begin
   Result:=x*x*x-3.0*x*x +2.0*x;
end;

const e = 1.0e-12;

function Secant(xA, xB : Float; f : TFunc) : Float;
const
   limit = 50;
var
   fA, fB : Float;
   d : Float;
   i : Integer;
begin
   fA:=f(xA);
   for i:=0 to limit do begin
      fB:=f(xB);
      d:=(xB-xA)/(fB-fA)*fB;
      if Abs(d)<e then
         Exit(xB);
      xA:=xB;
      fA:=fB;
      xB-=d;
   end;
   PrintLn(Format('Function is not converging near (%7.4f,%7.4f).', [xA, xB]));
   Result:=-99.0;
end;

const fstep = 1.0e-2;

var x := -1.032;		// just so we use secant method
var xx, value : Float;
var s := f(x)>0.0;

while (x < 3.0) do begin
   value := f(x);
   if Abs(value)<e then begin
      PrintLn(Format("Root found at x= %12.9f", [x]));
      s := (f(x+0.0001)>0.0);
   end else if (value>0.0)<>s then begin
      xx := Secant(x-fstep, x, f);
      if xx <> -99.0 then   // -99 meaning secand method failed
         PrintLn(Format('Root found at x = %12.9f', [xx]))
      else PrintLn(Format('Root found near x= %7.4f', [xx]));
      s := (f(x+0.0001)>0.0);
   end;
   x += fstep;
end;

