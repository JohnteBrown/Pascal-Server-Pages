Type
  TObjClass = Class Of TObject;

var
  o: TObjClass;
try
  println(o.ClassName);
except
  on E: Exception do
    println(E.Message);
end;

o := TObject;
println(o.ClassName);
