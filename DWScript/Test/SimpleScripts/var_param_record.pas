type
  TRecord = record
    X: Float;
  end;

procedure Test(const X: Float; var r: Float); overload;
begin
  r := X;
end;

procedure Test(const X: Float; var r: TRecord); overload;
var
  temp: Float;
begin
  Test(X, temp);
  r.X := temp;
end;

var
  r: TRecord;
Test(123, r);

PrintLn(r.X);
