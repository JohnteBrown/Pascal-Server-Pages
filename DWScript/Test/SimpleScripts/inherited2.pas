type
  TMyObject = class
  public
    function getText: String; virtual;
  end;

function TMyObject.getText: String;
begin
  result := 'We can ';
end;

type
  TSecond = class(TMyObject)
  public
    function getText: String; override;
  End;

function TSecond.getText: String;
begin
  result := inherited getText + 'do it!';
end;

var
o := TSecond.Create;
PrintLn(o.getText);
