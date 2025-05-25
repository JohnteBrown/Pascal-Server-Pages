type
  TTest = partial class function S: String;

begin
  exit('toto')
end;
end;

type
  TTest = class
  private
    FS: String = 'test';
  public
    function T: String;

    begin
      Result := FS
    end;
  end;

var
T := new TTest;
PrintLn(T.S);
PrintLn(T.T);
