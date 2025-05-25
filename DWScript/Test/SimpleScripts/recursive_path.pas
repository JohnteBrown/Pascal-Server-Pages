type
  TPersistent = class
    fString: string;
    function GetOwner: TPersistent;
    function GetNamePath: String;
  end;

function TPersistent.GetOwner: TPersistent;
begin
  result := nil;
end;

function TPersistent.GetNamePath: string;
var
  S: String;
begin
  result := ClassName;
  if (GetOwner <> nil) then
  begin
    S := GetOwner.GetNamePath;
    if S <> '' then
      result := S + '.' + result;
  end;
end;

var
p := TPersistent.Create;
PrintLn(p.GetNamePath);
