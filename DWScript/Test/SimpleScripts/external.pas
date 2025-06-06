type
  TAClass = class
    external
    procedure Hello;
  end;

type
  TBClass = class
    external 'hello'
    procedure Hello;
    external 'world';
  end;

{$IFDEF JS_CODEGEN}
  asm
    function Dummy(p) {
    PrintLn('Fake: Unhandled call to external symbol "Dummy" from [line: 25, column: 4]')
     }
  end;
{$ENDIF}
function Dummy(param: Integer): String; external;

function Dummy2(param: Integer): String; external 'byebye';

try
  Dummy(12);
except
  on E: Exception do
    PrintLn(E.Message);
end;
