// ####################################
// TFirstClass
// ####################################

Type

  TFirstClass = Class(TObject)
  private
  Public
    Constructor Create; virtual;
    Destructor Destroy; override;
  End;

Constructor TFirstClass.Create;
Begin
  PrintLn('Create ' + Classname);
  inherited Create;
end;

Destructor TFirstClass.Destroy;
begin
  PrintLn('Destroy ' + Classname);
  inherited Destroy;
end;

// ####################################
// TSecondClass
// ####################################

type

  TSecondClass = Class(TFirstClass)
  private
    FTemp: TFirstClass;
  public
    Constructor Create; Override;
    Destructor Destroy; Override;
  End;

Constructor TSecondClass.Create;
Begin
  PrintLn('2nd Create ' + Classname);
  inherited Create;
  FTemp := TFirstClass.Create;
end;

Destructor TSecondClass.Destroy;
begin
  PrintLn('2nd Destroy ' + Classname);
  FTemp.free;
  inherited Destroy;
end;

// ####################################
// Test
// ####################################

var
  First: TFirstClass;
First := TFirstClass.Create;

var
  Second: TSecondClass;
Second := TSecondClass.Create;

First.free;
Second.free;
