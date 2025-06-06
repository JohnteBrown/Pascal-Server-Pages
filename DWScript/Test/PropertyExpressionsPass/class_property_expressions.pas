type
   TBase = class 
      class var Field : Integer = 1;
      class property MultBy2 : Integer read (2*Field);
   end;

type
   TBaseRec = record
      Dummy : Integer;
      class var Field : Integer = 10;
      class property MultBy2 : Integer read (2*Field);
   end;

type   
   TSub = class (TBase)
      class property MultBy3 : Integer read (3*Field);
      class property MultBy2 : Integer read (42*Field);
   end;

type   
   TContainer = class
      class var FSub : TSub;
      class var FBase: TBase;
      class property MultBy2 : Integer read (FBase.MultBy2);
      class property MultBy42 : Integer read (FSub.MultBy2);
      class property MultByX : Integer read (TBase(FSub).MultBy2);
   end;

type
   TContainerRec = record
      Dummy : Integer;
      class var FBase: TBaseRec;
      class property MultBy2 : Integer read (FBase.MultBy2);
   end;

var c := new TContainer;
c.FSub := new TSub;
c.FBase := new TBase;

PrintLn(c.MultBy2);  
PrintLn(c.MultBy42);
PrintLn(c.MultByX);
c.FSub.Field := 2;
c.FBase.Field := 3;
PrintLn(c.MultBy2);
PrintLn(c.MultBy42);
PrintLn(c.MultByX);

var cr : TContainerRec;
PrintLn(cr.MultBy2);
PrintLn(cr.FBase.MultBy2);
PrintLn(cr.FBase.Field);
