function IsLeapYear(y : Integer) : Boolean;
begin
   Result:=    (y mod 4 = 0)
           and (   ((y mod 100) <> 0)
                or ((y mod 400) = 0) );
end;

const good : array [0..13] of Integer =
   [1600,1660,1724,1788,1848,1912,1972,2032,2092,2156,2220,2280,2344,2348];
const bad : array [0..13] of Integer =
   [1698,1699,1700,1750,1800,1810,1900,1901,1973,2100,2107,2200,2203,2289];

var i : Integer;

PrintLn('Checking leap years');
for i in good do
   if not IsLeapYear(i) then PrintLn(i);

PrintLn('Checking non-leap years');
for i in bad do
   if IsLeapYear(i) then PrintLn(i);