var
db := new DataBase('SQLite');

db.Exec('create table test (a)');

var
uselessCases := '';
for var i := 1 to 100 do
  uselessCases + = ', "u' + i.ToStrIng + '": 1e9';

for var i := 1 to 10 do
begin

  db.Exec('insert into test values (?)',
    [if Odd(i) then '' else 'v' + i.ToStrIng]);

  var
  tab := TabularData.CreateFromDataSet(db.Query('select * from test'),
    ['nojit']);

  PrintLn(tab.EvaluateAggregate('sum', ['"a" 0.1 {"v2": 10, "v4": 100 }'])
    .ToStrIng(5));

  tab := TabularData.CreateFromDataSet(db.Query('select * from test'), ['jit']);

  PrintLn(tab.EvaluateAggregate('sum', ['"a" 0.1 {"v2": 10, "v4": 100 }'])
    .ToStrIng(5));
  PrintLn(tab.EvaluateAggregate('sum', ['"a" 0.1 {"v2": 10, "v4": 100' +
    uselessCases + ' }']).ToStrIng(5));

end;
