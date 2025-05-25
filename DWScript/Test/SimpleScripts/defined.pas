if ConditionalDefined('ALPHA') then
  PrintLn('ALPHA 1');
if ConditionalDefined('BETA') then
  PrintLn('BETA 1');

{$IF Defined('ALPHA')}
PrintLn('Alpha defined');
{$ELSE}
PrintLn('Alpha not defined');
{$ENDIF}
{$DEFINE ALPHA}
{$IF Defined('ALPHA')}
PrintLn('Alpha defined');
{$ELSE}
PrintLn('Alpha not defined');
{$ENDIF}
if ConditionalDefined('ALPHA') then
  PrintLn('ALPHA 2');
if ConditionalDefined('BETA') then
  PrintLn('BETA 2');

{$DEFINE BETA}
if ConditionalDefined('ALPHA') then
  PrintLn('ALPHA 3');
if ConditionalDefined('BETA') then
  PrintLn('BETA 3');

{$UNDEF ALPHA}
if ConditionalDefined('ALPHA') then
  PrintLn('ALPHA 4');
if ConditionalDefined('BETA') then
  PrintLn('BETA 4');

{$IF Defined('ALPHA')}
PrintLn('Alpha defined');
{$ELSE}
PrintLn('Alpha not defined');
{$ENDIF}
