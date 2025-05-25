PrintLn('before');
{$INCLUDE_ONCE 'include.inc'}
{$INCLUDE_ONCE 'include.inc'}
PrintLn('after');
{$INCLUDE 'include.inc'}
{$INCLUDE_ONCE 'include.inc'}
PrintLn('done');
