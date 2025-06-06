﻿{ ********************************************************************** }
{ }
{ "The contents of this file are subject to the Mozilla Public }
{ License Version 1.1 (the "License"); you may not use this }
{ file except in compliance with the License. You may obtain }
{ a copy of the License at http://www.mozilla.org/MPL/ }
{ }
{ Software distributed under the License is distributed on an }
{ "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express }
{ or implied. See the License for the specific language }
{ governing rights and limitations under the License. }
{ }
{ Eric Grange }
{ }
{ ********************************************************************** }
unit UdwsXPlatformTests;

interface

uses Classes, SysUtils, Math, Types, dwsXPlatformTests, dwsUtils, dwsXPlatform;

type

  TdwsXPlatformTests = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure DecimalPointTest;
    procedure CollectFilesTest;
    procedure DateTimeConversionTest;
    procedure MillisecondsConversionTest;
    procedure UnicodeLowerAndUpperCaseTest;
    procedure UnicodeCompareTest;
    procedure RawBytesStringTest;
    procedure BytesToWordsTest;
    procedure SwapBytesTest;
    procedure MRSWTest;
    procedure VersionsTest;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

// ------------------
// ------------------ TdwsXPlatformTests ------------------
// ------------------

// SetUp
//
procedure TdwsXPlatformTests.SetUp;
begin

end;

// TearDown
//
procedure TdwsXPlatformTests.TearDown;
begin

end;

// DateTimeConversionTest
//
procedure TdwsXPlatformTests.CollectFilesTest;
var
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    CollectFiles(ExtractFilePath(ParamStr(0)) + 'Data' + PathDelim,
      '*.txt', Files);
    CheckEquals(3, Files.Count);
  finally
    Files.Free;
  end;
end;

procedure TdwsXPlatformTests.DateTimeConversionTest;
var
  CurrentDateTime: TDateTime;
begin
  CurrentDateTime := Now;
  CheckEquals(CurrentDateTime,
    UTCDateTimeToLocalDateTime(LocalDateTimeToUTCDateTime(CurrentDateTime)));
end;

// MillisecondsConversionTest
//
procedure TdwsXPlatformTests.DecimalPointTest;
var
  oldDecimalSeparator: Char;
begin
  oldDecimalSeparator := GetDecimalSeparator;
  try
    SetDecimalSeparator(',');
    CheckEquals(',', GetDecimalSeparator);
    SetDecimalSeparator('.');
    CheckEquals('.', GetDecimalSeparator);
  finally
    SetDecimalSeparator(oldDecimalSeparator);
  end;
end;

procedure TdwsXPlatformTests.MillisecondsConversionTest;
var
  currentMilliseconds: Int64;
begin
  currentMilliseconds := GetSystemMilliseconds;
  CheckTrue(Abs(currentMilliseconds - UnixTimeToSystemMilliseconds
    (SystemMillisecondsToUnixTime(currentMilliseconds))) < 1000);
end;

procedure TdwsXPlatformTests.RawBytesStringTest;
var
  Bytes: TBytes;
const
  cTestString = 'Hello World!';
begin
  Bytes := RawByteStringToBytes(RawByteString(cTestString));
  CheckEquals(cTestString, BytesToRawByteString(@Bytes[0], Length(Bytes)));
end;

// BytesToWordsTest
//
procedure TdwsXPlatformTests.BytesToWordsTest;
var
  buf: array [0 .. 63] of Byte;
  k, i, base: Integer;
begin
  for k := 0 to Length(buf) div 2 - 1 do
  begin
    base := $0F + k * 3;
    for i := 0 to High(buf) do
      buf[i] := i + base;
    BytesToWordsInPlace(@buf[0], k);
    for i := 0 to k - 1 do
      CheckEquals(base + i, buf[i * 2 + 1] * 256 + buf[i * 2],
        'n=' + IntToStr(k) + ' offset=' + IntToStr(i));
    CheckEquals((base + k * 2 + 1) * 256 + (base + k * 2),
      buf[k * 2 + 1] * 256 + buf[k * 2], 'n=' + IntToStr(k) + ' tail');
  end;
end;

// SwapBytesTest
//
procedure TdwsXPlatformTests.SwapBytesTest;
begin
  CheckEquals($12345671, SwapBytes($71563412));
end;

// MRSWTest
//
{$INLINE OFF}

procedure TdwsXPlatformTests.MRSWTest;
var
  m1: TMultiReadSingleWrite;
begin
  m1 := TMultiReadSingleWrite.Create;
  try
    Check(m1.State = mrswUnlocked, 'unlocked start');
    m1.BeginRead;
    try
      Check(m1.State = mrswReadLock, 'read lock');
      CheckTrue(m1.TryBeginRead, 'try 1');
      m1.EndRead;
      CheckFalse(m1.TryBeginWrite, 'try 2');
    finally
      m1.EndRead;
    end;
    CheckTrue(m1.TryBeginWrite, 'try 3');
    try
      Check(m1.State = mrswWriteLock, 'write lock');
      CheckFalse(m1.TryBeginRead, 'try 4');
    finally
      m1.EndWrite;
    end;
    Check(m1.State = mrswUnlocked, 'unlocked mid');
    m1.BeginWrite;
    try
      CheckFalse(m1.TryBeginRead, 'try 5');
    finally
      m1.EndWrite;
    end;
    Check(m1.State = mrswUnlocked, 'unlocked end');
  finally
    m1.Free;
  end;
end;
{$INLINE AUTO}

// VersionsTest
//
procedure TdwsXPlatformTests.VersionsTest;
var
  v: String;
begin
  v := ApplicationVersion;
  CheckTrue(StrMatches(v, '*.*.*.*'), v);
  CheckEquals(v, ApplicationVersion, 'recheck');
end;

// UnicodeLowerAndUpperCaseTest
//
procedure TdwsXPlatformTests.UnicodeCompareTest;
const
  CTestStrings: array [0 .. 5] of string = ('HeLlO WoRlD!', 'hElLo wOrLd!',
    'Héllo World!', 'AA', 'AAa', 'AB');
begin
  // test equal string with different cases
  CheckEquals(0, UnicodeCompareP(PWideChar(CTestStrings[0]),
    Length(CTestStrings[0]), PWideChar(CTestStrings[1]),
    Length(CTestStrings[1])));

  // test equal string with different cases (same length)
  CheckEquals(0, UnicodeCompareP(PWideChar(CTestStrings[0]),
    PWideChar(CTestStrings[1]), Max(Length(CTestStrings[0]),
    Length(CTestStrings[1]))));

  // test equal string with different cases but different length
  CheckEquals(1, UnicodeCompareP(PWideChar(CTestStrings[0]),
    Length(CTestStrings[0]), PWideChar(CTestStrings[1]),
    Length(CTestStrings[1]) - 2));

  // test unequal string with different cases (same length)
  CheckEquals(-1, UnicodeCompareP(PWideChar(CTestStrings[0]),
    PWideChar(CTestStrings[2]), Max(Length(CTestStrings[0]),
    Length(CTestStrings[2]))));
  CheckEquals(-1, UnicodeCompareP(PWideChar(CTestStrings[4]),
    Length(CTestStrings[4]), PWideChar(CTestStrings[5]),
    Length(CTestStrings[5])));
end;

procedure TdwsXPlatformTests.UnicodeLowerAndUpperCaseTest;
const
  TestStringUpperCaseBasic = '0123456789<=>ABCDEFGHIJKLMNOPQRSTUVWXYZ';
  TestStringUpperCaseSupplement = 'ÀÁÂÃÄÅÆÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖ';
  TestStringUpperCaseExtendedA =
    'ĀĂĄĆĈĊČĎĐĒĔĖĘĚĜĞĠĢĤĦĨĪĬĮĲĴĶĹĻĽĿŁŃŅŇŊŌŎŐŒŔŖŘŚŜŞŠŢŤŦŨŪŬŮŰŲŴŶŹŻŽ';
  TestStringUpperCaseExtendedB = 'ƇƉƑƓƔƘǄǇǊǍǏǑǓǕǗǙǛǞǠǢǤǦǨǪǬǮ';
  TestStringUpperCaseGreek = 'ΑΒΓΔΕΖΗΘΙΚΛΜΝΞΟΠΡΣΤΥΦΧΨΩΪΫϢ';
  TestStringUpperCaseCyrillic =
    'ЁЂЃЄЅІЇЈЉЊЋЌЎЏАБВГДЕЖЗИЙКЛМНОПРСТУФХЦЧШЩЪЫЬЭЮЯ';
  TestStringUpperCaseArmenian = 'ԱԲԳԴԵԶԷԸԹԺԻԼԽԾԿՀՁՂՃՄՅՆՇՈՉՊՋՌՍՎՏՐՑՒՓՔՕ';
  TestStringUpperCaseExtendedAdditional =
    'ḀḂḄḆḈḊḌḎḐḒḔḖḘḚḜḞḠḢḤḦḨḪḬḮḰḲḴḶḸḺḼḾṀṂṄṆṈṊṌṎṐṒṔṖṘṚṜṞṠṢṤṦṨṪṬṮṰṲṴṶṸṺṼṾ';
  TestStringUpperCaseGreekExtended =
    'ἈἉἊἋἌἍἎἏἘἙἚἛἜἝἨἩἪἫἬἭἮἯἸἹἺἻἼἽἾἿὈὉὊὋὌὍὙὛὝὟὨὩὪὫὬὭὮὯᾈᾉᾊᾋᾌᾍ';
  TestStringUpperCaseFullWidth = 'ＡＢＣＤＥＦＧＨＩＪＫＬＭＮＯＰＱＲＳＴＵＶＷＸＹＺ';
  TestStringLowerCaseBasic = '0123456789<=>abcdefghijklmnopqrstuvwxyz';
  TestStringLowerCaseSupplement = 'àáâãäåæçèéêëìíîïðñòóôõö';
  TestStringLowerCaseExtendedA =
    'āăąćĉċčďđēĕėęěĝğġģĥħĩīĭįĳĵķĺļľŀłńņňŋōŏőœŕŗřśŝşšţťŧũūŭůűųŵŷźżž';
  TestStringLowerCaseExtendedB = 'ƈɖƒɠɣƙǆǉǌǎǐǒǔǖǘǚǜǟǡǣǥǧǩǫǭǯ';
  TestStringLowerCaseGreek = 'αβγδεζηθικλμνξοπρστυφχψωϊϋϣ';
  TestStringLowerCaseCyrillic =
    'ёђѓєѕіїјљњћќўџабвгдежзийклмнопрстуфхцчшщъыьэюя';
  TestStringLowerCaseArmenian = 'աբգդեզէըթժիլխծկհձղճմյնշոչպջռսվտրցւփքօ';
  TestStringLowerCaseExtendedAdditional =
    'ḁḃḅḇḉḋḍḏḑḓḕḗḙḛḝḟḡḣḥḧḩḫḭḯḱḳḵḷḹḻḽḿṁṃṅṇṉṋṍṏṑṓṕṗṙṛṝṟṡṣṥṧṩṫṭṯṱṳṵṷṹṻṽṿ';
  TestStringLowerCaseGreekExtended =
    'ἀἁἂἃἄἅἆἇἐἑἒἓἔἕἠἡἢἣἤἥἦἧἰἱἲἳἴἵἶἷὀὁὂὃὄὅὑὓὕὗὠὡὢὣὤὥὦὧᾀᾁᾂᾃᾄᾅ';
  TestStringLowerCaseFullWidth = 'ａｂｃｄｅｆｇｈｉｊｋｌｍｎｏｐｑｒｓｔｕｖｗｘｙｚ';

  procedure CheckLower(const expected, test: String);
  var
    buf: String;
  begin
    UnicodeLowerCase(test, buf);
    CheckEquals(expected, buf);
  end;

  procedure CheckUpper(const expected, test: String);
  var
    buf: String;
  begin
    UnicodeUpperCase(test, buf);
    CheckEquals(expected, buf);
  end;

begin
  CheckLower(TestStringLowerCaseBasic, TestStringUpperCaseBasic);
  CheckLower(TestStringLowerCaseSupplement, TestStringUpperCaseSupplement);
  CheckLower(TestStringLowerCaseExtendedA, TestStringUpperCaseExtendedA);
  CheckLower(TestStringLowerCaseExtendedB, TestStringUpperCaseExtendedB);
  CheckLower(TestStringLowerCaseGreek, TestStringUpperCaseGreek);
  CheckLower(TestStringLowerCaseCyrillic, TestStringUpperCaseCyrillic);
  CheckLower(TestStringLowerCaseArmenian, TestStringUpperCaseArmenian);
  CheckLower(TestStringLowerCaseExtendedAdditional,
    TestStringUpperCaseExtendedAdditional);
  CheckLower(TestStringLowerCaseGreekExtended,
    TestStringUpperCaseGreekExtended);
  CheckLower(TestStringLowerCaseFullWidth, TestStringUpperCaseFullWidth);

  CheckUpper(TestStringUpperCaseBasic, TestStringLowerCaseBasic);
  CheckUpper(TestStringUpperCaseSupplement, TestStringLowerCaseSupplement);
  CheckUpper(TestStringUpperCaseExtendedA, TestStringLowerCaseExtendedA);
  CheckUpper(TestStringUpperCaseExtendedB, TestStringLowerCaseExtendedB);
  CheckUpper(TestStringUpperCaseGreek, TestStringLowerCaseGreek);
  CheckUpper(TestStringUpperCaseCyrillic, TestStringLowerCaseCyrillic);
  CheckUpper(TestStringUpperCaseArmenian, TestStringLowerCaseArmenian);
  CheckUpper(TestStringUpperCaseExtendedAdditional,
    TestStringLowerCaseExtendedAdditional);
  CheckUpper(TestStringUpperCaseGreekExtended,
    TestStringLowerCaseGreekExtended);
  CheckUpper(TestStringUpperCaseFullWidth, TestStringLowerCaseFullWidth);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

RegisterTest('XPlatformTests', TdwsXPlatformTests);

end.
