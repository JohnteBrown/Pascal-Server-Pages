{**********************************************************************}
{                                                                      }
{    "The contents of this file are subject to the Mozilla Public      }
{    License Version 1.1 (the "License"); you may not use this         }
{    file except in compliance with the License. You may obtain        }
{    a copy of the License at http://www.mozilla.org/MPL/              }
{                                                                      }
{    Software distributed under the License is distributed on an       }
{    "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express       }
{    or implied. See the License for the specific language             }
{    governing rights and limitations under the License.               }
{                                                                      }
{    The Initial Developer of the Original Code is Matthias            }
{    Ackermann. For other initial contributors, see contributors.txt   }
{    Subsequent portions Copyright Creative IT.                        }
{                                                                      }
{    Current maintainer: Eric Grange                                   }
{                                                                      }
{**********************************************************************}
unit dwsTokenizer;

{$I dws.inc}

interface

uses
  System.SysUtils, System.Classes, System.TypInfo,
  dwsTokenTypes, dwsScriptSource, dwsErrors, dwsUtils
  {$ifdef FPC},lazutf8{$endif};

type

   TTokenizerCaseSensitivity = (
      tcsCaseInsensitive,        // tokenizer alpha token are case insensitive
      tcsCaseSensitiveStrict,    // tokenizer alpha token are case sensitive (strict)
      tcsHintCaseMismatch        // tokenizer alpha token are case insensitive but will hint when case is mismatched
   );

   TTokenizer = class;

   // TTokenBuffer
   //
   TTokenBuffer = record
      Len : Integer;
      Capacity : Integer;
      CaseSensitive : TTokenizerCaseSensitivity;
      Buffer : array of Char;
      Unifier : TStringUnifier;
      Owner : TTokenizer;

      procedure AppendChar(c : Char);
      procedure Grow;
      procedure Clear; inline;
      function LastChar : Char;
      function ToStr : String; overload; inline;
      procedure ToStr(var result : String); overload;
      procedure AppendToStr(var result : String);
      procedure AppendMultiToStr(var result : String);
      procedure AppendTripleToStr(var result : String; msgs : TdwsCompileMessageList; const scriptPos : TScriptPos);
      procedure ToUpperStr(var result : String); overload;
      function UpperFirstChar : Char;
      function UpperMatchLen(const str : String) : Boolean;
      function MatchLen(const str : String) : Boolean;
      procedure RaiseInvalidIntegerConstant;
      function BinToInt64 : Int64;
      function HexToInt64 : Int64;
      procedure RemoveUnderscoresFromBuffer;
      function ToInt64 : Int64;
      function ToFloat : Double;
      function ToType : TTokenType;
      function ToAlphaType : TTokenType;
      function ToAlphaTypeCaseSensitive : TTokenType;
      function ToAlphaTypeHintMismatch : TTokenType;

      class function StringToTokenType(const str : String) : TTokenType; static;
   end;

   TToken = ^TTokenRecord;
   TTokenRecord = record
      private
         FString : String;
         FNext : TToken;

      public
         FFloat : Double;
         FInteger : Int64;
         FTyp : TTokenType;
         FScriptPos : TScriptPos;
         FPosPtr : PChar;

         property AsString : String read FString;

         function EmptyString : Boolean; inline;
   end;

   TCharsType = set of AnsiChar;
   TTransition = class;
   TTransitionArray = array [#0..#127] of TTransition;

   TState = class (TRefCountedObject)
      private
         FOwnedTransitions : TTightList;
         FTransitions : TTransitionArray;
         FName : String;

      public
         destructor Destroy; override;

         function FindTransition(c :  Char) : TTransition; inline;
         procedure AddTransition(const chrs : TCharsType; o : TTransition);
         procedure AddEOFTransition(o : TTransition);
         procedure SetTransition(c : AnsiChar; o : TTransition); inline;
         procedure SetElse(o : TTransition);
   end;

   TConvertAction = (
      caNone,
      caClear,
      caName, caNameEscaped,
      caBin, caHex, caInteger, caFloat,
      caChar, caCharHex,
      caString, caMultiLineString, caStringTriple,
      caSwitch,
      caDotDot, caAmp, caAmpAmp,
      caEndOfText
      );
   TTransitionOptions = set of (toStart, toFinal);

   TTransition = class (TRefCountedObject)
      private
         NextState : TState;
         Start : Boolean; // Marks the begin of a Token
         Final : Boolean; // Marks the end of a Token
         Action : TConvertAction;
         IsError : Boolean;
         Consume : Boolean;
         Seek : Boolean;
         Galop : Boolean;

      public
         constructor Create(aNextState: TState; opts: TTransitionOptions; actn: TConvertAction);
   end;

   TElseTransition = class(TTransition)
     constructor Create(actn : TConvertAction);
   end;

   TErrorTransitionLocation = (
      etlocCurrentPosition, // tokenizer position where error was detected
      etlocStartPosition    // start of token
   );

   TErrorTransition = class(TTransition)
      private
         ErrorMessage : String;
         ErrorLocation : TErrorTransitionLocation;

      public
         constructor Create(const msg : String; errLocation : TErrorTransitionLocation = etlocCurrentPosition);
   end;

   TCheckTransition = class(TTransition);
   TSeekTransition = class (TCheckTransition) // Transition, next Char
      constructor Create(nstate: TState; opts: TTransitionOptions; actn: TConvertAction);
   end;
   TConsumeTransition = class (TSeekTransition) // Transition, consume Char, next Char
      constructor Create(nstate: TState; opts: TTransitionOptions; actn: TConvertAction);
   end;
   TGalopTransition = class (TSeekTransition) // Eat characters
      constructor Create(nstate: TState; opts: TTransitionOptions; actn: TConvertAction);
   end;

   TSwitchHandler = function(const switchName : String) : Boolean of object;

   TTokenizerRules = class
      private
         FStates : TObjectList<TState>;
         FEOFTransition : TErrorTransition;
         FReservedNames : TTokenTypes;
         FSymbolTokens : TTokenTypes;
         FReservedTokens : TTokenTypes;
         FCaseSensitive : TTokenizerCaseSensitivity;

      protected
         function CreateState(const stateName : String = '') : TState;
         function StartState : TState; virtual; abstract;

      public
         constructor Create; virtual;
         destructor Destroy; override;

         procedure PrepareStates;

         function CreateTokenizer(msgs : TdwsCompileMessageList; unifier : TStringUnifier) : TTokenizer;

         property ReservedNames : TTokenTypes read FReservedNames write FReservedNames;
         property SymbolTokens : TTokenTypes read FSymbolTokens write FSymbolTokens;
         property ReservedTokens : TTokenTypes read FReservedTokens;
         property CaseSensitive : TTokenizerCaseSensitivity read FCaseSensitive write FCaseSensitive;
   end;

   TTokenizerSourceInfo = record
      FHotPos : TScriptPos;
      FCurPos : TScriptPos;
      FPosPtr : PChar;
      FPathName : TFileName;
      FLocation : TFileName;
      FText : String;
      FDefaultPos : TScriptPos;
   end;
   PTokenizerSourceInfo = ^TTokenizerSourceInfo;

   TTokenizerConditional = (tcIf, tcElse);
   TTokenizerConditionalInfo = record
      Conditional : TTokenizerConditional;
      ScriptPos : TScriptPos;
   end;

   TTokenizerEndSourceFileEvent = procedure (sourceFile : TSourceFile) of object;
   TTokenizerActionEvent = procedure (tokenizer : TTokenizer; action : TConvertAction) of object;

   TTokenizer = class
      private
         FTokenBuf : TTokenBuffer;
         FOnBeforeAction : TTokenizerActionEvent;
         FStartState : TState;
         FSource : TTokenizerSourceInfo;

         FOnEndSourceFile : TTokenizerEndSourceFileEvent;

         FToken : TToken;
         FNextToken : TToken;

         FRules : TTokenizerRules;
         FTokenPool : TToken;

         FSwitchHandler : TSwitchHandler;
         FSwitchProcessor : TSwitchHandler;

         FSourceStack : array of TTokenizerSourceInfo;

         FMsgs : TdwsCompileMessageList;
         FConditionalDefines : IAutoStrings;
         FConditionalDepth : TSimpleStack<TTokenizerConditionalInfo>;

         procedure AllocateToken;
         procedure ReleaseToken;

         procedure HandleChar(var tokenBuf : TTokenBuffer; var result : TToken);
         procedure HandleBin(var tokenBuf : TTokenBuffer; var result : TToken);
         procedure HandleHexa(var tokenBuf : TTokenBuffer; var result : TToken);
         procedure HandleInteger(var tokenBuf : TTokenBuffer; var result : TToken);
         procedure HandleFloat(var tokenBuf : TTokenBuffer; var result : TToken);
         function  HandleSwitch : Boolean;

         procedure ConsumeToken;

         procedure ReadToken;
         procedure AddCompilerStopFmtTokenBuffer(const formatString : String);

      public
         constructor Create(rules : TTokenizerRules; msgs : TdwsCompileMessageList;
                            unifier : TStringUnifier = nil);
         destructor Destroy; override;

         procedure BeginSourceFile(sourceFile : TSourceFile; const pathName : TFileName = '');
         procedure EndSourceFile;

         function GetToken : TToken; inline;
         function HasTokens : Boolean;
         procedure KillToken; inline;

         function RawTokenBuffer : String;
         function RawTokenBufferNameToType : TTokenType;

         function Test(t : TTokenType) : Boolean;
         function TestAny(const t : TTokenTypes) : TTokenType;
         function TestDelete(t : TTokenType) : Boolean;
         function TestDeleteAny(const t : TTokenTypes) : TTokenType;
         function TestName : Boolean;
         function TestAnyName : Boolean;

         function TestDeleteNamePos(var aName : String; var aPos : TScriptPos) : Boolean; inline;
         function TestDeleteAnyNamePos(var aName : String; var aPos : TScriptPos) : Boolean; inline;

         procedure SkipTo(t : TTokenType);

         procedure SimulateToken(t : TTokenType; const scriptPos : TScriptPos);
         procedure SimulateStringToken(const scriptPos : TScriptPos; const str : String);
         procedure SimulateIntegerToken(const scriptPos : TScriptPos; const i : Int64);
         procedure SimulateNameToken(const scriptPos : TScriptPos; const name : String);

         property PosPtr : PChar read FSource.FPosPtr;
         property Text : String read FSource.FText;
         property DefaultPos : TScriptPos read FSource.FDefaultPos;
         property HotPos : TScriptPos read FSource.FHotPos;
         property CurrentPos : TScriptPos read FSource.FCurPos;
         property PathName : TFileName read FSource.FPathName;
         property Location : TFileName read FSource.FLocation;

         function SafePathName : String; inline;

         property ConditionalDepth : TSimpleStack<TTokenizerConditionalInfo> read FConditionalDepth;
         property Rules : TTokenizerRules read FRules;

         property SwitchHandler : TSwitchHandler read FSwitchHandler write FSwitchHandler;
         property SwitchProcessor : TSwitchHandler read FSwitchProcessor write FSwitchProcessor;
         property ConditionalDefines : IAutoStrings read FConditionalDefines write FConditionalDefines;

         property OnEndSourceFile : TTokenizerEndSourceFileEvent read FOnEndSourceFile write FOnEndSourceFile;
         property OnBeforeAction : TTokenizerActionEvent read FOnBeforeAction write FOnBeforeAction;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsXPlatform, dwsStrings;

const
   cFormatSettings : TFormatSettings = ( DecimalSeparator : {%H-}'.' );

// EmptyString
//
function TTokenRecord.EmptyString : Boolean;
begin
   Result:=(FString='');
end;

// AppendChar
//
{$IFOPT R+}{$DEFINE RANGEON}{$R-}{$ELSE}{$UNDEF RANGEON}{$ENDIF}
procedure TTokenBuffer.AppendChar(c : Char);
begin
   if Len >= Capacity then Grow;
   Buffer[Len] := c;
   Inc(Len);
end;
{$IFDEF RANGEON}{$R+}{$UNDEF RANGEON}{$ENDIF}

// Grow
//
procedure TTokenBuffer.Grow;
begin
   if Capacity=0 then
      Capacity:=256
   else Capacity:=Capacity*2;
   SetLength(Buffer, Capacity);
end;

// Clear
//
procedure TTokenBuffer.Clear;
begin
   Len := 0;
end;

// LastChar
//
function TTokenBuffer.LastChar : Char;
begin
   if Len>0 then
      Result:=Buffer[Len-1]
   else Result:=#0;
end;

// ToStr
//
function TTokenBuffer.ToStr : String;
begin
   ToStr(Result);
end;

// ToStr
//
procedure TTokenBuffer.ToStr(var result : String);
begin
   case Len of
      0 : result := '';
      1 : UnifyAssignChar(@Buffer[0], result);
   else
      if Unifier <> nil then
         Unifier.UnifyAssignP(@Buffer[0], Len, result)
      else SetString(result, PChar(@Buffer[0]), Len);
   end;
end;

// AppendToStr
//
procedure TTokenBuffer.AppendToStr(var result : String);
var
   n : Integer;
begin
   if Len>0 then begin
      n:=Length(result);
      SetLength(result, n+Len);
      Move(Buffer[0], PChar(Pointer(result))[n], Len*SizeOf(Char));
   end;
end;

// AppendMultiToStr
//
procedure TTokenBuffer.AppendMultiToStr(var result : String);
var
   i, n, k, minWhite, white : Integer;
   leftWhite, firstIsCRLF, firstLine : Boolean;
begin
   if Len=0 then Exit;
   // count nb lines and minimum whitespace, also detect if first line is whitespace + CRLF
   minWhite:=MaxInt;
   leftWhite:=True;
   white:=0;
   firstIsCRLF:=False;
   firstLine:=True;
   for i:=0 to Len-1 do begin
      case Buffer[i] of
         ' ' : if leftWhite then Inc(white);
         #13 : ;
         #10 : begin
            if firstLine then begin
               if leftWhite then
                  firstIsCRLF:=True;
               firstLine:=False;
            end;
            if not leftWhite then begin
               if white<minWhite then
                  minWhite:=white;
               leftWhite:=True;
            end;
            white:=0;
         end;
      else
         leftWhite:=False;
      end;
   end;

   // ok now collect and remove indents
   k:=Length(result);
   SetLength(result, k+Len); // allocate for worst case
   i:=0;
   n:=Len;

   // do we have to remove indents?
   if firstIsCRLF then begin
      // skip first line
      while Buffer[i]<>#10 do
         Inc(i);
      Inc(i);
   end;

   leftWhite:=(minWhite>0);
   white:=0;
   while i<n do begin
      case Buffer[i] of
         ' ' : begin
            if leftWhite and (white<minWhite) then
               Inc(white)
            else begin
               Inc(k);
               result[k]:=' ';
            end;
         end;
         #10 : begin
            leftWhite:=(minWhite>0);
            white:=0;
            Inc(k);
            result[k]:=Buffer[i];
         end
      else
         leftWhite:=False;
         Inc(k);
         result[k]:=Buffer[i];
      end;
      Inc(i);
   end;

   SetLength(result, k);
end;

// AppendTripleToStr
//
procedure TTokenBuffer.AppendTripleToStr(
   var result : String; msgs : TdwsCompileMessageList;
   const scriptPos : TScriptPos
);
var
   temp : String;
begin
   result := '';
   AppendToStr(temp);
   // triple quoted string should
   // - start with apos (dedup) + newline
   // - end with newline + (optional indent) + 2x apos
   // - mid line should have same indent as what's before the last apos
   var n := Length(temp);
   if    (n < 5)
      or (temp[1] <> '''')
      or (not CharInSet(temp[2], [ #13, #10 ]))
      or (temp[n] <> '''')
      or (temp[n-1] <> '''') then begin
      msgs.AddCompilerError(scriptPos, TOK_TripleAposStringError);
      Exit;
   end;

   Dec(n);
   var lastIndentOffset := n;
   repeat
      Dec(n);
      case temp[n] of
         ' ', #9 :   // indent whitespace
            lastIndentOffset := n;
         #10 : begin
            if temp[n-1] = #13 then
               Dec(n);
            Break;
         end;
         #13 : Break;
      else
         msgs.AddCompilerError(scriptPos, TOK_TripleAposStringIndentError);
         Exit;
      end;
   until False; // we are guarded by the check for starting apos

   var pIndentPatternStart := PChar(@temp[lastIndentOffset]);
   var pIndentPatternLength := Length(temp) - lastIndentOffset - 1;

   SetLength(result, n);
   var pSrc := PChar(@temp[2]);
   var pSrcTail := PChar(@temp[n]);
   var pDest := PChar(result);
   var firstLine := True;
   var column : Integer := 0;
   while UIntPtr(pSrc) < UIntPtr(pSrcTail) do begin
      case pSrc^ of
         #13 : begin
            if not firstLine then begin
               pDest^ := pSrc^;
               Inc(pDest);
            end;
            Inc(pSrc);
            if pSrc^ = #10 then begin
               if not firstLine then begin
                  pDest^ := pSrc^;
                  Inc(pDest);
               end;
               Inc(pSrc);
            end;
            firstLine := False;
            column := 0;
         end;
         #10 : begin
            if not firstLine then begin
               pDest^ := pSrc^;
               Inc(pDest);
            end;
            Inc(pSrc);
            firstLine := False;
            column := 0;
         end;
      else
         if column < pIndentPatternLength then begin
            if pSrc^ <> pIndentPatternStart[column] then begin
               msgs.AddCompilerError(scriptPos, TOK_TripleAposStringIndentError);
               Exit;
            end;
            Inc(column);
            Inc(pSrc);
         end else begin
            pDest^ := pSrc^;
            Inc(pSrc);
            Inc(pDest);
         end;
      end;
   end;
   SetLength(Result, (UIntPtr(pDest) - UIntPtr(result)) div SizeOf(Char));
end;

// ToUpperStr
//
procedure TTokenBuffer.ToUpperStr(var result : String);
var
   i : Integer;
   ch : Char;
   pResult : PChar;
begin
   if Len=0 then
      result:=''
   else begin
      SetLength(result, Len);
      pResult:=PChar(result);
      for i:=0 to Len-1 do begin
         ch:=Buffer[i];
         case ch of
            'a'..'z' : pResult[i]:=Char(Word(ch) xor $0020)
         else
            pResult[i]:=ch;
         end;
      end;
   end;
end;

// UpperFirstChar
//
function TTokenBuffer.UpperFirstChar : Char;
begin
   if Len=0 then
      Result:=#0
   else begin
      Result:=Buffer[0];
      case Result of
         'a'..'z' : Result:=Char(Word(Result) xor $0020)
      end;
   end;
end;

// RaiseInvalidIntegerConstant
//
procedure TTokenBuffer.RaiseInvalidIntegerConstant;
begin
   raise EIntOverflow.CreateFmt(TOK_InvalidIntegerConstant, [ToStr]);
end;

// BinToInt64
//
function TTokenBuffer.BinToInt64 : Int64;
var
   i : Integer;
begin
   Result := 0;
   if Buffer[0] = '%' then
      i := 1
   else i := 2;
   for i := i to Len-1 do begin
      // highest bit already set, if we're still here we'll overflow
      if Result<0 then
         RaiseInvalidIntegerConstant;
      case Ord(Buffer[i]) of
         Ord('1') : Result:=(Result shl 1) or 1;
         Ord('0') : Result:=(Result shl 1);
      end;
   end;
end;

// BinToInt64
//
function TTokenBuffer.HexToInt64 : Int64;
var
   i : Integer;
   v : Integer;
begin
   if Buffer[0]='$' then
      i:=1     // $ form
   else i:=2;  // 0x form
   Result:=0;
   while i<Len do begin
      // highest nibble already set, if we're still here we'll overflow
      if (Result shr 60)>0 then RaiseInvalidIntegerConstant;
      v:=Ord(Buffer[i]);
      Inc(i);
      case v of
         Ord('0')..Ord('9') : v:=v-Ord('0');
         Ord('a')..Ord('f') : v:=v-(Ord('a')-10);
         Ord('A')..Ord('F') : v:=v-(Ord('A')-10);
      else
         continue;
      end;
      Result:=(Result shl 4) or v;
   end;
end;

// RemoveUnderscoresFromBuffer
//
procedure TTokenBuffer.RemoveUnderscoresFromBuffer;
begin
   var pSrc := PChar(@Buffer[0]);
   var pDest := pSrc;
   for var i := 0 to Len-1 do begin
      if pSrc^ = '_' then begin
         Dec(Len);
      end else begin
         if pSrc <> pDest then
            pDest^ := pSrc^;
         Inc(pDest);
      end;
      Inc(pSrc);
   end;
end;

// ToInt64
//
function TTokenBuffer.ToInt64 : Int64;

   function ComplexToInt64(var buffer : TTokenBuffer) : Int64;
   begin
      if Len > 1 then
         RemoveUnderscoresFromBuffer;
      Result := StrToInt64(buffer.ToStr);
   end;

var
   i, i2 : Integer;
begin
   case Len of
      1 : begin
         i:=Ord(Buffer[0])-Ord('0');
         if Cardinal(i)<Cardinal(10) then Exit(i);
      end;
      2 : begin
         i:=Ord(Buffer[0])-Ord('0');
         if Cardinal(i)<Cardinal(10) then begin
            i2:=Ord(Buffer[1])-Ord('0');
            if Cardinal(i2)<Cardinal(10) then
               Exit(i*10+i2);
         end;
      end;
   end;
   Result:=ComplexToInt64(Self);
end;

// ToFloat
//
function TTokenBuffer.ToFloat : Double;
begin
   if Len > 1 then
      RemoveUnderscoresFromBuffer;

   AppendChar(#0);
   if not TryStrToDouble(PChar(@Buffer[0]), Result, cFormatSettings) then
      raise EConvertError.Create('');
end;

// ToType
//
function TTokenBuffer.ToType : TTokenType;
begin
   Result := ttNAME;
   if Len=0 then Exit;

   case Buffer[0] of
      '/':
         if Len=1 then
            Result := ttDIVIDE
         else if Len=2 then
            if Buffer[1]='=' then
               Result := ttDIVIDE_ASSIGN; // '/='
      '*':
         if Len=1 then
            Result := ttTIMES
         else if Len=2 then case Buffer[1] of
            '=' : Result := ttTIMES_ASSIGN; // '*='
            '*' : Result := ttTIMES_TIMES; // '**'
         end;
      '+':
         if Len=1 then
            Result := ttPLUS
         else if Len=2 then case Buffer[1] of
            '=' : Result := ttPLUS_ASSIGN; // '+='
            '+' : Result := ttPLUS_PLUS;   // '++'
         end;
      '-':
         if Len=1 then
            Result := ttMINUS
         else if Len=2 then case Buffer[1] of
            '=': Result := ttMINUS_ASSIGN; // '-='
            '-': Result := ttMINUS_MINUS; // '--'
         end;
      '@':
         if Len=1 then
            Result := ttAT
         else if Len=2 then
            if Buffer[1]='=' then
               Result := ttAT_ASSIGN; // '@='
      '%':
         if Len=1 then
            Result := ttPERCENT
         else if Len=2 then
            if Buffer[1]='=' then
               Result := ttPERCENT_ASSIGN; // '%='
      '^':
         if Len=1 then
            Result := ttCARET
         else if Len=2 then
            if Buffer[1]='=' then
               Result := ttCARET_ASSIGN; // '^='
      '~':
         if Len=1 then
            Result := ttTILDE
         else if Len=2 then
            if Buffer[1]='=' then
               Result := ttTILDE_ASSIGN; // '~='
      '\':
         if Len=1 then
            Result := ttBACKSLASH;
      ';': Result := ttSEMI;
      '(': Result := ttBLEFT;
      ')': Result := ttBRIGHT;
      '[': Result := ttALEFT;
      ']': Result := ttARIGHT;
      '!':
         if Len=1 then
            Result := ttEXCLAMATION
         else if Len=2 then
            if Buffer[1]='=' then
               Result := ttEXCL_EQ;     // '!='
      '?':
         if Len=1 then
            Result := ttQUESTION
         else if Len=2 then case Buffer[1] of
            '?' : Result:= ttQUESTION_QUESTION;  // '??'
            '.' : Result:= ttQUESTION_DOT;       // '?.'
         end;
      '=':
         case Len of
            1 : Result := ttEQ;
            2 : case Buffer[1] of
               '>' : Result := ttEQ_GTR;      // '=>'
               '=' : Result := ttEQ_EQ;       // '=='
            end;
            3 : if Buffer[2] = '=' then
               Result := ttEQ_EQ_EQ;           // '==='
         end;
      '<':
         if Len=1 then // '<'
            Result := ttLESS
         else if Len=2 then case Buffer[1] of
            '=' : Result := ttLESS_EQ;     // '<='
            '>' : Result := ttNOT_EQ;      // '<>'
            '<' : Result := ttLESS_LESS;   // '<<'
         end;
      '>':
         if Len=1 then // '>'
            Result := ttGTR
         else if Len=2 then case Buffer[1] of
            '=' : Result := ttGTR_EQ;      // '>='
            '>' : Result := ttGTR_GTR;     // '>>'
         end;
      ':':
         if Len=1 then // ':'
            Result := ttCOLON
         else if Len=2 then
            if Buffer[1]='=' then
               Result := ttASSIGN; // ':='
      ',': Result := ttCOMMA;
      '{': Result := ttCLEFT;
      '}': Result := ttCRIGHT;
      '.':
         if Len=1 then
            Result := ttDOT;
      '$':
         if Len=1 then
            Result := ttDOLLAR;
      '|':
         if Len=1 then
            Result := ttPIPE
         else if Len=2 then
            if Buffer[1]='|' then
               Result := ttPIPE_PIPE;
   else
      case CaseSensitive of
         tcsCaseInsensitive : Result := ToAlphaType;
         tcsCaseSensitiveStrict : Result := ToAlphaTypeCaseSensitive;
         tcsHintCaseMismatch : Result := ToAlphaTypeHintMismatch;
      else
         Assert(False);
      end;
   end;
end;

// ToAlphaType
//
const
   cAlphaTypeTokens : TTokenTypes = [
      ttAND, ttARRAY, ttABSTRACT, ttAS, ttASM, ttASYNC, ttAWAIT,
      ttBEGIN, ttBREAK,
      ttCONST, ttCLASS, ttCONSTRUCTOR, ttCASE, ttCDECL, ttCONTINUE,
      ttDO, ttDOWNTO, ttDIV, ttDEFAULT, ttDESCRIPTION, ttDESTRUCTOR, ttDEPRECATED,
      ttELSE, ttEMPTY, ttEND, ttENSURE, ttENUM, ttEXCEPT, ttEXIT, ttEXTERNAL, ttEXPORT,
      ttFALSE, ttFINAL, ttFINALIZATION, ttFINALLY, ttFLAGS, ttFOR,
      ttFORWARD, ttFUNCTION, ttHELPER,
      ttIF, ttIMPLIES, ttIMPLEMENTATION, ttIMPLEMENTS, ttIMPLICIT,
      ttIN, ttINITIALIZATION, ttINLINE, ttINVARIANTS,
      ttINHERITED, ttINDEX, ttINTERFACE, ttIS,
      ttLAMBDA, ttLAZY, ttLIBRARY,
      ttMETHOD, ttMOD,
      ttNAMESPACE, ttNEW, ttNIL, ttNOT,
      ttOBJECT, ttOF, ttOLD, ttON, ttOPERATOR, ttOR, ttOVERLOAD, ttOVERRIDE,
      ttPARTIAL, ttPROCEDURE, ttPROPERTY, ttPASCAL, ttPROGRAM,
      ttPRIVATE, ttPROTECTED, ttPUBLIC, ttPUBLISHED,
      ttRAISE, ttRECORD, ttREAD, ttREADONLY, ttREINTRODUCE, ttREFERENCE, ttREGISTER,
      ttREPEAT, ttREQUIRE, ttRESOURCESTRING,
      ttSAFECALL, ttSAR, ttSEALED, ttSET, ttSHL, ttSHR, ttSTATIC, ttSTDCALL, ttSTRICT,
      ttTHEN, ttTO, ttTRUE, ttTRY, ttTYPE,
      ttUNIT, ttUNTIL, ttUSES,
      ttVAR, ttVIRTUAL,
      ttWHILE, ttWITH, ttWRITE,
      ttXOR
      ];
type
   TTokenAlphaLookup = record
      Alpha : String;
      Token : TTokenType;
   end;
   TTokenAlphaLookups = array of TTokenAlphaLookup;
   PTokenAlphaLookups = ^TTokenAlphaLookups;
var
   vAlphaToTokenType : array [2..14] of array ['A'..'X'] of TTokenAlphaLookups;

procedure PrepareAlphaToTokenType;
var
   n, len : Integer;
   tokenName : String;
   tt : TTokenType;
begin
   for tt in cAlphaTypeTokens do begin
      tokenName := GetEnumName(TypeInfo(TTokenType), Ord(tt));
      len:=Length(tokenName)-2;
      Assert(len<=14);
      n:=Length(vAlphaToTokenType[len][tokenName[3]]);
      SetLength(vAlphaToTokenType[len][tokenName[3]], n+1);
      with vAlphaToTokenType[len][tokenName[3]][n] do begin
         Alpha := StrDeleteLeft(tokenName, 2);
         Token := tt;
      end;
   end;
end;

// ------------------
// ------------------ TTokenBuffer ------------------
// ------------------

// UpperMatchLen
//
function TTokenBuffer.UpperMatchLen(const str : String) : Boolean;
begin
   var p := PChar(Pointer(str));
   for var i := 1 to Len-1 do begin
      var ch := Buffer[i];
      case ch of
         'a'..'z' : if Char(Word(ch) xor $0020) <> p[i] then Exit(False);
      else
         if ch <> p[i] then Exit(False);
      end;
   end;
   Result := True;
end;

// ToAlphaType
//
{$IFOPT R+}{$DEFINE RANGEON}{$R-}{$ELSE}{$UNDEF RANGEON}{$ENDIF}
function TTokenBuffer.ToAlphaType : TTokenType;
var
   lookups : Pointer;
begin
   if (Len<2) or (Len>14) then Exit(ttNAME);
   var ch := Buffer[0];
   case ch of
      'a'..'x' : lookups := @vAlphaToTokenType[Len][Char(Word(ch) xor $0020)];
      'A'..'X' : lookups := @vAlphaToTokenType[Len][ch];
   else
      Exit(ttNAME);
   end;
   lookups := PPointer(lookups)^;
   for var i := 0 to High(TTokenAlphaLookups(lookups)) do begin
      if UpperMatchLen(TTokenAlphaLookups(lookups)[i].Alpha) then
         Exit(TTokenAlphaLookups(lookups)[i].Token);
   end;
   Result := ttNAME;
end;
{$IFDEF RANGEON}{$R+}{$UNDEF RANGEON}{$ENDIF}

// MatchLen
//
function TTokenBuffer.MatchLen(const str : String) : Boolean;
var
   i : Integer;
   p : PChar;
   ch : Char;
begin
   p:=PChar(Pointer(str));
   for i:=1 to Len-1 do begin
      ch:=Buffer[i];
      case ch of
         'a'..'z' : if Char(Word(ch) xor $0020)<>p[i] then Exit(False);
      else
         Exit(False);
      end;
   end;
   Result:=True;
end;

// ToAlphaTypeCaseSensitive
//
function TTokenBuffer.ToAlphaTypeCaseSensitive : TTokenType;
var
   ch : Char;
   i : Integer;
   lookups : PTokenAlphaLookups;
begin
   if (Len<2) or (Len>14) then Exit(ttNAME);
   ch:=Buffer[0];
   case ch of
      'a'..'x' : lookups:=@vAlphaToTokenType[Len][Char(Word(ch) xor $0020)];
   else
      Exit(ttNAME);
   end;
   for i:=0 to High(lookups^) do begin
      if MatchLen(lookups^[i].Alpha) then
         Exit(lookups^[i].Token);
   end;
   Result:=ttNAME;
end;

// ToAlphaTypeHintMismatch
//
function TTokenBuffer.ToAlphaTypeHintMismatch : TTokenType;

   procedure HintMismatch;
   var
      buf : String;
      hint : TCaseMismatchHintMessage;
   begin
      SetString(buf, PChar(Buffer), Len);
      hint := TCaseMismatchHintMessage.Create(
         Owner.FMsgs,
         Format(CPH_KeywordCaseMismatch, [ buf, cTokenStrings[Result] ]),
         Owner.FToken.FScriptPos,
         hlPedantic
      );
      hint.Expected := cTokenStrings[Result];
   end;

begin
   Result := ToAlphaType;
   if (Result <> ttNAME) and (Owner.FMsgs.HintsLevel >= hlPedantic) then
      if not CompareMem(Pointer(Buffer), Pointer(cTokenStrings[Result]), Len*SizeOf(Char)) then
         HintMismatch;
end;

// StringToTokenType
//
class function TTokenBuffer.StringToTokenType(const str : String) : TTokenType;
var
   c : Char;
   buffer : TTokenBuffer;
begin
   if str='' then Exit(ttNone);

   buffer.Capacity := 0;
   buffer.Len := 0;
   buffer.CaseSensitive :=  tcsCaseInsensitive;
   for c in str do
      buffer.AppendChar(c);

   Result:=buffer.ToType;
end;

// ------------------
// ------------------ TState ------------------
// ------------------

// Destroy
//
destructor TState.Destroy;
begin
   FOwnedTransitions.Clean;
   inherited Destroy;
end;

// FindTransition
//
function TState.FindTransition(c : Char) : TTransition;
begin
   if c>#127 then
      c:=#127;
   Result:=FTransitions[c];
end;

// AddTransition
//
procedure TState.AddTransition(const chrs : TCharsType; o : TTransition);
var
   c : AnsiChar;
begin
   for c:=#0 to #127 do
      if c in chrs then begin
         if FTransitions[c]=nil then
            SetTransition(c, o);
      end;
   FOwnedTransitions.Add(o);
end;

// AddEOFTransition
//
procedure TState.AddEOFTransition(o : TTransition);
begin
   SetTransition(#0, o);
   FOwnedTransitions.Add(o);
end;

// SetTransition
//
procedure TState.SetTransition(c : AnsiChar; o : TTransition);
begin
   FTransitions[c]:=o;
end;

// SetElse
//
procedure TState.SetElse(o : TTransition);
var
   c : AnsiChar;
begin
   for c:=#1 to #127 do
      if FTransitions[c]=nil then
         SetTransition(c, o);
   FOwnedTransitions.Add(o);
end;

// ------------------
// ------------------ TTransition ------------------
// ------------------

// Create
//
constructor TTransition.Create;
begin
   inherited Create;
   NextState := aNextState;
   Start:=toStart in opts;
   Final:=toFinal in opts;
   Action:=actn;
end;

// ------------------
// ------------------ TElseTransition ------------------
// ------------------

// Create
//
constructor TElseTransition.Create(actn: TConvertAction);
begin
   inherited Create(nil, [], actn);
end;

// ------------------
// ------------------ TErrorTransition ------------------
// ------------------

constructor TErrorTransition.Create(const msg : String; errLocation : TErrorTransitionLocation = etlocCurrentPosition);
begin
   inherited Create(nil, [], caNone);
   IsError := True;
   ErrorMessage := msg;
   ErrorLocation := errLocation;
end;

// ------------------
// ------------------ TSeekTransition ------------------
// ------------------

// Create
//
constructor TSeekTransition.Create(nstate: TState; opts: TTransitionOptions; actn: TConvertAction);
begin
   inherited;
   Seek:=True;
end;

// ------------------
// ------------------ TConsumeTransition ------------------
// ------------------

// Create
//
constructor TConsumeTransition.Create(nstate: TState; opts: TTransitionOptions; actn: TConvertAction);
begin
   inherited;
   Consume:=True;
end;

// ------------------
// ------------------ TGalopTransition ------------------
// ------------------

// Create
//
constructor TGalopTransition.Create(nstate: TState; opts: TTransitionOptions; actn: TConvertAction);
begin
   inherited;
   Galop := True;
end;

// ------------------
// ------------------ TTokenizer ------------------
// ------------------

// Create
//
constructor TTokenizer.Create(rules : TTokenizerRules; msgs : TdwsCompileMessageList;
                              unifier : TStringUnifier = nil);
begin
   inherited Create;
   FMsgs := Msgs;
   FTokenBuf.Grow;
   FRules := rules;
   FStartState := FRules.StartState;
   FTokenBuf.CaseSensitive := rules.CaseSensitive;
   FTokenBuf.Unifier := unifier;
   FTokenBuf.Owner := Self;

   FConditionalDepth:=TSimpleStack<TTokenizerConditionalInfo>.Create;
end;

// Destroy
//
destructor TTokenizer.Destroy;
begin
   if FToken<>nil then Dispose(FToken);
   if FNextToken<>nil then Dispose(FNextToken);

   while FTokenPool<>nil do begin
      FToken:=FTokenPool;
      FTokenPool:=FToken.FNext;
      Dispose(FToken);
   end;

   FConditionalDepth.Free;

   inherited;
end;

// BeginSourceFile
//
procedure TTokenizer.BeginSourceFile(sourceFile : TSourceFile; const pathName : TFileName = '');
var
   n : Integer;
begin
   if DefaultPos.SourceFile<>nil then begin
      n:=Length(FSourceStack);
      SetLength(FSourceStack, n+1);
      FSourceStack[n]:=FSource;
   end;

   if pathName<>'' then
      FSource.FPathName := pathName
   else FSource.FPathName := sourceFile.Name;
   FSource.FLocation := sourceFile.Location;
   FSource.FText := sourceFile.Code + (cLineTerminator+#0);
   FSource.FDefaultPos := cNullPos;
   FSource.FDefaultPos.SourceFile := sourceFile;
   FSource.FHotPos := DefaultPos;
   FSource.FCurPos := DefaultPos;
   FSource.FPosPtr := PChar(Text);
   FSource.FCurPos.Line := 1;
   FSource.FCurPos.Col := 1;
end;

// EndSourceFile
//
procedure TTokenizer.EndSourceFile;
var
   n : Integer;
begin
   n:=Length(FSourceStack);
   if n>0 then begin
      if Assigned(FOnEndSourceFile) then
         FOnEndSourceFile(FSource.FDefaultPos.SourceFile);
      FSource:=FSourceStack[n-1];
      SetLength(FSourceStack, n-1);
   end else begin
      Assert(DefaultPos.SourceFile<>nil);
      FSource.FText:='';
      FSource.FDefaultPos:=cNullPos;
      FSource.FHotPos:=cNullPos;
      FSource.FCurPos:=cNullPos;
      FSource.FPosPtr:=nil;
   end;
end;

// GetToken
//
function TTokenizer.GetToken : TToken;
begin
   Result:=FToken;
end;

// ReadToken
//
procedure TTokenizer.ReadToken;
begin
   KillToken;

   if Assigned(FNextToken) then begin
      FToken:=FNextToken;
      FNextToken:=nil;
   end else ConsumeToken;
end;

// AddCompilerStopFmtTokenBuffer
//
procedure TTokenizer.AddCompilerStopFmtTokenBuffer(const formatString : String);
var
   buf : String;
begin
   buf := FTokenBuf.ToStr;
   FMsgs.AddCompilerStopFmt(CurrentPos, formatString, [buf]);
end;

// Test
//
function TTokenizer.Test(t: TTokenType): Boolean;
begin
   if not Assigned(FToken) then begin
      ReadToken;
      if not Assigned(FToken) then
         Exit(False);
   end;

   Result:=(FToken.FTyp=t);
   FSource.FHotPos.SetLineCol(FToken.FScriptPos);
end;

// TestAny
//
function TTokenizer.TestAny(const t: TTokenTypes) : TTokenType;
begin
   if not Assigned(FToken) then begin
      ReadToken;
      if not Assigned(FToken) then
         Exit(ttNone);
   end;

   FSource.FHotPos.SetLineCol(FToken.FScriptPos);
   if (FToken.FTyp in t) then
      Result:=FToken.FTyp
   else Result:=ttNone;
end;

// TestDelete
//
function TTokenizer.TestDelete(t: TTokenType): Boolean;
begin
   if not Assigned(FToken) then begin
      ReadToken;
      if not Assigned(FToken) then
         Exit(False);
   end;

   FSource.FHotPos.SetLineCol(FToken.FScriptPos);
   if FToken.FTyp=t then begin
      KillToken;
      Result:=True;
   end else Result:=False;
end;

// TestDeleteAny
//
function TTokenizer.TestDeleteAny(const t: TTokenTypes) : TTokenType;
begin
   if not Assigned(FToken) then begin
      ReadToken;
      if not Assigned(FToken) then
         Exit(ttNone);
   end;

   FSource.FHotPos.SetLineCol(FToken.FScriptPos);
   if FToken.FTyp in t then begin
      Result:=FToken.FTyp;
      KillToken;
   end else Result:=ttNone;
end;

// TestName
//
function TTokenizer.TestName : Boolean;
begin
   Result:=False;
   if not Assigned(FToken) then
      ReadToken;
   if Assigned(FToken) then begin
      Result:=(FToken.FString<>'') and not (FToken.FTyp in FRules.ReservedTokens);
      FSource.FHotPos.SetLineCol(FToken.FScriptPos);
   end;
end;

// TestAnyName
//
function TTokenizer.TestAnyName : Boolean;
begin
   Result:=False;
   if not Assigned(FToken) then
      ReadToken;
   if Assigned(FToken) then begin
      Result:=(FToken.FString<>'') and not (FToken.FTyp in FRules.SymbolTokens);
      FSource.FHotPos.SetLineCol(FToken.FScriptPos);
   end;
end;

// TestDeleteNamePos
//
function TTokenizer.TestDeleteNamePos(var aName : String; var aPos : TScriptPos) : Boolean;
begin
   if not TestName then
      Result:=False
   else begin
      aName:=GetToken.FString;
      aPos:=HotPos;
      KillToken;
      Result:=True;
   end;
end;

// TestDeleteAnyNamePos
//
function TTokenizer.TestDeleteAnyNamePos(var aName : String; var aPos : TScriptPos) : Boolean;
begin
   if not TestAnyName then
      Result:=False
   else begin
      aName:=GetToken.FString;
      aPos:=HotPos;
      KillToken;
      Result:=True;
   end;
end;

// SkipTo
//
procedure TTokenizer.SkipTo(t : TTokenType);
begin
   while (not Test(t)) and Assigned(FToken) do
      KillToken;
end;

// SimulateToken
//
procedure TTokenizer.SimulateToken(t : TTokenType; const scriptPos : TScriptPos);
begin
   Assert(FNextToken=nil);
   FNextToken:=FToken;
   AllocateToken;
   FToken.FTyp:=t;
   FToken.FScriptPos:=scriptPos;
   FToken.FPosPtr:=nil;
end;

// SimulateStringToken
//
procedure TTokenizer.SimulateStringToken(const scriptPos : TScriptPos; const str : String);
begin
   SimulateToken(ttStrVal, scriptPos);
   FToken.FString:=str;
end;

// SimulateIntegerToken
//
procedure TTokenizer.SimulateIntegerToken(const scriptPos : TScriptPos; const i : Int64);
begin
   SimulateToken(ttIntVal, scriptPos);
   FToken.FInteger:=i;
end;

// SimulateNameToken
//
procedure TTokenizer.SimulateNameToken(const scriptPos : TScriptPos; const name : String);
begin
   SimulateToken(ttNAME, scriptPos);
   FToken.FString:=name;
end;

// SafePathName
//
function TTokenizer.SafePathName : String;
begin
   if Self<>nil then
      Result:=PathName
   else Result:='';
end;

// HasTokens
//
function TTokenizer.HasTokens: Boolean;
begin
   if not Assigned(FToken) then begin
      ReadToken;
      if FToken<>nil then
         FSource.FHotPos.SetLineCol(FToken.FScriptPos);
   end;
   Result:=(FToken<>nil);
end;

// HandleChar
//
procedure TTokenizer.HandleChar(var tokenBuf : TTokenBuffer; var result : TToken);
var
   tokenIntVal, n : Integer;
begin
   tokenIntVal:=FTokenBuf.ToInt64;
   case tokenIntVal of
      0..$FFFF : begin
         n:=Length(result.FString)+1;
         SetLength(result.FString, n);
         result.FString[n]:=Char(tokenIntVal);
         result.FTyp:=ttStrVal;
      end;
      $10000..$10FFFF : begin
         tokenIntVal:=tokenIntVal-$10000;
         n:=Length(result.FString)+2;
         SetLength(result.FString, n);
         result.FString[n-1]:=Char($D800+(tokenIntVal shr 10));
         result.FString[n]:=Char($DC00+(tokenIntVal and $3FF));
         result.FTyp:=ttStrVal;
      end;
   else
      ReleaseToken;
      AddCompilerStopFmtTokenBuffer(TOK_InvalidCharConstant)
   end;
end;

// HandleBin
//
procedure TTokenizer.HandleBin(var tokenBuf : TTokenBuffer; var result : TToken);
begin
   try
      result.FInteger:=tokenBuf.BinToInt64;
      result.FTyp:=ttIntVal;
   except
      ReleaseToken;
      raise;
   end;
end;

// HandleHexa
//
procedure TTokenizer.HandleHexa(var tokenBuf : TTokenBuffer; var result : TToken);
begin
   try
      result.FInteger:=tokenBuf.HexToInt64;
      result.FTyp:=ttIntVal;
   except
      ReleaseToken;
      AddCompilerStopFmtTokenBuffer(TOK_InvalidHexConstant);
   end;
end;

// HandleInteger
//
procedure TTokenizer.HandleInteger(var tokenBuf : TTokenBuffer; var result : TToken);
begin
   try
      if tokenBuf.LastChar='.' then
         Dec(tokenBuf.Len);
      result.FInteger:=tokenBuf.ToInt64;
      result.FTyp:=ttIntVal;
   except
      ReleaseToken;
      AddCompilerStopFmtTokenBuffer(TOK_InvalidIntegerConstant);
   end;
end;

// HandleFloat
//
procedure TTokenizer.HandleFloat(var tokenBuf : TTokenBuffer; var result : TToken);
begin
   try
      result.FFloat:=tokenBuf.ToFloat;
      result.FTyp:=ttFloatVal;
   except
      ReleaseToken;
      AddCompilerStopFmtTokenBuffer(TOK_InvalidFloatConstant);
   end;
end;

// HandleSwitch
//
function TTokenizer.HandleSwitch : Boolean;
begin
   Result:=False;
   if Assigned(FSwitchHandler) then begin
      FSource.FHotPos.SetLineCol(FToken.FScriptPos);

      // Ask parser if we should create a token or not
      FTokenBuf.ToUpperStr(FToken.FString);
      if FSwitchHandler(FToken.FString) then begin
         FToken.FTyp:=ttSWITCH;
      end else begin
         if FToken=nil then
            AllocateToken;
         FTokenBuf.Len:=0;
         Exit(True);
      end;
   end;
end;

// ConsumeToken
//
procedure TTokenizer.ConsumeToken;

   // don't trigger error for EOF
   procedure DoErrorTransition(trns : TErrorTransition; ch : Char);
   begin
      if trns.ErrorMessage = '' then Exit;
      if trns.ErrorLocation = etlocStartPosition then
         FMsgs.AddCompilerStop(FToken.FScriptPos, trns.ErrorMessage)
      else case ch of
         #0 :
            FMsgs.AddCompilerStop(CurrentPos, trns.ErrorMessage);
         #1..#31 :
            FMsgs.AddCompilerStopFmt(CurrentPos, '%s (found #%d)', [trns.ErrorMessage, Ord(ch)]);
      else
         FMsgs.AddCompilerStopFmt(CurrentPos, '%s (found "%s")', [trns.ErrorMessage, ch])
      end;
   end;

   // return True to reset state and continue to next token
   function DoAction(action : TConvertAction) : Boolean;
   begin
      case action of
         caClear : begin
            FTokenBuf.Len:=0;
            FToken.FScriptPos:=DefaultPos;
            FToken.FPosPtr:=nil;
         end;

         // Convert name to token
         caName : begin
            FToken.FTyp:=FTokenBuf.ToType;
            FTokenBuf.ToStr(FToken.FString);
         end;

         // Convert escaped name to token
         caNameEscaped : begin
            FToken.FTyp:=ttNAME;
            FTokenBuf.ToStr(FToken.FString);
         end;

         // converts ASCII code to character (decimal or hex)
         caChar, caCharHex :
            HandleChar(FTokenBuf, FToken);

         // Concatenates the parts of a UnicodeString constant
         caString : begin
            FTokenBuf.AppendToStr(FToken.FString);
            FToken.FTyp:=ttStrVal;
         end;

         caMultiLineString : begin
            FTokenBuf.AppendMultiToStr(FToken.FString);
            FToken.FTyp:=ttStrVal;
         end;

         caStringTriple : begin
            FTokenBuf.AppendTripleToStr(FToken.FString, FMsgs, FToken.FScriptPos);
            FToken.FTyp:=ttStrVal;
         end;

         // Converts binary number to integer
         caBin :
            HandleBin(FTokenBuf, FToken);

         // Converts hexadecimal number to integer
         caHex :
            HandleHexa(FTokenBuf, FToken);

         // Converts integer constants
         caInteger :
            HandleInteger(FTokenBuf, FToken);

         // Converts Floating Point numbers
         caFloat :
            HandleFloat(FTokenBuf, FToken);

         caSwitch :
            if HandleSwitch then Exit(True);

         caDotDot : begin
            FToken.FPosPtr:=PosPtr;
            FToken.FScriptPos:=CurrentPos;
            FToken.FScriptPos.Col:=FToken.FScriptPos.Col-1;
            FToken.FTyp:=ttDOTDOT;
         end;

         caAmp : begin
            FToken.FPosPtr:=PosPtr;
            FToken.FScriptPos:=CurrentPos;
            FToken.FScriptPos.Col:=FToken.FScriptPos.Col-1;
            FToken.FTyp:=ttAMP;
         end;

         caAmpAmp : begin
            FToken.FPosPtr:=PosPtr;
            FToken.FScriptPos:=CurrentPos;
            FToken.FScriptPos.Col:=FToken.FScriptPos.Col-2;
            FToken.FTyp:=ttAMP_AMP;
         end;
      end;
      FTokenBuf.Len:=0;
      Result:=False;
   end;

   // process switch instruction
   procedure DoSwitchProcessor;
   begin
      if Assigned(FSwitchProcessor) then begin
         try
            if FSwitchProcessor(FToken.FString) then begin
               ReleaseToken;
               ConsumeToken;
            end;
         except
            ReleaseToken;
            raise;
         end;
      end;
   end;

var
   state : TState;
   trns : TTransition;
   pch : PChar;
   ch : Char;
begin
   AllocateToken;

   state:=FStartState;
   FTokenBuf.Len:=0;

   // Next character
   pch:=PosPtr;

   // Look for the next token in FText
   while Assigned(state) do begin

      // Find next state
      ch := pch^;
      if Ord(ch) > 127 then begin
         if Ord(ch) = 160 then  // treat no-break space as regular space
            ch := #32
         else ch := #127
      end;
      trns := state.FTransitions[ch];

      // Handle Errors
      if trns.IsError then begin
         if Assigned(FOnBeforeAction) and (state <> FStartState) then
            FOnBeforeAction(Self, caEndOfText);
         DoErrorTransition(trns as TErrorTransition, pch^);
         state := FStartState;
         FTokenBuf.Len:=0;
         if FSourceStack <> nil then begin
            EndSourceFile;
            pch := PosPtr;
            continue;
         end;
         Break;
      end;

      // A new token begins
      if trns.Start then begin
         FToken.FScriptPos:=CurrentPos;
         FToken.FPosPtr:=PosPtr;
      end;

      // Proceed to the next character
      if trns.Seek then begin

         // Add actual character to s
         if trns.Consume then
            FTokenBuf.AppendChar(pch^);

         if trns.Galop then begin
            repeat
               Inc(pch);
               Inc(FSource.FPosPtr);
               Inc(FSource.FCurPos.Col);
            until pch^ <> ch;
         end else begin
            Inc(FSource.FPosPtr);
            if ch = #10 then begin
               Inc(FSource.FCurPos.Line);
               FSource.FCurPos.Col:=1;
            end else Inc(FSource.FCurPos.Col);
            Inc(pch);
         end;
      end;

      // The characters in 's' have to be converted
      if trns.Action<>caNone then begin
         if Assigned(FOnBeforeAction) then
            FOnBeforeAction(Self, trns.Action);
         if DoAction(trns.Action) then begin
            state:=FRules.StartState;
            pch:=PosPtr;
            continue;
         end;
      end;

      // If the token is complete then exit
      if trns.Final then begin
         FStartState:=trns.NextState;
         if FToken.FTyp=ttSWITCH then
            DoSwitchProcessor;
         Exit;
      end else state:=trns.NextState;

   end;

   // Couldn't read a token (end of FText reached)
   ReleaseToken;
end;

// KillToken
//
procedure TTokenizer.KillToken;
begin
   ReleaseToken;
end;

// RawTokenBuffer
//
function TTokenizer.RawTokenBuffer : String;
begin
   FTokenBuf.ToStr(Result);
end;

// RawTokenBufferNameToType
//
function TTokenizer.RawTokenBufferNameToType : TTokenType;
begin
   Result := FTokenBuf.ToType;
end;

// AllocateToken
//
procedure TTokenizer.AllocateToken;
begin
   if FTokenPool<>nil then begin
      FToken:=FTokenPool;
      FTokenPool:=FToken.FNext;
   end else New(FToken);
   FToken.FTyp:=ttNone;
   FToken.FScriptPos.Clear;
end;

// ReleaseToken
//
procedure TTokenizer.ReleaseToken;
begin
   if FToken<>nil then begin
      FToken.FNext:=FTokenPool;
      FTokenPool:=FToken;
      if FTokenPool.FString<>'' then
         FTokenPool.FString:='';
      FToken:=nil;
   end;
end;

// ------------------
// ------------------ TTokenizerRules ------------------
// ------------------

// Create
//
constructor TTokenizerRules.Create;
begin
   FStates := TObjectList<TState>.Create;
   FCaseSensitive := tcsCaseInsensitive;
end;

// Destroy
//
destructor TTokenizerRules.Destroy;
begin
   FStates.Free;
   FEOFTransition.Free;
end;

// PrepareStates
//
procedure TTokenizerRules.PrepareStates;
var
   i : Integer;
   state : TState;
begin
   FReservedTokens:=FSymbolTokens+FReservedNames;

   FEOFTransition:=TErrorTransition.Create('');
   for i:=0 to FStates.Count-1 do begin
      state:=FStates[i];
      if (state.FTransitions[#0]=nil) or not (state.FTransitions[#0] is TErrorTransition) then
         state.FTransitions[#0]:=FEOFTransition;
   end;
end;

// CreateState
//
function TTokenizerRules.CreateState(const stateName : String) : TState;
begin
   Result := TState.Create;
   Result.FName := stateName;
   FStates.Add(Result);
end;

// CreateTokenizer
//
function TTokenizerRules.CreateTokenizer(msgs : TdwsCompileMessageList; unifier : TStringUnifier) : TTokenizer;
begin
   Result := TTokenizer.Create(Self, msgs, unifier);
end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
initialization
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

   PrepareAlphaToTokenType;

end.
