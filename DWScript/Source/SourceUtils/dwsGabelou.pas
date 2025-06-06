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
{    Copyright Eric Grange / Creative IT                               }
{                                                                      }
{**********************************************************************}
unit dwsGabelou;

{$I ../dws.inc}

interface

uses
   System.SysUtils,
   dwsUtils, dwsExprs, dwsSymbols, dwsErrors, dwsScriptSource, dwsSymbolDictionary;

type

   TGabelouMessage = class(TScriptMessage)
      public
         constructor Create(msgs : TdwsMessageList;
                            const msgFmt : UnicodeString; const args : array of const;
                            const scriptPos : TScriptPos);
         constructor CreateOnSymbolPosList(
                            msgs : TdwsMessageList; symPosList : TSymbolPositionList;
                            const description : UnicodeString);

         function AsInfo : UnicodeString; override;
   end;

   IdwsGabelouRule = interface
      function GetName : UnicodeString;
      function GetDescription : UnicodeString;

      property Name : UnicodeString read GetName;
      property Description : UnicodeString read GetDescription;

      procedure Evaluate(const aProg : IdwsProgram; msgs : TdwsMessageList;
                         const restrictToSourceFile : TSourceFile);
   end;

   TdwsGabelouRule = class abstract (TInterfacedSelfObject, IdwsGabelouRule)
      private
         FName : UnicodeString;
         FDescription : UnicodeString;

         function GetName : UnicodeString;
         function GetDescription : UnicodeString;

      public
         constructor Create; virtual;

         procedure Evaluate(const aProg : IdwsProgram; msgs : TdwsMessageList;
                            const restrictToSourceFile : TSourceFile); virtual; abstract;

         property Name : UnicodeString read FName write FName;
         property Description : UnicodeString read FDescription write FDescription;
   end;

   TdwsGabelouRuleClass = class of TdwsGabelouRule;

   TdwsGabelou = class abstract (TRefCountedObject)
      private
         FRules : array of IdwsGabelouRule;

         class var vRegisteredRuleClasses : array of TdwsGabelouRuleClass;

         procedure AddRegisteredRules;

      public
         constructor Create;

         procedure Evaluate(const aProg : IdwsProgram; msgs : TdwsMessageList;
                            const restrictToSourceFile : TSourceFile = nil); overload;
         procedure Evaluate(const aProg : IdwsProgram;
                            const restrictToSourceFile : TSourceFile = nil); overload;

         procedure AddRule(const rule : IdwsGabelouRule);
         procedure RemoveRule(const rule : IdwsGabelouRule);
         procedure ClearRules;

         class procedure RegisterRuleClass(aClass : TdwsGabelouRuleClass); static;
         class procedure RegisterRuleClasses(const aClasses : array of TdwsGabelouRuleClass); static;
         class procedure UnRegisterRuleClass(aClass : TdwsGabelouRuleClass); static;
         class procedure ClearRuleClasses; static;
   end;

   TdwsSymbolDictionaryGabelouRule = class abstract (TdwsGabelouRule)
      public
         procedure Evaluate(const aProg : IdwsProgram; msgs : TdwsMessageList;
                            const restrictToSourceFile : TSourceFile); override;
         procedure EvaluateSymbol(const aSymbolList : TSymbolPositionList; msgs : TdwsMessageList); virtual; abstract;
   end;

   TdwsFunctionUseGabelouSubRule = class abstract
      public
         class procedure Evaluate(const aProg : TdwsProgram; msgs : TdwsMessageList;
                                  funcExpr : TFuncExprBase); virtual; abstract;
   end;
   TdwsFunctionUseGabelouSubRuleClass = class of TdwsFunctionUseGabelouSubRule;

   TdwsFunctionUseGabelouRule = class (TdwsGabelouRule)
      private
         class var vSubRules : array of TdwsFunctionUseGabelouSubRuleClass;

      public
         procedure Evaluate(const aProg : IdwsProgram; msgs : TdwsMessageList;
                            const restrictToSourceFile : TSourceFile); override;

         class procedure RegisterSubRule(subRule : TdwsFunctionUseGabelouSubRuleClass); static;
         class procedure UnRegisterSubRule(subRule : TdwsFunctionUseGabelouSubRuleClass); static;
   end;

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------
implementation
// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses dwsGabelouStrings;

// ------------------
// ------------------ TdwsGabelouRule ------------------
// ------------------

// Create
//
constructor TdwsGabelouRule.Create;
begin
   inherited;
end;

// GetName
//
function TdwsGabelouRule.GetName : UnicodeString;
begin
   Result:=FName;
end;

// GetDescription
//
function TdwsGabelouRule.GetDescription : UnicodeString;
begin
   Result:=FDescription;
end;

// ------------------
// ------------------ TdwsGabelou ------------------
// ------------------

// Create
//
constructor TdwsGabelou.Create;
begin
   inherited;
   AddRegisteredRules;
end;

// AddRegisteredRules
//
procedure TdwsGabelou.AddRegisteredRules;
var
   i : Integer;
begin
   for i:=0 to High(vRegisteredRuleClasses) do
      AddRule(vRegisteredRuleClasses[i].Create);
end;

// Evaluate
//
procedure TdwsGabelou.Evaluate(const aProg : IdwsProgram; msgs : TdwsMessageList;
                               const restrictToSourceFile : TSourceFile = nil);
var
   i : Integer;
begin
   for i:=0 to High(FRules) do
      FRules[i].Evaluate(aProg, msgs, restrictToSourceFile);
end;

// Evaluate
//
procedure TdwsGabelou.Evaluate(const aProg : IdwsProgram;
                               const restrictToSourceFile : TSourceFile = nil);
begin
   Evaluate(aProg, aProg.Msgs, restrictToSourceFile);
end;

// AddRule
//
procedure TdwsGabelou.AddRule(const rule : IdwsGabelouRule);
var
   n : Integer;
begin
   n:=Length(FRules);
   SetLength(FRules, n+1);
   FRules[n]:=rule;
end;

// RemoveRule
//
procedure TdwsGabelou.RemoveRule(const rule: IdwsGabelouRule);
var
   i, n : Integer;
begin
   n:=Length(FRules)-1;
   for i:=0 to n do begin
      if FRules[i]=rule then begin
         if i<n then
           Move(FRules[i+1], FRules[i], (n-i)*SizeOf(Pointer));
         SetLength(FRules, n);
         Exit;
      end;
   end;
end;

// ClearRules
//
procedure TdwsGabelou.ClearRules;
begin
   SetLength(FRules, 0);
end;

// RegisterRuleClass
//
class procedure TdwsGabelou.RegisterRuleClass(aClass : TdwsGabelouRuleClass);
var
   n : Integer;
begin
   n:=Length(vRegisteredRuleClasses);
   SetLength(vRegisteredRuleClasses, n+1);
   vRegisteredRuleClasses[n]:=aClass;
end;

// RegisterRuleClasses
//
class procedure TdwsGabelou.RegisterRuleClasses(const aClasses : array of TdwsGabelouRuleClass);
var
   i : Integer;
begin
   for i:=0 to High(aClasses) do
      RegisterRuleClass(aClasses[i]);
end;

// UnRegisterRuleClass
//
class procedure TdwsGabelou.UnRegisterRuleClass(aClass: TdwsGabelouRuleClass);
var
   i, n : Integer;
begin
   n:=Length(vRegisteredRuleClasses)-1;
   for i:=0 to n do begin
      if vRegisteredRuleClasses[i]=aClass then begin
         if i<n then
           Move(vRegisteredRuleClasses[i+1], vRegisteredRuleClasses[i],
                (n-i)*SizeOf(Pointer));
         SetLength(vRegisteredRuleClasses, n);
         Exit;
      end;
   end;
end;

// ClearRuleClasses
//
class procedure TdwsGabelou.ClearRuleClasses;
begin
   SetLength(vRegisteredRuleClasses, 0);
end;

// ------------------
// ------------------ TdwsSymbolDictionaryGabelouRule ------------------
// ------------------

// Evaluate
//
procedure TdwsSymbolDictionaryGabelouRule.Evaluate(const aProg : IdwsProgram; msgs : TdwsMessageList;
                                                   const restrictToSourceFile : TSourceFile);
var
   symDict : TdwsSymbolDictionary;
   symPosList : TSymbolPositionList;
   symPos : TSymbolPosition;
   sym : TSymbol;
begin
   symDict:=aProg.SymbolDictionary;
   for symPosList in symDict do begin
      sym := symPosList.Symbol;
      // skip magic symbols
      if (sym.Name = '') or StrContains(sym.Name, ' ') then
         continue;
      symPos := symPosList.FindUsage(suDeclaration);
      if     (symPos <> nil)
         and (   (restrictToSourceFile = nil)
              or (symPos.ScriptPos.SourceFile = restrictToSourceFile)) then begin
         EvaluateSymbol(symPosList, msgs);
      end;
   end;
end;

// ------------------
// ------------------ TGabelouMessage ------------------
// ------------------

// Create
//
constructor TGabelouMessage.Create(msgs : TdwsMessageList;
                            const msgFmt : UnicodeString; const args : array of const;
                            const scriptPos : TScriptPos);
begin
   inherited Create(msgs, Format(msgFmt, args), scriptPos);
end;

// CreateOnSymbolPosList
//
constructor TGabelouMessage.CreateOnSymbolPosList(
                            msgs : TdwsMessageList; symPosList : TSymbolPositionList;
                            const description : UnicodeString);
begin
   inherited Create(msgs, Format('"%s", %s', [symPosList.Symbol.Name, description]),
                    symPosList.Items[0].ScriptPos);
end;

// AsInfo
//
function TGabelouMessage.AsInfo: UnicodeString;
begin
   Result:=Format(GAB_HintMessage, [inherited AsInfo]);
end;

// ------------------
// ------------------ TdwsFunctionUseGabelouRule ------------------
// ------------------

type
   TdwsFunctionUseGabelouContext = class
      Prog : TdwsProgram;
      Msgs : TdwsMessageList;
      Restrict : TSourceFile;
      procedure EnumeratorProc(parent, expr : TExprBase; var abort : Boolean);
   end;

procedure TdwsFunctionUseGabelouContext.EnumeratorProc(parent, expr : TExprBase; var abort : Boolean);
var
   i : Integer;
begin
   if not (expr is TFuncExprBase) then Exit;
   if (Restrict <> nil) and (expr.ScriptPos.SourceFile <> Restrict) then Exit;

   for i := 0 to High(TdwsFunctionUseGabelouRule.vSubRules) do begin
      TdwsFunctionUseGabelouRule.vSubRules[i].Evaluate(Prog, Msgs, TFuncExprBase(expr));
   end;
end;

// Evaluate
//
procedure TdwsFunctionUseGabelouRule.Evaluate(const aProg : IdwsProgram; msgs : TdwsMessageList;
                                              const restrictToSourceFile : TSourceFile);
var
   context : TdwsFunctionUseGabelouContext;

   procedure EnumerateProgram(prog : TdwsProgram);
   begin
      context.Prog := prog;
      prog.InitExpr.RecursiveEnumerateSubExprs(context.EnumeratorProc);
      prog.Expr.RecursiveEnumerateSubExprs(context.EnumeratorProc);
      if prog is TdwsMainProgram then
         TdwsMainProgram(prog).FinalExpr.RecursiveEnumerateSubExprs(context.EnumeratorProc);
   end;

   procedure EnumerateExecutable(const exec : IExecutable);
   var
      obj : TObject;
   begin
      if exec = nil then Exit;
      obj := exec.GetSelf;
      if obj is TdwsProgram then
         EnumerateProgram(TdwsProgram(obj));
   end;

var
   symPosList : TSymbolPositionList;
   sym : TSymbol;
begin
   context := TdwsFunctionUseGabelouContext.Create;
   try
      context.Msgs := msgs;
      context.Restrict := restrictToSourceFile;

      EnumerateProgram(aProg.ProgramObject);

      for symPosList in aProg.SymbolDictionary do begin
         sym := symPosList.Symbol;
         if sym is TFuncSymbol then begin
            if sym is TSourceFuncSymbol then
               EnumerateExecutable(TSourceFuncSymbol(sym).Executable)
            else if sym is TSourceMethodSymbol then
               EnumerateExecutable(TSourceMethodSymbol(sym).Executable)
         end;
      end;
   finally
      context.Free;
   end;
end;

// RegisterSubRule
//
class procedure TdwsFunctionUseGabelouRule.RegisterSubRule(subRule : TdwsFunctionUseGabelouSubRuleClass);
var
   n : Integer;
begin
   n := Length(vSubRules);
   SetLength(vSubRules, n+1);
   vSubRules[n] := subRule;
end;

// UnRegisterSubRule
//
class procedure TdwsFunctionUseGabelouRule.UnRegisterSubRule(subRule : TdwsFunctionUseGabelouSubRuleClass);
var
   i : Integer;
begin
   for i := 0 to High(vSubRules) do begin
      if vSubRules[i] = subRule then begin
         Delete(vSubRules, i, 1);
         Break;
      end;
   end;
end;

end.
