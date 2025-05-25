{ ********************************************************************** }
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
{ The Initial Developer of the Original Code is Matthias }
{ Ackermann. For other initial contributors, see contributors.txt }
{ Subsequent portions Copyright Creative IT. }
{ }
{ Current maintainer: Eric Grange }
{ }
{ ********************************************************************** }
unit dwsCustomData;

{$I dws.inc}

interface

uses
  dwsUtils;

type

  TdwsCustomState = record
    Key: TGUID;
    Value: Variant;
  end;

  TdwsCustomStates = class(TSimpleHash<TdwsCustomState>)
  protected
    function SameItem(const item1, item2: TdwsCustomState): Boolean; override;
    function GetItemHashCode(const item1: TdwsCustomState): Cardinal; override;

    function AddClonedState(const item: TdwsCustomState): TSimpleHashAction;

    function GetState(const index: TGUID): Variant; inline;
    procedure SetState(const index: TGUID; const v: Variant);

  public
    property States[const index: TGUID]: Variant read GetState
      write SetState; default;

    procedure VariantState(const index: TGUID; var result: Variant);

    function BooleanStateDef(const index: TGUID;
      const default: Boolean): Boolean;
    function IntegerStateDef(const index: TGUID;
      const default: Integer): Integer;
    function StringStateDef(const index: TGUID; const default: String): String;

    function Clone: TdwsCustomStates;
  end;

  TdwsCustomInterface = record
    Key: TGUID;
    Value: IInterface;
  end;

  TdwsCustomInterfaces = class(TSimpleHash<TdwsCustomInterface>)
  protected
    function SameItem(const item1, item2: TdwsCustomInterface)
      : Boolean; override;
    function GetItemHashCode(const item1: TdwsCustomInterface)
      : Cardinal; override;

    function GetInterface(const index: TGUID): IInterface; inline;
    procedure SetInterface(const index: TGUID; const intf: IInterface); inline;

  public
    property Interfaces[const index: TGUID]: IInterface read GetInterface
      write SetInterface; default;
  end;

  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
  // ------------------------------------------------------------------
implementation

// ------------------------------------------------------------------
// ------------------------------------------------------------------
// ------------------------------------------------------------------

uses System.SysUtils, System.Variants;

// GUIDToHash
//
function GUIDToHash(const guid: TGUID): Cardinal; inline;
type
  TCardinal4 = array [0 .. 3] of Cardinal;
begin
  result := TCardinal4(guid)[0] xor TCardinal4(guid)[1] xor TCardinal4(guid)
    [2] xor TCardinal4(guid)[3];
  if result = 0 then
    result := 1;
end;

// ------------------
// ------------------ TdwsCustomStates ------------------
// ------------------

// SameItem
//
function TdwsCustomStates.SameItem(const item1, item2: TdwsCustomState)
  : Boolean;
begin
  result := IsEqualGUID(item1.Key, item2.Key);
end;

// GetItemHashCode
//
function TdwsCustomStates.GetItemHashCode(const item1: TdwsCustomState)
  : Cardinal;
begin
  result := GUIDToHash(item1.Key);
end;

// GetState
//
function TdwsCustomStates.GetState(const index: TGUID): Variant;
begin
  VariantState(index, result);
end;

// SetState
//
procedure TdwsCustomStates.SetState(const index: TGUID; const v: Variant);
var
  s: TdwsCustomState;
begin
  s.Key := index;
  s.Value := v;
  Replace(s);
end;

// VariantState
//
procedure TdwsCustomStates.VariantState(const index: TGUID;
  var result: Variant);
var
  s: TdwsCustomState;
begin
  s.Key := index;
  if Match(s) then
    VarCopySafe(result, s.Value)
  else
    VarClearSafe(result);
end;

// BooleanStateDef
//
function TdwsCustomStates.BooleanStateDef(const index: TGUID;
  const default: Boolean): Boolean;
var
  s: TdwsCustomState;
begin
  s.Key := index;
  if Match(s) then
  begin
    case VarType(s.Value) of
      varBoolean:
        result := TVarData(s.Value).VBoolean;
      varNull, varEmpty:
        result := default;
    else
      result := VariantToBool(s.Value);
    end;
  end
  else
    result := default;
end;

// IntegerStateDef
//
function TdwsCustomStates.IntegerStateDef(const index: TGUID;
  const default: Integer): Integer;
var
  s: TdwsCustomState;
begin
  s.Key := index;
  if Match(s) then
    if VariantIsOrdinal(s.Value) then
      result := s.Value
    else if VarIsStr(s.Value) then
      result := StrToIntDef(s.Value, default)
    else
      result := default
  else
    result := default;
end;

// StringStateDef
//
function TdwsCustomStates.StringStateDef(const index: TGUID;
  const default: String): String;
var
  s: TdwsCustomState;
begin
  s.Key := index;
  if Match(s) then
    if VarIsStr(s.Value) or VarIsNumeric(s.Value) then
      VariantToString(s.Value, result)
    else
      result := default
  else
    result := default;
end;

// AddClonedState
//
function TdwsCustomStates.AddClonedState(const item: TdwsCustomState)
  : TSimpleHashAction;
begin
  SetState(item.Key, item.Value);
  result := shaNone;
end;

// Clone
//
function TdwsCustomStates.Clone: TdwsCustomStates;
begin
  result := TdwsCustomStates.Create;
  Self.Enumerate(result.AddClonedState);
end;

// ------------------
// ------------------ TdwsCustomInterfaces ------------------
// ------------------

// SameItem
//
function TdwsCustomInterfaces.SameItem(const item1,
  item2: TdwsCustomInterface): Boolean;
begin
  result := IsEqualGUID(item1.Key, item2.Key);
end;

// GetItemHashCode
//
function TdwsCustomInterfaces.GetItemHashCode(const item1: TdwsCustomInterface)
  : Cardinal;
begin
  result := GUIDToHash(item1.Key);
end;

// GetInterface
//
function TdwsCustomInterfaces.GetInterface(const index: TGUID): IInterface;
var
  s: TdwsCustomInterface;
begin
  s.Key := index;
  if Match(s) then
    result := s.Value
  else
    result := nil;
end;

// SetInterface
//
procedure TdwsCustomInterfaces.SetInterface(const index: TGUID;
  const intf: IInterface);
var
  s: TdwsCustomInterface;
begin
  s.Key := index;
  s.Value := intf;
  Replace(s);
end;

end.
