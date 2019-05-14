{ *************************************************************************** }
{                                                                             }
{ EControl Common Library                                                     }
{                                                                             }
{ Copyright (c) 2004 - 2015 EControl Ltd., Zaharov Michael                    }
{     www.econtrol.ru                                                         }
{     support@econtrol.ru                                                     }
{ Ported to Lazarus and FreePascal generics:                                  }
{     Alexey Torgashin, UVviewsoft.com                                        }
{                                                                             }
{ *************************************************************************** }

{$mode delphi}
{$define EC_UNICODE}

unit ec_Lists;

interface

uses
  Classes,
  ATSynEdit_FGL;

type
  TSortedItem = class
  protected
    function GetKey: integer; virtual; abstract;
  end;

  TSortedList = class
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TSortedItem;
  public
    constructor Create(OwnObjects: Boolean);
    destructor Destroy; override;

    function Add(Item: TSortedItem): integer;
    procedure Delete(Index: integer);
    procedure Clear;
    function PriorAt(Pos: integer): integer;

    property Items[Index: integer]: TSortedItem read GetItem; default;
    property Count: integer read GetCount;
  end;

  { TRange }

  TRange = packed record
  private
    function GetLength: integer;
  public                                              
    StartPos, EndPos: integer;
    PointStart, PointEnd: TPoint;
    constructor Create(AStartPos, AEndPos: integer);
    constructor Create(AStartPos, AEndPos: integer; const APointStart, APointEnd: TPoint);
    property Size: integer read GetLength;
    class operator=(const a, b: TRange): boolean;
  end;

  { GRangeList }

  // Array of sorted ranges
  GRangeList<GRange> = class (TFPGList<GRange>)
  private
    FUnionSiblings: Boolean;
    FPrevIdx: integer;
    //FSorted: boolean;
  protected
    // Union ranges with the [Index] and [Index + 1]
    // returns new range index (or union result)
    function IsGreater(I1, I2: integer): Boolean;
    function CompProc(const AValue: TRange; AKey: integer): integer;
  public
    constructor Create(UnionSiblings: Boolean = True);
    destructor Destroy; override;
    //property Sorted: boolean read FSorted write FSorted;
    function Add(const Range: GRange): integer; virtual;
    function ClearFromPos(APos: integer): integer;
    // At position or next
    function NextAt(APos: integer): integer;
    // At position or prior
    function PriorAt(APos: integer): integer;
  end;

  TecRangeList = GRangeList<TRange>;

implementation

uses
  //Math, Dialogs,
  SysUtils, Contnrs;

{ TRange }

constructor TRange.Create(AStartPos, AEndPos: integer; const APointStart, APointEnd: TPoint);
begin
  StartPos := AStartPos;
  EndPos := AEndPos;
  PointStart := APointStart;
  PointEnd := APointEnd;
end;

constructor TRange.Create(AStartPos, AEndPos: integer);
begin
  StartPos := AStartPos;
  EndPos := AEndPos;
  PointStart.X := -1;
  PointStart.Y := -1;
  PointEnd.X := -1;
  PointEnd.Y := -1;
end;

function TRange.GetLength: integer;
begin
  Result := EndPos-StartPos;
end;

class operator TRange.=(const a,b: TRange):boolean;
// Not used in real work
begin
  Result:=
    (a.StartPos=b.StartPos) and
    (a.EndPos=b.EndPos);
end;

{ TecRangeList }

constructor GRangeList<GRange>.Create(UnionSiblings: Boolean);
begin
  inherited Create;
  FUnionSiblings := UnionSiblings;
  //FSorted := false;
  Capacity := 512; // Usually editor has many tokens
end;

destructor GRangeList<GRange>.Destroy;
begin
  inherited;
end;

function GRangeList<GRange>.IsGreater(I1, I2: integer): Boolean;
begin
  if FUnionSiblings then
    Result := I1 >= I2
  else
    Result := I1 > I2;
end;

function GRangeList<GRange>.Add(const Range: GRange): integer;
var
  _Range: TRange absolute Range;
begin
  Result := Count;
  inherited Add(Range);
  (*
  if not FSorted or (Count=0) then
  begin
    Result := Count;
    inherited Add(Range);
  end
  else
  begin
    Result := PriorAt(_Range.StartPos);
    Inc(Result);
    if Result = Count then
      inherited Add(Range)
    else
      inherited Insert(Result, Range);
  end;
  *)
end;

function GRangeList<GRange>.NextAt(APos: integer): integer;
begin
  Result := PriorAt(APos);
  if Result = -1 then
   begin
     if Count > 0 then Result := 0
   end else
   if TRange(InternalItems[Result]^).EndPos <= APos then
    if Result < Count - 1 then Inc(Result)
     else Result := -1;
end;


function GRangeList<GRange>.CompProc(const AValue: TRange; AKey: integer): integer;
begin
  if AValue.StartPos > AKey then
    Result := 1
  else
   if (AValue.StartPos <= AKey) and (AValue.EndPos > AKey) then
    Result := 0
  else
    Result := -1;
end;

function GRangeList<GRange>.PriorAt(APos: integer): integer; 
var
  H, I, C: Integer;
begin
  if (FPrevIdx >= 0) and (FPrevIdx < Count - 1) and (TRange(InternalItems[FPrevIdx]^).StartPos <= APos)then
  begin
      if (FPrevIdx >= Count - 1) or (TRange(InternalItems[FPrevIdx + 1]^).StartPos > APos) then
      begin
        Result := FPrevIdx;
        Exit;
      end
      else if (FPrevIdx >= Count - 2) or (TRange(InternalItems[FPrevIdx + 2]^).StartPos > APos) then
      begin
        Result := FPrevIdx + 1;
        Exit;
      end;
  end;
  if Count = 0 then
  begin
    FPrevIdx := -1;
    Exit(-1);
  end
  else
  begin
    Result := 0;
    H := Count - 1;
    while Result <= H do
    begin
      I := (Result + H) shr 1;
      C := CompProc(TRange(InternalItems[i]^), APos);
      if C < 0 then
        Result := I + 1
      else
      begin
        if C = 0 then
        begin
          FPrevIdx := I;
          Exit(I);
        end;
        H := I - 1;
      end;
    end;
    if Result >= Count then
      Result := Count - 1;
    if Result >= 0 then
      if CompProc(TRange(InternalItems[i]^), APos) > 0 then
        dec(Result);

    FPrevIdx := Result;
  end;
end;

function GRangeList<GRange>.ClearFromPos(APos: integer): integer;
var idx, NStart: integer;
begin
  Result := APos;
  if APos <= 0 then
  begin
    Clear;
    Exit;
  end;

  idx := NextAt(APos);
  if idx <> -1 then
  begin
    NStart := TRange(InternalItems[idx]^).StartPos;
    if NStart < APos then
      Result := NStart;
    DeleteRange(idx, Count-1);
  end;
end;

{ TSortedList }

function TSortedList.Add(Item: TSortedItem): integer;
begin
  if (Count = 0) or (Items[Count - 1].GetKey <= Item.GetKey) then
  begin
    Result := Count;
    FList.Add(Item);
  end else
  begin
    Result := PriorAt(Item.GetKey);
    Inc(Result);
    if Result = Count then
      FList.Add(Item)
    else
      FList.Insert(Result, Item);
  end;
end;

procedure TSortedList.Clear;
begin
  FList.Clear;
end;

constructor TSortedList.Create(OwnObjects: Boolean);
begin
  inherited Create;
  if OwnObjects then
    FList := TObjectList.Create
  else
    FList := TList.Create;
end;

procedure TSortedList.Delete(Index: integer);
begin
  FList.Delete(Index);
end;

destructor TSortedList.Destroy;
begin
  FreeAndNil(FList);
  inherited;
end;

function TSortedList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TSortedList.GetItem(Index: integer): TSortedItem;
begin
  Result := TSortedItem(FList[Index]);
end;

function TSortedList.PriorAt(Pos: integer): integer;

  function CompProc(ItemIndex, Key: integer): integer; inline;
  begin
    Result := TSortedItem(FList[ItemIndex]).GetKey - Key;
  end;

  function QuickSearch(Key: integer; var Index: integer): Boolean; inline;
  var
    L, H, I, C, NCount: Integer;
  begin
    Result := False;
    NCount := FList.Count;
    if NCount = 0 then
    begin
      Index := -1;
      Exit;
    end;

    L := 0;
    H := NCount - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := CompProc(I, Key);
      if C < 0 then L := I + 1 else
      begin
        if C = 0 then
        begin
          Result := True;
          Index := I;
          Exit;
        end;
        H := I - 1;
      end;
    end;
    Index := L;
    if Index >= NCount then
      Index := NCount - 1;
    if Index >= 0 then
      if CompProc(Index, Key) > 0 then
        dec(Index);
  end;
begin
  QuickSearch(Pos, Result);
end;

end.
