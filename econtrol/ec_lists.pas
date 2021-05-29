{ *************************************************************************** }
{                                                                             }
{ EControl Common Library                                                     }
{                                                                             }
{ Copyright (c) 2004 - 2015 EControl Ltd., Zaharov Michael                    }
{     www.econtrol.ru                                                         }
{     support@econtrol.ru                                                     }
{                                                                             }
{ Changes in Lazarus port: by Alexey Torgashin (CudaText)                     }
{                                                                             }
{ *************************************************************************** }

{$mode delphi}

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

  { TSortedList }

  TSortedList = class
  private
    FList: TFPList;
    FOwnsObjects: boolean;
    function GetCount: integer;
    function GetItem(Index: integer): TSortedItem;
  public
    constructor Create(AOwnObjects: Boolean);
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
    StartPos, EndPos: integer;
    PointStart, PointEnd: TPoint;
    constructor Create(AStartPos, AEndPos: integer);
    constructor Create(AStartPos, AEndPos: integer; const APointStart, APointEnd: TPoint);
    class operator=(const a, b: TRange): boolean;
  end;

  { GRangeList }

  // Array of sorted ranges
  GRangeList<GRange> = class (TFPGList<GRange>)
  private
    type PGRange = ^GRange;
  protected
    function CompProc(const AValue: TRange; AKey: integer): integer;
    function CompLines(AItemIndex, ALine: integer): integer;
  public
    constructor Create(UnionSiblings: Boolean = True);
    //destructor Destroy; override;
    function IsIndexValid(AIndex: integer): boolean;
    //property Sorted: boolean read FSorted write FSorted;
    function Add(const Range: GRange): integer; virtual;
    procedure ClearFromIndex(AIndex: integer);
    procedure ClearFromLine(ALine: integer);
    function InternalGet(AIndex: integer): PGRange; inline;
    // At position or next
    function NextAt(APos: integer): integer;
    // At position or prior
    function PriorAt(APos: integer): integer;
    function PriorAtLine(ALine: integer): integer;
    // At position exactly, -1 if pos between tokens
    function FindAt(APos: integer): integer;
  end;

  TecRangeList = GRangeList<TRange>;

  { TecStateChange }

  TecStateChange = record
    TokenCount: integer;
    State: integer;
    class operator=(const a, b: TecStateChange): boolean;
  end;

  TecStateChanges = TFPGList<TecStateChange>;

implementation

uses
  SysUtils, Contnrs;

{ TecStateChange }

class operator TecStateChange.=(const a, b: TecStateChange): boolean;
begin
  Result := False;
end;

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
  //FUnionSiblings := UnionSiblings;
  //FSorted := false;
  Capacity := 512; // Usually editor has many tokens
end;

function GRangeList<GRange>.IsIndexValid(AIndex: integer): boolean; inline;
begin
  Result := (AIndex >= 0) and (AIndex < Count);
end;

function GRangeList<GRange>.Add(const Range: GRange): integer;
begin
  Result := Count;
  inherited Add(Range);
end;

(*
function GRangeList<GRange>.Add2(const Range: GRange): integer;
var
  _Range: TRange absolute Range;
begin
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
end;
*)

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

function GRangeList<GRange>.CompLines(AItemIndex, ALine: integer): integer;
var
  Ptr: pointer;
  Y1, Y2: integer;
begin
  Ptr := InternalItems[AItemIndex];
  // support multi-line comments
  Y1 := TRange(Ptr^).PointStart.Y;
  Y2 := TRange(Ptr^).PointEnd.Y;
  if Y1 = Y2 then
    Result := Y1 - ALine
  else
  if (Y1 <= ALine) and (Y2 >= ALine) then
    Result := 0
  else
    Result := Y1 - ALine
end;


function GRangeList<GRange>.PriorAt(APos: integer): integer;
var
  H, I, Diff, NCount: Integer;
begin
  NCount := Count;
  if NCount = 0 then
    Exit(-1);

  Result := 0;
  H := NCount - 1;
  while Result <= H do
  begin
    I := (Result + H) shr 1;
    Diff := CompProc(TRange(InternalItems[i]^), APos);
    if Diff < 0 then
      Result := I + 1
    else
    begin
      if Diff = 0 then
        Exit(I);
      H := I - 1;
    end;
  end;

  if Result >= NCount then
    Result := NCount - 1
  else
  if Result > 0 then
    if CompProc(TRange(InternalItems[i]^), APos) > 0 then
      Dec(Result);
end;

function GRangeList<GRange>.PriorAtLine(ALine: integer): integer;
var
  H, I, Diff, NCount: Integer;
begin
  NCount := Count;
  if NCount = 0 then
    Exit(-1);

  Result := 0;
  H := NCount - 1;
  while Result <= H do
  begin
    I := (Result + H) shr 1;
    Diff := CompLines(I, ALine);
    if Diff < 0 then
      Result := I + 1
    else
    begin
      if Diff = 0 then
      begin
        // found some token with needed line, let's move up
        while (I > 0) and (CompLines(I - 1, ALine) = 0) do
          Dec(I);
        Exit(I);
      end;
      H := I - 1;
    end;
  end;

  if Result >= NCount then
    Result := NCount - 1
  else
  if Result > 0 then
    if CompLines(I, ALine) > 0 then
      Dec(Result);
end;

function GRangeList<GRange>.FindAt(APos: integer): integer;
var
  L, H, I, Diff, NCount: Integer;
begin
  Result := -1;
  NCount := Count;
  if NCount = 0 then
    Exit;

  L := 0;
  H := NCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    Diff := CompProc(TRange(InternalItems[i]^), APos);
    if Diff < 0 then
      L := I + 1
    else
    begin
      if Diff = 0 then
        Exit(I);
      H := I - 1;
    end;
  end;
end;

procedure GRangeList<GRange>.ClearFromIndex(AIndex: integer); inline;
begin
  if AIndex >= 0 then
    DeleteRange(AIndex, Count-1);
end;


procedure GRangeList<GRange>.ClearFromLine(ALine: integer);
var
  idx: integer;
begin
  if ALine <= 0 then
    Clear
  else
  begin
    idx := PriorAtLine(ALine);
    if idx <= 0 then
      Clear
    else
      ClearFromIndex(idx);
  end;
end;

function GRangeList<GRange>.InternalGet(AIndex: integer): PGRange;
begin
  Result := inherited;
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
var
  i: integer;
begin
  if FOwnsObjects then
    for i := Count-1 downto 0 do
      TObject(FList[i]).Free;
  FList.Clear;
end;

constructor TSortedList.Create(AOwnObjects: Boolean);
{ old EControl did this:
  if AOwnObjects then
    FList := TObjectList.Create
  else
    FList := TList.Create;
}
begin
  inherited Create;
  FOwnsObjects := AOwnObjects;
  FList := TFPList.Create;
end;

procedure TSortedList.Delete(Index: integer);
begin
  if FOwnsObjects then
    TObject(FList[Index]).Free;
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
