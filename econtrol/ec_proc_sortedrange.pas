{
Copyright (C) Alexey Torgashin, uvviewsoft.com
License: MPL 2.0 or LGPL
}
unit ec_proc_SortedRange;

{$mode objfpc}{$H+}
{$ModeSwitch advancedrecords}
{$ScopedEnums on}

interface

uses
  Classes, SysUtils, Graphics, Math,
  ATStrings,
  ATStringProc,
  ATSynEdit,
  ATSynEdit_Globals,
  ATSynEdit_Carets,
  ATSynEdit_FGL,
  ec_SyntAnal,
  ec_syntax_format;

type
  TATRangeCond = (Inside, AtBound, Outside);

type
  { TATIntegerWithPointer }

  TATIntegerWithPointer = record
    Value: integer;
    Ptr: pointer;
    class operator =(const a, b: TATIntegerWithPointer): boolean;
  end;

  { TATIntegersWithPointers }

  TATIntegersWithPointers = class(specialize TFPGList<TATIntegerWithPointer>)
  public
    function FindByInteger(AValue: integer): pointer;
    procedure SortByInteger;
  end;

function ComparePoints(const P1, P2: TPoint): integer; inline;

type
  { TATSortedRange }

  PATSortedRange = ^TATSortedRange;
  TATSortedRange = record
    Pos1, Pos2: TPoint;
    Pos1Wide, Pos2Wide: TPoint;
    Token1, Token2: integer;
    Color: TColor;
    Rule: TecTagBlockCondition;
    ActiveAlways: boolean;
    Active: array[0..Pred(TATEditorOptions.MaxStringsClients)] of boolean;
    class operator =(const a, b: TATSortedRange): boolean;
    procedure Init(
      const APos1, APos2: TPoint;
      const APos1Wide, APos2Wide: TPoint;
      AToken1, AToken2: integer;
      AColor: TColor; ARule: TecTagBlockCondition;
      AActiveAlways: boolean);
    function IsPosInside(const APos: TPoint): boolean;
    function IsPosInsideWide(const APos: TPoint): boolean;
  end;

  { TATSortedRanges }

  TATSortedRanges = class(specialize TFPGList<TATSortedRange>)
  private
    FBoundTokensIndexer: TATIntegersWithPointers;
    FLineIndexer: array of array of integer;
  public
    function ItemPtr(AIndex: integer): PATSortedRange; inline;
    function Find(const APos: TPoint; AEditorIndex: integer; AOnlyActive: boolean): integer;
    function FindByLineIndexer(const APos: TPoint; AEditorIndex: integer; AOnlyActive: boolean): integer;
    function FindStyleByTokenIndex(ATokenIndex, AEditorIndex: integer): TecSyntaxFormat;
    procedure UpdateOnChange(AChange: TATLineChangeKind; ALine, AItemCount: integer);
    function CheckCaretInRange(Ed: TATSynEdit; const APos1, APos2: TPoint;
      ACond: TATRangeCond): boolean;
    procedure UpdateRangesActive(Ed: TATSynEdit);
    procedure UpdateBoundIndexer;
    procedure UpdateLineIndexer(ALineCount: integer);
    procedure DeactivateNotMinimalRanges(Ed: TATSynEdit);
    destructor Destroy; override;
    procedure Clear;
    procedure ClearBoundIndexer;
    procedure ClearLineIndexer;
    function DebugLineIndexer: string;
  end;

implementation

function ComparePoints(const P1, P2: TPoint): integer; inline;
begin
  if (P1.X=P2.X) and (P1.Y=P2.Y) then exit(0);
  if (P1.Y>P2.Y) then exit(1);
  if (P1.Y<P2.Y) then exit(-1);
  if (P1.X>P2.X) then exit(1) else exit(-1);
end;

{ TATIntegerWithPointer }

class operator TATIntegerWithPointer.=(const a, b: TATIntegerWithPointer): boolean;
begin
  Result:= false;
end;

{ TATIntegersWithPointers }

function TATIntegersWithPointers.FindByInteger(AValue: integer): pointer;
var
  a, b, m, dif, NCount: integer;
begin
  Result:= nil;
  NCount:= Count;
  if NCount=0 then
    Exit;

  a:= 0;
  b:= NCount-1;
  while a<=b do
  begin
    m:= (a+b) div 2;
    dif:= _GetItemPtr(m)^.Value - AValue;
    if dif<0 then
      a:= m+1
    else
    if dif=0 then
      Exit(_GetItemPtr(m)^.Ptr)
    else
      b:= m-1;
  end;
end;

function Compare_IntegerWithPointer(const a, b: TATIntegerWithPointer): integer;
begin
  Result:= a.Value - b.Value;
end;

procedure TATIntegersWithPointers.SortByInteger;
begin
  Sort(@Compare_IntegerWithPointer);
end;

{ TATSortedRange }

class operator TATSortedRange.=(const a, b: TATSortedRange): boolean;
begin
  Result:= false;
end;

procedure TATSortedRange.Init(const APos1, APos2: TPoint; const APos1Wide, APos2Wide: TPoint; AToken1,
  AToken2: integer; AColor: TColor; ARule: TecTagBlockCondition; AActiveAlways: boolean);
var
  i: integer;
begin
  Pos1:= APos1;
  Pos2:= APos2;
  Pos1Wide:= APos1Wide;
  Pos2Wide:= APos2Wide;
  Token1:= AToken1;
  Token2:= AToken2;
  Color:= AColor;
  Rule:= ARule;
  ActiveAlways:= AActiveAlways;
  for i:= Low(Active) to High(Active) do
    Active[i]:= false;
end;

function TATSortedRange.IsPosInside(const APos: TPoint): boolean;
begin
  Result:= IsPosInRange(
    APos.X, APos.Y,
    Pos1.X, Pos1.Y,
    Pos2.X, Pos2.Y
    ) = TATPosRelation.Inside;
end;

function TATSortedRange.IsPosInsideWide(const APos: TPoint): boolean;
begin
  Result:= IsPosInRange(
    APos.X, APos.Y,
    Pos1Wide.X, Pos1Wide.Y,
    Pos2Wide.X, Pos2Wide.Y
    ) = TATPosRelation.Inside;
end;

{ TATSortedRanges }

function TATSortedRanges.Find(const APos: TPoint; AEditorIndex: integer; AOnlyActive: boolean): integer;

  function CompProc(ItemIndex: integer): integer; inline;
  var
    Item: PATSortedRange;
    bOk: boolean;
  begin
    Item:= ItemPtr(ItemIndex);

    if AOnlyActive then
      bOk:= Item^.ActiveAlways or Item^.Active[AEditorIndex]
    else
      bOk:= true;

    if bOk and Item^.IsPosInside(APos) then
      Result:= 0
    else
      Result:= ComparePoints(Item^.Pos1, APos);
  end;

var
  L, H, I, C, NCount: Integer;
  bOk: boolean;
  Item: PATSortedRange;
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
    C := CompProc(I);
    if C < 0 then
      L := I + 1
    else
    if C = 0 then
      Exit(I)
    else
      H := I - 1;
  end;

  Result := L;
  if Result >= NCount then
    Result := NCount - 1;
  if Result >= 0 then
    if CompProc(Result) > 0 then
      Dec(Result);

  if AOnlyActive then
    if Result>=0 then
    begin
      Item:= ItemPtr(Result);
      bOk:= Item^.ActiveAlways or Item^.Active[AEditorIndex];
      if not bOk then
        Result:= -1;
    end;
end;

function TATSortedRanges.FindByLineIndexer(const APos: TPoint; AEditorIndex: integer; AOnlyActive: boolean): integer;
var
  Rng: PATSortedRange;
  NLine, iItem, iRange: integer;
begin
  Result:= -1;

  NLine:= APos.Y;
  if NLine>High(FLineIndexer) then exit;

  //test all ranges listed in FLineIndexer[NLine]
  for iItem:= High(FLineIndexer[NLine]) downto 0 do
  begin
    iRange:= FLineIndexer[NLine][iItem];
    Rng:= ItemPtr(iRange);
    if (not AOnlyActive) or (Rng^.ActiveAlways or Rng^.Active[AEditorIndex]) then
      if Rng^.IsPosInsideWide(APos) then
        exit(iRange);
  end;
end;

function TATSortedRanges.FindStyleByTokenIndex(ATokenIndex, AEditorIndex: integer): TecSyntaxFormat;
var
  Rng: PATSortedRange;
begin
  Result:= nil;
  if FBoundTokensIndexer=nil then
    exit;
  Rng:= FBoundTokensIndexer.FindByInteger(ATokenIndex);
  if Assigned(Rng) then
    if Rng^.Active[AEditorIndex] and Assigned(Rng^.Rule) then
      Result:= Rng^.Rule.Style;
end;

function TATSortedRanges.ItemPtr(AIndex: integer): PATSortedRange;
begin
  Result:= PATSortedRange(InternalGet(AIndex));
end;

procedure TATSortedRanges.UpdateOnChange(AChange: TATLineChangeKind; ALine, AItemCount: integer);
var
  Ptr: PATSortedRange;
  i: integer;
begin
  case AChange of
    TATLineChangeKind.DeletedAll:
      Clear;

    TATLineChangeKind.Added:
      begin
        for i:= Count-1 downto 0 do
        begin
          Ptr:= InternalGet(i);
          if Ptr^.Pos1.Y>=ALine then
          begin
            Ptr^.Pos1.Y+= AItemCount;
            Ptr^.Pos2.Y+= AItemCount;
          end
          else
          if Ptr^.Pos2.Y>=ALine then
            Ptr^.Pos2.Y+= AItemCount;
        end;
      end;

    TATLineChangeKind.Deleted:
      begin
        for i:= Count-1 downto 0 do
        begin
          Ptr:= InternalGet(i);
          if Ptr^.Pos1.Y>=ALine+AItemCount then
          begin
            Ptr^.Pos1.Y-= AItemCount;
            Ptr^.Pos2.Y-= AItemCount;
          end
          else
          if Ptr^.Pos1.Y>=ALine then
          begin
            if Ptr^.Pos2.Y<=ALine+AItemCount then
              Delete(i)
            else
            begin
              Ptr^.Pos1.Y:= Max(ALine, Ptr^.Pos1.Y-AItemCount);
              Ptr^.Pos2.Y-= AItemCount;
            end;
          end
          else
          if Ptr^.Pos2.Y>=ALine then
          begin
            Ptr^.Pos2.Y:= Max(ALine, Ptr^.Pos2.Y-AItemCount);
          end;
        end;
      end;
  end;
end;


function TATSortedRanges.CheckCaretInRange(Ed: TATSynEdit;
  const APos1, APos2: TPoint;
  ACond: TATRangeCond): boolean;
var
  Caret: TATCaretItem;
  Pnt: TPoint;
  dif1, dif2: integer;
  i: integer;
  ok: boolean;
begin
  Result:= false;

  for i:= 0 to Ed.Carets.Count-1 do
  begin
    Caret:= Ed.Carets[i];
    Pnt.X:= Caret.PosX;
    Pnt.Y:= Caret.PosY;

    dif1:= ComparePoints(Pnt, APos1);
    dif2:= ComparePoints(Pnt, APos2);

    case ACond of
      TATRangeCond.Inside:
        ok:= (dif1>=0) and (dif2<0);
      TATRangeCond.Outside:
        ok:= (dif1<0) or (dif2>=0);
      TATRangeCond.AtBound:
        ok:= (dif1=0) or (dif2=0);
      else
        ok:= false;
    end;

    if ok then exit(true);
  end;
end;

procedure TATSortedRanges.UpdateRangesActive(Ed: TATSynEdit);
var
  Rng: PATSortedRange;
  act: boolean;
  i: integer;
begin
  for i:= 0 to Count-1 do
  begin
    Rng:= ItemPtr(i);
    if Rng^.ActiveAlways then
      act:= true
    else
    begin
      if Rng^.Rule=nil then Continue;
      if not (Rng^.Rule.DynHighlight in [dhRange, dhRangeNoBound, dhBound]) then Continue;
      case Rng^.Rule.HighlightPos of
        cpAny:
          act:= true;
        cpBound:
          act:= CheckCaretInRange(Ed, Rng^.Pos1, Rng^.Pos2, TATRangeCond.AtBound);
        cpBoundTag:
          act:= false;//todo
        cpRange:
          act:= CheckCaretInRange(Ed, Rng^.Pos1, Rng^.Pos2, TATRangeCond.Inside);
        cpBoundTagBegin:
          act:= false;//todo
        cpOutOfRange:
          act:= CheckCaretInRange(Ed, Rng^.Pos1, Rng^.Pos2, TATRangeCond.Outside);
        else
          act:= false;
      end;
    end;
    Rng^.Active[Ed.EditorIndex]:= act;
  end;
end;

procedure TATSortedRanges.UpdateBoundIndexer;
var
  Pair: TATIntegerWithPointer;
  Rng: PATSortedRange;
  i: integer;
begin
  if FBoundTokensIndexer=nil then
    FBoundTokensIndexer:= TATIntegersWithPointers.Create;
  FBoundTokensIndexer.Clear;
  for i:= 0 to Count-1 do
  begin
    Rng:= ItemPtr(i);
    Pair.Value:= Rng^.Token1;
    Pair.Ptr:= Rng;
    FBoundTokensIndexer.Add(Pair);
    Pair.Value:= Rng^.Token2;
    FBoundTokensIndexer.Add(Pair);
  end;
  FBoundTokensIndexer.SortByInteger;
end;

procedure TATSortedRanges.UpdateLineIndexer(ALineCount: integer);
var
  NCount, NItemLen: integer;
  iRange, iLine: integer;
  Ptr: PATSortedRange;
begin
  for iLine:= High(FLineIndexer) downto 0 do
    SetLength(FLineIndexer[iLine], 0);

  SetLength(FLineIndexer, ALineCount);
  if ALineCount=0 then exit;

  NCount:= Count;
  if NCount=0 then exit;

  for iRange:= 0 to NCount-1 do
  begin
    Ptr:= ItemPtr(iRange);
    //range check for iLine vs FLineIndexer[]
    for iLine:= Ptr^.Pos1.Y to Min(Ptr^.Pos2.Y, High(FLineIndexer)) do
    begin
      NItemLen:= Length(FLineIndexer[iLine]);
      SetLength(FLineIndexer[iLine], NItemLen+1);
      FLineIndexer[iLine][NItemLen]:= iRange;
    end;
  end;
end;

function TATSortedRanges.DebugLineIndexer: string;
var
  S: string;
  i, iLine: integer;
begin
  Result:= '';
  for iLine:= 0 to Min(High(FLineIndexer), 30) do
  begin
    S:= IntToStr(iLine)+': ';
    for i:= 0 to High(FLineIndexer[iLine]) do
      S+= IntToStr(FLineIndexer[iLine][i])+' ';
    Result+= S+#10;
  end;
end;

procedure TATSortedRanges.DeactivateNotMinimalRanges(Ed: TATSynEdit);
var
  Rng, RngOut: PATSortedRange;
  i, j: integer;
begin
  for i:= Count-1 downto 0 do
  begin
    Rng:= ItemPtr(i);
    if not Rng^.Active[Ed.EditorIndex] then Continue;
    if Rng^.Rule=nil then Continue;
    if not Rng^.Rule.DynSelectMin then Continue;
    if not (Rng^.Rule.DynHighlight in [dhBound, dhRange, dhRangeNoBound]) then Continue;
    //take prev ranges which contain this range
    for j:= i-1 downto 0 do
    begin
      RngOut:= ItemPtr(j);
      if RngOut^.Rule=Rng^.Rule then
        if RngOut^.Active[Ed.EditorIndex] then
          if (ComparePoints(RngOut^.Pos1, Rng^.Pos1)<=0) and
             (ComparePoints(RngOut^.Pos2, Rng^.Pos2)>=0) then
            RngOut^.Active[Ed.EditorIndex]:= false;
    end;
  end;
end;

destructor TATSortedRanges.Destroy;
begin
  Clear;
  if Assigned(FBoundTokensIndexer) then
    FreeAndNil(FBoundTokensIndexer);
  inherited;
end;

procedure TATSortedRanges.Clear;
begin
  ClearBoundIndexer;
  ClearLineIndexer;
  inherited Clear;
end;

procedure TATSortedRanges.ClearBoundIndexer;
begin
  if Assigned(FBoundTokensIndexer) then
    FBoundTokensIndexer.Clear;
end;

procedure TATSortedRanges.ClearLineIndexer;
var
  iLine: integer;
begin
  for iLine:= High(FLineIndexer) downto 0 do
    FLineIndexer[iLine] := nil;
  FLineIndexer := nil;
end;

end.
