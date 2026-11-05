{ ******************************************************* }
{                                                         }
{ Copyright (c) 2026 Alexey Torgashin (CudaText)          }
{ License: MPL 2.0 / LGPL like in Lazarus                 }
{                                                         }
{ ******************************************************* }

{$mode delphi}

unit ec_Lists_UniStr;

interface

uses
  SysUtils,
  Classes;

type
  { TecUnicodeStringList }

  // Sorted string list which stores items as UnicodeString.
  // Created for TecSingleTagCondition.FTagList (optimization step A, 2025):
  // TStringList stores AnsiString, so the parser had to allocate a token copy
  // and convert it with UTF8Encode() on every tag lookup. This list stores
  // UnicodeString and can search a raw PWideChar buffer (FindBuffer) with
  // zero allocations.
  // - The class inherits TStrings, so published "TagList: TStrings" properties
  //   stream the same 'TagList.Strings = (...)' pseudo-property as before
  //   (TStrings.DefineProperties -> ReadData -> Add): old .lcf lexer files
  //   load unchanged.
  // - AnsiString values passed through the TStrings API are converted by
  //   ListStrToUnicode() (see the BFix1 notes below the class): plain UTF-8
  //   is the normal case, stray non-UTF-8 bytes get a codepage/Latin-1
  //   interpretation instead of being mangled to '?'.
  // - Comparison is code-unit wise (like the old UseLocale=False TStringList
  //   with UTF-8 bytes); IgnoreCase folding is ASCII-only, also like the old
  //   non-locale TStringList.

  TecUnicodeStringList = class(TStrings)
  private
    FItems: array of UnicodeString;
    FCount: integer;
    FSorted: boolean;
    FCaseSensitive: boolean;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FUpdateLock: integer;
    procedure SetSorted(AValue: boolean);
    procedure SetCaseSensitive(AValue: boolean);
    procedure Grow;
    procedure QuickSort(L, R: integer);
    procedure NotifyChanged;
    function CompareBuffers(A: PWideChar; ALen: integer;
      B: PWideChar; BLen: integer): integer;
    function CompareItemBuffer(Index: integer;
      ABuffer: PWideChar; ALen: integer): integer;
    procedure InsertItem(Index: integer; const S: UnicodeString);
    procedure InsertUnicodeItem(Index: integer; const S: UnicodeString);
  protected
    function Get(Index: integer): string; override;
    function GetCount: integer; override;
    procedure Put(Index: integer; const S: string); override;
    function GetObject(Index: integer): TObject; override;
    procedure PutObject(Index: integer; AObject: TObject); override;
    procedure Insert(Index: integer; const S: string); override;
    procedure Delete(Index: integer); override;
    procedure Clear; override;
    procedure SetUpdateState(Updating: boolean); override;
  public
    function AddObject(const S: string; AObject: TObject): integer; override;
    function AddUnicode(const S: UnicodeString): integer;
    procedure Assign(Source: TPersistent); override;
    function Find(const S: UnicodeString; var Index: integer): boolean;
    // Binary search for a raw buffer: no string allocation, no conversion.
    // ABuffer is only read up to ALen widechars (ALen=0 never dereferences it).
    function FindBuffer(ABuffer: PWideChar; ALen: integer;
      var Index: integer): boolean;
    function IndexOfUnicode(const S: UnicodeString): integer;
    function GetUnicode(Index: integer): UnicodeString;

    property Sorted: boolean read FSorted write SetSorted;
    property CaseSensitive: boolean read FCaseSensitive write SetCaseSensitive;
    property Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

{ TecUnicodeStringList }

// BFix1: how non-ASCII reaches this list from '.lcf' lexer files.
//
// .lcf files never store non-ASCII as raw UTF-8 in '#NNNN' escapes: they
// store it as DECIMAL CHARACTER CODES ('#1050#1086#1085#1077#1094' =
// 'Конец', see e.g. the 1C lexer). FPC's object-text reader (ObjectTextToBinary
// + TReader) decodes them like this (verified with a probe program on
// FPC 3.2.2, identical results for DefaultSystemCodePage 0 / 65001 / 1251 /
// 1252 in a runtime override):
//
//   property type        '#NNNN' <= 255        '#NNNN' > 255
//   ----------------     ------------------    ------------------------------
//   UnicodeString        correct UTF-16 code   correct UTF-16 code
//   (Expression etc.)
//   string / TStrings    raw byte NNNN         '?' (0x3F), lost in the
//   (TagList.Strings)    (no conversion)       reader before any app code
//
// Raw non-ASCII text inside quotes is byte-transparent for string/TStrings
// properties (that is how FPC's own saved values round-trip), but gets
// byte-split into wrong UTF-16 for UnicodeString properties - which is why
// lexer files use '#NNNN' escapes for Expression in the first place.
//
// Consequences for this list (the TagList path):
// - ASCII keywords: unaffected, UTF8Decode is an identity.
// - everything CudaText writes back (raw UTF-8 in quotes, or '#208#138'
//   byte escapes of UTF-8): valid UTF-8, UTF8Decode is correct.
// - '#NNNN' 128..255 escapes: arrive as single raw bytes; a bare
//   UTF8Decode() would mangle them to '?' (the old TStringList+UTF8Encode
//   comparison never matched them either). ListStrToUnicode gives them a
//   codepage/Latin-1 interpretation, so they keep their character values.
// - '#NNNN' > 255 escapes: already lost ('?') inside the FPC reader, before
//   this unit is called; nothing can restore them downstream. No lexer in
//   the CudaText-lexers collection (371 files checked) uses non-ASCII in
//   TagList.Strings at all, so this is a theoretical case.
//
// UTF8Decode/UTF8Encode (FPC System unit) are used for the UTF-8 tiers
// (AFix1: they replaced the initial built-in decoder; UTF8Decode reads the
// raw bytes as UTF-8 regardless of the input string's codepage annotation).
// UTF8Encode returns a CP_UTF8-annotated string; in-tree consumers treat
// strings as raw bytes (like the pre-A1 code that passed UTF8Encode results
// straight to TStringList.Find), and TStrings.GetTextStr assembles Text with
// Move(), so the observable bytes are exactly the stored UTF-8.

function IsValidUtf8(const S: RawByteString): boolean;
// strict validation (load-time only, not a hot path)
var
  i, n, Len: integer;
  b: byte;
begin
  Result := False;
  Len := Length(S);
  i := 1;
  while i <= Len do
  begin
    b := byte(S[i]);
    if b < $80 then
    begin
      Inc(i);
      Continue;
    end;
    if (b and $E0) = $C0 then
      n := 1
    else
    if (b and $F0) = $E0 then
      n := 2
    else
    if (b and $F8) = $F0 then
      n := 3
    else
      // stray continuation byte or invalid lead byte
      Exit;
    if i + n > Len then
      Exit;
    while n > 0 do
    begin
      Inc(i);
      if (byte(S[i]) and $C0) <> $80 then
        Exit;
      Dec(n);
    end;
    Inc(i);
  end;
  Result := True;
end;

function ListStrToUnicode(const S: string): UnicodeString;
var
  R: RawByteString;
  i: integer;
begin
  if S = '' then
    Exit('');
  if IsValidUtf8(S) then
    Exit(UTF8Decode(S));
  SetLength(Result, Length(S));
  for i := 1 to Length(S) do
    Result[i] := WideChar(byte(S[i]));
end;

procedure TecUnicodeStringList.NotifyChanged;
begin
  if (FUpdateLock = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TecUnicodeStringList.SetUpdateState(Updating: boolean);
begin
  if Updating then
    Inc(FUpdateLock)
  else
  if FUpdateLock > 0 then
  begin
    Dec(FUpdateLock);
    if FUpdateLock = 0 then
      NotifyChanged;
  end;
end;

function TecUnicodeStringList.GetCount: integer;
begin
  Result := FCount;
end;

procedure TecUnicodeStringList.Grow;
var
  NewCap: integer;
begin
  NewCap := Length(FItems) * 2;
  if NewCap < 16 then
    NewCap := 16;
  SetLength(FItems, NewCap);
end;

function TecUnicodeStringList.GetUnicode(Index: integer): UnicodeString;
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.Create('TecUnicodeStringList: index out of bounds');
  Result := FItems[Index];
end;

function TecUnicodeStringList.Get(Index: integer): string;
begin
  Result := UTF8Encode(GetUnicode(Index));
end;

procedure TecUnicodeStringList.Put(Index: integer; const S: string);
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.Create('TecUnicodeStringList: index out of bounds');
  FItems[Index] := ListStrToUnicode(S);
  NotifyChanged;
end;

function TecUnicodeStringList.GetObject(Index: integer): TObject;
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.Create('TecUnicodeStringList: index out of bounds');
  Result := nil;
end;

procedure TecUnicodeStringList.PutObject(Index: integer; AObject: TObject);
begin
  // objects are not stored
end;

procedure TecUnicodeStringList.InsertItem(Index: integer;
  const S: UnicodeString);
var
  i: integer;
begin
  if FCount = Length(FItems) then
    Grow;
  for i := FCount downto Index + 1 do
    FItems[i] := FItems[i - 1];
  FItems[Index] := S;
  Inc(FCount);
end;

procedure TecUnicodeStringList.Insert(Index: integer; const S: string);
begin
  InsertUnicodeItem(Index, ListStrToUnicode(S));
end;

procedure TecUnicodeStringList.Delete(Index: integer);
var
  i: integer;
begin
  if (Index < 0) or (Index >= FCount) then
    raise EListError.Create('TecUnicodeStringList: index out of bounds');
  FItems[Index] := '';
  for i := Index to FCount - 2 do
    FItems[i] := FItems[i + 1];
  FItems[FCount - 1] := '';
  Dec(FCount);
  NotifyChanged;
end;

procedure TecUnicodeStringList.Clear;
var
  i: integer;
begin
  if FCount > 0 then
  begin
    for i := 0 to FCount - 1 do
      FItems[i] := '';
    FCount := 0;
    NotifyChanged;
  end;
end;

function TecUnicodeStringList.AddObject(const S: string;
  AObject: TObject): integer;
begin
  Result := AddUnicode(ListStrToUnicode(S));
end;

procedure TecUnicodeStringList.InsertUnicodeItem(Index: integer;
  const S: UnicodeString);
var
  N: integer;
begin
  if FSorted then
  begin
    if Find(S, N) then
      case FDuplicates of
        dupIgnore:
          Exit;
        dupError:
          raise EListError.Create('TecUnicodeStringList: duplicates are not allowed');
      end;
    Index := N;
  end;
  if (Index < 0) or (Index > FCount) then
    raise EListError.Create('TecUnicodeStringList: index out of bounds');
  InsertItem(Index, S);
  NotifyChanged;
end;

function TecUnicodeStringList.AddUnicode(const S: UnicodeString): integer;
var
  N: integer;
begin
  if FSorted then
  begin
    if Find(S, N) then
      case FDuplicates of
        dupIgnore:
          Exit(N);
        dupError:
          raise EListError.Create('TecUnicodeStringList: duplicates are not allowed');
      end;
  end
  else
    N := FCount;
  InsertItem(N, S);
  NotifyChanged;
  Result := N;
end;

function TecUnicodeStringList.CompareBuffers(A: PWideChar; ALen: integer;
  B: PWideChar; BLen: integer): integer;
var
  n: integer;
  c1, c2: WideChar;
begin
  if ALen <= BLen then
    n := ALen
  else
    n := BLen;
  // while-loop instead of for (AFix2): the counter lives in a register,
  // no separate induction variable
  while n > 0 do
  begin
    c1 := A^;
    c2 := B^;
    Inc(A);
    Inc(B);
    Dec(n);
    if c1 <> c2 then
    begin
      if not FCaseSensitive then
      begin
        if (c1 >= 'a') and (c1 <= 'z') then
          c1 := WideChar(word(c1) - 32);
        if (c2 >= 'a') and (c2 <= 'z') then
          c2 := WideChar(word(c2) - 32);
        if c1 = c2 then
          Continue;
      end;
      if word(c1) < word(c2) then
        Result := -1
      else
        Result := 1;
      Exit;
    end;
  end;
  Result := ALen - BLen;
end;

function TecUnicodeStringList.CompareItemBuffer(Index: integer;
  ABuffer: PWideChar; ALen: integer): integer;
var
  S: UnicodeString;
begin
  S := FItems[Index];
  Result := CompareBuffers(PWideChar(S), Length(S), ABuffer, ALen);
end;

function TecUnicodeStringList.FindBuffer(ABuffer: PWideChar; ALen: integer;
  var Index: integer): boolean;
var
  L, R, M, C: integer;
begin
  Result := False;
  if (ABuffer = nil) and (ALen > 0) then
  begin
    Index := 0;
    Exit;
  end;
  L := 0;
  R := FCount - 1;
  while L <= R do
  begin
    M := (L + R) shr 1;
    C := CompareItemBuffer(M, ABuffer, ALen);
    if C < 0 then
      L := M + 1
    else
    begin
      if C = 0 then
        Result := True;
      R := M - 1;
    end;
  end;
  Index := L;
end;

function TecUnicodeStringList.Find(const S: UnicodeString;
  var Index: integer): boolean;
begin
  Result := FindBuffer(PWideChar(S), Length(S), Index);
end;

function TecUnicodeStringList.IndexOfUnicode(const S: UnicodeString): integer;
begin
  if not Find(S, Result) then
    Result := -1;
end;

procedure TecUnicodeStringList.QuickSort(L, R: integer);
var
  i, j: integer;
  Pivot, Tmp: UnicodeString;
begin
  i := L;
  j := R;
  Pivot := FItems[(L + R) div 2];
  repeat
    while CompareItemBuffer(i, PWideChar(Pivot), Length(Pivot)) < 0 do
      Inc(i);
    while CompareItemBuffer(j, PWideChar(Pivot), Length(Pivot)) > 0 do
      Dec(j);
    if i <= j then
    begin
      Tmp := FItems[i];
      FItems[i] := FItems[j];
      FItems[j] := Tmp;
      Inc(i);
      Dec(j);
    end;
  until i > j;
  if L < j then
    QuickSort(L, j);
  if i < R then
    QuickSort(i, R);
end;

procedure TecUnicodeStringList.SetSorted(AValue: boolean);
begin
  if FSorted = AValue then
    Exit;
  FSorted := AValue;
  if FSorted then
  begin
    if FCount > 1 then
      QuickSort(0, FCount - 1);
    NotifyChanged;
  end;
end;

procedure TecUnicodeStringList.SetCaseSensitive(AValue: boolean);
begin
  if FCaseSensitive = AValue then
    Exit;
  FCaseSensitive := AValue;
  if FSorted and (FCount > 1) then
    QuickSort(0, FCount - 1);
  NotifyChanged;
end;

procedure TecUnicodeStringList.Assign(Source: TPersistent);
var
  i: integer;
  Src: TecUnicodeStringList;
begin
  if Source is TecUnicodeStringList then
  begin
    Src := TecUnicodeStringList(Source);
    BeginUpdate;
    try
      Clear;
      if Src.FCount > 0 then
      begin
        if Length(FItems) < Src.FCount then
          SetLength(FItems, Src.FCount);
        for i := 0 to Src.FCount - 1 do
          FItems[i] := Src.FItems[i];
        FCount := Src.FCount;
        if FSorted and (FCount > 1) then
          QuickSort(0, FCount - 1);
      end;
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

end.
