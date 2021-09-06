{ *************************************************************************** }
{                                                                             }
{ EControl Syntax Editor SDK                                                  }
{                                                                             }
{ Copyright (c) 2004 - 2015 EControl Ltd., Zaharov Michael                    }
{     www.econtrol.ru                                                         }
{     support@econtrol.ru                                                     }
{                                                                             }
{ Changes in Lazarus port: by Alexey Torgashin (CudaText)                     }
{                                                                             }
{ *************************************************************************** }

{$mode delphi}

unit ec_RegExpr;

interface

uses
  Classes,
  {$IFDEF RE_DEBUG}ComCtrls,{$ENDIF}
  ec_StrUtils;

const
  MaskModI = 1;  // modifier /i bit in fModifiers
  MaskModR = 2;  // -"- /r
  MaskModS = 4;  // -"- /s
  MaskModG = 8;  // -"- /g
  MaskModM = 16; // -"- /m
  MaskModX = 32; // -"- /x

type
  TecRegExpr = class //(TPersistent)
  private
    FProgRoot: TObject;
    FModifiers: Word;
    FMatchOK: Boolean;
    FExpression: ecString;
    FModifiersStatic: Word;
    procedure SetModifiers(const Value: Word);
    function GetModifier(const Index: Integer): boolean;
    function GetModifierStr: ecString;
    procedure SetModifier(const Index: Integer; const Value: boolean);
    procedure SetModifierStr(const Value: ecString);
    function GetIsInvalid: Boolean;
    function GetMatchLen(Idx: integer): integer;
    function GetMatchPos(Idx: integer): integer;
    function GetSubExprMatchCount: integer;
    procedure SetExpression(const Value: ecString);
    procedure ClearRoot;
    function IsEmpty: Boolean;
    procedure ParseModifiers(const S: PWideChar; Len: integer; var Modifiers: Word); // Alexey
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TecRegExpr);

    function Compile: Boolean; overload;
    procedure Compile(const AExpression: UCString); overload;
    function Match(const InputString: UCString; var aPos: integer; Back: Boolean = False): Boolean; overload;
    //function MatchLength(const InputString: AnsiString; aPos: integer; Back: Boolean = False): integer; overload;
    function MatchLength(const InputString: UCString; aPos: integer; Back: Boolean = False): integer; overload;
    //function GetMatch(const InputString: AnsiString; SubIdx: integer): AnsiString; overload;
    function GetMatch(const InputString: UCString; SubIdx: integer): UCString; overload;
    function Substitute (const InputString, ATemplate : ecString) : ecString;

    property ModifierMask: Word read FModifiers write SetModifiers;
    property ModifierI: boolean index MaskModI read GetModifier write SetModifier;
    property ModifierR: boolean index MaskModR read GetModifier write SetModifier;
    property ModifierS: boolean index MaskModS read GetModifier write SetModifier;
    property ModifierG: boolean index MaskModG read GetModifier write SetModifier;
    property ModifierM: boolean index MaskModM read GetModifier write SetModifier;
    property ModifierX: boolean index MaskModX read GetModifier write SetModifier;
    property IsInvalid: Boolean read GetIsInvalid;

    property MatchPos[Idx: integer]: integer read GetMatchPos;
    property MatchLen[Idx: integer]: integer read GetMatchLen;
    property SubExprMatchCount: integer read GetSubExprMatchCount;
  published
    property Expression: ecString read FExpression write SetExpression;
    property ModifierStr: ecString read GetModifierStr write SetModifierStr stored False;
    property ModifierFlags: Word read FModifiers write FModifiers;
    property ModifiersStatic: Word read FModifiersStatic write FModifiersStatic;
  end;

type
  TGetCustomCharClass = function(ClassCode: UCChar): Boolean;
  TCheckCustomCharClass = function(ClassCode: UCChar; CharCode: Word; var IsInClass: Boolean): Boolean;

var
  DefaultModifiers: integer = MaskModI or MaskModR or MaskModG or MaskModM or MaskModX;
  //GetCustomCharClassProc: TGetCustomCharClass = nil;
  //CheckCustomCharClassProc: TCheckCustomCharClass = nil;

const
  // Error messages
  zreUnexpectedEnd = 'Unexpected end of expression';
  zreUnexpectedModifier = 'Unexpected modifier';
  zreUnexpectedBracket = 'Unexpected bracket';
  zreInvalidZeroWidth = 'Invalid zero-width expression';

{$IFDEF RE_DEBUG}
type TREDebugOnMatchProc = procedure(const TraceStr: string) of object;
var  REDebugOnMatchProc: TREDebugOnMatchProc = nil;
procedure REDebugCompiledBuildTree(RE: TecRegExpr; TV: TTreeView);
{$ENDIF}


implementation

uses SysUtils, Contnrs, Math;

function BufferHexToInt(p: PWideChar; Len: integer): integer; //Alexey
var
  N, i: integer;
  ch: WideChar;
begin
  Result:= 0;
  for i:= 1 to Len do
  begin
    ch:= p^;
    case ch of
      '0'..'9':
        N:= Ord(ch)-Ord('0');
      'a'..'f':
        N:= Ord(ch)-(Ord('a')-10);
      'A'..'F':
        N:= Ord(ch)-(Ord('A')-10);
      else
        exit(-1);
    end;
    Inc(p);
    Result:= Result*16+N;
  end;
end;

function BufferStrToInt(p: PWideChar; Len: integer): integer; //Alexey
var
  N, i: integer;
  ch: WideChar;
begin
  Result:= 0;
  for i:= 1 to Len do
  begin
    ch:= p^;
    if IsDigitChar(ch) then
      N:= Ord(ch)-Ord('0')
    else
      exit(-1);
    Inc(p);
    Result:= Result*10+N;
  end;
end;


{$IFDEF RE_DEBUG}
var
  LastNodeID: integer;
{$ENDIF}

type
  TreSubExpr = class;
  TreRootNode = class;
  // Base node class
  TRENodeBase = class
  private
    {$IFDEF RE_DEBUG}
    FNodeId: integer;
    {$ENDIF}
    FLoopMin: integer;    // repeat at least
    FLoopMax: integer;    // not more than
    FNonGreedy: Boolean;  // is greedy
    FNext: TRENodeBase;
    FOwner: TreSubExpr;
    function GetRoot: TreRootNode;
  protected
  public
    constructor Create(AOwner: TreSubExpr); virtual;

    function Match(const InputString: UCString; var aPos: integer): integer; overload; virtual; abstract;
    function BackMatch(const InputString: UCString; var aPos: integer): integer; overload; virtual; abstract;

    property Next: TRENodeBase read FNext write FNext;
    property Owner: TreSubExpr read FOwner;
    property Root: TreRootNode read GetRoot;
  end;

  // Char set & String
  TCharSetNode = class(TRENodeBase)
  private
    FIgnoreCase: Boolean;
    FInvert: Boolean;
    FCharRanges: UCString;
    FCharSets: UCString;
    FCharArray: UCString;
    {
    procedure AddChar(C: AnsiChar); overload;
    procedure AddRange(st, en: AnsiChar); overload;
    function HasChar(C: AnsiChar): Boolean; overload;
    }
    procedure AddChar(C: UCChar); overload;
    procedure AddRange(st, en: UCChar); overload;
    procedure AddSet(C: UCChar); overload;
    function HasChar(C: UCChar): Boolean; overload;
  public
    function Match(const InputString: UCString; var aPos: integer): integer; override;
    function BackMatch(const InputString: UCString; var aPos: integer): integer; override;
  end;

  // Simple text
  TCharSeqNode = class(TRENodeBase)
  private
    FIgnoreCase: Boolean;
    FChar: UCChar;
  public
    function Match(const InputString: UCString; var aPos: integer): integer; override;
    function BackMatch(const InputString: UCString; var aPos: integer): integer; override;
  end;

  // Special ckecking
  TSpecCheckNode = class(TRENodeBase)
  private
    FCheckType: UCChar;
  public
    function Match(const InputString: UCString; var aPos: integer): integer; override;
    function BackMatch(const InputString: UCString; var aPos: integer): integer; override;
  end;

  // Reference to sub expression

  { TRefNode }

  TRefNode = class(TRENodeBase)
  private
    FRef: integer;
    FIgnoreCase: Boolean;
    //function GetExprStr(const InputString: AnsiString): AnsiString; overload;
    //function GetExprStr(const InputString: UCString): UCString; overload;
    procedure GetExprPtr(const InputString: UCString; out Ptr: PWideChar; out Len: integer); // Alexey
  public
    function Match(const InputString: UCString; var aPos: integer): integer; override;
    function BackMatch(const InputString: UCString; var aPos: integer): integer; override;
  end;

  // Base class of containers
  TreListNodeBase = class(TRENodeBase)
  private
    FList: TFPObjectList;
  public
    constructor Create(AOwner: TreSubExpr); override;
    destructor Destroy; override;
    procedure Clear; virtual;
  end;

  // Branch of sub expression
  TreBranchNode = class(TreListNodeBase)
  private
    function GetClassChar(C: UCChar; Modifiers: Word): UCChar;
  public
    procedure Add(Node: TRENodeBase);
    procedure Invert;
    {
    procedure Compile(const Expression: AnsiString; var aPos: integer; Modifiers: Word); overload;
    }
    procedure Compile(const Expression: UCString; var aPos: integer; Modifiers: Word); overload;
    function Match(const InputString: UCString; var aPos: integer): integer; override;
    function BackMatch(const InputString: UCString; var aPos: integer): integer; override;
  end;

  // Zero-width testing
  // (?=RE) - positive, ahead
  // (?!RE) - negative, ahead
  // (?<=RE) - positive, behind
  // (?<!RE) - negative, behind
  TZeroWidth = class(TRENodeBase)
  private
    FIsBack: Boolean;
    FNegative: Boolean;
    FBranch: TreBranchNode;
    function DoResult(MatchRes: integer): integer;
  public
    function Match(const InputString: UCString; var aPos: integer): integer; override;
    function BackMatch(const InputString: UCString; var aPos: integer): integer; override;
    destructor Destroy; override;
  end;


  // Select operation (list of TreListNodeBase) => sub expression
  TreSubExpr = class(TreListNodeBase)
  private
    FStart: integer; // first char of sub expression
    FEnd: integer; // first char after sub expression
  public
    constructor Create(AOwner: TreSubExpr); override;
    //procedure Compile(const Expression: AnsiString; var aPos: integer; Modifiers: Word); overload;
    procedure Compile(const Expression: UCString; var aPos: integer; Modifiers: Word); overload;
    function Match(const InputString: UCString; var aPos: integer): integer; override;
    function BackMatch(const InputString: UCString; var aPos: integer): integer; override;
  end;

  // Root node
  TreRootNode = class(TreSubExpr)
  private
    FSubExpr: TFPList;
    FOwner: TecRegExpr;
  public
    constructor Create(AOwner: TecRegExpr); reintroduce;
    destructor Destroy; override;
    procedure Clear; override;
    function MatchStr(const InputString: UCString; var aPos: integer; Back: Boolean): Boolean; overload;

    property Owner: TecRegExpr read FOwner;
  end;


// =============================================================================
// Utils functions
// =============================================================================

function GetAbsoluteNext(Node: TRENodeBase):TRENodeBase;
begin
  Result := Node.Next;
  if (Result = nil) and (Node.Owner <> nil) then
    Result := GetAbsoluteNext(Node.Owner);
end;

function IsInRange(RngType: UCChar; C: UCChar): Boolean; overload;
begin
  Result := False;
  { // Alexey
  if not Assigned(CheckCustomCharClassProc) or
     not CheckCustomCharClassProc(RngType, Ord(C), Result)
  then
  }
  case RngType of
    // \c is \w without (?r)
    'c': Result := IsIdentChar(C);
    'C': Result := not IsIdentChar(C);
    // \k is \d without (?r)
    'k': Result := IsIdentDigitChar(C);
    'K': Result := not IsIdentDigitChar(C);
    // \w with (?r)
    'w': Result := IsWordChar(C);
    'W': Result := not IsWordChar(C);
    // \d with (?r)
    'd': Result := IsDigitChar(C);
    'D': Result := not IsDigitChar(C);
    // space
    's': Result := IsSpaceOrBreakChar(C);
    'S': Result := not IsSpaceOrBreakChar(C);
    // hex, rarely used
    'h': Result := IsHexDigitChar(C);
    'H': Result := not IsHexDigitChar(C);
    // Alexey: changed /l and /L meaning, now it's: word char except '_'
    'l': Result := IsWordChar(C) and (C<>'_');
    'L': Result := not IsWordChar(C) or (C='_');
    { //Alexey: removed
    'g': Result := IsIdentLetterChar(C);
    'G': Result := not IsIdentLetterChar(C);
    }
  end;
end;

function GetEscape(const Expression: UCString; var aPos: integer): UCChar; overload;
var
  strt: integer;
  N: integer;
begin
  Result := #0;
  case Expression[aPos] of
    't': Result := #$9;
    'n': Result := #$A;
    'r': Result := #$D;
    'f': Result := #$C;
    'a': Result := #$7;
    'e': Result := #$1B;
    'x': begin
          inc(aPos);
          if aPos > Length(Expression) then
             raise Exception.Create('Invalid escape char');
          if Expression[aPos] = '{' then
           begin
             inc(aPos);
             strt := aPos;
             while (aPos < Length(Expression)) and (Expression[aPos] <> '}') do
               inc(aPos);
             N := BufferHexToInt(@Expression[strt], aPos - strt);
             if N<0 then
               raise Exception.Create('Invalid hex digit in \x');
             Result := UCChar(N);
           end else
            begin
              N := BufferHexToInt(@Expression[aPos], 2);
              if N<0 then
                raise Exception.Create('Invalid hex digit in \x');
              Result := UCChar(N);
              Inc(aPos);
            end;
          if Result = '' then
            raise Exception.Create('Invalid hex digit in \x');
         end;
  end;
end;

{ TRENodeBase }

constructor TRENodeBase.Create(AOwner: TreSubExpr);
begin
  inherited Create;
  FOwner := AOwner;
  FLoopMin := 1;
  FLoopMax := 1;
  {$IFDEF RE_DEBUG}
  Inc(LastNodeID);
  FNodeId := LastNodeID;
  {$ENDIF}
end;

function TRENodeBase.GetRoot: TreRootNode;
var Node: TRENodeBase;
begin
  Node := Self;
  while Node.Owner <> nil do
    Node := Node.Owner;
  Result := Node as TreRootNode;
end;

{ TCharSetNode }

function TCharSetNode.HasChar(C: UCChar): Boolean;
var i, N, k: integer;
begin
  N := Length(FCharRanges);
  if N > 0 then
   for i := 1 to N shr 1 do
    begin
     k := i shl 1;
     if (Ord(FCharRanges[k]) >= Ord(C)) and
        (Ord(FCharRanges[k - 1]) <= Ord(C)) then
      begin
        Result := True;
        Exit;
      end;
    end;
  for i := 1 to Length(FCharSets) do
   if IsInRange(FCharSets[i], C) then
    begin
     Result := True;
     Exit;
    end;
  Result := Pos(C, FCharArray) <> 0;
end;

procedure TCharSetNode.AddChar(C: UCChar);
begin
  if FIgnoreCase then
    CharToUpCase(C);
  if Pos(C, FCharArray) = 0 then
    FCharArray := FCharArray + C;
end;

procedure TCharSetNode.AddRange(st, en: UCChar);
begin
  if FIgnoreCase then
   begin
    CharToUpCase(st);
    CharToUpCase(en);
   end;
  FCharRanges := FCharRanges + st + en;
end;

procedure TCharSetNode.AddSet(C: UCChar);
begin
  FCharSets := FCharSets + C;
end;

function TCharSetNode.Match(const InputString: UCString; var aPos: integer): integer;
var C: UCChar;
    b: Boolean;
begin
  if aPos > Length(InputString) then
    begin
      Result := 0;
      Exit;
    end;

  C := InputString[aPos];
  if FIgnoreCase then CharToUpCase(C);
  b := HasChar(C) xor FInvert;
  if b then Inc(aPos);
  if b then Result := 1 else Result := 0;
end;

function TCharSetNode.BackMatch(const InputString: UCString; var aPos: integer): integer;
var C: UCChar;
    b: Boolean;
begin
  if aPos <= 1 then
    begin
      Result := 0;
      Exit;
    end;

  C := InputString[aPos - 1];
  if FIgnoreCase then CharToUpCase(C);
  b := HasChar(C) xor FInvert;
  if b then Dec(aPos);
  if b then Result := 1 else Result := 0;
end;

{ TCharSeqNode }

function TCharSeqNode.BackMatch(const InputString: UCString; var aPos: integer): integer;
var C: UCChar;
begin
  Result := 0;
  if aPos > 1 then
    begin
      C := InputString[aPos - 1];
      if FIgnoreCase then
        CharToUpCase(C);
      if C = FChar then
        begin
          Dec(aPos);
          Result := 1;
        end;
    end;
end;

function TCharSeqNode.Match(const InputString: UCString; var aPos: integer): integer;
var C: UCChar;
begin
  Result := 0;
  if aPos <= Length(InputString) then
    begin
      C := InputString[aPos];
      if FIgnoreCase then
        CharToUpCase(C);
      if C = FChar then
        begin
          Inc(aPos, 1);
          Result := 1;
        end;
    end;
end;

{ TSpecCheckNode }

function TSpecCheckNode.BackMatch(const InputString: UCString; var aPos: integer): integer;
var C: UCChar;
    b: Boolean;
begin
  b := False;
  if aPos <= 1 then
   begin
    if aPos < 1 then
      begin
        Result := 0;
        Exit;
      end;
    C := #0;
   end
  else C := InputString[aPos - 1];

  case FCheckType of
    '^': b := (C = #0) or (C = #10) or
                   (C = #13) and (aPos <= Length(InputString)) and (InputString[aPos] <> #10);
    '$': b := (aPos > Length(InputString)) or (InputString[aPos] = #13) or
                   (InputString[aPos] = #10) and (C <> #13);
    'A': b := C = #0;
    'Z': b := aPos > Length(InputString);
    'b': b := IsWordBreak(aPos, InputString);
    'B': b := not IsWordBreak(aPos, InputString);
    'z': begin
           b := IsLineBreakChar(C);
           if b then
             begin
               Dec(aPos);
               if (C = #10) and (aPos > 0) and (InputString[aPos] = #13) then
                 Dec(aPos);
             end;
         end;
  else if C<> #0 then
   begin
     case FCheckType of
      '.': b := True;
      ':': b := not IsLineBreakChar(C);
      else b := IsInRange(FCheckType, C);
     end;
     if b then Dec(aPos);
   end;
  end;
  if b then Result := 1 else Result := 0;
end;

function TSpecCheckNode.Match(const InputString: UCString; var aPos: integer): integer;
var C: UCChar;
    b: Boolean;
begin
  b := False;
  if aPos > Length(InputString) then
   begin
    if aPos - 1 > Length(InputString) then
      begin
        Result := 0;
        Exit;
      end;
    C := #0;
   end
  else C := InputString[aPos];

  case FCheckType of
    '^': b := (aPos = 1) or (InputString[aPos - 1] = #10) or
                   (InputString[aPos - 1] = #13) and (C <> #10);
    '$': b := (C = #13) or (C = #10) and (aPos > 1) and (InputString[aPos - 1] <> #13) or (C = #0);
    'A': b := aPos = 1;
    'Z': b := C = #0;
    'b': b := IsWordBreak(aPos, InputString);
    'B': b := not IsWordBreak(aPos, InputString);
    'z': begin
           b := IsLineBreakChar(C);
           if b then
             begin
               Inc(aPos);
               if (C = #13) and (aPos <= Length(InputString)) and (InputString[aPos] = #10) then
                 Inc(aPos);
             end;
         end;
  else if C<> #0 then
   begin
     case FCheckType of
      '.': b := True;
      ':': b := not IsLineBreakChar(C);
      else b := IsInRange(FCheckType, C);
     end;
     if b then Inc(aPos);
   end;
  end;
  if b then Result := 1 else Result := 0;
end;

{ TRefNode }

procedure TRefNode.GetExprPtr(const InputString: UCString; out Ptr: PWideChar; out Len: integer); // Alexey
var se: TreSubExpr;
begin
  Ptr := nil;
  Len := 0;
  with Root do
    if FSubExpr.Count > FRef then
      begin
        se := TreSubExpr(FSubExpr[FRef]);
        Len := abs(se.FEnd - se.FStart);
        if (se.FStart > 0) and (se.FEnd > 0) then
          Ptr := @InputString[Min(se.FStart, se.FEnd)];
      end;
end;

function TRefNode.BackMatch(const InputString: UCString; var aPos: integer): integer; // Alexey
var
  P1, P2: PWideChar;
  L: integer;
  b: Boolean;
begin
  Result := 0;
  GetExprPtr(InputString, P1, L);
  if (P1 <> nil) and (L < aPos) then
    begin
      P2 := @InputString[aPos - L];
      if FIgnoreCase then
        b := strlicomp(P1, P2, L) = 0
      else
        b := strlcomp(P1, P2, L) = 0;
      if b then
        begin
          Dec(aPos, L);
          Result := 1;
        end;
    end;
end;

function TRefNode.Match(const InputString: UCString; var aPos: integer): integer; // Alexey
var
  P1, P2: PWideChar;
  L: integer;
  b: Boolean;
begin
  Result := 0;
  GetExprPtr(InputString, P1, L);
  if (P1 <> nil) and (L <= Length(InputString) - aPos + 1) then
    begin
      P2 := @InputString[aPos];
      if FIgnoreCase then
        b := strlicomp(P1, P2, L) = 0
      else
        b := strlcomp(P1, P2, L) = 0;
      if b then
        begin
          Inc(aPos, L);
          Result := 1;
        end;
    end;
end;

{ TZeroWidth }

destructor TZeroWidth.Destroy;
begin
  FreeAndNil(FBranch);
  inherited;
end;

function TZeroWidth.DoResult(MatchRes: integer): integer;
begin
  if FNegative then
    begin
      If MatchRes = 0 then
        Result := 2
      else
        Result := 0;
    end else
      Result := MatchRes;
end;

function TZeroWidth.BackMatch(const InputString: UCString;
  var aPos: integer): integer;
var testPos: integer;
begin
  testPos := aPos;
  if FIsBack then
    Result := DoResult(FBranch.Match(InputString, testPos))
  else
    Result := DoResult(FBranch.BackMatch(InputString, testPos));
end;

function TZeroWidth.Match(const InputString: UCString;
  var aPos: integer): integer;
var testPos: integer;
begin
  testPos := aPos;
  if FIsBack then
    Result := DoResult(FBranch.BackMatch(InputString, testPos))
  else
    Result := DoResult(FBranch.Match(InputString, testPos));
end;

{ TreListNodeBase }

procedure TreListNodeBase.Clear;
begin
  FList.Clear;
end;

constructor TreListNodeBase.Create(AOwner: TreSubExpr);
begin
  inherited;
  FList := TFPObjectList.Create;
end;

destructor TreListNodeBase.Destroy;
begin
  FList.Free;
  inherited;
end;

{ TreBranchNode }

function TreBranchNode.GetClassChar(C: UCChar; Modifiers: Word): UCChar;
// Alexey: rewritten
begin
  { // Alexey
  if Assigned(GetCustomCharClassProc) and GetCustomCharClassProc(C) then
    Result := C else // User defined character class
  }
  case C of
    'w':
      begin
        if (MaskModR and Modifiers) = 0 then
          Result := 'c'
        else
          Result := 'w';
      end;
    'W':
      begin
        if (MaskModR and Modifiers) = 0 then
          Result := 'C'
        else
          Result := 'W';
      end;
    'd':
      begin
        if (MaskModR and Modifiers) = 0 then
          Result := 'k'
        else
          Result := 'd';
      end;
    'D':
      begin
        if (MaskModR and Modifiers) = 0 then
          Result := 'K'
        else
          Result := 'D';
      end;
    // Alexey
    's', 'S',
    'h', 'H',
    'c', 'C',
    'l', 'L',
    //'g', 'G', // Alexey: removed
    'k', 'K':
      Result := C;
    else
      Result := #0;
  end;
end;

procedure TreBranchNode.Add(Node: TRENodeBase);
begin
  if FList.Count > 0 then
    TRENodeBase(FList.Last).Next := Node;
  FList.Add(Node);
end;

procedure TreBranchNode.Invert;
var i, N: integer;
    NextLast: TRENodeBase;
begin
  N := FList.Count;
  if N = 0 then Exit;
  NextLast := TRENodeBase(FList.Last).Next;
  for i := 0 to (N div 2) - 1 do
    FList.Exchange(i, N - 1 - i);
  for i := 0 to N - 2 do
    TRENodeBase(FList[i]).Next := TRENodeBase(FList[i + 1]);
  TRENodeBase(FList.Last).Next := NextLast;
end;

// All characters are ANSI characters.
// UCChar is used only for holding char codes less 255
// This is made to have unified storage of regular expr. nodes

procedure TreBranchNode.Compile(const Expression: UCString; var aPos: integer;
  Modifiers: Word);
var Len: integer;
    sub: TreSubExpr;
    C: UCChar;

    function SafeInc(RaiseEx: Boolean = False): UCChar; // Increment position to the next significant char
    var
      C: WideChar;
      bComment: boolean;
    begin
      inc(aPos);
      // Skip spaces and comments
      if (Modifiers and MaskModX) <> 0 then
        while (aPos <= Len) do
         begin
           C := Expression[aPos];
           bComment:= C = '#';
           if not (IsSpaceChar(C) or bComment) then Break;
           if bComment then
           begin
            Inc(aPos);
            while (aPos <= Len) and not IsLineBreakChar(Expression[aPos]) do
              Inc(aPos);
           end;
          Inc(aPos);
         end;

      if aPos <= Len then Result := Expression[aPos] else
       begin
         Result := #0;
         if RaiseEx then
           raise Exception.Create(zreUnexpectedEnd);
       end;
    end;

    function ReadNumber: integer;
    var strt: integer;
    begin
      strt := aPos;
      while (aPos <= Len) and IsDigitChar(Expression[aPos]) do
       Inc(aPos);
      if strt = aPos then
       raise Exception.Create('Number is expected');
      Result := BufferStrToInt(@Expression[strt], aPos - strt);
      Dec(aPos);
    end;

    // Read repeaters for node
    procedure ReadRepeaters(Node: TRENodeBase);
    begin
      if aPos > Len then Exit;// repeaters are optional
      case SafeInc of
        '+': Node.FLoopMax := -1;
        '?': Node.FLoopMin := 0;
        '*': begin
               Node.FLoopMax := -1;
               Node.FLoopMin := 0;
             end;
        '{': begin
              SafeInc;
              Node.FLoopMin := ReadNumber;
              case SafeInc of
                ',': if SafeInc <> '}' then
                       begin
                        Node.FLoopMax := ReadNumber;
                        if SafeInc <> '}' then
                          raise Exception.Create('There is no closing "}"');
                       end
                     else
                       Node.FLoopMax := -1;
                '}': Node.FLoopMax := Node.FLoopMin;
                else raise Exception.Create('Invalid loop specifier');
              end;
              if (Node.FLoopMax >= 0) and (Node.FLoopMax < Node.FLoopMin) then
                raise Exception.Create('Loop minimum must be less then loop maximum');
             end;
        else begin
              Dec(aPos);
              Exit;
             end
      end;
      if SafeInc = '?' then Node.FNonGreedy := True else
       begin
        Node.FNonGreedy := (Modifiers and MaskModG) = 0;
        Dec(aPos);
       end;
    end;

    procedure AddCharSeq(const C: UCChar);
    var csNode: TCharSeqNode;
    begin
      csNode := TCharSeqNode.Create(Owner);
      csNode.FIgnoreCase := (Modifiers and MaskModI) <> 0;
      if csNode.FIgnoreCase then csNode.FChar := ecUpCase(C)
                            else csNode.FChar := C;
      Add(csNode);
      ReadRepeaters(csNode);
    end;

    procedure AddSpecNode(const C: UCChar; WithRepeat: Boolean = True);
    var sn: TSpecCheckNode;
    begin
      sn := TSpecCheckNode.Create(Owner);
      sn.FCheckType := C;
      Add(sn);
      if WithRepeat then ReadRepeaters(sn);
    end;

    function PickSetChar(cs: TCharSetNode): UCChar;
    begin
      Result := Expression[aPos];
      if Result = '\' then
       begin
         Inc(aPos);
         if aPos > Length(Expression) then
          Exit;
         Result := GetEscape(Expression, aPos);
         if Result = #0 then
          begin
            Result := GetClassChar(Expression[aPos], Modifiers);
            if Result = #0 then Result := Expression[aPos]
              else
                begin
                  cs.AddSet(Result);
                  Result := #0;
                end;
          end;
       end;
    end;

    procedure ReadCharSet;
    var cs: TCharSetNode;
        Cstrt, Cend: UCChar;
    begin
      cs := TCharSetNode.Create(Owner);
      cs.FIgnoreCase := (Modifiers and MaskModI) <> 0;
      Add(cs);
      Cstrt := #0;
      if SafeInc(True) = '^' then cs.FInvert := True
        else Cstrt := PickSetChar(cs);
      while SafeInc(True) <> ']' do
       begin
         if (Expression[aPos] = '-') and (Cstrt <> #0) then // Add Range
          if SafeInc = ']' then
           begin
            cs.AddChar(Cstrt);
            Cstrt := '-';
            Break;
           end else
           begin
            Cend := PickSetChar(cs);
            if Cend = #0 then
             begin
              cs.AddChar(Cstrt);
              Cstrt := '-';
             end else
             if Ord(Cend) < Ord(Cstrt) then
              raise Exception.Create('Invalid set range') else
              begin
               // Extended russian support
               cs.AddRange(Cstrt, Cend);
               Cstrt := #0;
               if SafeInc(True) = ']' then Break;
              end;
           end;
         if Cstrt <> #0 then cs.AddChar(Cstrt);
         Cstrt := PickSetChar(cs);
       end;
      if Cstrt <> #0 then cs.AddChar(Cstrt);
      ReadRepeaters(cs);
    end;

    procedure AddRefNode(RefIdx: integer);
    var rn: TRefNode;
    begin
      if TreRootNode(Root).FSubExpr.Count <= RefIdx then
       raise Exception.Create('Invalid reference');
      rn := TRefNode.Create(Owner);
      rn.FRef := RefIdx;
      rn.FIgnoreCase := (Modifiers and MaskModI) <> 0;
      Add(rn);
      ReadRepeaters(rn);
    end;

    procedure AddZeroWidth(IsBack: Boolean);
    var Negative: Boolean;
        Branch: TreBranchNode;
        Node: TZeroWidth;
    begin
      case Expression[aPos] of
        '!': Negative := True;
        '=': Negative := False;
        else
          raise Exception.Create(zreInvalidZeroWidth);
      end;
      SafeInc(True);
      Branch := TreBranchNode.Create(nil);
      try
        Branch.Compile(Expression, aPos, Modifiers);
        Node := TZeroWidth.Create(Owner);
        Node.FIsBack := IsBack;
        Node.FNegative := Negative;
        Node.FBranch := Branch;
        Add(Node);
        if IsBack then
          Branch.Invert;
      except
        Branch.Free;
      end;
    end;

var tp: integer;
begin
  Clear;
  Len := Length(Expression);
  Dec(aPos);
  while aPos <= Len do
   begin
     case SafeInc of
       ')', '|', #0: Exit; // end of branch
       '(': begin
             if SafeInc = '?' then
              begin // Change modifiers
                case SafeInc(True) of
                  '<': begin
                         SafeInc(True);
                         AddZeroWidth(True);
                       end;
                  '=', '!': AddZeroWidth(False);
                  else
                    begin
                      tp := aPos;
                      repeat // skip comment
                        Inc(aPos);
                      until (aPos > Len) or (Expression[aPos] = ')');
                      if Expression[tp] <> '#' then
                        Root.Owner.ParseModifiers(@Expression[tp], aPos - tp, Modifiers)
                    end;
                end;
              end else
              begin // sub expression
                sub := TreSubExpr.Create(Owner);
                Add(sub);
                sub.Compile(Expression, aPos, Modifiers);
                if (aPos > Len) or (Expression[aPos] <> ')') then
                 raise Exception.Create('Do not closed sub expression');
                ReadRepeaters(sub);
              end;
            end;
       '[': begin    // char set node
              ReadCharSet;
            end;
       '^': if (Modifiers and MaskModM) = 0 then AddSpecNode('A', False)  // begin of text
                                            else AddSpecNode('^', False); // begin of line
       '$': if (Modifiers and MaskModM) = 0 then AddSpecNode('Z', False)  // end of text
                                            else AddSpecNode('$', False); // end of line
       '.': if (Modifiers and MaskModS) <> 0  then AddSpecNode('.')  // all
                                             else AddSpecNode(':'); // all without line separators
       '\': begin
              Inc(aPos);
              if aPos > Len then C := '\'
               else C := GetEscape(Expression, aPos);
              if C <> #0 then AddCharSeq(C) else
               begin
                 C := GetClassChar(Expression[aPos], Modifiers);
                 if C <> #0 then AddSpecNode(C) else
                 case Expression[aPos] of
                   'A', 'Z', 'b', 'B', 'z': AddSpecNode(Expression[aPos]);
                   '1'..'9': AddRefNode(Ord(Expression[aPos])-Ord('0'));
                   else      AddCharSeq(Expression[aPos]);
                 end;
               end;
            end;
       else AddCharSeq(Expression[aPos]); // Simple char
     end;
   end;
end;

// Main mtaching routine (recursive)
// Returns:
//  0 - does not match
//  1 - match the Node
//  2 - match The Node and all next nodes
function MatchNode(Node: TRENodeBase; const InputString: UCString;
   var aPos: integer): integer; overload;

var save, k, sv, LastSucc, total, Success: integer;
    IsBrEnd: Boolean;
    
begin
  if Node = nil then
    begin
      Result := 2;
      Exit;
    end;
  Result := 0;

  // required minimum repeat
  save := aPos;
  Success := 0;
  for k := 1 to node.FLoopMin do
    begin
      Success := Node.Match(InputString, aPos);
      if Success = 0 then
        begin
          aPos := save;
          {$IFDEF RE_DEBUG}
          if Assigned(REDebugOnMatchProc) then
            REDebugOnMatchProc(Format('Node: %d; Position: %d; Result: %d',[Node.FNodeID, aPos, 0]));
          {$ENDIF}
          Exit;
        end;
    end;

  k := node.FLoopMin - 1;
//  k := 0;
  LastSucc := 0;
  total := 0;

  IsBrEnd := Assigned(Node.Owner) and
             ((Node.Next = nil) or (Node.Next.Owner <> Node.Owner));
  repeat
    Inc(k);
    sv := aPos;
    if IsBrEnd then Node.Owner.FEnd := aPos;
    if Node is TreSubExpr then
      begin
        if Success <> 2 then
          Success := MatchNode(GetAbsoluteNext(Node), InputString, aPos);
      end else
    if Node.Next <> nil then
      Success := MatchNode(Node.Next, InputString, aPos) else
    if IsBrEnd then
      Success := MatchNode(GetAbsoluteNext(Node.Owner), InputString, aPos) else
      Success := 2;

    if Success = 2 then // success all next nodes
     begin
       {$IFDEF RE_DEBUG}
       if Assigned(REDebugOnMatchProc) then
          REDebugOnMatchProc(Format('Success at %d;  Node: %d; NextRes: %s',[sv, Node.FNodeID, IntToStr(Success)]));
       {$ENDIF}
       total := aPos;
       LastSucc := sv;
       aPos := LastSucc;
       Result := Success;
       if node.FNonGreedy then
         Break; // for non GREEDY mode
     end else
    if IsBrEnd then
      begin
       total := sv;
       Result := 1;
      end else
    if Success = 1 then
      begin
        aPos := sv;
        total := sv;
        Result := 1;
        if node.FNonGreedy then
          Break; // for non GREEDY mode
      end;

    if (node.FLoopMax > 0) and (k >= node.FLoopMax) then // check max limit
      begin
        if not IsBrEnd then  //v2.36
          Result := Success; //v2.33
        Break;
      end;

    sv := aPos;
    Success := Node.Match(InputString, aPos);
  until (Success = 0) or (aPos = sv);

//  if (node.FLoopMin > 0) and (k < node.FLoopMin) then
//    Result := 0;
{$IFDEF RE_DEBUG}
  if Assigned(REDebugOnMatchProc) then
    REDebugOnMatchProc(Format('Node: %d; Position: %d; Result: %d',[Node.FNodeID, aPos, integer(Result)]));
{$ENDIF}
  if Result > 0 then
   begin
    aPos := total;
    // Save sub-expression result
    if IsBrEnd and (LastSucc > 0) then
      Node.Owner.FEnd := LastSucc;
   end else
   begin
    aPos := save;
    if IsBrEnd then Node.Owner.FEnd := 0;
   end;
end;

// Main mtaching routine (recursive)
function BackMatchNode(Node: TRENodeBase; const InputString: UCString;
  var aPos: integer): integer; overload;
  
var save, k, sv, LastSucc, total, Success: integer;
    IsBrEnd: Boolean;

begin
  if Node = nil then
    begin
      Result := 2;
      Exit;
    end;
  Result := 0;

  // required minimum repeat
  save := aPos;
  Success := 0;
  for k := 1 to node.FLoopMin do
    begin
      Success := Node.BackMatch(InputString, aPos);
      if Success = 0 then
        begin
          aPos := save;
          {$IFDEF RE_DEBUG}
          if Assigned(REDebugOnMatchProc) then
            REDebugOnMatchProc(Format('Node: %d; Position: %d; Result: %d',[Node.FNodeID, aPos, 0]));
          {$ENDIF}
          Exit;
        end;
    end;

  k := node.FLoopMin - 1;
  LastSucc := 0;
  total := 0;

  IsBrEnd := Assigned(Node.Owner) and
             ((Node.Next = nil) or (Node.Next.Owner <> Node.Owner));
  repeat
    Inc(k);
    sv := aPos;
    if IsBrEnd then Node.Owner.FStart := aPos;
    if Node is TreSubExpr then
      begin
        if Success <> 2 then
          Success := BackMatchNode(GetAbsoluteNext(Node), InputString, aPos);
      end else
    if Node.Next <> nil then
      Success := BackMatchNode(Node.Next, InputString, aPos) else
    if IsBrEnd then
      Success := BackMatchNode(GetAbsoluteNext(Node.Owner), InputString, aPos) else
      Success := 2;

    if Success = 2 then
     begin
       {$IFDEF RE_DEBUG}
       if Assigned(REDebugOnMatchProc) then
          REDebugOnMatchProc(Format('Success at %d;  Node: %d; NextRes: %s',[sv, Node.FNodeID, IntToStr(Success)]));
       {$ENDIF}
       total := aPos;
       LastSucc := sv;
       aPos := LastSucc;
       Result := Success;
       if node.FNonGreedy then Break; // for non GREEDY mode
     end else
    if IsBrEnd then
      begin
       total := sv;
       Result := 1;
      end else
    if Success = 1 then
      begin
        aPos := sv;
        total := sv;
        Result := 1;
      end;

    if (node.FLoopMax > 0) and (k >= node.FLoopMax) then // check max limit
      begin
        if not IsBrEnd then  //v2.36
          Result := Success; //v2.33
        Break;
      end;

    sv := aPos;
    Success := Node.BackMatch(InputString, aPos);
  until (Success = 0) or (aPos = sv);

{$IFDEF RE_DEBUG}
  if Assigned(REDebugOnMatchProc) then
    REDebugOnMatchProc(Format('Node: %d; Position: %d; Result: %d',[Node.FNodeID, aPos, integer(Result)]));
{$ENDIF}
  if Result > 0 then
   begin
    aPos := total;
    // Save sub-expression result
    if IsBrEnd and (LastSucc > 0) then
      Node.Owner.FStart := LastSucc;
   end else
   begin
    aPos := save;
    if IsBrEnd then Node.Owner.FStart := 0;
   end;
end;

function TreBranchNode.Match(const InputString: UCString; var aPos: integer): integer;
begin
{$IFDEF RE_DEBUG}
  if Assigned(REDebugOnMatchProc) then
    REDebugOnMatchProc(Format('Branch<: %d; Position: %d',[FNodeID, aPos]));
{$ENDIF}
  if FList.Count = 0 then Result := 1
    else Result := MatchNode(TRENodeBase(FList.First), InputString, aPos);
{$IFDEF RE_DEBUG}
  if Assigned(REDebugOnMatchProc) then
    REDebugOnMatchProc(Format('Branch>: %d; Position: %d; Result: %d',[FNodeID, aPos, integer(Result)]));
{$ENDIF}
end;

function TreBranchNode.BackMatch(const InputString: UCString; var aPos: integer): integer;
begin
{$IFDEF RE_DEBUG}
  if Assigned(REDebugOnMatchProc) then
    REDebugOnMatchProc(Format('Branch<: %d; Position: %d',[FNodeID, aPos]));
{$ENDIF}
  if FList.Count = 0 then Result := 1
    else Result := BackMatchNode(TRENodeBase(FList.First), InputString, aPos);
{$IFDEF RE_DEBUG}
  if Assigned(REDebugOnMatchProc) then
    REDebugOnMatchProc(Format('Branch>: %d; Position: %d; Result: %d',[FNodeID, aPos, integer(Result)]));
{$ENDIF}
end;

{ TreSubExpr }

procedure TreSubExpr.Compile(const Expression: UCString; var aPos: integer;
  Modifiers: Word);
var Br: TreBranchNode;
begin
  Dec(aPos);
  repeat
    Inc(aPos);
    Br := TreBranchNode.Create(Self);
    FList.Add(Br);
    Br.Compile(Expression, aPos, Modifiers);
    if Br.FList.Count = 0 then FList.Remove(Br);
  until (aPos > Length(Expression)) or (Expression[aPos] = ')');
end;

constructor TreSubExpr.Create(AOwner: TreSubExpr);
begin
  inherited;
  if Owner <> nil then
    TreRootNode(Root).FSubExpr.Add(Self);
end;

function TreSubExpr.Match(const InputString: UCString; var aPos: integer): integer;
var i: integer;
    OldEnd, OldStart, CurRes, svPos: integer;
begin
  OldEnd := FEnd;
  OldStart := FStart;
  FStart := aPos;
  FEnd := 0;
  Result := 0;
  svPos := aPos;
  for i := 0 to FList.Count - 1 do
    begin
      CurRes := TreBranchNode(FList[i]).Match(InputString, aPos);
      if FNonGreedy then
        begin
          if CurRes > 0 then
            begin
              OldEnd := FEnd;
              OldStart := FStart;
              Result := CurRes;
              aPos := svPos;
              Break;
            end;
        end else
          if CurRes > Result then
            begin
              OldEnd := FEnd;
              OldStart := FStart;
              Result := CurRes;
              aPos := svPos;
              if Result = 2 then
                Break;
            end;
    end;
  FEnd := OldEnd;
  FStart := OldStart;
  if (FEnd > 0) and (Result > 0) then
    aPos := FEnd;
end;

function TreSubExpr.BackMatch(const InputString: UCString; var aPos: integer): integer;
var i: integer;
    OldEnd, OldStart, CurRes, svPos: integer;
begin
  OldEnd := FEnd;
  OldStart := FStart;
  FStart := 0;
  FEnd := aPos;
  Result := 0;
  svPos := aPos;
  for i := 0 to FList.Count - 1 do
    begin
      CurRes := TreBranchNode(FList[i]).BackMatch(InputString, aPos);
      if FNonGreedy then
        begin
          if CurRes > 0 then
            begin
              OldEnd := FEnd;
              OldStart := FStart;
              Result := CurRes;
              aPos := svPos;
              Break;
            end;
        end else
          if CurRes > Result then
            begin
              OldEnd := FEnd;
              OldStart := FStart;
              Result := CurRes;
              aPos := svPos;
              if Result = 2 then
                Break;
            end;
    end;
  FEnd := OldEnd;
  FStart := OldStart;
  if (FStart > 0) and (Result > 0) then
    aPos := FStart;
end;

{ TreRootNode }

procedure TreRootNode.Clear;
begin
  inherited;
  FSubExpr.Clear;
  FSubExpr.Add(Self);
end;

constructor TreRootNode.Create(AOwner: TecRegExpr);
begin
  inherited Create(nil);
  FOwner := AOwner;
  FSubExpr := TFPList.Create;
  FSubExpr.Add(Self);
end;

destructor TreRootNode.Destroy;
begin
  FSubExpr.Free;
  inherited;
end;

function TreRootNode.MatchStr(const InputString: UCString;
  var aPos: integer; Back: Boolean): Boolean;
var i: integer;
begin
  for i := 0 to FSubExpr.Count - 1 do
   begin
     TreSubExpr(FSubExpr[i]).FStart := -1;
     TreSubExpr(FSubExpr[i]).FEnd := -1;
   end;
  if Back then
    Result := BackMatchNode(Self, InputString, aPos) <> 0
  else
    Result := MatchNode(Self, InputString, aPos) <> 0;
end;

// =============================================================================
//   Application Level
// =============================================================================

{ TecRegExpr }

constructor TecRegExpr.Create;
begin
  inherited;
  FModifiers := DefaultModifiers;
end;

destructor TecRegExpr.Destroy;
begin
  FreeAndNil(FProgRoot);
  inherited;
end;

procedure TecRegExpr.ClearRoot;
begin
  FreeAndNil(FProgRoot);
  FMatchOK := False;
end;

function TecRegExpr.IsEmpty: Boolean;
begin
  Result := not Assigned(FProgRoot) or (TreRootNode(FProgRoot).FList.Count = 0);
end;

procedure TecRegExpr.Compile(const AExpression: UCString);
var Pos: integer;
begin
  {$IFDEF RE_DEBUG} LastNodeID := 0; {$ENDIF}
  FMatchOK := False;
  if not Assigned(FProgRoot) then
    FProgRoot := TreRootNode.Create(Self)
  else
    TreRootNode(FProgRoot).Clear;

  Pos := 1;
  try
    if AExpression <> '' then
      TreRootNode(FProgRoot).Compile(AExpression, Pos, FModifiers);
  except
    ClearRoot;
    raise;
  end;
  //FUnicodeCompiled := True;
end;

function TecRegExpr.Compile: Boolean;
begin
  try
    if IsEmpty then
      Compile(FExpression);
  except
  end;
  Result := not IsEmpty;
end;

function TecRegExpr.GetIsInvalid: Boolean;
begin
  Result := not Compile;
end;

function TecRegExpr.GetModifier(const Index: Integer): boolean;
begin
  Result := (FModifiers and Index) <> 0;
end;

function TecRegExpr.GetModifierStr: ecString;
const ModLet: ecString = 'irsgmx';
var s1, s2: string;
    i: integer;
begin
  s1 := ''; s2 := '';
  for i := 0 to 5 do
   if (FModifiers and (1 shl i)) <> 0 then
    s1 := s1 + ModLet[i + 1]
   else
    s2 := s2 + ModLet[i + 1];

  Result := '(?' + s1;
  if s2 <> '' then
   Result := Result + '-' + s2;
  Result := Result + ')';
end;

function TecRegExpr.Match(const InputString: UCString; var aPos: integer; Back: Boolean): Boolean;
begin
  Result := Compile; // ensure compiling and validity
  if Result then
    begin
      if aPos < 1 then
        aPos := 1;
      Result := TreRootNode(FProgRoot).MatchStr(InputString, aPos, Back);
      FMatchOK := Result;
    end;
end;

function TecRegExpr.MatchLength(const InputString: UCString;
  aPos: integer; Back: Boolean): integer;
begin
  Result := aPos;
  if Match(InputString, aPos, Back) then
    begin
     if Back then
       Result := Result - aPos
     else
       Result := aPos - Result;
    end
  else
    Result := 0;
end;


//Delete regex comments "#...." in multi-line text
function _MultilineToString(const Value: ecString): ecString;
  //
  function IsEol(C: WideChar): boolean; inline;
  begin
    Result:= (C=#10) or (C=#13);
  end;
  //
var
  NBegin, NEnd, i: integer;
begin
  Result:= Value;

  repeat
    NBegin:= 0;
    for i:= 1 to Length(Result) do
      if (Result[i]='#') and ((i=1) or (Result[i-1]<>'\')) then
      begin
        NBegin:= i;
        Break;
      end;
    if NBegin=0 then Break;

    NEnd:= Length(Result);
    for i:= NBegin+1 to Length(Result) do
      if IsEol(Result[i]) then
      begin
        NEnd:= i-1;
        Break;
      end;

    Delete(Result, NBegin, NEnd-NBegin+1);
  until false;

  for i:= 1 to Length(Result) do
     if IsEol(Result[i]) then
       Result[i]:= ' ';
end;

procedure TecRegExpr.SetExpression(const Value: ecString);
begin
  FExpression:= _MultilineToString(Value); // Alexey: fix to handle #-comments in regex
  ClearRoot;
end;

procedure TecRegExpr.SetModifier(const Index: Integer;
  const Value: boolean);
begin
  if Value then FModifiers := FModifiers or Index
   else FModifiers := FModifiers and not Index;
  ClearRoot;
end;

procedure TecRegExpr.SetModifiers(const Value: Word);
begin
  FModifiers := Value;
  ClearRoot;
end;

procedure TecRegExpr.SetModifierStr(const Value: ecString);
begin
  if (Length(Value) >= 3) and (Value[1] = '(') and (Value[2] = '?') then
    ParseModifiers(@Value[3], Length(Value) - 3, FModifiers);
end;

function TecRegExpr.GetMatchLen(Idx: integer): integer;
begin
  Result := -1;
  if FMatchOK then
    with TreRootNode(FProgRoot) do
      if (idx < FSubExpr.Count) then
        with TreSubExpr(FSubExpr[Idx]) do
          if (FStart <> -1) and (FEnd <> -1) then
            Result := FEnd - FStart;
end;

function TecRegExpr.GetMatchPos(Idx: integer): integer;
begin
  Result := -1;
  if FMatchOK then
    with TreRootNode(FProgRoot) do
      if (idx < FSubExpr.Count) then
        with TreSubExpr(FSubExpr[Idx]) do
          Result := FStart;
end;

function TecRegExpr.GetSubExprMatchCount: integer;
var i: integer;
begin
  Result := -1;
  if FMatchOK then
    with TreRootNode(FProgRoot) do
      for i := 0 to FSubExpr.Count - 1 do
        with TreSubExpr(FSubExpr[i]) do
          if (FStart <> -1) and (FEnd <> -1) then
            Inc(Result);
end;

function TecRegExpr.GetMatch(const InputString: UCString;
  SubIdx: integer): UCString;
begin
  Result := '';
  if FMatchOK then
    with TreRootNode(FProgRoot) do
      if (SubIdx < FSubExpr.Count) then
        with TreSubExpr(FSubExpr[SubIdx]) do
          if (FStart <> -1) and (FEnd <> -1) then
            Result := Copy(InputString, FStart, FEnd - FStart);
end;

function TecRegExpr.Substitute(const InputString, ATemplate: ecString): ecString;
var i: integer;
    C: ecChar;
begin
//  if not FMatchOK then
//   raise Exception.Create('No matched string');
  Result := '';
  i := 1;
  while i <= Length(ATemplate) do
   begin
     if (ATemplate[i] = '\') and (i < Length(ATemplate)) then
      begin
       inc(i);
       if IsDigitChar(ATemplate[i]) then
         Result := Result + GetMatch(InputString, StrToInt(ATemplate[i]))
       else
        begin
         C := ecChar(Ord(GetEscape(ATemplate, i)));
         if C = #0 then C := ATemplate[i];
         Result := Result + C;
        end;
      end else Result := Result + ATemplate[i];
     inc(i);
   end;
end;

procedure TecRegExpr.Assign(Source: TecRegExpr);
begin
  Self.Expression := Source.Expression;
  Self.ModifierMask := Source.ModifierMask;
end;

procedure TecRegExpr.ParseModifiers(const S: PWideChar; Len: integer; var Modifiers: Word); // Alexey
var IsOn : boolean;
    i: integer;
  procedure SetModif(m: integer); inline;
  begin
    if (m and FModifiersStatic) = 0 then
      if IsOn then Modifiers := Modifiers or m
       else Modifiers := Modifiers and not m;
  end;
begin
  IsOn := true;
  for i := 0 to Len-1 do
    case S[i] of
      '-': IsOn := false;
      'i','I': SetModif(MaskModI);
      'r','R': SetModif(MaskModR);
      's','S': SetModif(MaskModS);
      'g','G': SetModif(MaskModG);
      'm','M': SetModif(MaskModM);
      'x','X': SetModif(MaskModX);
    else
      raise Exception.Create(zreUnexpectedModifier);
    end;
end;

// =============================================================================
//  DEBUGGER
// =============================================================================
{$IFDEF RE_DEBUG}
// Fill tree with compiled nodes {debug purpose}
procedure REDebugCompiledBuildTree(RE: TecRegExpr; TV: TTreeView);
  function GetNodeCaption(Node: TRENodeBase): string;
  begin
   if Node.ClassType = TCharSeqNode then
    Result := ecChar(TCharSeqNode(Node).FChar) else
   if Node.ClassType = TCharSetNode then
    begin
      Result := '[ ... ]';
    end else
   if Node.ClassType = TSpecCheckNode then
     Result := '! '+ TSpecCheckNode(Node).FCheckType + ' !' else
   if Node.ClassType = TZeroWidth then
     Result := '0'
   else
     Result := '<' + Node.ClassName + '>';
   Result := IntToStr(Node.FNodeId) + '  ' + Result + Format(' {%d, %d}', [Node.FLoopMin, Node.FLoopMax]);
  end;

  procedure AddNode(Node: TRENodeBase; Prn: TTreeNode);
  var tn: TTreeNode;
      i: integer;
  begin
   tn := TV.Items.AddChild(Prn, GetNodeCaption(Node));
   if Node is TreListNodeBase then
    for i := 0 to TreListNodeBase(Node).FList.Count - 1 do
     AddNode(TRENodeBase(TreListNodeBase(Node).FList[i]), tn);
   if Node is TZeroWidth then
     AddNode(TZeroWidth(Node).FBranch, tn);
  end;
begin
  TV.Items.Clear;
  AddNode(TreRootNode(RE.FProgRoot), nil);
end;
{$ENDIF}

end.

