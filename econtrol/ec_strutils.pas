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

unit ec_StrUtils;

interface

uses
  SysUtils,
  ATSynEdit_UnicodeData;

type
  ecString = UnicodeString;
  ecChar = WideChar;
  UCString = UnicodeString;
  UCChar = WideChar;

function IsDigitChar(C: UCChar): Boolean; inline;
function IsHexDigitChar(C: UCChar): Boolean;
function IsLineBreakChar(c: UCChar): Boolean;
function IsWordChar(c: UCChar): Boolean; inline;
function IsSpaceChar(c: UCChar): Boolean; inline;
function IsSpaceOrBreakChar(c: UCChar): Boolean; inline;
//function IsAlphaChar(c: UCChar): Boolean; inline;
function IsIdentChar(C: UCChar): Boolean; inline;
function IsIdentDigitChar(C: UCChar): Boolean; inline;
//function IsIdentLetterChar(C: UCChar): Boolean; inline;
function IsWordBreak(aPos: integer; const Text: UCString): Boolean;

function ecUpCase(C: UCChar): UCChar; inline;
procedure CharToUpCase(var C: UCChar); inline;
function SkipSpacesAndBreaks(const Source: ecString; var APos: integer): integer;
function SkipSpacesNoLineBreak(const Source: ecString; var APos: integer): integer;

{
function ecEncodeString(const S: string): string;
function ecDecodeString(const S: string): string;
}

implementation

function IsSpaceChar(c: UCChar): Boolean; inline; // Alexey
// change since original EControl: it don't catch line-breaks
begin
  case c of
    #9,
    ' ':
      Result := True;
    else
      Result := False;
  end;
end;

function IsSpaceOrBreakChar(c: UCChar): Boolean; inline; // Alexey
// it catches line-breaks
begin
  case c of
    #9,
    ' ',
    #10,
    #13:
      Result := True;
    else
      Result := False;
  end;
end;

function IsLineBreakChar(c: UCChar): Boolean;
begin
   case C of
     #$000A, #$000D,
     #$2028, #$2029, #$0085:
       Result := True;
     else
       Result := False;
   end;
end;

function IsDigitChar(C: UCChar): Boolean; inline;
begin
  Result := (C>='0') and (C<='9');
end;

function IsHexDigitChar(C: UCChar): Boolean;
begin
  case C of
    '0'..'9',
    'a'..'f',
    'A'..'F':
      Result := true;
    else
      Result := false;
  end;
end;

function IsWordChar(C: UCChar): Boolean; inline;
begin
  // bit 7 in value: is word char
  Result := CharCategoryArray[Ord(C)] and 128 <> 0;
end;

{
function IsAlphaChar(c: UCChar): Boolean; inline;
begin
  case C of
    'a' .. 'z',
    'A' .. 'Z':
      Result := true
    else
      Result := false;
  end;
end;
}

function IsIdentChar(C: UCChar): Boolean; inline;
begin
  case C of
    'a' .. 'z',
    'A' .. 'Z',
    '0' .. '9',
    '_':
      Result := true
    else
      Result := false;
  end;
end;

{
function IsIdentLetterChar(C: UCChar): Boolean; inline;
begin
  case C of
    'a' .. 'z',
    'A' .. 'Z',
    '_':
      Result := true
    else
      Result := false;
  end;
end;
}

function IsIdentDigitChar(C: UCChar): Boolean; inline;
begin
  Result := (C >= '0') and (C <= '9');
end;

function IsWordBreak(aPos: integer; const Text: UCString): Boolean;
begin
  Result := (aPos = 1) or (aPos > Length(Text)) or
            (IsWordChar(Text[aPos]) xor IsWordChar(Text[aPos - 1]));
end;

function ecUpCase(C: UCChar): UCChar; inline;
// Alexey: to be fast, ignored Unicode here
begin
  Result := C;
  CharToUpCase(Result);
end;

procedure CharToUpCase(var C: UCChar); inline;
begin
  if (C >= 'a') and (C <= 'z') then
    Dec(C, 32)
  else
  //for Russian chars (needed for OneC_1C lexer, it's case insensitive)
  if (Ord(C) >= $430) and (Ord(C) <= $44F) then
    Dec(C, 32);
end;

function SkipSpacesNoLineBreak(const Source: ecString; var APos: integer): integer;
var N: integer;
begin
  Result := 0;
  N := Length(Source);
  while (APos <= N) and IsSpaceChar(Source[APos]) do // Alexey
    inc(APos);
  if APos > N then Result := -1;
end;

function SkipSpacesAndBreaks(const Source: ecString; var APos: integer): integer;
var N: integer;
begin
  Result := 0;
  N := Length(Source);
  while (APos <= N) and IsSpaceOrBreakChar(Source[APos]) do // Alexey
   begin
    if Source[APos] = #10 then inc(Result);
    inc(APos);
   end;
  if APos > N then Result := -1;
end;

function ecEncodeString(const S: string): string;
var I, L, K: integer;
begin
  Result := '';
  L := Length(S);
  I := 1;
  while I <= L do
    if (S[I] >= ' ') and (S[I] <> '''') then
     begin
       K := I;
       repeat
         Inc(I)
       until (I > L) or (S[I] < ' ') or (S[I] = '''');
       Result := Result + '''' + Copy(S, K, I - K) + '''';
     end else
     begin
      Result := Result + '#' + IntToStr(Ord(S[I]));
      Inc(I);
     end;
end;

function ecDecodeString(const S: string): string;
var I, L, K: integer;
begin
  Result := '';
  L := Length(S);
  I := 1;
  while I <= L do
   if S[I] = '''' then
     begin
       K := I;
       repeat
         Inc(I);
       until (I > L) or (S[i] = '''');
       Result := Result + Copy(S, K + 1, I - K - 1);
       Inc(I);
     end else
   if S[I] = '#' then
     begin
       K := I + 1;
       repeat
         Inc(I)
       until (I > L) or not IsIdentDigitChar(S[I]);
       if (K = I) or ((I - K) > 3) then
         raise Exception.Create('Invalid character code');
       Result := Result + Chr(StrToInt(Copy(S, K, I - K)));
     end else Exit;
//   else raise Exception.Create('Invalid property data');
end;


end.
