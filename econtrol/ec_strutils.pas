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
function SkipSpacesAndBreaks(const Source: ecString; var APos: integer): boolean;
function SkipSpacesNoLineBreak(const Source: ecString; var APos: integer): boolean;

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

function SkipSpacesNoLineBreak(const Source: ecString; var APos: integer): boolean;
//returns bool: reached end of buffer
var NLen: integer;
begin
  NLen := Length(Source);
  while (APos <= NLen) and IsSpaceChar(Source[APos]) do
    Inc(APos);
  Result := APos > NLen;
end;

function SkipSpacesAndBreaks(const Source: ecString; var APos: integer): boolean;
//returns bool: reached end of buffer
var NLen: integer;
begin
  NLen := Length(Source);
  while (APos <= NLen) and IsSpaceOrBreakChar(Source[APos]) do
    Inc(APos);
  Result := APos > NLen;
end;

end.
