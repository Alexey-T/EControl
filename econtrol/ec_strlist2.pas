(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit ec_strlist2;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils;

type
  TStringList_ForBuf = class(TStringList)
  private
    function DoCompareBuf(ABuf: PChar; ALen: integer; const AStr2: string): PtrInt;
  public
    function FindBuffer(ABuf: PChar; ALen: integer; out AIndex: integer): boolean;
  end;

implementation

uses
  RtlConsts;

{ TStringList_ForBuf }

function TStringList_ForBuf.DoCompareBuf(ABuf: PChar; ALen: integer; const AStr2: string): PtrInt;
begin
  if CaseSensitive then
  begin
    result:= strlcomp(ABuf, PChar(AStr2), ALen);
  end else
  begin
    result:= strlicomp(ABuf, PChar(AStr2), ALen);
  end;
end;

function TStringList_ForBuf.FindBuffer(ABuf: PChar; ALen: integer; out AIndex: integer): boolean;
var
  L, R, I: Integer;
  CompareRes: PtrInt;
begin
  Result := false;
  AIndex:=-1;
  if Not Sorted then
    Raise EListError.Create(SErrFindNeedsSortedList);
  // Use binary search.
  L := 0;
  R := Count - 1;
  while (L<=R) do
  begin
    I := L + (R - L) div 2;
    CompareRes := DoCompareBuf(ABuf, ALen, Get(I));
      //optim! replace Get(I) with Flist^[I].FString
    if (CompareRes>0) then
      L := I+1
    else begin
      R := I-1;
      if (CompareRes=0) then begin
         Result := true;
         if (Duplicates<>dupAccept) then
            L := I; // forces end of while loop
      end;
    end;
  end;
  AIndex := L;
end;

end.

