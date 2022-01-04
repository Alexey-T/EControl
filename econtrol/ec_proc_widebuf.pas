(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit ec_proc_widebuf;

{$mode objfpc}{$H+}
{$Optimization LEVEL3}

interface

uses
  Classes, Math;

function StringList_FindWideBuffer(AList: TStringList; ABuf: PWideChar; ABufLen: integer; out AIndex: integer): boolean;

implementation

//compares String item with WideChar buffer (ASCII string in wide buffer)
function CompareFunc(ABuf: PWideChar; ABufLen: integer; const AStr: AnsiString): integer; inline;
var
  NCmp: integer;
  i: PtrInt;
begin
  //compare 1st char
  NCmp:= Ord(ABuf[0])-Ord(AStr[1]);
  if NCmp<>0 then Exit(NCmp);

  //compare others
  for i:= 1{>0} to Min(ABufLen, Length(AStr))-1 do
  begin
    NCmp:= Ord(ABuf[i])-Ord(AStr[i+1]);
    if NCmp<>0 then Exit(NCmp);
  end;
  Result:= ABufLen-Length(AStr);
end;

function StringList_FindWideBuffer(AList: TStringList; ABuf: PWideChar; ABufLen: integer; out AIndex: integer): boolean;
var
  L, R, I: PtrInt;
  CompareRes: integer;
begin
  Result := false;
  AIndex := -1;
  if not AList.Sorted then
    Raise EListError.Create('Need sorted StringList');

  L := 0;
  R := AList.Count - 1;
  while (L<=R) do
  begin
    I := L + (R - L) div 2;
    CompareRes := CompareFunc(ABuf, ABufLen, AList[I]);
    if (CompareRes>0) then
      L := I+1
    else
    begin
      R := I-1;
      if (CompareRes=0) then begin
         Result := true;
         AIndex := I;
         Exit;
      end;
    end;
  end;
  AIndex := L;
end;

end.
