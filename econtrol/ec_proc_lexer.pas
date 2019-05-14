(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit ec_proc_lexer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ec_SyntAnal,
  ATStringProc;

function DoFindLexerForFilename(LexLib: TecSyntaxManager; FileName: string): TecSyntAnalyzer;
function DoGetLexerFileFilter(an: TecSyntAnalyzer; const AllFilesText: string): string;
function DoGetLexerDefaultExt(an: TecSyntAnalyzer): string;

implementation

function SItemListed(const AItem, AList: string): boolean;
const
  sep=' ';
begin
  if (AItem='') or (AList='') then
    Result:= false
  else
    Result:= Pos(sep+AItem+sep, sep+AList+sep)>0;
end;

{
This finds lexer by Extensions-property of lexer.
It is space-separated items. In lower case.
Items are
- usual extension: "pas" finds "anyname.pas"
- double extension (higher priority): "some.html" finds "dir/myfile.some.html"
  (before lexer HTML finds it)
- full filename: "/name.ext" finds "any/dir/name.ext"
}
function DoFindLexerForFilename(LexLib: TecSyntaxManager; FileName: string): TecSyntAnalyzer;
var
  An: TecSyntAnalyzer;
  fname, ext1, ext2: string;
  i: integer;
begin
  Result:= nil;

  //strip path, lower case
  FileName:= LowerCase(ExtractFileName(FileName));
  fname:= '/'+FileName;

  //usual extension
  ext1:= ExtractFileExt(FileName);
  if SBeginsWith(ext1, '.') then Delete(ext1, 1, 1);

  //double extension
  ext2:= '';
  if ext1<>'' then
  begin
    ext2:= ExtractFileExt(ChangeFileExt(FileName, ''));
    if SBeginsWith(ext2, '.') then Delete(ext2, 1, 1);
    if ext2<>'' then
      ext2:= ext2+'.'+ext1;
  end;

  //find by filename
  for i:= 0 to LexLib.AnalyzerCount-1 do
  begin
    An:= LexLib.Analyzers[i];
    if not An.Internal then
      if SItemListed(fname, An.Extentions) then
        Exit(An);
  end;

  //find by double extension
  if ext2<>'' then
    for i:= 0 to LexLib.AnalyzerCount-1 do
    begin
      An:= LexLib.Analyzers[i];
      if not An.Internal then
        if SItemListed(ext2, An.Extentions) then
          Exit(An);
    end;

  //find by usual extension
  for i:= 0 to LexLib.AnalyzerCount-1 do
  begin
    An:= LexLib.Analyzers[i];
    if not An.Internal then
      if SItemListed(ext1, An.Extentions) then
        Exit(An);
  end;
end;


function DoGetLexerFileFilter(an: TecSyntAnalyzer; const AllFilesText: string): string;
//Get filter-string for TSaveDialog:
//- filter for given lexer
//- and item "all files (*.*)"
var
  L: TStringList;
  i: integer;
begin
  Result:= '';
  L:= TStringList.Create;
  try
    L.LineBreak:= ' ';
    L.Text:= an.Extentions;
    if L.Count=0 then Exit;
    Result:= an.LexerName+' ('+an.Extentions+')|';
    for i:= 0 to L.Count-1 do
      if not SBeginsWith(L[i], '/') then //skip some for eg "Makefile" lexer
        Result:= Result+('*.'+L[i]+';');
    Result:= Result+'|';
  finally
    L.Free;
  end;

  if AllFilesText<>'' then
    Result:= Result+(AllFilesText+'|'+AllFilesMask+'|');
end;

function DoGetLexerDefaultExt(an: TecSyntAnalyzer): string;
var
  n: integer;
begin
  Result:= an.Extentions;
  if SBeginsWith(Result, '/') then exit('');
  n:= Pos(' ', Result);
  if n>0 then
    Delete(Result, n, Maxint);
end;


end.

