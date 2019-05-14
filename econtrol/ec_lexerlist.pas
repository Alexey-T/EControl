(*
This Source Code Form is subject to the terms of the Mozilla Public
License, v. 2.0. If a copy of the MPL was not distributed with this
file, You can obtain one at http://mozilla.org/MPL/2.0/.

Copyright (c) Alexey Torgashin
*)
unit ec_LexerList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  ec_SyntAnal;

type
  { TecLexerList }

  TecLexerList = class(TComponent)
  private
    FList: TList;
    FModified: boolean;
    function GetLexer(AIndex: integer): TecSyntAnalyzer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function LexerCount: integer;
    property Lexers[AIndex: integer]: TecSyntAnalyzer read GetLexer;
    property Modified: boolean read FModified write FModified;
    function AddLexer: TecSyntAnalyzer;
    procedure DeleteLexer(An: TecSyntAnalyzer);
    function FindLexerByFilename(AFilename: string): TecSyntAnalyzer;
    function FindLexerByName(const AName: string): TecSyntAnalyzer;
    procedure SetSublexersFromString(An: TecSyntAnalyzer; const ALinks: string; ASep: char);
  end;

implementation

function SBeginsWith(const S, SubStr: string): boolean;
begin
  Result:= (SubStr<>'') and (Copy(S, 1, Length(SubStr))=SubStr);
end;

function SGetItem(var S: string; const sep: Char = ','): string;
var
  i: integer;
begin
  i:= Pos(sep, s);
  if i=0 then i:= MaxInt;
  Result:= Copy(s, 1, i-1);
  Delete(s, 1, i);
end;

function SItemListed(const AItem, AList: string): boolean;
const
  sep=' ';
begin
  if (AItem='') or (AList='') then
    Result:= false
  else
    Result:= Pos(sep+AItem+sep, sep+AList+sep)>0;
end;

{ TecLexerList }

constructor TecLexerList.Create(AOwner: TComponent);
begin
  inherited;
  FList:= TList.Create;
end;

destructor TecLexerList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TecLexerList.GetLexer(AIndex: integer): TecSyntAnalyzer;
begin
  Result:= TecSyntAnalyzer(FList[AIndex]);
end;

procedure TecLexerList.Clear;
var
  i: integer;
begin
  for i:= FList.Count-1 downto 0 do
    TObject(FList[i]).Free;
  FList.Clear;
end;

function TecLexerList.LexerCount: integer;
begin
  Result:= FList.Count;
end;

function TecLexerList.AddLexer: TecSyntAnalyzer;
begin
  Result:= TecSyntAnalyzer.Create(Self);
  FList.Add(Result);
end;

procedure TecLexerList.DeleteLexer(An: TecSyntAnalyzer);
var
  N: integer;
begin
  N:= FList.IndexOf(An);
  if N>=0 then
  begin
    TObject(FList[N]).Free;
    FList.Delete(N);
  end;
end;


function TecLexerList.FindLexerByFilename(AFilename: string): TecSyntAnalyzer;
{
This finds lexer by Extensions-property of lexer.
It is space-separated items. In lower case.
Items are
- usual extension: "pas" finds "anyname.pas"
- double extension (higher priority): "some.html" finds "dir/myfile.some.html"
  (before lexer HTML finds it)
- full filename: "/name.ext" finds "any/dir/name.ext"
}
var
  An: TecSyntAnalyzer;
  fname, ext1, ext2: string;
  i: integer;
begin
  Result:= nil;

  //strip path, lower case
  AFileName:= LowerCase(ExtractFileName(AFileName));
  fname:= '/'+AFileName;

  //usual extension
  ext1:= ExtractFileExt(AFileName);
  if SBeginsWith(ext1, '.') then Delete(ext1, 1, 1);

  //double extension
  ext2:= '';
  if ext1<>'' then
  begin
    ext2:= ExtractFileExt(ChangeFileExt(AFileName, ''));
    if SBeginsWith(ext2, '.') then Delete(ext2, 1, 1);
    if ext2<>'' then
      ext2:= ext2+'.'+ext1;
  end;

  //find by filename
  for i:= 0 to LexerCount-1 do
  begin
    An:= Lexers[i];
    if not An.Internal then
      if SItemListed(fname, An.Extentions) then
        Exit(An);
  end;

  //find by double extension
  if ext2<>'' then
    for i:= 0 to LexerCount-1 do
    begin
      An:= Lexers[i];
      if not An.Internal then
        if SItemListed(ext2, An.Extentions) then
          Exit(An);
    end;

  //find by usual extension
  for i:= 0 to LexerCount-1 do
  begin
    An:= Lexers[i];
    if not An.Internal then
      if SItemListed(ext1, An.Extentions) then
        Exit(An);
  end;
end;


function TecLexerList.FindLexerByName(const AName: string): TecSyntAnalyzer;
var
  Lexer: TecSyntAnalyzer;
  i: integer;
begin
  Result:= nil;
  for i:= 0 to LexerCount-1 do
  begin
    Lexer:= Lexers[i];
    if SameText(Lexer.LexerName, AName) then
      exit(Lexer);
  end;
end;


procedure TecLexerList.SetSublexersFromString(An: TecSyntAnalyzer; const ALinks: string; ASep: char);
var
  S, SItem: string;
  Cnt: Integer;
begin
  S:= ALinks;
  Cnt:= 0;
  repeat
    SItem:= SGetItem(S, ASep);
    if Cnt>=An.SubAnalyzers.Count then Break;
    An.SubAnalyzers[Cnt].SyntAnalyzer:= FindLexerByName(SItem);
    Inc(Cnt);
  until false;
end;


end.

