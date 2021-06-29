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
  FileUtil,
  ATStringProc_Separator,
  ec_SyntAnal;

type
  TecLexerChooseFunc = function(const Filename: string; Lexers: TStringList): integer of object;
  TecLexerLoadedEvent = procedure(Sender: TObject; ALexer: TecSyntAnalyzer);

type
  { TecLexerList }

  TecLexerList = class(TComponent)
  private
    FList: TFPList;
    FModified: boolean;
    FFolder: string;
    FOnLexerLoaded: TecLexerLoadedEvent;
    function GetLexer(AIndex: integer): TecSyntAnalyzer;
    procedure CheckInited(const AMsg: string);
  public
    //AllowedThreadId: TThreadID;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function LexerCount: integer;
    property Lexers[AIndex: integer]: TecSyntAnalyzer read GetLexer;
    property Modified: boolean read FModified write FModified;
    procedure InitLibrary(const AFolder: string; out AErrorLines: string);
    function AddLexer: TecSyntAnalyzer;
    procedure DeleteLexer(An: TecSyntAnalyzer);
    function FindLexerByFilename(AFilename: string; AChooseFunc: TecLexerChooseFunc): TecSyntAnalyzer;
    function FindLexerByName(const AName: string): TecSyntAnalyzer;
    function FindLexerByFencedName(const AName: string): TecSyntAnalyzer;
    procedure SetSublexersFromString(An: TecSyntAnalyzer; const ALinks: string; ASep: char);
    property OnLexerLoaded: TecLexerLoadedEvent read FOnLexerLoaded write FOnLexerLoaded;
  end;

implementation

function SBeginsWithChar(const S: string; ch: char): boolean; inline;
begin
  Result:= (S<>'') and (S[1]=ch);
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
  FList:= TFPList.Create;
end;

destructor TecLexerList.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

function TecLexerList.GetLexer(AIndex: integer): TecSyntAnalyzer;
begin
  CheckInited('GetLexer');
  Result:= TecSyntAnalyzer(FList[AIndex]);
end;

procedure TecLexerList.CheckInited(const AMsg: string);
begin
  {
  if (AllowedThreadId<>0) then
    if (GetCurrentThreadId<>AllowedThreadId) then
      raise Exception.Create('Too early access to lexer manager in '+AMsg);
      }
end;

procedure TecLexerList.Clear;
var
  an: TecSyntAnalyzer;
  i: integer;
begin
  CheckInited('Clear');
  //don't free objects, only mark them as deleted
  for i:= FList.Count-1 downto 0 do
  begin
    an:= TecSyntAnalyzer(FList[i]);
    an.MarkAsDeleted;
  end;
  FList.Clear;
end;

function TecLexerList.LexerCount: integer;
begin
  CheckInited('LexerCount');
  Result:= FList.Count;
end;

procedure TecLexerList.InitLibrary(const AFolder: string; out AErrorLines: string);
var
  LexName: string;
  L: TStringlist;
  an, anSub: TecSyntAnalyzer;
  i, j: integer;
begin
  Clear;
  FFolder:= AFolder;
  AErrorLines:= '';

  L:= TStringList.Create;
  try
    FindAllFiles(L, FFolder, '*.lcf', false);
    if L.Count=0 then exit;
    L.Sort;

    for i:= 0 to L.Count-1 do
    begin
      an:= AddLexer;
      an.LoadFromFile(L[i]);
      if Assigned(FOnLexerLoaded) then
        FOnLexerLoaded(Self, an);
    end;

    //correct sublexer links
    for i:= 0 to LexerCount-1 do
    begin
      an:= Lexers[i];
      if an.Deleted then Continue;
      for j:= 0 to an.SubAnalyzers.Count-1 do
      begin
        LexName:= an.SubLexerName(j);
        if LexName<>'' then
        begin
          anSub:= FindLexerByName(LexName);
          if Assigned(anSub) then
            an.SubAnalyzers[j].SyntAnalyzer:= anSub
          else
          if an.LexerName<>'HTML' then //ignore the HTML lexer which uses 'VBScript' 2 times
            AErrorLines+= Format('Lexer "%s" needs sub-lexer "%s" which was not installed'#10, [an.LexerName, LexName]);
        end;
      end;
    end;
  finally
    FreeAndNil(L);
  end;
end;

function TecLexerList.AddLexer: TecSyntAnalyzer;
begin
  CheckInited('AddLexer');
  Result:= TecSyntAnalyzer.Create(Self);
  FList.Add(Result);
end;

procedure TecLexerList.DeleteLexer(An: TecSyntAnalyzer);
var
  N: integer;
begin
  CheckInited('DeleteLexer');
  N:= FList.IndexOf(An);
  if N>=0 then
  begin
    //don't free the object! only mark it as deleted
    {
    TObject(FList[N]).Free;
    FList.Delete(N);
    }
    An.MarkAsDeleted;
  end;
end;


function TecLexerList.FindLexerByFilename(AFilename: string;
  AChooseFunc: TecLexerChooseFunc): TecSyntAnalyzer;
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
  Names: TStringList;
  fname, ext1, ext2: string;
  i: integer;
begin
  CheckInited('FindLexerByFilename');
  Result:= nil;

  //strip path, lower case
  AFileName:= ExtractFileName(AFileName);
  fname:= '/'+AFileName;

  //usual extension
  ext1:= LowerCase(ExtractFileExt(AFileName));
  if SBeginsWithChar(ext1, '.') then
    Delete(ext1, 1, 1);

  //double extension
  ext2:= '';
  if ext1<>'' then
  begin
    ext2:= LowerCase(ExtractFileExt(ChangeFileExt(AFileName, '')));
    if SBeginsWithChar(ext2, '.') then
      Delete(ext2, 1, 1);
    if ext2<>'' then
      ext2:= ext2+'.'+ext1;
  end;

  //find by filename
  for i:= 0 to LexerCount-1 do
  begin
    An:= Lexers[i];
    if An.Deleted then Continue;
    if not An.Internal then
      if SItemListed(fname, An.Extentions) then
        Exit(An);
  end;

  //find by double extension
  if ext2<>'' then
    for i:= 0 to LexerCount-1 do
    begin
      An:= Lexers[i];
      if An.Deleted then Continue;
      if not An.Internal then
        if SItemListed(ext2, An.Extentions) then
          Exit(An);
    end;

  //find by usual extension
  Names:= TStringList.Create;
  try
    for i:= 0 to LexerCount-1 do
    begin
      An:= Lexers[i];
      if An.Deleted then Continue;
      if not An.Internal then
        if SItemListed(ext1, An.Extentions) then
          Names.AddObject(An.LexerName, An);
    end;

    if Names.Count=0 then exit;
    Names.Sort;

    if (Names.Count=1) or not Assigned(AChooseFunc) then
      exit(TecSyntAnalyzer(Names.Objects[0]));

    i:= AChooseFunc(AFilename, Names);
    if i>=0 then
      exit(TecSyntAnalyzer(Names.Objects[i]));
  finally
    FreeAndNil(Names);
  end;
end;


function TecLexerList.FindLexerByName(const AName: string): TecSyntAnalyzer;
var
  Lexer: TecSyntAnalyzer;
  i: integer;
begin
  CheckInited('FindLexerByName');
  Result:= nil;
  for i:= 0 to LexerCount-1 do
  begin
    Lexer:= Lexers[i];
    if Lexer.Deleted then Continue;
    if SameText(Lexer.LexerName, AName) then
      exit(Lexer);
  end;
end;

function TecLexerList.FindLexerByFencedName(const AName: string): TecSyntAnalyzer;
//find lexer by name from Markdown fenced block:
// ```php
// ```
var
  S: string;
begin
  case AName of
    'cpp':
      S:= 'C++';
    'csharp':
      S:= 'C#';
    'bash':
      S:= 'Bash script';
    'batch':
      S:= 'Batch files';
    'ini':
      S:= 'Ini files';
    else
      S:= AName;
  end;
  Result:= FindLexerByName(S); //it ignores case
end;


procedure TecLexerList.SetSublexersFromString(An: TecSyntAnalyzer; const ALinks: string; ASep: char);
var
  Sep: TATStringSeparator;
  SItem: string;
  Cnt: Integer;
begin
  CheckInited('SetSublexersFromString');
  Cnt:= 0;
  Sep.Init(ALinks, ASep);
  repeat
    if not Sep.GetItemStr(SItem) then Break;
    if Cnt>=An.SubAnalyzers.Count then Break;
    An.SubAnalyzers[Cnt].SyntAnalyzer:= FindLexerByName(SItem);
    Inc(Cnt);
  until false;
end;


end.

