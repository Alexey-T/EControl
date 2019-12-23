unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonFile: TButton;
    ButtonFindAll: TButton;
    ButtonSpeed: TButton;
    ListBox1: TListBox;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    procedure ButtonFileClick(Sender: TObject);
    procedure ButtonFindAllClick(Sender: TObject);
    procedure ButtonSpeedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    TestStr: UnicodeString;
    procedure DoMsg(const S: string);
    procedure Test_EC(const Subj: Unicodestring; AWithLog: boolean);
    procedure UseFile(const fn: string);
  public

  end;

var
  Form1: TForm1;

implementation

uses
  RegExpr,
  ec_RegExpr;

{$R *.lfm}

const
  Rules: array[0..9] of string = (
    '//.*',
    '(?s)\{.*?\}',
    '(?s)\(\*.*?\*\)',
    '\d+(\.\d+)?([eE][\-\+]?\d+)?',
    '\w+',
    '\#\$[0-9a-fA-F]+',
    '\$[0-9a-fA-F]+',
    '\#[0-9]+',
    '[\-\+/\*\[\]\(\)\.,:;=<>@\^]+',
    '''.*?'''
    );

function _IsSpace(ch: WideChar): boolean; inline;
begin
  case ch of
    ' ', #9, #10, #13:
      Result:= true
    else
      Result:= false;
  end;
end;

procedure TForm1.Test_EC(const Subj: Unicodestring; AWithLog: boolean);
var
  Obj: array[0..Length(Rules)-1] of TecRegExpr;
  NPos, NLen: integer;
  IndexRule, TokenNum, i: integer;
  ch: WideChar;
begin
  for i:= 0 to Length(Rules)-1 do
  begin
    Obj[i]:= TecRegExpr.Create;
    Obj[i].Expression:= Rules[i];
    Obj[i].ModifierI:= false;
    Obj[i].ModifierS:= false; //don't catch all text by .*
    Obj[i].ModifierM:= true; //allow to work with ^$
    Obj[i].ModifierX:= false; //don't ingore spaces
  end;

  NPos:= 1;
  TokenNum:= 0;

  repeat
    if NPos>Length(Subj) then Break;
    NLen:= 1;

    ch:= Subj[NPos];
    if not _IsSpace(ch) then
      for IndexRule:= 0 to Length(Rules)-1 do
      begin
        NLen:= Obj[IndexRule].MatchLength(Subj, NPos);
        if NLen>0 then
        begin
          if AWithLog then
          begin
            Inc(TokenNum);
            DoMsg('['+IntToStr(TokenNum)+'] '+Copy(Subj, NPos, NLen));
          end;
          Break;
        end
        else
          NLen:= 1;
      end;

    Inc(NPos, NLen);
  until false;

  for i:= 0 to Length(Rules)-1 do
    Obj[i].Free;
end;


{ TForm1 }

procedure TForm1.DoMsg(const S: string);
begin
  Listbox1.Items.Add(S);
  Listbox1.ItemIndex:= Listbox1.Items.Count-1;
end;

procedure TForm1.ButtonSpeedClick(Sender: TObject);
var
  t: QWord;
begin
  t:= GetTickCount64;
  Test_EC(TestStr, false);
  t:= GetTickCount64-t;
  DoMsg(Format('Parsing by ec_RegExpr: %d ms', [t]));
end;

procedure TForm1.ButtonFileClick(Sender: TObject);
begin
  with OpenDialog1 do
    if Execute then
      UseFile(FileName);
end;

procedure TForm1.ButtonFindAllClick(Sender: TObject);
begin
  Test_EC(TestStr, true);
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  fn: string;
begin
  fn:= ExtractFileDir(ExtractFileDir(ExtractFileDir(Application.ExeName)))+
    DirectorySeparator+'econtrol'+DirectorySeparator+'ec_syntanal.pas';

  if not FileExists(fn) then
  begin
    Listbox1.Items.Add('Cannot find sample file: '+fn);
    exit;
  end;
  UseFile(fn);
end;

procedure TForm1.UseFile(const fn: string);
var
  L: TStringList;
begin
  L:= TStringList.Create;
  L.LoadFromFile(fn);
  TestStr:= UTF8Decode(L.Text);
  L.Free;

  ListBox1.Items.Add('Test file: '+fn);
  ListBox1.Items.Add('Length: '+IntToStr(Length(TestStr)));
end;

end.

