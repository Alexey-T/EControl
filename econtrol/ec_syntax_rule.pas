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

unit ec_syntax_rule;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Contnrs;

type
  TParserRuleItem = class;
  TParserRule = class;
  TParserRuleBranch = class;

  TParserItemType = (itTerminal,           // "aaa"
                     itTerminalNoCase,     // 'aaa'
                     itTokenRule,          // <aaa>
                     itParserRule);        //  aaa

  TParserRuleItem = class
  private
    FItemType: TParserItemType;
    FTerminal: string;
    FTokenType: integer;
    FParserRule: TParserRule;
    FBranch: TParserRuleBranch;
    FRepMin: integer;
    FRepMax: integer;
    FOwnRule: Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function IsValid: Boolean;

    property ItemType: TParserItemType read FItemType write FItemType;
    property Terminal: string read FTerminal write FTerminal;
    property TokenType: integer read FTokenType write FTokenType;
    property ParserRule: TParserRule read FParserRule write FParserRule;
    property Branch: TParserRuleBranch read FBranch write FBranch;
    property RepMin: integer read FRepMin write FRepMin;
    property RepMax: integer read FRepMax write FRepMax;
    property IsSubRule: Boolean read FOwnRule write FOwnRule;
  end;

  TParserRuleBranch = class
  private
    FItems: TFPObjectList;
    FRule: TParserRule;
    function GetCount: integer;
    function GetItems(Index: integer): TParserRuleItem;
  public
    constructor Create;
    destructor Destroy; override;
    function IsValid: Boolean;

    property Count: integer read GetCount;
    property ItemsList: TFPObjectList read FItems;
    property Items[Index: integer]: TParserRuleItem read GetItems;
    property Rule: TParserRule read FRule write FRule;
  end;

  TParserRule = class
  private
    FBranches: TFPObjectList;
    FName: string;
    FIndex: integer;
    function GetCount: integer;
    function GetBranches(Index: integer): TParserRuleBranch;
  public
    constructor Create;
    destructor Destroy; override;
    function IsValid: Boolean;

    property Count: integer read GetCount;
    property BranchesList: TFPObjectList read FBranches;
    property Branches[Index: integer]: TParserRuleBranch read GetBranches;
    property Name: string read FName write FName;
    property Index: integer read FIndex write FIndex;
  end;

implementation

{ TParserRuleItem }

constructor TParserRuleItem.Create;
begin
  inherited Create;
  FTokenType := -1;
  FRepMin := 1;
  FRepMax := 1;
  FOwnRule := False;
end;

destructor TParserRuleItem.Destroy;
begin
  if FOwnRule then
    FParserRule.Free;
  inherited;
end;

function TParserRuleItem.IsValid: Boolean;
begin
  case FItemType of
    itTerminal,
    itTerminalNoCase: Result := FTerminal <> '';
    itTokenRule: Result := FTokenType <> -1;
    itParserRule: Result := FParserRule <> nil;
    else Result := False;
  end;
end;

{ TParserRuleBranch }

constructor TParserRuleBranch.Create;
begin
  inherited Create;
  FItems := TFPObjectList.Create;
end;

destructor TParserRuleBranch.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TParserRuleBranch.GetCount: integer;
begin
  Result := FItems.Count;
end;

function TParserRuleBranch.GetItems(Index: integer): TParserRuleItem;
begin
  Result := TParserRuleItem(FItems[Index]);
end;

function TParserRuleBranch.IsValid: Boolean;
var i: integer;
begin
  for i := 0 to FItems.Count - 1 do
   if not Items[i].IsValid then
     begin
      Result := False;
      Exit;
     end;
  Result := True;
end;

{ TParserRule }

constructor TParserRule.Create;
begin
  inherited Create;
  FBranches := TFPObjectList.Create;
end;

destructor TParserRule.Destroy;
begin
  FBranches.Free;
  inherited;
end;

function TParserRule.GetBranches(Index: integer): TParserRuleBranch;
begin
  Result := TParserRuleBranch(FBranches[Index]);
end;

function TParserRule.GetCount: integer;
begin
  Result := FBranches.Count;
end;

function TParserRule.IsValid: Boolean;
var i: integer;
begin
  for i := 0 to Count - 1 do
   if not Branches[i].IsValid then
    begin
     Result := False;
     Exit;
    end;
  Result := (Count > 1) or (Count = 1) and (Branches[0].Count > 0);
end;

end.
