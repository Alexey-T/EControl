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

unit ec_syntax_collection;

{$mode delphi}

interface

uses
  SysUtils, Classes;

type
  TSyntCollectionItem = class(TCollectionItem)
  private
    FName: string;
    FEnabled: Boolean;
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetItemBaseName: string; virtual;
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
    function GetIsInvalid: Boolean; virtual;
  public
    constructor Create(Collection: TCollection); override;
    property IsInvalid: Boolean read GetIsInvalid;
    procedure Loaded; virtual;
  published
    property DisplayName;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

type
  TSyntItemChanged = procedure(Sender: TCollection; Item: TSyntCollectionItem) of object;

type
  { TSyntCollection }

  TSyntCollection = class(TCollection)
  private
    FSyntOwner: TPersistent;
    FOnChange: TSyntItemChanged;
    function GetItems(Index: integer): TSyntCollectionItem;
  protected
    procedure Update(Item: TCollectionItem); override;
    function  GetOwner: TPersistent; override;
  public
    constructor Create(ItemClass: TCollectionItemClass);
    function ItemByName(const AName: string): TSyntCollectionItem;
    function ValidItem(Item: TSyntCollectionItem): Boolean;
    function GetUniqueName(const Base: string): string;
    function IndexOf(const AItem: TCollectionItem): integer;
    procedure Loaded;

    property SyntOwner: TPersistent read FSyntOwner write FSyntOwner;
    property Items[Index: integer]: TSyntCollectionItem read GetItems; default;
    property OnChange: TSyntItemChanged read FOnChange write FOnChange;
  end;

implementation

{ TSyntCollectionItem }

procedure TSyntCollectionItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TSyntCollectionItem then
   begin
    (Dest as TSyntCollectionItem).DisplayName := DisplayName;
    (Dest as TSyntCollectionItem).Enabled := Enabled;
   end;
end;

constructor TSyntCollectionItem.Create(Collection: TCollection);
var NewName: string;
    n: integer;
begin
  inherited;
  FEnabled := True;
  if Collection = nil then Exit;
  n := 1;
  repeat
    NewName  := GetItemBaseName + ' ' + IntToStr(n);
    inc(n);
  until TSyntCollection(Collection).ItemByName(NewName) = nil;
  FName := NewName;
end;

function TSyntCollectionItem.GetDisplayName: string;
begin
  Result := FName;
end;

function TSyntCollectionItem.GetIsInvalid: Boolean;
begin
  Result := False;
end;

function TSyntCollectionItem.GetItemBaseName: string;
begin
  Result := 'Item';
end;

procedure TSyntCollectionItem.Loaded;
begin

end;

procedure TSyntCollectionItem.SetDisplayName(const Value: string);
var i: integer;
begin
  if Collection <> nil then
  for i := 0 to Collection.Count - 1 do
   if Collection.Items[i] <> Self then
    if Collection.Items[i].DisplayName = Value then
      Exit;
  FName := Value;
end;

procedure TSyntCollectionItem.SetEnabled(const Value: Boolean);
begin
  if FEnabled <> Value then
   begin
     FEnabled := Value;
     Changed(False);
   end;
end;


{ TSyntCollection }

constructor TSyntCollection.Create(ItemClass: TCollectionItemClass);
begin
  if not ItemClass.InheritsFrom(TSyntCollectionItem) then
   raise Exception.Create('Allow only TSyntCollectionItem Class');
  inherited;
end;

function TSyntCollection.GetItems(Index: integer): TSyntCollectionItem;
begin
  Result := (inherited Items[Index]) as TSyntCollectionItem;
end;

function TSyntCollection.GetOwner: TPersistent;
begin
  Result := FSyntOwner;
end;

function TSyntCollection.GetUniqueName(const Base: string): string;
var n: integer;
begin
  Result := Base;
  n := 0;
  while ItemByName(Result) <> nil do
   begin
    Inc(n);
    Result := Base + IntToStr(n);
   end;
end;

function TSyntCollection.IndexOf(const AItem: TCollectionItem): integer;
var
  i: integer;
begin
  for i := 0 to Count-1 do
    if inherited Items[i] = AItem then
      Exit(i);
  Result := -1;
end;

function TSyntCollection.ItemByName(const AName: string): TSyntCollectionItem;
var i: integer;
begin
  for i := 0 to Count - 1 do
   if Items[i].DisplayName = AName then
    begin
      Result := Items[i] as TSyntCollectionItem;
      Exit;
    end;
  Result := nil;
end;

procedure TSyntCollection.Loaded;
var i: integer;
begin
  for i := 0 to Count - 1 do
   Items[i].Loaded;
end;

procedure TSyntCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then FOnChange(Self, TSyntCollectionItem(Item));
end;

function TSyntCollection.ValidItem(Item: TSyntCollectionItem): Boolean;
var i: integer;
begin
  Result := True;
  if Item <> nil then
   for i := 0 to Count - 1 do
    if Items[i] = Item then Exit;
  Result := False;
end;

end.
