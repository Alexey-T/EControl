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

{$mode Delphi}{$H+}

interface

uses
  SysUtils, Classes,
  ec_syntax_item;

type
  TSyntItemChanged = procedure(Sender: TCollection; Item: TSyntCollectionItem) of object;

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
