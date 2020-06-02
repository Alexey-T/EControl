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

unit ec_syntax_item;

{$mode Delphi}{$H+}

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

implementation

uses
  ec_syntax_collection;

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

end.
   
