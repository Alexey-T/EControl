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

unit ec_syntax_format;

{$mode Delphi}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
  ec_syntax_item,
  ec_syntax_collection;

type
  TecVertAlignment = (vaTop, vaCenter, vaBottom);

  TecFormatType = (ftCustomFont, // Any customizing
                   ftFontAttr,   // Except custom font
                   ftColor,      // Any color
                   ftBackGround);// Only background color

  TecBorderLineType = (blNone, blSolid, blDash, blDot, blDashDot, blDashDotDot,
                     blSolid2, blSolid3, blWavyLine, blDouble);

  TecChangeCase = (ccNone, ccUpper, ccLower, ccToggle, ccTitle);

  //Alexey: type must be equal to ATSynEdit.TATTokenKind to make casts
  TecTokenKind = (etkOther, etkComment, etkString);

// *******************************************************************
//  Format for syntax output
// *******************************************************************
  TecFormatFlag = (ffBold, ffItalic, ffUnderline, ffStrikeOut, ffReadOnly,
                 ffHidden, ffFontName, ffFontSize, ffFontCharset, ffVertAlign);
  TecFormatFlags = set of TecFormatFlag;

  TecSyntaxFormat = class(TSyntCollectionItem)
  private
    FIsBlock: Boolean;
    FFont: TFont;
    FBgColor: TColor;
    FVertAlign: TecVertAlignment;
    FFormatType: TecFormatType;
    FHidden: Boolean;
    FBorderTypes: array[0..3] of TecBorderLineType;
    FBorderColors: array[0..3] of TColor;
    FMultiLineBorder: Boolean;
    FReadOnly: Boolean;
    FChangeCase: TecChangeCase;
    FFormatFlags: TecFormatFlags;
    FTokenKind: TecTokenKind;
    procedure SetFont(Value: TFont);
    function GetBorderColor(Index: Integer): TColor;
    function GetBorderType(Index: Integer): TecBorderLineType;
    procedure SetBorderColor(Index: Integer; Value: TColor);
    procedure SetBorderType(Index: Integer;
      const Value: TecBorderLineType);
    function GetHidden: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetItemBaseName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function HasBorder: Boolean;

    procedure ApplyTo(Canvas: TCanvas; AllowChangeFont: Boolean = True);

    function IsEqual(Other: TecSyntaxFormat): Boolean;
    // Merges style above this style
    procedure Merge(Over: TecSyntaxFormat);
    // Save only common properties
    procedure Intersect(Over: TecSyntaxFormat);

    property BorderTypes[Index: integer]: TecBorderLineType read GetBorderType write SetBorderType;
    property BorderColors[Index: integer]: TColor read GetBorderColor write SetBorderColor;
    property IsBlock: Boolean read FIsBlock write FIsBlock;
    property TokenKind: TecTokenKind read FTokenKind write FTokenKind;

  published
    property Font: TFont read FFont write SetFont;
    property BgColor: TColor read FBgColor write FBgColor default clNone;
    property VertAlignment: TecVertAlignment read FVertAlign write FVertAlign default vaCenter;
    property FormatType: TecFormatType read FFormatType write FFormatType default ftFontAttr;
    property Hidden: Boolean read GetHidden write FHidden default False;
    property BorderTypeLeft: TecBorderLineType index 0 read GetBorderType write SetBorderType default blNone;
    property BorderColorLeft: TColor index 0 read GetBorderColor write SetBorderColor default clBlack;
    property BorderTypeTop: TecBorderLineType index 1 read GetBorderType write SetBorderType default blNone;
    property BorderColorTop: TColor index 1 read GetBorderColor write SetBorderColor default clBlack;
    property BorderTypeRight: TecBorderLineType index 2 read GetBorderType write SetBorderType default blNone;
    property BorderColorRight: TColor index 2 read GetBorderColor write SetBorderColor default clBlack;
    property BorderTypeBottom: TecBorderLineType index 3 read GetBorderType write SetBorderType default blNone;
    property BorderColorBottom: TColor index 3 read GetBorderColor write SetBorderColor default clBlack;
    property MultiLineBorder: Boolean read FMultiLineBorder write FMultiLineBorder default False;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property ChangeCase: TecChangeCase read FChangeCase write FChangeCase default ccNone;
    property FormatFlags: TecFormatFlags read FFormatFlags write FFormatFlags
                 default [ffBold, ffItalic, ffUnderline, ffStrikeOut, ffReadOnly,
                          ffHidden, ffFontName, ffFontSize, ffFontCharset, ffVertAlign];
  end;

  TecStylesCollection = class(TSyntCollection)
  private
    function GetItem(Index: integer): TecSyntaxFormat;
  public
    function Synchronize(Source: TecStylesCollection): integer;
    constructor Create;
    function Add: TecSyntaxFormat;
    property Items[Index: integer]: TecSyntaxFormat read GetItem; default;
  end;

implementation

{ TecSyntaxFormat }

constructor TecSyntaxFormat.Create(Collection: TCollection);
var i: integer;
begin
  FIsBlock := False;
  FFont := TFont.Create;
  FFont.Name := 'Courier New';
  FFont.Size := 10;
  FBgColor := clNone;
  FVertAlign := vaCenter;
  FFormatType := ftFontAttr;
  for i := 0 to 3 do
   begin
    FBorderTypes[i] := blNone;
    FBorderColors[i] := clBlack;
   end;
  FFormatFlags := [ffBold, ffItalic, ffUnderline, ffStrikeOut, ffReadOnly,
                   ffHidden, ffFontName, ffFontSize, ffFontCharset, ffVertAlign];
  FTokenKind := etkOther;
  inherited;
  //FFont.OnChange := FontChanged;
end;

destructor TecSyntaxFormat.Destroy;
begin
  FreeAndNil(FFont);
  inherited;
end;

procedure TecSyntaxFormat.AssignTo(Dest: TPersistent);
var i: integer;
begin
  inherited;
  if Dest is TecSyntaxFormat then
   with Dest as TecSyntaxFormat do
    begin
      FBgColor := Self.BgColor;
      FFont.Assign(Self.Font);
      FVertAlign := Self.FVertAlign;
      FIsBlock := Self.FIsBlock;
      FFormatType := Self.FFormatType;
      Hidden := Self.Hidden;
      ReadOnly := Self.ReadOnly;
      MultiLineBorder := Self.MultiLineBorder;
      FChangeCase := Self.ChangeCase;
      for i := 0 to 3 do
       begin
        FBorderTypes[i] := Self.FBorderTypes[i];
        FBorderColors[i] := Self.FBorderColors[i];
       end;
      FFormatFlags := Self.FFormatFlags;
    end;
end;

procedure TecSyntaxFormat.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

function TecSyntaxFormat.GetItemBaseName: string;
begin
  Result := 'Style';
end;

function TecSyntaxFormat.GetBorderColor(Index: Integer): TColor;
begin
  if (Index >= 0) and (Index <= 3) then
    Result := FBorderColors[Index]
  else
    Result := clBlack;
end;

function TecSyntaxFormat.GetBorderType(Index: Integer): TecBorderLineType;
begin
  if (Index >= 0) and (Index <= 3) then
    Result := FBorderTypes[Index]
  else
    Result := blNone;
end;

procedure TecSyntaxFormat.SetBorderColor(Index: Integer;
  Value: TColor);
begin
  if (Index >= 0) and (Index <= 3) then
   begin
    FBorderColors[Index] := Value;
   end;
end;

procedure TecSyntaxFormat.SetBorderType(Index: Integer;
  const Value: TecBorderLineType);
begin
  if (Index >= 0) and (Index <= 3) then
   begin
    FBorderTypes[Index] := Value;
   end;
end;

function TecSyntaxFormat.HasBorder: Boolean;
var i: integer;
begin
  for i := 0 to 3 do
   if FBorderTypes[i] <> blNone then
     begin
      Result := True;
      Exit;
     end;
  Result := False;
end;

procedure TecSyntaxFormat.ApplyTo(Canvas: TCanvas; AllowChangeFont: Boolean);
var fs: TFontStyles;
  procedure SwitchFontFlag(ff: TecFormatFlag; f: TFontStyle);
  begin
    if ff in FormatFlags then
      if f in Font.Style then Include(fs, f)
       else Exclude(fs, f);
  end;
begin
  if not Enabled then Exit;

  if BgColor <> clNone then
    Canvas.Brush.Color := BgColor;

  if FormatType = ftBackGround then Exit else
   begin
     if Font.Color <> clNone then
       Canvas.Font.Color := Font.Color;
     if FormatType <> ftColor then
      begin
       fs := Canvas.Font.Style;
       SwitchFontFlag(ffBold, fsBold);
       SwitchFontFlag(ffItalic, fsItalic);
       SwitchFontFlag(ffUnderline, fsUnderline);
       SwitchFontFlag(ffStrikeOut, fsStrikeOut);
       if Canvas.Font.Style <> fs then
         Canvas.Font.Style := fs;
       if (FormatType = ftCustomFont) and AllowChangeFont then
        begin
          if ffFontName in FormatFlags then
            Canvas.Font.Name := Font.Name;
          if ffFontCharset in FormatFlags then
            Canvas.Font.Charset := Font.Charset;
          if ffFontSize in FormatFlags then
            Canvas.Font.Size := Font.Size;
        end;
      end;
   end;
end;

function TecSyntaxFormat.IsEqual(Other: TecSyntaxFormat): Boolean;
begin
  Result := (BgColor = Other.BgColor) and
            (FormatType = Other.FormatType) and
            (FormatFlags = Other.FormatFlags) and
            (Hidden = Other.Hidden) and
            (ReadOnly = Other.ReadOnly) and
            (ChangeCase = Other.ChangeCase) and
            (VertAlignment = Other.VertAlignment);
  if Result and (FormatType <> ftBackGround) then
    begin
      Result := Font.Color = Other.Font.Color;
      if Result and (FormatType <> ftColor) then
        begin
          Result := Font.Style = Other.Font.Style;
          if Result and (FormatType <> ftFontAttr) then
            begin
              Result := (not (ffFontName in FormatFlags) or
                         (Font.Name = Other.Font.Name))
                        and
                        (not (ffFontSize in FormatFlags) or
                         (Font.Size = Other.Font.Size))
                        and
                        (not (ffFontCharSet in FormatFlags) or
                         (Font.Charset = Other.Font.Charset));
            end;
        end;
    end;
end;

procedure TecSyntaxFormat.Merge(Over: TecSyntaxFormat);
var fs: TFontStyles;
  procedure SwitchFontFlag(ff: TecFormatFlag; f: TFontStyle);
  begin
    if ff in Over.FormatFlags then
      begin
        Include(FFormatFlags, ff);
        if f in Over.Font.Style then Include(fs, f)
         else Exclude(fs, f);
      end;
  end;
begin
  if ffVertAlign in Over.FormatFlags then
    VertAlignment := Over.VertAlignment;
  if ffHidden in Over.FormatFlags then
    Hidden := Over.Hidden;
  if ffReadOnly in Over.FormatFlags then
    ReadOnly := Over.ReadOnly;
  if Over.BgColor <> clNone then
    BgColor := Over.BgColor;
  if Over.ChangeCase <> ccNone then
    ChangeCase := Over.ChangeCase;
  if Over.FormatType <> ftBackGround then
    begin
      if Over.Font.Color <> clNone then
        Font.Color := Over.Font.Color;
      if Over.FormatType <> ftColor then
        begin
          fs := Font.Style;
          SwitchFontFlag(ffBold, fsBold);
          SwitchFontFlag(ffItalic, fsItalic);
          SwitchFontFlag(ffUnderline, fsUnderline);
          SwitchFontFlag(ffStrikeOut, fsStrikeOut);
          Font.Style := fs;
          if Over.FormatType <> ftFontAttr then
            begin
              if ffFontName in Over.FormatFlags then
                Font.Name := Over.Font.Name;
              if ffFontCharset in Over.FormatFlags then
                Font.Charset := Over.Font.Charset;
              if ffFontSize in Over.FormatFlags then
                Font.Size := Over.Font.Size;
            end;
        end;
    end;
  FormatFlags := FormatFlags + Over.FormatFlags;
end;

function TecSyntaxFormat.GetHidden: Boolean;
begin
  Result := FHidden and (ffHidden in FFormatFlags);
end;

procedure TecSyntaxFormat.Intersect(Over: TecSyntaxFormat);
begin
  FormatFlags := Over.FormatFlags * FormatFlags;
  if Over.FormatType < FormatType then
    FormatType := Over.FormatType;

  if (ffVertAlign in FormatFlags) and
     (VertAlignment <> Over.VertAlignment) then
    FormatFlags := FormatFlags - [ffVertAlign];

  if (ffReadOnly in FormatFlags) and
     (ReadOnly <> Over.ReadOnly) then
    FormatFlags := FormatFlags - [ffReadOnly];

  if (ffHidden in FormatFlags) and
     (Hidden <> Over.Hidden) then
    FormatFlags := FormatFlags - [ffHidden];

  if Over.ChangeCase <> ChangeCase then
    ChangeCase := ccNone;

  if BgColor <> Over.BgColor then
    BgColor := clNone;

  if FormatType = ftBackGround then Exit;

  if Font.Color <> Over.Font.Color then
    Font.Color := clNone;

  if FormatType = ftColor then Exit;

  if (ffBold in FormatFlags) and
     ((fsBold in Font.Style) <> (fsBold in Over.Font.Style)) then
    FormatFlags := FormatFlags - [ffBold];

  if (ffItalic in FormatFlags) and
     ((fsItalic in Font.Style) <> (fsItalic in Over.Font.Style)) then
    FormatFlags := FormatFlags - [ffItalic];

  if (ffUnderline in FormatFlags) and
     ((fsUnderline in Font.Style) <> (fsUnderline in Over.Font.Style)) then
    FormatFlags := FormatFlags - [ffUnderline];

  if (ffStrikeOut in FormatFlags) and
     ((fsStrikeOut in Font.Style) <> (fsStrikeOut in Over.Font.Style)) then
    FormatFlags := FormatFlags - [ffStrikeOut];

  if FormatType = ftFontAttr then Exit;

  if (ffFontName in FormatFlags) and
     (not SameText(Font.Name, Over.Font.Name)) then
    FormatFlags := FormatFlags - [ffFontName];

  if (ffFontSize in FormatFlags) and
     (Font.Size <> Over.Font.Size) then
    FormatFlags := FormatFlags - [ffFontSize];

  if (ffFontCharset in FormatFlags) and
     (Font.Charset <> Over.Font.Charset) then
    FormatFlags := FormatFlags - [ffFontCharset];
end;

{ TecStylesCollection }

function TecStylesCollection.Add: TecSyntaxFormat;
begin
  Result := (inherited Add) as TecSyntaxFormat;
end;

constructor TecStylesCollection.Create;
begin
  inherited Create(TecSyntaxFormat);
end;

function TecStylesCollection.GetItem(Index: integer): TecSyntaxFormat;
begin
  Result := (inherited Items[Index]) as TecSyntaxFormat;
end;

function TecStylesCollection.Synchronize(Source: TecStylesCollection): integer;
var j: integer;
    f: TecSyntaxFormat;
begin
  Result := 0;
  for j := 0 to Count - 1 do
   begin
     f := TecSyntaxFormat(Source.ItemByName(Items[j].DisplayName));
     if f <> nil then
      begin
        Inc(Result);
        Items[j].Assign(f);
      end;
   end;
end;

end.
