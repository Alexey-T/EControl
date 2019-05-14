{ *************************************************************************** }
{                                                                             }
{ EControl Syntax Editor SDK                                                  }
{                                                                             }
{ Copyright (c) 2004 - 2015 EControl Ltd., Zaharov Michael                    }
{     www.econtrol.ru                                                         }
{     support@econtrol.ru                                                     }
{ Ported to Lazarus:                                                          }
{     Alexey Torgashin, UVviewsoft.com                                        }
{                                                                             }
{ *************************************************************************** }

{$mode delphi}

unit ec_SyntAnal;

interface

uses
  Classes, Graphics, Controls, ExtCtrls,
  Contnrs,
  LazUTF8Classes, //TFileStreamUTF8
  ec_RegExpr,
  ec_StrUtils,
  ec_Lists,
  ec_SyntGramma,
  ATStringProc_TextBuffer,
  ec_proc_StreamComponent;

type
  IecSyntClient = interface
    ['{045EAD6D-5584-4A60-849E-6B8994AA5B8F}']
    procedure FormatChanged; // Lexer properties changed (update without clear)
    procedure Finished;      // Compleat analysis
  end;

  TecLineBreakPos = (lbTop, lbBottom);
  TecLineBreakBound = set of TecLineBreakPos; // for user blocks
  TecVertAlignment = (vaTop, vaCenter, vaBottom);
  TecFormatType = (ftCustomFont, // Any customizing
                   ftFontAttr,   // Except custom font
                   ftColor,      // Any color
                   ftBackGround);// Only background color

  TecSyntAnalyzer       = class;
  TecParserResults      = class;
  TecClientSyntAnalyzer = class;
  TecTagBlockCondition  = class;
  TecSyntaxManager      = class;
  TecSyntaxFormat       = class;
  TecSubAnalyzerRule    = class;
  TecTextRange        = class;

  TOnMatchToken = procedure(Sender: TObject; Client: TecParserResults;
      const Text: ecString; APos: integer; var MatchLen: integer) of object;
  TOnBlockCheck = procedure(Sender: TObject; Client: TecClientSyntAnalyzer;
      const Text: ecString; var RefIdx: integer; var Accept: Boolean) of object;

  TBoundDefEvent = procedure(Sender: TecClientSyntAnalyzer; Range: TecTextRange; var sIdx, eIdx: integer) of object;

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
    procedure Loaded; virtual;
    function GetIsInvalid: Boolean; virtual;
  public
    constructor Create(Collection: TCollection); override;
    property IsInvalid: Boolean read GetIsInvalid;
  published
    property DisplayName;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;

  TSyntItemChanged = procedure(Sender: TCollection; Item: TSyntCollectionItem) of object;

  TSyntCollection = class(TCollection)
  private
    FSyntOwner: TecSyntAnalyzer;
    FOnChange: TSyntItemChanged;
    function GetItems(Index: integer): TSyntCollectionItem;
  protected
    procedure Update(Item: TCollectionItem); override;
    function  GetOwner: TPersistent; override;
    procedure Loaded;
  public
    constructor Create(ItemClass: TCollectionItemClass);
    function ItemByName(const AName: string): TSyntCollectionItem;
    function ValidItem(Item: TSyntCollectionItem): Boolean;
    function GetUniqueName(const Base: string): string;

    property SyntOwner: TecSyntAnalyzer read FSyntOwner write FSyntOwner;
    property Items[Index: integer]: TSyntCollectionItem read GetItems; default;
    property OnChange: TSyntItemChanged read FOnChange write FOnChange;
  end;

  TRuleCollectionItem = class(TSyntCollectionItem)
  private
    FStyleName: string;
    FBlockName: string;
    FFormat: TecSyntaxFormat;
    FBlock: TecTagBlockCondition;
    FStrictParent: Boolean;
    FNotParent: Boolean;
    FAlwaysEnabled: Boolean;
    FStatesAbsent: integer;
    FStatesAdd: integer;
    FStatesRemove: integer;
    FStatesPresent: integer;
    function GetStyleName: string;
    procedure SetStyleName(const Value: string);
    function GetBlockName: string;
    procedure SetBlockName(const Value: string);
    procedure SetNotParent(const Value: Boolean);
    procedure SetStrictParent(const Value: Boolean);
    procedure SetAlwaysEnabled(const Value: Boolean);
    function GetSyntOwner: TecSyntAnalyzer;
    procedure SetStatesAdd(const Value: integer);
    procedure SetStatesAbsent(const Value: integer);
    procedure SetStatesRemove(const Value: integer);
    procedure SetStatesPresent(const Value: integer);
  protected
    function ValidStyleName(const AStyleName: string; AStyle: TecSyntaxFormat): string;
    function ValidSetStyle(const AStyleName: string; var AStyleField: string; var AStyle: TecSyntaxFormat): string;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Loaded; override;
  public
    property Style: TecSyntaxFormat read FFormat write FFormat;
    property Block: TecTagBlockCondition read FBlock write FBlock;
    property SyntOwner: TecSyntAnalyzer read GetSyntOwner;
  published
    property StyleName: string read GetStyleName write SetStyleName;
    property BlockName: string read GetBlockName write SetBlockName;
    property StrictParent: Boolean read FStrictParent write SetStrictParent default False;
    property NotParent: Boolean read FNotParent write SetNotParent default False;
    property AlwaysEnabled: Boolean read FAlwaysEnabled write SetAlwaysEnabled default False;
    property StatesAdd: integer read FStatesAdd write SetStatesAdd default 0;
    property StatesRemove: integer read FStatesRemove write SetStatesRemove default 0;
    property StatesPresent: integer read FStatesPresent write SetStatesPresent default 0;
    property StatesAbsent: integer read FStatesAbsent write SetStatesAbsent default 0;
  end;

// *******************************************************************
//  Format for syntax output
// *******************************************************************
  TecBorderLineType = (blNone, blSolid, blDash, blDot, blDashDot, blDashDotDot,
                     blSolid2, blSolid3, blWavyLine, blDouble);
  TecFormatFlag = (ffBold, ffItalic, ffUnderline, ffStrikeOut, ffReadOnly,
                 ffHidden, ffFontName, ffFontSize, ffFontCharset, ffVertAlign);
  TecFormatFlags = set of TecFormatFlag;

  TecChangeCase = (ccNone, ccUpper, ccLower, ccToggle, ccTitle);

  TecSyntaxFormat = class(TSyntCollectionItem)
  private
    FIsBlock: Boolean;
    FFont: TFont;
    FBgColor: TColor;
    FVertAlign: TecVertAlignment;
    FFormatType: TecFormatType;
    FOnChange: TNotifyEvent;
    FHidden: Boolean;
    FBorderTypes: array[0..3] of TecBorderLineType;
    FBorderColors: array[0..3] of TColor;
    FMultiLineBorder: Boolean;
    FReadOnly: Boolean;
    FChangeCase: TecChangeCase;
    FFormatFlags: TecFormatFlags;
    procedure SetFont(const Value: TFont);
    procedure SetBgColor(const Value: TColor);
    procedure FontChanged(Sender: TObject);
    procedure SetVertAlign(const Value: TecVertAlignment);
    procedure SetFormatType(const Value: TecFormatType);
    procedure SetHidden(const Value: Boolean);
    function GetBorderColor(Index: Integer): TColor;
    function GetBorderType(Index: Integer): TecBorderLineType;
    procedure SetBorderColor(Index: Integer; const Value: TColor);
    procedure SetBorderType(Index: Integer;
      const Value: TecBorderLineType);
    procedure SetMultiLineBorder(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetChangeCase(const Value: TecChangeCase);
    procedure SetFormatFlags(const Value: TecFormatFlags);
    function GetHidden: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetItemBaseName: string; override;
    procedure Change; dynamic;
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

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property BorderTypes[Index: integer]: TecBorderLineType read GetBorderType write SetBorderType;
    property BorderColors[Index: integer]: TColor read GetBorderColor write SetBorderColor;
  published
    property Font: TFont read FFont write SetFont;
    property BgColor: TColor read FBgColor write SetBgColor default clNone;
    property VertAlignment: TecVertAlignment read FVertAlign write SetVertAlign default vaCenter;
    property FormatType: TecFormatType read FFormatType write SetFormatType default ftFontAttr;
    property Hidden: Boolean read GetHidden write SetHidden default False;
    property BorderTypeLeft: TecBorderLineType index 0 read GetBorderType write SetBorderType default blNone;
    property BorderColorLeft: TColor index 0 read GetBorderColor write SetBorderColor default clBlack;
    property BorderTypeTop: TecBorderLineType index 1 read GetBorderType write SetBorderType default blNone;
    property BorderColorTop: TColor index 1 read GetBorderColor write SetBorderColor default clBlack;
    property BorderTypeRight: TecBorderLineType index 2 read GetBorderType write SetBorderType default blNone;
    property BorderColorRight: TColor index 2 read GetBorderColor write SetBorderColor default clBlack;
    property BorderTypeBottom: TecBorderLineType index 3 read GetBorderType write SetBorderType default blNone;
    property BorderColorBottom: TColor index 3 read GetBorderColor write SetBorderColor default clBlack;
    property MultiLineBorder: Boolean read FMultiLineBorder write SetMultiLineBorder default False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property ChangeCase: TecChangeCase read FChangeCase write SetChangeCase default ccNone;
    property FormatFlags: TecFormatFlags read FFormatFlags write SetFormatFlags
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

// *******************************************************************
// description classes of text contents
// *******************************************************************

  { TecSyntToken }

  TecSyntToken = record
  private
    function GetStyle: TecSyntaxFormat;
  public
    Range: TRange;
    TokenType: integer;
    Rule: TRuleCollectionItem;
    constructor Create(ARule: TRuleCollectionItem;
      AStartPos, AEndPos: integer;
      const APointStart, APointEnd: TPoint);
    function GetStr(const Source: ecString): ecString;
    property Style: TecSyntaxFormat read GetStyle;
    class operator =(const A,B: TecSyntToken): boolean;
  end;

  TecTextRange = class(TSortedItem)
  private
    FCondIndex: integer;
    FEndCondIndex: integer;
    function GetLevel: integer;
    function GetIsClosed: Boolean;
  protected
    function GetKey: integer; override;
  public
    StartPos: integer;
    StartIdx, EndIdx: integer;
    IdentIdx: integer;
    Rule: TecTagBlockCondition;
    Parent: TecTextRange;
    Index: integer;

    constructor Create(AStartIdx, AStartPos: integer);
    property Level: integer read GetLevel;
    property IsClosed: Boolean read GetIsClosed;
  end;

  { TecSubLexerRange }

  TecSubLexerRange = record
  public
    Range: TRange;
    Rule: TecSubAnalyzerRule;   // Rule reference
    CondEndPos: integer;      // Start pos of the start condition
    CondStartPos: integer;    // End pos of the end condition
    class operator =(const a, b: TecSubLexerRange): boolean;
  end;

// *******************************************************************
//                Rules for syntax interpretation
// *******************************************************************

  TecTagConditionType = (tcEqual, tcNotEqual, tcMask, tcSkip, tcStrictMask);

  TecSingleTagCondition = class(TCollectionItem)
  private
    FTagList: TStrings;
    FCondType: TecTagConditionType;
    FTokenTypes: DWORD;
    procedure SetTagList(const Value: TStrings);
    procedure SetIgnoreCase(const Value: Boolean);
    procedure SetTokenTypes(const Value: DWORD);
    procedure SetCondType(const Value: TecTagConditionType);
    procedure TagListChanged(Sender: TObject);
    function GetIgnoreCase: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function CheckToken(const Source: ecString; const Token: TecSyntToken): Boolean;
  published
    property TagList: TStrings read FTagList write SetTagList;
    property CondType: TecTagConditionType read FCondType write SetCondType default tcEqual;
    property TokenTypes: DWORD read FTokenTypes write SetTokenTypes default 0;
    property IgnoreCase: Boolean read GetIgnoreCase write SetIgnoreCase default False;
  end;

  TecConditionCollection = class(TCollection)
  private
    FOwner: TecTagBlockCondition;
    FOnChange: TNotifyEvent;
    function GetItem(Index: integer): TecSingleTagCondition;
  protected
    procedure Update(Item: TCollectionItem); override;
    function  GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TecTagBlockCondition);
    function Add: TecSingleTagCondition;
    property Items[Index: integer]: TecSingleTagCondition read GetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TecTagBlockType = (btTagDetect, btLineBreak, btRangeStart, btRangeEnd);
  TecHighlightPos = (cpAny, cpBound, cpBoundTag, cpRange, cpBoundTagBegin, cpOutOfRange);
  TecDynamicHighlight = (dhNone, dhBound, dhRangeNoBound, dhRange);
  TecAutoCloseMode = (acmDisabled, acmCloseNearest, acmCloseOpened);

  TecTagBlockCondition = class(TRuleCollectionItem)
  private
    FConditions: TecConditionCollection;
    FIdentIndex: integer;
    FLinePos: TecLineBreakPos;
    FBlockOffset: integer;
    FBlockEndCond: TecTagBlockCondition;
    FBlockType: TecTagBlockType;
    FBlockEndName: string;
    FEndOfTextClose: Boolean;
    FNotCollapsed: Boolean;
    FSameIdent: Boolean;
    FInvertColors: Boolean;
    FHighlight: Boolean;
    FDisplayInTree: Boolean;
    FNameFmt: ecString;
    FGroupFmt: ecString;
    FRefToCondEnd: Boolean;
    FDynHighlight: TecDynamicHighlight;
    FHighlightPos: TecHighlightPos;
    FDynSelectMin: Boolean;
    FCancelNextRules: Boolean;
    FOnBlockCheck: TOnBlockCheck;
    FDrawStaple: Boolean;
    FGroupIndex: integer;
    FCollapseFmt: ecString;
    FSelfClose: Boolean;
    FNoEndRule: Boolean;
    FGrammaRuleName: string;
    FGrammaRule: TParserRule;
    FTokenType: integer;
    FTreeItemStyle: string;
    FTreeItemStyleObj: TecSyntaxFormat;
    FTreeGroupStyle: string;
    FTreeGroupStyleObj: TecSyntaxFormat;
    FTreeGroupImage: integer;
    FTreeItemImage: integer;
    FUseCustomPen: Boolean;
    FPen: TPen;
    FIgnoreAsParent: Boolean;
    FAutoCloseText: ecString;
    FAutoCloseMode: TecAutoCloseMode;
    procedure ConditionsChanged(Sender: TObject);
    function GetBlockEndName: string;
    procedure SetBlockEndName(const Value: string);
    procedure SetBlockType(const Value: TecTagBlockType);
    procedure SetConditions(const Value: TecConditionCollection);
    procedure SetBlockEndCond(const Value: TecTagBlockCondition);
    procedure SetLinePos(const Value: TecLineBreakPos);
    procedure SetIdentIndex(const Value: integer);
    procedure SetBlockOffset(const Value: integer);
    procedure SetEndOfTextClose(const Value: Boolean);
    procedure SetNotCollapsed(const Value: Boolean);
    procedure SetSameIdent(const Value: Boolean);
    procedure SetHighlight(const Value: Boolean);
    procedure SetInvertColors(const Value: Boolean);
    procedure SetDisplayInTree(const Value: Boolean);
    procedure SetCancelNextRules(const Value: Boolean);
    procedure SetDynHighlight(const Value: TecDynamicHighlight);
    procedure SetDynSelectMin(const Value: Boolean);
    procedure SetGroupFmt(const Value: ecString);
    procedure SetHighlightPos(const Value: TecHighlightPos);
    procedure SetNameFmt(const Value: ecString);
    procedure SetRefToCondEnd(const Value: Boolean);
    procedure SetDrawStaple(const Value: Boolean);
    procedure SetCollapseFmt(const Value: ecString);
    procedure SetSelfClose(const Value: Boolean);
    procedure SetNoEndRule(const Value: Boolean);
    procedure SetGrammaRuleName(const Value: string);
    procedure SetTokenType(const Value: integer);
    function GetTreeItemStyle: string;
    procedure SetTreeItemStyle(const Value: string);
    function GetTreeGroupStyle: string;
    procedure SetTreeGroupStyle(const Value: string);
    procedure SetTreeGroupImage(const Value: integer);
    procedure SetTreeItemImage(const Value: integer);
    procedure SetPen(const Value: TPen);
    procedure SetUseCustomPen(const Value: Boolean);
    procedure SetIgnoreAsParent(const Value: Boolean);
    procedure SetAutoCloseText(Value: ecString);
    procedure SetAutoCloseMode(const Value: TecAutoCloseMode);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetItemBaseName: string; override;
    procedure Loaded; override;
    function CheckOffset: integer;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function Check(const Source: ecString; Tags: TecClientSyntAnalyzer;
                   N: integer;  var RefIdx: integer): Boolean;
    property BlockEndCond: TecTagBlockCondition read FBlockEndCond write SetBlockEndCond;
    property TreeItemStyleObj: TecSyntaxFormat read FTreeItemStyleObj;
    property TreeGroupStyleObj: TecSyntaxFormat read FTreeGroupStyleObj;
  published
    property BlockType: TecTagBlockType read FBlockType write SetBlockType default btRangeStart;
    property ConditionList: TecConditionCollection read FConditions write SetConditions;
    property IdentIndex: integer read FIdentIndex write SetIdentIndex default 0;
    property LinePos: TecLineBreakPos read FLinePos write SetLinePos default lbTop;
    property BlockOffset: integer read FBlockOffset write SetBlockOffset default 0;
    property BlockEnd: string read GetBlockEndName write SetBlockEndName;
    property EndOfTextClose: Boolean read FEndOfTextClose write SetEndOfTextClose default False;
    property NotCollapsed: Boolean read FNotCollapsed write SetNotCollapsed default False;
    property SameIdent: Boolean read FSameIdent write SetSameIdent default False;
    property Highlight: Boolean read FHighlight write SetHighlight default False;
    property InvertColors: Boolean read FInvertColors write SetInvertColors default False;
    property DisplayInTree: Boolean read FDisplayInTree write SetDisplayInTree default True;
    property NameFmt: ecString read FNameFmt write SetNameFmt;
    property GroupFmt: ecString read FGroupFmt write SetGroupFmt;
    property RefToCondEnd: Boolean read FRefToCondEnd write SetRefToCondEnd default False;
    property DynHighlight: TecDynamicHighlight read FDynHighlight write SetDynHighlight default dhNone;
    property HighlightPos: TecHighlightPos read FHighlightPos write SetHighlightPos;
    property DynSelectMin: Boolean read FDynSelectMin write SetDynSelectMin default False;
    property CancelNextRules: Boolean read FCancelNextRules write SetCancelNextRules default False;
    property DrawStaple: Boolean read FDrawStaple write SetDrawStaple default False;
    property GroupIndex: integer read FGroupIndex write FGroupIndex default 0;
    property CollapseFmt: ecString read FCollapseFmt write SetCollapseFmt;
    property OnBlockCheck: TOnBlockCheck read FOnBlockCheck write FOnBlockCheck;
    property SelfClose: Boolean read FSelfClose write SetSelfClose default False;
    // New in v2.20
    property NoEndRule: Boolean read FNoEndRule write SetNoEndRule default False;
    property GrammaRuleName: string read FGrammaRuleName write SetGrammaRuleName;
    property TokenType: integer read FTokenType write SetTokenType default -1;
    property TreeItemStyle: string read GetTreeItemStyle write SetTreeItemStyle;
    property TreeGroupStyle: string read GetTreeGroupStyle write SetTreeGroupStyle;
    property TreeItemImage: integer read FTreeItemImage write SetTreeItemImage default -1;
    property TreeGroupImage: integer read FTreeGroupImage write SetTreeGroupImage default -1;
    // New in 2.40
    property Pen: TPen read FPen write SetPen;
    property UseCustomPen: Boolean read FUseCustomPen write SetUseCustomPen default False;
    property IgnoreAsParent: Boolean read FIgnoreAsParent write SetIgnoreAsParent;
    // New in 2.50
    property AutoCloseMode: TecAutoCloseMode read FAutoCloseMode write SetAutoCloseMode default acmDisabled;
    property AutoCloseText: ecString read FAutoCloseText write SetAutoCloseText;
  end;

  TecBlockRuleCollection = class(TSyntCollection)
  private
    function GetItem(Index: integer): TecTagBlockCondition;
  public
    constructor Create;
    function Add: TecTagBlockCondition;
    property Items[Index: integer]: TecTagBlockCondition read GetItem; default;
  end;

  // Token identification rule
  TecTokenRule = class(TRuleCollectionItem)
  private
    FRegExpr: TecRegExpr;
    FTokenType: integer;
    FOnMatchToken: TOnMatchToken;
    FColumnTo: integer;
    FColumnFrom: integer;
    function GetExpression: ecString;
    procedure SetExpression(const Value: ecString);
    procedure SetTokenType(const Value: integer);
    procedure SetColumnFrom(const Value: integer);
    procedure SetColumnTo(const Value: integer);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetItemBaseName: string; override;
    function GetIsInvalid: Boolean; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function Match(const Source: ecString; Pos: integer): integer;
  published
    property TokenType: integer read FTokenType write SetTokenType default 0;
    property Expression: ecString read GetExpression write SetExpression;
    property ColumnFrom: integer read FColumnFrom write SetColumnFrom;
    property ColumnTo: integer read FColumnTo write SetColumnTo;
    property OnMatchToken: TOnMatchToken read FOnMatchToken write FOnMatchToken;
  end;

  TecTokenRuleCollection = class(TSyntCollection)
  private
    function GetItem(Index: integer): TecTokenRule;
  public
    constructor Create;
    function Add: TecTokenRule;
    property Items[Index: integer]: TecTokenRule read GetItem; default;
  end;

  TecSubAnalyzerRule = class(TRuleCollectionItem)
  private
    FStartRegExpr: TecRegExpr;
    FEndRegExpr: TecRegExpr;
    FSyntAnalyzer: TecSyntAnalyzer;
    FFromTextBegin: Boolean;
    FToTextEnd: Boolean;
    FIncludeBounds: Boolean;
    function GetEndExpression: string;
    function GetStartExpression: string;
    procedure SetEndExpression(const Value: string);
    procedure SetStartExpression(const Value: string);
    procedure SetSyntAnalyzer(const Value: TecSyntAnalyzer);
    procedure SetFromTextBegin(const Value: Boolean);
    procedure SetToTextEnd(const Value: Boolean);
    procedure SetIncludeBounds(const Value: Boolean);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetItemBaseName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function MatchStart(const Source: ecString; Pos: integer): integer;
    function MatchEnd(const Source: ecString; Pos: integer): integer;
  published
    property StartExpression: string read GetStartExpression write SetStartExpression;
    property EndExpression: string read GetEndExpression write SetEndExpression;
    property SyntAnalyzer: TecSyntAnalyzer read FSyntAnalyzer write SetSyntAnalyzer;
    property FromTextBegin: Boolean read FFromTextBegin write SetFromTextBegin default False;
    property ToTextEnd: Boolean read FToTextEnd write SetToTextEnd default False;
    property IncludeBounds: Boolean read FIncludeBounds write SetIncludeBounds default False;
  end;

  TecSubAnalyzerRules = class(TSyntCollection)
  private
    function GetItem(Index: integer): TecSubAnalyzerRule;
  public
    constructor Create;
    function Add: TecSubAnalyzerRule;
    property Items[Index: integer]: TecSubAnalyzerRule read GetItem; default;
  end;

  { TecCodeTemplate }

  TecCodeTemplate = class(TCollectionItem)
  private
    FName: string;
    FDescription: string;
    FAdvanced: boolean;
    FCode: TStrings;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property Advanced: Boolean read FAdvanced write FAdvanced;
    property Code: TStrings read FCode;
  end;

  TecCodeTemplates = class(TOwnedCollection)
  private
    function GetItem(Index: integer): TecCodeTemplate;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TecCodeTemplate;
    property Items[Index: integer]: TecCodeTemplate read GetItem; default;
  end;

// *******************************************************************
//  Parser classes
//
// *******************************************************************


// *******************************************************************
//  Syntax analizer for single client
//            container of description objects
// *******************************************************************

  TecTokenList = GRangeList<TecSyntToken>;
  TecSubLexerRanges = GRangeList<TecSubLexerRange>;

  { TecParserResults }

  TecParserResults = class(TTokenHolder)
  private
    FBuffer: TATStringBuffer;
    FClient: IecSyntClient;
    FOwner: TecSyntAnalyzer;
    FFinished: Boolean;
    FSubLexerBlocks: TecSubLexerRanges;
    FTagList: TecTokenList;
    FCurState: integer;
    FStateChanges: TecRangeList;
    function GetLastPos(const Source: ecString): integer;
    function ExtractTag(const Source: ecString; var FPos: integer): Boolean;
    function GetTags(Index: integer): TecSyntToken;
    function GetSubLexerRangeCount: integer;
    function GetSubLexerRange(Index: integer): TecSubLexerRange;
    procedure SetTags(Index: integer; const AValue: TecSyntToken);
  protected
    function GetTokenCount: integer; override;
    function GetTokenStr(Index: integer): ecString; override;
    function GetTokenType(Index: integer): integer; override;
    procedure CloseAtEnd(StartTagIdx: integer); virtual; abstract;
  protected
    FLastAnalPos: integer;
    procedure Finished; virtual;
    function IsEnabled(Rule: TRuleCollectionItem; OnlyGlobal: Boolean): Boolean; virtual;
    procedure ApplyStates(Rule: TRuleCollectionItem);
    procedure SaveState;
    procedure RestoreState;
  public
    constructor Create(AOwner: TecSyntAnalyzer; ABuffer: TATStringBuffer; const AClient: IecSyntClient); virtual;
    destructor Destroy; override;
    procedure Clear; virtual;

    function AnalyzerAtPos(APos: integer): TecSyntAnalyzer;
    function ParserStateAtPos(TokenIndex: integer): integer;

    property Owner: TecSyntAnalyzer read FOwner;
    property Buffer: TATStringBuffer read FBuffer;
    property IsFinished: Boolean read FFinished;
    property TagStr[Index: integer]: ecString read GetTokenStr;
    property TagCount: integer read GetTokenCount;
    property Tags[Index: integer]: TecSyntToken read GetTags write SetTags; default;
    property SubLexerRangeCount: integer read GetSubLexerRangeCount;
    property SubLexerRanges[Index: integer]: TecSubLexerRange read GetSubLexerRange;
    property ParserState: integer read FCurState write FCurState;
    //property TagIndexes[Index: integer]: TRangeListIndex read GetTagIndexes;
  end;

  { TecClientSyntAnalyzer }

  TecClientSyntAnalyzer = class(TecParserResults)
  private
    FRanges: TSortedList;
    FOpenedBlocks: TSortedList;    // Opened ranges (without end)

    FTimerIdleMustStop: Boolean;
    FTimerIdleIsBusy: Boolean;
    FTimerIdle: TTimer;

    FPrevProgress: integer;
    FStartSepRangeAnal: integer;
    FDisableIdleAppend: Boolean;
    FRepeateAnalysis: Boolean;

    function GetRangeCount: integer;
    function GetRanges(Index: integer): TecTextRange;
    function GetOpened(Index: integer): TecTextRange;
    function GetOpenedCount: integer;
    procedure SetDisableIdleAppend(const Value: Boolean);
    function DoStopTimer(AndWait: boolean): boolean;
  protected
    procedure AddRange(Range: TecTextRange);
    function HasOpened(Rule: TRuleCollectionItem; Parent: TecTagBlockCondition; Strict: Boolean): Boolean;
    function IsEnabled(Rule: TRuleCollectionItem; OnlyGlobal: Boolean): Boolean; override;
    procedure Finished; override;
    procedure TimerIdleTick(Sender: TObject);
    procedure CloseAtEnd(StartTagIdx: integer); override;

  public
    constructor Create(AOwner: TecSyntAnalyzer; SrcProc: TATStringBuffer; const AClient: IecSyntClient); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure ChangedAtPos(APos: integer);
    function PriorTokenAt(Pos: integer): integer;

    function RangeFormat(const FmtStr: ecString; Range: TecTextRange): ecString;
    function GetRangeName(Range: TecTextRange): ecString;
    function GetRangeGroup(Range: TecTextRange): ecString;
    function GetCollapsedText(Range: TecTextRange): ecString;
    function Stop: boolean;

    procedure TextChanged(APos: integer);
    procedure AppendToPos(APos: integer; AUseTimer: boolean= true); // Requires analyzed to APos
    procedure Analyze(ResetContent: Boolean = True); // Requires analyzed all text
    procedure IdleAppend;                 // Start idle analysis

    procedure CompleteAnalysis;

    function CloseRange(Cond: TecTagBlockCondition; RefTag: integer): Boolean;
    function DetectTag(Rule: TecTagBlockCondition; RefTag: integer): Boolean;

    property OpenCount: integer read GetOpenedCount;
    property Opened[Index: integer]: TecTextRange read GetOpened;

    property RangeCount: integer read GetRangeCount;
    property Ranges[Index: integer]: TecTextRange read GetRanges;
    property DisableIdleAppend: Boolean read FDisableIdleAppend write SetDisableIdleAppend;
  end;

// *******************************************************************
//  Syntax analizer
//            container of syntax rules
// *******************************************************************

  TLoadableComponent = class(TComponent)
  private
    FSkipNewName: Boolean;
    FFileName: string;
    FIgnoreAll: Boolean;
    FSaving: Boolean;
  protected
    procedure OnReadError(Reader: TReader; const Message: string;
                          var Handled: Boolean); virtual;
    function NotStored: Boolean;
  public
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(const FileName: string); virtual;
    procedure LoadFromResourceID(Instance: Cardinal; ResID: Integer; ResType: string); virtual;
    procedure LoadFromResourceName(Instance: Cardinal; const ResName: string; ResType: string); virtual;
    procedure LoadFromStream(const Stream: TStream); virtual;
  protected
    procedure SetName(const NewName: TComponentName); override;
    property FileName: string read FFileName write LoadFromFile;
  end;

  TParseTokenEvent = procedure(Client: TecParserResults; const Text: ecString; Pos: integer;
      var TokenLength: integer; var Rule: TecTokenRule) of object;

  TecParseProgressEvent = procedure(Sender: TObject; AProgress: integer) of object;

  { TecSyntAnalyzer }

  TecSyntAnalyzer = class(TLoadableComponent)
  private
    FClientList: TList;
    FMasters: TList;      // Master lexer, i.e. lexers that uses it
    FOnChange: TNotifyEvent;
    FSampleText: TStrings;
    FFormats: TecStylesCollection;
    FTokenRules: TecTokenRuleCollection;
    FBlockRules: TecBlockRuleCollection;
    FCodeTemplates: TecCodeTemplates;
    FExtentions: string;
    FLexerName: string;
    FCoping: Boolean;
    FSkipSpaces: Boolean;
    FSubAnalyzers: TecSubAnalyzerRules;
    FTokenTypeNames: TStrings;
    FFullRefreshSize: integer;

    FMarkedBlock: TecSyntaxFormat;
    FMarkedBlockName: string;
    FSearchMatch: TecSyntaxFormat;
    FSearchMatchName: string;
    FCurrentLine: TecSyntaxFormat;
    FCurrentLineName: string;
    FDefStyle: TecSyntaxFormat;
    FDefStyleName: string;
    FCollapseStyle: TecSyntaxFormat;
    FCollapseStyleName: string;
    FNotes: TStrings;
    FInternal: boolean;
    FRestartFromLineStart: Boolean;
    FParseEndOfLine: Boolean;
    FGrammaParser: TGrammaAnalyzer;
    FLineComment: ecString;
    FCharset: TFontCharSet;
    FSeparateBlocks: integer;
    FAlwaysSyncBlockAnal: Boolean;   // Indicates that blocks analysis may after tokens
    FOnGetCollapseRange: TBoundDefEvent;
    FOnCloseTextRange: TBoundDefEvent;
    FIdleAppendDelayInit: Cardinal;
    FIdleAppendDelay: Cardinal;
    FOnParseToken: TParseTokenEvent;

    procedure SetSampleText(const Value: TStrings);
    procedure FormatsChanged(Sender: TCollection; Item: TSyntCollectionItem);
    procedure TokenRuleChanged(Sender: TCollection; Item: TSyntCollectionItem);
    procedure BlocksChanged(Sender: TCollection; Item: TSyntCollectionItem);
    procedure SubLexRuleChanged(Sender: TCollection; Item: TSyntCollectionItem);
    procedure SetBlockRules(const Value: TecBlockRuleCollection);
    procedure SetCodeTemplates(const Value: TecCodeTemplates);
    procedure SetTokenRules(const Value: TecTokenRuleCollection);
    procedure SetFormats(const Value: TecStylesCollection);
    function GetUniqueName(const Base: string): string;
    procedure SetSkipSpaces(const Value: Boolean);
    procedure SetSubAnalyzers(const Value: TecSubAnalyzerRules);
    procedure SetTokenTypeNames(const Value: TStrings);

    function GetStyleName(const AName: string; const AStyle: TecSyntaxFormat): string;
    procedure SetMarkedBlock(const Value: TecSyntaxFormat);
    function GetMarkedBlockName: string;
    procedure SetMarkedBlockName(const Value: string);
    procedure SetSearchMatch(const Value: TecSyntaxFormat);
    function GetSearchMatchStyle: string;
    procedure SetSearchMatchStyle(const Value: string);
    procedure SetCurrentLine(const Value: TecSyntaxFormat);
    function GetCurrentLineStyle: string;
    procedure SetCurrentLineStyle(const Value: string);
    procedure SetNotes(const Value: TStrings);
    procedure SetInternal(const Value: boolean);
    procedure SetRestartFromLineStart(const Value: Boolean);
    procedure SetParseEndOfLine(const Value: Boolean);
    procedure TokenNamesChanged(Sender: TObject);
    procedure CompileGramma;
    procedure SetGrammar(const Value: TGrammaAnalyzer);
    procedure GrammaChanged(Sender: TObject);
    procedure SetDefStyle(const Value: TecSyntaxFormat);
    function GetDefaultStyleName: string;
    procedure SetDefaultStyleName(const Value: string);
    procedure SetLineComment(const Value: ecString);
    procedure DetectBlockSeparate;
    procedure SetAlwaysSyncBlockAnal(const Value: Boolean);
    function GetCollapseStyleName: string;
    procedure SetCollapseStyleName(const Value: string);
    procedure SetCollapseStyle(const Value: TecSyntaxFormat);
    function GetSeparateBlocks: Boolean;
  protected
    function GetToken(Client: TecParserResults; const Source: ecString;
                       APos: integer; OnlyGlobal: Boolean): TecSyntToken; virtual;
    procedure HighlightKeywords(Client: TecParserResults; const Source: ecString;
                       OnlyGlobal: Boolean); virtual;
    procedure SelectTokenFormat(Client: TecParserResults; const Source: ecString;
                       OnlyGlobal: Boolean; N: integer = -1); virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; dynamic;
    property SeparateBlockAnalysis: Boolean read GetSeparateBlocks;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    //function AddClient(const Client: IecSyntClient; ABuffer: TATStringBuffer): TecClientSyntAnalyzer;
    procedure ClearClientContents;
    procedure UpdateClients;

    procedure AddMasterLexer(SyntAnal: TecSyntAnalyzer);
    procedure RemoveMasterLexer(SyntAnal: TecSyntAnalyzer);

    property MarkedBlock: TecSyntaxFormat read FMarkedBlock write SetMarkedBlock;
    property SearchMatch: TecSyntaxFormat read FSearchMatch write SetSearchMatch;
    property CurrentLine: TecSyntaxFormat read FCurrentLine write SetCurrentLine;
    property DefStyle: TecSyntaxFormat read FDefStyle write SetDefStyle;
    property CollapseStyle: TecSyntaxFormat read FCollapseStyle write SetCollapseStyle;
  published
    property Formats: TecStylesCollection read FFormats write SetFormats;
    property TokenRules: TecTokenRuleCollection read FTokenRules write SetTokenRules;
    property BlockRules: TecBlockRuleCollection read FBlockRules write SetBlockRules;
    property CodeTemplates: TecCodeTemplates read FCodeTemplates write SetCodeTemplates;
    property SubAnalyzers: TecSubAnalyzerRules read FSubAnalyzers write SetSubAnalyzers;
    property SampleText: TStrings read FSampleText write SetSampleText;

    property TokenTypeNames: TStrings read FTokenTypeNames write SetTokenTypeNames;
    property Gramma: TGrammaAnalyzer read FGrammaParser write SetGrammar;

    property MarkedBlockStyle: string read GetMarkedBlockName write SetMarkedBlockName;
    property SearchMatchStyle: string read GetSearchMatchStyle write SetSearchMatchStyle;
    property CurrentLineStyle: string read GetCurrentLineStyle write SetCurrentLineStyle;
    property DefaultStyleName: string read GetDefaultStyleName write SetDefaultStyleName;
    property CollapseStyleName: string read GetCollapseStyleName write SetCollapseStyleName;

    property Extentions: string read FExtentions write FExtentions;
    property LexerName: string read FLexerName write FLexerName;

    property SkipSpaces: Boolean read FSkipSpaces write SetSkipSpaces default True;
    property FullRefreshSize: integer read FFullRefreshSize write FFullRefreshSize default 0;
    property Notes: TStrings read FNotes write SetNotes;
    property Internal: boolean read FInternal write SetInternal default False;
    property RestartFromLineStart: Boolean read FRestartFromLineStart write SetRestartFromLineStart default False;
    property ParseEndOfLine: Boolean read FParseEndOfLine write SetParseEndOfLine default False;
    property LineComment: ecString read FLineComment write SetLineComment;
    property Charset: TFontCharSet read FCharset write FCharset; //AT
    property AlwaysSyncBlockAnal: Boolean read FAlwaysSyncBlockAnal write SetAlwaysSyncBlockAnal default False;
    property IdleAppendDelay: Cardinal read FIdleAppendDelay write FIdleAppendDelay default 200;
    property IdleAppendDelayInit: Cardinal read FIdleAppendDelayInit write FIdleAppendDelayInit default 50;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnGetCollapseRange: TBoundDefEvent read FOnGetCollapseRange write FOnGetCollapseRange;
    property OnCloseTextRange: TBoundDefEvent read FOnCloseTextRange write FOnCloseTextRange;
    property OnParseToken: TParseTokenEvent read FOnParseToken write FOnParseToken;
  end;

  TLibSyntAnalyzer = class(TecSyntAnalyzer)
  protected
    FParent: TecSyntaxManager;
//    function GetChildParent: TComponent; override;
//    procedure SetName(const NewName: TComponentName); override;
    procedure SetParentComponent(Value: TComponent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(const Stream: TStream); override;
    function GetParentComponent: TComponent; override;
    function HasParent: Boolean; override;
  end;

  TecSyntaxManager = class(TLoadableComponent)
  private
    FOnChange: TNotifyEvent;
    FList: TList;
    FCurrentLexer: TecSyntAnalyzer;
    FOnLexerChanged: TNotifyEvent;
    FModified: Boolean;
    function GeItem(Index: integer): TecSyntAnalyzer;
    function GetCount: integer;
    procedure SetCurrentLexer(const Value: TecSyntAnalyzer);
  protected
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Changed; dynamic;
    procedure OnReadError(Reader: TReader; const Message: string;
                          var Handled: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    function FindAnalyzer(const LexerName: string): TecSyntAnalyzer;
    function AddAnalyzer: TecSyntAnalyzer;
    procedure Clear;
    procedure Move(CurIndex, NewIndex: Integer);

    property AnalyzerCount: integer read GetCount;
    property Analyzers[Index: integer]: TecSyntAnalyzer read GeItem;
    property FileName;
    property CurrentLexer: TecSyntAnalyzer read FCurrentLexer write SetCurrentLexer;
    property Modified: Boolean read FModified write FModified;
  published
    property OnLexerChanged: TNotifyEvent read FOnLexerChanged write FOnLexerChanged stored NotStored;
    property OnChange: TNotifyEvent read FOnChange write FOnChange stored NotStored;
  end;

  TecSyntStyles = class(TLoadableComponent)
  private
    FStyles: TecStylesCollection;
    procedure SetStyles(const Value: TecStylesCollection);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Styles: TecStylesCollection read FStyles write SetStyles;
  end;


var
  OnLexerParseProgress: TecParseProgressEvent;


implementation

uses
  SysUtils, Forms, Dialogs,
  Math;

const
  SecDefaultTokenTypeNames = 'Unknown' + #13#10 +
                             'Comment' + #13#10 +
                             'Id'      + #13#10 +
                             'Symbol'  + #13#10 +
                             'String'  + #13#10 +
                             'Number'  + #13#10 +
                             'Preprocessor';

// Local copy of ecUpCase. it is faster, uses AnsiChar UpCase
function ecUpCase(ch: WideChar): char; inline;
begin
  Result:= System.UpCase(char(ch));
end;

procedure SetDefaultModifiers(RE: TecRegExpr);
begin
  RE.ModifierI := True;
  RE.ModifierG := True;
  RE.ModifierS := False;
  RE.ModifierM := True;
  RE.ModifierX := True;
  RE.ModifierR := False;
end;

function IsPosSorted(const A, B: TPoint; AllowEq: boolean): boolean; inline;
begin
  if A.Y<>B.Y then
    Result:= A.Y<B.Y
  else
    Result:= (A.X<B.X) or (AllowEq and (A.X=B.X));
end;

{ TecSubLexerRange }

class operator TecSubLexerRange.=(const a, b: TecSubLexerRange): boolean;
begin
  Result := false;
end;

{ TecSyntToken }

constructor TecSyntToken.Create(ARule: TRuleCollectionItem;
  AStartPos, AEndPos: integer;
  const APointStart, APointEnd: TPoint);
begin
  Range.StartPos := AStartPos;
  Range.EndPos := AEndPos;
  Range.PointStart := APointStart;
  Range.PointEnd := APointEnd;
  Rule := ARule;
  if Assigned(ARule) then
    TokenType := TecTokenRule(ARule).TokenType
  else
    TokenType := 0;
end;

function TecSyntToken.GetStr(const Source: ecString): ecString;
begin
  with Range do
    Result := Copy(Source, StartPos + 1, EndPos - StartPos);
end;

class operator TecSyntToken.=(const A, B: TecSyntToken): boolean;
begin
  Result:= false;
end;

function TecSyntToken.GetStyle: TecSyntaxFormat;
begin
  if Rule = nil then
    Result := nil
  else
    Result := Rule.Style;
end;

{ TecTextRange }

constructor TecTextRange.Create(AStartIdx, AStartPos: integer);
begin
  inherited Create;
  StartIdx := AStartIdx;
  StartPos := AStartPos;
  EndIdx := -1;
  FEndCondIndex := -1;
  Index := -1;
end;

function TecTextRange.GetIsClosed: Boolean;
begin
  Result := EndIdx <> -1;
end;

function TecTextRange.GetKey: integer;
begin
  Result := StartPos;
end;

function TecTextRange.GetLevel: integer;
var prn: TecTextRange;
begin
  prn := Parent;
  Result := 0;
  while prn <> nil do
   begin
     inc(Result);
     prn := prn.Parent;
   end;
end;

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
  inherited;
  FFont.OnChange := FontChanged;
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

procedure TecSyntaxFormat.SetBgColor(const Value: TColor);
begin
  FBgColor := Value;
  Change;
end;

procedure TecSyntaxFormat.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
  Change;
end;

procedure TecSyntaxFormat.FontChanged(Sender: TObject);
begin
  Change;
end;

procedure TecSyntaxFormat.SetVertAlign(const Value: TecVertAlignment);
begin
  FVertAlign := Value;
  Change;
end;

function TecSyntaxFormat.GetItemBaseName: string;
begin
  Result := 'Style';
end;

procedure TecSyntaxFormat.SetFormatType(const Value: TecFormatType);
begin
  FFormatType := Value;
  Change;
end;

procedure TecSyntaxFormat.Change;
begin
  Changed(False);
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TecSyntaxFormat.SetHidden(const Value: Boolean);
begin
  FHidden := Value;
  Change;
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
  const Value: TColor);
begin
  if (Index >= 0) and (Index <= 3) then
   begin
    FBorderColors[Index] := Value;
    Change;
   end;
end;

procedure TecSyntaxFormat.SetBorderType(Index: Integer;
  const Value: TecBorderLineType);
begin
  if (Index >= 0) and (Index <= 3) then
   begin
    FBorderTypes[Index] := Value;
    Change;
   end;
end;

procedure TecSyntaxFormat.SetMultiLineBorder(const Value: Boolean);
begin
  FMultiLineBorder := Value;
  Change;
end;

procedure TecSyntaxFormat.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
  Change;
end;

procedure TecSyntaxFormat.SetChangeCase(const Value: TecChangeCase);
begin
  FChangeCase := Value;
  Change;
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

procedure TecSyntaxFormat.SetFormatFlags(const Value: TecFormatFlags);
begin
  if FFormatFlags <> Value then
    begin
      FFormatFlags := Value;
      Change;
    end;
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

{ TecSingleTagCondition }

procedure TecSingleTagCondition.AssignTo(Dest: TPersistent);
var dst: TecSingleTagCondition;
begin
  if Dest is TecSingleTagCondition then
   begin
     dst := Dest as TecSingleTagCondition;
     dst.CondType := CondType;
     dst.TokenTypes := TokenTypes;
     dst.FTagList.Assign(FTagList);
     dst.IgnoreCase := IgnoreCase;
   end;
end;

function TecSingleTagCondition.CheckToken(const Source: ecString; const Token: TecSyntToken): Boolean;
var s: ecString;
    i, N: integer;
    RE: TecRegExpr;
begin
  Result := False;
  if FTokenTypes <> 0 then
   begin
    Result := ((1 shl Token.TokenType) and FTokenTypes) <> 0;
    if FCondType = tcSkip then Exit;
    if not Result then
    case FCondType of
      tcStrictMask, tcMask, tcEqual: Exit;
      tcNotEqual:
        begin
          Result := True;
          Exit;
        end;
    end;
   end;
  if FTagList.Count > 0 then
   begin
    s := Token.GetStr(Source);
    s := Trim(s); //AT
    if FCondType in [tcMask, tcStrictMask] then
     begin
       RE := TecRegExpr.Create;
       SetDefaultModifiers(RE);
       try
         for i := 0 to FTagList.Count - 1 do
          begin
            RE.Expression := FTagList[i];
            if FCondType = tcMask then
              Result := RE.MatchLength(s, 1) > 0
            else
              begin
                N := 1;
                Result := RE.Match(s, N);
                if Result then
                  Result := N > Length(S);
              end;

            if Result then break;
          end;
       except
       end;
       FreeAndNil(RE);
     end else
     begin
       Result := FTagList.IndexOf(s) <> -1;
       if FCondType = tcNotEqual then Result := not Result;
     end;
   end else Result := FCondType <> tcNotEqual;
end;

constructor TecSingleTagCondition.Create(Collection: TCollection);
begin
  inherited;
  FCondType := tcEqual;
  FTagList := TStringList.Create;
  TStringList(FTagList).Sorted := true;
  TStringList(FTagList).Delimiter := ' ';
  TStringList(FTagList).Duplicates := dupIgnore;
  TStringList(FTagList).CaseSensitive := True;
  TStringList(FTagList).OnChange := TagListChanged;
  TStringList(FTagList).QuoteChar := ' ';
end;

destructor TecSingleTagCondition.Destroy;
begin
  FreeAndNil(FTagList);
  inherited;
end;

procedure TecSingleTagCondition.SetIgnoreCase(const Value: Boolean);
begin
  TStringList(FTagList).CaseSensitive := not Value;
end;

function TecSingleTagCondition.GetIgnoreCase: Boolean;
begin
  Result := not TStringList(FTagList).CaseSensitive;
end;

procedure TecSingleTagCondition.SetTagList(const Value: TStrings);
begin
  TStringList(FTagList).DelimitedText := Value.Text;
  Changed(False);
end;

procedure TecSingleTagCondition.SetTokenTypes(const Value: DWORD);
begin
  FTokenTypes := Value;
  Changed(False);
end;

procedure TecSingleTagCondition.SetCondType(const Value: TecTagConditionType);
begin
  FCondType := Value;
  Changed(False);
end;

procedure TecSingleTagCondition.TagListChanged(Sender: TObject);
begin
  Changed(False);
end;

{ TecConditionCollection }

function TecConditionCollection.Add: TecSingleTagCondition;
begin
  Result := (inherited Add) as TecSingleTagCondition;
end;

constructor TecConditionCollection.Create(AOwner: TecTagBlockCondition);
begin
  inherited Create(TecSingleTagCondition);
  FOwner := AOwner;
  PropName := 'Conditions';
end;

function TecConditionCollection.GetItem(Index: integer): TecSingleTagCondition;
begin
  Result := (inherited Items[Index]) as TecSingleTagCondition;
end;

function TecConditionCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TecConditionCollection.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{ TecTagBlockCondition }

constructor TecTagBlockCondition.Create(Collection: TCollection);
begin
  inherited;
  FConditions := TecConditionCollection.Create(Self);
  FConditions.OnChange := ConditionsChanged;
  FBlockType := btRangeStart;
  FLinePos := lbTop;
  FDisplayInTree := True;
//  FHighlightPos := cpBound;
  FTokenType := -1;
  FTreeItemImage := -1;
  FTreeGroupImage := -1;
  FPen := TPen.Create;
  FPen.OnChange := ConditionsChanged;
end;

procedure TecTagBlockCondition.AssignTo(Dest: TPersistent);
var dst: TecTagBlockCondition;
begin
  inherited;
  if Dest is TecTagBlockCondition then
   begin
     dst := Dest as TecTagBlockCondition;
     dst.ConditionList := ConditionList;
     dst.FIdentIndex := IdentIndex;
     dst.FLinePos := LinePos;
     dst.FBlockOffset := BlockOffset;
     dst.FBlockType := BlockType;
     dst.BlockEnd := BlockEnd;
     dst.FEndOfTextClose := FEndOfTextClose;
     dst.FNotCollapsed := FNotCollapsed;
     dst.FSameIdent := FSameIdent;
     dst.Highlight := Highlight;
     dst.InvertColors := InvertColors;
     dst.DisplayInTree := DisplayInTree;
     dst.NameFmt := NameFmt;
     dst.GroupFmt := GroupFmt;
     dst.RefToCondEnd := RefToCondEnd;
     dst.DynHighlight := DynHighlight;
     dst.HighlightPos := HighlightPos;
     dst.DynSelectMin := DynSelectMin;
     dst.CancelNextRules := CancelNextRules;
     dst.DrawStaple := DrawStaple;
     dst.GroupIndex := GroupIndex;
     dst.OnBlockCheck := OnBlockCheck;
     dst.CollapseFmt := CollapseFmt;
     dst.FSelfClose := SelfClose;
     dst.FNoEndRule := NoEndRule;
     dst.GrammaRuleName := GrammaRuleName;
     dst.TokenType := TokenType;
     dst.TreeItemStyle := TreeItemStyle;
     dst.TreeGroupStyle := TreeGroupStyle;
     dst.TreeItemImage := TreeItemImage;
     dst.TreeGroupImage := TreeGroupImage;
     dst.Pen := Pen;
     dst.UseCustomPen := UseCustomPen;
     dst.IgnoreAsParent := IgnoreAsParent;
     dst.AutoCloseText := AutoCloseText;
     dst.AutoCloseMode := AutoCloseMode;
   end;
end;

function TecTagBlockCondition.Check(const Source: ecString;
  Tags: TecClientSyntAnalyzer; N: integer; var RefIdx: integer): Boolean;
var i, offs, idx, skipped, skip_cond: integer;
begin
  Result := False;
  offs := CheckOffset;
  skipped := 0;
  skip_cond := 0;
  i := 0;
  while i < ConditionList.Count do
  begin
   idx := N - 1 - i - offs - skipped + skip_cond;
   if (ConditionList[i].CondType = tcSkip) and (i < ConditionList.Count - 1)
      and (ConditionList[i+1].CondType <> tcSkip) then
     begin
      inc(i);
      inc(skip_cond);
      while (idx >= 0) and not ConditionList[i].CheckToken(Source, Tags[idx]) do
       begin
         if not ConditionList[i - 1].CheckToken(Source, Tags[idx]) then
           Exit;
         dec(idx);
         inc(skipped);
       end;
      if idx < 0 then Exit;
     end;
   with ConditionList[i] do
    if (idx < 0) or not CheckToken(Source, Tags[idx]) then Exit;
   inc(i);
  end;

  Result := ConditionList.Count > 0;
//  if FRefToCondEnd then
  RefIdx := N - ConditionList.Count - offs - skipped + skip_cond;
//  else
//    RefIdx := N - 1 - offs;
end;

destructor TecTagBlockCondition.Destroy;
begin
  FreeAndNil(FConditions);
  FreeAndNil(FPen);
  inherited;
end;

function TecTagBlockCondition.GetItemBaseName: string;
begin
  Result := 'Tag block rule';
end;

function TecTagBlockCondition.GetBlockEndName: string;
var FSynt: TecSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner;
  if not Assigned(FSynt) then Exit;

  if csLoading in FSynt.ComponentState then
    Result := FBlockEndName
  else
   if Assigned(FBlockEndCond) then
    Result := FBlockEndCond.DisplayName
   else
    Result := '';
end;

procedure TecTagBlockCondition.SetBlockEndName(const Value: string);
var FSynt: TecSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner;
  if not Assigned(FSynt) then Exit;
  if csLoading in FSynt.ComponentState then
    FBlockEndName := Value
  else
    FBlockEndCond := TecTagBlockCondition(TecBlockRuleCollection(Collection).ItemByName(Value));
  Changed(False);
end;

procedure TecTagBlockCondition.Loaded;
var FSynt: TecSyntAnalyzer;
begin
  inherited;
  FSynt := TSyntCollection(Collection).SyntOwner;
  if not Assigned(FSynt) then Exit;
  if FBlockEndName <> '' then
    FBlockEndCond := TecTagBlockCondition(FSynt.FBlockRules.ItemByName(FBlockEndName));
  if FTreeItemStyle <> '' then
    FTreeItemStyleObj := TecSyntaxFormat(FSynt.Formats.ItemByName(FTreeItemStyle));
  if FTreeGroupStyle <> '' then
    FTreeGroupStyleObj := TecSyntaxFormat(FSynt.Formats.ItemByName(FTreeGroupStyle));
end;

function TecTagBlockCondition.CheckOffset: integer;
begin
  Result := 0;
  if FRefToCondEnd then Exit;
  if FIdentIndex < 0 then Result := -FIdentIndex;
  if (FBlockOffset < 0) and (FBlockOffset < FIdentIndex) then
    Result := -FBlockOffset;
end;

procedure TecTagBlockCondition.SetBlockType(const Value: TecTagBlockType);
begin
  FBlockType := Value;
  if FBlockType in [btTagDetect, btLineBreak] then
   begin
     FBlockOffset := 0;
     FBlockEndCond := nil;
   end;
  Changed(False);
end;

procedure TecTagBlockCondition.SetConditions(
  const Value: TecConditionCollection);
begin
  FConditions.Assign(Value);
end;

procedure TecTagBlockCondition.ConditionsChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TecTagBlockCondition.SetBlockEndCond(
  const Value: TecTagBlockCondition);
begin
  if FBlockEndCond <> Value then
    begin
      FBlockEndCond := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetLinePos(const Value: TecLineBreakPos);
begin
  if FLinePos <> Value then
    begin
      FLinePos := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetIdentIndex(const Value: integer);
begin
  if FIdentIndex <> Value then
    begin
      FIdentIndex := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetBlockOffset(const Value: integer);
begin
  if FBlockOffset <> Value then
    begin
      FBlockOffset := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetEndOfTextClose(const Value: Boolean);
begin
  if FEndOfTextClose <> Value then
    begin
      FEndOfTextClose := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetNotCollapsed(const Value: Boolean);
begin
  if FNotCollapsed <> Value then
    begin
      FNotCollapsed := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetSameIdent(const Value: Boolean);
begin
  if FSameIdent <> Value then
    begin
      FSameIdent := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetHighlight(const Value: Boolean);
begin
  if FHighlight <> Value then
    begin
      FHighlight := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetInvertColors(const Value: Boolean);
begin
  if FInvertColors <> Value then
    begin
      FInvertColors := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetDisplayInTree(const Value: Boolean);
begin
  if FDisplayInTree <> Value then
    begin
      FDisplayInTree := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetCancelNextRules(const Value: Boolean);
begin
  if FCancelNextRules <> Value then
    begin
      FCancelNextRules := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetDynHighlight(
  const Value: TecDynamicHighlight);
begin
  if FDynHighlight <> Value then
    begin
      FDynHighlight := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetDynSelectMin(const Value: Boolean);
begin
  if FDynSelectMin <> Value then
    begin
      FDynSelectMin := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetGroupFmt(const Value: ecString);
begin
  if FGroupFmt <> Value then
    begin
      FGroupFmt := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetHighlightPos(const Value: TecHighlightPos);
begin
  if FHighlightPos <> Value then
    begin
      FHighlightPos := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetNameFmt(const Value: ecString);
begin
  if FNameFmt <> Value then
    begin
      FNameFmt := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetRefToCondEnd(const Value: Boolean);
begin
  if FRefToCondEnd <> Value then
    begin
      FRefToCondEnd := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetDrawStaple(const Value: Boolean);
begin
  if FDrawStaple <> Value then
    begin
      FDrawStaple := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetCollapseFmt(const Value: ecString);
begin
  if FCollapseFmt <> Value then
    begin
      FCollapseFmt := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetSelfClose(const Value: Boolean);
begin
  if FSelfClose <> Value then
    begin
      FSelfClose := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetNoEndRule(const Value: Boolean);
begin
  if FNoEndRule <> Value then
    begin
      FNoEndRule := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetGrammaRuleName(const Value: string);
begin
  if FGrammaRuleName <> Value then
   begin
    FGrammaRuleName := Value;
    FGrammaRule :=
      TSyntCollection(Collection).SyntOwner.Gramma.ParserRuleByName(Value);
   end;
end;

procedure TecTagBlockCondition.SetTokenType(const Value: integer);
begin
  if FTokenType <> Value then
    begin
      FTokenType := Value;
      Changed(False);
    end;
end;

function TecTagBlockCondition.GetTreeItemStyle: string;
begin
  Result := ValidStyleName(FTreeItemStyle, FTreeItemStyleObj);
end;

procedure TecTagBlockCondition.SetTreeItemStyle(const Value: string);
begin
  ValidSetStyle(Value, FTreeItemStyle, FTreeItemStyleObj);
end;

function TecTagBlockCondition.GetTreeGroupStyle: string;
begin
  Result := ValidStyleName(FTreeGroupStyle, FTreeGroupStyleObj);
end;

procedure TecTagBlockCondition.SetTreeGroupStyle(const Value: string);
begin
  ValidSetStyle(Value, FTreeGroupStyle, FTreeGroupStyleObj);
end;

procedure TecTagBlockCondition.SetTreeGroupImage(const Value: integer);
begin
  if FTreeGroupImage <> Value then
    begin
      FTreeGroupImage := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetTreeItemImage(const Value: integer);
begin
  if FTreeItemImage <> Value then
    begin
      FTreeItemImage := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetPen(const Value: TPen);
begin
  FPen.Assign(Value);
end;

procedure TecTagBlockCondition.SetUseCustomPen(const Value: Boolean);
begin
  if FUseCustomPen <> Value then
    begin
      FUseCustomPen := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetIgnoreAsParent(const Value: Boolean);
begin
  if FIgnoreAsParent <> Value then
    begin
      FIgnoreAsParent := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetAutoCloseText(Value: ecString);
begin
  if Value = sLineBreak then
    Value := '';
  if FAutoCloseText <> Value then
    begin
      FAutoCloseText := Value;
      Changed(False);
    end;
end;

procedure TecTagBlockCondition.SetAutoCloseMode(const Value: TecAutoCloseMode);
begin
  if FAutoCloseMode <> Value then
    begin
      FAutoCloseMode := Value;
      Changed(False);
    end;
end;

{ TecBlockRuleCollection }

function TecBlockRuleCollection.Add: TecTagBlockCondition;
begin
  Result := inherited Add as TecTagBlockCondition;
end;

constructor TecBlockRuleCollection.Create;
begin
  inherited Create(TecTagBlockCondition);
end;

function TecBlockRuleCollection.GetItem(Index: integer): TecTagBlockCondition;
begin
  Result := inherited Items[Index] as TecTagBlockCondition;
end;

{ TecTokenRule }

procedure TecTokenRule.AssignTo(Dest: TPersistent);
var dst: TecTokenRule;
begin
  inherited;
  if Dest is TecTokenRule then
   begin
    dst := Dest as TecTokenRule;
    dst.FTokenType := FTokenType;
    dst.FRegExpr.Expression := Expression;
    dst.OnMatchToken := OnMatchToken;
    dst.ColumnFrom := ColumnFrom;
    dst.ColumnTo := ColumnTo;
   end;
end;

constructor TecTokenRule.Create(Collection: TCollection);
begin
  inherited;
  FBlock := nil;
  FFormat := nil;
  FRegExpr := TecRegExpr.Create;
  SetDefaultModifiers(FRegExpr);
end;

destructor TecTokenRule.Destroy;
begin
  FreeAndNil(FRegExpr);
  inherited;
end;

function TecTokenRule.GetExpression: ecString;
begin
  Result := FRegExpr.Expression;
end;

function TecTokenRule.GetIsInvalid: Boolean;
begin
  Result := FRegExpr.IsInvalid;
end;

function TecTokenRule.GetItemBaseName: string;
begin
  Result := 'Token rule';
end;

function TecTokenRule.Match(const Source: ecString; Pos: integer): integer;
begin
 try
  Result := FRegExpr.MatchLength(Source, Pos);
 except
  Result := 0;
 end;
end;

procedure TecTokenRule.SetColumnFrom(const Value: integer);
begin
  if FColumnFrom <> Value then
    begin
      FColumnFrom := Value;
      Changed(False);
    end;
end;

procedure TecTokenRule.SetColumnTo(const Value: integer);
begin
  if FColumnTo <> Value then
    begin
      FColumnTo := Value;
      Changed(False);
    end;
end;

procedure TecTokenRule.SetExpression(const Value: ecString);
begin
  try
    FRegExpr.Expression := Value;
  except
    Application.HandleException(Self);
  end;
  Changed(False);
end;

procedure TecTokenRule.SetTokenType(const Value: integer);
begin
  if FTokenType <> Value then
    begin
      FTokenType := Value;
      Changed(False);
    end;
end;

{ TRuleCollectionItem }

function TRuleCollectionItem.ValidStyleName(const AStyleName: string;
  AStyle: TecSyntaxFormat): string;
var FSynt: TecSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner;
  Result := '';
  if not Assigned(FSynt) then Exit;

  if csLoading in FSynt.ComponentState then
    Result := AStyleName
  else
   if Assigned(AStyle) then
    Result := AStyle.DisplayName;
end;

function TRuleCollectionItem.ValidSetStyle(const AStyleName: string;
  var AStyleField: string; var AStyle: TecSyntaxFormat): string;
var FSynt: TecSyntAnalyzer;
begin
  Result := '';
  FSynt := TSyntCollection(Collection).SyntOwner;
  if not Assigned(FSynt) then Exit;
  if csLoading in FSynt.ComponentState then
    AStyleField := AStyleName
  else
    AStyle := TecSyntaxFormat(FSynt.FFormats.ItemByName(AStyleName));
  Changed(False);
end;

function TRuleCollectionItem.GetStyleName: string;
begin
  Result := ValidStyleName(FStyleName, FFormat);
end;

procedure TRuleCollectionItem.SetStyleName(const Value: string);
begin
  ValidSetStyle(Value, FStyleName, FFormat);
end;

procedure TRuleCollectionItem.Loaded;
var FSynt: TecSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner;
  if not Assigned(FSynt) then Exit;
  if FStyleName <> '' then
    FFormat := TecSyntaxFormat(FSynt.FFormats.ItemByName(FStyleName));
  if FBlockName <> '' then
    FBlock := TecTagBlockCondition(FSynt.BlockRules.ItemByName(FBlockName));
end;

function TRuleCollectionItem.GetBlockName: string;
var FSynt: TecSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner;
  if not Assigned(FSynt) then Exit;

  if csLoading in FSynt.ComponentState then
    Result := FBlockName
  else
   if Assigned(FBlock) then
    Result := FBlock.DisplayName
   else
    Result := '';
end;

procedure TRuleCollectionItem.SetBlockName(const Value: string);
var FSynt: TecSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner;
  if not Assigned(FSynt) then Exit;
  if csLoading in FSynt.ComponentState then
    FBlockName := Value
  else
   begin
//    FBlock := TecTagBlockCondition(FSynt.BlockRules.ItemByName(Value));
    FBlock := TecTagBlockCondition(FSynt.BlockRules.ItemByName(Value));
    Changed(False);
   end;
end;

procedure TRuleCollectionItem.AssignTo(Dest: TPersistent);
var dst: TRuleCollectionItem;
begin
  inherited;
  if Dest is TRuleCollectionItem then
    begin
      dst := Dest as TRuleCollectionItem;
      dst.StyleName := StyleName;
      dst.BlockName := BlockName;
      dst.StrictParent := StrictParent;
      dst.NotParent := NotParent;
      dst.AlwaysEnabled := AlwaysEnabled;
      dst.StatesAbsent := StatesAbsent;
      dst.StatesAdd := StatesAdd;
      dst.StatesRemove := StatesRemove;
      dst.StatesPresent := StatesPresent;
    end;
end;

procedure TRuleCollectionItem.SetNotParent(const Value: Boolean);
begin
  if FNotParent <> Value then
    begin
      FNotParent := Value;
      Changed(False);
    end;
end;

procedure TRuleCollectionItem.SetStrictParent(const Value: Boolean);
begin
  if FStrictParent <> Value then
    begin
      FStrictParent := Value;
      Changed(False);
    end;
end;

procedure TRuleCollectionItem.SetAlwaysEnabled(const Value: Boolean);
begin
  if FAlwaysEnabled <> Value then
    begin
      FAlwaysEnabled := Value;
      Changed(False);
    end;
end;

function TRuleCollectionItem.GetSyntOwner: TecSyntAnalyzer;
begin
  Result := TSyntCollection(Collection).SyntOwner;
end;

procedure TRuleCollectionItem.SetStatesAdd(const Value: integer);
begin
  if FStatesAdd <> Value then
    begin
      FStatesAdd := Value;
      Changed(False);
    end;
end;

procedure TRuleCollectionItem.SetStatesAbsent(const Value: integer);
begin
  if FStatesAbsent <> Value then
    begin
      FStatesAbsent := Value;
      Changed(False);
    end;
end;

procedure TRuleCollectionItem.SetStatesRemove(const Value: integer);
begin
  if FStatesRemove <> Value then
    begin
      FStatesRemove := Value;
      Changed(False);
    end;
end;

procedure TRuleCollectionItem.SetStatesPresent(const Value: integer);
begin
  if FStatesPresent <> Value then
    begin
      FStatesPresent := Value;
      Changed(False);
    end;
end;

{ TecTokenRuleCollection }

function TecTokenRuleCollection.Add: TecTokenRule;
begin
  Result := inherited Add as TecTokenRule;
end;

constructor TecTokenRuleCollection.Create;
begin
  inherited Create(TecTokenRule);
end;

function TecTokenRuleCollection.GetItem(Index: integer): TecTokenRule;
begin
  Result := inherited Items[Index] as TecTokenRule;
end;

{ TecParserResults }

constructor TecParserResults.Create(AOwner: TecSyntAnalyzer;
  ABuffer: TATStringBuffer; const AClient: IecSyntClient);
begin
  inherited Create;
  if ABuffer = nil then
    raise Exception.Create('TextBuffer not passed to parser');
  FOwner := AOwner;
  FBuffer := ABuffer;
  FClient := AClient;
  FTagList := TecTokenList.Create(False);
  FSubLexerBlocks := TecSubLexerRanges.Create;
  FOwner.FClientList.Add(Self);
  FCurState := 0;
  FStateChanges := TecRangeList.Create;
end;

destructor TecParserResults.Destroy;
begin
  FOwner.FClientList.Remove(Self);
  FreeAndNil(FTagList);
  FreeAndNil(FSubLexerBlocks);
  FreeAndNil(FStateChanges);
  inherited;
end;

procedure TecParserResults.Clear;
begin
  FTagList.Clear;
  FSubLexerBlocks.Clear;
  FStateChanges.Clear;
  FCurState := 0;
end;

procedure TecParserResults.Finished;
begin
  FFinished := True;
  // Performs Gramma parsing
  //AnalyzeGramma;
  //FTagList.UpdateIndexer; //AT
end;

function TecParserResults.IsEnabled(Rule: TRuleCollectionItem;
  OnlyGlobal: Boolean): Boolean;
begin
  Result := Rule.Enabled and (not OnlyGlobal or Rule.AlwaysEnabled) and
            ((Rule.StatesPresent = 0) or ((FCurState and Rule.StatesPresent) = Rule.StatesPresent)) and
            ((Rule.StatesAbsent = 0) or ((FCurState and Rule.StatesAbsent) = 0));
end;

function TecParserResults.GetTokenCount: integer;
begin
  Result := FTagList.Count;
end;

function TecParserResults.GetTags(Index: integer): TecSyntToken;
begin
  Result := FTagList[Index];
end;

function TecParserResults.GetTokenStr(Index: integer): ecString;
begin
  if Index >= 0 then
    with Tags[Index] do
      Result := FBuffer.SubString(Range.StartPos + 1, Range.EndPos - Range.StartPos)
  else
    Result := '';
end;

function TecParserResults.GetLastPos(const Source: ecString): integer;
begin
  if FTagList.Count = 0 then Result := 1 else
    Result := FTagList[FTagList.Count - 1].Range.EndPos + 1;
  if FLastAnalPos > Result then Result := FLastAnalPos;
end;

procedure TecParserResults.SaveState;
var b: Boolean;
begin
 if FStateChanges.Count = 0 then
   b := FCurState <> 0
 else
   b := FCurState <> FStateChanges.Last.EndPos;
 if b then
   FStateChanges.Add(TRange.Create(FTagList.Count, FCurState));
end;

// True if end of the text
function TecParserResults.ExtractTag(const Source: ecString; var FPos: integer
  ): Boolean;
var N: integer;
    p: TecSyntToken;
    own: TecSyntAnalyzer;

   // Select current lexer
   procedure GetOwner;
   var i, N: integer;
     Sub: TecSubLexerRange;
   begin
    own := FOwner;
    for i := FSubLexerBlocks.Count - 1 downto 0 do
     begin
       Sub:= FSubLexerBlocks[i];
       if FPos > Sub.Range.StartPos then
        if Sub.Range.EndPos = -1 then
          begin
            // try close sub lexer
    //        if Rule.ToTextEnd then N := 0 else
            N := Sub.Rule.MatchEnd(Source, FPos);
            if N > 0 then
             begin
               if Sub.Rule.IncludeBounds then
                 begin // New mode in v2.35
                   Sub.Range.EndPos := FPos - 1 + N;
                   Sub.Range.PointEnd := FBuffer.StrToCaret(Sub.Range.EndPos);
                   Sub.CondEndPos := Sub.Range.EndPos;
                   own := Sub.Rule.SyntAnalyzer;
                 end else
                 begin
                   Sub.Range.EndPos := FPos - 1;
                   Sub.Range.PointEnd := FBuffer.StrToCaret(Sub.Range.EndPos);
                   Sub.CondEndPos := Sub.Range.EndPos + N;
                 end;
               // Close ranges which belongs to this sub-lexer range
               CloseAtEnd(FTagList.PriorAt(Sub.Range.StartPos));
               FSubLexerBlocks[i] := Sub; // Write back to list
             end else
             begin
               own := Sub.Rule.SyntAnalyzer;
               Exit;
             end;
          end else
       if FPos < Sub.Range.EndPos then
         begin
               own := Sub.Rule.SyntAnalyzer;
               Exit;
         end;
    end;
   end;

   procedure CheckIntersect;
   var i: integer;
     Sub: TecSubLexerRange;
   begin
    for i := FSubLexerBlocks.Count - 1 downto 0 do
    begin
      Sub := FSubLexerBlocks[i];
      if (p.Range.EndPos > Sub.Range.StartPos) and (p.Range.StartPos < Sub.Range.StartPos) then
       begin
        p.Range.EndPos := Sub.Range.StartPos;
        p.Range.PointEnd := FBuffer.StrToCaret(p.Range.EndPos);
        Exit;
       end;
    end;
   end;

   function CanOpen(Rule: TecSubAnalyzerRule): Boolean;
   var N: integer;
       sub: TecSubLexerRange;
   begin
     Result := IsEnabled(Rule, False) and (Rule.SyntAnalyzer <> nil);
     if not Result then Exit;
     Result := Rule.FromTextBegin and (FPos = 1);
     if Result then N := 0 else
                    N := Rule.MatchStart(Source, FPos);
     Result := Result or (N > 0);
     if not Result then Exit;
     // To prevent repeated opening
     if FSubLexerBlocks.Count > 0 then
     begin
       sub := FSubLexerBlocks.Last;
       if (sub.Range.EndPos = FPos - 1) and
          (sub.Rule = Rule) then Exit;
     end;

     ApplyStates(Rule);

     FillChar(sub, SizeOf(sub), 0);
     sub.Rule := Rule;
     sub.CondStartPos := FPos - 1;
     if Rule.IncludeBounds then
       sub.Range.StartPos := FPos - 1
     else
       sub.Range.StartPos := FPos + N - 1;
     sub.Range.EndPos := -1;
     sub.Range.PointStart := FBuffer.StrToCaret(sub.Range.StartPos);
     sub.CondEndPos := -1;
     FSubLexerBlocks.Add(sub);
   end;


   procedure TryOpenSubLexer;
   var i: integer;
   begin
     for i := 0 to own.SubAnalyzers.Count - 1 do
      if CanOpen(own.SubAnalyzers[i]) then Exit;
     if own <> FOwner then
      for i := 0 to FOwner.SubAnalyzers.Count - 1 do
       if FOwner.SubAnalyzers[i].AlwaysEnabled and CanOpen(FOwner.SubAnalyzers[i]) then Exit;
   end;

var
  NNextPos: integer;
begin
  GetOwner;
  TryOpenSubLexer;
  if own.SkipSpaces then
    begin
     if own.ParseEndOfLine then N := SkipSpacesNoLineBreak(Source, FPos)
      else N := SkipSpaces(Source, FPos);
    end
   else if FPos > Length(Source) then N := -1 else N := 0;
  TryOpenSubLexer;
  GetOwner;

  Result := N = -1;
  if Result then Exit;

  p := FOwner.GetToken(Self, Source, FPos, own <> FOwner);
  if (own <> FOwner) and (p.Range.StartPos < 0) then
    p := own.GetToken(Self, Source, FPos, False);
  if p.Range.StartPos < 0 then  // no token
   begin
     NNextPos := FPos;
     SkipSpaces(Source, NNextPos); // needed for huge space-only lines, where Inc(FPos) is very slow
     if NNextPos > FPos then
       FPos := NNextPos
     else
       Inc(FPos);
   end else
   begin
    CheckIntersect;
    SaveState;
    FTagList.Add(p);
    if not FOwner.SeparateBlockAnalysis then
     begin
      FOwner.SelectTokenFormat(Self, Source, own <> FOwner);
      if own <> FOwner then
        own.SelectTokenFormat(Self, Source, False);
     end else
//    if not IsIdle then
     begin  // Only for first iteration of analysis
      FOwner.HighlightKeywords(Self, Source, own <> FOwner);
      if own <> FOwner then
        own.HighlightKeywords(Self, Source, False);
     end;
    FPos := p.Range.EndPos + 1;
   end;
   FLastAnalPos := FPos;
end;

function TecParserResults.AnalyzerAtPos(APos: integer): TecSyntAnalyzer;
var
  N: integer;
  Rng: TecSubLexerRange;
begin
  Result := FOwner;
  if APos < 0 then Exit;
  N := FSubLexerBlocks.PriorAt(APos);
  if N < 0 then Exit;
  Rng := FSubLexerBlocks.Items[N];
  if (Rng.Range.StartPos<=APos) and (APos<Rng.Range.EndPos) then
    Result := Rng.Rule.SyntAnalyzer;
 {
 for i := 0 to FSubLexerBlocks.Count - 1 do
  with FSubLexerBlocks[i] do
   if APos < Range.StartPos then Break else
    if (Range.EndPos = -1) or (APos < Range.EndPos) then
      Result := Rule.SyntAnalyzer;
  }
end;

function TecParserResults.GetSubLexerRangeCount: integer;
begin
  Result := FSubLexerBlocks.Count;
end;

function TecParserResults.GetSubLexerRange(Index: integer): TecSubLexerRange;
begin
  Result := FSubLexerBlocks[Index];
end;

procedure TecParserResults.SetTags(Index: integer; const AValue: TecSyntToken);
begin
  FTagList[Index] := AValue
end;

function TecParserResults.GetTokenType(Index: integer): integer;
begin
  Result := Tags[Index].TokenType;
end;

procedure TecParserResults.ApplyStates(Rule: TRuleCollectionItem);
begin
  if Rule.StatesRemove <> 0 then
    FCurState := FCurState and not Rule.StatesRemove;
  if Rule.StatesAdd <> 0 then
    FCurState := FCurState or Rule.StatesAdd;
end;

procedure TecParserResults.RestoreState;
var i: integer;
begin
  for i := FStateChanges.Count - 1 downto 0 do
    if FStateChanges.Last.StartPos >= TagCount then
      FStateChanges.Delete(FStateChanges.Count - 1)
    else
      Break;
  if FStateChanges.Count > 0 then
    FCurState := FStateChanges.Last.EndPos
  else
    FCurState := 0;
end;

function TecParserResults.ParserStateAtPos(TokenIndex: integer): integer;
var i: integer;
begin
   for i := FStateChanges.Count - 1 downto 0 do
     if FStateChanges[i].StartPos <= TokenIndex then
       begin
         Result := FStateChanges[i].EndPos;
         Exit;
       end;
   Result := 0;
end;

{ TecClientSyntAnalyzer }

constructor TecClientSyntAnalyzer.Create(AOwner: TecSyntAnalyzer; SrcProc: TATStringBuffer;
  const AClient: IecSyntClient);
begin
  inherited Create( AOwner, SrcProc, AClient);
  FRanges := TSortedList.Create(True);
  FOpenedBlocks := TSortedList.Create(False);
  FPrevProgress := -1;

  FTimerIdle := TTimer.Create(nil);
  FTimerIdle.OnTimer := TimerIdleTick;
  FTimerIdle.Enabled := False;
  FTimerIdle.Interval := 100;

  IdleAppend;
end;

destructor TecClientSyntAnalyzer.Destroy;
begin
  if Assigned(FTimerIdle) then
  begin
    DoStopTimer(true);
    FreeAndNil(FTimerIdle);
  end;

  FreeAndNil(FRanges);
  FreeAndNil(FOpenedBlocks);
  inherited;
end;

function TecClientSyntAnalyzer.Stop: boolean;
begin
  FFinished := true;
  Result := DoStopTimer(true);
end;

procedure TecClientSyntAnalyzer.Clear;
begin
  inherited;
  FRepeateAnalysis := False;
  FTagList.Clear;
  FRanges.Clear;
  FOpenedBlocks.Clear;

  DoStopTimer(false);
  FFinished := False;
  FLastAnalPos := 0;
  FStartSepRangeAnal := 0;

  IdleAppend;
end;

procedure TecClientSyntAnalyzer.AddRange(Range: TecTextRange);
begin
  Range.Index := FRanges.Count;
  FRanges.Add(Range);
  if FOpenedBlocks.Count > 0 then
    Range.Parent := TecTextRange(FOpenedBlocks[FOpenedBlocks.Count - 1]);
  if Range.EndIdx = -1 then
    FOpenedBlocks.Add(Range);
end;

function IndentOf(const S: ecString): Integer;
var
  i: Integer;
begin
  Result:= 0;
  for i:= 1 to Length(S) do
    case S[i] of
      ' ': Inc(Result);
      #9: Inc(Result, 4);
      else Break;
   end;
end;

function TecClientSyntAnalyzer.CloseRange(Cond: TecTagBlockCondition; RefTag: integer): Boolean;
var j: integer;
    b: boolean;
begin
  for j := FOpenedBlocks.Count - 1 downto 0 do
   with TecTextRange(FOpenedBlocks[j]) do
     if Assigned(Rule) then
       begin
         if Cond.BlockType = btRangeStart then
           b := Cond.SelfClose and (Rule = Cond)
         else
           b := (Rule.FBlockEndCond = Cond) or (Rule = Cond.FBlockEndCond);
         if b then
           begin
             if Cond.SameIdent and not SameText(TagStr[RefTag - Cond.IdentIndex] , TagStr[IdentIdx]) then Continue;
             EndIdx := RefTag - Cond.BlockOffset;
             if (Rule = Cond) and (EndIdx > 0) then Dec(EndIdx); // for self closing
             FEndCondIndex := RefTag;
             if Assigned(Owner.OnCloseTextRange) then
               Owner.OnCloseTextRange(Self, TecTextRange(FOpenedBlocks[j]), StartIdx, EndIdx);
             FOpenedBlocks.Delete(j);
             Result := True;
             Exit;
           end;
       end;
  Result := False;
end;

function TecClientSyntAnalyzer.HasOpened(Rule: TRuleCollectionItem; Parent: TecTagBlockCondition; Strict: Boolean): Boolean;
var i: integer;
    prn: TecTagBlockCondition;
begin
  if Strict then
    begin
      if FOpenedBlocks.Count > 0 then
        begin
          i := FOpenedBlocks.Count - 1;
          prn := TecTextRange(FOpenedBlocks[i]).Rule;
          if (Rule is TecTagBlockCondition) and TecTagBlockCondition(Rule).SelfClose and (prn = Rule) then
            Dec(i);
          repeat
            if i < 0 then
              begin
                Result := False;
                Exit;
              end;
            prn := TecTextRange(FOpenedBlocks[i]).Rule;
            Dec(i);
          until not prn.IgnoreAsParent;
          Result := prn = Parent;
        end else Result := Parent = nil;
    end
  else
    begin
      Result := True;
      if Parent = nil then Exit;
      for i := FOpenedBlocks.Count - 1 downto 0 do
        if TecTextRange(FOpenedBlocks[i]).Rule = Parent then
          Exit;
      Result := False;
    end;
end;

procedure TecClientSyntAnalyzer.Finished;
var i: integer;
  Sub: TecSubLexerRange;
begin
  if FFinished then Exit;
  inherited Finished;

  // Close SubLexers at the End of Text
  for i := FSubLexerBlocks.Count - 1 downto 0 do
  begin
    Sub := FSubLexerBlocks[i];
    if (Sub.Range.EndPos = -1) and Sub.Rule.ToTextEnd then
     begin
       Sub.Range.EndPos := FBuffer.TextLength{ - 1};
       Sub.Range.PointEnd := Point(
                      FBuffer.LineLength(FBuffer.Count-1),
                      FBuffer.Count-1); //at end
       Sub.CondEndPos := Sub.Range.EndPos;
       FSubLexerBlocks[i] := Sub;
     end;
  end;

  // Close blocks at the end of text
  CloseAtEnd(0);

  FRepeateAnalysis := True;
end;

procedure TecClientSyntAnalyzer.TimerIdleTick(Sender: TObject);
var FPos, tmp, i: integer;
    own: TecSyntAnalyzer;
    Progress: integer;
const
  ProgressStep = 3;
  ProgressMinPos = 2000;
begin
  if FTimerIdleIsBusy or FDisableIdleAppend then Exit;
  FTimerIdle.Enabled := False;
  FTimerIdleMustStop := False;
  FTimerIdleIsBusy := True;
  FPos := 0;

  try
    while not FTimerIdleMustStop and not FFinished do
    begin
      tmp := GetLastPos(FBuffer.FText);
      if tmp > FPos then FPos := tmp;

      if FPos < ProgressMinPos then
        Progress := 0
      else
        Progress := FPos * 100 div FBuffer.TextLength div ProgressStep * ProgressStep;
      if Progress <> FPrevProgress then
      begin
        FPrevProgress := Progress;
        if Assigned(OnLexerParseProgress) then
          OnLexerParseProgress(Owner, Progress);
      end;

      if ExtractTag(FBuffer.FText, FPos{, True}) then
      begin
        if FOwner.SeparateBlockAnalysis then
          for i := FStartSepRangeAnal + 1 to TagCount do
            begin
              own := Tags[i - 1].Rule.SyntOwner;
              FOwner.SelectTokenFormat(Self, FBuffer.FText, own <> FOwner, i);
              if own <> FOwner then
                own.SelectTokenFormat(Self, FBuffer.FText, False, i);

              Application.ProcessMessages;
              if Application.Terminated then Exit;
              if FTimerIdleMustStop then Exit;
            end;
        Finished;
      end
      else
      begin
        Application.ProcessMessages;
        if Application.Terminated then Exit;
        if FTimerIdleMustStop then Exit;
      end;
    end;
  finally
    FTimerIdleIsBusy := False;
  end;
end;

procedure TecClientSyntAnalyzer.IdleAppend;
begin
  //sets FTimerIdle interval and restarts it
  if not FFinished then
  begin
    FTimerIdle.Enabled := False;
    if FRepeateAnalysis then
      FTimerIdle.Interval := Owner.IdleAppendDelay
    else
      FTimerIdle.Interval := Owner.IdleAppendDelayInit;
    FTimerIdle.Enabled := True;
  end;
end;

procedure TecClientSyntAnalyzer.AppendToPos(APos: integer; AUseTimer: boolean=true);
var FPos: integer;
begin
  if FBuffer.TextLength = 0 then Exit;
  if FFinished then Exit;
  FPos := GetLastPos(FBuffer.FText);
  while FPos - 1 <= APos + 1 do
   begin
     if ExtractTag(FBuffer.FText, FPos{, False}) then
      begin
       if not FOwner.SeparateBlockAnalysis then
         Finished else
       if not FTimerIdleIsBusy then
         if AUseTimer then
           IdleAppend
         else
           TimerIdleTick(nil);
       Break;
      end;
   end;
end;

procedure TecClientSyntAnalyzer.ChangedAtPos(APos: integer);
var i, N: integer;
  Sub: TecSubLexerRange;

 procedure CleanRangeList(List: TSortedList; IsClosed: Boolean);
 var i: integer;
 begin
   for i := List.Count - 1 downto 0 do
    with TecTextRange(List[i]) do
     if (FCondIndex >= N) or (StartIdx >= N) or IsClosed and
        ((FEndCondIndex >= N) or (EndIdx >= N)) then
      List.Delete(i);
 end;

begin
{ if FBuffer.TextLength <= Owner.FullRefreshSize then
  begin
   Clear;
   Exit;
  end;}

   FFinished := False;
   Dec(APos);
   if APos<0 then
     APos := 0;
   DoStopTimer(false);

   if FBuffer.TextLength <= Owner.FullRefreshSize then
     APos := 0
   else
   if Owner.RestartFromLineStart then
     APos := Min(APos, FBuffer.OffsetToOffsetOfLineStart(APos + 1));

   // Check sub lexer ranges
   for i := FSubLexerBlocks.Count - 1 downto 0 do
   begin
     Sub:= FSubLexerBlocks[i];
     if APos < Sub.Range.StartPos then
      begin
        if APos > Sub.CondStartPos then APos := Sub.CondStartPos;
        FSubLexerBlocks.Delete(i);  // remove sub lexer
      end else
     if APos < Sub.CondEndPos then
      begin
        if APos > Sub.Range.EndPos then APos := Sub.Range.EndPos;
        Sub.Range.EndPos := -1;       // open sub lexer
        Sub.CondEndPos := -1;
        FSubLexerBlocks[i] := Sub;
      end;
   end;
   // Remove tokens
   FTagList.ClearFromPos(APos);

   FLastAnalPos := 0;   // Reset current position
   N := FTagList.Count;
   FStartSepRangeAnal := N;
   // Remove text ranges from service containers
   CleanRangeList(FOpenedBlocks, False);
   // Remove text ranges from main storage
   for i := FRanges.Count - 1 downto 0 do
    with TecTextRange(FRanges[i]) do
     if (FCondIndex >= N) or (StartIdx >= N) then FRanges.Delete(i)  else
      if (FEndCondIndex >= N) or (EndIdx >= N) then
       begin
         EndIdx := -1;
         FEndCondIndex := -1;
         FOpenedBlocks.Add(FRanges[i]);
       end;

   // Restore parser state
   RestoreState;

 IdleAppend;
end;

function TecClientSyntAnalyzer.PriorTokenAt(Pos: integer): integer;
begin
  Result := FTagList.PriorAt(Pos);
end;

function TecClientSyntAnalyzer.GetRangeCount: integer;
begin
  Result := FRanges.Count;
end;

function TecClientSyntAnalyzer.GetRanges(Index: integer): TecTextRange;
begin
  Result := TecTextRange(FRanges[Index]);
end;

procedure TecClientSyntAnalyzer.Analyze(ResetContent: Boolean);
var OldSep: integer;
begin
  if IsFinished then Exit;
  if ResetContent then
    begin
      OldSep := FOwner.FSeparateBlocks;
      FOwner.FSeparateBlocks := 2; // disanle separation analysis
      Clear;
      AppendToPos(FBuffer.TextLength);
      FOwner.FSeparateBlocks := OldSep;
    end else
    begin
      AppendToPos(FBuffer.TextLength);
    end;
end;

procedure TecClientSyntAnalyzer.CompleteAnalysis;
var own: TecSyntAnalyzer;
    i: integer;
begin
  AppendToPos(FBuffer.TextLength);
  if FOwner.SeparateBlockAnalysis then
    for i := FStartSepRangeAnal + 1 to TagCount do
     begin
      own := Tags[i - 1].Rule.SyntOwner;
      FOwner.SelectTokenFormat(Self, FBuffer.FText, own <> FOwner, i);
      if own <> FOwner then
        own.SelectTokenFormat(Self, FBuffer.FText, False, i);
      DoStopTimer(true);
      Finished;
     end;
end;

function TecClientSyntAnalyzer.RangeFormat(const FmtStr: ecString;
  Range: TecTextRange): ecString;
var i, j, idx, N, to_idx: integer;
    rng: TecTextRange;
    LineMode: integer;

{ HAW: hans@werschner.de [Oct'07] ......... additions to formatting token parts ......

     I have added syntax to each single ".... %xyz ...." clause processing

     a) %(S|E)P*(L|Z)?[0-9]+  may be expanded to

        %(S|E)P*([\[]<token>[\]]<offset>?)?

          where <token> is a specific token that is "searched from the specified
          starting point (S for first token in the range , or E for the last token)
          towards the respective range end (up- or downwards). The search-direction
          is kept in the variable "rngdir" which is set in the "S" , "E" decision.

          example:  line(s) is/are ".... for x = 1 to 12 do ...... end ;  ..."
                                         0   1 2 3 4  5  6  ...    27  28

          range-start = "for", range-end = "end"

          then "...%s[to] ..." will skip forward to the token "to" (with index 4).

          The token values are searched on a "asis" basis, there is no case-insensitivity
          option yet.

          A "numeric number following the token value will define an <offset> relative
          to the found token.

          For this clause, the variable "idx" is not set by taking the static
          numeric value as in "...%s2 ..." , instead the "found token index" is kept.

          For "%S..." the search starts at idx=0 up to max 28.     ---> rngdir = +1;
          For "%E..." the search starts at idx=28 downto min 0.    ---> rngdir = -1;

          The options L or Z introduced in V2.35 will not combine with the new (range)
          specifying options --> somebody else may find a use for such extended ranges.

          Notes:  Avoid to search for tokens that can occur at multiple places (for
                  example a ";" between statements).

                  The above syntax is simple as it allows to identify the

                    block-start-tokens      "for x = 1 to 12 do"

                    block-body                anything after block-start tokens up to

                    block-end-tokens        "end ;"

                  but many syntax formats do not trivially support this separation.

                  The current implementation does not provide the information where
                  "block-start", "block-body" and "block-end" are beginning/ending.
                  A "%B0..." for the "block-body" portion and a "ignore block-body
                  tokens" option may be nice !?

     b) any such clause (either absolute or given by token value) can "start a token
        range" by additionally specifying:

          %(S|E)...~(S|E)P*[0-9]+

        or

          %(S|E)...~(S|E)P*([\[]<token>[\]]<offset>?)?

        or

          %(S|E)...~[\[]<token>[\]]

        The first form uses the static index specification to define the end-range:

          "%s0~s3"        results in "for x = 1"            (tokens 0, 1, ... 3)

        The 2nd form uses the new syntax to "search for an end-token beginning at the
        starting range index (idx) up- or down-wards.

          "%s0~s[do]"     results in "for x = 1 to 12 do"   (tokens 0, 1, ... 6)

          if a search is not satisfied, the complete range up to "e0" is taken.

          Because of the same "S", the search starts with "TagStr[idx]" ...

          "s0~e[do]"      results in the same string, but starts at the final "end"
                          of the block and scanning downwards.

          Caution: This may produce WRONG results if nested loops are scanned !

                   I could not find a valid representation of "range-start" token-
                   streams, the range-body alone and/or the range-end token-stream.

                   Such information may be helpful to better display blocks and/or
                   collapse display of the "block-body" alone.

        The 3rd form is an abbreviation where the S/E indicators are taken to be
        identical as the starting point

          "S1~[do]1"      results in "x = 1 to 12"          (tokens 1, 2, ... 5)

                          The <offset> "1" will here skip back by 1 from the found
                          token "do".

        The range-end is kept in the variable "to_idx".

        The "token-value" to search for can not be whitespace #00..#20. Leading and
        trailing whitespace withing the "...[vvvvvvv] ..." enclosed by [ and ]
        characters sequence is removed before searching. the "vvvvvv" can contain
        escaped characters like "... [\]] ..." to allow "[" and/or "]" to be part
        of the value. The \r, \n, \f ...escapes are not supported here.

        The token accumulation simply (?) takes all tokens from "idx" ... "to_idx"
        and builds a string by appending all tokens with ONE " " (blank) as separating
        delimiter. There is no process to keep the original token positions within
        the source line(s) and any whitepace including cr/lf's there. This may be an
        addition but I currently do not see a need for it.

     c) "ranges as specified above may accumulate many tokens and it may be desirable
        to "limit" the result string.

        This can be done by using another operand syntax

          %(S|E)...~[0-9]+

        or

          %(S|E)...~(S|E)([\[]<token>[\]]<offset>?)?~[0-9]+

        or

          %(S|E)...~[\[]<token>[\]]~[0-9]+

        In all three forms the "~" is immediately followed by a numeric value which
        is interpreted as

          "maximum number of tokens in the substituted string", if the range takes
          MORE than this maximum

        The value is internally kept in the variable "rngmax" below.

        When the result string is accumulated (taking all tokens between "idx" up-
        resp. down-to "to_idx") the number of appended tokens can not go beyond "rngmax".

        If this happens the result will be created in the form "t-1 t-2 -- t-max ..." with
        the ellipsis string " ..."  appended.

     d) There is another addition to the token clause syntax which is NOT YET operational:

        I am not too happy that the collapsed string displayed is completely in "grey"
        colour. As I want to have some control over "highlighting" in this string also,
        I tried to add some style-reference information to the token clause.

        *** I currently do not yet understand HOW to activate such a style within
            the results of this formatting, but I will TRY !!

        OK, the added syntax for styles for future use ;-)

          .... %:<style>:(S|E) ....

        where the <style> is the alphanumeric name of a style as defined in the lexer
        definition and this style-name is "enclosed" with the ":" characters.

        The code keeps the found style-name in the variable "rngstyle" for any later
        use.

        This addition only assigns styles to the token substitution parts, but not to any
        text outside and/or the total "collapsed" text formatting. This may be some
        enhancement to define in the lexer GUI.

    Hans L. Werschner, Oct '07
}
var rngstyle: string;                      // HAW: add style identifier to range expression
    rngtoken, rngResult: string;           //      a few more vars
    swp_idx, rngdir, rngoffset, rngmax: integer;
    to_rng: TecTextRange;

function RangeNumber( const FmtStrNumber: string; var gotnbr: integer ): boolean;
begin
    N := 0; Result := false;
    while (j + N) <= length( FmtStrNumber ) do
      if (FmtStrNumber[j + N] >= '0') and (FmtStrNumber[j + N] <= '9') or (N = 0) and
         ((FmtStrNumber[j + N] = '+') or (FmtStrNumber[j + N] = '-'))
         then inc(N) else Break;
    if  N > 0  then  begin
      gotnbr := StrToInt( copy( FmtStrNumber, j, N ) );
      inc( j, N );
      Result := true;
    end;
end;

//var S_: string;
begin
  idx := 0;
  Result := FmtStr;
  try

   // HAW: obsolete -> to_idx := Length(Result);
   //      the variable "j" is now always pointing to the next character to process.
   //      Only during numeric sub-operand scan, the "N" will keep the found digits
   //      count. After such number, the var "j" is immediately adjusted again.

   for i := Length(Result) - 1 downto 1 do
    if Result[i] = '%' then
    begin
     j := i + 1;

     rngstyle := '';                  // HAW: keep style name
     if  Result[j] = ':' then  begin  // HAW: begin of embedded style name
       inc( j );
       while  (j <= length( Result ))  and  (Result[j] <> ':') do begin
         if  Result[j] > ' '  then
           rngstyle := rngstyle+Result[j];
         inc( j );
       end;
       if  (j > length( Result ))  or  (Result[j] <> ':')  then
         continue;
       inc( j );
       // now we have a style name, and can resume after "%:ssssss:"
     end;

     rng    := Range;
     rngdir := 1;                     // HAW: positive increment (for "%..e..")
                                      //      negative for "..e.." clauses
     rngmax := 1000000000;            // HAW: allow a great amount of tokens

     while ecUpCase(Result[j]) = 'P' do
      begin
       rng := rng.Parent;
       if (rng = nil) or (j = Length(Result)) then Continue;
       inc(j);
      end;

     case ecUpCase(Result[j]) of
       'S': idx := rng.StartIdx + rng.Rule.BlockOffset;
       'E': begin  rngdir := -1;      // HAW: mark downwards direction
                   if (rng.EndIdx <> -1) and Assigned(rng.Rule.BlockEndCond) then
                     idx := rng.EndIdx + rng.Rule.BlockEndCond.BlockOffset
                   else
                     idx := 1000000000;
            end;
       else continue;
     end;
     inc(j);

     case ecUpCase(Result[j]) of // <== v2.35
       'L': LineMode := 1; // from start of line
       'Z': LineMode := 2; // to end of line
       else LineMode := 0;
     end;
     if LineMode <> 0 then Inc(j);

     // HAW: check for "...s[token]..." instead of numeric index
     if  LineMode = 0  then
       if  (j < length( Result ))  and  (Result[j] = '[')  then  begin
         inc( j );  rngtoken := '';
         while  (j < length( Result ))  and  (Result[j] <> ']')  do  begin
           if  Result[j] = '\' then  inc( j );
           rngtoken := rngtoken + Result[j];  inc( j );
         end;
         if  j > length( Result ) then
           continue;
         while  (rngtoken <> '')  and  (rngtoken[length( rngtoken )] < ' ')  do
           rngtoken := copy( rngtoken, 1, length( rngtoken )-1 );
         while  (rngtoken <> '')  and  (rngtoken[1] < ' ')  do
           rngtoken := copy( rngtoken, 2, length( rngtoken )-1 );
         if  rngtoken = ''  then
           continue;
         inc( j );
         if  rngdir > 0 then  begin  // upwards search
           while  idx <= (rng.EndIdx + rng.Rule.BlockEndCond.BlockOffset) do  begin
             if  rngtoken = TagStr[idx]  then  break;
             inc( idx );
           end;
         end  else
         if  rngdir < 0 then         // downwards search
           while  idx >= (rng.StartIdx + rng.Rule.BlockOffset) do  begin
             if  rngtoken = TagStr[idx]  then  break;
             dec( idx );
           end;
         rngdir := 0;    // allow for missing <offset>
       end;
     if  not RangeNumber( Result, rngoffset )  then  begin
       if  rngdir <> 0 then
         Continue;
     end  else
       idx := idx - rngoffset;

     to_idx := idx;
     to_rng := rng;

     // HAW: now allow an explicit "to_idx" range by using "%from-idx~to-idx"
     if  (j < length( Result ))  and  (Result[j] = '~')  then
       // a numeric value alone sets just the maximum tokens to use
       if (Result[j+1] >= '0') and (Result[j+1] <= '9')  then  begin  // only positive values !
         to_idx := to_rng.EndIdx + to_rng.Rule.BlockEndCond.BlockOffset;
         LineMode := 3;
       end else
       begin

         if  LineMode <> 0  then  // not a good combination
           continue;
         // ... otherwise we have a real end-token clause
         inc( j );  // skip over the [

         rngdir := 1;
         if  Result[j] <> '['   then  begin
           // to_rng := Range;  // be sure that we start with the range itself
           while ecUpCase(Result[j]) = 'P' do
            begin
             to_rng := rng.Parent;
             if (to_rng = nil) or (j = Length(Result)) then Continue;
             inc(j);
            end;

           case ecUpCase(Result[j]) of
             'S': to_idx := to_rng.StartIdx + to_rng.Rule.BlockOffset;
             'E': begin
                    rngdir := -1;       // HAW: mark downwards direction
                    if (to_rng.EndIdx <> -1) and Assigned(to_rng.Rule.BlockEndCond) then
                     to_idx := to_rng.EndIdx + to_rng.Rule.BlockEndCond.BlockOffset
                    else
                     to_idx := 1000000000;
                  end;
             else continue;
           end;
           inc(j);
         end;
         if  (j < length( Result ))  and  (Result[j] = '[')  then  begin
           inc( j );  rngtoken := '';
           while  (j < length( Result ))  and  (Result[j] <> ']')  do  begin
             if  Result[j] = '\' then  inc( j );
             rngtoken := rngtoken + Result[j];  inc( j );
           end;
           if  j > length( Result ) then
             continue;
           while  (rngtoken <> '')  and  (rngtoken[length( rngtoken )] < ' ')  do
             rngtoken := copy( rngtoken, 1, length( rngtoken )-1 );
           while  (rngtoken <> '')  and  (rngtoken[1] < ' ')  do
             rngtoken := copy( rngtoken, 2, length( rngtoken )-1 );
           if  rngtoken = ''  then
             continue;
           inc( j );
           if  rngdir > 0 then  begin  // upwards search
             while  to_idx <= (rng.EndIdx + rng.Rule.BlockEndCond.BlockOffset) do  begin
               if  rngtoken = TagStr[to_idx]  then  break;
               inc( to_idx );
             end;
           end  else
           if  rngdir < 0 then         // downwards search
             while  to_idx >= (rng.StartIdx + rng.Rule.BlockOffset) do  begin
               if  rngtoken = TagStr[to_idx]  then  break;
               dec( to_idx );
             end;
           rngdir := 0;  // allow for missing <offset>
         end;
         if  not RangeNumber( Result, rngoffset )  then  begin
           if  rngdir <> 0 then
             Continue;
         end  else
           to_idx := to_idx - rngoffset;

         LineMode := 3;  // enforce new mode as we have an explicit range
       end;

     if  (j < length( Result ))  and
         (Result[j] = '~')         and
         (Result[j+1] >= '0') and (Result[j+1] <= '9')  // only positive values !
     then  begin  // we hav a "maximum" range value attached
       inc( j );
       if  not RangeNumber( Result, rngmax ) then
         Continue;
     end;
     // HAW: ... end of added range checking ,
     //      variable "j" points to first character AFTER all clauses
     Delete(Result, i, j - i);

     if (idx >= 0) and (idx < FTagList.Count) then
       case LineMode of
         0: Insert(TagStr[idx], Result, i);
         1: begin
              N := FBuffer.OffsetToOffsetOfLineStart(Tags[idx].Range.StartPos);
              to_idx := Tags[idx].Range.EndPos;
              Insert(FBuffer.SubString(N, to_idx - N + 1), Result, i);
            end;
         2: begin
              to_idx := FBuffer.OffsetToOffsetOfLineEnd(Tags[idx].Range.EndPos);
              N := Tags[idx].Range.StartPos;
              Insert(FBuffer.SubString(N+1, to_idx - N + 1), Result, i); //AT: fixed substring offset/len (2 patches)
            end;
         // HAW: new mode = 3 --- explicit range  idx...to_idx
         3: if  (to_idx >= 0)  and  (to_idx < FTagList.Count)  then  begin
              if  to_idx < idx  then  begin
                swp_idx := idx;  idx := to_idx;  to_idx := swp_idx;
              end;
              rngResult := '';
              while  idx <= to_idx  do  begin
                if  rngmax <= 0   then  begin
                  rngResult := rngResult+' ...';
                  break;
                end;
                if  (rngResult <> '') and (idx > 0) and (Tags[idx-1].Range.EndPos <> Tags[idx].Range.StartPos) then //MZ fix
                  rngResult := rngResult + ' ';
                rngResult := rngResult + TagStr[idx];
                inc( idx );  dec( rngmax );
              end;
              Insert(rngResult, Result, i);
            end;
         // HAW: ... end of token range accumulation mode
      end;

      // HAW: I am currently not sure how to handle the "stylename" property here
      //      ... will be added, when available
    end;
   Exit;
  except
    Result := '';
  end;
end;

function TecClientSyntAnalyzer.GetRangeName(Range: TecTextRange): ecString;
begin
  Result := '';
  if Assigned(Range.Rule) and (Range.Rule.NameFmt <> '') then
     Result := RangeFormat(Range.Rule.NameFmt, Range);
  if Result = '' then
   Result := TagStr[Range.IdentIdx];
end;

function TecClientSyntAnalyzer.GetRangeGroup(Range: TecTextRange): ecString;
begin
  Result := RangeFormat(Range.Rule.GroupFmt, Range);
end;

function TecClientSyntAnalyzer.GetCollapsedText(Range: TecTextRange): ecString;
begin
  Result := RangeFormat(Range.Rule.CollapseFmt, Range);
end;

function TecClientSyntAnalyzer.IsEnabled(Rule: TRuleCollectionItem; OnlyGlobal: Boolean): Boolean;
begin
  Result := inherited IsEnabled(Rule, OnlyGlobal) and
      (HasOpened(Rule, Rule.Block, Rule.StrictParent) xor Rule.NotParent);
end;

procedure TecClientSyntAnalyzer.TextChanged(APos: integer);
begin
  if APos = -1 then
    Clear
  else
    ChangedAtPos(APos);
end;

function TecClientSyntAnalyzer.GetOpened(Index: integer): TecTextRange;
begin
  Result := TecTextRange(FOpenedBlocks[Index]);
end;

function TecClientSyntAnalyzer.GetOpenedCount: integer;
begin
  Result := FOpenedBlocks.Count;
end;

procedure TecClientSyntAnalyzer.SetDisableIdleAppend(const Value: Boolean);
begin
  if FDisableIdleAppend <> Value then
    begin
      FDisableIdleAppend := Value;
      if not IsFinished then
        TimerIdleTick(nil);
    end;
end;

function TecClientSyntAnalyzer.DoStopTimer(AndWait: boolean): boolean;
begin
  FTimerIdleMustStop := True;
  FTimerIdle.Enabled := False;

  if FTimerIdleIsBusy then
  begin
    //dont call App.ProgressMessages, it causes CudaText bug #1927
    if AndWait then Sleep(100) else Sleep(20);
    Result := not FTimerIdleIsBusy;
  end
  else
    Result := True;
end;

function TecClientSyntAnalyzer.DetectTag(Rule: TecTagBlockCondition;
  RefTag: integer): Boolean;
var
  Tag: TecSyntToken;
begin
  Tag := Tags[RefTag];
  Tag.Rule := Rule;
  if Rule.TokenType >= 0 then
    Tag.TokenType := Rule.TokenType;
  Tags[RefTag] := Tag;
  Result := True;
end;

procedure TecClientSyntAnalyzer.CloseAtEnd(StartTagIdx: integer);
const
  cSpecIndentID = 20;
    //special number for "Group index" lexer property, which activates indent-based folding for a rule
  cSpecTokenStart: char = '1';
    //special char - must be first of token's type name (e.g. "1keyword");
    //Also such tokens must contain spaces+tabs at the beginning (use parser regex like "^[\x20\x09]*\w+")
var i, j, IndentSize: integer;
    Range: TecTextRange;
    Token: TecSyntToken;
    S: string;
begin
  for i := FOpenedBlocks.Count - 1 downto 0 do
   begin
    Range := TecTextRange(FOpenedBlocks[i]);
    if Range.Rule.EndOfTextClose and
       ((StartTagIdx = 0) or (Range.StartIdx >= StartTagIdx)) then
     begin
       Range.EndIdx := TagCount - 1;
       if Range.Rule.SyntOwner = Owner then
       if Range.Rule.GroupIndex = cSpecIndentID then
       begin
         IndentSize := IndentOf(TagStr[Range.StartIdx]);
         for j := Range.StartIdx+1 to TagCount-1 do
         begin
           Token := Tags[j];
           if Token.Rule.SyntOwner <> Owner then Continue; // Check that token is not from sublexer
           S := Owner.TokenTypeNames[Token.TokenType];
           if (S <> '') and (S[1] = cSpecTokenStart) and (IndentOf(TagStr[j]) <= IndentSize) then
           begin
             Range.EndIdx := j-1;
             Break
           end;
         end;
       end;
       FOpenedBlocks.Delete(i);
     end;
   end;
end;

{ TecSyntAnalyzer }

constructor TecSyntAnalyzer.Create(AOwner: TComponent);
begin
  inherited;
  FClientList := TList.Create;
  FMasters := TList.Create;
  FSampleText := TStringList.Create;
  FTokenTypeNames := TStringList.Create;
  FTokenTypeNames.Text := SecDefaultTokenTypeNames;
  TStringList(FTokenTypeNames).OnChange := TokenNamesChanged;

  FFormats := TecStylesCollection.Create;
  FFormats.OnChange := FormatsChanged;
  FFormats.SyntOwner := Self;

  FTokenRules := TecTokenRuleCollection.Create;
  FTokenRules.OnChange := TokenRuleChanged;
  FTokenRules.SyntOwner := Self;

  FBlockRules := TecBlockRuleCollection.Create;
  FBlockRules.OnChange := BlocksChanged;
  FBlockRules.SyntOwner := Self;

  FSubAnalyzers := TecSubAnalyzerRules.Create;
  FSubAnalyzers.SyntOwner := Self;
  FSubAnalyzers.OnChange := SubLexRuleChanged;

  FMarkedBlock := FFormats.Add as TecSyntaxFormat;
  FMarkedBlock.BgColor := clHighlight;
  FMarkedBlock.Font.Color := clHighlightText;
  FMarkedBlock.FormatType := ftColor;
  FMarkedBlock.DisplayName := 'Marked block';
  FMarkedBlock.FIsBlock := True;

  FCodeTemplates := TecCodeTemplates.Create(Self);
  FSkipSpaces := True;

  FNotes := TStringList.Create;

  FGrammaParser := TGrammaAnalyzer.Create;
  FGrammaParser.OnChange := GrammaChanged;

  FIdleAppendDelayInit := 50;
  FIdleAppendDelay := 200;
end;

destructor TecSyntAnalyzer.Destroy;
begin
  FBlockRules.OnChange := nil;
  FTokenRules.OnChange := nil;
  FFormats.OnChange := nil;
  FSubAnalyzers.OnChange := nil;
  TStringList(FTokenTypeNames).OnChange:= nil;
  FGrammaParser.OnChange := nil;

  FreeAndNil(FFormats);
  FreeAndNil(FMasters);
  FreeAndNil(FSampleText);
  FreeAndNil(FBlockRules);
  FreeAndNil(FTokenRules);
  FreeAndNil(FCodeTemplates);
  FreeAndNil(FTokenTypeNames);
  FreeAndNil(FSubAnalyzers);
  FreeAndNil(FNotes);
  FreeAndNil(FGrammaParser);
  inherited;
  FreeAndNil(FClientList);
end;

procedure TecSyntAnalyzer.Assign(Source: TPersistent);
var Src: TecSyntAnalyzer;
    i: integer;
begin
  if not (Source is TecSyntAnalyzer) then Exit;
  Src := Source as TecSyntAnalyzer;
//  ClearClientContents;
  FCoping := True;
  try
    FAlwaysSyncBlockAnal := Src.FAlwaysSyncBlockAnal;
    Extentions := Src.Extentions;
    LexerName := Src.LexerName;
    SkipSpaces := Src.SkipSpaces;
    SampleText := Src.SampleText;
    FullRefreshSize := Src.FullRefreshSize;
    Formats := Src.Formats;
    MarkedBlockStyle := Src.MarkedBlockStyle;
    SearchMatchStyle := Src.SearchMatchStyle;
    CurrentLineStyle := Src.CurrentLineStyle;
    DefaultStyleName := Src.DefaultStyleName;
    CollapseStyleName := Src.CollapseStyleName;
    BlockRules := Src.BlockRules;
    TokenRules := Src.TokenRules;
    CodeTemplates := Src.CodeTemplates;
    SubAnalyzers := Src.SubAnalyzers;
//    DefaultStyle := Src.DefaultStyle;
    TokenTypeNames := Src.TokenTypeNames;
    Notes := Src.Notes;
    Internal := Src.Internal;
    Gramma := Src.Gramma;
    RestartFromLineStart := Src.RestartFromLineStart;
    ParseEndOfLine := Src.ParseEndOfLine;
    LineComment := Src.LineComment;
    FIdleAppendDelayInit := Src.FIdleAppendDelayInit;
    FIdleAppendDelay := Src.FIdleAppendDelay;
    for i := 0 to BlockRules.Count - 1 do
     begin
       BlockRules[i].BlockEnd := Src.BlockRules[i].BlockEnd;
       BlockRules[i].BlockName := Src.BlockRules[i].BlockName;
     end;
  finally
    FCoping := False;
    ClearClientContents;
  end;
  UpdateClients;
  for i := 0 to FClientList.Count - 1 do
   TecClientSyntAnalyzer(FClientList[i]).IdleAppend;
end;

procedure TecSyntAnalyzer.HighlightKeywords(Client: TecParserResults;
  const Source: ecString; OnlyGlobal: Boolean);
var i, N, ki, RefIdx: integer;
    Accept: Boolean;
    Tag: TecSyntToken;
begin
  N := Client.TagCount;
  for i := 0 to FBlockRules.Count - 1 do
   with FBlockRules[i] do
    if Enabled and (BlockType = btTagDetect) and
       (Block = nil) and (FGrammaRule = nil) then
      begin
       if OnlyGlobal and not AlwaysEnabled then Continue;
       RefIdx := 0;
       Accept := Check(Source, TecClientSyntAnalyzer(Client), N, RefIdx);
       if Assigned(OnBlockCheck) then
         OnBlockCheck(FBlockRules[i], TecClientSyntAnalyzer(Client), Source, RefIdx, Accept);
       if Accept then
         begin
           if FRefToCondEnd then ki := RefIdx - IdentIndex
             else ki := N - 1 - CheckOffset - IdentIndex;

           Tag := TecClientSyntAnalyzer(Client).Tags[ki];
           Tag.Rule := FBlockRules[i];
           if TokenType >= 0 then
              Tag.TokenType := TokenType;
           TecClientSyntAnalyzer(Client).Tags[ki] := Tag;

           if CancelNextRules then Exit;   // 2.27
         end;
      end;
end;

procedure TecSyntAnalyzer.SelectTokenFormat(Client: TecParserResults;
            const Source: ecString; OnlyGlobal: Boolean; N: integer);
var i, li, ki, strt, RefIdx: integer;
    Range: TecTextRange;
    Accept: Boolean;
    RClient: TecClientSyntAnalyzer;

  function CheckIndex(Idx: integer): Boolean; inline;
  begin
   Result := (Idx >= 0) and (Idx < N);
  end;

begin
  if N = -1 then
    N := Client.TagCount;
  if not (Client is TecClientSyntAnalyzer)  then Exit;
  RClient := TecClientSyntAnalyzer(Client);
  RClient.FStartSepRangeAnal := N + 1;
  try
    for i := 0 to FBlockRules.Count - 1 do
      with FBlockRules[i] do
       if not SeparateBlockAnalysis or (BlockType <> btTagDetect) or
          (Block = nil) or (FGrammaRule = nil) then
       if Client.IsEnabled(FBlockRules[i], OnlyGlobal) then
        begin
          RefIdx := 0;
          if FGrammaRule <> nil then
           begin
             RefIdx := FGrammaParser.TestRule(N - 1, FGrammaRule, Client);
             Accept := RefIdx <> -1;
           end else
             Accept := Check(Source, RClient, N, RefIdx);

          if Assigned(OnBlockCheck) then
            OnBlockCheck(FBlockRules[i], RClient, Source, RefIdx, Accept);

          if Accept then
          begin
           Client.ApplyStates(FBlockRules[i]);
           if FRefToCondEnd then strt := RefIdx
             else strt := N - 1 - CheckOffset;
      //    strt := N - 1 - CheckOffset;
           ki := strt - IdentIndex;
           if CheckIndex(ki) then
            case BlockType of
               btTagDetect: // Tag detection
                 if not RClient.DetectTag(FBlockRules[i], ki) then
                   Continue;
               btRangeStart: // Start of block
                begin
                  if FBlockRules[i].SelfClose then
                    RClient.CloseRange(FBlockRules[i], strt);
                  li := strt - BlockOffset;
                  if CheckIndex(li) then
                   begin
                    Range := TecTextRange.Create(li, RClient.Tags[li].Range.StartPos);
                    Range.IdentIdx := ki;
                    Range.Rule := FBlockRules[i];
                    Range.FCondIndex := N - 1;
                    if NoEndRule then
                     begin
                      Range.EndIdx := N - 1 - CheckOffset;
                      Range.FEndCondIndex := N - 1;
                      Range.StartIdx := RefIdx - BlockOffset;
                     end;
                    RClient.AddRange(Range);
                   end;
                end;
               btRangeEnd:  // End of block
                 if not RClient.CloseRange(FBlockRules[i], strt) then
                   Continue;
               btLineBreak:
                 begin
                   //AT: deleted support for line separators
                 end;
            end;
           if CancelNextRules then Break;
          end;
        end;
  except
    Application.HandleException(Self);
  end;
end;

procedure TecSyntAnalyzer.SetSampleText(const Value: TStrings);
begin
  FSampleText.Assign(Value);
end;

function TecSyntAnalyzer.GetToken(Client: TecParserResults; const Source: ecString;
                              APos: integer; OnlyGlobal: Boolean): TecSyntToken;
var i, N, lp: integer;
    Rule: TecTokenRule;
    PntStart, PntEnd: TPoint;
begin
  PntStart.X := -1;
  PntStart.Y := -1;
  Result := TecSyntToken.Create(nil, -1, -1, PntStart, PntStart);

  if Assigned(FOnParseToken) then
    begin
      N := 0;
      Rule := nil;
      FOnParseToken(Client, Source, APos, N, Rule);
      if Assigned(Rule) then
        Result := TecSyntToken.Create(Rule,
               APos - 1,
               APos + N - 1,
               Client.Buffer.StrToCaret(APos-1),
               Client.Buffer.StrToCaret(APos+N-1)
               );
      Exit;
    end;

  lp := 0;
  for i := 0 to FTokenRules.Count - 1 do
    begin
      Rule := FTokenRules[i];
      if Client.IsEnabled(Rule, OnlyGlobal) then
        with Rule do
          begin
            if (ColumnFrom > 0) or (ColumnTo > 0) then
              begin
               if lp = 0 then
                 lp := Client.FBuffer.OffsetToDistanceFromLineStart(APos - 1)+1;

               if (ColumnFrom > 0) and (lp < ColumnFrom) or
                  (ColumnTo > 0) and (lp > ColumnTo) then
                  Continue;
              end;
            N := Match(Source, APos);
            if Assigned(OnMatchToken) then
              OnMatchToken(Rule, Client, Source, APos, N);
            if N > 0 then
              begin
                Client.ApplyStates(Rule);

                PntStart := Client.Buffer.StrToCaret(APos-1);

                //optimization: if token is short, get PntEnd simpler
                if PntStart.X + N >= Client.Buffer.LineLength(PntStart.Y) then
                  PntEnd := Client.Buffer.StrToCaret(APos+N-1)
                else
                begin
                  PntEnd.Y := PntStart.Y;
                  PntEnd.X := PntStart.X + N;
                end;

                Result := TecSyntToken.Create(Rule,
                       APos - 1,
                       APos + N - 1,
                       PntStart,
                       PntEnd
                       );
                Exit;
              end;
          end;
    end;
end;

procedure TecSyntAnalyzer.FormatsChanged(Sender: TCollection; Item: TSyntCollectionItem);
var i: integer;
begin
  ClearClientContents;
  if Item = nil then
   begin
    if not FFormats.ValidItem(FMarkedBlock) then FMarkedBlock := nil;
    if not FFormats.ValidItem(FCurrentLine) then FCurrentLine := nil;
    if not FFormats.ValidItem(FDefStyle) then FDefStyle := nil;
    if not FFormats.ValidItem(FSearchMatch) then FSearchMatch := nil;
    for i := 0 to FBlockRules.Count - 1 do
     begin
      if not FFormats.ValidItem(FBlockRules[i].Style) then FBlockRules[i].Style := nil;
      if not FFormats.ValidItem(FBlockRules[i].TreeItemStyleObj) then FBlockRules[i].FTreeItemStyleObj := nil;
      if not FFormats.ValidItem(FBlockRules[i].TreeGroupStyleObj) then FBlockRules[i].FTreeGroupStyleObj := nil;
     end;
    for i := 0 to FTokenRules.Count - 1 do
     if not FFormats.ValidItem(FTokenRules[i].Style) then FTokenRules[i].Style := nil;
    for i := 0 to FSubAnalyzers.Count - 1 do
     if not FFormats.ValidItem(FSubAnalyzers[i].Style) then FSubAnalyzers[i].Style := nil;
   end;
//  UpdateClients;
  Change;
end;

procedure TecSyntAnalyzer.BlocksChanged(Sender: TCollection;
  Item: TSyntCollectionItem);
var i: integer;
begin
  ClearClientContents;
  if Item = nil then
   begin
    for i := 0 to FBlockRules.Count - 1 do
     begin
      if not FBlockRules.ValidItem(FBlockRules[i].Block) then FBlockRules[i].Block := nil;
      if not FBlockRules.ValidItem(FBlockRules[i].BlockEndCond) then FBlockRules[i].BlockEndCond := nil;
     end;
    for i := 0 to FTokenRules.Count - 1 do
     if not FBlockRules.ValidItem(FTokenRules[i].Block) then FTokenRules[i].Block := nil;
    for i := 0 to FSubAnalyzers.Count - 1 do
     if not FSubAnalyzers.ValidItem(FSubAnalyzers[i].Block) then FSubAnalyzers[i].Block := nil;
   end;
//  UpdateClients;
  Change;
end;

procedure TecSyntAnalyzer.ClearClientContents;
var i:integer;
begin
  if FCoping then Exit;
  FCoping := True;
  try
    for i := 0 to FClientList.Count - 1 do
     with TecClientSyntAnalyzer(FClientList[i]) do
      begin
        Clear;
        IdleAppend;
      end;
    for i := 0 to FMasters.Count - 1 do
      TecSyntAnalyzer(FMasters[i]).ClearClientContents;
  finally
    FCoping := False;
  end;
  UpdateClients;
end;

procedure TecSyntAnalyzer.UpdateClients;
var i:integer;
begin
  if FCoping then Exit;
  FCoping := True;
  try
    for i := 0 to FClientList.Count - 1 do
     with TecClientSyntAnalyzer(FClientList[i]) do
       if FClient <> nil then
         FClient.FormatChanged;
    for i := 0 to FMasters.Count - 1 do
      TecSyntAnalyzer(FMasters[i]).UpdateClients;
  finally
    FCoping := False;
  end;
end;

procedure TecSyntAnalyzer.Loaded;
var i: integer;
begin
  inherited;
  MarkedBlockStyle := FMarkedBlockName;
  SearchMatchStyle := FSearchMatchName;
  CurrentLineStyle := FCurrentLineName;
  CollapseStyleName := FCollapseStyleName;
  DefaultStyleName := FDefStyleName;

  FFormats.Loaded;
  FBlockRules.Loaded;
  FTokenRules.Loaded;
  FSubAnalyzers.Loaded;
  CompileGramma;
  DetectBlockSeparate;
  for i := 0 to FMasters.Count - 1 do
    TecSyntAnalyzer(FMasters[i]).DetectBlockSeparate;
end;

procedure TecSyntAnalyzer.SetBlockRules(const Value: TecBlockRuleCollection);
begin
  FBlockRules.Assign(Value);
  ClearClientContents;
end;

procedure TecSyntAnalyzer.SetCodeTemplates(const Value: TecCodeTemplates);
begin
  FCodeTemplates.Assign(Value);
  ClearClientContents;
end;

procedure TecSyntAnalyzer.SetTokenRules(const Value: TecTokenRuleCollection);
begin
  FTokenRules.Assign(Value);
  ClearClientContents;
end;

procedure TecSyntAnalyzer.SetFormats(const Value: TecStylesCollection);
begin
  FFormats.Assign(Value);
end;

function TecSyntAnalyzer.GetUniqueName(const Base: string): string;
var n: integer;
begin
  n := 1;
  if Owner = nil then Result := Base + '1' else
  repeat
   Result := Base + IntToStr(n);
   inc(n);
  until Owner.FindComponent(Result) = nil;
end;

procedure TecSyntAnalyzer.SetSkipSpaces(const Value: Boolean);
begin
  if FSkipSpaces <> Value then
   begin
     FSkipSpaces := Value;
     ClearClientContents;
   end;
end;

procedure TecSyntAnalyzer.SetSubAnalyzers(const Value: TecSubAnalyzerRules);
begin
  FSubAnalyzers.Assign(Value);
  ClearClientContents;
end;

procedure TecSyntAnalyzer.Notification(AComponent: TComponent;
  Operation: TOperation);
var i: integer;
begin
  inherited;
  if (Operation = opRemove)  and (AComponent <> Self) and (aComponent is TecSyntAnalyzer) and
     Assigned(FSubAnalyzers) and Assigned(FMasters) then
   begin
     for i := 0 to FSubAnalyzers.Count - 1 do
      if FSubAnalyzers[i].FSyntAnalyzer = AComponent then
       FSubAnalyzers[i].FSyntAnalyzer := nil;
     FMasters.Remove(AComponent);
   end;
end;

procedure TecSyntAnalyzer.SubLexRuleChanged(Sender: TCollection;
  Item: TSyntCollectionItem);
begin
  DetectBlockSeparate;
  ClearClientContents;
  Change;
end;

procedure TecSyntAnalyzer.AddMasterLexer(SyntAnal: TecSyntAnalyzer);
begin
  if Assigned(SyntAnal) and (SyntAnal <> Self) and
     (FMasters.IndexOf(SyntAnal) = -1) then
   begin
     FMasters.Add(SyntAnal);
     SyntAnal.FreeNotification(Self);
   end;
end;

procedure TecSyntAnalyzer.RemoveMasterLexer(SyntAnal: TecSyntAnalyzer);
begin
  FMasters.Remove(SyntAnal);
end;

procedure TecSyntAnalyzer.TokenRuleChanged(Sender: TCollection;
  Item: TSyntCollectionItem);
begin
  DetectBlockSeparate;
  ClearClientContents;
  Change;
end;

procedure TecSyntAnalyzer.SetTokenTypeNames(const Value: TStrings);
begin
  FTokenTypeNames.Assign(Value);
end;

procedure TecSyntAnalyzer.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TecSyntAnalyzer.SetSearchMatch(const Value: TecSyntaxFormat);
begin
  if FSearchMatch = Value then Exit;
  FSearchMatch := Value;
  UpdateClients;
  Change;
end;

procedure TecSyntAnalyzer.SetMarkedBlock(const Value: TecSyntaxFormat);
begin
  if FMarkedBlock = Value then Exit;
  FMarkedBlock := Value;
  UpdateClients;
  Change;
end;

procedure TecSyntAnalyzer.SetCurrentLine(const Value: TecSyntaxFormat);
begin
  if FCurrentLine = Value then Exit;
  FCurrentLine := Value;
  UpdateClients;
  Change;
end;

procedure TecSyntAnalyzer.SetDefStyle(const Value: TecSyntaxFormat);
begin
  if FDefStyle = Value then Exit;
  FDefStyle := Value;
  UpdateClients;
  Change;
end;

function TecSyntAnalyzer.GetStyleName(const AName: string; const AStyle: TecSyntaxFormat): string;
begin
  if csLoading in ComponentState then
    Result := AName
  else
   if Assigned(AStyle) then
    Result := AStyle.DisplayName
   else
    Result := '';
end;

function TecSyntAnalyzer.GetMarkedBlockName: string;
begin
  Result := GetStyleName(FMarkedBlockName, FMarkedBlock);
end;

procedure TecSyntAnalyzer.SetMarkedBlockName(const Value: string);
begin
  if csLoading in ComponentState then
    FMarkedBlockName := Value
  else
    MarkedBlock := TecSyntaxFormat(FFormats.ItemByName(Value));
end;

function TecSyntAnalyzer.GetSearchMatchStyle: string;
begin
  Result := GetStyleName(FSearchMatchName, FSearchMatch);
end;

procedure TecSyntAnalyzer.SetSearchMatchStyle(const Value: string);
begin
  if csLoading in ComponentState then
    FSearchMatchName := Value
  else
    FSearchMatch := TecSyntaxFormat(FFormats.ItemByName(Value));
end;

function TecSyntAnalyzer.GetCurrentLineStyle: string;
begin
  Result := GetStyleName(FCurrentLineName, FCurrentLine);
end;

procedure TecSyntAnalyzer.SetCurrentLineStyle(const Value: string);
begin
  if csLoading in ComponentState then
    FCurrentLineName := Value
  else
    FCurrentLine := TecSyntaxFormat(FFormats.ItemByName(Value));
end;

function TecSyntAnalyzer.GetDefaultStyleName: string;
begin
  Result := GetStyleName(FDefStyleName, FDefStyle);
end;

procedure TecSyntAnalyzer.SetDefaultStyleName(const Value: string);
begin
  if csLoading in ComponentState then
    FDefStyleName := Value
  else
    FDefStyle := TecSyntaxFormat(FFormats.ItemByName(Value));
end;

procedure TecSyntAnalyzer.SetNotes(const Value: TStrings);
begin
  FNotes.Assign(Value);
end;

procedure TecSyntAnalyzer.SetInternal(const Value: boolean);
begin
  FInternal := Value;
end;

procedure TecSyntAnalyzer.SetRestartFromLineStart(const Value: Boolean);
begin
  FRestartFromLineStart := Value;
end;

procedure TecSyntAnalyzer.SetParseEndOfLine(const Value: Boolean);
begin
  if FParseEndOfLine <> Value then
    begin
      FParseEndOfLine := Value;
      ClearClientContents;
    end;
end;

procedure TecSyntAnalyzer.CompileGramma;
var i: integer;
begin
  FGrammaParser.CompileGramma(FTokenTypeNames);
  for i := 0 to FBlockRules.Count - 1 do
    FBlockRules[i].FGrammaRule :=
     FGrammaParser.ParserRuleByName(FBlockRules[i].FGrammaRuleName);
end;

procedure TecSyntAnalyzer.TokenNamesChanged(Sender: TObject);
begin
  CompileGramma;
  Change;
end;

procedure TecSyntAnalyzer.SetGrammar(const Value: TGrammaAnalyzer);
begin
  FGrammaParser.Assign(Value);
  CompileGramma;
end;

procedure TecSyntAnalyzer.GrammaChanged(Sender: TObject);
begin
  CompileGramma;
end;

procedure TecSyntAnalyzer.SetLineComment(const Value: ecString);
begin
  FLineComment := Value;
end;

function TecSyntAnalyzer.GetSeparateBlocks: Boolean;
  function HasStateModif(List: TCollection): Boolean;
  var i: integer;
  begin
    for i := 0 to List.Count - 1 do
      with TRuleCollectionItem(List.Items[i]) do
        if (StatesAdd <> 0) or (StatesRemove <> 0) then
          begin
            Result := True;
            Exit;
          end;
    Result := False;
  end;
var i: integer;
begin
  if FSeparateBlocks = 0 then
    begin
      Result := not FAlwaysSyncBlockAnal and
                not HasStateModif(FBlockRules) and
                not HasStateModif(FSubAnalyzers);
      if Result then
        for i := 0 to TokenRules.Count - 1 do
          if TokenRules[i].Block <> nil then
            begin
              Result := False;
              Break;
            end;
      if Result then
        for i := 0 to SubAnalyzers.Count - 1 do
          if (SubAnalyzers[i].SyntAnalyzer <> nil) and
             not SubAnalyzers[i].SyntAnalyzer.SeparateBlockAnalysis then
            begin
              Result := False;
              Break;
            end;
      if Result then
        FSeparateBlocks := 1
      else
        FSeparateBlocks := 2;
    end
  else
    Result := FSeparateBlocks = 1;
end;

procedure TecSyntAnalyzer.DetectBlockSeparate;
begin
  FSeparateBlocks := 0;
end;

procedure TecSyntAnalyzer.SetAlwaysSyncBlockAnal(const Value: Boolean);
begin
  FAlwaysSyncBlockAnal := Value;
  if FAlwaysSyncBlockAnal and SeparateBlockAnalysis then
   begin
    DetectBlockSeparate;
    ClearClientContents;
   end;
end;

function TecSyntAnalyzer.GetCollapseStyleName: string;
begin
  Result := GetStyleName(FCollapseStyleName, FCollapseStyle);
end;

procedure TecSyntAnalyzer.SetCollapseStyleName(const Value: string);
begin
  if csLoading in ComponentState then
    FCollapseStyleName := Value
  else
    FCollapseStyle := TecSyntaxFormat(FFormats.ItemByName(Value));
end;

procedure TecSyntAnalyzer.SetCollapseStyle(const Value: TecSyntaxFormat);
begin
  if FCollapseStyle <> Value then
    begin
      FCollapseStyle := Value;
      UpdateClients;
      Change;
    end;
end;

{ TecCodeTemplate }

constructor TecCodeTemplate.Create(Collection: TCollection);
begin
  inherited;
  FName:= '';
  FDescription:= '';
  FAdvanced:= false;
  FCode:= TStringList.Create;
end;

destructor TecCodeTemplate.Destroy;
begin
  FreeAndNil(FCode);
  inherited;
end;

function TecCodeTemplate.GetDisplayName: string;
begin
  Result := FName;
end;


{ TecCodeTemplates }

function TecCodeTemplates.Add: TecCodeTemplate;
begin
  Result := TecCodeTemplate(inherited Add);
end;

constructor TecCodeTemplates.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TecCodeTemplate);
end;

function TecCodeTemplates.GetItem(Index: integer): TecCodeTemplate;
begin
  Result := TecCodeTemplate(inherited Items[Index]);
end;

{ TecSyntaxManager }

function TecSyntaxManager.AddAnalyzer: TecSyntAnalyzer;
begin
  Result := TLibSyntAnalyzer.Create(Owner);
  Result.Name := Result.GetUniqueName('SyntAnal');
  Result.SetParentComponent(Self);
  FModified := True;
end;

procedure TecSyntaxManager.Clear;
begin
  while FList.Count > 0 do
  begin
    TObject(FList[0]).Free;
  end;

  Changed;
  FModified := True;
end;

constructor TecSyntaxManager.Create(AOwner: TComponent);
begin
  inherited;
  FList := TList.Create;
  FModified := False;
end;

destructor TecSyntaxManager.Destroy;
begin
  FOnChange := nil;
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TecSyntaxManager.Changed;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TecSyntaxManager.GeItem(Index: integer): TecSyntAnalyzer;
begin
  Result := TecSyntAnalyzer(FList[Index]);
end;

procedure TecSyntaxManager.GetChildren(Proc: TGetChildProc;
  Root: TComponent);
var i: integer;
begin
  if not (csDestroying in ComponentState) then
   for i := 0 to FList.Count - 1 do
    Proc(TComponent(FList[i]));
end;

function TecSyntaxManager.GetCount: integer;
begin
  Result := FList.Count;
end;

procedure TecSyntaxManager.LoadFromFile(const FileName: string);
begin
  Clear;
  inherited;
  Changed;
  FModified := False;
end;

procedure TecSyntaxManager.SaveToFile(const FileName: string);
begin
  inherited;
  FModified := False;
end;

procedure TecSyntaxManager.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
  FModified := True;
end;

procedure TecSyntaxManager.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
end;

procedure TecSyntaxManager.SetCurrentLexer(const Value: TecSyntAnalyzer);
begin
  if (FCurrentLexer <> Value) and ((Value = nil) or (FList.IndexOf(value) <> -1)) then
   begin
     FCurrentLexer := Value;
   end;
end;

function TecSyntaxManager.FindAnalyzer(
  const LexerName: string): TecSyntAnalyzer;
var i: integer;
begin
  for i := 0 to GetCount - 1 do
   if SameText(Analyzers[i].LexerName, LexerName) then
     begin
      Result := Analyzers[i];
      Exit;
     end;
  Result := nil;
end;

procedure TecSyntaxManager.OnReadError(Reader: TReader;
  const Message: string; var Handled: Boolean);
var S: string;
begin
  if not FIgnoreAll then
   begin
    if AnalyzerCount > 0 then
      S := 'Error in lexer: '+Analyzers[AnalyzerCount - 1].Name +'. '
    else
      S := '';
    S := S + Message;
    inherited OnReadError(Reader, S, Handled);
   end else
  inherited;
end;

{ TLibSyntAnalyzer }

constructor TLibSyntAnalyzer.Create(AOwner: TComponent);
begin
  if Assigned(AOwner) and (AOwner is TecSyntaxManager) then
   inherited Create((AOwner as TecSyntaxManager).Owner)
  else
   inherited Create(AOwner);
end;

destructor TLibSyntAnalyzer.Destroy;
begin
  if FParent <> nil then
   begin
     FParent.FList.Remove(Self);
     FParent := nil;
   end;
  inherited;
end;

function TLibSyntAnalyzer.GetParentComponent: TComponent;
begin
  Result := FParent;
end;

function TLibSyntAnalyzer.HasParent: Boolean;
begin
  Result := True;
end;

procedure TLibSyntAnalyzer.LoadFromStream(const Stream: TStream);
begin
  inherited LoadFromStream(Stream);
end;

procedure TLibSyntAnalyzer.SetParentComponent(Value: TComponent);
begin
  if FParent = Value then Exit;
  if FSkipNewName and (Value = nil) then Exit;
  if FParent <> nil then FParent.FList.Remove(Self);
  if (Value <> nil) and (Value is TecSyntaxManager) then
   begin
     FParent := TecSyntaxManager(Value);
     FParent.FList.Add(Self);
   end else FParent := nil;
end;

{ TLoadableComponent }

var
  CheckExistingName: Boolean = False;

procedure TLoadableComponent.LoadFromFile(const FileName: string);
var
  Stream: TFileStreamUTF8;
begin
  FFileName := FileName; //AT
  Stream := TFileStreamUTF8.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TLoadableComponent.LoadFromResourceID(Instance: Cardinal;
  ResID: Integer; ResType: string);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.CreateFromID(Instance, ResID,
    PChar(ResType));
  try
    LoadFromStream(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TLoadableComponent.LoadFromResourceName(Instance: Cardinal;
  const ResName: string; ResType: string);
var
  Stream: TResourceStream;
begin
  Stream := TResourceStream.Create(Instance, ResName,
    PChar(ResType));
  try
    LoadFromStream(Stream);
  finally
    FreeAndNil(Stream);
  end;
end;

procedure TLoadableComponent.LoadFromStream(const Stream: TStream);
begin
  FSkipNewName := True;
  CheckExistingName := True;
  try
    FIgnoreAll := False;
    LoadComponentFromStream(Self, Stream, OnReadError);
  finally
    FSkipNewName := False;
    CheckExistingName := False;
    FFileName := FileName;
 end;
end;

function TLoadableComponent.NotStored: Boolean;
begin
  Result := not FSaving;
end;

procedure TLoadableComponent.OnReadError(Reader: TReader;
  const Message: string; var Handled: Boolean);
begin
//  Handled := True;
  Handled := FIgnoreAll;
  if not Handled then
   case MessageDlg(Message + sLineBreak + 'Ignore this error?', mtError, [mbYes, mbNo, mbAll], 0) of
     mrYes: Handled := True;
     mrAll: begin
              Handled := True;
              FIgnoreAll := True;
            end;
   end;
end;

procedure TLoadableComponent.SaveToFile(const FileName: string);
var
  Stream: TStream;
begin
  Stream := TFileStreamUTF8.Create(FileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    FreeAndNil(Stream);
  end;
  FFileName := FileName;
end;

procedure TLoadableComponent.SaveToStream(Stream: TStream);
begin
  FSaving := True;
  try
    SaveComponentToStream(Self, Stream);
  finally
    FSaving := False;
  end;
end;

procedure TLoadableComponent.SetName(const NewName: TComponentName);
var Base: string;
    n:integer;
begin
  if not FSkipNewName then
   if CheckExistingName and (Owner.FindComponent(NewName) <> nil) then
    begin
     Base := ClassName;
     Delete(Base, 1, 1);
     n := 1;
     while Owner.FindComponent(Base + IntToStr(n)) <> nil do
      Inc(n);
     inherited SetName(Base + IntToStr(n));
    end
   else inherited;
end;

{ TecSubAnalyzerRule }

constructor TecSubAnalyzerRule.Create(Collection: TCollection);
begin
  inherited;
  FStartRegExpr := TecRegExpr.Create;
  FEndRegExpr := TecRegExpr.Create;
  SetDefaultModifiers(FStartRegExpr);
  SetDefaultModifiers(FEndRegExpr);
end;

destructor TecSubAnalyzerRule.Destroy;
begin
  FreeAndNil(FStartRegExpr);
  FreeAndNil(FEndRegExpr);
  inherited;
end;

procedure TecSubAnalyzerRule.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TecSubAnalyzerRule then
   with Dest as TecSubAnalyzerRule do
    begin
     StartExpression := Self.StartExpression;
     EndExpression := Self.EndExpression;
     SyntAnalyzer := Self.SyntAnalyzer;
     FromTextBegin := Self.FromTextBegin;
     ToTextEnd := Self.ToTextEnd;
     IncludeBounds := Self.IncludeBounds;
    end;
end;

function TecSubAnalyzerRule.GetEndExpression: string;
begin
  Result := FEndRegExpr.Expression;
end;

function TecSubAnalyzerRule.GetItemBaseName: string;
begin
  Result := 'Sub lexer rule';
end;

function TecSubAnalyzerRule.GetStartExpression: string;
begin
  Result := FStartRegExpr.Expression;
end;

function TecSubAnalyzerRule.MatchStart(const Source: ecString; Pos: integer): integer;
begin
 try
  Result := FStartRegExpr.MatchLength(Source, Pos);
 except
  Result := 0;
 end;
end;

function TecSubAnalyzerRule.MatchEnd(const Source: ecString; Pos: integer): integer;
begin
 try
  Result := FEndRegExpr.MatchLength(Source, Pos);
 except
  Result := 0;
 end;
end;

procedure TecSubAnalyzerRule.SetEndExpression(const Value: string);
begin
  FEndRegExpr.Expression := Value;
  Changed(False);
end;

procedure TecSubAnalyzerRule.SetStartExpression(const Value: string);
begin
  FStartRegExpr.Expression := Value;
  Changed(False);
end;

procedure TecSubAnalyzerRule.SetSyntAnalyzer(const Value: TecSyntAnalyzer);
var own: TecSyntAnalyzer;

  function IsLinked(SAnal: TecSyntAnalyzer): Boolean;
  var i: integer;
  begin
    for i := 0 to Collection.Count - 1 do
     if (Collection.Items[i] <> Self) and ((Collection.Items[i] as TecSubAnalyzerRule).SyntAnalyzer = SAnal) then
      begin
       Result := True;
       Exit;
      end;
    Result := False;
  end;

begin
  if FSyntAnalyzer <> Value then
   begin
     own := (Collection as TSyntCollection).SyntOwner;
     if Assigned(FSyntAnalyzer) and (FSyntAnalyzer <> own) and not IsLinked(FSyntAnalyzer) then
       FSyntAnalyzer.RemoveMasterLexer(own);
     FSyntAnalyzer := Value;
     if Assigned(FSyntAnalyzer) and (FSyntAnalyzer <> own) and not IsLinked(FSyntAnalyzer) then
      FSyntAnalyzer.AddMasterLexer(own);
     Changed(False);
   end;
end;

procedure TecSubAnalyzerRule.SetFromTextBegin(const Value: Boolean);
begin
  FFromTextBegin := Value;
  Changed(False);
end;

procedure TecSubAnalyzerRule.SetToTextEnd(const Value: Boolean);
begin
  FToTextEnd := Value;
  Changed(False);
end;

procedure TecSubAnalyzerRule.SetIncludeBounds(const Value: Boolean);
begin
  FIncludeBounds := Value;
  Changed(False);
end;

{ TecSubAnalyzerRules }

function TecSubAnalyzerRules.Add: TecSubAnalyzerRule;
begin
  Result := TecSubAnalyzerRule(inherited Add);
end;

constructor TecSubAnalyzerRules.Create;
begin
  inherited Create(TecSubAnalyzerRule);
end;

function TecSubAnalyzerRules.GetItem(Index: integer): TecSubAnalyzerRule;
begin
  Result := TecSubAnalyzerRule(inherited Items[Index]);
end;

{ TecSyntStyles }

constructor TecSyntStyles.Create(AOwner: TComponent);
begin
  inherited;
  FStyles := TecStylesCollection.Create;
end;

destructor TecSyntStyles.Destroy;
begin
  FreeAndNil(FStyles);
  inherited;
end;

procedure TecSyntStyles.SetStyles(const Value: TecStylesCollection);
begin
  FStyles.Assign(Value);
end;


initialization
  Classes.RegisterClass(TLibSyntAnalyzer);

end.
