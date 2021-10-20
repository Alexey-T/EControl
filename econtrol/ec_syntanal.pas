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

{$mode delphi}
{.$define ParseProgress}
{.$define ParseTime}

unit ec_SyntAnal;

interface

uses
  Classes, Graphics, Controls, ExtCtrls,
  Contnrs, syncobjs,
  ec_RegExpr,
  ec_StrUtils,
  ec_Lists,
  ec_gramma,
  ec_syntax_item,
  ec_syntax_collection,
  ec_syntax_format,
  ec_syntax_rule,
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

  TecSyntAnalyzer       = class;
  TecParserResults      = class;
  TecClientSyntAnalyzer = class;
  TecTagBlockCondition  = class;
  TecSyntaxManager      = class;
  TecSubAnalyzerRule    = class;
  TecTextRange        = class;

  TOnMatchToken = procedure(Sender: TObject; Client: TecParserResults;
      const Text: ecString; APos: integer; var MatchLen: integer) of object;
  TOnBlockCheck = procedure(Sender: TObject; Client: TecClientSyntAnalyzer;
      const Text: ecString; var RefIdx: integer; var Accept: Boolean) of object;

  TBoundDefEvent = procedure(Sender: TecClientSyntAnalyzer; Range: TecTextRange; var sIdx, eIdx: integer) of object;

  TecParseInThreadResult = (
    eprNormal,
    eprAppTerminated,
    eprBufferInvalidated
    );

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
  public
    property Style: TecSyntaxFormat read FFormat write FFormat;
    property Block: TecTagBlockCondition read FBlock write FBlock;
    property SyntOwner: TecSyntAnalyzer read GetSyntOwner;
    procedure Loaded; override;
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

  { TecSyntToken }

  PecSyntToken = ^TecSyntToken;
  TecSyntToken = record
  private
  public
    Range: TRange;
    TokenType: integer;
    Rule: TRuleCollectionItem;
    constructor Create(ARule: TRuleCollectionItem;
      AStartPos, AEndPos: integer;
      const APointStart, APointEnd: TPoint);
    function GetStr(const Source: ecString; ATrimLeft: boolean=False): ecString; // Alexey
    function Style: TecSyntaxFormat;
    class operator =(const A, B: TecSyntToken): boolean;
  end;

  { TecTextRange }

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
    procedure Assign(R: TecTextRange);
  end;

  { TecSubLexerRange }

  PecSubLexerRange = ^TecSubLexerRange;
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

  { TecSingleTagCondition }

  TecSingleTagCondition = class(TCollectionItem)
  private
    FTagList: TStrings;
    FCondType: TecTagConditionType;
    FTokenTypes: DWORD;
    FRegexes: TFPObjectList;
    procedure SetTagList(const Value: TStrings);
    procedure SetIgnoreCase(const Value: Boolean);
    procedure SetTokenTypes(const Value: DWORD);
    procedure SetCondType(const Value: TecTagConditionType);
    procedure TagListChanged(Sender: TObject);
    function GetIgnoreCase: Boolean;
    procedure UpdateRegexes;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function CheckToken(const Source: ecString; const Token: PecSyntToken): Boolean;
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
    FCriSec: TCriticalSection;
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
    function CheckOffset: integer;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function Check(const Source: ecString; Tags: TecClientSyntAnalyzer;
                   N: integer;  var RefIdx: integer): Boolean;
    property BlockEndCond: TecTagBlockCondition read FBlockEndCond write SetBlockEndCond;
    property TreeItemStyleObj: TecSyntaxFormat read FTreeItemStyleObj;
    property TreeGroupStyleObj: TecSyntaxFormat read FTreeGroupStyleObj;
    procedure Loaded; override;
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
    FCriSec: TCriticalSection;
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
    function GetEndExpression: ecString;
    function GetStartExpression: ecString;
    procedure SetEndExpression(const Value: ecString);
    procedure SetStartExpression(const Value: ecString);
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
    property StartExpression: ecString read GetStartExpression write SetStartExpression;
    property EndExpression: ecString read GetEndExpression write SetEndExpression;
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

  TecOnAddRangeSimple = procedure(AStartIdx, AEndIdx: integer) of object;

  TecPublicData = record
    Tokens: TecTokenList;
    FoldRanges: TSortedList;
    SublexRanges: TecSubLexerRanges;
    TokenIndexer: array of integer;
    LineTo: integer; //index of last parsed line (calculated from Tokens)
    Finished: boolean; //parsing is done until document end
    FinishedPartially: boolean; //parsing is done until (at least) editor's bottom line
  end;

  { TecParserResults }

  TecParserResults = class(TTokenHolder)
  private
    FBuffer: TATStringBuffer;
    FClient: IecSyntClient;
    FOwner: TecSyntAnalyzer;
    FFinished: Boolean;
    FPrevChangeLine: integer;
    FSubLexerBlocks: TecSubLexerRanges;
    FTagList: TecTokenList;
    FCurState: integer;
    FStateChanges: TecStateChanges;
    FLastAnalPos: integer;
    FOnAddRangeSimple: TecOnAddRangeSimple; //Alexey

    function GetLastPos: integer;
    function ExtractTag(var FPos: integer; ADisableFolding: Boolean): Boolean;
    function GetTags(Index: integer): PecSyntToken;
    function GetSubLexerRangeCount: integer;
    function GetSubLexerRange(Index: integer): TecSubLexerRange;
    function BufferInvalidated: Boolean; inline;

    //moved to 'private' by Alexey, not needed in CudaText
    property TagCount: integer read GetTokenCount;
    property Tags[Index: integer]: PecSyntToken read GetTags; default;
    property TagStr[Index: integer]: ecString read GetTokenStr;
    property SubLexerRangeCount: integer read GetSubLexerRangeCount;
    property SubLexerRanges[Index: integer]: TecSubLexerRange read GetSubLexerRange;

  protected
    function GetTokenCount: integer; override;
    function GetTokenStr(Index: integer): ecString; override;
    function GetTokenStrEx(Index: integer; ATags: TecTokenList): ecString;
    function GetTokenType(Index: integer): integer; override;
    function CloseAtEnd(StartTagIdx: integer): Boolean; virtual; abstract;
  protected
    function Finished: Boolean; virtual;
    function IsEnabled(Rule: TRuleCollectionItem; OnlyGlobal: Boolean): Boolean; virtual;
    procedure ApplyStates(Rule: TRuleCollectionItem);
    procedure SaveState;
    procedure RestoreState;
    procedure ClearTokenIndexer; //Alexey
    procedure UpdateTokenIndexer(const Token: TecSyntToken); //Alexey
    procedure FindCommentRangeBeforeToken(const Token: TecSyntToken;
      ATokenIsComment: boolean; out ALineFrom, ALineTo: integer); //Alexey
    procedure ShowTokenIndexer; //Alexey
    procedure ShowCmtIndexer; //Alexey
  public
    BufferVersion: integer;

    //holds index of first token overlapping that i-th line ("overlapping" is for multi-line tokens)
    TokenIndexer: array of integer; //Alexey
    //holds booleans: first token of i-th line is a 'comment'
    CmtIndexer: packed array of boolean; //Alexey

    constructor Create(AOwner: TecSyntAnalyzer; ABuffer: TATStringBuffer; const AClient: IecSyntClient);
    destructor Destroy; override;

    procedure Clear; virtual;
    function AnalyzerAtPos(APos: integer; ABlocks: TecSubLexerRanges): TecSyntAnalyzer;
    function ParserStateAtPos(ATokenIndex: integer): integer;

    property Owner: TecSyntAnalyzer read FOwner;
    property Buffer: TATStringBuffer read FBuffer;
    property IsFinished: Boolean read FFinished;
    function TokenIndent(Token: PecSyntToken): integer; // Alexey
    function TagsSame(Index1, Index2: integer): boolean; // Alexey
    function TagSameAs(Index: integer; const Str: ecString): boolean; // Alexey
    property ParserState: integer read FCurState write FCurState;
    property OnAddRangeSimple: TecOnAddRangeSimple read FOnAddRangeSimple write FOnAddRangeSimple; // Alexey
  end;

  { TecParserThread }

  TecParserThread = class(TThread)
  public
    An: TecClientSyntAnalyzer;
    DebugMsg: string;
    DebugTicks: QWord;
    procedure ThreadParseDone;
    procedure ThreadProgressFirst;
    procedure ThreadProgressSecond;
    procedure ThreadProgressBoth;
    procedure Execute; override;
    procedure ShowDebugMsg;
    procedure DummyProc;
  end;

  (* Alexey: the entire logic of thread is:

  //main thread, OnChange
  //---------------------
  FlagStop:=True
  EventParseIdle.WaitFor(inf)
  Buffer_Setup
  Parser_Setup(Buffer)
  EventParseNeeded.SetEvent

  //main thread, renderer
  //---------------------
  CriSec.Enter
  Render(PublicData)
  CriSec.Leave

  //parser thread
  //-------------
  repeat
    EventParseNeeded.WaitFor(inf)
    EventParseIdle.ResetEvent
    FlagStop:=False
    try
      //this 'repeat' block is ParseInThread()
      repeat
        if ExtractTag() then //text ended?
        begin
          Finished
          Break
        end
        else
          Show_some_progress

        if FlagStop then
          Break
      until false

      FlushData(PublicData)
      CriSec.Enter
      CopyDataTo(PublicData)
      CriSec.Leave
    finally
      EventParseIdle.SetEvent
    end
  until false
  *)

  { TecClientSyntAnalyzer }

  TecClientSyntAnalyzer = class(TecParserResults)
  private
    FRanges: TSortedList;
    FOpenedBlocks: TSortedList; // Opened ranges (without end)

    FStartSepRangeAnal: integer;
    FRepeateAnalysis: Boolean;
    FOnParseDone: TNotifyEvent;
    FOnProgressFirst: TNotifyEvent;
    FOnProgressSecond: TNotifyEvent;
    FOnProgressBoth: TNotifyEvent;
    {$ifdef ParseProgress}
    FProgress: integer;
    {$endif}

    function CheckBracketsAreClosed(ATokenIndexFrom, ATokenIndexTo: integer): boolean; //Alexey
    procedure ClearDataOnChange;
    procedure ClearSublexerRangesFromLine(ALine: integer);
    function GetDisabledFolding: boolean; //Alexey
    function GetRangeCount: integer;
    function GetRanges(Index: integer): TecTextRange;
    function GetOpened(Index: integer): TecTextRange;
    function GetOpenedCount: integer;
    procedure ClearPublicData;
    procedure UpdatePublicDataCore;
    procedure UpdatePublicData(AParseFinished: boolean);
    procedure UpdatePublicDataOnTextChange;
  protected
    procedure AddRange(Range: TecTextRange);
    procedure AddRangeSimple(AStartIdx, AEndIdx: integer); //Alexey
    function HasOpened(Rule: TRuleCollectionItem; Parent: TecTagBlockCondition; Strict: Boolean): Boolean;
    function IsEnabled(Rule: TRuleCollectionItem; OnlyGlobal: Boolean): Boolean; override;
    function Finished: Boolean; override;
    function CloseAtEnd(AStartTagIdx: integer): Boolean; override;
  public
    PublicDataNeedTo: integer; //line index on editor bottom, parser checks that it reached this index
    PublicDataNeedTo2: integer; //the same, but for the paired (splitted) editor
    PublicData: TecPublicData;

    FileName: string;
    ParserThread: TecParserThread;
    EventParseNeeded: TEvent;
    EventParseIdle: TEvent;
    CriSecForData: TCriticalSection;

    constructor Create(AOwner: TecSyntAnalyzer; ABuffer: TATStringBuffer);
    destructor Destroy; override;
    procedure Clear; override;
    function PriorTokenAt(Pos: integer): integer;
    function FindTokenAt(Pos: integer): integer;

    property OnParseDone: TNotifyEvent read FOnParseDone write FOnParseDone;
    property OnProgressFirst: TNotifyEvent read FOnProgressFirst write FOnProgressFirst;
    property OnProgressSecond: TNotifyEvent read FOnProgressSecond write FOnProgressSecond;
    property OnProgressBoth: TNotifyEvent read FOnProgressBoth write FOnProgressBoth;

    procedure DoParseDone;
    procedure DoProgressFirst;
    procedure DoProgressSecond;
    procedure DoProgressBoth;

    function RangeFormat(const FmtStr: ecString; Range: TecTextRange): ecString;
    function GetRangeName(Range: TecTextRange; ATags: TecTokenList): ecString;
    function GetRangeGroup(Range: TecTextRange): ecString;
    function GetCollapsedText(Range: TecTextRange): ecString;
    procedure Stop;

    procedure TextChangedOnLine(ALine: integer);
    procedure ParseAll(AResetContent: Boolean);
    procedure ParseToPos(APos: integer);
    function ParseInThread: TecParseInThreadResult;
    procedure DoShowProgress;
    //procedure CompleteAnalysis;

    function CloseRange(Cond: TecTagBlockCondition; RefTag: integer): Boolean;
    function DetectTag(Rule: TecTagBlockCondition; RefTag: integer): Boolean;

    //property OpenCount: integer read GetOpenedCount;
    //property Opened[Index: integer]: TecTextRange read GetOpened;

    property RangeCount: integer read GetRangeCount;
    property Ranges[Index: integer]: TecTextRange read GetRanges;

    procedure CopyRangesFold(L: TSortedList);
  end;

// *******************************************************************
//  Syntax analizer
//            container of syntax rules
// *******************************************************************

  { TLoadableComponent }

  TLoadableComponent = class(TComponent)
  private
    FSkipNewName: Boolean;
    FFileName: string;
    FIgnoreAll: Boolean;
    FSaving: Boolean;
    procedure LoadExtraData(const AFileName: string);
  protected
    procedure OnReadError(Reader: TReader; const Message: string;
                          var Handled: Boolean); virtual;
    function NotStored: Boolean;
  private
    ThemeMappingCount: integer;
    ThemeMappingArray: array[0..40] of record StrFrom, StrTo: string; end;
    SubLexerNames: array[0..12] of string;
  public
    CommentRangeBegin: string;
    CommentRangeEnd: string;
    CommentFullLinesBegin: string;
    CommentFullLinesEnd: string;
    StylesOfComments: string;
    StylesOfStrings: string;
    function SubLexerName(Index: integer): string;
    function ThemeMappingOfStyle(const AName: string): string;
  public
    procedure SaveToFile(const FileName: string); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromFile(const AFileName: string); virtual;
    procedure LoadFromResourceID(Instance: Cardinal; ResID: Integer; ResType: string); virtual;
    procedure LoadFromResourceName(Instance: Cardinal; const ResName: string; ResType: string); virtual;
    procedure LoadFromStream(const Stream: TStream); virtual;
  protected
    procedure SetName(const NewName: TComponentName); override;
    property FileName: string read FFileName;
  end;

  TParseTokenEvent = procedure(Client: TecParserResults; const Text: ecString; Pos: integer;
      var TokenLength: integer; var Rule: TecTokenRule) of object;

  TecParseProgressEvent = procedure(Sender: TObject; AProgress: integer) of object;

  TecSeparateBlocksMode = (sbmUnknown, sbmEnabled, sbmDisabled);

  { TecSyntAnalyzer }

  TecSyntAnalyzer = class(TLoadableComponent)
  private
    FDeleted: Boolean; //Alexey
    FClientList: TFPList;
    FMasters: TFPList;      // Master lexer, i.e. lexers that uses it
    FOnChange: TNotifyEvent;
    FSampleText: TStrings;
    FFormats: TecStylesCollection;
    FTokenRules: TecTokenRuleCollection;
    FBlockRules: TecBlockRuleCollection;
    FBlockRules_Detecters: array of TecTagBlockCondition; //Alexey
    FCodeTemplates: TecCodeTemplates;
    FExtentions: string;
    FLexerName: string;
    FCoping: Boolean;
    FSkipSpaces: Boolean;
    FSubAnalyzers: TecSubAnalyzerRules;
    FTokenTypeNames: TStrings;
    FFullRefreshSize: integer;

    FDummyString: string;
    {
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
    }

    FNotes: TStrings;
    FInternal: boolean;
    FRestartFromLineStart: Boolean;
    FParseEndOfLine: Boolean;
    FGrammaParser: TGrammaAnalyzer;
    FLineComment: ecString;
    FCharset: TFontCharSet;
    FSeparateBlocks: TecSeparateBlocksMode;
    FAlwaysSyncBlockAnal: Boolean;   // Indicates that blocks analysis may after tokens
    FOnGetCollapseRange: TBoundDefEvent;
    FOnCloseTextRange: TBoundDefEvent;
    FIdleAppendDelayInit: Cardinal;
    FIdleAppendDelay: Cardinal;
    FOnParseToken: TParseTokenEvent;

    procedure InitCommentRules;
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
    {
    procedure SetMarkedBlock(const Value: TecSyntaxFormat);
    function GetMarkedBlockName: string;
    procedure SetMarkedBlockName(const Value: string);
    procedure SetSearchMatch(const Value: TecSyntaxFormat);
    function GetSearchMatchStyle: string;
    procedure SetSearchMatchStyle(const Value: string);
    procedure SetCurrentLine(const Value: TecSyntaxFormat);
    function GetCurrentLineStyle: string;
    procedure SetCurrentLineStyle(const Value: string);
    procedure SetDefStyle(const Value: TecSyntaxFormat);
    function GetDefaultStyleName: string;
    procedure SetDefaultStyleName(const Value: string);
    function GetCollapseStyleName: string;
    procedure SetCollapseStyleName(const Value: string);
    procedure SetCollapseStyle(const Value: TecSyntaxFormat);
    }
    procedure SetNotes(const Value: TStrings);
    procedure SetInternal(const Value: boolean);
    procedure SetRestartFromLineStart(const Value: Boolean);
    procedure SetParseEndOfLine(const Value: Boolean);
    procedure TokenNamesChanged(Sender: TObject);
    procedure CompileGramma;
    procedure SetGrammar(const Value: TGrammaAnalyzer);
    procedure GrammaChanged(Sender: TObject);
    procedure SetLineComment(const Value: ecString);
    procedure DetectBlockSeparate;
    procedure SetAlwaysSyncBlockAnal(const Value: Boolean);
    function GetSeparateBlocks: Boolean;
    procedure UpdateSpecialKinds; //Alexey
  protected
    function GetToken(Client: TecParserResults; const Source: ecString;
                       APos: integer; OnlyGlobal: Boolean): TecSyntToken; virtual;
    procedure HighlightKeywords(Client: TecParserResults; const Source: ecString;
                       OnlyGlobal: Boolean); virtual;
    procedure SelectTokenFormat(Client: TecParserResults; const Source: ecString;
                       DisableFolding, OnlyGlobal: Boolean; ATokenIndex: integer = -1); virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Change; dynamic;
    property SeparateBlockAnalysis: Boolean read GetSeparateBlocks;
  public
    CommentRule1: TecTagBlockCondition; //Alexey
    CommentRule2: TecTagBlockCondition; //Alexey

    SpecialKinds: array of boolean; //Alexey: holds True for each TokenKind for indent-based folding
    IndentBasedFolding: boolean; //Alexey

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    //function AddClient(const Client: IecSyntClient; ABuffer: TATStringBuffer): TecClientSyntAnalyzer;
    procedure ClearClientContents;
    procedure UpdateClients;

    procedure AddMasterLexer(SyntAnal: TecSyntAnalyzer);
    procedure RemoveMasterLexer(SyntAnal: TecSyntAnalyzer);

    property Deleted: Boolean read FDeleted; //Alexey: used by TecLexerList and CudaText
    procedure MarkAsDeleted; //Alexey: used by TecLexerList

    {
    property MarkedBlock: TecSyntaxFormat read FMarkedBlock write SetMarkedBlock;
    property SearchMatch: TecSyntaxFormat read FSearchMatch write SetSearchMatch;
    property CurrentLine: TecSyntaxFormat read FCurrentLine write SetCurrentLine;
    property DefStyle: TecSyntaxFormat read FDefStyle write SetDefStyle;
    property CollapseStyle: TecSyntaxFormat read FCollapseStyle write SetCollapseStyle;
    }
  published
    property Formats: TecStylesCollection read FFormats write SetFormats;
    property TokenRules: TecTokenRuleCollection read FTokenRules write SetTokenRules;
    property BlockRules: TecBlockRuleCollection read FBlockRules write SetBlockRules;
    property CodeTemplates: TecCodeTemplates read FCodeTemplates write SetCodeTemplates;
    property SubAnalyzers: TecSubAnalyzerRules read FSubAnalyzers write SetSubAnalyzers;
    property SampleText: TStrings read FSampleText write SetSampleText;

    property TokenTypeNames: TStrings read FTokenTypeNames write SetTokenTypeNames;
    property Gramma: TGrammaAnalyzer read FGrammaParser write SetGrammar;

    {
    property MarkedBlockStyle: string read GetMarkedBlockName write SetMarkedBlockName;
    property SearchMatchStyle: string read GetSearchMatchStyle write SetSearchMatchStyle;
    property CurrentLineStyle: string read GetCurrentLineStyle write SetCurrentLineStyle;
    property DefaultStyleName: string read GetDefaultStyleName write SetDefaultStyleName;
    property CollapseStyleName: string read GetCollapseStyleName write SetCollapseStyleName;
    }
    property MarkedBlockStyle: string read FDummyString write FDummyString;
    property SearchMatchStyle: string read FDummyString write FDummyString;
    property CurrentLineStyle: string read FDummyString write FDummyString;
    property DefaultStyleName: string read FDummyString write FDummyString;
    property CollapseStyleName: string read FDummyString write FDummyString;

    property Extentions: string read FExtentions write FExtentions;
    property LexerName: string read FLexerName write FLexerName;

    property SkipSpaces: Boolean read FSkipSpaces write SetSkipSpaces default True;
    property FullRefreshSize: integer read FFullRefreshSize write FFullRefreshSize default 0;
    property Notes: TStrings read FNotes write SetNotes;
    property Internal: boolean read FInternal write SetInternal default False;
    property RestartFromLineStart: Boolean read FRestartFromLineStart write SetRestartFromLineStart default False;
    property ParseEndOfLine: Boolean read FParseEndOfLine write SetParseEndOfLine default False;
    property LineComment: ecString read FLineComment write SetLineComment;
    property Charset: TFontCharSet read FCharset write FCharset; // Alexey
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
    FList: TFPList;
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
  EControlOptions: record
    OnLexerParseProgress: TecParseProgressEvent;

    MaxLinesWhenParserEnablesFolding: integer;
    //how much chars can take %sz0 format
    MaxLengthForSZFormat: integer;

    //if N>1, N (or more) consecutive 'comment' lines make folding-range
    //(works for single-line and multi-line comments)
    AutoFoldComments: integer;

    //if True, several comments separated with an empty line, make several fold-ranges
    AutoFoldComments_BreakOnEmptyLine: boolean;
  end;

implementation

uses
  SysUtils, Forms, Dialogs,
  Math;

{
const
  SecDefaultTokenTypeNames = 'Unknown' + #10 +
                             'Comment' + #10 +
                             'Id'      + #10 +
                             'Symbol'  + #10 +
                             'String'  + #10 +
                             'Number';
}

function _IndentOfBuffer(S: PWideChar; Len: integer): Integer; inline; // Alexey
var
  i: integer;
begin
  Result:= 0;
  for i:= 0 to Len-1 do
  begin
    case S^ of
      ' ':
        Inc(Result);
      #9:
        Inc(Result, 4);
      else
        Break;
    end;
    Inc(S);
  end;
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

function IsCharSurrogateHigh(ch: WideChar): boolean; inline; // Alexey
begin
  Result := (Ord(ch) >= $D800) and (Ord(ch) <= $DBFF);
end;

{ TecParserThread }

procedure TecParserThread.ThreadParseDone;
begin
  An.DoParseDone;
end;

procedure TecParserThread.ThreadProgressFirst;
begin
  An.DoProgressFirst;
end;

procedure TecParserThread.ThreadProgressSecond;
begin
  An.DoProgressSecond;
end;

procedure TecParserThread.ThreadProgressBoth;
begin
  An.DoProgressBoth;
end;

procedure _LogException(E: Exception);
var
  f: System.Text;
  fn: string;
begin
  {$ifdef windows}
  fn:= ExtractFileDir(Application.ExeName)+'\cudatext.error';
  {$else}
  fn:= GetEnvironmentVariable('HOME')+'/cudatext.error';
  {$endif}

  AssignFile(f, fn);
  {$I-}
  Append(f);
  if IOResult<>0 then
    Rewrite(f);
  Writeln(f, '---');
  Writeln(f, 'Date: '+DateTimeToStr(Now));
  Writeln(f, 'Exception: '+E.ClassName+', message: '+E.Message);
  DumpExceptionBacktrace(f);
  Close(f);
end;

procedure TecParserThread.Execute;
var
  Res: TecParseInThreadResult;
  SavedChangeLine: integer;
{$ifdef ParseTime}
var
  tick: QWord;
{$endif}
begin
  try
    repeat
      if Terminated then Exit;
      if Application.Terminated then Exit;

      // set PublicData.Finished*, even if we didn't actually finished parsing,
      // to avoid ATSynEdit.Invalidate being blocked
      An.PublicData.Finished := True;
      An.PublicData.FinishedPartially := True;

      //constant in WaitFor() affects how fast 'Close all tabs' will run
      if An.EventParseNeeded.WaitFor(100)<>wrSignaled then
        Continue;

      An.EventParseIdle.ResetEvent;
      try
        {$ifdef ParseTime}
        DebugMsg := 'parse-begin';
        DebugTicks := 0;
        Synchronize(ShowDebugMsg);
        tick := GetTickCount64;
        {$else}
        //Synchronize(DummyProc); //otherwise editor is not highlighted
        {$endif}

        //this repeat/until is needed to avoid having broken PublicData, when eprInterrupted occurs
        SavedChangeLine := An.FPrevChangeLine;
        repeat
          Res := An.ParseInThread;
          if Res in [eprNormal, eprAppTerminated] then Break;
          An.FPrevChangeLine := SavedChangeLine;
        until False;

      finally
        if not Terminated and not Application.Terminated then
        begin
          {$ifdef ParseTime}
          DebugMsg := 'parse-done';
          DebugTicks := GetTickCount64-tick;
          Synchronize(ShowDebugMsg);
          {$endif}
          Synchronize(ThreadParseDone);
        end;

        An.EventParseIdle.SetEvent;
      end;
    until False;

  except
    on E: Exception do
      _LogException(E);
  end;
end;

procedure TecParserThread.ShowDebugMsg;
begin
  Application.MainForm.Caption:= Format('%s, file %s, %dms, %d tokens, %d ranges', [
      DebugMsg,
      An.FileName,
      DebugTicks,
      An.PublicData.Tokens.Count,
      An.PublicData.FoldRanges.Count
      ]);
end;

procedure TecParserThread.DummyProc;
begin
  //empty
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

function TecSyntToken.GetStr(const Source: ecString; ATrimLeft: boolean=False): ecString;
// Alexey: added ATrimLeft
var
  St, Len: integer;
begin
  St := Range.StartPos+1;
  Len := Range.EndPos - Range.StartPos;
  if ATrimLeft then
    while (Len>0) and (St>0) and (St<=Length(Source)) and IsSpaceChar(Source[St]) do
    begin
      Inc(St);
      Dec(Len);
    end;
  Result := Copy(Source, St, Len);
end;

class operator TecSyntToken.=(const A, B: TecSyntToken): boolean;
begin
  Result := false;
end;

function TecSyntToken.Style: TecSyntaxFormat;
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

procedure TecTextRange.Assign(R: TecTextRange);
begin
  StartPos := R.StartPos;
  StartIdx := R.StartIdx;
  EndIdx := R.EndIdx;
  IdentIdx := R.IdentIdx;
  Rule := R.Rule;
  Parent := R.Parent;
  Index := R.Index;
  FCondIndex := R.FCondIndex;
  FEndCondIndex := R.FEndCondIndex;
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

function TecSingleTagCondition.CheckToken(const Source: ecString; const Token: PecSyntToken): Boolean;
var SToken: ecString;
    i, N: integer;
    ReObj: TecRegExpr;
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
    if FCondType in [tcMask, tcStrictMask] then
     begin
       UpdateRegexes;
       SToken := Token.GetStr(Source, True); // Alexey

       try
         for i := 0 to FTagList.Count - 1 do
          begin
            ReObj := TecRegExpr(FRegexes[i]);
            if FCondType = tcMask then
              Result := ReObj.MatchLength(SToken, 1) > 0
            else
              begin
                N := 1;
                Result := ReObj.Match(SToken, N);
                if Result then
                  Result := N > Length(SToken);
              end;

            if Result then break;
          end;
       except
       end;
     end else
     begin
       SToken := Token.GetStr(Source, True);
       Result := (FTagList as TStringList).Find(SToken, N);
       if FCondType = tcNotEqual then
         Result := not Result;
     end;
   end else Result := FCondType <> tcNotEqual;
end;

constructor TecSingleTagCondition.Create(Collection: TCollection);
begin
  inherited;
  FCondType := tcEqual;
  FTagList := TStringList.Create;
  {$if FPC_FULLVERSION>=30200}
  TStringList(FTagList).UseLocale := false; //Alexey, makes Find() faster by 30%
  {$endif}
  TStringList(FTagList).Sorted := true;
  TStringList(FTagList).Delimiter := ' ';
  TStringList(FTagList).Duplicates := dupIgnore;
  TStringList(FTagList).CaseSensitive := True;
  TStringList(FTagList).OnChange := TagListChanged;
  TStringList(FTagList).QuoteChar := ' ';
end;

destructor TecSingleTagCondition.Destroy;
begin
  if Assigned(FRegexes) then
    FreeAndNil(FRegexes);
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

procedure TecSingleTagCondition.UpdateRegexes;
var
  ReObj: TecRegExpr;
  i: integer;
begin
  if FRegexes = nil then
  begin
    FRegexes := TFPObjectList.Create(True);
    for i:= 0 to FTagList.Count - 1 do
    begin
      ReObj := TecRegExpr.Create;
      ReObj.Expression := FTagList[i];
      SetDefaultModifiers(ReObj);
      ReObj.Compile;
      FRegexes.Add(ReObj);
    end;
  end;
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
  FCriSec := TCriticalSection.Create;
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
  FCriSec.Enter;
  try
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
    //if FRefToCondEnd then
    RefIdx := N - ConditionList.Count - offs - skipped + skip_cond;
    //else
    //  RefIdx := N - 1 - offs;
  finally
    FCriSec.Leave;
  end;
end;

destructor TecTagBlockCondition.Destroy;
begin
  FreeAndNil(FCriSec);
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
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
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
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
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
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
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
      (TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer).Gramma.ParserRuleByName(Value);
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
  FCriSec := TCriticalSection.Create;
  FRegExpr := TecRegExpr.Create;
  SetDefaultModifiers(FRegExpr);
end;

destructor TecTokenRule.Destroy;
begin
  FreeAndNil(FRegExpr);
  FreeAndNil(FCriSec);
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
    FCriSec.Enter; //solve CudaText issue #3352
    try
      Result := FRegExpr.MatchLength(Source, Pos);
    finally
      FCriSec.Leave;
    end;
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
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
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
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
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
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
  if not Assigned(FSynt) then Exit;
  if FStyleName <> '' then
    FFormat := TecSyntaxFormat(FSynt.FFormats.ItemByName(FStyleName));
  if FBlockName <> '' then
    FBlock := TecTagBlockCondition(FSynt.BlockRules.ItemByName(FBlockName));
end;

function TRuleCollectionItem.GetBlockName: string;
var FSynt: TecSyntAnalyzer;
begin
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
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
  FSynt := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
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
  Result := TSyntCollection(Collection).SyntOwner as TecSyntAnalyzer;
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
//TODO: del AUseTimer
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
  FStateChanges := TecStateChanges.Create;
  FPrevChangeLine := -1;
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
  SetLength(TokenIndexer, 0);
  SetLength(CmtIndexer, 0);
  FPrevChangeLine := -1;
end;

function TecParserResults.Finished: Boolean;
begin
  Result := True;
  FFinished := True;
  FPrevChangeLine := -1;

  // Performs Gramma parsing
  //AnalyzeGramma;
  //FTagList.UpdateIndexer; // Alexey
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

function TecParserResults.GetTags(Index: integer): PecSyntToken;
begin
  Result := FTagList.InternalGet(Index);
end;

function TecParserResults.GetTokenStrEx(Index: integer; ATags: TecTokenList): ecString;
var
  Ptr: PecSyntToken;
begin
  if Index >= 0 then
  begin
    Ptr := ATags._GetItemPtr(Index);
    Result := FBuffer.SubString(Ptr.Range.StartPos + 1, Ptr.Range.EndPos - Ptr.Range.StartPos)
  end
  else
    Result := '';
end;

function TecParserResults.GetTokenStr(Index: integer): ecString;
begin
  Result := GetTokenStrEx(Index, FTagList);
end;

function TecParserResults.TokenIndent(Token: PecSyntToken): integer; //Alexey
var
  N: integer;
  ch: WideChar;
begin
  Result := 0;
  N := Token.Range.StartPos+1;
  while N>1 do
  begin
    Dec(N);
    if N > Length(FBuffer.FText) then Exit(0);
    ch := FBuffer.FText[N];
    case ch of
      ' ':
        Inc(Result);
      #9:
        Inc(Result, 4);
      #10,
      #13:
        Break;
      else
        Exit(80);
        {
        if token begins not after spaces, return some big value;
        big value is needed for multiline tokens in Python:
        def f():
           s = '''ddd
           dddd
           dddd'''
        }
    end;
  end;
end;

function TecParserResults.TagsSame(Index1, Index2: integer): boolean; // Alexey
  //
  procedure SkipQuotes(var P: PWideChar; var Len: integer); inline;
  var
    ch: WideChar;
  begin
    ch := P^;
    if (ch='"') or (ch='''') then
      if Len>2 then
      begin
        if ch=P[Len-1] then
        begin
          Inc(P);
          Dec(Len, 2);
        end;
      end;
  end;
  //
var
  T1, T2: PecSyntToken;
  Len1, Len2: integer;
  St1, St2: integer;
  Ptr1, Ptr2: PWideChar;
begin
  T1 := Tags[Index1];
  T2 := Tags[Index2];
  St1 := T1.Range.StartPos;
  St2 := T2.Range.StartPos;
  Len1 := T1.Range.EndPos - St1;
  Len2 := T2.Range.EndPos - St2;

  Ptr1:= @FBuffer.FText[St1+1];
  Ptr2:= @FBuffer.FText[St2+1];

  // allow to compare "Id" with Id, and 'Id' with Id
  // ie skip quotes; needed for Bash lexer HereDoc
  SkipQuotes(Ptr1, Len1);
  SkipQuotes(Ptr2, Len2);

  if Len1 <> Len2 then
    Exit(false);

  // case-insensitive, like in original EControl compare
  // (used for HTML/XML lexer mostly)
  Result := strlicomp(Ptr1, Ptr2, Len1) = 0;
end;

function TecParserResults.TagSameAs(Index: integer; const Str: ecString): boolean; // Alexey
var
  T: PecSyntToken;
  Len: integer;
  St: integer;
begin
  T := Tags[Index];
  St := T.Range.StartPos;
  Len := T.Range.EndPos - St;
  if Len <> Length(Str) then
    Exit(false);
  // case-sensitive
  Result := strlcomp(
    PWideChar(Str),
    PWideChar(@FBuffer.FText[St+1]),
    Len) = 0;
end;

function TecParserResults.GetLastPos: integer;
begin
  if FTagList.Count = 0 then
    Result := 1
  else
    Result := FTagList.Last.Range.EndPos + 1;
  if FLastAnalPos > Result then
    Result := FLastAnalPos;
end;

procedure TecParserResults.ClearTokenIndexer;
var
  NCnt, NLastLine, i: integer;
  Token: PecSyntToken;
begin
  NCnt := FTagList.Count;
  if NCnt = 0 then
    NLastLine := -1
  else
  begin
    Token := FTagList.InternalGet(NCnt-1);
    NLastLine := Token.Range.PointEnd.Y;
  end;

  for i := NLastLine + 1 to High(TokenIndexer) do
  begin
    TokenIndexer[i] := -1;
    CmtIndexer[i] := false;
  end;
end;

procedure TecParserResults.UpdateTokenIndexer(const Token: TecSyntToken);
var
  NNewLen, NPrevLen, NTokenIndex, NLine, NLine2: integer;
  NCmtFrom, NCmtTo: integer;
  Style: TecSyntaxFormat;
  bComment: boolean;
  i: integer;
begin
  NNewLen := FBuffer.Count;
  NPrevLen := Length(TokenIndexer);
  if NPrevLen <> NNewLen then
  begin
    SetLength(TokenIndexer, NNewLen);
    SetLength(CmtIndexer, NNewLen);
    for i := NPrevLen to NNewLen - 1 do
    begin
      TokenIndexer[i] := -1;
      CmtIndexer[i] := false;
    end;
  end;

  NLine := Token.Range.PointStart.Y;
  NLine2 := Token.Range.PointEnd.Y;
  if NLine >= NNewLen then Exit;

  NTokenIndex := FTagList.Count-1;
  if (TokenIndexer[NLine] < 0) or (NTokenIndex < TokenIndexer[NLine]) then
  begin
    Style := Token.Style;
    bComment := Assigned(Style) and (Style.TokenKind = etkComment);

    TokenIndexer[NLine] := NTokenIndex;
    CmtIndexer[NLine] := bComment;

    if EControlOptions.AutoFoldComments>1 then
    begin
      FindCommentRangeBeforeToken(Token, bComment, NCmtFrom, NCmtTo);
      if NCmtFrom >= 0 then
        OnAddRangeSimple(TokenIndexer[NCmtFrom], TokenIndexer[NCmtTo]);
    end;
  end;

  //handle multi-line tokens
  for i := NLine + 1 to NLine2 do
  begin
    TokenIndexer[i] := NTokenIndex;
    CmtIndexer[i] := bComment;
  end;
end;

procedure TecParserResults.FindCommentRangeBeforeToken(const Token: TecSyntToken;
  ATokenIsComment: boolean; out ALineFrom, ALineTo: integer);
  //
  function IsBadLine(N: integer): boolean; inline;
  begin
    Result := (TokenIndexer[N]>=0) and (not CmtIndexer[N]);
  end;
  //
var
  NLineFrom, NLineOld: integer;
  NTokenIndex1, NTokenIndex2: integer;
begin
  ALineFrom := -1; //-1 means that we found nothing
  ALineTo := Token.Range.PointStart.Y; //it's always set

  if EControlOptions.AutoFoldComments_BreakOnEmptyLine or
     not ATokenIsComment then
    Dec(ALineTo);

  //skip empty lines
  while (ALineTo>0) and (TokenIndexer[ALineTo]<0) do
    Dec(ALineTo);

  if ALineTo < EControlOptions.AutoFoldComments-1 then exit;

  if IsBadLine(ALineTo) then exit;

  NLineFrom := ALineTo+1;
  NLineOld := NLineFrom+1;

  repeat
    Dec(NLineFrom);
    Dec(NLineOld);

    //found empty line (without tokens)
    if TokenIndexer[NLineFrom]<0 then
    begin
      if EControlOptions.AutoFoldComments_BreakOnEmptyLine then
      begin
        Inc(NLineFrom);
        Break;
      end
      else
        Continue;
    end;

    if IsBadLine(NLineFrom) then
    begin
      Inc(NLineFrom);
      Break;
    end;

    if NLineFrom=0 then Break;

    NTokenIndex1 := TokenIndexer[NLineFrom];
    NTokenIndex2 := TokenIndexer[NLineOld];
    //allow max 1 token per line! 0 is for multi-line comments
    if (NTokenIndex1>=0) and (NTokenIndex2>=0) then
      if NTokenIndex2-NTokenIndex1 > 1 then Break;
  until false;

  //move down to 1st non-empty
  while (TokenIndexer[NLineFrom]<0) and (NLineFrom<ALineTo) do
    Inc(NLineFrom);

  if ALineTo-NLineFrom+1 >= EControlOptions.AutoFoldComments then
  begin
    ALineFrom := NLineFrom;
    //ShowMessage(Format('rng %d..%d', [ALineFrom+1, ALineTo+1]));
  end;
end;

procedure TecParserResults.ShowTokenIndexer;
var
  S: string;
  i: integer;
begin
  S := '';
  for i := 0 to High(TokenIndexer) do
    S += IntToStr(i) + ':' + IntToStr(TokenIndexer[i])+' ';
  Application.MainForm.Caption := S;
end;

procedure TecParserResults.ShowCmtIndexer;
const
  strs: array[boolean] of char = ('0', '1');
var
  S: string;
  i: integer;
begin
  S := '';
  for i := 0 to High(CmtIndexer) do
    S += IntToStr(i) + ':' + strs[CmtIndexer[i]]+' ';
  Application.MainForm.Caption := S;
end;

procedure TecParserResults.SaveState;
var b: Boolean;
    Item: TecStateChange;
begin
 if FStateChanges.Count = 0 then
   b := FCurState <> 0
 else
   b := FCurState <> FStateChanges.Last.State;
 if b then
 begin
   Item.TokenCount := FTagList.Count;
   Item.State := FCurState;
   FStateChanges.Add(Item);
 end;
end;

// True if end of the text
function TecParserResults.ExtractTag(var FPos: integer; ADisableFolding: Boolean): Boolean;
var
  Source: ecString;
  CurToken: TecSyntToken;
  own: TecSyntAnalyzer;

   // Select current lexer
   procedure GetOwner;
   var i, N: integer;
       Sub: PecSubLexerRange;
   begin
    own := FOwner;
    for i := FSubLexerBlocks.Count - 1 downto 0 do
     begin
       Sub:= FSubLexerBlocks.InternalGet(i);
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
               //FSubLexerBlocks[i] := Sub; // Write back to list //Alexey: not needed with pointer
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
       Sub: PecSubLexerRange;
   begin
    for i := FSubLexerBlocks.Count - 1 downto 0 do
    begin
      Sub := FSubLexerBlocks.InternalGet(i);
      if (CurToken.Range.EndPos > Sub.Range.StartPos) and (CurToken.Range.StartPos < Sub.Range.StartPos) then
       begin
        CurToken.Range.EndPos := Sub.Range.StartPos;
        CurToken.Range.PointEnd := FBuffer.StrToCaret(CurToken.Range.EndPos);
        Exit;
       end;
    end;
   end;

   function CanOpen(Rule: TecSubAnalyzerRule): Boolean;
   var N: integer;
       Sub: TecSubLexerRange;
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
       Rule: TecSubAnalyzerRule;
   begin
     for i := 0 to own.SubAnalyzers.Count - 1 do
      if CanOpen(own.SubAnalyzers[i]) then Exit;
     if own <> FOwner then
      for i := 0 to FOwner.SubAnalyzers.Count - 1 do
      begin
       Rule := FOwner.SubAnalyzers[i];
       if Rule.AlwaysEnabled and CanOpen(Rule) then Exit;
      end;
   end;

var
  N, NNextPos: integer;
begin
  Source := FBuffer.FText;
  GetOwner;
  TryOpenSubLexer;
  if own.SkipSpaces then
    begin
     if own.ParseEndOfLine then N := SkipSpacesNoLineBreak(Source, FPos)
      else N := SkipSpacesAndBreaks(Source, FPos);
    end
   else if FPos > Length(Source) then N := -1 else N := 0;
  TryOpenSubLexer;
  GetOwner;

  Result := N = -1;
  if Result then Exit;

  CurToken := FOwner.GetToken(Self, Source, FPos, own <> FOwner);
  if (own <> FOwner) and (CurToken.Range.StartPos < 0) then
    CurToken := own.GetToken(Self, Source, FPos, False);
  if CurToken.Range.StartPos < 0 then  // no token
   begin
     NNextPos := FPos;
     SkipSpacesNoLineBreak(Source, NNextPos); // Alexey: needed for huge space-only lines, where Inc(FPos) is very slow
     if NNextPos > FPos then
       FPos := NNextPos
     else
       Inc(FPos);
   end else
   begin
    CheckIntersect;
    SaveState;

    // Alexey: Pascal lexer finds surrogate pairs and makes too short tokens (1 wordchar) from them
    if (CurToken.Range.EndPos-CurToken.Range.StartPos = 1) and IsCharSurrogateHigh(Source[FPos]) then
    begin
      CurToken.Range.EndPos += 1;
      CurToken.Range.PointEnd.X += 1;
    end;

    FTagList.Add(CurToken);
    UpdateTokenIndexer(CurToken);

    if not FOwner.SeparateBlockAnalysis then
     begin
      FOwner.SelectTokenFormat(Self, Source, ADisableFolding, own <> FOwner);
      if own <> FOwner then
        own.SelectTokenFormat(Self, Source, ADisableFolding, False);
     end else
//    if not IsIdle then
     begin  // Only for first iteration of analysis
      FOwner.HighlightKeywords(Self, Source, own <> FOwner);
      if own <> FOwner then
        own.HighlightKeywords(Self, Source, False);
     end;
    FPos := CurToken.Range.EndPos + 1;
   end;
   FLastAnalPos := FPos;
end;

function TecParserResults.AnalyzerAtPos(APos: integer; ABlocks: TecSubLexerRanges): TecSyntAnalyzer;
var
  N: integer;
  Rng: TecSubLexerRange;
begin
  Result := FOwner;
  if APos < 0 then Exit;
  N := ABlocks.PriorAt(APos);
  if N < 0 then Exit;
  Rng := ABlocks.Items[N];
  if (Rng.Range.StartPos<=APos) and
     ((Rng.Range.EndPos<0){Rng is not closed} or (APos<Rng.Range.EndPos)) then
    Result := Rng.Rule.SyntAnalyzer;
 {
 for i := 0 to ABlocks.Count - 1 do
  with ABlocks[i] do
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
var i, NCount: integer;
begin
  NCount := 0;
  for i := FStateChanges.Count - 1 downto 0 do
    if FStateChanges[i].TokenCount >= TagCount then
      Inc(NCount)
    else
      Break;

  if NCount > 0 then
    FStateChanges.DeleteRange(FStateChanges.Count-NCount, FStateChanges.Count-1);

  if FStateChanges.Count > 0 then
    FCurState := FStateChanges.Last.State
  else
    FCurState := 0;
end;

function TecParserResults.ParserStateAtPos(ATokenIndex: integer): integer;
var i: integer;
    Item: TecStateChange;
begin
   for i := FStateChanges.Count - 1 downto 0 do
   begin
     Item := FStateChanges[i];
     if Item.TokenCount <= ATokenIndex then
     begin
       Result := Item.State;
       Exit;
     end;
   end;
   Result := 0;
end;

function TecParserResults.BufferInvalidated: Boolean; inline;
begin
  Result := BufferVersion <> Buffer.Version;
end;

{ TecClientSyntAnalyzer }

constructor TecClientSyntAnalyzer.Create(AOwner: TecSyntAnalyzer; ABuffer: TATStringBuffer);
begin
  inherited Create(AOwner, ABuffer, nil);

  FRanges := TSortedList.Create(True);
  FOpenedBlocks := TSortedList.Create(False);

  if EControlOptions.AutoFoldComments > 1 then
    inherited OnAddRangeSimple := AddRangeSimple;

  PublicData.Tokens := TecTokenList.Create;
  PublicData.FoldRanges := TSortedList.Create(True);
  PublicData.SublexRanges := TecSubLexerRanges.Create;

  EventParseNeeded := TEvent.Create(nil, False, False, '');
  EventParseIdle := TEvent.Create(nil, True{ManualReset}, True{Signaled}, '');
  CriSecForData := TCriticalSection.Create;

  ParserThread := TecParserThread.Create(True);
  ParserThread.An := Self;
  ParserThread.Start;
end;

destructor TecClientSyntAnalyzer.Destroy;
begin
  FBuffer.IncreaseVersion;
  ParserThread.Terminate;
  ParserThread.WaitFor;
  FreeAndNil(ParserThread);

  FreeAndNil(EventParseIdle);
  FreeAndNil(EventParseNeeded);
  FreeAndNil(CriSecForData);

  FreeAndNil(PublicData.Tokens);
  FreeAndNil(PublicData.FoldRanges);
  FreeAndNil(PublicData.SublexRanges);

  FreeAndNil(FRanges);
  FreeAndNil(FOpenedBlocks);
  inherited;
end;

procedure TecClientSyntAnalyzer.Stop;
begin
  if not IsFinished then
  begin
    FBuffer.IncreaseVersion;
    Sleep(15);
  end;
  FFinished := True;
  //FPrevChangeLine := -1; //this causes CudaText issue #3410
end;

procedure TecClientSyntAnalyzer.Clear;
begin
  inherited;
  Stop;

  FRepeateAnalysis := False;
  FTagList.Clear;
  FRanges.Clear;
  FOpenedBlocks.Clear;
  SetLength(TokenIndexer, 0);
  SetLength(CmtIndexer, 0);

  FFinished := False;
  FLastAnalPos := 0;
  FStartSepRangeAnal := 0;

  ClearPublicData;
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

procedure TecClientSyntAnalyzer.AddRangeSimple(AStartIdx, AEndIdx: integer); //Alexey
var
  Range: TecTextRange;
  TokenPtr: PecSyntToken;
  NCount, NStartPos: integer;
begin
  //must avoid many ranges starting at the same comment beginning: line 5..9, 5..10, 5..11 etc
  NCount := FRanges.Count;
  if NCount>0 then
  begin
    Range := TecTextRange(FRanges[NCount-1]);
    if Range.StartIdx = AStartIdx then
    begin
      Range.EndIdx := AEndIdx;
      exit;
    end;
  end;

  TokenPtr := FTagList.InternalGet(AStartIdx);
  if TokenPtr=nil then exit;
  NStartPos := TokenPtr^.Range.StartPos;

  Range := TecTextRange.Create(AStartIdx, NStartPos);
  Range.EndIdx := AEndIdx;
  Range.Rule := Owner.CommentRule1;
  AddRange(Range);
end;


function TecClientSyntAnalyzer.CloseRange(Cond: TecTagBlockCondition; RefTag: integer): Boolean;
var j: integer;
    b: boolean;
    Range: TecTextRange;
begin
  for j := FOpenedBlocks.Count - 1 downto 0 do
  begin
   Range := TecTextRange(FOpenedBlocks[j]);
   with Range do
     if Assigned(Rule) then
       begin
         if Cond.BlockType = btRangeStart then
           b := Cond.SelfClose and (Rule = Cond)
         else
           b := (Rule.FBlockEndCond = Cond) or (Rule = Cond.FBlockEndCond);
         if b then
           begin
             if Cond.SameIdent and not TagsSame(RefTag - Cond.IdentIndex, IdentIdx) then Continue;
             EndIdx := RefTag - Cond.BlockOffset;
             if (Rule = Cond) and (EndIdx > 0) then Dec(EndIdx); // for self closing
             FEndCondIndex := RefTag;
             if Assigned(Owner.OnCloseTextRange) then
               Owner.OnCloseTextRange(Self, Range, StartIdx, EndIdx);
             FOpenedBlocks.Delete(j);
             Result := True;
             Exit;
           end;
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

function TecClientSyntAnalyzer.Finished: Boolean;
var i: integer;
  Sub: TecSubLexerRange;
begin
  //ShowTokenIndexer; //debugging only!
  //ShowCmtIndexer;

  Result := True;
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

  if BufferInvalidated then
  begin
    FOpenedBlocks.Clear;
    Exit(False);
  end;

  // Close blocks at the end of text
  if not CloseAtEnd(0) then
    Exit(False);

  FOpenedBlocks.Clear; //Alexey

  FRepeateAnalysis := True;
end;

procedure TecClientSyntAnalyzer.ClearPublicData;
begin
  CriSecForData.Enter;
  try
    PublicData.Finished := False;
    PublicData.FinishedPartially := False;
    PublicData.Tokens.Clear;
    PublicData.FoldRanges.Clear;
    PublicData.SublexRanges.Clear;
    SetLength(PublicData.TokenIndexer, 0);
    PublicData.LineTo := 0;
  finally
    CriSecForData.Leave;
  end;
end;

procedure TecClientSyntAnalyzer.UpdatePublicDataCore;
var
  TagPtr: PecSyntToken;
  NCount, NLastParsedLine: integer;
begin
  NCount := FTagList.Count;
  if NCount=0 then
  begin
    ClearPublicData;
    Exit;
  end;

  CriSecForData.Enter;
  try
    TagPtr := FTagList._GetItemPtr(NCount-1);
    NLastParsedLine := TagPtr^.Range.PointStart.Y;

    PublicData.Tokens.Assign(FTagList);
    CopyRangesFold(PublicData.FoldRanges);
    PublicData.SublexRanges.Assign(FSubLexerBlocks);
    PublicData.TokenIndexer := TokenIndexer;
    PublicData.LineTo := NLastParsedLine;
  finally
    CriSecForData.Leave;
  end;
end;

procedure TecClientSyntAnalyzer.UpdatePublicDataOnTextChange;
begin
  UpdatePublicDataCore;
  PublicData.Finished := False;
  PublicData.FinishedPartially := False;
end;

procedure TecClientSyntAnalyzer.UpdatePublicData(AParseFinished: boolean);
var
  bNeedUpdate, bNeedUpdate2: boolean;
  TagPtr: PecSyntToken;
  NCount, NLastParsedLine: integer;
begin
  ////now we don't check .Finished coz it's already set to True before ParseInThread call
  //if PublicData.Finished then Exit;

  NCount := FTagList.Count;
  if NCount=0 then
  begin
    ClearPublicData;
    PublicData.FinishedPartially := True; //fix unpainted editor, when no tokens are placed
    Exit;
  end;

  bNeedUpdate := True;
  bNeedUpdate2 := PublicDataNeedTo2>0;

  if AParseFinished then
  begin
    PublicData.Finished := True;
    PublicData.FinishedPartially := True;
  end
  else
  begin
    if PublicData.LineTo >= PublicDataNeedTo then
      bNeedUpdate := False;

    if PublicDataNeedTo2 > 0 then
    begin
      if PublicData.LineTo >= PublicDataNeedTo2 then
        bNeedUpdate2 := False;
    end;

    TagPtr := FTagList._GetItemPtr(NCount-1);
    NLastParsedLine := TagPtr^.Range.PointStart.Y;
    if bNeedUpdate then
      bNeedUpdate := (NLastParsedLine >= PublicDataNeedTo);
    if bNeedUpdate2 then
      bNeedUpdate2 := (PublicDataNeedTo2 > 0) and
                      (NLastParsedLine >= PublicDataNeedTo2);
  end;

  if bNeedUpdate or bNeedUpdate2 then
  begin
    UpdatePublicDataCore;
    PublicData.FinishedPartially := True;
  end;

  if Assigned(ParserThread) and not ParserThread.Terminated then
  begin
    if bNeedUpdate and bNeedUpdate2 then
      TThread.Queue(ParserThread, ParserThread.ThreadProgressBoth)
    else
    if bNeedUpdate then
      TThread.Queue(ParserThread, ParserThread.ThreadProgressFirst)
    else
    if bNeedUpdate2 then
      TThread.Queue(ParserThread, ParserThread.ThreadProgressSecond);
  end;
end;

function TecClientSyntAnalyzer.GetDisabledFolding: boolean; //Alexey
begin
  Result := FBuffer.Count > EControlOptions.MaxLinesWhenParserEnablesFolding;
end;

function TecClientSyntAnalyzer.ParseInThread: TecParseInThreadResult; //Alexey
var
  {$ifdef ParseProgress}
  BufLen: integer;
  ProgressPrev: integer;
  NMaxPercents: integer;
  {$endif}
  own: TecSyntAnalyzer;
  bSeparateBlocks: boolean;
  bDisableFolding: boolean;
  NPos, NTemp, NTagCount, iToken: integer;
const
  ProgressMinPos = 2000;
  ProcessMsgStep1 = 1000; //stage1: finding tokens
  ProcessMsgStep2 = 1000; //stage2: finding ranges
begin
  Result := eprNormal;
  BufferVersion := Buffer.Version;
  FFinished := False;
  ClearDataOnChange;

  NPos := 0;
  bSeparateBlocks := FOwner.SeparateBlockAnalysis;

  {$ifdef ParseProgress}
  BufLen := FBuffer.TextLength;
  if bSeparateBlocks then
    NMaxPercents := 50
  else
    NMaxPercents := 100;
  ProgressPrev := 0;
  {$endif}

  bDisableFolding := GetDisabledFolding;
  if bDisableFolding then
  begin
    FRanges.Clear;
    FOpenedBlocks.Clear;
  end;

  repeat
    if FFinished then
      Exit;

    if Application.Terminated then
      Exit(eprAppTerminated);

    if FBuffer = nil then
      Exit(eprBufferInvalidated);

    if BufferInvalidated then
      Exit(eprBufferInvalidated);

    NTemp := GetLastPos;
    if NTemp > NPos then
      NPos := NTemp;

    if ExtractTag(NPos, bDisableFolding) then
    begin
      //all tokens found, now find blocks (if bSeparateBlocks)
      if bSeparateBlocks then
      begin
        {$ifdef ParseProgress}
        ProgressPrev := 50;
        {$endif}
        NTagCount := TagCount;

        for iToken := FStartSepRangeAnal + 1 to NTagCount do
        begin
          own := Tags[iToken - 1].Rule.SyntOwner;
          FOwner.SelectTokenFormat(Self, FBuffer.FText, bDisableFolding, own <> FOwner, iToken);
          if own <> FOwner then
            own.SelectTokenFormat(Self, FBuffer.FText, bDisableFolding, False, iToken);

          if Application.Terminated then
            Exit(eprAppTerminated);

          if BufferInvalidated then
            Exit(eprBufferInvalidated);

          {$ifdef ParseProgress}
          if iToken mod ProcessMsgStep2 = 0 then
          begin
            //progress for 2nd half of parsing, range 50..100
            FProgress := 50 + iToken * 50 div NTagCount;
            if FProgress <> ProgressPrev then
            begin
              ProgressPrev := FProgress;
              DoShowProgress;
            end;
          end;
          {$endif}
        end;
      end;

      if not Finished then
        Exit(eprBufferInvalidated);
      UpdatePublicData(True); //after Finished, coz Finished must close Python ranges
    end
    else
    begin
      //this works when parsing has reached the ed's LineBottom,
      //it updates not-complete PublicData
      UpdatePublicData(False);

      {$ifdef ParseProgress}
      if TagCount mod ProcessMsgStep1 = 0 then
      begin
        //if bSeparateBlocks, it's progress for 1st half of parsing, 0..50
        //otherwise, it's progress for entire parsing, 0..100
        if BufLen > 0 then
          if FPos < ProgressMinPos then
            FProgress := 0
          else
            FProgress := NPos * NMaxPercents div BufLen;
        if FProgress <> ProgressPrev then
        begin
          ProgressPrev := FProgress;
          DoShowProgress;
        end;
      end;
      {$endif}
    end;
  until False;
end;

procedure TecClientSyntAnalyzer.DoShowProgress;
begin
  {$ifdef ParseProgress}
  if Assigned(OnLexerParseProgress) then
    OnLexerParseProgress(Owner, FProgress);
  {$endif}
end;

procedure TecClientSyntAnalyzer.ParseToPos(APos: integer);
var
  FPos: integer;
  bDisableFolding: boolean;
begin
  if FBuffer.TextLength = 0 then Exit;
  if FFinished then Exit;
  FPos := GetLastPos;
  bDisableFolding := GetDisabledFolding;

  while FPos - 1 <= APos + 1 do
   begin
     if ExtractTag(FPos, bDisableFolding) then
     begin
       if not FOwner.SeparateBlockAnalysis then
         Finished;
       Break;
     end;
   end;
end;

procedure TecClientSyntAnalyzer.ClearSublexerRangesFromLine(ALine: integer);
var
  Sub: PecSubLexerRange;
  Pnt: TPoint;
  i: integer;
begin
  for i := FSubLexerBlocks.Count - 1 downto 0 do
  begin
    Sub := PecSubLexerRange(FSubLexerBlocks.InternalGet(i));
    if ALine <= Sub.Range.PointStart.Y then
    begin
      Pnt := Buffer.StrToCaret(Sub.CondStartPos);
      if ALine > Pnt.Y then
         ALine := Pnt.Y;
      FSubLexerBlocks.Delete(i); // remove sublexer block
    end
    else
    begin
      Pnt := Buffer.StrToCaret(Sub.CondEndPos);
      if ALine < Pnt.Y then
      begin
        if ALine > Sub.Range.PointEnd.Y then
          ALine := Sub.Range.PointEnd.Y;
        Sub.Range.EndPos := -1; // open sublexer block
        Sub.CondEndPos := -1;
        //FSubLexerBlocks[i] := Sub; // no need to write back, we use pointer
      end;
    end;
  end;
end;

procedure TecClientSyntAnalyzer.ClearDataOnChange;
var
  NTagCount: integer;
 //
 procedure CleanRangeList(List: TSortedList; IsClosed: Boolean);
 var i: integer;
 begin
   for i := List.Count - 1 downto 0 do
    with TecTextRange(List[i]) do
     if (FCondIndex >= NTagCount) or (StartIdx >= NTagCount) or IsClosed and
        ((FEndCondIndex >= NTagCount) or (EndIdx >= NTagCount)) then
      List.Delete(i);
 end;
 //
var
  //lexer will update ranges, which have ending at changed-pos minus delta (in tokens)
  NDeltaRanges: integer;
  NLine, NTokenIndex, i: integer;
begin
  if FPrevChangeLine < 0 then Exit;
  NLine := FPrevChangeLine;

  if NLine > 0 then
  begin
    NTokenIndex:= FTagList.PriorAtLine(NLine);
    if NTokenIndex <= 0 then
      NLine := 0;
  end;

  if NLine = 0 then
  begin
    //total clear
    FSubLexerBlocks.Clear;
    FTagList.Clear;
    ClearTokenIndexer;
    FOpenedBlocks.Clear;
    FRanges.Clear;
    FLastAnalPos := 0;
    FStartSepRangeAnal := 0;
    RestoreState;
    UpdatePublicDataOnTextChange;
    Exit
  end;

  // delta>0 was added for Python: editing below block end must enlarge previous block to editing pos
  // delta>0 breaks HTML lexer: on editing in any place,
  // text in <p>text text</p> changes styles to "misspelled tag property"
  if Owner.IndentBasedFolding then
    NDeltaRanges := 4
  else
    NDeltaRanges := 0;

  ClearSublexerRangesFromLine(NLine);
  FTagList.ClearFromIndex(NTokenIndex);
  ClearTokenIndexer;

  FLastAnalPos := 0;   // Reset current position
  NTagCount := FTagList.Count;
  FStartSepRangeAnal := NTagCount;

  // Remove text ranges from service containers
  CleanRangeList(FOpenedBlocks, False);

  //Alexey: prevent almost hang when user fastly pastes blocks in big file,
  //which gives e.g. 400..800..3000 opened blocks
  if FOpenedBlocks.Count>50 then
    FOpenedBlocks.Clear;

  // Remove text ranges from main storage
  for i := FRanges.Count - 1 downto 0 do
    with TecTextRange(FRanges[i]) do
      if (FCondIndex >= NTagCount) or (StartIdx >= NTagCount) then
        FRanges.Delete(i)
      else
      if (FEndCondIndex >= NTagCount - NDeltaRanges) or (EndIdx >= NTagCount - NDeltaRanges) then // Alexey: delta
      begin
        EndIdx := -1;
        FEndCondIndex := -1;
        FOpenedBlocks.Add(FRanges[i]);
      end;

  // Restore parser state
  RestoreState;

  UpdatePublicDataOnTextChange;
end;

function TecClientSyntAnalyzer.PriorTokenAt(Pos: integer): integer;
begin
  Result := FTagList.PriorAt(Pos);
end;

function TecClientSyntAnalyzer.FindTokenAt(Pos: integer): integer;
begin
  Result := FTagList.FindAt(Pos);
end;

procedure TecClientSyntAnalyzer.DoParseDone;
begin
  if Assigned(FOnParseDone) then
    FOnParseDone(Self);
end;

procedure TecClientSyntAnalyzer.DoProgressFirst;
begin
  if Assigned(FOnProgressFirst) then
    FOnProgressFirst(Self);
end;

procedure TecClientSyntAnalyzer.DoProgressSecond;
begin
  if Assigned(FOnProgressSecond) then
    FOnProgressSecond(Self);
end;

procedure TecClientSyntAnalyzer.DoProgressBoth;
begin
  if Assigned(FOnProgressBoth) then
    FOnProgressBoth(Self);
end;

function TecClientSyntAnalyzer.GetRangeCount: integer;
begin
  Result := FRanges.Count;
end;

function TecClientSyntAnalyzer.GetRanges(Index: integer): TecTextRange;
begin
  Result := TecTextRange(FRanges[Index]);
end;

procedure TecClientSyntAnalyzer.ParseAll(AResetContent: Boolean);
var OldSep: TecSeparateBlocksMode;
begin
  if IsFinished then Exit;
  if AResetContent then
    begin
      OldSep := FOwner.FSeparateBlocks;
      FOwner.FSeparateBlocks := sbmDisabled;
      Clear;
      ParseToPos(FBuffer.TextLength);
      FOwner.FSeparateBlocks := OldSep;
    end else
    begin
      ParseToPos(FBuffer.TextLength);
    end;
end;

type
  TecParserLineMode = (plmNone, plmFromStart, plmToEnd, plmExplicitRange);

function TecClientSyntAnalyzer.RangeFormat(const FmtStr: ecString;
  Range: TecTextRange): ecString;

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

var
  j, N: integer;

  function RangeNumber( const FmtStrNumber: ecString; var NumValue: integer ): boolean;
  var
    ch: ecChar;
  begin
    N := 0;
    Result := false;
    while (j + N) <= length( FmtStrNumber ) do
    begin
      ch := FmtStrNumber[j + N];
      if IsDigitChar(ch) or (N = 0) and
         ((ch = '+') or (ch = '-'))
         then inc(N) else Break;
    end;
    if  N > 0  then  begin
      NumValue := StrToIntDef( copy( FmtStrNumber, j, N ), 0 );
      inc( j, N );
      Result := true;
    end;
  end;

var
  i, idx, to_idx: integer;
  rng: TecTextRange;
  LineMode: TecParserLineMode;
  rngtoken, rngResult: ecString;
  swp_idx, rngdir, rngoffset, rngmax: integer;
  to_rng: TecTextRange;
begin
  idx := 0;
  Result := FmtStr;
  if Range=nil then Exit;
  //try //Alexey: why try/except here?

   // HAW: obsolete -> to_idx := Length(Result);
   //      the variable "j" is now always pointing to the next character to process.
   //      Only during numeric sub-operand scan, the "N" will keep the found digits
   //      count. After such number, the var "j" is immediately adjusted again.

   for i := Length(Result) - 1 downto 1 do
    if Result[i] = '%' then
    begin
     j := i + 1;

     //rngstyle := '';                  // HAW: keep style name
     if  Result[j] = ':' then  begin  // HAW: begin of embedded style name
       inc( j );
       while  (j <= length( Result ))  and  (Result[j] <> ':') do begin
         { //Alexey: was not used
         if  Result[j] > ' '  then
           rngstyle := rngstyle+Result[j];
           }
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

     while Assigned(rng) and (UpCase(char(Result[j])) = 'P') do
      begin
       rng := rng.Parent;
       if (rng = nil) or (j = Length(Result)) then Continue;
       inc(j);
      end;

     case UpCase(char(Result[j])) of
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

     case UpCase(char(Result[j])) of // <== v2.35
       'L': LineMode := plmFromStart; // from start of line
       'Z': LineMode := plmToEnd; // to end of line
       else LineMode := plmNone;
     end;
     if LineMode <> plmNone then Inc(j);

     // HAW: check for "...s[token]..." instead of numeric index
     if  LineMode = plmNone  then
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
             if TagSameAs(idx, rngtoken) then  break;
             inc( idx );
           end;
         end  else
         if  rngdir < 0 then         // downwards search
           while  idx >= (rng.StartIdx + rng.Rule.BlockOffset) do  begin
             if TagSameAs(idx, rngtoken) then  break;
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
       if IsDigitChar(Result[j+1]) then  begin  // only positive values !
         to_idx := to_rng.EndIdx + to_rng.Rule.BlockEndCond.BlockOffset;
         LineMode := plmExplicitRange;
       end else
       begin

         if  LineMode <> plmNone  then  // not a good combination
           continue;
         // ... otherwise we have a real end-token clause
         inc( j );  // skip over the [

         rngdir := 1;
         if  Result[j] <> '['   then  begin
           // to_rng := Range;  // be sure that we start with the range itself
           while UpCase(char(Result[j])) = 'P' do
            begin
             to_rng := rng.Parent;
             if (to_rng = nil) or (j = Length(Result)) then Continue;
             inc(j);
            end;

           case UpCase(char(Result[j])) of
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
               if TagSameAs(to_idx, rngtoken) then  break;
               inc( to_idx );
             end;
           end  else
           if  rngdir < 0 then         // downwards search
             while  to_idx >= (rng.StartIdx + rng.Rule.BlockOffset) do  begin
               if TagSameAs(to_idx, rngtoken) then  break;
               dec( to_idx );
             end;
           rngdir := 0;  // allow for missing <offset>
         end;
         if  not RangeNumber( Result, rngoffset )  then  begin
           if  rngdir <> 0 then
             Continue;
         end  else
           to_idx := to_idx - rngoffset;

         LineMode := plmExplicitRange;  // enforce new mode as we have an explicit range
       end;

     if  (j < length( Result ))  and
         (Result[j] = '~')         and
         IsDigitChar(Result[j+1]) // only positive values !
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
         plmNone:
            Insert(TagStr[idx], Result, i);
         plmFromStart:
           begin
              N := FBuffer.OffsetToOffsetOfLineStart(Tags[idx].Range.StartPos);
              to_idx := Tags[idx].Range.EndPos;
              Insert(FBuffer.SubString(N, Min(to_idx - N + 1, EControlOptions.MaxLengthForSZFormat)), Result, i);
            end;
         plmToEnd:
           begin
              to_idx := FBuffer.OffsetToOffsetOfLineEnd(Tags[idx].Range.EndPos);
              N := Tags[idx].Range.StartPos;
              Insert(FBuffer.SubString(N+1, Min(to_idx - N + 1, EControlOptions.MaxLengthForSZFormat)), Result, i);
              // Alexey: fixed SubString offset/len (2 patches)
              // Alexey: limited SubString length, for CudaText issue #3796
            end;
         // HAW: new mode = 3 --- explicit range  idx...to_idx
         plmExplicitRange:
           if  (to_idx >= 0)  and  (to_idx < FTagList.Count)  then  begin
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
  //except
  //  Result := '';
  //end;
end;

function TecClientSyntAnalyzer.GetRangeName(Range: TecTextRange; ATags: TecTokenList): ecString;
begin
  Result := '';
  if Assigned(Range.Rule) and (Range.Rule.NameFmt <> '') then
    Result := RangeFormat(Range.Rule.NameFmt, Range);
  if Result = '' then
    Result := GetTokenStrEx(Range.IdentIdx, ATags);
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

procedure TecClientSyntAnalyzer.TextChangedOnLine(ALine: integer);
begin
  if ALine < 0 then
    ALine := 0;

  if FPrevChangeLine < 0 then
    FPrevChangeLine := ALine
  else
    FPrevChangeLine := Min(FPrevChangeLine, ALine);

  if FBuffer.TextLength <= Owner.FullRefreshSize then
    FPrevChangeLine := 0;

  EventParseNeeded.SetEvent;

  //avoid too many repaints, CudaText issue #3461
  PublicData.FinishedPartially := False;
end;

function TecClientSyntAnalyzer.GetOpened(Index: integer): TecTextRange;
begin
  Result := TecTextRange(FOpenedBlocks[Index]);
end;

function TecClientSyntAnalyzer.GetOpenedCount: integer;
begin
  Result := FOpenedBlocks.Count;
end;

function TecClientSyntAnalyzer.DetectTag(Rule: TecTagBlockCondition;
  RefTag: integer): Boolean;
var
  Tag: PecSyntToken;
begin
  Tag := Tags[RefTag];
  Tag.Rule := Rule;
  if Rule.TokenType >= 0 then
    Tag.TokenType := Rule.TokenType;
  //Tags[RefTag] := Tag; //Alexey: no need with pointer
  Result := True;
end;

procedure TecClientSyntAnalyzer.CopyRangesFold(L: TSortedList);
var
  R, RR: TecTextRange;
  i: integer;
begin
  L.Clear;
  for i := 0 to FRanges.Count-1 do
  begin
    R := TecTextRange(FRanges[i]);
    RR := TecTextRange.Create(0, 0);
    RR.Assign(R);
    L.Add(RR);
  end;
end;

procedure TecSyntAnalyzer.InitCommentRules;
begin
  //fixes AV in BlockRules.OnChange, on loading Python file, with thread-parser
  BlockRules.OnChange := nil;

  CommentRule1 := BlockRules.Add;
  CommentRule1.DisplayName := 'auto_cmt_1';
  CommentRule1.Enabled := false;
  CommentRule1.BlockType := btRangeStart;
  CommentRule1.DisplayInTree := false;
  CommentRule1.NoEndRule := false;
  CommentRule1.CollapseFmt:= '// ...';

  CommentRule2 := BlockRules.Add;
  CommentRule2.DisplayName := 'auto_cmt_2';
  CommentRule2.Enabled := false;
  CommentRule2.BlockType := btRangeEnd;
  CommentRule2.DisplayInTree := false;

  CommentRule1.BlockEndCond:= CommentRule2;
end;

procedure TecSyntAnalyzer.UpdateSpecialKinds; //Alexey
const
  cSpecTokenStart = '^';
var
  S: string;
  i: integer;
  b: boolean;
begin
  IndentBasedFolding := False;
  if Length(SpecialKinds) = 0 then
  begin
    SetLength(SpecialKinds, TokenTypeNames.Count);
    for i := 0 to High(SpecialKinds) do
    begin
      S := TokenTypeNames[i];
      b := (S <> '') and (S[1] = cSpecTokenStart);
      SpecialKinds[i] := b;
      if b then
        IndentBasedFolding := True;
    end;
  end;
end;

function TecClientSyntAnalyzer.CheckBracketsAreClosed(
  ATokenIndexFrom, ATokenIndexTo: integer): boolean; // Alexey
// CudaText issue #2773
var
  Token: PecSyntToken;
  iToken: integer;
  LevelRound, LevelSquare, LevelCurly: integer;
  NPosStart, NLen: integer;
  ch: WideChar;
begin
  LevelRound := 0;
  LevelSquare := 0;
  LevelCurly := 0;
  NLen := Length(FBuffer.FText);

  for iToken := ATokenIndexFrom to ATokenIndexTo do
  begin
     if BufferInvalidated then
       Exit(True);

     Token := Tags[iToken];
     // count only tokens of length=1
     NPosStart := Token.Range.StartPos;
     if Token.Range.EndPos <> NPosStart+1 then
       Continue;
     if NPosStart >= NLen then
       Continue;

     ch := FBuffer.FText[NPosStart+1];
     case ch of
       '(':
         Inc(LevelRound);
       ')':
         Dec(LevelRound);
       '[':
         Inc(LevelSquare);
       ']':
         Dec(LevelSquare);
       '{':
         Inc(LevelCurly);
       '}':
         Dec(LevelCurly);
     end;
  end;

  Result := (LevelRound <= 0) and (LevelSquare <= 0) and (LevelCurly <= 0);
end;

function TecClientSyntAnalyzer.CloseAtEnd(AStartTagIdx: integer): Boolean;
var
  NTagCount: integer;
  NIndentSize, NLine, NTokenIndex: integer;
  Range: TecTextRange;
  Token1, Token2: PecSyntToken;
  Style: TecSyntaxFormat;
  i, iLine: integer;
begin
  Result := True;
  NTagCount := TagCount;

  for i := FOpenedBlocks.Count - 1 downto 0 do
  begin
    if BufferInvalidated then
    begin
      FOpenedBlocks.Clear;
      //FRanges.Clear;
      Exit(False);
    end;

    Range := TecTextRange(FOpenedBlocks[i]);
    if Range.Rule.EndOfTextClose and
       ((AStartTagIdx = 0) or (Range.StartIdx >= AStartTagIdx)) then
     begin
       Range.EndIdx := NTagCount - 1;
       if Range.Rule.GroupIndex = 20 then
       if Range.Rule.SyntOwner = Owner then
       begin
         // Alexey: indentation-based ranges
         NTokenIndex := Range.StartIdx;
         if NTokenIndex >= NTagCount then Continue;
         Token1 := Tags[NTokenIndex];
         if Owner.SpecialKinds[Token1.TokenType] then
         begin
           NLine := Token1.Range.PointStart.Y;
           // check that Token1 is first in its line, using TokenIndexer
           if (NLine <= High(TokenIndexer)) and (TokenIndexer[NLine] = NTokenIndex) then
           begin
             NIndentSize := TokenIndent(Token1);
             for iLine := NLine+1 to High(TokenIndexer) do // not FBuffer.Count-1, it can be bigger
             begin
               if BufferInvalidated then
               begin
                 FOpenedBlocks.Clear;
                 //FRanges.Clear;
                 Exit(False);
               end;

               NTokenIndex := TokenIndexer[iLine];
               if (NTokenIndex >= 0) and (NTokenIndex < NTagCount) then
               begin
                 Token2 := Tags[NTokenIndex];
                 if Token2.Rule.SyntOwner <> Owner then // check that Token2 is not from sublexer
                   Continue;
                 if Owner.SpecialKinds[Token2.TokenType] then
                   if TokenIndent(Token2) <= NIndentSize then
                     // also check that all brackets ()[]{} are closed at this pos,
                     // CudaText issue #2773
                     if CheckBracketsAreClosed(Range.StartIdx, NTokenIndex) then
                     begin
                       // close range at prev token
                       Dec(NTokenIndex);
                       // make it nice for Python lexer: skip ending "comment" tokens
                       repeat
                         if NTokenIndex<=0 then Break;
                         Style:= Tags[NTokenIndex].Style;
                         if Style=nil then Break;
                         if Style.TokenKind<>etkComment then Break;
                         Dec(NTokenIndex);
                       until False;
                       Range.EndIdx := NTokenIndex;
                       Break
                     end;
               end;
             end;
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
  FClientList := TFPList.Create;
  FMasters := TFPList.Create;
  FSampleText := TStringList.Create;
  FTokenTypeNames := TStringList.Create;
  //FTokenTypeNames.Text := SecDefaultTokenTypeNames; //Alexey removed
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

  {
  FMarkedBlock := FFormats.Add as TecSyntaxFormat;
  FMarkedBlock.BgColor := clHighlight;
  FMarkedBlock.Font.Color := clHighlightText;
  FMarkedBlock.FormatType := ftColor;
  FMarkedBlock.DisplayName := 'Marked block';
  FMarkedBlock.IsBlock := True;
  }

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
    {
    MarkedBlockStyle := Src.MarkedBlockStyle;
    SearchMatchStyle := Src.SearchMatchStyle;
    CurrentLineStyle := Src.CurrentLineStyle;
    DefaultStyleName := Src.DefaultStyleName;
    CollapseStyleName := Src.CollapseStyleName;
    }
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
end;

procedure TecSyntAnalyzer.HighlightKeywords(Client: TecParserResults;
  const Source: ecString; OnlyGlobal: Boolean);
var iRule, N, ki, RefIdx: integer;
    Accept: Boolean;
    Tag: PecSyntToken;
    Rule: TecTagBlockCondition;
begin
  N := Client.TagCount;
  for iRule := 0 to High(FBlockRules_Detecters) do
  begin
    if Client.BufferInvalidated then Exit;

    Rule := FBlockRules_Detecters[iRule];
    with Rule do
      begin
       if OnlyGlobal and not AlwaysEnabled then Continue;
       RefIdx := 0;
       Accept := Check(Source, TecClientSyntAnalyzer(Client), N, RefIdx);
       if Assigned(OnBlockCheck) then
         OnBlockCheck(Rule, TecClientSyntAnalyzer(Client), Source, RefIdx, Accept);
       if Accept then
         begin
           if FRefToCondEnd then ki := RefIdx - IdentIndex
             else ki := N - 1 - CheckOffset - IdentIndex;

           Tag := TecClientSyntAnalyzer(Client).Tags[ki];
           Tag.Rule := Rule;
           if TokenType >= 0 then
              Tag.TokenType := TokenType;
           //TecClientSyntAnalyzer(Client).Tags[ki] := Tag; //Alexey: no need with pointer

           if CancelNextRules then Exit;   // 2.27
         end;
      end;
  end;
end;

procedure TecSyntAnalyzer.SelectTokenFormat(Client: TecParserResults;
            const Source: ecString;
            DisableFolding, OnlyGlobal: Boolean; ATokenIndex: integer);
var i, li, ki, strt, RefIdx: integer;
    Range: TecTextRange;
    Accept: Boolean;
    RClient: TecClientSyntAnalyzer;
    Rule: TecTagBlockCondition;

  function CheckIndex(Idx: integer): Boolean; inline;
  begin
   Result := (Idx >= 0) and (Idx < ATokenIndex);
  end;

begin
  if ATokenIndex = -1 then
    ATokenIndex := Client.TagCount;
  if not (Client is TecClientSyntAnalyzer)  then Exit;
  RClient := TecClientSyntAnalyzer(Client);
  RClient.FStartSepRangeAnal := ATokenIndex + 1;

    for i := 0 to FBlockRules.Count - 1 do
    begin
      if Client.BufferInvalidated then Exit;

      Rule := FBlockRules[i];
      if DisableFolding then
        if Rule.BlockType in [btRangeStart, btRangeEnd] then Continue;
      with Rule do
       if not SeparateBlockAnalysis or (BlockType <> btTagDetect) or
          (Block = nil) or (FGrammaRule = nil) then
       if Client.IsEnabled(Rule, OnlyGlobal) then
        begin
          RefIdx := 0;
          if FGrammaRule <> nil then
           begin
             RefIdx := FGrammaParser.TestRule(ATokenIndex - 1, FGrammaRule, Client);
             Accept := RefIdx <> -1;
           end else
             Accept := Check(Source, RClient, ATokenIndex, RefIdx);

          if Assigned(OnBlockCheck) then
            OnBlockCheck(Rule, RClient, Source, RefIdx, Accept);

          if Accept then
          begin
           Client.ApplyStates(Rule);
           if FRefToCondEnd then strt := RefIdx
             else strt := ATokenIndex - 1 - CheckOffset;
      //    strt := ATokenIndex - 1 - CheckOffset;
           ki := strt - IdentIndex;
           if CheckIndex(ki) then
            case BlockType of
               btTagDetect: // Tag detection
                 if not RClient.DetectTag(Rule, ki) then
                   Continue;
               btRangeStart: // Start of block
                begin
                  if Rule.SelfClose then
                    RClient.CloseRange(Rule, strt);
                  li := strt - BlockOffset;
                  if CheckIndex(li) then
                   begin
                    Range := TecTextRange.Create(li, RClient.Tags[li].Range.StartPos);
                    Range.IdentIdx := ki;
                    Range.Rule := Rule;
                    Range.FCondIndex := ATokenIndex - 1;
                    if NoEndRule then
                     begin
                      Range.EndIdx := ATokenIndex - 1 - CheckOffset;
                      Range.FEndCondIndex := ATokenIndex - 1;
                      Range.StartIdx := RefIdx - BlockOffset;
                     end;
                    RClient.AddRange(Range);
                   end;
                end;
               btRangeEnd:  // End of block
                 if not RClient.CloseRange(Rule, strt) then
                   Continue;
               btLineBreak:
                 begin
                   // Alexey: deleted support for line separators
                 end;
            end;
           if CancelNextRules then Break;
          end;
        end;
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
    Rule: TecTagBlockCondition;
begin
  ClearClientContents;
  if Item = nil then
   begin
    {
    if not FFormats.ValidItem(FMarkedBlock) then FMarkedBlock := nil;
    if not FFormats.ValidItem(FCurrentLine) then FCurrentLine := nil;
    if not FFormats.ValidItem(FDefStyle) then FDefStyle := nil;
    if not FFormats.ValidItem(FSearchMatch) then FSearchMatch := nil;
    }
    for i := 0 to FBlockRules.Count - 1 do
     begin
      Rule := FBlockRules[i];
      if not FFormats.ValidItem(Rule.Style) then Rule.Style := nil;
      if not FFormats.ValidItem(Rule.TreeItemStyleObj) then Rule.FTreeItemStyleObj := nil;
      if not FFormats.ValidItem(Rule.TreeGroupStyleObj) then Rule.FTreeGroupStyleObj := nil;
     end;
    for i := 0 to FTokenRules.Count - 1 do
      with FTokenRules[i] do
        if not FFormats.ValidItem(Style) then Style := nil;
    for i := 0 to FSubAnalyzers.Count - 1 do
      with FSubAnalyzers[i] do
        if not FFormats.ValidItem(Style) then Style := nil;
   end;
  //UpdateClients;
  Change;
end;

procedure TecSyntAnalyzer.BlocksChanged(Sender: TCollection;
  Item: TSyntCollectionItem);
var i: integer;
    Rule: TecTagBlockCondition;
begin
  ClearClientContents;
  if Item = nil then
   begin
    for i := 0 to FBlockRules.Count - 1 do
     begin
      Rule := FBlockRules[i];
      if not FBlockRules.ValidItem(Rule.Block) then Rule.Block := nil;
      if not FBlockRules.ValidItem(Rule.BlockEndCond) then Rule.BlockEndCond := nil;
     end;
    for i := 0 to FTokenRules.Count - 1 do
     with FTokenRules[i] do
      if not FBlockRules.ValidItem(Block) then Block := nil;
    for i := 0 to FSubAnalyzers.Count - 1 do
     with FSubAnalyzers[i] do
      if not FSubAnalyzers.ValidItem(Block) then Block := nil;
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
        Clear; //it starts parsing again
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
var
  Rule: TecTagBlockCondition;
  i: integer;
begin
  inherited;

  {
  MarkedBlockStyle := FMarkedBlockName;
  SearchMatchStyle := FSearchMatchName;
  CurrentLineStyle := FCurrentLineName;
  CollapseStyleName := FCollapseStyleName;
  DefaultStyleName := FDefStyleName;
  }

  FFormats.Loaded;
  FBlockRules.Loaded;
  FTokenRules.Loaded;
  FSubAnalyzers.Loaded;
  CompileGramma;
  DetectBlockSeparate;
  for i := 0 to FMasters.Count - 1 do
    TecSyntAnalyzer(FMasters[i]).DetectBlockSeparate;

  //Alexey
  SetLength(FBlockRules_Detecters, 0);
  for i := 0 to FBlockRules.Count - 1 do
  begin
    Rule := FBlockRules[i];
    with Rule do
      if Enabled and (BlockType = btTagDetect) and
         (Block = nil) and (FGrammaRule = nil) then
      begin
        SetLength(FBlockRules_Detecters, Length(FBlockRules_Detecters)+1);
        FBlockRules_Detecters[High(FBlockRules_Detecters)] := Rule;
      end;
  end;

  //Alexey
  UpdateSpecialKinds;

  //Alexey
  if EControlOptions.AutoFoldComments > 1 then
    InitCommentRules;
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
      with FSubAnalyzers[i] do
       if FSyntAnalyzer = AComponent then
        FSyntAnalyzer := nil;
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

procedure TecSyntAnalyzer.MarkAsDeleted; //Alexey
begin
  FDeleted := True;
  FInternal := True;
  FLexerName := '-'+FLexerName;
  FExtentions := '';
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

{
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
}

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

{
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
}

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
    Rule: TecTagBlockCondition;
begin
  FGrammaParser.CompileGramma(FTokenTypeNames);
  for i := 0 to FBlockRules.Count - 1 do
  begin
    Rule := FBlockRules[i];
    Rule.FGrammaRule :=
     FGrammaParser.ParserRuleByName(Rule.FGrammaRuleName);
  end;
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
  if FSeparateBlocks = sbmUnknown then
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
        FSeparateBlocks := sbmEnabled
      else
        FSeparateBlocks := sbmDisabled;
    end
  else
    Result := FSeparateBlocks = sbmEnabled;
end;

procedure TecSyntAnalyzer.DetectBlockSeparate;
begin
  FSeparateBlocks := sbmUnknown;
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

{
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
}

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
  FList := TFPList.Create;
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
    An: TecSyntAnalyzer;
begin
  for i := 0 to GetCount - 1 do
  begin
   An := Analyzers[i];
   if SameText(An.LexerName, LexerName) then
     begin
      Result := An;
      Exit;
     end;
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

procedure _GetIniValue(const SItem: string; out SKey, SValue: string); inline;
var
  N: integer;
begin
  N := Pos('=', SItem);
  if N=0 then
  begin
    SKey := '';
    SValue := '';
  end
  else
  begin
    SKey:= Copy(SItem, 1, N-1);
    SValue := Copy(SItem, N+1, MaxInt);
  end;
end;

procedure TLoadableComponent.LoadExtraData(const AFileName: string);
const
  //Utf8Bom = #$EF#$BB#$BF;
  sign1 = #$EF;
  sign2 = #$BB;
  sign3 = #$BF;
var
  F: TextFile;
  SItem, SKey, SValue: string;
  Section: (secNone, secComments, secMap, secRef);
  N: integer;
begin
  {$Push}
  {$IOChecks off}
  AssignFile(F, AFileName);
  Reset(F);
  if IOResult<>0 then exit;
  {$Pop}
  Section:= secNone;
  while not EOF(F) do
    begin
      Readln(F, SItem);
      if SItem='' then Continue;
      if SItem[1]=';' then Continue;

      //FreePascal writes BOM to ini files
      if (Length(SItem)>3) and
         (SItem[1]=sign1) and
         (SItem[2]=sign2) and
         (SItem[3]=sign3) then
        Delete(SItem, 1, 3);

      if SItem='[comments]' then
      begin
        Section := secComments;
        Continue;
      end;
      if SItem='[map]' then
      begin
        Section := secMap;
        Continue;
      end;
      if SItem='[ref]' then
      begin
        Section := secRef;
        Continue;
      end;
      if SItem[1]='[' then
      begin
        Section := secNone;
        Continue;
      end;

      _GetIniValue(SItem, SKey, SValue);
      case Section of
        secComments:
          begin
            if SKey='str1' then CommentRangeBegin := SValue else
            if SKey='str2' then CommentRangeEnd := SValue else
            if SKey='full1' then CommentFullLinesBegin := SValue else
            if SKey='full2' then CommentFullLinesEnd := SValue else
            if SKey='styles_cmt' then StylesOfComments := SValue else
            if SKey='styles_str' then StylesOfStrings := SValue;
          end;
        secMap:
          begin
            if ThemeMappingCount<High(ThemeMappingArray) then
            begin
              Inc(ThemeMappingCount);
              ThemeMappingArray[ThemeMappingCount-1].StrFrom := SKey;
              ThemeMappingArray[ThemeMappingCount-1].StrTo := SValue;
            end;
          end;
        secRef:
          begin
            N := StrToIntDef(SKey, -1);
            if (N>=0) and (N<=High(SubLexerNames)) then
             SubLexerNames[N] := SValue;
          end;
        secNone:
          begin
          end;
      end;
    end;
  CloseFile(F);
end;

procedure TLoadableComponent.LoadFromFile(const AFileName: string);
var
  Stream: TFileStream;
  Fmts: TecStylesCollection;
  Fmt: TecSyntaxFormat;
  i: integer;
begin
  FFileName := AFileName;
  Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
    LoadExtraData(ChangeFileExt(AFileName, '.cuda-lexmap'));
  finally
    FreeAndNil(Stream);
  end;

  {
  ShowMessage(ExtractFileName(AFileName)+#10+
    CommentRangeBegin+' '+CommentRangeEnd+#10+
    StylesOfComments+#10+
    StylesOfStrings
    );
  }

  //Alexey
  if Self is TecSyntAnalyzer then
  begin
    Fmts := TecSyntAnalyzer(Self).Formats;
    for i := 0 to Fmts.Count-1 do
    begin
      Fmt := Fmts[i];
      if Pos(','+Fmt.DisplayName+',', ','+StylesOfComments+',')>0 then
        Fmt.TokenKind := etkComment
      else
      if Pos(','+Fmt.DisplayName+',', ','+StylesOfStrings+',')>0 then
        Fmt.TokenKind := etkString;
    end;
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
  end;
end;

function TLoadableComponent.NotStored: Boolean;
begin
  Result := not FSaving;
end;

function TLoadableComponent.SubLexerName(Index: integer): string;
begin
  if (Index>=0) and (Index<=High(SubLexerNames)) then
    Result := SubLexerNames[Index]
  else
    Result := '';
end;

function TLoadableComponent.ThemeMappingOfStyle(const AName: string): string;
var
  i: integer;
begin
  for i := 0 to ThemeMappingCount-1 do
    if AName=ThemeMappingArray[i].StrFrom then
      exit(ThemeMappingArray[i].StrTo);
  Result := '';
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
  Stream := TFileStream.Create(FileName, fmCreate);
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

function TecSubAnalyzerRule.GetEndExpression: ecString;
begin
  Result := FEndRegExpr.Expression;
end;

function TecSubAnalyzerRule.GetItemBaseName: string;
begin
  Result := 'Sub lexer rule';
end;

function TecSubAnalyzerRule.GetStartExpression: ecString;
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

procedure TecSubAnalyzerRule.SetEndExpression(const Value: ecString);
begin
  FEndRegExpr.Expression := Value;
  Changed(False);
end;

procedure TecSubAnalyzerRule.SetStartExpression(const Value: ecString);
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
     own := (Collection as TSyntCollection).SyntOwner as TecSyntAnalyzer;
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

  FillChar(EControlOptions, SizeOf(EControlOptions), 0);
  with EControlOptions do
  begin
    MaxLinesWhenParserEnablesFolding := 10*1000;
    MaxLengthForSZFormat := 40;
    AutoFoldComments := 5;
    AutoFoldComments_BreakOnEmptyLine := true;
  end;

end.
