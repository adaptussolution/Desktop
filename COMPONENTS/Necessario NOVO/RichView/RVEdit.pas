
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRichViewEdit: document editor.                 }
{       (registered on "RichView" page of               }
{       the Component Palette)                          }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVEdit;

interface
{$I RV_Defs.inc}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, RichEdit,
  {$IFDEF RVUSEIME}
  Imm,
  {$ENDIF}
  {$IFNDEF RVDONOTUSELISTS}
  RVMarker,
  {$ENDIF}
  {$IFDEF RICHVIEWDEF4}
  ImgList,
  {$ENDIF}
  {$IFNDEF RVDONOTUSEDRAGDROP}
  ActiveX, ShlObj, RVDragDrop,
  {$ENDIF}
  RVScroll, RVStyle, RVItem, RichView, DLines, Clipbrd, RVFuncs, RVUni,
  CRVData, CRVFData, RVRVData,
  RVRTFErr;
const
  WM_RVUNDOFROMINPLACE = WM_USER+13;
  WM_RVREDOFROMINPLACE = WM_USER+14;

{$IFNDEF RICHVIEWCBDEF3}
  EM_REDO                             = WM_USER + 84;
  EM_CANREDO                          = WM_USER + 85;
{$ENDIF}

type
  { Options for TCustomRichViewEdit.SearchText }
  TRVESearchOption = (
    rvseoMatchCase,    // match case
    rvseoDown,         // search from top to bottom
    rvseoWholeWord);   // match whole words
  TRVESearchOptions = set of TRVESearchOption;
  { Values for TCustomRichViewEdit.EditorOptions }
  TRVEditorOption = (
    rvoClearTagOnStyleApp,  // ApplyTextStyle and ApplyStyleConversion clear tags
    rvoCtrlJumps,           // When user holds Ctrl key, switching to hypertext mode
    rvoDoNotWantReturns,    // Ignore Enter keys
    rvoDoNotWantShiftReturns, // Ignore Shift+Enter keys
    rvoWantTabs,            // Do not ignore Tab keys
    rvoAutoSwitchLang,      // Process WM_INPUTLANGCHANGE. For non-Unicode text,
                            //   switch to style with the proper charset (new
                            //   styles can be added to the collection!)
    rvoHideReadOnlyCaret,   // Hide caret when ReadOnly is True
    rvoNoImageResize,       // Disallow resizing images
    rvoNoCaretHighlightJumps); // Disallow highlighting hyperlinks at caret position
  TRVEditorOptions = set of TRVEditorOption;
  { Drag&drop formats }
  TRVDragDropFormat = (
    rvddRVF,           // RVF ('RichView Format')
    rvddRTF,           // RTF ('Rich Text Format')
    rvddText,          // ANSI text (CF_TEXT)
    rvddUnicodeText,   // Unicode text (CF_UNICODETEXT)
    rvddBitmap,        // Bitmap (CF_DIB or CF_BITMAP)
    rvddMetafile,      // Metafile (CF_ENHMETAFILE);
    rvddURL,           // 'UniformResourceLocator'
    rvddFiles);         // list of files (CF_HDROP)
  TRVDragDropFormats = set of TRVDragDropFormat;
  { Drag&drop effects }
  TRVDropFileAction = (
    rvdfNone,          // Files were ignored
    rvdfInsert,        // Files were Inserted
    rvdfLink);         // Link to files were created
  { Types of undo/redo operations }
  TRVUndoType = (
    rvutNone,          // n/a
    rvutDelete,        // deleting
    rvutInsert,        // inserting
    rvutPara,          // applying paragraph styles
    rvutMiscTyping,    // editing (such as Backspace)
    rvutInsertPageBreak, // insert page-break
    rvutRemovePageBreak, // remove page-break
    rvutTyping,        // text typing
    rvutTag,           // changing tags
    rvutStyleNo,       // applying text styles
    rvutAddCheckpoint, // adding checkpoints
    rvutRemoveCheckpoint, // removing checkpoints
    rvutModifyCheckpoint, // editing checkpoints
    rvutModifyItem,    // chaning item properties
    rvutList,          // applying/removing/editing paragraph lists
    rvutCustom);       // custom (see TCustomRichViewEdit.BeginUndoCustomGroup)
  { Values for Direction parameter of TCustomRichViewEdit.OnCaretGetOut event }
  TRVGetOutDirection = (
    rvdLeft,           // Left at the beginning of doc
    rvdUp,             // Up at the beginning of doc
    rvdRight,          // Right at the end of doc
    rvdDown,           // Down at the end of doc
    rvdTop,            // Ctrl+Home
    rvdBottom);        // Ctrl+End
const
  { Default value for TCustomRichViewEdit.EditorOptions }
  DEFAULT_RVEDITOROPTIONS = [rvoClearTagOnStyleApp];
type
  TCustomRichViewEdit = class;
  { ------------- Types for events of TCustomRichViewEdit -------------------- }
  { OnStyleConversion & OnParaStyleConversion. Occurs when calling
    ApplyStyleConversion or ApplyParaStyleConversion.
    Parameters:
    [in] StyleNo: current text/para style index;
    [in] UserData: value passed to ApplyStyleConversion/ApplyParaStyleConversion;
    [in] AppliedToText: false if the event is called to change current text style
       (not used in OnParaStyleConversion)
    [out] NewStyleNo: set it to text/para style index                          }
  TRVStyleConversionEvent = procedure (Sender: TCustomRichViewEdit;
    StyleNo, UserData: Integer; AppliedToText: Boolean;
    var NewStyleNo: Integer) of object;
  { OnPaste. Occurs when pasting from the Clipboard.
    Allows to paste in your formats or forbid default pasting.
    Parameters:
    [out] DoDefault: set it to false to prevent default pasting.               }
  TRVPasteEvent = procedure (Sender: TCustomRichViewEdit;
    var DoDefault: Boolean) of object;
  { OnCaretGetOut. Occurs when user presses arrow keys at the beginning/end of
    document in "outside" direction.
    Parameters:
    [in] Direction: see TRVGetOutDirection type                                }
  TRVOnCaretGetOutEvent = procedure (Sender: TCustomRichViewEdit;
    Direction: TRVGetOutDirection) of object;
  { OnChanging. Occurs before editing operations.
    Parameters:
    [out] CanEdit: set to false to disallow the operation.                     }
  TRVChangingEvent = procedure (Sender: TCustomRichViewEdit;
    var CanEdit: Boolean) of object;
 { OnDropFiles. Occurs when dropping files in CF_HDROP format, for example from
   Windows Explorer (rvddFiles must be in AcceptDragDropFormats.
   Parameters:
   [in] Files: a list of file names.
   [out] FileAction: if you inserted files, set to rvdfInserted. If you
     inserted hyperlinks to files, set to rvdfLinked. Ignored if DoDefault is True.
   [out] DoDefault: set to False if you want to allow default processing for
     dropped files.                                                            }
  TRVDropFilesEvent = procedure (Sender: TCustomRichViewEdit;
    Files: TStrings; var FileAction: TRVDropFileAction; var DoDefault: Boolean) of object;
  { ---------------------------------------------------------------------------
    TCustomRichViewEdit: ancestor class for TRichViewEdit and TDBRichViewEdit
    components.
  }
  TCustomRichViewEdit = class(TCustomRichView)
  private
    { Storing properties }
    FModified: Boolean;
    FReadOnly : Boolean;
    FEditorOptions: TRVEditorOptions;
    FAcceptDragDropFormats: TRVDragDropFormats;
    { Events }
    FOnCurParaStyleChanged, FOnCurTextStyleChanged, FOnChange,
    FOnCaretMove: TNotifyEvent;
    FOnChanging: TRVChangingEvent;
    FOnStyleConversion, FOnParaStyleConversion: TRVStyleConversionEvent;
    FOnPaste: TRVPasteEvent;
    {$IFDEF RVONCUT}
    FOnCut: TNotifyEvent;
    {$ENDIF}
    FOnCaretGetOut: TRVOnCaretGetOutEvent;
    FOnDropFiles: TRVDropFilesEvent;
    { Other fields }
    {$IFNDEF RVDONOTUSEDRAGDROP}
    FDropTarget: TRVDropTarget;               // object implemeting IDropTarget
    {$ENDIF}
    procedure CheckItemClass(ItemNo: Integer; RequiredClass: TCustomRVItemInfoClass);

    procedure WMInputLangChange(var Message: TMessage); message WM_INPUTLANGCHANGE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMPaste(var Message: TWMpaste); message WM_PASTE;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSysChar(var Message: TWMSysChar); message WM_SYSCHAR;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMChar(var Message: TWMChar); message WM_CHAR;
    procedure WMUndoFromInplace(var Message: TMessage); message WM_RVUNDOFROMINPLACE;
    procedure WMRedoFromInplace(var Message: TMessage); message WM_RVREDOFROMINPLACE;
    procedure WMClear(var Message: TMessage); message WM_CLEAR;
    procedure WMUndo(var Message: TMessage); message WM_UNDO;
    procedure EMUndo(var Message: TMessage); message EM_UNDO;
    procedure EMRedo(var Message: TMessage); message EM_REDO;
    procedure EMCanUndo(var Message: TMessage); message EM_CANUNDO;
    procedure EMCanRedo(var Message: TMessage); message EM_CANREDO;
    procedure EMCanPaste(var Message: TMessage); message EM_CANPASTE;
    procedure WMCreate(var Message: TMessage); message WM_CREATE;
    procedure WMDestroy(var Message: TMessage); message WM_DESTROY;

    {$IFDEF RVUSEIME}
    procedure WMImeStartComposition(var Message: TMessage); message WM_IME_STARTCOMPOSITION;
    procedure WMImeComposition(var Message: TMessage); message WM_IME_COMPOSITION;
    {$ENDIF}

    procedure SetCurParaStyleNo(const Value: Integer);
    procedure SetCurTextStyleNo(const Value: Integer);
    function  GetCurParaStyleNo: Integer;
    function  GetCurTextStyleNo: Integer;
    function GetCurItemStyle: Integer;
    function GetCurItemNo: Integer;
    function GetOffsetInCurItem: Integer;
    procedure ApplyTextStyleConversionProc(Sender: TCustomRichViewEdit;
      StyleNo, UserData: Integer; AppliedToText: Boolean;
      var NewStyleNo: Integer);
    function GetUndoLimit: Integer;
    procedure SetUndoLimit(const Value: Integer);

    function IsUndoShortcut(Shift: TShiftState; Key: Word): Boolean;
    function IsRedoShortcut(Shift: TShiftState; Key: Word): Boolean;
    {$IFNDEF RVDONOTUSEUNICODE}
    procedure InsertTextW_(const text: String);
    {$ENDIF}
    procedure SetTabNavigation(const Value: TRVTabNavigationType);
    function GetTopLevelEditor: TCustomRichViewEdit;
    function GetRootEditor: TCustomRichViewEdit;
    function DoChanging: Boolean;
    function GetActualCurTextStyleNo: Integer;
  protected
    { Init & info }
    procedure CreateParams(var Params: TCreateParams); override;
    function GetDataClass: TRichViewRVDataClass; override;
    procedure SetReadOnly(const Value: Boolean); virtual;
    function GetReadOnly: Boolean; virtual;    
    { Keyboard }
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure OnEnterPress(Shift: Boolean);
    procedure OnBackSpacePress(Ctrl: Boolean);
    procedure OnDeletePress(Ctrl: Boolean);
    { Scrolling }
    procedure AfterVScroll; override;
    procedure AfterHScroll; override;
    {$IFNDEF RVDONOTUSEDRAGDROP}
    { Ole drag&drop: IDropTarget related }
    function OleDragEnter(X,Y: Integer): Boolean; override;
    procedure OleDragLeave; override;
    function OleDragOver(X, Y: Integer): Boolean; override;
    function OleDrop(const DataObj: IDataObject; FMove: Boolean): Integer; override;
    procedure ReleaseOleDropTargetObject; override;
    function GetAcceptableRVFormat: Word;
    function OleCanAcceptFormat(Format: Word): Boolean; override;
    {$ENDIF}
    {$IFNDEF RVDONOTUSERTFIMPORT}
    function GetBasePathFromHTMLInClipboard: String;
    {$ENDIF}
    property OnCurParaStyleChanged: TNotifyEvent read FOnCurParaStyleChanged write FOnCurParaStyleChanged;
    property OnCurTextStyleChanged: TNotifyEvent read FOnCurTextStyleChanged write FOnCurTextStyleChanged;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnCaretMove: TNotifyEvent read FOnCaretMove write FOnCaretMove;
    property OnChanging: TRVChangingEvent read FOnChanging write FOnChanging;
    property OnPaste: TRVPasteEvent read FOnPaste write FOnPaste;
    {$IFDEF RVONCUT}
    property OnCut: TNotifyEvent read FOnCut write FOnCut;
    {$ENDIF}
    property TabNavigation read GetTabNavigation write SetTabNavigation default rvtnNone;
    property AcceptDragDropFormats: TRVDragDropFormats read FAcceptDragDropFormats
      write FAcceptDragDropFormats default [rvddRVF, rvddRTF, rvddText, rvddUnicodeText,
       rvddBitmap, rvddMetafile, rvddFiles];
  public
    LockCount: Integer;
    FCurStyleConversion: TRVStyleConversionEvent;
    { Constructor - destructor }
    constructor Create(AOwner: TComponent); override;
    { For internal use }
    procedure SetFReadOnly(Value: Boolean);
    function BeforeInserting: Boolean;
    procedure AfterInserting;
    procedure CurParaStyleChange; dynamic;
    procedure CurTextStyleChange; dynamic;
    function BeforeChange(FromOutside: Boolean): Boolean; virtual;
    procedure DoChange(ClearRedo: Boolean); dynamic;
    procedure Selecting; dynamic;
    procedure AfterCaretMove;
    procedure AssignEvents(Source: TCustomRichView);
    {$IFNDEF RVDONOTUSEUNICODE}
    procedure BeforeUnicode;
    {$ENDIF}
    { Inserting in the caret position (can be undone/redone) }
    {$IFNDEF RVDONOTUSEUNICODE}
    {$IFDEF RICHVIEWCBDEF3}
    procedure InsertTextW(const text: WideString; CaretBefore: Boolean {$IFDEF RICHVIEWDEF4}=False{$ENDIF});
    procedure InsertStringWTag(const s: WideString; Tag: Integer);
    {$ENDIF}
    {$ENDIF}
    function InsertItem(const Name: String; Item: TCustomRVItemInfo): Boolean;
    procedure InsertText(const text: String; CaretBefore: Boolean
      {$IFDEF RICHVIEWDEF4}=False{$ENDIF});
    function InsertStringTag(const s: String; Tag: Integer): Boolean;
    function InsertControl(const Name: String; ctrl: TControl;
      VAlign: TRVVAlign): Boolean;
    function InsertPicture(const Name: String; gr: TGraphic;
      VAlign: TRVVAlign): Boolean;
    function InsertHotPicture(const Name: String; gr: TGraphic;
      VAlign: TRVVAlign): Boolean;
    function InsertBreak(Width: Byte; Style: TRVBreakStyle;
      Color: TColor): Boolean;
    function InsertBullet(ImageIndex: Integer;
      ImageList: TCustomImageList): Boolean;
    function InsertHotspot(ImageIndex, HotImageIndex: Integer;
      ImageList: TCustomImageList): Boolean;
    { Inserting in the caret position from files and streams
      (can be undone/redone) }
    function InsertRVFFromStreamEd(Stream: TStream):Boolean;
    function InsertRVFFromFileEd(const FileName: String):Boolean;
    function InsertTextFromFile(const FileName: String):Boolean;
    function InsertOEMTextFromFile(const FileName: String):Boolean;
    {$IFNDEF RVDONOTUSEUNICODE}
    function InsertTextFromFileW(const FileName: String):Boolean;
    {$ENDIF}
    {$IFNDEF RVDONOTUSERTFIMPORT}
    function InsertRTFFromStreamEd(Stream: TStream): Boolean;
    function InsertRTFFromFileEd(const FileName: String): Boolean;
    {$ENDIF}
    { Page breaks (can be undone/redone) }
    procedure InsertPageBreak;
    procedure RemoveCurrentPageBreak;
    { Operations specific to some item types (can be undone/redone) }
    procedure ConvertToPicture(ItemNo: Integer);
    procedure ConvertToHotPicture(ItemNo: Integer);
    { Operations on selection (can be undone/redone) }
    function  CanDelete: Boolean;
    procedure DeleteSelection;dynamic;
    procedure CutDef;
    procedure ApplyParaStyle(ParaStyleNo: Integer);
    procedure ApplyTextStyle(TextStyleNo: Integer);
    procedure ApplyStyleConversion(UserData: Integer);
    procedure ApplyParaStyleConversion(UserData: Integer);
    { Can paste ? }
    {$IFNDEF RVDONOTUSERTFIMPORT}
    function CanPasteRTF: Boolean;
    {$ENDIF}
    function CanPaste: Boolean; dynamic;
    function CanPasteRVF: Boolean;
    { Pasting from the Clipboard (can be undone/redone) }
    procedure Paste; dynamic;
    procedure PasteBitmap(TextAsName: Boolean);
    procedure PasteMetafile(TextAsName: Boolean);
    procedure PasteText;
    procedure PasteRVF;
    {$IFNDEF RVDONOTUSERTFIMPORT}
    procedure PasteRTF;
    {$ENDIF}
    {$IFNDEF RVDONOTUSEUNICODE}
    procedure PasteTextW;
    {$ENDIF}
    { Get info about item in the position of caret - general properties }
    function GetCurrentTag: Integer;
    function GetCurrentItemText: String;
    function GetCurrentItem: TCustomRVItemInfo;
    function GetCurrentItemEx(RequiredClass: TCustomRVItemInfoClass;
      var ItemRichViewEdit: TCustomRichViewEdit;
      var Item: TCustomRVItemInfo): Boolean;
    {$IFNDEF RVDONOTUSEUNICODE}
    function GetCurrentItemTextA: String;
    {$IFDEF RICHVIEWCBDEF3}
    function GetCurrentItemTextW: WideString;
    {$ENDIF}
    {$ENDIF}
    { Get info about item in the position of caret - for specific item types }
    procedure GetCurrentBreakInfo(var AWidth: Byte;
      var AStyle: TRVBreakStyle; var AColor: TColor; var ATag: Integer);
    procedure GetCurrentBulletInfo(var AName: String;
      var AImageIndex: Integer; var AImageList: TCustomImageList;
      var ATag: Integer);
    procedure GetCurrentHotspotInfo(var AName: String;
      var AImageIndex, AHotImageIndex: Integer; var AImageList: TCustomImageList;
      var ATag: Integer);
    procedure GetCurrentPictureInfo(var AName: String; var Agr: TGraphic;
      var AVAlign: TRVVAlign; var ATag: Integer);
    procedure GetCurrentControlInfo(var AName: String; var Actrl: TControl;
      var AVAlign: TRVVAlign; var ATag: Integer);
    procedure GetCurrentTextInfo(var AText: String; var ATag: Integer);
    { Set info for item - general properties (can be undone/redone) }
    procedure SetItemTextEd(ItemNo: Integer; const s: String);
    procedure SetItemTagEd(ItemNo: Integer; ATag: Integer);    
    procedure ResizeControl(ItemNo, NewWidth, NewHeight: Integer);
    {$IFNDEF RVDONOTUSEUNICODE}
    procedure SetItemTextEdA(ItemNo: Integer; const s: String);
    {$IFDEF RICHVIEWCBDEF3}
    procedure SetItemTextEdW(ItemNo: Integer; const s: WideString);
    {$ENDIF}
    {$ENDIF}
    { Set info for item in the position of caret - general properties
      (can be undone/redone) }
    procedure SetCurrentItemText(const s: String);
    procedure SetCurrentTag(ATag: Integer);
    {$IFNDEF RVDONOTUSEUNICODE}
    procedure SetCurrentItemTextA(const s: String);
    {$IFDEF RICHVIEWCBDEF3}
    procedure SetCurrentItemTextW(const s: WideString);
    {$ENDIF}
    {$ENDIF}
    { Set info for item - for specific item types
      (can be undone/redone) }
    procedure SetBreakInfoEd(ItemNo: Integer; AWidth: Byte;
      AStyle: TRVBreakStyle; AColor: TColor; ATag: Integer);
    procedure SetBulletInfoEd(ItemNo: Integer; const AName: String;
      AImageIndex: Integer; AImageList: TCustomImageList; ATag: Integer);
    procedure SetHotspotInfoEd(ItemNo: Integer; const AName: String;
      AImageIndex,  AHotImageIndex: Integer; AImageList: TCustomImageList;
      ATag: Integer);
    procedure SetPictureInfoEd(ItemNo: Integer; const AName: String;
      Agr: TGraphic; AVAlign: TRVVAlign; ATag: Integer);
    procedure SetControlInfoEd(ItemNo: Integer; const AName: String;
      AVAlign: TRVVAlign; ATag: Integer);
    { Set info for item in the position of caret - for specific item types
      (can be undone/redone) }
    procedure SetCurrentBreakInfo(AWidth: Byte; AStyle: TRVBreakStyle;
      AColor: TColor; ATag: Integer);
    procedure SetCurrentBulletInfo(const AName: String; AImageIndex: Integer;
      AImageList: TCustomImageList; ATag: Integer);
    procedure SetCurrentHotspotInfo(const AName: String;
      AImageIndex, AHotImageIndex: Integer; AImageList: TCustomImageList;
      ATag: Integer);
    procedure SetCurrentPictureInfo(const AName: String; Agr: TGraphic;
      AVAlign: TRVVAlign; ATag: Integer);
    procedure SetCurrentControlInfo(const AName: String; AVAlign: TRVVAlign;
      ATag: Integer);
    procedure ResizeCurrentControl(NewWidth, NewHeight: Integer);
    { Item operations: misc. }
    procedure AdjustControlPlacement(ItemNo: Integer);
    procedure AdjustControlPlacement2(Control: TControl);
    procedure BeginItemModify(ItemNo: Integer; var ModifyData: Integer);
    procedure EndItemModify(ItemNo: Integer; ModifyData: Integer);
    procedure BeginCurrentItemModify(var ModifyData: Integer);
    procedure EndCurrentItemModify(ModifyData: Integer);
    { Extra item properties (changing can be undone/redone) }
    function GetCurrentItemExtraIntProperty(Prop: TRVExtraItemProperty;
      var Value: Integer): Boolean;
    procedure SetItemExtraIntPropertyEd(ItemNo: Integer;
      Prop: TRVExtraItemProperty; Value: Integer; AutoReformat: Boolean);
    procedure SetCurrentItemExtraIntProperty(Prop: TRVExtraItemProperty;
      Value: Integer; AutoReformat: Boolean);
    function GetCurrentItemExtraStrProperty(Prop: TRVExtraItemStrProperty;
      var Value: String): Boolean;
    procedure SetItemExtraStrPropertyEd(ItemNo: Integer;
      Prop: TRVExtraItemStrProperty; const Value: String; AutoReformat: Boolean);
    procedure SetCurrentItemExtraStrProperty(Prop: TRVExtraItemStrProperty;
      const Value: String; AutoReformat: Boolean);
    { Checkpoints (changing can be undone/redone) }
    function GetCurrentCheckpoint: TCheckpointData;
    procedure SetCheckpointInfoEd(ItemNo: Integer; ATag: Integer;
      const AName: String; ARaiseEvent: Boolean);
    procedure RemoveCheckpointEd(ItemNo: Integer);
    procedure SetCurrentCheckpointInfo(ATag: Integer; const AName: String;
      ARaiseEvent: Boolean);
    procedure RemoveCurrentCheckpoint;
    { Selecting }
    function SearchText(s: String; SrchOptions: TRVESearchOptions): Boolean;
    procedure SelectCurrentWord;
    procedure SelectCurrentLine;
    { Changing }
    function CanChange: Boolean;
    procedure Change;
    procedure BeginUpdate;
    procedure EndUpdate;
    { Undo and redo }
    procedure Undo; dynamic;
    procedure Redo; dynamic;
    function UndoAction: TRVUndoType;
    function RedoAction: TRVUndoType;
    function UndoName: String;
    function RedoName: String;
    procedure ClearUndo;
    procedure SetUndoGroupMode(GroupUndo: Boolean);
    procedure BeginUndoGroup(UndoType: TRVUndoType);
    procedure BeginUndoCustomGroup(const Name: String);
    {$IFNDEF RVDONOTUSELISTS}
    procedure ApplyListStyle(AListNo, AListLevel, AStartFrom: Integer;
                             AUseStartFrom, ARecursive: Boolean);
    procedure RemoveLists(ARecursive: Boolean);
    procedure ChangeListLevels(LevelDelta: Integer);
    {$ENDIF}
    procedure GetCurrentLineCol(var Line, Column: Integer);



    property Modified: Boolean read FModified write FModified;

    property CurItemNo: Integer read GetCurItemNo;
    property CurItemStyle: Integer read GetCurItemStyle;
    property CurParaStyleNo: Integer read GetCurParaStyleNo write SetCurParaStyleNo;
    property CurTextStyleNo: Integer read GetCurTextStyleNo write SetCurTextStyleNo;
    property ActualCurTextStyleNo: Integer read GetActualCurTextStyleNo write SetCurTextStyleNo;
    property OffsetInCurItem: Integer read GetOffsetInCurItem;

    property EditorOptions: TRVEditorOptions read FEditorOptions write FEditorOptions default DEFAULT_RVEDITOROPTIONS;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly;
    property UndoLimit: Integer read GetUndoLimit write SetUndoLimit default -1;

    property TopLevelEditor: TCustomRichViewEdit read GetTopLevelEditor;

    property OnCaretGetOut: TRVOnCaretGetOutEvent read FOnCaretGetOut write FOnCaretGetOut;
    property OnDropFiles: TRVDropFilesEvent read FOnDropFiles write FOnDropFiles;
    property OnParaStyleConversion: TRVStyleConversionEvent read FOnParaStyleConversion write FOnParaStyleConversion;
    property OnStyleConversion: TRVStyleConversionEvent read FOnStyleConversion write FOnStyleConversion;
    property OnMouseMove;
    property OnDragOver;
    property OnDragDrop;

  end;
  { ---------------------------------------------------------------------------
    TRichViewEdit component: editor.
    TRichViewEdit publishes properties of TCustomRichViewEdit.
  }
  TRichViewEdit = class (TCustomRichViewEdit)
  published
    { Published declarations: new for TRichViewEdit }
    property AcceptDragDropFormats;
    property EditorOptions;
    property ReadOnly;
    property UndoLimit;

    property OnCaretGetOut;
    property OnChange;
    property OnChanging;
    property OnCaretMove;
    property OnCurParaStyleChanged;
    property OnCurTextStyleChanged;
    {$IFDEF RVONCUT}
    property OnCut;
    {$ENDIF}
    property OnDropFiles;
    property OnParaStyleConversion;
    property OnPaste;
    property OnStyleConversion;
    property TabNavigation;
    { Published standard properties }
    property Align;
    {$IFDEF RICHVIEWDEF4}
    property Anchors;
    property Constraints;
    {$ENDIF}
    property Color default clNone;
    property Ctl3D;
    {$IFDEF RICHVIEWDEF4}
    property DragKind;
    {$ENDIF}
    property DragMode;
    property Enabled;
    property HelpContext;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    { Published standard events }
    property OnClick;
    {$IFDEF RICHVIEWDEF5}
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseMove;
    {$IFDEF RICHVIEWDEF4}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;    
    {$ENDIF}
    property OnStartDrag;
    { Published RichView properties }
    property BackgroundBitmap;
    property BackgroundStyle default bsNoBitmap;
    property BiDiMode;
    property BorderStyle default bsSingle;
    property BottomMargin;
    //property CPEventKind stored False;
    property Cursor default crIBeam;
    property Delimiters;
    //property DocProperties;
    property DoInPaletteMode;
    property FirstJumpNo;
    property HScrollVisible;
    property LeftMargin;
    property MaxTextWidth;
    property MinTextWidth;
    property Options;
    property RightMargin;
    property RTFOptions;
    property RTFReadProperties;
    property RVFOptions;
    property RVFParaStylesReadMode;
    property RVFTextStylesReadMode;
    {$IFDEF RVFLATSCROLLBARS}
    property ScrollBarColor;
    property ScrollBarStyle;
    {$ENDIF}
    property Style;
    //property TabNavigation;
    property TopMargin;
    property Tracking;
    property UseXPThemes;
    {$IFDEF RICHVIEWDEF3}
    property VAlign;
    {$ENDIF}
    property VScrollVisible;
    {$IFDEF RICHVIEWDEF4}
    property WheelStep;
    {$ENDIF}
    { Published RichView events }
    //property OnCheckpointVisible;
    property OnControlAction;
    property OnCopy;
    {$IFDEF RV_ODHC}
    property OnDocumentHeightChange;
    {$ENDIF}
    property OnImportPicture;
    property OnItemAction;
    property OnItemHint;
    property OnJump;
    property OnHScrolled;    
    property OnHTMLSaveImage;
    property OnPaint;
    property OnProgress;    
    property OnReadHyperlink;    
    property OnRVDblClick;
    property OnRVFImageListNeeded;
    property OnRVFControlNeeded;
    property OnRVFPictureNeeded;
    property OnRVMouseDown;
    property OnRVMouseMove;
    property OnRVMouseUp;
    property OnRVRightClick;
    property OnSaveComponentToFile;
    property OnSaveHTMLExtra;
    property OnSaveImage2;
    property OnSaveItemToFile;    
    property OnSaveRTFExtra;    
    property OnSelect;
    property OnVScrolled;
    property OnWriteHyperlink;
    { obsolete properties }
    property AllowSelection;
    property SingleClick;
    property OnURLNeeded;    
  end;
  
  TRichViewUnicodeInput = (rvuiStandard, rvuiAlternative);
const RichViewUnicodeInput: TRichViewUnicodeInput = rvuiStandard;

implementation
uses RVERVData, RVUndo, RVStr;
{------------------------------------------------------------------------------}
constructor TCustomRichViewEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  KeyboardScroll   := False;
  Flags            := Flags + [rvflRootEditor] - [rvflUseJumps{, rvflTrim},rvflUseExternalLeading];
  Cursor           := crIBeam;
  EditorOptions    := DEFAULT_RVEDITOROPTIONS;
  UndoLimit        := -1;
  AcceptDragDropFormats := [rvddRVF, rvddRTF, rvddText, rvddUnicodeText,
    rvddBitmap, rvddMetafile, rvddFiles];
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.CreateParams(var Params: TCreateParams);
begin
   inherited CreateParams(Params);
   FVDisableNoScroll := True;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetDataClass: TRichViewRVDataClass;
begin
   Result := TRVEditRVData;
end;
{------------------------------------------------------------------------------}
{ Processing message: WM_CREATE.
  Registering itself as a drag&drop target.                                    }
procedure TCustomRichViewEdit.WMCreate(var Message: TMessage);
begin
  inherited;
  {$IFNDEF RVDONOTUSEDRAGDROP}
  if not (csDesigning in ComponentState) then begin
    if FDropTarget=nil then
      FDropTarget := TRVDropTarget.Create(Self);
    FDropTarget.RegisterDragDropForOwner;
  end;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
{ Processing message: WM_DESTROY.
  Unregistering itself as a drag&drop target.                                  }
procedure TCustomRichViewEdit.WMDestroy(var Message: TMessage);
begin
  {$IFNDEF RVDONOTUSEDRAGDROP}
  if not (csDesigning in ComponentState) and Assigned(FDropTarget) then
    FDropTarget.UnRegisterDragDropForOwner;
  {$ENDIF}
  inherited;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.BeforeInserting: Boolean;
var StyleNo: Integer;
begin
  Result := False;
  if (RVData.PartialSelectedItem<>nil) or not CanDelete or
    not BeforeChange(False) then begin
    TRVEditRVData(RVData).Beep;
    Exclude(RVData.State, rvstDoNotClearCurTag);
    exit;
  end;
  TRVEditRVData(RVData).BeginUndoSequence(rvutInsert, True);
  SetUndoGroupMode(True);
  StyleNo := ActualCurTextStyleNo;
  TRVEditRVData(RVData).AssignCurTag;
  TRVEditRVData(RVData).DeleteSelection_;
  TRVEditRVData(RVData).Deselect(nil, True);
  CurTextStyleNo := StyleNo;
  Result := True;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.AfterInserting;
begin
  SetUndoGroupMode(False);
  Refresh;
  Change;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertBreak(Width: Byte; Style: TRVBreakStyle;
  Color: TColor): Boolean;
var info: TRVBreakItemInfo;
    s: String;
begin
  Result := False; 
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).InsertBreak(Width, Style, Color);
    exit;
  end;
  {$ENDIF}
  if not BeforeInserting then exit;
  try
    info := TRVBreakItemInfo.CreateEx(RVData, Width, Style, Color);
    s := '';
    Result := TRVEditRVData(RVData).InsertSomething(info, s, False, False);
  finally
    AfterInserting;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertBullet(ImageIndex: Integer;
  ImageList: TCustomImageList): Boolean;
var info: TRVBulletItemInfo;
    s: String;
begin
  Result := False;
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).InsertBullet(ImageIndex, ImageList);
    exit;
  end;
  {$ENDIF}
  if not BeforeInserting then exit;
  try
    info  := TRVBulletItemInfo.CreateEx(RVData, ImageIndex, ImageList, rvvaBaseline);
    s := '';
    Result := TRVEditRVData(RVData).InsertSomething(info, s, False, False);
  finally
    AfterInserting;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertItem(const Name: String; Item: TCustomRVItemInfo): Boolean;
var s: String;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).InsertItem(Name, Item);
    exit;
  end;
  {$ENDIF}
  if not BeforeInserting then begin
    Item.Free;
    Result := False;
    exit;
  end;
  try
    s := Name;
    Result := TRVEditRVData(RVData).InsertSomething(Item, s, False, False);
  finally
    AfterInserting;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.InsertText(const text: String;  CaretBefore: Boolean {$IFDEF RICHVIEWDEF4}=False{$ENDIF});
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).InsertText(text, CaretBefore);
    exit;
  end;
  {$ENDIF}
  Include(RVData.State, rvstDoNotClearCurTag);
  if not BeforeInserting then exit;
  try
    TRVEditRVData(RVData).InsertText_(text, True, CaretBefore);
  finally
    SetUndoGroupMode(False);
    Exclude(RVData.State, rvstDoNotClearCurTag);
    Change;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertTextFromFile(const FileName: String):Boolean;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).InsertTextFromFile(FileName);
    exit;
  end;
  {$ENDIF}
  Result := True;
  Include(RVData.State, rvstDoNotClearCurTag);
  if not BeforeInserting then exit;
  try
    Result := TRVEditRVData(RVData).InsertTextFromFile(FileName, False, False);
  finally
    SetUndoGroupMode(False);
    Exclude(RVData.State, rvstDoNotClearCurTag);    
    Change;
  end;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEUNICODE}
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.BeforeUnicode;
begin
  if not Style.TextStyles[CurTextStyleNo].Unicode and
     (Style.DefUnicodeStyle>=0) and
     (Style.DefUnicodeStyle<Style.TextStyles.Count) then
    CurTextStyleNo := Style.DefUnicodeStyle;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertTextFromFileW(const FileName: String):Boolean;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).InsertTextFromFileW(FileName);
    exit;
  end;
  {$ENDIF}
  Result := True;
  Include(RVData.State, rvstDoNotClearCurTag);
  if not BeforeInserting then exit;
  try
    BeforeUnicode;
    Result := TRVEditRVData(RVData).InsertTextFromFileW(FileName,True);
  finally
    SetUndoGroupMode(False);
    Exclude(RVData.State, rvstDoNotClearCurTag);
    Change;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.InsertTextW_(const text: String);
begin
  Include(RVData.State, rvstDoNotClearCurTag);
  if not BeforeInserting then exit;
  try
    BeforeUnicode;
    TRVEditRVData(RVData).InsertTextW_(text,True, False);
  finally
    SetUndoGroupMode(False);
    Exclude(RVData.State, rvstDoNotClearCurTag);
    Change;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.PasteTextW;
var
    mem: Cardinal;
    ptr: Pointer;
    s: String;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).PasteTextW;
    exit;
  end;
  {$ENDIF}
  if not BeforeChange(False) then exit;
  if not Clipboard.HasFormat(CF_UNICODETEXT) then exit;
  Clipboard.Open;
  try
    mem := Clipboard.GetAsHandle(CF_UNICODETEXT);
    SetLength(s, GlobalSize(mem));
    ptr := GlobalLock(mem);
    Move(ptr^,PChar(s)^, Length(s));
    GlobalUnlock(mem);
  finally
    Clipboard.Close;
  end;
  if Length(s)=0 then exit;
  ptr := RVU_StrScanW(Pointer(s), 0, Length(s) div 2);
  if ptr<>nil then
    SetLength(s, PChar(ptr)-PChar(s));
  InsertTextW_(s);
end;
function TCustomRichViewEdit.GetCurrentItemTextA: String;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).GetCurrentItemTextA;
    exit;
  end;
  {$ENDIF}
  TRVEditRVData(RVData).PrepareForEdit;
  Result := GetItemTextA(CurItemNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetItemTextEdA(ItemNo: Integer;
  const s: String);
begin
  SetItemTextEd(ItemNo, RVData.GetTextInItemFormatA(ItemNo, s));
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetCurrentItemTextA(const s: String);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SetCurrentItemTextA(s);
    exit;
  end;
  {$ENDIF}
  TRVEditRVData(RVData).PrepareForEdit;
  SetItemTextEdA(CurItemNo, s);
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
function TCustomRichViewEdit.GetCurrentItemTextW: WideString;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).GetCurrentItemTextW;
    exit;
  end;
  {$ENDIF}
  TRVEditRVData(RVData).PrepareForEdit;
  Result := GetItemTextW(CurItemNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetItemTextEdW(ItemNo: Integer;
  const s: WideString);
begin
  SetItemTextEd(ItemNo, RVData.GetTextInItemFormatW(ItemNo, s));
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetCurrentItemTextW(const s: WideString);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SetCurrentItemTextW(s);
    exit;
  end;
  {$ENDIF}
  TRVEditRVData(RVData).PrepareForEdit;
  SetItemTextEdW(CurItemNo, s);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.InsertTextW(const text: WideString; CaretBefore: Boolean {$IFDEF RICHVIEWDEF4}=False{$ENDIF});
var s: String;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).InsertTextW(text, CaretBefore);
    exit;
  end;
  {$ENDIF}
  if not BeforeInserting then exit;
  try
    BeforeUnicode;
    s := RVU_GetRawUnicode(text);
    TRVEditRVData(RVData).InsertTextW_(s,True, CaretBefore);
  finally
    SetUndoGroupMode(False);
    Change;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.InsertStringWTag(const s: WideString; Tag: Integer);
var info: TRVTextItemInfo;
    s2: String;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).InsertStringTag(s, Tag);
    exit;
  end;
  {$ENDIF}
  if not BeforeInserting then exit;
  try
    BeforeUnicode;
    info         := RichViewTextItemClass.Create(RVData);
    info.StyleNo := ActualCurTextStyleNo;
    info.Tag     := Tag;
    info.ItemOptions := info.ItemOptions+[rvioUnicode];
    s2 := RVU_GetRawUnicode(s);
    s2 := RV_ReplaceTabsW(s2, Style.SpacesInTab);    
    {$IFNDEF RVDONOTUSEUNICODE}
    if not Style.TextStyles[info.GetActualStyleNo(Style)].Unicode then begin
      s2 := RVU_UnicodeToAnsi(RVData.GetStyleCodePage(
        info.GetActualStyleNo(Style)), s2);
      info.ItemOptions := info.ItemOptions-[rvioUnicode];
    end;
    {$ENDIF}
    TRVEditRVData(RVData).InsertSomething(info, s2, False, False);
  finally
    AfterInserting;
  end;
end;
{$ENDIF}
{$ENDIF}
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertOEMTextFromFile(const FileName: String):Boolean;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).InsertOEMTextFromFile(FileName);
    exit;
  end;
  {$ENDIF}
  Result := True;
  if not BeforeInserting then exit;
  try
    Result := TRVEditRVData(RVData).InsertTextFromFile(FileName, True, True);
  finally
    SetUndoGroupMode(False);
    Change;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertStringTag(const s: String; Tag: Integer): Boolean;
var info: TRVTextItemInfo;
    s2: String;
begin
  Result := False;
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).InsertStringTag(s, Tag);
    exit;
  end;
  {$ENDIF}
  if not BeforeInserting then exit;
  try
    info         := RichViewTextItemClass.Create(RVData);
    info.StyleNo := ActualCurTextStyleNo;
    info.Tag     := Tag;
    s2 := RV_ReplaceTabsA(s, Style.SpacesInTab);
    {$IFNDEF RVDONOTUSEUNICODE}
    if Style.TextStyles[info.GetActualStyleNo(Style)].Unicode then begin
      s2 := RVU_AnsiToUnicode(RVData.GetStyleCodePage(
        info.GetActualStyleNo(Style)), s);
      info.ItemOptions := info.ItemOptions+[rvioUnicode];
    end;
    {$ENDIF}
    Result := TRVEditRVData(RVData).InsertSomething(info, s2, False, False);
  finally
    AfterInserting;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertControl(const Name: String; ctrl: TControl;
  VAlign: TRVVAlign): Boolean;
var info: TRVControlItemInfo;
    s: String;
begin
  Result := False;
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).InsertControl(Name, ctrl, VAlign);
    exit;
  end;
  {$ENDIF}
  if not BeforeInserting then exit;
  try
    info         := TRVControlItemInfo.CreateEx(RVData, ctrl, VAlign);
    ctrl.Visible := False;
    ctrl.Parent  := Self;
    s := Name;
    Result := TRVEditRVData(RVData).InsertSomething(info, s, False, False);
  finally
    AfterInserting;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertHotspot(ImageIndex, HotImageIndex: Integer;
  ImageList: TCustomImageList): Boolean;
var info: TRVHotspotItemInfo;
    s: String;
begin
  Result := False;
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).InsertHotspot(ImageIndex, HotImageIndex, ImageList);
    exit;
  end;
  {$ENDIF}
  if not BeforeInserting then exit;
  try
    info := TRVHotspotItemInfo.CreateEx(RVData, ImageIndex, HotImageIndex, ImageList, rvvaBaseline);
    s := '';
    Result := TRVEditRVData(RVData).InsertSomething(info, s, False, False);
  finally
    AfterInserting;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertPicture(const Name: String; gr: TGraphic;
  VAlign: TRVVAlign): Boolean;
var info: TRVGraphicItemInfo;
    s: String;
begin
  Result := False;
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).InsertPicture(Name, gr, VAlign);
    exit;
  end;
  {$ENDIF}
  if not BeforeInserting then exit;
  try
    info := TRVGraphicItemInfo.CreateEx(RVData, gr, VAlign);
    info.UpdatePaletteInfo(DoInPaletteMode, False, RVPalette, PRVLogPalette);
    s := Name;
    Result := TRVEditRVData(RVData).InsertSomething(info, s, False, False);
  finally
    AfterInserting;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertHotPicture(const Name: String; gr: TGraphic;
  VAlign: TRVVAlign): Boolean;
var info: TRVHotGraphicItemInfo;
    s: String;
begin
  Result := False;
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).InsertHotPicture(Name, gr, VAlign);
    exit;
  end;
  {$ENDIF}
  if not BeforeInserting then exit;
  try
    info := TRVHotGraphicItemInfo.CreateEx(RVData, gr, VAlign);
    info.UpdatePaletteInfo(DoInPaletteMode, False, RVPalette, PRVLogPalette);
    s := Name;
    Result := TRVEditRVData(RVData).InsertSomething(info, s, False, False);
  finally
    AfterInserting;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.CanDelete: Boolean;
begin
  Result := TRVEditRVData(GetTopLevelEditor.RVData).CanDelete;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.DeleteSelection;
var Selected: Boolean;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).DeleteSelection;
    exit;
  end;
  {$ENDIF}
  if not BeforeChange(False) then exit;
  Selected := RVData.SelectionExists(False, True);
  TRVEditRVData(RVData).DeleteSelection_;
  if Selected then
    Invalidate;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetCurItemNo: Integer;
begin
  Result := TRVEditRVData(RVData).GetCurItemNo;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetOffsetInCurItem: Integer;
begin
  Result := TRVEditRVData(RVData).GetOffsetInCurItem;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetCurItemStyle: Integer;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).GetCurItemStyle;
    exit;
  end;
  {$ENDIF}
  TRVEditRVData(RVData).PrepareForEdit;
  Result := GetItemStyle(GetCurItemNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.WMKillFocus(var Message: TWMKillFocus);
var PRVData: TCustomRVData;
begin
  inherited;
  if (csDestroying in ComponentState) or (rvstNoKillFocusEvents in RVData.GetAbsoluteRootData.State) then exit;
  TRVEditRVData(RVData).ClearJumpsCoords;
  if not ReadOnly then begin
    PRVData := RVData.GetAbsoluteParentData;
    if (PRVData<>nil) and (PRVData is TRVEditRVData) then begin
      TRVEditRVData(PRVData).ClearJumps;
      TRVEditRVData(PRVData).Flags := TRVEditRVData(PRVData).Flags - [rvflUseJumps];
      TRVEditRVData(PRVData).State := TRVEditRVData(PRVData).State - [rvstDrawHover];
    end;
  end;
  GenerateMouseMove;
  if (Style<>nil) and (not TRVEditRVData(RVData).NotFormatted) then begin
    HideCaret(Handle);
    DestroyCaret;
//    Invalidate;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.WMInputLangChange(var Message: TMessage);
{$IFDEF RICHVIEWCBDEF3}
var NewStyleNo : integer;
{$ENDIF}
begin
  {$IFDEF RICHVIEWCBDEF3}
  if (Style=nil) or (ItemCount=0) or
      not (rvoAutoSwitchLang in EditorOptions)
      {$IFNDEF RVDONOTUSEUNICODE}
      or Style.TextStyles[CurTextStyleNo].Unicode
      {$ENDIF}
      then
    exit;
  NewStyleNo := Style.TextStyles.FindStyleWithCharset(CurTextStyleNo, Message.WParam);
  if NewStyleNo=-1 then begin
    Style.TextStyles.Add;
    NewStyleNo := Style.TextStyles.Count-1;
    Style.TextStyles[NewStyleNo].Assign(Style.TextStyles[CurTextStyleNo]);
    Style.TextStyles[NewStyleNo].Standard := False;    
    Style.TextStyles[NewStyleNo].Charset := Message.WParam;
  end;
  CurTextStyleNo:=NewStyleNo;
  Message.Result := 1;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if {TRVEditRVData(RVData).NotFormatted} True then begin
    TRVEditRVData(RVData).PrepareForEdit;
    if (Style<>nil) and (not TRVEditRVData(RVData).NotFormatted) then begin
      TRVEditRVData(RVData).ChangeCaret(True,False,True,False);
      Invalidate;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS or DLGC_WANTCHARS;
  if not (rvoDoNotWantReturns in EditorOptions) then
    Message.Result := Message.Result or DLGC_WANTALLKEYS;
  if rvoWantTabs in EditorOptions then
    Message.Result := Message.Result or DLGC_WANTTAB;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.EMRedo(var Message: TMessage);
begin
  Message.Result := Integer(RedoAction<>rvutNone);
  if Message.Result<>0 then
    Redo;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.WMClear(var Message: TMessage);
begin
  DeleteSelection;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.WMUndo(var Message: TMessage);
begin
  Message.Result := Integer(UndoAction<>rvutNone);
  Undo;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.EMUndo(var Message: TMessage);
begin
  Message.Result := Integer(UndoAction<>rvutNone);
  Undo;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.EMCanRedo(var Message: TMessage);
begin
  Message.Result := Integer(RedoAction<>rvutNone);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.EMCanUndo(var Message: TMessage);
begin
  Message.Result := Integer(UndoAction<>rvutNone);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.CurParaStyleChange;
begin
  if (Style<>nil) and not (csDestroying in ComponentState) and
     Assigned(FOnCurParaStyleChanged) then
    FOnCurParaStyleChanged(GetRootEditor);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.CurTextStyleChange;
begin
  if (Style<>nil) and not (csDestroying in ComponentState) and
     Assigned(FOnCurTextStyleChanged) then
     FOnCurTextStyleChanged(GetRootEditor);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetCurParaStyleNo(const Value: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SetCurParaStyleNo(Value);
    exit;
  end;
  {$ENDIF}
  if (TRVEditRVData(RVData).FCurParaStyleNo <> Value) or
     (rvstForceStyleChangeEvent in RVData.State) then begin
    TRVEditRVData(RVData).FCurParaStyleNo := Value;
    CurParaStyleChange;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetCurTextStyleNo(const Value: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SetCurTextStyleNo(Value);
    exit;
  end;
  {$ENDIF}
  if (TRVEditRVData(RVData).FCurTextStyleNo <> Value) or
      (rvstForceStyleChangeEvent in RVData.State)
   then begin
    Exclude(RVData.State, rvstForceStyleChangeEvent);
    TRVEditRVData(RVData).FCurTextStyleNo := Value;
    CurTextStyleChange;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetCurParaStyleNo: Integer;
begin
  Result := TRVEditRVData(GetTopLevelEditor.RVData).FCurParaStyleNo;
  {$IFDEF RVDEBUG}{$I Debug\k.inc}{$ENDIF}
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetCurTextStyleNo: Integer;
begin
  Result := RVData.GetActualStyle2(
    TRVEditRVData(GetTopLevelEditor.RVData).FCurTextStyleNo,
    TRVEditRVData(GetTopLevelEditor.RVData).FCurParaStyleNo);
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetActualCurTextStyleNo: Integer;
begin
  Result := TRVEditRVData(GetTopLevelEditor.RVData).FCurTextStyleNo;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Key=VK_CONTROL then begin
    TRVEditRVData(RVdata).ClearJumpsCoords;
    GenerateMouseMove;
  end;
  inherited KeyUp(Key, Shift);
end;
{-----------------------------------------------------------------------}
procedure TCustomRichViewEdit.WMSysChar(var Message: TWMSysChar);
begin
  if (Message.CharCode = VK_BACK) and (UndoLimit<>0) then
    Message.Result := 0
  else
    inherited;
end;
{-----------------------------------------------------------------------}
{
function IsLeadByte(ch: Char; CodePage: TRVCodePage): Boolean;
var cpinfo: TCPInfo;
    i: Integer;
begin
  Result := False;
  GetCPInfo(CodePage, cpinfo);
  if cpinfo.MaxCharSize=1 then
    exit;
  for i := 0 to MAX_LEADBYTES div 2-1 do begin
    if (cpinfo.LeadByte[i*2]=0) and (cpinfo.LeadByte[i*2+1]=0) then
      break;
    if ch in [chr(cpinfo.LeadByte[i*2])..chr(cpinfo.LeadByte[i*2+1])] then begin
      Result := True;
      break;
    end;
  end;
end;
}
{-----------------------------------------------------------------------}
{$IFDEF VER93} // C++Builder 1
function ToUnicode(wVirtKey, wScanCode: UINT; const KeyState: TKeyboardState;
  var pwszBuff; cchBuff: Integer; wFlags: UINT): Integer; stdcall;
  external user32 name 'ToUnicode';
{$ENDIF}
procedure TCustomRichViewEdit.WMKeyDown(var Message: TWMKeyDown);
{$IFNDEF RVDONOTUSEUNICODE}
var KeyState: TKeyboardState;
    s: String;
    Len: Integer;
    CodePage: TRVCodePage;    
{$ENDIF}
begin
  {$IFNDEF RVDONOTUSEUNICODE}
  CodePage := RVU_GetKeyboardCodePage;  
  RVData.State := RVData.State-[rvstIgnoreNextChar];
  Len := 0;
  if RVNT and (CodePage=0) then
    BeforeUnicode;
  if ((RichViewUnicodeInput=rvuiAlternative) or (CodePage=0)) and RVNT and
     Style.TextStyles[CurTextStyleNo].Unicode then begin
    // Manual translation of key codes to Unicode
    if Message.CharCode in [ord('A')..ord('Z'),ord('0')..ord('9'),187..192,220] then begin
      if Style.TextStyles[CurTextStyleNo].Unicode then begin
        SetLength(s,10);
        if GetKeyboardState(KeyState)
          and not (((KeyState[VK_CONTROL] and $80)<>0) and ((KeyState[VK_MENU] and $80)=0)) then begin
          Len := ToUnicode(Message.CharCode, Message.KeyData, KeyState, PChar(s)^, 5, 0);
          if Len>0 then begin
            TRVEditRVData(RVData).DeleteSelection_;
            SetLength(s, Len*2);
            if Len=1 then
              TRVEditRVData(RVData).InsertTextTyping(s)
            else
              InsertTextW_(s);
          end;
        end;
      end;
    end;
  end;
  if Len<=0 then
    inherited
  else begin
    Message.CharCode := 0;
    Message.Result   := 0;
    RVData.State := RVData.State+[rvstIgnoreNextChar];
  end;
  {$ELSE}
  inherited;
  {$ENDIF}
end;
{-----------------------------------------------------------------------}
procedure TCustomRichViewEdit.KeyDown(var Key: Word; Shift: TShiftState);
var selchanged : ByteBool;
  {........................................................}

  procedure GetOut(Direction: TRVGetOutDirection);
  begin
    if Assigned(FOnCaretGetOut) then
      FOnCaretGetOut(Self, Direction);
  end;
begin
  TRVEditRVData(RVData).PrepareForEdit;
  if (Key=VK_RETURN) and (rvoDoNotWantReturns in EditorOptions) then
    Key := 0;
  if IsUndoShortcut(Shift, Key) then begin
    Undo;
    Key := 0;
    end
  else if IsRedoShortcut(Shift, Key) then begin
    Redo;
    Key := 0;
  end;
  if Key = 0 then
    exit;  
  inherited KeyDown(Key, Shift);
  if Key = 0 then
    exit;
  if (Key=VK_ESCAPE) and TRVEditRVData(RVData).CancelResize then
    exit;
  // Step 1 of 3. Working with selection
  selchanged := False;
  case Key of
    VK_LEFT, VK_RIGHT, VK_UP, VK_DOWN,
    VK_HOME, VK_END, VK_PRIOR, VK_NEXT:
    begin
      // if cursor movement key, then handle selection
      if ssShift in Shift then begin
          TRVEditRVData(RVData).StartShiftMoving;
          selchanged := True;
        end
      else begin
        selchanged := SelectionExists;
        if selchanged then Deselect;
      end
    end;
  VK_MENU, VK_SHIFT,
  VK_F1..VK_F24, VK_SCROLL, VK_NUMLOCK, VK_PAUSE, VK_CAPITAL, VK_ESCAPE,
  VK_SNAPSHOT, VK_LWIN, VK_RWIN, VK_APPS, VK_LSHIFT..{VK_LAUNCH_APP2}$B7,
  VK_PROCESSKEY, VK_ATTN..$FF:
    begin
      // Do nothing
    end;
  VK_CONTROL:
    begin
      if (rvoCtrlJumps in EditorOptions) and not (rvflUseJumps in Flags) then begin
        TRVEditRVData(RVData).BuildJumpsCoords(False);
        GenerateMouseMove;
      end;
    end;
  VK_BACK, VK_DELETE:
    begin
      if not BeforeChange(False) then exit;
      // if selection exists then delete it and forbid further actions
      selchanged := SelectionExists;
      if selchanged then begin
        if IsCutShortcut(Shift, Key) then
          SendMessage(Handle, WM_CUT, 0, 0)
        else begin
          TRVEditRVData(RVData).DeleteSelection_;
          Key := 0;
          Refresh;
        end;
        Exit;
      end;
    end;
  else
    begin
      if IsCopyShortcut(Shift,Key) then begin
        //SendMessage(Handle, WM_COPY, 0, 0); handled by RichView
        Key := 0;
        exit;
      end;
      if IsCutShortcut(Shift, Key) then begin
        SendMessage(Handle, WM_CUT, 0, 0);
        exit;
      end;
      if IsPasteShortcut(Shift, Key) then begin
        SendMessage(Handle, WM_PASTE, 0, 0);
        exit;
      end;
      if not (ssAlt in Shift) and not (ssCtrl in Shift) and (Key<>VK_INSERT) then begin
        // Just delete selection, if it exists
        selchanged := SelectionExists;
        if selchanged then begin
          if not BeforeChange(False) then exit;
          TRVEditRVData(RVData).DeleteSelection_;
        end;
      end;
    end;
  end;
  // Step 2 of 3. Processing the key
  case Key of
    VK_RETURN:
      begin
        if not ((ssShift in Shift) and (rvoDoNotWantShiftReturns in EditorOptions)) then
          OnEnterPress(ssShift in Shift);
        Key := 0;
        exit;
      end;
    VK_BACK:
      begin
        OnBackSpacePress(ssCtrl in Shift);
        Key := 0;
        exit;
      end;
    VK_DELETE:
      begin
        OnDeletePress(ssCtrl in Shift);
        {$IFDEF RVDEBUG}{$I Debug\j.inc}{$ENDIF}
        Key := 0;
        exit;
      end;
    VK_HOME:
      if TRVEditRVData(RVData).OnHomePress(ssCtrl in Shift) then
        GetOut(rvdTop);
    VK_END:
      if TRVEditRVData(RVData).OnEndPress(ssCtrl in Shift) then
        GetOut(rvdBottom);
    VK_LEFT:
      if BiDiMode=rvbdRightToLeft then begin
        if TRVEditRVData(RVData).OnRightPress(ssShift in Shift, ssCtrl in Shift) then
          GetOut(rvdRight);
        end
      else begin
        if TRVEditRVData(RVData).OnLeftPress(ssShift in Shift, ssCtrl in Shift) then
          GetOut(rvdLeft);
      end;
    VK_RIGHT:
      if BiDiMode=rvbdRightToLeft then begin
        if TRVEditRVData(RVData).OnLeftPress(ssShift in Shift, ssCtrl in Shift) then
          GetOut(rvdLeft);
        end
      else begin
        if TRVEditRVData(RVData).OnRightPress(ssShift in Shift, ssCtrl in Shift) then
          GetOut(rvdRight);
      end;
    VK_UP:
      if TRVEditRVData(RVData).OnUpPress(ssShift in Shift, ssCtrl in Shift) then
        GetOut(rvdUp);
    VK_DOWN:
      if TRVEditRVData(RVData).OnDownPress(ssShift in Shift, ssCtrl in Shift) then
        GetOut(rvdDown);      
    VK_PRIOR:
      TRVEditRVData(RVData).OnPgUpPress;
    VK_NEXT:
      TRVEditRVData(RVData).OnPgDownPress;
    VK_TAB:
      begin
        if ssCtrl in Shift then
          PostMessage(Handle, WM_CHAR, VK_TAB, 0)
      end;
    else
      begin
        if selchanged then
          Invalidate;
        exit;
      end;
  end;
  // Step 3 of 3: Processing selection (if key is arrow-moving key)
  if ssShift in Shift then
    TRVEditRVData(RVData).EndShiftMoving;
  if selchanged then begin
    RVData.DoOnSelection(True);
    Invalidate;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.ApplyParaStyle(ParaStyleNo: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).ApplyParaStyle(ParaStyleNo);
    exit;
  end;
  {$ENDIF}
  if not BeforeChange(False) then exit;
  TRVEditRVData(RVData).BeginUndoSequence(rvutPara, True);
  TRVEditRVData(RVData).ApplyParaStyle(ParaStyleNo,False);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.ApplyTextStyleConversionProc(
  Sender: TCustomRichViewEdit; StyleNo, UserData: Integer;
  AppliedToText: Boolean; var NewStyleNo: Integer);
begin
  if not (AppliedToText and (rvprStyleProtect in Style.TextStyles[StyleNo].Protection)) then
    NewStyleNo := UserData;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.ApplyStyleConversion(UserData: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).ApplyStyleConversion(UserData);
    exit;
  end;
  {$ENDIF}
  if Assigned(FOnStyleConversion) and BeforeChange(False) then begin
    FCurStyleConversion := FOnStyleConversion;
    TRVEditRVData(RVData).ApplyStyleConversion_(UserData);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.ApplyParaStyleConversion(UserData: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).ApplyParaStyleConversion(UserData);
    exit;
  end;
  {$ENDIF}
  if Assigned(FOnParaStyleConversion) and BeforeChange(False) then begin
    TRVEditRVData(RVData).BeginUndoSequence(rvutPara, True);
    FCurStyleConversion := FOnParaStyleConversion;
    TRVEditRVData(RVData).ApplyParaStyle(UserData,True);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.ApplyTextStyle(TextStyleNo: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).ApplyTextStyle(TextStyleNo);
    exit;
  end;
  {$ENDIF}
  if BeforeChange(False) then begin
    FCurStyleConversion := ApplyTextStyleConversionProc;
    TRVEditRVData(RVData).ApplyStyleConversion_(TextStyleNo);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.OnBackSpacePress(Ctrl: Boolean);
begin
  if not BeforeChange(False) then exit;
  TRVEditRVData(RVData).OnBackSpacePress_(Ctrl, False, False);
  Change;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.OnDeletePress(Ctrl: Boolean);
begin
  if not BeforeChange(False) then exit;
  TRVEditRVData(RVData).OnDeletePress_(Ctrl, False);
  Change;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.OnEnterPress(Shift: Boolean);
begin
  if not BeforeChange(False) then exit;
  if TRVEditRVData(RVData).OnEnterPress_(Shift, False) then begin
    Refresh;
    Change;
  end;
end;

{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertRVFFromStreamEd(Stream: TStream):Boolean;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).InsertRVFFromStreamEd(Stream);
    exit;
  end;
  {$ENDIF}
  Result := True;
  if not BeforeChange(False) then exit;
  if (RVData.PartialSelectedItem<>nil)or not CanDelete then begin
    TRVEditRVData(RVData).Beep;
    exit;
  end;
  TRVEditRVData(RVData).BeginUndoSequence(rvutInsert, True);
  SetUndoGroupMode(True);
  try
    TRVEditRVData(RVData).DeleteSelection_;
    Result := TRVEditRVData(RVData).InsertRVFFromStreamEd_(Stream);
  finally
    SetUndoGroupMode(False);
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertRVFFromFileEd(const FileName: String):Boolean;
var Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      Result := InsertRVFFromStreamEd(Stream);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.WMChar(var Message: TWMChar);
{$IFNDEF RVDONOTUSEUNICODE}
var s: String;
{$ENDIF}
begin
  {$IFNDEF RVDONOTUSEUNICODE}
  if rvstIgnoreNextChar in RVData.State then
    exit;
  if Message.CharCode>$FF then begin
    BeforeUnicode;
    SetLength(s,2);
    s[1] := chr(Message.CharCode and $00FF);
    s[2] := chr((Message.CharCode and $FF00) shr 8);
    s := RVU_AnsiToUnicode(RVU_GetKeyboardCodePage, s);
    if Length(s)>0 then
      if (Length(s)=2) and Style.TextStyles[CurTextStyleNo].Unicode then
        TRVEditRVData(RVData).InsertTextTyping(s)
      else
        InsertTextW_(s);
    exit;
  end;
  {$ENDIF}
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.KeyPress(var Key: Char);
var s: String;
    i: Integer;
begin
  if rvstIgnoreNextChar in RVData.State then
    exit;
  if (ord(Key)=VK_RETURN) and (rvoDoNotWantReturns in EditorOptions) then begin
    Key := #0;
    TRVEditRVData(RVData).Beep;
    exit;
  end;

  if ((Key < #32) or (Key=#127)) and (ord(Key)<>VK_TAB) then exit;

  if not BeforeChange(False) then exit;

  inherited KeyPress(Key);

  if Key=#0 then
    exit;

  DeleteSelection;

  if ord(Key)=VK_TAB then begin
    SetLength(s, Style.SpacesInTab);
    for i := 1 to Style.SpacesInTab do
      s[i] := ' ';
    InsertText(s, False);
    exit;
  end;

  TRVEditRVData(RVData).KeyPress(Key);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetCheckpointInfoEd(ItemNo: Integer; ATag: Integer; const AName: String;
                              ARaiseEvent: Boolean);
var cp: TRVCPInfo;
begin
  if not BeforeChange(False) then exit;
  cp := TCustomRVItemInfo(RVData.Items.Objects[ItemNo]).Checkpoint;
  if cp<>nil then begin
    TRVEditRVData(RVData).BeginUndoSequence(rvutModifyCheckpoint, True);
    if ATag=cp.Tag then
      ATag := RV_CopyTag(ATag, rvoTagsArePChars in Options);
    TRVEditRVData(RVData).Do_DeleteCP(ItemNo);
    end
  else
    TRVEditRVData(RVData).BeginUndoSequence(rvutAddCheckpoint, True);
  cp := TRVCPInfo.Create;
  cp.Name       := AName;
  cp.Tag        := ATag;
  cp.RaiseEvent := ARaiseEvent;
  TRVEditRVData(RVData).Do_AddCP(ItemNo, cp);
  Change;
  Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.RemoveCheckpointEd(ItemNo: Integer);
begin
  if TCustomRVItemInfo(RVData.Items.Objects[ItemNo]).Checkpoint=nil then exit;
  if not BeforeChange(False) then exit;
  TRVEditRVData(RVData).BeginUndoSequence(rvutRemoveCheckpoint, True);
  TRVEditRVData(RVData).Do_DeleteCP(ItemNo);
  Change;
  Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetCurrentCheckpointInfo(ATag: Integer; const AName: String;
                                                 ARaiseEvent: Boolean);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SetCurrentCheckpointInfo(ATag, AName, ARaiseEvent);
    exit;
  end;
  {$ENDIF}
  SetCheckpointInfoEd(CurItemNo, ATag, AName, ARaiseEvent);
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetCurrentCheckpoint: TCheckpointData;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).GetCurrentCheckpoint;
    exit;
  end;
  {$ENDIF}
  Result := GetItemCheckpoint(CurItemNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.RemoveCurrentCheckpoint;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).RemoveCurrentCheckpoint;
    exit;
  end;
  {$ENDIF}
  RemoveCheckpointEd(CurItemNo);
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.BeforeChange(FromOutside: Boolean): Boolean;
begin
  Result := (not ReadOnly) and (Style<>nil) and
            (FromOutside or (RVData.CaptureMouseItem=nil)) and
            DoChanging;
  if Result then begin
    TRVEditRVData(RVData).PrepareForEdit;
    ClearSoftPageBreaks;
    end
  else
    TRVEditRVData(RVData).Beep;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.DoChange(ClearRedo: Boolean);
begin
  TRVEditRVData(RVData).CreateResizer;
  Modified := True;
  ClearSoftPageBreaks;
  TRVEditRVData(RVData).ClearJumpsCoords;
  if ClearRedo then
    TRVEditRVData(RVData).RedoList.Clear;
  if (LockCount<=0) and Assigned(FOnChange) then FOnChange(Self);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.Change;
begin
  DoChange(True);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.BeginUpdate;
begin
  inc(LockCount);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.EndUpdate;
begin
  dec(LockCount);
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.CanPasteRVF: Boolean;
begin
  Result := Clipboard.HasFormat(CFRV_RVF);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERTFIMPORT}
function TCustomRichViewEdit.CanPasteRTF: Boolean;
begin
  Result := Clipboard.HasFormat(CFRV_RTF);
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.CanPaste: Boolean;
begin
  Result := (Clipboard.HasFormat(CF_TEXT) or
          {$IFNDEF RVDONOTUSEUNICODE}
          Clipboard.HasFormat(CF_UNICODETEXT) or
          {$ENDIF}
          Clipboard.HasFormat(CF_BITMAP) or
          Clipboard.HasFormat(CF_METAFILEPICT) or
          {$IFNDEF RVDONOTUSERTFIMPORT}
          Clipboard.HasFormat(CFRV_RTF) or
          {$ENDIF}
          Clipboard.HasFormat(CFRV_RVF));
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.Paste;
var DoDefault: Boolean;
begin
  if not BeforeChange(False) then exit;
  try
    DoDefault := True;
    if Assigned(FOnPaste) then
      FOnPaste(GetRootEditor, DoDefault);
    if not DoDefault then exit;
    if Clipboard.HasFormat(CFRV_RVF) then
      PasteRVF
    {$IFNDEF RVDONOTUSERTFIMPORT}
    else if Clipboard.HasFormat(CFRV_RTF) then
      PasteRTF
    {$ENDIF}
    {$IFNDEF RVDONOTUSEUNICODE}
    else if Clipboard.HasFormat(CF_UNICODETEXT) then begin
      if not Style.TextStyles [CurTextStyleNo].Unicode and
         Clipboard.HasFormat(CF_TEXT) then
        PasteText
      else
        PasteTextW;
      end
    {$ENDIF}
    else if Clipboard.HasFormat(CF_TEXT) then
      PasteText
    else if Clipboard.HasFormat(CF_BITMAP) then
      PasteBitmap(False)
    else if Clipboard.HasFormat(CF_METAFILEPICT) then
      PasteMetafile(False)
  except
    TRVEditRVData(RVData).Beep;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.CutDef;
begin
  if not BeforeChange(False) then exit;
  {$IFDEF RVONCUT}
  if Assigned(FOnCut) then
    FOnCut(GetRootEditor);
  {$ENDIF}
  if CopyDef then DeleteSelection;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.WMCut(var Message: TWMCut);
begin
  CutDef;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.WMPaste(var Message: TWMpaste);
begin
  Paste;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.EMCanPaste(var Message: TMessage);
begin
  Message.Result := 0;
  if ReadOnly then
    exit;
  case Message.WParam of
    0:
      Message.Result := Integer(CanPaste);
    CF_BITMAP, CF_TEXT, CF_UNICODETEXT, CF_METAFILEPICT:
      Message.Result := Integer(Clipboard.HasFormat(Message.WParam));
    else
      {$IFNDEF RVDONOTUSERTFIMPORT}
      if UINT(Message.WParam)=CFRV_RTF then
        Message.Result := Integer(CanPasteRTF)
      else
      {$ENDIF}
      if UINT(Message.WParam)=CFRV_RVF then
        Message.Result := Integer(CanPasteRVF)
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.PasteBitmap(TextAsName: Boolean);
var bmp: TBitmap;
    s: String;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).PasteBitmap(TextAsName);
    exit;
  end;
  {$ENDIF}
  if not BeforeChange(False) then exit;
  if not Clipboard.HasFormat(CF_BITMAP) then exit;
  bmp := TBitmap.Create;
  bmp.Assign(Clipboard);
  if TextAsName and Clipboard.HasFormat(CF_TEXT) then
    s := Clipboard.AsText
  else
    s := '';
  InsertPicture(s,bmp,rvvaBaseline);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.PasteMetafile(TextAsName: Boolean);
var wmf: TMetafile;
    s: String;
    w,h: Integer;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).PasteMetafile(TextAsName);
    exit;
  end;
  {$ENDIF}
  if not BeforeChange(False) then exit;
  if not Clipboard.HasFormat(CF_METAFILEPICT) then exit;
  wmf := TMetafile.Create;
  wmf.Assign(Clipboard);
  {$IFNDEF RVDONOTCORRECTWMFSCALE}
  if wmf.Inch=0 then
    wmf.Inch := 1440;
  {$ENDIF}
  w := wmf.Width;
  h := wmf.Height;
  if TextAsName and Clipboard.HasFormat(CF_TEXT) then
    s := Clipboard.AsText
  else
    s := '';
  InsertPicture(s,wmf,rvvaBaseline);
  if (GetCurrentItem is TRVGraphicItemInfo) and
     (TRVGraphicItemInfo(GetCurrentItem).Image = wmf) then begin
    GetCurrentItem.SetExtraIntProperty(rvepImageWidth, w);
    GetCurrentItem.SetExtraIntProperty(rvepImageHeight, h);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.PasteRVF;
var Stream: TMemoryStream;
    {$IFNDEF RICHVIEWCBDEF3}
    Stream2: TMemoryStream;
    {$ENDIF}
    mem: Cardinal;
    ptr: Pointer;
    Size: Integer;
begin
  if not Clipboard.HasFormat(CFRV_RVF) then exit;
  Clipboard.Open;
  Stream := TMemoryStream.Create;
  try
    mem := Clipboard.GetAsHandle(CFRV_RVF);
    Size := GlobalSize(mem);
    Stream.SetSize(Size);
    ptr := GlobalLock(mem);
    Move(ptr^,Stream.Memory^,Size);
    GlobalUnlock(mem);
    Stream.Position := 0;
    Stream.ReadBuffer(Size, SizeOf(Size));
    {$IFDEF RICHVIEWCBDEF3}
    Stream.SetSize(SizeOf(Size)+Size);
    {$ELSE}
    // Delphi2 and Builder1 clear memory streams inside SetSize
    if (Stream.Size<>SizeOf(Size)+Size) then begin
      Stream2 := TMemoryStream.Create;
      try
        Stream.Position := 0;
        Stream2.CopyFrom(Stream,Stream.Size);
        Stream.SetSize(SizeOf(Size)+Size);
        Stream2.Position := 0;
        Stream2.ReadBuffer(Stream.Memory^,SizeOf(Size)+Size);
      finally
        Stream2.Free;
      end;
    end;
    {$ENDIF}
    Stream.Position := SizeOf(Size);
    InsertRVFFromStreamEd(Stream);
  finally
    Stream.Free;
    Clipboard.Close;
  end;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERTFIMPORT}

function TCustomRichViewEdit.GetBasePathFromHTMLInClipboard: String;
var
  mem: Cardinal;
  ptr: Pointer;
  HTML: String;
  http: Boolean;
  p: Integer;
begin
  Result := '';
  if not Clipboard.HasFormat(CFRV_HTML) then exit;
  Clipboard.Open;
  try
    mem := Clipboard.GetAsHandle(CFRV_HTML);
    SetLength(HTML, GlobalSize(mem));
    ptr := GlobalLock(mem);
    Move(ptr^,PChar(HTML)^, Length(HTML));
    GlobalUnlock(mem);
  finally
    Clipboard.Close;
  end;
  p := Pos(HTMLClipboardSourceURL, HTML);
  if p=0 then
    exit;
  inc(p, Length(HTMLClipboardSourceURL));
  while not (HTML[p] in [#0,#10,#13]) do begin
    Result := Result + HTML[p];
    inc(p);
  end;
  {$IFDEF RICHVIEWCBDEF3}
  Result := UTF8Decode(Result);
  {$ENDIF}
  if Pos('file://', LowerCase(Result))=1 then
    Result := System.Copy(Result, 8, Length(Result));
  http := Pos('http://', LowerCase(Result))=1;
  if http then
    Result := System.Copy(Result, 8, Length(Result));
  for p := 1 to Length(Result) do
    if Result[p]='/' then
      Result[p]:='\' ;
  Result := ExtractFilePath(Result);
  if http then begin
    Result := 'http://'+Result;
    for p := 1 to Length(Result) do
      if Result[p]='\' then
        Result[p]:='/' ;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.PasteRTF;
var Stream: TMemoryStream;
    mem: Cardinal;
    ptr: Pointer;
    Size: Integer;
begin
  if not Clipboard.HasFormat(CFRV_RTF) then exit;
  Clipboard.Open;
  Stream := TMemoryStream.Create;
  try
    mem := Clipboard.GetAsHandle(CFRV_RTF);
    Size := GlobalSize(mem);
    Stream.SetSize(Size);
    ptr := GlobalLock(mem);
    Move(ptr^,Stream.Memory^,Size);
    GlobalUnlock(mem);
    Stream.Position := 0;
    RTFReadProperties.BasePath := GetBasePathFromHTMLInClipboard;
    try
      InsertRTFFromStreamEd(Stream);
    finally
      RTFReadProperties.BasePath := '';
    end;
  finally
    Stream.Free;
    Clipboard.Close;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertRTFFromStreamEd(Stream: TStream): Boolean;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).InsertRTFFromStreamEd(Stream);
    exit;
  end;
  {$ENDIF}
  Result := True;
  if not BeforeChange(False) then exit;
  if (RVData.PartialSelectedItem<>nil)or not CanDelete then begin
    TRVEditRVData(RVData).Beep;
    exit;
  end;
  TRVEditRVData(RVData).BeginUndoSequence(rvutInsert, True);
  SetUndoGroupMode(True);
  try
    TRVEditRVData(RVData).DeleteSelection_;
    RTFReadProperties;
    Result := TRVEditRVData(RVData).InsertRTFFromStreamEd_(Stream);
  finally
    SetUndoGroupMode(False);
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.InsertRTFFromFileEd(const FileName: String): Boolean;
var Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      Result := InsertRTFFromStreamEd(Stream);
    finally
      Stream.Free;
    end;
  except
    RTFReadProperties.ErrorCode := rtf_ec_FileOpenError;
    Result := False;
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.PasteText;
begin
  if not BeforeChange(False) then exit;
  if not Clipboard.HasFormat(CF_TEXT) then exit;
  InsertText(Clipboard.AsText, False);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.GetCurrentBreakInfo(var AWidth: Byte;
                             var AStyle: TRVBreakStyle; var AColor: TColor;
                             var ATag: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).GetCurrentBreakInfo(AWidth, AStyle, AColor, ATag);
    exit;
  end;
  {$ENDIF}
  GetBreakInfo(CurItemNo, AWidth, AStyle, AColor, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.GetCurrentBulletInfo(var AName: String;
                        var AImageIndex: Integer;
                        var AImageList: TCustomImageList;
                        var ATag: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).GetCurrentBulletInfo(AName, AImageIndex, AImageList, ATag);
    exit;
  end;
  {$ENDIF}
  GetBulletInfo(CurItemNo, AName, AImageIndex, AImageList, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.GetCurrentHotspotInfo(var AName: String;
                   var AImageIndex, AHotImageIndex: Integer;
                   var AImageList: TCustomImageList;
                   var ATag: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).GetCurrentHotspotInfo(AName, AImageIndex, AHotImageIndex, AImageList, ATag);
    exit;
  end;
  {$ENDIF}
  GetHotspotInfo(CurItemNo, AName, AImageIndex, AHotImageIndex, AImageList, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.GetCurrentPictureInfo(var AName: String;
                 var Agr: TGraphic; var AVAlign: TRVVAlign; var ATag: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).GetCurrentPictureInfo(AName, Agr, AVAlign, ATag);
    exit;
  end;
  {$ENDIF}
  GetPictureInfo(CurItemNo, AName, Agr, AVAlign, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.GetCurrentControlInfo(var AName: String;
                var Actrl: TControl; var AVAlign: TRVVAlign; var ATag: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).GetCurrentControlInfo(AName, Actrl, AVAlign, ATag);
    exit;
  end;
  {$ENDIF}
  GetControlInfo(CurItemNo, AName, Actrl, AVAlign, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.GetCurrentTextInfo(var AText: String; var ATag: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).GetCurrentTextInfo(AText, ATag);
    exit;
  end;
  {$ENDIF}
  GetTextInfo(CurItemNo, AText, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.CheckItemClass(ItemNo: Integer; RequiredClass: TCustomRVItemInfoClass);
begin
  if not (RVData.Items.Objects[ItemNo] is RequiredClass) then
    raise ERichViewError.Create(errRVTypesMismatch);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetBreakInfoEd(ItemNo: Integer; AWidth: Byte;
                             AStyle: TRVBreakStyle; AColor: TColor;
                             ATag: Integer);
var item: TRVBreakItemInfo;
begin
  if not BeforeChange(False) then exit;
  CheckItemClass(ItemNo, TRVBreakItemInfo);
  TRVEditRVData(RVData).BeginUndoSequence(rvutModifyItem, True);
  item := TRVBreakItemInfo.CreateEx(RVData, AWidth, AStyle, AColor);
  item.Tag := ATag;
  TRVEditRVData(RVData).Do_ModifyItem(ItemNo,'',item);
  Invalidate;
  Change;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetBulletInfoEd(ItemNo: Integer; const AName: String;
                                        AImageIndex: Integer;
                                        AImageList: TCustomImageList;
                                        ATag: Integer);
var item, olditem: TRVBulletItemInfo;
begin
  if not BeforeChange(False) then exit;
  CheckItemClass(ItemNo, TRVBulletItemInfo);
  TRVEditRVData(RVData).BeginUndoSequence(rvutModifyItem, True);
  olditem := TRVBulletItemInfo(RVData.Items.Objects[ItemNo]);
  item := TRVBulletItemInfo.CreateEx(RVData, AImageIndex, olditem.ImageList,
                                     TRVBulletItemInfo(RVData.Items.Objects[ItemNo]).VAlign);
  item.Tag := ATag;
  TRVEditRVData(RVData).Do_ModifyItem(ItemNo, AName, item);
  Invalidate;
  Change;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetHotspotInfoEd(ItemNo: Integer; const AName: String;
                                        AImageIndex, AHotImageIndex: Integer;
                                        AImageList: TCustomImageList;
                                        ATag: Integer);
var item, olditem: TRVHotspotItemInfo;
begin
  if not BeforeChange(False) then exit;
  CheckItemClass(ItemNo, TRVHotspotItemInfo);
  TRVEditRVData(RVData).BeginUndoSequence(rvutModifyItem, True);
  olditem := TRVHotspotItemInfo(RVData.Items.Objects[ItemNo]);
  item := TRVHotspotItemInfo.CreateEx(RVData, AImageIndex, AHotImageIndex, olditem.ImageList,
                                     TRVHotspotItemInfo(RVData.Items.Objects[ItemNo]).VAlign);
  item.Tag := ATag;
  TRVEditRVData(RVData).Do_ModifyItem(ItemNo, AName, item);
  Invalidate;
  Change;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetPictureInfoEd(ItemNo: Integer; const AName: String; Agr: TGraphic; AVAlign: TRVVAlign; ATag: Integer);
var item, olditem: TRVGraphicItemInfo;
    NeedFormat: Boolean;
    OldWidth: Integer;
begin
  if not BeforeChange(False) then exit;
  CheckItemClass(ItemNo, TRVGraphicItemInfo);
  TRVEditRVData(RVData).BeginUndoSequence(rvutModifyItem, True);
  olditem := TRVGraphicItemInfo(RVData.Items.Objects[ItemNo]);
  BeginItemModify(ItemNo, OldWidth);
  if olditem.Image = Agr then begin
    Agr := RV_CreateGraphics(TGraphicClass(olditem.Image.ClassType));
    Agr.Assign(olditem.Image);
  end;
  NeedFormat := (Agr.Width<>olditem.Image.Width) or
                (Agr.Height<>olditem.Image.Height) or
                (AValign<>olditem.VAlign);
  item := TRVGraphicItemInfoClass(olditem.ClassType).CreateEx(RVData, nil, AVAlign);
  item.Assign(olditem);
  item.Image.Free;
  item.Image := Agr;
  item.VAlign := AVAlign;
  item.ItemOptions := [];
  item.ParaNo := olditem.ParaNo;
  item.Tag := ATag;
  TRVEditRVData(RVData).Do_ModifyItem(ItemNo, AName, item);
  if NeedFormat then
    EndItemModify(ItemNo, OldWidth);
  Invalidate;
  Change;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.AdjustControlPlacement(ItemNo: Integer);
begin
  TRVEditRVData(RVData).AdjustControlPlacement(ItemNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.AdjustControlPlacement2(Control: TControl);
var ItemNo: Integer;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil)  and
     (InplaceEditor is TCustomRichViewEdit) then
    TCustomRichViewEdit(InplaceEditor).AdjustControlPlacement2(Control);
  {$ENDIF}
  ItemNo := FindControlItemNo(Control);
  if ItemNo=-1 then
    exit;
  if GetItem(ItemNo) is TRVControlItemInfo then
    TRVEditRVData(RVData).AdjustControlPlacement(ItemNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.ResizeControl(ItemNo, NewWidth, NewHeight: Integer);
begin
  TRVEditRVData(RVData).BeginUndoSequence(rvutModifyItem, True);
  TRVEditRVData(RVData).Do_Resize(ItemNo, NewWidth, NewHeight, True);
  Change;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.ResizeCurrentControl(NewWidth, NewHeight: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).ResizeCurrentControl(NewWidth, NewHeight);
    exit;
  end;
  {$ENDIF}
  ResizeControl(CurItemNo, NewWidth, NewHeight);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetControlInfoEd(ItemNo: Integer; const AName: String;
                                         AVAlign: TRVVAlign; ATag: Integer);
var item: TRVControlItemInfo;
    Ref: Boolean;
begin
  if not BeforeChange(False) then exit;
  CheckItemClass(ItemNo, TRVControlItemInfo);
  item := TRVControlItemInfo(RVData.Items.Objects[ItemNo]);
  Ref := item.VAlign<>AVAlign;
  TRVEditRVData(RVData).BeginUndoSequence(rvutModifyItem, True);
  SetUndoGroupMode(True);
  TRVEditRVData(RVData).Do_ChangeText(ItemNo, AName);
  TRVEditRVData(RVData).Do_ChangeVAlign(ItemNo, AVAlign);
  SetItemTagEd(ItemNo, ATag);
  SetUndoGroupMode(False);
  if Ref then
    TRVEditRVData(RVData).Reformat(False,False,False,ItemNo,True);
  Change;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetItemTextEd(ItemNo: Integer; const s: String);
var Data, StartNo, EndNo, StartOffs, EndOffs: Integer;
begin
  if not BeforeChange(False) then exit;
  TRVEditRVData(RVData).BeginUndoSequence(rvutModifyItem, True);
  RVData.StoreSelBounds(StartNo, EndNo, StartOffs, EndOffs, False);
  BeginItemModify(ItemNo, Data);
  TRVEditRVData(RVData).Do_ChangeText(ItemNo, s);
  EndItemModify(ItemNo, Data);
  RVData.RestoreSelBounds(StartNo, EndNo, StartOffs, EndOffs);
  Change;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetCurrentBreakInfo(AWidth: Byte;
                             AStyle: TRVBreakStyle; AColor: TColor;
                             ATag: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SetCurrentBreakInfo(AWidth, AStyle, AColor, ATag);
    exit;
  end;
  {$ENDIF}
  SetBreakInfoEd(CurItemNo, AWidth, AStyle, AColor, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetCurrentBulletInfo(const AName: String;
                             AImageIndex: Integer;
                             AImageList: TCustomImageList;
                             ATag: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SetCurrentBulletInfo(AName, AImageIndex, AImageList, ATag);
    exit;
  end;
  {$ENDIF}
  SetBulletInfoEd(CurItemNo, AName, AImageIndex, AImageList, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetCurrentHotspotInfo(const AName: String;
                             AImageIndex, AHotImageIndex: Integer;
                             AImageList: TCustomImageList;
                             ATag: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SetCurrentHotspotInfo(AName, AImageIndex, AHotImageIndex,
                             AImageList, ATag);
    exit;
  end;
  {$ENDIF}
  SetHotspotInfoEd(CurItemNo, AName, AImageIndex, AHotImageIndex, AImageList, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetCurrentPictureInfo(const AName: String; Agr: TGraphic;
                             AVAlign: TRVVAlign; ATag: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SetCurrentPictureInfo(AName, Agr, AVAlign, ATag);
    exit;
  end;
  {$ENDIF}
  SetPictureInfoEd(CurItemNo, AName, Agr, AVAlign, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetCurrentControlInfo(const AName: String;
                             AVAlign: TRVVAlign; ATag: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SetCurrentControlInfo(AName, AVAlign, ATag);
    exit;
  end;
  {$ENDIF}
  SetControlInfoEd(CurItemNo, AName, AVAlign, ATag);
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetCurrentTag: Integer;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).GetCurrentTag;
    exit;
  end;
  {$ENDIF}
  TRVEditRVData(RVData).PrepareForEdit;
  Result := GetItemTag(CurItemNo);
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetCurrentItemText: String;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).GetCurrentItemText;
    exit;
  end;
  {$ENDIF}
  TRVEditRVData(RVData).PrepareForEdit;
  Result := GetItemText(CurItemNo);
end;
{------------------------------------------------------------------------------}
{ Sets value of item property of integer type.
  ItemNo - index of item. Prop identifies the property. Value - new property
  value.
  If AutoReformat is true, document will be reformatted automatically, and
  OnChange event will occur.
  This is an editing-style method.                                             }
procedure TCustomRichViewEdit.SetItemExtraIntPropertyEd(ItemNo: Integer;
  Prop: TRVExtraItemProperty; Value: Integer; AutoReformat: Boolean);
var OldValue, Data: Integer;
begin
  if not GetItemExtraIntProperty(ItemNo, Prop, OldValue) or (Value=OldValue) then
    exit;
  if not BeforeChange(False) then
    exit;
  TRVEditRVData(RVData).BeginUndoSequence(rvutModifyItem, True);
  if AutoReformat then
    BeginItemModify(ItemNo, Data);
  TRVEditRVData(RVData).Do_ExtraIntProperty(ItemNo, Prop, Value);
  if AutoReformat then begin
    EndItemModify(ItemNo, Data);
    Change;
  end;
end;
{------------------------------------------------------------------------------}
{ The same, but sets value of item property of integer type at the position caret -
  in this RVData or in inplace editor.                                         }
procedure TCustomRichViewEdit.SetCurrentItemExtraIntProperty(
  Prop: TRVExtraItemProperty;
  Value: Integer; AutoReformat: Boolean);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SetCurrentItemExtraIntProperty(Prop, Value, AutoReformat);
    exit;
  end;
  {$ENDIF}
  SetItemExtraIntPropertyEd(CurItemNo, Prop, Value, AutoReformat);
end;
{------------------------------------------------------------------------------}
{ Gets value of item property of integer type at the position caret -
  in this RVData or in inplace editor.
  Prop identifies the property. Value receives a property value.
  Returns True if this item type has this property.                            }
function TCustomRichViewEdit.GetCurrentItemExtraIntProperty(
  Prop: TRVExtraItemProperty; var Value: Integer): Boolean;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).GetCurrentItemExtraIntProperty(Prop, Value);
    exit;
  end;
  {$ENDIF}
  Result := GetItemExtraIntProperty(CurItemNo, Prop, Value);
end;
{------------------------------------------------------------------------------}
{ Sets value of item property of string type.
  ItemNo - index of item. Prop identifies the property. Value - new property
  value.
  If AutoReformat is true, document will be reformatted automatically, and
  OnChange event will occur.
  This is an editing-style method.                                             }
procedure TCustomRichViewEdit.SetItemExtraStrPropertyEd(ItemNo: Integer;
  Prop: TRVExtraItemStrProperty; const Value: String;
  AutoReformat: Boolean);
var OldValue: String;
//    Data: Integer;
begin
  if not GetItemExtraStrProperty(ItemNo, Prop, OldValue) or (Value=OldValue) then
    exit;
  if not BeforeChange(False) then
    exit;
  TRVEditRVData(RVData).BeginUndoSequence(rvutModifyItem, True);
  //if AutoReformat then
  //  BeginItemModify(ItemNo, Data);
  TRVEditRVData(RVData).Do_ExtraStrProperty(ItemNo, Prop, Value);
  if AutoReformat then begin
    // EndItemModify(ItemNo, Data);
    Change;
  end;
end;
{------------------------------------------------------------------------------}
{ The same, but sets value of item property of string type at the position caret -
  in this RVData or in inplace editor.                                         }
procedure TCustomRichViewEdit.SetCurrentItemExtraStrProperty(
  Prop: TRVExtraItemStrProperty; const Value: String; AutoReformat: Boolean);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SetCurrentItemExtraStrProperty(Prop, Value, AutoReformat);
    exit;
  end;
  {$ENDIF}
  SetItemExtraStrPropertyEd(CurItemNo, Prop, Value, AutoReformat);
end;
{------------------------------------------------------------------------------}
{ Gets value of item property of string type at the position caret -
  in this RVData or in inplace editor.
  Prop identifies the property. Value receives a property value.
  Returns True if this item type has this property.                            }
function TCustomRichViewEdit.GetCurrentItemExtraStrProperty(
  Prop: TRVExtraItemStrProperty; var Value: String): Boolean;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).GetCurrentItemExtraStrProperty(Prop, Value);
    exit;
  end;
  {$ENDIF}
  Result := GetItemExtraStrProperty(CurItemNo, Prop, Value);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetItemTagEd(ItemNo: Integer; ATag: Integer);
begin
  if TCustomRVItemInfo(RVData.Items.Objects[ItemNo]).Tag=ATag then exit;
  if not BeforeChange(False) then exit;
  TRVEditRVData(RVData).BeginUndoSequence(rvutTag, True);
  TRVEditRVData(RVData).Do_Tag(ItemNo,ATag,True);
  Change;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetCurrentItemText(const s: String);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SetCurrentItemText(s);
    exit;
  end;
  {$ENDIF}
  SetItemTextEd(CurItemNo, s);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetCurrentTag(ATag: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SetCurrentTag(ATag);
    exit;
  end;
  {$ENDIF}
  if not BeforeChange(False) then exit;
  SetItemTagEd(CurItemNo, ATag);
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.SearchText(s: String; SrchOptions: TRVESearchOptions): Boolean;
begin
  Result := RVData.SearchText(rvseoDown in SrchOptions, rvseoMatchCase in SrchOptions,
                              rvseoWholeWord in SrchOptions, s);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.AfterVScroll;
begin
  if Assigned(FOnVScrolled) then FOnVScrolled(Self);
  TRVEditRVData(RVData).ChangeCaret(False,False,False,False);
  GenerateMouseMove;
  InplaceRedrawing(False);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.AfterHScroll;
begin
  if Assigned(FOnHScrolled) then FOnHScrolled(Self);
  TRVEditRVData(RVData).ChangeCaret(False,False,False,False);
  GenerateMouseMove;
  InplaceRedrawing(False);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SelectCurrentWord;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SelectCurrentWord;
    exit;
  end;
  {$ENDIF}
  TRVEditRVData(RVData).SelectCurrentWord;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SelectCurrentLine;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).SelectCurrentLine;
    exit;
  end;
  {$ENDIF}
  RVData.SelectLine(CurItemNo, OffsetInCurItem);
  Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.InsertPageBreak;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if InplaceEditor<>nil then begin
    Beep;
    exit;
  end;
  {$ENDIF}
  if not BeforeChange(False) then
    exit;
  TRVEditRVData(RVData).InsertPageBreak;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.RemoveCurrentPageBreak;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if InplaceEditor<>nil then begin
    Beep;
    exit;
  end;
  {$ENDIF}
  if not BeforeChange(False) then
    exit;
  TRVEditRVData(RVData).BeginUndoSequence(rvutRemovePageBreak, True);
  TRVEditRVData(RVData).Do_PageBreak(CurItemNo,False);
  Change;
  Invalidate;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.IsRedoShortcut(Shift: TShiftState;
  Key: Word): Boolean;
begin
  Result := (ssShift in Shift) and
            (((ssCtrl in Shift) and (Key=ord('Z')) and not (ssAlt in Shift)) or
             ((ssAlt in Shift) and (Key=VK_BACK)));
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.IsUndoShortcut(Shift: TShiftState;
  Key: Word): Boolean;
begin
  Result := not (ssShift in Shift) and
            (((ssCtrl in Shift) and (Key=ord('Z')) and not (ssAlt in Shift)) or
             ((ssAlt in Shift) and (Key=VK_BACK)));
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetUndoLimit: Integer;
begin
  Result := TRVEditRVData(RVData).UndoList.Limit;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetUndoLimit(const Value: Integer);
begin
  TRVEditRVData(RVData).UndoList.Limit := Value;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.BeginUndoCustomGroup(const Name: String);
begin
  TRVEditRVData(RVData).BeginNamedUndoSequence(rvutCustom, Name, True);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.BeginUndoGroup(UndoType: TRVUndoType);
begin                      
  TRVEditRVData(RVData).BeginUndoSequence(UndoType, True);
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.RedoAction: TRVUndoType;
begin
{$IFNDEF RVDONOTUSEINPLACE}
  Result:=rvutNone;
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then
    Result := TCustomRichViewEdit(InplaceEditor).RedoAction;
  if Result=rvutNone then
  {$ENDIF}
    Result := TRVEditRVData(RVData).RedoList.CurrentUndoType;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.UndoAction: TRVUndoType;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  Result:=rvutNone;
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then
    Result := TCustomRichViewEdit(InplaceEditor).UndoAction;
  if Result=rvutNone then
  {$ENDIF}
    Result := TRVEditRVData(RVData).UndoList.CurrentUndoType;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.RedoName: String;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) and
     (TCustomRichViewEdit(InplaceEditor).RedoAction<>rvutNone) then begin
    Result := TCustomRichViewEdit(InplaceEditor).RedoName;
    exit;
  end;
  {$ENDIF}
  Result := TRVEditRVData(RVData).RedoList.CurrentUndoCaption;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.UndoName: String;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) and
     (TCustomRichViewEdit(InplaceEditor).UndoAction<>rvutNone) then begin
    Result := TCustomRichViewEdit(InplaceEditor).UndoName;
    exit;
  end;
  {$ENDIF}
  Result := TRVEditRVData(RVData).UndoList.CurrentUndoCaption;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.WMRedoFromInplace(var Message: TMessage);
begin
  DestroyInplace;
  Redo;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.WMUndoFromInplace(var Message: TMessage);
begin
  DestroyInplace;
  Undo;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.Redo;
begin
  if ReadOnly then begin
    Beep;
    exit;
  end;
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    if (TCustomRichViewEdit(InplaceEditor).RedoAction<>rvutNone) then begin
      TCustomRichViewEdit(InplaceEditor).Redo;
      exit;
    end;
    if RedoAction<>rvutNone then
      PostMessage(Handle, WM_RVREDOFROMINPLACE,0,0);
    exit;
  end;
  {$ENDIF}
  if RedoAction<>rvutNone then begin
    TRVEditRVData(RVData).RedoList.Redo(RVData);
    DoChange(False);
    end
  else
    RVData.Beep;
  if Visible then
    SetFocusSilent;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.Undo;
begin
  if ReadOnly then begin
    Beep;
    exit;
  end;
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    if (TCustomRichViewEdit(InplaceEditor).UndoAction<>rvutNone) then begin
      TCustomRichViewEdit(InplaceEditor).Undo;
      exit;
    end;
    if UndoAction<>rvutNone then
      PostMessage(Handle, WM_RVUNDOFROMINPLACE,0,0);
    exit;
  end;
  {$ENDIF}
  if UndoAction<>rvutNone then begin
    TRVEditRVData(RVData).UndoList.Undo(RVData);
    DoChange(False);
    end
  else
    RVData.Beep;
  if Visible then
    SetFocusSilent;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetUndoGroupMode(GroupUndo: Boolean);
begin
  TRVEditRVData(RVData).SetUndoGroupMode(GroupUndo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.ClearUndo;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then
    TCustomRichViewEdit(InplaceEditor).ClearUndo;
  {$ENDIF}
  with TRVEditRVData(RVData) do begin
    UndoList.Clear;
    RedoList.Clear;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.BeginItemModify(ItemNo: Integer; var ModifyData: Integer);
begin
  TRVEditRVData(RVData).BeginItemModify(ItemNo, ModifyData);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.EndItemModify(ItemNo: Integer; ModifyData: Integer);
begin
  TRVEditRVData(RVData).EndItemModify(ItemNo, ModifyData);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.BeginCurrentItemModify(var ModifyData: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).BeginCurrentItemModify(ModifyData);
    exit;
  end;
  {$ENDIF}
  BeginItemModify(CurItemNo, ModifyData);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.EndCurrentItemModify(ModifyData: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).EndCurrentItemModify(ModifyData);
    exit;
  end;
  {$ENDIF}
  EndItemModify(CurItemNo, ModifyData);
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetCurrentItem: TCustomRVItemInfo;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    Result := TCustomRichViewEdit(InplaceEditor).GetCurrentItem;
    exit;
  end;
  {$ENDIF}
  Result := GetItem(CurItemNo);
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetCurrentItemEx(RequiredClass: TCustomRVItemInfoClass;
                               var ItemRichViewEdit: TCustomRichViewEdit;
                               var Item: TCustomRVItemInfo): Boolean;
var Item2: TCustomRVItemInfo;
     ItemRichViewEdit2: TCustomRichViewEdit;
begin
   if GetItem(CurItemNo) is RequiredClass then begin
     Item := GetItem(CurItemNo);
     ItemRichViewEdit := Self;
     end
   else begin
     Item := nil;
     ItemRichViewEdit := nil;
   end;
   if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) and
      (TCustomRichViewEdit(InplaceEditor).CurItemNo >= 0) and
      TCustomRichViewEdit(InplaceEditor).GetCurrentItemEx(RequiredClass,
                      ItemRichViewEdit2, Item2) then begin
       Item := Item2;
       ItemRichViewEdit := ItemRichViewEdit2;
     end;
   Result := Item<>nil;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.CanChange: Boolean;
begin
  Result := BeforeChange(True);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetTabNavigation(const Value: TRVTabNavigationType);
begin
  if Value<>rvtnNone then
    raise ERichViewError.Create(errRViewerOnly);
end;
{------------------------------------------------------------------------------}
{$IFDEF RVUSEIME}
procedure TCustomRichViewEdit.WMImeStartComposition(var Message: TMessage);
var
  IMC: HIMC;
  LogFont: TLogFont;
  CF: TCompositionForm;
begin
  if Style=nil then begin
    inherited;
    exit;
  end;
  Message.Result := 1;
  inherited;
  BeforeUnicode;
  IMC := ImmGetContext(Handle);
  if IMC <> 0 then begin
    Style.TextStyles[CurTextStyleNo].AssignToLogFont(LogFont, Canvas);
    ImmSetCompositionFont(IMC, @LogFont);
    CF.dwStyle := CFS_RECT;
    CF.rcArea  := ClientRect;
    inc(CF.rcArea.Left, LeftMargin);
    dec(CF.rcArea.Right, RightMargin);
    with Style.ParaStyles[GetCurParaStyleNo] do begin
      inc(CF.rcArea.Left, LeftIndent);
      dec(CF.rcArea.Right, RightIndent);
    end;
    CF.ptCurrentPos := TRVEditRVData(RVData).GetIMEWinCoord;
    if (GetKeyboardLayout(0) and $FFFF)= $0412 then begin// Special support for Korean IME
      CF.rcArea.TopLeft := CF.ptCurrentPos;
      OffsetRect(CF.rcArea,0,1);
    end;
    ImmSetCompositionWindow(IMC, @CF);
    ImmReleaseContext(Handle, IMC);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.WMImeComposition(var Message: TMessage);
var
  IMC: HIMC;
  s: String;
  Size: Integer;
  strend: PChar;
begin
  if BeforeChange(False) and ((Message.LParam and GCS_RESULTSTR) <> 0) then begin
    IMC := ImmGetContext(Handle);
    if IMC<>0 then begin
      if RVNT then begin
        try
          Size := ImmGetCompositionStringW(IMC, GCS_RESULTSTR, nil, 0);
          inc(Size, 2);
          SetLength(s, Size);
          FillChar(PChar(s)^, Size, 0);
          ImmGetCompositionStringW(IMC, GCS_RESULTSTR, PChar(s), Size);
        finally
          ImmReleaseContext(Handle, IMC);
        end;
        strend := RVU_StrScanW(PChar(s), 0, Size div 2);
        if strend<>nil then
          SetLength(s, strend-PChar(s));
        end
      else begin
        // IME support for Win95-98
        // Unfortunately, should properly work not for all versions
        // (you'll get a line of '?')
        try
          Size := ImmGetCompositionStringA(IMC, GCS_RESULTSTR, nil, 0);
          SetLength(s, Size);
          ImmGetCompositionStringA(IMC, GCS_RESULTSTR, PChar(s), Size);
        finally
          ImmReleaseContext(Handle, IMC);
        end;
        s := RVU_KeyToUnicode(s);
      end;
      InsertTextW_(s);
      Message.Result := 0;
    end;
    if (GetKeyboardLayout(0) and $FFFF)= $0412 then // Special support for Korean IME
      PostMessage(Handle, WM_IME_STARTCOMPOSITION,0,0);
    end
  else
    inherited;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.AssignEvents(Source: TCustomRichView);
begin
  inherited AssignEvents(Source);
  if Source is TCustomRichViewEdit then begin
    OnStyleConversion     := TCustomRichViewEdit(Source).OnStyleConversion;
    OnParaStyleConversion := TCustomRichViewEdit(Source).OnParaStyleConversion;
    OnPaste               := TCustomRichViewEdit(Source).OnPaste;
    {$IFDEF RVONCUT}
    OnCut                 := TCustomRichViewEdit(Source).OnCut;
    {$ENDIF}
    OnCurParaStyleChanged := TCustomRichViewEdit(Source).OnCurParaStyleChanged;
    OnCurTextStyleChanged := TCustomRichViewEdit(Source).OnCurTextStyleChanged;
    OnCaretMove           := TCustomRichViewEdit(Source).OnCaretMove;
    OnDropFiles           := TCustomRichViewEdit(Source).OnDropFiles;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.Selecting;
begin

end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.AfterCaretMove;
begin
  if Assigned(FOnCaretMove) and not (csDestroying in ComponentState) then
    FOnCaretMove(Self);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetFReadOnly(Value: Boolean);
begin
  FReadOnly := Value;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.SetReadOnly(const Value: Boolean);
var rve: TCustomRichViewEdit;
begin
  FReadOnly := Value;
  if FReadOnly then
    TRVEditRVData(RVData).BuildJumpsCoords(True)
  else
    TRVEditRVData(RVData).ClearJumpsCoords;
  rve := Self;
  while (rve.InplaceEditor<>nil) and (rve.InplaceEditor is TCustomRichViewEdit) do
    rve := TCustomRichViewEdit(rve.InplaceEditor);
  TRVEditRVData(rve.RVData).ChangeCaret(False, False, True, False);
  Invalidate;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSELISTS}
procedure TCustomRichViewEdit.ApplyListStyle(AListNo, AListLevel, AStartFrom: Integer;
                                       AUseStartFrom, ARecursive: Boolean);
begin
  if not BeforeChange(False) then exit;
  TRVEditRVData(GetTopLevelEditor.RVData).ApplyListStyle(AListNo, AListLevel, AStartFrom, AUseStartFrom, ARecursive, rvplopChange);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.RemoveLists(ARecursive: Boolean);
begin
  if not BeforeChange(False) then exit;
  TRVEditRVData(GetTopLevelEditor.RVData).ApplyListStyle(-1, -1, -1, False, ARecursive, rvplopRemove);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.ChangeListLevels(LevelDelta: Integer);
begin
  if not BeforeChange(False) then exit;
  TRVEditRVData(GetTopLevelEditor.RVData).ApplyListStyle(-1, LevelDelta, -1, False, False, rvplopLevel);
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.GetCurrentLineCol(var Line, Column: Integer);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).GetCurrentLineCol(Line, Column);
    exit;
  end;
  {$ENDIF}
  Line := RVData.GetLineNo(CurItemNo,OffsetInCurItem);
  Column := TRVEditRVData(RVData).CaretOffs+1;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.ConvertToHotPicture(ItemNo: Integer);
var item: TRVHotGraphicItemInfo;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (ItemNo<0) and (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).ConvertToHotPicture(ItemNo);
    exit;
  end;
  {$ENDIF}
  if ItemNo<0 then
    ItemNo := CurItemNo;
  if GetItemStyle(ItemNo)<>rvsPicture then
    exit;
  if not BeforeChange(False) then exit;
  TRVEditRVData(RVData).BeginUndoSequence(rvutModifyItem, True);
  item := TRVHotGraphicItemInfo.CreateEx(RVData, nil, rvvaBaseline);
  SetUndoGroupMode(True);
  try
    TRVEditRVData(RVData).Do_ReplaceItem(ItemNo, item);
  finally
    SetUndoGroupMode(False);
  end;
  Change;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichViewEdit.ConvertToPicture(ItemNo: Integer);
var item: TRVGraphicItemInfo;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (ItemNo<0) and (InplaceEditor<>nil) and (InplaceEditor is TCustomRichViewEdit) then begin
    TCustomRichViewEdit(InplaceEditor).ConvertToPicture(ItemNo);
    exit;
  end;
  {$ENDIF}
  if ItemNo<0 then
    ItemNo := CurItemNo;
  if GetItemStyle(ItemNo)<>rvsHotPicture then
    exit;
  if not BeforeChange(False) then exit;
  TRVEditRVData(RVData).BeginUndoSequence(rvutModifyItem, True);
  item := TRVGraphicItemInfo.CreateEx(RVData, nil, rvvaBaseline);
  SetUndoGroupMode(True);
  try
    TRVEditRVData(RVData).Do_ReplaceItem(ItemNo, item);
  finally
    SetUndoGroupMode(False);
  end;
  Change;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetTopLevelEditor: TCustomRichViewEdit;
begin
  Result := Self;
  while (Result.InplaceEditor<>nil) and (Result.InplaceEditor is TCustomRichViewEdit) do
    Result := TCustomRichViewEdit(Result.InplaceEditor);
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.GetRootEditor: TCustomRichViewEdit;
begin
  Result := RVData.GetAbsoluteRootData.GetParentControl as TCustomRichViewEdit;
end;
{------------------------------------------------------------------------------}
function TCustomRichViewEdit.DoChanging: Boolean;
begin
  Result := True;
  if (LockCount<=0) and Assigned(FOnChanging) then
    FOnChanging(GetRootEditor, Result);
end;
{$IFNDEF RVDONOTUSEDRAGDROP}
{------------------------------------------------------------------------------}
{ OLE drag&drop,  All these functions are called by FDropTarget:TRVDropTarget,
  related to IDropTarget.                                                      }
{------------------------------------------------------------------------------}
{ Drag cursor is entered into the control. From IDropTarget.DragEnter.
  X,Y - client coordinates.
  Creating RVData.FDragDropCaretInfo, activating scrolling timer,
  calling OleDragOver.                                                         }
function TCustomRichViewEdit.OleDragEnter(X,Y: Integer): Boolean;
begin
  if (csDesigning in ComponentState) or (Style=nil) or
     (ReadOnly and not (rvflDBRichViewEdit in Flags)) then begin
    Result := False;
    exit;
  end;
  TRVEditRVData(RVData.GetAbsoluteRootData).CreateDragDropCaretInfo;
  ActivateScrollTimer(True);
  OleDragOver(X, Y);
  TRVEditRVData(TopLevelEditor.RVData). ChangeCaret(False, False, True, False);
  Result := True;
end;
{------------------------------------------------------------------------------}
{ Finished. From IDropTarget.DragLeave
  Deactivating scrolling timer, destroying RVData.FDragDropCaretInfo.          }
procedure TCustomRichViewEdit.OleDragLeave;
begin
  DeactivateScrollTimer;
  RVData.RemoveDragDropCaret;
  TRVEditRVData(RVData.GetAbsoluteRootData).ReleaseDragDropCaretInfo;
  if RVData.GetDragDropCaretInfo=nil then
    TRVEditRVData(TopLevelEditor.RVData).ChangeCaret(False, False, True, False);
end;
{------------------------------------------------------------------------------}
{ Dragging over. From IDropTarget.DragOver.
  X,Y - client coordinates.
  Scrolling if the caret is in 20-pixels area near the top or bottom
  (to-do: scrolling does not work for inplace editor).
  Displaying drag&drop caret.                                                  }
function TCustomRichViewEdit.OleDragOver(X, Y: Integer): Boolean;
begin
  if Y<20 then
    VScrollDelta := -1
  else if Y>ClientHeight-20 then
    VScrollDelta := +1
  else
    VScrollDelta := 0;
  if (TRVEditRVData(RVData).GetDragDropCaretInfo<>nil) then begin
    RVData.SetDragDropCaretTo(X,Y);
    with TRVEditRVData(RVData).GetDragDropCaretInfo do
      Result := TCustomRVFormattedData(RVData).CanInsertHere(ItemNo, ItemOffs);
    end
  else
    Result := False;
end;
{------------------------------------------------------------------------------}
{ Dropping. From IDropTarget.Drop.
  Known problems:
  - when moving to inplace editor, will be undone in two steps;
}
function TCustomRichViewEdit.OleDrop(const DataObj: IDataObject;
  FMove: Boolean): Integer;
var CItemNo, COffs: Integer;
    Format: Word;
    DragDropCaretInfo: TRVDragDropCaretInfo;
    UndoGrouped, Beginning, BeginningIsText: Boolean;
    {..............................................}
    procedure BeforeInsert;
    begin
      if not UndoGrouped then begin
        BeginUndoGroup(rvutInsert);
        SetUndoGroupMode(True);
        LockWindowUpdate(TopLevelEditor.Handle);
        UndoGrouped := True;
      end;
      CItemNo := TopLevelEditor.CurItemNo;
      COffs   := TopLevelEditor.OffsetInCurItem;
      Beginning := (COffs<=TopLevelEditor.GetOffsBeforeItem(CItemNo));
      if Beginning then
        BeginningIsText := TopLevelEditor.GetItemStyle(CItemNo)>=0;
    end;
    {..............................................}
    procedure AfterInsert;
    var IsText2: Boolean;
    begin
      if UndoGrouped then begin
        //TRVEditRVData(RVData).FinalizeUndoGroup;
        SetUndoGroupMode(False);
      end;
      if Beginning then begin
        IsText2 := TopLevelEditor.GetItemStyle(CItemNo)>=0;
        if BeginningIsText and not IsText2 then
          COffs := 0
        else if IsText2 and (COffs=0) then
          COffs := 1;
      end;
      TopLevelEditor.SetSelectionBounds(CItemNo, COffs,
        TopLevelEditor.CurItemNo, TopLevelEditor.OffsetInCurItem);
      if UndoGrouped then begin
        LockWindowUpdate(0);
        TopLevelEditor.Invalidate;
        UndoGrouped := False;
      end;
    end;
    {..............................................}
    function InsertAsRTF: Boolean;
    {$IFNDEF RVDONOTUSERTFIMPORT}
    var Stream : TMemoryStream;
    {$ENDIF}
    begin
      Result := False;
      {$IFNDEF RVDONOTUSERTFIMPORT}
      if not (rvddRTF in AcceptDragDropFormats) then
        exit;
      if FDropTarget.HasFormat(DataObj, CFRV_RTF) then begin
        Stream := FDropTarget.GetAsStream(DataObj, CFRV_RTF);
        if Stream<>nil then begin
          BeforeInsert;
          try
            InsertRTFFromStreamEd(Stream);
            Result := True;
          finally
            AfterInsert;
            Stream.Free;
          end;
        end;
      end;
      {$ENDIF}
    end;
    {..............................................}
    function InsertAsRVF: Boolean;
    var Stream : TMemoryStream;
    begin
      Result := False;
      if not (rvddRVF in AcceptDragDropFormats) then
        exit;
      if FDropTarget.HasFormat(DataObj, CFRV_RVF) then begin
        Stream := FDropTarget.GetAsStream(DataObj, CFRV_RVF);
        if Stream<>nil then begin
          BeforeInsert;
          try
            InsertRVFFromStreamEd(Stream);
            Result := True;
          finally
            AfterInsert;
            Stream.Free;
          end;
        end;
      end;
    end;
    {..............................................}
    function InsertAsTextA: Boolean;
    var s: String;
    begin
      Result := False;
      if not (rvddText in AcceptDragDropFormats) then
        exit;
      if FDropTarget.HasFormat(DataObj, CF_TEXT) and
         FDropTarget.GetAsText(DataObj, CF_TEXT, s) then begin
        BeforeInsert;
        try
          InsertText(s, False);
          Result := True;
        finally
          AfterInsert;
        end;
      end;
    end;
    {..............................................}
    function InsertAsTextW: Boolean;
    {$IFNDEF RVDONOTUSEUNICODE}
    var s: String;
    {$ENDIF}
    begin
      Result := False;
      {$IFNDEF RVDONOTUSEUNICODE}
      if not (rvddUnicodeText in AcceptDragDropFormats) then
        exit;
      if FDropTarget.HasFormat(DataObj, CF_UNICODETEXT) and
         FDropTarget.GetAsTextW(DataObj, s) then begin
        BeforeInsert;
        try
          InsertTextW_(s);
          Result := True;
        finally
          AfterInsert;
        end;
      end;
      {$ENDIF}
    end;
    {..............................................}
    function InsertAsURL: Boolean;
    var Target, Title: String;
        StyleNo, Tag: Integer;
        StgMedium: TStgMedium;
        PFGD: PFileGroupDescriptor;
        cf: Word;
    begin
      Result := False;
      if not (rvddURL in AcceptDragDropFormats) then
        exit;
      if FDropTarget.HasFormat(DataObj, CFRV_URL) and
         FDropTarget.GetAsText(DataObj, CFRV_URL, Target) then begin
        Title := Target;
        {
        if FDropTarget.HasFormat(DataObj, CF_HDROP) then begin
          Files := FDropTarget.GetAsFiles(DataObj);
          if (Files.Count=1) and
             (AnsiCompareText(ExtractFileExt(Files[0]), '.url')=0) then
            Title := System.Copy(Files[0], 1, Length(Files[0])-4);
          Files.Free;
        end;
        }
        cf := RegisterClipboardFormat(CFSTR_FILEDESCRIPTOR);
        if FDropTarget.HasFormat(DataObj, cf) and
           FDropTarget.GetMedium(DataObj, cf, StgMedium) then begin
          PFGD := Pointer(GlobalLock(StgMedium.HGlobal));
          Title := pFGD^.fgd[0].cFileName;
          GlobalUnlock(StgMedium.HGlobal);
          if Length(Title)>4 then
            Delete(Title, Length(Title)-3,4)
          else
            Title := Target;
          ReleaseStgMedium(StgMedium);
        end;
        BeforeInsert;
        try
          if Assigned(OnReadHyperlink) then begin
            StyleNo := CurTextStyleNo;
            Tag := 0;
            OnReadHyperlink(Self, Target, '', rvlfURL, StyleNo, Tag, Title);
            CurTextStyleNo := StyleNo;
            InsertStringTag(Title, Tag);
            end
          else
            InsertText(Target, False);
          Result := True;
        finally
          AfterInsert;
        end;
      end;
    end;
    {..............................................}
    function InsertAsText: Boolean;
    begin
      {$IFNDEF RVDONOTUSEUNICODE}
      if Style.TextStyles[CurTextStyleNo].Unicode then
        Result := InsertAsTextW or InsertAsTextA
      else
      {$ENDIF}
        Result := InsertAsTextA or InsertAsTextW;
    end;
    {..............................................}
    function InsertAsBitmap: Boolean;
    var bmp: TBitmap;
    begin
      Result := False;
      if not (rvddBitmap in AcceptDragDropFormats) then
        exit;
      if FDropTarget.HasFormat(DataObj, CF_BITMAP) or
         FDropTarget.HasFormat(DataObj, CF_DIB) then begin
        bmp := FDropTarget.GetAsBitmap(DataObj, Format<>CF_BITMAP);
        if (bmp<>nil) and not bmp.Empty then begin
          BeforeInsert;
          try
            InsertPicture('', bmp, rvvaBaseline);
            Result := True;
          finally
            AfterInsert;
          end;
          end
        else
          bmp.Free;
      end;
    end;
    {..............................................}
    function InsertAsMetafile: Boolean;
    var wmf: TMetafile;
    begin
      Result := False;
      if not (rvddMetafile in AcceptDragDropFormats) then
        exit;
      if FDropTarget.HasFormat(DataObj, CF_ENHMETAFILE) then begin
        wmf := FDropTarget.GetAsMetafile(DataObj);
        if (wmf<>nil) then begin
          BeforeInsert;
          try
            InsertPicture('', wmf, rvvaBaseline);
            Result := True;
          finally
            AfterInsert;
          end;
        end;
      end;
    end;
    {..............................................}
    function InsertFile(const FileName: String): Boolean;
    var pic: TPicture;
        gr: TGraphic;
        Ext: String;
    begin
      Result := False;
      try
        pic := TPicture.Create;
        try
          pic.LoadFromFile(FileName);
          gr := RV_CreateGraphics(TGraphicClass(pic.Graphic.ClassType));
          gr.Assign(pic.Graphic);
          InsertPicture('', gr, rvvaBaseline);
          Result := True;
        except;
        end;
        pic.Free;
        if Result then
          exit;
        Ext := LowerCase(ExtractFileExt(FileName));
        {$IFNDEF RVDONOTUSERTFIMPORT}
        if Ext='.rtf' then begin
          InsertRTFFromFileEd(FileName);
          Result := True;
          exit;
        end;
        {$ENDIF}
        if Ext='.rvf' then begin
          InsertRVFFromFileEd(FileName);
          Result := True;
          exit;
        end;
        if Ext='.txt' then begin
          InsertTextFromFile(FileName);
          Result := True;
          exit;
        end;
      except
      end;
    end;
    {..............................................}
    function InsertAsFiles: Boolean;
    var Files: TStringList;
        DoDefault: Boolean;
        i: Integer;
        FileAction: TRVDropFileAction;
    begin
      Result := False;
      if not (rvddFiles in AcceptDragDropFormats) then
        exit;
      if FDropTarget.HasFormat(DataObj, CF_HDROP) then begin
        Files := FDropTarget.GetAsFiles(DataObj);
        if (Files<>nil) then begin
          BeforeInsert;
          try
            DoDefault := True;
            FileAction := rvdfNone;
            if Assigned(FOnDropFiles) then
              FOnDropFiles(GetRootEditor, Files, FileAction, DoDefault);
            if DoDefault then begin
              for i := 0 to Files.Count-1 do
                Result := InsertFile(Files[i]) or Result;
              end
            else
              Result := FileAction<>rvdfNone;
          finally
            AfterInsert;
            Files.Free;
          end;
        end;
      end;
    end;
    {..............................................}
    function DoInsert: Boolean;
    begin
      case Format of
        0:
          Result := InsertAsRVF or InsertAsRTF or InsertAsURL or InsertAsText or
            InsertAsBitmap or InsertAsMetafile or InsertAsFiles;
        CF_TEXT:
          Result := InsertAsTextA;
        {$IFNDEF RVDONOTUSEUNICODE}
        CF_UNICODETEXT:
          Result := InsertAsTextW;
        {$ENDIF}
        CF_BITMAP:
          Result := InsertAsBitmap;
        CF_ENHMETAFILE:
          Result := InsertAsMetafile;
        else
          begin
            if Format=CFRV_RVF then
              Result := InsertAsRVF
            else if Format=CFRV_RTF then
              Result := InsertAsRTF
            else
              Result := False;
          end;

      end;
    end;
    {..............................................}
begin
  UndoGrouped := False;
  DeactivateScrollTimer;
  RVData.RemoveDragDropCaret;
  DragDropCaretInfo := TRVEditRVData(RVData).GetDragDropCaretInfo;
  if
    not TCustomRVFormattedData(DragDropCaretInfo.RVData).Item_InsideSelection(
      DragDropCaretInfo.ItemNo, DragDropCaretInfo.ItemOffs) then begin
    if TCustomRVFormattedData(DragDropCaretInfo.RVData).CanInsertHere(
        DragDropCaretInfo.ItemNo, DragDropCaretInfo.ItemOffs) then begin
      if RVData.IsDragging then begin
        Format := GetAcceptableRVFormat;
        if Format<>0 then
          if not TRichViewRVData(RVData.GetAbsoluteRootData).FDropSource.StoreData(Format) then
            Format := 0;
        if FMove then begin
          LockWindowUpdate(TopLevelEditor.Handle);
          UndoGrouped := True;
          BeginUndoGroup(rvutInsert);
          SetUndoGroupMode(True);
          DeleteSelection;
          RVData.GetAbsoluteRootData.State := RVData.GetAbsoluteRootData.State-
            [rvstCanDragDropDeleteSelection];
        end;
        end
      else
        Format := 0;
      DragDropCaretInfo.RVData := DragDropCaretInfo.RVData.Edit;
      TCustomRVFormattedData(DragDropCaretInfo.RVData).
        SetSelectionBounds(DragDropCaretInfo.ItemNo, DragDropCaretInfo.ItemOffs,
          DragDropCaretInfo.ItemNo, DragDropCaretInfo.ItemOffs);
      if DragDropCaretInfo.RVData is TRVEditRVData then begin
        TRVEditRVData(RVData.GetAbsoluteRootData).ReleaseDragDropCaretInfo;
        if DoInsert then
          if FMove then
            Result := DROPEFFECT_MOVE
          else
            Result := DROPEFFECT_COPY
        else
          Result := DROPEFFECT_NONE;
        end
      else begin
        Result := DROPEFFECT_NONE;
        RVData.Beep;
      end;
      end
    else
      Result := DROPEFFECT_NONE;
    end
  else begin
    DragDropCaretInfo.RVData := DragDropCaretInfo.RVData.Edit;
    TCustomRVFormattedData(DragDropCaretInfo.RVData).
      SetSelectionBounds(DragDropCaretInfo.ItemNo, DragDropCaretInfo.ItemOffs,
        DragDropCaretInfo.ItemNo, DragDropCaretInfo.ItemOffs);
    TCustomRVFormattedData(DragDropCaretInfo.RVData).Invalidate;
    Result := DROPEFFECT_NONE;
  end;
  TRVEditRVData(RVData.GetAbsoluteRootData).ReleaseDragDropCaretInfo;
  {if Result = DROPEFFECT_NONE then
    RVData.Beep;}
  if UndoGrouped then begin
    SetUndoGroupMode(False);
    LockWindowUpdate(0);
  end;
  TRVEditRVData(TopLevelEditor.RVData).ChangeCaret(False, False, True, True);
end;
{------------------------------------------------------------------------------}
{ Informs about destroying of FDropTarget.
  It is destroyed when ref-count = 0.
  Its ref-count is incremented in WMCreate and decremented in WMDestroy.       }
procedure TCustomRichViewEdit.ReleaseOleDropTargetObject;
begin
  FDropTarget := nil;
end;
{------------------------------------------------------------------------------}
{ Can the format be accepted?                                                  }
function TCustomRichViewEdit.OleCanAcceptFormat(Format: Word): Boolean;
begin
  case Format of
    CF_TEXT:
      Result := rvddText in AcceptDragDropFormats;
    {$IFNDEF RVDONOTUSEUNICODE}
    CF_UNICODETEXT:
      Result := rvddUnicodeText in AcceptDragDropFormats;
    {$ENDIF}
    CF_BITMAP, CF_DIB:
      Result := rvddBitmap in AcceptDragDropFormats;
    CF_ENHMETAFILE:
      Result := rvddMetafile in AcceptDragDropFormats;
    CF_HDROP:
      Result := rvddFiles in AcceptDragDropFormats;
    else
      begin
        {$IFNDEF RVDONOTUSERTFIMPORT}
        if Format=CFRV_RTF then begin
          Result := rvddRTF in AcceptDragDropFormats;
          exit;
        end;
        {$ENDIF}
        {$IFNDEF RVDONOTUSERVF}
        if Format=CFRV_RVF then begin
          Result := rvddRVF in AcceptDragDropFormats;
          exit;
        end;
        {$ENDIF}
        if Format=CFRV_URL then begin
          Result := rvddURL in AcceptDragDropFormats;
          exit;
        end;
        Result := False;
      end;
  end;
end;
{------------------------------------------------------------------------------}
{ Returns a preferable drag&drop format that can be accepted when dragging
  from itself.                                                                 }
function TCustomRichViewEdit.GetAcceptableRVFormat: Word;
var gr: TGraphic;
begin
  if rvddRVF in AcceptDragDropFormats then
    Result := CFRV_RVF
  {$IFNDEF RVDONOTUSERTFIMPORT}
  {$IFNDEF RVDONOTUSERTF}
  else
    if rvddRTF in AcceptDragDropFormats then
    Result := CFRV_RTF
  {$ENDIF}
  {$ENDIF}
  else begin
    Result := 0;
    gr := GetSelectedImage;
    if gr<>nil then begin
      if (gr is TBitmap) and (rvddBitmap in AcceptDragDropFormats) then
        Result := CF_BITMAP
      else if (gr is TMetafile) and (rvddMetafile in AcceptDragDropFormats) then
        Result := CF_ENHMETAFILE;
    end;
    if Result=0 then
      {$IFNDEF RVDONOTUSEUNICODE}
      if rvddUnicodeText in AcceptDragDropFormats then
        Result := CF_UNICODETEXT
      else
      {$ENDIF}
      if rvddText in AcceptDragDropFormats then
        Result := CF_TEXT;
  end;
end;
{$ENDIF}

end.

