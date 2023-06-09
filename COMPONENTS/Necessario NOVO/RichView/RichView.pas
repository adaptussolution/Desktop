
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRichView: document viewer                      }
{       (registered on "RichView" page of               }
{       the Component Palette)                          }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RichView;

interface
{$I RV_Defs.inc}
{$IFDEF RICHVIEWDEF6}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  DLines, RVItem, RVStyle, RVScroll, RVFMisc, RVFuncs, CRVData, CRVFData, RVRVData,
  RVBack, RVUni, Registry, Menus,
  {$IFDEF RICHVIEWDEF4}
  ImgList,
  {$ENDIF}
  {$IFNDEF RVDONOTUSEDRAGDROP}
  RVDragDrop,
  {$ENDIF}
  ClipBrd,
  RVRTFProps, RVRTFErr, StdCtrls;
const
  WM_RVDRAGDROP = WM_USER+16;
{------------------------------------------------------------------------------}

  { For internal use. Types and constants for implementing Windows messages with
    inplace editor.
    Some messages cannot be processed directly in inplace editor, because
    they can cause its destruction (from inside Delphi events).
    Due to VCL design, destruction of component during processing these
    messages will crash the application.
    So, RV sends (via PostMessage) WM_RVEVENT to the root editor, which in its
    case call proper event                                                     }

const
  WM_RVEVENT    = WM_USER + 15;

type
  // identifies event
  TRVEventType = (rvetRVDblClick, rvetJump, rvetRVMouseUp, rvetRVMouseDown,
    rvetClick, rvetDblClick, rvetMouseMove, rvetDragDrop, rvetEndDrag);

  // basic class for data passed to WM_RVEVENT handler
  TRVMessageData = class
    public
      Event: TRVEventType;
  end;

  // for OnClick
  TRVClickMessageData = class (TRVMessageData)
  end;

  // for OnDblClick
  TRVStdDblClickMessageData = class (TRVMessageData)
  end;

  // for OnRVDblClick
  TRVDblClickMessageData = class (TRVMessageData)
    public
      ClickedWord: String;
      StyleNo: Integer;
  end;

  // for OnJump
  TRVJumpMessageData = class (TRVMessageData)
    public
      id: Integer;
  end;

  // for OnRVMouseMove
  TRVMouseMoveMessageData = class (TRVMessageData)
    public
      X,Y,ItemNo: Integer;
      Shift: TShiftState;
  end;

  // for OnRVMouseUp and OnRVMouseDown
  TRVMouseUpDownMessageData = class (TRVMouseMoveMessageData)
    public
      Button: TMouseButton;
  end;

  // for rvetDragDrop and rvetEndDrag
  TRVDNDMessageData = class (TRVMessageData)
    public
      X, Y: Integer;
      Obj: TObject;
  end;

  TCustomRichView = class;

  { Step of printing or repaginating }
  TRVPrintingStep = (
    rvpsStarting,       // operation is started
    rvpsProceeding,     // next page is completed
    rvpsFinished);      // operation is finished

  { ---------------- Types for events of TCustomRichView --------------------- }
  TJumpEvent = procedure (Sender: TObject; id: Integer) of object;

  TRVMouseMoveEvent = procedure (Sender: TObject; id: Integer) of object;

  TRVMouseEvent = procedure (Sender: TCustomRichView; Button: TMouseButton;
    Shift: TShiftState; ItemNo, X, Y: Integer) of object;

  TRVSaveComponentToFileEvent = procedure (Sender: TCustomRichView;
    Path: String; SaveMe: TPersistent; SaveFormat: TRVSaveFormat;
    var OutStr:String) of object;

  TRVSaveItemToFileEvent = procedure (Sender: TCustomRichView;
    const Path: String; RVData: TCustomRVData; ItemNo: Integer;
    SaveFormat: TRVSaveFormat; Unicode: Boolean; var OutStr:String;
    var DoDefault: Boolean) of object;

  TRVURLNeededEvent = procedure (Sender: TCustomRichView; id: Integer;
    var url:String) of object;

  TRVDblClickEvent = procedure (Sender: TCustomRichView; ClickedWord: String;
    Style: Integer) of object;

  TRVRightClickEvent = procedure (Sender: TCustomRichView; ClickedWord: String;
    Style, X, Y: Integer) of object;

  TRVFPictureNeededEvent = procedure  (Sender: TCustomRichView; Name: String;
    Tag: Integer; var gr: TGraphic) of object;

  TRVFControlNeededEvent = procedure  (Sender: TCustomRichView; Name: String;
    Tag: Integer; var ctrl: TControl) of object;

  TRVCheckpointVisibleEvent = procedure (Sender: TCustomRichView;
    CheckpointData: TCheckpointData) of object;

  TRVControlActionEvent = procedure (Sender: TCustomRichView;
    ControlAction: TRVControlAction; ItemNo: Integer;
    var ctrl: TControl) of object;

  TRVItemActionEvent = procedure (Sender: TCustomRichView;
    ItemAction: TRVItemAction; Item: TCustomRVItemInfo; var Text: String;
    RVData: TCustomRVData) of object;

  TRVFImageListNeededEvent = procedure (Sender: TCustomRichView;
    ImageListTag: Integer; var il: TCustomImageList) of object;

  TRVHTMLSaveImageEvent = procedure (Sender: TCustomRichView;
    RVData: TCustomRVData; ItemNo: Integer; const Path: String;
    BackgroundColor: TColor; var Location: String;
    var DoDefault: Boolean) of object;

  TRVSaveImageEvent2 = procedure (Sender: TCustomRichView; Graphic: TGraphic;
    SaveFormat: TRVSaveFormat; const Path, ImagePrefix: String;
    var ImageSaveNo: Integer; var Location: String;
    var DoDefault: Boolean) of object;

  TRVReadHyperlink = procedure (Sender: TCustomRichView;
    const Target, Extras: String; DocFormat: TRVLoadFormat;
   var StyleNo, ItemTag: Integer; var ItemName: String) of object;

   TRVWriteHyperlink = procedure (Sender: TCustomRichView;
     id: Integer; RVData: TCustomRVData; ItemNo: Integer;
     SaveFormat: TRVSaveFormat; var Target, Extras: String) of object;

  TRVSaveRTFExtraEvent = procedure (Sender: TCustomRichView;
    Area: TRVRTFSaveArea; Obj: TObject; Index1, Index2: Integer;
    InStyleSheet: Boolean; var RTFCode: String) of object;

  TRVSaveHTMLExtraEvent = procedure (Sender: TCustomRichView;
    Area: TRVHTMLSaveArea; CSSVersion: Boolean;
    var HTMLCode: String) of object;

  TRVSaveParaToHTMLEvent = procedure (Sender: TCustomRichView;
    RVData: TCustomRVData; ItemNo: Integer; ParaStart, CSSVersion: Boolean;
    var HTMLCode: String) of object;

  TRVPaintEvent = procedure (Sender: TCustomRichView;
    Canvas: TCanvas; Prepaint: Boolean) of object;

  TRVImportPictureEvent = procedure (Sender: TCustomRichView;
    const Location: String; Width, Height: Integer;
    var Graphic: TGraphic) of object;

  TRVItemHintEvent = procedure (Sender: TCustomRichView;
    RVData: TCustomRVData; ItemNo: Integer; var HintText: String) of object;

  TRVProgressEvent = procedure (Sender: TCustomRichView;
    Operation: TRVLongOperation; Stage: TRVProgressStage;
    PercentDone: Byte) of object;

  { -------------------------------------------------------------------------- }
  { TCustomRichView: ancestor class for TRichView, TRichViewEdit, TDBRichView,
      TDBRichViewEdit
  }

  TCustomRichView = class(TRVScroller)
  private
    { Private declarations }
    FCursor: TCursor;
    FOptions: TRVOptions;
    FRTFOptions: TRVRTFOptions;
    ScrollTimerActive: Boolean;
    FDelimiters: String;
    FOnJump: TJumpEvent;
    FOnRVMouseMove: TRVMouseMoveEvent;
    FOnSaveComponentToFile: TRVSaveComponentToFileEvent;
    FOnSaveItemToFile: TRVSaveItemToFileEvent;
    FOnURLNeeded: TRVURLNeededEvent;
    FOnRVDblClick: TRVDblClickEvent;
    FOnRVRightClick: TRVRightClickEvent;
    FOnRVMouseUp,FOnRVMouseDown: TRVMouseEvent;
    FOnControlAction: TRVControlActionEvent;
    FOnItemAction: TRVItemActionEvent;
    FCPEventKind: TCPEventKind;
    FOnRVFPictureNeeded: TRVFPictureNeededEvent;
    FOnRVFControlNeeded: TRVFControlNeededEvent;
    FOnRVFImageListNeeded: TRVFImageListNeededEvent;
    FOnCheckpointVisible: TRVCheckpointVisibleEvent;
    FMaxTextWidth, FMinTextWidth, FLeftMargin, FRightMargin, FTopMargin, FBottomMargin: Integer;
    FRVFOptions: TRVFOptions;
    FRVFWarnings: TRVFWarnings;
    {$IFDEF RV_ODHC}
    FOnDocumentHeightChange: TNotifyEvent;
    {$ENDIF}
    FOnCopy: TNotifyEvent;
    FOnHTMLSaveImage: TRVHTMLSaveImageEvent;
    FOnSaveImage2: TRVSaveImageEvent2;
    FRTFReadProperties: TRVRTFReaderProperties;
    FRVFTextStylesReadMode: TRVFReaderStyleMode;
    FRVFParaStylesReadMode: TRVFReaderStyleMode;
    FOnReadHyperlink: TRVReadHyperlink;
    FOnWriteHyperlink: TRVWriteHyperlink;
    FOnSaveRTFExtra: TRVSaveRTFExtraEvent;
    FOnSaveHTMLExtra: TRVSaveHTMLExtraEvent;
    FOnSaveParaToHTML: TRVSaveParaToHTMLEvent;    
    FOnPaint: TRVPaintEvent;
    FOnImportPicture: TRVImportPictureEvent;
    FOnItemHint: TRVItemHintEvent;
    FDocProperties: TStringList;
    FOnProgress: TRVProgressEvent;
    {$IFDEF RICHVIEWDEF3}
    FVAlign: TTextLayout;
    {$ENDIF}
    {$IFDEF RICHVIEWDEF5}
    procedure WMContextMenu(var Message: TWMContextMenu); message WM_CONTEXTMENU;
    {$ENDIF}
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMCopy(var Message: TWMCopy); message WM_COPY;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMDestroy(var Message: TWMDestroy); message WM_DESTROY;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMRVEvent(var Message: TMessage); message WM_RVEVENT;
    procedure WMRVDragDrop(var Message: TMessage); message WM_RVDRAGDROP;

    function GetLineCount: Integer;

    function GetAllowSelection: Boolean;
    function GetSingleClick: Boolean;
    procedure SetAllowSelection(const Value: Boolean);
    procedure SetSingleClick(const Value: Boolean);
    procedure DoOnBackBitmapChange(Sender: TObject);
    function GetPageBreaksBeforeItems(Index: Integer): Boolean;
    procedure SetPageBreaksBeforeItems(Index: Integer;  Value: Boolean);
    function GetDocumentHeight: Integer;
    function GetFirstJumpNo: Integer;
    procedure SetFirstJumpNo(Value: Integer);
    procedure SetTabNavigation(const Value: TRVTabNavigationType);
    procedure SetRTFReadProperties(const Value: TRVRTFReaderProperties);
    function StoreDelimiters: Boolean;
    procedure SetDocProperties(const Value: TStringList);
    {$IFDEF RVDEBUG}{$I Debug\DebPropDef.inc}{$ENDIF}
  protected
    { Protected declarations }
    VScrollDelta, HScrollDelta: Integer;    
    FOnSelect: TNotifyEvent;
    FStyle: TRVStyle;

    imgSavePrefix: String;
    SaveOptions: TRVSaveOptions;
    CurrentFileColor: TColor;
    procedure SetBiDiModeRV(const Value: TRVBiDiMode); override;

    procedure SetVSmallStep(Value: Integer); override;
    procedure Paint; override;

    function GetColor: TColor;
    function GetHoverColor(Color: TColor):TColor;

    function IsCopyShortcut(Shift: TShiftState; Key: Word): Boolean;
    function IsCutShortcut(Shift: TShiftState; Key: Word): Boolean;
    function IsPasteShortcut(Shift: TShiftState; Key: Word): Boolean;

    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ClearTemporal; virtual;
    function GetFirstItemVisible: Integer;
    function GetLastItemVisible: Integer;
    function GetBackBitmap: TBitmap;
    procedure SetBackBitmap(Value: TBitmap);
    procedure SetBackgroundStyle(Value: TBackgroundStyle);
    function GetBackgroundStyle: TBackgroundStyle;
    procedure Notification(AComponent: TComponent; Operation: TOperation);override;

    procedure Loaded; override;
    function CompareTags(Tag1, Tag2: Integer): Boolean;
    procedure SetStyle(Value: TRVStyle);
    procedure AfterVScroll; override;
    procedure InplaceRedrawing(AllowRedrawItself: Boolean); virtual;
    procedure AfterHScroll; override;
    procedure GenerateMouseMove;
    procedure Format_(OnlyResized,ForceFormat:Boolean; Canvas: TCanvas;
          OnlyTail, NoCaching: Boolean);
    function GetDataClass: TRichViewRVDataClass; virtual;
    function GetTabNavigation:TRVTabNavigationType;
    function GetRTFReadProperties: TRVRTFReaderProperties; virtual;
    procedure AfterCreateWnd1; override;
    procedure AfterCreateWnd2; override;
    procedure SetName(const NewName: TComponentName); override;
    { obsolete properties }
    property AllowSelection: Boolean read GetAllowSelection write SetAllowSelection stored False;
    property SingleClick   : Boolean read GetSingleClick    write SetSingleClick    stored False;
    property OnPaint: TRVPaintEvent  read FOnPaint          write FOnPaint;
  public
    { Should be protected. Do not use! }
    RVData: TRichViewRVData;
    Flags : TRVFlags;
    Background: TRVBackground;
    imgSaveNo: Integer;    
    property Canvas;
    procedure SelectNext_(GoForward: Boolean);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
    procedure ActivateScrollTimer(Slow: Boolean);
    procedure DeactivateScrollTimer;
    function RTFReaderAssigned: Boolean; dynamic;
    procedure AssignEvents(Source: TCustomRichView);
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetTabOrderList(List: TList); override;
    procedure AssignSoftPageBreaks(RVPrint: TComponent);
    procedure ClearSoftPageBreaks;

    { add... methods: }
    procedure AddItem(const Text: String; Item: TCustomRVItemInfo);
    procedure AddNL(const s: String; StyleNo, ParaNo: Integer);
    procedure AddFmt(const FormatStr: String; const Args: array of const;
                     StyleNo, ParaNo: Integer);
    procedure Add(const s: String; StyleNo:Integer);
    procedure AddTextNL(const s: String; StyleNo, FirstParaNo, OtherParaNo: Integer);
    procedure AddTextBlockNL(s: String; StyleNo, ParaNo: Integer);
    procedure AddBreak;
    function AddCheckpoint: Integer; { returns cp # }
    function AddNamedCheckpoint(CpName: String): Integer; { returns cp # }
    function AddNamedCheckpointEx(const CpName: String; RaiseEvent: Boolean): Integer; { returns cp # }
    procedure AddPictureEx(const Name: String; gr: TGraphic; ParaNo: Integer;
                           VAlign: TRVVAlign);
    procedure AddHotPicture(const Name: String; gr: TGraphic; ParaNo: Integer;
                           VAlign: TRVVAlign);
    procedure AddHotspotEx(const Name: String; ImageIndex, HotImageIndex: Integer;
                           ImageList: TCustomImageList; ParaNo: Integer);
    procedure AddBulletEx (const Name: String; ImageIndex: Integer;
                           ImageList: TCustomImageList; ParaNo: Integer);
    procedure AddControlEx(const Name: String; ctrl: TControl;
                           ParaNo: Integer; VAlign: TRVVAlign);
    procedure AddBreakEx(Width: Byte; Style: TRVBreakStyle; Color: TColor);
    {$IFDEF RVDEBUG}{$I Debug\DebPropDef2.inc}{$ENDIF}
    { add...tag methods: }
    procedure AddNLTag(const s: String; StyleNo, ParaNo, Tag: Integer);
    procedure AddTag(const s: String;StyleNo,Tag:Integer);
    procedure AddBreakTag(Tag:Integer);
    function AddCheckpointTag(Tag: Integer): Integer; { returns cp # }
//    function AddNamedCheckpointTag(const CpName: String; Tag: Integer): Integer; { returns cp # }
    function AddNamedCheckpointExTag(const CpName: String; RaiseEvent: Boolean;
                                    Tag: Integer): Integer; { returns cp # }
    procedure AddPictureExTag(const Name: String; gr: TGraphic; ParaNo: Integer;
                              VAlign: TRVVAlign; Tag: Integer);
    procedure AddHotPictureTag(const Name: String; gr: TGraphic; ParaNo: Integer;
                              VAlign: TRVVAlign; Tag: Integer);
    procedure AddHotspotExTag(const Name: String; ImageIndex, HotImageIndex: Integer;
                              ImageList: TCustomImageList; ParaNo,Tag: Integer);
    procedure AddBulletExTag (const Name: String; ImageIndex: Integer;
                              ImageList: TCustomImageList; ParaNo,Tag: Integer);
    procedure AddControlExTag(const Name: String; ctrl: TControl;
                              ParaNo: Integer; VAlign: TRVVAlign; Tag: Integer);
    procedure AddBreakExTag(Width: Byte; Style: TRVBreakStyle;
                            Color: TColor; Tag: Integer);
    { add... methods for backward compatibility: }
    procedure AddFromNewLine(const s: String;StyleNo:Integer);  {$IFDEF RICHVIEWDEF6}deprecated;{$ENDIF}
    procedure AddCenterLine(const s: String;StyleNo:Integer);  {$IFDEF RICHVIEWDEF6}deprecated;{$ENDIF}
    procedure AddText(const s: String;StyleNo:Integer);  {$IFDEF RICHVIEWDEF6}deprecated;{$ENDIF}
    procedure AddTextFromNewLine(const s: String;StyleNo:Integer);  {$IFDEF RICHVIEWDEF6}deprecated;{$ENDIF}
    procedure AddPicture(gr: TGraphic);  {$IFDEF RICHVIEWDEF6}deprecated;{$ENDIF}
    procedure AddHotspot(ImageIndex: Integer; ImageList: TCustomImageList;
                         fromnewline: Boolean);  {$IFDEF RICHVIEWDEF6}deprecated;{$ENDIF}
    procedure AddBullet (ImageIndex: Integer; ImageList: TCustomImageList;
                         fromnewline: Boolean);  {$IFDEF RICHVIEWDEF6}deprecated;{$ENDIF}
    procedure AddControl(ctrl: TControl; center: Boolean);  {$IFDEF RICHVIEWDEF6}deprecated;{$ENDIF}

    // checkpoint methods:
    function GetCheckpointY(no: Integer): Integer;
    function GetFirstCheckpoint: TCheckpointData;
    function GetNextCheckpoint(CheckpointData: TCheckpointData): TCheckpointData;
    function GetLastCheckpoint: TCheckpointData;
    function GetPrevCheckpoint(CheckpointData: TCheckpointData): TCheckpointData;
    function GetItemCheckpoint(ItemNo: Integer):TCheckpointData;
    function FindCheckpointByName(const Name: String): TCheckpointData;
    function FindCheckpointByTag(Tag: Integer): TCheckpointData;
    function GetCheckpointByNo(No: Integer): TCheckpointData;
    procedure GetCheckpointInfo(CheckpointData: TCheckpointData;
                                var Tag: Integer; var Name: String;
                                var RaiseEvent: Boolean);
    procedure GetCheckpointXY(CheckpointData: TCheckpointData; var X,Y: Integer);
    function GetCheckpointYEx(CheckpointData: TCheckpointData): Integer;
    function GetCheckpointItemNo(CheckpointData: TCheckpointData): Integer;
    function GetCheckpointNo(CheckpointData: TCheckpointData): Integer;

    function GetJumpPointY(id: Integer): Integer;
    function GetJumpPointItemNo(id: Integer): Integer;
    procedure GetJumpPointLocation(id: Integer; var RVData: TCustomRVFormattedData; var ItemNo: Integer);

    function GetItemCoords(ItemNo: Integer;var Left,Top: Integer): Boolean;
    function GetItemClientCoords(ItemNo: Integer;var Left,Top: Integer): Boolean;

    procedure Clear;
    procedure Format;
    procedure Reformat;
    procedure FormatTail;

    procedure AppendFrom(Source: TCustomRichView);
    {$IFNDEF RVDONOTUSEHTML}
    function SaveHTMLToStreamEx(Stream: TStream;
                        const Path, Title, ImagesPrefix, ExtraStyles,
                        ExternalCSS, CPPrefix: String;
                        Options: TRVSaveOptions):Boolean;
    function SaveHTMLToStream(Stream: TStream; const Path, Title,ImagesPrefix: String;
                        Options: TRVSaveOptions):Boolean;
    function SaveHTMLEx(const FileName, Title, ImagesPrefix, ExtraStyles,
                        ExternalCSS, CPPrefix: String;
                        Options: TRVSaveOptions):Boolean;
    function SaveHTML(const FileName,Title,ImagesPrefix: String;
                        Options: TRVSaveOptions):Boolean;
    {$ENDIF}
    function SaveText(const FileName: String; LineWidth: Integer):Boolean;
    function SaveTextToStream(const Path: String; Stream: TStream;
                        LineWidth: Integer;
                        SelectionOnly, TextOnly: Boolean):Boolean;
    function LoadText(const FileName: String; StyleNo, ParaNo: Integer;
      AsSingleParagraph: Boolean):Boolean;
    function LoadTextFromStream(Stream: TStream; StyleNo, ParaNo: Integer;
      AsSingleParagraph: Boolean):Boolean;

    {$IFNDEF RVDONOTUSERVF}
    function CreateLayoutInfo: TRVLayoutInfo;
    procedure ApplyLayoutInfo (Layout: TRVLayoutInfo); dynamic;
    function LoadRVFFromStream(Stream: TStream):Boolean;
    function InsertRVFFromStream(Stream: TStream; Index: Integer):Boolean;
    function AppendRVFFromStream(Stream: TStream; ParaNo: Integer):Boolean;
    function LoadRVF(const FileName: String):Boolean;
    function SaveRVFToStream(Stream: TStream; SelectionOnly: Boolean):Boolean;
    function SaveRVF(const FileName: String; SelectionOnly: Boolean):Boolean;
    procedure CopyRVF;
    {$ENDIF}

    {$IFNDEF RVDONOTUSERTF}
    function SaveRTFToStream(Stream: TStream; SelectionOnly: Boolean):Boolean;
    function SaveRTF(const FileName: String; SelectionOnly: Boolean):Boolean;
    procedure CopyRTF;
    {$ENDIF}
    {$IFNDEF RVDONOTUSERTFIMPORT}
    function LoadRTFFromStream(Stream: TStream):Boolean;
    function LoadRTF(const FileName: String):Boolean;
    {$ENDIF}

    {$IFNDEF RVDONOTUSEUNICODE}
    function SaveTextW(const FileName: String; LineWidth: Integer):Boolean;
    function SaveTextToStreamW(const Path: String; Stream: TStream;
                        LineWidth: Integer;
                        SelectionOnly, TextOnly: Boolean):Boolean;
    function LoadTextW(const FileName: String; StyleNo, ParaNo: Integer;
      DefAsSingleParagraph: Boolean):Boolean;
    function LoadTextFromStreamW(Stream: TStream; StyleNo, ParaNo: Integer;
      DefAsSingleParagraph: Boolean):Boolean;
    procedure AddNLATag(const s: String; StyleNo, ParaNo, Tag: Integer);
    function GetItemTextA(ItemNo: Integer): String;
    procedure SetItemTextA(ItemNo: Integer; const s: String);    

    {$IFDEF RICHVIEWCBDEF3}
    procedure AddNLWTag(const s: WideString; StyleNo, ParaNo, Tag: Integer);
    procedure AddTextNLW(const s: WideString; StyleNo, FirstParaNo, OtherParaNo: Integer;
                         DefAsSingleParagraph: Boolean);
    function GetSelTextW: WideString;
    function GetItemTextW(ItemNo: Integer): WideString;
    procedure SetItemTextW(ItemNo: Integer; const s: WideString);
    {$ENDIF}
    {$ENDIF}

    procedure DeleteSection(const CpName: String);
    procedure DeleteItems(FirstItemNo, Count: Integer);
    procedure DeleteParas(FirstItemNo, LastItemNo: Integer);

    procedure CopyText;
    procedure CopyTextW;
    procedure CopyImage;
    procedure Copy;
    function CopyDef: Boolean;

    function GetSelectedImage: TGraphic;

    function GetSelText: String;
    function SelectionExists: Boolean;
    procedure Deselect;
    procedure SelectAll;

    function SearchText(const s: String; SrchOptions: TRVSearchOptions): Boolean;

    function GetItemStyle(ItemNo: Integer): Integer;

    procedure GetBreakInfo(ItemNo: Integer; var AWidth: Byte;
                            var AStyle: TRVBreakStyle; var AColor: TColor;
                            var ATag: Integer);
    procedure GetBulletInfo(ItemNo: Integer; var AName: String;
                            var AImageIndex: Integer;
                            var AImageList: TCustomImageList;
                            var ATag: Integer);
    procedure GetHotspotInfo(ItemNo: Integer; var AName: String;
                            var AImageIndex, AHotImageIndex: Integer;
                            var AImageList: TCustomImageList;
                            var ATag: Integer);
    procedure GetPictureInfo(ItemNo: Integer; var AName: String;
                            var Agr: TGraphic; var AVAlign: TRVVAlign;
                            var ATag: Integer);
    procedure GetControlInfo(ItemNo: Integer; var AName: String;
                            var Actrl: TControl; var AVAlign: TRVVAlign;
                            var ATag: Integer);
    procedure GetTextInfo(ItemNo: Integer; var AText: String;
                            var ATag: Integer);
    function GetItemTag(ItemNo: Integer): Integer;

    procedure SetItemText(ItemNo: Integer; const s: String);
    function GetItemText(ItemNo: Integer): String;

    function SetItemExtraIntProperty(ItemNo: Integer; Prop: TRVExtraItemProperty;
      Value: Integer): Boolean;
    function GetItemExtraIntProperty(ItemNo: Integer; Prop: TRVExtraItemProperty;
      var Value: Integer): Boolean;
    function SetItemExtraStrProperty(ItemNo: Integer; Prop: TRVExtraItemStrProperty;
      const Value: String): Boolean;
    function GetItemExtraStrProperty(ItemNo: Integer; Prop: TRVExtraItemStrProperty;
      var Value: String): Boolean;

    function IsParaStart(ItemNo: Integer): Boolean;
    function GetItemPara(ItemNo: Integer): Integer;
    function IsFromNewLine(ItemNo: Integer): Boolean;

    procedure SetBreakInfo(ItemNo: Integer; AWidth: Byte;
                            AStyle: TRVBreakStyle; AColor: TColor;
                            ATag: Integer);
    procedure SetBulletInfo(ItemNo: Integer; const AName: String;
                            AImageIndex: Integer;
                            AImageList: TCustomImageList;
                            ATag: Integer);
    procedure SetHotspotInfo(ItemNo: Integer; const AName: String;
                            AImageIndex, AHotImageIndex: Integer;
                            AImageList: TCustomImageList;
                            ATag: Integer);
    // ret value: reformatting needed
    function SetPictureInfo(ItemNo: Integer; const  AName: String;
                            Agr: TGraphic; AVAlign: TRVVAlign;
                            ATag: Integer): Boolean;
    function SetControlInfo(ItemNo: Integer; const AName: String;
                            AVAlign: TRVVAlign; ATag: Integer): Boolean;
    procedure SetItemTag(ItemNo: Integer; ATag: Integer);
    procedure SetCheckpointInfo(ItemNo: Integer; ATag: Integer;
                                const AName: String;
                                ARaiseEvent: Boolean);
    function RemoveCheckpoint(ItemNo: Integer): Boolean;

    function FindControlItemNo(actrl: TControl): Integer;
    function SelectControl(actrl: TControl): Boolean;

    procedure GetSelectionBounds(var StartItemNo, StartItemOffs,
                                 EndItemNo, EndItemOffs: Integer;
                                 Normalize: Boolean);
    procedure SetSelectionBounds(StartItemNo, StartItemOffs,
                                 EndItemNo, EndItemOffs: Integer);
    procedure GetWordAt(X,Y: Integer; var RVData: TCustomRVFormattedData; var ItemNo: Integer; var Word: String); {$IFDEF RICHVIEWDEF4}overload;
    function GetWordAt(X,Y: Integer): String; overload;{$ENDIF}
    procedure SelectWordAt(X,Y: Integer);

    procedure UpdatePaletteInfo; override;

    function GetOffsBeforeItem(ItemNo: Integer): Integer;
    function GetOffsAfterItem(ItemNo: Integer): Integer;

    procedure SetAddParagraphMode(AllowNewPara: Boolean);

    function SavePicture(DocumentSaveFormat: TRVSaveFormat; const Path: String;
      gr: TGraphic): String; virtual;
    function GetSelectionRect: TRect;
    function GetItem(ItemNo: Integer): TCustomRVItemInfo;
    function GetItemNo(Item: TCustomRVItemInfo): Integer;

    procedure GetFocusedItem(var ARVData: TCustomRVFormattedData; var AItemNo: Integer);
    procedure DeleteUnusedStyles(TextStyles, ParaStyles, ListStyles: Boolean);

    {$IFNDEF RVDONOTUSEDRAGDROP}
    procedure BeginOleDrag;
    {$ENDIF}

    {$IFNDEF RVDONOTUSELISTS}
    function SetListMarkerInfo(AItemNo, AListNo, AListLevel, AStartFrom,
      AParaNo: Integer; AUseStartFrom: Boolean): Integer;
    procedure RemoveListMarker(ItemNo: Integer);
    function GetListMarkerInfo(AItemNo: Integer; var AListNo, AListLevel,
      AStartFrom: Integer; var AUseStartFrom: Boolean): Integer;
    procedure RefreshListMarkers;
    {$ENDIF}
    function GetLineNo(ItemNo, ItemOffs: Integer): Integer;
    function GetItemAt(X,Y: Integer; var RVData: TCustomRVFormattedData; var ItemNo, OffsetInItem: Integer;
      Strict: Boolean): Boolean;
    function ClientToDocument(const APoint: TPoint): TPoint;

    property LineCount: Integer read GetLineCount;
    property ItemCount: Integer read GetLineCount;

    property FirstItemVisible: Integer read GetFirstItemVisible;
    property LastItemVisible: Integer read GetLastItemVisible;
    property RVFWarnings: TRVFWarnings read FRVFWarnings write FRVFWarnings;

    property DocumentHeight: Integer read GetDocumentHeight;

    property PageBreaksBeforeItems[Index: Integer]: Boolean
      read GetPageBreaksBeforeItems write SetPageBreaksBeforeItems;

    property BackgroundBitmap: TBitmap   read GetBackBitmap write SetBackBitmap;
    property BackgroundStyle: TBackgroundStyle read GetBackgroundStyle write SetBackgroundStyle;
    property BottomMargin: Integer       read FBottomMargin write FBottomMargin default 5;
    property Color default clNone;
    property CPEventKind : TCPEventKind  read FCPEventKind  write FCPEventKind;
    property Cursor: TCursor read FCursor write FCursor;
    property Delimiters  : String        read FDelimiters   write FDelimiters stored StoreDelimiters;
    property DoInPaletteMode;
    property FirstJumpNo    : Integer               read GetFirstJumpNo   write SetFirstJumpNo default 0;
    property FullRedraw;
    property HScrollVisible;
    property HScrollMax;
    property HScrollPos;
    property InplaceEditor;
    property LeftMargin  : Integer       read FLeftMargin   write FLeftMargin default 5;
    property MaxTextWidth: Integer       read FMaxTextWidth write FMaxTextWidth default 0;
    property MinTextWidth: Integer       read FMinTextWidth write FMinTextWidth default 0;
    property Options     : TRVOptions    read FOptions      write FOptions
             default [rvoAllowSelection, rvoScrollToEnd, rvoAutoCopyText,
                       rvoAutoCopyImage, rvoAutoCopyRVF, rvoAutoCopyRTF,
                       rvoDblClickSelectsWord, rvoRClickDeselects,
                       rvoFormatInvalidate,
                       rvoShowPageBreaks];
    property RightMargin : Integer       read FRightMargin  write FRightMargin default 5;
    property RTFOptions  : TRVRTFOptions read FRTFOptions   write FRTFOptions default [rvrtfDuplicateUnicode, rvrtfSaveEMFAsWMF, rvrtfSaveJpegAsJpeg];
    property RTFReadProperties: TRVRTFReaderProperties read GetRTFReadProperties write SetRTFReadProperties;
    property RVFOptions  : TRVFOptions   read FRVFOptions   write FRVFOptions
      default [rvfoSavePicturesBody, rvfoSaveControlsBody, rvfoSaveBinary,
        rvfoSaveDocProperties, rvfoLoadDocProperties];
    property RVFParaStylesReadMode: TRVFReaderStyleMode read FRVFParaStylesReadMode write FRVFParaStylesReadMode default rvf_sInsertMerge;
    property RVFTextStylesReadMode: TRVFReaderStyleMode read FRVFTextStylesReadMode write FRVFTextStylesReadMode default rvf_sInsertMerge;
    property Style       : TRVStyle      read FStyle        write SetStyle;
    property TabNavigation: TRVTabNavigationType read GetTabNavigation write SetTabNavigation default rvtnTab;
    property TopMargin   : Integer       read FTopMargin    write FTopMargin default 5;
    property DocProperties: TStringList  read FDocProperties write SetDocProperties;
    property VScrollMax;
    property VScrollPos;
    property VScrollVisible;
    property VSmallStep;
    {$IFDEF RICHVIEWDEF3}
    property VAlign         : TTextLayout           read FVAlign          write FVAlign default tlTop;
    {$ENDIF}

    property OnRVDblClick   : TRVDblClickEvent      read FOnRVDblClick    write FOnRVDblClick;
    property OnCheckpointVisible: TRVCheckpointVisibleEvent read FOnCheckpointVisible write FOnCheckpointVisible;
    property OnControlAction: TRVControlActionEvent read FOnControlAction write FOnControlAction;
    property OnItemAction   : TRVItemActionEvent    read FOnItemAction    write FOnItemAction;
    property OnCopy         : TNotifyEvent          read FOnCopy          write FOnCopy;
    {$IFDEF RV_ODHC}
    property OnDocumentHeightChange: TNotifyEvent read FOnDocumentHeightChange write FOnDocumentHeightChange;
    {$ENDIF}
    property OnImportPicture: TRVImportPictureEvent read FOnImportPicture write FOnImportPicture;
    property OnItemHint: TRVItemHintEvent           read FOnItemHint      write FOnItemHint;
    property OnJump         : TJumpEvent            read FOnJump          write FOnJump;
    property OnHTMLSaveImage: TRVHTMLSaveImageEvent read FOnHTMLSaveImage write FOnHTMLSaveImage;
    property OnSaveImage2: TRVSaveImageEvent2       read FOnSaveImage2    write FOnSaveImage2;
    property OnReadHyperlink: TRVReadHyperlink      read FOnReadHyperlink write FOnReadHyperlink;
    property OnWriteHyperlink: TRVWriteHyperlink    read FOnWriteHyperlink write FOnWriteHyperlink;    
    property OnURLNeeded    : TRVURLNeededEvent     read FOnURLNeeded     write FOnURLNeeded;
    property OnRVMouseDown  : TRVMouseEvent         read FOnRVMouseDown   write FOnRVMouseDown;
    property OnRVMouseMove  : TRVMouseMoveEvent     read FOnRVMouseMove   write FOnRVMouseMove;
    property OnRVMouseUp    : TRVMouseEvent         read FOnRVMouseUp     write FOnRVMouseUp;
    property OnRVRightClick : TRVRightClickEvent    read FOnRVRightClick  write FOnRVRightClick;
    property OnRVFControlNeeded: TRVFControlNeededEvent read FOnRVFControlNeeded write FOnRVFControlNeeded;
    property OnRVFImageListNeeded: TRVFImageListNeededEvent read FOnRVFImageListNeeded write FOnRVFImageListNeeded;
    property OnRVFPictureNeeded: TRVFPictureNeededEvent read FOnRVFPictureNeeded write FOnRVFPictureNeeded;
    property OnSaveComponentToFile: TRVSaveComponentToFileEvent read FOnSaveComponentToFile write FOnSaveComponentToFile;
    property OnSaveItemToFile: TRVSaveItemToFileEvent read FOnSaveItemToFile write FOnSaveItemToFile;
    property OnSelect       : TNotifyEvent          read FOnSelect        write FOnSelect;
    property OnSaveRTFExtra: TRVSaveRTFExtraEvent   read FOnSaveRTFExtra  write FOnSaveRTFExtra;
    property OnSaveHTMLExtra: TRVSaveHTMLExtraEvent read FOnSaveHTMLExtra write FOnSaveHTMLExtra;
    property OnSaveParaToHTML: TRVSaveParaToHTMLEvent read FOnSaveParaToHTML write FOnSaveParaToHTML;
    property OnProgress: TRVProgressEvent           read FOnProgress      write FOnProgress;
  end;

  { -------------------------------------------------------------------------- }
  { TRichView: visual component, document viewer.
    Component Palette page: "RichView".
    This class publishes many inherited properties.                            }

  TRichView = class (TCustomRichView)
  published
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
    property UseXPThemes;
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
    property CPEventKind default cpeNone;
    property Cursor default crDefault;
    property Delimiters;
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
    property TabNavigation;
    property TopMargin;
    property Tracking;
    {$IFDEF RICHVIEWDEF3}
    property VAlign;
    {$ENDIF}
    property VScrollVisible;
    {$IFDEF RICHVIEWDEF4}
    property WheelStep;
    {$ENDIF}
    { Published RichView events }
    property OnCheckpointVisible;
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

{------------------------------------------------------------------------------}
implementation
uses ShellApi, PtblRV,
     {$IFNDEF RVDONOTUSELISTS}
     RVMarker,
     {$ENDIF}
     RVStr;
{============================= TCustomRichView ======================================}
{$IFDEF RVDEBUG}{$I Debug\Decl.inc}{$ENDIF}
constructor TCustomRichView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  RVData := GetDataClass.Create(Self);
  Cursor         := crDefault;
  Color          := clNone;
  FLeftMargin    := 5;
  FRightMargin   := 5;
  FTopMargin     := 5;
  FBottomMargin  := 5;
  FMaxTextWidth  := 0;
  {$IFDEF RVDEBUG}{$I Debug\l.inc}{$ENDIF}
  FMinTextWidth  := 0;
  FStyle         := nil;

  Background     := TRVBackground.Create(True);
  Background.Bitmap.OnChange := DoOnBackBitmapChange;
  Width          := 100;
  Height         := 40;
  Flags          := [rvflUseJumps, rvflTrim, {rvflUseExternalLeading,} rvflRoot];
  FDelimiters    := RVDEFAULTDELEMITERS;
  ScrollTimerActive := False;
  FOptions       := [rvoAllowSelection, rvoScrollToEnd, rvoAutoCopyText,
                     rvoAutoCopyImage, rvoAutoCopyRVF, rvoAutoCopyRTF,
                     rvoDblClickSelectsWord, rvoRClickDeselects,
                     rvoFormatInvalidate,
                     rvoShowPageBreaks];
  FRVFOptions    := [rvfoSavePicturesBody, rvfoSaveControlsBody, rvfoSaveBinary,
    rvfoSaveDocProperties, rvfoLoadDocProperties];
  FRTFOptions    := [rvrtfDuplicateUnicode, rvrtfSaveEMFAsWMF, rvrtfSaveJpegAsJpeg];
  BorderStyle    := bsSingle;
  FRVFTextStylesReadMode := rvf_sInsertMerge;
  FRVFParaStylesReadMode := rvf_sInsertMerge;
  FDocProperties := TStringList.Create;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetDataClass: TRichViewRVDataClass;
begin
  Result := TRichViewRVData;
end;
{------------------------------------------------------------------------------}
destructor TCustomRichView.Destroy;
begin
  Destroying;
  Background.Free;
  Clear;
  RVData.Free;
  RTFReadProperties := nil;
  FDocProperties.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.GetTabOrderList(List: TList);
var i: Integer;
begin
  inherited GetTabOrderList(List);
  if TabNavigation<>rvtnNone then begin
    for i := List.Count-1 downto 2 do
      if TWinControl(List[i]).Parent=Self then
        List.Insert(i,Self);
    if List.Count>1 then
      List.Add(Self);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent=FStyle) then begin
      Style := nil;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.WMSize(var Message: TWMSize);
begin
  Format_(True, False, Canvas, False, False);
  inherited;
//  if Assigned(FOnResized) then FOnResized(Self);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.Format;
begin
  Format_(False, True, Canvas, False, True);
  if rvoFormatInvalidate in Options then Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.Reformat;
begin
  Format_(True, True, Canvas, False, True);
  if rvoFormatInvalidate in Options then Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.FormatTail;
begin
  Format_(False, True, Canvas, True, True);
  if rvoFormatInvalidate in Options then Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.ClearTemporal;
begin
  DeactivateScrollTimer;
  RVData.ClearTemporal;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.Deselect;
begin
  RVData.Deselect(nil, True);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SelectAll;
begin
  RVData.SelectAll;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetOffsBeforeItem(ItemNo: Integer): Integer;
begin
  Result := RVData.GetOffsBeforeItem(ItemNo);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetOffsAfterItem(ItemNo: Integer): Integer;
begin
  Result := RVData.GetOffsAfterItem(ItemNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.Clear;
begin
  ClearTemporal;
  RVData.Clear;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetFirstJumpNo: Integer;
begin
  Result := RVData.FirstJumpNo;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetFirstJumpNo(Value: Integer);
begin
  RVData.FirstJumpNo := Value;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetDocProperties(const Value: TStringList);
begin
  FDocProperties.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddNLTag(const s: String; StyleNo, ParaNo, Tag: Integer);
begin
  RVData.AddNLTag(s, StyleNo, ParaNo, Tag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddItem(const Text: String; Item: TCustomRVItemInfo);
begin
  RVData.AddItem(Text, Item);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddNL(const s: String; StyleNo, ParaNo: Integer);
begin
  RVData.AddNL(s, StyleNo, ParaNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddFmt(const FormatStr: String; const Args: array of const;
                           StyleNo, ParaNo: Integer);
begin
  RVData.AddFmt(FormatStr, Args, StyleNo, ParaNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddFromNewLine(const s: String; StyleNo:Integer);
begin
  RVData.AddNLTag(s, StyleNo, 0, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddTag(const s: String; StyleNo,Tag:Integer);
begin
  RVData.AddNLTag(s, StyleNo, -1, Tag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.Add(const s: String; StyleNo:Integer);
begin
  RVData.AddNLTag(s, StyleNo, -1, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddText(const s: String;StyleNo:Integer);
begin
  RVData.AddTextNL(s, StyleNo, -1, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddTextFromNewLine(const s: String;StyleNo:Integer);
begin
  RVData.AddTextNL(s, StyleNo, 0, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddTextNL(const s: String; StyleNo, FirstParaNo, OtherParaNo : Integer);
begin
  RVData.AddTextNL(s, StyleNo, FirstParaNo, OtherParaNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddTextBlockNL(s: String; StyleNo, ParaNo: Integer);
begin
  RVData.AddTextBlockNL(s, StyleNo, ParaNo);
end;
{------------------------------------------------------------------------------}
{
procedure TCustomRichView.AddTextNLTag(s: String; StyleNo, ParaNo, Tag: Integer);
begin

end;
}
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddCenterLine(const s: String;StyleNo:Integer);
begin
  RVData.AddNLTag(s, StyleNo, 1, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddBreakExTag(Width: Byte; Style: TRVBreakStyle; Color: TColor; Tag: Integer);
begin
  RVData.AddBreakExTag(Width,Style,Color,Tag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddBreakEx(Width: Byte; Style: TRVBreakStyle; Color: TColor);
begin
  RVData.AddBreakEx(Width,Style,Color);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddBreakTag(Tag: Integer);
begin
  RVData.AddBreakTag(Tag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddBreak;
begin
  RVData.AddBreakTag(0);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.AddNamedCheckpointExTag(const CpName: String;
                                           RaiseEvent: Boolean;
                                           Tag: Integer): Integer;
begin
  Result := RVData.AddNamedCheckpointExTag(CpName, RaiseEvent, Tag);
end;
{------------------------------------------------------------------------------}
{
function TCustomRichView.AddNamedCheckpointTag(const CpName: String; Tag: Integer): Integer;
begin
  Result := AddNamedCheckpointExTag(CpName, False, Tag);
end;
}
{------------------------------------------------------------------------------}
function TCustomRichView.AddNamedCheckpointEx(const CpName: String;
                                           RaiseEvent: Boolean): Integer;
begin
  Result := RVData.AddNamedCheckpointExTag(CpName, RaiseEvent, 0);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.AddNamedCheckpoint(CpName: String): Integer;
begin
  Result := RVData.AddNamedCheckpointExTag(CpName, False, 0);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.AddCheckpointTag(Tag: Integer): Integer;
begin
  Result := RVData.AddNamedCheckpointExTag('',False,Tag);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.AddCheckpoint: Integer;
begin
  Result := RVData.AddNamedCheckpointExTag('',False,0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddHotspotExTag(const Name: String;
                                    ImageIndex, HotImageIndex: Integer;
                                    ImageList: TCustomImageList;
                                    ParaNo, Tag: Integer);
begin
  RVData.AddHotspotExTag(Name, ImageIndex, HotImageIndex, ImageList, ParaNo, Tag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddHotspotEx(const Name: String;
                                 ImageIndex, HotImageIndex: Integer;
                                 ImageList: TCustomImageList;
                                 ParaNo: Integer);
begin
  RVData.AddHotspotEx(Name, ImageIndex, HotImageIndex, ImageList, ParaNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddHotspot(ImageIndex: Integer; ImageList: TCustomImageList;
                               fromnewline: Boolean);
begin
  if FromNewLine then
    RVData.AddHotspotExTag('', ImageIndex, ImageIndex, ImageList, 0, 0)
  else
    RVData.AddHotspotExTag('', ImageIndex, ImageIndex, ImageList, -1, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddBulletExTag(const Name: String; ImageIndex: Integer;
                                   ImageList: TCustomImageList;
                                   ParaNo, Tag: Integer);
begin
  RVData.AddBulletExTag(Name, ImageIndex, ImageList, ParaNo, Tag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddBulletEx(const Name: String; ImageIndex: Integer;
                                ImageList: TCustomImageList;
                                ParaNo: Integer);
begin
  RVData.AddBulletEx(Name, ImageIndex, ImageList, ParaNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddBullet(ImageIndex: Integer; ImageList: TCustomImageList; fromnewline: Boolean);
begin
  if FromNewLine then
    RVData.AddBulletExTag('', ImageIndex, ImageList, 0, 0)
  else
    RVData.AddBulletExTag('', ImageIndex, ImageList, -1, 0);
end;
{------------------------------------------------------------------------------}
{ "gr" does not copied, do not free it!}
procedure TCustomRichView.AddPictureExTag(const Name: String; gr: TGraphic;
                                    ParaNo: Integer; VAlign: TRVVAlign; Tag: Integer);
begin
  RVData.AddPictureExTag(Name, gr, ParaNo, VAlign, Tag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddPictureEx(const Name: String; gr: TGraphic; ParaNo: Integer; VAlign: TRVVAlign); { gr does not copied, do not free it!}
begin
  RVData.AddPictureExTag(Name, gr, ParaNo, VAlign, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddHotPictureTag(const Name: String; gr: TGraphic;
                                    ParaNo: Integer; VAlign: TRVVAlign; Tag: Integer);
begin
  RVData.AddHotPictureTag(Name, gr, ParaNo, VAlign, Tag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddHotPicture(const Name: String; gr: TGraphic; ParaNo: Integer;
                           VAlign: TRVVAlign);
begin
  RVData.AddHotPictureTag(Name, gr, ParaNo, VAlign, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddPicture(gr: TGraphic); { gr not copied, do not free it!}
begin
  RVData.AddPictureExTag('', gr, 1, rvvaBaseline, 0);
end;
{------------------------------------------------------------------------------}
{ do not free ctrl yourself! }
procedure TCustomRichView.AddControlExTag(const Name: String; ctrl: TControl;
                                    ParaNo: Integer; VAlign: TRVVAlign; Tag: Integer);
begin
  RVData.AddControlExTag(Name, ctrl, ParaNo, VAlign, Tag);
end;
{------------------------------------------------------------------------------}
{ do not free ctrl yourself!                                            }
procedure TCustomRichView.AddControlEx(const Name: String; ctrl: TControl; ParaNo: Integer;
                                 VAlign: TRVVAlign);
begin
  RVData.AddControlEx(Name, ctrl, ParaNo, VAlign);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddControl(ctrl: TControl; center: Boolean); { do not free ctrl! }
begin
  if Center then
    RVData.AddControlExTag('',ctrl, 1, rvvaBaseline, 0)
  else
    RVData.AddControlExTag('',ctrl, 0, rvvaBaseline, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.Format_(OnlyResized,ForceFormat:Boolean; Canvas: TCanvas;
          OnlyTail, NoCaching: Boolean);
begin
   if VSmallStep = 0 then exit;
   if (csDesigning in ComponentState) then exit;
   {$IFDEF RVDEBUG}{$I Debug\g.inc}{$ENDIF}
   RVData.Format_(OnlyResized, ForceFormat, False, 0, Canvas, OnlyTail, NoCaching);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetFirstItemVisible: Integer;
begin
  Result := RVData.GetFirstItemVisible;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetLastItemVisible: Integer;
begin
  Result := RVData.GetLastItemVisible;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.Paint;
  {.......................................................}
    procedure DrawDesignInfo(const msg: String);
    var r: TRect;
    begin
      Canvas.Brush.Color := GetColor;
      Canvas.Brush.Style := bsSolid;
      if Ctl3d then
        Canvas.Pen.Color := clWindow
      else
        Canvas.Pen.Color := clWindowText;
      Canvas.Font.Color := clWindowText;
      Canvas.Font.Name := RVDEFAULTDESIGNFONT;
      Canvas.Font.Size := 8;
      Canvas.Font.Style := [];
      Canvas.FillRect(Canvas.ClipRect);
      r := ClientRect;
      DrawText(Canvas.Handle, PChar(msg), Length(msg),  r, DT_TOP or DT_WORDBREAK);
      if not Ctl3d then begin
        Canvas.Brush.Color := clWindowText;
        Canvas.FrameRect(ClientRect);
      end;
    end;
  {.......................................................}
var NeedXOR: Boolean;
begin
 if (csDesigning in ComponentState) then
   DrawDesignInfo(SysUtils.Format('%s %s (%s)', [ClassName, RVVersion, RVAddress]))
 else if not Assigned(FStyle) then
   DrawDesignInfo(SysUtils.Format(RVNOSTYLEMSG, [Name]))
 else begin
   NeedXOR := RVData.ClearXorDrawing;
   //if Assigned(FOnPaint) then
   //  FOnPaint(Self, Canvas, True);
   RVData.PaintBuffered;
   if Assigned(FOnPaint) then
     FOnPaint(Self, Canvas, False);
   if NeedXOR then
     RVData.XorDrawing;
 end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.CMMouseLeave(var Message: TMessage);
begin
  RVData.MouseLeave;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  if {RVData.CaptureMouseItem=nil} True then begin
    if Y<-20 then
      VScrollDelta := -10
    else if Y<0 then
      VScrollDelta := -1
    else if Y>ClientHeight+20 then
      VScrollDelta := 10
    else if Y>ClientHeight then
      VScrollDelta := 1
    else
      VScrollDelta := 0;

    if X<-20 then
      HScrollDelta := -10
    else if X<0 then
      HScrollDelta := -1
    else if X>ClientWidth+20 then
      HScrollDelta := 10
    else if X>ClientWidth then
      HScrollDelta := 1
    else
      HScrollDelta := 0;
    inherited MouseMove(Shift, X, Y);
  end;

  RVData.MouseMove(Shift, X, Y);
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWDEF5}
type
  TControlPopupMenuHack = class (TControl)
    public
      property PopupMenu;
  end;
{$ENDIF}
procedure TCustomRichView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
{$IFDEF RICHVIEWDEF5}
var p: TPoint;
    Handled: Boolean;
    RootRichView: TCustomRichView;
    Menu: TPopupMenu;

    function GetPopupMenu: TPopupMenu;
    var ctrl: TControl;
    begin
      Result := nil;
      ctrl := Self;
      while (ctrl<>nil) do begin
        Result := TControlPopupMenuHack(ctrl).PopupMenu;
        if Result<>nil then
          exit;
        ctrl := ctrl.Parent;
      end;
    end;

{$ENDIF}
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Button=mbLeft) then
    DeactivateScrollTimer;
  RVData.MouseUp(Button, Shift, X, Y);
  {$IFDEF RICHVIEWDEF5}
  RootRichView := TCustomRichView(RVData.GetAbsoluteRootData.GetParentControl);
  if not (csDesigning in ComponentState) and (Button=mbRight) then begin
    Handled := False;
    p.X := X;
    p.Y := Y;
    p := ClientToScreen(p);
    RootRichView.DoContextPopup(RootRichView.ScreenToClient(p), Handled);
    if not Handled then begin
      Menu := GetPopupMenu;
      if (Menu<>nil) and Menu.AutoPopup then begin
        SendCancelMode(nil);
        Menu.PopupComponent := Self;
        Menu.Popup(p.X, p.Y);
      end;
    end;
  end;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWDEF5}
type
  TControlHack = class (TControl)
    public
      function GetPopupMenu: TPopupMenu; override;
  end;

  function TControlHack.GetPopupMenu: TPopupMenu;
  begin
    Result := inherited GetPopupMenu;
  end;

procedure TCustomRichView.WMContextMenu(var Message: TWMContextMenu);
var Ctrl: TControl;
begin
  if (csDesigning in ComponentState) then begin
    inherited;
    exit;
  end;
  if (Message.XPos=-1) and (Message.YPos=-1) then begin
    if RVData.GetAbsoluteRootData.GetParentControl=Self then
      inherited
    else
      with Message do
        RVData.GetAbsoluteRootData.GetParentControl.Perform(Msg, hWnd, -1)
  end;
  Ctrl := ControlAtPos(ScreenToClient(SmallPointToPoint(Message.Pos)), False);
  if (Ctrl<>nil) and (TControlHack(Ctrl).GetPopupMenu<>GetPopupMenu) then
    inherited;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TCustomRichView.SelectNext_(GoForward: Boolean);
begin
  SelectNext(nil,GoForward,True);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if InplaceEditor=nil then begin
    Include(RVData.State,rvstDoNotTab);
    if not (ssDouble in Shift) then
      Windows.SetFocus(Handle);
  end;
  RVData.MouseDown(Button, Shift, X, Y);
  inherited MouseDown(Button, Shift, X, Y);  
  if (Button=mbLeft) then
    ActivateScrollTimer(False);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.ActivateScrollTimer(Slow: Boolean);
var Interval: Integer;
begin
  if not ScrollTimerActive then begin
    if Slow then
      Interval := 300
    else
      Interval := 100;
    SetTimer(Handle,1,Interval,nil);
    ScrollTimerActive := True;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.DeactivateScrollTimer;
begin
  if ScrollTimerActive then begin
    if HandleAllocated then
      KillTimer(Handle,1);
    ScrollTimerActive := False;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.CompareTags(Tag1, Tag2: Integer): Boolean;
begin
  if (rvoTagsArePChars in Options) then
    if (Tag1=0) then
      if (Tag2=0) then
        Result := True
      else
        Result := False
    else
      if (Tag2=0) then
        Result := False
      else
        Result := StrComp(PChar(Tag1),PChar(Tag2))=0
  else
    Result := Tag1=Tag2;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AppendFrom(Source: TCustomRichView);
begin
  RVData.AppendFrom(Source.RVData);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetBackBitmap: TBitmap;
begin
  Result := Background.Bitmap;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetBackBitmap(Value: TBitmap);
begin
  Background.Bitmap.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.DoOnBackBitmapChange(Sender: TObject);
begin
  if not RVData.UpdatingBAckgroundPalette then begin
    FullRedraw := Background.ScrollRequiresFullRedraw;//or (RVData.FZoomPercent<>100);
    RVData.UpdateBackgroundPaletteInfo(Background);
    if rvoFormatInvalidate in Options then Invalidate;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetBackgroundStyle: TBackgroundStyle;
begin
  Result := Background.Style;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetBackgroundStyle(Value: TBackgroundStyle);
begin
  Background.Style := Value;
  DoOnBackBitmapChange(nil);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetColor: TColor;
begin
  {$IFDEF RVDEBUG}{$I Debug\e.inc}{$ENDIF}
  if Color<>clNone then
    Result := Color
  else if Assigned(FStyle) then
    Result := FStyle.Color
  else
    Result := clWindow;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetHoverColor(Color: TColor):TColor;
begin
  if Color<>clNone then
    Result := Color
  else
    Result := FStyle.HoverColor;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetVSmallStep(Value: Integer);
begin
   if (Value<=0) or (VScrollVisible and (DocumentHeight div Value > 32000)) then
     exit;
   inherited SetVSmallStep(Value);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetBiDiModeRV(const Value: TRVBiDiMode);
begin
  if Value<>BiDiMode then begin
    inherited SetBiDiModeRV(Value);
    Format;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SelectWordAt(X,Y: Integer);
begin
  RVData.SelectWordAt(X, Y);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.DblClick;
begin
  inherited DblClick;
  RVData.DblClick;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.DeleteSection(const CpName: String);
begin
  RVData.DeleteSection(CpName);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.DeleteItems(FirstItemNo, Count: Integer);
begin
  RVData.DeleteItems(FirstItemNo, Count);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.DeleteParas(FirstItemNo, LastItemNo: Integer);
begin
  RVData.DeleteParas(FirstItemNo, LastItemNo);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetLineCount: Integer;
begin
  Result := RVData.Items.Count;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SelectionExists: Boolean;
begin
  Result := RVData.SelectionExists(True, True);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetSelectedImage: TGraphic;
begin
  Result := RVData.GetSelectedImage;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetSelText: String;
begin
  Result := RVData.GetSelText(False);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.CopyText;
begin
  RVData.CopyText;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.CopyTextW;
begin
  RVData.CopyTextW;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.CopyImage;
begin
  RVData.CopyImage;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.Copy;
begin
  RVData.Copy(GetColor, Background);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.IsCopyShortcut(Shift: TShiftState; Key: Word): Boolean;
begin
  Result := (ssCtrl in Shift) and ((Key = ord('C')) or (Key = VK_INSERT)) and not (ssAlt in Shift);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.IsCutShortcut(Shift: TShiftState; Key: Word): Boolean;
begin
  Result := ((ssCtrl in Shift) and (Key = ord('X')) and not (ssAlt in Shift)) or
            ((ssShift in Shift) and (Key = VK_DELETE));
end;
{------------------------------------------------------------------------------}
function TCustomRichView.IsPasteShortcut(Shift: TShiftState; Key: Word): Boolean;
begin
  Result := ((ssCtrl in Shift) and (Key = ord('V')) and not (ssAlt in Shift)) or
            ((ssShift in Shift) and (Key = VK_INSERT));
end;
{------------------------------------------------------------------------------}
function TCustomRichView.CopyDef: Boolean;
begin
  Result := RVData.CopyDef(GetColor, Background);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.WMCopy(var Message: TWMCopy);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (InplaceEditor<>nil) then begin
    PostMessage(InplaceEditor.Handle, WM_COPY, 0, 0);
    exit;
  end;
  {$ENDIF}
  CopyDef;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key,Shift);
  if IsCopyShortCut(Shift, Key) then
    SendMessage(Handle, WM_COPY, 0, 0);
  if (TabNavigation<>rvtnNone) and
     ((Key=VK_TAB) and not (ssAlt in Shift) and
     (((ssCtrl in Shift) and (TabNavigation=rvtnCtrlTab)) or
       (not (ssCtrl in Shift) and (TabNavigation=rvtnTab)))) then begin
    Exclude(RVData.State,rvstDoNotTab);
    RVData.DoTabNavigation(ssShift in Shift, Self);
  end;
  if (Key=VK_RETURN) then
    RVData.ExecuteFocused;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.GenerateMouseMove;
var State: TShiftState;
    KeyboardState: TKeyboardState;
    p: TPoint;
begin
  State := [];
  GetKeyboardState(KeyboardState);
  if KeyboardState[VK_SHIFT] and $80 <> 0 then Include(State, ssShift);
  if KeyboardState[VK_CONTROL] and $80 <> 0 then Include(State, ssCtrl);
  if KeyboardState[VK_MENU] and $80 <> 0 then Include(State, ssAlt);
  if KeyboardState[VK_LBUTTON] and $80 <> 0 then Include(State, ssLeft);
  if KeyboardState[VK_RBUTTON] and $80 <> 0 then Include(State, ssRight);
  if KeyboardState[VK_MBUTTON] and $80 <> 0 then Include(State, ssMiddle);
  GetCursorPos(p);
  p := ScreenToClient(p);
  if (p.X>=0) and (p.X<ClientWidth) and
     (p.Y>=0) and (p.Y<ClientHeight) then
    MouseMove(State,p.X,p.Y);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.WMTimer(var Message: TWMTimer);
begin
  case Message.TimerID of
    1:
      begin
        //if RVData.CaptureMouseItem<>nil then exit;
        if VScrollDelta<>0 then
          VScrollPos := VScrollPos+VScrollDelta;
        if HScrollDelta<>0 then
          SetHPos(HPos+HScrollDelta);
        if (HScrollDelta<>0) or (VScrollDelta<>0) then begin
          {$IFDEF RVDEBUG}{$I Debug\f.inc}{$ENDIF}
          RVData.OnTimerScroll;
          GenerateMouseMove;
        end;
      end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.Loaded;
begin
  inherited Loaded;
  UpdatePaletteInfo;
  Format;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.InplaceRedrawing(AllowRedrawItself: Boolean);
begin
  if AllowRedrawItself then
    Refresh;
  if not FullRedraw or (InplaceEditor=nil) or not (InplaceEditor is TCustomRichView) then
    exit;
  TCustomRichView(InplaceEditor).InplaceRedrawing(True);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetAllowSelection: Boolean;
begin
  Result := rvoAllowSelection in Options;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetSingleClick: Boolean;
begin
  Result := rvoSingleClick in Options;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetAllowSelection(const Value: Boolean);
begin
  if Value then
    Include(FOptions, rvoAllowSelection)
  else
    Exclude(FOptions, rvoAllowSelection)
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetSingleClick(const Value: Boolean);
begin
  if Value then
    Include(FOptions, rvoSingleClick)
  else
    Exclude(FOptions, rvoSingleClick)
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetTabNavigation:TRVTabNavigationType;
begin
  Result := RVData.TabNavigation;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetTabNavigation(const Value: TRVTabNavigationType);
begin
  RVData.TabNavigation := Value;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SearchText(const s: String; SrchOptions: TRVSearchOptions): Boolean;
begin
  Result := RVData.SearchText(rvsroDown in SrchOptions, rvsroMatchCase in SrchOptions,
                              rvsroWholeWord in SrchOptions, s);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetItemStyle(ItemNo: Integer): Integer;
begin
  Result := RVData.GetItemStyle(ItemNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.GetBreakInfo(ItemNo: Integer; var AWidth: Byte;
                            var AStyle: TRVBreakStyle; var AColor: TColor;
                            var ATag: Integer);
begin
  RVData.GetBreakInfo(ItemNo, AWidth, AStyle, AColor, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.GetBulletInfo(ItemNo: Integer; var AName: String;
                                  var AImageIndex: Integer;
                                  var AImageList: TCustomImageList;
                                  var ATag: Integer);
begin
  RVData.GetBulletInfo(ItemNo, AName, AImageIndex, AImageList, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.GetHotspotInfo(ItemNo: Integer; var AName: String;
                                  var AImageIndex, AHotImageIndex: Integer;
                                  var AImageList: TCustomImageList;
                                  var ATag: Integer);
begin
  RVData.GetHotspotInfo(ItemNo, AName, AImageIndex, AHotImageIndex,AImageList, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.GetPictureInfo(ItemNo: Integer; var AName: String;
                                   var Agr: TGraphic; var AVAlign: TRVVAlign;
                                   var ATag: Integer);
begin
  RVData.GetPictureInfo(ItemNo, AName, Agr, AVAlign, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.GetControlInfo(ItemNo: Integer; var AName: String;
                                   var Actrl: TControl; var AVAlign: TRVVAlign;
                                   var ATag: Integer);
begin
  RVData.GetControlInfo(ItemNo, AName, Actrl, AVAlign, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.GetTextInfo(ItemNo: Integer; var AText: String;
                                var ATag: Integer);
begin
  RVData.GetTextInfo(ItemNo, AText, ATag);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetItemTag(ItemNo: Integer): Integer;
begin                                     
  Result := RVData.GetItemTag(ItemNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetBreakInfo(ItemNo: Integer; AWidth: Byte;
                            AStyle: TRVBreakStyle; AColor: TColor;
                            ATag: Integer);
begin
  RVData.SetBreakInfo(ItemNo, AWidth, AStyle, AColor, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetBulletInfo(ItemNo: Integer; const AName: String;
                                  AImageIndex: Integer;
                                  AImageList: TCustomImageList;
                                  ATag: Integer);
begin
  RVData.SetBulletInfo(ItemNo, AName, AImageIndex, AImageList, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetHotspotInfo(ItemNo: Integer; const AName: String;
                                   AImageIndex, AHotImageIndex: Integer;
                                   AImageList: TCustomImageList;
                                   ATag: Integer);
begin
  RVData.SetHotspotInfo(ItemNo, AName, AImageIndex, AHotImageIndex,
                        AImageList, ATag);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SetPictureInfo(ItemNo: Integer; const AName: String;
                                  Agr: TGraphic; AVAlign: TRVVAlign;
                                  ATag: Integer): Boolean;
begin
  Result := RVData.SetPictureInfo(ItemNo, AName, Agr, AVAlign, ATag);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SetControlInfo(ItemNo: Integer; const AName: String;
                                  AVAlign: TRVVAlign; ATag: Integer): Boolean;
begin
  Result := RVData.SetControlInfo(ItemNo, AName, AVAlign, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetItemTag(ItemNo: Integer; ATag: Integer);
begin
  RVData.SetItemTag(ItemNo, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetItemText(ItemNo: Integer; const s: String);
begin
  RVData.Items[ItemNo] := s;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetItemText(ItemNo: Integer): String;
begin
  Result := RVData.Items[ItemNo];
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetCheckpointInfo(ItemNo: Integer; ATag: Integer; const AName: String;
                                      ARaiseEvent: Boolean);
begin
  RVData.SetCheckpointInfo(ItemNo, ATag, AName, ARaiseEvent);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.RemoveCheckpoint(ItemNo: Integer):Boolean;
begin
  Result := RVData.RemoveCheckpoint(ItemNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetStyle(Value: TRVStyle);
begin
  FStyle := Value;
  if (csDesigning in ComponentState) or
     (rvoFormatInvalidate in Options) then Invalidate;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.FindControlItemNo(actrl: TControl): Integer;
begin
  Result := RVData.FindControlItemNo(actrl);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SelectControl(actrl: TControl): Boolean;
begin
  Result := RVData.SelectControl(actrl);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AfterHScroll;
begin
  inherited AfterHScroll;
  GenerateMouseMove;
  InplaceRedrawing(False);  
  {$IFDEF RVDEBUG}{$I Debug\a.inc}{$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AfterVScroll;
begin
  {$IFDEF RVDEBUG}{$I Debug\b.inc}{$ENDIF}
  inherited AfterVScroll;
  GenerateMouseMove;
  RVData.AfterVScroll;
  InplaceRedrawing(False);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.GetSelectionBounds(var StartItemNo, StartItemOffs,
                                 EndItemNo, EndItemOffs: Integer;
                                 Normalize: Boolean);
begin
  RVData.GetSelectionBounds(StartItemNo, StartItemOffs, EndItemNo, EndItemOffs,
                            Normalize);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetSelectionBounds(StartItemNo, StartItemOffs,
                                 EndItemNo, EndItemOffs: Integer);
begin
  RVData.SetSelectionBounds(StartItemNo, StartItemOffs, EndItemNo, EndItemOffs);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.IsFromNewLine(ItemNo: Integer): Boolean;
begin
  Result := RVData.IsFromNewLine(ItemNo);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.IsParaStart(ItemNo: Integer): Boolean;
begin
  Result := RVData.IsParaStart(ItemNo);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetItemPara(ItemNo: Integer): Integer;
begin
  Result := RVData.GetItemPara(ItemNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.GetWordAt(X,Y: Integer;
  var RVData: TCustomRVFormattedData;  var ItemNo: Integer; var Word: String);
begin
  Self.RVData.GetWordAt(X, Y, RVData, ItemNo, Word);
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWDEF4}
function TCustomRichView.GetWordAt(X,Y: Integer): String;
var ARVData: TCustomRVFormattedData;
    AItemNo: Integer;
begin
  RVData.GetWordAt(X, Y, ARVData, AItemNo, Result);
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TCustomRichView.WMDestroy(var Message: TWMDestroy);
begin
  DeactivateScrollTimer;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
  if TabNavigation<>rvtnNone then begin
    Message.Result := Message.Result or DLGC_WANTTAB;
    if RVData.TopLevelFocusedRVData<>nil then
      Message.Result := Message.Result or DLGC_WANTALLKEYS; // wants ENTER
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.WMSetFocus(var Message: TWMSetFocus);
  {....................................}
  function GetFocusedControl(Handle: HWND): TWinControl;
  var NewHandle: HWND;
  begin
    Result := nil;
    if Handle=0 then
      exit;
    NewHandle := Windows.GetParent(Handle);
    if NewHandle=0 then begin
      Result := FindControl(Handle);
      exit;
    end;
    Result := FindControl(NewHandle);
    if not (Result is TComboBox) then
      Result := FindControl(Handle);
  end;
  {....................................}
begin
  inherited;
  if csDestroying in ComponentState then
    exit;
  if TabNavigation<>rvtnNone then begin
    if Message.FocusedWnd=0 then
      Include(RVData.State,rvstDoNotTab);
    RVData.DoTabNavigation(GetKeyState(VK_SHIFT)<0, GetFocusedControl(Message.FocusedWnd));
  end;
  if (Style<>nil) and
     ((Style.SelTextColor<>Style.InactiveSelTextColor) or
      (Style.SelColor<>Style.InactiveSelColor)) then
    Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if (Style<>nil) and
     ((Style.SelTextColor<>Style.InactiveSelTextColor) or
      (Style.SelColor<>Style.InactiveSelColor)) then
    Invalidate;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetSelectionRect: TRect;
begin
  Result := RVData.GetSelectionRect;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetPageBreaksBeforeItems(Index: Integer): Boolean;
begin
  Result := RVData.PageBreaksBeforeItems[Index];
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetPageBreaksBeforeItems(Index: Integer;  Value: Boolean);
begin
  RVData.PageBreaksBeforeItems[Index] := Value;
  if rvoShowPageBreaks in Options then
    Invalidate;  
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetAddParagraphMode(AllowNewPara: Boolean);
begin
  RVData.SetAddParagraphMode(AllowNewPara);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetDocumentHeight: Integer;
begin
  Result := RVData.DocumentHeight;
end;
{----------------------------- Checkpoints ------------------------------------}
function TCustomRichView.GetFirstCheckpoint: TCheckpointData;
begin
  Result := RVData.GetFirstCheckpoint;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetNextCheckpoint(CheckpointData: TCheckpointData): TCheckpointData;
begin
  Result := RVData.GetNextCheckpoint(CheckpointData);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetLastCheckpoint: TCheckpointData;
begin
  Result := RVData.GetLastCheckpoint;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetPrevCheckpoint(CheckpointData: TCheckpointData): TCheckpointData;
begin
  Result := RVData.GetPrevCheckpoint(CheckpointData);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.GetCheckpointInfo(CheckpointData: TCheckpointData;
                                var Tag: Integer; var Name: String;
                                var RaiseEvent: Boolean);
begin
  RVData.GetCheckpointInfo(CheckpointData, Tag, Name, RaiseEvent);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.GetCheckpointXY(CheckpointData: TCheckpointData; var X,Y: Integer);
begin
  RVData.GetCheckpointXY(CheckpointData, X, Y);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetCheckpointYEx(CheckpointData: TCheckpointData): Integer;
begin
  Result := RVData.GetCheckpointYEx(CheckpointData);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.FindCheckpointByName(const Name: String): TCheckpointData;
begin
  Result := RVData.FindCheckpointByName(Name);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.FindCheckpointByTag(Tag: Integer): TCheckpointData;
begin
  Result := RVData.FindCheckpointByTag(Tag);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetCheckpointNo(CheckpointData: TCheckpointData): Integer;
begin
  Result := RVData.GetCheckpointNo(CheckpointData);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetCheckpointByNo(No: Integer): TCheckpointData;
begin
  Result := RVData.GetCheckpointByNo(No);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetCheckpointItemNo(CheckpointData: TCheckpointData): Integer;
begin
  Result := RVData.GetCheckpointItemNo(CheckpointData);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetItemCheckpoint(ItemNo: Integer): TCheckpointData;
begin
  Result := RVData.GetItemCheckpoint(ItemNo);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetCheckpointY(no: Integer): Integer;
begin
  Result := RVData.GetCheckpointYEx(GetCheckpointByNo(No));
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetJumpPointY(id: Integer): Integer;
begin
  Result := RVData.GetJumpPointY(id);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetJumpPointItemNo(id: Integer): Integer;
begin
  Result := RVData.GetJumpPointItemNo(id);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.GetJumpPointLocation(id: Integer;
  var RVData: TCustomRVFormattedData; var ItemNo: Integer);
begin
  Self.RVData.GetJumpPointLocation(id, RVData, ItemNo);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetItemCoords(ItemNo: Integer;var Left,Top: Integer): Boolean;
begin
  Result := RVData.GetItemCoords(ItemNo, Left,Top);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetItemClientCoords(ItemNo: Integer;var Left,Top: Integer): Boolean;
begin
  Result := RVData.GetItemClientCoords(ItemNo, Left,Top);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERVF}
function TCustomRichView.CreateLayoutInfo: TRVLayoutInfo;
begin
  Result := TRVLayoutInfo.Create;
  Result.LeftMargin   := LeftMargin;
  Result.RightMargin  := RightMargin;
  Result.BottomMargin := BottomMargin;
  Result.TopMargin    := TopMargin;
  Result.MinTextWidth := MinTextWidth;
  Result.MaxTextWidth := MaxTextWidth;
  Result.BiDiMode     := BiDiMode;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.ApplyLayoutInfo(Layout: TRVLayoutInfo);
begin
  LeftMargin   := Layout.LeftMargin;
  RightMargin  := Layout.RightMargin;
  BottomMargin := Layout.BottomMargin;
  TopMargin    := Layout.TopMargin;
  MinTextWidth := Layout.MinTextWidth;
  MaxTextWidth := Layout.MaxTextWidth;
  BiDiMode     := Layout.BiDiMode;
end;
{-------------------------- Loading RVF ---------------------------------------}
function TCustomRichView.InsertRVFFromStream(Stream: TStream; Index: Integer):Boolean;
var AColor: TColor;
begin
  AColor := Color;
  Result := RVData.InsertRVFFromStream(Stream, Index, AColor, Background, nil, False);
  Color := AColor;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.AppendRVFFromStream(Stream: TStream; ParaNo: Integer):Boolean;
var AColor: TColor;
begin
  AColor := Color;
  Result := RVData.AppendRVFFromStream(Stream, ParaNo, AColor, Background);
  Color := AColor;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.LoadRVFFromStream(Stream: TStream):Boolean;
var AColor: TColor;
   Layout: TRVLayoutInfo;
begin
  AColor := Color;
  Layout := TRVLayoutInfo.Create;
  Result := RVData.LoadRVFFromStream(Stream, AColor, Background, Layout);
  if Layout.Loaded then
    ApplyLayoutInfo(Layout);
  Layout.Free;
  Color := AColor;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.LoadRVF(const FileName: String):Boolean;
var AColor: TColor;
   Layout: TRVLayoutInfo;
begin
  AColor := Color;
  Layout := TRVLayoutInfo.Create;  
  Result := RVData.LoadRVF(FileName, AColor, Background, Layout);
  if Layout.Loaded then
    ApplyLayoutInfo(Layout);
  Layout.Free;
  Color := AColor;
end;
{--------------------------- Saving RVF ---------------------------------------}
function TCustomRichView.SaveRVFToStream(Stream: TStream; SelectionOnly: Boolean):Boolean;
var Layout: TRVLayoutInfo;
begin
  Layout := CreateLayoutInfo;
  Result := RVData.SaveRVFToStream(Stream, SelectionOnly, Color, Background, Layout);
  Layout.Free;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SaveRVF(const FileName: String; SelectionOnly: Boolean):Boolean;
var Layout: TRVLayoutInfo;
begin
  Layout := CreateLayoutInfo;
  Result := RVData.SaveRVF(FileName, SelectionOnly, Color, Background, Layout);
  Layout.Free;  
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.CopyRVF;
begin
  RVData.CopyRVF(Color, Background);
end;
{------------------------------------------------------------------------------}
{$ENDIF}
{--------------------------- Saving RTF ---------------------------------------}
{$IFNDEF RVDONOTUSERTF}
procedure TCustomRichView.CopyRTF;
begin
  RVData.CopyRTF(Color, Background);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SaveRTFToStream(Stream: TStream; SelectionOnly: Boolean):Boolean;
begin
  Result := RVData.SaveRTFToStream(Stream, SelectionOnly, 0, Color, Background,
                                   nil, nil, nil, nil, nil, 0.0);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SaveRTF(const FileName: String; SelectionOnly: Boolean):Boolean;
begin
  Result := RVData.SaveRTF(FileName, SelectionOnly, Color, Background);
end;
{$ENDIF}
{------------------------------- HTML methods ---------------------------------}
{$IFNDEF RVDONOTUSERTFIMPORT}
function TCustomRichView.LoadRTFFromStream(Stream: TStream):Boolean;
begin
  RTFReadProperties; // creating if nil
  Result := RVData.LoadRTFFromStream(Stream)=rtf_ec_OK;
end;
{------------------------------- HTML methods ---------------------------------}
function TCustomRichView.LoadRTF(const FileName: String):Boolean;
begin
  RTFReadProperties; // creating if nil
  Result := RVData.LoadRTF(FileName)=rtf_ec_OK;
end;
{$ENDIF}
{------------------------------- HTML methods ---------------------------------}
{$IFNDEF RVDONOTUSEHTML}
function TCustomRichView.SaveHTMLEx(const FileName, Title, ImagesPrefix, ExtraStyles,
                              ExternalCSS, CPPrefix: String;
                              Options: TRVSaveOptions):Boolean;
begin
  imgSavePrefix := ImagesPrefix;
  SaveOptions   := Options;
  imgSaveNo     := 0;
  Result := RVData.SaveHTMLEx( FileName, Title, ImagesPrefix,
                              ExtraStyles, ExternalCSS, CPPrefix,
                              Options, GetColor, CurrentFileColor, imgSaveNo,
                              LeftMargin, TopMargin, RightMargin, BottomMargin,
                              Background);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SaveHTMLToStreamEx(Stream: TStream;
                        const Path, Title, ImagesPrefix, ExtraStyles,
                        ExternalCSS, CPPrefix: String;
                        Options: TRVSaveOptions):Boolean;
begin
  imgSavePrefix := ImagesPrefix;
  SaveOptions   := Options;
  imgSaveNo     := 0;
  {$IFDEF RVDEBUG}{$I Debug\d.inc}{$ENDIF}
  Result := RVData.SaveHTMLToStreamEx(Stream, Path, Title, ImagesPrefix,
                                      ExtraStyles, ExternalCSS, CPPrefix,
                                      Options, GetColor, CurrentFileColor, imgSaveNo,
                                      LeftMargin, TopMargin,
                                      RightMargin, BottomMargin,
                                      Background,nil);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SaveHTML(const FileName,Title,ImagesPrefix: String;
                           Options: TRVSaveOptions):Boolean;
begin
  imgSavePrefix    := ImagesPrefix;
  CurrentFileColor := GetColor;
  SaveOptions      := Options;
  imgSaveNo        := 0;
  Result := RVData.SaveHTML(FileName, Title, ImagesPrefix, Options,
                            GetColor, imgSaveNo,
                            LeftMargin, TopMargin, RightMargin, BottomMargin,
                            Background);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SaveHTMLToStream(Stream: TStream; const Path, Title,ImagesPrefix: String;
                                    Options: TRVSaveOptions):Boolean;
begin
  imgSavePrefix    := ImagesPrefix;
  CurrentFileColor := GetColor;
  SaveOptions      := Options;
  imgSaveNo        := 0;
  Result := RVData.SaveHTMLToStream(Stream, Path, Title, ImagesPrefix, Options,
                            GetColor, imgSaveNo,
                            LeftMargin, TopMargin, RightMargin, BottomMargin,
                            Background,nil);
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TCustomRichView.SavePicture(DocumentSaveFormat: TRVSaveFormat;
                               const Path: String;
                               gr: TGraphic): String;
begin
  Result := RVData.SavePicture(DocumentSaveFormat, imgSavePrefix, Path,
    imgSaveNo, rvsoOverrideImages in SaveOptions, CurrentFileColor, gr);
end;
{----------------------------- Text Files -------------------------------------}
function TCustomRichView.LoadText(const FileName: String; StyleNo, ParaNo: Integer;
  AsSingleParagraph: Boolean):Boolean;
begin
  Result := RVData.LoadText(FileName, StyleNo, ParaNo,AsSingleParagraph);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.LoadTextFromStream(Stream: TStream; StyleNo,
  ParaNo: Integer; AsSingleParagraph: Boolean):Boolean;
begin
  Result := RVData.LoadTextFromStream(Stream, StyleNo, ParaNo,AsSingleParagraph);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SaveTextToStream(const Path: String; Stream: TStream;
  LineWidth: Integer; SelectionOnly, TextOnly: Boolean):Boolean;
begin
  Result := RVData.SaveTextToStream(Path, Stream, LineWidth, SelectionOnly,
    TextOnly, False, False);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SaveText(const FileName: String; LineWidth: Integer):Boolean;
begin
  Result := RVData.SaveText(FileName, LineWidth, False);
end;
{------------------------------- Unicode --------------------------------------}
{$IFNDEF RVDONOTUSEUNICODE}
function TCustomRichView.LoadTextW(const FileName: String; StyleNo, ParaNo: Integer;
  DefAsSingleParagraph: Boolean):Boolean;
begin
  Result := RVData.LoadTextW(FileName, StyleNo, ParaNo, DefAsSingleParagraph);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.LoadTextFromStreamW(Stream: TStream; StyleNo, ParaNo: Integer;
  DefAsSingleParagraph: Boolean):Boolean;
begin
  Result := RVData.LoadTextFromStreamW(Stream, StyleNo, ParaNo, DefAsSingleParagraph);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SaveTextW(const FileName: String; LineWidth: Integer):Boolean;
begin
  Result := RVData.SaveText(FileName, LineWidth, True);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.SaveTextToStreamW(const Path: String; Stream: TStream;
  LineWidth: Integer; SelectionOnly, TextOnly: Boolean):Boolean;
begin
  Result := RVData.SaveTextToStream(Path, Stream, LineWidth, SelectionOnly,
    TextOnly, True, False);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddNLATag(const s: String; StyleNo, ParaNo, Tag: Integer);
begin
  RVData.AddNLATag(s, StyleNo, ParaNo, Tag);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetItemTextA(ItemNo: Integer): String;
begin
  Result := RVData.GetItemTextA(ItemNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetItemTextA(ItemNo: Integer; const s: String);
begin
  RVData.SetItemTextA(ItemNo, s);
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
procedure TCustomRichView.AddTextNLW(const s: WideString;
  StyleNo, FirstParaNo, OtherParaNo: Integer;
  DefAsSingleParagraph: Boolean);
var ansis: String;
    LParaNo: Integer;
begin
  SetLength(ansis, Length(s)*2);
  Move(Pointer(s)^, Pointer(ansis)^, Length(ansis));
  LParaNo := FirstParaNo;
  if (Style<>nil) and (FirstParaNo=-1) and ((StyleNo<0) or (StyleNo=rvsDefStyle)) then begin
    if ItemCount>0 then
      LParaNo := GetItemPara(ItemCount-1)
    else
      LParaNo := 0;
  end;
  if (Style<>nil) and not Style.TextStyles[RVData.GetActualStyle2(StyleNo,LParaNo)].Unicode then begin
    ansis := RVU_UnicodeToAnsi(RVData.GetStyleCodePage(RVData.GetActualStyle2(StyleNo,LParaNo)), ansis);
    if DefAsSingleParagraph then
      AddTextBlockNL(ansis, StyleNo, FirstParaNo)
    else
      AddTextNL(ansis, StyleNo, FirstParaNo, OtherParaNo);
    end
  else
    RVData.AddTextNLW(ansis, StyleNo, FirstParaNo, OtherParaNo, DefAsSingleParagraph);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AddNLWTag(const s: WideString; StyleNo, ParaNo, Tag: Integer);
begin
  RVData.AddNLWTag(s, StyleNo, ParaNo, Tag);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetSelTextW: WideString;
var s: String;
begin
  s := RVData.GetSelText(True);
  SetLength(Result, Length(s) div 2);
  if Length(s)<>0 then
    Move(PChar(s)^, Pointer(Result)^, Length(s));
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetItemTextW(ItemNo: Integer): WideString;
begin
  Result := RVData.GetItemTextW(ItemNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetItemTextW(ItemNo: Integer;
  const s: WideString);
begin
  RVData.SetItemTextW(ItemNo, s);
end;
{$ENDIF}
{$ENDIF}
{--------------------------- Palette  support ---------------------------------}
procedure TCustomRichView.UpdatePaletteInfo;
begin
  inherited UpdatePaletteInfo;
  RVData.UpdateItemsPaletteInfo;
  RVData.UpdateBackgroundPaletteInfo(Background);
  Invalidate;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetItem(ItemNo: Integer): TCustomRVItemInfo;
begin
  Result := TCustomRVItemInfo(RVData.Items.Objects[ItemNo]);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetItemNo(Item: TCustomRVItemInfo): Integer;
begin
  Result := RVData.Items.IndexOfObject(Item);
end;

procedure TCustomRichView.GetFocusedItem(var ARVData: TCustomRVFormattedData; var AItemNo: Integer);
begin
  ARVData := RVData.TopLevelFocusedRVData;
  AItemNo := RVData.TopLevelFocusedItemNo;
end;

{$IFDEF RVDEBUG}{$I Debug\Decl2.inc}{$ENDIF}
{------------------------------------------------------------------------------}
function TCustomRichView.GetRTFReadProperties: TRVRTFReaderProperties;
begin
  if FRTFReadProperties=nil then
    FRTFReadProperties := TRVRTFReaderProperties.Create;
  Result := FRTFReadProperties;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetRTFReadProperties(const Value: TRVRTFReaderProperties);
begin
  if Value=nil then begin
    FRTFReadProperties.Free;
    FRTFReadProperties := nil;
    end
  else begin
    RTFReadProperties.Assign(Value);
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.RTFReaderAssigned: Boolean;
begin
  Result := FRTFReadProperties<>nil;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AssignEvents(Source: TCustomRichView);
begin
  OnSaveComponentToFile := Source.OnSaveComponentToFile;
  OnSaveItemToFile     := Source.OnSaveItemToFile;
  OnSaveParaToHTML     := Source.OnSaveParaToHTML;
  OnURLNeeded          := Source.OnURLNeeded;
  OnSelect             := Source.OnSelect;
  OnImportPicture      := Source.OnImportPicture;
  OnRVRightClick       := Source.OnRVRightClick;
  OnRVFPictureNeeded   := Source.OnRVFPictureNeeded;
  OnRVFImageListNeeded := Source.OnRVFImageListNeeded;
  OnRVFControlNeeded   := Source.OnRVFControlNeeded;
  OnRVDblClick         := Source.OnRVDblClick;
  OnJump               := Source.OnJump;
  OnRVMouseMove        := Source.OnRVMouseMove;
  OnCopy               := Source.OnCopy;
  OnRVMouseDown        := Source.OnRVMouseDown;
  OnRVMouseUp          := Source.OnRVMouseUp;
  OnControlAction      := Source.OnControlAction;
  OnItemAction         := Source.OnItemAction;
  OnClick              := Source.OnClick;
  OnKeyDown            := Source.OnKeyDown;
  OnKeyUp              := Source.OnKeyUp;
  OnKeyPress           := Source.OnKeyPress;
  OnHTMLSaveImage      := Source.OnHTMLSaveImage;
  OnSaveImage2         := Source.OnSaveImage2;
  OnDblClick           := Source.OnDblClick;
  OnItemHint           := Source.OnItemHint;
  // OnCheckpointVisible  := Source.OnCheckpointVisible;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.DeleteUnusedStyles(TextStyles, ParaStyles, ListStyles: Boolean);
begin
  RVData.DeleteUnusedStyles(TextStyles, ParaStyles, ListStyles);
end;
{------------------------------------------------------------------------------}
{ Gets item's property of integer type.
  ItemNo - index of item. Prop identifies the property. Value receives a
  property value.
  Returns true is this item type has this property                             }
function TCustomRichView.GetItemExtraIntProperty(ItemNo: Integer;
  Prop: TRVExtraItemProperty; var Value: Integer): Boolean;
begin
  Result := RVData.GetItemExtraIntProperty(ItemNo, Prop, Value);
end;
{------------------------------------------------------------------------------}
{ Sets item's property of integer type.
  ItemNo - index of item. Prop identifies the property. Value - new property
  value.
  Returns true is this item type has this property                             }
function TCustomRichView.SetItemExtraIntProperty(ItemNo: Integer;
  Prop: TRVExtraItemProperty; Value: Integer): Boolean;
begin
  Result := RVData.SetItemExtraIntProperty(ItemNo, Prop, Value);
end;
{------------------------------------------------------------------------------}
{ Gets item's property of string type.
  ItemNo - index of item. Prop identifies the property. Value receives a
  property value.
  Returns true is this item type has this property                             }
function TCustomRichView.GetItemExtraStrProperty(ItemNo: Integer;
  Prop: TRVExtraItemStrProperty; var Value: String): Boolean;
begin
  Result := RVData.GetItemExtraStrProperty(ItemNo, Prop, Value);
end;
{------------------------------------------------------------------------------}
{ Sets item's property of string type.
  ItemNo - index of item. Prop identifies the property. Value - new property
  value.
  Returns true is this item type has this property                             }
function TCustomRichView.SetItemExtraStrProperty(ItemNo: Integer;
  Prop: TRVExtraItemStrProperty; const Value: String): Boolean;
begin
  Result := RVData.SetItemExtraStrProperty(ItemNo, Prop, Value);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AssignSoftPageBreaks(RVPrint: TComponent);
begin
  RVData.AssignSoftPageBreaks(RVPrint as TCustomRVPrint);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.ClearSoftPageBreaks;
begin
  if RVData.ClearSoftPageBreaks then
    Invalidate;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSELISTS}
function TCustomRichView.SetListMarkerInfo(AItemNo, AListNo, AListLevel,
  AStartFrom, AParaNo: Integer; AUseStartFrom: Boolean): Integer;
begin
  Result := RVData.SetListMarkerInfo(AItemNo, AListNo, AListLevel, AStartFrom,
    AParaNo, AUseStartFrom)
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.RemoveListMarker(ItemNo: Integer);
begin
  RVData.RemoveListMarker(ItemNo);
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetListMarkerInfo(AItemNo: Integer; var AListNo,
  AListLevel, AStartFrom: Integer; var AUseStartFrom: Boolean): Integer;
begin
  Result := RVData.GetListMarkerInfo(AItemNo, AListNo, AListLevel, AStartFrom,
    AUseStartFrom);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.RefreshListMarkers;
var Markers: TRVMarkerList;
begin
  Markers := RVData.GetMarkers(False);
  if (Markers<>nil) and (Style<>nil) then
    Markers.RecalcDisplayStrings(Style);
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TCustomRichView.GetLineNo(ItemNo, ItemOffs: Integer): Integer;
begin
  Result := RVData.GetLineNo(ItemNo, ItemOffs);
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.WMRVEvent(var Message: TMessage);
var data: TRVMessageData;
begin
  data := TRVMessageData(Message.wParam);
  case data.Event of
    rvetDragDrop:
      with TRVDNDMessageData(data) do
        DragDrop(Obj,X,Y);
    rvetEndDrag:
      with TRVDNDMessageData(data) do
        DoEndDrag(Obj,X,Y);
    rvetClick:
      Click;
    rvetDblClick:
      DblClick;
    rvetMouseMove:
      with TRVMouseMoveMessageData(data) do
        MouseMove(Shift, X, Y);
    rvetRVDblClick:
      RVData.DoRVDblClick(TRVDblClickMessageData(data).ClickedWord,
                          TRVDblClickMessageData(data).StyleNo);
    rvetJump:
      RVData.DoJump(TRVJumpMessageData(data).id);
    rvetRVMouseUp:
      with TRVMouseUpDownMessageData(data) do
        RVData.DoRVMouseUp(Button,Shift,ItemNo,X,Y);
    rvetRVMouseDown:
      with TRVMouseUpDownMessageData(data) do
        RVData.DoRVMouseDown(Button,Shift,ItemNo,X,Y);
  end;
  data.Free;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.SetName(const NewName: TComponentName);
var reg: TRegistry;
  procedure SetRVFOption(Value: Boolean; Option: TRVFOption);
  begin
    if Value then
      RVFOptions := RVFOptions + [Option]
    else
      RVFOptions := RVFOptions - [Option];
  end;
begin
  if (csDesigning in ComponentState) and
     not (csLoading in ComponentState) and
     (Name=EmptyStr) and
     not (rvstNameSet in RVData.State) then begin
    //Application.MessageBox('Applying!','');
    try
      reg := TRegistry.Create;
      try
        if reg.OpenKey('Software\TRichView', False) then begin
          try
            case reg.ReadInteger('Styles') of
              0:
              begin
                RVFTextStylesReadMode := rvf_sInsertMap;
                RVFParaStylesReadMode := rvf_sInsertMap;
                RVFOptions := RVFOptions-[rvfoSaveTextStyles, rvfoSaveParaStyles];
                RTFReadProperties.TextStyleMode := rvrsUseClosest;
                RTFReadProperties.ParaStyleMode := rvrsUseClosest;
              end;
              1:
              begin
                RVFTextStylesReadMode := rvf_sInsertMerge;
                RVFParaStylesReadMode := rvf_sInsertMerge;
                RVFOptions := RVFOptions+[rvfoSaveTextStyles, rvfoSaveParaStyles];
                RTFReadProperties.TextStyleMode := rvrsAddIfNeeded;
                RTFReadProperties.ParaStyleMode := rvrsAddIfNeeded;
              end;
            end;
          except;
            RVFTextStylesReadMode := rvf_sInsertMerge;
            RVFParaStylesReadMode := rvf_sInsertMerge;
            RVFOptions := RVFOptions+[rvfoSaveTextStyles, rvfoSaveParaStyles];
            RTFReadProperties.TextStyleMode := rvrsAddIfNeeded;
            RTFReadProperties.ParaStyleMode := rvrsAddIfNeeded;
          end;
          try
            case reg.ReadInteger('Tags') of
              0:
              begin
                Options := Options-[rvoTagsArePChars];
              end;
              1:
              begin
                Options := Options+[rvoTagsArePChars];
              end;
            end;
          except;
          end;
          try
            SetRVFOption(reg.ReadBool('rvf01'), rvfoSaveBack);
            SetRVFOption(reg.ReadBool('rvf02'), rvfoSaveLayout);
            SetRVFOption(reg.ReadBool('rvf03'), rvfoSaveBinary);
            SetRVFOption(reg.ReadBool('rvf04'), rvfoSavePicturesBody);
            SetRVFOption(reg.ReadBool('rvf05'), rvfoSaveControlsBody);
            SetRVFOption(reg.ReadBool('rvf06'), rvfoLoadBack);
            SetRVFOption(reg.ReadBool('rvf07'), rvfoLoadLayout);
            SetRVFOption(reg.ReadBool('rvf08'), rvfoIgnoreUnknownPicFmt);
            SetRVFOption(reg.ReadBool('rvf09'), rvfoIgnoreUnknownCtrls);
            SetRVFOption(reg.ReadBool('rvf10'), rvfoConvUnknownStylesToZero);
            SetRVFOption(reg.ReadBool('rvf11'), rvfoConvLargeImageIdxToZero);
            SetRVFOption(reg.ReadBool('rvf12'), rvfoSaveDocProperties);
            SetRVFOption(reg.ReadBool('rvf13'), rvfoLoadDocProperties);
          except;
          end;
        end;
      finally
        reg.Free;
      end;
    except
    end;
  end;
  RVData.State := RVData.State+[rvstNameSet];
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AfterCreateWnd1;
begin
  if (RVData=nil) or (RVData.Items=nil) or (RVData.Items.Count=0) then
    inherited;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.AfterCreateWnd2;
begin
  if (RVData<>nil) and (RVData.Items<>nil) and (RVData.Items.Count>0) then
    Format_(True, False, Canvas, False, False)
  else
    inherited;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.StoreDelimiters: Boolean;
begin
  Result := FDelimiters<>RVDEFAULTDELEMITERS;
end;
{------------------------------------------------------------------------------}
function TCustomRichView.GetItemAt(X, Y: Integer;
  var RVData: TCustomRVFormattedData; var ItemNo, OffsetInItem: Integer;
  Strict: Boolean): Boolean;
var Dummy: Boolean;
begin
  Self.RVData.GetItemAtEx(X,Y,RVData,ItemNo,OffsetInItem,Strict,Dummy);
  Result := ItemNo<>-1;
end;
{------------------------------------------------------------------------------}
procedure TCustomRichView.WMRVDragDrop(var Message: TMessage);
begin
  {$IFNDEF RVDONOTUSEDRAGDROP}
  RVData.DoDrag;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEDRAGDROP}
procedure TCustomRichView.BeginOleDrag;
begin
  PostMessage(RVData.GetAbsoluteRootData.GetParentControl.Handle, WM_RVDRAGDROP,
    0, 0);
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TCustomRichView.ClientToDocument(const APoint: TPoint): TPoint;
var X, Y: Integer;
begin
  RVData.GetOriginEx(X, Y);
  Result := Point(APoint.X-X+RVData.GetHOffs, APoint.Y-Y+RVData.GetVOffs);
end;

initialization
{$IFDEF RVDEBUG}{$I Debug\Init.inc}{$ENDIF}

{$IFDEF RVDEBUG}{$I Debug\Final.inc}{$ENDIF}
end.




