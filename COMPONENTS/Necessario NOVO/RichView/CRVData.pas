
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TCustomRVData is a basic class representing     }
{       RichView document.                              }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit CRVData;

interface
{$I RV_Defs.inc}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFNDEF RVDONOTUSEJPEGIMAGE}
  Jpeg,
  {$ENDIF}
  {$IFNDEF RVDONOTUSELISTS}
  RVMarker,
  {$ENDIF}
  {$IFDEF RICHVIEWDEF4}
  ImgList,
  {$ENDIF}
  RVStyle, RVBack, RVFuncs, RVItem, RVScroll, RVUni, RVClasses,
  RVRTFErr;

type

  { State of RVData }
  TRVState = (
    rvstMakingSelection,     // Mouse selection is in process
    rvstLineSelection,       // Line selection (started from the left margin)
                             //   is in process
    rvstDrawHover,           // There is a highlighted hyperlink
    rvstSkipFormatting,      // Reformatting is not allowed
    rvstIgnoreNextMouseDown, // Next mouse-down event must be ignored
    rvstChangingBkPalette,   // Palette of background image is being updated
                             //   (avoiding recursive call)
    rvstCompletelySelected,  // RVData is completely selected (table cell)
    rvstClearing,            // Document is being cleared
                             //   (avoiding recursive call)
    rvstDoNotMoveChildren,   // Call of AdjustChildrenCoords is not allowed
    rvstForceStyleChangeEvent, // Editor must call events for changing styles
                             //   even if next value assigned to current style
                             //   is equal to the current value
    rvstIgnoreNextChar,      // Next call of WMChar or KeyPress must be ignored
                             //  (the character is already processed in WMKeyDown)
    rvstDoNotTab,            // Next call of DoTabNavigation will be ignored
    rvstDeselecting,         // Deselection is in process
                             //   (avoiding recursive call)
    rvstUnAssigningChosen,   // Unassigning chosen data is in process
                             //   (avoiding recursive call)
    rvstNoScroll,            // Scrolling is not allowed
    rvstFinalizingUndo,      // Avoiding recursive calls of FinalizeUndoGroup
    rvstRTFSkipPar,          // Saving to RTF: do not save paragraph mark
    rvstLoadingAsPartOfItem, // This RVData is loaded from RVF as a part of item
                             //   (cell is loaded with table)
                             //   (not calling AfterLoading(rvlfRVF) for items
                             //   of this RVData: will be called for the container item
    rvstNoKillFocusEvents,   // Disabling processing of WMKillFocus
    rvstEditorUnformatted,   // TRichViewEdit was not formatted before the call of Format
    rvstNameSet,             // TRichView.Name was assigned. Used to detect placing
                             // the component on the form from the Component Palette
    rvstFirstParaAborted,    // This is a page saved and reloaded in ReportHelper,
                             //   and the first paragraph is not completely on this page
    rvstLastParaAborted,     // The same for the last paragraph
    rvstInvalidSelection,    // Selection is not valid: do not create item resizer
    rvstDoNotClearCurTag,    // TRVEditRVData.ChangeEx must not clear "current tag"
                             //   ("current tag" is used in methods for inserting text)
    rvstStartingDragDrop,    // Dragging from this RichView is about to start
                             //   (WM_RVDRAGDROP was posted). Only in absolute
                             //    root RVData
    rvstCanDragDropDeleteSelection, // After dragging from this editor, selection
                             //  must be deleted (set when moving data to
                             //  another control)
    rvstDragDropCursorNotMoved); // Dragging is finished, but cursor was not
                             //  moved (it is not a d&d, it is a simple click on
                             //  the selection)
  TRVStates = set of TRVState;

  { Flags for RVData }
  TRVFlag = (
    rvflUseJumps,            // List of hyperlink coords must be maintained
    rvflTrim,                // Formatting routine may not show spaces in line
                             //   wrap places (always set)
    rvflShareContents,       // This RVData uses smb. else's items
    rvflUseExternalLeading,  // Formatting routine uses font external leading
                             // (never set)
    rvflMouseXYAlwaysCorrect,// Mouse processing procedures may not preprocess
                             //   coordinates (used in table cells)
    rvflAllowCustomDrawItems,// Formatting routine may create drawing items
                             // of custom types (not only TRVDrawLineInfo) -
                             // used in RVPrint and RVReportHelper
    rvflPrinting,            // This is RVData with formatting for printing (or
                             //   reports)
    rvflRootEditor,          // This is TRVEditRVData (not RVData of inplace)
    rvflRoot,                // This is TRichViewRVData or TRVEditRVData
                             //  (not RVData of inplace)
    rvflDBRichViewEdit);     // This is TDBRichViewEdit.RVData
  TRVFlags = set of TRVFlag;

  { Which part to save in RVF? }
  TRVFSaveScope = (
    rvfss_Full,              // document
     rvfss_Selection,        // selection
     rvfss_Page);            // page (for TRVPrint or TRVReportHelper)

  PRVIntegerList = ^TRVIntegerList;
  TRVRTFFontTable = class;

  { ----------------------------------------------------------------------------
    TRVLayoutInfo: information about document layout for saving and loading in
    RVF.
    Main properties:
    - margins,
    - min- and maxtextwidth,
    - bidimode.
    For saving RVReportHelper page, additional properties:
    - LastParaAborted: <>0 if the last page paragraph is not completely on the page;
    - FirstParaAborted: the same for the first page paragraph;
    - FirstMarkerListNo, FirstMarkerLevel - information about marker of the
      first page paragraph (marker is before this page)
  }
  TRVLayoutInfo = class
    public
      Loaded: Boolean;
      LeftMargin, RightMargin, TopMargin, BottomMargin: Integer;
      MinTextWidth, MaxTextWidth: Integer;
      BiDiMode: TRVBiDiMode;
      LastParaAborted, FirstParaAborted: Integer;
      FirstMarkerListNo, FirstMarkerLevel: Integer;
      constructor Create;
      procedure SaveToStream(Stream: TStream; IncludeSize: Boolean);
      procedure LoadFromStream(Stream: TStream; IncludeSize: Boolean);
      procedure SaveTextToStream(Stream: TStream);
      procedure LoadText(const s: String);
      procedure LoadBinary(const s: String);
  end;
  { ----------------------------------------------------------------------------
    TRVHTMLBulletInfo: information for saving shared images in HTML
      (several items can use the same image file).
    Used by: "bullets", "hotspots", list markers with pictures and image lists.
  }
  TRVHTMLBulletInfo = class
    public
      FileName: String;
      ImageList: TCustomImageList;
      ImageIndex: Integer;
      BackColor: TColor;
      Graphic: TGraphic;
  end;
  { ----------------------------------------------------------------------------
    TRVRTFFontTableItem: item of RTF font table (TRVRTFFontTable)
  }
  TRVRTFFontTableItem = class
    public
      FontName: String;
      {$IFDEF RICHVIEWCBDEF3}
      Charset: TFontCharset;
      {$ENDIF}
  end;
  { ----------------------------------------------------------------------------
    TRVRTFFontTable: RTF font table. Created for saving to RTF, contains all
    fonts used in the document (both by styles and by items)
  }
  TRVRTFFontTable = class (TRVList)
    private
      function Get(Index: Integer): TRVRTFFontTableItem;
      procedure Put(Index: Integer; const Value: TRVRTFFontTableItem);
    public
      function Find(const FontName: String
        {$IFDEF RICHVIEWCBDEF3}; Charset: TFontCharset{$ENDIF}): Integer;
      function AddUnique(const FontName: String
        {$IFDEF RICHVIEWCBDEF3}; Charset: TFontCharset{$ENDIF}): Integer;
      property Items[Index: Integer]: TRVRTFFontTableItem read Get write Put; default;
  end;
  { ----------------------------------------------------------------------------
    TCustomRVData: RichView document. This class is not used directly.
    Direct descendant: TCustomRVFormattedData.
  }
  TCustomRVData = class(TPersistent)
  private
    { Private declarations }
    FFirstJumpNo: Integer;
    FItems: TStringList;
    { Property values }
    function GetPageBreaksBeforeItems(Index: Integer): Boolean;
    procedure SetPageBreaksBeforeItems(Index: Integer;  Value: Boolean);
    function GetItemCount: Integer;
    { HTML & RTF }
    {$IFNDEF RVDONOTUSEHTML}
    function ShouldSaveTextToHTML(StyleNo: Integer): Boolean;
    function GetHTMLATag(ItemNo: Integer; CSS: String): String;
    {$ENDIF}
    {$IFNDEF RVDONOTUSERTF}
    function ShouldSaveTextToRTF(StyleNo: Integer): Boolean;
    {$ENDIF}
  protected
    { Protected declarations }
    FAllowNewPara: Boolean;
    FirstCP, LastCP, NotAddedCP: TRVCPInfo;
    CPCount: Integer;

    function NextCharStr(const str: String; ItemNo, Index: Integer): Integer;
    function PrevCharStr(const str: String; ItemNo, Index: Integer): Integer;
    function NextChar(ItemNo: Integer; Index: Integer): Integer;
    function PrevChar(ItemNo: Integer; Index: Integer): Integer;
    procedure CheckItemClass(ItemNo: Integer;
      RequiredClass: TCustomRVItemInfoClass);
    function ShareItems: Boolean; dynamic;
    function CanLoadLayout: Boolean; dynamic;
    function GetURL(id: Integer): String; dynamic; abstract;
    function GetOptions: TRVOptions; virtual;
    procedure SetOptions(const Value: TRVOptions); virtual;
    function GetRVFOptions: TRVFOptions; virtual;
    procedure SetRVFOptions(const Value: TRVFOptions); virtual;
    function GetRTFOptions: TRVRTFOptions; virtual;
    procedure SetRTFOptions(const Value: TRVRTFOptions); virtual;
    function GetRVFWarnings: TRVFWarnings; virtual;
    procedure SetRVFWarnings(const Value: TRVFWarnings); virtual;
    function GetDelimiters: String; dynamic;
    function GetRVFTextStylesReadMode: TRVFReaderStyleMode; virtual;
    function GetRVFParaStylesReadMode: TRVFReaderStyleMode; virtual;
    procedure RVFGetLimits(SaveScope: TRVFSaveScope;
      var StartItem, EndItem, StartOffs, EndOffs: Integer;
      var StartPart, EndPart: TRVMultiDrawItemPart); dynamic;
    function GetRTFProperties:TPersistent  {TRVRTFReaderProperties}; dynamic;
    {$IFNDEF RVDONOTUSERVF}
    procedure DoOnStyleReaderError(Reader: TReader; const Message: string;
      var Handled: Boolean);
    function InsertRVFFromStream_(Stream: TStream; var Index: Integer;
      AParaNo: Integer; AllowReplaceStyles, AppendMode, EditFlag: Boolean;
      var Color: TColor; Background: TRVBackground;
      Layout: TRVLayoutInfo; var NonFirstItemsAdded: Integer;
      var Protect, FullReformat: Boolean):Boolean;
    procedure DataWriter(Stream: TStream);
    procedure DataReader(Stream: TStream);
    {$ENDIF}
    procedure NormalizeParas(StartItemNo: Integer);
    procedure InsertCheckpoint(ItemNo, Tag: Integer; const Name: String;
      RaiseEvent: Boolean);
    procedure UpdateCPPos(cp: TRVCPInfo; ItemNo: Integer);
    procedure UnlinkCheckpoint(cp: TRVCPInfo; DecCPCount: Boolean);
    function FindCPBeforeItem(ItemNo: Integer): TRVCPInfo;
    procedure UpdateCPItemNo;
    procedure InternalFreeItem(item: TCustomRVItemInfo; Clearing: Boolean); virtual;
    function IsDelimiterA(ch: Char): Boolean;
    function IsDelimiterW(ch: Word): Boolean;
    function IsDelimiter(const s: String; Index: Integer;
      ItemOptions: TRVItemOptions): Boolean;
    function GetItemOptions(ItemNo: Integer): TRVItemOptions;
    procedure Replace0(var s: String);
    function RV_CanConcateItems(FirstItemNo: Integer; item1, item2: TCustomRVItemInfo;
      IgnorePara: Boolean): Boolean;
    procedure SimpleConcate(FirstItemNo: Integer; item1, item2: TCustomRVItemInfo);
    procedure MassSimpleConcate(FirstItemNo, LastItemNo: Integer);
    procedure AfterDeleteStyles(TextStylesShift, ParaStylesShift,
      ListStylesShift: TRVIntegerList); dynamic;
    procedure InitStyleMappings(var PTextStylesMapping, PParaStylesMapping,
      PListStylesMapping: PRVIntegerList); dynamic;
    procedure DoneStyleMappings(PTextStylesMapping, PParaStylesMapping,
      PListStylesMapping: PRVIntegerList); dynamic;
    function SupportsPageBreaks: Boolean; dynamic;
    procedure AdjustInItemsRange(var ItemNo: Integer);
    {$IFNDEF RVDONOTUSEHTML}
    procedure SaveHTMLCheckpoint(Stream: TStream; Checkpoint: TRVCPInfo;
      var cpno: Integer; const Prefix: String; FromNewLine: Boolean;
      Options: TRVSaveOptions);
    function GetTextForHTML(const Path: String; ItemNo: Integer): String;
    {$ENDIF}
    function GetFirstParaItem(ItemNo: Integer): Integer;
    function GetFirstParaSectionItem(ItemNo: Integer): Integer;
    {$IFNDEF RVDONOTUSELISTS}
    procedure DestroyMarkers; dynamic;
    function FindPreviousMarker(ItemNo: Integer): TRVMarkerItemInfo;
    function FindMarkerLocalLocationFrom(StartItemNo: Integer;
      Marker: TRVMarkerItemInfo): Integer;
    function FindLastMarkerIndex(StartAfterMeIndex: Integer; ListStyles: TRVIntegerList): Integer;
    {$ENDIF}
    function GetFlags: TRVFlags; virtual;                   abstract;
    procedure SetFlags(const Value: TRVFlags); virtual;     abstract;
    procedure AddStringFromFile(const s: String; StyleNo,ParaNo: Integer;
      AsSingleParagraph: Boolean; var FirstTime, PageBreak: Boolean);
    procedure AddStringFF(var s: String; StyleNo,ParaNo: Integer;
      ProcessPageBreaks, AsSingleParagraph: Boolean;
      var FirstTime, PageBreak: Boolean);
    procedure AddUnixString(var s: String; StyleNo,ParaNo: Integer;
      ProcessPageBreaks, AsSingleParagraph: Boolean;
      var FirstTime, PageBreak: Boolean);
  public
    State: TRVStates;
    { Constructors - destructors }
    constructor Create;
    destructor Destroy; override;
    { Document/control & styles properties }
    function GetRVData: TCustomRVData; virtual;
    function GetSourceRVData: TCustomRVData; virtual;
    function GetStyleCodePage(StyleNo: Integer): TRVCodePage;
    function GetStyleLocale(StyleNo: Integer): Cardinal;
    function GetDefaultCodePage: TRVCodePage;
    function GetRVStyle: TRVStyle; virtual;
    function GetParentControl: TWinControl; dynamic;
    procedure GetParentInfo(var ParentItemNo: Integer;
      var Location: TRVStoreSubRVData); dynamic;
    function GetChosenRVData: TCustomRVData; dynamic;
    function GetChosenItem: TCustomRVItemInfo; dynamic;
    function GetParentData: TCustomRVData; virtual;
    function GetRootData: TCustomRVData; virtual;
    function GetAbsoluteParentData: TCustomRVData; virtual;
    function GetAbsoluteRootData: TCustomRVData; virtual;
    { Palette }
    function GetRVPalette: HPALETTE; virtual;
    function GetRVLogPalette: PLogPalette; virtual;
    function GetDoInPaletteMode: TRVPaletteAction; virtual;
    procedure UpdateItemsPaletteInfo;
    { Item properties }
    function GetItemNo(Item: TCustomRVItemInfo): Integer;
    function GetItem(ItemNo: Integer): TCustomRVItemInfo;
    function SetItemExtraIntProperty(ItemNo: Integer;
      Prop: TRVExtraItemProperty; Value: Integer): Boolean;
    function GetItemExtraIntProperty(ItemNo: Integer;
      Prop: TRVExtraItemProperty; var Value: Integer): Boolean;
    function SetItemExtraStrProperty(ItemNo: Integer;
      Prop: TRVExtraItemStrProperty; const Value: String): Boolean;
    function GetItemExtraStrProperty(ItemNo: Integer;
      Prop: TRVExtraItemStrProperty; var Value: String): Boolean;
    function GetItemTag(ItemNo: Integer): Integer;
    function IsParaStart(ItemNo: Integer): Boolean;
    function GetItemPara(ItemNo: Integer): Integer;
    function IsFromNewLine(ItemNo: Integer): Boolean;
    function GetOffsAfterItem(ItemNo: Integer): Integer;
    function GetOffsBeforeItem(ItemNo: Integer): Integer;
    function ItemLength(ItemNo: Integer): Integer;
    procedure SetItemTag(ItemNo: Integer; ATag: Integer);
    function GetItemStyle(ItemNo: Integer): Integer;
    function GetActualStyle(Item: TCustomRVItemInfo): Integer;
    function GetActualStyle2(StyleNo, ParaNo: Integer): Integer;
    function GetItemText(ItemNo: Integer): String;
    procedure SetItemText(ItemNo: Integer; const s: String);
    {$IFNDEF RVDONOTUSEUNICODE}
    {$IFDEF RICHVIEWCBDEF3}
    function GetTextInItemFormatW(ItemNo: Integer; const s: WideString): String;
    function GetItemTextW(ItemNo: Integer): WideString;
    procedure SetItemTextW(ItemNo: Integer; const s: WideString);
    {$ENDIF}
    function GetTextInItemFormatA(ItemNo: Integer; const s: String): String;
    function GetItemTextA(ItemNo: Integer): String;
    procedure SetItemTextA(ItemNo: Integer; const s: String);
    {$ENDIF}
    function FindControlItemNo(actrl: TControl): Integer;
    { BiDi }
    function GetItemBiDiMode(ItemNo: Integer): TRVBiDiMode;
    function GetParaBiDiMode(ParaNo: Integer): TRVBiDiMode;
    function GetBiDiMode: TRVBiDiMode; virtual;
    { Operations on items - internal }
    procedure FreeItem(ItemNo: Integer; Clearing: Boolean);
    { Operations on items - public }
    procedure Clear; dynamic;
    procedure DeleteItems(FirstItemNo, Count: Integer); dynamic;
    procedure DeleteSection(const CpName: String);
    { Related to events }
    function IsAssignedOnProgress: Boolean; dynamic;
    procedure DoProgress(Operation: TRVLongOperation; Stage: TRVProgressStage;
      PercentDone: Byte); dynamic;
    function GetExtraRTFCode(Area: TRVRTFSaveArea; Obj: TObject;
      Index1, Index2: Integer; InStyleSheet: Boolean): String; dynamic;
    function GetExtraHTMLCode(Area: TRVHTMLSaveArea;
      CSSVersion: Boolean): String; dynamic;
    function GetParaHTMLCode(RVData: TCustomRVData; ItemNo: Integer;
      ParaStart, CSSVersion: Boolean): String; dynamic;
    procedure ReadHyperlink(const Target, Extras: String; DocFormat: TRVLoadFormat;
      var StyleNo, ItemTag: Integer; var ItemName: String); dynamic;
    procedure WriteHyperlink(id: Integer; RVData: TCustomRVData; ItemNo: Integer;
       SaveFormat: TRVSaveFormat; var Target, Extras: String); dynamic;
    function SaveItemToFile(const Path: String; RVData: TCustomRVData;
      ItemNo: Integer; SaveFormat: TRVSaveFormat; Unicode: Boolean;
      var Text: String): Boolean; virtual;
    function ImportPicture(const Location: String;
      Width, Height: Integer): TGraphic; dynamic;
    function GetItemHint(RVData: TCustomRVData; ItemNo: Integer): String; dynamic;
    function DoSavePicture(DocumentSaveFormat: TRVSaveFormat;
      const imgSavePrefix, Path: String; var imgSaveNo: Integer;
      OverrideFiles: Boolean; CurrentFileColor: TColor;
      gr: TGraphic): String; virtual;
    function SavePicture(DocumentSaveFormat: TRVSaveFormat;
      const imgSavePrefix, Path: String; var imgSaveNo: Integer;
      OverrideFiles: Boolean; CurrentFileColor: TColor;
      gr: TGraphic): String;
    function RVFPictureNeeded(const ItemName: String; ItemTag: Integer): TGraphic; dynamic;
    procedure ControlAction(ControlAction: TRVControlAction; ItemNo: Integer;
      Item: TCustomRVItemInfo);
    procedure ItemAction(ItemAction: TRVItemAction; Item: TCustomRVItemInfo;
      var Text: String; RVData: TCustomRVData); virtual;
    procedure ControlAction2(ControlAction: TRVControlAction; ItemNo: Integer;
      var Control:  TControl); dynamic; abstract;
    function RVFControlNeeded(const ItemName: String; ItemTag: Integer): TControl; dynamic;
    function RVFImageListNeeded(ImageListTag: Integer): TCustomImageList; dynamic;
    procedure HTMLSaveImage(RVData: TCustomRVData; ItemNo: Integer;
      const Path: String; BackgroundColor: TColor; var Location: String;
      var DoDefault: Boolean); dynamic;
    procedure SaveImage2(Graphic: TGraphic; SaveFormat: TRVSaveFormat;
      const Path, ImagePrefix: String; var ImageSaveNo: Integer;
      var Location: String; var DoDefault: Boolean); dynamic;
    function SaveComponentToFile(const Path: String; SaveMe: TComponent;
      SaveFormat: TRVSaveFormat): String; virtual;
    { Text save and load }
    {$IFNDEF RVDONOTUSEUNICODE}
    function LoadTextFromStreamW(Stream: TStream; StyleNo, ParaNo: Integer;
      DefAsSingleParagraph: Boolean):Boolean;
    function LoadTextW(const FileName: String; StyleNo, ParaNo: Integer;
      DefAsSingleParagraph: Boolean):Boolean;
    {$ENDIF}
    function SaveTextToStream(const Path: String; Stream: TStream;
      LineWidth: Integer; SelectionOnly, TextOnly, Unicode,
      UnicodeWriteSignature: Boolean):Boolean;
    function SaveText(const FileName: String; LineWidth: Integer;
      Unicode: Boolean):Boolean;
    function LoadText(const FileName: String; StyleNo, ParaNo: Integer;
      AsSingleParagraph: Boolean):Boolean;
    function LoadTextFromStream(Stream: TStream; StyleNo, ParaNo: Integer;
      AsSingleParagraph: Boolean):Boolean;
    { HTML save }
    {$IFNDEF RVDONOTUSEHTML}
    function SaveBackgroundToHTML(bmp: TBitmap; Color: TColor;
      const Path, ImagesPrefix: String; var imgSaveNo: Integer;
      OverrideImages: Boolean): String;
    function SaveHTMLToStreamEx(Stream: TStream;
      const Path, Title, ImagesPrefix, ExtraStyles, ExternalCSS, CPPrefix: String;
      Options: TRVSaveOptions; Color: TColor; var CurrentFileColor: TColor;
      var imgSaveNo: Integer; LeftMargin, TopMargin, RightMargin, BottomMargin: Integer;
      Background: TRVBackground; Bullets: TRVList): Boolean; dynamic;
    function SaveHTMLToStream(Stream: TStream; const Path, Title,ImagesPrefix: String;
      Options: TRVSaveOptions; Color: TColor; var imgSaveNo: Integer;
      LeftMargin, TopMargin, RightMargin, BottomMargin: Integer;
      Background: TRVBackground; Bullets: TRVList): Boolean; dynamic;
    function SaveHTMLEx(const FileName, Title, ImagesPrefix, ExtraStyles,
      ExternalCSS, CPPrefix: String; Options: TRVSaveOptions;
      Color: TColor; var CurrentFileColor: TColor;
      var imgSaveNo: Integer; LeftMargin, TopMargin, RightMargin, BottomMargin: Integer;
      Background: TRVBackground): Boolean;
    function SaveHTML(const FileName,Title,ImagesPrefix: String;
      Options: TRVSaveOptions; Color: TColor; var imgSaveNo: Integer;
      LeftMargin, TopMargin, RightMargin, BottomMargin: Integer;
      Background: TRVBackground): Boolean;
    {$ENDIF}
    function GetNextFileName(const ImagesPrefix, Path, Ext: String;
      var imgSaveNo: Integer; OverrideFiles: Boolean): String; dynamic;
    { RVF save and load }
    {$IFNDEF RVDONOTUSERVF}
    function LoadRVFFromStream(Stream: TStream; var Color: TColor;
      Background: TRVBackground; Layout: TRVLayoutInfo):Boolean;
    function InsertRVFFromStream(Stream: TStream; Index: Integer;
      var Color: TColor; Background: TRVBackground; Layout: TRVLayoutInfo;
      AllowReplaceStyles: Boolean):Boolean;
    function AppendRVFFromStream(Stream: TStream; ParaNo: Integer;
      var Color: TColor; Background: TRVBackground):Boolean;
    function LoadRVF(const FileName: String;
      var Color: TColor; Background: TRVBackground;
      Layout: TRVLayoutInfo):Boolean;
    //SelectionOnly=True - reserved here
    function SaveRVFToStream(Stream: TStream; SelectionOnly: Boolean;
      Color: TColor; Background: TRVBackground;
      Layout: TRVLayoutInfo):Boolean;
    function SaveRVFToStreamEx(Stream: TStream; SaveScope: TRVFSaveScope;
      Color: TColor; Background: TRVBackground;
      Layout: TRVLayoutInfo):Boolean;
    //SelectionOnly=True - reserved here
    function SaveRVF(const FileName: String; SelectionOnly: Boolean;
      Color: TColor; Background: TRVBackground;
      Layout: TRVLayoutInfo):Boolean;
    {$ENDIF}
    function InsertFirstRVFItem(var Index: Integer; var s: String;
      var item: TCustomRVItemInfo; EditFlag: Boolean; var FullReformat: Boolean;
      var NewListNo: Integer): Boolean; dynamic;
    function GetRVFSaveScope(SelectionOnly: Boolean):TRVFSaveScope;
    { RTF save and load }
    {$IFNDEF RVDONOTUSERTF}
    {$IFNDEF RVDONOTUSELISTS}
    procedure SaveRTFListTable97(Stream: TStream; ColorList: TRVColorList;
      ListOverrideOffsetsList: TRVIntegerList;
      FontTable: TRVRTFFontTable; tpp: Double);
    {$ENDIF}
    function SaveRTFToStream(Stream: TStream; SelectionOnly: Boolean;
      Level: Integer; Color: TColor; Background: TRVBackground; ColorList: TRVColorList;
      StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
      FontTable: TRVRTFFontTable; tpp: Double):Boolean; dynamic;
    function SaveRTF(const FileName: String; SelectionOnly: Boolean;
      Color: TColor; Background: TRVBackground):Boolean;
    {$ENDIF}
    {$IFNDEF RVDONOTUSERTFIMPORT}
    function LoadRTFFromStream(Stream: TStream):TRVRTFErrorCode;
    function LoadRTF(const FileName: String):TRVRTFErrorCode;
    {$ENDIF}
    procedure MakeRTFTables(ColorList: TRVColorList;
      ListOverrideCountList: TRVIntegerList; TopLevel: Boolean);
    { Adding items - general }
    procedure AddItem(const Text: String; Item: TCustomRVItemInfo);
    { Adding items - text }
    procedure AddFmt(const FormatStr: String; const Args: array of const;
      StyleNo, ParaNo: Integer);
    procedure AddNL(const s: String; StyleNo, ParaNo: Integer);
    procedure AddNLTag(const s: String; StyleNo, ParaNo, Tag: Integer);
    procedure AddTextNL(s: String; StyleNo, FirstParaNo, OtherParaNo: Integer
      {$IFDEF RICHVIEWDEF4};Tag: Integer=0{$ENDIF});
    procedure AddTextBlockNL(s: String; StyleNo, ParaNo: Integer
      {$IFDEF RICHVIEWDEF4};Tag: Integer=0{$ENDIF});
    {$IFNDEF RVDONOTUSEUNICODE}
    {$IFDEF RICHVIEWCBDEF3}
    procedure AddNLWTag(const s: WideString; StyleNo, ParaNo, Tag: Integer);
    {$ENDIF}
    procedure AddNLWTagRaw(const s: String; StyleNo, ParaNo, Tag: Integer);
    procedure AddNLATag(const s: String; StyleNo, ParaNo, Tag: Integer);
    procedure AddTextNLW(const s: String; StyleNo, FirstParaNo,
      OtherParaNo: Integer; DefAsSingleParagraph: Boolean);
    {$ENDIF}
    { Adding items - others }
    procedure AddBreakExTag(Width: Byte; Style: TRVBreakStyle;
      Color: TColor; Tag: Integer);
    procedure AddBreak;
    procedure AddBreakEx(Width: Byte; Style: TRVBreakStyle; Color: TColor);
    procedure AddBreakTag(Tag: Integer);
    procedure AddBulletEx(const Name: String; ImageIndex: Integer;
      ImageList: TCustomImageList; ParaNo: Integer);
    procedure AddBulletExTag(const Name: String; ImageIndex: Integer;
      ImageList: TCustomImageList; ParaNo, Tag: Integer);
    procedure AddHotspotEx(const Name: String; ImageIndex,
      HotImageIndex: Integer; ImageList: TCustomImageList; ParaNo: Integer);
    procedure AddHotspotExTag(const Name: String; ImageIndex,
      HotImageIndex: Integer; ImageList: TCustomImageList; ParaNo, Tag: Integer);
    procedure AddPictureExTag(const Name: String; gr: TGraphic; ParaNo: Integer;
      VAlign: TRVVAlign; Tag: Integer);
    procedure AddControlExTag(const Name: String; ctrl: TControl;
      ParaNo: Integer; VAlign: TRVVAlign; Tag: Integer);
    procedure AddPictureEx(const Name: String; gr: TGraphic; ParaNo: Integer;
      VAlign: TRVVAlign);
    procedure AddControlEx(const Name: String; ctrl: TControl;
      ParaNo: Integer; VAlign: TRVVAlign);
    procedure AddHotPicture(const Name: String; gr: TGraphic; ParaNo: Integer;
      VAlign: TRVVAlign);
    procedure AddHotPictureTag(const Name: String; gr: TGraphic; ParaNo: Integer;
      VAlign: TRVVAlign; Tag: Integer);
    { Checkpoints - internal }
    procedure FreeCheckpoint(var cp: TRVCPInfo; AdjustLinks, DecCPCount: Boolean);
    procedure SetCP(Item: TCustomRVItemInfo; var PrevCP, CP: TRVCPInfo);
    { Checkpoints - public }
    function AddNamedCheckpointExTag(const CpName: String; RaiseEvent: Boolean;
      Tag: Integer): Integer;
    procedure SetCheckpointInfo(ItemNo: Integer; ATag: Integer; const AName: String;
      ARaiseEvent: Boolean);
    function RemoveCheckpoint(ItemNo: Integer): Boolean;
    function GetFirstCheckpoint: TCheckpointData;
    function GetNextCheckpoint(CheckpointData: TCheckpointData): TCheckpointData;
    function GetLastCheckpoint: TCheckpointData;
    function GetPrevCheckpoint(CheckpointData: TCheckpointData): TCheckpointData;
    function GetItemCheckpoint(ItemNo: Integer):TCheckpointData;
    function FindCheckpointByName(const Name: String): TCheckpointData;
    function FindCheckpointByTag(Tag: Integer): TCheckpointData;
    function GetCheckpointByNo(No: Integer): TCheckpointData;
    function GetCheckpointItemNo(CheckpointData: TCheckpointData): Integer;
    function GetCheckpointNo(CheckpointData: TCheckpointData): Integer;
    procedure GetCheckpointInfo(CheckpointData: TCheckpointData;
      var Tag: Integer; var Name: String; var RaiseEvent: Boolean);
    { Get info for specific item types }
    procedure GetBreakInfo(ItemNo: Integer; var AWidth: Byte;
      var AStyle: TRVBreakStyle; var AColor: TColor; var ATag: Integer);
    procedure GetBulletInfo(ItemNo: Integer; var AName: String;
      var AImageIndex: Integer; var AImageList: TCustomImageList;
      var ATag: Integer);
    procedure GetHotspotInfo(ItemNo: Integer; var AName: String;
      var AImageIndex, AHotImageIndex: Integer; var AImageList: TCustomImageList;
      var ATag: Integer);
    procedure GetPictureInfo(ItemNo: Integer; var AName: String;
      var Agr: TGraphic; var AVAlign: TRVVAlign; var ATag: Integer);
    procedure GetControlInfo(ItemNo: Integer; var AName: String;
      var Actrl: TControl; var AVAlign: TRVVAlign; var ATag: Integer);
    procedure GetTextInfo(ItemNo: Integer; var AText: String;
      var ATag: Integer);
    { Set info for specific item types }
    procedure SetBreakInfo(ItemNo: Integer; AWidth: Byte; AStyle: TRVBreakStyle;
      AColor: TColor; ATag: Integer);
    procedure SetBulletInfo(ItemNo: Integer; const AName: String;
      AImageIndex: Integer; AImageList: TCustomImageList; ATag: Integer);
    procedure SetHotspotInfo(ItemNo: Integer; const AName: String;
      AImageIndex, AHotImageIndex: Integer; AImageList: TCustomImageList;
      ATag: Integer);
    function SetPictureInfo(ItemNo: Integer; const  AName: String;
      Agr: TGraphic; AVAlign: TRVVAlign; ATag: Integer): Boolean;
    function SetControlInfo(ItemNo: Integer; const AName: String;
      AVAlign: TRVVAlign; ATag: Integer): Boolean;
    { Styles }
    procedure MarkStylesInUse(UsedTextStyles, UsedParaStyles,
      UsedListStyles: TRVIntegerList); dynamic;
    procedure UpdateStyles(TextStylesShift, ParaStylesShift,
      ListStylesShift: TRVIntegerList);
    procedure DeleteUnusedStyles(TextStyles, ParaStyles, ListStyles: Boolean);
    { Paragraph list markers}
    {$IFNDEF RVDONOTUSELISTS}
    function GetMarkers(AllowCreate: Boolean): TRVMarkerList; dynamic;
    function GetPrevMarkers: TRVMarkerList; dynamic;
    function SetListMarkerInfo(AItemNo, AListNo, AListLevel, AStartFrom,
      AParaNo: Integer; AUseStartFrom: Boolean): Integer;
    procedure RecalcMarker(AItemNo: Integer; AllowCreateList: Boolean);
    procedure RemoveListMarker(ItemNo: Integer);
    function GetListMarkerInfo(AItemNo: Integer; var AListNo, AListLevel,
      AStartFrom: Integer; var AUseStartFrom: Boolean): Integer;
    procedure AddMarkerInList(ItemNo: Integer);
    procedure DeleteMarkerFromList(Item: TCustomRVItemInfo; Clearing: Boolean);
    {$ENDIF}
    { Others }
    procedure ShareItemsFrom(Source: TCustomRVData);
    procedure DrainFrom(Victim: TCustomRVData);
    procedure SetParagraphStyleToAll(ParaNo: Integer);
    procedure SetAddParagraphMode(AllowNewPara: Boolean);
    procedure AppendFrom(Source: TCustomRVData);
    procedure Inserting(RVData: TCustomRVData; Safe: Boolean);
    function Edit: TCustomRVData; dynamic;
    procedure Beep;
    procedure ExpandToParaSection(ItemNo1,ItemNo2: Integer;
      var FirstItemNo, LastItemNo: Integer);
    procedure ExpandToPara(ItemNo1,ItemNo2: Integer;
      var FirstItemNo, LastItemNo: Integer);
    function ReplaceTabs(const s: String; StyleNo: Integer;
      UnicodeDef: Boolean): String;
    { Properties }
    function GetDocProperties: TStringList; dynamic;
    property Flags: TRVFlags read GetFlags write SetFlags;
    property Items: TStringList read FItems;
    property ItemCount: Integer read GetItemCount;
    property Options: TRVOptions read GetOptions write SetOptions;
    property RVFOptions: TRVFOptions read GetRVFOptions write SetRVFOptions;
    property RTFOptions: TRVRTFOptions read GetRTFOptions write SetRTFOptions;
    property RVFWarnings: TRVFWarnings read GetRVFWarnings write SetRVFWarnings;
    property FirstJumpNo: Integer read FFirstJumpNo write FFirstJumpNo;
    property PageBreaksBeforeItems[Index: Integer]: Boolean
      read GetPageBreaksBeforeItems write SetPageBreaksBeforeItems;
  end;


  procedure RVCheckUni(Length: Integer);

{$IFNDEF RVDONOTUSERTF}
procedure RVSaveFontToRTF(Stream: TStream; Font: TFont;
  ColorList: TRVColorList; FontTable: TRVRTFFontTable;
  RVStyle: TRVStyle);
{$ENDIF}

const
  RichViewPixelsPerInch : Integer = 0;
  RichViewSavePInHTML:    Boolean = False;

  rv_cssBkAttachment : array[TBackgroundStyle] of String
      = ('', 'fixed', 'fixed','scroll','fixed');
  rv_cssBkRepeat     : array[TBackgroundStyle] of String =
        ('','no-repeat', 'repeat','repeat','no-repeat');

procedure RV_RegisterHTMLGraphicFormat(ClassType: TGraphicClass);
function RV_IsHTMLGraphicFormat(gr: TGraphic): Boolean;
procedure RV_ReplaceStr(var str: String; oldstr, newstr: String);

implementation
uses RVFMisc, RVStr, RVRTFProps;

const RVF_DOCPROP_TEXTSTYLES  = 1;
      RVF_DOCPROP_PARASTYLES  = 2;
      RVF_DOCPROP_LISTSTYLES  = 4;
      RVF_DOCPROP_LAYOUT      = 3;
      RVF_DOCPROP_DOCPROPLIST = 5;
      RVF_DOCPROP_PREVMARKERS = 6;

const RVFVersion = 1;
      RVFSubVersion = 3;

const
  crlf = #13#10;
{==============================================================================}
{ Replaces in str all substrings oldstr with substring newstr.
  Case insensitive. Newstr CANNOT contain oldstr as a substring.               }
procedure RV_ReplaceStr(var str: String; oldstr, newstr: String);
var p: Integer;
begin
   while true do begin
     p := pos(oldstr, str);
     if p=0 then break;
     Delete(str,p, Length(oldstr));
     Insert(newstr, str, p);
   end;
end;
{------------------------------------------------------------------------------}
{ Raises an exception - error in processing Unicode text.                      }
procedure RVRaiseUni;
begin
  raise ERichViewError.Create(errRVUnicode);
end;
{------------------------------------------------------------------------------}
{ Raises an exception is Length is odd value. It's used to check lengths of
  "raw Unicode" string.                                                        }
procedure RVCheckUni(Length: Integer);
begin
  if Length mod 2 <> 0 then
    RVRaiseUni;
end;
{========================== HTML Graphic Classes ==============================}
{ List of HTML graphic classes.
  Pictures of HTML graphic classes will be saved in HTML without
  converting to Jpegs.
  Initialization: nilled.
  Finalization: freed and nilled.                                              }
var HTMLGraphicFormats: TList;
{------------------------------------------------------------------------------}
{ Registers the graphic class ClassType as an HTML graphic class.              }
procedure RV_RegisterHTMLGraphicFormat(ClassType: TGraphicClass);
begin
  if HTMLGraphicFormats=nil then
    HTMLGraphicFormats := TList.Create;
  if HTMLGraphicFormats.IndexOf(ClassType)<0 then
    HTMLGraphicFormats.Add(ClassType);
end;
{------------------------------------------------------------------------------}
{ Is this a picture of HTML graphic class?                                     }
function RV_IsHTMLGraphicFormat(gr: TGraphic): Boolean;
begin
  Result := (HTMLGraphicFormats<>nil) and
    (HTMLGraphicFormats.IndexOf(gr.ClassType)>=0)
end;
{================================ TRTFFontTable ===============================}
{ Returns an index of (FontName, Charset) item, or -1 if not found.
  Charset is not supported in D2/CB1 version.
  FontName is case insensitive.                                                }
function TRVRTFFontTable.Find(const FontName: String
  {$IFDEF RICHVIEWCBDEF3}; Charset: TFontCharset{$ENDIF}): Integer;
var i: Integer;
begin
  for i := 0 to Count-1 do
    if (AnsiCompareText(Items[i].FontName,FontName)=0)
       {$IFDEF RICHVIEWCBDEF3}
       and (Items[i].Charset = Charset)
       {$ENDIF}
       then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;
{------------------------------------------------------------------------------}
{ Adds (FontName, Charset) item if it does not present.
  In any case, returns an index of (FontName, Charset) item.
  Charset is not supported in D2/CB1 version.
  FontName is case insensitive.                                                }  
function TRVRTFFontTable.AddUnique(const FontName: String
 {$IFDEF RICHVIEWCBDEF3}; Charset: TFontCharset{$ENDIF}): Integer;
var item: TRVRTFFontTableItem;
begin
  Result := Find(FontName{$IFDEF RICHVIEWCBDEF3}, Charset{$ENDIF});
  if Result<0 then begin
    item := TRVRTFFontTableItem.Create;
    item.FontName := FontName;
    {$IFDEF RICHVIEWCBDEF3}
    item.Charset := Charset;
    {$ENDIF}
    Add(item);
    Result := Count-1;
  end;
end;
{------------------------------------------------------------------------------}
{ Reads Items[Index]                                                            }
function TRVRTFFontTable.Get(Index: Integer): TRVRTFFontTableItem;
begin
  Result := TRVRTFFontTableItem(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
{ Writes Items[Index]                                                          }
procedure TRVRTFFontTable.Put(Index: Integer; const Value: TRVRTFFontTableItem);
begin
  inherited Put(Index, Value);
end;
{============================ TRVLayoutInfo ===================================}
{ Constructor.                                                                 }
constructor TRVLayoutInfo.Create;
begin
  inherited Create;
  FirstMarkerListNo := -1;
end;
{------------------------------------------------------------------------------}
{ Loads iteslf from the Stream.
  If IncludeSize=True, first reads size (4 bytes) of the rest of data; reports
  error is the size is too small; reads at least size bytes (for compatibility
  with possible future extensions).                                            }
procedure TRVLayoutInfo.LoadFromStream(Stream: TStream; IncludeSize: Boolean);
var v: Integer;
const defsize1 = sizeof(Integer)*(4+2)+sizeof(TRVBiDiMode);
begin
   if IncludeSize then
     Stream.ReadBuffer(v, sizeof(Integer)); // ignoring
   Stream.ReadBuffer(v,            sizeof(Integer));
   Stream.ReadBuffer(v,            sizeof(Integer));
   if v<defsize1 then
     raise ERichViewError.Create(errRVFDocProp);
   Stream.ReadBuffer(LeftMargin,   sizeof(Integer));
   Stream.ReadBuffer(RightMargin,  sizeof(Integer));
   Stream.ReadBuffer(TopMargin,    sizeof(Integer));
   Stream.ReadBuffer(BottomMargin, sizeof(Integer));
   Stream.ReadBuffer(MinTextWidth, sizeof(Integer));
   Stream.ReadBuffer(MaxTextWidth, sizeof(Integer));
   Stream.ReadBuffer(BiDiMode,     sizeof(TRVBiDiMode));
   dec(v, defsize1);
   if v>=sizeof(Integer)*4 then begin
     Stream.ReadBuffer(FirstParaAborted, sizeof(Integer));
     Stream.ReadBuffer(LastParaAborted, sizeof(Integer));
     Stream.ReadBuffer(FirstMarkerListNo, sizeof(Integer));
     Stream.ReadBuffer(FirstMarkerLevel, sizeof(Integer));
     dec(v, sizeof(Integer)*4);
   end;
   if v>0 then
     Stream.Seek(v,soFromCurrent);
   Loaded := True;
end;
{------------------------------------------------------------------------------}
{ Saves itself to the stream.
  If IncluseSize=True, first saves its size (4 bytes).
  Size is usually processed by RVF loading procedures.                         }
procedure TRVLayoutInfo.SaveToStream(Stream: TStream; IncludeSize: Boolean);
var v,size: Integer;
const defsize1 = sizeof(Integer)*(4+2)+sizeof(TRVBiDiMode);
begin
   size := defsize1;
   if (FirstParaAborted<>0) or (LastParaAborted<>0) then
     inc(size, sizeof(Integer)*4);
   if IncludeSize then begin
     v := size+sizeof(Integer)*2;
     Stream.WriteBuffer(v, sizeof(Integer));
   end;
   v := 0;
   Stream.WriteBuffer(v,            sizeof(Integer));
   v := size;
   Stream.WriteBuffer(v,            sizeof(Integer));
   Stream.WriteBuffer(LeftMargin,   sizeof(Integer));
   Stream.WriteBuffer(RightMargin,  sizeof(Integer));
   Stream.WriteBuffer(TopMargin,    sizeof(Integer));
   Stream.WriteBuffer(BottomMargin, sizeof(Integer));
   Stream.WriteBuffer(MinTextWidth, sizeof(Integer));
   Stream.WriteBuffer(MaxTextWidth, sizeof(Integer));
   Stream.WriteBuffer(BiDiMode,     sizeof(TRVBiDiMode));
   if (FirstParaAborted<>0) or (LastParaAborted<>0) then begin
     Stream.WriteBuffer(FirstParaAborted, sizeof(Integer));
     Stream.WriteBuffer(LastParaAborted, sizeof(Integer));
     Stream.WriteBuffer(FirstMarkerListNo, sizeof(Integer));
     Stream.WriteBuffer(FirstMarkerLevel, sizeof(Integer));
   end;
end;
{------------------------------------------------------------------------------}
{ Loads itself from the hexadecimal string: extracts the string to a temporal
  memory stream, and calls LoadFromStream(..., False).                         }
procedure TRVLayoutInfo.LoadText(const s: String);
var TmpStream: TMemoryStream;
begin
   TmpStream := TMemoryStream.Create;
   try
     RVFTextString2Stream(s, TmpStream);
     TmpStream.Position := 0;
     LoadFromStream(TmpStream, False);
   finally
     TmpStream.Free;
   end;
end;
{------------------------------------------------------------------------------}
{ Loads itself from the binary string: copies the string to a temporal memory
  stream, and calls LoadFromStream(..., False).                                }
procedure TRVLayoutInfo.LoadBinary(const s: String);
var TmpStream: TMemoryStream;
begin
   TmpStream := TMemoryStream.Create;
   try
     TmpStream.WriteBuffer(PChar(s)^, Length(s));
     TmpStream.Position := 0;
     LoadFromStream(TmpStream, False);
   finally
     TmpStream.Free;
   end;
end;
{------------------------------------------------------------------------------}
{ Saves itself to stream as a hexadecimal string that can be loaded by
  LoadText.                                                                    }
procedure TRVLayoutInfo.SaveTextToStream(Stream: TStream);
var TmpStream: TMemoryStream;
    s: String;
begin
   TmpStream := TMemoryStream.Create;
   try
     SaveToStream(TmpStream, False);
     TmpStream.Position := 0;
     s := RVFStream2TextString(TmpStream);
     RVFWriteLine(Stream, s);
   finally
     TmpStream.Free;
   end;
end;
{$I+}
{================================ TCustomRVData ===============================}
constructor TCustomRVData.Create;
begin
  inherited Create;
  if not ShareItems then
    FItems := TStringList.Create;
  FAllowNewPara  := True;
  CPCount        := 0;
  State          := [];
end;
{------------------------------------------------------------------------------}
destructor TCustomRVData.Destroy;
begin
  Clear;
  if not ShareItems then
    FItems.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SavePicture(DocumentSaveFormat: TRVSaveFormat;
  const imgSavePrefix, Path: String; var imgSaveNo: Integer;
  OverrideFiles: Boolean; CurrentFileColor: TColor;
  gr: TGraphic): String;
var fn: String;
    bmp: TBitmap;
    ext: String;
    {$IFNDEF RVDONOTUSEJPEGIMAGE}
    jpg: TJpegImage;
    {$ENDIF}
begin
   {$IFNDEF RVDONOTUSEJPEGIMAGE}
   if DocumentSaveFormat=rvsfHTML then begin
     ext := '.jpg';
     if RV_IsHTMLGraphicFormat(gr) then
       ext := '.'+GraphicExtension(TGraphicClass(gr.ClassType));
     end
   else
     ext := '.bmp';
   {$ELSE}
   ext := '.bmp';
   {$ENDIF}
   fn := GetNextFileName(imgSavePrefix, Path, Ext, imgSaveNo, OverrideFiles);
   Result := ExtractFilePath(imgSavePrefix);
   if (Length(Result)>0) and (Result[Length(Result)]<>'\') then
     Result := Result+'\';
   Result := Result+ExtractFileName(fn);
   {$IFNDEF RVDONOTUSEJPEGIMAGE}
   if (DocumentSaveFormat=rvsfHTML) and
      ((gr is TJpegImage) or RV_IsHTMLGraphicFormat(gr)) then begin
     gr.SaveToFile(fn);
     exit;
   end;
   {$ENDIF}
   bmp := TBitmap.Create;
   try
     if gr is TBitmap then
       bmp.Assign(gr)
     else begin
       {$IFDEF RICHVIEWCBDEF3}
       bmp.PixelFormat := pf32bit;
       {$ENDIF}
       bmp.Height := gr.Height;
       bmp.Width := gr.Width;
       if CurrentFileColor=clNone then
         CurrentFileColor := clWhite;
       bmp.Canvas.Brush.Color := CurrentFileColor;
       bmp.Canvas.Pen.Color := CurrentFileColor;
       bmp.Canvas.FillRect(Rect(0,0,gr.Width,gr.Height));
       bmp.Canvas.Draw(0,0,gr);
     end;
     {$IFNDEF RVDONOTUSEJPEGIMAGE}
     if DocumentSaveFormat=rvsfHTML then begin
       jpg := TJpegImage.Create;
       try
         jpg.Assign(bmp);
         jpg.SaveToFile(fn);
       finally
         jpg.Free;
       end;
       end
     else
       bmp.SaveToFile(fn);
     {$ELSE}
       bmp.SaveToFile(fn);
     {$ENDIF}
   finally
     bmp.Free;
   end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.DoSavePicture(DocumentSaveFormat: TRVSaveFormat;
  const imgSavePrefix, Path: String; var imgSaveNo: Integer;
  OverrideFiles: Boolean; CurrentFileColor: TColor;
  gr: TGraphic): String;
var DoDefault: Boolean;
begin
   SaveImage2(gr, DocumentSaveFormat, Path, imgSavePrefix, imgSaveNo, Result,
     DoDefault);
   if not DoDefault then
     exit;
  Result := SavePicture(DocumentSaveFormat, imgSavePrefix, Path, imgSaveNo,
    OverrideFiles, CurrentFileColor, gr);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.ItemLength(ItemNo: Integer): Integer;
begin
  with GetItem(ItemNo) do
    if StyleNo<0 then
      Result := 1
    else
      Result := RVU_Length(Items[ItemNo], ItemOptions);
end;
{------------------------------------------------------------------------------}
{ Returns the file name in the directory Path. File name is built as
  ImagesPrefix + <number> + Ext.
  If OverrideFiles=True, <number> is imgSaveNo+1.
  If not, <number> is increased until file name does not belong to an existing
  file.
  On exit, imgSaveNo = <number>.
  Notes:
  - ImagesPrefix can contain path. It may be the full path (contains ':')
    or relative path. In the last case the file is assumed to be in
    Path + ExtractFilePath(ImagesPrefix).
  - It's assumed that the directory exists. }
function TCustomRVData.GetNextFileName(const ImagesPrefix, Path, Ext: String;
  var imgSaveNo: Integer; OverrideFiles: Boolean): String;
var FullPath: String;
begin
  if {$IFDEF RICHVIEWCBDEF3}AnsiPos{$ELSE}Pos{$ENDIF}(':',ImagesPrefix)>0 then
    FullPath := ImagesPrefix
  else
    FullPath := Path+ImagesPrefix;
  while True do begin
    inc(imgSaveNo);
    Result := FullPath+IntToStr(imgSaveNo)+Ext;
    if not FileExists(Result) then
      exit;
    {$WARNINGS OFF}
    if OverrideFiles and ((FileGetAttr(Result) and faReadOnly)=0) then
      exit;
    {$WARNINGS ON}
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddItem(const Text: String; Item: TCustomRVItemInfo);
var s: String;
begin
  if (Item.ParaNo=-1) and (Items.Count<>0) and
     not GetItem(Items.Count-1).GetBoolValue(rvbpFullWidth) then begin
    Item.SameAsPrev := True;
    Item.ParaNo := TCustomRVItemInfo(Items.Objects[Items.Count-1]).ParaNo;
    end
  else begin
    {$IFNDEF RVDONOTUSELISTS}
    if (Items.Count<>0) and (GetItemStyle(Items.Count-1)=rvsListMarker) then
      AddNL('',0,-1);
    {$ENDIF}
    Item.SameAsPrev := False;
    Item.BR := (Item.BR  or not FAllowNewPara) and not Item.GetBoolValue(rvbpFullWidth);
    if Item.ParaNo=-1 then
      Item.ParaNo := 0;
  end;
  if Item.Checkpoint<>nil then
    with Item.Checkpoint do
      AddNamedCheckpointExTag(Name, RaiseEvent, Tag);
  SetCP(Item, LastCP, NotAddedCP);
  Item.UpdatePaletteInfo(GetDoInPaletteMode, False, GetRVPalette, GetRVLogPalette);
  s := Text;
  Item.Inserting(Self, s, False);
  Items.AddObject(s, Item);
  Item.Inserted(Self, Items.Count-1);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddNLTag(const s: String; StyleNo, ParaNo, Tag: Integer);
var Item: TCustomRVItemInfo;
begin
  Item := RichViewTextItemClass.Create(Self);
  if StyleNo<0 then
    Item.StyleNo := rvsDefStyle
  else
    Item.StyleNo := StyleNo;
  Item.ParaNo  := ParaNo;
  Item.Tag     := Tag;
  {$IFNDEF RVDONOTUSEUNICODE}
  if (GetRVStyle<>nil) and (GetRVStyle.TextStyles[GetActualStyle(Item)].Unicode) then
    Include(Item.ItemOptions, rvioUnicode);
  {$ENDIF}
  AddItem(ReplaceTabs(s, GetActualStyle(Item), False), Item);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEUNICODE}
function TCustomRVData.GetTextInItemFormatA(ItemNo: Integer; const s: String): String;
begin
  if (GetItemStyle(ItemNo)>=0) and (rvioUnicode in GetItemOptions(ItemNo)) then
    Result := RVU_AnsiToUnicode(GetStyleCodePage(GetItemStyle(ItemNo)), s)
  else
    Result := s;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddNLWTagRaw(const s: String; StyleNo, ParaNo, Tag: Integer);
var ansis: String;
begin
  ansis := s;
  if StyleNo<0 then
    StyleNo := rvsDefStyle;
  if (GetRVStyle<>nil) and
     not GetRVStyle.TextStyles[GetActualStyle2(StyleNo, ParaNo)].Unicode then
    ansis := RVU_UnicodeToAnsi(GetStyleCodePage(GetActualStyle2(StyleNo, ParaNo)), ansis);
  AddNLTag(ansis, StyleNo, ParaNo, Tag);
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
function TCustomRVData.GetTextInItemFormatW(ItemNo: Integer; const s: WideString): String;
begin
  Result := RVU_GetRawUnicode(s);
  if (GetItemStyle(ItemNo)<0) or not (rvioUnicode in GetItemOptions(ItemNo)) then
    Result := RVU_UnicodeToAnsi(GetStyleCodePage(GetItemStyle(ItemNo)), Result);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddNLWTag(const s: WideString; StyleNo, ParaNo, Tag: Integer);
begin
  AddNLWTagRaw(RVU_GetRawUnicode(s), StyleNo, ParaNo, Tag);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetItemTextW(ItemNo: Integer): WideString;
var s: String;
begin
  s := Items[ItemNo];
  if (GetItemStyle(ItemNo)<0) or (not (rvioUnicode in GetItemOptions(ItemNo))) then
    s := RVU_AnsiToUnicode(GetStyleCodePage(GetItemStyle(ItemNo)), s);
  Result := RVU_RawUnicodeToWideString(s);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SetItemTextW(ItemNo: Integer; const s: WideString);
begin
  Items[ItemNo] := GetTextInItemFormatW(ItemNo, s);
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddNLATag(const s: String; StyleNo, ParaNo, Tag: Integer);
var ress: String;
    LParaNo: Integer;
begin
  LParaNo := ParaNo;
  if (StyleNo<0) or (StyleNo=rvsDefStyle) then begin
    StyleNo := rvsDefStyle;
    if LParaNo=-1 then begin
     if Items.Count<>0 then
       LParaNo := GetItemPara(Items.Count-1)
     else
       LParaNo := 0;
    end;
  end;
  if (GetRVStyle<>nil) and
     (GetRVStyle.TextStyles[GetActualStyle2(StyleNo, LParaNo)].Unicode) then
    ress := RVU_AnsiToUnicode(GetStyleCodePage(GetActualStyle2(StyleNo, LParaNo)), s)
  else
    ress := s;
  AddNLTag(ress, StyleNo, ParaNo, Tag);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetItemTextA(ItemNo: Integer): String;
begin
  Result := Items[ItemNo];
  if (GetItemStyle(ItemNo)>=0) and (rvioUnicode in GetItemOptions(ItemNo)) then
    Result := RVU_UnicodeToAnsi(GetStyleCodePage(GetItemStyle(ItemNo)), Result);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SetItemTextA(ItemNo: Integer; const s: String);
begin
  Items[ItemNo] := GetTextInItemFormatA(ItemNo, s);
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddNL(const s: String; StyleNo, ParaNo: Integer);
begin
  AddNLTag(s, StyleNo, ParaNo, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddFmt(const FormatStr: String; const Args: array of const;
  StyleNo, ParaNo: Integer);
begin
  AddNL(Format(FormatStr,Args), StyleNo, ParaNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddTextNL(s: String; StyleNo, FirstParaNo, OtherParaNo : Integer
                                  {$IFDEF RICHVIEWDEF4};Tag: Integer=0{$ENDIF});
var p: Integer;
    ParaNo: Integer;
    s2: String;
    {$IFDEF RICHVIEWDEF4}
    tap: Boolean;
    function GetTag: Integer;
    begin
      Result := RV_CopyTag(Tag, tap)
    end;
    {$ELSE}
    const GetTag = 0;
    {$ENDIF}
begin
   {$IFDEF RICHVIEWDEF4}
   tap := rvoTagsArePchars in Options;
   {$ENDIF}
   ParaNo := FirstParaNo;
   s := AdjustLineBreaks(s);
   repeat
     p := Pos(crlf,s);
     if p=0 then begin
       if (ParaNo<>-1) or (Length(s)>0) then
         AddNLTag(s,StyleNo,ParaNo,GetTag);
       break;
     end;
     s2 := Copy(s,1,p-1);
     if (ParaNo<>-1) or (Length(s2)>0) then
       AddNLTag(s2, StyleNo, ParaNo, GetTag);
     if FAllowNewPara then
       ParaNo := OtherParaNo;
     Delete(s,1, p+1);
   until False;
   {$IFDEF RICHVIEWDEF4}
   if tap and (Tag<>0) then
     StrDispose(PChar(Tag));
   {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddTextBlockNL(s: String; StyleNo, ParaNo: Integer
                                       {$IFDEF RICHVIEWDEF4};Tag: Integer=0{$ENDIF});
var p: Integer;
    FirstTime: Boolean;
    ANP: Boolean;
    {$IFDEF RICHVIEWDEF4}
    tap: Boolean;
    function GetTag: Integer;
    begin
      Result := RV_CopyTag(Tag, tap)
    end;
    {$ELSE}
    const GetTag = 0;
    {$ENDIF}
begin
   {$IFDEF RICHVIEWDEF4}
   tap := rvoTagsArePchars in Options;
   {$ENDIF}
   ANP := FAllowNewPara;
   SetAddParagraphMode(True);
   FirstTime := True;
   s:=AdjustLineBreaks(s);
   repeat
     p := Pos(crlf,s);
     if p=0 then begin
       AddNLTag(s,StyleNo,ParaNo, GetTag);
       SetAddParagraphMode(ANP);
       break;
     end;
     AddNLTag(System.Copy(s,1,p-1), StyleNo, ParaNo, GetTag);
     if FirstTime then begin
       SetAddParagraphMode(False);
       FirstTime := False;
     end;
     Delete(s,1, p+1);
   until False;
   {$IFDEF RICHVIEWDEF4}
   if tap and (Tag<>0) then
     StrDispose(PChar(Tag));
   {$ENDIF}   
end;
{------------------------------------------------------------------------------}
function TCustomRVData.AddNamedCheckpointExTag(const CpName: String;
                                               RaiseEvent: Boolean;
                                               Tag: Integer): Integer;
begin
  {$IFDEF RVALLOWCPBYCP}
  if NotAddedCP<>nil then begin
    Result := CPCount-1;
    exit;
  end;
  {$ELSE}
  if NotAddedCP<>nil then
    raise ERichViewError.Create(errCPByCP);
  {$ENDIF}
  NotAddedCP := TRVCPInfo.Create;
  NotAddedCP.Name := CPName;
  NotAddedCP.Tag := Tag;
  NotAddedCP.Next := nil;
  NotAddedCP.Prev := nil;
  //NotAddedCP.ItemNo := -1;
  NotAddedCP.RaiseEvent := RaiseEvent;
  Result := CPCount;
  inc(CPCount);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddBreakExTag(Width: Byte; Style: TRVBreakStyle;
                                      Color: TColor; Tag: Integer);
var Item: TRVBreakItemInfo;
begin
  Item := TRVBreakItemInfo.CreateEx(Self, Width, Style, Color);
  Item.SameAsPrev := False;
  Item.ParaNo     := 0;
  Item.Tag        := Tag;
  AddItem('',Item);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddBreakEx(Width: Byte; Style: TRVBreakStyle;
                                      Color: TColor);
begin
  AddBreakExTag(Width, Style, Color, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddBreakTag(Tag: Integer);
begin
  AddBreakExTag(1, rvbsLine, clNone, Tag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddBreak;
begin
  AddBreakTag(0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddHotspotExTag(const Name: String;
                                    ImageIndex, HotImageIndex: Integer;
                                    ImageList: TCustomImageList;
                                    ParaNo, Tag: Integer);
var Item: TRVHotspotItemInfo;
begin
  Item               := TRVHotspotItemInfo.CreateEx(Self, ImageIndex, HotImageIndex,
                                                    ImageList, rvvaBaseLine);
  Item.ParaNo        := ParaNo;
  Item.Tag := Tag;
  AddItem(Name, Item);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddHotspotEx(const Name: String;
                                     ImageIndex, HotImageIndex: Integer;
                                     ImageList: TCustomImageList;
                                     ParaNo: Integer);
begin
  AddHotspotExTag(Name, ImageIndex, HotImageIndex, ImageList, ParaNo, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddBulletExTag(const Name: String; ImageIndex: Integer;
                                       ImageList: TCustomImageList;
                                       ParaNo, Tag: Integer);
var Item: TRVBulletItemInfo;
begin
  Item            := TRVBulletItemInfo.CreateEx(Self, ImageIndex, ImageList, rvvaBaseline);
  Item.ParaNo     := ParaNo;
  Item.Tag        := Tag;
  AddItem(Name, Item);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddBulletEx(const Name: String; ImageIndex: Integer;
                                    ImageList: TCustomImageList;
                                    ParaNo: Integer);
begin
  AddBulletExTag(Name, ImageIndex, ImageList, ParaNo, 0)
end;
{------------------------------------------------------------------------------}
{ "gr" does not copied, do not free it!                                        }
procedure TCustomRVData.AddPictureExTag(const Name: String; gr: TGraphic;
                                       ParaNo: Integer; VAlign: TRVVAlign;
                                       Tag: Integer);
var Item: TRVGraphicItemInfo;
begin
  Item := TRVGraphicItemInfo.CreateEx(Self, gr, VAlign);
  Item.ParaNo  := ParaNo;
  Item.Tag     := Tag;
  AddItem(Name, Item);
end;
{------------------------------------------------------------------------------}
{ gr does not copied, do not free it!                                          }
procedure TCustomRVData.AddPictureEx(const Name: String; gr: TGraphic;
                                     ParaNo: Integer;
                                     VAlign: TRVVAlign);
begin
  AddPictureExTag(Name, gr, ParaNo, VAlign, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddHotPicture(const Name: String; gr: TGraphic;
  ParaNo: Integer; VAlign: TRVVAlign);
begin
  AddHotPictureTag(Name, gr, ParaNo, VAlign, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddHotPictureTag(const Name: String; gr: TGraphic;
  ParaNo: Integer; VAlign: TRVVAlign; Tag: Integer);
var Item: TRVHotGraphicItemInfo;
begin
  Item := TRVHotGraphicItemInfo.CreateEx(Self, gr, VAlign);
  Item.ParaNo  := ParaNo;
  Item.Tag     := Tag;
  AddItem(Name, Item);
end;
{------------------------------------------------------------------------------}
{ do not free ctrl yourself!                                                   }
procedure TCustomRVData.AddControlExTag(const Name: String; ctrl: TControl;
                                       ParaNo: Integer; VAlign: TRVVAlign;
                                       Tag: Integer);
var Item: TRVControlItemInfo;
begin
  Item         := TRVControlItemInfo.CreateEx(Self, ctrl, VAlign);
  Item.StyleNo := rvsComponent;
  Item.ParaNo  := ParaNo;
  Item.Tag     := Tag;
  AddItem(Name, Item);
  ctrl.Parent := GetParentControl;
end;
{------------------------------------------------------------------------------}
{ do not free ctrl yourself!                                                   }
procedure TCustomRVData.AddControlEx(const Name: String; ctrl: TControl;
                                     ParaNo: Integer;
                                     VAlign: TRVVAlign);
begin
  AddControlExTag(Name, ctrl, ParaNo, VAlign, 0);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SetAddParagraphMode(AllowNewPara: Boolean);
begin
 FAllowNewPara := AllowNewPara;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SetCP(Item: TCustomRVItemInfo; var PrevCP, CP: TRVCPInfo);
begin
  if CP=nil then
    exit;
  CP.Prev := PrevCP;
  CP.ItemInfo := Item;
  if (PrevCP=nil) then begin // inserting before first, making first
    if FirstCP<>nil then
      FirstCP.Prev := CP;
    CP.Next := FirstCP;
    FirstCP := CP;
    end
  else
    CP.Next := PrevCP.Next;
  if PrevCP<>nil then
    PrevCP.Next := CP;
  if CP.Next<>nil then
    CP.Next.Prev := CP;
  if PrevCP=LastCP then
    LastCP := CP;
  Item.Checkpoint := CP;
  CP := nil;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.UnlinkCheckpoint(cp: TRVCPInfo; DecCPCount: Boolean);
begin
  if cp<>nil then begin
    cp.ItemInfo := nil;
    if FirstCP = cp then FirstCP := cp.Next;
    if LastCP = cp  then LastCP  := cp.Prev;
    if cp.Prev<>nil then cp.Prev.Next := cp.Next;
    if cp.Next<>nil then cp.Next.Prev := cp.Prev;
    if DecCPCount then
      dec(CPCount);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.FreeCheckpoint(var cp: TRVCPInfo; AdjustLinks,DecCPCount: Boolean);
begin
  if cp<>nil then begin
    if AdjustLinks then
      UnlinkCheckpoint(cp,False);
    if rvoTagsArePChars in Options then
      StrDispose(PChar(cp.Tag));
    cp.Free;
    cp := nil;
    if DecCPCount then
      dec(CPCount);
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.ShareItems: Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.CanLoadLayout: Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.DeleteItems(FirstItemNo, Count: Integer);
var i: Integer;
begin
  if ShareItems then exit;
  if FirstItemNo>=Items.Count then exit;
  if FirstItemNo+Count>Items.Count then
    Count := Items.Count-FirstItemNo;
  Items.BeginUpdate;
  try
    for i := FirstItemNo to FirstItemNo+Count-1 do
      FreeItem(i,False);
    for i :=1 to Count do
      Items.Delete(FirstItemNo);
  finally
    Items.EndUpdate;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.DeleteSection(const CpName: String);
var startno, endno: Integer;
    cp: TRVCPInfo;
begin
  if ShareItems then exit;
   cp := FirstCP;
   startno := -1;
   endno := -1;
   while cp<>nil do begin
     if cp.Name=CpName then begin
       startno := Items.IndexOfObject(cp.ItemInfo);
       endno := Items.Count-1;
       break;
     end;
     cp := cp.Next;
   end;
   if startno=-1 then exit;
   cp := cp.Next;
   while cp<>nil do begin
     if cp.Name<>'' then begin
       endno := Items.IndexOfObject(cp.ItemInfo)-1;
       break;
     end;
     cp := cp.Next;
   end;
   DeleteItems(startno, endno-startno+1);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.InternalFreeItem(item: TCustomRVItemInfo; Clearing: Boolean);
begin
  if Item=nil then
    exit;
  {$IFNDEF RVDONOTUSELISTS}
  DeleteMarkerFromList(item, Clearing);
  {$ENDIF}
  FreeCheckpoint(Item.Checkpoint, True, True);
  if rvoTagsArePChars in Options then
    StrDispose(PChar(Item.Tag));
  Item.Free;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.FreeItem(ItemNo: Integer; Clearing: Boolean);
var item: TCustomRVItemInfo;
    s: String;
begin
  item := TCustomRVItemInfo(Items.Objects[ItemNo]);
  s := Items[ItemNo];
  ItemAction(rviaDestroying, item, s, Self);
  ControlAction(rvcaDestroy, ItemNo, item);
  InternalFreeItem(item, Clearing);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.Clear;
var i: Integer;
    Clearing: Boolean;
begin
  Clearing := rvstClearing in State;
  Include(State, rvstClearing);
  try
    Exclude(State, rvstFirstParaAborted);
    Exclude(State, rvstLastParaAborted);    
    if not ShareItems then begin
      Items.BeginUpdate;
      for i:=0 to Items.Count-1 do
        FreeItem(i,True);
      Items.Clear;
      Items.EndUpdate;
    end;
    FreeCheckpoint(NotAddedCP, False, True);
    FirstCP := nil;
    LastCP  := nil;
    if GetDocProperties<>nil then
      GetDocProperties.Clear;
  finally
    if not Clearing then
      Exclude(State, rvstClearing);  
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetOffsBeforeItem(ItemNo: Integer): Integer;
begin
  if GetItemStyle(ItemNo)<0 then
    Result := 0
  else
    Result := 1;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetOffsAfterItem(ItemNo: Integer): Integer;
begin
  if GetItemStyle(ItemNo)<0 then
    Result := 1
  else
    Result := RVU_Length(Items[ItemNo], GetItemOptions(ItemNo))+1;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.ReplaceTabs(const s: String; StyleNo: Integer;
  UnicodeDef: Boolean): String;
begin
  if GetRVStyle = nil then begin
    {$IFNDEF RVDONOTUSEUNICODE}
    if UnicodeDef then
      Result := RV_ReplaceTabsW(s,8)
    else
    {$ENDIF}
      Result := RV_ReplaceTabsA(s,8)
    end
  else
    {$IFNDEF RVDONOTUSEUNICODE}
    if GetRVStyle.TextStyles[StyleNo].Unicode then
      Result := RV_ReplaceTabsW(s, GetRVStyle.SpacesInTab)
    else
    {$ENDIF}
      Result := RV_ReplaceTabsA(s, GetRVStyle.SpacesInTab);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddStringFromFile(const s: String;
  StyleNo,ParaNo: Integer; AsSingleParagraph: Boolean;
  var FirstTime, PageBreak: Boolean);
begin
  {$IFNDEF RVDONOTUSEUNICODE}
  AddNLATag(s,StyleNo,ParaNo,0);
  {$ELSE}
  AddNLTag(s,StyleNo,ParaNo,0);
  {$ENDIF}
  if AsSingleParagraph and FirstTime then begin
    SetAddParagraphMode(False);
    FirstTime := False;
  end;
  if PageBreak then begin
    PageBreaksBeforeItems[Items.Count-1] := True;
    PageBreak := False;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddStringFF(var s: String; StyleNo,ParaNo: Integer;
  ProcessPageBreaks, AsSingleParagraph: Boolean; var FirstTime, PageBreak: Boolean);
var  P: Integer;
begin
  if ProcessPageBreaks then begin
    if s='' then
      AddStringFromFile(s, StyleNo,ParaNo, AsSingleParagraph, FirstTime, PageBreak);
    while s<>'' do begin
      P := RV_CharPos(PChar(s), #$0C, Length(s));
      if P<>0 then begin
        if P>1 then
          AddStringFromFile(Copy(s,1,P-1), StyleNo,ParaNo, AsSingleParagraph,
            FirstTime, PageBreak);
        s := Copy(s, P+1, Length(s));
        PageBreak := True;
        end
      else begin
        if s<>'' then
          AddStringFromFile(s, StyleNo, ParaNo, AsSingleParagraph,
            FirstTime, PageBreak);
        break;
      end;
    end;
    end
  else
    AddStringFromFile(s, StyleNo,ParaNo, AsSingleParagraph, FirstTime, PageBreak);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddUnixString(var s: String; StyleNo,ParaNo: Integer;
  ProcessPageBreaks, AsSingleParagraph: Boolean;
  var FirstTime, PageBreak: Boolean);
var PrevCRLoc, CRLoc, StartLoc: PChar;
    s2: String;
    Pos, Len: Integer;
begin
  StartLoc  := PChar(s);
  PrevCRLoc := StartLoc;
  Len       := Length(s);
  repeat
    Pos := RV_CharPos(PrevCRLoc, #10, Len);
    if Pos <> 0 then begin
      CRLoc := PrevCRLoc+Pos-1;
      s2 := Copy(s, PrevCRLoc-StartLoc+1, CRLoc-PrevCRLoc);
      dec(Len, Length(s2)+1);
      PrevCRLoc := CRLoc+1;
      end
    else
      s2 := Copy(s, PrevCRLoc-StartLoc+1, Length(s));
    AddStringFF(s2, StyleNo,ParaNo, ProcessPageBreaks, AsSingleParagraph,
      FirstTime, PageBreak);
  until Pos=0;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.LoadText(const FileName: String; StyleNo, ParaNo: Integer;
  AsSingleParagraph: Boolean): Boolean;
var f: TextFile;
    s: String;
    ANP: Boolean;
    FirstTime, ProcessPageBreaks, PageBreak: Boolean;
begin
  ANP := FAllowNewPara;
  FirstTime := True;
  Result := True;
  ProcessPageBreaks := SupportsPageBreaks;
  PageBreak         := False;
  try
    AssignFile(f, FileName);
    Reset(f);
    try
      while not eof(f) do begin
        ReadLn(f,s);
        Replace0(s);
        {$IFDEF RICHVIEWDEF6}
        AddStringFF(s, StyleNo, ParaNo, ProcessPageBreaks, AsSingleParagraph,
          FirstTime, PageBreak);
        {$ELSE}
        AddUnixString(s, StyleNo, ParaNo, ProcessPageBreaks, AsSingleParagraph,
          FirstTime, PageBreak);
        {$ENDIF}
      end;
    finally
      CloseFile(f);
      SetAddParagraphMode(ANP);
    end;
  except
    Result := False;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.LoadTextFromStream(Stream: TStream; StyleNo,
  ParaNo: Integer; AsSingleParagraph: Boolean):Boolean;
var FullText, s: String;
    ANP: Boolean;
    FirstTime, ProcessPageBreaks, PageBreak: Boolean;
    fulltextstartptr, startptr, ptr, endptr: PChar;
    SkipIfEqual: Char;
begin
  ANP := FAllowNewPara;
  FirstTime := True;
  Result := True;
  ProcessPageBreaks := SupportsPageBreaks;
  PageBreak         := False;
  try
    if Stream.Size=Stream.Position then
      exit;
    SetLength(FullText, Stream.Size-Stream.Position);
    Stream.ReadBuffer(PChar(FullText)^, Length(FullText));
    Replace0(FullText);
    fulltextstartptr := PChar(FullText);
    startptr := fulltextstartptr;
    ptr      := startptr;
    endptr   := PChar(FullText)+Length(FullText);
    SkipIfEqual := #0;
    while PChar(ptr)<PChar(endptr) do begin
     if (ptr^=SkipIfEqual) then begin
       SkipIfEqual := #0;
       inc(startptr);
       end
     else if (ptr^=#13) then begin
       s := System.Copy(FullText, startptr-fulltextstartptr+1, ptr-startptr);
       AddStringFF(s, StyleNo, ParaNo, ProcessPageBreaks, AsSingleParagraph,
         FirstTime, PageBreak);
       startptr := ptr+1;
       SkipIfEqual := #10;
       end
     else if (ptr^=#10) then begin
       s := System.Copy(FullText, startptr-fulltextstartptr+1, ptr-startptr);
       AddStringFF(s, StyleNo, ParaNo, ProcessPageBreaks, AsSingleParagraph,
         FirstTime, PageBreak);
       startptr := ptr+1;
       SkipIfEqual := #13;
     end;
     inc(ptr);
   end;
   s := System.Copy(FullText, startptr-fulltextstartptr+1, ptr-startptr);
   AddStringFF(s, StyleNo, ParaNo, ProcessPageBreaks, AsSingleParagraph,
     FirstTime, PageBreak);
  except
    Result := False;
  end;
  SetAddParagraphMode(ANP);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SaveTextToStream(const Path: String; Stream: TStream;
  LineWidth: Integer;
  SelectionOnly, TextOnly, Unicode, UnicodeWriteSignature: Boolean):Boolean;
var i, StartItemNo,EndItemNo,StartOffs,EndOffs: Integer;
    {$IFNDEF RVDONOTUSELISTS}
    MarkerItemNo: Integer;
    {$ENDIF}
    Item: TCustomRVItemInfo;
    s: String;
    NotUsedPart: TRVMultiDrawItemPart;
    CustomSave: Boolean;
    {$IFNDEF RVDONOTUSEUNICODE}
    UniSign: Word;
    {$ENDIF}
    {..................................................}
    function GetStr(Item:TCustomRVItemInfo; const s: String;
      CustomSave: Boolean) : String;
    begin
      {$IFNDEF RVDONOTUSEUNICODE}
      if not CustomSave then begin
        if (Item=nil) or (Item.StyleNo<0) then begin
          if Unicode and ((Item=nil) or not Item.GetBoolValue(rvbpCanSaveUnicode)) then
            Result := RVU_AnsiToUnicode(GetDefaultCodePage, s)
          else
            Result := s
          end
        else if (rvioUnicode in Item.ItemOptions) and not Unicode then
          Result := RVU_UnicodeToAnsi(GetDefaultCodePage, s)
        else if not (rvioUnicode in Item.ItemOptions) and Unicode then
          Result := RVU_AnsiToUnicode(GetStyleCodePage(GetActualStyle(Item)), s)
        else
          Result := s;
        end
      else
      {$ENDIF}
        Result := s;
    end;
    {..................................................}
    function GetTextStr(ItemNo, StartOffs, EndOffs: Integer;
      var CustomSave: Boolean): String;
    begin
      if StartOffs<0 then
        Result := Items[ItemNo]
      else
        Result := RVU_Copy(Items[ItemNo], StartOffs, EndOffs-StartOffs,
          GetItem(ItemNo).ItemOptions);
      CustomSave := SaveItemToFile(Path, Self, ItemNo, rvsfText, Unicode, Result);
    end;
    {..................................................}
    function GetNonTextStr(ItemNo, StartOffs, EndOffs: Integer;
      var CustomSave: Boolean): String;
    var SaveUnicode: Boolean;
        Item: TCustomRVItemInfo;
    begin
      CustomSave := False;
      Item := GetItem(ItemNo);
      Result := '';
      if (not TextOnly or Item.GetBoolValue(rvbpAlwaysInText)) and
         (StartOffs<EndOffs) then begin
        CustomSave := SaveItemToFile(Path, Self, ItemNo, rvsfText, Unicode, Result);
        if not CustomSave then begin
          {$IFNDEF RVDONOTUSEUNICODE}
          SaveUnicode := Unicode and Item.GetBoolValue(rvbpCanSaveUnicode);
          {$ELSE}
          SaveUnicode := False;
          {$ENDIF}
          Result := GetItem(ItemNo).AsText(LineWidth, Self, Items[ItemNo], Path,
            TextOnly, SaveUnicode);
        end;
      end;
    end;
    {..................................................}
begin
  try
    Result := True;
    RVFGetLimits(GetRVFSaveScope(SelectionOnly),StartItemNo,EndItemNo,StartOffs,EndOffs,NotUsedPart,NotUsedPart);
    if (StartItemNo=-1) or (StartItemNo>EndItemNo) then
      exit;
    {$IFNDEF RVDONOTUSEUNICODE}
    if Unicode and UnicodeWriteSignature then begin
      UniSign := UNI_LSB_FIRST;
      Stream.WriteBuffer(UniSign, 2);
    end;
    {$ENDIF}
    {$IFNDEF RVDONOTUSELISTS}
    if SelectionOnly then begin
      MarkerItemNo := GetFirstParaSectionItem(StartItemNo);
      if GetItemStyle(MarkerItemNo)=rvsListMarker then begin
        s := GetNonTextStr(MarkerItemNo, 0, 1, CustomSave);
        RVFWrite(Stream, GetStr(GetItem(MarkerItemNo), s, CustomSave));
      end;
    end;
    {$ENDIF}
    Item := GetItem(StartItemNo);
    if StartItemNo = EndItemNo then begin
      if Item.StyleNo<0 then
        s := GetNonTextStr(StartItemNo, StartOffs, EndOffs, CustomSave)
      else
        s := GetTextStr(StartItemNo, StartOffs, EndOffs, CustomSave);
      RVFWrite(Stream, GetStr(Item, s, CustomSave));
      end
    else begin
      if Item.StyleNo < 0 then
        s := GetNonTextStr(StartItemNo, StartOffs, 1, CustomSave)
      else
        s := GetTextStr(StartItemNo, StartOffs, RVU_Length(Items[StartItemNo],
          Item.ItemOptions)+1, CustomSave);
      RVFWrite(Stream, GetStr(Item, s, CustomSave));
      for i := StartItemNo+1 to EndItemNo-1 do begin
        Item := GetItem(i);
        if not Item.SameAsPrev then
          RVFWrite(Stream, GetStr(nil, crlf, False));
        if Item.StyleNo < 0 then
          s := GetNonTextStr(i, 0, 1, CustomSave)
        else
          s := GetTextStr(i, -1, -1, CustomSave);
        RVFWrite(Stream, GetStr(Item, s, CustomSave));
      end;
      Item := GetItem(EndItemNo);
      if not Item.SameAsPrev then
        RVFWrite(Stream, GetStr(nil, crlf, False));
       if Item.StyleNo < 0 then
        s := GetNonTextStr(EndItemNo, 0, EndOffs, CustomSave)
      else
        s := GetTextStr(EndItemNo, 1, EndOffs, CustomSave);
      RVFWrite(Stream, GetStr(Item, s, CustomSave));
    end;
  except
    Result := False;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SaveText(const FileName: String; LineWidth: Integer;
  Unicode: Boolean): Boolean;
var Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Result := SaveTextToStream(ExtractFilePath(FileName), Stream, LineWidth,
        False, False, Unicode, Unicode);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEUNICODE}
procedure TCustomRVData.AddTextNLW(const s: String; StyleNo, FirstParaNo,
  OtherParaNo : Integer; DefAsSingleParagraph: Boolean);
var
    ParaNo: Integer;
    startptr,ptr,endptr: PWord;
    SkipIfEqual: Word;
    ANP: Boolean;
    ProcessPageBreaks, PageBreak: Boolean;
    {.................................}
    procedure AddS;
    var str: String;
    begin
      if PageBreak and (startptr=ptr) then
        exit;
      if (ParaNo<>-1) or (startptr<>ptr) then begin
        str := Copy(s, PChar(startptr)-PChar(s)+1, PChar(ptr)-PChar(startptr));
        AddNLWTagRaw(str, StyleNo, ParaNo, 0);
      end;
      if FAllowNewPara then
        ParaNo := OtherParaNo;
      startptr := ptr;
      inc(PChar(startptr),2);
      if PageBreak then begin
        PageBreaksBeforeItems[Items.Count-1] := True;
        PageBreak := False;
      end;
    end;
    {.................................}
begin
   ANP := FAllowNewPara;
   if Length(s)=0 then begin
     if FirstParaNo<>-1 then
       AddNL(s, StyleNo, FirstParaNo);
     exit;
   end;
   RVCheckUni(Length(s));
   startptr := PWord(s);
   ptr      := startptr;
   endptr   := PWord(PChar(s)+Length(s));
   ParaNo := FirstParaNo;
   SkipIfEqual := 0;
   SetAddParagraphMode(not DefAsSingleParagraph);
   ProcessPageBreaks := SupportsPageBreaks;
   PageBreak         := False;
   while PChar(ptr)<PChar(endptr) do begin
     if (ptr^=UNI_LineSeparator) or
        (ptr^=UNI_ParagraphSeparator) or
        (ptr^=UNI_VerticalTab) or
        (ptr^=0) then begin
       SetAddParagraphMode(ptr^=UNI_ParagraphSeparator);
       AddS;
       SkipIfEqual := 0;
       end
     else if (ptr^=SkipIfEqual) then begin
       SkipIfEqual := 0;
       inc(PChar(startptr),2);
       end
     else if (ptr^=UNI_CR) then begin
       SetAddParagraphMode(not DefAsSingleParagraph);
       AddS;
       SkipIfEqual := UNI_LF;
       end
     else if (ptr^=UNI_LF) then begin
       SetAddParagraphMode(not DefAsSingleParagraph);
       AddS;
       SkipIfEqual := UNI_CR;
       end
     else if (ptr^=UNI_FF) then begin
       SetAddParagraphMode(not DefAsSingleParagraph);
       if startptr<>ptr then
         AddS;
       PChar(startptr) := PChar(ptr)+2;
       PageBreak := ProcessPageBreaks;
       SkipIfEqual := 0;
       end
     else begin
       ;
     end;
     inc(PChar(ptr), 2);
   end;
   AddS;
   SetAddParagraphMode(ANP);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.LoadTextFromStreamW(Stream: TStream; StyleNo, ParaNo: Integer;
      DefAsSingleParagraph: Boolean):Boolean;
var s: String;
    FirstChar: Word;
    SwapBytes: Boolean;
begin
  Result := True;
  RVCheckUni(Stream.Size-Stream.Position);
  try
    SwapBytes := False;
    if Stream.Size>0 then begin
      Stream.ReadBuffer(FirstChar, 2);
      if (FirstChar=UNI_LSB_FIRST) or
         (FirstChar=UNI_MSB_FIRST) then
        SwapBytes := FirstChar=UNI_MSB_FIRST
      else
        Stream.Seek(-2, soFromCurrent);
    end;
    SetLength(s, Stream.Size-Stream.Position);
    Stream.ReadBuffer(PChar(s)^,Stream.Size-Stream.Position);
    if SwapBytes then
      RVU_SwapWordBytes(PWord(s), Length(s) div 2);
    AddTextNLW(s, StyleNo, ParaNo, ParaNo, DefAsSingleParagraph);
  except
    Result := False;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.LoadTextW(const FileName: String; StyleNo, ParaNo: Integer;
  DefAsSingleParagraph: Boolean): Boolean;
var Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead);
  try
    Result := LoadTextFromStreamW(Stream, StyleNo, ParaNo, DefAsSingleParagraph);
  finally
    Stream.Free;
  end;
end;
{$ENDIF}
{$IFNDEF RVDONOTUSEHTML}
{------------------------------------------------------------------------------}
function TCustomRVData.SaveHTML(const FileName, Title,
  ImagesPrefix: String; Options: TRVSaveOptions; Color: TColor;
  var imgSaveNo: Integer;
  LeftMargin, TopMargin, RightMargin, BottomMargin: Integer;
  Background: TRVBackground): Boolean;
var Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Result := SaveHTMLToStream(Stream, ExtractFilePath(FileName),
                                 Title, ImagesPrefix, Options,
                                 Color, imgSaveNo,
                                 LeftMargin, TopMargin,
                                 RightMargin, BottomMargin,
                                 Background, nil);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SaveHTMLEx(const FileName, Title, ImagesPrefix,
  ExtraStyles, ExternalCSS, CPPrefix: String; Options: TRVSaveOptions;
  Color: TColor; var CurrentFileColor: TColor; var imgSaveNo: Integer;
  LeftMargin, TopMargin, RightMargin, BottomMargin: Integer;
  Background: TRVBackground):Boolean;
var Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      Result := SaveHTMLToStreamEx(Stream, ExtractFilePath(FileName),
        Title, ImagesPrefix, ExtraStyles, ExternalCSS, CPPrefix, Options,
        Color, CurrentFileColor, imgSaveNo, LeftMargin, TopMargin,
        RightMargin, BottomMargin, Background, nil);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.ShouldSaveTextToHTML(StyleNo: Integer): Boolean;
begin
  with GetRVStyle.TextStyles[StyleNo] do
    Result := (rvteoHTMLCode in Options) or not (rvteoRTFCode in Options)
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetHTMLATag(ItemNo: Integer; CSS: String): String;
var Target, Extras: String;
begin
  WriteHyperlink(GetItem(ItemNo).JumpID+FirstJumpNo, Self, ItemNo, rvsfHTML,
    Target, Extras);
  if (Target<>'') or (Extras<>'') then begin
    if Extras<>'' then
      Extras := ' '+Extras;
    if CSS<>'' then
      CSS := ' '+CSS;
    Result := Format('<A%s href="%s"%s>',[CSS, Target, Extras]);
    end
  else
    Result := '';
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SaveHTMLCheckpoint(Stream: TStream;
  Checkpoint: TRVCPInfo; var cpno: Integer; const Prefix: String;
  FromNewLine: Boolean; Options: TRVSaveOptions);
begin
  if Checkpoint<>nil then begin
    if FromNewLine then
      RVWriteLn(Stream,'');
    if (rvsoUseCheckpointsNames in Options) and (Checkpoint.Name<>'') then
      RVWriteLn(Stream,'<A name="'+Checkpoint.Name+'"></A>')
    else
      RVWriteLn(Stream,'<A name='+Prefix+IntToStr(cpno)+'></A>');
    inc(cpno);
  end;
end;
{------------------------------------------------------------------------------}
{ Returns text string for saving to HTML. Path - path for saving HTML (pictures).
  ItemNo - index of text item to save.
  Calls OnSaveItemToHTML, if assigned.                                         }
function TCustomRVData.GetTextForHTML(const Path: String; ItemNo: Integer): String;
var Item: TCustomRVItemInfo;
begin
  Result := Items[ItemNo];
  if not SaveItemToFile(Path, Self, ItemNo, rvsfHTML, False, Result) then begin
    Item := GetItem(ItemNo);
    {$IFNDEF RVDONOTUSEUNICODE}
    if rvioUnicode in Item.ItemOptions then
      Result := RVU_GetHTMLEncodedUnicode(Result, True,
        rvteoHTMLCode in GetRVStyle.TextStyles[GetActualStyle(Item)].Options)
    else
    {$ENDIF}
      Result := RV_MakeHTMLStr(Result, True,
        rvteoHTMLCode in GetRVStyle.TextStyles[GetActualStyle(Item)].Options);
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SaveHTMLToStream(Stream: TStream; const Path, Title,
  ImagesPrefix: String; Options: TRVSaveOptions; Color: TColor;
  var imgSaveNo: Integer; LeftMargin, TopMargin, RightMargin, BottomMargin: Integer;
  Background: TRVBackground; Bullets: TRVList): Boolean;
    {......................................................}
    procedure WriteExtraHTMLCode(Area: TRVHTMLSaveArea; AddSpace: Boolean);
    var s: String;
    begin
      s := GetExtraHTMLCode(Area, False);
      if s<>'' then
        if AddSpace then
          RVWrite(Stream,' '+s)
        else
          RVWrite(Stream, s);
    end;
    {...........................................................}
    procedure SaveFirst(Stream: TStream; const Path, Title: String);
    var s: String;
    begin
      RVWriteLn(Stream,'<HTML><HEAD><TITLE>'+Title+'</TITLE>');
      {$IFDEF RICHVIEWCBDEF3}
      s := RV_CharSet2HTMLLang(GetRVStyle.TextStyles[0].CharSet);
      if s<>'' then
        RVWriteLn(Stream,SysUtils.Format('<META HTTP-EQUIV="Content-Type"  CONTENT="text/html; CHARSET=%s">',[s]));
      {$ENDIF}
      WriteExtraHTMLCode(rv_thms_Head, False);
      RVWriteLn(Stream,'</HEAD>');
      RVWrite(Stream,'<BODY');
      if Color<>clNone then
         RVWrite(Stream,' bgcolor='+RV_GetHTMLRGBStr(Color, True));
      if (Background.Style<>bsNoBitmap) and
         (not Background.Bitmap.Empty) then begin
         s := SaveBackgroundToHTML(Background.Bitmap, Color, Path, ImagesPrefix,
           imgSaveNo, rvsoOverrideImages in Options);
         if s<>'' then begin
           RVWrite(Stream, Format(' background="%s"', [s]));
           if (Background.Style<>bsTiledAndScrolled) then
             RVWrite(Stream,' bgproperties=fixed');
         end;
      end;
      WriteExtraHTMLCode(rv_thms_BodyAttribute, True);
      RVWriteLn(Stream, Format(' leftmargin=%d topmargin=%d rightmargin=%d bottommargin=%d>',
                               [LeftMargin, TopMargin, RightMargin, BottomMargin]));
      WriteExtraHTMLCode(rv_thms_Body, False);
    end;
    {...........................................................}
    procedure SaveLast(Stream: TStream);
    begin
      WriteExtraHTMLCode(rv_thms_End, False);
      RVWriteLn(Stream,'</BODY></HTML>');
    end;
    {......................................................}
    function GetPageBreakCSS(item: TCustomRVItemInfo): String;
    begin
      if (rvsoForceNonTextCSS in Options) and item.PageBreakBefore then 
        Result := ' style="page-break-before: always;"'
      else
        Result := '';
    end;
    {...........................................................}
    function GetOpenDIVTag(Align: TRVAlignment; item: TCustomRVItemInfo): String;
    var s: String;
    begin
      case Align of
        rvaCenter:
          Result := '<%s align=center%s>';
        rvaRight:
          Result := '<%s align=right%s>';
        rvaJustify:
          Result := '<%s align=justify%s>';
        else
          Result := '<%s%s>';
      end;
      if RichViewSavePInHTML then
        s := 'P'
      else
        s := 'DIV';
      Result := SysUtils.Format(Result, [s, GetPageBreakCSS(item)]);
    end;
    {...........................................................}
    function GetCloseDIVTag: String;
    begin
      if RichViewSavePInHTML then
        Result := '</P>'
      else
        Result := '</DIV>';
    end;
    {...........................................................}
    procedure SaveMiddle(Stream: TStream;
                         const Path: String);
    var
      i: Integer;
      item: TCustomRVItemInfo;
      CloseDIV: Boolean;
      s2, ATag, HintTag: String;
      cpno: Integer;
      CreateBulletList: Boolean;
      RVStyle: TRVStyle;
      {$IFNDEF RVDONOTUSELISTS}
      marker: TRVMarkerItemInfo;
      {$ENDIF}
    begin
      cpno := 0;
      {$IFNDEF RVDONOTUSELISTS}
      marker := nil;
      {$ENDIF}
      RVStyle := GetRVStyle;
      CreateBulletList := Bullets=nil;
      if CreateBulletList then
        Bullets := TRVList.Create;
      try
        if not (rvsoDefault0Style in Options) then
          RVWriteLn(Stream,RV_HTMLOpenFontTag(RVStyle.TextStyles[0],
            RVStyle.TextStyles[0], False));
        CloseDIV := False;
        for i:=0 to Items.Count-1 do begin
          item := GetItem(i);
          if not item.SameAsPrev then begin
            if item.BR then
              RVWriteLn(Stream,'<BR>')
            else begin
              {$IFNDEF RVDONOTUSELISTS}
              if marker<>nil then begin
                RVWriteLn(Stream,'</LI>');
                RVWrite(Stream, GetParaHTMLCode(Self, i, False, False));
              end;
              {$ENDIF}
              if CloseDIV then begin
                RVWriteLn(Stream,GetCloseDIVTag);
                RVWrite(Stream, GetParaHTMLCode(Self, i, False, False));
                CloseDIV := False;
              end;
            end;
            if not item.BR then
              case item.StyleNo of
                rvsBreak: ;
                {$IFNDEF RVDONOTUSELISTS}
                rvsListMarker:
                  begin
                    if (rvsoMarkersAsText in Options) or
                       (TRVMarkerItemInfo(item).GetLevelInfo(RVStyle)=nil) then begin
                      RVWrite(Stream, GetParaHTMLCode(Self, i, True, False));
                      RVWrite(Stream, GetOpenDIVTag(RVStyle.ParaStyles[item.ParaNo].Alignment, item));
                      CloseDIV := True;
                      end
                    else begin
                      TRVMarkerItemInfo(item).SaveHTMLSpecial(Stream, marker, RVStyle, False);
                      RVWrite(Stream, GetParaHTMLCode(Self, i, True, False));
                      marker := TRVMarkerItemInfo(item);
                      if marker.GetLevelInfo(RVStyle).HasNumbering then
                        RVWrite(Stream,Format('<LI value=%d%s>',[marker.Counter, GetPageBreakCSS(marker)]))
                      else
                        RVWrite(Stream,Format('<LI%s>',[GetPageBreakCSS(marker)]));
                    end;
                  end;
                {$ENDIF}
                else
                  begin
                    {$IFNDEF RVDONOTUSELISTS}
                    if marker<>nil then begin
                      marker.HTMLOpenOrCloseTags(Stream, marker.Level, -1, RVStyle, False);
                      marker := nil;
                    end;
                    {$ENDIF}
                    RVWrite(Stream, GetParaHTMLCode(Self, i, True, False));
                    RVWrite(Stream, GetOpenDIVTag(RVStyle.ParaStyles[item.ParaNo].Alignment, item));
                    CloseDIV := True;
                  end;
              end;
          end;
          SaveHTMLCheckpoint(Stream, item.Checkpoint, cpno, RVDEFAULTCHECKPOINTPREFIX, True, Options);
          ATag := '';
          HintTag := '';
          if item.GetBoolValueEx(rvbpJump, RVStyle) then
            ATag := GetHTMLATag(i, '');
          if ATag<>'' then
            RVWrite(Stream, ATag)
          else begin
            item.GetExtraStrProperty(rvespHint, HintTag);
            if HintTag<>'' then
              HintTag := RV_GetHintStr(rvsfHTML, HintTag);
          end;
          if item.StyleNo<0 then begin // non-text
            s2 := '';
            if SaveItemToFile(Path, Self, i, rvsfHTML, False, s2) then
              RVWrite(Stream, s2)
            else begin
              item.SaveToHTML(Stream, Self, i, Items[i], Path, ImagesPrefix,
                imgSaveNo, Color, Options, False, Bullets);
              if item.StyleNo=rvsBreak then
                RVWriteLn(Stream,'');
            end;
            end
          else if ShouldSaveTextToHTML(GetActualStyle(item)) then begin // text
            if HintTag<>'' then
              RVWrite(Stream, '<SPAN '+HintTag+'>');
            s2 := GetTextForHTML(Path, i);
            if GetActualStyle(item)=0 then
              RVWrite(Stream, s2)
            else
              RVWrite(Stream,
                RV_HTMLOpenFontTag(RVStyle.TextStyles[GetActualStyle(item)], RVStyle.TextStyles[0], True)+
                s2+
                RV_HTMLCloseFontTag(RVStyle.TextStyles[GetActualStyle(item)],RVStyle.TextStyles[0], True));
            if HintTag<>'' then
              RVWrite(Stream, '</SPAN>');
          end;
          if ATag<>'' then
            RVWrite(Stream,'</A>');
        end;
        {$IFNDEF RVDONOTUSELISTS}
        if marker<>nil then begin
          RVWriteLn(Stream,'</LI>');
          RVWrite(Stream, GetParaHTMLCode(Self, ItemCount, False, False));
          marker.HTMLOpenOrCloseTags(Stream, marker.Level, -1, RVStyle, False);
        end;
        {$ENDIF}
        if CloseDIV then begin
          RVWriteLn(Stream,GetCloseDIVTag);
          RVWrite(Stream, GetParaHTMLCode(Self, ItemCount, False, False));
        end;
        if not (rvsoDefault0Style in Options) then
          RVWriteLn(Stream,
                  RV_HTMLCloseFontTag(RVStyle.TextStyles[0], RVStyle.TextStyles[0], False));
        SaveHTMLCheckpoint(Stream, NotAddedCP, cpno, RVDEFAULTCHECKPOINTPREFIX, False, Options);
      finally
        if CreateBulletList then begin
          Bullets.Free;
        end;
      end;
    end;
    {...........................................................}
begin
  Result := False;
  if GetRVStyle = nil then exit;
  Result := True;
  try
    if not (rvsoMiddleOnly in Options) and
       not (rvsoLastOnly in Options) then
       SaveFirst(Stream, Path, Title);
    if not (rvsoFirstOnly in Options) and
       not (rvsoLastOnly in Options) then
       SaveMiddle(Stream, Path);
    if not (rvsoFirstOnly in Options) and
       not (rvsoMiddleOnly in Options) then
       SaveLast(Stream);
  except
    Result := False;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SaveBackgroundToHTML(bmp: TBitmap; Color: TColor;
  const Path, ImagesPrefix: String; var imgSaveNo: Integer;
  OverrideImages: Boolean): String;
var DoDefault: Boolean;
begin
  Result := '';
  HTMLSaveImage(Self, -1, Path, Color, Result, DoDefault);
  if DoDefault then
    Result := RV_GetHTMLPath(DoSavePicture(rvsfHTML, ImagesPrefix, Path,
      imgSaveNo, OverrideImages, Color, bmp));
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SaveHTMLToStreamEx(Stream: TStream; const Path,
  Title, ImagesPrefix, ExtraStyles, ExternalCSS, CPPrefix: String;
  Options: TRVSaveOptions; Color: TColor; var CurrentFileColor: TColor;
  var imgSaveNo: Integer; LeftMargin, TopMargin, RightMargin, BottomMargin: Integer;
  Background: TRVBackground; Bullets: TRVList): Boolean;
    {......................................................}
    procedure WriteExtraHTMLCode(Area: TRVHTMLSaveArea; AddSpace: Boolean);
    var s: String;
    begin
      s := GetExtraHTMLCode(Area, True);
      if s<>'' then
        if AddSpace then
          RVWrite(Stream,' '+s)
        else
          RVWrite(Stream, s);
    end;
    {......................................................}
    procedure SaveFirst(Stream: TStream;
      const Path,Title,ExtraStyles,ExternalCSS: String);
    var s: String;
        CSSOptions: TRVSaveCSSOptions;
    begin
      RVWriteLn(Stream,'<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">');
      RVWriteLn(Stream,'<HTML><HEAD><TITLE>'+Title+'</TITLE>');
      {$IFDEF RICHVIEWCBDEF3}
      s := RV_CharSet2HTMLLang(GetRVStyle.TextStyles[0].CharSet);
      if s<>'' then
        RVWriteLn(Stream,SysUtils.Format('<META HTTP-EQUIV="Content-Type"  CONTENT="text/html; CHARSET=%s">',[s]));
      {$ENDIF}
      RVWriteLn(Stream,'<STYLE type="text/css"><!--');
      s := RV_GetHTMLRGBStr(Color, False);
      RVWriteLn(Stream, 'BODY {');
      RVWriteLn(Stream, SysUtils.Format('  margin: %dpx %dpx %dpx %dpx;',
                [TopMargin, RightMargin, BottomMargin, LeftMargin]));
      if s<>'' then
        RVWriteLn(Stream, '  background-color: '+s+';');
      if (Background.Style<>bsNoBitmap) and
         (not Background.Bitmap.Empty) then begin
         s := SaveBackgroundToHTML(Background.Bitmap, Color, Path, ImagesPrefix,
           imgSaveNo, rvsoOverrideImages in Options);
         if s<>'' then begin
           RVWriteLn(Stream, SysUtils.Format('  background-image: url("%s");', [s]));
           RVWriteLn(Stream, '  background-repeat: '+rv_cssBkRepeat[Background.Style]+';');
           RVWriteLn(Stream, '  background-attachment: '+rv_cssBkAttachment[Background.Style]+';');
           if Background.Style in [bsStretched, bsCentered] then
             RVWriteLn(Stream, '  background-position: center center;');
         end;
      end;
      RVWriteLn(Stream, '}');
      if (ExternalCSS='') and not (rvsoInlineCSS in Options) then begin
        CSSOptions := [];
        if (rvsoNoDefCSSStyle in Options) then
          Include(CSSOptions, rvcssNoDefCSSStyle);
        GetRVStyle.SaveCSSToStream(Stream, CSSOptions);
      end;
      RVWriteLn(Stream, ExtraStyles);
      RVWriteLn(Stream,'--></STYLE>');
      if (ExternalCSS<>'') and not (rvsoInlineCSS in Options) then
        RVWriteLn(Stream, '<LINK type="text/css" href="'+ExternalCSS+'" rel=STYLESHEET>');
      WriteExtraHTMLCode(rv_thms_Head, False);
      RVWriteLn(Stream,'</HEAD>');
      RVWrite(Stream,'<BODY');
      WriteExtraHTMLCode(rv_thms_BodyAttribute, True);
      RVWriteLn(Stream,'>');
      WriteExtraHTMLCode(rv_thms_Body, False);
    end;
    {......................................................}
    procedure SaveLast(Stream: TStream);
    begin
      RVWriteLn(Stream,'');
      WriteExtraHTMLCode(rv_thms_End, False);
      RVWriteLn(Stream,'</BODY></HTML>');
    end;
    {......................................................}
    function GetTextCSS(item: TCustomRVItemInfo; RVStyle: TRVStyle): String;
    var MemoryStream: TStream;
    begin
      if (rvsoInlineCSS in Options) then begin
        MemoryStream := TMemoryStream.Create;
        try
          RVStyle.TextStyles[GetActualStyle(item)].SaveCSSToStream(MemoryStream,
            nil, False);
          SetLength(Result, MemoryStream.Size);
          MemoryStream.Position := 0;
          MemoryStream.ReadBuffer(PChar(Result)^, Length(Result));
        finally
          MemoryStream.Free;
        end;
        Result := 'style="'+Result+'"';
        end
      else
        Result := 'class=RVTS'+IntToStr(GetActualStyle(item));
    end;
    {......................................................}
    function GetPageBreakCSS(item: TCustomRVItemInfo; OnlyValue, SpaceBefore: Boolean): String;
    begin
      if item.PageBreakBefore then begin
        Result := 'page-break-before: always;';
        if not OnlyValue then
          Result := 'style="'+Result+'"';
        end
      else
        Result := '';
      if (Result<>'') and SpaceBefore then
        Result := ' '+Result;
    end;
    {......................................................}
    function GetParaCSSValue(item: TCustomRVItemInfo; RVStyle: TRVStyle;
      IgnoreLeftIndents: Boolean): String;
    var MemoryStream: TStream;
    begin
      if (rvsoInlineCSS in Options) then begin
        MemoryStream := TMemoryStream.Create;
        try
          RVStyle.ParaStyles[item.ParaNo].SaveCSSToStream(MemoryStream, nil,
            False, False, IgnoreLeftIndents);
          SetLength(Result, MemoryStream.Size);
          MemoryStream.Position := 0;
          MemoryStream.ReadBuffer(PChar(Result)^, Length(Result));
        finally
          MemoryStream.Free;
        end;
        end
      else
        Result := '';
      Result := Result+GetPageBreakCSS(item, True, Result<>'')
    end;
    {......................................................}
    function GetParaCSS(item: TCustomRVItemInfo; RVStyle: TRVStyle;
      IgnoreLeftIndents: Boolean): String;
    begin
      if (rvsoInlineCSS in Options) then
        Result := 'style="'+GetParaCSSValue(item, RVStyle, IgnoreLeftIndents)+'"'
      else if (Item.ParaNo>0) or (rvsoNoDefCSSStyle in Options) then
        Result := 'class=RVPS'+IntToStr(item.ParaNo)+GetPageBreakCSS(item, False, True)
      else
        Result := GetPageBreakCSS(item, False, False);
    end;
    {......................................................}
    procedure SaveMiddle(Stream: TStream; const Path: String; CPPrefix: String);
    var i: Integer;
        item: TCustomRVItemInfo;
        s, s2, ATag, HintAttr: String;
        cpno, CurFont, OpenedPara: Integer;
        CreateBulletList, Use0StyleAsDef, DIVOpened: Boolean;
        RVStyle: TRVStyle;
        {$IFNDEF RVDONOTUSELISTS}
        marker: TRVMarkerItemInfo;
        {$ENDIF}
    begin
      if CPPrefix='' then
        CPPrefix := RVDEFAULTCHECKPOINTPREFIX;
      RVStyle := GetRVStyle;
      cpno    := 0;
      CurFont := -1;
      OpenedPara := -1;
      DIVOpened := False;
      {$IFNDEF RVDONOTUSELISTS}
      marker := nil;
      {$ENDIF}
      CreateBulletList := Bullets=nil;
      if CreateBulletList then
        Bullets := TRVList.Create;
      try
        Use0StyleAsDef := (RVStyle.TextStyles.Count>=1) and
          not RVStyle.TextStyles[0].Jump and
          (RVStyle.TextStyles[0].BackColor=clNone) and
          not (rvsoNoDefCSSStyle in Options);
        for i:=0 to Items.Count-1 do begin
          item := GetItem(i);
          if (not item.SameAsPrev) then begin
            if ((OpenedPara<0) {$IFNDEF RVDONOTUSELISTS}and (marker=nil){$ENDIF}) or
               item.BR then
              RVWriteLn(Stream,'');
            if item.BR then
              RVWrite(Stream, '<BR>')
            else begin
              {$IFNDEF RVDONOTUSELISTS}
              if marker<>nil then begin
                RVWriteLn(Stream,'</LI>');
                RVWrite(Stream, GetParaHTMLCode(Self, i, False, True));
                end
              else if (OpenedPara>=0) then
              {$ENDIF}
              begin
                if DIVOpened then
                  RVWriteLn(Stream,'</DIV>')
                else
                  RVWriteLn(Stream,'</P>');
                DIVOpened := False;
                RVWrite(Stream, GetParaHTMLCode(Self, i, False, True));
              end;
              CurrentFileColor := RVStyle.ParaStyles[item.ParaNo].Background.Color;
              if CurrentFileColor=clNone then
                CurrentFileColor := Color;
              case item.StyleNo of
                rvsBreak:
                  OpenedPara := -1;
                {$IFNDEF RVDONOTUSELISTS}
                rvsListMarker:
                  begin
                    if TRVMarkerItemInfo(item).GetLevelInfo(RVStyle)<>nil then begin
                      if rvsoMarkersAsText in Options then begin
                        RVWrite(Stream, GetParaHTMLCode(Self, i, True, True));
                        if ((item.ParaNo=0) and not (rvsoNoDefCSSStyle in Options)) or
                           (rvsoInlineCSS in Options) then
                          RVWrite(Stream, Format('<P style="%s %s">',
                            [GetParaCSSValue(item, RVStyle, True),
                             TRVMarkerItemInfo(item).GetLevelInfo(RVStyle).GetIndentCSSForTextVersion]))
                        else
                          RVWrite(Stream, Format('<P %s style="%s">',
                            [GetParaCSS(item, RVStyle, True),
                             TRVMarkerItemInfo(item).GetLevelInfo(RVStyle).GetIndentCSSForTextVersion]));
                        OpenedPara := item.ParaNo;
                        end
                      else begin
                        TRVMarkerItemInfo(item).SaveHTMLSpecial(Stream, marker, RVStyle, True);
                        RVWrite(Stream, GetParaHTMLCode(Self, i, True, True));
                        marker := TRVMarkerItemInfo(item);
                        if marker.GetLevelInfo(RVStyle).HasNumbering then
                          RVWrite(Stream,Format('<LI value=%d',[marker.Counter]))
                        else
                          RVWrite(Stream,'<LI');
                        s := GetParaCSS(item, RVStyle, True);
                        if s<>'' then
                          RVWrite(Stream, ' '+s);
                        RVWrite(Stream, marker.GetLICSS(Self, i, Path,
                          ImagesPrefix, imgSaveNo, CurrentFileColor, Options, Bullets));
                        RVWrite(Stream,'>');
                        OpenedPara := -1;
                      end
                      end
                    else begin
                      {$IFNDEF RVDONOTUSELISTS}
                      if marker<>nil then begin
                        marker.HTMLOpenOrCloseTags(Stream, marker.Level, -1, RVStyle, True);
                        marker := nil;
                      end;
                      {$ENDIF}
                      RVWrite(Stream, GetParaHTMLCode(Self, i, True, True));
                      s := GetParaCSS(item, RVStyle, False);
                      if item.GetBoolValue(rvbpNoHTML_P) then
                      if s='' then
                        RVWrite(Stream, '<P>')
                      else
                        RVWrite(Stream, Format('<P %s>',[s]));
                      OpenedPara := item.ParaNo;
                    end;
                  end;
                {$ENDIF}
                else
                  begin
                    {$IFNDEF RVDONOTUSELISTS}
                    if marker<>nil then begin
                      marker.HTMLOpenOrCloseTags(Stream, marker.Level, -1, RVStyle, True);
                      marker := nil;
                    end;
                    {$ENDIF}
                    RVWrite(Stream, GetParaHTMLCode(Self, i, True, True));
                    s := GetParaCSS(item, RVStyle, False);
                    if item.GetBoolValue(rvbpNoHTML_P) then begin
                      DIVOpened := True;
                      if s='' then
                        RVWrite(Stream, '<DIV>')
                      else
                        RVWrite(Stream, Format('<DIV %s>',[s]))
                      end
                    else
                      if s='' then
                        RVWrite(Stream, '<P>')
                      else
                        RVWrite(Stream, Format('<P %s>',[s]));
                    OpenedPara := item.ParaNo;
                  end;
              end
            end;
          end;
          SaveHTMLCheckpoint(Stream, item.Checkpoint, cpno, CPPrefix, False, Options);
          if item.StyleNo<0 then begin
            ATag := '';
            if item.GetBoolValueEx(rvbpJump, RVStyle) then begin
              ATag := GetHTMLATag(i, '');
              if ATag<>'' then
                RVWrite(Stream, ATag);
            end;
            s2 := '';
            if SaveItemToFile(Path, Self, i, rvsfHTML, False, s2) then
              RVWrite(Stream, s2)
            else
              item.SaveToHTML(Stream, Self, i, Items[i], Path, ImagesPrefix,
                imgSaveNo, CurrentFileColor, Options, True, Bullets);
            if item.GetBoolValueEx(rvbpJump, RVStyle) then begin
              if ATag<>'' then
                RVWrite(Stream, '</A>');
            end;
            end
          else if ShouldSaveTextToHTML(GetActualStyle(item)) then begin
            ATag := '';
            if item.GetBoolValueEx(rvbpJump, RVStyle) then
              ATag := GetHTMLATag(i, GetTextCSS(item, RVStyle));
            if ATag<>'' then
              RVWrite(Stream, ATag)
            else begin
              if CurFont<>GetActualStyle(item) then begin
                if (GetActualStyle(item)=0) and Use0StyleAsDef and
                   not (rvsoInlineCSS in Options) then
                  CurFont := -1
                else begin
                  CurFont := GetActualStyle(item);
                  RVWrite(Stream, SysUtils.Format('<SPAN %s>',
                    [GetTextCSS(item, RVStyle)]));
                end;
              end;
            end;
            HintAttr := '';
            if ATag='' then begin
              item.GetExtraStrProperty(rvespHint, HintAttr);
              HintAttr := RV_GetHintStr(rvsfHTML, HintAttr);
              if HintAttr<>'' then
                RVWrite(Stream, SysUtils.Format('<SPAN %s>', [HintAttr]));
            end;
            s2 := GetTextForHTML(Path, i);
            RVWrite(Stream, s2);
            if HintAttr<>'' then
              RVWrite(Stream,'</SPAN>');            
            if ATag<>'' then
              RVWrite(Stream,'</A>')
            else
              if (CurFont<>-1) and
                 ((i=Items.Count-1) or (GetItemStyle(i+1)<>GetActualStyle(item)) or
                 (not GetItem(i+1).SameAsPrev)) then begin
                RVWrite(Stream, '</SPAN>');
                CurFont := -1;
              end;
          end;
        end;
        {$IFNDEF RVDONOTUSELISTS}
        if marker<>nil then begin
          RVWriteLn(Stream,'</LI>');
          RVWrite(Stream, GetParaHTMLCode(Self, ItemCount, False, True));
          marker.HTMLOpenOrCloseTags(Stream, marker.Level, -1, RVStyle, True);
        end;
        {$ENDIF}
        if (OpenedPara<>-1) then begin
          if DIVOpened then
            RVWriteLn(Stream,'</DIV>')
          else
            RVWriteLn(Stream,'</P>');
          RVWrite(Stream, GetParaHTMLCode(Self, ItemCount, False, True));
        end;
        SaveHTMLCheckpoint(Stream, NotAddedCP, cpno, CPPrefix, False, Options);
      finally
        if CreateBulletList then begin
          Bullets.Free;
        end;
      end;
    end;
    {......................................................}
begin
  Result := False;
  if GetRVStyle = nil then exit;
  Result := True;
  CurrentFileColor := Color;
  try
    if not (rvsoMiddleOnly in Options) and
       not (rvsoLastOnly in Options) then
       SaveFirst(Stream, Path, Title, ExtraStyles, ExternalCSS);
    if not (rvsoFirstOnly in Options) and
       not (rvsoLastOnly in Options) then
       SaveMiddle(Stream, Path, CPPrefix);
    if not (rvsoFirstOnly in Options) and
       not (rvsoMiddleOnly in Options) then
       SaveLast(Stream);
  except
    Result := False;
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TCustomRVData.CheckItemClass(ItemNo: Integer; RequiredClass: TCustomRVItemInfoClass);
begin
  if not (Items.Objects[ItemNo] is RequiredClass) then
    raise ERichViewError.Create(errRVTypesMismatch);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.FindControlItemNo(actrl: TControl): Integer;
var i: Integer;
begin
  for i := 0 to Items.Count-1 do
    if TCustomRVItemInfo(Items.Objects[i]).OwnsControl(actrl) then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.RemoveCheckpoint(ItemNo: Integer): Boolean;
var OldCP: TRVCPInfo;
begin
  with TCustomRVItemInfo(Items.Objects[ItemNo]) do begin
    OldCP := Checkpoint;
    FreeCheckpoint(Checkpoint, True, True);
  end;
  Result := OldCP<>nil;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SetBreakInfo(ItemNo: Integer; AWidth: Byte;
  AStyle: TRVBreakStyle; AColor: TColor; ATag: Integer);
begin
  CheckItemClass(ItemNo, TRVBreakItemInfo);
  with TRVBreakItemInfo(Items.Objects[ItemNo]) do begin
    Color     := AColor;
    LineWidth := AWidth;
    Style     := AStyle;
  end;
  SetItemTag(ItemNo, ATag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SetBulletInfo(ItemNo: Integer; const AName: String;
  AImageIndex: Integer; AImageList: TCustomImageList; ATag: Integer);
begin
  CheckItemClass(ItemNo, TRVBulletItemInfo);
  with TRVBulletItemInfo(Items.Objects[ItemNo]) do
    ImageIndex := AImageIndex;
  SetItemTag(ItemNo, ATag);
  Items[ItemNo] := AName;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SetControlInfo(ItemNo: Integer; const AName: String;
  AVAlign: TRVVAlign; ATag: Integer): Boolean;
begin
  CheckItemClass(ItemNo, TRVControlItemInfo);
  with TRVControlItemInfo(Items.Objects[ItemNo])  do begin
    Result := (VAlign<>AVAlign);
    VAlign := AVAlign;
  end;
  SetItemTag(ItemNo, ATag);
  Items[ItemNo] := AName;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SetHotspotInfo(ItemNo: Integer;
                             const AName: String; AImageIndex, AHotImageIndex: Integer;
                             AImageList: TCustomImageList; ATag: Integer);
begin
  CheckItemClass(ItemNo, TRVHotspotItemInfo);
  with TRVHotspotItemInfo(Items.Objects[ItemNo]) do begin
    ImageIndex    := AImageIndex;
    HotImageIndex := AHotImageIndex;
  end;
  SetItemTag(ItemNo, ATag);
  Items[ItemNo] := AName;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SetCheckpointInfo(ItemNo, ATag: Integer;
                             const AName: String; ARaiseEvent: Boolean);
begin
  with TCustomRVItemInfo(Items.Objects[ItemNo]) do begin
    if Checkpoint=nil then
      InsertCheckpoint(ItemNo, ATag, AName, ARaiseEvent)
    else begin
      if rvoTagsArePChars in Options then
        StrDispose(PChar(Checkpoint.Tag));
      Checkpoint.Tag        := ATag;
      Checkpoint.Name       := AName;
      Checkpoint.RaiseEvent := ARaiseEvent;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SetItemTag(ItemNo, ATag: Integer);
begin
  with GetItem(ItemNo) do begin
    if Tag=ATag then exit;
    if rvoTagsArePChars in Options then
      StrDispose(PChar(Tag));
    Tag := ATag;
  end;
end;
{------------------------------------------------------------------------------}
{ Sets item's property of integer type.
  ItemNo - index of item. Prop identifies the property. Value - new property
  value.
  Returns true is this item type has this property                             }
function TCustomRVData.SetItemExtraIntProperty(ItemNo: Integer;
  Prop: TRVExtraItemProperty; Value: Integer): Boolean;
begin
  Result := GetItem(ItemNo).SetExtraIntProperty(Prop, Value);
end;
{------------------------------------------------------------------------------}
{ Gets item's property of integer type.
  ItemNo - index of item. Prop identifies the property. Value receives a
  property value.
  Returns true is this item type has this property                             }
function TCustomRVData.GetItemExtraIntProperty(ItemNo: Integer;
  Prop: TRVExtraItemProperty; var Value: Integer): Boolean;
begin
  Result := GetItem(ItemNo).GetExtraIntProperty(Prop, Value);
end;
{------------------------------------------------------------------------------}
{ Sets item's property of string type.
  ItemNo - index of item. Prop identifies the property. Value - new property
  value.
  Returns true is this item type has this property                             }
function TCustomRVData.SetItemExtraStrProperty(ItemNo: Integer;
  Prop: TRVExtraItemStrProperty; const Value: String): Boolean;
begin
  Result := GetItem(ItemNo).SetExtraStrProperty(Prop, Value);
end;
{------------------------------------------------------------------------------}
{ Gets item's property of string type.
  ItemNo - index of item. Prop identifies the property. Value receives a
  property value.
  Returns true is this item type has this property                             }
function TCustomRVData.GetItemExtraStrProperty(ItemNo: Integer;
  Prop: TRVExtraItemStrProperty; var Value: String): Boolean;
begin
  Result := GetItem(ItemNo).GetExtraStrProperty(Prop, Value);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SetPictureInfo(ItemNo: Integer; const AName: String;
  Agr: TGraphic; AVAlign: TRVVAlign; ATag: Integer): Boolean;
begin
  CheckItemClass(ItemNo, TRVGraphicItemInfo);
  with TRVGraphicItemInfo(Items.Objects[ItemNo]) do begin
    Result := (Agr.Width<>Image.Width) or
              (Agr.Height<>Image.Height) or
              (VAlign<>AVAlign);
    if Agr<>Image then begin
      Image.Free;
      Image := Agr;
    end;
    VAlign := AVAlign;
  end;
  SetItemTag(ItemNo, ATag);
  Items[ItemNo] := AName;
  GetItem(ItemNo).UpdatePaletteInfo(GetDoInPaletteMode, True, GetRVPalette,
    GetRVLogPalette);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetItemStyle(ItemNo: Integer): Integer;
begin
  Result := GetActualStyle(GetItem(ItemNo));
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetActualStyle(Item: TCustomRVItemInfo): Integer;
begin
  Result := Item.StyleNo;
  if Result=rvsDefStyle then begin
    if GetRVStyle.ParaStyles[Item.ParaNo].DefStyleNo>=0 then
      Result := GetRVStyle.ParaStyles[Item.ParaNo].DefStyleNo
    else
      Result := 0;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetActualStyle2(StyleNo, ParaNo: Integer): Integer;
begin
  Result := StyleNo;
  if Result=rvsDefStyle then begin
    if GetRVStyle.ParaStyles[ParaNo].DefStyleNo>=0 then
      Result := GetRVStyle.ParaStyles[ParaNo].DefStyleNo
    else
      Result := 0;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.GetBreakInfo(ItemNo: Integer; var AWidth: Byte;
  var AStyle: TRVBreakStyle; var AColor: TColor; var ATag: Integer);
begin
  CheckItemClass(ItemNo, TRVBreakItemInfo);
  with TRVBreakItemInfo(Items.Objects[ItemNo]) do begin
    AWidth := LineWidth;
    AStyle := Style;
    AColor := Color;
    ATag := Tag;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.GetBulletInfo(ItemNo: Integer; var AName: String;
  var AImageIndex: Integer; var AImageList: TCustomImageList; var ATag: Integer);
begin
  CheckItemClass(ItemNo, TRVBulletItemInfo);
  with Items.Objects[ItemNo] as TRVBulletItemInfo do begin
    AImageIndex := ImageIndex;
    AImageList := ImageList;
    ATag := Tag;
  end;
  AName := Items[ItemNo];
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.GetControlInfo(ItemNo: Integer; var AName: String;
  var Actrl: TControl; var AVAlign: TRVVAlign; var ATag: Integer);
begin
  CheckItemClass(ItemNo, TRVControlItemInfo);
  with TRVControlItemInfo(Items.Objects[ItemNo]) do begin
    Actrl := Control;
    ATag := Tag;
    AVAlign := VAlign;
  end;
  AName := Items[ItemNo];
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.GetHotspotInfo(ItemNo: Integer; var AName: String;
  var AImageIndex, AHotImageIndex: Integer; var AImageList: TCustomImageList;
  var ATag: Integer);
begin
  CheckItemClass(ItemNo, TRVHotspotItemInfo);
  with TRVHotspotItemInfo(Items.Objects[ItemNo]) do begin
    AImageIndex := ImageIndex;
    AHotImageIndex := HotImageIndex;
    AImageList := ImageList;
    ATag := Tag;
  end;
  AName := Items[ItemNo];
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetItemTag(ItemNo: Integer): Integer;
begin
  Result := GetItem(ItemNo).Tag;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.GetPictureInfo(ItemNo: Integer; var AName: String;
  var Agr: TGraphic; var AVAlign: TRVVAlign; var ATag: Integer);
begin
  CheckItemClass(ItemNo, TRVGraphicItemInfo);
  with Items.Objects[ItemNo] as TRVGraphicItemInfo do begin
    Agr := Image;
    ATag := Tag;
    AVAlign := VAlign;
  end;
  AName := Items[ItemNo];
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.GetTextInfo(ItemNo: Integer; var AText: String;
  var ATag: Integer);
begin
  if (GetItemStyle(ItemNo)<0) then
    raise ERichViewError.Create(errRVTypesMismatch);
  ATag  := GetItem(ItemNo).Tag;
  AText := Items[ItemNo];
end;
{------------------------------------------------------------------------------}
function TCustomRVData.IsFromNewLine(ItemNo: Integer): Boolean;
begin
  Result := not GetItem(ItemNo).SameAsPrev;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetItemPara(ItemNo: Integer): Integer;
begin
  Result := GetItem(ItemNo).ParaNo;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.IsParaStart(ItemNo: Integer): Boolean;
begin
  Result := not GetItem(ItemNo).SameAsPrev and
            not GetItem(ItemNo).BR;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetPageBreaksBeforeItems(Index: Integer): Boolean;
begin
  if (Index<0) or (Index>=Items.Count) then
    raise ERichViewError.Create(errRVItemRangeError);
  {$IFNDEF RVDONOTUSELISTS}
  if (Index>0) and (GetItemStyle(Index-1)=rvsListMarker) then
    dec(Index);
  {$ENDIF}
  Result := GetItem(Index).PageBreakBefore;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SetPageBreaksBeforeItems(Index: Integer;
  Value: Boolean);
begin
  if (Index<0) or (Index>=Items.Count) then
    raise ERichViewError.Create(errRVItemRangeError);
  TCustomRVItemInfo(Items.Objects[Index]).PageBreakBefore := Value;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.FindCheckpointByName(
  const Name: String): TCheckpointData;
var cp: TRVCPInfo;
begin
  Result := nil;
  cp := FirstCP;
  while cp<>nil do begin
    if cp.Name=Name then begin
      Result := cp;
      exit;
    end;
    cp := cp.Next;
  end;
  if (NotAddedCP<>nil) and (NotAddedCP.Name=Name) then
    Result := NotAddedCP;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.FindCheckpointByTag(Tag: Integer): TCheckpointData;
var cp: TRVCPInfo;
begin
  Result := nil;
  cp := FirstCP;
  while cp<>nil do begin
    if RV_CompareTags(cp.Tag,Tag, rvoTagsArePChars in Options) then begin
      Result := cp;
      exit;
    end;
    cp := cp.Next;
  end;
  if (NotAddedCP<>nil) and RV_CompareTags(NotAddedCP.Tag,Tag,rvoTagsArePChars in Options) then
    Result := NotAddedCP;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetCheckpointByNo(No: Integer): TCheckpointData;
var i: Integer;
    cp: TRVCPInfo;
begin
  if (no<0) or (no>=CPCount) then begin
    raise ERichViewError.Create(SysUtils.Format(errRVNoSuchCP,[no]));
    exit;
  end;
  if (no=CPCount-1) and (NotAddedCP<>nil) then
    Result := NotAddedCP
  else begin
    cp := FirstCP;
    for i := 1 to no do begin
      if cp = nil then break;
      cp := cp.Next;
    end;
    //Assert(cp<>nil, 'Can''t find checkpoint');
    Result := cp;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetFirstCheckpoint: TCheckpointData;
begin
  Result := FirstCP;
  if Result = nil then
    Result := NotAddedCP;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetLastCheckpoint: TCheckpointData;
begin
  Result := NotAddedCP;
  if Result = nil then
    Result := LastCP;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetNextCheckpoint(CheckpointData: TCheckpointData): TCheckpointData;
begin
  Result := nil;
  if CheckpointData=nil then
    raise ERichViewError.Create(errRVNil);
  if CheckpointData=NotAddedCP then exit;
  Result := TRVCPInfo(CheckpointData).Next;
  if Result = nil then
    Result := NotAddedCP;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetPrevCheckpoint(CheckpointData: TCheckpointData): TCheckpointData;
begin
  if CheckpointData=nil then
    raise ERichViewError.Create(errRVNil);
  if CheckpointData=NotAddedCP then begin
    Result := LastCP;
    exit;
  end;
  Result := TRVCPInfo(CheckpointData).Prev;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetItemCheckpoint(ItemNo: Integer): TCheckpointData;
begin
  Result := TCustomRVItemInfo(Items.Objects[ItemNo]).Checkpoint;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetCheckpointItemNo(CheckpointData: TCheckpointData): Integer;
begin
  if CheckpointData = nil then
    raise ERichViewError.Create(errRVNil);

  if CheckpointData=NotAddedCP then
    Result := -1
  else begin
    Result := Items.IndexOfObject(TRVCPInfo(CheckpointData).ItemInfo);
    if Result=-1 then
      raise ERichViewError.Create(errRVNoSuchCP2);
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetCheckpointNo(CheckpointData: TCheckpointData): Integer;
var cp: TRVCPInfo;
begin
  if CheckpointData = nil then
    raise ERichViewError.Create(errRVNil);

  cp := FirstCP;
  Result := 0;

  while cp<>nil do begin
    if cp=CheckpointData then exit;
    cp := cp.Next;
    inc(Result);
  end;

  if CheckpointData=NotAddedCP then exit;

  if CheckpointData = nil then
    raise ERichViewError.Create(errRVNoSuchCP2);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.GetCheckpointInfo(CheckpointData: TCheckpointData;
  var Tag: Integer; var Name: String; var RaiseEvent: Boolean);
begin
  if CheckpointData = nil then
    raise ERichViewError.Create(errRVNil);
  Name       := TRVCPInfo(CheckpointData).Name;
  Tag        := TRVCPInfo(CheckpointData).Tag;
  RaiseEvent := TRVCPInfo(CheckpointData).RaiseEvent;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.NormalizeParas(StartItemNo: Integer);
var i,ParaNo: Integer;
begin
  if Items.Count=0 then exit;
  i := StartItemNo;
  if i>=Items.Count then
    i := Items.Count-1;
  while (i>0) and not TCustomRVItemInfo(Items.Objects[i]).CanBeBorderStart do
    dec(i);
  ParaNo := TCustomRVItemInfo(Items.Objects[i]).ParaNo;
  inc(i);
  while (i<Items.Count) and not TCustomRVItemInfo(Items.Objects[i]).CanBeBorderStart do begin
    TCustomRVItemInfo(Items.Objects[i]).ParaNo := ParaNo;
    inc(i);
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.FindCPBeforeItem(ItemNo: Integer): TRVCPInfo;
begin
  UpdateCPItemNo;
  if (FirstCP=nil) or
     (FirstCP.ItemNo>=ItemNo) then begin
    Result := nil; // no CP before
    exit;
  end;
  Result := FirstCP;
  while Result.Next<>nil do begin
    if Result.Next.ItemNo>=ItemNo then exit;
    Result := Result.Next;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.RVFGetLimits(SaveScope: TRVFSaveScope; var StartItem,
  EndItem, StartOffs, EndOffs: Integer; var StartPart, EndPart: TRVMultiDrawItemPart);
begin
  StartItem := 0;
  EndItem   := Items.Count-1;
  if StartItem<Items.Count then begin
    StartOffs := GetOffsBeforeItem(StartItem);
    if EndItem>=0 then
      EndOffs   := GetOffsAfterItem(EndItem);
  end;
  StartPart := nil;
  EndPart   := nil;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetRVFOptions: TRVFOptions;
begin
  Result := GetRootData.GetRVFOptions;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetExtraRTFCode(Area: TRVRTFSaveArea; Obj: TObject; Index1, Index2: Integer; InStyleSheet: Boolean): String;
begin
  Result := GetAbsoluteRootData.GetExtraRTFCode(Area, Obj, Index1, Index2, InStyleSheet);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetExtraHTMLCode(Area: TRVHTMLSaveArea; CSSVersion: Boolean): String;
begin
  Result := GetAbsoluteRootData.GetExtraHTMLCode(Area, CSSVersion);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.IsAssignedOnProgress: Boolean;
begin
 Result := GetAbsoluteRootData.IsAssignedOnProgress;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.DoProgress(Operation: TRVLongOperation;
  Stage: TRVProgressStage; PercentDone: Byte);
begin
  GetAbsoluteRootData.DoProgress(Operation, Stage, PercentDone);
end;  
{------------------------------------------------------------------------------}
function TCustomRVData.GetParaHTMLCode(RVData: TCustomRVData; ItemNo: Integer;
  ParaStart, CSSVersion: Boolean): String;
begin
  Result := GetAbsoluteRootData.GetParaHTMLCode(RVData, ItemNo, ParaStart,
    CSSVersion);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SetRVFOptions(const Value: TRVFOptions);
begin
  GetRootData.SetRVFOptions(Value);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetRVFWarnings: TRVFWarnings;
begin
  Result := GetRootData.GetRVFWarnings;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SetRVFWarnings(const Value: TRVFWarnings);
begin
  GetRootData.SetRVFWarnings(Value);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetRVFSaveScope(SelectionOnly: Boolean):TRVFSaveScope;
begin
  if SelectionOnly then
    Result := rvfss_Selection
  else
    Result := rvfss_Full;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERVF}
{------------------------------------------------------------------------------}
type
  TRVFHeader = record
    StyleNo,ParaNo, ReadType, ExtraValue: Integer;
    DataCount, DataRead: Integer;
    Item: TCustomRVItemInfo;
    ClassName: String;
    Name: String;
    Version, SubVersion: Integer;
    RaiseEvent: Integer;
    PersistentCheckpoint: Integer;
    CheckPointTag: Integer;
  end;
  {------------------------------------------------------------------------------}
procedure TCustomRVData.DataReader(Stream: TStream);
var Size: Integer;
    MemStream: TMemoryStream;
    Color: TColor;
    Back: TRVBackground;
begin
  MemStream := TMemoryStream.Create;
  Include(State, rvstLoadingAsPartOfItem);
  try
    Stream.ReadBuffer(Size, SizeOf(Size));
    MemStream.SetSize(Size);
    Stream.ReadBuffer(MemStream.Memory^, Size);
    Back := nil;
    LoadRVFFromStream(MemStream, Color, Back, nil);
  finally
    MemStream.Free;
    Exclude(State, rvstLoadingAsPartOfItem);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.DataWriter(Stream: TStream);
var StartPos,Size: Integer;
    sb: Boolean;
begin
  Size := 0;
  StartPos := Stream.Position;
  Stream.WriteBuffer(Size, SizeOf(Size));
  sb := rvfoSaveBack in RVFOptions;
  RVFOptions := RVFOptions - [rvfoSaveBack];
  GetRVData.SaveRVFToStream(Stream, False, clNone, nil, nil);
  if sb then
    RVFOptions := RVFOptions + [rvfoSaveBack];
  Size := Stream.Position-SizeOf(Size)-StartPos;
  Stream.Position := StartPos;
  Stream.WriteBuffer(Size, SizeOf(Size));
  Stream.Position := StartPos+SizeOf(Size)+Size;
end;
{------------------------------------------------------------------------------}
function InsertRVFHeaderData(RVData: TCustomRVData; const Caption: String;
  var Header: TRVFHeader; var PrevCP, CurCP: TRVCPInfo;
  var Index, InsertPoint: Integer; var FirstTime: Boolean; AParaNo: Integer;
  AppendMode, EditFlag: Boolean; var NonFirstItemsAdded: Integer;
  var FullReformat: Boolean; ListStylesMapping: TRVIntegerList): Boolean;
var item: TCustomRVItemInfo;
    CP: TRVCPInfo;
    Caption2: String;
    NewListNo: Integer;
    {$IFNDEF RVDONOTUSELISTS}
    OldListNo: Integer;
    {$ENDIF}
begin
  Result := True;
  {$IFNDEF RVDONOTUSELISTS}
  OldListNo := -1;
  {$ENDIF}
  if (Header.StyleNo=rvsBack) or
     (Header.StyleNo=rvsVersionInfo) then exit;
  if Header.Item<>nil then begin
    if Header.StyleNo>=0 then begin
      if EditFlag and
         (rvprRVFInsertProtect in RVData.GetRVStyle.TextStyles[RVData.GetActualStyle(Header.Item)].Protection) then
        exit;
      item := RichViewTextItemClass.Create(RVData);
      item.BeforeLoading(rvlfRVF);
      item.Assign(Header.Item);
      item.Tag := RV_CopyTag(Header.Item.Tag, rvoTagsArePChars in RVData.Options);
      end
    else begin
      item := Header.Item;
      Header.Item := nil;
      if not item.GetBoolValue(rvbpValid) then begin
        item.Free;
        exit;
      end;
    end;
    if (Header.ParaNo=-1) and (RVData.Items.Count<>0) and (InsertPoint>0) and
       not RVData.GetItem(InsertPoint-1).GetBoolValue(rvbpFullWidth) then begin
      item.SameAsPrev := True;
      item.ParaNo := RVData.GetItem(InsertPoint-1).ParaNo;
      end
    else begin
      item.SameAsPrev := False;
      if (Header.ParaNo<>-1) then
        item.ParaNo := Header.ParaNo
      else
        item.ParaNo := 0;
    end;
    item.UpdatePaletteInfo(RVData.GetDoInPaletteMode, False,
      RVData.GetRVPalette, RVData.GetRVLogPalette);
    if CurCP<> nil then begin
      if CurCP=RVData.NotAddedCP then begin
        dec(RVData.CPCount);
        RVData.NotAddedCP := nil;
      end;
      CP := CurCP;
      if not EditFlag then begin
        inc(RVData.CPCount);
        RVData.SetCP(item, PrevCP, CurCP)
        end
      else begin
        item.Checkpoint := CurCP;
        CurCP.ItemInfo := item;
        CurCP := nil;
      end;
      PrevCP := CP;
    end;
    Caption2 := Caption;
    {$IFNDEF RVDONOTUSEUNICODE}
    if (rvioUnicode in item.ItemOptions) and (item.StyleNo>=0) and
       (Header.ReadType=3) then
      Caption2 := RVDecodeString(Caption);    
    if (rvioUnicode in item.ItemOptions) and
       (item.StyleNo>=0) and
       (RVData.GetRVStyle<>nil) and
       not RVData.GetRVStyle.TextStyles[RVData.GetActualStyle(item)].Unicode then begin
      Caption2 := RVU_UnicodeToAnsi(RVData.GetStyleCodePage(RVData.GetActualStyle(item)), Caption);
      Exclude(item.ItemOptions, rvioUnicode);
      RVData.RVFWarnings := RVData.RVFWarnings + [rvfwConvFromUnicode];
    end;
    if not (rvioUnicode in item.ItemOptions) and
       (item.StyleNo>=0) and
       (RVData.GetRVStyle<>nil) and
       RVData.GetRVStyle.TextStyles[RVData.GetActualStyle(item)].Unicode then begin
      Caption2 := RVU_AnsiToUnicode(RVData.GetStyleCodePage(RVData.GetActualStyle(item)), Caption);
      Include(item.ItemOptions, rvioUnicode);
      RVData.RVFWarnings := RVData.RVFWarnings + [rvfwConvToUnicode];
    end;
    {$ENDIF}
    {$IFNDEF RVDONOTUSELISTS}
    if item.StyleNo=rvsListMarker then begin
      if (ListStylesMapping<>nil) and (TRVMarkerItemInfo(item).ListNo>=0) then begin
        if TRVMarkerItemInfo(item).ListNo>=ListStylesMapping.Count then begin
          RVData.RVFWarnings := RVData.RVFWarnings + [rvfwConvUnknownStyles];
          TRVMarkerItemInfo(item).ListNo := 0;
          end
        else begin
          OldListNo := TRVMarkerItemInfo(item).ListNo;
          TRVMarkerItemInfo(item).ListNo := ListStylesMapping[OldListNo];
        end;
      end;
      if TRVMarkerItemInfo(item).ListNo>=RVData.GetRVStyle.ListStyles.Count then begin
        RVData.RVFWarnings := RVData.RVFWarnings + [rvfwConvUnknownStyles];
        TRVMarkerItemInfo(item).ListNo := 0;
        if TRVMarkerItemInfo(item).ListNo>=RVData.GetRVStyle.ListStyles.Count then
          TRVMarkerItemInfo(item).ListNo := -1;
      end;
    end;
    {$ENDIF}
    if FirstTime then begin
      if AppendMode then begin
        if AParaNo=-1 then begin
          item.SameAsPrev := (InsertPoint>0) and
            not RVData.GetItem(InsertPoint-1).GetBoolValue(rvbpFullWidth);
          if item.SameAsPrev then
            item.ParaNo := RVData.GetItem(InsertPoint-1).ParaNo
          else
            item.ParaNo := 0;
          end
        else begin
          item.SameAsPrev := False;
          item.ParaNo := AParaNo;
        end;
      end;
      if not RVData.InsertFirstRVFItem(InsertPoint, Caption2, item, EditFlag,
        FullReformat, NewListNo) then begin
        Result := False;
        exit;
      end;
      if item<>nil then begin
        inc(InsertPoint);
        Index := InsertPoint-1;
        FirstTime := False;
      end;
      {$IFNDEF RVDONOTUSELISTS}
      if (OldListNo>=0) and (NewListNo>=0) then
        ListStylesMapping[OldListNo] := NewListNo;
      {$ENDIF}
      end
    else begin
      item.Inserting(RVData, Caption2, False);
      RVData.Items.InsertObject(InsertPoint, Caption2, item);
      item.Inserted(RVData, InsertPoint);
      {$IFNDEF RVDONOTUSELISTS}
      RVData.AddMarkerInList(InsertPoint);
      {$ENDIF}
      inc(InsertPoint);
      inc(NonFirstItemsAdded);
    end;
    if item<>nil then begin
      RVData.ControlAction(rvcaAfterRVFLoad, InsertPoint-1, item);
      if not (rvstLoadingAsPartOfItem in RVData.State) then
        item.AfterLoading(rvlfRVF);
      end
    else
      RVData.FreeCheckpoint(CurCP, False, False);
    end
  else begin
    // unknown item type
    if CurCP<> nil then begin
       if not EditFlag then begin
         inc(RVData.CPCount);
         item := RichViewTextItemClass.Create(RVData);
         RVData.SetCP(item, PrevCP, CurCP);
         RVData.InternalFreeItem(item,False);
         end
       else begin
         RVData.FreeCheckpoint(CurCP, False, False);
       end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function RVFReadHeader(RVData: TCustomRVData; const CurrentLine: String;
  var Header: TRVFHeader; AParaNo: Integer; var Color: TColor;
  Background: TRVBackground;
  TextStylesMapping, ParaStylesMapping: TRVIntegerList): Boolean;
var P: PChar;
    ItemOptions: Integer;
    ABackgroundStyle, AColor, Tag: Integer;
begin
  P := PChar(CurrentLine);
  Result := False;
  Header.DataRead := 0;
  if not RVFReadTextStyle(RVData.GetRVStyle,P,Header.StyleNo) then
    exit; {error}
  if Header.StyleNo = rvsVersionInfo then begin
    Header.DataCount := 0;
    Result := (RVFReadInteger(P,Header.Version) and
               RVFReadInteger(P,Header.SubVersion));
    exit;
  end;
  if not (RVFReadInteger(P,Header.DataCount) and
          RVFReadParaStyle(RVData.GetRVStyle,P,Header.ParaNo)) then
    exit; {error}
  if (Header.StyleNo<>rvsBack) and (Header.StyleNo<>rvsCheckpoint) and
     (Header.StyleNo<>rvsDocProperty) then
    Header.Item := CreateRichViewItem(Header.StyleNo, RVData);
  if Header.Item<>nil then
    Header.Item.BeforeLoading(rvlfRVF);
  if (Header.Version>=1)and(Header.SubVersion>=2) then begin
    if not RVFReadInteger(P,ItemOptions) then
      exit; {error}
    if Header.Item<>nil then
      Header.Item.ItemOptions := TRVItemOptions(Byte(ItemOptions));
  end;
  if not (RVFReadInteger(P,Header.ReadType) and
     RVFReadTag(P, rvoTagsArePChars in RVData.Options,
     (Header.Version>1) or (Header.SubVersion>2), Tag)) then
    exit; {error}
  if Header.StyleNo = rvsDocProperty then begin
    if not RVFReadInteger(P,Header.ExtraValue) then
      exit; {error}
  end;
  if Header.StyleNo=rvsCheckpoint then
    Header.CheckpointTag := Tag
  else if Header.Item<>nil then
    Header.Item.Tag := Tag;
  if (Header.Item<>nil) and (Header.StyleNo>=0) then begin
    if TextStylesMapping<>nil then begin
      if (Header.StyleNo<>rvsDefStyle) and
         (Header.StyleNo>=TextStylesMapping.Count) then begin
        RVData.RVFWarnings := RVData.RVFWarnings + [rvfwConvUnknownStyles];
        exit;
      end;
      if Header.StyleNo<>rvsDefStyle then
        Header.StyleNo := TextStylesMapping[Header.StyleNo];
      Header.Item.StyleNo := Header.StyleNo;
    end;
    if (Header.StyleNo<>rvsDefStyle) and
       (Header.StyleNo>=RVData.GetRVStyle.TextStyles.Count) then begin
      RVData.RVFWarnings := RVData.RVFWarnings + [rvfwConvUnknownStyles];
      if rvfoConvUnknownStylesToZero in RVData.RVFOptions then
        Header.Item.StyleNo := 0
      else
        exit;
    end;
  end;
  if (Header.Item<>nil) and
     (Header.ParaNo>=0) then begin
    if ParaStylesMapping<>nil then begin
      if Header.ParaNo>=ParaStylesMapping.Count then begin
        RVData.RVFWarnings := RVData.RVFWarnings + [rvfwConvUnknownStyles];
        exit;
      end;
      Header.ParaNo := ParaStylesMapping[Header.ParaNo];
    end;
    if Header.ParaNo>=RVData.GetRVStyle.ParaStyles.Count then begin
      RVData.RVFWarnings := RVData.RVFWarnings + [rvfwConvUnknownStyles];
      if rvfoConvUnknownStylesToZero in RVData.RVFOptions then
        Header.ParaNo := 0
      else
        exit;
    end;
  end;
  case Header.StyleNo of
  {*}rvsCheckpoint:
    begin
      if not (P^ in [#0, #10, #13]) then begin
        if not RVFReadInteger(P,Header.RaiseEvent) then
          exit;
        end
      else
        Header.RaiseEvent := 0;
      if not (P^ in [#0, #10, #13]) then begin
        if not RVFReadInteger(P,Header.PersistentCheckpoint) then
          exit;
        end
      else
        Header.PersistentCheckpoint := 0;
    end;
  {*}rvsBack:
    begin
      if not (RVFReadInteger(P, ABackgroundStyle) and
         RVFReadInteger(P, AColor)) then
        exit;
      if rvfoLoadBack in RVData.RVFOptions then begin
        Color := AColor;
        if Background<>nil then begin
          Background.Style := TBackgroundStyle(ABackgroundStyle);
          Background.Bitmap.Handle := 0;
        end;
      end;
    end;
  {*}else
    begin
      if Header.Item = nil then begin
        Result := True;
        exit;
      end;
      if not Header.Item.ReadRVFHeader(P, RVData) then
        exit;
    end;
  end;
  Result := True;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.DoOnStyleReaderError(Reader: TReader;
  const Message: string; var Handled: Boolean);
begin
  RVFWarnings := RVFWarnings + [rvfwUnknownStyleProperties];
  Handled := True;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.InsertRVFFromStream_(Stream: TStream; var Index: Integer;
  AParaNo: Integer; AllowReplaceStyles, AppendMode, EditFlag: Boolean;
  var Color: TColor; Background: TRVBackground; Layout: TRVLayoutInfo;
  var NonFirstItemsAdded: Integer; var Protect, FullReformat: Boolean):Boolean;
var BufferString, CurrentLine: String;
    Size: Integer;
    P, EndP: PChar;
    ReadState:TRVFReadState;
    ReadMode: TRVFReadMode;
    Header: TRVFHeader;
    FirstInsert: Boolean;
    PrevCP, CurCP: TRVCPInfo;
    PTextStylesMapping: PRVIntegerList;
    PParaStylesMapping: PRVIntegerList;
    PListStylesMapping: PRVIntegerList;    
    InsertPoint: Integer;
  {.......................................................}
   procedure FreeCheckpointTag; // in-out: Header
   begin
     if rvoTagsArePChars in Options then StrDispose(PChar(Header.CheckpointTag));
     Header.CheckpointTag := 0;
   end;
  {.......................................................}
  {$IFDEF RICHVIEWCBDEF3}
  procedure ReadStyles(Styles: TCustomRVInfos; StylesReadMode: TRVFReaderStyleMode);
  var Reader: TReader;
      TmpStream: TMemoryStream;
      Val: TValueType;
  begin
    TmpStream := TMemoryStream.Create;
    try
      case Header.ReadType of
        0: // text
          RVFTextString2Stream(CurrentLine, TmpStream);
        2: // binary
          TmpStream.WriteBuffer(PChar(CurrentLine)^, Length(CurrentLine));
      end;
      if (TmpStream.Size>0) and (StylesReadMode<>rvf_sIgnore) then begin
        TmpStream.Position := 0;
        TmpStream.ReadBuffer(Val, sizeof(Val));
        if Val<>vaCollection then
          abort;
        Reader := TReader.Create(TmpStream, 4096);
        try
          Reader.OnError := DoOnStyleReaderError;
          if Styles is TRVListInfos then
            TRVListInfos(Styles).FRVData := Self;
          try
            Reader.ReadCollection(Styles);
          finally
            if Styles is TRVListInfos then
              TRVListInfos(Styles).FRVData := nil;
          end;
        finally
          Reader.Free;
        end;
      end;
    finally
      TmpStream.Free;
    end;
  end;
  {.......................................................}
  procedure MergeStyles(Base, Loaded: TCustomRVInfos;
                        var Mapping: TRVIntegerList;
                        StylesReadMode: TRVFReaderStyleMode);
  begin
    if (Loaded.Count=0) or (StylesReadMode = rvf_sIgnore) or (Mapping<>nil) then
      exit;
    if AllowReplaceStyles then begin
      Base.Assign(Loaded);
      exit;
    end;
    Mapping := TRVIntegerList.Create;
    case StylesReadMode of
      rvf_sInsertMap:
        Base.MergeWith(Loaded, rvs_merge_Map, Mapping, PTextStylesMapping^);
      rvf_sInsertMerge:
        Base.MergeWith(Loaded, rvs_merge_SmartMerge, Mapping, PTextStylesMapping^);
    end;
  end;
  {.......................................................}
   procedure ReadDocProperty;
   var Styles, BaseStyles: TCustomRVInfos;
       StylesReadMode: TRVFReaderStyleMode;
       PMapping: PRVIntegerList;
       {$IFNDEF RVDONOTUSELISTS}
       i, ListNo: Integer;
       {$ENDIF}
   begin
     case Header.DataRead of
       0:
         begin
           if (Header.ExtraValue=RVF_DOCPROP_DOCPROPLIST) and AllowReplaceStyles and
              (rvfoLoadDocProperties in RVFOptions) and (GetDocProperties<>nil) then
             GetDocProperties.Add(RVFDecodeLineBreaks(CurrentLine));
           // Header.ExtraValue<>RVF_DOCPROP_DOCPROPLIST then ignoring this line (should be name of TRVStyle)
           if Header.ReadType=2 then ReadMode := rmBeforeBinary;
         end;
       1:
         begin
           case Header.ExtraValue of
             RVF_DOCPROP_TEXTSTYLES:
               begin
                 Styles := TFontInfos.Create(GetRVStyle.GetTextStyleClass, GetRVStyle);
                 StylesReadMode := GetRVFTextStylesReadMode;
                 BaseStyles := GetRVStyle.TextStyles;
                 PMapping := PTextStylesMapping;
               end;
             RVF_DOCPROP_PARASTYLES:
               begin
                 Styles := TParaInfos.Create(GetRVStyle.GetParaStyleClass, GetRVStyle);
                 StylesReadMode := GetRVFParaStylesReadMode;
                 BaseStyles := GetRVStyle.ParaStyles;
                 PMapping := PParaStylesMapping;
               end;
             RVF_DOCPROP_LISTSTYLES:
               begin
                 Styles := TRVListInfos.Create(GetRVStyle.GetListStyleClass, GetRVStyle);
                 StylesReadMode := GetRVFParaStylesReadMode;
                 BaseStyles := GetRVStyle.ListStyles;
                 PMapping := PListStylesMapping;
               end;
             RVF_DOCPROP_LAYOUT:
               begin
                 if (Layout=nil) or (not AllowReplaceStyles) or
                    not (rvfoLoadLayout in RVFOptions) then
                   exit;
                 case Header.ReadType of
                   0: // text
                      Layout.LoadText(CurrentLine);
                   2: // binary
                      Layout.LoadBinary(CurrentLine);
                 end;
                 if (Layout.FirstParaAborted<>0) and (Layout.FirstMarkerListNo>=0) and
                    (PListStylesMapping^<>nil) then
                   Layout.FirstMarkerListNo := PListStylesMapping^[Layout.FirstMarkerListNo];
                 exit;
               end;
             RVF_DOCPROP_PREVMARKERS:
               begin
                 {$IFNDEF RVDONOTUSELISTS}
                 if (GetPrevMarkers=nil) or (not AllowReplaceStyles) or
                   not (rvfoLoadLayout in RVFOptions) then
                   exit;
                 case Header.ReadType of
                   0: // text
                      GetPrevMarkers.LoadText(CurrentLine, Self);
                   2: // binary
                      GetPrevMarkers.LoadBinary(CurrentLine, Self);
                 end;
                 if (PListStylesMapping^<>nil) then
                   for i := 0 to GetPrevMarkers.Count-1 do begin
                     ListNo := TRVMarkerItemInfo(GetPrevMarkers.Items[i]).ListNo;
                     if ListNo>=0 then
                     TRVMarkerItemInfo(GetPrevMarkers.Items[i]).ListNo :=
                       PListStylesMapping^[ListNo];
                   end;
                 {$ENDIF}
                 exit;
               end;
             RVF_DOCPROP_DOCPROPLIST:
               begin
                 if AllowReplaceStyles and (rvfoLoadDocProperties in RVFOptions) and
                   (GetDocProperties<>nil) then
                   GetDocProperties.Add(RVFDecodeLineBreaks(CurrentLine));
                 exit;
               end;
             else
               exit;
           end;
           try
             ReadStyles(Styles, StylesReadMode);
             MergeStyles(BaseStyles, Styles, PMapping^, StylesReadMode);
           finally
             Styles.Free;
           end;
         end;
       else
         if (Header.ExtraValue=RVF_DOCPROP_DOCPROPLIST) and AllowReplaceStyles and
            (rvfoLoadDocProperties in RVFOptions) and (GetDocProperties<>nil) then
           GetDocProperties.Add(RVFDecodeLineBreaks(CurrentLine));
     end;
   end;
  {$ENDIF}
  {.......................................................}
                             // in    : CurrentLine
   procedure ReadBackground; // in-out: Header
                             // out   : ReadMode, ReadState
   var bmp : TBitmap;
   begin
     case Header.DataRead of
       0:
         begin
           // ignoring this line (should be TBitmap)
           if Header.ReadType=2 then ReadMode := rmBeforeBinary;
         end;
       1:
         begin
           if rvfoLoadBack in RVFOptions then begin
             if Background<>nil then begin
               if Header.ReadType=2 then
                 RVFLoadPictureBinary(CurrentLine, Background.Bitmap)
               else
                 if not RVFLoadPicture(CurrentLine, Background.Bitmap) then abort; {error}
               end
             else begin
               bmp := TBitmap.Create;
               if Header.ReadType=2 then
                 RVFLoadPictureBinary(CurrentLine, bmp)
               else
                 if not RVFLoadPicture(CurrentLine, bmp) then abort; {error}
               bmp.Free;
             end;
           end;
           ReadState := rstSkip;
         end;
     end;
   end;
  {.......................................................}
                               // in: ReadMode
                               // in-out: P
    procedure ReadCurrentLine; // out: CurrentLine
    var Start: PChar;
        Size: Integer;
    begin
      Start := P;
      case ReadMode of
        rmBinary:
          begin
            Move(P^,Size, SizeOf(Size));
            inc(Start, SizeOf(Size));
            inc(P,     SizeOf(Size)+Size);
          end;
        rmUnicode:
          begin
            while (PWord(P)^<>UNI_ParagraphSeparator) and
                  (PWord(P)^<>0) and
                  (P<EndP) do Inc(P,2);
          end;
        else
          begin
            while not (P^ in [#0, #10, #13]) do Inc(P);
          end;
      end;
      SetString(CurrentLine, Start, P - Start);
    end;
  {.......................................................}
   procedure SkipCurrentLineTail; // in-out: P, ReadMode
   begin
     case ReadMode of
       rmText:
         begin
           if P^ = #13 then Inc(P);
           if P^ = #10 then Inc(P);
         end;
       rmBeforeUnicode:
         begin
           if P^ = #13 then Inc(P) else abort; {error}
           if P^ = #10 then Inc(P) else abort; {error}
           ReadMode := rmUnicode;
         end;
       rmUnicode:
         begin
           if PWord(P)^=UNI_ParagraphSeparator then
             Inc(P, 2);
         end;
       rmAfterUnicode:
         begin
           if PWord(P)^=UNI_ParagraphSeparator then
             Inc(P, 2);
           ReadMode := rmText;
         end;
       rmBeforeBinary:
         begin
           if P^ = #13 then Inc(P) else abort; {error}
           if P^ = #10 then Inc(P) else abort; {error}
           ReadMode := rmBinary;
         end;
       rmBinary:
         begin
           ReadMode := rmText;
         end;
     end;
   end;
  {.......................................................}
  var StartIndex: Integer;
begin
  NonFirstItemsAdded := 0;
  Result           := True;
  Protect          := True;
  FirstInsert      := True;
  StartIndex       := Index;
  InsertPoint      := Index;
  if Index>Items.Count then
    Index := Items.Count;
  if Index=Items.Count then begin
    PrevCP := LastCP;
    if EditFlag then
      CurCP := nil
    else
      CurCP  := NotAddedCP;
    end
  else begin
    PrevCP := FindCPBeforeItem(Index);
    CurCP := nil;
  end;
  RVFWarnings := [];
  if AllowReplaceStyles and (GetDocProperties<>nil) then
    GetDocProperties.Clear;
  FillChar(Header,sizeof(Header),0);
  Header.Version := 1;
  Header.SubVersion := 0;
  InitStyleMappings(PTextStylesMapping, PParaStylesMapping, PListStylesMapping);
  try
    Size := Stream.Size - Stream.Position;
    SetString(BufferString, nil, Size);
    Stream.Read(Pointer(BufferString)^, Size);
    P := Pointer(BufferString);
    EndP := PChar(BufferString)+Size;
    ReadState := rstHeader;
    ReadMode := rmText;
    if P <> nil then
      while P < EndP do begin
        ReadCurrentLine;
        case ReadState of
          rstHeader:
            begin
              if not RVFReadHeader(Self, CurrentLine, Header, AParaNo,
                Color, Background, PTextStylesMapping^, PParaStylesMapping^) then
                abort; {error}
              if (Header.DataCount=0) then begin
                if not InsertRVFHeaderData(Self, '', Header, PrevCP, CurCP,
                  Index, InsertPoint, FirstInsert, AParaNo, AppendMode, EditFlag,
                  NonFirstItemsAdded, FullReformat, PListStylesMapping^) then
                  exit;
                ReadState := rstHeader;
                end
              else
                if ((Header.Item=nil) and (Header.StyleNo<>rvsCheckpoint) and
                    (Header.StyleNo<>rvsBack)
                    {$IFDEF RICHVIEWCBDEF3}
                    and (Header.StyleNo<>rvsDocProperty)
                    {$ENDIF}
                    ) or
                   ((Header.Item<>nil) and
                    not Header.Item.GetBoolValue(rvbpRequiresRVFLines)) then
                  ReadState := rstSkip
                else begin
                  ReadState := rstData;
                  {$IFNDEF RVDONOTUSEUNICODE}
                  if (Header.Item<>nil) and
                     (rvioUnicode in Header.Item.ItemOptions) and
                     (Header.ReadType<>3) then
                    ReadMode := rmBeforeUnicode;
                  {$ENDIF}
                end;
            end;
          rstData:
            begin
              if Header.StyleNo<0 then begin
                case Header.StyleNo of
                {*} rvsBack:
                   ReadBackground;
                {$IFDEF RICHVIEWCBDEF3}
                {*} rvsDocProperty:
                   ReadDocProperty;
                {$ENDIF}
                {*} rvsCheckpoint:
                  begin
                    if CurCP = nil then begin
                      CurCP := TRVCPInfo.Create;
                      CurCP.Name := CurrentLine;
                      CurCP.Tag  := Header.CheckpointTag;
                      CurCP.RaiseEvent := Boolean(Header.RaiseEvent);
                      CurCP.Persistent := Boolean(Header.PersistentCheckpoint);
                      Header.CheckpointTag := 0;
                    end;
                  end;
                {*} else
                  begin
                    if Header.Item<>nil then
                      if not Header.Item.ReadRVFLine(CurrentLine, Self,
                         Header.ReadType, Header.DataRead, Header.DataCount,
                         Header.Name, ReadMode, ReadState) then
                        abort;
                    if Header.DataRead=Header.DataCount-1 then
                      if not InsertRVFHeaderData(Self, Header.Name, Header,
                         PrevCP, CurCP, Index, InsertPoint, FirstInsert, AParaNo,
                         AppendMode, EditFlag, NonFirstItemsAdded, FullReformat,
                         PListStylesMapping^) then
                        exit;
                  end
                end
                end
              else begin
                if not InsertRVFHeaderData(Self, CurrentLine, Header, PrevCP, CurCP,
                   Index, InsertPoint, FirstInsert, AParaNo, AppendMode, EditFlag,
                   NonFirstItemsAdded, FullReformat, PListStylesMapping^) then
                  exit;
                if Header.DataRead=Header.DataCount-1 then begin
                  if rvoTagsArePChars in Options then
                    StrDispose(PChar(Header.Item.Tag));
                  Header.Item.Free;
                  Header.Item := nil;
                end;
              end;
              inc(Header.DataRead);
              if Header.DataRead=Header.DataCount then begin
                ReadState := rstHeader;
                if ReadMode=rmUnicode then
                  ReadMode := rmAfterUnicode;
              end;
            end;
          rstSkip:
            begin
              inc(Header.DataRead);
              if (Header.DataRead=Header.DataCount-1) and (Header.ReadType=2) then
                ReadMode := rmBeforeBinary
              else if Header.DataRead=Header.DataCount then begin
                if not InsertRVFHeaderData(Self, Header.Name, Header, PrevCP, CurCP,
                   Index, InsertPoint, FirstInsert, AParaNo, AppendMode, EditFlag,
                   NonFirstItemsAdded, FullReformat, PListStylesMapping^) then
                  exit;
                ReadState := rstHeader;
              end;
            end;
        end;
        SkipCurrentLineTail;
      end; // of while
    Result := (ReadState = rstHeader);
    {$IFNDEF RVDONOTUSELISTS}
    if (InsertPoint-1>=0) and (InsertPoint-1<ItemCount) and
       (GetItemStyle(InsertPoint-1)=rvsListMarker) and
       ((InsertPoint=ItemCount) or IsParaStart(InsertPoint)) then begin
      Header.StyleNo := 0;
      Header.ParaNo := -1;
      Header.Item := RichViewTextItemClass.Create(Self);
      InsertRVFHeaderData(Self, '', Header, PrevCP, CurCP, Index, InsertPoint,
        FirstInsert, AParaNo, AppendMode, EditFlag,
        NonFirstItemsAdded, FullReformat, PListStylesMapping^);
      Header.Item.Free;
      Header.Item := nil;
    end;
    {$ENDIF}
    if not EditFlag then
      NormalizeParas(StartIndex);
    Protect := False;
  except
    Result := False;
  end;

  DoneStyleMappings(PTextStylesMapping,PParaStylesMapping,PListStylesMapping);
  FreeCheckpointTag;
  if Result and (InsertPoint=Items.Count) and (NotAddedCP=nil) then begin
    if CurCP<> nil then inc(CPCount);
    NotAddedCP := CurCP
    end
  else
    if NotAddedCP<>CurCP then
      FreeCheckpoint(CurCP, False, False); // ignore cp from stream
end;
{------------------------------------------------------------------------------}
function TCustomRVData.AppendRVFFromStream(Stream: TStream; ParaNo: Integer;
                             var Color: TColor;
                             Background: TRVBackground):Boolean;
var Dummy: Integer;
    Dummy2, Dummy3: Boolean;
    Index: Integer;
begin
  Index := Items.Count;
  Result := InsertRVFFromStream_(Stream, Index, ParaNo, False, True, False,
    Color, Background, nil, Dummy, Dummy2, Dummy3);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.InsertRVFFromStream(Stream: TStream; Index: Integer;
  var Color: TColor; Background: TRVBackground; Layout: TRVLayoutInfo;
  AllowReplaceStyles: Boolean):Boolean;
var Dummy: Integer;
    Dummy2,Dummy3: Boolean;
begin
  // AParaNo is used only if AppendMode=True
  Result := InsertRVFFromStream_(Stream, Index, -1, AllowReplaceStyles, False, False,
                             Color, Background, Layout, Dummy, Dummy2,Dummy3);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.LoadRVF(const FileName: String; var Color: TColor;
  Background: TRVBackground; Layout: TRVLayoutInfo):Boolean;
var Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName,fmOpenRead);
    try
      Result := LoadRVFFromStream(Stream, Color, Background, Layout);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.LoadRVFFromStream(Stream: TStream; var Color: TColor;
  Background: TRVBackground; Layout: TRVLayoutInfo):Boolean;
begin
  Clear;
  Result := InsertRVFFromStream(Stream,0, Color, Background, Layout, True);
end;
{------------------------------------------------------------------------------}
procedure RVFWriteCheckpoint(Stream: TStream; TagsArePChars: Boolean;
                             cp: TRVCPInfo);
begin
  if cp=nil then
    exit;
  RVFWriteLine(Stream, Format('%d %d %d %d %d %s %d %d',
    [rvsCheckpoint, 1, 0, 0, 0, RVFSaveTag(TagsArePChars,cp.Tag),
     Integer(cp.RaiseEvent), Integer(cp.Persistent)]));
  RVFWriteLine(Stream, cp.Name);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SaveRVF(const FileName: String; SelectionOnly: Boolean;
  Color: TColor; Background: TRVBackground; Layout: TRVLayoutInfo):Boolean;
var Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName,fmCreate);
    try
      Result := SaveRVFToStream(Stream, SelectionOnly, Color, Background, Layout);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SaveRVFToStream(Stream: TStream; SelectionOnly: Boolean;
  Color: TColor; Background: TRVBackground; Layout: TRVLayoutInfo):Boolean;
begin
  Result := SaveRVFToStreamEx(Stream, GetRVFSaveScope(SelectionOnly), Color,
    Background, Layout);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SaveRVFToStreamEx(Stream: TStream; SaveScope: TRVFSaveScope;
  Color: TColor; Background: TRVBackground; Layout: TRVLayoutInfo):Boolean;
var i: Integer;
    Header: TRVFHeader;
    SectionBackOffs: Integer;
    StartItem, EndItem, StartOffs, EndOffs: Integer;
    StartPart, EndPart: TRVMultiDrawItemPart;
    MarkerItemNo: Integer;
   {.......................................................}
     procedure RVFSaveVersionInfo;
     begin
       RVFWriteLine(Stream, SysUtils.Format('%d %d %d',
         [rvsVersionInfo, RVFVersion, RVFSubVersion]));
     end;
   {.......................................................}
   {$IFDEF RICHVIEWCBDEF3}
     procedure RVFSaveStyles(Id: Integer; Styles: TCollection);
     var SaveType: Integer;
         Writer: TWriter;
         TmpStream: TMemoryStream;
         Pos,Pos2: Integer;
     begin
       if rvfoSaveBinary in RVFOptions then
         SaveType := 2 // save binary
       else
         SaveType := 0; // save hex dump
       RVFWriteLine(Stream, Format('%d %d %d %d %d %d %d',
          [rvsDocProperty, 2, 0, 0, SaveType, 0, Id]));
       RVFWriteLine(Stream, GetRVStyle.Name);
       if rvfoSaveBinary in RVFOptions then begin
         Pos := Stream.Position;
         Stream.WriteBuffer(Pos, sizeof(Pos));
         Writer := TWriter.Create(Stream, 4096);
         if Styles is TRVListInfos then
           TRVListInfos(Styles).FRVData := Self;
         try
           Writer.WriteCollection(Styles)
         finally
           Writer.Free;
           if Styles is TRVListInfos then
             TRVListInfos(Styles).FRVData := nil;
         end;
         Pos2 := Stream.Position;
         Stream.Position := Pos;
         Pos := Pos2-Pos-sizeof(Pos);
         Stream.WriteBuffer(Pos, sizeof(Pos));
         Stream.Position := Pos2;
         end
       else begin
         TmpStream := TMemoryStream.Create;
         try
           Writer := TWriter.Create(TmpStream, 4096);
           if Styles is TRVListInfos then
             TRVListInfos(Styles).FRVData := Self;
           try
             Writer.WriteCollection(Styles);
           finally
             Writer.Free;
             if Styles is TRVListInfos then
               TRVListInfos(Styles).FRVData := nil;
           end;
           TmpStream.Position := 0;
           RVFWriteLine(Stream, RVFStream2TextString(TmpStream));
         finally
           TmpStream.Free;
         end;
       end;
     end;
   {$ENDIF}
   {.......................................................}
     procedure RVFSaveLayout;
     var SaveType : Integer;
     begin
       if rvfoSaveBinary in RVFOptions then
         SaveType := 2 // save binary
       else
         SaveType := 0; // save hex dump
       RVFWriteLine(Stream, Format('%d %d %d %d %d %d %d',
          [rvsDocProperty, 2, 0, 0, SaveType, 0, RVF_DOCPROP_LAYOUT]));
       RVFWriteLine(Stream, '');
       case SaveType of
         2:
           Layout.SaveToStream(Stream,True);
         0:
           Layout.SaveTextToStream(Stream);
       end;
     end;
   {.......................................................}
   {$IFNDEF RVDONOTUSELISTS}
     procedure RVFSavePrevMarkers(StartItemNo: Integer);
     var SaveType, MarkerIndex : Integer;
         Marker: TRVMarkerItemInfo;
     begin
       if StartItemNo=0 then
         exit;
       Marker := FindPreviousMarker(StartItemNo-1);
       if Marker=nil then
         exit;
       MarkerIndex := Marker.GetIndexInList(GetMarkers(False));
       if MarkerIndex<0 then
         exit;
       if rvfoSaveBinary in RVFOptions then
         SaveType := 2 // save binary
       else
         SaveType := 0; // save hex dump
       RVFWriteLine(Stream, Format('%d %d %d %d %d %d %d',
          [rvsDocProperty, 2, 0, 0, SaveType, 0, RVF_DOCPROP_PREVMARKERS]));
       RVFWriteLine(Stream, '');
       case SaveType of
         2:
           GetMarkers(False).SaveToStream(Stream, MarkerIndex+1, True);
         0:
           GetMarkers(False).SaveTextToStream(Stream, MarkerIndex+1);
       end;
     end;
   {$ENDIF}
   {.......................................................}
     procedure RVFSaveDocPropertiesStringList;
     var i: Integer;
         dp: TStringList;
     begin
       dp := GetAbsoluteRootData.GetDocProperties;
       if (dp=nil) or (dp.Count=0) then
         exit;
       RVFWriteLine(Stream, Format('%d %d %d %d %d %d %d',
          [rvsDocProperty, dp.Count, 0, 0, 0, 0, RVF_DOCPROP_DOCPROPLIST]));
       for i := 0 to dp.Count-1 do
         RVFWriteLine(Stream, RVFEncodeLineBreaks(dp.Strings[i]));
     end;
   {.......................................................}
     procedure RVFSaveBackground;
     var SaveType, LineCount: Integer;
     begin
       if Background=nil then exit;
       if Background.Bitmap.Empty or (Background.Style=bsNoBitmap) then
         RVFWriteLine(Stream, Format('%d %d %d %d %d %d %d %d',
                            [rvsBack, 0, -1, 0, 0, 0, ord(Background.Style), Color]))
       else begin
         LineCount := 2;
         if rvfoSaveBinary in RVFOptions then
           SaveType := 2 // save binary
         else
           SaveType := 0 // save hex dump
         ;
         RVFWriteLine(Stream, Format('%d %d %d %d %d %d %d %d',
            [rvsBack, LineCount, 0, 0, SaveType, 0, ord(Background.Style), Color]));
         if SaveType<>1 then begin
           RVFWriteLine(Stream, Background.Bitmap.ClassName);
           if rvfoSaveBinary in RVFOptions then
             RVFSavePictureBinary(Stream, Background.Bitmap)
           else
             RVFWriteLine(Stream, RVFSavePicture(Background.Bitmap));
         end;
       end;
     end;
   {.......................................................}
    procedure WritePartialTextLine(ItemNo, StartOffs, EndOffs: Integer;
      ForceSavingPara: Boolean); // in: Stream
    var AFromStart: Boolean;
        AParaNo: Integer;
        SaveMode: Integer;
        Tail: String;
    begin
      AFromStart := (StartOffs <= GetOffsBeforeItem(ItemNo));
      with GetItem(ItemNo) do begin
         {$IFDEF RICHVIEWCBDEF3}
          if (rvfoSaveBinary in RVFOptions) or
             not (rvioUnicode in ItemOptions) then
            SaveMode := 0
          else
            SaveMode := 3;
         {$ELSE}
         SaveMode := 0;
         {$ENDIF}
         if (AFromStart and not SameAsPrev) or ForceSavingPara then
           AParaNo := ParaNo
         else
           AParaNo   := -1;
         if AFromStart then
           RVFWriteCheckpoint(Stream, rvoTagsArePChars in Options, Checkpoint);
         //if (AParaNo=-1) and (EndOffs=StartOffs) then
         //  exit;
         Tail := '';
         {$IFNDEF RVDONOTUSEITEMHINTS}
         {$IFDEF RICHVIEWCBDEF3}
         if Hint<>'' then
           Tail := ' '+AnsiQuotedStr(Hint, '"');
         {$ENDIF}
         {$ENDIF}
         RVFWriteLine(Stream, SysUtils.Format('%s %d %s %d %d %s%s',
           [RVFSaveText(GetRVStyle, rvfoUseStyleNames in RVFOptions, StyleNo), 1,
           RVFSavePara(GetRVStyle, rvfoUseStyleNames in RVFOptions, AParaNo),
           Byte(GetItemOptions(ItemNo)) and RVItemOptionsMask,
           SaveMode, RVFSaveTag(rvoTagsArePChars in Options, Tag),
           Tail]));
      end;
      RVFWriteLineX(Stream,
        RVU_Copy(Items[ItemNo], StartOffs, EndOffs-StartOffs, GetItemOptions(ItemNo)),
        rvioUnicode in GetItemOptions(ItemNo), SaveMode=3);
    end;
   {.......................................................}
    function IsTheSameStyleText: Boolean; // in: i, Header
    begin
      with GetItem(i) do
        Result := (not SameAsPrev) and (StyleNo>=0) and (StyleNo=Header.StyleNo) and
          (ParaNo=Header.ParaNo) and
          ((Byte(ItemOptions) and RVItemOptionsMask) = (Byte(Header.Item.ItemOptions)and RVItemOptionsMask)) and
          RV_CompareTags(Tag, Header.Item.Tag, rvoTagsArePChars in Options) and
          {$IFNDEF RVDONOTUSEITEMHINTS}
          (Hint=Header.Item.Hint) and
          {$ENDIF}
          (Checkpoint=nil);
    end;
  {.......................................................}
   procedure RVFWritePrevStrings(i: Integer); // in: Header, SectionBackOffs
   var j: Integer;
       ItemOptions: TRVItemOptions;
       SaveMode: Integer;
       Tail: String;
   begin
     {$IFDEF RICHVIEWCBDEF3}
      if (rvfoSaveBinary in RVFOptions) or
         not (rvioUnicode in GetItemOptions(i-SectionBackOffs)) then
        SaveMode := 0
      else
        SaveMode := 3;
     {$ELSE}
     SaveMode := 0;
     {$ENDIF}
     RVFWriteCheckpoint(Stream, rvoTagsArePChars in Options, Header.Item.Checkpoint);
     if MarkerItemNo>=0 then begin
       ItemOptions := RVFGetItemOptions(Header.Item.ItemOptions, MarkerItemNo>=0);
       RVFWriteLine(Stream, Format('%s %d %s %d %d %s',
         [RVFSaveText(GetRVStyle, rvfoUseStyleNames in RVFOptions, Header.StyleNo),
         1,
         RVFSavePara(GetRVStyle, rvfoUseStyleNames in RVFOptions, -1),
         Byte(ItemOptions) and RVItemOptionsMask,
         SaveMode, RVFSaveTag(rvoTagsArePChars in Options, Header.Item.Tag)]));
       RVFWriteLineX(Stream, Items[i-SectionBackOffs],
         rvioUnicode in GetItemOptions(i-SectionBackOffs), SaveMode=3);
       dec(SectionBackOffs);
       MarkerItemNo := -1;
     end;
     if SectionBackOffs=0 then
       exit;
     Tail := '';
     {$IFNDEF RVDONOTUSEITEMHINTS}
     {$IFDEF RICHVIEWCBDEF3}
     if GetItem(i-SectionBackOffs).Hint<>'' then
       Tail := ' '+AnsiQuotedStr(GetItem(i-SectionBackOffs).Hint, '"');
     {$ENDIF}
     {$ENDIF}
     RVFWriteLine(Stream, Format('%s %d %s %d %d %s%s',
       [RVFSaveText(GetRVStyle, rvfoUseStyleNames in RVFOPtions, Header.StyleNo),
       SectionBackOffs,
       RVFSavePara(GetRVStyle, rvfoUseStyleNames in RVFOPtions, Header.ParaNo),
       Byte(Header.Item.ItemOptions) and RVItemOptionsMask,
       SaveMode, RVFSaveTag(rvoTagsArePChars in Options, Header.Item.Tag),
       Tail]));
     for j := i-SectionBackOffs to i-1 do
       RVFWriteLineX(Stream, Items[j], rvioUnicode in GetItemOptions(j), SaveMode=3);
     SectionBackOffs := 0;
   end;
  {.......................................................}
   procedure RVFSetHeaderHeader(i: Integer); // in: Header
   begin
     with GetItem(i) do begin
       Header.Item.Checkpoint := Checkpoint;
       Header.Item.ItemOptions := ItemOptions;
       Header.StyleNo      := StyleNo;
       Header.Item.StyleNo := StyleNo;
       {$IFNDEF RVDONOTUSEITEMHINTS}
       Header.Item.Hint := Hint;
       {$ENDIF}
       if SameAsPrev then
         Header.ParaNo   := -1
       else
         Header.ParaNo   := ParaNo;
       Header.Item.Tag     := Tag;
     end;
   end;
  {.......................................................}
   procedure RVFWriteNonText(i: Integer; Part: TRVMultiDrawItemPart); // in: Header
   {$IFNDEF RVDONOTUSELISTS}
   var StartFrom: Integer;
       Reset: Boolean;
       marker: TRVMarkerItemInfo;
   {$ENDIF}
   begin
     with GetItem(i) do begin
       RVFWriteCheckpoint(Stream, rvoTagsArePChars in Options, Checkpoint);
       {$IFNDEF RVDONOTUSELISTS}
       if StyleNo=rvsListMarker then begin
         marker := TRVMarkerItemInfo(GetItem(i));
         StartFrom := marker.StartFrom;
         Reset     := marker.Reset;
         if SaveScope=rvfss_Page then begin
           marker.StartFrom := marker.Counter;
           marker.Reset     := True;
         end;
         end
       else begin
         StartFrom := 0;    // avoiding warnings
         Reset     := False;
         marker    := nil;
       end;
       {$ENDIF}
       SaveRVF(Stream, Self, i, Header.ParaNo, Items[i], Part, MarkerItemNo>=0);
       {$IFNDEF RVDONOTUSELISTS}
       if StyleNo=rvsListMarker then begin
         marker.StartFrom := StartFrom;
         marker.Reset     := Reset;
       end;
       {$ENDIF}
       MarkerItemNo := -1;
     end;
   end;
  {.......................................................}
begin
  Result := True;
  if (Items.Count=0) {or (SelectionOnly and not SelectionExists)} then
    exit;
  FillChar(Header, sizeof(Header), 0);
  Header.Item := RichViewTextItemClass.Create(Self);
  try
    RVFSaveVersionInfo;
    if (SaveScope<>rvfss_Selection) and (rvfoSaveBack in RVFOptions) then
      RVFSaveBackground;
    if (rvflRoot in Flags) or (SaveScope=rvfss_Selection) then begin
      {$IFDEF RICHVIEWCBDEF3}
      if (rvfoSaveTextStyles in RVFOptions) then
        RVFSaveStyles(RVF_DOCPROP_TEXTSTYLES, GetRVStyle.TextStyles);
      if (rvfoSaveParaStyles in RVFOptions) and (Self=GetRootData) then begin
        RVFSaveStyles(RVF_DOCPROP_PARASTYLES, GetRVStyle.ParaStyles);
        RVFSaveStyles(RVF_DOCPROP_LISTSTYLES, GetRVStyle.ListStyles);
      end;
      {$ENDIF}
      if (rvfoSaveDocProperties in RVFOptions) and (SaveScope<>rvfss_Page) then
        RVFSaveDocPropertiesStringList;
    end;
    if (SaveScope=rvfss_Page) and (Layout<>nil) then begin
      RVFGetLimits(SaveScope,StartItem,EndItem,StartOffs,EndOffs,StartPart,EndPart);
      if (StartItem>=0) and (StartItem<=EndItem) then begin
        if (StartOffs>GetOffsBeforeItem(StartItem)) or
           ((StartOffs<=GetOffsBeforeItem(StartItem)) and not IsParaStart(StartItem)) then begin
          Layout.FirstParaAborted := 1;
          {$IFNDEF RVDONOTUSELISTS}
          MarkerItemNo := GetFirstParaItem(StartItem);
          if (MarkerItemNo<>StartItem) and (GetItemStyle(MarkerItemNo)=rvsListMarker) then begin
            Layout.FirstMarkerListNo := TRVMarkerItemInfo(GetItem(MarkerItemNo)).ListNo;
            Layout.FirstMarkerLevel := TRVMarkerItemInfo(GetItem(MarkerItemNo)).Level;
          end;
          {$ENDIF}
        end;
        if (EndOffs<GetOffsAfterItem(EndItem)) or
           ((EndOffs>=GetOffsAfterItem(EndItem)) and not ((EndItem+1=ItemCount) or (IsParaStart(EndItem+1)))) then
          Layout.LastParaAborted := 1;
        {$IFNDEF RVDONOTUSELISTS}
        RVFSavePrevMarkers(StartItem);
        {$ENDIF}
      end;
    end;
    if (rvfoSaveLayout in RVFOptions) and (Self=GetRootData) and (Layout<>nil) and
       (SaveScope<>rvfss_Selection) then
      RVFSaveLayout;
    {$IFNDEF RVDONOTUSEINPLACE}
    if (SaveScope=rvfss_Selection) and (GetChosenRVData<>nil) then begin
      Result := GetChosenRVData.SaveRVFToStreamEx(Stream, SaveScope,
        clNone, nil, nil);
      Header.Item.Free;
      exit;
    end;
    {$ENDIF}
    RVFGetLimits(SaveScope,StartItem,EndItem,StartOffs,EndOffs,StartPart,EndPart);
    if (StartItem=-1) or (StartItem>EndItem) then exit;
    if (StartItem=EndItem) and
       ((StartOffs>GetOffsBeforeItem(StartItem)) or
        (EndOffs  <GetOffsAfterItem(EndItem))) then begin
      // only part of text line is selected
      WritePartialTextLine(StartItem, StartOffs, EndOffs, SaveScope=rvfss_Page);
      exit;
    end;
    SectionBackOffs := 0;
    if (StartPart<>nil) then begin
      Header.ParaNo := GetItem(StartItem).ParaNo;
      RVFWriteNonText(StartItem, StartPart);
      inc(StartItem);
    end;
    MarkerItemNo := -1;
    {$IFNDEF RVDONOTUSELISTS}
    if (SaveScope=rvfss_Selection) and (StartPart=nil) then begin
      MarkerItemNo := GetFirstParaItem(StartItem);
      if (MarkerItemNo<>StartItem) and (GetItemStyle(MarkerItemNo)=rvsListMarker) then begin
        RVFWriteNonText(MarkerItemNo, nil);
        MarkerItemNo := GetFirstParaItem(StartItem);
        end
      else
        MarkerItemNo := -1;
    end;
    {$ENDIF}
    if (EndPart<>nil) then
      dec(EndItem);
    for i := StartItem to EndItem do begin
      if (i = StartItem) and
         ((StartOffs>GetOffsBeforeItem(StartItem)) or
         ((SaveScope=rvfss_Page) and (GetItemStyle(i)>=0))) then begin
        // only last part of first text line selected
        WritePartialTextLine(StartItem, StartOffs, GetOffsAfterItem(StartItem), SaveScope=rvfss_Page);
        continue;
      end;
      if (i>StartItem) and IsTheSameStyleText then
        inc(SectionBackOffs)
      else begin
        if SectionBackOffs>0 then
          RVFWritePrevStrings(i);
        RVFSetHeaderHeader(i);
        if Header.StyleNo<0 then begin
          if (i<EndItem) or (EndOffs=1) then
            RVFWriteNonText(i, nil)
          end
        else
          SectionBackOffs := 1;
      end;
    end;
    if (Header.StyleNo>=0) and (EndOffs<GetOffsAfterItem(EndItem)) then begin
      // last line is partially selected
      //Assert(SectionBackOffs>0);
      dec(SectionBackOffs);
      if SectionBackOffs>0 then
        RVFWritePrevStrings(EndItem);
      WritePartialTextLine(EndItem, GetOffsBeforeItem(EndItem), EndOffs, False);
      end
    else begin
      if SectionBackOffs<>0 then
        RVFWritePrevStrings(EndItem+1);
      if (EndItem=Items.Count-1) and (EndOffs=1) then
        RVFWriteCheckpoint(Stream, rvoTagsArePChars in Options, NotAddedCP);
    end;
    if (EndPart<>nil) then begin
      RVFSetHeaderHeader(EndItem+1);
      RVFWriteNonText(EndItem+1, EndPart);
    end;
  except;
    Result := False;
  end;
  Header.Item.Free;
end;
{$ENDIF}{RVDONOTUSERVF}
{------------------------------------------------------------------------------}
function TCustomRVData.InsertFirstRVFItem(var Index: Integer;
  var s: String; var item: TCustomRVItemInfo; EditFlag: Boolean;
  var FullReformat: Boolean;
  var NewListNo: Integer): Boolean;
begin
  FullReformat := False;
  NewListNo := -1;
  item.Inserting(Self, s, False);
  Items.InsertObject(Index, s, item);
  item.Inserted(Self, Index);
  {$IFNDEF RVDONOTUSELISTS}
  AddMarkerInList(Index);
  {$ENDIF}
  Result := True;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.UpdateItemsPaletteInfo;
var i: Integer;
begin
  if not ShareItems then
    for i := 0 to Items.Count-1 do
      TCustomRVItemInfo(Items.Objects[i]).
         UpdatePaletteInfo(GetDoInPaletteMode, False, GetRVPalette, GetRVLogPalette);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.InsertCheckpoint(ItemNo, Tag: Integer;
                             const Name: String; RaiseEvent: Boolean);
var
  cp: TRVCPInfo;
begin
  if TCustomRVItemInfo(Items.Objects[ItemNo]).Checkpoint<>nil then
    raise ERichViewError.Create(errRVCPExists);
  cp            := TRVCPInfo.Create;
  cp.Tag        := Tag;
  cp.Name       := Name;
  cp.RaiseEvent := RaiseEvent;
  cp.ItemInfo   := TCustomRVItemInfo(Items.Objects[ItemNo]);
  cp.Next := nil;
  cp.Prev := nil;
  TCustomRVItemInfo(Items.Objects[ItemNo]).Checkpoint := cp;
  inc(CPCount);
  UpdateCPPos(cp, ItemNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.UpdateCPPos(cp: TRVCPInfo; ItemNo: Integer);
var cpi: TRVCPInfo;
begin
  if ItemNo=-1 then exit;
  UpdateCPItemNo;
  cp.Prev := nil;
  cp.Next := nil;
  if FirstCP = nil then begin
    FirstCP := cp;
    LastCP  := cp;
    end
  else if FirstCP.ItemNo>cp.ItemNo then begin
    cp.Next := FirstCP;
    FirstCP.Prev := cp;
    FirstCP      := cp;
    end
  else if LastCP.ItemNo<=cp.ItemNo then begin
    LastCP.Next := cp;
    cp.Prev := LastCP;
    LastCP := cp
    end
  else begin
    cpi := FirstCP;
    while cpi.Next<>nil do begin
      if cpi.Next.ItemNo>cp.ItemNo then break;
      cpi := cpi.Next;
    end;
    if cpi.Next<>nil then cpi.Next.Prev := cp;
    cp.Next := cpi.Next;
    cpi.Next := cp;
    cp.Prev := cpi;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.ShareItemsFrom(Source: TCustomRVData);
begin
  if ShareItems then begin
    Clear;
    FItems := Source.Items;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AppendFrom(Source: TCustomRVData);
var i: Integer;
    item,itemcopy: TCustomRVItemInfo;
begin
  if (rvoTagsArePChars in Options) <> (rvoTagsArePChars in Source.Options) then
    raise ERichViewError.Create(errRVTagsTypesMismatch);
  for i:=0 to Source.Items.Count-1 do begin
    item := Source.GetItem(i);
    itemcopy := RV_DuplicateItem(item, Self, True);
    if itemcopy.GetBoolValue(rvbpValid) then begin
      if itemcopy.SameAsPrev then
        itemcopy.ParaNo := -1;
      AddItem(Source.Items[i],itemcopy);
      {$IFNDEF RVDONOTUSELISTS}
      AddMarkerInList(ItemCount-1);
      {$ENDIF}
      end
    else
      InternalFreeItem(itemcopy,False);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.UpdateCPItemNo;
var i,cnt: Integer;
begin
  cnt := 0;
  if cnt=CPCount then exit;
  for i := 0 to Items.Count-1 do
    if TCustomRVItemInfo(Items.Objects[i]).Checkpoint<>nil then begin
      TCustomRVItemInfo(Items.Objects[i]).Checkpoint.ItemNo := i;
      inc(cnt);
      if cnt=CPCount then
        exit;
    end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.IsDelimiterA(ch: Char): Boolean;
var Del: String;
begin
  Del := GetDelimiters;
  Result := RV_CharPos(PChar(Del), ch, Length(Del))<>0;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.IsDelimiterW(ch: Word): Boolean;
begin
  Result := (ch<256) and (Pos(Char(ch), GetDelimiters)<>0);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.IsDelimiter(const s: String; Index: Integer;
                                   ItemOptions: TRVItemOptions): Boolean;
begin
  {$IFNDEF RVDONOTUSEUNICODE}
  if rvioUnicode in ItemOptions then
    Result := IsDelimiterW(PWord(PChar(s)+(Index-1)*2)^)
  else
  {$ENDIF}
    Result := IsDelimiterA(s[Index]);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetItemOptions(ItemNo: Integer): TRVItemOptions;
begin
  Result := TCustomRVItemInfo(Items.Objects[ItemNo]).ItemOptions;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetStyleCodePage(StyleNo: Integer): TRVCodePage;
begin
  {$IFDEF RICHVIEWCBDEF3}
  if (GetRVStyle<>nil) then
    if StyleNo>=0 then
      Result := RVU_Charset2CodePage(GetRVStyle.TextStyles[StyleNo].Charset)
    else
      Result := GetRVStyle.DefCodePage
  else
  {$ENDIF}
    Result := CP_ACP;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetStyleLocale(StyleNo: Integer): Cardinal;
begin
  {$IFDEF RICHVIEWCBDEF3}
  if (GetRVStyle<>nil) and (StyleNo>=0) then
    Result := RVMAKELCID(RVU_Charset2Language(GetRVStyle.TextStyles[StyleNo].Charset))
  else
  {$ENDIF}
    Result := RVMAKELCID(LANG_NEUTRAL);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetDefaultCodePage: TRVCodePage;
begin
  {$IFNDEF RVDONOTUSEUNICODE}
  if (GetRVStyle<>nil) then
    Result := GetRVStyle.DefCodePage
  else
  {$ENDIF}
    Result := CP_ACP;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetDoInPaletteMode: TRVPaletteAction;
begin
  Result := GetRootData.GetDoInPaletteMode;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetOptions: TRVOptions;
begin
  Result := GetRootData.GetOptions;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SetOptions(const Value: TRVOptions);
begin
  GetRootData.SetOptions(Value);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetDocProperties: TStringList;
begin
  Result := nil;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetRVLogPalette: PLogPalette;
begin
  Result := GetRootData.GetRVLogPalette;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetRVPalette: HPALETTE;
begin
  Result := GetRootData.GetRVPalette;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetDelimiters: String;
begin
  Result := GetRootData.GetDelimiters;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetRVFParaStylesReadMode: TRVFReaderStyleMode;
begin
  Result := GetRootData.GetRVFParaStylesReadMode;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetRVFTextStylesReadMode: TRVFReaderStyleMode;
begin
  Result := GetRootData.GetRVFTextStylesReadMode;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.RVFPictureNeeded(const ItemName: String; ItemTag: Integer): TGraphic;
begin
  Result := GetRootData.RVFPictureNeeded(ItemName, ItemTag);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SaveComponentToFile(const Path: String;
  SaveMe: TComponent; SaveFormat: TRVSaveFormat): String;
begin
  Result := GetAbsoluteRootData.SaveComponentToFile(Path, SaveMe, SaveFormat);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SaveItemToFile(const Path: String;
  RVData: TCustomRVData; ItemNo: Integer; SaveFormat: TRVSaveFormat;
  Unicode: Boolean; var Text: String): Boolean;
begin
  Result := GetAbsoluteRootData.SaveItemToFile(Path, RVData, ItemNo, SaveFormat,
    Unicode, Text);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.ImportPicture(const Location: String; Width,
  Height: Integer): TGraphic;
begin
  Result := GetAbsoluteRootData.ImportPicture(Location, Width, Height);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetItemHint(RVData: TCustomRVData; ItemNo: Integer): String;
begin
  Result := GetAbsoluteRootData.GetItemHint(RVData, ItemNo);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.RVFControlNeeded(const ItemName: String; ItemTag: Integer): TControl;
begin
  Result := GetRootData.RVFControlNeeded(ItemName, ItemTag);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.RVFImageListNeeded(ImageListTag: Integer): TCustomImageList;
begin
  Result := GetRootData.RVFImageListNeeded(ImageListTag);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.HTMLSaveImage(RVData: TCustomRVData;
  ItemNo: Integer; const Path: String; BackgroundColor: TColor;
  var Location: String; var DoDefault: Boolean);
begin
  GetAbsoluteRootData.HTMLSaveImage(RVData, ItemNo, Path, BackgroundColor,
    Location, DoDefault);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SaveImage2(Graphic: TGraphic;
  SaveFormat: TRVSaveFormat; const Path, ImagePrefix: String;
  var ImageSaveNo: Integer; var Location: String; var DoDefault: Boolean);
begin
  GetAbsoluteRootData.SaveImage2(Graphic, SaveFormat, Path, ImagePrefix,
    ImageSaveNo, Location, DoDefault);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetRVStyle: TRVStyle;
begin
  Result := GetParentData.GetRVStyle;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetParentControl: TWinControl;
begin
  Result := GetRootData.GetParentControl;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.ReadHyperlink(const Target, Extras: String;
  DocFormat: TRVLoadFormat; var StyleNo, ItemTag: Integer;
  var ItemName: String);
begin
  GetRootData.ReadHyperlink(Target, Extras, DocFormat, StyleNo, ItemTag, ItemName);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.WriteHyperlink(id: Integer; RVData: TCustomRVData;
  ItemNo: Integer; SaveFormat: TRVSaveFormat; var Target, Extras: String);
begin
  GetAbsoluteRootData.WriteHyperlink(id, RVData, ItemNo, SaveFormat,
    Target, Extras);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.ControlAction(ControlAction: TRVControlAction; ItemNo: Integer;
                            Item: TCustomRVItemInfo);
begin
  if (item is TRVControlItemInfo) then
     ControlAction2(ControlAction, ItemNo, TRVControlItemInfo(item).Control);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.ItemAction(ItemAction: TRVItemAction;
  Item: TCustomRVItemInfo; var Text: String; RVData: TCustomRVData);
begin

end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.Replace0(var s: String);
var p: Integer;
begin
  while True do begin
    p := Pos(#0,s);
    if p=0 then break;
    s[p] := RVDEFAULTCHARACTER;
  end;
end;
{------------------------------- RTF ------------------------------------------}
procedure TCustomRVData.SetRTFOptions(const Value: TRVRTFOptions);
begin
  GetRootData.SetRTFOptions(Value);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetRTFOptions: TRVRTFOptions;
begin
  Result := GetRootData.GetRTFOptions;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.MakeRTFTables(ColorList: TRVColorList;
  ListOverrideCountList: TRVIntegerList; TopLevel: Boolean);
   const ArrDefColorTable: array [0..16] of TColor =
         (
         clWindowText, clBlack, clBlue, clAqua,
         clLime, clFuchsia, clRed, clYellow,
         clWhite, clNavy, clTeal, clGreen,
         clPurple, clMaroon, clOlive,
         clGray, clSilver
         );
   var i{$IFNDEF RVDONOTUSELISTS},j{$ENDIF}: Integer;
       RVStyle: TRVStyle;
begin
   RVStyle := GetRVStyle;
   if TopLevel then begin
     ColorList.Clear;
     ListOverrideCountList.Clear;
     for i := Low(ArrDefColorTable) to High(ArrDefColorTable) do
       ColorList.Add(ArrDefColorTable[i]);
     for i := 0 to RVStyle.TextStyles.Count-1 do
       with RVStyle.TextStyles[i] do begin
         ColorList.AddUnique(Color);
         ColorList.AddUnique(BackColor);
         ColorList.AddUnique(HoverColor);
         ColorList.AddUnique(HoverBackColor);
       end;
     for i := 0 to RVStyle.ParaStyles.Count-1 do
       with RVStyle.ParaStyles[i] do begin
         if (Border.Style<>rvbNone) then
           ColorList.AddUnique(Border.Color);
         ColorList.AddUnique(Background.Color);
       end;
     {$IFNDEF RVDONOTUSELISTS}
     for i := 0 to RVStyle.ListStyles.Count-1 do begin
       ListOverrideCountList.Add(1);
       for j := 0 to RVStyle.ListStyles[i].Levels.Count-1 do
         with RVStyle.ListStyles[i].Levels[j] do
           if UsesFont then
             ColorList.AddUnique(Font.Color);
     end;
     {$ENDIF}
   end;
   for i := 0 to Items.Count-1 do
     with GetItem(i) do
       if StyleNo<0 then
         FillRTFTables(ColorList, ListOverrideCountList, Self);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERTF}
function TCustomRVData.SaveRTF(const FileName: String; SelectionOnly: Boolean;
                       Color: TColor;
                       Background: TRVBackground):Boolean;
var Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(FileName,fmCreate);
    try
      Result := SaveRTFToStream(Stream, SelectionOnly, 0, Color, Background,
                                nil, nil, nil, nil, nil, 0.0);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;
{------------------------------------------------------------------------------}
procedure RVSaveFontToRTF(Stream: TStream; Font: TFont;
                          ColorList: TRVColorList; FontTable: TRVRTFFontTable;
                          RVStyle: TRVStyle);
var idx: Integer;
    {$IFDEF RICHVIEWCBDEF3}
    Language: Cardinal;
    {$ENDIF}
begin
  idx := FontTable.Find(Font.Name {$IFDEF RICHVIEWCBDEF3}, Font.Charset{$ENDIF});
  if idx>=0 then
    RVFWrite(Stream, Format('\f%d', [idx]));
  if fsBold in Font.Style then
    RVFWrite(Stream, '\b');
  if fsItalic in Font.Style then
    RVFWrite(Stream, '\i');
  if fsUnderline in Font.Style then
    RVFWrite(Stream, '\ul');
  if fsStrikeOut in Font.Style then
    RVFWrite(Stream, '\strike');
  RVFWrite(Stream, Format('\fs%d', [Font.Size*2]));
  {$IFDEF RICHVIEWCBDEF3}
  if (Font.Charset<>DEFAULT_CHARSET) and (Font.Charset<>RVStyle.TextStyles[0].Charset) then begin
    Language := RVU_Charset2Language(Font.Charset);
    RVFWrite(Stream, Format('\lang%d', [Language]));
  end;
  {$ENDIF}
  if Font.Color<>clWindowText then
    RVFWrite(Stream, Format('\cf%d', [ColorList.IndexOf(Pointer(Font.Color))]));
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSELISTS}
procedure TCustomRVData.SaveRTFListTable97(Stream: TStream; ColorList: TRVColorList;
  ListOverrideOffsetsList: TRVIntegerList;
  FontTable: TRVRTFFontTable; tpp: Double);
var IDList, TemplateIDList: TRVIntegerList;
    i,j, id, levelcount, idx: Integer;
    s1,s2: String;
    RVStyle: TRVStyle;
    LevelInfo: TRVListLevel;
    {...................................................}
    function Getlevelnfc(LevelInfo: TRVListLevel): Integer;
    begin
      case LevelInfo.ListType of
        rvlstBullet,
        {$IFNDEF RVDONOTUSEUNICODE}
        rvlstUnicodeBullet,
        {$ENDIF}
        rvlstPicture, rvlstImageList:
          Result := 23;
        rvlstDecimal,rvlstImageListCounter:
          Result := 0;
        rvlstLowerAlpha:
          Result := 4;
        rvlstUpperAlpha:
          Result := 3;
        rvlstLowerRoman:
          Result := 2;
        rvlstUpperRoman:
          Result := 1;
        else
          Result := 255;
      end;
    end;
    {...................................................}
    function Getleveljc(LevelInfo: TRVListLevel): Integer;
    begin
      case LevelInfo.MarkerAlignment of
        rvmaLeft:
          Result := 0;
        rvmaCenter:
          Result := 1;
        rvmaRight:
          Result := 2;
        else
          Result := -1;
      end;
    end;
    {...................................................}
    procedure Getlevetext(LevelInfo: TRVListLevel; var LevelText, LevelNumbers: String);
    var
        {$IFNDEF RVDONOTUSEUNICODE}
        {$IFDEF RICHVIEWCBDEF3}
        Stream: TMemoryStream;
        {$ENDIF}
        {$ENDIF}
        s: String;
        i: Integer;
    begin
      case LevelInfo.ListType of
        rvlstBullet:
          begin
            LevelText := RVMakeRTFStr(LevelInfo.FormatString, False);
            LevelText := Format('\''%.2x%s',[Length(LevelInfo.FormatString),LevelText]);
            LevelNumbers := '';
          end;
        {$IFNDEF RVDONOTUSEUNICODE}
        {$IFDEF RICHVIEWCBDEF3}        
        rvlstUnicodeBullet:
          begin
            SetLength(s, Length(LevelInfo.FormatStringW)*2);
            if Length(s)>0 then
              Move(Pointer(LevelInfo.FormatStringW)^, Pointer(s)^, Length(s));
            Stream := TMemoryStream.Create;
            RVWriteUnicodeRTFStr(Stream, s, RVStyle.DefCodePage, rvrtfDuplicateUnicode in RTFOptions, False);
            SetLength(LevelText, Stream.Size);
            Stream.Position := 0;
            Stream.ReadBuffer(Pointer(LevelText)^, Stream.Size);
            Stream.Free;
            LevelText := Format('\''%.2x%s',[Length(LevelInfo.FormatStringW),LevelText]);
            LevelNumbers := '';
          end;
        {$ENDIF}
        {$ENDIF}
        rvlstDecimal,rvlstImageListCounter,
        rvlstLowerAlpha,rvlstUpperAlpha,
        rvlstLowerRoman,rvlstUpperRoman:
          begin
            LevelText := Format(LevelInfo.FormatString, ['\''00','\''01','\''02','\''03','\''04','\''05','\''06','\''07','\''08']);
            s := Format(LevelInfo.FormatString, [#0, #1, #2, #3, #4, #5, #6, #7, #8]);
            LevelNumbers := '';
            for i := 1 to Length(s) do
              if s[i]<#9 then
                LevelNumbers := Format('%s\''%.2x',[LevelNumbers,i]);
            LevelText := Format('\''%.2x%s',[Length(s),LevelText]);
          end;
        else
          begin
            LevelText := '\''00';
            LevelNumbers := '';
          end;
      end;
    end;
    {...................................................}
    procedure SaveListOverrideTable;
    var i,j,k,prevcount,curcount,index: Integer;
        Markers: TRVMarkerList;
        Marker: TRVMarkerItemInfo;
    begin
      Markers := GetMarkers(False);
      //if Markers=nil then
      //  exit;
      RVFWriteLine(Stream, '{\*\listoverridetable');
      index := 1;
      for i := 0 to IDList.Count-1 do begin
        RVFWriteLine(Stream,
          Format('{\listoverride\listid%d\listoverridecount0\ls%d}', [IDList[i],index]));
        inc(index);
        if (Markers<>nil) and (ListOverrideOffsetsList[i]>1) then begin
          for j := 0 to Markers.Count-1 do begin
            Marker := Markers[j];
            if (Marker.ListNo=i) and (Marker.Level>=0) and Marker.Reset then begin
              RVFWrite(Stream,
                Format('{\listoverride\listid%d\listoverridecount%d',
                  [IDList[i],Marker.Level+1]));
              for k := 0 to Marker.Level-1 do
                RVFWrite(Stream, '{\lfolevel}');
              RVFWrite(Stream,
                Format('{\lfolevel\listoverridestartat\levelstartat%d}', [Marker.StartFrom]));
              RVFWriteLine(Stream, Format('\ls%d}', [index]));
              inc(index);
            end;
          end;
        end;
      end;
      RVFWriteLine(Stream, '}');
      // transforming a list of counts to a list of offsets
      if RVStyle.ListStyles.Count>0 then begin
        prevcount := ListOverrideOffsetsList[0];
        ListOverrideOffsetsList[0] := 1; // starting from 1
        for i := 1 to RVStyle.ListStyles.Count-1 do begin
          curcount := ListOverrideOffsetsList[i];
          ListOverrideOffsetsList[i] := ListOverrideOffsetsList[i-1]+prevcount;
          prevcount := curcount;
        end;
      end;
    end;
    {...................................................}
//var listsarenotused: Boolean;
begin
  RVStyle := GetRVStyle;
  {
  listsarenotused := True;
  for i := 0 to ListOverrideOffsetsList.Count-1 do
    if ListOverrideOffsetsList[i]>0 then begin
      listsarenotused := False;
      break;
    end;
  }
  if (RVStyle.ListStyles.Count=0) {or listsarenotused} then begin
    RVFWriteLine(Stream, '');
    exit;
  end;

  IDList := TRVIntegerList.Create;
  TemplateIDList := TRVIntegerList.Create;
  try
    // writing list table
    RVFWrite(Stream, '{\*\listtable');
    for i := 0 to RVStyle.ListStyles.Count-1 do begin
      // if ListOverrideOffsetsList[i]>1 then begin
        // writing list
        repeat
          id := Random(MaxInt);
        until IDList.IndexOf(Pointer(id))<0;
        TemplateIDList.Add(id);
        RVFWrite(Stream, Format('{\list\listtemplateid%d',[id]));
        if RVStyle.ListStyles[i].Levels.Count=1 then
          RVFWrite(Stream, '\listsimple1');
        RVFWriteLine(Stream, '');
        levelcount := RVStyle.ListStyles[i].Levels.Count;
        if levelcount>1 then
          levelcount := 9;
        for j := 0 to levelcount-1 do begin
          // writing list level
          if j<RVStyle.ListStyles[i].Levels.Count then
            idx := j
          else
            idx := RVStyle.ListStyles[i].Levels.Count-1;
          LevelInfo := RVStyle.ListStyles[i].Levels[idx];
          RVFWrite(Stream, Format('{\listlevel\levelnfc%d\leveljc%d\li%d\fi%d\jclisttab\tx%d',
            [Getlevelnfc(LevelInfo), Getleveljc(LevelInfo),
             Round(LevelInfo.LeftIndent*tpp),
             Round((LevelInfo.MarkerIndent-LevelInfo.LeftIndent)*tpp),
             Round((LevelInfo.FirstIndent+LevelInfo.LeftIndent)*tpp)]));
          if Getlevelnfc(LevelInfo)<>23 then
            RVFWrite(Stream, Format('\levelstartat%d', [LevelInfo.StartFrom]));
          if rvloLegalStyleNumbering in LevelInfo.Options then
            RVFWrite(Stream, '\levellegal1');
          if not (rvloLevelReset in LevelInfo.Options) then
            RVFWrite(Stream, '\levelnorestart1');
          Getlevetext(LevelInfo, s1, s2);
          RVFWrite(Stream, Format('{\leveltext%s;}{\levelnumbers%s;}', [s1,s2]));
          if LevelInfo.UsesFont then
            RVSaveFontToRTF(Stream, LevelInfo.Font, ColorList, FontTable, RVStyle);
          RVFWriteLine(Stream, '}');
        end;
        // writing list table (continued)
        repeat
          id := Random(MaxInt);
        until TemplateIDList.IndexOf(Pointer(id))<0;
        IDList.Add(id);
        RVFWriteLine(Stream, Format('\listid%d}',[id]));
      {
        end
      else begin
        IDList.Add(-1);
        TemplateIDList.Add(-1);
      end;
      }
    end;
    RVFWriteLine(Stream, '}');
    SaveListOverrideTable;
  finally
    IDList.Free;
    TemplateIDList.Free;
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TCustomRVData.ShouldSaveTextToRTF(StyleNo: Integer): Boolean;
begin
  with GetRVStyle.TextStyles[StyleNo] do
    Result := (rvteoRTFCode in Options) or not (rvteoHTMLCode in Options)
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SaveRTFToStream(Stream: TStream; SelectionOnly: Boolean;
  Level: Integer; Color: TColor; Background: TRVBackground; ColorList: TRVColorList;
  StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
  FontTable: TRVRTFFontTable; tpp: Double): Boolean;
var RVStyle: TRVStyle;
    function GetTwipsPerPixel: Double;
    var DC: HDC;
    begin
      DC := CreateCompatibleDC(0);
      if RichViewPixelsPerInch>0 then
        Result := (72*20) / RichViewPixelsPerInch
      else
        Result := (72*20) / GetDeviceCaps(DC, LOGPIXELSY);
      DeleteDC(DC);
    end;
   {.................................................}
   procedure MakeFontTable(FontTable: TRVRTFFontTable;StyleToFont: TRVIntegerList);
   var i {$IFNDEF RVDONOTUSELISTS},j{$ENDIF}: Integer;
       Index: Integer;
   begin
     FontTable.Clear;
     StyleToFont.Clear;
     for i := 0 to RVStyle.TextStyles.Count-1 do begin
       Index := FontTable.AddUnique(RVStyle.TextStyles[i].FontName
         {$IFDEF RICHVIEWCBDEF3}
         , RVStyle.TextStyles[i].Charset
         {$ENDIF});
       StyleToFont.Add(Index);
     end;
     {$IFNDEF RVDONOTUSELISTS}
     for i := 0 to RVStyle.ListStyles.Count-1 do
       for j := 0 to RVStyle.ListStyles[i].Levels.Count-1 do
         if RVStyle.ListStyles[i].Levels[j].UsesFont then
           with RVStyle.ListStyles[i].Levels[j].Font do
             FontTable.AddUnique(Name {$IFDEF RICHVIEWCBDEF3}, Charset{$ENDIF});
     {$ENDIF}
   end;
   {.................................................}
   procedure SaveFontTable(FontTable: TRVRTFFontTable);
   var i: Integer;
       Charset: Integer;
   begin
     RVFWrite(Stream, '{\fonttbl');
     for i := 0 to FontTable.Count-1 do begin
       {$IFDEF RICHVIEWCBDEF3}
       Charset := FontTable[i].Charset;
       {$ELSE}
       Charset := 1;
       {$ENDIF}
       RVFWrite(Stream, Format('{\f%d\fnil\fcharset%d %s;}',[i, Charset, FontTable[i].FontName]));
     end;
     RVFWrite(Stream, '}');
   end;
   {.................................................}
   procedure SaveColorTable(List: TList);
   var i: Integer;
       Color: Integer;
   begin
     RVFWrite(Stream, '{\colortbl;');
     for i := 1 to List.Count-1 do begin
       Color := ColorToRGB(Integer(List.Items[i]));
       RVFWrite(Stream, Format('\red%d\green%d\blue%d;',
                [
                  Color and $0000FF,
                  (Color and $00FF00) shr 8,
                  (Color and $FF0000) shr 16
                ]));
     end;
     RVFWrite(Stream, '}');
   end;
   {.................................................}
   procedure SaveTextStyle(StyleNo: Integer; StyleToFont, ColorTable: TList;
     ToStyleSheet: Boolean);
   var idx, fsscale: Integer;
       {$IFDEF RICHVIEWCBDEF3}
       ALanguage: Cardinal;
       {$ENDIF}
   begin
     if StyleNo>=RVStyle.TextStyles.Count then
       StyleNo := 0;
     with RVStyle.TextStyles[StyleNo] do begin
       RVFWrite(Stream, Format('\f%d', [Integer(StyleToFont.Items[StyleNo])]));
       if fsBold in Style then
         RVFWrite(Stream, '\b');
       if fsItalic in Style then
         RVFWrite(Stream, '\i');
       if fsUnderline in Style then
         RVFWrite(Stream, '\ul');
       if fsStrikeOut in Style then
         RVFWrite(Stream, '\strike');
       // RTF-viewer will also shrink font size for s/s scripts...
       fsscale := 1;
       if VShift<>0 then
         fsscale := 2;
       if VShift<0 then
         RVFWrite(Stream, '\sub');
       if VShift>0 then
         RVFWrite(Stream, '\super');
       if CharScale<>100 then
         RVFWrite(Stream, Format('\charscalex%d',[CharScale]));
       if CharSpacing<>0 then
         RVFWrite(Stream, Format('\expndtw%d',[Round(CharSpacing*tpp)]));
       if rvfsAllCaps in StyleEx then
         RVFWrite(Stream, '\caps');
       case BiDiMode of
         rvbdLeftToRight:
           RVFWrite(Stream, '\ltrch');
         rvbdRightToLeft:
           RVFWrite(Stream, '\rtlch');
       end;
       {$IFDEF RICHVIEWCBDEF3}
       if (Charset<>DEFAULT_CHARSET) and (Charset<>RVStyle.TextStyles[0].Charset) then begin
         {$IFDEF RVLANGUAGEPROPERTY}
         ALanguage := Language;
         {$ELSE}
         ALanguage := RVU_Charset2Language(Charset);
         {$ENDIF}
         if ALanguage<>0 then
           RVFWrite(Stream, Format('\lang%d', [ALanguage]));
       end;
       {$ENDIF}
       RVFWrite(Stream, Format('\fs%d', [Size*2*fsscale]));
       if BackColor<>clNone then begin
         idx := ColorTable.IndexOf(Pointer(BackColor));
         //RVFWrite(Stream, Format('\chshdng0\chcfpat0\chcbpat%d', [idx]));
         RVFWrite(Stream, Format('\chcbpat%d', [idx]));
       end;
       if Color<>clWindowText then begin
         idx := ColorTable.IndexOf(Pointer(Color));
         RVFWrite(Stream, Format('\cf%d', [idx]));
       end;
       {$IFDEF RVTEXTFOOTNOTES}
       if (rvfsFootnotes in StyleEx) and not ToStyleSheet then
         RVFWrite(Stream, ' {\chftn {\footnote { \chftn } { ' + FootNote + ' }}}');
       {$ENDIF}
       RVFWrite(Stream, GetExtraRTFCode(rv_rtfs_TextStyle, RVStyle.TextStyles[StyleNo], StyleNo, -1, ToStyleSheet));
       RVFWrite(Stream, ' ');
     end;
   end;
   {.................................................}
   procedure SaveParaStyle(ParaNo: Integer; ColorTable: TList; ToStyleSheet: Boolean;
                           item: TCustomRVItemInfo);
   var s,s2,s3,s4: String;
       bw: Integer;
   begin
     with RVStyle.ParaStyles[ParaNo] do begin
       case Alignment of
         rvaLeft:
           s := 'l';
         rvaRight:
           s := 'r';
         rvaCenter:
           s := 'c';
         rvaJustify:
           s := 'j';
       end;
       if (LineSpacingType=rvlsPercent) and (LineSpacing>100) then
         RVFWrite(Stream, Format('\sl%d\slmult1', [LineSpacing*240 div 100]));
       if rvpaoKeepLinesTogether in Options then
         RVFWrite(Stream, '\keep');
       if rvpaoKeepWithNext in Options then
         RVFWrite(Stream, '\keepn');

       {$IFNDEF RVDONOTUSELISTS}
       if (item<>nil) and (item.StyleNo = rvsListMarker) and
          (TRVMarkerItemInfo(item).GetLevelInfo(RVStyle)<>nil) then
         with TRVMarkerItemInfo(item).GetLevelInfo(RVStyle) do
           RVFWrite(Stream, Format('\li%d\fi%d\jclisttab\tx%d',
            [Round(LeftIndent*tpp),
             Round((MarkerIndent-LeftIndent)*tpp),
             Round((FirstIndent+LeftIndent)*tpp)]))
       else
       {$ENDIF}
         RVFWrite(Stream, Format('\fi%d\li%d', [Round(FirstIndent*tpp), Round(LeftIndent*tpp)]));

       RVFWrite(Stream, Format('\q%s\ri%d\sb%d\sa%d',
                        [s, Round(RightIndent*tpp),
                         Round(SpaceBefore*tpp), Round(SpaceAfter*tpp)]));
       case BiDiMode of
         rvbdLeftToRight:
           RVFWrite(Stream, '\ltrpar');
         rvbdRightToLeft:
           RVFWrite(Stream, '\rtlpar');
       end;
       if Background.Color<>clNone then
         RVFWrite(Stream, Format('\cbpat%d', [ColorTable.IndexOf(Pointer(Background.Color))]));
       if (Border.Style<>rvbNone) and (Border.Color<>clNone) then begin
         RVFWrite(Stream, '\brdrbtw'); // <- does not work, unfortunately
         s2 := '\brdr';
         bw := Border.Width;
         case Border.Style of
           rvbSingle:
             s2 := s2+'s';
           rvbDouble:
             s2 := s2+'db';
           rvbTriple:
             s2 := s2+'triple';
           rvbThickInside:
             begin
               s2 := s2+'thtnmg';
               bw := bw*2;
             end;
           rvbThickOutside:
             begin
               s2 := s2+'tnthmg';
               bw := bw*2;
             end;
         end;
         case Border.Style of
           rvbThickInside:
             s3 := '\brdrtnthmg';
           rvbThickOutside:
             s3 := '\brdrthtnmg';
           else
             s3 := s2;
         end;
         s4 := Format('\brdrcf%d\brdrw%d',
               [ColorTable.IndexOf(Pointer(Border.Color)),
                Round(bw*tpp)
               ]);
         s2 := s2 + s4;
         s3 := s3 + s4;
         s := '';
         with Border.VisibleBorders do begin
           if Left   then s := s+Format('\brdrl\brsp%d',[Round(Border.BorderOffsets.Left*tpp)])+s2;
           if Top    then s := s+Format('\brdrt\brsp%d',[Round(Border.BorderOffsets.Top*tpp)])+s2;
           if Right  then s := s+Format('\brdrr\brsp%d',[Round(Border.BorderOffsets.Right*tpp)])+s3;
           if Bottom then s := s+Format('\brdrb\brsp%d',[Round(Border.BorderOffsets.Bottom*tpp)])+s3;
         end;
         RVFWrite(Stream, s);
       end;
       if not ToStyleSheet then
         RVFWrite(Stream, Format('\itap%d',[Level]));
       if Level=1 then
         RVFWrite(Stream, '\intbl');
         RVFWrite(Stream, GetExtraRTFCode(rv_rtfs_ParaStyle, RVStyle.ParaStyles[ParaNo], ParaNo, -1, ToStyleSheet));
       RVFWrite(Stream, ' ');
     end;
   end;
   {.................................................}
   procedure SaveStyleSheet(StyleToFont, ColorTable: TList);
   var i: Integer;
   begin
     RVFWrite(Stream, '{\stylesheet');
     for i := 0 to RVStyle.ParaStyles.Count-1 do begin
       RVFWrite(Stream, '{');
       RVFWrite(Stream, Format('\s%d',[i]));
       SaveParaStyle(i, ColorTable,True,nil);
       RVFWrite(Stream, MakeRTFIdentifierStr(RVStyle.ParaStyles[i].StyleName));
       RVFWrite(Stream, ';}');
     end;
     for i := 0 to RVStyle.TextStyles.Count-1 do begin
       RVFWrite(Stream, Format('{\*\cs%d',[i+RVStyle.ParaStyles.Count]));
       SaveTextStyle(i, StyleToFont, ColorTable,True);
       RVFWrite(Stream, MakeRTFIdentifierStr(RVStyle.TextStyles[i].StyleName)+';}');
     end;
     RVFWrite(Stream, '}');
   end;
   {.................................................}
   procedure SaveHeader(ColorList: TRVColorList; StyleToFont: TRVIntegerList;
                        FontTable: TRVRTFFontTable);
   var CodePage: TRVCodePage;
       Language: Cardinal;
   begin
     {$IFNDEF RVDONOTUSEUNICODE}
     {$IFDEF RICHVIEWCBDEF3}
     CodePage := GetRVStyle.DefCodePage;
     {$IFDEF RVLANGUAGEPROPERTY}
     Language := GetRVStyle.TextStyles[0].Language;
     if Language=0 then
       Language := $0400;
     {$ELSE}
     Language := RVU_Charset2Language(GetRVStyle.TextStyles[0].CharSet);
     {$ENDIF}
     {$ELSE}
     CodePage := 1252;
     Language := $0400;
     {$ENDIF}
     {$ELSE}
     CodePage := 1252;
     Language := $0400;
     {$ENDIF}
     RVFWrite(Stream, Format('{\rtf1\ansi\ansicpg%d\uc1\deff0\deflang%d\deflangfe%d',
       [CodePage, Language, Language]));
     case GetBiDiMode of
       rvbdLeftToRight:
         RVFWrite(Stream, '\ltrdoc');
       rvbdRightToLeft:
         RVFWrite(Stream, '\rtldoc');
     end;
     SaveFontTable(FontTable);
     SaveColorTable(ColorList);
     if rvrtfSaveStyleSheet in RTFOptions then
       SaveStyleSheet(StyleToFont, ColorList);
     {$IFNDEF RVDONOTUSELISTS}
     SaveRTFListTable97(Stream, ColorList, ListOverrideOffsetsList1, FontTable,tpp);
     ListOverrideOffsetsList2.Assign(ListOverrideOffsetsList1);
     {$ELSE}
     RVFWriteLine(Stream, '');
     {$ENDIF}
     RVFWriteLine(Stream, GetExtraRTFCode(rv_rtfs_Doc, nil, -1, -1, False));     
   end;
   {.................................................}
var i, CPIndex: Integer;
    item: TCustomRVItemInfo;
    s: String;
    StartItem,EndItem,StartOffs,EndOffs
    {$IFNDEF RVDONOTUSELISTS}
    ,MarkerItemNo
    {$ENDIF}
    : Integer;
    UrlTarget, UrlExtras: String;
    NotUsedPart: TRVMultiDrawItemPart;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if SelectionOnly and (GetChosenRVData<>nil) then begin
    Result := GetChosenRVData.SaveRTFToStream(Stream, SelectionOnly, Level,
      Color, Background, ColorList, StyleToFont,
      ListOverrideOffsetsList1, ListOverrideOffsetsList2,
      FontTable, tpp);
    exit;
  end;
  {$ENDIF}
  Result := True;
  RVFGetLimits(GetRVFSaveScope(SelectionOnly),StartItem,EndItem,StartOffs,EndOffs,NotUsedPart,NotUsedPart);
  if (StartItem=-1) or (StartItem>EndItem) then exit;
  if Level=0 then begin
    ColorList   := TRVColorList.Create;
    StyleToFont := TRVIntegerList.Create;
    FontTable := TRVRTFFontTable.Create;
    ListOverrideOffsetsList1 := TRVIntegerList.Create;
    ListOverrideOffsetsList2 := TRVIntegerList.Create;    
  end;
  RVStyle := GetRVStyle;
  CPIndex := 0;
  try
    Include(State, rvstRTFSkipPar);
    if Level=0 then begin
      tpp := GetTwipsPerPixel;
      MakeFontTable(FontTable, StyleToFont);
      MakeRTFTables(ColorList, ListOverrideOffsetsList1, True);
      if (Color<>clWindow) then
        ColorList.AddUnique(Color);
      SaveHeader(ColorList, StyleToFont, FontTable);
      if rvrtfDuplicateUnicode in RTFOptions then
        RVFWrite(Stream,'\uc1')
      else
        RVFWrite(Stream,'\uc0');
      {$IFNDEF RVDONOTUSELISTS}
      if SelectionOnly then begin
        MarkerItemNo := GetFirstParaItem(StartItem);
        if (MarkerItemNo<>StartItem) and (GetItemStyle(MarkerItemNo)=rvsListMarker) then begin
          GetItem(MarkerItemNo).SaveRTF(Stream, Self, MarkerItemNo,
            Items[MarkerItemNo], tpp, Level, ColorList, StyleToFont,
            ListOverrideOffsetsList1, ListOverrideOffsetsList2, FontTable);
          Exclude(State, rvstRTFSkipPar);
        end;
      end;
      {$ENDIF}
    end;
    for i := StartItem to EndItem do begin
      if not ((StartItem=EndItem) and (GetItemStyle(StartItem)>=0)) then begin
        if (i=StartItem) and (StartOffs>=GetOffsAfterItem(i)) and (Items[i]<>'') then
          continue
        else if (i=EndItem) and (EndOffs<=GetOffsBeforeItem(i)) and (Items[i]<>'') then
          continue;
      end;
      item := GetItem(i);
      if not item.SameAsPrev then begin
        RVFWriteLine(Stream,'');
        if item.GetBoolValue(rvbpFullWidth) and PageBreaksBeforeItems[i]  then
          RVFWrite(Stream,'\page');
        if item.BR then
          RVFWrite(Stream,'\line')
        else begin
          if not (rvstRTFSkipPar in State) then begin
            {$IFNDEF RVDONOTUSELISTS}
            if (i>0) and (GetItemStyle(GetFirstParaItem(i-1))=rvsListMarker) then
              RVFWrite(Stream, '\plain');
            {$ENDIF}
            RVFWrite(Stream, '\par');
          end;
          Exclude(State, rvstRTFSkipPar);
          if not item.GetBoolValue(rvbpFullWidth) and PageBreaksBeforeItems[i]  then
            RVFWrite(Stream,'\page');
          RVFWrite(Stream, '\pard');
          if rvrtfSaveStyleSheet in RTFOptions then
            RVFWrite(Stream, Format('\s%d', [item.ParaNo]));
          SaveParaStyle(item.ParaNo, ColorList,False,item);
        end;
      end;
      if item.Checkpoint<>nil then begin
        // I decided to use names of checkpoints here (if assigned).
        // If several checkpoints have the same name, only one of them
        // will be used as a bookmark in MS Word.
        s := MakeRTFBookmarkNameStr(item.Checkpoint.Name);
        if s='' then
          s := 'RichViewCheckpoint'+IntToStr(CPIndex);
        RVFWrite(Stream, Format('{\*\bkmkstart %s}{\*\bkmkend %s}',[s,s]));
        inc(CPIndex);
      end;
      if item.GetBoolValueEx(rvbpJump,RVStyle) then begin
        WriteHyperlink(item.JumpID+FirstJumpNo, Self, i, rvsfRTF,
          UrlTarget, UrlExtras);
        UrlTarget := RVMakeRTFStr(UrlTarget,False);
        UrlExtras := RVMakeRTFStr(UrlExtras,False);
        if (UrlTarget<>'') or (UrlExtras<>'') then begin
          if UrlExtras<>'' then
            UrlExtras := ' '+UrlExtras;
          RVFWrite(Stream,
            Format('{\field{\*\fldinst HYPERLINK "%s"%s}{\fldrslt',[UrlTarget, UrlExtras]));
        end;
        end
      else begin
        UrlTarget := '';      
        UrlExtras := '';
      end;
      if (item.StyleNo>=0) and ShouldSaveTextToRTF(GetActualStyle(item)) then begin
        if (i=StartItem) then
          if (i=EndItem) then
            s := RVU_Copy(Items[i], StartOffs, EndOffs-StartOffs, item.ItemOptions)
          else
            s := RVU_Copy(Items[i], StartOffs, RVU_Length(Items[i],item.ItemOptions)-StartOffs+1, item.ItemOptions)
        else
          if i=EndItem then
            s := RVU_Copy(Items[i], 1, EndOffs-1, item.ItemOptions)
          else
            s := Items[i];
        RVFWrite(Stream, '\plain ');
        if rvrtfSaveStyleSheet in RTFOptions then
          RVFWrite(Stream,Format('\s%d', [GetActualStyle(item)+RVStyle.ParaStyles.Count]));
        SaveTextStyle(GetItemStyle(i), StyleToFont, ColorList, False);
        if SaveItemToFile('', Self, i, rvsfRTF, False, s) then
          RVFWrite(Stream, s)
        else begin
          {$IFNDEF RVDONOTUSEUNICODE}
          if rvioUnicode in item.ItemOptions then
            RVWriteUnicodeRTFStr(Stream, s, GetStyleCodePage(GetActualStyle(item)),
              rvrtfDuplicateUnicode in RTFOptions,
              rvteoRTFCode in RVStyle.TextStyles[GetActualStyle(item)].Options)
          else
          {$ENDIF}
            RVFWrite(Stream, RVMakeRTFStr(s,rvteoRTFCode in
              RVStyle.TextStyles[GetActualStyle(item)].Options));
        end;
        end
      else begin
        s := '';
        if SaveItemToFile('', Self, i, rvsfRTF, False, s) then
          RVFWrite(Stream, s)
        else
          item.SaveRTF(Stream, Self, i, Items[i], tpp, Level, ColorList,
            StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2,
            FontTable);
      end;
      if (UrlTarget<>'') or (UrlExtras<>'') then
        RVFWrite(Stream, '}}');
    end;
    if NotAddedCP<>nil then begin
      // I decided to use names of checkpoints here (if assigned).
      // If several checkpoints have the same name, only one of them
      // will be used as a bookmark in MS Word.
      s := MakeRTFBookmarkNameStr(NotAddedCP.Name);
      if s='' then
        s := 'RichViewCheckpoint'+IntToStr(CPIndex);
      RVFWrite(Stream, Format('{\*\bkmkstart %s}{\*\bkmkend %s}',[s,s]));
    end;
    if (Level=0) and (StartItem<>EndItem) and IsParaStart(EndItem) and (GetItemStyle(EndItem)>=0) and (Items[EndItem]='') then
      RVFWrite(Stream, '\par');
    if Level=0 then
      RVFWrite(Stream, '}');
  except
    Result := False;
  end;
  if Level=0 then begin
    ColorList.Free;
    StyleToFont.Free;
    FontTable.Free;
    ListOverrideOffsetsList1.Free;
    ListOverrideOffsetsList2.Free;    
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TCustomRVData.GetParentData: TCustomRVData;
begin
  Result := nil;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetRootData: TCustomRVData;
begin
  Result := Self;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetAbsoluteParentData: TCustomRVData;
begin
  Result := nil;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetAbsoluteRootData: TCustomRVData;
begin
  Result := Self;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.DrainFrom(Victim: TCustomRVData);
var i: Integer;
    item: TCustomRVItemInfo;
begin
  if Victim=nil then exit;
  if (rvoTagsArePChars in Options) <> (rvoTagsArePChars in Victim.Options) then
    raise ERichViewError.Create(errRVTagsTypesMismatch);
  for i := 0 to Victim.Items.Count-1 do begin
    item := Victim.GetItem(i);
    if item.SameAsPrev then
      item.ParaNo := -1;
    AddItem(Victim.Items[i], item);
  end;
  if NotAddedCP<>nil then
    NotAddedCP := Victim.NotAddedCP;
  Victim.Items.Clear;
  Victim.FirstCP := nil;
  Victim.LastCP := nil;
  Victim.NotAddedCP := nil;
  Victim.CPCount := 0;
  Victim.Clear;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetItemNo(Item: TCustomRVItemInfo): Integer;
begin
  Result := Items.IndexOfObject(Item);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.Inserting(RVData: TCustomRVData; Safe: Boolean);
var i: Integer;
    s: String;
begin
  for i := 0 to Items.Count-1 do begin
    s := Items[i];
    GetItem(i).Inserting(RVData, s, Safe);
    Items[i] := s;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.Beep;
begin
  if (GetRVStyle<>nil) and (GetRVStyle.UseSound) then
    MessageBeep(MB_OK);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SetParagraphStyleToAll(ParaNo: Integer);
var i: Integer;
begin
  for i := 0 to Items.Count-1 do
    if GetItemStyle(i)<>rvsBreak then
      GetItem(i).ParaNo := ParaNo;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetRVData: TCustomRVData;
begin
  Result := Self;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetSourceRVData: TCustomRVData;
begin
  Result := Self;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetItem(ItemNo: Integer): TCustomRVItemInfo;
begin
  Result := TCustomRVItemInfo(Items.Objects[ItemNo]);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetRTFProperties: TPersistent;
begin
  Result := GetRootData.GetRTFProperties;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.RV_CanConcateItems(FirstItemNo: Integer;
                                          item1, item2: TCustomRVItemInfo;
                                          IgnorePara: Boolean): Boolean;
var RVStyle: TRVStyle;
begin
  RVStyle := GetRVStyle;
  if (item1=nil) or (item2=nil) or (item1.StyleNo<0) or (item2.StyleNo<0) then begin
    Result := False;
    exit;
  end;
  if ((Items[FirstItemNo]='') or (Items[FirstItemNo+1]='')) and
     (IgnorePara or item2.SameAsPrev) then begin
    Result := True;
    exit;
  end;
  Result := (item1.StyleNo=item2.StyleNo) and
            (IgnorePara or item2.SameAsPrev) and
            {$IFNDEF RVDONOTUSEUNICODE}
            (RVStyle.TextStyles[GetActualStyle(item1)].Unicode=
              RVStyle.TextStyles[GetActualStyle(item2)].Unicode) and
            {$ENDIF}
            {$IFNDEF RVDONOTUSEITEMHINTS}
            (item1.Hint=item2.Hint) and
            {$ENDIF}
            RV_CompareTags(item1.Tag,item2.Tag, rvoTagsArePChars in Options) and
            (item2.Checkpoint=nil) and
            (
            (Length(Items[FirstItemNo])=0) or
            (Length(Items[FirstItemNo+1])=0) or
            ([rvprConcateProtect,rvprModifyProtect]*
              RVStyle.TextStyles[GetActualStyle(item1)].Protection=[])
            )
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.SimpleConcate(FirstItemNo: Integer; item1,
  item2: TCustomRVItemInfo);
begin
  Items[FirstItemNo] := Items[FirstItemNo]+Items[FirstItemNo+1];
  InternalFreeItem(item2,False);
  Items.Delete(FirstItemNo+1);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.MassSimpleConcate(FirstItemNo,
  LastItemNo: Integer);
var i: Integer;
    item1,
    item2: TCustomRVItemInfo;
begin
  if FirstItemNo<0 then
    FirstItemNo := 0;
  if LastItemNo>=Items.Count then
    LastItemNo := Items.Count-1;
  for i := LastItemNo downto FirstItemNo+1 do begin
    item1 := TCustomRVItemInfo(Items.Objects[i-1]);
    item2 := TCustomRVItemInfo(Items.Objects[i]);
    if RV_CanConcateItems(i-1, item1, item2, False) then
      SimpleConcate(i-1, item1, item2);
  end;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERTFIMPORT}
function TCustomRVData.LoadRTF(const FileName: String): TRVRTFErrorCode;
var rp: TRVRTFReaderProperties;
    ItemNo: Integer;
begin
  rp := TRVRTFReaderProperties(GetRTFProperties);
  if rp<>nil then begin
    ItemNo := Items.Count-1;
    Result := rp.ReadFromFile(FileName, Self);
    MassSimpleConcate(ItemNo, Items.Count-1);
    end
  else
    Result := rtf_ec_Assertion;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.LoadRTFFromStream(Stream: TStream): TRVRTFErrorCode;
var rp: TRVRTFReaderProperties;
    ItemNo: Integer;
begin
  rp := TRVRTFReaderProperties(GetRTFProperties);
  if rp<>nil then begin
    ItemNo := Items.Count-1;
    Result := rp.ReadFromStream(Stream, Self);
    MassSimpleConcate(ItemNo, Items.Count-1);
    end
  else
    Result := rtf_ec_Assertion;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TCustomRVData.MarkStylesInUse(UsedTextStyles, UsedParaStyles, UsedListStyles: TRVIntegerList);
var i: Integer;
begin
  for i := 0 to Items.Count-1 do
    GetItem(i).MarkStylesInUse(UsedTextStyles, UsedParaStyles, UsedListStyles);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.UpdateStyles(TextStylesShift, ParaStylesShift, ListStylesShift: TRVIntegerList);
var i: Integer;
begin
  for i := 0 to Items.Count-1 do
    GetItem(i).UpdateStyles(TextStylesShift, ParaStylesShift, ListStylesShift);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.DeleteUnusedStyles(TextStyles, ParaStyles, ListStyles: Boolean);
var UsedTextStyles, UsedParaStyles, UsedListStyles: TRVIntegerList;
    {............................................}
    procedure UpdStyles(Styles: TCustomRVInfos; Used: TRVIntegerList);
    var i,idx,val: Integer;
        IsTextStyle, IsParaStyle: Boolean;
    begin
      IsTextStyle := Styles is TFontInfos;
      IsParaStyle := Styles is TParaInfos;
      idx := 1;
      for i := 0 to Used.Count-1 do
        if Used[i]=0 then
          inc(idx)
        else
          Used[i] := idx;
      for i := Used.Count-1 downto 0 do
        if Used[i]=0 then
          Styles.Items[i].Free;
      for i := 0 to Styles.Count-1 do begin
        val := TCustomRVInfo(Styles.Items[i]).BaseStyleNo;
        if val>=0 then
          if val>=Used.Count then
            TCustomRVInfo(Styles.Items[i]).BaseStyleNo := -1
          else
            TCustomRVInfo(Styles.Items[i]).BaseStyleNo := val-Used[val]+1;
        if IsTextStyle then begin
          val := TFontInfo(Styles.Items[i]).NextStyleNo;
          if val>=0 then
            if val>=Used.Count then
              TFontInfo(Styles.Items[i]).NextStyleNo := -1
            else
              TFontInfo(Styles.Items[i]).NextStyleNo := val-Used[val]+1;
          end
        else if IsParaStyle then begin
          val := TParaInfo(Styles.Items[i]).NextParaNo;
          if val>=0 then
            if val>=Used.Count then
              TParaInfo(Styles.Items[i]).NextParaNo := -1
            else
              TParaInfo(Styles.Items[i]).NextParaNo := val-Used[val]+1;
        end;
      end;
    end;
    {............................................}
    procedure MarkStandardStyles(Styles: TCustomRVInfos; Used: TRVIntegerList);
    var i: Integer;
    begin
      for i := 0 to Used.Count-1 do
        if TCustomRVInfo(Styles.Items[i]).Standard then
          Used[i] := 1;
    end;
    {............................................}
    procedure ExpandStyle(Index, FirstIndex: Integer; Styles: TCustomRVInfos;
      Used, Expanded: TRVIntegerList);
    var Style: TCustomRVInfo;
    begin
      if Expanded[Index]<>0 then
        exit;
      Used[Index] := 1;
      Expanded[Index] := 1;
      Style := TCustomRVInfo(Styles.Items[Index]);
      if Style.BaseStyleNo>=0 then begin
        if Style.BaseStyleNo >= Styles.Count then
          Style.BaseStyleNo := -1
        else
          ExpandStyle(Style.BaseStyleNo, FirstIndex, Styles, Used, Expanded);
      end;
      if (Styles is TFontInfos) and (TFontInfo(Style).NextStyleNo>=0) then begin
        if TFontInfo(Style).NextStyleNo>= Styles.Count then
          TFontInfo(Style).NextStyleNo := -1
        else
          ExpandStyle(TFontInfo(Style).NextStyleNo, FirstIndex, Styles, Used, Expanded)
        end
      else if (Styles is TParaInfos) and (TParaInfo(Style).NextParaNo>=0) then begin
        if TParaInfo(Style).NextParaNo >= Styles.Count then
          TParaInfo(Style).NextParaNo := -1
        else
          ExpandStyle(TParaInfo(Style).NextParaNo, FirstIndex, Styles, Used, Expanded);
      end;
    end;
    {............................................}
    procedure ExpandStyles(Styles: TCustomRVInfos; Used: TRVIntegerList);
    var i: Integer;
        Expanded: TRVIntegerList;
    begin
      Expanded := TRVIntegerList.CreateEx(Used.Count, 0);
      for i := 0 to Used.Count-1 do
        if (Used[i]<>0) then
          ExpandStyle(i, i, Styles, Used, Expanded);
      Expanded.Free;
    end;
    {............................................}
    procedure MarkDefStyles;
    var i: Integer;
        RVStyle: TRVStyle;
    begin
      RVStyle := GetRVStyle;
      for i := 0 to RVStyle.ParaStyles.Count-1 do
        if (UsedParaStyles[i]<>0) and
           (RVStyle.ParaStyles[i].DefStyleNo>=0) then
          UsedTextStyles[RVStyle.ParaStyles[i].DefStyleNo] := 1;
    end;
    {............................................}
begin
  UsedTextStyles := nil;
  UsedParaStyles := nil;
  UsedListStyles := nil;
  try
    UsedTextStyles := TRVIntegerList.CreateEx(GetRVStyle.TextStyles.Count, ord(not TextStyles));
    UsedParaStyles := TRVIntegerList.CreateEx(GetRVStyle.ParaStyles.Count, ord(not ParaStyles));
    UsedListStyles := TRVIntegerList.CreateEx(GetRVStyle.ListStyles.Count, ord(not ListStyles));
    {$IFNDEF RVDONOTUSEUNICODE}
    if TextStyles and (GetRVStyle.DefUnicodeStyle>=0) then
      if GetRVStyle.DefUnicodeStyle>=UsedTextStyles.Count then
        GetRVStyle.DefUnicodeStyle := -1
      else
        UsedTextStyles[GetRVStyle.DefUnicodeStyle] := 1;
    {$ENDIF}
    if TextStyles then
      MarkStandardStyles(GetRVStyle.TextStyles, UsedTextStyles);
    if ParaStyles then
      MarkStandardStyles(GetRVStyle.ParaStyles, UsedParaStyles);
    if ListStyles then
      MarkStandardStyles(GetRVStyle.ListStyles, UsedListStyles);
    MarkStylesInUse(UsedTextStyles, UsedParaStyles, UsedListStyles);

    if ParaStyles then
      ExpandStyles(GetRVStyle.ParaStyles, UsedParaStyles);
    if TextStyles then begin
      MarkDefStyles;
      ExpandStyles(GetRVStyle.TextStyles, UsedTextStyles);
    end;
    if ListStyles then
      ExpandStyles(GetRVStyle.ListStyles, UsedListStyles);

    if TextStyles then
      UpdStyles(GetRVStyle.TextStyles, UsedTextStyles);
    if ParaStyles then
    UpdStyles(GetRVStyle.ParaStyles, UsedParaStyles);
    if ListStyles then
      UpdStyles(GetRVStyle.ListStyles, UsedListStyles);
    {$IFNDEF RVDONOTUSEUNICODE}
    if TextStyles and (GetRVStyle.DefUnicodeStyle>=0) then
      if GetRVStyle.DefUnicodeStyle>=UsedTextStyles.Count then
        GetRVStyle.DefUnicodeStyle := -1
      else
        GetRVStyle.DefUnicodeStyle :=
          GetRVStyle.DefUnicodeStyle-UsedTextStyles[GetRVStyle.DefUnicodeStyle]+1;
    {$ENDIF}
    UpdateStyles(UsedTextStyles, UsedParaStyles, UsedListStyles);
    AfterDeleteStyles(UsedTextStyles, UsedParaStyles, UsedListStyles);
  finally
    UsedTextStyles.Free;
    UsedParaStyles.Free;
    UsedListStyles.Free;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AfterDeleteStyles(TextStylesShift,
  ParaStylesShift, ListStylesShift: TRVIntegerList);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.InitStyleMappings(var PTextStylesMapping,
  PParaStylesMapping, PListStylesMapping: PRVIntegerList);
begin
  GetRootData.InitStyleMappings(PTextStylesMapping, PParaStylesMapping, PListStylesMapping);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.DoneStyleMappings(PTextStylesMapping,
  PParaStylesMapping, PListStylesMapping: PRVIntegerList);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
function TCustomRVData.SupportsPageBreaks: Boolean;
begin
  Result := True;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AdjustInItemsRange(var ItemNo: Integer);
begin
  if ItemNo>=Items.Count then
    ItemNo := Items.Count-1;
  if ItemNo<0 then
    ItemNo := 0;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.NextChar(ItemNo, Index: Integer): Integer;
begin
  Result := NextCharStr(Items[ItemNo], ItemNo, Index);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.PrevChar(ItemNo, Index: Integer): Integer;
begin
  Result := PrevCharStr(Items[ItemNo], ItemNo, Index);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.NextCharStr(const str: String;
  ItemNo, Index: Integer): Integer;
{$IFNDEF RVDONOTUSEUNICODE}
var s: String;
    p1,p2: Pointer;
{$ENDIF}
begin
  {$IFNDEF RVDONOTUSEUNICODE}
  if RVNT and (rvioUnicode in GetItemOptions(ItemNo)) then begin
    s := str;
    SetLength(s, Length(s)+1);
    s[Length(s)]:=#0;
    p1 := Pointer(s);
    p2 := CharNextW(Pointer(PChar(p1)+(Index-1)*2));
    Result := (PChar(p2)-PChar(p1)) div 2+1;
    end
  else
  {$ENDIF}
    Result := Index+1;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.PrevCharStr(const str: String;
  ItemNo, Index: Integer): Integer;
{$IFNDEF RVDONOTUSEUNICODE}
var s: String;
    p1,p2: Pointer;
{$ENDIF}
begin
  {$IFNDEF RVDONOTUSEUNICODE}
  if RVNT and (rvioUnicode in GetItemOptions(ItemNo)) then begin
    s := str;
    SetLength(s, Length(s)+1);
    s[Length(s)]:=#0;
    p1 := Pointer(s);
    p2 := CharPrevW(p1, Pointer(PChar(p1)+(Index-1)*2));
    if p2=PChar(p1)+(Index-1)*2 then
      p2 := p1;
    Result := (PChar(p2)-PChar(p1)) div 2+1;
    end
  else
  {$ENDIF}
    Result := Index-1;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetFirstParaItem(ItemNo: Integer): Integer;
begin
  Result := ItemNo;
  while (Result>0) and not IsParaStart(Result) do
    dec(Result);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetFirstParaSectionItem(ItemNo: Integer): Integer;
begin
  Result := ItemNo;
  while (Result>0) and not IsFromNewLine(Result) do
    dec(Result);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.GetParentInfo(var ParentItemNo: Integer;
  var Location: TRVStoreSubRVData);
begin
  ParentItemNo := -1;
  Location   := nil;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetBiDiMode: TRVBiDiMode;
begin
  Result := GetRootData.GetBiDiMode;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetItemBiDiMode(ItemNo: Integer): TRVBiDiMode;
var item: TCustomRVItemInfo;
begin
  item := GetItem(ItemNo);
  if item.StyleNo>=0 then
    Result := GetRVStyle.TextStyles[GetActualStyle(item)].BiDiMode
  else
    Result := rvbdUnspecified;
  if Result=rvbdUnspecified then
    Result := GetParaBiDiMode(item.ParaNo);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetParaBiDiMode(
  ParaNo: Integer): TRVBiDiMode;
begin
  Result := GetRVStyle.ParaStyles[ParaNo].BiDiMode;
  if Result=rvbdUnspecified then
    Result := GetBiDiMode;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSELISTS}
function TCustomRVData.SetListMarkerInfo(AItemNo, AListNo, AListLevel, AStartFrom, AParaNo: Integer;
                                     AUseStartFrom: Boolean): Integer;
var Marker: TRVMarkerItemInfo;
    s: String;
    Markers: TRVMarkerList;
begin
  if (AItemNo>=Items.Count) or (AItemNo<0) then
    Result := Items.Count
  else begin
    Result := GetFirstParaItem(AItemNo);
    if GetItem(Result).GetBoolValue(rvbpFullWidth) then begin
      Result := -1;
      exit;
    end;
  end;
  if (Result<Items.Count) and (GetItemStyle(Result)=rvsListMarker) then begin
    Marker := TRVMarkerItemInfo(GetItem(Result));
    Marker.ListNo    := AListNo;
    Marker.Level     := AListLevel;
    Marker.StartFrom := AStartFrom;
    Marker.Reset     := AUseStartFrom;
    Markers := GetMarkers(False);
    Markers.RecalcCounters(Marker.GetIndexInList(Markers), GetRVStyle);
    end
  else begin
    Marker := TRVMarkerItemInfo.CreateEx(Self, AListNo, AListLevel, AStartFrom, AUseStartFrom);
    s := '';
    Marker.Inserting(Self,s,False);
    if Result<Items.Count then begin
      GetItem(Result).SameAsPrev := True;
      Marker.ParaNo := GetItemPara(Result);
      end
    else begin
      Marker.ParaNo := AParaNo;
      if AParaNo<0 then
        if Items.Count=0 then
          Marker.ParaNo := 0
        else
          Marker.ParaNo := GetItemPara(Items.Count-1);
    end;
    Items.InsertObject(Result, s, Marker);
    Marker.Inserted(Self, Result);
    AddMarkerInList(Result);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.RecalcMarker(AItemNo: Integer; AllowCreateList: Boolean);
var Markers: TRVMarkerList;
begin
  if GetItemStyle(AItemNo)<>rvsListMarker then
    exit;
  Markers := GetMarkers(AllowCreateList);
  if Markers=nil then
    exit;
  Markers.RecalcCounters(TRVMarkerItemInfo(GetItem(AItemNo)).GetIndexInList(Markers), GetRVStyle);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.RemoveListMarker(ItemNo: Integer);
begin
  ItemNo := GetFirstParaItem(ItemNo);
  if GetItemStyle(ItemNo)=rvsListMarker then begin
    DeleteItems(ItemNo,1);
    if ItemNo<Items.Count then
      GetItem(ItemNo).SameAsPrev := False;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetListMarkerInfo(AItemNo: Integer;
                                 var AListNo, AListLevel, AStartFrom: Integer;
                                 var AUseStartFrom: Boolean): Integer;
begin
  Result := GetFirstParaItem(AItemNo);
  if GetItemStyle(Result)<>rvsListMarker then begin
    Result := -1;
    exit;
  end;
  with TRVMarkerItemInfo(GetItem(Result)) do begin
    AListNo := ListNo;
    AListLevel := Level;
    AStartFrom := StartFrom;
    AUseStartFrom := Reset;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetMarkers(AllowCreate: Boolean): TRVMarkerList;
begin
  Result := GetAbsoluteRootData.GetMarkers(AllowCreate);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.GetPrevMarkers: TRVMarkerList;
begin
  Result := nil;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.DestroyMarkers;
begin
  GetAbsoluteRootData.DestroyMarkers;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.AddMarkerInList(ItemNo: Integer);
var List: TRVMarkerList;
    PrevMarker: TRVMarkerItemInfo;
    Index: Integer;
begin
  if GetItemStyle(ItemNo)<>rvsListMarker then
    exit;
  List := GetMarkers(True);
  if List=nil then
    exit;
  if TRVMarkerItemInfo(GetItem(ItemNo)).GetIndexInList(List)>=0 then
    exit;
  PrevMarker := FindPreviousMarker(ItemNo-1);
  Index := List.InsertAfter(TRVMarkerItemInfo(GetItem(ItemNo)), PrevMarker);
  GetMarkers(False).RecalcCounters(Index, GetRVStyle);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVData.DeleteMarkerFromList(Item: TCustomRVItemInfo; Clearing: Boolean);
var List: TRVMarkerList;
    Index: Integer;
begin
  if Item.StyleNo=rvsListMarker then begin
    List := GetMarkers(False);
    if List=nil then
      exit;
    Index := List.IndexOf(Item);
    List.Delete(Index);
    if List.Count=0 then
      DestroyMarkers
    else if not Clearing then
      List.RecalcCounters(Index, GetRVStyle);
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.FindPreviousMarker(ItemNo: Integer): TRVMarkerItemInfo;
   {...................................................}
   function FindMarkerInRVData(RVData: TCustomRVData; LastItemNo: Integer): TRVMarkerItemInfo; forward;
   {...................................................}
   function FindMarkerInItem(Item: TCustomRVItemInfo; StoreSub: TRVStoreSubRVData): TRVMarkerItemInfo;
   var RVData: TCustomRVData;
   begin
     Result := nil;
     if StoreSub=nil then
       RVData := TCustomRVData(item.GetSubRVData(StoreSub, rvdLast))
     else
       RVData := TCustomRVData(item.GetSubRVData(StoreSub, rvdPrev));
     if RVData<>nil then begin
       repeat
         Result := FindMarkerInRVData(RVData, RVData.Items.Count-1);
         if Result<>nil then
           break;
         RVData := TCustomRVData(item.GetSubRVData(StoreSub, rvdPrev));
       until RVData=nil;
     end;
     StoreSub.Free;
   end;
   {...................................................}
   function FindMarkerInRVData(RVData: TCustomRVData; LastItemNo: Integer): TRVMarkerItemInfo;
   var i: Integer;
   begin
     for i := LastItemNo downto 0 do begin
       if RVData.GetItem(i).StyleNo=rvsListMarker then
         Result := TRVMarkerItemInfo(RVData.GetItem(i))
       else
         Result := FindMarkerInItem(RVData.GetItem(i), nil);
       if Result<>nil then
         exit;
     end;
     Result := nil;
   end;
   {...................................................}
var RVData: TCustomRVData;
    StoreSub: TRVStoreSubRVData;
begin
  Result := nil;
  RVData := Self;
  while RVData<>nil do begin
    Result := FindMarkerInRVData(RVData, ItemNo);
    if Result<>nil then
      break;
    RVData.GetParentInfo(ItemNo, StoreSub);
    if ItemNo<0 then begin
      StoreSub.Free;
      break;
    end;
    RVData := RVData.GetAbsoluteParentData;
    Result := FindMarkerInItem(RVData.GetItem(ItemNo), StoreSub);
    if Result<>nil then
      break;
    dec(ItemNo);
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVData.FindMarkerLocalLocationFrom(StartItemNo: Integer;
  Marker: TRVMarkerItemInfo): Integer;
   {...................................................}
   function FindMarkerInRVData(RVData: TCustomRVData; FirstItemNo: Integer): Integer; forward;
   {...................................................}
   function FindMarkerInItem(Item: TCustomRVItemInfo): Boolean;
   var RVData: TCustomRVData;
       StoreSub: TRVStoreSubRVData;
   begin
     Result := False;
     RVData := TCustomRVData(item.GetSubRVData(StoreSub, rvdFirst));
     if RVData<>nil then begin
       repeat
         Result := FindMarkerInRVData(RVData, 0)>=0;
         if Result then
           break;
         RVData := TCustomRVData(item.GetSubRVData(StoreSub, rvdNext));
       until RVData=nil;
     end;
     StoreSub.Free;
   end;
   {...................................................}
   function FindMarkerInRVData(RVData: TCustomRVData; FirstItemNo: Integer): Integer;
   var i: Integer;
   begin
     for i := FirstItemNo to RVData.Items.Count-1 do
       if (RVData.GetItem(i)=Marker) or
           FindMarkerInItem(RVData.GetItem(i)) then begin
         Result := i;
         exit;
       end;
     Result := -1;
   end;
   {...................................................}
begin;
  Result := FindMarkerInRVData(Self, StartItemNo);
end;
{------------------------------------------------------------------------------}
function TCustomRVData.FindLastMarkerIndex(StartAfterMeIndex: Integer;
  ListStyles: TRVIntegerList): Integer;
var i, j, ListNo: Integer;
    ok: Boolean;
    Markers: TRVMarkerList;
begin
  Result := -1;
  Markers := GetMarkers(False);
  if Markers=nil then
    exit;
  for i := Markers.Count-1 downto StartAfterMeIndex+1 do begin
    ok := False;
    ListNo := TRVMarkerItemInfo(Markers[i]).ListNo;
    for j := 0 to ListStyles.Count-1 do
      if ListStyles[j] = ListNo then begin
        ok := True;
        break;
      end;
    if ok then begin
      Result := i;
      exit;
    end;
  end;
end;

{$ENDIF}


function TCustomRVData.GetChosenItem: TCustomRVItemInfo;
begin
  Result := nil;
end;

function TCustomRVData.GetChosenRVData: TCustomRVData;
begin
  Result := nil;
end;

function TCustomRVData.GetItemText(ItemNo: Integer): String;
begin
  Result := Items[ItemNo];
end;

procedure TCustomRVData.SetItemText(ItemNo: Integer; const s: String);
begin
  if rvioUnicode in GetItemOptions(ItemNo) then
    RVCheckUni(Length(s));
  Items[ItemNo] := s;
end;
{------------------------------------------------------------------------------}
// Returns the first and the last item of paragraph section containing
// the given range of items
procedure TCustomRVData.ExpandToParaSection(ItemNo1,ItemNo2: Integer;
  var FirstItemNo, LastItemNo: Integer);
begin
  FirstItemNo := ItemNo1;
  while (FirstItemNo>0) and not IsFromNewLine(FirstItemNo) do
    dec(FirstItemNo);
  LastItemNo := ItemNo2+1;
  while (LastItemNo<Items.Count) and not IsFromNewLine(LastItemNo) do
    inc(LastItemNo);
  dec(LastItemNo);
end;
{------------------------------------------------------------------------------}
// Returns the first and the last item of paragraph containing
// the given range of items
procedure TCustomRVData.ExpandToPara(ItemNo1,ItemNo2: Integer;
  var FirstItemNo, LastItemNo: Integer);
begin
  FirstItemNo := ItemNo1;
  while (FirstItemNo>0) and not IsParaStart(FirstItemNo) do
    dec(FirstItemNo);
  LastItemNo := ItemNo2+1;
  while (LastItemNo<Items.Count) and not IsParaStart(LastItemNo) do
    inc(LastItemNo);
  dec(LastItemNo);
end;

function TCustomRVData.GetItemCount: Integer;
begin
  Result := Items.Count;
end;

function TCustomRVData.Edit: TCustomRVData;
begin
  Result := Self;
end;

initialization
  {$IFNDEF RVDONOTUSERVF}
  RegisterClasses([TBitmap, TIcon, TMetafile]);
  {$IFNDEF RVDONOTUSEJPEGIMAGE}
  RegisterClasses([TJpegImage]);
  {$ENDIF}
  {$ENDIF}
  HTMLGraphicFormats := nil;
finalization
  HTMLGraphicFormats.Free;
  HTMLGraphicFormats := nil;


end.

