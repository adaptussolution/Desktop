
{*******************************************************}
{                                                       }
{       RichView                                        }
{       Basic item types.                               }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}


unit RVItem;

interface

{$I RV_Defs.inc}

uses SysUtils, Classes, Windows, Graphics, Controls, Forms,
     CommCtrl,
     {$IFNDEF RVDONOTUSEJPEGIMAGE}
     Jpeg,
     {$ENDIF}
     {$IFDEF RICHVIEWDEF4}
     ImgList,
     {$ENDIF}
     RVClasses, RVFuncs, RVScroll, RVStyle, DLines;

type
  { Exception class }
  ERichViewError = class(Exception);

  { Options for RichViewItems }
  TRVItemOption = (
    rvioSameAsPrev,      // item is not a first item on the paragraph [section]
    rvioPageBreakBefore, // item starts a page (rvioSameAsPrev must be excluded)
    rvioBR,              // item starts a paragraph section, but not a paragraph
                         // (rvioSameAsPrev must be excluded)
    rvioUnicode);        // this is an Unicode text item
  TRVItemOptions = set of TRVItemOption;

  { Item drawing states. Used in item.Paint() }
  TRVItemDrawState = (
    rvidsSelected,       // the item is selected
    rvidsCurrent,        // the item is current (active) - at the caret position
    rvidsHover,          // the item is a hypertext item under the mouse pointer
    rvidsPrinting,       // this is not drawing but printing (or previewing)
    rvidsPreview,        // this is drawing for print preview
    rvidsPreviewCorrection, // this is drawing for print preview with preview
                         // correction
    rvidsControlFocused);// RichView has input focus
  TRVItemDrawStates = set of TRVItemDrawState;

  { RVF reading: reading mode }
  TRVFReadMode = (
    rmText,              // reading ANSI text
    rmBeforeBinary,      // reading line break after ANSI text before binary data
    rmBinary,            // reading binary data
    rmBeforeUnicode,     // reading line break after ANSI text before Unicode text
    rmUnicode,           // reading Unicode text
    rmAfterUnicode);     // reading line break after Unicode text before ANSI text
  { RVF reading: reading state }
  TRVFReadState = (
    rstHeader,           // reading item header
    rstData,             // reading item data lines
    rstSkip);            // skipping unknown lines

  { Identifiers of item boolean properties, for item.GetBoolValue() }
  TRVItemBoolProperty = (
    rvbpFullWidth,       // full line item (such as "break" or table) [y/n?]
    rvbpValid,           // the item is valid (has correct data)
    rvbpRequiresRVFLines,   // the item has one or more data lines in RVF
    rvbpDisplayActiveState, // the item shows its active state (at the position
                            // of caret)
    rvbpDrawingChangesFont, // drawing of item may change Canvas.Font
    rvbpCanSaveUnicode,  // the item can represent itself in Unicode
                         // (item.AsText() returns a "raw" Unicode, if Unicode
                         // parameter is True), so RichView does not need
                         // to convert the result of item.AsText() to Unicode
    rvbpAlwaysInText,    // the item must be saved in text, event when saving
                         // the selection
    rvbpImmediateControlOwner, // this item directly owns a VCL control
    rvbpResizable,       // the item can be resized by mouse (RichViewEdit
                         // must create a resizer for it, if this item is selected)
    rvbpResizeHandlesOutside, // resize handles must be drawn not inside, but
                         // outside of item rectangle
    rvbpHasSubRVData,    // item has subdocuments
    rvbpClickSelect,     // item is selected on single click (resizable items
                         //   are always selected on single click)
    rvbpNoHTML_P);       // this item cannot be nested in HTML's <p>...</p>
                         // (<div>...</div> must be used)

  { Identifiers of item boolean properties, for item.GetBoolValueEx() }
  TRVItemBoolPropertyEx = (
    rvbpPrintToBMP,      // item.PrintToBitmap() must be used instead of item.Print()
    rvbpJump,            // this is a hypertext item
    rvbpAllowsFocus,     // this item can have input focus
    rvbpHotColdJump,     // this hypertext item can be highlighted under
                         // the mouse pointer
    rvbpXORFocus,        // RichView must draw XOR dotted frame, if this item
                         // has input focus
    rvbpActualPrintSize  // item.OnDocWidthChange() returns item size in printer
                         // (not screen) resolution
    );

  { From there the caret enters into the item, for method EnterItem() }
  TRVEnterDirection = (rvedLeft, rvedRight, rvedTop, rvedBottom);

  { Extra item integer properties }
  TRVExtraItemProperty = (
    rvepUnknown,         // (none)
    rvepVShift,          // vertical offset, in pixels or %
    rvepVShiftAbs,       // if <>0, vertical offset is in pixels
    rvepImageWidth,      // image width (for stretching)
    rvepImageHeight,     // image height (for stretching)
    rvepTransparent,     // bitmap image is transparent, see TBitmap.Transparent
    rvepTransparentMode, // see TBitmep.TransparentMode
    rvepTransparentColor,// see TBitmap.TransparentColor
    rvepMinHeightOnPage, // if <>0, the item can be splitted between pages
                         // if the rest of page > this value; such items
                         // are always printed from the new line
    rvepSpacing,         // spacing around the item
    rvepResizable,       // this item (control) is resizable
    rvepDeleteProtect,   // this item cannot be deleted by editing operations
    rvepNoHTMLImageSize);// if<>0, image size is not saved in HTML,
                         //   even if rvsoImageSizes is included in Options
                         //   for SaveHTML
                         //   (this option is ignored if rvepImageWidth or Height
                         //   are non-zero

  TRVExtraItemStrProperty = (
    rvespUnknown,        // (none)
    rvespHint,           // hint
    rvespAlt,            // text representation of images
    rvespImageFileName); // image file name
  { ----------------------------------------------------------------------------
    TRVMultiDrawItemPart: ancestor class of items in
    TRVMultiDrawItemInfo.PartsList.
    Inherited classes:
    - TRVImagePrintPart (for TRVMultiImagePrintInfo.PartsList);
    - TRVTablePrintPart (for TRVTablePrintInfo.PartsList).
  }
  TRVMultiDrawItemPart = class
    public
      Height: Integer;
      function GetSoftPageBreakInfo: Integer; dynamic;
  end;

{------------------------------------------------------------------------------}
  TRVCPInfo = class;
  TCustomRVItemInfo = class;
  TRVCPInfo = class
    public
      Name: String;
      Next, Prev: TRVCPInfo;
      RaiseEvent, Persistent: Boolean;
      ItemInfo : TCustomRVItemInfo;
      ItemNo: Integer; // <- not maintained automatically
      Tag: Integer;
      procedure Assign(Source: TRVCPInfo; TagsArePChars: Boolean);
      function CreateCopy(TagsArePChars: Boolean): TRVCPInfo;
  end;
{------------------------------------------------------------------------------}
 TRVSubRVDataPos = (rvdFirst, rvdLast, rvdChosenUp, rvdChosenDown, rvdNext, rvdPrev);
 TRVStoreSubRVData = class
   function Duplicate: TRVStoreSubRVData; dynamic;
 end;

{------------------------------------------------------------------------------}
  TCustomRVItemInfo = class (TPersistent)
    private
      function GetSameAsPrev: Boolean;
      procedure SetSameAsPrev(const Value: Boolean);
      function GetBR: Boolean;
      procedure SetBR(Value: Boolean);
      function GetPageBreakBefore: Boolean;
      procedure SetPageBreakBefore(const Value: Boolean);
    protected
      function SaveRVFHeaderTail(RVData: TPersistent): String; dynamic;
      function GetRVFExtraPropertyCount: Integer; dynamic;
      procedure SaveRVFExtraProperties(Stream: TStream); dynamic;
      procedure SetExtraPropertyFromRVFStr(const Str: String);
    public
      StyleNo,ParaNo: Integer;
      ItemOptions: TRVItemOptions;
      Checkpoint: TRVCPInfo;
      JumpID: Integer;
      Tag: Integer;
      DrawItemNo: Integer;
      {$IFNDEF RVDONOTUSEITEMHINTS}
      Hint: String;
      {$ENDIF}
      constructor Create(RVData: TPersistent); virtual;
      procedure Assign(Source: TCustomRVItemInfo); {$IFDEF RICHVIEWDEF4} reintroduce; {$ENDIF} dynamic;
      function GetSubRVDataAt(X,Y: Integer): TPersistent; dynamic;
      function GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas;
        RVData: TPersistent): Integer; virtual;
      function OwnsControl(AControl: TControl): Boolean; dynamic;
      function OwnsInplaceEditor(AEditor: TControl): Boolean; dynamic;
      function CanBeBorderStart: Boolean;
      function ParaStart(CountBR: Boolean): Boolean;
      property SameAsPrev: Boolean read GetSameAsPrev write SetSameAsPrev;
      function AsImage: TGraphic; virtual;
      procedure SaveToHTML(Stream: TStream; RVData: TPersistent;
        ItemNo: Integer; const Text, Path: String;
        const imgSavePrefix: String; var imgSaveNo: Integer;
        CurrentFileColor: TColor; SaveOptions: TRVSaveOptions;
        UseCSS: Boolean; Bullets: TRVList); dynamic;
      function AsText(LineWidth: Integer; RVData: TPersistent;
        const Text, Path: String; TextOnly,Unicode: Boolean): String; dynamic;
      procedure UpdatePaletteInfo(PaletteAction: TRVPaletteAction;
        ForceRecreateCopy: Boolean; Palette: HPALETTE;
        LogPalette: PLogPalette); dynamic;
      function ReadRVFHeader(var P: PChar; RVData: TPersistent): Boolean; dynamic;
      function GetBoolValue(Prop: TRVItemBoolProperty): Boolean; virtual;
      function GetBoolValueEx(Prop: TRVItemBoolPropertyEx;
        RVStyle: TRVStyle): Boolean; virtual;
      function ReadRVFLine(const s: String; RVData: TPersistent;
        ReadType, LineNo, LineCount: Integer; var Name: String;
        var ReadMode: TRVFReadMode; var ReadState: TRVFReadState): Boolean; dynamic;
      procedure SaveRVF(Stream: TStream; RVData: TPersistent;
        ItemNo, ParaNo: Integer; const Name: String; Part: TRVMultiDrawItemPart;
        ForceSameAsPrev: Boolean); dynamic;
      procedure SaveRTF(Stream: TStream; RVData: TPersistent; ItemNo: Integer;
        const Name: String; TwipsPerPixel: Double; Level: Integer;
        ColorList: TRVColorList;
        StyleToFont, ListOverrideOffsetsList1,
        ListOverrideOffsetsList2: TRVIntegerList; FontTable: TRVList); dynamic;
      procedure FillRTFTables(ColorList: TRVColorList;
        ListOverrideCountList: TRVIntegerList; RVData: TPersistent); dynamic;
      procedure PaintFullWidth(Left, Right, Top: Integer; Canvas: TCanvas;
                      State: TRVItemDrawStates;
                      Style: TRVStyle; const ClipRect: TRect;
                      dli: TRVDrawLineInfo); virtual;
      procedure Paint(x,y: Integer; Canvas: TCanvas; State: TRVItemDrawStates;
                      Style: TRVStyle; dli: TRVDrawLineInfo); virtual;
      function PrintToBitmap(Bkgnd: TBitmap; Preview: Boolean;
        RichView: TRVScroller; dli: TRVDrawLineInfo; Part: Integer;
        ColorMode: TRVColorMode):Boolean; virtual;
      procedure Print(Canvas: TCanvas; x,y,x2: Integer;
        Preview, Correction: Boolean; const sad: TRVScreenAndDevice;
        RichView: TRVScroller; dli: TRVDrawLineInfo;
        Part: Integer; ColorMode: TRVColorMode; RVData: TPersistent); virtual;
      function GetImageWidth(RVStyle: TRVStyle): Integer; virtual;
      function GetImageHeight(RVStyle: TRVStyle): Integer; virtual;
      function GetBorderWidth: Integer; virtual;
      function GetBorderHeight: Integer; virtual;

      procedure MovingToUndoList(ItemNo: Integer;
        RVData, AContainerUndoItem: TObject); dynamic;
      procedure MovingFromUndoList(ItemNo: Integer; RVData: TObject); dynamic;
      procedure FinalizeUndoGroup; dynamic;

      function MouseMove(Shift: TShiftState; X,Y, ItemNo: Integer;
        RVData: TObject):Boolean; dynamic;
      function MouseDown(Button: TMouseButton; Shift: TShiftState;
        X,Y, ItemNo: Integer; RVData: TObject):Boolean; dynamic;
      function MouseUp(Button: TMouseButton; Shift: TShiftState;
        X,Y, ItemNo: Integer; RVData: TObject):Boolean; dynamic;

      procedure BeforeLoading(FileFormat: TRVLoadFormat); dynamic;
      procedure AfterLoading(FileFormat: TRVLoadFormat); dynamic;
      procedure DeselectPartial; dynamic;
      function PartiallySelected: Boolean; dynamic;
      function CanDeletePartiallySelected: Boolean; dynamic;
      procedure DeletePartiallySelected; dynamic;
      procedure ApplyParaStyleToSubRVDatas(ParaNo: Integer; SelectedOnly: Boolean); dynamic;
      procedure ApplyParaStyleConversionToSubRVDatas(UserData: Integer; SelectedOnly: Boolean); dynamic;
      procedure ApplyStyleConversionToSubRVDatas(UserData: Integer; SelectedOnly: Boolean);dynamic;
      procedure ApplyStyleConversion(RVData: TPersistent; UserData: Integer); dynamic;
      function CreatePrintingDrawItem(RVData: TObject;
        const sad: TRVScreenAndDevice): TRVDrawLineInfo; virtual;

      procedure StartExport; dynamic;
      procedure EndExport; dynamic;

      procedure Inserting(RVData: TObject; var Text: String; Safe: Boolean); virtual;
      procedure Inserted(RVData: TObject; ItemNo: Integer); virtual;
      procedure BeforeUndoChangeProperty; dynamic;
      procedure AfterUndoChangeProperty; dynamic;

      function EnterItem(From: TRVEnterDirection; Coord: Integer): Boolean; dynamic;
      function GetHypertextCursor(RVStyle: TRVStyle): TCursor; dynamic;
      procedure BuildJumps(Left,Top: Integer; var StartJumpNo: Integer; jumps: TList); dynamic;
      procedure Focusing;dynamic;
      function MoveFocus(GoForward: Boolean; var TopLevelRVData: TPersistent;
        var TopLevelItemNo: Integer): Boolean; dynamic;
      procedure ClearFocus; dynamic;
      procedure Execute(RVData:TPersistent);dynamic;
      function AdjustFocusToControl(Control: TControl;
        var TopLevelRVData: TPersistent;
        var TopLevelItemNo: Integer):Boolean;dynamic;
      procedure OnDocWidthChange(DocWidth: Integer; dli: TRVDrawLineInfo;
        Printing: Boolean; Canvas: TCanvas; RVData: TPersistent;
        sad: PRVScreenAndDevice; var HShift, Desc: Integer;
        NoCaching: Boolean); virtual;
      procedure MarkStylesInUse(UsedTextStyles, UsedParaStyles,
        UsedListStyles: TRVIntegerList); dynamic;
      procedure UpdateStyles(TextStylesShift, ParaStylesShift,
        ListStylesShift: TRVIntegerList); dynamic;
      function GetSubRVData(var StoreState: TRVStoreSubRVData;
        Position: TRVSubRVDataPos): TPersistent; dynamic;
      procedure ChooseSubRVData(StoreState: TRVStoreSubRVData); dynamic;
      procedure CleanUpChosen; dynamic;
      procedure ResetSubCoords; dynamic;
      function SetExtraIntProperty(Prop: TRVExtraItemProperty;
        Value: Integer): Boolean; dynamic;
      function GetExtraIntProperty(Prop: TRVExtraItemProperty;
        var Value: Integer): Boolean; dynamic;
      function SetExtraStrProperty(Prop: TRVExtraItemStrProperty;
        const Value: String): Boolean; dynamic;
      function GetExtraStrProperty(Prop: TRVExtraItemStrProperty;
        var Value: String): Boolean; dynamic;
      procedure SetExtraCustomProperty(const PropName, Value: String); dynamic;
      function GetSoftPageBreakDY(Data: Integer): Integer; dynamic;
      function GetActualStyleNo(RVStyle: TRVStyle): Integer;
      procedure DrawBackgroundForPrinting(Canvas: TCanvas;
        const Rect, FullRect: TRect; ColorMode: TRVColorMode;
        ItemBackgroundLayer: Integer); virtual;

      property BR: Boolean read GetBR write SetBR;
      property PageBreakBefore: Boolean read GetPageBreakBefore write SetPageBreakBefore;
  end;

  TCustomRVItemInfoClass = class of TCustomRVItemInfo;

  TRVTextItemInfo = class (TCustomRVItemInfo)
    public
      function GetBoolValueEx(Prop: TRVItemBoolPropertyEx; RVStyle: TRVStyle): Boolean; override;
      procedure Execute(RVData:TPersistent);override;
      procedure MarkStylesInUse(UsedTextStyles, UsedParaStyles, UsedListStyles: TRVIntegerList); override;
      procedure UpdateStyles(TextStylesShift, ParaStylesShift, ListStylesShift: TRVIntegerList); override;
      function ReadRVFHeader(var P: PChar; RVData: TPersistent): Boolean; override;
  end;

  TRVTextItemInfoClass = class of TRVTextItemInfo;

  TRVNonTextItemInfo = class (TCustomRVItemInfo)
    protected
      function GetHeight: Integer; virtual;
      function GetWidth: Integer;  virtual;
      function GetRVFExtraPropertyCount: Integer; override;
      procedure SaveRVFExtraProperties(Stream: TStream); override;
    public
      DeleteProtect: Boolean;
      function GetLeftOverhang: Integer; virtual;
      procedure AdjustInserted(x,y: Integer; adjusty: Boolean); virtual;
      procedure Assign(Source: TCustomRVItemInfo); override;
      function SetExtraIntProperty(Prop: TRVExtraItemProperty; Value: Integer): Boolean; override;
      function GetExtraIntProperty(Prop: TRVExtraItemProperty; var Value: Integer): Boolean; override;
      property Height: Integer read GetHeight;
      property Width: Integer read GetWidth;
  end;

  TRVFullLineItemInfo = class (TRVNonTextItemInfo)
    public
      function GetBoolValue(Prop: TRVItemBoolProperty): Boolean; override;
  end;

  TRVRectItemInfo = class (TRVNonTextItemInfo)
    protected
      FMinHeightOnPage: Integer;
      function GetDescent: Integer; virtual;
      function SaveRVFHeaderTail(RVData: TPersistent): String; override;
      function GetRVFExtraPropertyCount: Integer; override;
      procedure SaveRVFExtraProperties(Stream: TStream); override;
      function GetVShiftCSS(RVStyle: TRVStyle): String;
    public
      VAlign: TRVVAlign;
      VShift: Integer;
      VShiftAbs: Boolean;
      Spacing: Integer;
      constructor Create(RVData: TPersistent); override;
      procedure Assign(Source: TCustomRVItemInfo); override;
      function SetExtraIntProperty(Prop: TRVExtraItemProperty; Value: Integer): Boolean; override;
      function GetExtraIntProperty(Prop: TRVExtraItemProperty; var Value: Integer): Boolean; override;
      function ReadRVFHeader(var P: PChar; RVData: TPersistent): Boolean; override;
      function GetBorderWidth: Integer; override;
      function GetBorderHeight: Integer; override;
      property Descent: Integer read GetDescent;
  end;

  TRVControlItemInfo = class (TRVRectItemInfo)
    protected
      FResizable: Boolean;
      function GetHeight: Integer; override;
      function GetWidth: Integer; override;
      function GetRVFExtraPropertyCount: Integer; override;
      procedure SaveRVFExtraProperties(Stream: TStream); override;
    public
      Control:  TControl;
      PercentWidth: Integer;
      constructor CreateEx(RVData: TPersistent;AControl: TControl; AVAlign: TRVVAlign);
      procedure Assign(Source: TCustomRVItemInfo); override;
      destructor Destroy; override;
      function GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas; RVData: TPersistent): Integer; override;
      procedure AdjustInserted(x,y: Integer; adjusty: Boolean); override;
      function PrintToBitmap(Bkgnd: TBitmap; Preview: Boolean; RichView: TRVScroller;
        dli: TRVDrawLineInfo; Part: Integer; ColorMode: TRVColorMode):Boolean; override;
      function CreatePrintingDrawItem(RVData: TObject; const sad: TRVScreenAndDevice): TRVDrawLineInfo; override;
      procedure Paint(x,y: Integer; Canvas: TCanvas; State: TRVItemDrawStates;
                      Style: TRVStyle; dli: TRVDrawLineInfo); override;
      function OwnsControl(AControl: TControl): Boolean; override;
      function AsText(LineWidth: Integer;
                           RVData: TPersistent;
                           const Text, Path: String;
                           TextOnly,Unicode: Boolean): String; override;
      procedure SaveToHTML(Stream: TStream; RVData: TPersistent;
                           ItemNo: Integer;
                           const Text, Path: String;
                           const imgSavePrefix: String;
                           var imgSaveNo: Integer;
                           CurrentFileColor: TColor;
                           SaveOptions: TRVSaveOptions;
                           UseCSS: Boolean;
                           Bullets: TRVList); override;
      function ReadRVFLine(const s: String; RVData: TPersistent;
                           ReadType, LineNo, LineCount: Integer;
                           var Name: String;
                           var ReadMode: TRVFReadMode;
                           var ReadState: TRVFReadState): Boolean; override;
      procedure SaveRVF(Stream: TStream; RVData: TPersistent; ItemNo, ParaNo: Integer;
                        const Name: String; Part: TRVMultiDrawItemPart;
                        ForceSameAsPrev: Boolean); override;
      function SetExtraIntProperty(Prop: TRVExtraItemProperty; Value: Integer): Boolean; override;
      function GetExtraIntProperty(Prop: TRVExtraItemProperty; var Value: Integer): Boolean; override;
      procedure SaveRTF(Stream: TStream; RVData: TPersistent; ItemNo: Integer;
        const Name: String; TwipsPerPixel: Double; Level: Integer; ColorList: TRVColorList;
        StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
        FontTable: TRVList); override;
      function GetBoolValue(Prop: TRVItemBoolProperty): Boolean; override;
      function GetBoolValueEx(Prop: TRVItemBoolPropertyEx; RVStyle: TRVStyle): Boolean; override;
      function GetImageHeight(RVStyle: TRVStyle): Integer; override;
      function GetImageWidth(RVStyle: TRVStyle): Integer; override;
      procedure MovingToUndoList(ItemNo: Integer; RVData, AContainerUndoItem: TObject); override;
      procedure MovingFromUndoList(ItemNo: Integer; RVData: TObject); override;
      procedure Inserting(RVData: TObject; var Text: String; Safe: Boolean); override;
      procedure Focusing; override;
      procedure OnDocWidthChange(DocWidth: Integer; dli: TRVDrawLineInfo; Printing: Boolean;
                                 Canvas: TCanvas; RVData: TPersistent; sad: PRVScreenAndDevice;
                                 var HShift, Desc: Integer; NoCaching: Boolean); override;
      property MinHeightOnPage: Integer read FMinHeightOnPage write FMinHeightOnPage;
  end;

  TRVGraphicItemInfo = class (TRVRectItemInfo)
    protected
      function GetHeight: Integer; override;
      function GetWidth: Integer; override;
      function GetRVFExtraPropertyCount: Integer; override;
      procedure SaveRVFExtraProperties(Stream: TStream); override;
    public
      Image, ImageCopy: TGraphic;
      ImageWidth, ImageHeight : Integer;
      NoHTMLImageSize: Boolean;
      Alt, ImageFileName: String;
      constructor CreateEx(RVData: TPersistent; AImage: TGraphic; AVAlign: TRVVAlign); virtual;
      procedure Assign(Source: TCustomRVItemInfo); override;
      function SetExtraIntProperty(Prop: TRVExtraItemProperty;
        Value: Integer): Boolean; override;
      function GetExtraIntProperty(Prop: TRVExtraItemProperty;
        var Value: Integer): Boolean; override;
      function SetExtraStrProperty(Prop: TRVExtraItemStrProperty;
        const Value: String): Boolean; override;
      function GetExtraStrProperty(Prop: TRVExtraItemStrProperty;
        var Value: String): Boolean; override;
      procedure UpdatePaletteInfo(PaletteAction: TRVPaletteAction;
        ForceRecreateCopy: Boolean; Palette: HPALETTE;
        LogPalette: PLogPalette); override;
      function GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas;
        RVData: TPersistent): Integer; override;
      destructor Destroy; override;
      function AsImage: TGraphic; override;
      procedure Paint(x,y: Integer; Canvas: TCanvas; State: TRVItemDrawStates;
        Style: TRVStyle; dli: TRVDrawLineInfo); override;
      function GetBoolValue(Prop: TRVItemBoolProperty): Boolean; override;
      function GetBoolValueEx(Prop: TRVItemBoolPropertyEx; RVStyle: TRVStyle): Boolean; override;
      procedure SaveToHTML(Stream: TStream; RVData: TPersistent;
        ItemNo: Integer; const Text, Path: String; const imgSavePrefix: String;
        var imgSaveNo: Integer; CurrentFileColor: TColor;
        SaveOptions: TRVSaveOptions; UseCSS: Boolean; Bullets: TRVList); override;
      function ReadRVFLine(const s: String; RVData: TPersistent;
        ReadType, LineNo, LineCount: Integer; var Name: String;
        var ReadMode: TRVFReadMode; var ReadState: TRVFReadState): Boolean; override;
      procedure SaveRVF(Stream: TStream; RVData: TPersistent; ItemNo, ParaNo: Integer;
        const Name: String; Part: TRVMultiDrawItemPart;
        ForceSameAsPrev: Boolean); override;
      procedure SaveRTF(Stream: TStream; RVData: TPersistent; ItemNo: Integer;
        const Name: String; TwipsPerPixel: Double; Level: Integer; ColorList: TRVColorList;
        StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
        FontTable: TRVList); override;
      function PrintToBitmap(Bkgnd: TBitmap; Preview: Boolean; RichView: TRVScroller;
        dli: TRVDrawLineInfo; Part: Integer; ColorMode: TRVColorMode):Boolean; override;
      procedure Print(Canvas: TCanvas; x,y,x2: Integer; Preview, Correction: Boolean;
        const sad: TRVScreenAndDevice; RichView: TRVScroller; dli: TRVDrawLineInfo;
        Part: Integer; ColorMode: TRVColorMode; RVData: TPersistent); override;
      function GetImageHeight(RVStyle: TRVStyle): Integer; override;
      function GetImageWidth(RVStyle: TRVStyle): Integer; override;
      procedure MovingToUndoList(ItemNo: Integer; RVData, AContainerUndoItem: TObject); override;
      function CreatePrintingDrawItem(RVData: TObject; const sad: TRVScreenAndDevice): TRVDrawLineInfo; override;
      property MinHeightOnPage: Integer read FMinHeightOnPage write FMinHeightOnPage;
  end;

  TRVGraphicItemInfoClass = class of TRVGraphicItemInfo;

  TRVHotGraphicItemInfo = class(TRVGraphicItemInfo)
    public
      function GetBoolValueEx(Prop: TRVItemBoolPropertyEx; RVStyle: TRVStyle): Boolean; override;
      constructor CreateEx(RVData: TPersistent; AImage: TGraphic; AVAlign: TRVVAlign); override;
      procedure Execute(RVData:TPersistent); override;
  end;

  TRVBulletItemInfo = class (TRVRectItemInfo)
    protected
      function GetHeight: Integer; override;
      function GetWidth: Integer; override;
      function GetImageIndex(Hot: Boolean): Integer; virtual;
      function SaveRVFHeaderTail(RVData: TPersistent): String; override;
      function GetRVFExtraPropertyCount: Integer; override;
      procedure SaveRVFExtraProperties(Stream: TStream); override;      
    public
      ImageList: TCustomImageList;
      ImageIndex: Integer;
      NoHTMLImageSize: Boolean;
      constructor CreateEx(RVData: TPersistent; AImageIndex: Integer;
        AImageList: TCustomImageList; AVAlign: TRVVAlign);
      procedure SaveToHTML(Stream: TStream; RVData: TPersistent; ItemNo: Integer;
        const Text, Path: String; const imgSavePrefix: String;
        var imgSaveNo: Integer; CurrentFileColor: TColor;
        SaveOptions: TRVSaveOptions; UseCSS: Boolean; Bullets: TRVList); override;
      procedure Assign(Source: TCustomRVItemInfo); override;
      function GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas;
        RVData: TPersistent): Integer; override;
      procedure Paint(x,y: Integer; Canvas: TCanvas; State: TRVItemDrawStates;
        Style: TRVStyle; dli: TRVDrawLineInfo); override;
      function PrintToBitmap(Bkgnd: TBitmap; Preview: Boolean; RichView: TRVScroller;
        dli: TRVDrawLineInfo; Part: Integer; ColorMode: TRVColorMode):Boolean; override;
      function ReadRVFHeader(var P: PChar; RVData: TPersistent): Boolean; override;
      function ReadRVFLine(const s: String; RVData: TPersistent;
        ReadType, LineNo, LineCount: Integer; var Name: String;
        var ReadMode: TRVFReadMode; var ReadState: TRVFReadState): Boolean; override;
      procedure SaveRVF(Stream: TStream; RVData: TPersistent; ItemNo, ParaNo: Integer;
        const Name: String; Part: TRVMultiDrawItemPart;
        ForceSameAsPrev: Boolean); override;
      procedure SaveRTF(Stream: TStream; RVData: TPersistent; ItemNo: Integer;
        const Name: String; TwipsPerPixel: Double; Level: Integer; ColorList: TRVColorList;
        StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
        FontTable: TRVList); override;
      function GetBoolValue(Prop: TRVItemBoolProperty): Boolean; override;
      function SetExtraIntProperty(Prop: TRVExtraItemProperty;
        Value: Integer): Boolean; override;
      function GetExtraIntProperty(Prop: TRVExtraItemProperty;
        var Value: Integer): Boolean; override;
      function GetImageHeight(RVStyle: TRVStyle): Integer; override;
      function GetImageWidth(RVStyle: TRVStyle): Integer; override;
  end;

  TRVHotspotItemInfo = class (TRVBulletItemInfo)
    protected
      function GetImageIndex(Hot: Boolean): Integer; override;
      function SaveRVFHeaderTail(RVData: TPersistent): String; override;
    public
      HotImageIndex: Integer;
      constructor CreateEx(RVData: TPersistent; AImageIndex, AHotImageIndex: Integer;
                           AImageList: TCustomImageList; AVAlign: TRVVAlign);
      procedure Assign(Source: TCustomRVItemInfo); override;
      function ReadRVFHeader(var P: PChar; RVData: TPersistent): Boolean; override;
      procedure SaveRVF(Stream: TStream; RVData: TPersistent; ItemNo, ParaNo: Integer;
                        const Name: String; Part: TRVMultiDrawItemPart;
                        ForceSameAsPrev: Boolean); override;
      function GetBoolValueEx(Prop: TRVItemBoolPropertyEx; RVStyle: TRVStyle): Boolean;override;
      procedure Execute(RVData:TPersistent);override;
  end;

  TRVBreakItemInfo = class (TRVFullLineItemInfo)
    protected
      function SaveRVFHeaderTail(RVData: TPersistent): String; override;
    public
      LineWidth: Byte;
      Style: TRVBreakStyle;
      Color: TColor;
      constructor CreateEx(RVData: TPersistent; ALineWidth: Byte; AStyle: TRVBreakStyle; AColor: TColor);
      procedure Assign(Source: TCustomRVItemInfo); override;
      procedure PaintFullWidth(Left, Right, Top: Integer; Canvas: TCanvas;
                      State: TRVItemDrawStates;
                      Style: TRVStyle; const ClipRect: TRect;
                      dli: TRVDrawLineInfo); override;
      procedure Print(Canvas: TCanvas; x,y,x2: Integer; Preview, Correction: Boolean;
        const sad: TRVScreenAndDevice; RichView: TRVScroller; dli: TRVDrawLineInfo;
        Part: Integer; ColorMode: TRVColorMode; RVData: TPersistent); override;
      function AsText(LineWidth: Integer;
                           RVData: TPersistent;
                           const Text, Path: String;
                           TextOnly,Unicode: Boolean): String; override;
      procedure SaveToHTML(Stream: TStream; RVData: TPersistent;
                           ItemNo: Integer;
                           const Text, Path: String;
                           const imgSavePrefix: String;
                           var imgSaveNo: Integer;
                           CurrentFileColor: TColor;
                           SaveOptions: TRVSaveOptions;
                           UseCSS: Boolean;
                           Bullets: TRVList); override;
      function ReadRVFHeader(var P: PChar; RVData: TPersistent): Boolean; override;
      procedure SaveRVF(Stream: TStream; RVData: TPersistent; ItemNo, ParaNo: Integer;
                        const Name: String; Part: TRVMultiDrawItemPart;
                        ForceSameAsPrev: Boolean); override;
      procedure SaveRTF(Stream: TStream; RVData: TPersistent; ItemNo: Integer;
        const Name: String; TwipsPerPixel: Double; Level: Integer; ColorList: TRVColorList;
        StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
        FontTable: TRVList); override;
      procedure FillRTFTables(ColorList: TRVColorList; ListOverrideCountList: TRVIntegerList;
        RVData: TPersistent); override;
      function GetBoolValue(Prop: TRVItemBoolProperty): Boolean; override;
      function GetBoolValueEx(Prop: TRVItemBoolPropertyEx; RVStyle: TRVStyle): Boolean;override;

  end;

  function RV_DuplicateItem(Source: TCustomRVItemInfo;
    RVData: TPersistent; DuplicateCheckpoint: Boolean): TCustomRVItemInfo;
  procedure RegisterRichViewItemClass(StyleNo: Integer; ItemClass: TCustomRVItemInfoClass);
  function CreateRichViewItem(StyleNo: Integer; RVData: TPersistent): TCustomRVItemInfo;
  function RVFGetItemOptions(ItemOptions: TRVItemOptions; ForceSameAsPrev: Boolean): TRVItemOptions;
  procedure RVSaveImageToRTF(Stream: TStream; TwipsPerPixel: Double;
    Image: TGraphic; ImageWidth, ImageHeight: Integer; Options: TRVRTFOptions);
  procedure RVSaveImageListImageToRTF(Stream: TStream;
    TwipsPerPixel: Double; ImageList: TCustomImageList; ImageIndex: Integer;
    RTFOptions: TRVRTFOptions);
  procedure RVSaveImageSharedImageInHTML(ImageList: TCustomImageList;
    ImageIndex: Integer; Graphic: TGraphic; var Location: String;
    RVData: TPersistent; const Path,
    imgSavePrefix: String; var imgSaveNo: Integer; CurrentFileColor: TColor;
    SaveOptions: TRVSaveOptions;
    Bullets: TRVList);
  function RV_GetExtraIMGStr(SaveOptions: TRVSaveOptions; Width, Height: Integer;
    NoHTMLImageSize: Boolean): String;

var
  RichViewTextItemClass: TRVTextItemInfoClass;

const
  RVItemOptionsMask = $0F;

procedure WriteRVFExtraIntPropertyStr(Stream: TStream; Prop: TRVExtraItemProperty;
  Value: Integer);
procedure WriteRVFExtraStrPropertyStr(Stream: TStream; Prop: TRVExtraItemStrProperty;
  const Value: String);  

implementation
uses RVFMisc,RichView, PtblRV, PtRVData, CRVData, CRVFData, RVUni, RVStr, CtrlImg
     {$IFNDEF RVDONOTUSELISTS}
     , RVMarker
     {$ENDIF}
     ;
procedure RichView_InitializeList; forward;
{============================= TRVCPInfo ======================================}
procedure TRVCPInfo.Assign(Source: TRVCPInfo; TagsArePChars: Boolean);
begin
  Name       := Source.Name;
  RaiseEvent := Source.RaiseEvent;
  Tag        := RV_CopyTag(Source.Tag, TagsArePChars);
  Persistent := Source.Persistent;
  //DrawItemNo, Next, Prev, ItemInfo are not copied
end;
{------------------------------------------------------------------------------}
function TRVCPInfo.CreateCopy(TagsArePChars: Boolean): TRVCPInfo;
begin
  Result := TRVCPInfo.Create;
  Result.Assign(Self,TagsArePChars);
end;
{========================= TRVImagePrintPart ==================================}
type
  TRVImagePrintPart = class (TRVMultiDrawItemPart)
    public
      ImgTop, ImgHeight: Integer;
      function GetSoftPageBreakInfo: Integer; override;
  end;

function TRVImagePrintPart.GetSoftPageBreakInfo: Integer;
begin
  Result := ImgTop;
end;
{========================= TRVMultiImagePrintInfo =============================}
type
  TRVMultiImagePrintInfo = class (TRVMultiDrawItemInfo)
    private
      sad: TRVScreenAndDevice;
      FItem: TRVRectItemInfo;
    public
      constructor Create(AItem: TRVRectItemInfo);
      procedure SetSize(AWidth, AHeight: Integer); override;
      function InitSplit: Boolean; override;
      function CanSplitFirst(Y: Integer): Boolean; override;
      function SplitAt(Y: Integer): Boolean; override;
  end;
{------------------------------------------------------------------------------}
constructor TRVMultiImagePrintInfo.Create(AItem: TRVRectItemInfo);
begin
  inherited Create;
  FItem := AItem;
end;
{------------------------------------------------------------------------------}
function TRVMultiImagePrintInfo.CanSplitFirst(Y: Integer): Boolean;
begin
  Y := MulDiv(Y, sad.ppiyScreen, sad.ppiyDevice);
  Result :=
    (Y>0) and
    FItem.GetBoolValueEx(rvbpPrintToBMP, nil) and
    (FItem.FMinHeightOnPage>0) and
    (Y>=FItem.FMinHeightOnPage) and
    ((FItem.GetImageHeight(nil)-Y >= FItem.FMinHeightOnPage) or
     (Y>FItem.GetImageHeight(nil)));
end;
{------------------------------------------------------------------------------}
function TRVMultiImagePrintInfo.InitSplit: Boolean;
var part: TRVImagePrintPart;
begin
  Result := FItem.FMinHeightOnPage<>0;
  if not Result then
    exit;
  part := TRVImagePrintPart.Create;
  part.ImgTop := 0;
  part.ImgHeight := FItem.GetImageHeight(nil);
  part.Height := MulDiv(part.ImgHeight, sad.ppiyDevice, sad.ppiyScreen);
  PartsList.Add(part);
end;
{------------------------------------------------------------------------------}
function TRVMultiImagePrintInfo.SplitAt(Y: Integer): Boolean;
var PrevHeight, NewHeight, PrevTop: Integer;
    part: TRVImagePrintPart;
begin
  if FItem.FMinHeightOnPage<=0 then begin
    Result := False;
    exit;
  end;
  if PartsList.Count=0 then
    raise ERichViewError.Create(errPrint);
  part := TRVImagePrintPart(PartsList[PartsList.Count-1]);
  if (part.ImgHeight<=FItem.FMinHeightOnPage) then begin
    Result := False;
    exit;
  end;
  PrevHeight := MulDiv(Y, sad.ppiyScreen, sad.ppiyDevice);
  NewHeight := part.ImgHeight-PrevHeight;
  if (NewHeight<FItem.FMinHeightOnPage) or (PrevHeight<FItem.FMinHeightOnPage) then begin
    Result := False;
    exit;
  end;
  part.ImgHeight := PrevHeight;
  part.Height := MulDiv(part.ImgHeight, sad.ppiyDevice, sad.ppiyScreen);
  PrevTop := part.ImgTop;

  part := TRVImagePrintPart.Create;
  part.ImgTop := PrevTop+PrevHeight;
  part.ImgHeight := NewHeight;
  part.Height := MulDiv(part.ImgHeight, sad.ppiyDevice, sad.ppiyScreen);
  PartsList.Add(part);
  Result := True;
end;
{------------------------------------------------------------------------------}
procedure TRVMultiImagePrintInfo.SetSize(AWidth, AHeight: Integer);
begin
  // do nothing
end;
{==============================================================================}
type
  TRichViewItemTypeInfo = class
    public
      StyleNo: Integer;
      ItemClass: TCustomRVItemInfoClass;
  end;

const RichViewItemClassesList: TList = nil;
{------------------------------------------------------------------------------}
const RVFExtraItemIntPropNames: array [TRVExtraItemProperty] of String =
  ('', 'vshift', 'vshiftabs', 'width', 'height', 'transparent', 'tmode',
   'tcolor', 'minheightonpage', 'spacing', 'resizable', 'unremovable',
   'nohtmlsize');
const RVFExtraItemStrPropNames: array [TRVExtraItemStrProperty] of String =
  ('', 'hint', 'alt', 'filename');

function GetRVFExtraIntPropertyByName(const PropName: String):TRVExtraItemProperty;
var i: TRVExtraItemProperty;
begin
  Result := rvepUnknown;
  for i := Low(TRVExtraItemProperty) to High(TRVExtraItemProperty) do
    if RVFExtraItemIntPropNames[i]=PropName then begin
      Result := i;
      exit;
    end;
end;
{------------------------------------------------------------------------------}
function GetRVFExtraStrPropertyByName(const PropName: String):TRVExtraItemStrProperty;
var i: TRVExtraItemStrProperty;
begin
  Result := rvespUnknown;
  for i := Low(TRVExtraItemStrProperty) to High(TRVExtraItemStrProperty) do
    if RVFExtraItemStrPropNames[i]=PropName then begin
      Result := i;
      exit;
    end;
end;
{------------------------------------------------------------------------------}
procedure WriteRVFExtraIntPropertyStr(Stream: TStream; Prop: TRVExtraItemProperty;
  Value: Integer);
begin
  RVFWriteLine(Stream, Format('%s=%d', [RVFExtraItemIntPropNames[Prop], Value]));
end;
{------------------------------------------------------------------------------}
procedure WriteRVFExtraStrPropertyStr(Stream: TStream; Prop: TRVExtraItemStrProperty;
  const Value: String);
begin
  RVFWriteLine(Stream, Format('%s=%s', [RVFExtraItemStrPropNames[Prop], Value]));
end;
{------------------------------------------------------------------------------}
procedure RegisterRichViewItemClass(StyleNo: Integer; ItemClass: TCustomRVItemInfoClass);
var i: Integer;
    item: TRichViewItemTypeInfo;
begin
  if RichViewItemClassesList=nil then
    RichView_InitializeList;
  if StyleNo>=-9 then
    raise ERichViewError.Create(errRVItemReg2);
  for i := 0 to RichViewItemClassesList.Count-1 do
    if TRichViewItemTypeInfo(RichViewItemClassesList[i]).StyleNo=StyleNo then begin
      if TRichViewItemTypeInfo(RichViewItemClassesList[i]).ItemClass=ItemClass then
        exit;
      raise ERichViewError.Create(errRVItemReg2);
    end;
  item := TRichViewItemTypeInfo.Create;
  item.StyleNo := StyleNo;
  item.ItemClass := ItemClass;
  RichViewItemClassesList.Add(item);
end;
{------------------------------------------------------------------------------}
function CreateRichViewItem(StyleNo: Integer; RVData: TPersistent): TCustomRVItemInfo;
var i: Integer;
begin
  if StyleNo>=0 then begin
    Result := RichViewTextItemClass.Create(RVData);
    Result.StyleNo := StyleNo;
    exit;
  end;
  Result := nil;
  case StyleNo of
    rvsBullet:
      Result := TRVBulletItemInfo.Create(RVData);
    rvsHotspot:
      Result := TRVHotspotItemInfo.Create(RVData);
    rvsPicture:
      Result := TRVGraphicItemInfo.Create(RVData);
    rvsComponent:
      Result := TRVControlItemInfo.Create(RVData);
    rvsBreak:
      Result := TRVBreakItemInfo.Create(RVData);
    rvsHotPicture:
      Result := TRVHotGraphicItemInfo.Create(RVData);
    {$IFNDEF RVDONOTUSELISTS}
    rvsListMarker:
      Result := TRVMarkerItemInfo.Create(RVData);
    {$ENDIF}
  end;
  if Result<>nil then begin
    Result.StyleNo := StyleNo;
    exit;
  end;
  for i := 0 to RichViewItemClassesList.Count-1 do
    if TRichViewItemTypeInfo(RichViewItemClassesList[i]).StyleNo=StyleNo then begin
      Result := TRichViewItemTypeInfo(RichViewItemClassesList[i]).ItemClass.Create(RVData);
      Result.StyleNo := StyleNo;
      exit;
    end;
  Result := nil;
end;
{------------------------------------------------------------------------------}
procedure RichView_InitializeList;
begin
  if RichViewItemClassesList=nil then
    RichViewItemClassesList := TList.Create;
end;
{------------------------------------------------------------------------------}
procedure RichView_FinalizeList;
var i: Integer;
begin
  for i := 0 to RichViewItemClassesList.Count-1 do
    TRichViewItemTypeInfo(RichViewItemClassesList.Items[i]).Free;
  RichViewItemClassesList.Free;
end;
{==============================================================================}
function RV_DuplicateItem(Source: TCustomRVItemInfo; RVData: TPersistent;
  DuplicateCheckpoint: Boolean): TCustomRVItemInfo;
var TagsArePChars: Boolean;
begin
  TagsArePChars := rvoTagsArePChars in TCustomRVData(RVData).Options;
  Result := TCustomRVItemInfoClass(Source.ClassType).Create(RVData);
  Result.StyleNo := Source.StyleNo;
  Result.Assign(Source);
  if DuplicateCheckpoint and (Source.Checkpoint<>nil) then begin
    Result.Checkpoint := TRVCPInfo.Create;
    Result.Checkpoint.Assign(Source.Checkpoint, TagsArePChars);
  end;
  Result.Tag  := RV_CopyTag(Source.Tag, TagsArePChars)
end;
{------------------------------------------------------------------------------}
function GetHTMLImageAlign(Align: TRVVAlign): String;
begin
  case Align of
    rvvaMiddle:
      Result := ' align=middle';
    else
      Result := '';
  end;
end;
{======================= TCustomRVItemInfo ====================================}
constructor TCustomRVItemInfo.Create;
begin
  inherited Create;
  DrawItemNo := -1;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.Assign(Source: TCustomRVItemInfo);
begin
  StyleNo := Source.StyleNo;
  ParaNo  := Source.ParaNo;
  ItemOptions := Source.ItemOptions;
  {$IFNDEF RVDONOTUSEITEMHINTS}
  Hint    := Source.Hint;
  {$ENDIF}
  // Checkpoint, JumpID and Tag are not assigned
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.ReadRVFHeader(var P: PChar;
                                         RVData: TPersistent): Boolean;
begin
  Result := True;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetSameAsPrev: Boolean;
begin
  Result := (rvioSameAsPrev in ItemOptions);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.SetSameAsPrev(const Value: Boolean);
begin
  if Value then begin
    Exclude(ItemOptions, rvioPageBreakBefore);
    Exclude(ItemOptions, rvioBR);
    Include(ItemOptions , rvioSameAsPrev);
    end
  else
    Exclude(ItemOptions, rvioSameAsPrev);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.SetBR(Value: Boolean);
begin
  if GetBoolValue(rvbpFullWidth) then
    Value := False;
  if Value then begin
    Exclude(ItemOptions, rvioSameAsPrev);
    Include(ItemOptions, rvioBR);
    end
  else
    Exclude(ItemOptions, rvioBR);
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetBR: Boolean;
begin
  Result := (rvioBR in ItemOptions);
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetPageBreakBefore: Boolean;
begin
  Result := (rvioPageBreakBefore in ItemOptions);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.SetPageBreakBefore(const Value: Boolean);
begin
  if Value then begin
    Exclude(ItemOptions, rvioSameAsPrev);
    Exclude(ItemOptions, rvioBR);
    Include(ItemOptions, rvioPageBreakBefore);
    end
  else
    Exclude(ItemOptions, rvioPageBreakBefore);
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.CanBeBorderStart: Boolean;
begin
  Result := not (rvioSameAsPrev in ItemOptions) and
            not (rvioBR in ItemOptions)
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.ParaStart(CountBR: Boolean): Boolean;
begin
  Result := not (rvioSameAsPrev in ItemOptions) and
            (CountBR or not (rvioBR in ItemOptions));
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.UpdatePaletteInfo(PaletteAction: TRVPaletteAction;
  ForceRecreateCopy: Boolean; Palette: HPALETTE; LogPalette: PLogPalette);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.AsImage: TGraphic;
begin
  Result := nil;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.SaveToHTML(Stream: TStream; RVData: TPersistent;
  ItemNo: Integer; const Text, Path: String; const imgSavePrefix: String;
  var imgSaveNo: Integer; CurrentFileColor: TColor;
  SaveOptions: TRVSaveOptions; UseCSS: Boolean; Bullets: TRVList);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.AsText(LineWidth: Integer; RVData: TPersistent;
  const Text, Path: String; TextOnly,Unicode: Boolean): String;
begin
  Result := '';
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.OwnsControl(AControl: TControl): Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetSubRVDataAt(X,Y: Integer): TPersistent;
begin
  Result := nil;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.OwnsInplaceEditor(AEditor: TControl): Boolean;
begin
   Result := False;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.ReadRVFLine(const s: String; RVData: TPersistent;
  ReadType, LineNo, LineCount: Integer; var Name: String;
  var ReadMode: TRVFReadMode; var ReadState: TRVFReadState): Boolean;
begin
  Result := True;
  SetExtraPropertyFromRVFStr(s);
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.SaveRVFHeaderTail(RVData: TPersistent): String;
begin
  // nothing to do here
  Result := '';
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetRVFExtraPropertyCount: Integer;
begin
  Result := 0;
  {$IFNDEF RVDONOTUSEITEMHINTS}
  if Hint<>'' then
    inc(Result);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.SaveRVFExtraProperties(Stream: TStream);
begin
  {$IFNDEF RVDONOTUSEITEMHINTS}
  if Hint<>'' then
    WriteRVFExtraStrPropertyStr(Stream, rvespHint, Hint);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.SetExtraPropertyFromRVFStr(const Str: String);
var PropName, PropValStr: String;
    PropVal: Integer;
    IntProp: TRVExtraItemProperty;
    StrProp: TRVExtraItemStrProperty;
    p: Integer;
begin
  p := Pos('=', Str);
  if p=0 then
    exit;
  PropName := Copy(Str,1,p-1);
  PropValStr := Copy(Str,p+1, Length(Str));
  IntProp := GetRVFExtraIntPropertyByName(PropName);
  if IntProp<>rvepUnknown then begin
    PropVal  := StrToIntDef(PropValStr,0);
    SetExtraIntProperty(IntProp, PropVal);
    end
  else begin
    StrProp := GetRVFExtraStrPropertyByName(PropName);
    if StrProp<>rvespUnknown then begin
      SetExtraStrProperty(StrProp, PropValStr);
      end
    else
      SetExtraCustomProperty(PropName,PropValStr);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.SaveRVF(Stream: TStream; RVData: TPersistent;
  ItemNo, ParaNo: Integer; const Name: String; Part: TRVMultiDrawItemPart;
  ForceSameAsPrev: Boolean);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.SaveRTF(Stream: TStream; RVData: TPersistent;
  ItemNo: Integer; const Name: String; TwipsPerPixel: Double;
  Level: Integer; ColorList: TRVColorList;
  StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
  FontTable: TRVList);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.FillRTFTables(ColorList: TRVColorList;
  ListOverrideCountList: TRVIntegerList; RVData: TPersistent);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.PrintToBitmap(Bkgnd: TBitmap; Preview: Boolean;
  RichView: TRVScroller; dli: TRVDrawLineInfo; Part: Integer;
  ColorMode: TRVColorMode):Boolean;
begin
  // nothing was printed
  Result := False;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.Print(Canvas: TCanvas; x, y, x2: Integer;
  Preview, Correction: Boolean; const sad: TRVScreenAndDevice;
  RichView: TRVScroller; dli: TRVDrawLineInfo;
  Part: Integer; ColorMode: TRVColorMode;
  RVData: TPersistent);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetBoolValue(Prop: TRVItemBoolProperty): Boolean;
begin
  case Prop of
    rvbpFullWidth,rvbpDisplayActiveState,rvbpDrawingChangesFont,
    rvbpCanSaveUnicode,rvbpAlwaysInText,rvbpImmediateControlOwner,
    rvbpResizable, rvbpResizeHandlesOutside, rvbpHasSubRVData,
    rvbpClickSelect, rvbpNoHTML_P:
      Result := False;
    else
      Result := True;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetBoolValueEx(Prop: TRVItemBoolPropertyEx;
  RVStyle: TRVStyle): Boolean;
begin
  case Prop of
    rvbpPrintToBMP:
      Result := True;
    else
      Result := False;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.PaintFullWidth(Left, Right, Top: Integer;
  Canvas: TCanvas; State: TRVItemDrawStates; Style: TRVStyle;
  const ClipRect: TRect; dli: TRVDrawLineInfo);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.Paint(x, y: Integer; Canvas: TCanvas;
  State: TRVItemDrawStates; Style: TRVStyle; dli: TRVDrawLineInfo);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetImageHeight(RVStyle: TRVStyle): Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetImageWidth(RVStyle: TRVStyle): Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetBorderHeight: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetBorderWidth: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.MovingToUndoList(ItemNo: Integer;
  RVData, AContainerUndoItem: TObject);
var s: String;
begin
  s := TCustomRVFormattedData(RVData).GetItemText(ItemNo);
  TCustomRVData(RVData).ItemAction(rviaMovingToUndoList, Self, s, TCustomRVData(RVData));
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.MovingFromUndoList(ItemNo: Integer; RVData: TObject);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.FinalizeUndoGroup;
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.BeforeLoading(FileFormat: TRVLoadFormat);
begin
  // nothing to do here    // currently only fo RVF and RTF
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.AfterLoading(FileFormat: TRVLoadFormat);
begin
  // nothing to do here     // currently only fo RVF and RTF
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.MouseMove(Shift: TShiftState; X, Y, ItemNo: Integer;
  RVData: TObject): Boolean;
var s: String;
begin
  if rvoShowItemHints in TCustomRVData(RVData).Options then begin
    s := TCustomRVData(RVData).GetAbsoluteRootData.GetItemHint(
      TCustomRVData(RVData), ItemNo);
    TCustomRVData(RVData).GetAbsoluteRootData.GetParentControl.Hint := s;
    if s='' then
      Application.CancelHint;
  end;
  Result := False; // default cursor;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y, ItemNo: Integer; RVData: TObject): Boolean;
begin
  Result := False; // default cursor;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y, ItemNo: Integer; RVData: TObject): Boolean;
begin
  Result := False; // default cursor;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas;
                                       RVData: TPersistent): Integer;
begin
  Result := 20; // min width of doc - 20 pixels
  if sad<>nil then
    Result := MulDiv(Result, sad.ppixDevice, sad.ppixScreen);  
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.DeselectPartial;
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.BuildJumps(Left,Top: Integer; var StartJumpNo: Integer;
  jumps: TList);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.Focusing;
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.MoveFocus(GoForward: Boolean;
  var TopLevelRVData: TPersistent; var TopLevelItemNo: Integer): Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.ClearFocus;
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.Execute(RVData: TPersistent);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.AdjustFocusToControl(Control: TControl;
  var TopLevelRVData: TPersistent; var TopLevelItemNo: Integer):Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.PartiallySelected: Boolean;
begin
  Result :=  False;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.CanDeletePartiallySelected: Boolean;
begin
  Result :=  False;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.DeletePartiallySelected;
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.ApplyParaStyleToSubRVDatas(
  ParaNo: Integer;  SelectedOnly: Boolean);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.ApplyParaStyleConversionToSubRVDatas(UserData: Integer;
  SelectedOnly: Boolean);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.ApplyStyleConversionToSubRVDatas(
  UserData: Integer; SelectedOnly: Boolean);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.ApplyStyleConversion(RVData: TPersistent; UserData: Integer);
begin
  ApplyStyleConversionToSubRVDatas(UserData, False);
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.CreatePrintingDrawItem(RVData: TObject;
  const sad: TRVScreenAndDevice): TRVDrawLineInfo;
begin
  Result := TRVDrawLineInfo.Create;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.StartExport;
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.EndExport;
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.Inserting(RVData: TObject; var Text: String; Safe: Boolean);
begin
  if RVData<>nil then
    TCustomRVData(RVData).ItemAction(rviaInserting, Self, Text, TCustomRVData(RVData));
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.Inserted(RVData: TObject; ItemNo: Integer);
var s: String;
begin
  if RVData<>nil then begin
    s := '';
    TCustomRVData(RVData).ItemAction(rviaInserted, Self, s, TCustomRVData(RVData));
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.AfterUndoChangeProperty;
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.BeforeUndoChangeProperty;
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.EnterItem(From: TRVEnterDirection; Coord: Integer): Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetHypertextCursor(RVStyle: TRVStyle): TCursor;
begin
  Result := RVStyle.JumpCursor;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.OnDocWidthChange(DocWidth: Integer;
  dli: TRVDrawLineInfo; Printing: Boolean; Canvas: TCanvas;
  RVData: TPersistent; sad: PRVScreenAndDevice; var HShift, Desc: Integer;
  NoCaching: Boolean);
begin
  HShift := 0;
  Desc := 1;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.MarkStylesInUse(UsedTextStyles, UsedParaStyles, UsedListStyles: TRVIntegerList);
begin
  UsedParaStyles[ParaNo] := 1;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.UpdateStyles(TextStylesShift,
  ParaStylesShift, ListStylesShift: TRVIntegerList);
begin
  dec(ParaNo,ParaStylesShift[ParaNo]-1);
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetSubRVData(var StoreState: TRVStoreSubRVData;
                                         Position: TRVSubRVDataPos): TPersistent;
begin
  Result := nil;
  StoreState := nil;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.ChooseSubRVData(StoreState: TRVStoreSubRVData);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.CleanUpChosen;
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.ResetSubCoords;
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.SetExtraIntProperty(Prop: TRVExtraItemProperty; Value: Integer): Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetExtraIntProperty(Prop: TRVExtraItemProperty; var Value: Integer): Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetExtraStrProperty(Prop: TRVExtraItemStrProperty;
  var Value: String): Boolean;
begin
  Result := False;
  {$IFNDEF RVDONOTUSEITEMHINTS}
  case Prop of
    rvespHint:
      begin
        Value := Hint;
        Result := True;
      end;
  end;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.SetExtraStrProperty(Prop: TRVExtraItemStrProperty;
  const Value: String): Boolean;
begin
  Result := False;
  {$IFNDEF RVDONOTUSEITEMHINTS}
  case Prop of
    rvespHint:
      begin
        Hint := Value;
        Result := True;
      end;
  end;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetSoftPageBreakDY(Data: Integer): Integer;
begin
  Result := Data;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.SetExtraCustomProperty(const PropName,
  Value: String);
begin

end;
{------------------------------------------------------------------------------}
procedure TCustomRVItemInfo.DrawBackgroundForPrinting(Canvas: TCanvas;
  const Rect, FullRect: TRect; ColorMode: TRVColorMode;
  ItemBackgroundLayer: Integer);
begin
  // This method is only for items containing subdocuments, i.e. tables.
  // Used for drawing background in bitmap
  // ItemBackgroundLayer: 0 - do not draw; -1 - draw completely; others - item specific
end;
{------------------------------------------------------------------------------}
function TCustomRVItemInfo.GetActualStyleNo(RVStyle: TRVStyle): Integer;
begin
  Result := StyleNo;
  if Result=rvsDefStyle then begin
    if RVStyle.ParaStyles[ParaNo].DefStyleNo>=0 then
      Result := RVStyle.ParaStyles[ParaNo].DefStyleNo
    else
      Result := 0;
  end;
end;
{============================= TRVNonTextItemInfo =============================}
procedure TRVNonTextItemInfo.AdjustInserted(x, y: Integer; adjusty: Boolean);
begin
  // nothing to do here
end;
{------------------------------------------------------------------------------}
function TRVNonTextItemInfo.GetHeight: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
function TRVNonTextItemInfo.GetWidth: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
function TRVNonTextItemInfo.GetLeftOverhang: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
procedure TRVNonTextItemInfo.Assign(Source: TCustomRVItemInfo);
begin
  if Source is TRVNonTextItemInfo then begin
    DeleteProtect := TRVNonTextItemInfo(Source).DeleteProtect;
  end;
  inherited;
end;
{------------------------------------------------------------------------------}
function TRVNonTextItemInfo.GetExtraIntProperty(Prop: TRVExtraItemProperty;
  var Value: Integer): Boolean;
begin
  case Prop of
    rvepDeleteProtect:
      begin
        Value := ord(DeleteProtect);
        Result := True;
      end;
    else
      Result := inherited GetExtraIntProperty(Prop, Value);
  end;
end;
{------------------------------------------------------------------------------}
function TRVNonTextItemInfo.SetExtraIntProperty(Prop: TRVExtraItemProperty;
  Value: Integer): Boolean;
begin
  case Prop of
    rvepDeleteProtect:
      begin
        DeleteProtect := Value<>0;
        Result := True;
      end;
    else
      Result := inherited SetExtraIntProperty(Prop, Value);
  end;
end;
{------------------------------------------------------------------------------}
function TRVNonTextItemInfo.GetRVFExtraPropertyCount: Integer;
begin
  Result := inherited GetRVFExtraPropertyCount;
  if DeleteProtect then
    inc(Result);
end;
{------------------------------------------------------------------------------}
procedure TRVNonTextItemInfo.SaveRVFExtraProperties(Stream: TStream);
begin
  inherited;
  if DeleteProtect then
    WriteRVFExtraIntPropertyStr(Stream, rvepDeleteProtect, ord(DeleteProtect));
end;
{=========================== TRVFullLineItemInfo ==============================}
function TRVFullLineItemInfo.GetBoolValue(Prop: TRVItemBoolProperty): Boolean;
begin
  case Prop of
    rvbpFullWidth:
      Result := True;
    else
      Result := inherited GetBoolValue(Prop);
  end;
end;
{=============================== TRVRectItemInfo ==============================}
constructor TRVRectItemInfo.Create(RVData: TPersistent);
begin
  inherited Create(RVData);
  Spacing := 1;
end;
{------------------------------------------------------------------------------}
procedure TRVRectItemInfo.Assign(Source: TCustomRVItemInfo);
begin
  if Source is TRVRectItemInfo then begin
    FMinHeightOnPage := TRVRectItemInfo(Source).FMinHeightOnPage;
    VAlign           := TRVRectItemInfo(Source).VAlign;
    VShift           := TRVRectItemInfo(Source).VShift;
    VShiftAbs        := TRVRectItemInfo(Source).VShiftAbs;
    Spacing          := TRVRectItemInfo(Source).Spacing;
  end;
  inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
function TRVRectItemInfo.GetDescent: Integer;
begin
  Result := Spacing;
end;
{------------------------------------------------------------------------------}
function TRVRectItemInfo.GetBorderHeight: Integer;
begin
  Result := Spacing;
end;
{------------------------------------------------------------------------------}
function TRVRectItemInfo.GetBorderWidth: Integer;
begin
  Result := Spacing;
end;
{------------------------------------------------------------------------------}
function TRVRectItemInfo.ReadRVFHeader(var P: PChar;
  RVData: TPersistent): Boolean;
var va: Integer;
begin
  Result := True;
  if not (P^ in [#0, #10, #13]) then begin
    Result := RVFReadInteger(P,va);
    if Result then
      VAlign := TRVVAlign(va);
    end
  else
    VAlign := rvvaBaseLine;
end;
{------------------------------------------------------------------------------}
function TRVRectItemInfo.SaveRVFHeaderTail(RVData: TPersistent): String;
begin
  Result := IntToStr(ord(VAlign));
end;
{------------------------------------------------------------------------------}
function TRVRectItemInfo.GetRVFExtraPropertyCount: Integer;
begin
  Result := inherited GetRVFExtraPropertyCount;
  if VShift<>0 then
    inc(Result);
  if VShiftAbs then
    inc(Result);
  if Spacing<>1 then
    inc(Result);
end;
{------------------------------------------------------------------------------}
procedure TRVRectItemInfo.SaveRVFExtraProperties(Stream: TStream);
begin
  inherited;
  if VShift<>0 then
    WriteRVFExtraIntPropertyStr(Stream, rvepVShift, VShift);
  if VShiftAbs then
    WriteRVFExtraIntPropertyStr(Stream, rvepVShiftAbs, 1);
  if Spacing<>1 then
    WriteRVFExtraIntPropertyStr(Stream, rvepSpacing, Spacing);
end;
{------------------------------------------------------------------------------}
function TRVRectItemInfo.GetVShiftCSS(RVStyle: TRVStyle): String;
begin
  Result := '';
  if (VShift=0) or (GetImageHeight(RVStyle)=0) or (VAlign<>rvvaBaseLine) then
    exit;
  if VShiftAbs then
    Result := Format('vertical-align : %d', [VShift])
  else
    Result := Format('vertical-align : %d', [MulDiv(GetImageHeight(RVStyle),VShift,100)])
end;
{------------------------------------------------------------------------------}
function TRVRectItemInfo.SetExtraIntProperty(Prop: TRVExtraItemProperty; Value: Integer): Boolean;
begin
  Result := True;
  case Prop of
    rvepVShift:
      VShift := Value;
    rvepVShiftAbs:
      VShiftAbs := Value<>0;
    rvepSpacing:
      begin
        Spacing := Value;
        if Spacing<0 then
          Spacing := 0;
      end;
    else
      Result := inherited SetExtraIntProperty(Prop, Value);
  end;
end;
{------------------------------------------------------------------------------}
function TRVRectItemInfo.GetExtraIntProperty(Prop: TRVExtraItemProperty; var Value: Integer): Boolean;
begin
  Result := True;
  case Prop of
    rvepVShift:
      Value := VShift;
    rvepVShiftAbs:
      Value := ord(VShiftAbs);
    rvepSpacing:
      Value := Spacing;
    else
      Result := inherited GetExtraIntProperty(Prop, Value);
  end;
end;
{================================ TRVControlItemInfo ==========================}
constructor TRVControlItemInfo.CreateEx(RVData: TPersistent; AControl: TControl; AVAlign: TRVVAlign);
begin
  inherited Create(RVData);
  StyleNo  := rvsComponent;
  Control  := AControl;
  VAlign   := AVAlign;
end;
{------------------------------------------------------------------------------}
destructor TRVControlItemInfo.Destroy;
begin
  Control.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVControlItemInfo.Assign(Source: TCustomRVItemInfo);
begin
  if Source is TRVControlItemInfo then begin
    PercentWidth := TRVControlItemInfo(Source).PercentWidth;
    FResizable   := TRVControlItemInfo(Source).FResizable;
  end;
  inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
procedure TRVControlItemInfo.AdjustInserted(x, y: Integer; adjusty: Boolean);
begin
  Control.Left := x+Spacing;
  Control.Tag  := y+Spacing;
  if adjusty then
    RV_Tag2Y(Control);
  Control.Visible := True;    
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.GetHeight: Integer;
begin
  Result := Control.Height+Spacing*2;
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.GetImageHeight(RVStyle: TRVStyle): Integer;
begin
  Result := Control.Height;
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.GetWidth: Integer;
begin
  Result := Control.Width+Spacing*2;
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.GetImageWidth(RVStyle: TRVStyle): Integer;
begin
  Result := Control.Width;
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas;
                                        RVData: TPersistent): Integer;
begin
  if PercentWidth<>0 then
    Result := 20
  else
    Result := Control.Width+Spacing*2;
  if sad<>nil then
    Result := MulDiv(Result, sad.ppixDevice, sad.ppixScreen);
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.CreatePrintingDrawItem(RVData: TObject; const sad: TRVScreenAndDevice): TRVDrawLineInfo;
begin
  if not GetBoolValueEx(rvbpPrintToBMP, nil) or (MinHeightOnPage=0) then begin
    Result := TRVDrawLineInfo.Create;
    exit;
  end;
  Result := TRVMultiImagePrintInfo.Create(Self);
  Result.Width  := MulDiv(GetWidth, sad.ppixDevice, sad.ppixScreen);
  Result.Height := MulDiv(GetHeight, sad.ppiyDevice, sad.ppiyScreen);
  TRVMultiImagePrintInfo(Result).sad := sad;
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.PrintToBitmap(Bkgnd: TBitmap; Preview: Boolean;
  RichView: TRVScroller; dli: TRVDrawLineInfo; Part: Integer;
  ColorMode: TRVColorMode): Boolean;
var ctrlbmp: TBitmap;
    Top: Integer;
begin
  Result := False;
  ctrlbmp := nil;
  if Assigned(TPrintableRV(RichView).RVPrint.OnPrintComponent) then
    TPrintableRV(RichView).RVPrint.OnPrintComponent(TPrintableRV(RichView).RVPrint, Control, ctrlbmp)
  else
    ctrlbmp := DrawControl(Control);
  if (dli is TRVMultiImagePrintInfo) and (Part>=0) then
    Top := -TRVImagePrintPart(TRVMultiImagePrintInfo(dli).PartsList[Part]).ImgTop
  else
    Top := 0;
  if (ctrlbmp<>nil) then
    try
      Result := True;
      if MinHeightOnPage<=0 then begin
        if ctrlbmp.Width>bkgnd.Width then
          bkgnd.Width := ctrlbmp.Width;
        if ctrlbmp.Height>bkgnd.Height then
          bkgnd.Height := ctrlbmp.Height;
        Bkgnd.Canvas.Draw((Bkgnd.Width-ctrlbmp.Width) div 2,
          (Bkgnd.Height-ctrlbmp.Height) div 2, ctrlbmp);
        end
      else
        Bkgnd.Canvas.Draw(0, Top, ctrlbmp);
    finally
      ctrlbmp.Free;
    end;
end;
{------------------------------------------------------------------------------}
procedure TRVControlItemInfo.Paint(x, y: Integer; Canvas: TCanvas;
  State: TRVItemDrawStates; Style: TRVStyle; dli: TRVDrawLineInfo);
begin
  if (rvidsCurrent in State) and (Style.CurrentItemColor<>clNone) then begin
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := Style.CurrentItemColor;
    Canvas.Pen.Style := psSolid;
    Canvas.Rectangle(x-1,y-1,
      x+Control.Width+Spacing*2+1, y+Control.Height+Spacing*2+1);
  end;
  if (rvidsSelected in State) then begin
    if rvidsControlFocused in State then
      Canvas.Pen.Color := Style.SelColor
    else
      Canvas.Pen.Color := Style.InactiveSelColor;
    if Canvas.Pen.Color<>clNone then begin
      Canvas.Pen.Width := 1;
      Canvas.Pen.Style := psSolid;
      Canvas.Rectangle(x,y,
        x+Control.Width+Spacing*2, y+Control.Height+Spacing*2);
    end;
  end
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.OwnsControl(AControl: TControl): Boolean;
begin
  Result := (AControl=Control);
end;
{------------------------------------------------------------------------------}
procedure TRVControlItemInfo.SaveToHTML(Stream: TStream; RVData: TPersistent;
  ItemNo: Integer; const Text, Path: String; const imgSavePrefix: String;
  var imgSaveNo: Integer; CurrentFileColor: TColor; SaveOptions: TRVSaveOptions;
  UseCSS: Boolean; Bullets: TRVList);
var s: String;
begin
  s := TCustomRVData(RVData).SaveComponentToFile(Path, Control, rvsfHTML);
  if s<>'' then
    RVWrite(Stream,s);
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.ReadRVFLine(const s: String;
  RVData: TPersistent; ReadType, LineNo, LineCount: Integer;
  var Name: String; var ReadMode: TRVFReadMode;
  var ReadState: TRVFReadState): Boolean;
var ControlClass: TControlClass;
begin
  Result := True;
  case ReadType of
    1: // ask owner
      begin
        case LineNo of
          0:
            begin
              Name := s;
              Control := TCustomRVData(RVData).RVFControlNeeded(s, Tag);
              Control.Visible := False;
              Control.Parent := TCustomRVData(RVData).GetParentControl;
            end;
          else
            begin
              SetExtraPropertyFromRVFStr(s);
            end;
        end;
      end;
    else // read from file
      begin
        if LineNo=0 then
          Name := s
        else if LineNo=1 then begin
          ControlClass := TControlClass(GetClass(s));
          if ControlClass<>nil then begin
            Control := ControlClass.Create(nil);
            Control.Visible := False;
            Control.Parent := TCustomRVData(RVData).GetParentControl;
          end;
          end
        else if LineNo=LineCount-1 then begin
          if ReadType=2 then
            RVFLoadControlBinary(s, TComponent(Control), '', TCustomRVData(RVData).GetParentControl)
          else
            Result := RVFLoadControl(s, TComponent(Control), '', TCustomRVData(RVData).GetParentControl);
          if Result then
            if Control=nil then begin
              TCustomRVData(RVData).RVFWarnings := TCustomRVData(RVData).RVFWarnings + [rvfwUnknownCtrls];
              if not (rvfoIgnoreUnknownCtrls in TCustomRVData(RVData).RVFOptions) then
                Result := False;
            end;
          ReadState := rstSkip;
          end
        else
          SetExtraPropertyFromRVFStr(s);
        if (ReadType=2) and (LineNo=LineCount-2) then
            ReadMode := rmBeforeBinary;
      end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.GetBoolValue(Prop: TRVItemBoolProperty): Boolean;
begin
  case Prop of
    rvbpValid:
      Result := Control<>nil;
    rvbpDisplayActiveState,rvbpImmediateControlOwner:
      Result := True;
    rvbpResizable:
      Result := FResizable and (PercentWidth=0);
    rvbpResizeHandlesOutside:
      Result := True;
    else
      Result := inherited GetBoolValue(Prop);
  end;
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.GetBoolValueEx(Prop: TRVItemBoolPropertyEx; RVStyle: TRVStyle): Boolean;
begin
  case Prop of
    rvbpAllowsFocus:
      Result := (Control<>nil) and (Control is TWinControl) and
                TWinControl(Control).TabStop and
                TWinControl(Control).CanFocus;
    rvbpXORFocus:
      Result := False;
    else
      Result := inherited GetBoolValueEx(Prop, RVStyle);
  end;
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.AsText(LineWidth: Integer;
                           RVData: TPersistent;
                           const Text, Path: String;
                           TextOnly,Unicode: Boolean): String;
begin
  Result := TCustomRVData(RVData).SaveComponentToFile(Path, Control, rvsfText);
end;
{------------------------------------------------------------------------------}
procedure TRVControlItemInfo.SaveRTF(Stream: TStream; RVData: TPersistent;
  ItemNo: Integer; const Name: String; TwipsPerPixel: Double;
  Level: Integer; ColorList: TRVColorList;
  StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
  FontTable: TRVList);
var s: String;
begin
  s := TCustomRVData(RVData).SaveComponentToFile('', Control, rvsfRTF);
  if s<>'' then
    RVWrite(Stream,s);
end;
{------------------------------------------------------------------------------}
function RVFGetItemOptions(ItemOptions: TRVItemOptions; ForceSameAsPrev: Boolean): TRVItemOptions;
begin
  Result := ItemOptions;
  if ForceSameAsPrev then begin
    Include(Result, rvioSameAsPrev);
    Exclude(Result, rvioBR);
    Exclude(Result, rvioPageBreakBefore);    
  end;
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.GetRVFExtraPropertyCount: Integer;
begin
  Result := inherited GetRVFExtraPropertyCount;
  if MinHeightOnPage>0 then
    inc(Result);
  if FResizable then
    inc(Result);
end;
{------------------------------------------------------------------------------}
procedure TRVControlItemInfo.SaveRVFExtraProperties(Stream: TStream);
begin
  inherited SaveRVFExtraProperties(Stream);
  if MinHeightOnPage>0 then
    WriteRVFExtraIntPropertyStr(Stream, rvepMinHeightOnPage, MinHeightOnPage);
  if FResizable then
    WriteRVFExtraIntPropertyStr(Stream, rvepResizable, ord(FResizable));  
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.SetExtraIntProperty(Prop: TRVExtraItemProperty; Value: Integer): Boolean;
begin
  case Prop of
    rvepResizable:
      begin
        FResizable := Value<>0;
        Result := True;
      end;
    rvepMinHeightOnPage:
      begin
        MinHeightOnPage := Value;
        Result := True;
      end;
    else
      Result := inherited SetExtraIntProperty(Prop, Value);
  end;
end;
{------------------------------------------------------------------------------}
function TRVControlItemInfo.GetExtraIntProperty(Prop: TRVExtraItemProperty; var Value: Integer): Boolean;
begin
  case Prop of
    rvepResizable:
      begin
        Value := ord(FResizable);
        Result := True;
      end;
    rvepMinHeightOnPage:
      begin
        Value := MinHeightOnPage;
        Result := True;
      end;
    else
      Result := inherited GetExtraIntProperty(Prop, Value);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVControlItemInfo.SaveRVF(Stream: TStream;
                                     RVData: TPersistent;
                                     ItemNo, ParaNo: Integer;
                                     const Name: String;
                                     Part: TRVMultiDrawItemPart;
                                     ForceSameAsPrev: Boolean);
var SaveType, LineCount: Integer;
begin
   if rvfoSaveControlsBody in TCustomRVData(RVData).RVFOptions then begin
     if rvfoSaveBinary in TCustomRVData(RVData).RVFOptions then
       SaveType := 2 // save binary
     else
       SaveType := 0; // save hex dump
     LineCount := 3+GetRVFExtraPropertyCount;
     end
   else begin
     SaveType := 1; // do not save
     LineCount := 1+GetRVFExtraPropertyCount;
   end;
   RVFWriteLine(Stream,
     Format('%d %d %s %d %d %s %s',
            [StyleNo, LineCount,
             RVFItemSavePara(ParaNo, TCustomRVData(RVData), ForceSameAsPrev),
             Byte(RVFGetItemOptions(ItemOptions, ForceSameAsPrev)) and RVItemOptionsMask,
             SaveType,
             RVFSaveTag(rvoTagsArePChars in TCustomRVData(RVData).Options,Tag),
             SaveRVFHeaderTail(RVData)]));
   RVFWriteLine(Stream, Name);
   if SaveType<>1 then begin
     RVFWriteLine(Stream, Control.ClassName);
     SaveRVFExtraProperties(Stream);
     TCustomRVData(RVData).ControlAction(rvcaBeforeRVFSave, ItemNo, Self);
     if rvfoSaveBinary in TCustomRVData(RVData).RVFOptions then
       RVFSaveControlBinary(Stream, Control)
     else
       RVFWriteLine(Stream, RVFSaveControl(Control));
     TCustomRVData(RVData).ControlAction(rvcaAfterRVFSave, ItemNo, Self);
     end
   else
      SaveRVFExtraProperties(Stream);
end;
{------------------------------------------------------------------------------}
procedure TRVControlItemInfo.MovingToUndoList(ItemNo: Integer; RVData, AContainerUndoItem: TObject);
begin
  Control.Parent := nil;
  TCustomRVData(RVData).ControlAction(rvcaMoveToUndoList, ItemNo, Self);
  inherited MovingToUndoList(ItemNo, RVData, AContainerUndoItem);
end;
{------------------------------------------------------------------------------}
procedure TRVControlItemInfo.MovingFromUndoList(ItemNo: Integer; RVData: TObject);
begin
  TCustomRVData(RVData).ControlAction(rvcaMoveFromUndoList, ItemNo, Self);
  Control.Parent := TCustomRVData(RVData).GetParentControl;
end;
{------------------------------------------------------------------------------}
procedure TRVControlItemInfo.Inserting(RVData: TObject; var Text: String; Safe: Boolean);
begin
  Control.Visible := False;
  if not Safe and (RVData<>nil) then
    Control.Parent := TCustomRVData(RVData).GetParentControl
  else
    Control.Parent := nil;
  inherited Inserting(RVData, Text, Safe);
end;
{------------------------------------------------------------------------------}
procedure TRVControlItemInfo.Focusing;
begin
  if Control is TWinControl then
    TWinControl(Control).SetFocus;
end;
{------------------------------------------------------------------------------}
procedure TRVControlItemInfo.OnDocWidthChange(DocWidth: Integer;
  dli: TRVDrawLineInfo; Printing: Boolean; Canvas: TCanvas;
  RVData: TPersistent; sad: PRVScreenAndDevice;
  var HShift, Desc: Integer; NoCaching: Boolean);
begin
  if (PercentWidth>0) and (PercentWidth<=100) then
    Control.Width := DocWidth * PercentWidth div 100-2;
  HShift := 0;
  Desc := GetDescent;
end;
{============================ TRVGraphicItemInfo ==============================}
constructor TRVGraphicItemInfo.CreateEx(RVData: TPersistent; AImage: TGraphic; AVAlign: TRVVAlign);
begin
  inherited Create(RVData);
  StyleNo := rvsPicture;
  Image   := AImage;
  VAlign  := AVAlign;
end;
{------------------------------------------------------------------------------}
destructor TRVGraphicItemInfo.Destroy;
begin
  Image.Free;
  ImageCopy.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVGraphicItemInfo.Assign(Source: TCustomRVItemInfo);
var  grclass: TGraphicClass;
begin
  if Source is TRVGraphicItemInfo then begin
    Alt := TRVGraphicItemInfo(Source).Alt;
    ImageFileName := TRVGraphicItemInfo(Source).ImageFileName;
    ImageWidth := TRVGraphicItemInfo(Source).ImageWidth;
    ImageHeight := TRVGraphicItemInfo(Source).ImageHeight;
    NoHTMLImageSize := TRVGraphicItemInfo(Source).NoHTMLImageSize;
    Image.Free;
    ImageCopy.Free;
    grclass := TGraphicClass(TRVGraphicItemInfo(Source).Image.ClassType);
    Image := RV_CreateGraphics(grclass);
    Image.Assign(TRVGraphicItemInfo(Source).Image);
    if TRVGraphicItemInfo(Source).ImageCopy<>nil then begin
      grclass := TGraphicClass(TRVGraphicItemInfo(Source).ImageCopy.ClassType);
      ImageCopy := RV_CreateGraphics(grclass);
      ImageCopy.Assign(TRVGraphicItemInfo(Source).ImageCopy);
      end
    else
      ImageCopy := nil;
  end;
  inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.GetHeight: Integer;
begin
  if (ImageHeight>0) then
    Result := ImageHeight
  else
    Result := Image.Height;
  inc(Result,Spacing*2);
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.GetImageHeight(RVStyle: TRVStyle): Integer;
begin
  if (ImageHeight>0) then
    Result := ImageHeight
  else
    Result := Image.Height;
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.GetWidth: Integer;
begin
  if (ImageWidth>0) then
    Result := ImageWidth
  else
    Result := Image.Width;
  inc(Result, Spacing*2);
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.GetImageWidth(RVStyle: TRVStyle): Integer;
begin
  if (ImageWidth>0) then
    Result := ImageWidth
  else
    Result := Image.Width;
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas;
                                        RVData: TPersistent): Integer;
begin
  if (ImageWidth>0) then
    Result := ImageWidth
  else
    Result := Image.Width;
  inc(Result, Spacing*2);
  if sad<>nil then
    Result := MulDiv(Result, sad.ppixDevice, sad.ppixScreen);
end;
{------------------------------------------------------------------------------}
procedure TRVGraphicItemInfo.Paint(x, y: Integer; Canvas: TCanvas;
                                   State: TRVItemDrawStates;
                                   Style: TRVStyle;
                                   dli: TRVDrawLineInfo);
var w,h: Integer;
  {...............................................}
  procedure DrawBmp;
  begin
    if (ImageWidth=0) and (ImageHeight=0) then
      BitBlt(Canvas.Handle, x, y,
             ImageCopy.Width, ImageCopy.Height,
             TBitmap(ImageCopy).Canvas.Handle, 0, 0, SRCCOPY)
    else
      StretchBlt(Canvas.Handle, x, y, w, h,
             TBitmap(ImageCopy).Canvas.Handle, 0, 0,
             ImageCopy.Width, ImageCopy.Height, SRCCOPY);
  end;
  {...............................................}
  procedure DrawImage(Image: TGraphic);
  var DCState: Integer;
  begin
    DCState := 0;
    try
      if (ImageWidth=0) and (ImageHeight=0) then begin
        if Image is TMetafile then begin
          DCState := SaveDC(Canvas.Handle);
          IntersectClipRect(Canvas.Handle, x, y, x+Image.Width, y+Image.Height);
        end;
        try
          Canvas.Draw(x, y, Image);
        except
        end;
        end
      else begin
        if Image is TMetafile then begin
          DCState := SaveDC(Canvas.Handle);
          IntersectClipRect(Canvas.Handle, x, y, x+w, y+h);
        end;
        try
          Canvas.StretchDraw(Bounds(x, y, w, h), Image);
        except
        end;
      end;
    finally
      if DCState<>0 then
        RestoreDC(Canvas.Handle, DCState);
    end;
  end;
  {...............................................}
begin
  w := GetImageWidth(Style);
  h := GetImageHeight(Style);
  inc(x, Spacing); inc(y, Spacing);
  if ImageCopy<>nil then
    if ImageCopy is TBitmap then
      DrawBmp
    else
      DrawImage(ImageCopy)
  else
    DrawImage(Image);
  if (rvidsCurrent in State) and (Style.CurrentItemColor<>clNone) then begin
    Canvas.Pen.Width := 1;
    Canvas.Pen.Color := Style.CurrentItemColor;
    Canvas.Pen.Style := psSolid;
    Canvas.Brush.Style := bsClear;
    Canvas.Rectangle(x-Spacing-1,y-Spacing-1, x+w+Spacing+1, y+h+Spacing+1);
  end;
  if (rvidsSelected in State) then begin
    if rvidsControlFocused in State then
      Canvas.Pen.Color := Style.SelColor
    else
      Canvas.Pen.Color := Style.InactiveSelColor;
    Canvas.Brush.Style := bsClear;
    if Canvas.Pen.Color<>clNone then begin
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Width := 1;
      Canvas.Rectangle(x-Spacing,y-Spacing, x+w+Spacing, y+h+Spacing);
    end;
  end
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.AsImage: TGraphic;
begin
  Result := Image;
end;
{------------------------------------------------------------------------------}
procedure TRVGraphicItemInfo.MovingToUndoList(ItemNo: Integer; RVData, AContainerUndoItem: TObject);
begin
  ImageCopy.Free;
  ImageCopy := nil;
  inherited MovingToUndoList(ItemNo, RVData, AContainerUndoItem);  
end;
{------------------------------------------------------------------------------}
procedure TRVGraphicItemInfo.UpdatePaletteInfo(PaletteAction: TRVPaletteAction;
                                               ForceRecreateCopy: Boolean;
                                               Palette: HPALETTE;
                                               LogPalette: PLogPalette);
begin
  if not (PaletteAction in [rvpaCreateCopies,rvpaCreateCopiesEx]) or ForceRecreateCopy or
     (Palette=0) then begin
    ImageCopy.Free;
    ImageCopy := nil;
  end;
//  if ImageCopy=nil then
//    ImageCopy := TBitmap.Create;
//  ImageCopy.Width  := Image.Width;
//  ImageCopy.Height := Image.Height;
//  TBitmap(ImageCopy).Canvas.Draw(0,0,Image);
  case PaletteAction of
  {*} rvpaAssignPalette:
      begin
        if LogPalette<>nil then
          RV_SetPaletteToPicture(Image,LogPalette);
      end;
  {*} rvpaCreateCopies,rvpaCreateCopiesEx:
      begin
        if (LogPalette<>nil) and (ImageCopy=nil) then begin
          {$IFNDEF RVDONOTUSEJPEGIMAGE}
          if (PaletteAction=rvpaCreateCopiesEx) and
             (Image is TJpegImage) then
            ImageCopy := TBitmap.Create
          else
          {$ENDIF}
            ImageCopy := RV_CreateGraphics(TGraphicClass(Image.ClassType));
          ImageCopy.Assign(Image);
          RV_SetPaletteToPicture(ImageCopy,LogPalette);
          if ImageCopy is TBitmap then
            TBitmap(ImageCopy).IgnorePalette := True;
        end;
      end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.GetBoolValue(Prop: TRVItemBoolProperty): Boolean;
begin
  case Prop of
    rvbpResizable:
      Result := (Image<>nil) and not (Image is TIcon);
    rvbpValid:
      Result := Image<>nil;
    rvbpDisplayActiveState, rvbpDrawingChangesFont:
      Result := True;
    else
      Result := inherited GetBoolValue(Prop);
  end;
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.GetBoolValueEx(Prop: TRVItemBoolPropertyEx; RVStyle: TRVStyle): Boolean;
begin
  case Prop of
    rvbpPrintToBMP:
      Result := not (Image is TMetafile);
    else
      Result := inherited GetBoolValueEx(Prop, RVStyle);
  end;
end;
{------------------------------------------------------------------------------}
function RV_GetExtraIMGStr(SaveOptions: TRVSaveOptions; Width, Height: Integer;
  NoHTMLImageSize: Boolean): String;
begin
  if rvsoNoHypertextImageBorders in SaveOptions then
    Result := ' border=0 '
  else
    Result := ' ';
  if (rvsoImageSizes in SaveOptions) and not NoHTMLImageSize then
    Result := Result+Format('width=%d height=%d ',[Width, Height]);
end;
{------------------------------------------------------------------------------}
procedure TRVGraphicItemInfo.SaveToHTML(Stream: TStream; RVData: TPersistent;
  ItemNo: Integer; const Text, Path: String; const imgSavePrefix: String;
  var imgSaveNo: Integer; CurrentFileColor: TColor;
  SaveOptions: TRVSaveOptions; UseCSS: Boolean; Bullets: TRVList);
  {.................................................................}
  function GetExtraIMGStr: String;
  var s: String;
      RVStyle: TRVStyle;
  begin
    if rvsoNoHypertextImageBorders in SaveOptions then
      Result := ' border=0 '
    else
      Result := ' ';
    RVStyle := TCustomRVData(RVData).GetRVStyle;
    if ((rvsoImageSizes in SaveOptions) and not NoHTMLImageSize) or
       (ImageWidth>0) or (ImageHeight>0) then
      Result := Result+Format('width=%d height=%d ',
        [GetImageWidth(RVStyle), GetImageHeight(RVStyle)]);
    if (Alt<>'') or UseCSS then
      Result := Result+'alt="'+Alt+'" ';
    {$IFNDEF RVDONOTUSEITEMHINTS}
    if Hint<>'' then
      Result := Result+RV_GetHintStr(rvsfHTML, Hint)+' ';
    {$ENDIF}
    if UseCSS then begin
      s := GetVShiftCSS(RVStyle);
      if s<>'' then
        Result := Result + Format('style="{%s}" ',[s]);
    end;
  end;
  {.................................................................}
var Location: String;
    DoDefault: Boolean;
begin
  if (ImageFileName<>'') and (rvsoUseItemImageFileNames in SaveOptions) then
    Location := ExtractRelativePath(Path, ImageFileName)
  else
    Location := '';
  TCustomRVData(RVData).HTMLSaveImage(TCustomRVData(RVData), ItemNo, Path, CurrentFileColor, Location, DoDefault);
  if DoDefault then
    if (ImageFileName<>'') and (rvsoUseItemImageFileNames in SaveOptions) then
      Location := ExtractRelativePath(Path, ImageFileName)
    else
      Location := TCustomRVData(RVData).DoSavePicture(rvsfHTML, imgSavePrefix, Path,
        imgSaveNo, rvsoOverrideImages in SaveOptions, CurrentFileColor, Image);
  if Location<>'' then
    RVWrite(Stream, Format('<IMG%s%ssrc="%s">',
      [GetHTMLImageAlign(VAlign), GetExtraIMGStr, RV_GetHTMLPath(Location)]));
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.ReadRVFLine(const s: String;
                                        RVData: TPersistent;
                                        ReadType, LineNo, LineCount: Integer;
                                        var Name: String;
                                        var ReadMode: TRVFReadMode;
                                        var ReadState: TRVFReadState): Boolean;
var grcls : TGraphicClass;
begin
  Result := True;
  case ReadType of
    1: // ask user
      begin
        case LineNo of
          0:
            begin
              Image := TCustomRVData(RVData).RVFPictureNeeded(s, Tag);
              Name := s;
            end;
          else
            begin
              SetExtraPropertyFromRVFStr(s);
            end;
        end;
      end;
    else // load picture from file
      begin
        if LineNo=0 then
          Name := s
        else if LineNo=1 then begin
          grcls := TGraphicClass(GetClass(s));
          if grcls=nil then begin
            TCustomRVData(RVData).RVFWarnings := TCustomRVData(RVData).RVFWarnings + [rvfwUnknownPicFmt];
            if not (rvfoIgnoreUnknownPicFmt in TCustomRVData(RVData).RVFOptions) then
              Result := False;
            end
          else begin
            Image := RV_CreateGraphics(grcls);
          end;
          end
        else if LineNo=LineCount-1 then begin
          if Image<>nil then begin
            try
              if ReadType=2 then
                RVFLoadPictureBinary(s, Image)
              else
                Result := RVFLoadPicture(s, Image);
               {$IFNDEF RVDONOTCORRECTWMFSCALE}
                if (Image is TMetafile) {$IFNDEF RVCORRECTWMFSCALE2} and  not TMetafile(Image).Enhanced{$ENDIF} and (TMetafile(Image).Inch=0)  then
                   TMetafile(Image).Inch := 1440;
               {$ENDIF}
            except
              Image.Free;
              Image := RV_CreateGraphics(TGraphicClass(TCustomRVData(RVData).GetRVStyle.InvalidPicture.Graphic.ClassType));
              Image.Assign(TCustomRVData(RVData).GetRVStyle.InvalidPicture.Graphic);
              TCustomRVData(RVData).RVFWarnings := TCustomRVData(RVData).RVFWarnings+[rvfwInvalidPicture];
            end;
          end;
          ReadState := rstSkip;
          end
        else
          SetExtraPropertyFromRVFStr(s);
        if (ReadType=2) and (LineNo=LineCount-2) then
            ReadMode := rmBeforeBinary;          
      end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.GetRVFExtraPropertyCount: Integer;
begin
  Result := inherited GetRVFExtraPropertyCount;
  if ImageWidth>0 then
    inc(Result);
  if ImageHeight>0 then
    inc(Result);
  if MinHeightOnPage>0 then
    inc(Result);
  if NoHTMLImageSize then
    inc(Result);
  {$IFDEF RICHVIEWCBDEF3}
  if (Image<>nil) and (Image is TBitmap) and TBitmap(Image).Transparent then begin
    inc(Result,2);
    if TBitmap(Image).TransparentMode=tmFixed then
      inc(Result);
  end;
  {$ENDIF}
  if ImageFileName<>'' then
    inc(Result);
  if Alt<>'' then
    inc(Result);
end;
{------------------------------------------------------------------------------}
procedure TRVGraphicItemInfo.SaveRVFExtraProperties(Stream: TStream);
begin
  inherited SaveRVFExtraProperties(Stream);
  if ImageWidth>0 then
    WriteRVFExtraIntPropertyStr(Stream, rvepImageWidth, ImageWidth);
  if ImageHeight>0 then
    WriteRVFExtraIntPropertyStr(Stream, rvepImageHeight, ImageHeight);
  if MinHeightOnPage>0 then
    WriteRVFExtraIntPropertyStr(Stream, rvepMinHeightOnPage, MinHeightOnPage);
  if NoHTMLImageSize then
    WriteRVFExtraIntPropertyStr(Stream, rvepNoHTMLImageSize, 1);
  {$IFDEF RICHVIEWCBDEF3}
  if (Image<>nil) and (Image is TBitmap) and TBitmap(Image).Transparent then begin
    WriteRVFExtraIntPropertyStr(Stream, rvepTransparent, 1);
    WriteRVFExtraIntPropertyStr(Stream, rvepTransparentMode, ord(TBitmap(Image).TransparentMode));
    if TBitmap(Image).TransparentMode=tmFixed then
      WriteRVFExtraIntPropertyStr(Stream, rvepTransparentColor, TBitmap(Image).TransparentColor);
  end;
  {$ENDIF}
  if ImageFileName<>'' then
    WriteRVFExtraStrPropertyStr(Stream, rvespImageFileName, ImageFileName);
  if Alt<>'' then
    WriteRVFExtraStrPropertyStr(Stream, rvespAlt, Alt);
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.SetExtraIntProperty(Prop: TRVExtraItemProperty; Value: Integer): Boolean;
begin
  Result := False;
  case Prop of
    rvepImageWidth:
      begin
        ImageWidth := Value;
        Result := True;
      end;
    rvepImageHeight:
      begin
        ImageHeight := Value;
        Result := True;
      end;
    rvepMinHeightOnPage:
      begin
        MinHeightOnPage := Value;
        Result := True;
      end;
    rvepNoHTMLImageSize:
      begin
        NoHTMLImageSize := Value<>0;
        Result := True;
      end;
    {$IFDEF RICHVIEWCBDEF3}
    rvepTransparent:
      if (Image<>nil) and (Image is TBitmap) then begin
        TBitmap(Image).Transparent := Value<>0;
        Result := True;
      end;
    rvepTransparentMode:
      if (Image<>nil) and (Image is TBitmap) then begin
        TBitmap(Image).TransparentMode := TTransparentMode(Value);
        Result := True;
      end;
    rvepTransparentColor:
      begin
        if (Image<>nil) and (Image is TBitmap) then begin
          TBitmap(Image).TransparentColor := TColor(Value);
          Result := True;
        end;
      end;
    {$ENDIF}
    else
      Result := inherited SetExtraIntProperty(Prop, Value);
  end;
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.GetExtraIntProperty(Prop: TRVExtraItemProperty; var Value: Integer): Boolean;
begin
  Result := False;
  case Prop of
    rvepImageWidth:
      begin
        Value := ImageWidth;
        Result := True;
      end;
    rvepImageHeight:
      begin
        Value := ImageHeight;
        Result := True;
      end;
    rvepMinHeightOnPage:
      begin
        Value := MinHeightOnPage;
        Result := True;
      end;
    rvepNoHTMLImageSize:
      begin
        Value := ord(NoHTMLImageSize);
        Result := True;
      end;
    {$IFDEF RICHVIEWCBDEF3}
    rvepTransparent:
      if (Image<>nil) and (Image is TBitmap) then begin
        Value := ord(TBitmap(Image).Transparent);
        Result := True;
      end;
    rvepTransparentMode:
      if (Image<>nil) and (Image is TBitmap) then begin
        Value := ord(TBitmap(Image).TransparentMode);
        Result := True;
      end;
    rvepTransparentColor:
      begin
        if (Image<>nil) and (Image is TBitmap) then begin
          Value := Integer(TBitmap(Image).TransparentColor);
          Result := True;
        end;
      end;
    {$ENDIF}
    else
      Result := inherited GetExtraIntProperty(Prop, Value);
  end;
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.GetExtraStrProperty(
  Prop: TRVExtraItemStrProperty; var Value: String): Boolean;
begin
  case Prop of
    rvespImageFileName:
      begin
        Value := ImageFileName;
        Result := True;
      end;
    rvespAlt:
      begin
        Value := Alt;
        Result := True;
      end;
    else
      Result := inherited GetExtraStrProperty(Prop, Value);
  end;
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.SetExtraStrProperty(
  Prop: TRVExtraItemStrProperty; const Value: String): Boolean;
begin
  case Prop of
    rvespImageFileName:
      begin
        ImageFileName := Value;
        Result := True;
      end;
    rvespAlt:
      begin
        Alt := Value;
        Result := True;
      end;
    else
      Result := inherited SetExtraStrProperty(Prop, Value);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVGraphicItemInfo.SaveRVF(Stream: TStream;
  RVData: TPersistent; ItemNo, ParaNo: Integer;
  const Name: String; Part: TRVMultiDrawItemPart;
  ForceSameAsPrev: Boolean);
var SaveType, LineCount: Integer;
begin
  if rvfoSavePicturesBody in TCustomRVData(RVData).RVFOptions then begin
    if rvfoSaveBinary in TCustomRVData(RVData).RVFOptions then
      SaveType := 2 // save binary
    else
      SaveType := 0; // save hex dump
    LineCount := 3+GetRVFExtraPropertyCount;
    end
  else begin
    SaveType := 1; // do not save
    LineCount := 1+GetRVFExtraPropertyCount;
  end;
  RVFWriteLine(Stream,
               Format('%d %d %s %d %d %s %s',
                 [StyleNo, LineCount,
                  RVFItemSavePara(ParaNo, TCustomRVData(RVData), ForceSameAsPrev),
                  Byte(RVFGetItemOptions(ItemOptions,ForceSameAsPrev)) and RVItemOptionsMask,
                  SaveType,
                  RVFSaveTag(rvoTagsArePChars in TCustomRVData(RVData).Options, Tag),
                  SaveRVFHeaderTail(RVData)]));
  RVFWriteLine(Stream, Name);
  if SaveType<>1 then begin
    RVFWriteLine(Stream, Image.ClassName);
    SaveRVFExtraProperties(Stream);
    if rvfoSaveBinary in TCustomRVData(RVData).RVFOptions then
      RVFSavePictureBinary(Stream, Image)
    else
      RVFWriteLine(Stream, RVFSavePicture(Image));
    end
  else
    SaveRVFExtraProperties(Stream);
end;
{------------------------------------------------------------------------------}
procedure RVSaveImageToRTF(Stream: TStream; TwipsPerPixel: Double;
                           Image: TGraphic; ImageWidth, ImageHeight: Integer;
                           Options: TRVRTFOptions);
var s: String;
    wmf: TMetafile;
    FreeWMF: Boolean;
    {$IFDEF RICHVIEWCBDEF3}
    bmp: TBitmap;
    slw: Integer;
    {$ENDIF}
    picw, pich: Integer;
begin
  if Image=nil then
    exit;
  RVFWrite(Stream,'{\pict');

  if (ImageWidth>0) and (Image.Width>0) then begin
    if (Image is TMetafile) and (TMetafile(Image).MMWidth>0) and
       (not TMetafile(Image).Enhanced or (rvrtfSaveEMFAsWMF in Options)) then
      picw := Round(TMetafile(Image).MMWidth*72/(127*TwipsPerPixel))
    else
      picw := Image.Width;
    RVFWrite(Stream,Format('\picscalex%d', [Round(ImageWidth*100/picw)]));
  end;
  if (ImageHeight>0) and (Image.Height>0) then begin
    if (Image is TMetafile) and (TMetafile(Image).MMHeight>0) and
       (not TMetafile(Image).Enhanced or (rvrtfSaveEMFAsWMF in Options)) then
      pich := Round(TMetafile(Image).MMHeight*72/(127*TwipsPerPixel))
    else
      pich := Image.Height;
    RVFWrite(Stream,Format('\picscaley%d', [Round(ImageHeight*100/pich)]));
  end;

  {$IFDEF RICHVIEWCBDEF3} // requires ScanLine property...
  // Saving bitmaps ...
  if Image is TBitmap then begin
    s := RVFSavePicture(Image);
    if TBitmap(Image).Height>1 then
      slw := abs(PChar(TBitmap(Image).ScanLine[1])-PChar(TBitmap(Image).ScanLine[0]))
    else
      slw := Image.Width;
    RVFWrite(Stream,
      Format('\dibitmap0\wbmwidthbytes%d\picw%d\pich%d\picwgoal%d\pichgoal%d ',
        [ slw, Image.Width, Image.Height, Image.Width*15, Image.Height*15]));
    RVFWrite(Stream, PChar(s)+sizeof(TBitmapFileHeader)*2);
  end
  // Saving metafiles ...
  else
  {$ENDIF}
    if Image is TMetafile then begin
    if TMetafile(Image).Enhanced then
      if not (rvrtfSaveEMFAsWMF in Options) then begin
        s := RVFSavePicture(Image);
        RVFWrite(Stream,Format('\emfblip\picw%d\pich%d ',
               [TMetafile(Image).MMWidth, TMetafile(Image).MMHeight]));
        RVFWrite(Stream,PChar(s));
        wmf := nil;
        FreeWMF := False;
        end
      else begin
        wmf := TMetafile.Create;
        wmf.Assign(Image);
        wmf.Enhanced := False;
        FreeWMF := True;
      end
    else begin
      wmf := TMetafile(Image);
      FreeWMF := False;
    end;
    if wmf<>nil then begin
      s := RVFSavePicture(wmf);
      RVFWrite(Stream,Format('\wmetafile8\picw%d\pich%d ',
               [wmf.MMWidth, wmf.MMHeight]));
      RVFWrite(Stream,PChar(s)+22*2); // sizeof(TMetafileHeader)=22
      if FreeWMF then
        wmf.Free;
    end;
   end
  else
  // Saving Jpegs ...
   {$IFNDEF RVDONOTUSEJPEGIMAGE}
  if (Image is TJpegImage) and (rvrtfSaveJpegAsJpeg in Options) then begin
    s := RVFSavePicture(Image);
    RVFWrite(Stream,Format('\jpegblip\picw%d\pich%d ',
               [Image.Width, Image.Height]));
    RVFWrite(Stream,PChar(s));
    end
  else
  {$ENDIF}
  {$IFDEF RICHVIEWCBDEF3}
  if rvrtfSaveBitmapDefault in Options then begin
    // Saving other image formats, such as icons, as bitmaps
    bmp := TBitmap.Create;
    try
      bmp.Assign(Image);
    except
      bmp.Width := Image.Width;
      bmp.Height := Image.Height;
      bmp.Canvas.Brush.Color := clWhite;
      bmp.Canvas.FillRect(Rect(0,0,bmp.Width,bmp.Height));
      bmp.Canvas.Draw(0,0,Image);
    end;
    s := RVFSavePicture(bmp);
    if bmp.Height>1 then
      slw := abs(PChar(bmp.ScanLine[1])-PChar(bmp.ScanLine[0]))
    else
      slw := bmp.Width;
    RVFWrite(Stream,Format('\dibitmap0\wbmwidthbytes%d\picw%d\pich%d\picwgoal%d\pichgoal%d ',
               [ slw, bmp.Width, bmp.Height, bmp.Width*15, bmp.Height*15]));
    RVFWrite(Stream, PChar(s)+sizeof(TBitmapFileHeader)*2);
    bmp.Free;
    end
  else
  {$ENDIF}
  begin
    // Saving other image formats, such as icons, as metafiles
    wmf :=  TMetafile.Create;
    wmf.Enhanced := False;
    wmf.Width := Image.Width;
    wmf.Height := Image.Height;
    with TMetafileCanvas.Create(wmf, 0) do begin
      Draw(0,0, Image);
      Free;
    end;
    if rvrtfSaveEMFDefault in Options then begin
      wmf.Enhanced := True;
      s := RVFSavePicture(wmf);
      RVFWrite(Stream,Format('\emfblip\picw%d\pich%d ',
               [TMetafile(wmf).MMWidth, TMetafile(wmf).MMHeight]));
      RVFWrite(Stream,PChar(s));
      end
    else begin
      s := RVFSavePicture(wmf);
      // Unfortunately, some RTF readers can read only wmetafile8 (for ex., WordPad).
      // MS Word reads all correctly
      // (there are some problems with picture size when saving wmetafile8)
      // May be it will be better to store unknown formats as bitmaps,
      // but it's not recommended, and some quality losing is possible.
      RVFWrite(Stream,Format('\wmetafile1\picw%d\pich%d ',
                 [wmf.Width, wmf.Height]));
      RVFWrite(Stream,PChar(s)+22*2); // sizeof(TMetafileHeader)=22
    end;
    wmf.Free;
  end;
  RVFWrite(Stream,'}');
end;
{------------------------------------------------------------------------------}
procedure TRVGraphicItemInfo.SaveRTF(Stream: TStream; RVData: TPersistent;
  ItemNo: Integer; const Name: String; TwipsPerPixel: Double;
  Level: Integer; ColorList: TRVColorList;
  StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
  FontTable: TRVList);
begin
  RVSaveImageToRTF(Stream, TwipsPerPixel, Image, ImageWidth, ImageHeight,
    TCustomRVData(RVData).RTFOptions);
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.PrintToBitmap(Bkgnd: TBitmap; Preview: Boolean;
  RichView: TRVScroller; dli: TRVDrawLineInfo; Part: Integer;
  ColorMode: TRVColorMode):Boolean;
var Top: Integer;
{$IFDEF RICHVIEWCBDEF3}
    tmp: TBitmap;
{$ENDIF}
begin
  Result := True;
  if (dli is TRVMultiImagePrintInfo) and (Part>=0) then
    Top := -TRVImagePrintPart(TRVMultiImagePrintInfo(dli).PartsList[Part]).ImgTop
  else
    Top := 0;
  if (ImageWidth<=0) and (ImageHeight<=0) then
    if Preview and (ImageCopy<>nil) then
      Bkgnd.Canvas.Draw(0,Top, ImageCopy)
    else
      Bkgnd.Canvas.Draw(0,Top, Image)
  else
    if Preview and (ImageCopy<>nil) then
      Bkgnd.Canvas.StretchDraw(Bounds(0,Top, Bkgnd.Width,Bkgnd.Height), ImageCopy)
    else begin
      if ((ImageWidth<=0) or (ImageWidth=Image.Width)) and ((ImageHeight<=0) or (ImageHeight=Image.Height)) then
        Bkgnd.Canvas.StretchDraw(Bounds(0,Top, Bkgnd.Width,Bkgnd.Height), Image)
      else begin
        {$IFDEF RICHVIEWCBDEF3}      
        tmp := nil;
        if Image.Transparent then begin
          tmp := TBitmap.Create;
          tmp.Assign(bkgnd);
        end;
        {$ENDIF}
        bkgnd.Width  := Image.Width;
        if Part<0 then
          bkgnd.Height := Image.Height;
        {$IFDEF RICHVIEWCBDEF3}
        if Image.Transparent then begin
          bkgnd.Canvas.StretchDraw(Rect(0,0,bkgnd.Width,bkgnd.Height), tmp);
          tmp.Free
        end;
        {$ENDIF}
        Bkgnd.Canvas.Draw(0, Top, Image)
      end;
    end;
end;
{------------------------------------------------------------------------------}
procedure TRVGraphicItemInfo.Print(Canvas: TCanvas; x, y, x2: Integer;
  Preview, Correction: Boolean; const sad: TRVScreenAndDevice;
  RichView: TRVScroller; dli: TRVDrawLineInfo;
  Part: Integer; ColorMode: TRVColorMode; RVData: TPersistent);
var DCState: Integer;
    R: TRect;
begin
  // will be called only for metafiles
  DCState := SaveDC(Canvas.Handle);
  try
    R := Bounds(x+MulDiv(Spacing,  sad.ppixDevice, sad.ppixScreen),
      y+MulDiv(Spacing, sad.ppiyDevice, sad.ppiyScreen),
      MulDiv(GetImageWidth(TCustomRichView(RichView).Style),  sad.ppixDevice, sad.ppixScreen),
      MulDiv(GetImageHeight(TCustomRichView(RichView).Style), sad.ppiyDevice, sad.ppiyScreen));
    with R do
      IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
    Canvas.StretchDraw(r, Image);
  finally
    RestoreDC(Canvas.Handle, DCState);
  end;
end;
{------------------------------------------------------------------------------}
function TRVGraphicItemInfo.CreatePrintingDrawItem(RVData: TObject;
  const sad: TRVScreenAndDevice): TRVDrawLineInfo;
begin
  if not GetBoolValueEx(rvbpPrintToBMP, nil) or (MinHeightOnPage=0) or
    ((ImageHeight>0) and (ImageHeight<>Image.Height)) then begin
    Result := TRVDrawLineInfo.Create;
    exit;
  end;
  Result := TRVMultiImagePrintInfo.Create(Self);
  Result.Width  := MulDiv(GetWidth, sad.ppixDevice, sad.ppixScreen);
  Result.Height := MulDiv(GetHeight, sad.ppiyDevice, sad.ppiyScreen);
  TRVMultiImagePrintInfo(Result).sad := sad;
end;
{============================ TRVHotGraphicItemInfo ===========================}
constructor TRVHotGraphicItemInfo.CreateEx(RVData: TPersistent;
  AImage: TGraphic; AVAlign: TRVVAlign);
begin
  inherited CreateEx(RVData, AImage, AVAlign);
  StyleNo := rvsHotPicture;
end;
{------------------------------------------------------------------------------}
function TRVHotGraphicItemInfo.GetBoolValueEx(Prop: TRVItemBoolPropertyEx;
  RVStyle: TRVStyle): Boolean;
begin
  case Prop of
    rvbpJump, rvbpAllowsFocus, rvbpXORFocus:
      Result := True;
    else
      Result := inherited GetBoolValueEx(Prop, RVStyle);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVHotGraphicItemInfo.Execute(RVData:TPersistent);
begin
  if RVData is TCustomRVFormattedData then
    TCustomRVFormattedData(RVData).DoJump(JumpID+
      TCustomRVFormattedData(RVData).FirstJumpNo)
end;
{============================== TRVBulletItemInfo =============================}
constructor TRVBulletItemInfo.CreateEx(RVData: TPersistent; AImageIndex: Integer; AImageList: TCustomImageList; AVAlign: TRVVAlign);
begin
  inherited Create(RVData);
  StyleNo    := rvsBullet;
  ImageIndex := AImageIndex;
  ImageList  := AImageList;
  VAlign     := AVAlign;
end;
{------------------------------------------------------------------------------}
procedure TRVBulletItemInfo.Assign(Source: TCustomRVItemInfo);
begin
  if Source is TRVBulletItemInfo then begin
    ImageList  := TRVBulletItemInfo(Source).ImageList;
    ImageIndex := TRVBulletItemInfo(Source).ImageIndex;
    NoHTMLImageSize := TRVBulletItemInfo(Source).NoHTMLImageSize;
  end;
  inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
function TRVBulletItemInfo.GetHeight: Integer;
begin
  Result := TImageList(ImageList).Height+Spacing*2;
end;
{------------------------------------------------------------------------------}
function TRVBulletItemInfo.GetImageHeight(RVStyle: TRVStyle): Integer;
begin
  Result := TImageList(ImageList).Height;
end;
{------------------------------------------------------------------------------}
function TRVBulletItemInfo.GetWidth: Integer;
begin
  Result := TImageList(ImageList).Width+Spacing*2;
end;
{------------------------------------------------------------------------------}
function TRVBulletItemInfo.GetImageWidth(RVStyle: TRVStyle): Integer;
begin
  Result := TImageList(ImageList).Width;
end;
{------------------------------------------------------------------------------}
function TRVBulletItemInfo.GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas;
                                       RVData: TPersistent): Integer;
begin
  Result := TImageList(ImageList).Width+Spacing*2;
  if sad<>nil then
    Result := MulDiv(Result, sad.ppixDevice, sad.ppixScreen);
end;
{------------------------------------------------------------------------------}
function TRVBulletItemInfo.GetImageIndex(Hot: Boolean): Integer;
begin
  Result := ImageIndex;
end;
{------------------------------------------------------------------------------}
function TRVBulletItemInfo.GetBoolValue(Prop: TRVItemBoolProperty): Boolean;
begin
  case Prop of
    rvbpValid:
      Result := ImageList<>nil;
    rvbpDisplayActiveState:
      Result := True;
    else
      Result := inherited GetBoolValue(Prop);
  end;
end;
{------------------------------------------------------------------------------}
function TRVBulletItemInfo.GetExtraIntProperty(Prop: TRVExtraItemProperty;
  var Value: Integer): Boolean;
begin
  case Prop of
    rvepNoHTMLImageSize:
      begin
        Value := ord(NoHTMLImageSize);
        Result := True;
      end;
    else
      Result := inherited GetExtraIntProperty(Prop, Value);
  end;
end;
{------------------------------------------------------------------------------}
function TRVBulletItemInfo.SetExtraIntProperty(Prop: TRVExtraItemProperty;
  Value: Integer): Boolean;
begin
  case Prop of
    rvepNoHTMLImageSize:
      begin
        NoHTMLImageSize := Value<>0;
        Result := True;
      end;
    else
      Result := inherited SetExtraIntProperty(Prop, Value);
  end;
end;
{------------------------------------------------------------------------------}
function TRVBulletItemInfo.GetRVFExtraPropertyCount: Integer;
begin
  Result := inherited GetRVFExtraPropertyCount;
  if NoHTMLImageSize then
    inc(Result);
end;
{------------------------------------------------------------------------------}
procedure TRVBulletItemInfo.SaveRVFExtraProperties(Stream: TStream);
begin
  inherited SaveRVFExtraProperties(Stream);
  if NoHTMLImageSize then
    WriteRVFExtraIntPropertyStr(Stream, rvepNoHTMLImageSize, 1);
end;
{------------------------------------------------------------------------------}
function TRVBulletItemInfo.PrintToBitmap(Bkgnd: TBitmap; Preview: Boolean;
  RichView: TRVScroller; dli: TRVDrawLineInfo; Part: Integer;
  ColorMode: TRVColorMode): Boolean;
begin
  Result := True;
  ImageList.Draw(Bkgnd.Canvas,0,0, ImageIndex);
end;
{------------------------------------------------------------------------------}
procedure TRVBulletItemInfo.Paint(x, y: Integer; Canvas: TCanvas;
                                  State: TRVItemDrawStates;
                                  Style: TRVStyle;
                                  dli: TRVDrawLineInfo);
var SelColor: TColor;
    BlendColor: TColorRef;
    ILDrawOptions: Integer;
begin
  if (rvidsSelected in State) then begin
    if rvidsControlFocused in State then
      SelColor := Style.SelColor
    else
      SelColor := Style.InactiveSelColor;
    end
  else
    SelColor := clNone;
  if SelColor<clNone then begin
    BlendColor := ColorToRGB(SelColor);
    ILDrawOptions := ILD_TRANSPARENT or ILD_SELECTED;
    end
  else begin
    BlendColor := CLR_NONE;
    ILDrawOptions := ILD_TRANSPARENT;
  end;
  ImageList_DrawEx(ImageList.Handle, GetImageIndex(rvidsHover in State),
                   Canvas.Handle, x+Spacing, y+Spacing,
                   TImageList(ImageList).Width, TImageList(ImageList).Height,
                   CLR_NONE, BlendColor, ILDrawOptions);
  if (rvidsCurrent in State) and (Style.CurrentItemColor<>clNone) then begin
    Canvas.Pen.Color := Style.CurrentItemColor;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    Canvas.Rectangle(x,y, x+TImageList(ImageList).Width+Spacing*2, y+TImageList(ImageList).Height+Spacing*2);
  end;
end;
{------------------------------------------------------------------------------}
function TRVBulletItemInfo.ReadRVFHeader(var P: PChar; RVData: TPersistent): Boolean;
var ImageListNo: Integer;
begin
  Result := (RVFReadInteger(P,ImageListNo) and
             RVFReadInteger(P,ImageIndex));
  if not Result then exit;
  ImageList := TCustomRVData(RVData).RVFImageListNeeded(ImageListNo);
  if ImageList<>nil then
    if ImageList.Count<=ImageIndex then begin
      TCustomRVData(RVData).RVFWarnings := TCustomRVData(RVData).RVFWarnings+[rvfwConvLargeImageIdx];
      if rvfoConvLargeImageIdxToZero in TCustomRVData(RVData).RVFOptions then
        ImageIndex := 0
      else
        Result := False;
    end;
end;
{------------------------------------------------------------------------------}
function TRVBulletItemInfo.ReadRVFLine(const s: String;
                                       RVData: TPersistent;
                                       ReadType, LineNo, LineCount: Integer;
                                       var Name: String;
                                       var ReadMode: TRVFReadMode;
                                       var ReadState: TRVFReadState): Boolean;
begin
  if (LineNo=0) then
    Name := s
  else
    SetExtraPropertyFromRVFStr(s);
  Result := True;
end;
{------------------------------------------------------------------------------}
function TRVBulletItemInfo.SaveRVFHeaderTail(RVData: TPersistent): String;
begin
  Result :=  Format('%d %d', [ImageList.Tag, ImageIndex]);
end;
{------------------------------------------------------------------------------}
procedure TRVBulletItemInfo.SaveRVF(Stream: TStream; RVData: TPersistent;
                                    ItemNo,ParaNo: Integer; const Name: String;
                                    Part: TRVMultiDrawItemPart;
                                    ForceSameAsPrev: Boolean);
begin

  RVFWriteLine(Stream,
    Format('%d %d %s %d %d %s %s',
          [StyleNo, 1+GetRVFExtraPropertyCount,
           RVFItemSavePara(ParaNo,TCustomRVData(RVData), ForceSameAsPrev),
           Byte(RVFGetItemOptions(ItemOptions, ForceSameAsPrev)) and RVItemOptionsMask,
           0, RVFSaveTag(rvoTagsArePChars in TCustomRVData(RVData).Options, Tag),
           SaveRVFHeaderTail(RVData)]));
  RVFWriteLine(Stream, Name);
  SaveRVFExtraProperties(Stream);
end;
{------------------------------------------------------------------------------}
procedure RVSaveImageListImageToRTF(Stream: TStream;
                                    TwipsPerPixel: Double;
                                    ImageList: TCustomImageList;
                                    ImageIndex: Integer;
                                    RTFOptions: TRVRTFOptions);
var s: String;
    wmf: TMetafile;
    {$IFDEF RICHVIEWCBDEF3}
    bmp: TBitmap;
    slw: Integer;
    {$ENDIF}
    Canvas: TMetafileCanvas;
begin
  if (ImageList=nil) or (ImageIndex<0) or
     (ImageIndex>=ImageList.Count) then
    exit;
  RVFWrite(Stream,'{\pict');
  {$IFDEF RICHVIEWCBDEF3}
  if rvrtfSaveBitmapDefault in RTFOptions then begin
    bmp := TBitmap.Create;
    ImageList.GetBitmap(ImageIndex, bmp);
    s := RVFSavePicture(bmp);
    if bmp.Height>1 then
      slw := abs(PChar(bmp.ScanLine[1])-PChar(bmp.ScanLine[0]))
    else
      slw := bmp.Width;
    RVFWrite(Stream,Format('\dibitmap0\wbmwidthbytes%d\picw%d\pich%d\picwgoal%d\pichgoal%d ',
               [slw, bmp.Width, bmp.Height, bmp.Width*15, bmp.Height*15]));
    RVFWrite(Stream, PChar(s)+sizeof(TBitmapFileHeader)*2);
    bmp.Free;
    end
  else
  {$ENDIF}
  begin
    wmf :=  TMetafile.Create;
    wmf.Enhanced := False;
    wmf.Width := TImageList(ImageList).Width;
    wmf.Height := TImageList(ImageList).Height;
    Canvas := TMetafileCanvas.Create(wmf, 0);
    ImageList.Draw(Canvas, 0, 0, ImageIndex);
    Canvas.Free;
    s := RVFSavePicture(wmf);
    RVFWrite(Stream,Format('\wmetafile1\picw%d\pich%d ',
               [wmf.Width, wmf.Height]));
    RVFWrite(Stream,PChar(s)+22*2); // sizeof(TMetafileHeader)=22
    wmf.Free;
  end;
  RVFWrite(Stream,'}');
end;
{------------------------------------------------------------------------------}
procedure TRVBulletItemInfo.SaveRTF(Stream: TStream; RVData: TPersistent;
  ItemNo: Integer; const Name: String; TwipsPerPixel: Double;
  Level: Integer; ColorList: TRVColorList;
  StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
  FontTable: TRVList);
begin
  RVSaveImageListImageToRTF(Stream, TwipsPerPixel,
    ImageList, ImageIndex, TCustomRVData(RVData).RTFOptions);
end;
{------------------------------------------------------------------------------}
procedure RVSaveImageSharedImageInHTML(ImageList: TCustomImageList;
  ImageIndex: Integer; Graphic: TGraphic;
  var Location: String;
  RVData: TPersistent; const Path,
  imgSavePrefix: String; var imgSaveNo: Integer; CurrentFileColor: TColor;
  SaveOptions: TRVSaveOptions;
  Bullets: TRVList);
var j: Integer;
    bmp: TBitmap;
    bi : TRVHTMLBulletInfo;
begin
  Location := '';
  for j:=0 to Bullets.Count-1 do begin
    bi := TRVHTMLBulletInfo(Bullets[j]);
    if (ImageList = bi.ImageList) and
       (ImageIndex = bi.ImageIndex) and
       (Graphic = bi.Graphic) and
       (CurrentFileColor = bi.BackColor) then begin
      Location := bi.FileName;
      break;
    end;
  end;
  if Location='' then begin
    bmp := TBitmap.Create;
    try
      if ImageList<>nil then begin
        bmp.Width := TImageList(ImageList).Width;
        bmp.Height := TImageList(ImageList).Height;
        bmp.Canvas.Brush.Color := CurrentFileColor;
        bmp.Canvas.Pen.Color := CurrentFileColor;
        bmp.Canvas.FillRect(Rect(0,0,bmp.Width,bmp.Height));
        ImageList.Draw(bmp.Canvas, 0, 0, ImageIndex);
        end
      else begin
        bmp.Width := Graphic.Width;
        bmp.Height := Graphic.Width;
        bmp.Canvas.Brush.Color := CurrentFileColor;
        bmp.Canvas.Pen.Color := CurrentFileColor;
        bmp.Canvas.FillRect(Rect(0,0,bmp.Width,bmp.Height));
        bmp.Canvas.Draw(0,0, Graphic);
      end;
      Location := TCustomRVData(RVData).DoSavePicture(rvsfHTML, imgSavePrefix, Path,
                     imgSaveNo, rvsoOverrideImages in SaveOptions,
                     CurrentFileColor, bmp);
      Location := RV_GetHTMLPath(Location);
      bi := TRVHTMLBulletInfo.Create;
      bi.FileName   := Location;
      bi.BackColor  := CurrentFileColor;
      bi.ImageList  :=  ImageList;
      bi.ImageIndex := ImageIndex;
      bi.Graphic    := Graphic;
      Bullets.Add(bi);
    finally
      bmp.Free;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVBulletItemInfo.SaveToHTML(Stream: TStream;
  RVData: TPersistent; ItemNo: Integer; const Text, Path,
  imgSavePrefix: String; var imgSaveNo: Integer; CurrentFileColor: TColor;
  SaveOptions: TRVSaveOptions;
  UseCSS: Boolean; Bullets: TRVList);
var
    Location: String;
    DoDefault: Boolean;
    {....................................................}
    function GetCSS: String;
    begin
      Result := '';
      if UseCSS then begin
        Result := GetVShiftCSS(TCustomRVData(RVData).GetRVStyle);
        if Result<>'' then
          Result := Format(' style="{%s}" ',[Result]);
        if Result='' then
          Result := ' ';
        Result := Result+'alt="" ';
      end;
      {$IFNDEF RVDONOTUSEITEMHINTS}
      if Hint<>'' then begin
        if Result='' then
          Result := ' ';
        Result := Result+RV_GetHintStr(rvsfHTML, Hint)+' ';
      end;
      {$ENDIF}
    end;
    {....................................................}
begin
  TCustomRVData(RVData).HTMLSaveImage(TCustomRVData(RVData), ItemNo, Path, CurrentFileColor, Location, DoDefault);
  if DoDefault then
    RVSaveImageSharedImageInHTML(ImageList, ImageIndex, nil, Location, RVData, Path,
      imgSavePrefix, imgSaveNo, CurrentFileColor, SaveOptions, Bullets);
  RVWrite(Stream,Format('<IMG%s%ssrc="'+Location+'">',
    [RV_GetExtraIMGStr(SaveOptions, TImageList(ImageList).Width,
      TImageList(ImageList).Height, NoHTMLImageSize), GetCSS]));
end;
{============================= TRVHotspotItemInfo =============================}
constructor TRVHotspotItemInfo.CreateEx(RVData: TPersistent; AImageIndex, AHotImageIndex: Integer;
                                        AImageList: TCustomImageList; AVAlign: TRVVAlign);
begin
  inherited CreateEx(RVData, AImageIndex, AImageList, AVAlign);
  StyleNo       := rvsHotspot;
  HotImageIndex := AHotImageIndex;
end;
{------------------------------------------------------------------------------}
procedure TRVHotspotItemInfo.Assign(Source: TCustomRVItemInfo);
begin
  if Source is TRVHotspotItemInfo then
    HotImageIndex := TRVHotspotItemInfo(Source).HotImageIndex;
  inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
function TRVHotspotItemInfo.GetImageIndex(Hot: Boolean): Integer;
begin
  if Hot then
    Result := HotImageIndex
  else
    Result := ImageIndex;
end;
{------------------------------------------------------------------------------}
function TRVHotspotItemInfo.ReadRVFHeader(var P: PChar;
  RVData: TPersistent): Boolean;
begin
  Result := (inherited ReadRVFHeader(P, RVData));
  if not Result then
    exit;
  if not (P^ in [#0, #10, #13]) then begin
    Result := RVFReadInteger(P,HotImageIndex);
    if not Result then
      exit;
    end
  else
    HotImageIndex := ImageIndex;
  if ImageList<>nil then
    if ImageList.Count<=HotImageIndex then begin
      TCustomRVData(RVData).RVFWarnings := TCustomRVData(RVData).RVFWarnings+[rvfwConvLargeImageIdx];
    if rvfoConvLargeImageIdxToZero in TCustomRVData(RVData).RVFOptions then
      HotImageIndex := 0
    else
      Result := False;
    end;
end;
{------------------------------------------------------------------------------}
function TRVHotspotItemInfo.SaveRVFHeaderTail(RVData: TPersistent): String;
begin
  Result := Format('%s %d', [inherited SaveRVFHeaderTail(RVData), HotImageIndex]);
end;
{------------------------------------------------------------------------------}
procedure TRVHotspotItemInfo.SaveRVF(Stream: TStream; RVData: TPersistent;
                                     ItemNo,ParaNo: Integer; const Name: String;
                                     Part: TRVMultiDrawItemPart;
                                     ForceSameAsPrev: Boolean);
begin
  RVFWriteLine(Stream,
    Format('%d %d %s %d %d %s %s',
          [StyleNo, 1,
           RVFItemSavePara(ParaNo,TCustomRVData(RVData),ForceSameAsPrev),
           Byte(RVFGetItemOptions(ItemOptions,ForceSameAsPrev)) and RVItemOptionsMask,
           0, RVFSaveTag(rvoTagsArePChars in TCustomRVData(RVData).Options, Tag),
           SaveRVFHeaderTail(RVData)]));
  RVFWriteLine(Stream, Name);
end;
{------------------------------------------------------------------------------}
function TRVHotspotItemInfo.GetBoolValueEx(Prop: TRVItemBoolPropertyEx;
                                           RVStyle: TRVStyle): Boolean;
begin
  case Prop of
    rvbpJump, rvbpAllowsFocus,rvbpXORFocus:
       Result := True;
    rvbpHotColdJump:
       Result := ImageIndex<>HotImageIndex;
    else
      Result := inherited GetBoolValueEx(Prop, RVStyle);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVHotspotItemInfo.Execute(RVData: TPersistent);
begin
  if RVData is TCustomRVFormattedData then
    TCustomRVFormattedData(RVData).DoJump(JumpID+
      TCustomRVFormattedData(RVData).FirstJumpNo)
end;
{============================ TRVBreakItemInfo ================================}
constructor TRVBreakItemInfo.CreateEx(RVData: TPersistent; ALineWidth: Byte; AStyle: TRVBreakStyle; AColor: TColor);
begin
  inherited Create(RVData);
  StyleNo   := rvsBreak;
  LineWidth := ALineWidth;
  Style     := AStyle;
  Color     := AColor;
end;
{------------------------------------------------------------------------------}
procedure TRVBreakItemInfo.Assign(Source: TCustomRVItemInfo);
begin
  if Source is TRVBreakItemInfo then begin
    LineWidth := TRVBreakItemInfo(Source).LineWidth;
    Color     := TRVBreakItemInfo(Source).Color;
    Style     := TRVBreakItemInfo(Source).Style;
  end;
  inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
{ Drawing 3d edge with colors TopLeftColor, BottomRightColor.
  r - outer rectangle (right bottom coordinates inclusive).
  LineWidth - width of edge.                                                   }
procedure DrawEdge(Canvas: TCanvas; r: TRect;
  TopLeftColor, BottomRightColor: TColor; LineWidth: Integer);
var i: Integer;
    DrawBottom: Boolean;
begin
  if LineWidth<=0 then
    LineWidth := 1;
  DrawBottom := r.Bottom-r.Top>=LineWidth;
  for i := LineWidth-1 downto 0 do begin
    Canvas.Pen.Color := TopLeftColor;
    Canvas.MoveTo(r.Left, r.Bottom);
    Canvas.LineTo(r.Left, r.Top);
    Canvas.LineTo(r.Right, r.Top);
    if DrawBottom then begin
      Canvas.Pen.Color := BottomRightColor;
      Canvas.LineTo(r.Right, r.Bottom);
      Canvas.LineTo(r.Left, r.Bottom);
      InflateRect(r, -1, -1);
      end
    else
      InflateRect(r, 0, -1);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVBreakItemInfo.PaintFullWidth(Left, Right, Top: Integer;
  Canvas: TCanvas; State: TRVItemDrawStates; Style: TRVStyle;
  const ClipRect: TRect; dli: TRVDrawLineInfo);
begin
  inc(Left,5);
  dec(Right,5);
  inc(Top,5);
  if Color = clNone then
    Canvas.Pen.Color := Style.TextStyles[0].Color
  else
    Canvas.Pen.Color := Color;
  Canvas.Pen.Style := psInsideFrame;
  case Self.Style of
    rvbsLine:
      begin
        Canvas.Pen.Width := LineWidth;
        Canvas.MoveTo(Left, Top);
        Canvas.LineTo(Right, Top);
      end;
    rvbsRectangle:
      begin
        Canvas.Pen.Width := 1;
        Canvas.Rectangle(Left, Top-LineWidth div 2,
          Right, Top-LineWidth div 2+LineWidth);
      end;
    rvbs3d:
      begin
        Canvas.Pen.Width := 1;
        DrawEdge(Canvas,
          Rect(Left, Top-LineWidth div 2, Right-1, Top-LineWidth div 2+LineWidth-1),
          clBtnShadow, clBtnFace, 1);
      end;
  end;
  if rvidsSelected in State then begin
    if rvidsControlFocused in State then
      Canvas.Pen.Color := Style.SelColor
    else
      Canvas.Pen.Color := Style.InactiveSelColor;
    if Canvas.Pen.Color<>clNone then begin
      Canvas.Pen.Width := LineWidth;
      Canvas.MoveTo(Left, Top);
      Canvas.LineTo(Right, Top);
    end;
  end;
  Canvas.Pen.Width := 1;  
  Canvas.Pen.Style := psSolid;  
end;
{------------------------------------------------------------------------------}
procedure TRVBreakItemInfo.Print(Canvas: TCanvas; x, y, x2: Integer;
  Preview, Correction: Boolean; const sad: TRVScreenAndDevice; RichView: TRVScroller;
  dli: TRVDrawLineInfo;Part: Integer; ColorMode: TRVColorMode; RVData: TPersistent);
var x5, y5, w: Integer;
    clr: TColor;
begin
  Canvas.Pen.Style := psInsideFrame;
  Canvas.Pen.Mode := pmCopy;
  if Color = clNone then
    clr := TCustomRichView(RichView).Style.TextStyles[0].Color
  else
    clr := Color;
  Canvas.Pen.Color := RV_GetColor(clr, ColorMode);;
  y5 := (5 * sad.ppiyDevice) div sad.ppiyScreen;
  x5 := (5 * sad.ppixDevice) div sad.ppixScreen;
  case Style of
    rvbsLine:
      begin
        Canvas.Pen.Width := (LineWidth * sad.ppiyDevice) div sad.ppiyScreen;
        Canvas.MoveTo(x+x5, y+y5);
        Canvas.LineTo(x2-x5, y+y5);
      end;
    rvbsRectangle:
      begin
        w := (LineWidth * sad.ppiyDevice) div sad.ppiyScreen;
        Canvas.Pen.Width := Round(sad.ppiyDevice/sad.ppiyScreen);
        Canvas.Rectangle(x+x5, y+y5-(w div 2), x2-x5, y+y5-(w div 2)+w);
      end;
    rvbs3d:
      begin
        Canvas.Pen.Width := 1;
        w := (LineWidth * sad.ppiyDevice) div sad.ppiyScreen;
        DrawEdge(Canvas,
          Rect(x+x5, y+y5-(w div 2), x2-x5-1, y+y5-(w div 2)+w-1),
          RV_GetColor(clBtnShadow, ColorMode),
          RV_GetColor(clBtnFace, ColorMode),
          Round(sad.ppiyDevice/sad.ppiyScreen));
      end;
  end;
  Canvas.Pen.Style := psSolid;
end;
{------------------------------------------------------------------------------}
function TRVBreakItemInfo.AsText(LineWidth: Integer;
                           RVData: TPersistent;
                           const Text, Path: String;
                           TextOnly,Unicode: Boolean): String;
var c: Char;
begin
  if Self.LineWidth>1 then
    c := '='
  else
    c := '-';
  if LineWidth<1 then
    LineWidth := 1;
  SetLength(Result, LineWidth);
  FillChar(PChar(Result)^, LineWidth, ord(c));
end;
{------------------------------------------------------------------------------}
procedure TRVBreakItemInfo.SaveToHTML(Stream: TStream; RVData: TPersistent;
  ItemNo: Integer; const Text, Path: String; const imgSavePrefix: String;
  var imgSaveNo: Integer; CurrentFileColor: TColor;
  SaveOptions: TRVSaveOptions; UseCSS: Boolean; Bullets: TRVList);
var title: String;
begin
  if rvsoForceNonTextCSS in SaveOptions then
    UseCSS := True;
  title := '';
  {$IFNDEF RVDONOTUSEITEMHINTS}
  if Hint<>'' then
    title := RV_GetHintStr(rvsfHTML, Hint)+' ';;
  {$ENDIF}
  if UseCSS and (Color<>clNone) then
    RVWrite(Stream, Format('<HR noshade size=%d %sstyle="{color : %s}">',
      [LineWidth,  title, RV_GetHTMLRGBStr(Color, False)]))
  else if (Color<>clNone) then
    RVWrite(Stream, Format('<HR noshade size=%d color=%s %s>',
      [LineWidth, RV_GetHTMLRGBStr(Color, True), title]))
  else
    RVWrite(Stream, Format('<HR noshade size=%d %s>',
      [LineWidth, title]));
end;
{------------------------------------------------------------------------------}
function TRVBreakItemInfo.ReadRVFHeader(var P: PChar;
  RVData: TPersistent): Boolean;
var bc, bs,bw: Integer;
begin
  if not (P^ in [#0, #10, #13]) then begin
    Result := (RVFReadInteger(P,bc) and
               RVFReadInteger(P,bs) and
               RVFReadInteger(P,bw));
    if Result then begin
      LineWidth := Byte(bw);
      Style     := TRVBreakStyle(bs);
      Color     := bc;
    end;
    end
  else begin
    Color := clNone;
    Style := rvbsLine;
    LineWidth := 1;
    Result := True;
  end;
end;
{------------------------------------------------------------------------------}
function TRVBreakItemInfo.SaveRVFHeaderTail(RVData: TPersistent): String;
begin
  Result := Format('%d %d %d', [Integer(Color), Integer(Style), Integer(LineWidth)]);
end;
{------------------------------------------------------------------------------}
procedure TRVBreakItemInfo.SaveRVF(Stream: TStream; RVData: TPersistent;
                                   ItemNo, ParaNo: Integer; const Name: String;
                                   Part: TRVMultiDrawItemPart;
                                   ForceSameAsPrev: Boolean);
begin
  RVFWriteLine(Stream,
               Format('%d %d %s %d %d %s %s',
                     [StyleNo, 0,
                      RVFItemSavePara(ParaNo, TCustomRVData(RVData), False),
                      Byte(ItemOptions) and RVItemOptionsMask,
                      0, RVFSaveTag(rvoTagsArePChars in TCustomRVData(RVData).Options,Tag),
                      SaveRVFHeaderTail(RVData)]));
end;
{------------------------------------------------------------------------------}
procedure TRVBreakItemInfo.FillRTFTables(ColorList: TRVColorList;
  ListOverrideCountList: TRVIntegerList; RVData: TPersistent); 
begin
  ColorList.AddUnique(Color);
end;
{------------------------------------------------------------------------------}
procedure TRVBreakItemInfo.SaveRTF(Stream: TStream; RVData: TPersistent;
  ItemNo: Integer; const Name: String; TwipsPerPixel: Double;
  Level: Integer; ColorList: TRVColorList;
  StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
  FontTable: TRVList);
var ColorIdx: Integer;
    tbl: String;
begin
  if Color = clNone then
    ColorIdx := 0
  else
    ColorIdx := ColorList.IndexOf(Pointer(Color));
  case Level of
    0:
      tbl := '';
    1:
      tbl := '\intbl\itap1';
    else
      tbl := Format('\itap%d',[Level]);
  end;
  RVWrite(Stream, Format('\pard%s\plain\fs6\brdrb\brdrs\brdrw%d\brdrcf%d\par\pard%s',
                  [tbl, Round(LineWidth*TwipsPerPixel), ColorIdx, tbl]));
end;
{------------------------------------------------------------------------------}
function TRVBreakItemInfo.GetBoolValue(Prop: TRVItemBoolProperty): Boolean;
begin
  case Prop of
    rvbpRequiresRVFLines:
      Result := False;
    else
      Result := inherited GetBoolValue(Prop);
  end;
end;
{------------------------------------------------------------------------------}
function TRVBreakItemInfo.GetBoolValueEx(Prop: TRVItemBoolPropertyEx; RVStyle: TRVStyle): Boolean;
begin
  case Prop of
    rvbpPrintToBMP:
      Result := False;
    else
      Result := inherited GetBoolValueEx(Prop, RVStyle);
  end;
end;
{================================ TRVTextItemInfo =============================}
procedure TRVTextItemInfo.Execute(RVData: TPersistent);
begin
  if RVData is TCustomRVFormattedData then begin
    if GetBoolValueEx(rvbpJump, TCustomRVData(RVData).GetRVStyle) then
      TCustomRVFormattedData(RVData).DoJump(JumpID+
          TCustomRVFormattedData(RVData).FirstJumpNo)
  end;
end;
{------------------------------------------------------------------------------}
function TRVTextItemInfo.GetBoolValueEx(Prop: TRVItemBoolPropertyEx;
  RVStyle: TRVStyle): Boolean;
begin
  case Prop of
    rvbpJump, rvbpAllowsFocus,rvbpXORFocus:
      Result := RVStyle.TextStyles[GetActualStyleNo(RVStyle)].Jump;
    rvbpHotColdJump:
      Result := RVStyle.TextStyles[GetActualStyleNo(RVStyle)].Jump and
                RVStyle.StyleHoverSensitive(GetActualStyleNo(RVStyle));
    else
      Result := inherited GetBoolValueEx(Prop, RVStyle);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTextItemInfo.MarkStylesInUse(UsedTextStyles, UsedParaStyles, UsedListStyles: TRVIntegerList);
begin
  inherited MarkStylesInUse(UsedTextStyles, UsedParaStyles, UsedListStyles);
  if StyleNo<>rvsDefStyle then
    UsedTextStyles[StyleNo] := 1;
end;
{------------------------------------------------------------------------------}
function TRVTextItemInfo.ReadRVFHeader(var P: PChar;
  RVData: TPersistent): Boolean;
begin
  Result := True;
  {$IFNDEF RVDONOTUSEITEMHINTS}
  {$IFDEF RICHVIEWCBDEF3}
  if P^<>#0 then
    Hint := AnsiExtractQuotedStr(P, '"');
  {$ENDIF}
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRVTextItemInfo.UpdateStyles(TextStylesShift,
  ParaStylesShift, ListStylesShift: TRVIntegerList);
begin
  inherited UpdateStyles(TextStylesShift, ParaStylesShift, ListStylesShift);
  if StyleNo<>rvsDefStyle then
    dec(StyleNo,TextStylesShift[StyleNo]-1);
end;
{=========================== TRVStoreSubRVData ================================}
function TRVStoreSubRVData.Duplicate: TRVStoreSubRVData;
begin
  Result := nil;
end;
{=========================== TRVMultiDrawItemPart =============================}
function TRVMultiDrawItemPart.GetSoftPageBreakInfo: Integer;
begin
  Result := -1;
end;
{==============================================================================}

initialization
  RichView_InitializeList;
  RichViewTextItemClass := TRVTextItemInfo;
finalization
  RichView_FinalizeList;

end.
