
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TCustomRVFormattedData is a basic class         }
{       representing RichView document with             }
{       formatting.                                     }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit CRVFData;

interface
{$I RV_Defs.inc}
uses SysUtils, Windows, Messages, Classes, Graphics, Controls, Clipbrd,
     Forms,
     RVFuncs, RVStyle, RVItem, CRVData, DLines, RVScroll, RVBack, RVUni,
    {$IFNDEF RVDONOTUSEDRAGDROP}
     ActiveX, RVDragDrop,
    {$ENDIF}
     RVClasses;

type
  TCustomRVFormattedData = class;
{-----------------------------------------------------------------------}
  TRVJumpInfo = class
    public
     RVData: TCustomRVFormattedData;
     l,t,w,h: Integer;
     id, DrawItemNo: Integer;
     Cursor: TCursor;
  end;

  TRVSoftPageBreakInfo = class
    ItemNo, Offset, ExtraData: Integer;
  end;

  TRVSoftPageBreakList = class (TRVList)
  public
    function FindItem(ItemNo: Integer): Integer;
  end;


  TRVSelectingState = (rvsesInWord, rvsesOutsideWord, rvsesFreeMode,
                       rvsesParaMode);
  { TRVSelectingInfo: information about the selection process (with mouse).
    For internal use (used as FSelectingInfo field of TCustomRVFormattedData.
    This field is allocated while user is making selection.
    Contains two set of fields: for starting and for ending parts of selection
    (bounds of word containing selection end, actual selection position).      }
  TRVSelectingInfo = class
    public
      DrawItemSOffs, DrawItemEOffs, DrawItemSWordOffs1, DrawItemSWordOffs2,
      DrawItemEWordOffs1, DrawItemEWordOffs2: Integer;
      DrawItemSNo, DrawItemENo: Integer;
      SWordState, EWordState: TRVSelectingState;
      procedure InitE(ASWordState: TRVSelectingState);
      function IsAboveSWord(ADrawItemNo, ADrawItemOffs: Integer): Boolean;
      function IsBelowSWord(ADrawItemNo, ADrawItemOffs: Integer): Boolean;
      function IsInSWord(ADrawItemNo, ADrawItemOffs: Integer): Boolean;
      function AreWordsEqual: Boolean;
      function IsEWord(ADrawItemNo,
        ADrawItemWordOffs1, ADrawItemWordOffs2: Integer): Boolean;
      function IsEFreeStateNeeded(ADrawItemNo, ADrawItemOffs: Integer): Boolean;
  end;
  {--------------------------------------------------------------------------- }
  {$IFNDEF RVDONOTUSEDRAGDROP}
  { TCustomRVFormattedData: information about drag&drop caret location.
    See: TCustomRVFormattedData.GetDragDropCaretInfo                           }
  TRVDragDropCaretInfo = class
    public
      X,Y,                       // coordinates relative to RVData top-left
      Height,                    // caret height
      ItemNo, ItemOffs: Integer; // item index and offset in item, in RVData
      RVData: TCustomRVData;     // RVData containing d&d caret
      RefCount: Integer;
  end;
  {$ENDIF}
  {--------------------------------------------------------------------------- }
  { TCustomRVFormattedData: RichView document with formatting.                 }
  TCustomRVFormattedData = class (TCustomRVData)
    private
      LastRaisedCP: TRVCPInfo;
      FXORDrawing: TMouseMoveEvent;
      FCaptureMouseItem: TCustomRVItemInfo;
      FSelectingInfo: TRVSelectingInfo;
      //FZoomPercent: Integer;
      //LastSelectionRect: TRect;
      function FindDrawItemAtPos(X, Y: Integer): Integer;
      procedure CopyText_;
      procedure CopyTextW_;
      procedure CopyImage_;
      {$IFNDEF RVDONOTUSERVF}
      procedure CopyRVF_(Color: TColor; Background: TRVBackground);
      {$ENDIF}
      {$IFNDEF RVDONOTUSERTF}
      procedure CopyRTF_(Color: TColor; Background: TRVBackground);
      {$ENDIF}
      procedure StartFormatting;
      procedure EndFormatting;
      procedure SearchHotItem(X, Y, HOffs, VOffs: Integer);
      procedure AdjustSelection;
      //procedure SetZoomPercent(const Value: Integer);
    protected
      { Protected declarations }
      XorImageDrawn: Boolean;
      MouseX, MouseY: Integer;
      Jumps: TList;
      nJmps: Integer;
      LastItemFormatted: Integer;
      LastJumpMovedAbove, LastDIMovedAbove: Integer;
      LastRVDataMovedAbove: TCustomRVFormattedData;
      LastJumpDowned: Integer;
      FPartialSelectedItem: TCustomRVItemInfo;
      AlreadyFormatted: Boolean;
      FSoftPageBreaks: TRVSoftPageBreakList;
      FClickedDrawItemNo: Integer;
      { Inplace }
      function GetInplaceEditor: TControl; dynamic;
      procedure DestroyInplaceEditor; dynamic;
      { Clearing }
      procedure ClearLastJump;
      { Coordinates }
      procedure ResetSubCoords; dynamic;
      { Drawing item info }
      function IsDrawItemParaStart(DrawItemNo: Integer): Boolean;
      function IsDrawItemParaEnd(DrawItemNo: Integer): Boolean;
      function IsDrawItemFromNewLine(DrawItemNo: Integer): Boolean;
      { Drawing }
      procedure PostPaintTo(Canvas: TCanvas;
        HOffs, VOffs, FirstDrawItemNo, LastDrawItemNo: Integer); virtual;
      { Selecting }
      procedure FindDrawItemForSel(X, Y: Integer; var No, Offs: Integer; Strict: Boolean);      
      function AdjustSelectionByMode(X,Y: Integer): Boolean;
      function AdjustLineSelection(X,Y: Integer): Boolean;
      function ExpandSelectionToParagraph(OnlyIfMultiple: Boolean): Boolean;
      procedure ExpandSelectionToLines(OneLine: Boolean);
      procedure DeselectPartiallySelectedItem(NewPartiallySelected: TCustomRVItemInfo); dynamic;
      procedure SetPartialSelectedItem(Item: TCustomRVItemInfo); virtual;
      { Selection info }
      function IsMultiParagraphSelection: Boolean;
      function IsSelectionTopDown: Boolean;
      function DItem_InsideSelection(DItemNo, DItemOffs: Integer): Boolean;
      function GetClientSelectionRect: TRect;
      {$IFNDEF RVDONOTUSEDRAGDROP}
      { Drag & drop: drop to }
      procedure DrawDragDropCaret(Canvas: TCanvas; OnlyForSelf: Boolean);
      { Drag & drop: drop from }
      function CanStartDragging: Boolean; dynamic;
      function InitDragging(var DropSource: TRVDropSource; var OKEffect: Integer): Boolean; dynamic;
      procedure DoneDragging(FDeleteSelection: Boolean); dynamic;
      {$ENDIF}
      { Formatting }
      procedure FinishScreenLine(const sad: TRVScreenAndDevice;
        LineWidth, LastDrawItem: Integer; parafinished: Boolean;
        var ExtraSpace: Integer; var dontdoit: Boolean; Canvas: TCanvas);
      procedure FormatLine(const Text: String; StartOffs, Len, ItemNo: Integer;
        var x,baseline,prevdesc,prevabove:Integer; Canvas: TCanvas;
        var sad: TRVScreenAndDevice; var IsParaStart: Boolean;
        var LineWidth, FirstIndent, LastDrawItem, VerticalOffs: Integer;
        var DontFSL: Boolean; var LastTextStyle: Integer;
        var TextMetric: TTextMetric; var FirstParaItemNo: Integer;
        NoCaching: Boolean);
      procedure FormatWords(ItemNo: Integer;
        var x,baseline,prevdesc,prevabove:Integer; Canvas: TCanvas;
        var sad: TRVScreenAndDevice; var IsParaStart: Boolean;
        var LineWidth, FirstIndent, LastDrawItem, VerticalOffs: Integer;
        var DontFSL: Boolean; var LastTextStyle: Integer;
        var TextMetric: TTextMetric; var FirstParaItemNo: Integer;
        NoCaching: Boolean);
      procedure GetSADForFormatting(Canvas: TCanvas; var sad: TRVScreenAndDevice);  virtual;
      procedure Formatted(FirstItemNo, LastItemNo: Integer; Partial: Boolean); dynamic;
      { Misc. }
      procedure ConcateItems(FirstItemNo: Integer);
      function InsideWord(DrawItemNo, DrawItemOffs: Integer): Boolean;
      procedure GetWordBounds(DrawItemNo, DrawItemOffs: Integer;
        var DrawItemWordOffs1, DrawItemWordOffs2: Integer);
      procedure GetScreenLineBounds(DrawItemNo: Integer; var First, Last: Integer);
      procedure AfterDeleteStyles(TextStylesShift, ParaStylesShift,
        ListStylesShift: TRVIntegerList); override;
      procedure InternalFreeItem(item: TCustomRVItemInfo; Clearing: Boolean); override;
      procedure ApplyZoom(Canvas: TCanvas);
      procedure ZoomRectDown(var r: TRect);
      procedure ZoomInt(var v: Integer);
      procedure RestoreZoom(Canvas: TCanvas);
      function GetFirstItemMarker(var ListNo, Level: Integer): Boolean; virtual;
      procedure GetIndents(ItemNo: Integer; IsParaStart: Boolean;
        var FirstParaItemNo: Integer; var LeftIndent, RightIndent: Integer);
      function GetMaxIndent(ItemNo: Integer; var FirstParaItemNo: Integer): Integer;
      {$IFNDEF RVDONOTUSERVF}
      procedure RVFGetLimits(SaveScope: TRVFSaveScope;
        var StartItem, EndItem, StartOffs, EndOffs: Integer;
        var StartPart, EndPart: TRVMultiDrawItemPart); override;
      {$ENDIF}
    public
      FActiveItem: TCustomRVItemInfo;
      DocumentWidth: Integer;
      DocumentHeight, TextWidth,FocusedItemNo: Integer;
      DrawItems: TRVDrawLines;
      FSelStartNo, FSelEndNo, FSelStartOffs, FSelEndOffs: Integer;
      { Create & Destroy }
      constructor Create;
      destructor Destroy; override;
      { Clearing }
      procedure ClearTemporal;virtual;
      procedure Clear; override;
      { Soft page breaks }
      procedure AssignSoftPageBreaks(RVPrint: TComponent);
      function ClearSoftPageBreaks: Boolean;
      { Focused items }
      function GetNextFocusedItem(ItemNo: Integer; GoForward: Boolean;
        var TopLevelRVData: TCustomRVFormattedData;
        var TopLevelItemNo: Integer): Integer;
      procedure ClearFocus;
      procedure AdjustFocus(NewFocusedItemNo: Integer;
        TopLevelRVData: TPersistent; TopLevelItemNo: Integer); dynamic;
      { Size and position of document }
      procedure GetOrigin(var ALeft, ATop: Integer); virtual;
      procedure GetOriginEx(var ALeft, ATop: Integer); dynamic;
      function GetAreaWidth: Integer; virtual;                abstract;
      function GetLeftMargin: Integer; virtual;               abstract;
      function GetRightMargin: Integer; virtual;              abstract;
      function GetTopMargin: Integer; virtual;                abstract;
      function GetBottomMargin: Integer; virtual;             abstract;
      function GetMinTextWidth: Integer; virtual;             abstract;
      function GetMaxTextWidth: Integer; virtual;             abstract;
      function GetWidth: Integer; virtual; abstract;
      function GetHeight: Integer; virtual; abstract;
      procedure SetDocumentAreaSize(Width,Height: Integer;
        UpdateH: Boolean); virtual; abstract;
      function CalculatePureParaSectionWidth(ItemNo: Integer;
        var FirstParaItemNo: Integer; sad: PRVScreenAndDevice; Canvas: TCanvas): Integer;
      function CalculateParaSectionMinWidth(StartItemNo: Integer;
        var FirstParaItemNo: Integer; sad: PRVScreenAndDevice;
        Canvas: TCanvas): Integer;
      function CalculateParaSectionMinWidthDef(StartItemNo: Integer): Integer;
      function CalculateParaSectionsMinWidth(StartItemNo, EndItemNo: Integer;
        var FirstParaItemNo: Integer; sad: PRVScreenAndDevice;
        Canvas: TCanvas): Integer;
      function CalculateParaSectionsMinWidthDef(StartItemNo, EndItemNo: Integer): Integer;
      function CalculateMinItemWidthPlus_WithoutPSWidth(ItemNo: Integer;
        sad: PRVScreenAndDevice; Canvas: TCanvas): Integer;
      function CalculateMinItemsWidthPlus(StartItemNo, EndItemNo: Integer;
        var FirstParaItemNo: Integer; sad: PRVScreenAndDevice;
        Canvas: TCanvas): Integer;
      function CalculateMinItemWidthPlusEx(ItemNo: Integer): Integer;
      function CalculateMinItemsWidthPlusEx(StartItemNo, EndItemNo: Integer): Integer;
      function CalculateMinDocWidthPlus(sad: PRVScreenAndDevice; Canvas: TCanvas): Integer;
      function CalculateMinWidthAfterInsert(item:TCustomRVItemInfo; InsertItemNo: Integer): Integer;
      { Coordinates }
      function GetDrawItemNo(BoundLine: Integer; Option: Integer): Integer;
      function GetItemCoords(ItemNo: Integer;var Left,Top: Integer): Boolean;
      function GetItemClientCoords(ItemNo: Integer;var Left,Top: Integer): Boolean;
      function ClientToScreen(const p: TPoint): TPoint; dynamic;
      function ScreenToClient(const p: TPoint): TPoint; dynamic;
      procedure AdjustChildrenCoords;
      { Get ... at (X,Y) }
      procedure GetWordAt(X, Y: Integer; var RVData: TCustomRVFormattedData;
        var ItemNo: Integer; var Word: String);
      function FindWordAt(var Word: String; X, Y: Integer; var StyleNo,
        ItemNo, Offs: Integer; var RVData: TCustomRVFormattedData): Boolean;
      procedure GetItemAt(X,Y: Integer; var ItemNo, OffsetInItem: Integer);
      procedure GetItemAtEx(X,Y: Integer; var RVData: TCustomRVFormattedData;
        var ItemNo, OffsetInItem: Integer; Strict: Boolean;
        var InSubRVDataOwnerItem: Boolean);
      { Scrolling }
      procedure ShowRectangle(Left, Top, Width, Height: Integer); dynamic;
      procedure AdjustVScrollUnits; virtual; abstract;
      function GetHOffs: Integer; virtual;
      function GetVOffs: Integer; virtual;
      function GetZHOffs: Integer;
      function GetZVOffs: Integer;
      procedure ScrollTo(Y: Integer; Redraw: Boolean); virtual; abstract;
      procedure HScrollTo(X: Integer); virtual; abstract;
      function GetVSmallStep: Integer; virtual; abstract;
      procedure AfterVScroll;
      procedure OnTimerScroll;
      { Calling events }
      procedure DoCopy; dynamic;
      function IsAssignedCopy: Boolean; dynamic;
      function IsAssignedRVMouseUp: Boolean; dynamic; abstract;
      function IsAssignedRVMouseDown: Boolean; dynamic; abstract;
      function IsAssignedRVRightClick: Boolean; dynamic;
      function IsAssignedJump: Boolean; dynamic; abstract;
      function IsAssignedRVDblClick: Boolean; dynamic;
      function IsAssignedCheckpointVisible: Boolean; dynamic; abstract;
      procedure DoRVMouseUp(Button: TMouseButton; Shift: TShiftState;
        ItemNo, X, Y: Integer); dynamic; abstract;
      procedure DoRVMouseDown(Button: TMouseButton; Shift: TShiftState;
                            ItemNo, X, Y: Integer); dynamic; abstract;
      procedure DoRVRightClick(const ClickedWord: String; StyleNo, X, Y: Integer);dynamic;
      procedure DoJump(id: Integer); dynamic; abstract;
      procedure DoRVMouseMove(id: Integer); dynamic; abstract;
      procedure DoRVDblClick(const ClickedWord: String; StyleNo: Integer); dynamic;
      procedure DoSelect; dynamic;
      procedure DoCheckpointVisible(CheckpointData: TCheckpointData); dynamic; abstract;
      { Properties }
      function GetNormalCursor: TCursor; dynamic; abstract;
      function GetCPEventKind: TCPEventKind; dynamic;
      function GetBackground: TRVBackground; virtual; abstract;
      function GetCanvas: TCanvas; virtual;
      function GetColor: TColor; virtual; abstract;
      procedure SetCursor(Cursor: TCursor); dynamic;
      function GetEditor: TWinControl; dynamic;
      { Mouse }
      procedure MouseLeave;
      procedure MouseMove(Shift: TShiftState; X, Y: Integer); dynamic;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);dynamic;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);dynamic;
      procedure DblClick;
      procedure AdjustMouseUpSelection; dynamic;
      { Mouse capture }
      procedure SetMouseCapture(Item: TCustomRVItemInfo;  var Left,Top: Integer);
      procedure ReleaseMouseCapture(Item: TCustomRVItemInfo);
      {$IFNDEF RVDONOTUSEDRAGDROP}
      { Drag & drop: drop to }
      function GetDragDropCaretInfo: TRVDragDropCaretInfo; dynamic;      
      procedure SetDragDropCaretTo(X,Y: Integer);
      procedure RemoveDragDropCaret;
      procedure SetDragDropCaret(ItemNo, Offs: Integer);
      { Drag & drop: drop from }
      procedure DoDrag;
      function IsDragging: Boolean; dynamic;
      {$ENDIF}
      { Insertion and protection (used in editor or by drag&drop) }
      function CanInsertHere(ItemNo, Offs: Integer): Boolean;
      function IsProtected(ItemNo: Integer; Option: TRVProtectOption): Boolean;
      function IsParaProtected(ParaNo: Integer; Option: TRVParaOption): Boolean;
      function IsItemParaProtected(ItemNo: Integer): Boolean;
      function IsSticking(FirstItemNo: Integer; NoSound: Boolean): Boolean;
      { Drawing }
      procedure DrawBackground(Canvas: TCanvas; r: TRect); virtual; 
      procedure PaintTo(Canvas: TCanvas; AClipRect: TRect);
      { Redrawing }
      procedure InvalidateJumpRect(id: Integer);
      procedure Refresh;
      procedure UpdateView;
      procedure Invalidate;
      procedure InvalidateRect(const r: TRect);
      { Palette }
      procedure UpdateBackgroundPaletteInfo(Background: TRVBackground);
      function UpdatingBackgroundPalette: Boolean;
      { Drawing XOR }
      procedure XorDrawing; virtual;
      procedure XorDrawingEx(X,Y: Integer);
      function ClearXorDrawing: Boolean;
      procedure AssignXorDrawing(P: TMouseMoveEvent);
      function UsingThisXorDrawing(P: TMouseMoveEvent): Boolean;
      procedure UnAssignXorDrawing(P: TMouseMoveEvent);
      { Formatting }
      procedure FormatParas(StartDrawItemNo, EndDrawItemNo, ItemsInserted: Integer);
      procedure FormatParasExact(StartDrawItemNo, EndDrawItemNo,
        ItemsInserted: Integer; NoCaching: Boolean);
      procedure Format_(OnlyResized, ForceFormat, NoScroll: Boolean;
        depth: Integer; Canvas: TCanvas; OnlyTail, NoCaching: Boolean); virtual;
      procedure Format(NoCaching: Boolean);
      { Selecting }
      procedure SetSelectionBounds(StartItemNo, StartItemOffs, EndItemNo,
        EndItemOffs: Integer);
      function SearchText(Down, MatchCase, WholeWord: Boolean; s: String): Boolean;
      procedure SelectWordAt(X, Y: Integer);
      function SelectControl(AControl: TControl): Boolean;
      procedure Deselect(NewPartiallySelected: TCustomRVItemInfo;
        MakeEvent: Boolean); dynamic;
      procedure SelectAll;
      procedure SelectLine(ItemNo, Offs: Integer);
      procedure RestoreSelBounds(StartNo, EndNo, StartOffs, EndOffs: Integer);
      procedure SrchSelectIt(strt, offs, len: Integer; Invert: Boolean); dynamic;
      procedure SrchStart(Down: Boolean; var strt, offs: Integer);dynamic;
      { Chosen RVData & Item }
      procedure AssignChosenRVData(RVData: TCustomRVFormattedData;
        Item: TCustomRVItemInfo); dynamic;
      procedure SilentReplaceChosenRVData(RVData: TCustomRVFormattedData); dynamic;
      procedure UnassignChosenRVData(RVData: TCustomRVData); dynamic;
      { Get selection }
      function SelectionExists(AllowReset: Boolean;
        UsePartialSelected: Boolean): Boolean;
      procedure GetSelectionBounds(var StartItemNo, StartItemOffs, EndItemNo,
        EndItemOffs: Integer; Normalize: Boolean);
      procedure GetSelBounds(var StartNo, EndNo, StartOffs, EndOffs: Integer;
        Normalize: Boolean);
      procedure StoreSelBounds(var StartNo, EndNo, StartOffs, EndOffs: Integer;
        Normalize: Boolean);
      procedure GetSelectionBoundsEx(var StartItemNo, StartItemOffs, EndItemNo,
        EndItemOffs: Integer; Normalize: Boolean); dynamic;
      procedure GetSelStart(var DINo, DIOffs: Integer);dynamic;
      function GetSelText(Unicode: Boolean): String;
      function GetSelectedImage: TGraphic;
      function GetSelectionRect: TRect;
      { Selection misc. }
      procedure DoOnSelection(AllowScrolling: Boolean); dynamic;
      function Item_InsideSelection(ItemNo, ItemOffs: Integer): Boolean;      
      { Checkpoints }
      procedure GetCheckpointXY(CheckpointData: TCheckpointData; var X, Y: Integer);
      function  GetCheckpointYEx(CheckpointData: TCheckpointData): Integer;
      { Hypertext }
      procedure BuildJumpsCoords(var StartJumpNo: Integer; jumps: TList);
      procedure ClearJumps;
      procedure AdjustJumpsCoords;
      function  GetJumpPointY(id: Integer): Integer;
      function  GetJumpPointItemNo(id: Integer): Integer;
      procedure GetJumpPointLocation(id: Integer; var RVData: TCustomRVFormattedData; var ItemNo: Integer);
      { Copying to the Clipboard }
      {$IFNDEF RVDONOTUSERVF}
      procedure CopyRVF(Color: TColor; Background: TRVBackground);
      {$ENDIF}
      {$IFNDEF RVDONOTUSERTF}
      procedure CopyRTF(Color: TColor; Background: TRVBackground);
      {$ENDIF}
      procedure CopyText;
      procedure CopyTextW;
      procedure CopyImage;
      procedure Copy(Color: TColor; Background: TRVBackground);
      function  CopyDef(Color: TColor; Background: TRVBackground): Boolean;
      { Items and drawitems indices, line numbers }
      function GetLineNo(ItemNo, ItemOffs: Integer): Integer;
      procedure GetParaBounds(DINo1, DINo2: Integer; var ParaStart, ParaEnd: Integer);
      function GetFirstVisible(TopLine: Integer): Integer;
      function GetFirstItemVisible: Integer; dynamic;
      function GetLastItemVisible: Integer; dynamic;
      function GetOffsBeforeDrawItem(DrawItemNo: Integer): Integer;
      function GetOffsAfterDrawItem(DrawItemNo: Integer): Integer;
      procedure DrawItem2Item(DrawItemNo, DrawItemOffs: Integer; var ItemNo, ItemOffs: Integer);
      procedure Item2DrawItem(ItemNo, ItemOffs: Integer; var DrawItemNo, DrawItemOffs: Integer);
      procedure Item2FirstDrawItem(ItemNo: Integer; var DrawItemNo: Integer);
      function FindDrawItemByItem(ItemNo: Integer): Integer;
      { Deleting }
      procedure DeleteItems(FirstItemNo, Count: Integer); override;
      procedure DeleteParas(FirstItemNo, LastItemNo: Integer);
      { Others }
      procedure Normalize;
      { Properties }
      property PartialSelectedItem: TCustomRVItemInfo
        read FPartialSelectedItem write SetPartialSelectedItem;
      property CaptureMouseItem: TCustomRVItemInfo read FCaptureMouseItem;
      property SoftPageBreaks: TRVSoftPageBreakList read FSoftPageBreaks;
      //property ZoomPercent:Integer read FZoomPercent write SetZoomPercent;
  end;
const
  RichViewSafeFormatting:Boolean = False;

implementation
uses RichView, RVCtrlData, RVStr, PtblRV, PtRVData
    {$IFNDEF RVDONOTUSELISTS}
    , RVMarker
    {$ENDIF}
    ;
{==============================================================================}
const gdinFirstVisible        = 1;
const gdinLastCompleteVisible = 2;
const gdinLastVisible         = 3;
const MAXLINELENGTH           = 1000;
{------------------------------------------------------------------------------}
function MyStrRScanA(Str: PChar; Chr: Char; Length: Integer): PChar;
begin
  Result := Str+Length-1;
  while Result>=Str do begin
    if Result^=Chr then
      exit;
    dec(Result);
  end;
  Result := nil;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEUNICODE}
{
function MyStrRScanW(Str: PChar; Chr: Char; Length: Integer): PChar;
begin
  Result := Str+(Length-1)*2;
  while Result>=Str do begin
    if (Result^=Chr) and ((Result+1)^=#0) then
      exit;
    dec(Result, 2);
  end;
  Result := nil;
end;
}
{$ENDIF}
{------------------------------------------------------------------------------}
function max(a,b: Integer): Integer;
begin
  if a>b then
    max := a
  else
    max := b;
end;
{================================ TRVSelectingInfo ============================}
{ Initializing ending values with the starting values                          }
procedure TRVSelectingInfo.InitE(ASWordState: TRVSelectingState);
begin
  DrawItemEOffs := DrawItemSOffs;
  DrawItemENo := DrawItemSNo;
  DrawItemEWordOffs1 := DrawItemSWordOffs1;
  DrawItemEWordOffs2 := DrawItemSWordOffs2;
  if ASWordState=rvsesParaMode then begin
    SWordState := rvsesParaMode;
    EWordState := rvsesParaMode;
    end
  else begin
    SWordState := ASWordState;
    EWordState := rvsesInWord;
  end;
end;
{------------------------------------------------------------------------------}
{ Is (ADrawItemNo, ADrawItemOffs) in inside the starting word?                 }
function TRVSelectingInfo.IsInSWord(ADrawItemNo, ADrawItemOffs: Integer): Boolean;
begin
  Result := (ADrawItemNo=DrawItemSNo) and
            (ADrawItemOffs>=DrawItemSWordOffs1) and
            (ADrawItemOffs<=DrawItemSWordOffs2)
end;
{------------------------------------------------------------------------------}
{ Is (ADrawItemNo, ADrawItemOffs) below the starting word?                     }
function TRVSelectingInfo.IsBelowSWord(ADrawItemNo, ADrawItemOffs: Integer): Boolean;
begin
  Result := (ADrawItemNo>DrawItemSNo) or
            ((ADrawItemNo=DrawItemSNo) and (ADrawItemOffs>DrawItemSWordOffs2));
end;
{------------------------------------------------------------------------------}
{ Is (ADrawItemNo, ADrawItemOffs) above the starting word?                     }
function TRVSelectingInfo.IsAboveSWord(ADrawItemNo, ADrawItemOffs: Integer): Boolean;
begin
  Result := (ADrawItemNo<DrawItemSNo) or
            ((ADrawItemNo=DrawItemSNo) and (ADrawItemOffs<DrawItemSWordOffs1));
end;
{------------------------------------------------------------------------------}
{ Are the positions of the ending and the starting words equal?                }
function TRVSelectingInfo.AreWordsEqual: Boolean;
begin
  Result := (DrawItemENo=DrawItemSNo) and
            (DrawItemEWordOffs1=DrawItemSWordOffs1) and
            (DrawItemEWordOffs2=DrawItemSWordOffs2);
end;
{------------------------------------------------------------------------------}
{ Are the parameters equal to the ending word?                                 }
function TRVSelectingInfo.IsEWord(ADrawItemNo,
  ADrawItemWordOffs1, ADrawItemWordOffs2: Integer): Boolean;
begin
  Result := (DrawItemENo=DrawItemENo) and
            (DrawItemEWordOffs1=ADrawItemWordOffs1) and
            (DrawItemEWordOffs2=ADrawItemWordOffs2);
end;
{------------------------------------------------------------------------------}
{ Should we change EWordState to rvsesFreeMode?
  This function assumes that the ending point of the selection
  (ADrawItemNo, ADrawItemOffs) is inside the ending word                       }
function TRVSelectingInfo.IsEFreeStateNeeded(ADrawItemNo,
  ADrawItemOffs: Integer): Boolean;
begin
  Result :=
    (IsAboveSWord(ADrawItemNo,ADrawItemOffs) and (ADrawItemOffs>DrawItemEOffs))
    or
    (IsBelowSWord(ADrawItemNo,ADrawItemOffs) and (ADrawItemOffs<DrawItemEOffs));
end;
{=========================== TRVSoftPageBreakList =============================}
function TRVSoftPageBreakList.FindItem(ItemNo: Integer): Integer;
var a,b,no: Integer;
  {....................................}
  procedure AdjustResult(var R: Integer);
  begin
    while (R>0) and (TRVSoftPageBreakInfo(Items[R-1]).ItemNo=ItemNo) do
      dec(R);

  end;
  {....................................}
begin
  a := 0;
  b := Count-1;
  while b-a>1 do begin
    Result := (a+b) div 2;
    no := TRVSoftPageBreakInfo(Items[Result]).ItemNo;
    if no=ItemNo then begin
      AdjustResult(Result);
      exit;
    end;
    if no<ItemNo then
      a := Result
    else
      b := Result;
  end;
  if TRVSoftPageBreakInfo(Items[a]).ItemNo=ItemNo then
    Result := a
  else if TRVSoftPageBreakInfo(Items[b]).ItemNo=ItemNo then
    Result := b
  else
    Result := -1;
  AdjustResult(Result);
end;
{============================ TCustomRVFormattedData ==========================}
constructor TCustomRVFormattedData.Create;
begin
  inherited Create;
  DrawItems := TRVDrawLines.Create;
  Jumps     := TList.Create;
  FSelStartNo    := -1;
  FSelEndNo      := -1;
  FSelStartOffs  := 0;
  FSelEndOffs    := 0;
  TextWidth      := -1;
  DocumentWidth  := -1;
  DocumentHeight := 0;//GetTopMargin;
  LastItemFormatted  := -1;
  LastJumpMovedAbove := -1;
  LastDIMovedAbove   := -1;
  LastJumpDowned     := -1;
  FocusedItemNo      := -1;
  FActiveItem        := nil;
  FPartialSelectedItem :=  nil;
  //FZoomPercent       := 100;
end;
{------------------------------------------------------------------------------}
destructor TCustomRVFormattedData.Destroy;
begin
  ClearTemporal;
  DrawItems.Free;
  DrawItems := nil;
  Jumps.Free;
  Jumps  := nil;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.ClearLastJump;
begin
  if LastRVDataMovedAbove<>nil then begin
    LastRVDataMovedAbove.State := LastRVDataMovedAbove.State - [rvstDrawHover];
    LastRVDataMovedAbove.LastJumpMovedAbove := -1;
    LastRVDataMovedAbove.LastJumpDowned     := -1;
    DoRVMouseMove(-1);
  end;
  State := State - [rvstDrawHover];
  LastJumpMovedAbove := -1;
  LastJumpDowned     := -1;
  LastRVDataMovedAbove := nil;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.ClearJumps;
var i: Integer;
begin
  if Jumps<>nil then begin
    for i:=0 to Jumps.Count-1 do
      TObject(Jumps[i]).Free;
    Jumps.Clear;
    ClearLastJump;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.ClearTemporal;
var i: Integer;
begin
  if DrawItems<>nil then begin
    for i:=0 to DrawItems.Count-1 do
      DrawItems[i].Free;
    DrawItems.Clear;
    ClearJumps;
    nJmps        := 0;
    FActiveItem  := nil;
    //FPartialSelectedItem := nil;
    FCaptureMouseItem := nil;
    LastDIMovedAbove := -1;
    LastRVDataMovedAbove := nil;
    State := State - [ rvstMakingSelection, rvstLineSelection, rvstDrawHover];
    LastRaisedCP := nil;
    FClickedDrawItemNo := -1;
    FSelectingInfo.Free;
    FSelectingInfo := nil;
    LastItemFormatted := -1;
    DocumentHeight := GetTopMargin+GetBottomMargin;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.Clear;
var Clearing: Boolean;
begin
  Clearing := rvstClearing in State;
  Include(State, rvstClearing);
  try
    ClearSoftPageBreaks;
    Deselect(nil, True);
    ClearTemporal;
    inherited Clear;
  finally
    if not Clearing then
      Exclude(State, rvstClearing);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.Deselect(NewPartiallySelected: TCustomRVItemInfo;
                                          MakeEvent: Boolean);
begin
  if rvstDeselecting in State then
    exit;
  State := State + [rvstDeselecting];
  try
    State := State - [rvstMakingSelection, rvstLineSelection];
    FSelStartNo   := -1;
    FSelEndNo     := -1;
    FSelStartOffs := 0;
    FSelEndOffs   := 0;
    FocusedItemNo := -1;
    DeselectPartiallySelectedItem(NewPartiallySelected);
    if MakeEvent then
      DoSelect;
  finally
    State := State - [rvstDeselecting];
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.SelectAll;
begin
  if Items.Count=0 then
    exit;
  DestroyInplaceEditor;
  DeselectPartiallySelectedItem(nil);
  FSelStartNo   := 0;
  FSelEndNo     := DrawItems.Count-1;
  FSelStartOffs := GetOffsBeforeDrawItem(0);
  FSelEndOffs   := GetOffsAfterDrawItem(FSelEndNo);
  DoOnSelection(False);
  DoSelect;
end;

procedure TCustomRVFormattedData.ConcateItems(FirstItemNo: Integer);
var item1,item2: TCustomRVItemInfo;
begin
  item1 := TCustomRVItemInfo(Items.Objects[FirstItemNo]);
  item2 := TCustomRVItemInfo(Items.Objects[FirstItemNo+1]);
  if ((rvioUnicode in item1.ItemOptions)<>
     (rvioUnicode in item2.ItemOptions)) and
     (Items[FirstItemNo+1]<>'') then begin
    item2.SameAsPrev := item1.SameAsPrev;
    item2.BR := item1.BR;
    FreeItem(FirstItemNo, False);
    Items.Delete(FirstItemNo);
    end
  else begin
    Items[FirstItemNo] := Items[FirstItemNo]+Items[FirstItemNo+1];
    FreeItem(FirstItemNo+1, False);
    Items.Delete(FirstItemNo+1);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.Normalize;
var li, li2: TCustomRVItemInfo;
    i: Integer;
begin
  for i := Items.Count-1 downto 1 do begin
    li := TCustomRVItemInfo(Items.Objects[i-1]);
    li2 := TCustomRVItemInfo(Items.Objects[i]);
    if RV_CanConcateItems(i-1, li,li2,False) then
      ConcateItems(i-1);
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetOffsBeforeDrawItem(DrawItemNo: Integer): Integer;
begin
  if GetItemStyle(DrawItems[DrawItemNo].ItemNo)<0 then
    Result := 0
  else
    Result := 1;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetOffsAfterDrawItem(DrawItemNo: Integer): Integer;
begin
  if GetItemStyle(DrawItems[DrawItemNo].ItemNo)<0 then
    Result := 1
  else
    Result := DrawItems[DrawItemNo].Length+1;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.FinishScreenLine(const sad: TRVScreenAndDevice;
  LineWidth, LastDrawItem: Integer; parafinished: Boolean;
  var ExtraSpace: Integer; var dontdoit: Boolean; Canvas: TCanvas);
var  pi: TParaInfo;
     BiDiMode: TRVBiDiMode;
     Alignment: TRVAlignment;
  {.......................................................}
  function  GetDevY(ScreenY: Integer): Integer;
  begin
    Result := MulDiv(ScreenY, sad.ppiyDevice, sad.ppiyScreen);
  end;
  {.......................................................}
  function  GetDevX(ScreenX: Integer): Integer;
  begin
    Result := MulDiv(ScreenX, sad.ppixDevice, sad.ppixScreen);
  end;
  {.......................................................}
  procedure RearrangeMarker(FirstDrawItem, LastDrawItem: Integer);
  var extrax, i: Integer;
  begin
    if not GetItem(DrawItems[FirstDrawItem].ItemNo).SameAsPrev and
       (DrawItems[FirstDrawItem].Offs<=GetOffsBeforeItem(DrawItems[FirstDrawItem].ItemNo)) then begin
       {$IFNDEF RVDONOTUSELISTS}
       if (GetItemStyle(DrawItems[FirstDrawItem].ItemNo)=rvsListMarker) then begin
         extrax := GetDevX(-TRVMarkerItemInfo(GetItem(DrawItems[FirstDrawItem].ItemNo)).GetLeftOverhang);
         end
       else
       {$ENDIF}
       begin
         extrax := GetDevX(pi.FirstIndent);
       end;
      if extrax<>0 then
        for i := FirstDrawItem to LastDrawItem do
          dec(DrawItems[i].Left, extrax);
    end;
  end;
  {.......................................................}
  function GetDrawItemBiDi(DrawItemNo: Integer; var Start, Ending: TRVBiDiMode): Boolean;
  var item: TCustomRVItemInfo;
      Locale: Cardinal;
      Len: Integer;
      s: String;
      CharTypes: PWordArray;
  begin
    Start  := BiDiMode;
    Ending := BiDiMode;
    Result := True;
    item := GetItem(DrawItems[DrawItemNo].ItemNo);
    if item.StyleNo<0 then
      exit;
    s := DrawItems.GetString(DrawItemNo, Items);
    if s='' then
      exit;
    {$IFNDEF RVDONOTUSEUNICODE}
    if GetRVStyle.TextStyles[GetActualStyle(item)].Unicode then
      Len := Length(s) div 2
    else
    {$ENDIF}
      Len := Length(s);
    Locale := GetStyleLocale(GetActualStyle(item));
    if GetRVStyle.TextStyles[GetActualStyle(item)].BiDiMode<>rvbdUnspecified then begin
      Start  := GetRVStyle.TextStyles[GetActualStyle(item)].BiDiMode;
      Ending := GetRVStyle.TextStyles[GetActualStyle(item)].BiDiMode;
    end;
    GetMem(CharTypes, Len*sizeof(Word));
    try
      {$IFNDEF RVDONOTUSEUNICODE}
      if GetRVStyle.TextStyles[GetActualStyle(item)].Unicode then
        GetStringTypeExW(Locale, CT_CTYPE2, Pointer(s), Len, CharTypes^)
      else
      {$ENDIF}
        GetStringTypeExA(Locale, CT_CTYPE2, Pointer(s), Len, CharTypes^);
      case CharTypes[0] of
        C2_LEFTTORIGHT:
          Start := rvbdLeftToRight;
        C2_RIGHTTOLEFT:
          Start := rvbdRightToLeft;
      end;
      case CharTypes[Len-1] of
        C2_LEFTTORIGHT:
          Ending := rvbdLeftToRight;
        C2_RIGHTTOLEFT:
          Ending := rvbdRightToLeft;
      end;
    finally
      FreeMem(CharTypes);
    end;
  end;
  {.......................................................}
  function RearrangeLineEx(LastDrawItem: Integer): Boolean;
  var FirstDrawItem, i,insertpoint: Integer;
      extrax, curx: Integer;
      diorder: TRVIntegerList;
      CurBiDi, PrevBiDi, StartBiDi, EndBiDi: TRVBiDiMode;
  begin
    Result := True;
    if BiDiMode=rvbdUnspecified then
      exit;
    FirstDrawItem := LastDrawItem;
    while (FirstDrawItem>0) and not DrawItems[FirstDrawItem].FromNewLine do
      dec(FirstDrawItem);
    if BiDiMode=rvbdRightToLeft then
      RearrangeMarker(FirstDrawItem, LastDrawItem);
    if FirstDrawItem=LastDrawItem then
      exit;
    insertpoint := 0;
    diorder := TRVIntegerList.Create;
    try
      PrevBiDi := BiDiMode;
      for i := FirstDrawItem to LastDrawItem do begin
        if not GetDrawItemBiDi(i, StartBiDi, EndBiDi) then begin
          diorder.Free;
          Result := False;
          exit;
        end;
        CurBiDi := BiDiMode;
        if PrevBiDi=StartBiDi then
          CurBiDi := PrevBiDi;
        PrevBiDi := EndBiDi;
        if CurBiDi=rvbdLeftToRight then begin
          if insertpoint<diorder.Count then
            inc(insertpoint);
        end;
        diorder.Insert(insertpoint, i);
      end;
      curx := DrawItems[FirstDrawItem].Left;
      for i := 0 to diorder.Count-1 do begin
        if diorder[i]>FirstDrawItem then
          extrax := DrawItems[diorder[i]].Left-(DrawItems[diorder[i]-1].Left+DrawItems[diorder[i]-1].Width)
        else
          extrax := 0;
        DrawItems[diorder[i]].Left := curx;
        inc(curx, DrawItems[diorder[i]].Width+extrax);
      end;
    finally
      diorder.Free;
    end;
  end;
  {.......................................................}
  procedure RearrangeLine(LastDrawItem: Integer);
  var FirstDrawItem, i: Integer;
      curx, extrax: Integer;
  begin
    if BiDiMode<>rvbdRightToLeft then
      exit;
    FirstDrawItem := LastDrawItem;
    while (FirstDrawItem>0) and
          not DrawItems[FirstDrawItem].FromNewLine do
      dec(FirstDrawItem);
    RearrangeMarker(FirstDrawItem, LastDrawItem);
    if FirstDrawItem=LastDrawItem then
      exit;
    curx := DrawItems[FirstDrawItem].Left;
    for i := LastDrawItem downto FirstDrawItem do begin
      if i>FirstDrawItem then
        extrax := DrawItems[i].Left-(DrawItems[i-1].Left+DrawItems[i-1].Width)
      else
        extrax := 0;
      DrawItems[i].Left := curx;
      inc(curx, DrawItems[i].Width+extrax);
    end;
  end;
  {.......................................................}
var i, j, dx, min,max: Integer;
    fl: Boolean;
    dli: TRVDrawLineInfo;
    item: TCustomRVItemInfo;
    {$IFNDEF RVDONOTUSEJUSTIFY}
    SpaceCount, strt: Integer;
    s:  String;
    {$ENDIF}
begin
  ExtraSpace := 0;
  if dontdoit then begin
    dontdoit := False;
    exit;
  end;
  i := LastDrawItem-1;
  if i=-1 then
    exit;
  dli := DrawItems[i];
  item := GetItem(dli.ItemNo);
  if item.GetBoolValue(rvbpFullWidth) then
    exit;
  pi := GetRVStyle.ParaStyles[item.ParaNo];
  BiDiMode := GetParaBiDiMode(item.ParaNo);
  Alignment := pi.Alignment;
  if (Alignment=rvaJustify)
    {$IFNDEF RVDONOTUSEJUSTIFY}
      and (BiDiMode<>rvbdUnspecified)
    {$ENDIF}
    then
    Alignment := rvaLeft;
  if (Alignment = rvaLeft) or ((Alignment = rvaJustify) and ParaFinished) then
    dx := 0
  else begin
    dx := LineWidth - (dli.Left+dli.Width);
    if Alignment = rvaCenter then begin
      dx := dx div 2;
      if dx<0 then
        dx := 0;
    end;
  end;
  case pi.LineSpacingType of
    rvlsSpaceBetween:
      if pi.LineSpacing>=0 then
        ExtraSpace := GetDevY(pi.LineSpacing);
    rvlsPercent:
      begin
        if pi.LineSpacing>=50 then begin
          j := i;
          min := 0;
          max := 0;
          fl  := True;
          repeat
            if GetItemStyle(DrawItems[j].ItemNo)>=0 then begin
              if fl or (DrawItems[j].Top<min) then
                min := DrawItems[j].Top;
              if fl or (DrawItems[j].Top+DrawItems[j].Height>max) then
                max := DrawItems[j].Top+DrawItems[j].Height;
              fl := False;
            end;
            dec(j);
          until (j<0) or DrawItems[j+1].FromNewLine;
          if not fl then
            ExtraSpace := (max-min)*(pi.LineSpacing-100) div 100;
        end;
      end;
  end;
  if ExtraSpace<>0 then begin
    j := i;
    repeat
      DrawItems[j].ExtraSpaceBelow := ExtraSpace;
      dec(j);
    until (j<0) or DrawItems[j+1].FromNewLine;
  end;
  {$IFNDEF RVDONOTUSEJUSTIFY}
  if (Alignment = rvaJustify) and not ParaFinished then begin
    if DrawItems[i].FromNewLine then
      exit;
    SpaceCount := 0;
    while (i>=0) and not DrawItems[i].FromNewLine do
      with DrawItems[i] do begin
        if (GetItemStyle(ItemNo)>=0) then begin
          s := DrawItems.GetString(i,Items);
          if (System.Length(s)>0) and RVU_IsSpace(s, 1, GetItemOptions(ItemNo)) then
            inc(SpaceCount)
        end;
        dec(i);
      end;
    strt := i+1;
    for i := strt to LastDrawItem-1 do
      with DrawItems[i] do begin
        if (GetItemStyle(ItemNo)>=0) then begin
          s := DrawItems.GetString(i,Items);
          if (System.Length(s)>0) and RVU_IsSpace(s, 1, GetItemOptions(ItemNo)) then begin
            SpaceBefore := dx div SpaceCount;
            inc(Width,SpaceBefore);
            dec(dx,SpaceBefore);
            dec(SpaceCount);
          end;
        end;
        if not FromNewLine then
          Left := DrawItems[i-1].Left+DrawItems[i-1].Width;
      end;
    RearrangeLineEx(LastDrawItem-1);
    exit;
  end;
  {$ENDIF}
  while (i>=0) do begin
    with DrawItems[i] do begin
      inc(Left,dx);
      if FromNewLine then begin
        RearrangeLineEx(LastDrawItem-1);
        exit;
      end;
    end;
    dec(i);
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetFirstItemMarker(var ListNo, Level: Integer): Boolean;
begin
  ListNo := -1;
  Level  := -1;
  Result := False;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.GetIndents(ItemNo: Integer; IsParaStart: Boolean;
  var FirstParaItemNo: Integer; var LeftIndent, RightIndent: Integer);
var RVStyle: TRVStyle;
    item   : TCustomRVItemInfo;
    {$IFNDEF RVDONOTUSELISTS}
    item2  : TCustomRVItemInfo;
    bidimode: TRVBiDiMode;
    ListNo, Level: Integer;
    {$ENDIF}
    pi     : TParaInfo;
begin
  RVStyle := GetRVStyle;
  item  := GetItem(ItemNo);
  {$IFNDEF RVDONOTUSELISTS}
  bidimode := GetParaBiDiMode(item.ParaNo);
  {$ENDIF}
  pi    := RVStyle.ParaStyles[item.ParaNo];

  if (rvstFirstParaAborted in State) then begin
    if FirstParaItemNo<0 then
      FirstParaItemNo := GetFirstParaItem(ItemNo);
    if FirstParaItemNo=0 then
      IsParaStart := False;
  end;
  if IsParaStart then begin
    {$IFNDEF RVDONOTUSELISTS}
    if (item.StyleNo=rvsListMarker) and (TRVMarkerItemInfo(item).GetLevelInfo(RVStyle)<>nil) then
      if bidimode=rvbdRightToLeft then begin
        RightIndent := TRVMarkerItemInfo(item).GetLevelInfo(RVStyle).MarkerIndent;
        LeftIndent  := pi.LeftIndent;
        end
      else begin
        LeftIndent := TRVMarkerItemInfo(item).GetLevelInfo(RVStyle).MarkerIndent;
        RightIndent  := pi.RightIndent;
      end
    else begin
      if FirstParaItemNo<0 then
        FirstParaItemNo := GetFirstParaItem(ItemNo);
      item2 := GetItem(FirstParaItemNo);
      if (item2.StyleNo=rvsListMarker) and (TRVMarkerItemInfo(item2).GetLevelInfo(RVStyle)<>nil) then
        if bidimode=rvbdRightToLeft then begin
          RightIndent := TRVMarkerItemInfo(item2).GetLevelInfo(RVStyle).LeftIndent;
          LeftIndent  := pi.LeftIndent;
          end
        else begin
          LeftIndent := TRVMarkerItemInfo(item2).GetLevelInfo(RVStyle).LeftIndent;
          RightIndent  := pi.RightIndent;
        end
      else
      {$ENDIF}
      begin
        LeftIndent := pi.LeftIndent;
        if not item.BR and not item.GetBoolValue(rvbpFullWidth) then
           inc(LeftIndent, pi.FirstIndent);
         RightIndent := pi.RightIndent;
      end;
    {$IFNDEF RVDONOTUSELISTS}
    end;
    {$ENDIF}
    end
  else begin
    {$IFNDEF RVDONOTUSELISTS}
    if FirstParaItemNo<0 then
      FirstParaItemNo := GetFirstParaItem(ItemNo);
    item2 := GetItem(FirstParaItemNo);
    if (item2.StyleNo=rvsListMarker) and (TRVMarkerItemInfo(item2).GetLevelInfo(RVStyle)<>nil) then begin
      if bidimode=rvbdRightToLeft then begin
        RightIndent := TRVMarkerItemInfo(item2).GetLevelInfo(RVStyle).LeftIndent;
        LeftIndent  := pi.LeftIndent;
        end
      else begin
        LeftIndent := TRVMarkerItemInfo(item2).GetLevelInfo(RVStyle).LeftIndent;
        RightIndent  := pi.RightIndent;
      end
      end
    else if (FirstParaItemNo=0) and (rvstFirstParaAborted in State) and
      GetFirstItemMarker(ListNo, Level) then begin
      if bidimode=rvbdRightToLeft then begin
        RightIndent := RVGetLevelInfo(RVStyle, ListNo, Level).LeftIndent;
        LeftIndent  := pi.LeftIndent;
        end
      else begin
        LeftIndent := RVGetLevelInfo(RVStyle, ListNo, Level).LeftIndent;
        RightIndent  := pi.RightIndent;
      end
      end
    else
    {$ENDIF}
    begin
      LeftIndent := pi.LeftIndent;
      RightIndent := pi.RightIndent;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetMaxIndent(ItemNo: Integer; var FirstParaItemNo: Integer): Integer;
var RVStyle: TRVStyle;
    item: TCustomRVItemInfo;
    pi     : TParaInfo;
    {$IFNDEF RVDONOTUSELISTS}
    levelinfo: TRVListLevel;
    item2   : TCustomRVItemInfo;
    bidimode: TRVBiDiMode;
    ListNo, Level: Integer;
    {$ENDIF}
begin
  RVStyle := GetRVStyle;
  item  := GetItem(ItemNo);
  {$IFNDEF RVDONOTUSELISTS}
  bidimode := GetParaBiDiMode(item.ParaNo);
  {$ENDIF}
  pi    := RVStyle.ParaStyles[item.ParaNo];
  {$IFNDEF RVDONOTUSELISTS}
  if FirstParaItemNo<0 then
    FirstParaItemNo := GetFirstParaItem(ItemNo);
  item2 := GetItem(FirstParaItemNo);
  if (item2.StyleNo=rvsListMarker) and (TRVMarkerItemInfo(item2).GetLevelInfo(RVStyle)<>nil) then
    levelinfo := TRVMarkerItemInfo(item2).GetLevelInfo(RVStyle)
  else if (FirstParaItemNo=0) and (rvstFirstParaAborted in State) and GetFirstItemMarker(ListNo, Level) then
    levelinfo := RVGetLevelInfo(RVStyle, ListNo, Level)
  else
    levelinfo := nil;
  if levelinfo<>nil then begin
    Result := levelinfo.LeftIndent;
    if levelinfo.FirstIndent>0 then
      inc(Result,levelinfo.FirstIndent);
    if bidimode=rvbdRightToLeft then
      inc(Result, pi.LeftIndent)
    else
      inc(Result, pi.RightIndent);
    end
  else
  {$ENDIF}
  begin
    Result := pi.LeftIndent+pi.RightIndent;
    if (pi.FirstIndent>0) and not item.GetBoolValue(rvbpFullWidth) then
      inc(Result,pi.FirstIndent);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.FormatLine(
                             const Text: String;
                             StartOffs, Len, ItemNo: Integer;
                             var x, baseline, prevdesc, prevabove: Integer;
                             Canvas: TCanvas;
                             var sad: TRVScreenAndDevice;
                             var IsParaStart: Boolean;
                             var LineWidth, FirstIndent, LastDrawItem, VerticalOffs: Integer;
                             var DontFSL: Boolean; var LastTextStyle: Integer;
                             var TextMetric: TTextMetric;
                             var FirstParaItemNo: Integer;
                             NoCaching: Boolean);
var sourceStrPtr,sourceStrPtr2, strForAdd, strSpacePos: PChar;
    sourceStrPtrLen: Integer;
    sz: TSIZE;
    max,max2,j, y: Integer;
    dli: TRVDrawLineInfo;
    newline, spaceEaten, AtStart:Boolean;
    jmpinfo: TRVJumpInfo;
    y5, Offs : Integer;
    pi: TParaInfo;
    SpaceAfter, ExternalLeading, ExtraSpaceBelowLine: Integer;
    li: TCustomRVItemInfo;
    RVStyle: TRVStyle;
  {.......................................................}
  function  GetDevX(ScreenX: Integer): Integer;
  begin
    Result := MulDiv(ScreenX, sad.ppixDevice, sad.ppixScreen);
  end;
  {.......................................................}
  function  GetDevY(ScreenY: Integer): Integer;
  begin
    Result := MulDiv(ScreenY, sad.ppiyDevice, sad.ppiyScreen);
  end;
  {.......................................................}
  {$IFNDEF RVDONOTUSEUNICODE}
  function AdjustMaxFitR(value: Integer): Integer;
  var PStart, PCur, PCur2: PChar;
  begin
     Result := value;
     PStart := sourceStrPtr;
     // we cannot use CharNextW() because string does not have WideChar(0) at the end
     while True do begin
       if (Result=0) or (Result=sourceStrPtrLen) then
         exit;
       PCur := PStart + (Result+1)*2;
       PCur2 := PChar(CharPrevW(Pointer(PStart), Pointer(PCur)));
       if PCur2=PStart+(Result)*2 then
         exit;
       inc(Result);
     end;
  end;
  {.......................................................}
  function AdjustMaxFitL(value: Integer): Integer;
  var PStart, PCur, PCur2: PChar;
  begin
     Result := value;
     GetTextExtentPoint32W(Canvas.handle,Pointer(sourceStrPtr),Result,sz);
     if sz.cx<=SpaceAfter then
       exit;
     PStart := sourceStrPtr;
     PCur := PStart + (Result-1)*2;
     PCur2 := PChar(CharPrevW(Pointer(PStart), Pointer(PCur)));
     Result := (PCur2-PStart) div 2;
  end;
  {$ENDIF}
  function MaxFitSafe: Integer; // out: sz
  {$IFNDEF RVDONOTUSEUNICODE}
  var r: Integer;
  {$ENDIF}
  begin
    if (rvpaoNoWrap in pi.Options) then
      Result := sourceStrPtrLen
    else begin
      RVU_GetTextExtentExPointPC(Canvas, sourceStrPtr, sourceStrPtrLen, SpaceAfter, Result, nil, li.ItemOptions, sz);
      {$IFNDEF RVDONOTUSEUNICODE}
      if (rvioUnicode in li.ItemOptions) and RVNT then begin
        r := Result;
        Result := AdjustMaxFitR(r);
      end;
      {$ENDIF}
    end;
  end;
  {.......................................................}
  function MaxFitA: Integer; // out: sz
  var maxlo, maxhi, cxlo, cxhi : Integer;
  begin
    if RichViewSafeFormatting or (not RVNT and (rvflPrinting in Flags)) then begin
      Result := MaxFitSafe;
      exit;
    end;
    { First, find the length of the whole string }
    GetTextExtentPoint32(Canvas.Handle, sourceStrPtr, sourceStrPtrLen, sz);
    if (rvpaoNoWrap in pi.Options) or ((sz.cx <= SpaceAfter) and (sz.cx>0)) then
      { it will fit on the line }
      Result := sourceStrPtrLen
    else begin
      maxlo := 0;
      maxhi := sourceStrPtrLen;
      cxlo := 0;
      cxhi := sz.cx;
      while maxhi > (maxlo + 1) do begin
        { make the best estimate of where max should be }
        if cxhi=cxlo then
          inc(cxhi);
        Result := maxlo + (maxhi - maxlo) * (SpaceAfter - cxlo) div (cxhi - cxlo);
        if Result <= maxlo then
          Result := maxlo + 1;
        if Result >= maxhi then
          Result := maxhi - 1;
        GetTextExtentPoint32(Canvas.handle,sourceStrPtr,Result,sz);
        { if sz.cx = SpaceAfter then max is correct }
        if sz.cx = SpaceAfter then begin
          maxlo := Result;
          maxhi := Result;
          end
        else if (sz.cx < SpaceAfter) and (sz.cx>0) then begin
          maxlo := Result;
          cxlo := sz.cx;
          end
        else { sz.cx > SpaceAfter } begin
          maxhi := Result;
          cxhi := sz.cx;
        end;
      end;
      Result := maxlo;
    end
  end;
  {.......................................................}
  {$IFNDEF RVDONOTUSEUNICODE}
  function MaxFitW: Integer; // out: sz
  var maxlo, maxhi, cxlo, cxhi : Integer;
  begin
    if RichViewSafeFormatting or (not RVNT and (rvflPrinting in Flags)) then begin
      Result := MaxFitSafe;
      exit;
    end;
    { First, find the length of the whole string }
    GetTextExtentPoint32W(Canvas.Handle, Pointer(sourceStrPtr), sourceStrPtrLen, sz);
    if (rvpaoNoWrap in pi.Options) or ((sz.cx <= SpaceAfter) and (sz.cx>0)) then
      { it will fit on the line }
      Result := sourceStrPtrLen
    else begin
      maxlo := 0;
      maxhi := sourceStrPtrLen;
      cxlo := 0;
      cxhi := sz.cx;
      while maxhi > (maxlo + 1) do begin
        { make the best estimate of where max should be }
        if cxhi=cxlo then
          inc(cxhi);
        Result := maxlo + (maxhi - maxlo) * (SpaceAfter - cxlo) div (cxhi - cxlo);
        if Result <= maxlo then
          Result := maxlo + 1;
        if Result >= maxhi then
          Result := maxhi - 1;
        GetTextExtentPoint32W(Canvas.handle,Pointer(sourceStrPtr),Result,sz);
        { if sz.cx = SpaceAfter then max is correct }
        if sz.cx = SpaceAfter then begin
          maxlo := Result;
          maxhi := Result;
          end
        else if (sz.cx < SpaceAfter) and (sz.cx>0)  then begin
          maxlo := Result;
          cxlo := sz.cx;
          end
        else { sz.cx > SpaceAfter } begin
          maxhi := Result;
          cxhi := sz.cx;
        end;
      end;
      if RVNT then begin
        Result := AdjustMaxFitR(maxlo);
        if Result<>maxlo then
          Result := AdjustMaxFitL(Result);
        end
      else
        Result := maxlo;
    end
  end;
  {$ENDIF}
  {.......................................................}
  procedure OnStartNewLine(li: TCustomRVItemInfo; wrapping: Boolean);
  var LeftIndent, RightIndent: Integer;
      dli: TRVDrawLineInfo;
      SpaceAtEnd: Boolean;
      StyleNo: Integer;
  begin
     if not spaceEaten and wrapping and (LastDrawItem>0) then begin
       dli := DrawItems[LastDrawItem-1];
       StyleNo := GetItemStyle(dli.ItemNo);
       if StyleNo>=0 then begin
        {$IFNDEF RVDONOTUSEUNICODE}
        if rvioUnicode in GetItemOptions(dli.ItemNo) then
          SpaceAtEnd := (dli.Length>0) and
            (PWord(PChar(Items[dli.ItemNo])+(dli.Offs+dli.Length-2)*2)^=ord(' '))
        else
        {$ENDIF}
          SpaceAtEnd := (dli.Length>0) and
            (Items[dli.ItemNo][dli.Offs+dli.Length-1]=' ');
         if SpaceAtEnd and not IsDrawItemFromNewLine(LastDrawItem-1) then begin
           if (dli.Length=1) and (LastDrawItem-2>=0) and
             (dli.ItemNo=DrawItems[LastDrawItem-2].ItemNo) and
             not dli.FromNewLine then begin
             DrawItems.Delete(LastDrawItem-1);
             dec(LastDrawItem);
             end
           else begin
             dec(dli.Length);
             RVStyle.ApplyStyle(Canvas, StyleNo, GetItemBiDiMode(dli.ItemNo));
             dli.Width := RVU_TextWidth(DrawItems.GetString(LastDrawItem-1, Items), Canvas, GetItemOptions(dli.ItemNo));
             if (GetItemStyle(ItemNo)>=0) and (GetItemStyle(ItemNo)<>StyleNo) then
               RVStyle.ApplyStyle(Canvas, GetItemStyle(ItemNo), GetItemBiDiMode(ItemNo));
           end;
           spaceEaten := True;
         end;
       end;
     end;
     FinishScreenLine(sad, LineWidth+FirstIndent, LastDrawItem, IsParaStart, ExtraSpaceBelowLine, DontFSL, Canvas);
     pi := RVStyle.ParaStyles[TCustomRVItemInfo(Items.Objects[ItemNo]).ParaNo];
     GetIndents(ItemNo,IsParaStart,FirstParaItemNo, LeftIndent,RightIndent);
     if IsParaStart and not li.BR then begin
         if ItemNo>0 then
           inc(prevdesc,GetDevY(RVStyle.ParaStyles[GetItemPara(ItemNo-1)].SpaceAfter));
         inc(prevdesc,GetDevY(pi.SpaceBefore));
     end;
     if rvpaoNoWrap in pi.Options then
       LineWidth := DocumentWidth - GetDevX(LeftIndent+RightIndent)
     else
       LineWidth := TextWidth - GetDevX(LeftIndent+RightIndent);
     FirstIndent :=  sad.LeftMargin+GetDevX(LeftIndent);
  end;
  {.......................................................}
  procedure AdjustLineHeight(h: Integer);
  begin
    if prevabove < h then begin
      j := LastDrawItem-1;
      if j>=0 then
        repeat
          inc(DrawItems[j].Top,h-prevabove);
          dec(j);
        until DrawItems[j+1].FromNewLine;
      inc(baseline,h-prevabove);
      prevabove := h;
    end;
  end;
  {.......................................................}
  procedure OnNonText(li: TRVRectItemInfo; dli: TRVDrawLineInfo);
  var above, VerticalOffs: Integer;
      w,h,desc,hshift: Integer;
  begin
    hshift := 0;
    li.OnDocWidthChange(TextWidth - GetDevX(pi.LeftIndent+pi.RightIndent+pi.FirstIndent),
                        dli, rvflPrinting in Flags, Canvas, Self, @sad, hshift, desc, NoCaching);
    if li.GetBoolValue(rvbpDrawingChangesFont) then
      LastTextStyle := -1;
    if li.GetBoolValueEx(rvbpActualPrintSize, RVStyle) then begin
      w := dli.Width;
      h := dli.Height;
      end
    else begin
      w := GetDevX(li.Width);
      h := GetDevY(li.Height);
      desc := GetDevY(desc);
    end;
    if li.VShiftAbs then
      VerticalOffs := GetDevY(li.VShift)
    else
      VerticalOffs := MulDiv(h,li.VShift,100);
    dli.SetSize(w, h);
    if not li.SameAsPrev or
      ((dli is TRVMultiDrawItemInfo)
      )
       or
      (
        (x+w > LineWidth) and
        not (rvpaoNoWrap in pi.Options)
        {$IFNDEF RVDONOTUSELISTS}
        and not ((ItemNo>0) and (GetItemStyle(ItemNo-1)=rvsListMarker))
        {$ENDIF}
      ) then begin
      if li.SameAsPrev then
        IsParaStart := False;
      OnStartNewLine(li, li.SameAsPrev);
      dli.FromNewLine := True;
      x :=0;
      inc(baseline, ExtraSpaceBelowLine);
      y := baseline + prevdesc;
      case li.VAlign of
        rvvaBaseline:
          prevabove := h-desc;
        else {rvvaMiddle:}
          prevabove := h div 2;
      end;
      inc(prevabove, VerticalOffs);
      inc (baseline,prevdesc+prevabove);
      prevdesc :=h-prevabove;
      end
    else begin
      pi := RVStyle.ParaStyles[li.ParaNo];
      case li.VAlign of
        rvvaBaseline:
          above := h-desc;
        else {rvvaMiddle:}
          above := h div 2;
      end;
      AdjustLineHeight(above+VerticalOffs);
      y := baseline - above-VerticalOffs;
      if prevdesc < (h-above-VerticalOffs) then
        prevdesc := (h-above-VerticalOffs);
      dli.FromNewLine := False;
    end;
    dli.Left := x+FirstIndent+hshift;
    inc(x, w+hshift);
    dli.Top :=  y;
    dli.ItemNo := ItemNo;
    dli.SetSize(w, h);
    dli.Offs := 0;
    dli.Length := 0;
    if not ShareItems then
      li.DrawItemNo := LastDrawItem;
    DrawItems.Insert(LastDrawItem, dli);

    if rvflUseJumps in Flags then
      li.BuildJumps(dli.Left, dli.Top, nJmps, jumps);

    //AdjustCP(li);
    inc(LastDrawItem);
  end;
  {.......................................................}
var HShift, Desc, ActualStyleNo: Integer;
begin
  HShift := 0;
  Desc   := 0;
  RVStyle := GetRVStyle;
  li := TCustomRVItemInfo(Items.Objects[ItemNo]);
  pi := RVStyle.ParaStyles[li.ParaNo];
  if (not li.SameAsPrev) and (StartOffs=0) then
    IsParaStart := True;
  {
  if LineWidth<0 then
    OnStartNewLine(not li.BR, li);
  }
  if li.StyleNo<0 then begin
    if li is TRVRectItemInfo then begin
      // rvsComponent, rvsHotspot, rvsBullet, rvsPicture:
      if rvflAllowCustomDrawItems in Flags then
        dli := li.CreatePrintingDrawItem(Self,sad)
      else
        dli := TRVDrawLineInfo.Create;
      OnNonText(TRVRectItemInfo(li), dli);
      if li.GetBoolValue(rvbpDrawingChangesFont) then
        LastTextStyle := -1;
      if li.GetBoolValueEx(rvbpJump, RVStyle) then begin
        if rvflUseJumps in Flags then begin
          jmpinfo     := TRVJumpInfo.Create;
          jmpinfo.w   := dli.Width;
          jmpinfo.h   := dli.Height;
          jmpinfo.id  := nJmps;
          jmpinfo.DrawItemNo := LastDrawItem-1;
          jmpinfo.Cursor := li.GetHypertextCursor(RVStyle);
          jmpinfo.RVData := Self;
          jumps.Add(jmpinfo);
        end;
        TCustomRVItemInfo(Items.Objects[ItemNo]).JumpID := nJmps;
        inc(nJmps);
      end;
      end
    else begin
      FinishScreenLine(sad, LineWidth+FirstIndent,LastDrawItem, True, ExtraSpaceBelowLine, DontFSL, Canvas);
      if ItemNo>0 then
        inc(prevdesc,GetDevY(RVStyle.ParaStyles[TCustomRVItemInfo(Items.Objects[ItemNo-1]).ParaNo].SpaceAfter));
      inc(prevdesc,GetDevY(RVStyle.ParaStyles[TCustomRVItemInfo(Items.Objects[ItemNo]).ParaNo].SpaceBefore));
      if li.StyleNo=rvsBreak then begin
        inc(prevdesc,GetDevY(RVStyle.ParaStyles[0].SpaceBefore));
        y5          := MulDiv(5, sad.ppiyDevice, sad.ppiyScreen);
        dli        := TRVDrawLineInfo.CreateEx(sad.LeftMargin,baseline + prevdesc,
                                               TextWidth,y5+y5+1,ItemNo,True);
        end
      else begin
        LineWidth := TextWidth - GetDevX(pi.LeftIndent+pi.RightIndent);
        y5 := sad.LeftMargin+GetDevX(pi.LeftIndent); //  y5 <- left
        if rvflAllowCustomDrawItems in Flags then
          dli := li.CreatePrintingDrawItem(Self,sad)
        else
          dli := TRVDrawLineInfo.Create;
        li.OnDocWidthChange(LineWidth, dli, rvflPrinting in Flags, Canvas, Self, @sad, HShift, Desc, NoCaching);
        if li.GetBoolValue(rvbpDrawingChangesFont) then
          LastTextStyle := -1;
        dli.SetSize(GetDevX(TRVFullLineItemInfo(li).Width),
                    GetDevY(TRVFullLineItemInfo(li).Height));
        case pi.Alignment of
          rvaRight:
            inc(y5, LineWidth-dli.Width);
          rvaCenter:
            begin
              inc(y5, (LineWidth-dli.Width) div 2);
              if y5<0 then y5 := 0;
            end;
        end;
        dli.SetData(y5+HShift, baseline + prevdesc, ItemNo,True);
      end;
      if not ShareItems then
        li.DrawItemNo := LastDrawItem;
      DrawItems.Insert(LastDrawItem, dli);

      if rvflUseJumps in Flags then
        li.BuildJumps(dli.Left, dli.Top, nJmps, jumps);

      //AdjustCP(li);
      inc(LastDrawItem);
      if li.StyleNo=rvsBreak then begin
        inc (baseline,prevdesc+y5+1);
        prevdesc  := y5;
        prevabove := y5;
        end
      else begin
        inc (baseline,prevdesc+dli.Height);
        prevabove := dli.Height;
        prevdesc  := 0;
      end;
      end
      end
  else begin // text
     spaceEaten := False;
     {$IFNDEF RVDONOTUSEUNICODE}
     if rvioUnicode in li.ItemOptions then
       sourceStrPtr := PChar(Text)+StartOffs*2
     else
     {$ENDIF}
       sourceStrPtr := PChar(Text)+StartOffs;
     sourceStrPtrLen := Len;
     ActualStyleNo := GetActualStyle(li);
     if ActualStyleNo<>LastTextStyle then begin
       LastTextStyle := ActualStyleNo;
       RVStyle.ApplyStyle(Canvas, ActualStyleNo,
         GetParaBiDiMode(GetItemPara(ItemNo)));
       if (RVStyle.TextStyles[ActualStyleNo].CharSpacing<>0) and
          (sad.ppixScreen<>sad.ppixDevice) and
          (GetItemBiDiMode(ItemNo)=rvbdUnspecified) then
         SetTextCharacterExtra(Canvas.Handle,
           GetDevX(RVStyle.TextStyles[ActualStyleNo].CharSpacing));
       GetTextMetrics(Canvas.Handle, TextMetric);
       VerticalOffs := MulDiv(TextMetric.tmHeight,
         RVStyle.TextStyles[ActualStyleNo].VShift, 100);
     end;

     if rvflUseExternalLeading in Flags then
       ExternalLeading := TextMetric.tmExternalLeading
     else
       ExternalLeading := 0;
     AtStart := (StartOffs=0);
     if sourceStrPtrLen=0 then begin {empty string}
       sz.cy := Canvas.TextHeight(' ');
       sz.cx := 0;
       if not li.SameAsPrev then begin
         OnStartNewLine(li, False);
         x :=0;
         inc(baseline, ExtraSpaceBelowLine);
         y := baseline+prevDesc;
         prevabove := ExternalLeading+TextMetric.tmAscent+VerticalOffs;
         inc(baseline, prevDesc+prevabove);
         prevDesc := TextMetric.tmDescent-VerticalOffs;
         end
       else begin
         AdjustLineHeight(ExternalLeading+TextMetric.tmAscent+VerticalOffs);
         y := baseline - (ExternalLeading+TextMetric.tmAscent+VerticalOffs);
         if prevDesc < TextMetric.tmDescent-VerticalOffs then
           prevDesc := TextMetric.tmDescent-VerticalOffs;
       end;
       dli := TRVDrawLineInfo.CreateEx(x+FirstIndent,y, sz.cx, sz.cy, ItemNo, not li.SameAsPrev);
       dli.Offs := 1+StartOffs;
       inc(x, sz.cx);
       if RVStyle.TextStyles[ActualStyleNo].Jump then
         TCustomRVItemInfo(Items.Objects[ItemNo]).JumpID := -3;
       dli.Length := 0;
       if AtStart and not ShareItems then
         li.DrawItemNo := LastDrawItem;
       DrawItems.Insert(LastDrawItem, dli);
       //AdjustCP(li);
       inc(LastDrawItem);
       exit;
     end;
     newline := AtStart and not li.SameAsPrev;
     while sourceStrPtrLen>0 do begin
       if newline then begin
         OnStartNewLine(li, not IsParaStart);
         x:=0;
         if (rvflTrim in Flags) and not SpaceEaten and
            not IsParaStart and (sourceStrPtrLen>1) then
           {$IFNDEF RVDONOTUSEUNICODE}
           if rvioUnicode in li.ItemOptions then begin
             if PWord(sourceStrPtr)^=ord(' ') then begin
               inc(sourceStrPtr,2);
               dec(sourceStrPtrLen);
             end;
             end
           else
           {$ENDIF}
           if sourceStrPtr^=' ' then begin
             inc(sourceStrPtr);
             dec(sourceStrPtrLen);
           end;
         SpaceEaten := False;
       end;
       SpaceAfter := LineWidth-x;
       if SpaceAfter<0 then SpaceAfter := 0;
       {$IFNDEF RVDONOTUSEUNICODE}
       if rvioUnicode in li.ItemOptions then
         max := MaxFitW
       else
       {$ENDIF}
         max := MaxFitA;
       max2 := max;
       sourceStrPtr2 := sourceStrPtr;
       if max=0 then
         if newline
         {$IFNDEF RVDONOTUSELISTS}
         or (AtStart and (ItemNo>0) and (GetItemStyle(ItemNo-1)=rvsListMarker))
         {$ENDIF}
          then begin
             max := 1;
             {$IFNDEF RVDONOTUSEUNICODE}
             if RVNT and (rvioUnicode in li.ItemOptions) then
               max := AdjustMaxFitR(max);
             {$ENDIF}
           end
         else begin
           x:=0;
           IsParaStart := False;
           newline := true;
           continue;
         end;
       if max<sourceStrPtrLen then begin
           {$IFNDEF RVDONOTUSEUNICODE}
           if rvioUnicode in li.ItemOptions then begin
             strSpacePos := RVU_FindLineBreak(PRVWordArray(sourceStrPtr), max); // MyStrRScanW(sourceStrPtr,' ',max)
             if strSpacePos=sourceStrPtr+max*2 then
               strSpacePos := nil;
             end
           else
           {$ENDIF}
             strSpacePos := MyStrRScanA(sourceStrPtr,' ',max);
           if (strSpacePos=nil) and (rvflTrim in Flags) and
              ((max+1<sourceStrPtrLen) or ((ItemNo+1<Items.Count) and not IsFromNewLine(ItemNo+1)))
             then begin
             {$IFNDEF RVDONOTUSEUNICODE}
             if rvioUnicode in li.ItemOptions then begin
               if PRVWordArray(sourceStrPtr)[max]=UNI_Space then begin
                 strSpacePos := sourceStrPtr+max*2;
                 inc(max);
               end;
               end
             else
             {$ENDIF}
             begin
               if sourceStrPtr[max]=' ' then begin
                 strSpacePos := sourceStrPtr+max;
                 inc(max);
               end;
             end;
           end;
           if strSpacePos<>nil then begin
             {$IFNDEF RVDONOTUSEUNICODE}
             if rvioUnicode in li.ItemOptions then begin
               if (rvflTrim in Flags) and (PRVWordArray(strSpacePos)[0]=UNI_Space) and
                 (strSpacePos<>sourceStrPtr) then begin
                 max := (strSpacePos-sourceStrPtr) div 2;
                 SpaceEaten := True;
                 end
               else
                 max := (strSpacePos-sourceStrPtr) div 2+1;
               end
             else
             {$ENDIF}
             begin
               if (rvflTrim in Flags) and (strSpacePos<>sourceStrPtr) then begin
                 max := strSpacePos-sourceStrPtr;
                 SpaceEaten := True;
                 end
               else
                 max := strSpacePos-sourceStrPtr+1;
             end;
             end
           else if not (newline
           {$IFNDEF RVDONOTUSELISTS}
           or (AtStart and (ItemNo>0) and (GetItemStyle(ItemNo-1)=rvsListMarker))
           {$ENDIF}
           )
           then begin
             IsParaStart := False;
             newline := true;
             continue;
           end;
       end;
       {$IFNDEF RVDONOTUSEUNICODE}
       if rvioUnicode in li.ItemOptions then begin
         Offs := (sourceStrPtr - PChar(Text)) div 2+1;
         StrForAdd := sourceStrPtr;
         inc(sourceStrPtr,max*2);
         dec(sourceStrPtrLen,max);
         if SpaceEaten then begin
           inc(sourceStrPtr, 2);
           dec(sourceStrPtrLen);
           SpaceEaten := False;
         end;
         end
       else
       {$ENDIF}
       begin
         Offs := sourceStrPtr - PChar(Text)+1;
         StrForAdd := sourceStrPtr;
         inc(sourceStrPtr,max);
         dec(sourceStrPtrLen,max);
         if SpaceEaten then begin
           inc(sourceStrPtr);
           dec(sourceStrPtrLen);
           SpaceEaten := False;
         end;
       end;
       if (newline or (max<>0)) then begin
         dli := TRVDrawLineInfo.Create;
         dli.ItemNo := ItemNo;
         dli.Offs := Offs;
         if {(sourceStrPtrLen<>0) and} ((max<>max2) or (sourceStrPtr2<>sourceStrPtr) or (rvpaoNoWrap in pi.Options)) then
          {$IFNDEF RVDONOTUSEUNICODE}
           if rvioUnicode in li.ItemOptions then
             GetTextExtentPoint32W(Canvas.Handle,Pointer(strForAdd),max,sz)
           else
           {$ENDIF}
             GetTextExtentPoint32(Canvas.Handle,strForAdd,max,sz);
         if not newline then begin {continue line}
           AdjustLineHeight(ExternalLeading+TextMetric.tmAscent+VerticalOffs);
           y := baseline - (ExternalLeading+TextMetric.tmAscent+VerticalOffs);
           dli.FromNewLine := False;
           end
         else  begin { new line }
           dli.FromNewLine := True;
           x :=0;
           inc(baseline, ExtraSpaceBelowLine);
           y := baseline+prevDesc;
           prevabove := ExternalLeading+TextMetric.tmAscent+VerticalOffs;
           inc(baseline, prevDesc+prevabove);
         end;
         dli.Left   :=x+FirstIndent;
         dli.Top    := y;
         dli.Width  := sz.cx;
         dli.Height := sz.cy;
         dli.Length := max;
         DrawItems.Insert(LastDrawItem,dli);
         if AtStart and not ShareItems then
           li.DrawItemNo := LastDrawItem;
         inc(LastDrawItem);
         if (rvflUseJumps in Flags) and RVStyle.TextStyles[ActualStyleNo].Jump then begin
           jmpinfo := TRVJumpInfo.Create;
           jmpinfo.w := sz.cx;
           jmpinfo.h := sz.cy;
           jmpinfo.id := nJmps;
           if StartOffs>0 then dec(jmpinfo.id);
           jmpinfo.DrawItemNo := LastDrawItem-1;
           jmpinfo.Cursor := RVStyle.TextStyles[ActualStyleNo].JumpCursor;
           jmpinfo.RVData := Self;
           jumps.Add(jmpinfo);
         end;
         if newline or (prevDesc < TextMetric.tmDescent-VerticalOffs) then
           prevDesc := TextMetric.tmDescent-VerticalOffs;
         inc(x,sz.cx);
         newline := True;
         IsParaStart := False;
         AtStart := False;
         end
       else
         newline := true;
     end; { while }
     if RVStyle.TextStyles[ActualStyleNo].Jump and (StartOffs=0) then begin
       TCustomRVItemInfo(Items.Objects[ItemNo]).JumpID := nJmps;
       inc(nJmps);
     end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.FormatWords(ItemNo: Integer;
                             var x, baseline, prevdesc, prevabove: Integer;
                             Canvas: TCanvas;
                             var sad: TRVScreenAndDevice;
                             var IsParaStart: Boolean;
                             var LineWidth, FirstIndent, LastDrawItem,
                             VerticalOffs: Integer;
                             var DontFSL: Boolean;
                             var LastTextStyle: Integer;
                             var TextMetric: TTextMetric;
                             var FirstParaItemNo: Integer;
                             NoCaching: Boolean);
var offs: Integer;
    LinesNo, TmpStr, SpacePos: PChar;
    Len, WordLen: Integer;
    {$IFNDEF RVDONOTUSEUNICODE}
    Unicode: Boolean;
    {$ENDIF}
    Text: String;
begin
  Len := Length(Items[ItemNo]);
  if Len=0 then begin
    FormatLine('', 0, Len, ItemNo, x, baseline, prevdesc, prevabove,
                  Canvas, sad, IsParaStart,
                  LineWidth, FirstIndent, LastDrawItem, VerticalOffs,
                  DontFSL,LastTextStyle, TextMetric, FirstParaItemNo, NoCaching);
    exit;
  end;
  {$IFNDEF RVDONOTUSEUNICODE}
  Unicode := rvioUnicode in GetItemOptions(ItemNo);
  if Unicode then
    Len := Len div 2;
  {$ENDIF}
  offs := 0;
  {$IFNDEF RVDONOTUSEALLCAPS}
    Text := RV_ReturnCapitalized(Items[ItemNo],
      GetRVStyle.TextStyles[GetItemStyle(ItemNo)]);
  {$ELSE}
    Text := Items[ItemNo];
  {$ENDIF}
  LinesNo := PChar(Text);
  while Len>0 do begin
    TmpStr := LinesNo;
    {$IFNDEF RVDONOTUSEUNICODE}
    if Unicode then begin
      if (PWord(TmpStr)^ = ord(' ')) then
        inc(PChar(TmpStr), 2);
      SpacePos := RVU_StrScanW(TmpStr, ord(' '), Len)
      end
    else
    {$ENDIF}
    begin
      if (TmpStr^ = ' ') then
        inc(TmpStr);
      SpacePos := StrScan(TmpStr, ' ');
    end;
    if SpacePos=nil then
      WordLen := Len
    else begin
      WordLen := SpacePos-LinesNo;
      {$IFNDEF RVDONOTUSEUNICODE}
      if Unicode then
        WordLen := WordLen div 2;
      {$ENDIF}
    end;
    FormatLine(Text, Offs, WordLen, ItemNo, x, baseline, prevdesc, prevabove,
                  Canvas, sad, IsParaStart,
                  LineWidth, FirstIndent, LastDrawItem, VerticalOffs,
                  DontFSL, LastTextStyle, TextMetric, FirstParaItemNo, NoCaching);
    inc(offs, WordLen);
    inc(LinesNo, WordLen);
    {$IFNDEF RVDONOTUSEUNICODE}
    if Unicode then
      inc(LinesNo, WordLen);
    {$ENDIF}
    dec(Len, WordLen);
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetHOffs: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetVOffs: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.Format(NoCaching: Boolean);
begin
  Format_(False, True, False, 0, GetCanvas, False, NoCaching);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.ApplyZoom(Canvas: TCanvas);
begin
  {
  if ZoomPercent<>100 then begin
    SetMapMode(Canvas.Handle,MM_ANISOTROPIC);
    SetWindowExtEx(Canvas.Handle,100,100,nil);
    SetViewportExtEx(Canvas.Handle,ZoomPercent,ZoomPercent,nil);
  end;
  }
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.RestoreZoom(Canvas: TCanvas);
begin
  {
  if ZoomPercent<>100 then
    SetMapMode(Canvas.Handle,MM_TEXT);
  }
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.ZoomInt(var v: Integer);
begin
  v := v;
  //v := MulDiv(v,100,ZoomPercent);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.ZoomRectDown(var r: TRect);
begin
  {
  with r do begin
    Left := MulDiv(Left,100,ZoomPercent);
    Top := MulDiv(Top,100,ZoomPercent);
    Right := MulDiv(Right,100,ZoomPercent);
    Bottom := MulDiv(Bottom,100,ZoomPercent);
  end;
  }
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.GetSADForFormatting(Canvas: TCanvas;
  var sad: TRVScreenAndDevice);
begin
//  ApplyZoom(Canvas);
  RV_InfoAboutSaD(sad, Canvas);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.Format_(OnlyResized, ForceFormat,
  NoScroll: Boolean; depth: Integer; Canvas: TCanvas;
  OnlyTail, NoCaching: Boolean);
var i: Integer;
    x,b,d,a: Integer;
    oldtextwidth, olddocumentwidth, cw, ch: Integer;
    sad: TRVScreenAndDevice;
    StartLine: Integer;
    StartNo, EndNo, StartOffs, EndOffs: Integer;
    DevMinTextWidth, DevMaxTextWidth: Integer;
    FV, DeltaFV, LineWidth, Indent, LastDrawItem: Integer;
    IsParaStart: Boolean;
    DontFSL: Boolean;
    LastTextStyle, VerticalOffs: Integer;
    TextMetric: TTextMetric;
    RVStyle: TRVStyle;
    ExtraSpaceBelowLine: Integer;
    FirstParaItemNo: Integer;
begin
   if AlreadyFormatted then begin
     AlreadyFormatted := False;
     exit;
   end;
   if (depth>1) or (GetRVStyle=nil) or (rvstSkipformatting in State) then exit;
   if Canvas=nil then
     Canvas := GetCanvas;
   if depth=0 then begin
     StartFormatting;
   end;
   try
     FirstParaItemNo := -1;
     LastTextStyle := -1;
     IsParaStart := True;
     FV := -1;     { avoiding warings }
     DeltaFV := 0; { avoiding warings }
     if depth=0 then begin
        if OnlyResized then begin
          StoreSelBounds(StartNo, EndNo, StartOffs, EndOffs, False);
          if not NoScroll then begin
            FV := GetDrawItemNo(GetVOffs, gdinFirstVisible);
            if FV>=DrawItems.Count then FV:=DrawItems.Count-1;
            if FV<>-1 then  begin
              DeltaFV := GetVOffs-DrawItems[FV].Top;
              FV := DrawItems[FV].ItemNo;
            end;
          end;
        end;
        if OnlyTail then FV := GetVOffs;
     end;

     oldtextwidth := TextWidth;
     olddocumentwidth := DocumentWidth;

     GetSADForFormatting(Canvas, sad);

     sad.LeftMargin := MulDiv(GetLeftMargin,  sad.ppixDevice, sad.ppixScreen);
     sad.RightMargin := MulDiv(GetRightMargin,  sad.ppixDevice, sad.ppixScreen);

     DocumentWidth := max(
               GetWidth-(sad.LeftMargin+sad.RightMargin),
               CalculateMinDocWidthPlus(@sad, Canvas)
              );
     DevMinTextWidth := MulDiv(GetMinTextWidth,  sad.ppixDevice, sad.ppixScreen);
     DevMaxTextWidth := MulDiv(GetMaxTextWidth,  sad.ppixDevice, sad.ppixScreen);
     if DocumentWidth<DevMinTextWidth then DocumentWidth := DevMinTextWidth;
     if rvoClientTextWidth in Options then begin { widths of pictures and maxtextwidth are ignored }
       TextWidth := GetWidth-(sad.LeftMargin+sad.RightMargin);
       if TextWidth<DevMinTextWidth then TextWidth := DevMinTextWidth;
       end
     else begin
        if (DocumentWidth > DevMaxTextWidth{-(sad.LeftMargin+sad.RightMargin)}) and (GetMaxTextWidth>0) then
          TextWidth := DevMaxTextWidth//-(sad.LeftMargin+sad.RightMargin)
        else
          TextWidth := DocumentWidth;
     end;
     if not (OnlyResized and (TextWidth=OldTextWidth) and (DocumentWidth=OldDocumentWidth)) or ForceFormat then begin
       if OnlyTail then begin
          StartLine := LastItemFormatted+1;
          b:= DocumentHeight-GetBottomMargin;
          if LastItemFormatted>=0 then
            dec(b, MulDiv(GetRVStyle.ParaStyles[TCustomRVItemInfo(Items.Objects[LastItemFormatted]).ParaNo].SpaceAfter,sad.ppiyDevice, sad.ppiyScreen));
          end
       else begin
          StartLine := 0;
          b := GetTopMargin;
          ClearTemporal;
       end;
       x:=0;
       d:=0;
       LastDrawItem := DrawItems.Count;
       DontFSL := True;
       LineWidth := -1;
       Indent    := 0;
       RVStyle   := GetRVStyle;
       for i:=StartLine to Items.Count-1 do
         with TCustomRVItemInfo(Items.Objects[i]) do begin
           if not SameAsPrev and not BR then
             FirstParaItemNo := i;
           if StyleNo>=0 then
             if RVStyle.ParaStyles[ParaNo].Alignment = rvaJustify then
               FormatWords(i,x,b, d,a, Canvas, sad, IsParaStart, LineWidth, Indent,
                 LastDrawItem, VerticalOffs, DontFSL, LastTextStyle, TextMetric,
                 FirstParaItemNo, NoCaching)
             else
               FormatLine(
                 {$IFNDEF RVDONOTUSEALLCAPS}
                 RV_ReturnCapitalized(Items[i], RVStyle.TextStyles[GetActualStyleNo(RVStyle)]),
                 {$ELSE}
                 Items[i],
                 {$ENDIF}
                 0,
                 {$IFNDEF RVDONOTUSEUNICODE}
                 RVU_Length(Items[i], ItemOptions),
                 {$ELSE}
                 Length(Items[i]),
                 {$ENDIF}
                 i,x,b, d,a, Canvas, sad, IsParaStart,
                 LineWidth, Indent, LastDrawItem, VerticalOffs,
                 DontFSL, LastTextStyle, TextMetric,
                 FirstParaItemNo, NoCaching)
           else
               FormatLine('',0, Length(Items[i]), i,x,b, d,a, Canvas, sad, IsParaStart, LineWidth, Indent,
                       LastDrawItem, VerticalOffs, DontFSL, LastTextStyle, TextMetric, FirstParaItemNo, NoCaching)
         end;
       FinishScreenLine(sad, LineWidth+Indent, LastDrawItem,
         not (rvstLastParaAborted in State), ExtraSpaceBelowLine, DontFSL, Canvas);
       DocumentHeight := b+d+GetBottomMargin+ExtraSpaceBelowLine;
       if Items.Count>0 then
         inc(DocumentHeight,
           MulDiv(GetRVStyle.ParaStyles[GetItemPara(Items.Count-1)].SpaceAfter,
             sad.ppiyDevice, sad.ppiyScreen));
       AdjustVScrollUnits;
       AdjustJumpsCoords;
       end
     else;
      // AdjustChildrenCoords;
     cw := GetWidth;
     ch := GetHeight;
     AlreadyFormatted := True;
     SetDocumentAreaSize(DocumentWidth+GetLeftMargin+GetRightMargin, DocumentHeight, True);
     AlreadyFormatted := False;
     if (cw<>GetWidth) or (ch<>GetHeight) then begin
       //EndFormatting;
       //ScrollTo(OldY);
       EndFormatting;
       Format_(OnlyResized, ForceFormat, False, depth+1, Canvas, False, NoCaching);
       StartFormatting;
     end;
     if (depth=0) then begin
       if OnlyResized and not NoScroll then begin
         FV := FindDrawItemByItem(FV);
         if FV<>-1 then
           ScrollTo(DrawItems[FV].Top+DeltaFV,False)
         else
           ScrollTo(0,False);
       end;
       AdjustChildrenCoords;
       if OnlyTail then begin
         if rvoScrollToEnd in Options then
           ScrollTo(DocumentHeight,False)
         else
           ScrollTo(FV,False);
  //       AdjustChildrenCoords;
       end;
       if OnlyResized then
         RestoreSelBounds(StartNo, EndNo, StartOffs, EndOffs);
     end;
   finally
     if (depth=0) then begin
       EndFormatting;
       LastItemFormatted := Items.Count-1;
       Formatted(0,Items.Count-1, False); // <- wrong for FormatTail
     end;
   end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.GetSelBounds(var StartNo, EndNo,
                                   StartOffs, EndOffs: Integer;
                                   Normalize: Boolean);
begin
  if not Normalize or (FSelStartNo <= FSelEndNo) then begin
    StartNo := FSelStartNo;
    EndNo   := FSelEndNo;
    if not Normalize or not ((StartNo=EndNo) and (FSelStartOffs>FSelEndOffs)) then begin
      StartOffs := FSelStartOffs;
      EndOffs   := FSelEndOffs;
      end
    else begin
      StartOffs := FSelEndOffs;
      EndOffs   := FSelStartOffs;
    end;
    end
  else begin
    StartNo := FSelEndNo;
    EndNo   := FSelStartNo;
    StartOffs := FSelEndOffs;
    EndOffs   := FSelStartOffs;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.GetScreenLineBounds(DrawItemNo: Integer;
                                                     var First, Last: Integer);
begin
  First := DrawItemNo;
  while (First>0) and not DrawItems[First].FromNewLine do
    dec(First);
  Last := DrawItemNo+1;
  while (Last<DrawItems.Count) and not DrawItems[Last].FromNewLine do
    inc(Last);
  dec(Last);
end;
{------------------------------------------------------------------------------}
var SortDrawItems: TRVDrawLines;

function CompareCoords(Item1, Item2: Pointer): Integer;
begin
  Result := SortDrawItems[Integer(Item1)].Left-SortDrawItems[Integer(Item2)].Left;
end;


procedure TCustomRVFormattedData.FindDrawItemForSel(X,Y: Integer;
                                  var No, Offs: Integer;
                                  Strict: Boolean);
var
    styleno,i, a,b,mid, midtop, midbottom, midleft, midright, beginline, endline, min, minsign: Integer;
    firstinline,lastinline, delta: Integer;
    dli: TRVDrawLineInfo;
    arr: PRVIntegerArray;
    Canvas: TCanvas;
    s: String;
    BiDiMode:TRVBiDiMode;
    order, revorder: TRVIntegerList;
begin
  Canvas := GetCanvas;
  No := -1;
  Offs := -1;
  if DrawItems.Count = 0 then
    exit;
  dli := DrawItems[0];
  if (dli.Top+dli.Height>Y) then
    mid := 0
  else begin
    a := 1;
    b := DrawItems.Count-1;
    while (b-a)>1 do begin
      mid := (a+b) div 2;
      if (DrawItems[mid].Top<=Y) then
        a := mid
      else
        b := mid;
    end;
    mid := a;
    if DrawItems[b].Top<=Y then mid := b;
  end;
  GetScreenLineBounds(mid, beginline, endline);
  if endline<>DrawItems.Count-1 then begin
    // may be the next line is better?
    b := endline+2;
    while b<DrawItems.Count do begin
      if DrawItems[b].FromNewLine then break;
      inc(b);
    end;
    dec(b);
    a := mid;
    for i := endline+1 to b do
      if DrawItems[i].Top<=Y then begin
        a := i;
        break;
      end;
    if a<>mid then begin
      beginline := endline+1;
      endline   := b;
    end;
  end;

  // calculating line bounds (in pixels)
  midtop    := DrawItems[mid].Top;
  midbottom := midtop + DrawItems[mid].Height;
  midleft   := DrawItems[mid].Left;
  midright  := midleft+DrawItems[mid].Width;

  for i:= beginline to endline do begin
    dli := DrawItems[i];
    if dli.Top < midtop then midtop := dli.Top;
    if dli.Top + dli.Height > midbottom then midbottom := dli.Top + dli.Height;
    if dli.Left < midleft then midleft := dli.Left;
    if dli.Left + dli.Width > midright then midright := dli.Left + dli.Width;
  end;
  BiDiMode := GetParaBiDiMode(GetItemPara(DrawItems[beginline].ItemNo));
  if BiDiMode<>rvbdUnspecified then begin
    order := TRVIntegerList.Create;
    for i:= beginline to endline do
      order.Add(i);
    SortDrawItems := DrawItems;
    TList(order).Sort(CompareCoords);
    SortDrawItems := nil;
    revorder := TRVIntegerList.CreateEx(endline-beginline+1,0);
    for i:= beginline to endline do
      revorder[order[i-beginline]-beginline] := i;
    end
  else begin
    order := nil;
    revorder := nil;
  end;
  if BiDiMode=rvbdRightToLeft then begin
    firstinline := endline;
    lastinline  := beginline;
    delta       := -1;
    end
  else begin
    firstinline := beginline;
    lastinline  := endline;
    delta       := +1;
  end;
  for i:= beginline to endline do begin
    dli := DrawItems[i];
    if (
         (Strict and (dli.Left<=X) and (dli.Left+dli.Width>X) and (Y>=dli.Top) and (Y<=dli.Top+dli.Height))
       or
         (not Strict and (order=nil) and
          (
           ((dli.Left<=X) and ((i=lastinline) or (DrawItems[i+delta].Left>X)))
           or
           ((i=firstinline) and (X<dli.Left))
           )
         )
       or
         (not Strict and (order<>nil) and
          (
           ((dli.Left<=X) and ((revorder[i-beginline]=endline) or (DrawItems[order[revorder[i-beginline]-beginline+1]].Left>X)))
           or
           ((revorder[i-beginline]=beginline) and (X<dli.Left))
           )
         )
       )
       then begin
      styleno := GetItemStyle(dli.ItemNo);
      No := i;
      Offs := 0;
      if styleno>=0 then begin
        GetRVStyle.ApplyStyle(Canvas, StyleNo, GetParaBiDiMode(GetItemPara(dli.ItemNo)));
        s := DrawItems.GetString(i,Items);
        {$IFNDEF RVDONOTUSEALLCAPS}
        s := RV_ReturnCapitalized(s, GetRVStyle.TextStyles[StyleNo]);
        {$ENDIF}
        if Length(s)>0 then begin
          GetMem(arr, (dli.Length+2)*sizeof(Integer));
          try
            if (GetItemBiDiMode(dli.ItemNo)<>rvbdUnspecified) and
               RVU_GetTextCaretPos(Canvas, s, arr, GetItemOptions(dli.ItemNo),
                 dli.Width{$IFNDEF RVDONOTUSEJUSTIFY}-dli.SpaceBefore{$ENDIF}) then begin
              min := Abs(X-arr[0]-dli.Left{$IFNDEF RVDONOTUSEJUSTIFY}-dli.SpaceBefore{$ENDIF});
              offs := 0;
              for a := 0 to dli.Length do
                if Abs(X-arr[a]-dli.Left{$IFNDEF RVDONOTUSEJUSTIFY}-dli.SpaceBefore{$ENDIF})<min then begin
                  Offs := a;
                  min := Abs(X-arr[a]-dli.Left{$IFNDEF RVDONOTUSEJUSTIFY}-dli.SpaceBefore{$ENDIF});
                end;
              if (Offs<Length(s)) and (Offs>0)and (NextCharStr(s, dli.ItemNo, Offs)>Offs+1) then
                Offs := NextCharStr(s, dli.ItemNo, Offs)-1;
              end
            else begin
              RVU_GetTextExtentExPoint(Canvas, s,
                                   dli.Width*10, b, arr,
                                   TCustomRVItemInfo(Items.Objects[dli.ItemNo]).ItemOptions);
              minsign := X-dli.Left;
              min := Abs(minsign);
              offs := 0;
              for a := 1 to b do
              if Abs(X-arr[a-1]-dli.Left{$IFNDEF RVDONOTUSEJUSTIFY}-dli.SpaceBefore{$ENDIF})<min then begin
                Offs := a;
                minsign := X-arr[a-1]-dli.Left{$IFNDEF RVDONOTUSEJUSTIFY}-dli.SpaceBefore{$ENDIF};
                min := Abs(minsign);
              end;
              if (Offs<Length(s)) and (Offs>0)and (NextCharStr(s, dli.ItemNo, Offs)>Offs+1) then
                Offs := NextCharStr(s, dli.ItemNo, Offs)-1;
            end;
            inc(Offs);
            if Offs>DrawItems[i].Length+1 then
              Offs := DrawItems[i].Length+1;
            if (Offs < 1) and (DrawItems[i].Length>0) then
              Offs := 1;
          finally
            FreeMem(arr);
          end;
          end
        else begin
          Offs := 1;
        end;
        end
      else begin
        Offs := (1-delta) div 2; 
        if X > dli.Left + dli.Width div 2 then begin
          {State := State + [rvstClickedBeyondItem];}
          Offs := (1+delta) div 2;
        end;
      end;
    end;
  end;
  order.Free;
  revorder.Free;
  {$IFNDEF RVDONOTUSELISTS}
  if (No>=0) and not Strict then begin
    if GetItemStyle(DrawItems[No].ItemNo)=rvsListMarker then begin
      inc(No);
      Offs := GetOffsBeforeDrawItem(No);
    end;
  end;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.RestoreSelBounds(StartNo, EndNo,
  StartOffs, EndOffs: Integer);
begin
  Item2DrawItem(StartNo, StartOffs, {->} FSelStartNo, FSelStartOffs);
  Item2DrawItem(EndNo,   EndOffs,   {->} FSelEndNo,   FSelEndOffs);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.StoreSelBounds(var StartNo, EndNo,
  StartOffs, EndOffs: Integer; Normalize: Boolean);
var SelStartNo, SelEndNo, SelStartOffs, SelEndOffs: Integer;
begin
  GetSelBounds(SelStartNo, SelEndNo, SelStartOffs, SelEndOffs, Normalize);
  DrawItem2Item(SelStartNo,SelStartOffs, {->} StartNo,StartOffs);
  DrawItem2Item(SelEndNo,  SelEndOffs,   {->} EndNo,  EndOffs);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.DrawItem2Item(DrawItemNo, DrawItemOffs: Integer;
                             var ItemNo, ItemOffs: Integer);
var dli: TRVDrawLineInfo;
begin
  ItemNo := -1;
  if DrawItemNo = -1 then exit;
  dli := DrawItems[DrawItemNo];
  if GetItemStyle(dli.ItemNo)>=0 then
    ItemOffs := DrawItemOffs + dli.Offs-1
  else
    ItemOffs := DrawItemOffs;
  ItemNo := dli.ItemNo;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.Item2DrawItem(ItemNo, ItemOffs: Integer;
                             var DrawItemNo, DrawItemOffs: Integer);
var item: TCustomRVItemInfo;
    i: Integer;
begin
  DrawItemNo := -1;
  if ItemNo = -1 then
    exit;
  item := TCustomRVItemInfo(Items.Objects[ItemNo]);
  DrawItemNo := item.DrawItemNo;
  if item.StyleNo<0 then begin
    DrawItemOffs := ItemOffs-DrawItems[DrawItemNo].Offs;
    exit;
  end;
  DrawItemOffs := ItemOffs-DrawItems[DrawItemNo].Offs+1;
  for i := DrawItemNo+1 to DrawItems.Count-1 do begin
    if (DrawItems[i].ItemNo<>ItemNo) or
       ((DrawItems[i].Offs>=ItemOffs) and not (DrawItems[i-1].Offs+DrawItems[i-1].Length<ItemOffs)) then begin
      DrawItemNo   := i-1;
      DrawItemOffs := ItemOffs-DrawItems[DrawItemNo].Offs+1;
      if (GetItemStyle(DrawItems[DrawItemNo].ItemNo)>=0) and
         (DrawItemOffs>DrawItems[DrawItemNo].Length+DrawItems[DrawItemNo].Offs) then begin
        DrawItemNo   := i;
        DrawItemOffs := DrawItems[DrawItemNo].Offs;
      end;
      break;
    end;
    if i=DrawItems.Count-1 then begin
      DrawItemNo   := i;
      DrawItemOffs := ItemOffs-DrawItems[DrawItemNo].Offs+1;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.Item2FirstDrawItem(ItemNo: Integer;
                                                    var DrawItemNo: Integer);
begin
  DrawItemNo := -1;
  if ItemNo = -1 then
    exit;
  DrawItemNo := TCustomRVItemInfo(Items.Objects[ItemNo]).DrawItemNo;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetDrawItemNo(BoundLine, Option: Integer): Integer;
var
    a,b,mid: Integer;
begin
  if DrawItems.Count = 0 then begin
     Result := 0;
     exit;
  end;
  if DrawItems[0].Top>=BoundLine then begin
     Result := 0;
     exit;
  end;
  if (Option=gdinLastVisible) and (DrawItems[DrawItems.Count-1].Top<BoundLine) then begin
     Result := DrawItems.Count-1;
     exit;
  end;
  a := 1;
  b := DrawItems.Count-1;
  mid := a;
  if Option = gdinLastCompleteVisible then begin
  {
    while (b-a)>1 do begin
      mid := (a+b) div 2;
      if (TRVDrawLineInfo(DrawItems.Objects[mid]).Top+TRVDrawLineInfo(DrawItems.Objects[mid]).Height>BoundLine) then
          b := mid
      else
          a := mid;
    end;
    if mid>= DrawItems.Count then mid := DrawItems.Count-1;
    while (mid>0) and (TRVDrawLineInfo(DrawItems.Objects[mid]).Top+TRVDrawLineInfo(DrawItems.Objects[mid]).Height>BoundLine) do dec(mid);

      if (mid>0) then dec(mid);
      while (mid>0) and not TRVDrawLineInfo(DrawItems.Objects[mid]).FromNewLine do dec(mid);
      if (mid>0) then dec(mid);
    end
    }
  end
  else begin
    while (b-a)>1 do begin
      mid := (a+b) div 2;
      if (DrawItems[mid].Top>=BoundLine) then begin
          if (DrawItems[mid-1].Top<BoundLine) then break;
          b := mid;
        end
      else
        a := mid;
    end;
    if mid>= DrawItems.Count then mid := DrawItems.Count-1;
    if Option = gdinFirstVisible then begin
      while (mid>0) and not DrawItems[mid].FromNewLine do dec(mid);
      if (mid>0) then dec(mid);
      while (mid>0) and not DrawItems[mid].FromNewLine do dec(mid);
      if (mid>0) then dec(mid);
      end
    else
      while DrawItems[mid].Top<BoundLine do inc(mid);
  end;
  Result := mid;
end;
{------------------------------------------------------------------------------}
// Calculating minimal possible width for paragraph sections containing
// items in the given range.
// This is a maximum of min widths of all paragraph sections
// Works both for normal and no-wrap paragraphs
function TCustomRVFormattedData.CalculateParaSectionsMinWidth(StartItemNo,
  EndItemNo: Integer; var FirstParaItemNo: Integer; sad: PRVScreenAndDevice;
  Canvas: TCanvas): Integer;
var i,w: Integer;
begin
  ExpandToParaSection(StartItemNo, EndItemNo, StartItemNo, EndItemNo);
  Result := 0;
  for i := StartItemNo to EndItemNo do begin
    if IsParaStart(i) then
      FirstParaItemNo := i;
    if IsFromNewLine(i) then begin
      w := CalculateParaSectionMinWidth(i, FirstParaItemNo, sad, Canvas);
      if w>Result then
        Result := w;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.CalculateParaSectionsMinWidthDef(StartItemNo,
  EndItemNo: Integer): Integer;
var FirstParaItemNo: Integer;
begin
  FirstParaItemNo := -1;
  Result := CalculateParaSectionsMinWidth(StartItemNo, EndItemNo, FirstParaItemNo, nil, nil);
end;
{------------------------------------------------------------------------------}
// Calculating total indents for the paragraph section
function TCustomRVFormattedData.CalculatePureParaSectionWidth(ItemNo: Integer;
  var FirstParaItemNo: Integer; sad: PRVScreenAndDevice; Canvas: TCanvas): Integer;
begin
  ItemNo := GetFirstParaSectionItem(ItemNo);
  Result := GetMaxIndent(ItemNo, FirstParaItemNo);
  if (sad<>nil) then
    Result := MulDiv(Result, sad.ppixDevice, sad.ppixScreen);
end;
{------------------------------------------------------------------------------}
// Calculates minimal possible width for paragraph section containing
// StartItemNo-th item.
// Works both for normal paragraphs (calls CalculateMinItemsWidthPlus)
// and for no-wrap paragraphs (calculates sum of widths of all items)
function TCustomRVFormattedData.CalculateParaSectionMinWidth(StartItemNo: Integer;
  var FirstParaItemNo: Integer; sad: PRVScreenAndDevice;
  Canvas: TCanvas): Integer;
var i, EndItemNo: Integer;
    item: TCustomRVItemInfo;
    RVStyle: TRVStyle;
    {$IFNDEF RVDONOTUSEUNICODE}
    sz: TSize;
    {$ENDIF}
begin
  item := GetItem(StartItemNo);
  if Canvas=nil then
    Canvas := GetCanvas;
  ExpandToParaSection(StartItemNo, StartItemNo, StartItemNo, EndItemNo);
  if not (rvpaoNoWrap in GetRVStyle.ParaStyles[item.ParaNo].Options) then begin
    Result := CalculateMinItemsWidthPlus(StartItemNo, EndItemNo, FirstParaItemNo, sad, Canvas);
    exit;
  end;
  RVStyle := GetRVStyle;

  Result := CalculatePureParaSectionWidth(StartItemNo, FirstParaItemNo, sad, Canvas);

  for i := StartItemNo to EndItemNo do begin
    item := GetItem(i);
    if item.StyleNo>=0 then
      with RVStyle.TextStyles[GetActualStyle(item)] do begin
        Apply(Canvas, GetParaBiDiMode(item.ParaNo));
        {$IFNDEF RVDONOTUSEUNICODE}
        if Unicode then begin
          GetTextExtentPoint32W(Canvas.Handle, Pointer(Items[i]), Length(Items[i]) div 2, sz);
          inc(Result, sz.cx);
          end
        else
        {$ENDIF}
          inc(Result, Canvas.TextWidth(Items[i]));
      end
    else
      inc(Result, item.GetMinWidth(sad, Canvas,Self));
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.CalculateParaSectionMinWidthDef(StartItemNo: Integer): Integer;
var FirstParaItemNo: Integer;
begin
  FirstParaItemNo := -1;
  Result := CalculateParaSectionMinWidth(StartItemNo, FirstParaItemNo,nil,nil);
end;
{------------------------------------------------------------------------------}
// Calculates a minimal possible width for the given range of items in
// normal (i.e. wrappable) paragraphs
// This is a maximum of individual widths of items in this range.
function TCustomRVFormattedData.CalculateMinItemsWidthPlus(StartItemNo, EndItemNo: Integer;
  var FirstParaItemNo: Integer; sad: PRVScreenAndDevice;
  Canvas: TCanvas): Integer;
var i,w, pspw: Integer;
begin
  Result := 0;
  if StartItemNo>EndItemNo then
    exit;
  pspw := CalculatePureParaSectionWidth(StartItemNo, FirstParaItemNo, sad, Canvas);
  for i := StartItemNo to EndItemNo do begin
    if IsParaStart(i) then
      FirstParaItemNo := i;
    if IsFromNewLine(i) and (i<>StartItemNo) then
      pspw := CalculatePureParaSectionWidth(i, FirstParaItemNo, sad, Canvas);
    w := pspw+CalculateMinItemWidthPlus_WithoutPSWidth(i, sad, Canvas);
    if w>Result then
      Result := w;
  end;
end;
{------------------------------------------------------------------------------}
// Calculates a minimal possible width for the given item in
// normal (i.e. wrappable) paragraphs
// If this is an item preceded by a marker, this function returns
// a sum of min widths of this item and marker.
function TCustomRVFormattedData.CalculateMinItemWidthPlus_WithoutPSWidth(ItemNo: Integer;
                                                          sad: PRVScreenAndDevice;
                                                          Canvas: TCanvas): Integer;
var item: TCustomRVItemInfo;
begin
  item := GetItem(ItemNo);
  Result := item.GetMinWidth(sad, Canvas, Self);
  {$IFNDEF RVDONOTUSELISTS}
  if item.SameAsPrev and (ItemNo>0) then begin
    item := GetItem(ItemNo-1);
    if item.StyleNo=rvsListMarker then
      inc(Result, item.GetMinWidth(sad, Canvas, Self));
  end;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
// Calculates a minimal possible width for the paragraph section containing
// the given item (both for wrappable and no-wrap paragraphs)
function TCustomRVFormattedData.CalculateMinItemWidthPlusEx(ItemNo: Integer): Integer;
var FirstParaItemNo: Integer;
begin
  FirstParaItemNo := -1;
  Result := CalculateParaSectionMinWidth(ItemNo, FirstParaItemNo, nil, GetCanvas);
end;
{------------------------------------------------------------------------------}
// Calculates a minimal possible width for the given range of items
// (both for wrappable and no-wrap paragraphs)
function TCustomRVFormattedData.CalculateMinItemsWidthPlusEx(StartItemNo,
  EndItemNo: Integer): Integer;
var item: TCustomRVItemInfo;
    dummy, sno, eno,w: Integer;
    FirstParaItemNo: Integer;
begin
  if StartItemNo>EndItemNo then begin
    Result := 0;
    exit;
  end;
  FirstParaItemNo := -1;
  item := GetItem(StartItemNo);
  ExpandToParaSection(StartItemNo, StartItemNo, dummy,eno);
  if rvpaoNoWrap in GetRVStyle.ParaStyles[item.ParaNo].Options then
    Result := CalculateParaSectionMinWidthDef(StartItemNo)
  else begin
    if eno>EndItemNo then
      eno := EndItemNo;
    Result := CalculateMinItemsWidthPlus(StartItemNo, eno, FirstParaItemNo, nil, GetCanvas);
  end;
  if eno>=EndItemNo then
    exit;
  ExpandToParaSection(EndItemNo, EndItemNo, sno, dummy);
  item := GetItem(EndItemNo);
  if rvpaoNoWrap in GetRVStyle.ParaStyles[item.ParaNo].Options then
    w := CalculateParaSectionMinWidth(EndItemNo, FirstParaItemNo, nil,GetCanvas)
  else begin
    if sno<StartItemNo then
      eno := StartItemNo;
    w := CalculateMinItemsWidthPlus(sno, EndItemNo, FirstParaItemNo, nil,GetCanvas);
  end;
  if w>Result then
    Result := w;
  if sno<=eno then
    exit;
  w := CalculateParaSectionsMinWidth(eno, sno, FirstParaItemNo, nil,nil);
  if w>Result then
    Result := w;
end;
{------------------------------------------------------------------------------}
// Calculates the minimal possible width for the whole document
function TCustomRVFormattedData.CalculateMinDocWidthPlus(sad: PRVScreenAndDevice;
                                                      Canvas: TCanvas): Integer;
var FirstParaItemNo: Integer;
begin
  if (sad<>nil) and (sad.ppixScreen=sad.ppixDevice) and
     (sad.ppiyScreen=sad.ppiyDevice) then
    sad := nil;
  FirstParaItemNo := 0;
  Result := CalculateParaSectionsMinWidth(0, Items.Count-1, FirstParaItemNo, sad, Canvas);
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.CalculateMinWidthAfterInsert(
  item: TCustomRVItemInfo; InsertItemNo: Integer): Integer;
begin
  Result := item.GetMinWidth(nil,nil,Self);
  if rvpaoNoWrap in GetRVStyle.ParaStyles[GetItemPara(InsertItemNo)].Options then
    inc(Result,CalculateParaSectionMinWidthDef(InsertItemNo));
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.AdjustJumpsCoords;
var i,x,y: Integer;
    jumpinfo: TRVJumpInfo;
begin
  for i:= 0 to Jumps.Count-1 do begin
    jumpinfo := TRVJumpInfo(Jumps[i]);
    jumpinfo.RVData.GetOrigin(x,y);
    jumpinfo.l := x+jumpinfo.RVData.DrawItems[jumpinfo.DrawItemNo].Left;
    jumpinfo.t := y+jumpinfo.RVData.DrawItems[jumpinfo.DrawItemNo].Top;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.AdjustChildrenCoords;
var i: Integer;
    ditem: TRVDrawLineInfo;
    item : TCustomRVItemInfo;
begin
  if (rvflShareContents in Flags) or (GetParentControl.ControlCount=0) or
     (rvstDoNotMoveChildren in State) then exit;
  for i:=0 to DrawItems.Count-1 do begin
    ditem := DrawItems[i];
    item := GetItem(ditem.ItemNo);
    if item.StyleNo<0 then begin
      TRVNonTextItemInfo(item).AdjustInserted(ditem.Left-GetZHOffs, ditem.Top-GetZVOffs, True);
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.FindDrawItemByItem(ItemNo: Integer): Integer;
var i: Integer;
begin
  Result := -1;
  if ItemNo = -1 then exit;
  for i :=0 to DrawItems.Count-1 do
    if DrawItems[i].ItemNo = ItemNo then begin
      Result := i;
      exit;
    end;
  //Assert(Result<>-1, 'Can''t FindDrawLineByLine');
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.FindDrawItemAtPos(X,Y: Integer): Integer;
var
    i, a,b,mid, midtop: Integer;
    dli: TRVDrawLineInfo;
begin
  if DrawItems.Count = 0 then begin
     Result := -1;
     exit;
  end;
  dli := DrawItems[0];
  if (dli.Top<=Y) and (dli.Top+dli.Height>Y) and
     (dli.Left<=X) and (dli.Left+dli.Width>X) then begin
     Result := 0;
     exit;
  end;
  if DrawItems.Count = 1 then begin
     Result := -1;
     exit;
  end;
  a := 1;
  b := DrawItems.Count-1;
  while (b-a)>1 do begin
    mid := (a+b) div 2;
    if (DrawItems[mid].Top<=Y) then
      a := mid
    else
      b := mid;
  end;
  mid := a;
  midtop := DrawItems[mid].Top;
  while (mid>=1) and
         (DrawItems[mid-1].Top+
          DrawItems[mid-1].Height>midtop) do dec(mid);
  for i:=1 to 2 do begin
    if mid = DrawItems.Count then break;
    midtop := DrawItems[mid].Top+
              DrawItems[mid].Height-1;
    while (mid<DrawItems.Count) do begin
     dli := DrawItems[mid];
     if (dli.Top>midtop) then break;
     if (dli.Top<=Y) and (dli.Top+dli.Height>Y) and
     (dli.Left<=X) and (dli.Left+dli.Width>X) then begin
        Result := mid;
        exit;
     end;
     inc(mid);
    end;
  end;
  Result := -1;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetFirstVisible(TopLine: Integer): Integer;
begin
   Result := GetDrawItemNo(TopLine,gdinFirstVisible);
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetFirstItemVisible: Integer;
var v: Integer;
begin
   v := GetDrawItemNo(GetVOffs, gdinFirstVisible);
   if v>=DrawItems.Count then
     v := DrawItems.Count-1;
   if v<0 then
     Result := -1
   else
     Result := DrawItems[v].ItemNo;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetLastItemVisible: Integer;
var v: Integer;
begin
   v := GetDrawItemNo(GetVOffs+GetHeight, gdinLastVisible);
   if v>=DrawItems.Count then
     v := DrawItems.Count-1;
   if v<0 then
     Result := -1
   else
     Result := DrawItems[v].ItemNo;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.DrawBackground(Canvas: TCanvas; r: TRect);
var X, Y: Integer;
begin
  GetOrigin(X, Y);
  GetBackground.Draw(Canvas, r, GetHOffs+X, GetVOffs+Y, 0, 0, GetWidth, GetHeight,
    GetColor, False);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.PaintTo(Canvas: TCanvas; AClipRect: TRect);
var i,no, yshift, xshift, fullwidth, selwidth, ditemselstart, ditemselend: Integer;
    dli:TRVDrawLineInfo;
    li: TCustomRVItemInfo;
    HoverNow, res: Boolean;
    s : String;
    StartNo, EndNo, StartOffs, EndOffs: Integer;
    LastDrawnStyle: Integer;
    RVStyle: TRVStyle;
    BiDiMode: TRVBiDiMode;
    DefaultTextDrawState: TRVTextDrawStates;
    DefaultDrawState: TRVItemDrawStates;
    FirstDLine, LastDLine: Integer;
    LineTopY, LineBottomY: Integer;
    ShowSpecialCharacters: Boolean;
  {.......................................................}
    function GetSelDrawState: TRVTextDrawStates;
    begin
      Result := [rvtsSelected];
      if HoverNow then
        Include(Result, rvtsHover);
    end;
  {.......................................................}
    procedure DrawText(X,Y,W, Offs: Integer; const s: String;
                       IsStart, IsEnd, Selected, SBAdded: Boolean);
                       // in : dli, Canvas, xshift, yshift, hovernow, no
    {$IFNDEF RVDONOTUSEJUSTIFY}
    var SpaceBefore: Integer;
    {$ENDIF}
    var DrawState: TRVTextDrawStates;
    begin
      {
      if s='' then begin
        Canvas.Pen.Color := clBlue;
        Canvas.Pen.Style := psSolid;
        Canvas.Ellipse(x-10-xshift,y-10-yshift,x+10-xshift,y+10-yshift);
      end;
      }
      DrawState := DefaultTextDrawState;
      if Selected then Include(DrawState, rvtsSelected);
      if HoverNow then
        Include(DrawState, rvtsHover);
      if Assigned(RVStyle.OnDrawStyleText) then begin
        if IsStart then begin
          Include(DrawState, rvtsDrawItemStart);
          if dli.Offs=1 then
            Include(DrawState, rvtsItemStart);
        end;
        if IsEnd then begin
          Include(DrawState, rvtsDrawItemEnd);
          if dli.Offs+dli.Length-1=ItemLength(dli.ItemNo) then
            Include(DrawState, rvtsItemEnd);
        end;
      end;
      RVStyle.ApplyStyleColor(Canvas, No, DrawState, False, rvcmColor);
      {$IFNDEF RVDONOTUSEJUSTIFY}
      if IsStart then begin
        SpaceBefore := dli.SpaceBefore;
        if not SBAdded then
          inc(W,SpaceBefore);
        end
      else begin
        SpaceBefore := 0;
        inc(X, dli.SpaceBefore);
        if SBAdded then
          dec(W,dli.SpaceBefore);
      end;
      {$ENDIF}
      if ShowSpecialCharacters then
        Include(DrawState, rvtsSpecialCharacters);
      if Selected and (RVStyle.SelectionStyle=rvssLines) and (W>0) then
        Canvas.FillRect(Rect(X-xshift-1, LineTopY-yshift, X-xshift+W+1, LineBottomY-yshift));
      RVStyle.DrawStyleText(s, Canvas, dli.ItemNo, Offs+dli.Offs-1, No, Self,
        {$IFNDEF RVDONOTUSEJUSTIFY}SpaceBefore,{$ELSE}0,{$ENDIF}
        X-xshift, Y-yshift, W, dli.Height, DrawState, False, False,
        rvcmColor, BiDiMode);
    end;
  {.......................................................}
  function GetLineTopY(DrawItemNo: Integer): Integer;
  var i: Integer;
  begin
    Result := DrawItems[DrawItemNo].Top;
    for i := DrawItemNo downto 0 do begin
      if DrawItems[i].Top<Result then
        Result := DrawItems[i].Top;
      if DrawItems[i].FromNewLine then
        break;
    end;
    for i := DrawItemNo+1 to DrawItems.Count-1 do begin
      if DrawItems[i].FromNewLine then
        break;
      if DrawItems[i].Top<Result then
        Result := DrawItems[i].Top;
    end;
  end;
  {.......................................................}
  procedure DrawSoftPageBreaks;
  var FirstPB, i,DItemNo, Offs, Y: Integer;
      dli: TRVDrawLineInfo;
      PBInfo: TRVSoftPageBreakInfo;
  begin
    FirstPB := -1;
    dli := DrawItems[FirstDLine];
    for i := 0 to FSoftPageBreaks.Count-1 do
      if TRVSoftPageBreakInfo(FSoftPageBreaks.Items[i]).ItemNo>=dli.ItemNo then begin
        FirstPB := i;
        break;
      end;
    if FirstPB<0 then
      exit;
    for i := FirstPB to FSoftPageBreaks.Count-1 do begin
      PBInfo := TRVSoftPageBreakInfo(FSoftPageBreaks.Items[i]);
      Item2DrawItem(PBInfo.ItemNo, PBInfo.Offset, DItemNo, Offs);
      if DItemNo>LastDLine then
        exit;
      if (DItemNo=0) and (PBInfo.ExtraData<0) then
        continue;
      if PBInfo.ExtraData>=0 then
        Y := DrawItems[DItemNo].Top+GetItem(PBInfo.ItemNo).GetSoftPageBreakDY(PBInfo.ExtraData)
      else
        Y := GetLineTopY(DItemNo);
      RVStyle.DrawPageBreak(Canvas, Y-yshift-1, xshift, rvpbSoftPageBreak, GetParentControl);
    end;
  end;
  {.......................................................}
  procedure GetLineVerticalBounds(FirstDrawItemNo: Integer;
    var LineTopY, LineBottomY: Integer);
  var i, LastDrawItemNo: Integer;
      ditem: TRVDrawLineInfo;
  begin
    ditem := DrawItems[FirstDrawItemNo];
    LineTopY := ditem.Top;
    LineBottomY := LineTopY+ditem.Height+ditem.ExtraSpaceBelow;
    if (FirstDrawItemNo+1>=DrawItems.Count) or
       DrawItems[FirstDrawItemNo+1].FromNewLine then
       LastDrawItemNo := FirstDrawItemNo
    else begin
      LastDrawItemNo := DrawItems.Count-1;
      for i := FirstDrawItemNo+1 to DrawItems.Count-1 do begin
        ditem := DrawItems[i];
        if ditem.FromNewLine then begin
          LastDrawItemNo := i-1;
          break;
        end;
        if LineTopY>ditem.Top then
          LineTopY := ditem.Top;
        if LineBottomY<ditem.Top+ditem.Height+ditem.ExtraSpaceBelow then
          LineBottomY := ditem.Top+ditem.Height+ditem.ExtraSpaceBelow;
      end;
    end;
    if IsDrawItemParaStart(FirstDrawItemNo) then
      dec(LineTopY, RVStyle.ParaStyles[GetItemPara(
        DrawItems[FirstDrawItemNo].ItemNo)].SpaceBefore);
    if IsDrawItemParaEnd(LastDrawItemNo) then
      inc(LineBottomY, RVStyle.ParaStyles[GetItemPara(
        DrawItems[FirstDrawItemNo].ItemNo)].SpaceAfter);
  end;
  {.......................................................}
  function UseLineSelection(ditem: TRVDrawLineInfo): Boolean;
  begin
    Result := (FSelStartNo>=0) and (RVStyle.SelectionStyle=rvssLines) and
      (GetItemBiDiMode(ditem.ItemNo)=rvbdUnspecified);
  end;
  {.......................................................}
  procedure DrawParaMarkAfter(DrawItemNo: Integer);
  var BiDiMode: TRVBiDiMode;
      Left: Integer;
      s: String;
      sz: TSize;
      ditem: TRVDrawLineInfo;
  begin
    if (DrawItemNo+1<DrawItems.Count) and
       ((DrawItems[DrawItemNo].ItemNo=DrawItems[DrawItemNo+1].ItemNo) or
        not IsFromNewLine(DrawItems[DrawItemNo+1].ItemNo)) then
      exit;
    ditem := DrawItems[DrawItemNo];
    if LastDrawnStyle<0 then
      LastDrawnStyle := 0;
    BiDiMode := GetParaBiDiMode(GetItemPara(ditem.ItemNo));
    RVStyle.ApplyStyle(Canvas, LastDrawnStyle, BiDiMode);
    RVStyle.ApplyStyleColor(Canvas, LastDrawnStyle, [], False, rvcmColor);
    if (DrawItemNo+1=DrawItems.Count) or IsParaStart(DrawItems[DrawItemNo+1].ItemNo) then
      s := #$B6
    else begin
      {$IFDEF RICHVIEWCBDEF3}
      Canvas.Font.Charset := SYMBOL_CHARSET;
      {$ENDIF}
      Canvas.Font.Name := 'Symbol';
      s := #$BF;
      {
      Canvas.Font.Name := 'Wingdings';
      s := #$C3;
      }
      LastDrawnStyle := -1;
    end;
    GetTextExtentPoint32(Canvas.Handle, PChar(s), 1, sz);
    if BiDiMode<>rvbdRightToLeft then
      Left := ditem.Left+ditem.Width
    else
      Left := ditem.Left-sz.cx;
    Canvas.TextOut(Left-xshift, ditem.Top+ditem.Height-yshift-sz.cy, s);
  end;
  {.......................................................}
  var
    AParaNo, MaxTextWidth: Integer;

    BelowBottom: Boolean;
    BRect, BRect1: TRect;
    DrawState: TRVItemDrawStates;
    TextDrawState: TRVTextDrawStates;
    ShiftedClipRect: TRect;
begin
  RVStyle := GetRVStyle;
  GetSelBounds(StartNo, EndNo, StartOffs, EndOffs, True);
  ShiftedClipRect := AClipRect;

  yshift := GetZVOffs;
  xshift := GetZHOffs;
  OffsetRect(ShiftedClipRect,xshift,yshift);
  inc(yshift, AClipRect.Top);
  inc(xshift, AClipRect.Left);
  OffsetRect(AClipRect,-AClipRect.Left,-AClipRect.Top);
  Canvas.Brush.Style := bsClear;
  LastDrawnStyle := -1;
  BiDiMode       := rvbdUnspecified;
  DefaultTextDrawState := [];
  DefaultDrawState := [];
  ShowSpecialCharacters := rvoShowSpecialCharacters in GetOptions;
  if TRVScroller(GetParentControl).FocusedEx then begin
    Include(DefaultTextDrawState, rvtsControlFocused);
    Include(DefaultDrawState, rvidsControlFocused);
  end;
   {
  if GetMaxTextWidth<>0 then begin
    Canvas.Pen.Color := clBtnFace;
    Canvas.MoveTo(GetLeftMargin+GetMaxTextWidth-xshift, AClipRect.Top);
    Canvas.LineTo(GetLeftMargin+GetMaxTextWidth-xshift, AClipRect.Bottom);
  end;
   }

  // drawing paragraph backgrounds and frames...
  FirstDLine := GetFirstVisible(ShiftedClipRect.Top);
  LastDLine  := DrawItems.Count-1;
  BelowBottom := False;

  i := FirstDLine;
  while (i>0) and
       ((DrawItems[i].ItemNo=DrawItems[i-1].ItemNo) or
         not GetItem(DrawItems[i].ItemNo).CanBeBorderStart) do
    dec(i);
  MaxTextWidth := GetMaxTextWidth;
  while i<DrawItems.Count do begin
    dli := DrawItems[i];
    if BelowBottom and dli.FromNewLine then begin
      if LastDLine>i then
        LastDLine := i;
      break;
    end;
    if GetItem(dli.ItemNo).CanBeBorderStart and
      (GetItem(dli.ItemNo).StyleNo<>rvsBreak) and
      (
      (RVStyle.ParaStyles[GetItemPara(dli.ItemNo)].Border.Style<>rvbNone) or
      (RVStyle.ParaStyles[GetItemPara(dli.ItemNo)].Background.Color<>clNone) or
      Assigned(RVStyle.OnDrawParaBack)
      ) then begin
      AParaNo := GetItemPara(dli.ItemNo);
      with RVStyle.ParaStyles[AParaNo] do begin
        BRect.Left := GetLeftMargin+LeftIndent;
        if FirstIndent<0 then
          inc(BRect.Left, FirstIndent);
        if rvoClientTextWidth in Self.Options then
          BRect.Right := GetWidth-(GetRightMargin+RightIndent)
        else if MaxTextWidth>0 then
          BRect.Right := GetLeftMargin+MaxTextWidth-RightIndent
        else
          BRect.Right := GetAreaWidth-(GetRightMargin+RightIndent);
        if dli.Left+dli.Width>BRect.Right then
          BRect.Right := dli.Left+dli.Width;
      end;
      BRect.Top := dli.Top;
      BRect.Bottom := dli.Top+dli.Height+dli.ExtraSpaceBelow;
      inc(i);
      while (i<DrawItems.Count) and
            ((DrawItems[i].ItemNo=DrawItems[i-1].ItemNo) or
             not GetItem(DrawItems[i].ItemNo).CanBeBorderStart) do begin
        dli := DrawItems[i];
        if dli.Top<BRect.Top then
          BRect.Top := dli.Top;
        if dli.Top+dli.Height+dli.ExtraSpaceBelow>BRect.Bottom then
          BRect.Bottom := dli.Top+dli.Height+dli.ExtraSpaceBelow;
        if dli.Left+dli.Width>BRect.Right then
          BRect.Right := dli.Left+dli.Width;
        if BelowBottom and dli.FromNewLine and (LastDLine>i) then
           LastDLine := i;
        if (dli.Top>ShiftedClipRect.Bottom) and (GetMaxTextWidth=0) then
          BelowBottom := True;
        inc(i);
      end;
      OffsetRect(BRect, -xshift, -yshift);
      BRect1 := BRect;
      RVStyle.ParaStyles[AParaNo].Background.PrepareDraw(BRect1);
      RVStyle.DrawParaBack(Canvas, AParaNo, BRect1, False, rvcmColor);
      RVStyle.ParaStyles[AParaNo].Border.Draw(BRect, Canvas);
      end
    else
      inc(i);
    if dli.Top>ShiftedClipRect.Bottom then
      BelowBottom := True;
  end;
  // drawing items...
  if Assigned(RVStyle.OnDrawTextBack) then
    for i:= FirstDLine to LastDLine do begin
      dli := DrawItems[i];
      with GetItem(dli.ItemNo) do
        if StyleNo>=0 then begin
          TextDrawState := [];
          if RVStyle.TextStyles[GetActualStyleNo(RVStyle)].Jump and
             (rvstDrawHover in State) and
             (LastJumpMovedAbove<>-1) and (JumpID = LastJumpMovedAbove) then
            Include(TextDrawState, rvtsHover);
          RVStyle.DrawTextBack(Canvas, dli.ItemNo, GetActualStyleNo(RVStyle),
            Self, dli.Left-xshift, dli.Top-yshift, dli.Width, dli.Height,
            TextDrawState);
        end;
    end;
  // drawing soft page breaks
  if (FSoftPageBreaks<>nil) and (rvoShowPageBreaks in Options) then
    DrawSoftPageBreaks;
  LineTopY := 0;
  LineBottomY := 0;
  for i:= FirstDLine to LastDLine do begin
    dli := DrawItems[i];
    if dli.FromNewLine and UseLineSelection(dli) then
      GetLineVerticalBounds(i, LineTopY, LineBottomY);
    li := GetItem(dli.ItemNo);
    no := GetActualStyle(li);
    if (rvioPageBreakBefore in li.ItemOptions) and
       (rvoShowPageBreaks in Options) and
       ((i=0) or (DrawItems[i-1].ItemNo<>dli.ItemNo))
       then begin
      RVStyle.DrawPageBreak(Canvas, GetLineTopY(i)-yshift-1, xshift, rvpbPageBreak, GetParentControl);
      LastDrawnStyle := -1;
    end;
    if (li.Checkpoint<>nil) and (rvoShowCheckpoints in Options) and
       ((i=0) or (DrawItems[i-1].ItemNo<>dli.ItemNo)) then begin
      RVStyle.DrawCheckpoint(Canvas, dli.Left-xshift, dli.Top-yshift,
                             dli.ItemNo, xshift,
                             li.Checkpoint.RaiseEvent,
                             GetParentControl);
      LastDrawnStyle := -1;
    end;
    if no>=0 then begin { text }
      if No<>LastDrawnStyle then begin
        LastDrawnStyle := No;
        RVStyle.ApplyStyle(Canvas, No, GetParaBiDiMode(li.ParaNo));
      end;
      BiDiMode := GetItemBiDiMode(dli.ItemNo);
      HoverNow := RVStyle.TextStyles[no].Jump and (rvstDrawHover in State) and
                 (LastJumpMovedAbove<>-1) and (li.JumpID = LastJumpMovedAbove);
      if ((StartNo>i) or (EndNo<i)) and
         not (rvstCompletelySelected in State) then begin {not selected}
        DrawText(dli.Left, dli.Top, dli.Width, 1, DrawItems.GetString(i,Items), True, True, False,True);
        end
      else if (rvstCompletelySelected in State) or
          (((StartNo<i) or ((StartNo=i) and (StartOffs<=1))) and {selected completely}
           (((EndNo>i)) or ((EndNo=i) and (EndOffs>DrawItems[i].Length)))) then begin
        DrawText(dli.Left, dli.Top, dli.Width, 1, DrawItems.GetString(i,Items),True, True, True, True);
        end
      else begin { selected partially }
        DrawText(dli.Left, dli.Top, dli.Width, 1, DrawItems.GetString(i,Items), True, True, False,True);
        if (i<>StartNo) or (StartOffs<=1) then
          ditemselstart := 1
        else
          ditemselstart := StartOffs;
        if (i<>EndNo) or (EndOffs>dli.Length) then
          ditemselend := dli.Length
        else
          ditemselend := EndOffs-1;
        if ditemselend<ditemselstart then begin
          if ShowSpecialCharacters then
            DrawParaMarkAfter(i);
          continue;
        end;
        s  := DrawItems.GetString(i,Items);
        if BiDiMode<>rvbdUnspecified then begin
          {$IFNDEF RVDONOTUSEALLCAPS}
          s  := RV_ReturnCapitalized(s, RVStyle.TextStyles[No]);
          {$ENDIF}
          RVStyle.ApplyStyleColor(Canvas, no, GetSelDrawState, False, rvcmColor);
          res := RVU_DrawSelectedTextEx(dli.Left{$IFNDEF RVDONOTUSEJUSTIFY}+dli.SpaceBefore{$ENDIF}-xshift,
            dli.Top-yshift, dli.Width{$IFNDEF RVDONOTUSEJUSTIFY}-dli.SpaceBefore{$ENDIF},
            dli.Height, s, Canvas, ditemselstart, ditemselend, li.ItemOptions,
            BiDiMode);
          end
        else
          res := False;
        if not res then begin
          if ditemselstart>1 then begin
            s := DrawItems.GetSubString(i, Items, 1, ditemselstart-1);
            {$IFNDEF RVDONOTUSEALLCAPS}
            s  := RV_ReturnCapitalized(s, RVStyle.TextStyles[No]);
            {$ENDIF}
            fullwidth := RVU_TextWidth(s, Canvas, li.ItemOptions);
            end
          else
            fullwidth := 0;
          s := DrawItems.GetSubString(i, Items, ditemselstart, ditemselend-ditemselstart+1);
          if Assigned(RVStyle.OnDrawStyleText) or
            (RVStyle.SelectionStyle=rvssLines) then begin
            {$IFNDEF RVDONOTUSEALLCAPS}
            s  := RV_ReturnCapitalized(s, RVStyle.TextStyles[No]);
            {$ENDIF}
            selwidth := RVU_TextWidth(s, Canvas, li.ItemOptions);
            end
          else
            selwidth := 0;
          DrawText(dli.Left+fullwidth, dli.Top, selwidth, ditemselstart,
                   s, ditemselstart<=1, ditemselend>=dli.Length, True, False);
        end;
      end;
      if ShowSpecialCharacters then
        DrawParaMarkAfter(i);
      continue;
    end;
    DrawState := DefaultDrawState;
    if ((StartNo<=i) and (EndNo>=i) and
      not ((EndNo=i) and (EndOffs=0)) and
      not ((StartNo=i) and (StartOffs=1))) or
      (rvstCompletelySelected in State) then
      Include(DrawState,rvidsSelected);
    if (rvstDrawHover in State) and
       (
       ((LastJumpMovedAbove<>-1) and (li.JumpID = LastJumpMovedAbove))
       ) then
      Include(DrawState, rvidsHover);

    if (li = FActiveItem) then
      Include(DrawState, rvidsCurrent);
    if not li.GetBoolValue(rvbpFullWidth) then begin
      // controls, pictures, bullets, hotspots, etc.
        if (rvidsSelected in DrawState) and UseLineSelection(dli)
          {$IFNDEF RVDONOTUSELISTS} and (li.StyleNo<>rvsListMarker) {$ENDIF}
          then begin
          if rvidsControlFocused in DefaultDrawState then
            Canvas.Brush.Color := RVStyle.SelColor
          else
            Canvas.Brush.Color := RVStyle.InactiveSelColor;
          if Canvas.Brush.Color<>clNone then begin
            Canvas.Brush.Style := bsSolid;
            Canvas.FillRect(Rect(dli.Left-xshift, LineTopY-yshift,
              dli.Left-xshift+dli.Width, LineBottomY-yshift));
          end;
          Canvas.Brush.Style := bsClear;
        end;
      li.Paint(dli.Left-xshift, dli.Top-yshift, Canvas, DrawState, RVStyle, dli)
      end
    else begin
      // breaks, tables
      if rvoClientTextWidth in Options then
        fullwidth := GetWidth
      else begin
        fullwidth := GetAreaWidth;
        if (GetMaxTextWidth>0) and (GetMaxTextWidth+GetLeftMargin+GetRightMargin<fullwidth) then
          fullwidth := GetMaxTextWidth+GetLeftMargin+GetRightMargin;
      end;
      li.PaintFullWidth(dli.Left-xshift, fullwidth-GetRightMargin-xshift,
                                dli.Top-yshift, Canvas, DrawState, RVStyle, AClipRect, dli);
    end;
    if ShowSpecialCharacters then
      DrawParaMarkAfter(i);
    if li.GetBoolValue(rvbpDrawingChangesFont) then
      LastDrawnStyle := -1;
  end;
  if (NotAddedCP<>nil) and (rvoShowCheckpoints in Options) then
    RVStyle.DrawCheckpoint(Canvas, -1, DocumentHeight-yshift, -1, xshift,
                           NotAddedCP.RaiseEvent, GetParentControl);
  PostPaintTo(Canvas, xshift, yshift, FirstDLine, LastDLine);
  {$IFDEF RVWATERMARK}
  if rvflRoot in Flags then begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Name := 'Arial';
    Canvas.Font.Size := 8;
    Canvas.Font.Style := [];
    Canvas.Font.Color := clRed;
    Canvas.TextOut(-xshift+GetWidth-80, -yshift, 'unregistered');
  end;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.PostPaintTo(Canvas: TCanvas;
  HOffs, VOffs, FirstDrawItemNo, LastDrawItemNo: Integer);
begin

end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.MouseLeave;
var id: Integer;
begin
  if not (rvflUseJumps in Flags) then
    exit;
  if LastRVDataMovedAbove<>nil then
    id := LastRVDataMovedAbove.LastJumpMovedAbove
  else
    id := -1;
  ClearLastJump;
  if id<>-1 then
    InvalidateJumpRect(id);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.InvalidateJumpRect(id: Integer);
var rec: TRect;
    i : Integer;
begin
   if GetRVStyle.FullRedraw then
     Invalidate
   else begin
     for i:=0 to Jumps.Count -1 do
       if id = TRVJumpInfo(Jumps[i]).id then
         with TRVJumpInfo(Jumps[i]) do begin
           rec := Bounds(l-GetZHOffs-5, t-GetZVOffs-5, w+10, h+10);
           InvalidateRect(rec);
         end;
   end;
   GetParentControl.Update;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.Invalidate;
begin
  if GetParentControl<>nil then
    GetParentControl.Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.Refresh;
begin
  if GetParentControl<>nil then
    GetParentControl.Refresh;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.UpdateView;
begin
  if GetParentControl<>nil then
    GetParentControl.Update;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.InvalidateRect(const r: TRect);
begin
  if (GetParentControl<>nil) and GetParentControl.HandleAllocated then
    Windows.InvalidateRect(GetParentControl.Handle, @r, False);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.SearchHotItem(X,Y,HOffs,VOffs: Integer);
begin
  if FCaptureMouseItem<>nil then
    exit;
  inc(X, HOffs);
  inc(Y, VOffs);
  if (LastDIMovedAbove<>-1) then
   if (LastDIMovedAbove>=DrawItems.Count) then
     LastDIMovedAbove := -1
   else
     with DrawItems[LastDIMovedAbove] do
       if not RV_PointInRect(X,Y, Left,Top,Width,Height) then
         LastDIMovedAbove := -1;
  if LastDIMovedAbove=-1 then
    LastDIMovedAbove := FindDrawItemAtPos(X,Y);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.MouseMove(Shift: TShiftState; X, Y: Integer);
var  JumpFound: Boolean;
     VOffs, HOffs: Integer;
    //SelRect: TRect;
    {...................................................}
    function MouseMoveHotItem(HOffs,VOffs: Integer): Boolean;
    {$IFNDEF RVDONOTUSEDRAGDROP}
    var DItemNo, DItemOffs: Integer;
    {$ENDIF}
    begin
      SearchHotItem(X,Y,HOffs,VOffs);
      if (LastDIMovedAbove<>-1) then
        with DrawItems[LastDIMovedAbove] do begin
          Result := GetItem(ItemNo).MouseMove(Shift,
            X-(Left-GetZHOffs),Y-(Top-GetZVOffs), ItemNo, Self);
          {$IFNDEF RVDONOTUSEDRAGDROP}
          if not Result and not (ssShift in Shift) and CanStartDragging then begin
            FindDrawItemForSel(X+HOffs, Y+VOffs, DItemNo, DItemOffs, True);
            if (DItemNo>=0) and DItem_InsideSelection(DItemNo, DItemOffs) then begin
              SetCursor(crArrow);
              Result := True;
            end;
          end;
          {$ENDIF}
        end
      else begin
        if rvoShowItemHints in GetOptions then begin
          GetAbsoluteRootData.GetParentControl.Hint := '';
          Application.CancelHint;
        end;
        Result := False;
      end;
    end;
    {...................................................}
    procedure ChangeSelection;
    var xs,ys,VPos,HPos,Width,Height: Integer;
    begin
      HPos := GetZHOffs;
      VPos := GetZVOffs;
      if rvstMakingSelection in State  then begin
        ys := y;
        if not (rvflMouseXYAlwaysCorrect in Flags) then begin
          Height := GetHeight;
          ZoomInt(Height);
          if ys<0 then
            ys := 0
          else if ys>Height then
            ys := Height;
        end;
        xs := x;
        if not (rvflMouseXYAlwaysCorrect in Flags) then begin
          Width := GetWidth;
          ZoomInt(Width);
          if xs<0 then
            xs := 0
          else if xs>Width then
            xs := Width;
        end;
        if rvstLineSelection in State then begin
          if AdjustLineSelection(xs+HPos, ys+VPos) then begin
            ExpandSelectionToLines(False);
            DoOnSelection(False);
            Refresh;
          end;
          end
        else if AdjustSelectionByMode(xs+HPos, ys+VPos) then begin
          DoOnSelection(True);
          //if LastSelectionRect.Left=-1 then
            Refresh
          //else begin
          //  SelRect := GetClientSelectionRect;
          //  UnionRect(LastSelectionRect,LastSelectionRect,SelRect);
          //  InvalidateRect(LastSelectionRect);
          //  LastSelectionRect := SelRect;
          //end;
        end;
      end;
    end;
    {...................................................}
    function HighlightJump: Boolean;
    var i,HPos, VPos: Integer;
        item: TCustomRVItemInfo;
    begin
      HPos := GetZHOffs;
      VPos := GetZVOffs;
      for i:=0 to jumps.Count-1 do
        with TRVJumpInfo(Jumps[i]) do
          if RV_PointInRect(X ,Y, l-HPos, t-VPos, w, h) then begin
            Result := True;
            SetCursor(TRVJumpInfo(Jumps[i]).Cursor);
            if (LastRVDataMovedAbove=nil) or (LastRVDataMovedAbove.LastJumpMovedAbove<>id) then
              DoRVMouseMove(id+FirstJumpNo);
            if (LastRVDataMovedAbove<>nil) and (rvstDrawHover in LastRVDataMovedAbove.State) and
               (LastRVDataMovedAbove.LastJumpMovedAbove<>id) then begin
              LastRVDataMovedAbove.State := LastRVDataMovedAbove.State - [rvstDrawHover];
              InvalidateJumpRect(LastRVDataMovedAbove.LastJumpMovedAbove);
            end;
            RVData.LastJumpMovedAbove := id;
            item := RVData.GetItem(RVData.DrawItems[DrawItemNo].ItemNo);
            item.MouseMove(Shift, X-(RVData.DrawItems[DrawItemNo].Left-HPos),
              Y-(RVData.DrawItems[DrawItemNo].Top-VPos),
              RVData.DrawItems[DrawItemNo].ItemNo, RVData);
            LastRVDataMovedAbove := RVData;
            if not (rvstDrawHover in RVData.State) then begin
              if not item.GetBoolValueEx(rvbpHotColdJump,GetRVStyle) then
                exit;
              RVData.State := RVData.State + [rvstDrawHover];
              InvalidateJumpRect(RVData.LastJumpMovedAbove);
            end;
            exit;
          end;
      Result := False;
    end;
    {...................................................}
begin
  if GetRVStyle=nil then exit;
  HOffs := GetZHOffs;
  VOffs := GetZVOffs;
  ZoomInt(X);
  ZoomInt(Y);
  ClearXorDrawing;
  if FCaptureMouseItem=nil then begin
    ChangeSelection;
    if not (rvstMakingSelection in State) and (GetBiDiMode<>rvbdRightToLeft) and
      (X+HOffs<GetLeftMargin) and (rvoAllowSelection in Options) then
      SetCursor(GetRVStyle.LineSelectCursor)
    else begin
      if rvflUseJumps in Flags then begin
        JumpFound := HighlightJump;
        if not JumpFound then begin
          if (LastRVDataMovedAbove<>nil) then begin
            LastRVDataMovedAbove.State := LastRVDataMovedAbove.State - [rvstDrawHover];
            InvalidateJumpRect(LastRVDataMovedAbove.LastJumpMovedAbove);
            LastRVDataMovedAbove.LastJumpMovedAbove := -1;
            LastRVDataMovedAbove := nil;
            DoRVMouseMove(-1);
          end;
          if not MouseMoveHotItem(HOffs,VOffs) then
            SetCursor(GetNormalCursor);
        end
        end
      else
        if not MouseMoveHotItem(HOffs,VOffs) then
          SetCursor(GetNormalCursor);
    end;
    end
  else
    if not MouseMoveHotItem(HOffs,VOffs) then
      SetCursor(GetNormalCursor);

  XorDrawingEx(X,Y);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.MouseDown(Button: TMouseButton; Shift: TShiftState;
                                           X, Y: Integer);
var i,StyleNo, ItemNo, Dummy, HOffs, VOffs: Integer;
    ClickedWord: String;
    RVData: TCustomRVFormattedData;
begin
  HOffs := GetZHOffs;
  VOffs := GetZVOffs;
  ZoomInt(X);
  ZoomInt(Y);
  begin
    SearchHotItem(X,Y, HOffs,VOffs);
    FClickedDrawItemNo := LastDIMovedAbove;
    if LastDIMovedAbove<>-1 then begin
      ItemNo := DrawItems[LastDIMovedAbove].ItemNo;
      if (FPartialSelectedItem<>GetItem(ItemNo)) and
         (GetChosenItem<>GetItem(ItemNo)) then begin
        DeselectPartiallySelectedItem(nil);
        if not (ssDouble in Shift) then
          Windows.SetFocus(GetParentControl.Handle);
      end;
      if LastDIMovedAbove<>-1 then
        with DrawItems[LastDIMovedAbove] do begin
           if (GetInplaceEditor<>nil) and
             not GetItem(ItemNo).OwnsInplaceEditor(GetInplaceEditor) then begin
               DestroyInplaceEditor;
               Windows.SetFocus(GetParentControl.Handle);
             end;
           GetItem(ItemNo).MouseDown(Button, Shift, X-(Left-HOffs),Y-(Top-VOffs),
             ItemNo, Self);
        end;
      end
    else begin
      ItemNo := -1;
      DestroyInplaceEditor;
      if not (ssDouble in Shift) then
        Windows.SetFocus(GetParentControl.Handle);
    end;
    if (GetInplaceEditor=nil) and IsAssignedRVMouseDown then
      DoRVMouseDown(Button, Shift, ItemNo, X, Y);
    if (GetInplaceEditor=nil) and (Button=mbLeft) then begin
      LastJumpDowned := -1;
      for i:=0 to Jumps.Count-1 do
        with TRVJumpInfo(Jumps[i]) do
        if (X>=l-HOffs) and (X<=l+w-HOffs) and
           (Y>=t-VOffs) and (Y<=t+h-VOffs) then begin
          LastJumpDowned := id;
          break;
        end;
    end;
    if (FCaptureMouseItem<>nil) or (GetInplaceEditor<>nil) then begin
      if LastDIMovedAbove<>-1 then begin
        FSelStartNo := LastDIMovedAbove;
        FSelEndNo := LastDIMovedAbove;
        FSelStartOffs := 1;
        FSelEndOffs   := 1;
        DoOnSelection(True);
      end;
      exit;
    end;
  end;
  if (Button = mbLeft) and not (rvstIgnoreNextMouseDown in State) and not
     (rvstStartingDragDrop in GetAbsoluteRootData.State) then begin
    if (rvoSingleClick in Options) and IsAssignedRVDblClick  and
       FindWordAt(Clickedword, X, Y, StyleNo, ItemNo, Dummy, RVData) then
      DoRVDblClick(ClickedWord, StyleNo);
    if (rvoAllowSelection in Options) and  (ssLeft in Shift) then begin
      State := State - [rvstLineSelection];
      DeselectPartiallySelectedItem(nil);
      GetSelStart(FSelStartNo,FSelStartOffs);
      FindDrawItemForSel(X+HOffs, Y+VOffs, ItemNo, Dummy, False);
      {$IFNDEF RVDONOTUSEDRAGDROP}
      if not (ssShift in Shift) and (X+HOffs>=GetLeftMargin) and
         not (rvstDragDropCursorNotMoved in GetAbsoluteRootData.State) and
         DItem_InsideSelection(ItemNo, Dummy) and CanStartDragging then begin
        GetAbsoluteRootData.State := GetAbsoluteRootData.State+[rvstStartingDragDrop];
        PostMessage(GetAbsoluteRootData.GetParentControl.Handle, WM_RVDRAGDROP, 0, 0);
        end
      else
      {$ENDIF}
      begin
        FSelEndNo := ItemNo;
        FSelEndOffs := Dummy;
        if not (ssShift in Shift) or (FSelStartNo=-1) then begin
          FSelStartNo   := FSelEndNo;
          FSelStartOffs := FSelEndOffs;
        end;
        if (FSelEndNo<>-1) then begin
          State := State + [rvstMakingSelection];
          FSelectingInfo.Free;
          FSelectingInfo := nil;
          if (GetBiDiMode<>rvbdRightToLeft) and (X+HOffs<GetLeftMargin) and
             not (ssShift in Shift) then begin
            State := State + [rvstLineSelection];
            ExpandSelectionToLines(True);
            end
          else begin
            if (GetRVStyle.SelectionMode in [rvsmWord, rvsmParagraph]) then begin
              FSelectingInfo := TRVSelectingInfo.Create;
              FSelectingInfo.DrawItemSOffs := FSelStartOffs;
              FSelectingInfo.DrawItemEOffs := FSelStartOffs;
              FSelectingInfo.DrawItemSNo   := FSelStartNo;
              if GetRVStyle.SelectionMode = rvsmParagraph then begin
                FSelectingInfo.InitE(rvsesParaMode);
                if ssShift in Shift then
                  AdjustSelectionByMode(X+HOffs, Y+VOffs);
                end
              else begin
                if InsideWord(FSelStartNo, FSelStartOffs) then begin
                  GetWordBounds(FSelStartNo, FSelStartOffs,
                    FSelectingInfo.DrawItemSWordOffs1, FSelectingInfo.DrawItemSWordOffs2);;
                  FSelectingInfo.InitE(rvsesInWord)
                  end
                else
                  FSelectingInfo.InitE(rvsesFreeMode);
              end;
            end;
          end;
          end
        else
          State := State - [rvstMakingSelection];
        DoOnSelection(True);
        Invalidate;
      end;
      //LastSelectionRect := GetClientSelectionRect;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                   X, Y: Integer);
var i, StyleNo, no, xs, ys: Integer;
    clickedword: String;
    p: TPoint;
    DoNotJump: Boolean;
    ItemNo, ItemOffs, Dummy: Integer;
    ASelStartNo, ASelEndNo, ASelStartOffs, ASelEndOffs: Integer;
    HOffs, VOffs, Width, Height: Integer;
    RVData: TCustomRVFormattedData;
begin
  HOffs := GetZHOffs;
  VOffs := GetZVOffs;
  Height := GetHeight;
  Width := GetWidth;
  ZoomInt(X);
  ZoomInt(Y);
  ZoomInt(Width);
  ZoomInt(Height);
  DoNotJump := SelectionExists(True, True);
  {$IFDEF RVMUDESELECT}
  if rvstMidSelClicked in State then begin
    State := State - [rvstMidSelClicked];
    Deselect(nil, True);
    Invalidate;
  end;
  {$ENDIF}
  if FCaptureMouseItem=nil then begin
    State := State - [rvstIgnoreNextMouseDown];
    if (rvstMakingSelection in State) and (Button = mbLeft) then begin
      ys := y;
      if not (rvflMouseXYAlwaysCorrect in Flags) then
        if ys<0 then
          ys := 0
        else if ys>Height then
          ys := Height;
      xs := x;
      if not (rvflMouseXYAlwaysCorrect in Flags) then
        if xs<0 then
          xs := 0
        else if xs>Width then
          xs := Width;
      if rvstLineSelection in State then
        AdjustLineSelection(xs+HOffs, ys+VOffs)
      else
        AdjustSelectionByMode(xs+HOffs, ys+VOffs);
      State := State - [rvstMakingSelection, rvstLineSelection];
      FSelectingInfo.Free;
      FSelectingInfo := nil;
      SearchHotItem(X,Y, HOffs,VOffs);      
      AdjustMouseUpSelection;      
      DoOnSelection(True);
      Invalidate;
      DoSelect;
    end;
    if (rvoRClickDeselects in Options) and (Button = mbRight) then begin
      GetSelBounds(ASelStartNo, ASelEndNo, ASelStartOffs, ASelEndOffs, True);
      FindDrawItemForSel(X+HOffs, Y+VOffs, ItemNo, ItemOffs, False);
      if (ItemNo<>-1) and
         (((ItemNo<ASelStartNo) or ((ItemNo=ASelStartNo) and (ItemOffs<ASelStartOffs))) or
         ((ItemNo>ASelEndNo)    or ((ItemNo=ASelEndNo)   and (ItemOffs>ASelEndOffs)))) then begin
        FSelStartNo   := ItemNo;
        FSelStartOffs := ItemOffs;
        FSelEndNo   := ItemNo;
        FSelEndOffs := ItemOffs;
        DoOnSelection(True);
        Refresh;
        DoSelect;
      end;
    end;
  end;

  SearchHotItem(X,Y, HOffs,VOffs);
  if LastDIMovedAbove<>-1 then begin
    ItemNo := DrawItems[LastDIMovedAbove].ItemNo;
    with DrawItems[LastDIMovedAbove] do
      GetItem(ItemNo).MouseUp(Button, Shift, X-(Left-HOffs),Y-(Top-VOffs),
        ItemNo, Self);
    end
  else
    ItemNo := -1;
  if FCaptureMouseItem<>nil then exit;
  if not DoNotJump and
     (Button in [mbLeft, mbRight]) and
     (ItemNo<>-1) and
     GetItem(ItemNo).GetBoolValueEx(rvbpAllowsFocus, GetRVStyle) and
     (GetRootData is TRVControlData) and
     (TRVControlData(GetRootData).TabNavigation<>rvtnNone)
   then begin
     TRVControlData(GetRootData).ClearFocus;
     AdjustFocus(ItemNo, Self, ItemNo);
  end;
  if IsAssignedRVMouseUp then
    DoRVMouseUp(Button, Shift, ItemNo, X, Y);
  if (Button = mbRight) and IsAssignedRVRightClick and
     FindWordAt(ClickedWord, X,Y, StyleNo,no, Dummy, RVData) then begin
    p := ClientToScreen(Point(X,Y));
    DoRVRightClick(ClickedWord, StyleNo, p.X, p.Y);
  end;
  if Button <> mbLeft then exit;
  if (LastJumpDowned=-1) or not IsAssignedJump or DoNotJump then
    exit;
  for i:=0 to Jumps.Count-1 do
    with TRVJumpInfo(Jumps[i]) do
      if (LastJumpDowned=id) and
         (X>=l-HOffs) and (X<=l+w-HOffs) and
         (Y>=t-VOffs) and (Y<=t+h-VOffs) then begin
        LastJumpDowned:=-1;
        DoJump(id+FirstJumpNo);
        exit;
      end;
  LastJumpDowned:=-1;
  FClickedDrawItemNo :=-1;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.DblClick;
var
  StyleNo, ItemNo, Offs: Integer;
  ClickedWord: String;
  p: TPoint;
  RVData: TCustomRVFormattedData;
begin
  if (not (rvoSingleClick  in Options) and IsAssignedRVDblClick) or
     (rvoDblClickSelectsWord in Options) then begin
    GetCursorPos(p);
    p := ScreenToClient(p);
    //ZoomInt(p.X);
    //ZoomInt(p.Y);
    if (rvoDblClickSelectsWord in Options) and (rvoAllowSelection in Options) then begin
      if FindWordAt(ClickedWord, p.X, p.Y, StyleNo, ItemNo, Offs, RVData) then begin
        if StyleNo>=0 then
          RVData.SetSelectionBounds(ItemNo, Offs, ItemNo,
            Offs+RVU_Length(clickedword,RVData.GetItemOptions(ItemNo)))
        else
          RVData.SetSelectionBounds(ItemNo, 0, ItemNo, 1);
        RVData.State := RVData.State + [rvstIgnoreNextMouseDown];
        RVData.State := RVData.State - [rvstMakingSelection];
        Invalidate;
        State := State - [rvstMakingSelection];
        DoRVDblClick(ClickedWord, StyleNo);
        State := State + [rvstIgnoreNextMouseDown];
      end;
      exit;
    end;
    if IsAssignedRVDblClick and
       FindWordAt(ClickedWord, p.X, p.Y, StyleNo, ItemNo, Offs, RVData) then
       DoRVDblClick(ClickedWord, StyleNo);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.DeleteItems(FirstItemNo, Count: Integer);
begin
  Deselect(nil, True);
  inherited DeleteItems(FirstItemNo, Count);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.DeleteParas(FirstItemNo, LastItemNo: Integer);
var FirstItemNo2, LastItemNo2,
    FirstDItemNo, LastDItemNo, tmp: Integer;
begin
  ExpandToPara(FirstItemNo, LastItemNo, FirstItemNo, LastItemNo);
  FirstItemNo2 := FirstItemNo;
  LastItemNo2  := LastItemNo;
  if FirstItemNo2>0 then
    dec(FirstItemNo2);
  if LastItemNo2<Items.Count-1 then
    inc(LastItemNo2);
  ExpandToPara(FirstItemNo2, LastItemNo2, FirstItemNo2, LastItemNo2);
  Item2FirstDrawItem(FirstItemNo2, FirstDItemNo);
  Item2FirstDrawItem(LastItemNo2, LastDItemNo);
  DeleteItems(FirstItemNo, LastItemNo-FirstItemNo+1);
  FormatParasExact(FirstDItemNo,LastDItemNo, - (LastItemNo-FirstItemNo+1), False);
  ClearJumps;  
  tmp := 0;
  BuildJumpsCoords(tmp, jumps);
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.SelectionExists(AllowReset: Boolean;UsePartialSelected: Boolean): Boolean;
var StartNo, EndNo, StartOffs, EndOffs: Integer;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    Result := TCustomRVFormattedData(GetChosenRVData).SelectionExists(AllowReset, UsePartialSelected);
    exit;
  end;
  {$ENDIF}
  if rvstMakingSelection in State then
    AllowReset := False;
  if UsePartialSelected and
     (FPartialSelectedItem<>nil) and
     FPartialSelectedItem.PartiallySelected then begin
    Result := True;
    exit;
  end;
  GetSelBounds(StartNo, EndNo, StartOffs, EndOffs, True);
  if (StartNo <> -1) and (StartNo=EndNo) and (StartOffs=EndOffs) then begin
    if AllowReset then
      Deselect(FPartialSelectedItem, True);
    Result := False;
    exit;
  end;
  Result :=  (StartNo<>-1) and (EndNo<>-1);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.SetCursor(Cursor: TCursor);
begin
  GetParentControl.Cursor := Cursor;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.ClientToScreen(const p: TPoint): TPoint;
begin
  Result := GetParentControl.ClientToScreen(p);
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.ScreenToClient(const p: TPoint): TPoint;
begin
  Result := GetParentControl.ScreenToClient(p);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.GetSelStart(var DINo, DIOffs: Integer);
begin
  SelectionExists(True, False);
  DINo   := FSelStartNo;
  DIOffs := FSelStartOffs;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetSelectedImage: TGraphic;
var StartNo, EndNo, StartOffs, EndOffs,Index: Integer;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    Result := TCustomRVFormattedData(GetChosenRVData).GetSelectedImage;
    exit;
  end;
  {$ENDIF}
  Result := nil;
  StoreSelBounds(StartNo, EndNo, StartOffs, EndOffs, True);
  if (StartNo=-1) then exit;
  case EndNo-StartNo of
    0:
      begin
        if (StartOffs>0) or (EndOffs<1) then exit;
        Index := StartNo;
      end;
    1:
      begin
        if StartOffs>GetOffsBeforeItem(StartNo) then begin
          if StartOffs<GetOffsAfterItem(StartNo) then exit;
          if EndOffs<GetOffsAfterItem(EndNo) then exit;
          Index := StartNo+1
          end
        else if EndOffs<GetOffsAfterItem(EndNo) then begin
          if EndOffs>GetOffsBeforeItem(EndNo) then exit;
          if StartOffs>GetOffsBeforeItem(StartNo) then exit;
          Index := StartNo
          end
        else
          exit;
      end;
    2:
      begin
        if StartOffs<GetOffsAfterItem(StartNo) then exit;
        if EndOffs>GetOffsBeforeItem(EndNo) then exit;
        Index := StartNo+1;
      end;
    else
      exit;
  end;
  Result := TCustomRVItemInfo(Items.Objects[Index]).AsImage;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetSelText(Unicode: Boolean): String;
var Stream: TMemoryStream;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    Result := TCustomRVFormattedData(GetChosenRVData).GetSelText(Unicode);
    exit;
  end;
  {$ENDIF}
  if not SelectionExists(False, True) then begin
    Result := '';
    exit;
  end;
  Stream := TMemoryStream.Create;
  try
    if SaveTextToStream('', Stream, 80, True, True, Unicode, False) then begin
      SetLength(Result, Stream.Size);
      Stream.Position := 0;
      Stream.ReadBuffer(PChar(Result)^, Stream.Size);
      end
    else
      Result := '';
  finally
    Stream.Free;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.SrchSelectIt(strt, offs, len: Integer;
                                              Invert: Boolean);
begin
  DeselectPartiallySelectedItem(nil);
  if not Invert then
    RestoreSelBounds(strt,strt,offs,offs+len)
  else
    RestoreSelBounds(strt,strt,offs+len,offs);
  with DrawItems[FSelStartNo] do
    ShowRectangle(Left,Top,Width,Height);
  Invalidate;
  DoOnSelection(True);
  DoSelect;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.SrchStart(Down: Boolean; var strt, offs: Integer);
var  sl,so: Integer;
begin
  if SelectionExists(True, False) then begin
    StoreSelBounds(sl, strt, so, offs, False);
    end
  else begin
    if Down then begin
      strt := GetFirstItemVisible;
      offs := GetOffsBeforeItem(0);
      end
    else begin
      strt := GetLastItemVisible;
      offs := GetOffsAfterItem(strt)+1;
    end;
  end;
  if strt<0 then strt := 0;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.SearchText(Down, MatchCase, WholeWord: Boolean; s:String):Boolean;
    {....................................................}
    function FwdPos(const substr, astr: String): Integer;
    var offs,r,roffs: Integer;
        str: String;
    begin
      str := astr;
      Result := 0;
      offs := 0;
      r := Pos(substr, str);
      if WholeWord then begin
        while r <> 0 do begin
          roffs := offs+r;
          if ((roffs = 1) or ((roffs > 1) and IsDelimiter(astr, roffs-1, []))) and
             (((roffs+Length(substr)) > Length(astr)) or
              (((roffs+Length(substr))<= Length(astr)) and IsDelimiter(astr, roffs+Length(substr),[])))
              then begin
            Result := roffs;
            exit;
          end;
          str := System.Copy(str,r+Length(substr),Length(str));
          inc(offs, r+Length(substr)-1);
          r := Pos(substr, str);
        end;
        end
      else
        Result := r;
    end;
    {....................................................}
    function RevPos(substr, str: String): Integer;
    var offs,r: Integer;
    begin
      Result := 0;
      offs   := 0;
      while Length(str)<>0 do begin
        r := FwdPos(Substr, str);
        if r=0 then exit;
        Result := offs + r;
        str := System.Copy(str,r+Length(substr),Length(str));
        inc(offs, r+Length(substr)-1);
      end;
    end;
    {....................................................}
    function SearchInItem(item: TCustomRVItemInfo; StoreSub: TRVStoreSubRVData;
                          SubRVData: TCustomRVFormattedData): Boolean;
    var SubPos: TRVSubRVDataPos;
    begin
      if Down then
        SubPos := rvdNext
      else
        SubPos := rvdPrev;
      Result := TCustomRVFormattedData(SubRVData.GetRVData).SearchText(Down, MatchCase, WholeWord, s);
      if not Result then
        repeat
          SubRVData := TCustomRVFormattedData(item.GetSubRVData(StoreSub,SubPos));
          if SubRVData=nil then
            break;
          Result := TCustomRVFormattedData(SubRVData.GetRVData).SearchText(Down, MatchCase, WholeWord, s);
        until Result;
      if Result then
        item.ChooseSubRVData(StoreSub);
      StoreSub.Free;
    end;
    {....................................................}
    procedure UpdateUnicodePos(const editorstring, soughtstring: String;
                               ItemOptions: TRVItemOptions;
                               var Offs, Len: Integer);
    {$IFNDEF RVDONOTUSEUNICODE}
    var s: String;
    {$ENDIF}
    begin
      {$IFNDEF RVDONOTUSEUNICODE}
      if not (rvioUnicode in ItemOptions) then
        exit;
      s := RVU_AnsiToUnicode(GetDefaultCodePage, soughtstring);
      Len := Length(s) div 2;
      s := System.Copy(editorstring, 1, Offs-1);
      s := RVU_AnsiToUnicode(GetDefaultCodePage, s);
      Offs := Length(s) div 2+1;
      {$ENDIF}
    end;
    {....................................................}
var strt, offs, len, i, so: Integer;
    s2: String;
    ItemOptions: TRVItemOptions;
    item: TCustomRVItemInfo;
    StoreSub: TRVStoreSubRVData;
    SubPos: TRVSubRVDataPos;
    SubRVData: TCustomRVFormattedData;
begin
  Result := False;
  if not MatchCase then
    s := AnsiUpperCase(s);
  if s='' then exit;
  if Items.Count=0 then exit;
  StoreSub := nil;
  item := GetChosenItem;
  if item=nil then
    item := PartialSelectedItem;
  if item<>nil then begin
    if Down then
      SubPos := rvdChosenDown
    else
      SubPos := rvdChosenUp;
    SubRVData := TCustomRVFormattedData(item.GetSubRVData(StoreSub, SubPos));
    if SubRVData<>nil then begin
      Result := SearchInItem(item, StoreSub, SubRVData);
      if Result then
        exit;
    end;
  end;
  if Down then
    SubPos := rvdFirst
  else
    SubPos := rvdLast;
  SrchStart(Down, strt, offs);
  if (strt>=Items.Count) or (strt<0) then exit;
  if Items.Objects[strt]=item then begin
    if Down then
      inc(strt)
    else
      dec(strt);
    if (strt>=Items.Count) or (strt<0) then exit;
    end
  else if GetItemStyle(strt)>=0 then begin
    ItemOptions := GetItemOptions(strt);
    if not Down then begin
      s2 := RVU_Copy(Items[strt], 1, offs-1, ItemOptions);
      offs := 1;
      end
    else
      s2 := RVU_Copy(Items[strt],offs,RVU_Length(Items[strt],ItemOptions), ItemOptions);
    {$IFNDEF RVDONOTUSEUNICODE}
    if rvioUnicode in ItemOptions then
      s2 := RVU_UnicodeToAnsi(GetDefaultCodePage, s2);
    {$ENDIF}
    if not MatchCase then
      s2 := AnsiUpperCase(s2);
    if not Down then
      so := RevPos(s,s2)
    else
      so := FwdPos(s,s2);
    if so<>0 then begin
      Result := True;
      len := Length(s);
      UpdateUnicodePos(s2, s, ItemOptions, so, len);
      SrchSelectIt(strt, offs+so-1, len, not Down);
      exit;
    end;
    if Down then
      inc(strt)
    else
      dec(strt);
  end;
  if not Down then
    for i := strt downto 0 do begin
      if GetItemStyle(i)>=0 then begin
        {$IFNDEF RVDONOTUSEUNICODE}
         if rvioUnicode in GetItemOptions(i) then
           s2 := RVU_UnicodeToAnsi(GetDefaultCodePage, Items[i])
         else
        {$ENDIF}
           s2 := Items[i];
        if not MatchCase then
          offs := RevPos(s,AnsiUpperCase(s2))
        else
          offs := RevPos(s, s2);
        if offs<>0 then begin
          Result := True;
          len := Length(s);
          UpdateUnicodePos(s2, s, GetItemOptions(i), offs, len);
          SrchSelectIt(i, offs, len, not Down);
          exit;
        end;
        end
      else begin
        item := TCustomRVItemInfo(Items.Objects[i]);
        SubRVData := TCustomRVFormattedData(item.GetSubRVData(StoreSub, SubPos));
        if SubRVData<>nil then begin
          Result := SearchInItem(item, StoreSub, SubRVData);
          if Result then
           exit;
        end;
      end;
    end
  else
    for i := strt to Items.Count-1 do
      if GetItemStyle(i)>=0 then begin
         {$IFNDEF RVDONOTUSEUNICODE}
         if rvioUnicode in GetItemOptions(i) then
           s2 := RVU_UnicodeToAnsi(GetDefaultCodePage, Items[i])
         else
        {$ENDIF}
           s2 := Items[i];
        if not MatchCase then
          offs := FwdPos(s,AnsiUpperCase(s2))
        else
          offs := FwdPos(s, s2);
        if offs<>0 then begin
          Result := True;
          len := Length(s);
          UpdateUnicodePos(s2, s, GetItemOptions(i), offs, len);
          SrchSelectIt(i, offs, len, not Down);
          exit;
        end;
        end
      else begin
        item := GetItem(i);
        SubRVData := TCustomRVFormattedData(item.GetSubRVData(StoreSub, SubPos));
        if SubRVData<>nil then begin
          Result := SearchInItem(item, StoreSub, SubRVData);
          if Result then
           exit;
        end;
      end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.GetSelectionBounds(var StartItemNo, StartItemOffs,
                                 EndItemNo, EndItemOffs: Integer;
                                 Normalize: Boolean);
begin
  StoreSelBounds(StartItemNo, EndItemNo, StartItemOffs, EndItemOffs, Normalize);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.GetSelectionBoundsEx(var StartItemNo,
  StartItemOffs, EndItemNo, EndItemOffs: Integer; Normalize: Boolean);
begin
  GetSelectionBounds(StartItemNo, StartItemOffs, EndItemNo, EndItemOffs, Normalize);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.AdjustSelection;
begin
  if (FSelStartNo<=0) or (FSelEndNo<=0) then
    exit;
  if FSelStartOffs<GetOffsBeforeDrawItem(FSelStartNo) then
    FSelStartOffs := GetOffsBeforeDrawItem(FSelStartNo);
  if FSelStartOffs>GetOffsAfterDrawItem(FSelStartNo) then
    FSelStartOffs := GetOffsAfterDrawItem(FSelStartNo);
  if FSelEndOffs<GetOffsBeforeDrawItem(FSelEndNo) then
    FSelEndOffs := GetOffsBeforeDrawItem(FSelEndNo);
  if FSelEndOffs>GetOffsAfterDrawItem(FSelEndNo) then
    FSelEndOffs := GetOffsAfterDrawItem(FSelEndNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.SetSelectionBounds(StartItemNo, StartItemOffs,
                                 EndItemNo, EndItemOffs: Integer);
begin
  DeselectPartiallySelectedItem(nil);
  DestroyInplaceEditor;
  {$IFNDEF RVDONOTUSELISTS}
  if (StartItemNo>=0) and (EndItemNo>=0) then begin
    if (StartItemNo+1<Items.Count) and (GetItemStyle(StartItemNo)=rvsListMarker) then begin
      inc(StartItemNo);
      StartItemOffs := GetOffsBeforeItem(StartItemNo);
    end;
    if (EndItemNo+1<Items.Count) and (GetItemStyle(EndItemNo)=rvsListMarker) then begin
      inc(EndItemNo);
      EndItemOffs := GetOffsBeforeItem(EndItemNo);
    end;
  end;
  {$ENDIF}
  RestoreSelBounds(StartItemNo, EndItemNo, StartItemOffs, EndItemOffs);
  AdjustSelection;  
//  if GetRVStyle.SelectionMode=rvsmParagraph then
//    ExpandSelectionToParagraph(True);
  DoOnSelection(True);
  DoSelect;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.GetWordAt(X,Y: Integer;
  var RVData: TCustomRVFormattedData; var ItemNo: Integer;
  var Word: String);
var StyleNo, Offs: Integer;
begin
  if not FindWordAt(Word, X,Y, StyleNo, ItemNo, Offs, RVData) then
    ItemNo := -1;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetClientSelectionRect: TRect;
var i: Integer;
    StartNo, EndNo, StartOffs, EndOffs, x,y: Integer;
    R: TRect;
begin
  // This is very rough function for returning selection rectangle
  // It does not count offsets in text items.
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    Result := TCustomRVFormattedData(GetChosenRVData).GetClientSelectionRect;
    exit;
  end;
  {$ENDIF}
  if SelectionExists(False, False) then begin
    ResetSubCoords;
    GetSelBounds(StartNo, EndNo, StartOffs, EndOffs, True);
    Result := Rect(0,0,0,0);
    for i:= StartNo to EndNo do begin
      with DrawItems[i] do
        R := Bounds(Left-GetZHOffs,Top-GetZVOffs, Width, Height);
      UnionRect(Result, Result, R);
    end;
    R := Rect(0,0,GetWidth,GetHeight);
    IntersectRect(Result, Result, R);
    GetOriginEx(x,y);
    OffsetRect(Result, x,y);
    end
  else begin
    GetOriginEx(x,y);
    Result := Bounds(x,y,GetWidth,GetHeight);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.OnTimerScroll;
begin
  //LastSelectionRect.Left := 1;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetSelectionRect:TRect;
var P: TPoint;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetInplaceEditor<>nil) and (GetInplaceEditor is TCustomRichView) then begin
    Result := TCustomRichView(GetInplaceEditor).RVData.GetSelectionRect;
    exit;
  end;
  {$ENDIF}
  Result := GetClientSelectionRect;
  P := Point(0,0);
  P := ClientToScreen(P);
  OffsetRect(Result, P.x, P.y);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.SelectWordAt(X,Y: Integer);
var StyleNo, ItemNo, Offs: Integer;
    Word: String;
    RVData: TCustomRVFormattedData;
begin
  if not (rvoAllowSelection in Options) then exit;
  if FindWordAt(Word, X, Y, StyleNo, ItemNo, Offs, RVData) then begin
    if StyleNo>=0 then
      RVData.SetSelectionBounds(ItemNo, Offs, ItemNo, Offs+RVU_Length(Word, GetItemOptions(ItemNo)))
    else
      RVData.SetSelectionBounds(ItemNo, 0, ItemNo, 1);
    Invalidate;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.GetCheckpointXY(CheckpointData: TCheckpointData;
                                                 var X,Y: Integer);
begin
  if CheckpointData = nil then
    raise ERichViewError.Create(errRVNil);
  if CheckpointData = NotAddedCP then begin
    X := 0;
    Y := DocumentHeight;
    end
  else
    with TRVCPInfo(CheckpointData) do begin
      if (ItemInfo=nil) or (ItemInfo.DrawItemNo=-1) then
        raise ERichViewError.Create(errRVNotFormatted);
      X := DrawItems[ItemInfo.DrawItemNo].Left;
      Y := DrawItems[ItemInfo.DrawItemNo].Top;
    end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetCheckpointYEx(CheckpointData: TCheckpointData): Integer;
begin
  if CheckpointData = nil then
    raise ERichViewError.Create(errRVNil);
  if CheckpointData = NotAddedCP then
    Result := DocumentHeight
  else
    with TRVCPInfo(CheckpointData) do begin
      if (ItemInfo=nil) or (ItemInfo.DrawItemNo=-1) then
        raise ERichViewError.Create(errRVNotFormatted);
      Result := DrawItems[ItemInfo.DrawItemNo].Top;
    end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetJumpPointY(id: Integer): Integer;
var i: Integer;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    Result := TCustomRVFormattedData(GetChosenRVData).GetJumpPointY(id);
    if Result>0 then
      exit;
  end;
  {$ENDIF}
  Result := 0;
  for i := 0 to Jumps.Count-1 do
   if  TRVJumpInfo(Jumps[i]).id = id-FirstJumpNo then begin
     Result := TRVJumpInfo(Jumps[i]).t;
     exit;
   end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetJumpPointItemNo(id: Integer): Integer;
var i: Integer;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    Result := TCustomRVFormattedData(GetChosenRVData).GetJumpPointItemNo(id);
    if Result>=0 then
      exit;
  end;
  {$ENDIF}
  Result := -1;
  for i :=0 to Jumps.Count-1 do
    if TRVJumpInfo(Jumps[i]).id = id-FirstJumpNo then begin
      Result := TRVJumpInfo(Jumps[i]).RVData.DrawItems[TRVJumpInfo(Jumps[i]).DrawItemNo].ItemNo;
      exit;
    end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.GetJumpPointLocation(id: Integer;
  var RVData: TCustomRVFormattedData; var ItemNo: Integer);
var i: Integer;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    TCustomRVFormattedData(GetChosenRVData).GetJumpPointLocation(id,RVData,ItemNo);
    if ItemNo>=0 then
      exit;
  end;
  {$ENDIF}
  ItemNo := -1;
  RVData := nil;
  for i :=0 to Jumps.Count-1 do
    if TRVJumpInfo(Jumps[i]).id = id-FirstJumpNo then begin
      RVData := TRVJumpInfo(Jumps[i]).RVData;
      ItemNo := RVData.DrawItems[TRVJumpInfo(Jumps[i]).DrawItemNo].ItemNo;
      exit;
    end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.CopyText_;
begin
  if SelectionExists(True, True) then begin
    Clipboard.SetTextBuf(PChar(GetSelText(False)));
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.CopyTextW_;
var mem: Cardinal;
    ptr: Pointer;
    s: String;
    zero: Word;
begin
  if SelectionExists(True, True) then begin
    s := GetSelText(True);
    mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, Length(s)+2);
    if mem=0 then raise ERichViewError.Create(errRVNoMemory);
    ptr := GlobalLock(mem);
    Move(PChar(s)^,ptr^, Length(s));
    zero := 0;
    Move(zero,(PChar(ptr)+Length(s))^, 2);
    GlobalUnlock(mem);
    Clipboard.SetAsHandle(CF_UNICODETEXT ,mem);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.CopyImage_;
var gr: TGraphic;
begin
  gr := GetSelectedImage;
  if gr<>nil then
    try
      Clipboard.Assign(gr);
    except;
    end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.Copy(Color: TColor; Background: TRVBackground);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    TCustomRVFormattedData(GetChosenRVData).Copy(clNone, nil);
    exit;
  end;
  {$ENDIF}
  if SelectionExists(True, True) then begin
    Clipboard.Clear;
    Clipboard.Open;
    try
      DoCopy;
      {$IFNDEF RVDONOTUSERVF}
      CopyRVF_(Color, Background);
      {$ENDIF}
      CopyText_;
      CopyImage_;
      {$IFNDEF RVDONOTUSEUNICODE}
      CopyTextW_;
      {$ENDIF}
      {$IFNDEF RVDONOTUSERTF}
      CopyRTF_(Color, Background);
      {$ENDIF}
    finally
      Clipboard.Close;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.CopyDef(Color: TColor; Background: TRVBackground): Boolean;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    Result := TCustomRVFormattedData(GetChosenRVData).CopyDef(clNone, nil);
    exit;
  end;
  {$ENDIF}
  Result := ((rvoAutoCopyText in Options) or
             {$IFNDEF RVDONOTUSERVF}
             (rvoAutoCopyRVF in Options) or
             {$ENDIF}
             {$IFNDEF RVDONOTUSEUNICODE}
             (rvoAutoCopyUnicodeText in Options) or
             {$ENDIF}
             {$IFNDEF RVDONOTUSERTF}
             (rvoAutoCopyRTF in Options) or
             {$ENDIF}
             (rvoAutoCopyImage in Options) or
             IsAssignedCopy
             );
  if Result and SelectionExists(True, True) then begin
    Clipboard.Clear;
    Clipboard.Open;
    DoCopy;
    {$IFNDEF RVDONOTUSERVF}
    if (rvoAutoCopyRVF in Options) then CopyRVF_(Color, Background);
    {$ENDIF}
    if (rvoAutoCopyText in Options) and not (RVNT and (rvoAutoCopyUnicodeText in Options)) then CopyText_;
    if (rvoAutoCopyImage in Options) then CopyImage_;
    {$IFNDEF RVDONOTUSEUNICODE}
    if (rvoAutoCopyUnicodeText in Options) then CopyTextW_;
    {$ENDIF}
    {$IFNDEF RVDONOTUSERTF}
    if (rvoAutoCopyRTF in Options) then CopyRTF_(Color, Background);
    {$ENDIF}
    Clipboard.Close;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.CopyImage;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    TCustomRVFormattedData(GetChosenRVData).CopyImage;
    exit;
  end;
  {$ENDIF}
  if SelectionExists(True, True) then begin
    Clipboard.Clear;
    CopyImage_;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.CopyText;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    TCustomRVFormattedData(GetChosenRVData).CopyText;
    exit;
  end;
  {$ENDIF}
  if SelectionExists(True, True) then begin
    Clipboard.Clear;
    CopyText_;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.CopyTextW;
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    TCustomRVFormattedData(GetChosenRVData).CopyTextW;
    exit;
  end;
  {$ENDIF}
  if SelectionExists(True, True) then begin
    Clipboard.Clear;
    CopyTextW_;
  end;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERTF}
procedure TCustomRVFormattedData.CopyRTF(Color: TColor; Background: TRVBackground);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    TCustomRVFormattedData(GetChosenRVData).CopyRTF(clNone, nil);
    exit;
  end;
  {$ENDIF}
  if SelectionExists(True, True) then begin
    Clipboard.Clear;
    CopyRTF_(Color, Background);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.CopyRTF_(Color: TColor; Background: TRVBackground);
var mem: Cardinal;
    ptr: Pointer;
    Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
     SaveRTFToStream(Stream, True, 0, Color, Background,  nil, nil, nil, nil, nil, 0.0);
     mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, Stream.Size);
     if mem=0 then raise ERichViewError.Create(errRVNoMemory);
     ptr := GlobalLock(mem);
     Move(Stream.Memory^,ptr^,Stream.Size);
     GlobalUnlock(mem);
  finally
    Stream.Free;
  end;
  Clipboard.SetAsHandle(CFRV_RTF, mem);
end;
{$ENDIF}
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERVF}
procedure TCustomRVFormattedData.RVFGetLimits(SaveScope: TRVFSaveScope;
                             var StartItem, EndItem, StartOffs, EndOffs: Integer;
                             var StartPart, EndPart: TRVMultiDrawItemPart);
begin
  if SaveScope=rvfss_Selection then begin
    StartPart := nil;
    EndPart   := nil;
    StoreSelBounds(StartItem, EndItem, StartOffs, EndOffs, True);
    end
  else
    inherited RVFGetLimits(SaveScope, StartItem, EndItem, StartOffs, EndOffs, StartPart, EndPart);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.CopyRVF_(Color: TColor; Background: TRVBackground);
var mem: Cardinal;
    ptr: Pointer;
    Size: Integer;
    Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
     Size := 0;
     Stream.WriteBuffer(Size, SizeOf(Size));
     SaveRVFToStream(Stream, True, Color, Background, nil);
     Size := Stream.Position-SizeOf(Size);
     Stream.Position := 0;
     Stream.WriteBuffer(Size, SizeOf(Size));
     mem := GlobalAlloc(GMEM_MOVEABLE or GMEM_DDESHARE, Stream.Size);
     if mem=0 then raise ERichViewError.Create(errRVNoMemory);
     ptr := GlobalLock(mem);
     Move(Stream.Memory^,ptr^,Stream.Size);
     GlobalUnlock(mem);
  finally
    Stream.Free;
  end;
  Clipboard.SetAsHandle(CFRV_RVF,mem);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.CopyRVF(Color: TColor; Background: TRVBackground);
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    TCustomRVFormattedData(GetChosenRVData).CopyRVF(clNone, nil);
    exit;
  end;
  {$ENDIF}
  if SelectionExists(True,True) then begin
    Clipboard.Clear;
    CopyRVF_(Color, Background);
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.AfterVScroll;
var CP: TRVCPInfo;
    y, ymin, ymax: Integer;
    {..........................................................}
    function FindCPBefore(EndCP: TRVCPInfo): TRVCPInfo;
    begin
      Result := EndCP;
      while (Result<>nil) and (not Result.RaiseEvent) do
        Result := Result.Prev;
    end;
    {..........................................................}
begin
  if not IsAssignedCheckpointVisible or (GetCPEventKind=cpeNone) then
    exit;
  ymin := GetVOffs;
  ymax := ymin+GetHeight;
  CP := FirstCP;
  case GetCPEventKind  of
    cpeWhenVisible: // raise event for first visible cp
      begin
        while CP<>nil do begin
          if CP.RaiseEvent then begin
            if CP.ItemInfo.DrawItemNo=-1 then exit; // not formatted
            y := DrawItems[CP.ItemInfo.DrawItemNo].Top;
            if y>=ymin then begin
              if y>ymax then begin
                if LastRaisedCP<>nil then begin
                  LastRaisedCP := nil;
                  DoCheckpointVisible(nil);
                end;
                exit;
              end;
              if CP=LastRaisedCP then exit;
              LastRaisedCP := CP;
              DoCheckpointVisible(CP);
              exit;
            end;
          end;
          CP := CP.Next;
        end;
        if (NotAddedCP<>nil) and (NotAddedCP.RaiseEvent) and (NotAddedCP<>LastRaisedCP) and
           (DocumentHeight<ymax) then begin
          LastRaisedCP := NotAddedCP;
          DoCheckpointVisible(NotAddedCP);
          exit;
        end;
        if LastRaisedCP<>nil then begin
          LastRaisedCP := nil;
          DoCheckpointVisible(nil);
        end;
      end;
    cpeAsSectionStart: // raise event for first visible cp
                       // if there is no such cp, raise event for the closest cp
                       // before visible area
      begin
        while CP<>nil do begin
          if CP.RaiseEvent then begin
            if CP.ItemInfo.DrawItemNo=-1 then exit; // not formatted
            y := DrawItems[CP.ItemInfo.DrawItemNo].Top;
            if y>=ymin then begin
              if y>ymax then
                CP := FindCPBefore(CP.Prev);
              if CP<>nil then begin
                if CP=LastRaisedCP then exit;
                LastRaisedCP := CP;
                DoCheckpointVisible(CP);
              end;
              exit;
            end;
          end;
          CP := CP.Next;
        end;
        if (NotAddedCP<>nil) and (NotAddedCP.RaiseEvent) and (DocumentHeight<ymax) then begin
          if NotAddedCP=LastRaisedCP then exit;
          LastRaisedCP := NotAddedCP;
          DoCheckpointVisible(NotAddedCP);
          exit;
        end;
        CP := FindCPBefore(LastCP);
        if CP<>nil then begin
          y := DrawItems[CP.ItemInfo.DrawItemNo].Top;
          if y<=ymin then begin
            if CP=LastRaisedCP then exit;
            LastRaisedCP := CP;
            DoCheckpointVisible(CP);
            exit;
          end;
        end;
      end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.UpdateBackgroundPaletteInfo(Background: TRVBackground);
begin
  State := State + [rvstChangingBkPalette];
  Background.UpdatePaletted(GetDoInPaletteMode, GetRVPalette, GetRVLogPalette);
  State := State - [rvstChangingBkPalette];
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.UpdatingBackgroundPalette: Boolean;
begin
  Result := rvstChangingBkPalette in State;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.StartFormatting;
begin
   State := State + [rvstSkipformatting];
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.EndFormatting;
begin
   State := State - [rvstSkipformatting];
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.GetParaBounds(DINo1, DINo2: Integer; var ParaStart,
  ParaEnd: Integer);
var ItemNo: Integer;
    item: TCustomRVItemInfo;
begin
  { calculating paragraphs bounds }
  ParaStart := DINo1;
  while (ParaStart>0) do begin
    item := GetItem(DrawItems[ParaStart].ItemNo);
    if not item.SameAsPrev and not item.BR and
       (DrawItems[ParaStart-1].ItemNo<>DrawItems[ParaStart].ItemNo) then break;
    dec(ParaStart);
  end;
  { and prev. paragraph...}
  if ParaStart<>0 then dec(ParaStart);
  while (ParaStart>0) do begin
    item := GetItem(DrawItems[ParaStart].ItemNo);
    if not item.SameAsPrev and not item.BR and
       (DrawItems[ParaStart-1].ItemNo<>DrawItems[ParaStart].ItemNo) then break;
    dec(ParaStart);
  end;

  ParaEnd := DINo2;
  ItemNo := DrawItems[ParaEnd].ItemNo;
  while (ParaEnd<DrawItems.Count) and (DrawItems[ParaEnd].ItemNo=ItemNo) do
    inc(ParaEnd);
  while (ParaEnd<DrawItems.Count) and (DrawItems[ParaEnd].ItemNo<Items.Count) do begin
    item := GetItem(DrawItems[ParaEnd].ItemNo);
    if not item.SameAsPrev and not item.BR then
      break;
    inc(ParaEnd);
  end;
  { and next paragraph...}
  if (ParaEnd<DrawItems.Count) then begin
    ItemNo := DrawItems[ParaEnd].ItemNo;
    while (ParaEnd<DrawItems.Count) and
      (DrawItems[ParaEnd].ItemNo=ItemNo) do inc(ParaEnd);
    while (ParaEnd<DrawItems.Count) and (DrawItems[ParaEnd].ItemNo<Items.Count) do begin
      item := GetItem(DrawItems[ParaEnd].ItemNo);
      if not item.SameAsPrev and not item.BR then
        break;
      inc(ParaEnd);
    end;
  end;
  dec(ParaEnd);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.FormatParas(StartDrawItemNo, EndDrawItemNo,
                                             ItemsInserted: Integer);
var First,Last, ItemNo: Integer;
begin
  { calculating paragraphs bounds }
  First := StartDrawItemNo;
  while (First>0) do begin
    if not TCustomRVItemInfo(Items.Objects[DrawItems[First].ItemNo]).SameAsPrev and
       (DrawItems[First-1].ItemNo<>DrawItems[First].ItemNo) then break;
    dec(First);
  end;
  Last := EndDrawItemNo;
  ItemNo := DrawItems[Last].ItemNo;
  if ItemNo>=Items.Count-1 then
    Last := DrawItems.Count-1
  else begin
    while (Last<DrawItems.Count) and
          (DrawItems[Last].ItemNo=ItemNo) do inc(Last);
    if Last<DrawItems.Count then begin
      ItemNo := DrawItems[Last].ItemNo;
      if ItemNo>=Items.Count-1 then
        Last := DrawItems.Count-1
      else begin
        while (Last<DrawItems.Count) and
              (DrawItems[Last].ItemNo+ItemsInserted<Items.Count) and
              TCustomRVItemInfo(Items.Objects[DrawItems[Last].ItemNo+ItemsInserted]).SameAsPrev do
            inc(Last);
        if Last<>DrawItems.Count-1 then dec(Last);
      end;
      end
    else
      Last := DrawItems.Count-1;
  end;
  FormatParasExact(First, Last,ItemsInserted, False);
end;
{-----------------------------------------------------------------------}
procedure TCustomRVFormattedData.FormatParasExact(StartDrawItemNo,EndDrawItemNo,
                                                  ItemsInserted: Integer;
                                                  NoCaching: Boolean);


  function GetParagraphBottom(EndDrawItemNo: Integer):Integer;
  var i: Integer;
  begin
   Result := GetTopMargin;
   i := EndDrawItemNo;
   while (i>=0) do
     with DrawItems[i] do begin
       if Top+Height+ExtraSpaceBelow>Result then
         Result := Top+Height+ExtraSpaceBelow;
       if FromNewLine then
         break;
       dec(i);
     end;
  end;

  function GetParagraphTop(StartDrawItemNo: Integer):Integer;
  var i: Integer;
  begin
   Result := DrawItems[StartDrawItemNo].Top;
   i := StartDrawItemNo+1;
   while (i<DrawItems.Count) and
         not DrawItems[i].FromNewLine do
     with DrawItems[i] do begin
       if Top<Result then
         Result := Top;
       inc(i);
     end;
  end;

  procedure FormatParasExact_(var StartItemNo,EndItemNo: Integer);
  var PrevParBottom, ParBottom, NextParTop: Integer;
    i: Integer;
    x, baseline, prevdesc, prevabove,LineWidth,Indent: Integer;
    sad: TRVScreenAndDevice;
    IsParaStart,DontFSL: Boolean;
    DLC {, CW}: Integer;
    LastTextStyle, VerticalOffset: Integer;
    TextMetric: TTextMetric;
    Canvas: TCanvas;
    item: TCustomRVItemInfo;
    RVStyle: TRVStyle;
    ExtraSpaceBelowLine: Integer;
    RepaintRect: TRect;
    SavedVOffs: Integer;
    FirstParaItemNo: Integer;
  begin
    Canvas := GetCanvas;
    DLC := DrawItems.Count;
    LastTextStyle := -1;
    FirstParaItemNo := -1;
    StartItemNo := DrawItems[StartDrawItemNo].ItemNo;
    EndItemNo   := DrawItems[EndDrawItemNo  ].ItemNo+ItemsInserted;
    AdjustInItemsRange(StartItemNo);
    if EndItemNo<StartItemNo then
      EndItemNo := StartItemNo;
    AdjustInItemsRange(EndItemNo);
    //ExpandToPara(StartItemNo, EndItemNo, StartItemNo, EndItemNo);

    PrevParBottom := GetParagraphBottom(StartDrawItemNo-1);
    if EndDrawItemNo<>DrawItems.Count-1 then
       NextParTop := GetParagraphTop(EndDrawItemNo+1)
    else
       NextParTop := -1;

    RepaintRect.Top := PrevParBottom-10-GetVOffs;
    RepaintRect.Left := 0;
    RepaintRect.Right := GetWidth;       

    x := 0;
    prevdesc := 0;
    prevabove := 0;
    baseline := PrevParBottom;
    RV_InfoAboutSaD(sad, GetCanvas);
    sad.LeftMargin := MulDiv(GetLeftMargin,  sad.ppixDevice, sad.ppixScreen);
    sad.RightMargin := MulDiv(GetRightMargin,  sad.ppixDevice, sad.ppixScreen);
    IsParaStart := True;
    LineWidth   := 0;
    Indent      := 0;
    DrawItems.MarkForDelete(StartDrawItemNo,EndDrawItemNo);

    DontFSL := True;
    RVStyle := GetRVStyle;
    for i:=StartItemNo to EndItemNo do
      with TCustomRVItemInfo(Items.Objects[i]) do begin
        if not SameAsPrev and not BR then
          FirstParaItemNo := i;
        if (StyleNo>=0) then
          if (RVStyle.ParaStyles[ParaNo].Alignment = rvaJustify) then
             FormatWords(i,x,baseline, prevdesc,prevabove, Canvas, sad, IsParaStart, LineWidth, Indent,
                         StartDrawItemNo,VerticalOffset,DontFSL, LastTextStyle,TextMetric, FirstParaItemNo, NoCaching)
          else
             FormatLine(
               {$IFNDEF RVDONOTUSEALLCAPS}
               RV_ReturnCapitalized(Items[i], RVStyle.TextStyles[GetActualStyleNo(RVStyle)]),
               {$ELSE}
               Items[i],
               {$ENDIF}
               0,
               {$IFNDEF RVDONOTUSEUNICODE}
               RVU_Length(Items[i], ItemOptions),
               {$ELSE}
               Length(Items[i]),
               {$ENDIF}
               i,x,baseline, prevdesc,prevabove, Canvas, sad, IsParaStart,
               LineWidth, Indent, StartDrawItemNo,VerticalOffset,DontFSL,
               LastTextStyle,TextMetric, FirstParaItemNo, NoCaching)
        else
          FormatLine('', 0, Length(Items[i]),i,x,baseline, prevdesc,prevabove, Canvas, sad, IsParaStart, LineWidth, Indent,
                    StartDrawItemNo,VerticalOffset,DontFSL, LastTextStyle,TextMetric, FirstParaItemNo, NoCaching);
      end;
    FinishScreenLine(sad, LineWidth+Indent, StartDrawItemNo, True,
      ExtraSpaceBelowLine, DontFSL, Canvas);
    inc(baseline, ExtraSpaceBelowLine);
    DrawItems.DeleteMarked;
    if (EndItemNo=Items.Count-1) or GetItem(EndItemNo+1).CanBeBorderStart then
      inc(prevdesc, GetRVStyle.ParaStyles[GetItemPara(EndItemNo)].SpaceAfter);
    if (EndItemNo<Items.Count-1) and GetItem(EndItemNo+1).CanBeBorderStart then
      inc(prevdesc, GetRVStyle.ParaStyles[GetItemPara(EndItemNo+1)].SpaceBefore);
    RepaintRect.Bottom := baseline+prevdesc+10-GetVOffs;
    if (ItemsInserted<>0) or (DLC<>DrawItems.Count) or
       (NextParTop<>baseline+prevdesc) then begin
      ParBottom := baseline+prevdesc;
      // RepaintRect.Bottom := GetHeight;
      for i:=StartDrawItemNo to DrawItems.Count-1 do
        with DrawItems[i] do begin
          Top := Top-NextParTop+(baseline+prevdesc);
          if Top+Height>ParBottom then
            ParBottom := Top+Height;
          ItemNo := ItemNo + ItemsInserted;
          item := TCustomRVItemInfo(Items.Objects[ItemNo]);
          if GetOffsBeforeItem(ItemNo)=Offs then
            item.DrawItemNo := i;
        end;
      if (EndItemNo<Items.Count-1) then
        inc(ParBottom, GetRVStyle.ParaStyles[GetItemPara(Items.Count-1)].SpaceAfter);
      RepaintRect.Bottom := ParBottom-GetVOffs+10;
      if DocumentHeight-GetBottomMargin+10>RepaintRect.Bottom then
        RepaintRect.Bottom := DocumentHeight-GetBottomMargin+10;
      if RepaintRect.Bottom>GetHeight then
        RepaintRect.Bottom := GetHeight;
      if DocumentHeight <> (ParBottom+GetBottomMargin) then begin
        SavedVOffs := GetVOffs;
        DocumentHeight := ParBottom+GetBottomMargin;
        AdjustVScrollUnits;
        AlreadyFormatted := True;
        SetDocumentAreaSize(GetAreaWidth, DocumentHeight, False);
        AlreadyFormatted := False;
        if SavedVOffs<>GetVOffs then
          RepaintRect := GetParentControl.ClientRect;
      end;
    end;
    AdjustChildrenCoords;
    LastItemFormatted := Items.Count-1;
    InvalidateRect(RepaintRect);
    GetParentControl.Update;
  end;
var StartItemNo,EndItemNo: Integer;
begin
  FormatParasExact_(StartItemNo,EndItemNo);
  Formatted(StartItemNo,EndItemNo,True);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.DoOnSelection(AllowScrolling: Boolean);
begin

end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.XorDrawing;
begin
  if Assigned(FXORDrawing) then begin
    FXORDrawing(Self, [], MouseX, MouseY);
    XorImageDrawn := not XorImageDrawn;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.XorDrawingEx(X, Y: Integer);
begin
  MouseX := X;
  MouseY := Y;
  XorDrawing;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.UnAssignXorDrawing(P: TMouseMoveEvent);
begin
  if @FXORDrawing=@P then begin
    if XorImageDrawn and not (csDestroying in GetParentControl.ComponentState) then
      XorDrawing;
    FXORDrawing := nil;
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.AssignXorDrawing(P: TMouseMoveEvent);
begin
  if XorImageDrawn then
    XorDrawing;
  FXORDrawing := P;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.UsingThisXorDrawing(P: TMouseMoveEvent): Boolean;
begin
  Result := @FXORDrawing=@P;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.ClearXorDrawing: Boolean;
begin
  Result := XorImageDrawn;
  if XorImageDrawn then
    XorDrawing;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.InternalFreeItem(item: TCustomRVItemInfo;
                                                  Clearing: Boolean);
begin
  if item=FCaptureMouseItem then
    FCaptureMouseItem := nil;
//  if item=GetChosenItem then
//    UnassignChosenRVData(GetChosenRVData);
  inherited InternalFreeItem(item, Clearing);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.ReleaseMouseCapture(Item: TCustomRVItemInfo);
begin
  if Item=FCaptureMouseItem then
    FCaptureMouseItem := nil;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.SetMouseCapture(Item: TCustomRVItemInfo;
                                                 var Left,Top: Integer);
begin
  if (LastDIMovedAbove<0) or (LastDIMovedAbove>=DrawItems.Count) or
     (Item <> Items.Objects[DrawItems[LastDIMovedAbove].ItemNo]) then
    Item2FirstDrawItem(Items.IndexOfObject(Item),LastDIMovedAbove);
  Left := DrawItems[LastDIMovedAbove].Left;
  Top := DrawItems[LastDIMovedAbove].Top;
  FCaptureMouseItem := Item;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetCPEventKind: TCPEventKind;
begin
  Result := TCustomRVFormattedData(GetRootData).GetCPEventKind;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.DoSelect;
begin
  TCustomRVFormattedData(GetRootData).DoSelect;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.IsAssignedRVDblClick: Boolean;
begin
   Result := TCustomRVFormattedData(GetRootData).IsAssignedRVDblClick;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.DoRVDblClick(const ClickedWord: String;
  StyleNo: Integer);
begin
  TCustomRVFormattedData(GetRootData).DoRVDblClick(ClickedWord, StyleNo);
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.IsAssignedRVRightClick: Boolean;
begin
  Result := TCustomRVFormattedData(GetRootData).IsAssignedRVRightClick;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.DoRVRightClick(const ClickedWord: String;
  StyleNo, X, Y: Integer);
begin
  TCustomRVFormattedData(GetRootData).DoRVRightClick(ClickedWord, StyleNo, X,Y);
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetCanvas: TCanvas;
begin
  Result := TCustomRVFormattedData(GetRootData).GetCanvas;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.DeselectPartiallySelectedItem(NewPartiallySelected: TCustomRVItemInfo);
begin
  if (FPartialSelectedItem<>nil) and (FPartialSelectedItem<>NewPartiallySelected) then begin
    //if not (csDestroying in GetParentControl.ComponentState) then
      FPartialSelectedItem.DeselectPartial;
    FPartialSelectedItem := nil;
  end;
  if (GetChosenItem<>nil) and (GetChosenItem<>NewPartiallySelected) then begin
    if not (csDestroying in GetParentControl.ComponentState) then
      UnassignChosenRVData(GetChosenRVData);
  end;  
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.SetPartialSelectedItem(Item: TCustomRVItemInfo);
begin
  if FPartialSelectedItem<>Item then begin
    Deselect(Item, True);
    FPartialSelectedItem := Item;
    DoSelect;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetItemCoords(ItemNo: Integer; var Left, Top: Integer): Boolean;
begin
  Item2FirstDrawItem(ItemNo, ItemNo);
  Result := ItemNo<>-1;
  if Result then begin
    Left := DrawItems[ItemNo].Left;
    Top := DrawItems[ItemNo].Top;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetItemClientCoords(ItemNo: Integer; var Left, Top: Integer): Boolean;
begin
  Result := GetItemCoords(ItemNo, Left, Top);
  if Result then begin
    dec(Left, GetHOffs);
    dec(Top, GetVOffs);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.DoCopy;
var root: TCustomRVData;
begin
  root := GetRootData;
  if root is TCustomRVFormattedData then
    TCustomRVFormattedData(GetRootData).DoCopy;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.IsAssignedCopy: Boolean;
var root: TCustomRVData;
begin
  root := GetRootData;
  if root is TCustomRVFormattedData then
    Result := TCustomRVFormattedData(GetRootData).IsAssignedCopy
  else
    Result := False;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.AssignChosenRVData(RVData: TCustomRVFormattedData;
                                                    Item: TCustomRVItemInfo);
begin

end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.SilentReplaceChosenRVData(RVData: TCustomRVFormattedData);
begin

end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.UnassignChosenRVData(RVData: TCustomRVData);
begin
  Include(State,rvstForceStyleChangeEvent);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.Formatted(FirstItemNo, LastItemNo: Integer;Partial: Boolean);
begin

end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetInplaceEditor: TControl;
begin
  Result := nil
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.DestroyInplaceEditor;
begin

end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetNextFocusedItem(ItemNo: Integer; GoForward: Boolean;
                                  var TopLevelRVData: TCustomRVFormattedData;
                                  var TopLevelItemNo: Integer): Integer;
var i: Integer;
    Style: TRVStyle;
    {...........................................}
    function CheckItem(ItemNo: Integer): Boolean;
    begin
      if TCustomRVItemInfo(Items.Objects[i]).GetBoolValueEx(rvbpAllowsFocus, Style) then begin
        TopLevelRVData := Self;
        TopLevelItemNo := ItemNo;
        Result := True;
        exit;
      end;
      Result := TCustomRVItemInfo(Items.Objects[i]).MoveFocus(GoForward, TPersistent(TopLevelRVData), TopLevelItemNo);
    end;
    {...........................................}
begin
  if (FocusedItemNo<0) or (FocusedItemNo>=Items.Count) then
    FocusedItemNo := -1;
  Style := GetRVStyle;
  if GoForward then begin
    if (ItemNo<0) or
       TCustomRVItemInfo(Items.Objects[ItemNo]).GetBoolValueEx(rvbpAllowsFocus, Style) then
      inc(ItemNo);
    for i := ItemNo to Items.Count-1 do
      if CheckItem(i) then begin
        Result := i;
        exit;
      end;
    end
  else begin
    if ItemNo<0 then
      ItemNo := Items.Count;
    if (ItemNo=Items.Count) or
       TCustomRVItemInfo(Items.Objects[ItemNo]).GetBoolValueEx(rvbpAllowsFocus, Style) then
      dec(ItemNo);
    for i := ItemNo downto 0 do
      if CheckItem(i) then begin
        Result := i;
        exit;
      end;
  end;
  Result := -1;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.ClearFocus;
begin
  if (FocusedItemNo<0) or (FocusedItemNo>=Items.Count) then
    FocusedItemNo := -1;
  if FocusedItemNo<>-1 then
    TCustomRVItemInfo(Items.Objects[FocusedItemNo]).ClearFocus;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.AdjustFocus(NewFocusedItemNo: Integer;
                  TopLevelRVData: TPersistent; TopLevelItemNo: Integer);
begin
  FocusedItemNo := NewFocusedItemNo;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.ShowRectangle(Left, Top, Width, Height: Integer);
begin
  if (Left<GetHOffs) then begin
    HScrollTo(Left-GetWidth div 4);
    end
  else if (Left+Width>GetHOffs+GetWidth) then begin
    HScrollTo(Left-GetWidth div 4);
  end;

  if (Top+Height>GetVOffs+GetHeight-GetVSmallStep*2) and (Top<GetVOffs+GetVSmallStep) then exit;

  if (Top+Height>GetVOffs+GetHeight) then begin
     ScrollTo(Top+Height-GetHeight+GetVSmallStep,True);
    end
  else if (Top<GetVOffs) then begin
     ScrollTo(Top,True);
  end;
end;
{------------------------------------------------------------------------------}
{
procedure TCustomRVFormattedData.SetZoomPercent(const Value: Integer);
begin
  FZoomPercent := Value;
end;
}
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetZHOffs: Integer;
begin
  Result := GetHOffs;
  ZoomInt(Result);
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetZVOffs: Integer;
begin
  Result := GetVOffs;
  ZoomInt(Result);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.BuildJumpsCoords(var StartJumpNo: Integer; jumps: TList);
var i: Integer;
    LastItemNo,tmp: Integer;
    jmpinfo: TRVJumpInfo;
    Style: TRVStyle;
    item: TCustomRVItemInfo;
begin
  if jumps=nil then
    jumps := Self.jumps;
  Style := GetRVStyle;
  if Style=nil then exit;
  nJmps := 0;
  LastItemNo := -1;
  for i := 0 to DrawItems.Count-1 do begin
    item := TCustomRVItemInfo(Items.Objects[DrawItems[i].ItemNo]);
    if item.GetBoolValueEx(rvbpJump, Style) then
      with DrawItems[i] do begin
        jmpinfo     := TRVJumpInfo.Create;
        jmpinfo.l   := Left;
        jmpinfo.t   := Top;
        jmpinfo.w   := Width;
        jmpinfo.h   := Height;
        jmpinfo.id  := nJmps+StartJumpNo;
        if LastItemNo=DrawItems[i].ItemNo then
          dec(jmpinfo.id);
        jmpinfo.RVData := Self;
        jmpinfo.DrawItemNo := i;
        TCustomRVItemInfo(Items.Objects[DrawItems[i].ItemNo]).JumpID := jmpinfo.id;
        if (item.StyleNo<0) then
          jmpinfo.Cursor := item.GetHypertextCursor(Style)
        else
          jmpinfo.Cursor := Style.TextStyles[GetActualStyle(item)].JumpCursor;
        jumps.Add(jmpinfo);
        if LastItemNo<>DrawItems[i].ItemNo then begin
           inc(nJmps);
           LastItemNo := DrawItems[i].ItemNo;
        end;
      end
    else if item.StyleNo<0 then
      with DrawItems[i] do begin
        tmp := nJmps+ StartJumpNo;
        item.BuildJumps(Left, Top, tmp, jumps);
        nJmps := tmp-StartJumpNo;
      end;
  end;
  inc(StartJumpNo,nJmps);
end;
{------------------------------------------------------------------------------}
{ Coordinates of the top left corner of this RVData relative to the closest
  top left of the parent RichView's document (root one or inplace editor)
  (overriden in TRVTableCellData)                                              }
procedure TCustomRVFormattedData.GetOrigin(var ALeft, ATop: Integer);
begin
  ALeft := 0;
  ATop := 0;
end;
{------------------------------------------------------------------------------}
{ Coordinates of the top left corner of this RVData relative to the top left of
  the root parent RichView's document
  (overriden in TRVTableCellData, TRVTableInplaceRVData)                       }
procedure TCustomRVFormattedData.GetOriginEx(var ALeft, ATop: Integer);
begin
  ALeft := 0;
  ATop := 0;
end;
{------------------------------------------------------------------------------}
{ Is the specified position (drawing item, offset in it) inside the selection? }
function TCustomRVFormattedData.DItem_InsideSelection(DItemNo,
  DItemOffs: Integer): Boolean;
var sno,soff,eno,eoff: Integer;
begin
  Result := False;
  if not SelectionExists(False, False) then
    exit;
  GetSelBounds(sno, eno, soff,eoff, True);
  Result :=
    (sno>=0) and
    ((DItemNo>sno) or ((DItemNo=sno) and (DItemOffs>=soff))) and
    ((DItemNo<eno) or ((DItemNo=eno) and (DItemOffs<=eoff)));
end;
{------------------------------------------------------------------------------}
{ Is the specified position (item, offset in it) inside the selection?
  Unlike DItem_InsideSelection, checks if the current RVData is completely
  selected in its parent RVData                                                }
function  TCustomRVFormattedData.Item_InsideSelection(ItemNo,
  ItemOffs: Integer): Boolean;
var sno,soff,eno,eoff: Integer;
    PItemNo: Integer;
    Location: TRVStoreSubRVData;
begin
  Result := False;
  if GetParentData<>nil then begin
    GetParentInfo(PItemNo, Location);
    Location.Free;
    if (PItemNo>=0) then
      Result := TCustomRVFormattedData(GetParentData).Item_InsideSelection(PItemNo, 0);
  end;
  if Result then
    exit;
  if not SelectionExists(False, False) then
    exit;
  GetSelectionBounds(sno, soff, eno, eoff, True);
  Result :=
    (sno>=0) and
    ((sno<>eno) or (soff<>eoff)) and
    ((ItemNo>sno) or ((ItemNo=sno) and (ItemOffs>=soff))) and
    ((ItemNo<eno) or ((ItemNo=eno) and (ItemOffs<=eoff)));
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.AfterDeleteStyles(TextStylesShift,
  ParaStylesShift, ListStylesShift: TRVIntegerList);
{$IFNDEF RVDONOTUSEINPLACE}
var inplace: TControl;
{$ENDIF}
begin
  {$IFNDEF RVDONOTUSEINPLACE}
  inplace := GetInplaceEditor;
  if inplace is TCustomRichView then
    TCustomRichView(inplace).RVData.AfterDeleteStyles(TextStylesShift,
      ParaStylesShift, ListStylesShift);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.GetItemAt(X, Y: Integer; var ItemNo,
  OffsetInItem: Integer);
begin
  FindDrawItemForSel(X, Y, ItemNo, OffsetInItem, True);
  if ItemNo<>-1 then
    DrawItem2Item(ItemNo, OffsetInItem, ItemNo, OffsetInItem);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.GetItemAtEx(X,Y: Integer;
  var RVData: TCustomRVFormattedData; var ItemNo, OffsetInItem: Integer;
  Strict: Boolean; var InSubRVDataOwnerItem: Boolean);
var AX, AY, BX,BY: Integer;
begin
  InSubRVDataOwnerItem := False;
  RVData := Self;
  FindDrawItemForSel(X, Y, ItemNo, OffsetInItem, Strict);
  if ItemNo<>-1 then begin
    DrawItem2Item(ItemNo, OffsetInItem, ItemNo, OffsetInItem);
    GetItemCoords(ItemNo, AX, AY);
    RVData := TCustomRVFormattedData(GetItem(ItemNo).GetSubRVDataAt(X-AX,Y-AY));
    if RVData=nil then begin
      RVData := Self;
      InSubRVDataOwnerItem := GetItem(ItemNo).GetBoolValue(rvbpHasSubRVData);
      end
    else begin
      RVData.GetOriginEx(BX,BY);
      GetOriginEx(AX,AY);
      X := X+AX-BX;
      Y := Y+AY-BY;
      RVData.GetItemAtEx(X,Y, RVData, ItemNo, OffsetInItem, Strict, InSubRVDataOwnerItem);
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.FindWordAt(var Word: String; X,Y: Integer;
  var StyleNo, ItemNo, Offs: Integer;
  var RVData: TCustomRVFormattedData): Boolean;
var no: Integer;
    arr: PRVIntegerArray;
    i,max,min,first,len: Integer;
    Canvas: TCanvas;
    ItemOptions: TRVItemOptions;
    item: TCustomRVItemInfo;
    ditem: TRVDrawLineInfo;
    AX, AY: Integer;
    ARVData: TCustomRVFormattedData;
    s: String;
begin
  Canvas := GetCanvas;
  Result := False;
  RVData := Self;
  no := FindDrawItemAtPos(X+GetZHOffs, Y+GetZVOffs);
  ItemNo := -1;
  if no<>-1 then begin
     ditem := DrawItems[no];
     ItemNo := ditem.ItemNo;
     Word   := Items[ItemNo];
     StyleNo := GetItemStyle(ItemNo);
     if StyleNo>=0 then begin
       GetRVStyle.ApplyStyle(Canvas, StyleNo, GetParaBiDiMode(GetItemPara(ItemNo)));
       ItemOptions := GetItemOptions(ItemNo);
       s := DrawItems.GetString(no,Items);
       {$IFNDEF RVDONOTUSEALLCAPS}
       s := RV_ReturnCapitalized(s, GetRVStyle.TextStyles[StyleNo]);
       {$ENDIF}
       GetMem(arr, (ditem.Length+2)*sizeof(Integer));
       try
         if (GetItemBiDiMode(ditem.ItemNo)<>rvbdUnspecified) and
            RVU_GetTextCaretPos(Canvas, s, arr, ItemOptions,
              ditem.Width{$IFNDEF RVDONOTUSEJUSTIFY}-DrawItems[no].SpaceBefore{$ENDIF}) then begin
           min := Abs(X-arr[0]-ditem.Left{$IFNDEF RVDONOTUSEJUSTIFY}-ditem.SpaceBefore{$ENDIF});
           max := 0;
           for i := 1 to ditem.Length do
             if Abs(X-arr[i]-ditem.Left{$IFNDEF RVDONOTUSEJUSTIFY}-ditem.SpaceBefore{$ENDIF})<min then begin
               max := i;
               min := Abs(X-arr[i]-ditem.Left{$IFNDEF RVDONOTUSEJUSTIFY}-ditem.SpaceBefore{$ENDIF});
             end;
           end
         else begin
           RVU_GetTextExtentExPoint(Canvas,  s, X+GetZHOffs-ditem.Left
             {$IFNDEF RVDONOTUSEJUSTIFY}-ditem.SpaceBefore{$ENDIF},
             max,
             {$IFDEF RICHVIEWDEF4}nil,{$ELSE}@(arr[0]),{$ENDIF} ItemOptions);
         end;
       finally
         FreeMem(arr);
       end;
       inc(max,DrawItems[no].Offs);
       if max>RVU_Length(Word,ItemOptions) then
         max := RVU_Length(Word,ItemOptions);
       first := max;
       if IsDelimiter(Word, first, ItemOptions) then begin
         Offs := First;
         Word := '';
         Result := True;
         exit;
       end;
       while (first>1) and not IsDelimiter(Word, first-1, ItemOptions) do
         dec(first);
       len := max-first+1;
       while (first+len-1<RVU_Length(Word,ItemOptions)) and
             not IsDelimiter(Word, first+len, ItemOptions) do
         inc(len);
       Word := RVU_Copy(Word, first, len, ItemOptions);
       Offs := First;
       end
     else begin
       item := GetItem(ItemNo);
       item.ResetSubCoords;
       GetItemClientCoords(ItemNo,AX,AY);
       ARVData := TCustomRVFormattedData(item.GetSubRVDataAt(X-AX,Y-AY));
       if ARVData<>nil then begin
         if ARVData.GetRVData.GetParentControl.Parent=GetParentControl then begin
           dec(X, ARVData.GetRVData.GetParentControl.Left);
           dec(Y, ARVData.GetRVData.GetParentControl.Top);
         end;
         Result := TCustomRVFormattedData(ARVData.GetRVData).FindWordAt(Word,
           X,Y,StyleNo,ItemNo,Offs, RVData);
         exit;
       end;
     end;
     Result := True;
  end;
end;
{------------------------------------------------------------------------------}
function TCustomRVFormattedData.GetEditor: TWinControl;
begin
  Result := nil;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVFormattedData.ResetSubCoords;
begin

end;
{------------------------------------------------------------------------------}
{ Selects the screen line containing (ItemNo, Offs) location.                  }
procedure TCustomRVFormattedData.SelectLine(ItemNo, Offs: Integer);
var DItemNo, DOffs: Integer;
    DStrt, DEnd: Integer;
    ItemNo1,ItemNo2,Offs1, Offs2: Integer;
begin
  Item2DrawItem(ItemNo,Offs,DItemNo,DOffs);
  DStrt := DItemNo;
  while not DrawItems[DStrt].FromNewLine do
    dec(DStrt);
  DEnd := DItemNo+1;
  while (DEnd<Items.Count) and not DrawItems[DEnd].FromNewLine do
    inc(DEnd);
  dec(DEnd);
  DrawItem2Item(DStrt, GetOffsBeforeDrawItem(Dstrt), ItemNo1, Offs1);
  DrawItem2Item(DEnd, GetOffsAfterDrawItem(DEnd), ItemNo2, Offs2);
  SetSelectionBounds(ItemNo1, Offs1, ItemNo2, Offs2);
end;
{------------------------------------------------------------------------------}
{ Fills the list of soft page breaks by information form RVPrint (must be of
  TCustomRVPrint class).
  RVPrint must be formatted for printing.                                      }
procedure TCustomRVFormattedData.AssignSoftPageBreaks(RVPrint: TComponent);
var i: Integer;
    Item: TRVSoftPageBreakInfo;
begin
  ClearSoftPageBreaks;
  FSoftPageBreaks := TRVSoftPageBreakList.Create;
  for i := 1 to TCustomRVPrint(RVPrint).PagesCount do begin
    item := TRVSoftPageBreakInfo.Create;
    TCustomRVPrint(RVPrint).GetFirstItemOnPageEx(i, item.ItemNo, item.Offset,
      item.ExtraData);
    FSoftPageBreaks.Add(Item);
  end;
  if rvoShowPageBreaks in Options then
    Invalidate;
end;
{------------------------------------------------------------------------------}
{ Clears, destorys and nils the list of soft page breaks                       }
function TCustomRVFormattedData.ClearSoftPageBreaks: Boolean;
begin
  Result := FSoftPageBreaks<>nil;
  if Result then begin
    FSoftPageBreaks.Free;
    FSoftPageBreaks := nil;
  end;
end;
{------------------------------------------------------------------------------}
{ Returns a line number of location (ItemNo, ItemOffs). Line numbers are
  started from 1. Line number are counted from the beginning of each RVData.
  This method is relatively slow: iteration from the beginning of this RVData
  items.                                                                       }
function TCustomRVFormattedData.GetLineNo(ItemNo, ItemOffs: Integer): Integer;
var i, DItemNo, DItemOffs: Integer;
begin
  Item2DrawItem(ItemNo, ItemOffs, DItemNo, DItemOffs);
  Result := 0;
  for i := DItemNo downto 0 do
    if DrawItems[i].FromNewLine then
      inc(Result);
end;
{------------------------------------------------------------------------------}
{ Called in MouseUp. Adjusts selection. Overriden in TRVEditRVData (where
  selects resizable item if needed)                                            } 
procedure TCustomRVFormattedData.AdjustMouseUpSelection;
begin

end;
{------------------------------------------------------------------------------}
{ Searching for the item containing AControl and selecting it. Search is
  recursive, it includes nested RVData-s.                                      }
function TCustomRVFormattedData.SelectControl(AControl: TControl): Boolean;
    {........................................................}
    function SearchInItem(item: TCustomRVItemInfo): Boolean;
    var SubRVData: TCustomRVFormattedData;
        StoreSub: TRVStoreSubRVData;
    begin
      Result := False;
      SubRVData := TCustomRVFormattedData(item.GetSubRVData(StoreSub, rvdFirst));
      if SubRVData=nil then
        exit;
      repeat
        Result := TCustomRVFormattedData(SubRVData.GetRVData).SelectControl(AControl);
        if Result then
          break;
        SubRVData := TCustomRVFormattedData(item.GetSubRVData(StoreSub, rvdNext));
      until SubRVData=nil;
      StoreSub.Free;
    end;
    {........................................................}    
var i: Integer;
    RVData: TCustomRVFormattedData;
begin
  Result := False;
  for i := 0 to ItemCount-1 do
    if GetItemStyle(i)<0 then begin
      if (GetItem(i) is TRVControlItemInfo) and
         GetItem(i).OwnsControl(AControl) then begin
         RVData := TCustomRVFormattedData(Edit);
         RVData.SetSelectionBounds(i, 0, i, 1);
         RVData.Invalidate;
         Result := True;
         exit;
      end;
      Result := SearchInItem(GetItem(i));
      if Result then
        break;
    end;
end;
{------------------------------------------------------------------------------}
{ Returns True if the position (DrawItemNo,DrawItemOffs) is inside a word      }
function TCustomRVFormattedData.InsideWord(DrawItemNo,
  DrawItemOffs: Integer): Boolean;
var s: String;
    StyleNo : Integer;
    ItemOptions: TRVItemOptions;
begin
  Result := False;
  StyleNo := GetItemStyle(DrawItems[DrawItemNo].ItemNo);
  if (StyleNo<0) or
     (DrawItemOffs<=GetOffsBeforeDrawItem(DrawItemNo)) or
     (DrawItemOffs>=GetOffsAfterDrawItem(DrawItemNo)) then
    exit;
  s := DrawItems.GetString(DrawItemNo, Items);
  ItemOptions := GetItemOptions(DrawItems[DrawItemNo].ItemNo);
  Result := not IsDelimiter(s, DrawItemOffs-1, ItemOptions) and
            not IsDelimiter(s, DrawItemOffs, ItemOptions);
end;
{------------------------------------------------------------------------------}
{ Returns bounds of word containing position (DrawItemNo,DrawItemOffs).
  Returns:
  DrawItemWordOffs1 - index of the first word character (1-based),
  DrawItemWordOffs2 - index of the first character after the word (1-based).
  This procedure assumes that position is inside word (check by InsideWord())  }
procedure TCustomRVFormattedData.GetWordBounds(DrawItemNo,
  DrawItemOffs: Integer; var DrawItemWordOffs1, DrawItemWordOffs2: Integer);
var s: String;
    ItemOptions: TRVItemOptions;
    Len: Integer;
begin
  s := DrawItems.GetString(DrawItemNo, Items);
  ItemOptions := GetItemOptions(DrawItems[DrawItemNo].ItemNo);
  DrawItemWordOffs1 := DrawItemOffs-1;
  while (DrawItemWordOffs1-1>0) and
        not IsDelimiter(s, DrawItemWordOffs1-1, ItemOptions) do
    dec(DrawItemWordOffs1);
  DrawItemWordOffs2 := DrawItemOffs;
  Len := RVU_Length(s, ItemOptions);
  while (DrawItemWordOffs2<=Len) and
        not IsDelimiter(s, DrawItemWordOffs2, ItemOptions) do
    inc(DrawItemWordOffs2);
end;
{------------------------------------------------------------------------------}
{ Adjusts selection bounds depending on RVStyle.SelectionMode.
  (X,Y) is coordinate of mouse making selection (relative to the top left
  corner of the document).
  Returns True if the new selection bounds are different from their previous
  values.                                                                      }
function TCustomRVFormattedData.AdjustSelectionByMode(X, Y: Integer): Boolean;
var w1, w2: Integer;
    AdjustSelEnd: Boolean;
    OldSelSNo, OldSelENo, OldSelSOffs, OldSelEOffs: Integer;
    DrawItemNo, DrawItemOffs: Integer;
    {...........................................................}
    function IsSelectionChanged: Boolean;
    begin
      Result := (OldSelSNo<>FSelStartNo) or (OldSelENo<>FSelEndNo) or
        (OldSelSOffs<>FSelStartOffs) or (OldSelEOffs<>FSelEndOffs);
    end;
    {...........................................................}
begin
  GetSelectionBounds(OldSelSNo, OldSelSOffs, OldSelENo, OldSelEOffs, False);
  FindDrawItemForSel(X, Y, DrawItemNo, DrawItemOffs, False);
  if FSelectingInfo<>nil then begin
    if FSelectingInfo.SWordState=rvsesParaMode then begin
      FSelectingInfo.DrawItemENo := DrawItemNo;
      FSelectingInfo.DrawItemEOffs := DrawItemOffs;
      FSelStartNo   := FSelectingInfo.DrawItemSNo;
      FSelStartOffs := FSelectingInfo.DrawItemSOffs;
      FSelEndNo     := DrawItemNo;
      FSelEndOffs   := DrawItemOffs;
      ExpandSelectionToParagraph(True);
      Result := IsSelectionChanged;
      exit;
    end;
    AdjustSelEnd := True;
    // 1. Adjusting selection start
    case FSelectingInfo.SWordState of
      rvsesInWord: // mouse is inside the initial word
        begin
          // checking mouse leaving the initial word
          // when it leaves, setting SWordState := rvsesOutsideWord and
          // adjusting the selection start to the word bound
          if FSelectingInfo.IsAboveSWord(DrawItemNo, DrawItemOffs) then begin
            FSelectingInfo.SWordState := rvsesOutsideWord;
            FSelStartOffs := FSelectingInfo.DrawItemSWordOffs2;
            end
          else if FSelectingInfo.IsBelowSWord(DrawItemNo, DrawItemOffs) then begin
            FSelectingInfo.SWordState := rvsesOutsideWord;
            FSelStartOffs := FSelectingInfo.DrawItemSWordOffs1;
            end
          else
            AdjustSelEnd := False;
        end;
      rvsesOutsideWord: // mouse is outside the initial word
        begin
          // checking mouse returning to the initial word
          // when it returns, stopping adjusting the selection start,
          // setting SWordState := rvsesFreeMode
          if FSelectingInfo.IsInSWord(DrawItemNo, DrawItemOffs) then begin
            FSelStartOffs := FSelectingInfo.DrawItemSOffs;
            FSelectingInfo.SWordState := rvsesFreeMode;
            AdjustSelEnd := False;
          end;
        end;
    end;
    // 1. Adjusting selection end
    if AdjustSelEnd then begin
      if InsideWord(DrawItemNo, DrawItemOffs) then begin
        GetWordBounds(DrawItemNo, DrawItemOffs, w1, w2);
        if FSelectingInfo.IsEWord(DrawItemNo, w1, w2) then
          // mouse is in the same word as before
          case FSelectingInfo.EWordState of
            rvsesInWord:
              begin
                // if it's moved in backward direction, allowing selection
                // by characters (setting EWordState := rvsesFreeMode)
                if FSelectingInfo.IsEFreeStateNeeded(DrawItemNo, DrawItemOffs) then begin
                  FSelectingInfo.EWordState := rvsesFreeMode;
                  AdjustSelEnd := False;
                  end
                else
                // if it's moved in forward direction, updating DrawItemEOffs
                  FSelectingInfo.DrawItemEOffs := DrawItemOffs;
              end;
            rvsesFreeMode:
              AdjustSelEnd := False;
          end
        else begin
          // mouse is entered a new word
          FSelectingInfo.EWordState := rvsesInWord;
          FSelectingInfo.DrawItemEWordOffs1 := w1;
          FSelectingInfo.DrawItemEWordOffs2 := w2;
          FSelectingInfo.DrawItemEOffs := DrawItemOffs;
        end;
        // adjusting the selection end
        if AdjustSelEnd then
          if FSelectingInfo.IsAboveSWord(DrawItemNo, DrawItemOffs) then begin
            DrawItemOffs := w1;
            end
          else if FSelectingInfo.IsBelowSWord(DrawItemNo, DrawItemOffs) then begin
            DrawItemOffs := w2;
          end;
        end
      else begin
        // mouse is outside word
        FSelectingInfo.EWordState := rvsesInWord;
        FSelectingInfo.DrawItemEWordOffs1 := -1;
        FSelectingInfo.DrawItemEWordOffs2 := -1;
      end;
    end;
  end;
  FSelEndNo := DrawItemNo;
  FSelEndOffs := DrawItemOffs;
  Result := IsSelectionChanged;
end;
{------------------------------------------------------------------------------}
{ Making selection by lines.
  (X,Y) is coordinate of mouse making selection (relative to the top left
  corner of the document).                                                     }
function TCustomRVFormattedData.AdjustLineSelection(X, Y: Integer): Boolean;
var OldSelSNo, OldSelENo, OldSelSOffs, OldSelEOffs: Integer;
    {...........................................................}
    function IsSelectionChanged: Boolean;
    begin
      Result := (OldSelSNo<>FSelStartNo) or (OldSelENo<>FSelEndNo) or
        (OldSelSOffs<>FSelStartOffs) or (OldSelEOffs<>FSelEndOffs);
    end;
    {...........................................................}
begin
  GetSelectionBounds(OldSelSNo, OldSelSOffs, OldSelENo, OldSelEOffs, False);
  FindDrawItemForSel(X, Y, FSelEndNo, FSelEndOffs, False);
  ExpandSelectionToLines(False);
  if GetRVStyle.SelectionMode=rvsmParagraph then
    ExpandSelectionToParagraph(True);
  Result := IsSelectionChanged;
end;
{------------------------------------------------------------------------------}
{ Expands the selection (if it is not empty) to the paragraphs boundaries
  Does nothing if OnlyIfMultiple=True and selection does not include
  more than 1 paragraph.                                                       }
function TCustomRVFormattedData.ExpandSelectionToParagraph(
  OnlyIfMultiple: Boolean): Boolean;
  {..........................................................}
  procedure ExpandUp(var DrawItemNo, DrawItemOffs: Integer);
  begin
    while DrawItemNo>0 do begin
      if IsDrawItemParaStart(DrawItemNo) then
        break;
      dec(DrawItemNo);
    end;
    DrawItemOffs := GetOffsBeforeDrawItem(DrawItemNo);
  end;
  {..........................................................}
  procedure ExpandDown(var DrawItemNo, DrawItemOffs: Integer);
  begin
    while DrawItemNo+1<DrawItems.Count do begin
      if IsDrawItemParaStart(DrawItemNo+1) then
        break;
      inc(DrawItemNo);
    end;
    DrawItemOffs := GetOffsAfterDrawItem(DrawItemNo);
  end;
begin
  Result := (FSelStartNo>=0) and
     not ((FSelStartNo=FSelEndNo) and (FSelStartOffs=FSelEndOffs));
  if not Result then
    exit;
  Result := not OnlyIfMultiple or IsMultiParagraphSelection;
  if not Result then
    exit;
  if IsSelectionTopDown then begin
    ExpandUp(FSelStartNo, FSelStartOffs);
    ExpandDown(FSelEndNo, FSelEndOffs);
    end
  else begin
    ExpandDown(FSelStartNo, FSelStartOffs);
    ExpandUp(FSelEndNo, FSelEndOffs);
  end;
end;
{------------------------------------------------------------------------------}
{ If OneLine=False, expands selection to the lines boundaries.
  If OneLine=True, selects the line containing the beginning of selection,
  from the beginning to the end of the line                                    }
procedure TCustomRVFormattedData.ExpandSelectionToLines(OneLine: Boolean);
{..........................................................}
  procedure ExpandUp(var DrawItemNo, DrawItemOffs: Integer);
  begin
    while (DrawItemNo>0) and not DrawItems[DrawItemNo].FromNewLine do
      dec(DrawItemNo);
    DrawItemOffs := GetOffsBeforeDrawItem(DrawItemNo);
  end;
  {..........................................................}
  procedure ExpandDown(var DrawItemNo, DrawItemOffs: Integer);
  begin
    while (DrawItemNo+1<DrawItems.Count) and
      not DrawItems[DrawItemNo+1].FromNewLine do
      inc(DrawItemNo);
    DrawItemOffs := GetOffsAfterDrawItem(DrawItemNo);
  end;
  {..........................................................}
begin
  if FSelStartNo<0 then
    exit;
  if OneLine then begin
    FSelEndNo := FSelStartNo;
    ExpandUp(FSelStartNo, FSelStartOffs);
    ExpandDown(FSelEndNo, FSelEndOffs);
    end
  else begin
    if IsSelectionTopDown then begin
      ExpandUp(FSelStartNo, FSelStartOffs);
      ExpandDown(FSelEndNo, FSelEndOffs);
      end
    else begin
      ExpandDown(FSelStartNo, FSelStartOffs);
      ExpandUp(FSelEndNo, FSelEndOffs);
    end;
  end;
end;
{------------------------------------------------------------------------------}
{ Returns True if the selection is in top-down direction.                     }
function TCustomRVFormattedData.IsSelectionTopDown: Boolean;
begin
  Result := (FSelStartNo<FSelEndNo) or
    ((FSelStartNo=FSelEndNo) and (FSelStartOffs<=FSelEndOffs));
end;
{------------------------------------------------------------------------------}
{ Returns True if the drawing item starts a new paragraph.                     }
function TCustomRVFormattedData.IsDrawItemParaStart(DrawItemNo: Integer): Boolean;
begin
  Result := (DrawItemNo=0) or
    ((DrawItems[DrawItemNo].ItemNo<>DrawItems[DrawItemNo-1].ItemNo) and
     IsParaStart(DrawItems[DrawItemNo].ItemNo));
end;
{------------------------------------------------------------------------------}
{ Returns True if the drawing item ends a paragraph.                           }
function TCustomRVFormattedData.IsDrawItemParaEnd(DrawItemNo: Integer): Boolean;
begin
  Result := (DrawItemNo=DrawItems.Count-1) or IsDrawItemParaStart(DrawItemNo+1);
end;
{------------------------------------------------------------------------------}
{ Returns true, if the given DrawItem either starts a new line
  or follows a marker                                                          }
function TCustomRVFormattedData.IsDrawItemFromNewLine(DrawItemNo: Integer): Boolean;
begin
  Result := DrawItems[DrawItemNo].FromNewLine;
  {$IFNDEF RVDONOTUSELISTS}
  if not Result and (DrawItemNo>0) and
    (DrawItems[DrawItemNo].ItemNo<>DrawItems[DrawItemNo-1].ItemNo) and
    (GetItemStyle(DrawItems[DrawItemNo-1].ItemNo)=rvsListMarker) then
    Result := True;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
{ Returns True, if the selection includes more than one paragraphs (at least
  partially.                                                                   }
function TCustomRVFormattedData.IsMultiParagraphSelection: Boolean;
var StartDrawItemNo, EndDrawItemNo, i: Integer;
begin
  Result := FSelStartNo>=0;
  if not Result then
    exit;
  if IsSelectionTopDown then begin
    StartDrawItemNo := FSelStartNo;
    EndDrawItemNo   := FSelEndNo;
    end
  else begin
    StartDrawItemNo := FSelEndNo;
    EndDrawItemNo   := FSelStartNo;
  end;
  Result := False;
  for i := StartDrawItemNo+1 to EndDrawItemNo do
    if IsDrawItemParaStart(i) then begin
      Result := True;
      exit;
    end;
end;
{$IFNDEF RVDONOTUSEDRAGDROP}
{------------------------------------------------------------------------------}
{ Drag&Drop: IDropTarget related                                               }
{------------------------------------------------------------------------------}
{ Sets the position of drag&drop target to (X,Y).
  (X,Y) - client coordinates.
  Usually it's called for the root (or inplace editor's) RVData.
  Calls SetDragDropCaret for the RVData containing
  d&d caret. Redraws d&d caret. Scrolls to make it visible.                    }
procedure TCustomRVFormattedData.SetDragDropCaretTo(X,Y: Integer);
var RVData: TCustomRVFormattedData;
    ItemNo, Offs: Integer;
    DragDropCaretInfo: TRVDragDropCaretInfo;
var NoScroll: Boolean;
begin
  inc(X, GetHOffs);
  inc(Y, GetVOffs);
  DrawDragDropCaret(GetCanvas, False);
  GetItemAtEx(X,Y, RVData, ItemNo, Offs, False, NoScroll);
  if (RVData<>nil) and (ItemNo>=0) then
    RVData.SetDragDropCaret(ItemNo, Offs);
  DrawDragDropCaret(GetCanvas, False);
  if not NoScroll then begin
    DragDropCaretInfo := GetDragDropCaretInfo;
    if DragDropCaretInfo<>nil then
      with DragDropCaretInfo do
        TCustomRVFormattedData(RVData).ShowRectangle(X, Y, 1, Height);
  end;
end;
{------------------------------------------------------------------------------}
{ Draw drag&drop caret. Usually called to remove it.                           }
procedure TCustomRVFormattedData.RemoveDragDropCaret;
begin
  DrawDragDropCaret(GetCanvas, False);
end;
{------------------------------------------------------------------------------}
{ Sets the position of drag&drop caret
  (fills the field of GetDragDropCaretInfo)                                    }
procedure TCustomRVFormattedData.SetDragDropCaret(ItemNo, Offs: Integer);
var s: String;
    StyleNo, DItemNo, DOffs, max: Integer;
    ItemOptions: TRVItemOptions;
    arr: PRVIntegerArray;
    ditem: TRVDrawLineInfo;
    DragDropCaretInfo: TRVDragDropCaretInfo;
begin
  DragDropCaretInfo := GetDragDropCaretInfo;
  if DragDropCaretInfo=nil then
    exit;
  DragDropCaretInfo.RVData := Self;
  DragDropCaretInfo.ItemNo := ItemNo;
  DragDropCaretInfo.ItemOffs := Offs;
  StyleNo := GetActualStyle(GetItem(ItemNo));
  Item2DrawItem(ItemNo, Offs, DItemNo, DOffs);
  ditem := DrawItems[DItemNo];
  DragDropCaretInfo.Y := ditem.Top;
  DragDropCaretInfo.Height := ditem.Height;
  DragDropCaretInfo.X := ditem.Left;
  if (StyleNo>=0) and (ditem.Length>0) then begin
    GetRVStyle.ApplyStyle(GetCanvas, StyleNo, GetParaBiDiMode(GetItemPara(ItemNo)));
    ItemOptions := GetItemOptions(ItemNo);
    s := DrawItems.GetString(DItemNo,Items);
    {$IFNDEF RVDONOTUSEALLCAPS}
    s := RV_ReturnCapitalized(s, GetRVStyle.TextStyles[StyleNo]);
    {$ENDIF}
       GetMem(arr, (ditem.Length+2)*sizeof(Integer));
       try
         if (GetItemBiDiMode(ItemNo)=rvbdUnspecified) or
            not RVU_GetTextCaretPos(GetCanvas, s, arr, ItemOptions, ditem.Width-ditem.SpaceBefore) then begin
           if DOffs=1 then
             DragDropCaretInfo.X := ditem.Left
           else begin
             RVU_GetTextExtentExPoint(GetCanvas,  s, ditem.Width*10, max, arr, ItemOptions);
             DragDropCaretInfo.X := ditem.Left+arr[DOffs-2];
           end;
           end
         else
           DragDropCaretInfo.X := ditem.Left+arr[DOffs-1];
       finally
         FreeMem(arr);
       end;
    end
  else if Offs<=GetOffsBeforeItem(ItemNo) then
    DragDropCaretInfo.X := ditem.Left
  else
    DragDropCaretInfo.X := ditem.Left+ditem.Width;
end;
{------------------------------------------------------------------------------}
{ Returns information about drag&drop caret location.
  Returned value <> nil only if dragging to this RichViewEdit is in process.
  Physically, returned object is a field of the root TRichViewEdit.
  This method is overriden in TRVEditRVData and
  in TRichViewRVData (returns nil)                                             }
function TCustomRVFormattedData.GetDragDropCaretInfo: TRVDragDropCaretInfo;
begin
  Result := TCustomRVFormattedData(GetAbsoluteRootData).GetDragDropCaretInfo;
end;
{------------------------------------------------------------------------------}
{ Drawing drag&drop caret (using inverted color).
  Draws something only when d&d cursor is above the editor.
  If OnlyForSelf, drawing is performed only if this RVData contains
  d&d caret. Otherwise, drawing is performed by RVData contained d&d caret.
  Canvas - canvas for drawing to. If drawing is performed not by this RVData,
  its own Canvas is used instead.                                              }
procedure TCustomRVFormattedData.DrawDragDropCaret(Canvas: TCanvas;
  OnlyForSelf: Boolean);
var DragDropCaretInfo: TRVDragDropCaretInfo;
    i: Integer;
    dx,dy: Integer;
begin
  DragDropCaretInfo := GetDragDropCaretInfo;
  if (DragDropCaretInfo=nil) or (DragDropCaretInfo.RVData=nil) then
    exit;
  if (DragDropCaretInfo.RVData<>Self) then begin
    if not OnlyForSelf then
      TCustomRVFormattedData(DragDropCaretInfo.RVData).DrawDragDropCaret(
        TCustomRVFormattedData(DragDropCaretInfo.RVData).GetCanvas, True);
    exit;
  end;
  Canvas.Pen.Mode := pmNot;
  Canvas.Pen.Width := 1;
  Canvas.Brush.Style := bsClear;
  Canvas.Pen.Style := psDot;
  GetOrigin(dx,dy);
  dec(dx, TCustomRVFormattedData(GetRootData).GetHOffs);
  dec(dy, TCustomRVFormattedData(GetRootData).GetVOffs);
  for i := DragDropCaretInfo.Y+dy to DragDropCaretInfo.Y+DragDropCaretInfo.Height+dy do
    if i mod 2=0 then begin
      Canvas.MoveTo(dx+DragDropCaretInfo.X,i);
      Canvas.LineTo(dx+DragDropCaretInfo.X+2,i);
    end;
  Canvas.Pen.Style := psSolid;
  Canvas.Pen.Mode := pmCopy;
end;
{------------------------------------------------------------------------------}
{ Drag&Drop: IDropSource related                                               }
{------------------------------------------------------------------------------}
{ Can dragging from this TRichView be started?
  Overriden in TRichViewRVData.                                                }
function TCustomRVFormattedData.CanStartDragging: Boolean;
begin
  Result := TCustomRVFormattedData(GetAbsoluteRootData).CanStartDragging;
end;
{------------------------------------------------------------------------------}
{ Is dragging from this TRichView in process? Overriden in TRichViewRVData.    }
function TCustomRVFormattedData.IsDragging: Boolean;
begin
  Result := TCustomRVFormattedData(GetAbsoluteRootData).IsDragging;
end;
{------------------------------------------------------------------------------}
{ Initializing dragging. Overriden in TRichViewRVData.
  Returns True on success.
  Returns DropSource and OKEffect for call of DoDragDrop.                      }
function TCustomRVFormattedData.InitDragging(var DropSource: TRVDropSource;
  var OKEffect: Integer): Boolean;
begin
  Result := TCustomRVFormattedData(GetAbsoluteRootData).InitDragging(DropSource, OKEffect);
end;
{------------------------------------------------------------------------------}
{ Finalizing dragging. Overriden in TRichViewRVData.                           }
procedure TCustomRVFormattedData.DoneDragging(FDeleteSelection: Boolean);
begin
  TCustomRVFormattedData(GetAbsoluteRootData).DoneDragging(FDeleteSelection);
end;
{------------------------------------------------------------------------------}
{ Perform drag & drop from this RVData (called only for absolute root RVData)  }
procedure TCustomRVFormattedData.DoDrag;
var Effect, OKEffect: LongInt;
    DropSource: TRVDropSource;
    pt: TPoint;
    RVData: TCustomRVFormattedData;
    ItemNo, Offs: Integer;
    Dummy: Boolean;
begin
  State := State-[rvstStartingDragDrop]+
    [rvstCanDragDropDeleteSelection,rvstDragDropCursorNotMoved];
  try
    if InitDragging(DropSource, OKEffect) then begin
      DoDragDrop(DropSource as IDataObject, DropSource as IDropSource,
        OKEffect, Effect);
      DoneDragging((Effect=DROPEFFECT_MOVE) and (rvstCanDragDropDeleteSelection in State));
      if (Effect=DROPEFFECT_NONE) and (rvstDragDropCursorNotMoved in State) then begin
        GetCursorPos(pt);
        pt := GetParentControl.ScreenToClient(pt);
        GetItemAtEx(pt.X+GetHOffs,pt.Y+GetVOffs, RVData, ItemNo, Offs, False, Dummy);
        if ItemNo>=0 then begin
          RVData := TCustomRVFormattedData(RVData.Edit);
          RVData.SetSelectionBounds(ItemNo, Offs, ItemNo, Offs);
          RVData.Invalidate;
        end;
        {
        MouseDown(mbLeft, [ssLeft], pt.X, pt.Y);
        MouseUp(mbLeft, [], pt.X, pt.Y);
        }
      end;
    end;
  finally
    State := State-[rvstCanDragDropDeleteSelection,rvstDragDropCursorNotMoved];
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
{ Returns true, if insertion in the specified location is possible.
  Used by drag&drop functions.                                                 }
function TCustomRVFormattedData.CanInsertHere(ItemNo, Offs: Integer): Boolean;
begin
  Result := False;
  if IsItemParaProtected(ItemNo) then
    exit;
  if Offs<=GetOffsBeforeItem(ItemNo) then
    Result := not IsSticking(ItemNo-1, True)
  else if Offs>=GetOffsAfterItem(ItemNo) then
    Result := not IsSticking(ItemNo, True)
  else
    Result := not IsProtected(ItemNo, rvprModifyProtect);
end;
{------------------------------------------------------------------------------}
{ Returns true if the ItemNo-th item has the specified protection.
  There is a special processing of rvprDeleteProtect - for non-text items,
  their DeleteProtect property is returned.
  This function used in editor and by CanInsertHere().                         }
function TCustomRVFormattedData.IsProtected(ItemNo: Integer;
  Option: TRVProtectOption): Boolean;
var item: TCustomRVItemInfo;
begin
  item := GetItem(ItemNo);
  if item.StyleNo>=0 then
    Result := Option in GetRVStyle.TextStyles[GetActualStyle(item)].Protection
  else
    if Option = rvprDeleteProtect then
      Result := TRVNonTextItemInfo(item).DeleteProtect
    else
      Result := False;
end;
{------------------------------------------------------------------------------}
{ Returns true if the ParaNo-th paragraph style has the specified option.
  This function used in editor and by CanInsertHere().                         }
function TCustomRVFormattedData.IsParaProtected(ParaNo: Integer;
  Option: TRVParaOption): Boolean;
begin
  Result := Option in GetRVStyle.ParaStyles[ParaNo].Options;
end;
{------------------------------------------------------------------------------}
{ Returns true if the ItemNo-th item's paragraph style is read-only.
  This function used in editor and by CanInsertHere().                         }
function TCustomRVFormattedData.IsItemParaProtected(ItemNo: Integer): Boolean;
begin
  Result := IsParaProtected(GetItemPara(ItemNo), rvpaoReadOnly);
end;
{------------------------------------------------------------------------------}
{ Returns true if insertion between FirstItemNo and (FirstItemNo+1) items is
  forbidden.
  FirstItemNo=-1 and FirstItemNo=Items.Count-1 are valid indices.
  This function used in editor and by CanInsertHere().
  If the function returns True and not NoSound, the function beeps             }
function TCustomRVFormattedData.IsSticking(FirstItemNo: Integer;
  NoSound: Boolean): Boolean;
begin
  if (FirstItemNo=-1) then
    Result := IsProtected(0, rvprStickToTop)
  else if (FirstItemNo=Items.Count-1) then
    Result := IsProtected(FirstItemNo, rvprStickToBottom)
  else
    Result := IsProtected(FirstItemNo, rvprSticking) and
              IsProtected(FirstItemNo+1, rvprSticking);
  if Result and not NoSound then
    Beep;
end;

end.







