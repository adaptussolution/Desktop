
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRVScroller: ancestor of all visual             }
{       RichView components.                            }
{       Also contains definition of some types used     }
{       elsewhere.                                      }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVScroll;

interface

uses
{$I RV_Defs.inc}
  Windows, Messages, SysUtils, Classes, Controls, Graphics, Forms,
  {$IFDEF RICHVIEWDEF4}
  ImgList,
  {$ENDIF}
  {$IFNDEF RVDONOTUSEDRAGDROP}
  ActiveX,
  {$ENDIF}
  CommCtrl, RVXPTheme, RVStr;
type
  TRVScroller = class;
  { Bi-di properties of TCustomRichView, TParaInfo, TFontInfo }
  TRVBiDiMode = (
    rvbdUnspecified,       // BiDiMode is not supported / use parent BiDiMode
    rvbdLeftToRight,       // left to right
    rvbdRightToLeft);      // right to left

  { Values for TCustomRichView.Options }
  TRVOption = (
    rvoAllowSelection,      // allows selection; must be set in editor
    rvoSingleClick,         // if set, OnRVDblClick occurs on single click
    rvoScrollToEnd,         // if set, FormatTail scrolls to end
    rvoClientTextWidth,     // text is wrapped when exceed window width
    rvoShowCheckpoints,     // shows checkpoints
    rvoShowPageBreaks,      // shows page breaks
    rvoShowSpecialCharacters, // shows dots in spaces and paragraph marks    
    rvoTagsArePChars,       // tags are pointers to strings allocated by StrNew
    rvoAutoCopyText,        // Default Clipboard copying: copy ANSI text (CF_TEXT)
    rvoAutoCopyUnicodeText, // DCC: copy Unicode text (CF_UNICODETEXT)
    rvoAutoCopyRVF,         // DCC: copy RVF ('RichView Format')
    rvoAutoCopyImage,       // DCC: copy image, if it is selected
    rvoAutoCopyRTF,         // DCC: copy RTF ('Rich Text Format');
    rvoFormatInvalidate,    // Format and FormatTail redraws document
    rvoDblClickSelectsWord, // Double click selects word
    rvoRClickDeselects,     // If set, right click outside selection deselects
    rvoDisallowDrag,        // If set, drag&drop from this TRichView is disabled
    rvoShowItemHints        // shows items' hint
    );
  TRVOptions = set of TRVOption;

  { Values for TCustomRichView.TabNavigation }
  TRVTabNavigationType = (
    rvtnNone,               // Tab does nothing (the only available option for
                            //   TCustomRichViewEdit)
    rvtnTab,                // Tab and Shift+Tab navigate hypertext links
    rvtnCtrlTab);           // Ctrl+Tab and Ctrl+Shift+Tab navigate hypertext
                            //   links and controls.

  { Values for TCustomRichView.DoInPaletteMode }
  TRVPaletteAction = (
    rvpaDoNothing,          // No special action in 256-color mode (more than 16-
                            //   color images will be displayed incorrectly)
    rvpaAssignPalette,      // In 256-color mode, common palette is assigned to
                            // all bitmaps (inserted images are modified)
    rvpaCreateCopies,       // (default and recommended) In 256-color mode,
                            // paletted copies of all images are created and
                            // displayed 
    rvpaCreateCopiesEx);    // Reserved

  { Values for TCustomRichView.BackgroundStyle }
  TBackgroundStyle = (
    bsNoBitmap,             // color background (no image)
    bsStretched,            // stretched image (BackgroundBitmap)
    bsTiled,                // tiled image
    bsTiledAndScrolled,     // tiled image, scrolled with text
    bsCentered);            // centered image

  TRVDisplayOption = (
    rvdoImages,
    rvdoComponents,
    rvdoBullets);
  TRVDisplayOptions = set of TRVDisplayOption;

  TRVSearchOption = (
    rvsroMatchCase,
    rvsroDown,
    rvsroWholeWord);
  TRVSearchOptions = set of TRVSearchOption;

  TCPEventKind = (
    cpeNone,
    cpeAsSectionStart,
    cpeWhenVisible);

  TRVScrollBarStyle = (
    rvssRegular,
    rvssFlat,
    rvssHotTrack);

  TRVRTFHighlight = (
    rtfhlIgnore,
    rtfhlFixedColors,
    rtfhlColorTable);
  {----------------------------------------------------------------------------}
  { TRVScrollerInternalIfcObject: an ancestor class for objects implementing
    COM interfaces. These objects are contained in RVScroller's descendants.
    This class implements:
    - constructor and destructor;
    - methods - wrappers for methods of RVSscroller (this allows to make
      RVScroller's methods protected).
    Used for drag&drop.                                                        }
  {$IFNDEF RVDONOTUSEDRAGDROP}
  TRVScrollerInternalIfcObject = class (TInterfacedObject)
    protected
      FOwner: TRVScroller;
      function OwnerDragEnter(X,Y: Integer): Boolean;
      procedure OwnerDragLeave;
      function OwnerDragOver(X,Y: Integer): Boolean;
      procedure OwnerReleaseDropTargetObject;
      function OwnerDrop(const DataObj: IDataObject; FMove: Boolean): Integer;
      function OwnerCanAcceptFormat(Format: Word): Boolean;
    public
      constructor Create(AOwner: TRVScroller);
      destructor Destroy; override;
  end;
  {$ENDIF}
{-----------------------------------------------------------------------}
  TRVScroller = class(TCustomControl)
  private
    FBorderStyle: TBorderStyle;
    FSmallStep: Integer;
    FTracking: Boolean;
    FFullRedraw: Boolean;
    FVScrollVisible, FHScrollVisible, FUpdatingScrollBars: Boolean;
    FVScrollMax, FVScrollPage: Integer;
    FHScrollMax, FHScrollPage: Integer;
    FDoInPaletteMode: TRVPaletteAction;
    FBiDiMode: TRVBiDiMode;
    FUseXPThemes: Boolean;
    {$IFDEF RVFLATSCROLLBARS}
    FScrollBarStyle: TRVScrollBarStyle;
    FScrollBarColor: TColor;
    {$ENDIF}
    { Window messages }
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure WMQueryNewPalette(var Message: TWMQueryNewPalette); message WM_QUERYNEWPALETTE;
    procedure WMPaletteChanged(var Message: TWMPaletteChanged); message WM_PALETTECHANGED;
    procedure WMThemeChanged(var Message: TMessage); message WM_THEMECHANGED;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;

    function GetVScrollMax: Integer;
    function GetHScrollMax: Integer;
    procedure SetVScrollVisible(vis: Boolean);
    procedure SetHScrollVisible(vis: Boolean);
    procedure SetBorderStyle(const Value: TBorderStyle);
    procedure SetDoInPaletteMode(Value: TRVPaletteAction);
    procedure SetVScrollPos(Value: Integer);
    function GetInplaceEditor: TWinControl;
    function GetChosenRVData: TPersistent;
    {$IFDEF RVFLATSCROLLBARS}
    procedure SetScrollBarStyle(const Value: TRVScrollBarStyle);
    procedure SetScrollBarColor(const Value: TColor);
    procedure UpdateScrollStyle(Redraw: Boolean);
    procedure UpdateScrollColor(Redraw: Boolean);
    {$ENDIF}
    procedure CreateThemeHandle; virtual;
    procedure FreeThemeHandle; virtual;
    procedure SetUseXPThemes(const Value: Boolean);
  protected
    FOnVScrolled, FOnHScrolled: TNotifyEvent;
    FVDisableNoScroll: ByteBool;
    HPos, VPos, XSize, YSize: Integer;
    KeyboardScroll: Boolean;
    FChosenItem: TPersistent;
    FChosenRVData: TPersistent;
    FTheme: HTheme;
    FScrollFactor: Integer;
    {$IFDEF RICHVIEWDEF4}
    FWheelStep: Integer;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    {$ENDIF}
    procedure SetBiDiModeRV(const Value: TRVBiDiMode); virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure AfterCreateWnd1; dynamic;
    procedure AfterCreateWnd2; dynamic;
    procedure DestroyWnd; override;
    function GetPalette: HPALETTE; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure SetVPos(p: Integer; Redraw: Boolean);virtual;
    procedure SetHPos(p: Integer); virtual;
    procedure ScrollChildren(dx, dy: Integer);
    procedure AfterVScroll; virtual;
    procedure AfterHScroll; virtual;
    function GetDefSmallStep: Integer; dynamic;

    function AllocLogPalette(ColorCount: Integer): PLogPalette;
    procedure FreeLogPalette(var lpLogPal: PLogPalette);
    function GenerateLogPalette: PLogPalette; dynamic;
    //function GetLogPalette(hpal: HPALETTE):PLogPalette;
    procedure UpdatePaletteInfo; dynamic;
    procedure SetVSmallStep(Value: Integer); virtual;
    {$IFNDEF RVDONOTUSEDRAGDROP}
    { OLE drag&drop, related to IDropTarget }
    function OleDragEnter(X,Y: Integer): Boolean; dynamic;
    procedure OleDragLeave; dynamic;
    function OleDragOver(X,Y: Integer): Boolean; dynamic;
    procedure ReleaseOleDropTargetObject; dynamic;
    function OleDrop(const DataObj: IDataObject; FMove: Boolean): Integer; dynamic;
    function OleCanAcceptFormat(Format: Word): Boolean; dynamic;
    {$ENDIF}
    property Tracking: Boolean read FTracking write FTracking default True;
    property OnVScrolled: TNotifyEvent read FOnVScrolled write FOnVScrolled;
    property OnHScrolled: TNotifyEvent read FOnHScrolled write FOnHScrolled;
    property DoInPaletteMode: TRVPaletteAction read FDoInPaletteMode write SetDoInPaletteMode;

    property VSmallStep: Integer read FSmallStep write SetVSmallStep;
    property InplaceEditor: TWinControl read GetInplaceEditor;

    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle;
    {$IFDEF RICHVIEWDEF4}
    property WheelStep: Integer read FWheelStep write FWheelStep default 2;
    {$ENDIF}
    {$IFDEF RVFLATSCROLLBARS}
    property ScrollBarColor: TColor read FScrollBarColor write SetScrollBarColor default clBtnHighlight;
    property ScrollBarStyle: TRVScrollBarStyle read FScrollBarStyle write SetScrollBarStyle default rvssRegular;
    {$ENDIF}
    property FullRedraw: Boolean read FFullRedraw write FFullRedraw;
    property VScrollVisible: Boolean read FVScrollVisible write SetVScrollVisible default True;
    property HScrollVisible: Boolean read FHScrollVisible write SetHScrollVisible default True;
    property VScrollPos: Integer read VPos write SetVScrollPos;
    property HScrollPos: Integer read HPos write SetHPos;
    property VScrollMax: Integer read GetVScrollMax;
    property HScrollMax: Integer read GetHScrollMax;
  public
    { All these public methods are for internal use, except for Create, Destroy
      and ScrollTo }
    { Variables }
    RVPalette: HPALETTE;
    PRVLogPalette: PLogPalette;
    { Create & Destory }
    constructor Create(AOwner: TComponent);override;
    destructor Destroy; override;
    { Size, Scrolling }
    procedure UpdateScrollBars(XS, YS: Integer; UpdateH, UseDNS: Boolean);
    procedure ScrollToNoRedraw(y: Integer);
    procedure ScrollTo(y: Integer);
    { Chosen RVData & Item }
    procedure AssignChosenRVData(RVData: TPersistent; Item: TPersistent);
    procedure SilentReplaceChosenRVData(RVData: TPersistent);
    procedure UnassignChosenRVData(RVData: TPersistent);
    procedure DestroyInplace;
    { Focus }
    function FocusedEx: Boolean;
    procedure SetFocusSilent;
    { Properties }
    property AreaWidth: Integer read XSize;
    property ChosenRVData: TPersistent read GetChosenRVData;
    property ChosenItem: TPersistent read FChosenItem;
    property BiDiMode: TRVBiDiMode read FBiDiMode write SetBiDiModeRV default rvbdUnspecified;
    property UseXPThemes: Boolean read FUseXPThemes write SetUseXPThemes default True;
  end;

const   rvdoALL = [rvdoImages, rvdoComponents, rvdoBullets];
procedure RV_Tag2Y(AControl: TControl);
function RV_GetYByTag(AControl: TControl): Integer;

implementation
uses CRVData, CRVFData, RVItem;

var
  RV_SetScrollProp: function(p1: HWND; index: Integer; newValue: Integer;
    p4: Bool): Bool; stdcall;
  RV_InitializeFlatSB: function(hWnd: HWND): Bool; stdcall;
  RV_UninitializeFlatSB: procedure (hWnd: HWND); stdcall;
  RV_ShowScrollBar: function(hWnd: HWND; wBar: Integer; bShow: BOOL): BOOL; stdcall;
  RV_GetScrollInfo: function(hWnd: HWND; BarFlag: Integer;
    var ScrollInfo: TScrollInfo): BOOL; stdcall;
  RV_GetScrollPos: function(hWnd: HWND; nBar: Integer): Integer; stdcall;
  RV_SetScrollPos: function(hWnd: HWND; nBar, nPos: Integer;
    bRedraw: BOOL): Integer; stdcall;
  RV_SetScrollInfo: function(hWnd: HWND; BarFlag: Integer;
    const ScrollInfo: TScrollInfo; Redraw: BOOL): Integer; stdcall;
  RV_EnableScrollBar: function(hWnd: HWND; wSBflags, wArrows: UINT): BOOL; stdcall;

{------------------------------------------------------}
function RV_GetYByTag(AControl: TControl): Integer;
begin
  if AControl.Tag>10000 then
    Result := 10000
  else if AControl.Tag<-10000 then
    Result := -10000
  else
    Result := AControl.Tag;
end;
{------------------------------------------------------}
procedure RV_Tag2Y(AControl: TControl);
begin
  AControl.Top := RV_GetYByTag(AControl);
end;
{------------------------------------------------------------------------------}
constructor TRVScroller.Create(AOwner: TComponent);
begin
 inherited Create(AOwner);
 FUseXPThemes    := True;
 FSmallStep      := 1;
 KeyboardScroll  := True;
 TabStop         := True;
 FTracking       := True;
 FFullRedraw     := False;
 FVScrollVisible := True;
 FHScrollVisible := True;
 FBorderStyle    := bsNone;
 FScrollFactor   := 1;
 {$IFDEF RICHVIEWDEF4}
 WheelStep       := 2;
 BorderWidth     := 0;
 {$ENDIF}
 {$IFDEF RICHVIEWCBDEF3}
 FDoInPaletteMode := rvpaCreateCopies;
 {$ELSE}
 FDoInPaletteMode := rvpaDoNothing;
 {$ENDIF}
 ControlStyle    := ControlStyle+[csReplicatable]{+[csFramed]};
 {$IFDEF RVFLATSCROLLBARS}
 FScrollBarStyle := rvssRegular;
 FScrollBarColor := clBtnHighlight;
 {$ENDIF}
end;
{------------------------------------------------------------------------------}
destructor TRVScroller.Destroy;
begin
  if RVPalette<>0 then
    DeleteObject(RVPalette);
  FreeLogPalette(PRVLogPalette);
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
begin
  inherited   CreateParams(Params);   //CreateWindow
  with Params do
  begin
    Style := Style or BorderStyles[FBorderStyle];
    if NewStyleControls and Ctl3D and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or WS_EX_CLIENTEDGE;
    end;
    if BiDiMode=rvbdRightToLeft then
      ExStyle := ExStyle or WS_EX_LEFTSCROLLBAR;
    //WindowClass.style := WindowClass.style and not (CS_HREDRAW or CS_VREDRAW);
  end;
  Params.Style := Params.Style or WS_CLIPCHILDREN or WS_HSCROLL or WS_VSCROLL;
  FVDisableNoScroll := False;
end;
{------------------------------------------------------}
procedure  TRVScroller.CreateWnd;
begin
  inherited CreateWnd;
  FSmallStep := GetDefSmallStep;
  AfterCreateWnd1;
  if {$IFDEF RICHVIEWDEF4} not SysLocale.MiddleEast and {$ENDIF}
     Assigned(RV_InitializeFlatSB) then
    RV_InitializeFlatSB(Handle);
  {$IFDEF RVFLATSCROLLBARS}
  UpdateScrollStyle(False);
  UpdateScrollColor(False);
  {$ENDIF}
  CreateThemeHandle;
  AfterCreateWnd2;
end;
{------------------------------------------------------}
procedure TRVScroller.DestroyWnd;
begin
  inherited DestroyWnd;
  FreeThemeHandle;
end;
{------------------------------------------------------}
procedure TRVScroller.UpdateScrollBars(XS, YS: Integer; UpdateH, UseDNS: Boolean);
var
  ScrollInfo: TScrollInfo;
begin
  if FUpdatingScrollBars or not HandleAllocated then
    exit;
  FUpdatingScrollBars := True;
  try
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    if UpdateH then begin
      XSize := XS;
      FHScrollPage := ClientWidth;
      FHScrollMax := XSize-1;
      if HScrollVisible then begin
        ScrollInfo.fMask := SIF_ALL;
        ScrollInfo.nMin := 0;
        ScrollInfo.nMax := FHScrollMax;
        ScrollInfo.nPage := ClientWidth;
        if HPos > ScrollInfo.nMax - (Integer(ScrollInfo.nPage)-1) then
          HPos := ScrollInfo.nMax - (Integer(ScrollInfo.nPage)-1);
        if HPos<0 then HPos := 0;
        ScrollInfo.nPos := HPos;
        ScrollInfo.nTrackPos := 0;
        RV_SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
        end
      else begin
        ScrollInfo.fMask := SIF_ALL;
        RV_GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
        with ScrollInfo do
          if (nMin<>0) or (nMax<>1) or (nPage<>0) or (nPos<>0) then begin
            fMask := SIF_ALL;
            nMin := 0;
            nMax := 1;
            nPage := 2;
            nPos := 0;
            RV_SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
          end;
      end;
    end;
    YSize := YS;
    FVScrollPage := ClientHeight div FSmallStep;
//    if ClientHeight mod FSmallStep >0 then
//      inc(FVScrollPage);
    FVScrollMax := YSize-1;
    if VPos > FVScrollMax - (FVScrollPage-1) then
      VPos := FVScrollMax - (FVScrollPage-1);
    if VPos<0 then VPos := 0;
    if VScrollVisible then begin
      ScrollInfo.cbSize := SizeOf(ScrollInfo);
      ScrollInfo.fMask := SIF_ALL;

      if UseDNS and FVDisableNoScroll then
        ScrollInfo.fMask := ScrollInfo.fMask or SIF_DISABLENOSCROLL;

      ScrollInfo.nMin := 0;
      ScrollInfo.nPage := FVScrollPage;
      ScrollInfo.nMax := FVScrollMax;
      ScrollInfo.nPos := VPos;
      ScrollInfo.nTrackPos := 0;
      RV_SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
      end
    else begin
      ScrollInfo.fMask := SIF_ALL;
      RV_GetScrollInfo(Handle, SB_VERT, ScrollInfo);
      with ScrollInfo do
        if (nMin<>0) or (nMax<>1) or (nPage<>0) or (nPos<>0) then begin
          fMask := SIF_ALL;
          nMin := 0;
          nMax := 1;
          nPage := 2;
          nPos := 0;
          RV_SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
        end;
    end;
  finally
    FUpdatingScrollBars := False;
  end;
end;
{------------------------------------------------------}
procedure TRVScroller.ScrollChildren(dx, dy: Integer);
var i: Integer;
begin
  if (dx=0) and (dy=0) then exit;
  for i:=0 to ControlCount-1 do begin
   if dy<>0 then begin
    Controls[i].Tag := Controls[i].Tag+dy;
    RV_Tag2Y(Controls[i]);
   end;
   if dx<>0 then Controls[i].Left := Controls[i].Left + dx;
  end
end;
{------------------------------------------------------}
procedure TRVScroller.WMHScroll(var Message: TWMHScroll);
begin
  with Message do
    case ScrollCode of
      SB_LINEUP: SetHPos(HPos - FSmallStep*FScrollFactor);
      SB_LINEDOWN: SetHPos(HPos + FSmallStep*FScrollFactor);
      SB_PAGEUP: SetHPos(HPos-ClientWidth);
      SB_PAGEDOWN: SetHPos(HPos+ClientWidth);
      SB_THUMBPOSITION: SetHPos(Pos);
      SB_THUMBTRACK: if FTracking then SetHPos(Pos);
      SB_TOP: SetHPos(0);
      SB_BOTTOM: SetHPos(XSize);
    end;

end;
{------------------------------------------------------}
procedure TRVScroller.WMVScroll(var Message: TWMVScroll);
begin
  with Message do
    case ScrollCode of
      SB_LINEUP: SetVScrollPos(VPos - FScrollFactor);
      SB_LINEDOWN: SetVScrollPos(VPos + FScrollFactor);
      SB_PAGEUP: SetVScrollPos(VPos-(ClientHeight div FSmallStep));
      SB_PAGEDOWN: SetVScrollPos(VPos+(ClientHeight div FSmallStep));
      SB_THUMBPOSITION: SetVScrollPos(Pos);
      SB_THUMBTRACK: if FTracking then SetVScrollPos(Pos);
      SB_TOP: SetVScrollPos(0);
      SB_BOTTOM: SetVScrollPos(YSize);
    end;
end;
{------------------------------------------------------}
procedure TRVScroller.KeyDown(var Key: Word; Shift: TShiftState);
var vScrollNotify, hScrollNotify: Integer;
begin
  inherited KeyDown(Key, Shift);
  if not KeyboardScroll then exit;
  vScrollNotify := -1;
  hScrollNotify := -1;
    case Key of
        VK_UP:
            vScrollNotify := SB_LINEUP;
        VK_PRIOR:
            vScrollNotify := SB_PAGEUP;
        VK_NEXT:
            vScrollNotify := SB_PAGEDOWN;
        VK_DOWN:
            vScrollNotify := SB_LINEDOWN;
        VK_HOME:
            vScrollNotify := SB_TOP;
        VK_END:
            vScrollNotify := SB_BOTTOM;
        VK_LEFT:
            hScrollNotify := SB_LINELEFT;
        VK_RIGHT:
            hScrollNotify := SB_LINERIGHT;
    end;
  if (vScrollNotify <> -1) then
        Perform(WM_VSCROLL, vScrollNotify, 0);
  if (hScrollNotify <> -1) then
        Perform(WM_HSCROLL, hScrollNotify, 0);
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.SetVScrollPos(Value: Integer);
begin
  SetVPos(Value,True);
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.SetVPos(p: Integer; Redraw: Boolean);
var   ScrollInfo: TScrollInfo;
      oldPos: Integer;
      r: TRect;
begin
  if not HandleAllocated then exit;
  OldPos := VPos;
  VPos := p;
  if VScrollVisible then begin
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.nPos := VPos;
    ScrollInfo.fMask := SIF_POS;
    RV_SetScrollInfo(Handle, SB_VERT, ScrollInfo, True);
    RV_GetScrollInfo(Handle, SB_VERT, ScrollInfo);
    VPos := ScrollInfo.nPos;
    end
  else begin
    if VPos > FVScrollMax - (FVScrollPage-1) then
      VPos := FVScrollMax - (FVScrollPage-1);
    if VPos<0 then VPos := 0;
  end;
  r := ClientRect;
  if OldPos-VPos <> 0 then begin
   if not Redraw then begin
     ScrollChildren(0, (OldPos-VPos)*FSmallStep);
     AfterVScroll;
     exit;
   end;
   if FFullRedraw then begin
         ScrollChildren(0, (OldPos-VPos)*FSmallStep);
         Refresh;
       end
   else begin
         ScrollWindowEx(Handle, 0, (OldPos-VPos)*FSmallStep, nil, @r, 0, nil, SW_INVALIDATE {or
                   SW_SCROLLCHILDREN});
         ScrollChildren(0, (OldPos-VPos)*FSmallStep);
         Update;
       end;
   AfterVScroll;
  end;
end;
{------------------------------------------------------}
procedure TRVScroller.SetHPos(p: Integer);
var   ScrollInfo: TScrollInfo;
      oldPos: Integer;
      r: TRect;
begin
  if not HandleAllocated then exit;
  OldPos := HPos;
  HPos := p;
  if HScrollVisible then begin
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.nPos := HPos;
    ScrollInfo.fMask := SIF_POS;
    RV_SetScrollInfo(Handle, SB_HORZ, ScrollInfo, True);
    RV_GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
    HPos := ScrollInfo.nPos;
    end
  else begin
    if HPos > FHScrollMax - (FHScrollPage-1) then
      HPos := FHScrollMax - (FHScrollPage-1);
    if HPos<0 then HPos := 0;
  end;
  r := ClientRect;
  if OldPos-HPos <> 0 then begin
   if FFullRedraw then begin
         ScrollChildren((OldPos-HPos), 0);
         Refresh;
       end
   else begin
         ScrollWindowEx(Handle, (OldPos-HPos), 0,  nil, @r, 0, nil, SW_INVALIDATE{or
                   SW_SCROLLCHILDREN});
         ScrollChildren((OldPos-HPos), 0);
         Update;
       end;
   AfterHScroll;
  end;
end;
{------------------------------------------------------}
procedure TRVScroller.ScrollToNoRedraw(y: Integer);
begin
    SetVPos(y div FSmallStep, False);
end;
{------------------------------------------------------}
procedure TRVScroller.ScrollTo(y: Integer);
begin
    SetVPos(y div FSmallStep, True);
end;
{-------------------------------------------------------}
function TRVScroller.GetVScrollMax: Integer;
var ScrollInfo: TScrollInfo;
begin
  if VScrollVisible then begin
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.nPos := VPos;
    ScrollInfo.fMask := SIF_RANGE or SIF_PAGE;
    RV_GetScrollInfo(Handle, SB_VERT, ScrollInfo);
    Result := ScrollInfo.nMax - Integer(ScrollInfo.nPage)+1;
    end
  else
    Result := FVScrollMax - (FVScrollPage-1);
end;
{-------------------------------------------------------}
function TRVScroller.GetHScrollMax: Integer;
var ScrollInfo: TScrollInfo;
begin
  if HScrollVisible then begin
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.nPos := HPos;
    ScrollInfo.fMask := SIF_RANGE or SIF_PAGE;
    RV_GetScrollInfo(Handle, SB_HORZ, ScrollInfo);
    Result := ScrollInfo.nMax - Integer(ScrollInfo.nPage)+1;
    end
  else
    Result := FHScrollMax - (FHScrollPage-1);
end;
{-------------------------------------------------------}
procedure TRVScroller.SetVScrollVisible(vis: Boolean);
var Changed: Boolean;
begin
  Changed := FVScrollVisible<>vis;
  FVScrollVisible := vis;
  if not HandleAllocated then exit;
  if not (csLoading in ComponentState) then begin
    UpdateScrollBars(XSize, YSize, True, False);
    if FVScrollVisible and Changed and FVDisableNoScroll then begin
      UpdateScrollBars(XSize, YSize, True, True);
      RV_ShowScrollBar(Handle, SB_VERT, True);
    end;
    end
  else
    UpdateScrollBars(XSize, YSize, True, True);
end;
{-------------------------------------------------------}
procedure TRVScroller.SetHScrollVisible(vis: Boolean);
begin
  FHScrollVisible := vis;
  UpdateScrollBars(XSize, YSize, True, True);
end;
{-------------------------------------------------------}
procedure TRVScroller.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  Message.Result := DLGC_WANTARROWS;
end;
{-------------------------------------------------------}
procedure TRVScroller.AfterVScroll;
begin
  if Assigned(FOnVScrolled) then FOnVScrolled(Self);
end;
{-------------------------------------------------------}
procedure TRVScroller.AfterHScroll;
begin
  if Assigned(FOnHScrolled) then FOnHScrolled(Self);
end;
{-------------------------------------------------------}
procedure TRVScroller.SetBorderStyle(const Value: TBorderStyle);
begin
  if FBorderStyle <> Value then begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;
{-------------------------------------------------------}
procedure TRVScroller.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then
    RecreateWnd;
  inherited;
end;
{-------------------------------------------------------}
function TRVScroller.GetDefSmallStep: Integer;
begin
  Result := 10;
end;
{$R-}
function TRVScroller.AllocLogPalette(ColorCount: Integer): PLogPalette;
begin
  Result := PLogPalette(
                GlobalAlloc(GPTR, SizeOf(TLogPalette) + (ColorCount-1) * SizeOf(TPaletteEntry))
                );
  Result^.palVersion := $0300;
  Result^.palNumEntries := ColorCount;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.FreeLogPalette(var lpLogPal: PLogPalette);
begin
  if lpLogPal<>nil then
    GlobalFree(Cardinal(lpLogPal));
  lpLogPal := nil;
end;
{------------------------------------------------------------------------------}
function TRVScroller.GenerateLogPalette: PLogPalette;
var red, green, blue, i: Integer;
    var ColorCount: Integer;
begin
  ColorCount := 6*6*6;
  Result := AllocLogPalette(ColorCount);
  i := 0;
  for blue := 0 to 5 do
    for green := 0 to 5 do
      for red := 0 to 5 do
        with Result^.palPalEntry[i] do begin
          peRed := red*51;
          peGreen := green*51;
          peBlue := blue*51;
          peFlags := 0;
          inc(i);
        end;
end;
{------------------------------------------------------------------------------}
{
function TRVScroller.GetLogPalette(hpal: HPALETTE):PLogPalette;
var ColorCount: Integer;
begin
  Result := nil;
  ColorCount := 0;
  if hpal=0 then
    exit;
  if (GetObject(hpal, 2, @ColorCount)=0) or
     (ColorCount=0) then exit;
  Result := AllocLogPalette(ColorCount);
  GetPaletteEntries(hpal, 0, ColorCount, Result^.palPalEntry);
end;
}
{------------------------------------------------------------------------------}
function IsPaletteMode: Boolean;
var ScreenDC: HDC;
begin
  ScreenDC := CreateCompatibleDC(0);
  Result := (GetDeviceCaps(ScreenDC,RASTERCAPS) and RC_PALETTE)<>0;
  DeleteDC(ScreenDC);
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.SetDoInPaletteMode(Value: TRVPaletteAction);
begin
  if Value<>FDoInPaletteMode then begin
    FDoInPaletteMode := Value;
    UpdatePaletteInfo;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.UpdatePaletteInfo;
begin
  if not (csDesigning in ComponentState) and
    (DoInPaletteMode<>rvpaDoNothing) and
    IsPaletteMode then begin
    if RVPalette=0 then begin
      PRVLogPalette := GenerateLogPalette;
      RVPalette := CreatePalette(PRVLogPalette^);
    end;
    end
  else begin
    if RVPalette<>0 then
      DeleteObject(RVPalette);
    RVPalette := 0;
    FreeLogPalette(PRVLogPalette);
  end;
end;
{------------------------------------------------------------------------------}
function TRVScroller.GetPalette: HPALETTE;
begin
  Result := RVPalette;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.WMQueryNewPalette(var Message: TWMQueryNewPalette);
begin
  inherited;
  Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.WMPaletteChanged(var Message: TWMPaletteChanged);
//var DC: HDC;
begin
  inherited;
  {if (Message.PalChg<>Handle) and (RVPalette<>0) then begin
    DC := GetWindowDC(Handle);
    UpdateColors(DC);
    ReleaseDC(Handle, DC);}
    Invalidate;
{  end;}
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.AssignChosenRVData(RVData: TPersistent; Item: TPersistent);
var Editor: TWinControl;
begin
  if RVData<>FChosenRVData then begin
    DestroyInplace;
    UnassignChosenRVData(FChosenRVData);
    FChosenRVData := RVData;
    FChosenItem   := Item;
  end;
  Editor := InplaceEditor;
  if Editor<>nil then
    Editor.Tag := Editor.Top;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.SilentReplaceChosenRVData(RVData: TPersistent);
begin
  FChosenRVData := RVData;
  if RVData=nil then
    FChosenItem := nil;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.UnassignChosenRVData(RVData: TPersistent);
begin
  if (RVData=FChosenRVData) or
     ((FChosenRVData<>nil) and (TCustomRVData(FChosenRVData).GetRVData=RVData)) then begin
    if (FChosenRVData<>nil) and not (csDestroying in ComponentState) then
      TCustomRVFormattedData(TCustomRVFormattedData(FChosenRVData).GetRVData).Deselect(nil,False);
    FChosenRVData := nil;
    if FChosenItem<>nil then
      TCustomRVItemInfo(FChosenItem).CleanUpChosen;
    FChosenItem   := nil;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.DestroyInplace;
begin
  InplaceEditor.Free;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  //DestroyInplace;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.WMSetFocus(var Message: TWMSetFocus);
  function IsDestroying: Boolean;
  var ctrl: TWinControl;
  begin
    Result := False;
    ctrl := Self;
    while (ctrl<>nil) and (ctrl is TRVScroller) do begin
      Result := (csDestroying in ctrl.ComponentState);
      if Result then
        exit;
      ctrl := ctrl.Parent;
    end;
  end;
begin
  inherited;
  if not IsDestroying and (InplaceEditor<>nil) then
    InplaceEditor.SetFocus;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.SetVSmallStep(Value: Integer);
begin
  FSmallStep := Value;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.SetBiDiModeRV(const Value: TRVBiDiMode);
begin
  if FBiDiMode<>Value then begin
    FBiDiMode := Value;
    RecreateWnd;
  end;
end;
{------------------------------------------------------------------------------}
{$IFDEF RVFLATSCROLLBARS}
procedure TRVScroller.SetScrollBarStyle(const Value: TRVScrollBarStyle);
begin
  if Value<>FScrollBarStyle then begin
    FScrollBarStyle := Value;
    UpdateScrollStyle(True);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.SetScrollBarColor(const Value: TColor);
begin
  if Value<>FScrollBarColor then begin
    FScrollBarColor := Value;
    UpdateScrollColor(True);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.UpdateScrollStyle(Redraw: Boolean);
const
  FSB_FLAT_MODE     = 2;
  FSB_ENCARTA_MODE  = 1;
  FSB_REGULAR_MODE  = 0;
  WSB_PROP_VSTYLE   = $00000100;
  WSB_PROP_HSTYLE   = $00000200;
  Styles: array[TRVScrollBarStyle] of Integer =
  (FSB_REGULAR_MODE, FSB_ENCARTA_MODE, FSB_FLAT_MODE);
begin
  if HandleAllocated and Assigned(RV_SetScrollProp) then begin
    RV_SetScrollProp(Handle, WSB_PROP_HSTYLE, Styles[FScrollBarStyle], Redraw);
    RV_SetScrollProp(Handle, WSB_PROP_VSTYLE, Styles[FScrollBarStyle], Redraw);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.UpdateScrollColor(Redraw: Boolean);
begin
  if HandleAllocated and Assigned(RV_SetScrollProp) then begin
    RV_SetScrollProp(Handle, WSB_PROP_HBKGCOLOR, ColorToRGB(FScrollBarColor), Redraw);
    RV_SetScrollProp(Handle, WSB_PROP_VBKGCOLOR, ColorToRGB(FScrollBarColor), Redraw);
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWDEF4}
function TRVScroller.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
var V: Integer;
begin
  inherited DoMouseWheelDown(Shift, MousePos);
  V := 0;
  if not SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @V, 0) then
    V := 3;
  VScrollPos := VScrollPos+Round(WheelStep*V/3);
  Result := WheelStep<>0;
end;
{------------------------------------------------------------------------------}
function TRVScroller.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
var V: Integer;  
begin
  inherited DoMouseWheelUp(Shift, MousePos);
  V := 0;
  if not SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @V, 0) then
    V := 3;
  VScrollPos := VScrollPos-Round(WheelStep*V/3);
  Result := WheelStep<>0;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure InitSB;
{$IFDEF RVFLATSCROLLBARS}
var
  Handle: THandle;
{$ENDIF}
begin
  // Unfortunately, flat scrollbars do not work properly.
  // They are OK in Win2000 + IE5, but when tested in
  // Win95 + IE4, editor window initially always has
  // wrong and not working scroll bars (they become OK
  // after manual resizing of window).
  // I do not know how to defeat it. The problem appears
  // in DISABLENOSCROLL option.

  RV_InitializeFlatSB := nil;
  RV_UninitializeFlatSB := nil;
  RV_SetScrollProp    := nil;
  @RV_ShowScrollBar  := @ShowScrollBar;
  @RV_GetScrollInfo  := @GetScrollInfo;
  @RV_GetScrollPos   := @GetScrollPos;
  @RV_SetScrollPos   := @SetScrollPos;
  @RV_SetScrollInfo  := @SetScrollInfo;
  @RV_EnableScrollBar:= @EnableScrollBar;

  {$IFDEF RVFLATSCROLLBARS}
  Handle := GetModuleHandle('comctl32.dll');
  if Handle <> 0 then
  begin
    @RV_InitializeFlatSB := GetProcAddress(Handle, 'InitializeFlatSB');
    @RV_UninitializeFlatSB := GetProcAddress(Handle, 'UninitializeFlatSB');
    @RV_SetScrollProp := GetProcAddress(Handle, 'FlatSB_SetScrollProp');
    @RV_ShowScrollBar := GetProcAddress(Handle, 'FlatSB_ShowScrollBar');
    if not Assigned(RV_ShowScrollBar) then
      @RV_ShowScrollBar := @ShowScrollBar;
    @RV_GetScrollInfo := GetProcAddress(Handle, 'FlatSB_GetScrollInfo');
    if not Assigned(RV_GetScrollInfo) then
      @RV_GetScrollInfo := @GetScrollInfo;
    @RV_GetScrollPos := GetProcAddress(Handle, 'FlatSB_GetScrollPos');
    if not Assigned(RV_GetScrollPos) then
      @RV_GetScrollPos := @GetScrollPos;
    @RV_SetScrollPos := GetProcAddress(Handle, 'FlatSB_SetScrollPos');
    if not Assigned(RV_SetScrollPos) then
      @RV_SetScrollPos := @SetScrollPos;
    @RV_SetScrollInfo := GetProcAddress(Handle, 'FlatSB_SetScrollInfo');
    if not Assigned(RV_SetScrollInfo) then
      @RV_SetScrollInfo := @SetScrollInfo;
    @RV_EnableScrollBar := GetProcAddress(Handle, 'FlatSB_EnableScrollBar');
    if not Assigned(RV_EnableScrollBar) then
      @RV_EnableScrollBar := @EnableScrollBar;
  end;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
function TRVScroller.FocusedEx: Boolean;
var Editor: TWinControl;
begin
  Result := False;
  Editor := Self;
  while Editor<>nil do begin
    if Editor.Focused then begin
      Result := True;
      exit;
    end;
    if Editor is TRVScroller then
      Editor := TRVScroller(Editor).InplaceEditor;
  end;
end;
{------------------------------------------------------------------------------}
function TRVScroller.GetInplaceEditor: TWinControl;
begin
  if FChosenRVData=nil then
    Result := nil
  else
    Result := TCustomRVFormattedData(FChosenRVData).GetEditor;
end;
{------------------------------------------------------------------------------}
function TRVScroller.GetChosenRVData: TPersistent;
begin
  if FChosenRVData=nil then
    Result := nil
  else
    Result := TCustomRVData(FChosenRVData).GetRVData;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.SetFocusSilent;
{$IFNDEF RVNESTEDFORMS}
{$IFDEF RICHVIEWCBDEF3}
var Form: TCustomForm;
{$ELSE}
var Form: TForm;
{$ENDIF}
{$ENDIF}
begin
  if not CanFocus then
    exit;
  {$IFDEF RVNESTEDFORMS}
  Windows.SetFocus(Self.Handle);
  {$ELSE}
  Form := GetParentForm(Self);
  if Form<>nil then
    Form.ActiveControl := Self;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.CreateThemeHandle;
begin
  if UseXPThemes and Assigned(RV_IsAppThemed) and RV_IsAppThemed() and
     RV_IsThemeActive() then
    FTheme := RV_OpenThemeData(Handle, Pointer(PChar(RVWCEDIT)))
  else
    FTheme := 0;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.FreeThemeHandle;
begin
  if FTheme<>0 then
    RV_CloseThemeData(FTheme);
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.WMThemeChanged(var Message: TMessage);
begin
  inherited;
  FreeThemeHandle;
  CreateThemeHandle;
  SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
  RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
  Message.Result := 1;
end;

procedure TRVScroller.WMNCPaint(var Message: TMessage);
var
  DC: HDC;
  RC, RW: TRect;
begin
  if FTheme=0 then begin
    inherited;
    exit;
  end;
  if (BorderStyle = bsSingle) then begin
    DefaultHandler(Message);
    DC := GetWindowDC(Handle);
    try
      Windows.GetClientRect(Handle, RC);
      if GetWindowLong(Handle, GWL_STYLE) and WS_VSCROLL <> 0 then
        if (GetWindowLong(Handle, GWL_EXSTYLE) and WS_EX_LEFTSCROLLBAR)<>0 then
          dec(RC.Left, GetSystemMetrics(SM_CXVSCROLL))
        else
          inc(RC.Right, GetSystemMetrics(SM_CXVSCROLL));
      if GetWindowLong(Handle, GWL_STYLE) and WS_HSCROLL <> 0 then
        inc(RC.Bottom, GetSystemMetrics(SM_CYHSCROLL));
      GetWindowRect(Handle, RW);
      MapWindowPoints(0, Handle, RW, 2);
      OffsetRect(RC, -RW.Left, -RW.Top);
      ExcludeClipRect(DC, RC.Left, RC.Top, RC.Right, RC.Bottom);
      OffsetRect(RW, -RW.Left, -RW.Top);
      RV_DrawThemeBackground(FTheme, DC, 0, 0, RW, nil);
      Message.Result := 0;
    finally
      ReleaseDC(Handle, DC);
    end;
    end
  else
    inherited;
end;
{------------------------------------------------------------------------------}
procedure TRVScroller.SetUseXPThemes(const Value: Boolean);
begin
  if FUseXPThemes<>Value then begin
    FUseXPThemes := Value;
    if HandleAllocated then begin
      FreeThemeHandle;
      CreateThemeHandle;
      SetWindowPos(Handle, 0, 0, 0, 0, 0, SWP_FRAMECHANGED or SWP_NOMOVE or SWP_NOSIZE or SWP_NOZORDER);
      RedrawWindow(Handle, nil, 0, RDW_FRAME or RDW_INVALIDATE or RDW_ERASE);
    end;
  end;
end;

procedure TRVScroller.AfterCreateWnd1;
begin
  VPos := 0;
  HPos := 0;
end;

procedure TRVScroller.AfterCreateWnd2;
begin
  UpdateScrollBars(ClientWidth, (ClientHeight div FSmallStep), True, True);
end;
{$IFNDEF RVDONOTUSEDRAGDROP}
{------------------------------------------------------------------------------}
{ OLE drag&drop,  All these functions are called by TRVDropTarget, related to
  IDropTarget.
  They are overriden in TCustomRichViewEdit.                                   }
{------------------------------------------------------------------------------}
{ Drag cursor is entered into the control. From IDropTarget.DragEnter.
  X,Y - client coordinates. Returns - can dropping be accepted                 }
function TRVScroller.OleDragEnter(X,Y: Integer): Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
{ Finished. From IDropTarget.DragLeave                                         }
procedure TRVScroller.OleDragLeave;
begin

end;
{------------------------------------------------------------------------------}
{ Dragging over. From IDropTarget.DragOver.
  X,Y - client coordinates.
  Returns - can dropping be accepted at the given position                     }
function TRVScroller.OleDragOver(X, Y: Integer): Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
{ Dropping. From IDropTarget.Drop. Must return Effects for Drop.               }
function TRVScroller.OleDrop(const DataObj: IDataObject; FMove: Boolean): Integer;
begin
  Result := DROPEFFECT_NONE;
end;
{------------------------------------------------------------------------------}
{ Is the specified format accepted?                                            }
function TRVScroller.OleCanAcceptFormat(Format: Word): Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
{ Informs about destroying linked TRVDropTarget object.                        }
procedure TRVScroller.ReleaseOleDropTargetObject;
begin

end;
{$ENDIF}
{=========================== TRVScrollerInternalIfcObject =====================}
{$IFNDEF RVDONOTUSEDRAGDROP}
constructor TRVScrollerInternalIfcObject.Create(AOwner: TRVScroller);
begin
  inherited Create;
  OleInitialize(nil);
  FOwner := AOwner;
end;
{------------------------------------------------------------------------------}
destructor TRVScrollerInternalIfcObject.Destroy;
begin
  OleUninitialize;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TRVScrollerInternalIfcObject.OwnerDragLeave;
begin
  FOwner.OleDragLeave;
end;
{------------------------------------------------------------------------------}
function TRVScrollerInternalIfcObject.OwnerDragEnter(X,
  Y: Integer): Boolean;
begin
  Result := FOwner.OleDragEnter(X, Y);
end;
{------------------------------------------------------------------------------}
function TRVScrollerInternalIfcObject.OwnerDragOver(X, Y: Integer): Boolean;
begin
  Result := FOwner.OleDragOver(X, Y);
end;
{------------------------------------------------------------------------------}
function TRVScrollerInternalIfcObject.OwnerDrop(
  const DataObj: IDataObject; FMove: Boolean): Integer;
begin
  Result := FOwner.OleDrop(DataObj, FMove);
end;
{------------------------------------------------------------------------------}
function TRVScrollerInternalIfcObject.OwnerCanAcceptFormat(Format: Word): Boolean;
begin
  Result := FOwner.OleCanAcceptFormat(Format);
end;
{------------------------------------------------------------------------------}
procedure TRVScrollerInternalIfcObject.OwnerReleaseDropTargetObject;
begin
  FOwner.ReleaseOleDropTargetObject;
end;
{$ENDIF}

initialization
  InitSB;

end.
