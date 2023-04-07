
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TCustomRVPrintPreview is an ancestor class      }
{       for print previewer.                            }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit CRVPP;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  RVScroll, Printers;
{$I RV_Defs.inc}
{$R CRVPP}
const
  crRVZoomIn  = 102;
  crRVZoomOut = 103;
type
{-----------------------------------------------------------------------}
  TRVZoomMode = (rvzmFullPage, rvzmPageWidth, rvzmCustom);
  TRVClickMode = (rvcmNone, rvcmSwitchZoom);
{-----------------------------------------------------------------------}
  TRVMarginsPen = class (TPen)
    property Style default psClear;
    property Color default clSilver;
  end;

  TCustomRVPrintPreview = class(TRVScroller)
  private
    { Private declarations }
    SavedZoomPercent: Integer;
    FPageNo: Integer;
    FZoomPercent: Integer;
    FZoomMode: TRVZoomMode;
    FPageWidth, FPageHeight: Integer;
    FZoomInCursor: TCursor;
    FZoomOutCursor: TCursor;
    FZoomChanged: TNotifyEvent;
    FMarginsPen: TRVMarginsPen;
    FClickMode: TRVClickMode;
    FPageBorderColor: TColor;
    FShadowColor: TColor;
    FShadowWidth: Integer;
    FPageBorderWidth: Integer;
    FBackgroundMargin: Integer;
    procedure SetZoomPercent(const Value: Integer);
    procedure SetZoomMode(const Value: TRVZoomMode);
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetZoomInCursor(const Value: TCursor);
    procedure SetZoomOutCursor(const Value: TCursor);
    procedure SetMarginsPen(const Value: TRVMarginsPen);
  protected
    { Protected declarations }
    function CanDrawContents: Boolean; dynamic;
    procedure DrawContents(Canvas:TCanvas; const R: TRect); dynamic;
    procedure DrawMargins(Canvas:TCanvas; const R: TRect; PageNo: Integer); virtual;
    function GetPreview100PercentWidth: Integer; dynamic;
    function GetPreview100PercentHeight: Integer; dynamic;
    function GetPageCount: Integer; dynamic;
    procedure Paint; override;
    procedure Loaded; override;
    procedure Click; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    function GetDefSmallStep: Integer; override;
    procedure UpdateCursor;
    procedure SetPageNo(const Value: Integer); virtual;

    property ZoomInCursor: TCursor read FZoomInCursor write SetZoomInCursor default crRVZoomIn;
    property ZoomOutCursor: TCursor read FZoomOutCursor write SetZoomOutCursor default crRVZoomOut;
    property OnZoomChanged: TNotifyEvent read FZoomChanged  write FZoomChanged;
    property MarginsPen: TRVMarginsPen read FMarginsPen write SetMarginsPen;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetZoom(Percent: Integer);
    procedure First;
    procedure Last;
    procedure Next;
    procedure Prev;
    procedure UpdateView;
    property PageNo: Integer read FPageNo write SetPageNo;
    property ZoomPercent:Integer read FZoomPercent write SetZoomPercent;
    property ZoomMode:TRVZoomMode read FZoomMode write SetZoomMode;
    property ClickMode: TRVClickMode read FClickMode write FClickMode default rvcmSwitchZoom;
    property PageBorderColor: TColor read FPageBorderColor write FPageBorderColor default clHighlight;
    property PageBorderWidth: Integer read FPageBorderWidth write FPageBorderWidth default 2;
    property ShadowColor: TColor read FShadowColor write FShadowColor default cl3DDkShadow;
    property ShadowWidth: Integer read FShadowWidth write FShadowWidth default 4;
    property BackgroundMargin: Integer read FBackgroundMargin write FBackgroundMargin default 20;
  published
    property Color default clBtnShadow;
  end;

implementation
uses RVStr;

{========================== TCustomRVPrintPreview ============================}
constructor TCustomRVPrintPreview.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Screen.Cursors[crRVZoomIn] := LoadCursor(hInstance,RVRC_ZOOMIN_CURSOR);
  Screen.Cursors[crRVZoomOut] := LoadCursor(hInstance,RVRC_ZOOMOUT_CURSOR);
  BorderStyle := bsSingle;
  Width  := 100;
  Height := 100;
  PageNo := 1;
  FullRedraw := False;
  ZoomInCursor := crRVZoomIn;
  ZoomOutCursor := crRVZoomOut;
  SavedZoomPercent := 50;
  FZoomPercent := 100;
  FClickMode := rvcmSwitchZoom;
  FMarginsPen := TRVMarginsPen.Create;
  FMarginsPen.Style := psClear;
  FMarginsPen.Color := clSilver;
  FScrollFactor := 10;
  Color := clBtnShadow;
  ShadowColor := cl3DDkShadow;
  PageBorderColor := clHighlight;
  ShadowWidth := 4;
  PageBorderWidth := 2;
  BackgroundMargin := 20;
end;
{-----------------------------------------------------------------------}
destructor TCustomRVPrintPreview.Destroy;
begin
  FMarginsPen.Free;
  inherited Destroy;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.Loaded;
begin
  inherited Loaded;
  UpdateView;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.Click;
begin
  inherited;
  case ClickMode of
    rvcmSwitchZoom:
      begin
        ZoomMode := rvzmCustom;
        if ZoomPercent=100 then
          ZoomPercent := SavedZoomPercent
        else
          ZoomPercent := 100;
      end;
  end;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.SetMarginsPen(const Value: TRVMarginsPen);
begin
  FMarginsPen.Assign(Value);
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.WMSize(var Message: TWMSize);
begin
  UpdateView;
end;
{-----------------------------------------------------------------------}
function TCustomRVPrintPreview.GetDefSmallStep: Integer;
begin
  Result := 1;
end;
{-----------------------------------------------------------------------}
function TCustomRVPrintPreview.GetPreview100PercentWidth: Integer;
begin
  Result := -1;
end;
{-----------------------------------------------------------------------}
function TCustomRVPrintPreview.GetPreview100PercentHeight: Integer;
begin
  Result := -1;
end;
{-----------------------------------------------------------------------}
function TCustomRVPrintPreview.GetPageCount: Integer;
begin
  Result := 1;
end;
{-----------------------------------------------------------------------}
function TCustomRVPrintPreview.CanDrawContents: Boolean;
begin
  Result := True;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.DrawContents(Canvas:TCanvas; const R: TRect);
begin
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.DrawMargins(Canvas: TCanvas;
  const R: TRect; PageNo: Integer);
begin
  Canvas.Pen := MarginsPen;
  Canvas.Brush.Style := bsClear;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.Paint;
var xoff,yoff: Integer;
    w,h: Integer;
    r: TRect;
    OldPalette: HPALETTE;
    MemBitmap, OldBitmap: HBITMAP;
    MemDC: HDC;
    canv: TCanvas;
//    DCIdx: Integer;
begin
  with ClientRect do
    MemBitmap := CreateCompatibleBitmap(Canvas.Handle, Right-Left, Bottom-Top);
  MemDC := CreateCompatibleDC(0);
  OldBitmap := SelectObject(MemDC, MemBitmap);
  if RVPalette<>0 then begin
    OldPalette := SelectPalette(MemDC, RVPalette, False);
    RealizePalette(MemDC);
    end
  else
    OldPalette := 0;
  canv := TCanvas.Create;
  canv.Handle := MemDC;
  try
    with canv do begin
      Brush.Color := Self.Color;
      Pen.Color := Self.Color;
      FillRect(ClientRect);
      if CanDrawContents then begin
        w := ClientWidth;
        if XSize>w then w := XSize;
        h := ClientHeight;
        if YSize>h then h := YSize;
        xoff := (w-FPageWidth) div 2;
        yoff := (h-FPageHeight) div 2;
        if ShadowWidth>0 then begin
          r := Bounds(xoff,yoff,FPageWidth,FPageHeight);
          OffsetRect(r,-HPos+ShadowWidth,-VPos+ShadowWidth);
          Brush.Color := ShadowColor;
          FillRect(r);
        end;
        r := Bounds(xoff,yoff,FPageWidth,FPageHeight);
        OffsetRect(r,-HPos,-VPos);
        //DCIdx := SaveDC(canv.Handle);
        with r do
          IntersectClipRect(canv.Handle,Left,Top,Right,Bottom);
        DrawContents(canv, r);
        //RestoreDC(canv.Handle,DCIdx);
        SelectClipRgn(canv.Handle,0);
        Pen.Style := psSolid;
        Pen.Width := PageBorderWidth;
        Pen.Color := PageBorderColor;
        Brush.Color := clNone;
        Brush.Style := bsClear;
        with R do
          Rectangle(Left, Top, Right, Bottom);
        if MarginsPen.Style<>psClear then
          DrawMargins(canv, r, PageNo);
      end;
    end;
    with ClientRect do
      BitBlt(Canvas.Handle, Left, Top, Right-Left, Bottom-Top, MemDC, 0, 0, SRCCOPY);
  finally
    if RVPalette<>0 then
      SelectPalette(MemDC, OldPalette, True);
    SelectObject(MemDC, OldBitmap);
    canv.Handle := 0;
    canv.Free;
    DeleteDC(MemDC);
    DeleteObject(MemBitmap);
  end;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.SetPageNo(const Value: Integer);
begin
  FPageNo := Value;
  Invalidate;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.SetZoomMode(const Value: TRVZoomMode);
begin
  FZoomMode := Value;
  UpdateView;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.SetZoomPercent(const Value: Integer);
var isnew: Boolean;
begin
  isnew := Value<>FZoomPercent;
  if (Value=100) and (FZoomPercent<>100) then
    SavedZoomPercent := FZoomPercent;
  FZoomPercent := Value;
  if not CanDrawContents then exit;
  FPageWidth := MulDiv(GetPreview100PercentWidth, FZoomPercent, 100);
  FPageHeight := MulDiv(GetPreview100PercentHeight, FZoomPercent, 100);
  Invalidate;
  case ZoomMode of
    rvzmPageWidth:
      UpdateScrollBars(10, FPageHeight+BackgroundMargin*2, True, True);
    rvzmFullPage:
      UpdateScrollBars(10, 10, True, True);
    rvzmCustom:
      UpdateScrollBars(FPageWidth+BackgroundMargin*2, FPageHeight+BackgroundMargin*2, True, True);
  end;
  UpdateCursor;
  if isnew and Assigned(FZoomChanged) then FZoomChanged(Self);
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.UpdateView;
var ZP,ZP2: Integer;
begin
  if not CanDrawContents then exit;
  case ZoomMode of
    rvzmPageWidth:
      begin
        ZoomPercent := MulDiv(ClientWidth-BackgroundMargin*2, 100, GetPreview100PercentWidth);
      end;
    rvzmFullPage:
      begin
        ZP := MulDiv(Width-GetSystemMetrics(SM_CXHSCROLL)- BackgroundMargin*2, 100, GetPreview100PercentWidth);
        ZP2 := MulDiv(Height-GetSystemMetrics(SM_CYVSCROLL)-BackgroundMargin*2, 100, GetPreview100PercentHeight);
        if ZP2<ZP then
          ZoomPercent := ZP2
        else
          ZoomPercent := ZP;
      end;
    rvzmCustom:
      ZoomPercent := ZoomPercent;
  end;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.First;
begin
  PageNo := 1;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.Last;
begin
  if not CanDrawContents then exit;
  PageNo := GetPageCount;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.Next;
begin
  if not CanDrawContents then exit;
  if PageNo<GetPageCount then
    PageNo := PageNo+1;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.Prev;
begin
  if PageNo>1 then
    PageNo := PageNo-1;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.SetZoomInCursor(const Value: TCursor);
begin
  FZoomInCursor := Value;
  UpdateCursor;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.SetZoomOutCursor(const Value: TCursor);
begin
  FZoomOutCursor := Value;
  UpdateCursor;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.UpdateCursor;
var ZP: Integer;
begin
  case ClickMode of
    rvcmSwitchZoom:
      begin
        if ZoomPercent=100 then
          ZP := SavedZoomPercent
        else
          ZP := 100;
        if ZoomPercent<ZP then
          Self.Cursor := FZoomInCursor
        else
          Self.Cursor := FZoomOutCursor;
      end;
    rvcmNone:
      begin
        Self.Cursor := crDefault;
      end;
  end;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.SetZoom(Percent: Integer);
begin
  FZoomMode := rvzmCustom;
  ZoomPercent := Percent;
end;
{-----------------------------------------------------------------------}
procedure TCustomRVPrintPreview.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  SetFocus;
end;

end.
