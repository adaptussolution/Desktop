
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TCustomRVPrint: ancestor of TRVPrint and        }
{       TRVReportHelper.                                }
{       TRVPrint: component for printing                }
{       RichView.                                       }
{       (registered on "RichView" page of               }
{       the Component Palette)                          }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit PtblRV;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVScroll, RichView, RVItem, RVStyle, Printers, CommDlg, DLines, RVFuncs,
  CRVData, CRVFData, RVRVData, PtRVData;

{$I RV_Defs.inc}

type
  TCustomRVPrint = class;
  TRVPrint = class;  
  {------------------------------------------------------------}
  TRVPrintComponentEvent =
    procedure (Sender: TCustomRVPrint;
               PrintMe: TControl;
               var ComponentImage: TBitmap) of object;
  {------------------------------------------------------------}
  TRVPrintingEvent = procedure (Sender: TCustomRichView; PageCompleted: Integer; Step:TRVPrintingStep) of object;
  {------------------------------------------------------------}
  TRVPagePrepaintEvent = procedure (Sender: TRVPrint; PageNo: Integer;
                               Canvas: TCanvas; Preview: Boolean;
                               PageRect, PrintAreaRect: TRect) of object;
  {------------------------------------------------------------}
  TCustomPrintableRV = class(TCustomRichView)
    private
      FRVPrint: TCustomRVPrint;
    public
      constructor Create(AOwner: TComponent); override;
      {$IFNDEF RVDONOTUSERVF}
      procedure ApplyLayoutInfo (Layout: TRVLayoutInfo); override;
      {$ENDIF}
      function FormatPages: Integer;
      procedure DrawPage(pgNo: Integer; Canvas: TCanvas; Preview, Correction: Boolean);
      property RVPrint: TCustomRVPrint read FRVPrint write FRVPrint;
  end;
  {------------------------------------------------------------}
  TPrintableRV = class(TCustomPrintableRV)
    private
      procedure DoOnPrinting(PageCompleted: Integer; Step:TRVPrintingStep);
    protected
      function GetDataClass: TRichViewRVDataClass; override;
    public
      FMirrorMargins: Boolean;
      FLeftMarginMM, FRightMarginMM, FTopMarginMM, FBottomMarginMM: Integer;
      procedure PrintPages(firstPgNo, lastPgNo: Integer;
                         const Title: String;
                         Copies: Integer; Collate: Boolean);
      procedure Print(const Title: String;
                    Copies: Integer; Collate: Boolean);
      procedure ContinuousPrint;
  end;
  {------------------------------------------------------------}
  TCustomRVPrint = class(TComponent)
    private
      { Private declarations }
      FPreviewCorrection: Boolean;
      FOnPrintComponent: TRVPrintComponentEvent;
      function GetPagesCount: Integer;
      function GetTransparentBackground: Boolean;
      procedure SetTransparentBackground(const Value: Boolean);
      function GetPreview100PercentHeight: Integer;
      function GetPreview100PercentWidth: Integer;
      function GetColorMode: TRVColorMode;
      procedure SetColorMode(const Value: TRVColorMode);
      function GetIsDestinationReady: Boolean;
    protected
      { Protected declarations }
      procedure Loaded; override;
      function CreateRichView: TCustomPrintableRV; virtual;
    public
      { Public declarations }
      rv: TCustomPrintableRV;
      Ready: Boolean;
      StartAt,EndAt: Integer;
      FormattingID: Integer;
      constructor Create(AOwner: TComponent); override;
      procedure Clear;
      procedure UpdatePaletteInfo;
      procedure GetFirstItemOnPage(PageNo: Integer; var ItemNo, OffsetInItem: Integer);
      procedure GetFirstItemOnPageEx(PageNo: Integer; var ItemNo, OffsetInItem, ExtraData: Integer);
      procedure DrawPreview(pgNo: Integer; Canvas:  TCanvas; const PageRect: TRect);
      procedure DrawMarginsRect(Canvas:  TCanvas; const PageRect: TRect; PageNo: Integer);
      {$IFNDEF RVDONOTUSERVF}
      function SavePageAsRVF(Stream: TStream; PageNo: Integer): Boolean;
      {$ENDIF}
      property PagesCount: Integer read GetPagesCount;
      property Preview100PercentWidth: Integer read GetPreview100PercentWidth;
      property Preview100PercentHeight: Integer read GetPreview100PercentHeight;
      property IsDestinationReady: Boolean read GetIsDestinationReady;
    published
      { Published declarations }
      property PreviewCorrection: Boolean read FPreviewCorrection  write FPreviewCorrection;
      property OnPrintComponent: TRVPrintComponentEvent read FOnPrintComponent write FOnPrintComponent;
      property TransparentBackground: Boolean read GetTransparentBackground write SetTransparentBackground default False;
      property ColorMode: TRVColorMode read GetColorMode write SetColorMode default rvcmPrinterColor;
  end;

  TRVPrint = class(TCustomRVPrint)
    private
      { Private declarations }
      FOnFormatting, FOnPrinting: TRVPrintingEvent;
      FOnPagePrepaint, FOnPagePostPaint: TRVPagePrepaintEvent;
      FClipMargins: Boolean;
      function GetLM: Integer;
      function GetRM: Integer;
      function GetTM: Integer;
      function GetBM: Integer;
      procedure SetLM(mm: Integer);
      procedure SetRM(mm: Integer);
      procedure SetTM(mm: Integer);
      procedure SetBM(mm: Integer);
      function GetMirrorMargins: Boolean;
      procedure SetMirrorMargins(const Value: Boolean);
      function GetFooterY: Integer;
      function GetHeaderY: Integer;
      procedure SetFooterY(const Value: Integer);
      procedure SetHeaderY(const Value: Integer);
    protected
      { Protected declarations }
      function CreateRichView: TCustomPrintableRV; override;
    public
      { Public declarations }
      constructor Create(AOwner: TComponent); override;
      procedure AssignSource(PrintMe: TCustomRichView);
      procedure SetHeader(RVData: TCustomRVFormattedData);
      procedure SetFooter(RVData: TCustomRVFormattedData);
      function FormatPages(PrintOptions:TRVDisplayOptions): Integer;
      procedure PrintPages(firstPgNo, lastPgNo: Integer; Title: String;
                         Copies: Integer; Collate: Boolean);
      procedure Print(Title: String; Copies: Integer; Collate: Boolean);
      procedure ContinuousPrint;
      procedure MakePreview(pgNo: Integer; bmp: TBitmap);
      procedure MakeScaledPreview(pgNo: Integer; bmp: TBitmap);
      function GetHeaderRect: TRect;
      function GetFooterRect: TRect;
    published
      { Published declarations }
      property ClipMargins:   Boolean read FClipMargins write FClipMargins default False;
      property MirrorMargins: Boolean read GetMirrorMargins write SetMirrorMargins default False;
      property LeftMarginMM:  Integer read GetLM write SetLM;
      property RightMarginMM: Integer read GetRM write SetRM;
      property TopMarginMM:   Integer read GetTM write SetTM;
      property BottomMarginMM:Integer read GetBM write SetBM;
      property FooterYMM:     Integer read GetFooterY write SetFooterY default 10;
      property HeaderYMM:     Integer read GetHeaderY write SetHeaderY default 10;
      property OnFormatting: TRVPrintingEvent read FOnFormatting write FOnFormatting;
      property OnSendingToPrinter: TRVPrintingEvent read FOnPrinting write FOnPrinting;
      property OnPagePrepaint: TRVPagePrepaintEvent read FOnPagePrepaint write FOnPagePrepaint;
      property OnPagePostpaint: TRVPagePrepaintEvent read FOnPagePostpaint write FOnPagePostpaint;
  end;

function RV_GetPrinterDC: HDC;
implementation
{==============================================================================}
type
  TPrinterDevice = class
    Driver, Device, Port: String;
  end;

function RV_GetPrinterDC: HDC;
var ADevice, ADriver, APort: array[0..79] of Char;
    ADeviceMode: THandle;
    DevMode: PDeviceMode;
begin
  Printer.GetPrinter(ADevice,ADriver,APort,ADeviceMode);
  if ADeviceMode<>0 then
    DevMode := PDeviceMode(GlobalLock(ADeviceMode))
  else
    DevMode := nil;
  Result := CreateDC(ADriver, ADevice, APort, DevMode);
  if ADeviceMode<>0 then
    GlobalUnlock(ADeviceMode);
end;
{=============================== TCustomRVPrint ===============================}
constructor TCustomRVPrint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  rv := CreateRichView;
  rv.FRVPrint := Self;
  if not (csDesigning in ComponentState) then
    rv.Parent := TWinControl(Self.Owner);
  PreviewCorrection := True;
  ColorMode := rvcmPrinterColor;
end;
{------------------------------------------------------------------------------}
function TCustomRVPrint.CreateRichView: TCustomPrintableRV;
begin
  Result := nil;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVPrint.Clear;
begin
  rv.Clear;
  Ready := False;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVPrint.GetFirstItemOnPageEx(PageNo: Integer; var ItemNo, OffsetInItem, ExtraData: Integer);
var dino, dioffs, Part: Integer;
begin
  dino := TPrintableRVData(rv.RVData).Pages[PageNo-1].StartDrawItemNo;
  Part := TPrintableRVData(rv.RVData).Pages[PageNo-1].StartPart;
  if Part>0 then
    ExtraData := (rv.RVData.DrawItems[dino] as TRVMultiDrawItemInfo).PartsList[Part-1].GetSoftPageBreakInfo
  else
    ExtraData := -1;

  if rv.GetItemStyle(rv.RVData.DrawItems[dino].ItemNo)<0 then
    dioffs := 0
  else
    dioffs := 1;
  rv.RVData.DrawItem2Item(dino, dioffs, ItemNo, OffsetInItem);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVPrint.GetFirstItemOnPage(PageNo: Integer; var ItemNo,
  OffsetInItem: Integer);
var ExtraData: Integer;
begin
  GetFirstItemOnPageEx(PageNo, ItemNo, OffsetInItem, ExtraData);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERVF}
function TCustomRVPrint.SavePageAsRVF(Stream: TStream; PageNo: Integer): Boolean;
var Layout: TRVLayoutInfo;
begin
  Layout := rv.CreateLayoutInfo;
  Result := TCustomMainPtblRVData(rv.RVData).SavePageAsRVF(Stream, PageNo, rv.Color, rv.Background, Layout);
  Layout.Free;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TCustomRVPrint.GetPagesCount: Integer;
begin
  Result := TPrintableRVData(rv.RVData).Pages.Count;
end;
{------------------------------------------------------------------------------}
function TCustomRVPrint.GetTransparentBackground: Boolean;
begin
  Result := TPrintableRVData(rv.RVData).Transparent;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVPrint.SetTransparentBackground(const Value: Boolean);
begin
  TPrintableRVData(rv.RVData).Transparent := Value;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVPrint.Loaded;
begin
  inherited Loaded;
  UpdatePaletteInfo;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVPrint.UpdatePaletteInfo;
begin
  rv.UpdatePaletteInfo;
end;
{------------------------------------------------------------------------------}
function TCustomRVPrint.GetIsDestinationReady: Boolean;
begin
  Result := TCustomMainPtblRVData(rv.RVData).FIsDestinationReady;
end;
{------------------------------------------------------------------------------}
function TCustomRVPrint.GetPreview100PercentHeight: Integer;
begin
  with TCustomMainPtblRVData(rv.RVData) do
    Result := MulDiv(rv.ClientHeight+TmpTM+TmpBM,
                     PrnSaD.ppiyScreen, PrnSaD.ppiyDevice);
end;
{------------------------------------------------------------------------------}
function TCustomRVPrint.GetPreview100PercentWidth: Integer;
begin
  with TCustomMainPtblRVData(rv.RVData) do
    Result := MulDiv(rv.ClientWidth+TmpLM+TmpRM,
                   PrnSaD.ppixScreen, PrnSaD.ppixDevice);
end;
{------------------------------------------------------------------------------}
function TCustomRVPrint.GetColorMode: TRVColorMode;
begin
  Result := TCustomMainPtblRVData(rv.RVData).ColorMode;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVPrint.SetColorMode(const Value: TRVColorMode);
begin
  TCustomMainPtblRVData(rv.RVData).ColorMode := Value;
end;
{------------------------------------------------------------------------------}
procedure TCustomRVPrint.DrawPreview(pgNo: Integer; Canvas: TCanvas;
  const PageRect: TRect);
begin
   SetMapMode(Canvas.Handle,MM_TEXT);
   Canvas.Brush.Color := clWhite;
   Canvas.Pen.Color := clWhite;
   Canvas.FillRect(PageRect);
   SetMapMode(Canvas.Handle,MM_ANISOTROPIC);
   with TCustomMainPtblRVData(rv.RVData) do
     SetWindowExtEx(Canvas.Handle,
                    rv.ClientWidth +TmpLM+TmpRM,
                    rv.ClientHeight+TmpTM+TmpBM,nil);
   with PageRect do begin
     SetViewportExtEx(Canvas.Handle, Right-Left, Bottom-Top,nil);
     SetViewportOrgEx(Canvas.Handle,Left,Top,nil);
   end;
   Canvas.Font.PixelsPerInch := TCustomMainPtblRVData(rv.RVData).PrnSaD.ppiyDevice;
   rv.DrawPage(pgNo, Canvas, True, PreviewCorrection);
   SetMapMode(Canvas.Handle,MM_TEXT);
   SetViewportOrgEx(Canvas.Handle,0,0,nil);
end;
{------------------------------------------------------------------------------}
procedure TCustomRVPrint.DrawMarginsRect(Canvas: TCanvas; const PageRect: TRect; PageNo: Integer);
var FullWidth, FullHeight: Integer;
    RectWidth, RectHeight, LM: Integer;
begin
  with TCustomMainPtblRVData(rv.RVData) do begin
     FullWidth  := rv.ClientWidth+TmpLM+TmpRM;
     FullHeight := rv.ClientHeight+TmpTM+TmpBM;
     RectWidth  := PageRect.Right-PageRect.Left;
     RectHeight := PageRect.Bottom-PageRect.Top;
     LM := TCustomPrintableRVData(rv.RVData).GetPrintableAreaLeft(PageNo);
     Canvas.Rectangle(
       PageRect.Left+MulDiv(LM,RectWidth,FullWidth),
       PageRect.Top+MulDiv(TmpTM,RectHeight,FullHeight),
       PageRect.Left+MulDiv(LM+rv.ClientWidth,RectWidth,FullWidth),
       PageRect.Top+MulDiv(TmpTM+rv.ClientHeight,RectHeight,FullHeight)
      );
  end;
end;
{=============================== TRVPrint =====================================}
constructor TRVPrint.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  LeftMarginMM   := 20;
  RightMarginMM  := 20;
  TopMarginMM    := 20;
  BottomMarginMM := 20;
end;
{------------------------------------------------------------------------------}
function TRVPrint.CreateRichView: TCustomPrintableRV;
begin
  Result := TPrintableRV.Create(Self);
end;
{------------------------------------------------------------------------------}
function TRVPrint.GetMirrorMargins: Boolean;
begin
  Result := TPrintableRV(rv).FMirrorMargins;
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.SetMirrorMargins(const Value: Boolean);
begin
  TPrintableRV(rv).FMirrorMargins := Value;
end;
{------------------------------------------------------------------------------}
function TRVPrint.GetLM: Integer;
begin
   GetLM := TPrintableRV(rv).FLeftMarginMM;
end;
{------------------------------------------------------------------------------}
function  TRVPrint.GetRM: Integer;
begin
   GetRM := TPrintableRV(rv).FRightMarginMM;
end;
{------------------------------------------------------------------------------}
function  TRVPrint.GetTM: Integer;
begin
   GetTM := TPrintableRV(rv).FTopMarginMM;
end;
{------------------------------------------------------------------------------}
function  TRVPrint.GetBM: Integer;
begin
   GetBM := TPrintableRV(rv).FBottomMarginMM;
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.SetLM(mm: Integer);
begin
   TPrintableRV(rv).FLeftMarginMM := mm;
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.SetRM(mm: Integer);
begin
   TPrintableRV(rv).FRightMarginMM := mm;
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.SetTM(mm: Integer);
begin
   TPrintableRV(rv).FTopMarginMM := mm;
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.SetBM(mm: Integer);
begin
   TPrintableRV(rv).FBottomMarginMM := mm;
end;
{------------------------------------------------------------------------------}
function TRVPrint.GetFooterY: Integer;
begin
  Result := TPrintableRVData(rv.RVData).FooterY;
end;
{------------------------------------------------------------------------------}
function TRVPrint.GetHeaderY: Integer;
begin
  Result := TPrintableRVData(rv.RVData).HeaderY;
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.SetFooterY(const Value: Integer);
begin
  TPrintableRVData(rv.RVData).FooterY := Value;
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.SetHeaderY(const Value: Integer);
begin
  TPrintableRVData(rv.RVData).HeaderY := Value;
end;
{------------------------------------------------------------------------------}
function TRVPrint.FormatPages(PrintOptions:TRVDisplayOptions): Integer;
begin
  inc(FormattingID);
  if FormattingID=10000 then
    FormattingID := 0;
  Result := rv.FormatPages;
  Ready := True;
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.Print(Title: String; Copies: Integer; Collate: Boolean);
begin
  TPrintableRV(rv).Print(Title, Copies, Collate);
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.PrintPages(firstPgNo, lastPgNo: Integer; Title: String;
                              Copies: Integer; Collate: Boolean);
begin
  TPrintableRV(rv).PrintPages(firstPgNo, lastPgNo, Title, Copies, Collate);
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.ContinuousPrint;
begin
  TPrintableRV(rv).ContinuousPrint;
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.AssignSource(PrintMe: TCustomRichView);
begin
  rv.RVData.ShareItemsFrom(PrintMe.RVData);
  rv.RVData.State := rv.RVData.State+[rvstSkipFormatting];
  try
    rv.LeftMargin   := PrintMe.LeftMargin;
    rv.RightMargin  := PrintMe.RightMargin;
    rv.TopMargin    := 0;
    rv.BottomMargin := 0;
    TPrintableRVData(rv.RVData).FTopMarginPix    := PrintMe.TopMargin;
    TPrintableRVData(rv.RVData).FBottomMarginPix := PrintMe.BottomMargin;
    rv.Style := PrintMe.Style;
    rv.DoInPaletteMode := PrintMe.DoInPaletteMode;
    rv.BackgroundBitmap := PrintMe.BackgroundBitmap;
    rv.BackgroundStyle := PrintMe.BackgroundStyle;
    rv.Color := PrintMe.Color;
    rv.BiDiMode := PrintMe.BiDiMode;
  finally
    rv.RVData.State := rv.RVData.State-[rvstSkipFormatting];
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.SetHeader(RVData: TCustomRVFormattedData);
begin
  TPrintableRVData(rv.RVData).Header.Free;
  TPrintableRVData(rv.RVData).Header := nil;
  if RVData<>nil then
    TPrintableRVData(rv.RVData).Header := TRVHeaderFooterRVData.Create(rv, RVData, rv.RVData as TCustomPrintableRVData);
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.SetFooter(RVData: TCustomRVFormattedData);
begin
  TPrintableRVData(rv.RVData).Footer.Free;
  TPrintableRVData(rv.RVData).Footer := nil;
  if RVData<>nil then
    TPrintableRVData(rv.RVData).Footer := TRVHeaderFooterRVData.Create(rv, RVData, rv.RVData as TCustomPrintableRVData);
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.MakeScaledPreview(pgNo: Integer; bmp: TBitmap);
begin
  DrawPreview(pgNo, bmp.Canvas, Rect(0,0,bmp.Width, bmp.Height));
end;
{------------------------------------------------------------------------------}
procedure TRVPrint.MakePreview(pgNo: Integer; bmp: TBitmap);
var w,h: Integer;
begin
   w := GetPreview100PercentWidth;
   h := GetPreview100PercentHeight;

   if bmp.Width <> w then bmp.Width := w;
   if bmp.Height <> h then bmp.Height := h;
   MakeScaledPreview(pgNo,bmp);
end;
{------------------------------------------------------------------------------}
function TRVPrint.GetFooterRect: TRect;
begin
  if TPrintableRVData(rv.RVData).Footer=nil then
    Result := Rect(0,0,0,0)
  else
    with TPrintableRVData(rv.RVData).Footer do
      Result := Bounds(Left+DX,Top+DY,Width,DocumentHeight);
end;
{------------------------------------------------------------------------------}
function TRVPrint.GetHeaderRect: TRect;
begin
  if TPrintableRVData(rv.RVData).Header=nil then
    Result := Rect(0,0,0,0)
  else
    with TPrintableRVData(rv.RVData).Header do
      Result := Bounds(Left+DX,Top+DY,Width,DocumentHeight);
end;
{======================== TCustomPrintableRV ==================================}
constructor TCustomPrintableRV.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible := False;
  Flags := Flags - [rvflUseJumps] + [rvflPrinting,rvflShareContents,rvflAllowCustomDrawItems];
  TopMargin    := 0;     // do not change!
  BottomMargin := 0;  // do not change!
  BorderStyle  := bsNone;
end;
{------------------------------------------------------------------------------}
function TCustomPrintableRV.FormatPages: Integer;
begin
  RVData.State := RVData.State+[rvstSkipformatting];
  try
    VScrollVisible := False;
    HScrollVisible := False;
  finally
    RVData.State := RVData.State-[rvstSkipformatting];
  end;
  Result := TPrintableRVData(RVData).FormatPages;
end;
{------------------------------------------------------------------------------}
procedure TCustomPrintableRV.DrawPage(pgNo: Integer; Canvas: TCanvas;
  Preview, Correction: Boolean);
begin
  TPrintableRVData(RVData).DrawPage(pgNo, Canvas, Preview, Correction);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERVF}
procedure TCustomPrintableRV.ApplyLayoutInfo(Layout: TRVLayoutInfo);
begin
  inherited;
  if Layout.FirstParaAborted<>0 then begin
    Include(RVData.State, rvstFirstParaAborted);
    TCustomMainPtblRVData(RVData).FFirstParaListNo := Layout.FirstMarkerListNo;
    TCustomMainPtblRVData(RVData).FFirstParaLevel := Layout.FirstMarkerLevel;
    end
  else begin
    Exclude(RVData.State, rvstFirstParaAborted);
    TCustomMainPtblRVData(RVData).FFirstParaListNo := -1;
    TCustomMainPtblRVData(RVData).FFirstParaLevel := -1;
  end;
  if Layout.LastParaAborted<>0 then
    Include(RVData.State, rvstLastParaAborted)
  else
    Exclude(RVData.State, rvstLastParaAborted);
end;
{$ENDIF}
{============================= TPrintableRV ===================================}
function TPrintableRV.GetDataClass: TRichViewRVDataClass;
begin
  Result := TPrintableRVData;
end;
{------------------------------------------------------------------------------}
procedure TPrintableRV.ContinuousPrint;
var i: Integer;
begin
  for i := 1 to TPrintableRVData(RVData).Pages.Count do begin
    if i<>1 then
      Printer.NewPage;
    DrawPage(i, Printer.Canvas,False,False);
  end;
end;
{------------------------------------------------------------------------------}
procedure TPrintableRV.DoOnPrinting(PageCompleted: Integer; Step:TRVPrintingStep);
begin
  if Assigned(TRVPrint(RVPrint).FOnPrinting) then
    TRVPrint(RVPrint).FOnPrinting(Self, PageCompleted, Step);
end;
{------------------------------------------------------------------------------}
procedure TPrintableRV.PrintPages(firstPgNo, lastPgNo: Integer;
                                  const Title: String;
                                  Copies: Integer; Collate: Boolean);
var i,copyno: Integer;
    PrinterCopies: Integer;
begin
   if lastPgNo<firstPgNo then
     exit;
   FRVPrint.StartAt := 0;
   DoOnPrinting(0, rvpsStarting);
   Printer.Title := Title;
   PrinterCopies := Printer.Copies; { storing }
   if pcCopies in Printer.Capabilities then
     begin
       Printer.Copies := Copies;
                                 // Printer can make copies and collation if needed
       Copies := 1;              // TRichView need not support copies and collation itself
     end
   else
     Printer.Copies := 1;        // TRichView will provide copies and collation itself
   Printer.BeginDoc;
   if Collate then
     for copyno:= 1 to Copies do
       for i := firstPgNo to lastPgNo do
       begin
         DrawPage(i, Printer.Canvas,False,False);
         DoOnPrinting(i, rvpsProceeding);
         if not ((i=lastPgNo) and (copyno=Copies)) then Printer.NewPage;
       end
   else
     for i := firstPgNo to lastPgNo do
       for copyno:= 1 to Copies do
       begin
         DrawPage(i, Printer.Canvas,False,False);
         DoOnPrinting(i, rvpsProceeding);
         if not ((i=lastPgNo) and (copyno=Copies)) then Printer.NewPage;
       end;
   Printer.EndDoc;
   Printer.Copies := PrinterCopies; { restoring }
   DoOnPrinting(0, rvpsFinished);
end;
{------------------------------------------------------------------------------}
procedure TPrintableRV.Print(const Title: String; Copies: Integer; Collate: Boolean);
begin
   PrintPages(1, TPrintableRVData(RVData).Pages.Count, Title, Copies, Collate);
end;

end.
