
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRVReportHelper: contains RichView document     }
{       and draws/prints in onto the specified          }
{       Canvas                                          }
{       (registered on "RichView" page of               }
{       the Component Palette)                          }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVReport;

interface

{$I RV_Defs.inc}
uses Windows, Graphics, Classes, RVStyle, RVClasses, RVItem,
     CRVData, CRVFData, RVRVData, RichView, PtRVData, PtblRV, RVFuncs;


type
  TRVReportHelper = class;
  TRVDrawHyperlinkEvent = procedure (Sender: TRVReportHelper;
    RVData: TCustomRVData; ItemNo: Integer; R: TRect) of object;
  { ----------------------------------------------------------------------------
    TReportRVData: main data object for TReportRichView
  }
  TReportRVData = class(TCustomMainPtblRVData)
    private
      StartY,       StartAt,       Y,       DrawItemNo,   CurHeight: Integer;
      StoredStartY, StoredStartAt, StoredY, StoredDrawItemNo, StoredMaxHeight: Integer;
      Splitting: Boolean;
      StoredSplitting: Boolean;
      procedure StoreMargins;
      procedure RestoreMargins;
      procedure Init(ACanvas: TCanvas; APageWidth: Integer);
      function FormatNextPage(AMaxHeight: Integer): Boolean;
      procedure UnformatLastPage;
      function Finished: Boolean;
      procedure Reset;
    protected
      function ShareItems: Boolean; override;
      (*procedure DoFormatting(PageCompleted: Integer; Step:TRVPrintingStep); override;*)
      procedure DoOnHyperlink(RVData: TCustomRVData; ItemNo: Integer;
        const R: TRect); override;
    public
      function GetHeight: Integer; override;
  end;
  { ----------------------------------------------------------------------------
    TReportRichView: class of hidden TRichView inside TRVReportHelper
  }
  TReportRichView = class(TCustomPrintableRV)
    private
      function GetHeight: Integer;
      function GetWidth: Integer;
      function GetLeft: Integer;
      function GetTop: Integer;
    protected
      function GetDataClass: TRichViewRVDataClass; override;
    public
      constructor Create(AOwner: TComponent); override;
    published
      { Published standard properties }
      property Color default clNone;
      { Published RichView properties }
      property BackgroundBitmap;
      property BackgroundStyle;
      property BiDiMode;
      property BottomMargin;
      property Delimiters;
      property LeftMargin;
      property Options;
      property RightMargin;
      property RTFOptions;
      property RTFReadProperties;
      property RVFOptions;
      property RVFParaStylesReadMode;
      property RVFTextStylesReadMode;
      property Style;
      property TopMargin;
      { Published RichView events }
      property OnControlAction;
      property OnHTMLSaveImage;
      property OnRVFImageListNeeded;
      property OnRVFControlNeeded;
      property OnRVFPictureNeeded;
      property OnSaveComponentToFile;
      property OnURLNeeded;
      property OnReadHyperlink;
      property OnWriteHyperlink;      
      property Width: Integer read GetWidth;
      property Height: Integer read GetHeight;
      property Left: Integer read GetLeft;
      property Top: Integer read GetTop;
  end;
  (*
  { ----------------------------------------------------------------------------
    TReportRichView: class of hidden TRichView inside TRVReportHelper
  }
  TRVHHyperlinkInfo = class
    private
      FPageNo, FItemNo: Integer;
      FLeft, FTop, FWidth, FHeight: Integer;
      FRVData: TCustomRVData;
    public
      property PageNo: Integer read FPageNo;
      property ItemNo: Integer read FItemNo;
      property Left: Integer read FLeft;
      property Top: Integer read FTop;
      property Width: Integer read FWidth;
      property Height: Integer read FHeight;
      property RVData: TCustomRVData read FRVData;
  end;
  { ----------------------------------------------------------------------------
    TRVHHyperlinkList: list of TRVHHyperlinkInfo
  }
  TRVHHyperlinkList = class (TRVList)
    private
      function Get(Index: Integer): TRVHHyperlinkInfo;
      procedure Put(Index: Integer; const Value: TRVHHyperlinkInfo);
    public
      property Items[Index: Integer]: TRVHHyperlinkInfo read Get write Put; default;
  end;
  *)
  { ----------------------------------------------------------------------------
    TRVReportHelper: component for drawing TRichView document on the specified
    canvas as several pages
  }
  TRVReportHelper = class (TCustomRVPrint)
    private
      FOnDrawHyperlink: TRVDrawHyperlinkEvent;
      (*FHyperlinks: TRVHHyperlinkList;*)
      function GetRichView: TReportRichView;
    protected
      function CreateRichView: TCustomPrintableRV; override;
    public
      { Create & Destroy }
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      { Formatting }
      procedure Init(ACanvas: TCanvas; APageWidth: Integer);
      function FormatNextPage(AMaxHeight: Integer): Boolean;
      procedure UnformatLastPage;
      procedure Reset;
      { Drawing }
      procedure DrawPage(APageNo: Integer; ACanvas: TCanvas; APreview: Boolean;
        AHeight: Integer);
      procedure DrawPageAt(Left, Top, APageNo: Integer; ACanvas: TCanvas;
        APreview: Boolean; AHeight: Integer);
      { Information }
      function Finished: Boolean;
      function GetLastPageHeight: Integer;
      { Properties }
      (*property Hyperlinks: TRVHHyperlinkList read FHyperlinks;*)
    {$IFDEF RICHVIEWDEF6}
    published
    {$ENDIF}
      property RichView: TReportRichView read GetRichView;
    published
      property ColorMode default rvcmColor;
      property OnDrawHyperlink: TRVDrawHyperlinkEvent
        read FOnDrawHyperlink write FOnDrawHyperlink;
  end;

implementation

uses DLines;

{============================ TRVReportData ===================================}
function TReportRVData.ShareItems: Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
procedure TReportRVData.Reset;
begin
  Pages.Clear;
  DrawItemNo := 0;
  Splitting  := False;
end;
{------------------------------------------------------------------------------}
procedure TReportRVData.Init(ACanvas: TCanvas; APageWidth: Integer);
begin
  Reset;

  PrinterCanvas := ACanvas;
  TmpLM := 0;
  TmpTM := 0;
  StoreMargins;
  Prepare;
  State := State+[rvstSkipformatting];
  try
    TCustomRichView(FRichView).HandleNeeded;
    TCustomRichView(FRichView).VScrollVisible := False;
    TCustomRichView(FRichView).HScrollVisible := False;
    FRichView.ClientWidth := APageWidth;
    FRichView.ClientHeight:= APageWidth;
  finally
    State := State-[rvstSkipformatting];
  end;
  TCustomRichView(FRichView).MaxTextWidth := RV_XToScreen(FRichView.Width, PrnSaD)-
    TCustomRichView(FRichView).LeftMargin-TCustomRichView(FRichView).RightMargin;

  Format_(False, True, False, 0, PrinterCanvas, False, False);
  RestoreMargins;
  FIsDestinationReady := True;
end;
{------------------------------------------------------------------------------}
procedure TReportRVData.StoreMargins;
begin
  FTopMarginPix    := TCustomRichView(FRichView).TopMargin;
  FBottomMarginPix := TCustomRichView(FRichView).BottomMargin;
  TCustomRichView(FRichView).TopMargin := 0;
  TCustomRichView(FRichView).BottomMargin := 0;
end;
{------------------------------------------------------------------------------}
procedure TReportRVData.RestoreMargins;
begin
  TCustomRichView(FRichView).TopMargin    := FTopMarginPix;
  TCustomRichView(FRichView).BottomMargin := FBottomMarginPix;
end;
{------------------------------------------------------------------------------}
function TReportRVData.Finished: Boolean;
begin
  Result := (DrawItems.Count=0) or (DrawItemNo>=DrawItems.Count);
end;
{------------------------------------------------------------------------------}
function TReportRVData.FormatNextPage(AMaxHeight: Integer): Boolean;
begin
  Result := not Finished;
  if Result then begin
    StoredDrawItemNo := DrawItemNo;
    StoredStartAt    := StartAt;
    StoredStartY     := StartY;
    StoredY          := Y;
    StoredSplitting  := Splitting;
    if Splitting then begin
      dec(Y, StoredMaxHeight);
      inc(Y, AMaxHeight);
    end;
    StoredMaxHeight  :=  AMaxHeight;
    StoreMargins;
    dec(AMaxHeight, TmpTMPix+TmpBMPix);
    inherited FormatNextPage(DrawItemNo, StartAt, StartY, Y, Splitting, AMaxHeight);
    RestoreMargins;
  end;
end;
{------------------------------------------------------------------------------}
procedure TReportRVData.UnformatLastPage;
begin
  Pages[Pages.Count-1].Free;
  DrawItemNo := StoredDrawItemNo;
  StartAt    := StoredStartAt;
  StartY     := StoredStartY;
  Y          := StoredY;
  Splitting  := StoredSplitting;
end;
{------------------------------------------------------------------------------}
function TReportRVData.GetHeight: Integer;
begin
  Result := CurHeight;
end;
{------------------------------------------------------------------------------}
(*
procedure TReportRVData.DoFormatting(PageCompleted: Integer;
  Step: TRVPrintingStep);
var StartDItemNo, EndDItemNo, i: Integer;
    HypInfo: TRVHHyperlinkInfo;
begin
  inherited;
  if Step<>rvpsProceeding then
    exit;
  StartDItemNo := Pages[PageCompleted-1].StartDrawItemNo;
  if PageCompleted>=Pages.Count then
    EndDItemNo := DrawItems.Count-1
  else
    EndDItemNo := Pages[PageCompleted].StartDrawItemNo;
  for i := StartDItemNo to EndDItemNo do begin
    if GetItem(DrawItems[i].ItemNo).GetBoolValueEx(rvbpJump, GetRVStyle) then begin
      HypInfo := TRVHHyperlinkInfo.Create;
      HypInfo.FPageNo := PageCompleted;
      HypInfo.FRVData := GetSourceRVDataForPrinting.GetSourceRVData;
      with DrawItems[i] do begin
        HypInfo.FLeft   := Left;
        HypInfo.FTop    := Top;
        HypInfo.FWidth  := Width;
        HypInfo.FHeight := Height;
      end;
      TRVReportHelper(TReportRichView(FRichView).RVPrint).Hyperlinks.Add(HypInfo);
    end;
  end;
end;
*)
{------------------------------------------------------------------------------}
procedure TReportRVData.DoOnHyperlink(RVData: TCustomRVData;
  ItemNo: Integer; const R: TRect);
var Helper: TRVReportHelper;
begin
  Helper := TRVReportHelper(TReportRichView(FRichView).RVPrint);
  if Assigned(Helper.FOnDrawHyperlink) then
    Helper.FOnDrawHyperlink(Helper, RVData, ItemNo, R);
end;
{================================ TReportRichView =============================}
constructor TReportRichView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'RichView';
  Flags := Flags - [rvflShareContents];
  {$IFDEF RICHVIEWDEF6}
  SetSubComponent(True);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
function TReportRichView.GetDataClass: TRichViewRVDataClass;
begin
  Result := TReportRVData;
end;
{------------------------------------------------------------------------------}
function TReportRichView.GetHeight: Integer;
begin
  Result := inherited Height;
end;
{------------------------------------------------------------------------------}
function TReportRichView.GetLeft: Integer;
begin
  Result := inherited Left;
end;
{------------------------------------------------------------------------------}
function TReportRichView.GetTop: Integer;
begin
  Result := inherited Top;
end;
{------------------------------------------------------------------------------}
function TReportRichView.GetWidth: Integer;
begin
  Result := inherited Width;
end;
{================================ TRVReportHelper =============================}
{ Constructor.                                                                 }
constructor TRVReportHelper.Create(AOwner: TComponent);
begin
  inherited;
  (*FHyperlinks := TRVHHyperlinkList.Create;*)
  ColorMode := rvcmColor;
end;
{------------------------------------------------------------------------------}
{ Destructor.                                                                  }
destructor TRVReportHelper.Destroy;
begin
  (*
  FHyperlinks.Free;
  FHyperlinks := nil;
  *)
  inherited;
end;
{------------------------------------------------------------------------------}
function TRVReportHelper.CreateRichView: TCustomPrintableRV;
begin
  Result := TReportRichView.Create(Self);
end;
{------------------------------------------------------------------------------}
procedure TRVReportHelper.Init(ACanvas: TCanvas; APageWidth: Integer);
begin
  inc(FormattingID);
  if FormattingID=10000 then
    FormattingID := 0;
  TReportRVData(TReportRichView(rv).RVData).Init(ACanvas,APageWidth);
end;
{------------------------------------------------------------------------------}
function TRVReportHelper.Finished: Boolean;
begin
  Result := TReportRVData(TReportRichView(rv).RVData).Finished;
end;
{------------------------------------------------------------------------------}
function TRVReportHelper.FormatNextPage(AMaxHeight: Integer): Boolean;
begin
  Result := TReportRVData(TReportRichView(rv).RVData).FormatNextPage(AMaxHeight);
end;
{------------------------------------------------------------------------------}
procedure TRVReportHelper.UnformatLastPage;
begin
  TReportRVData(TReportRichView(rv).RVData).UnformatLastPage;
end;
{------------------------------------------------------------------------------}
function TRVReportHelper.GetRichView: TReportRichView;
begin
  Result := TReportRichView(rv);
end;
{------------------------------------------------------------------------------}
procedure TRVReportHelper.DrawPage(APageNo: Integer; ACanvas: TCanvas;
  APreview: Boolean; AHeight: Integer);
begin
  TReportRVData(TReportRichView(rv).RVData).CurHeight := AHeight;
  rv.DrawPage(APageNo, ACanvas, APreview, PreviewCorrection);
end;
{------------------------------------------------------------------------------}
procedure TRVReportHelper.DrawPageAt(Left, Top, APageNo: Integer;
  ACanvas: TCanvas; APreview: Boolean; AHeight: Integer);
var pt: TPoint;
begin
  SetViewportOrgEx(ACanvas.Handle, Left, Top, @pt);
  DrawPage(APageNo, ACanvas, APreview, AHeight);
  SetViewportOrgEx(ACanvas.Handle, pt.x, pt.y, nil);
end;
{------------------------------------------------------------------------------}
function TRVReportHelper.GetLastPageHeight: Integer;
begin
  Result := EndAt+TReportRVData(RichView.RVData).TmpTMPix+
    TReportRVData(RichView.RVData).TmpBMPix;
end;
{------------------------------------------------------------------------------}
procedure TRVReportHelper.Reset;
begin
  (*FHyperlinks.Clear;*)
  TReportRVData(TReportRichView(rv).RVData).Reset;
end;
{==================================== TRVHHyperlinkList =======================}
(*
function TRVHHyperlinkList.Get(Index: Integer): TRVHHyperlinkInfo;
begin
  Result := TRVHHyperlinkInfo(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
procedure TRVHHyperlinkList.Put(Index: Integer;
  const Value: TRVHHyperlinkInfo);
begin
  inherited Put(Index, Value);
end;
*)

end.
