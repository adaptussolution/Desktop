
{*******************************************************}
{                                                       }
{       RichView                                        }
{       A set of classes representing documents and     }
{       subdocuments in RichView stored in TRVPrint     }
{       and TRVReportHelper.                            }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit PtRVData;

interface
{$I RV_Defs.inc}

uses SysUtils, Classes, Windows, Graphics, Printers,
     DLines, RVFuncs, RVItem, RVBack,
     CRVData, CRVFData, RVRVData,
     {$IFNDEF RVDONOTUSELISTS}
     RVMarker,
     {$ENDIF}
     RVStyle, RVScroll, RichView, RVUni, RVClasses;
type
  { ----------------------------------------------------------------------------
    TRVMultiDrawItemPartsList: list of TRVMultiDrawItemPart.
    Used as TRVMultiDrawItemInfo.PartsList.
  }
  TRVMultiDrawItemPartsList = class (TRVList)
    private
      function Get(Index: Integer): TRVMultiDrawItemPart;
      procedure Put(Index: Integer; const Value: TRVMultiDrawItemPart);
    public
      property Items[Index: Integer]: TRVMultiDrawItemPart read Get write Put; default;
  end;
  { ----------------------------------------------------------------------------
    TRVMultiDrawItemInfo: ancestor class of drawing item containing multiple
    parts on several tables. Inherited classes: TRVTablePrintPart,
    TRVImagePrintPart.
  }
  TRVMultiDrawItemInfo = class (TRVDrawLineInfo)
    private
      FPartsList: TRVMultiDrawItemPartsList;
    public
      constructor Create;
      destructor Destroy; override;
      property PartsList: TRVMultiDrawItemPartsList read FPartsList;
  end;
  { ----------------------------------------------------------------------------
    TRVPageInfo: information about one page.
  }
  TRVPageInfo = class (TCollectionItem)
    public
      StartY, StartDrawItemNo, StartPart, StartY2 : Integer;
      procedure Assign(Source: TPersistent); override;
  end;
  { ----------------------------------------------------------------------------
    TRVPageCollection: collection of TRVPageInfo.
    Class of TCustomMainPtblRVData.Pages.
  }
  TRVPageCollection = class (TCollection)
    private
      function GetItem(Index: Integer): TRVPageInfo;
      procedure SetItem(Index: Integer; const Value: TRVPageInfo);
    public
      constructor Create;
      function Add: TRVPageInfo;
      property Items[Index: Integer]: TRVPageInfo
       read GetItem write SetItem; default;
  end;

  TCustomPrintableRVData = class(TRichViewRVData)
    private
      FBackgroundBmp: TBitmap;    // valid only in DrawPage
      FDrawItem: TRVDrawLineInfo; // valid only in DrawPage
      FItemTop: Integer;       // valid only in DrawPage
      FPageNo: Integer;
    protected
      function GetBack: TRVBackground; virtual;
      function GetTopCoord(PageNo: Integer): Integer; virtual;
      function GetTopCoord2(PageNo: Integer): Integer; virtual;
      function GetPrintableAreaTop: Integer; virtual;
      procedure DoPagePrepaint(Canvas: TCanvas; PageNo: Integer; Preview, Correction: Boolean); virtual;
      procedure DoPagePostpaint(Canvas: TCanvas; PageNo: Integer; Preview: Boolean); virtual;
      procedure GetDrawItemsRange(PageNo: Integer; var StartNo, EndNo, Part: Integer);  virtual;
      procedure CheckPageNo(PageNo: Integer); virtual;
      function ShareItems: Boolean; override;
      function InitPrinterCanvas: TCanvas; dynamic;
      procedure DonePrinterCanvas(Canvas: TCanvas); dynamic;
      function GetColorMode: TRVColorMode; virtual;
      function GetSourceRVDataForPrinting: TCustomRVData; dynamic;
      procedure DoOnHyperlink(RVData: TCustomRVData; ItemNo: Integer; const R: TRect); virtual;
    public
      ParentDrawsBack: Boolean;
      function GetPrintableAreaLeft(PageNo: Integer): Integer; virtual;
      procedure DrawPage(pgNo: Integer; Canvas: TCanvas; Preview, Correction: Boolean); virtual;
      procedure DrawBackToBitmap(Left,Top: Integer; bmp: TBitmap;
        const sad: TRVScreenAndDevice;
        ItemBackgroundLayer: Integer; // 0 - do not draw; -1 - draw completely; others - item specific
        RelativeToItem: Boolean); virtual;
  end;

  TCustomMainPtblRVData = class(TCustomPrintableRVData)
    private
      {$IFNDEF RVDONOTUSELISTS}
      FPrevMarkers: TRVMarkerList;
      {$ENDIF}
    protected
      TmpTMPix, TmpBMPix: Integer;
      StreamSavePage: Integer;
      PrinterCanvas : TCanvas;
      procedure DoFormatting(PageCompleted: Integer; Step:TRVPrintingStep); dynamic;
      function GetBack: TRVBackground; override;
      function GetTopCoord(PageNo: Integer): Integer; override;
      function GetTopCoord2(PageNo: Integer): Integer; override;
      procedure GetSADForFormatting(Canvas: TCanvas; var sad: TRVScreenAndDevice); override;
      function GetPrintableAreaTop: Integer; override;
      procedure GetDrawItemsRange(PageNo: Integer; var StartNo, EndNo, Part: Integer);  override;
      procedure CheckPageNo(PageNo: Integer); override;
      function GetPageWidth: Integer; dynamic;
      function GetPageHeight: Integer; dynamic;
      procedure Prepare; dynamic;
      procedure RVFGetLimits(SaveScope: TRVFSaveScope;
        var StartItem, EndItem, StartOffs, EndOffs: Integer;
        var StartPart, EndPart: TRVMultiDrawItemPart); override;
      function GetColorMode: TRVColorMode; override;
      function GetFirstItemMarker(var ListNo, Level: Integer): Boolean; override;
      function GetSourceRVDataForPrinting: TCustomRVData; override;
    public
      PrnSad: TRVScreenAndDevice;
      Pages: TRVPageCollection;
      FTopMarginPix, FBottomMarginPix: Integer;
      Transparent: Boolean;
      TmpLM, TmpTM: Integer;
      TmpRM, TmpBM: Integer;
      ColorMode: TRVColorMode;
      FFirstParaListNo, FFirstParaLevel: Integer;
      FIsDestinationReady: Boolean;
      procedure Clear; override;
      function GetPrintableAreaLeft(PageNo: Integer): Integer; override;
      function FormatPages: Integer;
      procedure FormatNextPage(var i, StartAt, StartY, Y: Integer;
        var Splitting: Boolean; MaxHeight: Integer);
      procedure DrawPage(pgNo: Integer; Canvas: TCanvas; Preview, Correction: Boolean); override;
      function GetColor: TColor; override;
      {$IFNDEF RVDONOTUSERVF}
      function SavePageAsRVF(Stream: TStream; PageNo: Integer; Color: TColor;
        Background: TRVBackground; Layout: TRVLayoutInfo): Boolean;
      {$ENDIF}
      {$IFNDEF RVDONOTUSELISTS}
      function GetPrevMarkers: TRVMarkerList; override;
      {$ENDIF}
      constructor Create(RichView: TRVScroller); override;
      destructor Destroy; override;
  end;

  TRVHeaderFooterRVData = class;

  TPrintableRVData = class(TCustomMainPtblRVData)
    protected
      procedure DoFormatting(PageCompleted: Integer; Step:TRVPrintingStep); override;
      function InitPrinterCanvas: TCanvas; override;
      procedure DonePrinterCanvas(Canvas: TCanvas); override;
      procedure DoPagePrepaint(Canvas: TCanvas; PageNo:Integer; Preview, Correction: Boolean); override;
      procedure DoPagePostpaint(Canvas: TCanvas; PageNo:Integer; Preview: Boolean); override;
      function GetPageWidth: Integer; override;
      function GetPageHeight: Integer; override;
      procedure Prepare; override;
    public
      TmpLMMir: Integer;
      Header, Footer: TRVHeaderFooterRVData;
      HeaderY, FooterY: Integer;
      function GetPrintableAreaLeft(PageNo: Integer): Integer; override;
      constructor Create(RichView: TRVScroller); override;
      destructor Destroy; override;
  end;

  TRectPtblRVData = class(TCustomPrintableRVData)
    protected
      procedure GetSADForFormatting(Canvas: TCanvas; var sad: TRVScreenAndDevice); override;
      function GetPrintableAreaTop: Integer; override;
      function GetTopCoord(PageNo: Integer): Integer; override;
      function InitPrinterCanvas: TCanvas; override;
      function GetColorMode: TRVColorMode; override;
      function GetSourceRVDataForPrinting: TCustomRVData; override;
      procedure DoOnHyperlink(RVData: TCustomRVData; ItemNo: Integer;
        const R: TRect); override;
    public
      FSourceDataForPrinting: TCustomRVData;
      FParentPrintData: TCustomPrintableRVData;
      Left,Top,DX,DY,Width,Height: Integer;
      Transparent: Boolean;
      FColor: TColor;
      function GetMaxTextWidth: Integer; override;
      function GetMinTextWidth: Integer; override;      
      function GetPrintableAreaLeft(PageNo: Integer): Integer; override;
      function GetParentData: TCustomRVData; override;
      function GetRootData: TCustomRVData; override;
      constructor Create(RichView: TRVScroller; SourceDataForPrinting: TCustomRVFormattedData;
        ParentPrintData: TCustomPrintableRVData); {$IFDEF RICHVIEWDEF4} reintroduce;{$ENDIF}
      procedure DrawBackToBitmap(Left,Top: Integer; bmp: TBitmap;
        const sad: TRVScreenAndDevice; ItemBackgroundLayer: Integer;
        RelativeToItem: Boolean); override;
      function GetWidth: Integer; override;
      function GetHeight: Integer; override;
      function GetLeftMargin: Integer; override;
      function GetRightMargin: Integer; override;
      function GetTopMargin: Integer; override;
      function GetBottomMargin: Integer; override;
      function GetCanvas: TCanvas; override;
      function GetColor: TColor; override;
  end;

  TRVHeaderFooterRVData = class (TRectPtblRVData)
    public
      constructor Create(RichView: TRVScroller; SourceDataForPrinting: TCustomRVFormattedData;
        ParentPrintData: TCustomPrintableRVData);
      function GetRVStyle: TRVStyle; override;
  end;



implementation
uses PtblRV, RVStr;

{============================ TRVMultiDrawItemInfo ============================}
constructor TRVMultiDrawItemInfo.Create;
begin
  inherited Create;
  FPartsList := TRVMultiDrawItemPartsList.Create;
end;
{------------------------------------------------------------------------------}
destructor TRVMultiDrawItemInfo.Destroy;
begin
  FPartsList.Free;
  inherited  Destroy;
end;
{=============================== TRVPageInfo ==================================}
procedure TRVPageInfo.Assign(Source: TPersistent);
begin
  if Source is TRVPageInfo then begin
    StartY := TRVPageInfo(Source).StartY;
    StartDrawItemNo := TRVPageInfo(Source).StartDrawItemNo;
    end
  else
    inherited Assign(Source);
end;
{============================= TRVPageCollection ==============================}
function TRVPageCollection.Add: TRVPageInfo;
begin
  Result := TRVPageInfo(inherited Add);
end;
{------------------------------------------------------------------------------}
constructor TRVPageCollection.Create;
begin
  inherited Create(TRVPageInfo);
end;
{------------------------------------------------------------------------------}
function TRVPageCollection.GetItem(Index: Integer): TRVPageInfo;
begin
  Result := TRVPageInfo(inherited GetItem(Index));
end;
{------------------------------------------------------------------------------}
procedure TRVPageCollection.SetItem(Index: Integer;
  const Value: TRVPageInfo);
begin
  inherited SetItem(Index, Value);
end;
{======================== TRVMultiDrawItemPartsList ===========================}
function TRVMultiDrawItemPartsList.Get(
  Index: Integer): TRVMultiDrawItemPart;
begin
  Result := TRVMultiDrawItemPart(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
procedure TRVMultiDrawItemPartsList.Put(Index: Integer;
  const Value: TRVMultiDrawItemPart);
begin
  inherited Put(Index, Value);
end;
{============================== TCustomPrintableRVData ========================}
procedure TCustomPrintableRVData.DonePrinterCanvas(Canvas: TCanvas);
begin

end;
{------------------------------------------------------------------------------}
function TCustomPrintableRVData.GetSourceRVDataForPrinting: TCustomRVData;
begin
  Result := nil;
end;
{------------------------------------------------------------------------------}
function TCustomPrintableRVData.GetColorMode: TRVColorMode;
begin
  Result := rvcmPrinterColor;
end;
{------------------------------------------------------------------------------}
function TCustomPrintableRVData.ShareItems: Boolean;
begin
  Result := True;
end;
{------------------------------------------------------------------------------}
function TCustomPrintableRVData.InitPrinterCanvas: TCanvas;
begin
  Result := nil;
end;
{------------------------------------------------------------------------------}
function TCustomPrintableRVData.GetBack: TRVBackground;
begin
  Result := nil;
end;
{------------------------------------------------------------------------------}
function TCustomPrintableRVData.GetTopCoord(PageNo: Integer): Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
function TCustomPrintableRVData.GetTopCoord2(PageNo: Integer): Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
function TCustomPrintableRVData.GetPrintableAreaLeft(PageNo: Integer): Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
function TCustomPrintableRVData.GetPrintableAreaTop: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
procedure TCustomPrintableRVData.DoPagePrepaint(Canvas: TCanvas; PageNo:Integer; Preview, Correction: Boolean);
begin

end;
{------------------------------------------------------------------------------}
procedure TCustomPrintableRVData.DoPagePostpaint(Canvas: TCanvas; PageNo: Integer; Preview: Boolean);
begin

end;
{------------------------------------------------------------------------------}
procedure TCustomPrintableRVData.GetDrawItemsRange(PageNo: Integer; var StartNo, EndNo, Part: Integer);
begin
  StartNo := 0;
  EndNo   := DrawItems.Count-1;
  Part    := -1;
  //FirstOffs := 0;
end;
{------------------------------------------------------------------------------}
procedure TCustomPrintableRVData.CheckPageNo(PageNo: Integer);
begin

end;
{------------------------------------------------------------------------------}
procedure TCustomPrintableRVData.DrawBackToBitmap(Left, Top: Integer;
  bmp: TBitmap; const sad: TRVScreenAndDevice; ItemBackgroundLayer: Integer;
  RelativeToItem: Boolean);
var pi: TParaInfo;
    item: TCustomRVItemInfo;
    Clr: TColor;
    r,r2: TRect;
begin
  if RelativeToItem then begin
    inc(Left, RV_XToScreen(FDrawItem.Left, sad));
    inc(Top,  RV_YToScreen(FItemTop-GetPrintableAreaTop, sad));
  end;
  item := GetItem(FDrawItem.ItemNo);
  pi := GetRVStyle.ParaStyles[item.ParaNo];
  r := Rect(0,0, bmp.Width, bmp.Height);
  if (pi.Background.Color=clNone) and (FBackgroundBmp<>nil) then
    bmp.Canvas.CopyRect(Rect(0,0, bmp.Width, bmp.Height),
      FBackgroundBmp.Canvas, Bounds(Left, Top, bmp.Width, bmp.Height))
  else begin
    Clr := pi.Background.Color;
    if Clr = clNone then
      Clr := GetColor;
    if Clr = clNone then
      Clr := clWhite;
    bmp.Canvas.Pen.Color := Clr;
    bmp.Canvas.Brush.Color := Clr;
    bmp.Canvas.FillRect(r);
  end;
  if ItemBackgroundLayer<>0 then begin
    r2 := Bounds(RV_XToScreen(FDrawItem.Left, sad)-Left,
      RV_YToScreen(FItemTop-GetPrintableAreaTop, sad)-Top,
      RV_XToScreen(FDrawItem.Width,sad),
      RV_YToScreen(FDrawItem.Height,sad));
    item.DrawBackgroundForPrinting(bmp.Canvas, r, r2, GetColorMode, ItemBackgroundLayer);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomPrintableRVData.DoOnHyperlink(RVData: TCustomRVData; ItemNo: Integer; const R: TRect);
begin
end;
{------------------------------------------------------------------------------}
procedure TCustomPrintableRVData.DrawPage(pgNo: Integer; Canvas: TCanvas; Preview, Correction: Boolean);
var i,no: Integer;
    dli:TRVDrawLineInfo;
    item: TCustomRVItemInfo;
    zerocoord: Integer;
    first, last, part: Integer;
    tmpbmp : TBitmap;
    LeftOffs: Integer;
    RVStyle: TRVStyle;
    sad: TRVScreenAndDevice;
    {.......................................}
    function GetDevX(ScreenX: Integer):Integer;
    begin
      Result := MulDiv(ScreenX, sad.ppixDevice, sad.ppixScreen);
    end;
    {.......................................}
    function GetDevY(ScreenY: Integer):Integer;
    begin
      Result := MulDiv(ScreenY, sad.ppiyDevice, sad.ppiyScreen);
    end;
    {.......................................}
    procedure DrawBackground; // in-out: backgroundbmp
                              // in: Canvas
    var BackWidth, BackHeight: Integer;
        Color: TColor;
    begin
      BackWidth  := MulDiv(GetWidth,  sad.ppixScreen, sad.ppixDevice);
      BackHeight := MulDiv(GetHeight, sad.ppiyScreen, sad.ppiyDevice);
      if (GetBack<>nil) and
         (GetBack.Style <> bsNoBitmap) and
         not GetBack.Bitmap.Empty then begin
        if GetBack.Style=bsTiledAndScrolled then
          GetBack.Style:=bsTiled;
        FBackgroundBmp := TBitmap.Create;
        FBackgroundBmp.Width := BackWidth;
        FBackgroundBmp.Height := BackHeight;
        if Preview and (GetRVLogPalette<>nil) then
          FBackgroundBmp.Palette := CreatePalette(GetRVLogPalette^);
        GetBack.Draw(FBackgroundBmp.Canvas, Rect(0,0, BackWidth, BackHeight),
                 0,0, 0, 0, BackWidth, BackHeight, GetColor, False);
        RV_PictureToDevice(Canvas, GetPrintableAreaLeft(pgNo),
          GetPrintableAreaTop, -1, -1, sad, FBackgroundBmp, Preview);
        end
      else begin
        FBackgroundBmp := nil;
        if not ParentDrawsBack then
          with Canvas do
            if GetColor<>clNone then begin
              case GetColorMode of
                rvcmColor:
                  Color := GetColor;
                rvcmPrinterColor:
                  Color := RV_GetPrnColor(GetColor);
                rvcmGrayScale:
                  Color := RV_GetGray(RV_GetPrnColor(GetColor));
                else
                  Color := clWhite;
              end;
              Pen.Color := Color;
              Brush.Color := Color;
              Brush.Style := bsSolid;
              Pen.Style := psClear;
              FillRect(Bounds(GetPrintableAreaLeft(pgNo), GetPrintableAreaTop, GetWidth,GetHeight));
              Pen.Style := psSolid;
            end;
      end;
      DoPagePrepaint(Canvas, pgNo, Preview, Correction);
    end;
    {.......................................}
    procedure DrawBackTotmpbmp(Top: Integer); // in: backgroundbmp,tmpbmp,dli,item
    begin
      DrawBackToBitmap(
        RV_XToScreen(FDrawItem.Left, sad)+item.GetBorderWidth,
        RV_YToScreen(FItemTop-GetPrintableAreaTop, sad)+item.GetBorderHeight,
        tmpbmp, sad, -1, False);
    end;
    {.......................................}
    procedure DrawParagraph(i: Integer);
    var R, R1: TRect;
        dli: TRVDrawLineInfo;
        item: TCustomRVItemInfo;
        pi: TParaInfo;
        j: Integer;
    begin
      dli := DrawItems[i];
      item := GetItem(dli.ItemNo);
      pi := GetRVStyle.ParaStyles[item.ParaNo];
      if (pi.Border.Style=rvbNone) and (pi.Background.Color=clNone) and not Assigned(RVStyle.OnDrawParaBack) then
        exit;
      R.Left := GetPrintableAreaLeft(pgNo)+GetDevX(GetLeftMargin+pi.LeftIndent);
      if pi.FirstIndent<0 then
        inc(R.Left, GetDevX(pi.FirstIndent));
      R.Right:= GetPrintableAreaLeft(pgNo)+GetWidth-GetDevX(GetRightMargin+pi.RightIndent);
      if (i=first) and (part>=0) then begin
        R.Top := 0;
        R.Bottom := TRVMultiDrawItemInfo(dli).PartsList[part].Height;
        end
      else begin
        R.Top := dli.Top;
        if (i=last) and (dli is TRVMultiDrawItemInfo) and
          (TRVMultiDrawItemInfo(dli).PartsList.Count>0) then
          R.Bottom := dli.Top+TRVMultiDrawItemInfo(dli).PartsList[0].Height
        else
          R.Bottom := dli.Top+dli.Height+dli.ExtraSpaceBelow;
      end;
      for j := i+1 to last do begin
        dli := DrawItems[j];
        if (dli.ItemNo<>DrawItems[j-1].ItemNo) and
           TCustomRVItemInfo(Items.Objects[dli.ItemNo]).CanBeBorderStart then break;
        if dli.Top<R.Top then
          R.Top := dli.Top;
        if (j=last) and (dli is TRVMultiDrawItemInfo) and
          (TRVMultiDrawItemInfo(dli).PartsList.Count>0) then
          R.Bottom := dli.Top+TRVMultiDrawItemInfo(dli).PartsList[0].Height
        else
          if dli.Top+dli.Height+dli.ExtraSpaceBelow>R.Bottom then
            R.Bottom := dli.Top+dli.Height+dli.ExtraSpaceBelow;
      end;
      OffsetRect(R,0,-zerocoord);
      R1 := R;
      pi.Background.PrepareDrawSaD(R1, sad);
      GetRVStyle.DrawParaBack(Canvas, item.ParaNo, R1, True, GetColorMode);
      pi.Border.DrawSaD(R, Canvas, sad, GetColorMode);
    end;
    {.......................................}
var w, h: Integer;
    BiDiMode, BiDiMode2: TRVBiDiMode;
    Dummy: Boolean;
    TextStyle: TFontInfo;
begin
  FPageNo := pgNo;
  RVStyle := GetRVStyle;
  if RVStyle=nil then
    raise ERichViewError.Create(errStyleIsNotAssigned);
  GetSADForFormatting(Canvas, sad);

  GetDrawItemsRange(pgNo, first, last, part);
  zerocoord := GetTopCoord(PgNo);
  LeftOffs  := GetPrintableAreaLeft(pgNo);
  Canvas.Brush.Style := bsClear;
  DrawBackground;
  tmpbmp := TBitmap.Create;
  {$IFDEF RVDEBUGTABLE}
  if Self is TRectPtblRVData then begin
    Canvas.Pen.Color := clRed;
    Canvas.Pen.Width := 0;
    Canvas.Rectangle(GetPrintableAreaLeft,GetPrintableAreaTop,
                     GetPrintableAreaLeft+ TRectPtblRVData(Self).Width,
                     GetPrintableAreaTop+TRectPtblRVData(Self).Height);
    end
  else begin
  {
    if PgNo=1 then
      with Self as TCustomMainPtblRVData do  begin
        Canvas.Pen.Color := clRed;
        Canvas.Pen.Width := 0;
        Canvas.Pen.Style  := psDot;
        Canvas.MoveTo(LeftOffs-300, TmpTM+TmpTMPix+TPrintableRV(FRichView).FRVPrint.StartAt);
        Canvas.LineTo(300+GetWidth+LeftOffs-GetDevX(GetRightMargin), TmpTM+TmpTMPix+TPrintableRV(FRichView).FRVPrint.StartAt);
        Canvas.Pen.Style  := psSolid;
      end;
    if PgNo=(Self as TCustomMainPtblRVData).PagesColl.Count then
      with Self as TCustomMainPtblRVData do  begin
        Canvas.Pen.Color := clRed;
        Canvas.Pen.Width := 0;
        Canvas.Pen.Style  := psDot;
        Canvas.MoveTo(LeftOffs-300, TmpTM+TmpTMPix+TPrintableRV(FRichView).FRVPrint.EndAt);
        Canvas.LineTo(300+GetWidth+LeftOffs-GetDevX(GetRightMargin), TmpTM+TmpTMPix+TPrintableRV(FRichView).FRVPrint.EndAt);
        Canvas.Pen.Style  := psSolid;
      end
    }
  end;
  {$ENDIF}
  if Preview and (GetRVLogPalette<>nil) then
    tmpbmp.Palette := CreatePalette(GetRVLogPalette^);
  try
    for i:=first to last do begin
      dli := DrawItems[i];
      FDrawItem := dli;
      item := GetItem(dli.ItemNo);
      BiDiMode := GetParaBiDiMode(item.ParaNo);
      if (i=last) and (i<>first) and (dli is TRVMultiDrawItemInfo) and
          (TRVMultiDrawItemInfo(dli).PartsList.Count>0) then
        part := 0;
      if ((i=first) or
         ((dli.ItemNo<>DrawItems[i-1].ItemNo) and
           item.CanBeBorderStart)) and
         (item.StyleNo<>rvsBreak) then
         DrawParagraph(i);
      if item.GetBoolValueEx(rvbpJump, RVStyle) then
        DoOnHyperlink(GetSourceRVDataForPrinting, dli.ItemNo,
          Bounds(dli.Left+LeftOffs, dli.Top-zerocoord, dli.Width, dli.Height));
      no := GetActualStyle(item);
      if no>=0 then begin{ text }
        RVStyle.ApplyStyleColor(Canvas, no, [], True, GetColorMode);
        RVStyle.ApplyStyle(Canvas, no, BiDiMode);
        TextStyle := RVStyle.TextStyles[no];
        if TextStyle.BiDiMode=rvbdUnspecified then
          BidiMode2 := BiDiMode
        else
          BidiMode2 := TextStyle.BiDiMode;
        if BidiMode2=rvbdUnspecified then begin
          if TextStyle.CharSpacing<>0 then
            SetTextCharacterExtra(Canvas.Handle, GetDevX(TextStyle.CharSpacing));
        end;
        if Assigned(RVStyle.OnDrawTextBack) then begin
          Dummy := True;
          RVStyle.OnDrawTextBack(RVStyle, Canvas, no, dli.Left+LeftOffs, dli.Top-zerocoord, dli.Width, dli.Height, [], Dummy);
        end;
        RVStyle.DrawStyleText(DrawItems.GetString(i,Items), Canvas, dli.ItemNo, 1,  no, Self,
             {$IFNDEF RVDONOTUSEJUSTIFY}dli.SpaceBefore,{$ELSE}0,{$ENDIF}
             dli.Left+LeftOffs, dli.Top-zerocoord, dli.Width, dli.Height, [], True, Preview and Correction, GetColorMode, BiDiMode);
        end
      else begin // nontext
        if item.GetBoolValueEx(rvbpPrintToBMP, RVStyle) then begin
          if (Part>0) or ((Part=0)and(i=first)) then
            FItemTop := -zerocoord
          else
            FItemTop := dli.Top-zerocoord;
          if (Part>=0) then
            h := MulDiv(TRVMultiDrawItemInfo(dli).PartsList[Part].Height,
              sad.ppiyScreen, sad.ppiyDevice)
          else
            h := item.GetImageHeight(RVStyle);
          if item.GetBoolValue(rvbpFullWidth) then
            w := MulDiv(GetWidth-GetDevX(GetRightMargin)-dli.Left,
                                   sad.ppixScreen, sad.ppixDevice)
          else
            w  := item.GetImageWidth(RVStyle);
          tmpbmp.Width := w;
          tmpbmp.Height := h;
          DrawBackToTmpBmp(FItemTop);
          if item.PrintToBitmap(tmpbmp, Preview, FRichView, dli, Part, GetColorMode) then
            RV_PictureToDevice(Canvas,
                               dli.Left+LeftOffs+ GetDevX(item.GetBorderWidth),
                               FItemTop + GetDevY(item.GetBorderWidth),
                               w, h,
                               sad, tmpbmp, Preview);
          end
        else begin
          if (Part>0) or ((Part=0)and(i=first)) then
            FItemTop := -zerocoord
          else
            FItemTop := dli.Top-zerocoord;
          if item.GetBoolValue(rvbpFullWidth) then
            item.Print(Canvas, dli.Left+LeftOffs, FItemTop,
                     GetWidth+LeftOffs-GetDevX(GetRightMargin),
                     Preview, Correction, sad, FRichView, dli, part, GetColorMode, Self)
          else
            item.Print(Canvas, dli.Left+LeftOffs, FItemTop,0,
                     Preview, Correction, sad, FRichView, dli, part, GetColorMode, Self);
        end;
        if part<>-1 then begin
          if i=first then
            zerocoord := GetTopCoord2(PgNo);
          part := -1;
        end;
      end;
    end;
    DoPagePostpaint(Canvas, pgNo, Preview);
    {$IFDEF RVWATERMARK}
    if rvflRoot in Flags then begin
      Canvas.Brush.Style := bsClear;
      Canvas.Font.Name := 'Arial';
      Canvas.Font.Size := 8;
      Canvas.Font.Style := [];
      Canvas.Font.Color := clRed;
      Canvas.TextOut(GetPrintableAreaLeft(PgNo), GetPrintableAreaTop, 'unregistered');
    end;
    {$ENDIF}
  finally
    FBackgroundBmp.Free;
    FBackgroundBmp := nil;
    FDrawItem := nil;
    tmpbmp.Free;
  end;
end;
{============================ TCustomMainPtblRVData ===========================}
constructor TCustomMainPtblRVData.Create(RichView: TRVScroller);
begin
  inherited;
  Pages := TRVPageCollection.Create;
  ColorMode := rvcmPrinterColor;
end;
{------------------------------------------------------------------------------}
destructor TCustomMainPtblRVData.Destroy;
begin
  Pages.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
function TCustomMainPtblRVData.GetSourceRVDataForPrinting: TCustomRVData;
begin
  Result := TCustomRichView(RichView).RVData;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSELISTS}
function TCustomMainPtblRVData.GetPrevMarkers: TRVMarkerList;
begin
  if FPrevMarkers=nil then
    FPrevMarkers := TRVMarkerList.Create;
  Result := FPrevMarkers;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TCustomMainPtblRVData.Clear;
{$IFNDEF RVDONOTUSELISTS}
var i: Integer;
{$ENDIF}
begin
  inherited;
  FFirstParaListNo := -1;
  FFirstParaLevel  := -1;
  {$IFNDEF RVDONOTUSELISTS}
  if FPrevMarkers<>nil then begin
    for i := 0 to FPrevMarkers.Count-1 do
      TRVMarkerItemInfo(FPrevMarkers.Items[i]).Free;
    FPrevMarkers.Free;
    FPrevMarkers := nil;
  end;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
function TCustomMainPtblRVData.GetFirstItemMarker(var ListNo,
  Level: Integer): Boolean;
begin
  if (rvstFirstParaAborted in State) then begin
    ListNo := FFirstParaListNo;
    Level  := FFirstParaLevel;
    Result := ListNo>=0;
    end
  else
    Result := inherited GetFirstItemMarker(ListNo, Level);
end;
{------------------------------------------------------------------------------}
function TCustomMainPtblRVData.GetColor: TColor;
begin
  if Transparent then
    Result := clNone
  else
    Result := inherited GetColor;
end;
{------------------------------------------------------------------------------}
function TCustomMainPtblRVData.GetPageHeight: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
function TCustomMainPtblRVData.GetPageWidth: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
procedure TCustomMainPtblRVData.GetDrawItemsRange(PageNo: Integer; var StartNo, EndNo, Part: Integer);
begin
  StartNo := Pages[PageNo-1].StartDrawItemNo;
  Part := Pages[PageNo-1].StartPart-1;
  //FirstOffs := TRVPageInfo(PagesColl.Items[PageNo-1]).FirstItemOff;
  if PageNo=Pages.Count then
    EndNo := DrawItems.Count-1
  else begin
    EndNo := Pages[PageNo].StartDrawItemNo-1;
    if Pages[PageNo].StartPart>1 then
      inc(EndNo);
  end;
end;
{------------------------------------------------------------------------------}
procedure TCustomMainPtblRVData.FormatNextPage(var i, StartAt, StartY, Y: Integer;
                                               var Splitting: Boolean;
                                               MaxHeight: Integer);
var j: Integer;
    dli, dli2, dli3 :TRVDrawLineInfo;
    oldnexntewline, nextnewline, nextnewline2, tmp  : Integer;
    rvpi       : TRVPageInfo;
    nPages: Integer;
    MustBreak: Boolean;
    pi: TParaInfo;
    {................................................}
    function GetDevY(Y: Integer): Integer;
    begin
      Result := MulDiv(Y, PrnSaD.ppiyDevice, PrnSaD.ppiyScreen);
    end;
    {................................................}
begin
  if Pages.Count=0 then begin
    rvpi := Pages.Add;
    rvpi.StartY := 0;
    rvpi.StartDrawItemNo := 0;
    StartY := 0;
    i := 0;
    StartAt := TPrintableRV(FRichView).RVPrint.StartAt;
    Splitting := False;
  end;
  nPages := Pages.Count;

  if Splitting then begin
    dli := DrawItems[i];
    pi := GetRVStyle.ParaStyles[GetItemPara(dli.ItemNo)];  
    while dli.SplitAt(y) do begin
      rvpi             := Pages.Add;
      rvpi.StartDrawItemNo := i;
      dli              := DrawItems[i];
      rvpi.StartY      := -GetDevY(pi.SpaceBefore);
      rvpi.StartPart   := TRVMultiDrawItemInfo(dli).PartsList.Count;
      if Pages[nPages-1].StartDrawItemNo=i then
         Pages[nPages-1].StartY2 :=
           dli.Top+dli.Height-
           TRVMultiDrawItemInfo(dli).PartsList[TRVMultiDrawItemInfo(dli).PartsList.Count-2].Height-
           GetDevY(pi.SpaceBefore);
      DoFormatting(nPages, rvpsProceeding);
      StartAt := 0;
      StartY := 0;
      y := StartY+MaxHeight-StartAt+rvpi.StartY;
      dec(y, GetDevY(pi.SpaceAfter));
      exit;
    end;
    if Pages[nPages-1].StartDrawItemNo=i then begin
      StartY := dli.Top+dli.Height-
                TRVMultiDrawItemInfo(dli).PartsList[TRVMultiDrawItemInfo(dli).PartsList.Count-1].Height-
                GetDevY(pi.SpaceBefore);
      Pages[nPages-1].StartY2 := StartY;
    end;
    inc(i);
    Splitting := False;
  end;
  while i<DrawItems.Count do begin
    dli := DrawItems[i];
    pi := GetRVStyle.ParaStyles[GetItemPara(dli.ItemNo)];
    MustBreak := ((i>0) and (dli.ItemNo<>DrawItems[i-1].ItemNo) and
      GetItem(dli.ItemNo).PageBreakBefore and
      (Pages[nPages-1].StartDrawItemNo<>i));
    if (dli is TRVMultiDrawItemInfo) and dli.InitSplit then begin
      if Pages[nPages-1].StartDrawItemNo=i then begin
        Pages[nPages-1].StartPart := 1;
        Pages[nPages-1].StartY := -GetDevY(pi.SpaceBefore);
      end;
      y := StartY+MaxHeight-StartAt-dli.Top;
      dec(y, GetDevY(pi.SpaceAfter));
      if MustBreak or
         ((Pages[nPages-1].StartDrawItemNo<>i) and
         not dli.CanSplitFirst(y)) then begin
        rvpi := Pages.Add;
        rvpi.StartDrawItemNo := i;
        rvpi.StartY       := -GetDevY(pi.SpaceBefore);
        rvpi.StartPart    := 1;
        DoFormatting(nPages, rvpsProceeding);
        StartY := 0;
        StartAt := 0;
        y := StartY+MaxHeight-StartAt+rvpi.StartY;
        dec(y, GetDevY(pi.SpaceAfter));
        Splitting := True;
        exit;
      end;
      Splitting := True;
      FormatNextPage(i, StartAt, StartY, Y, Splitting, MaxHeight);
      exit;
    end;
    y := dli.Top+dli.Height;
    inc(y, GetDevY(pi.SpaceAfter));
    if (y>StartY+MaxHeight-StartAt) or MustBreak then begin // i-th item does not fit in page, or mandatory break
      { searching for the item which will be the first on the new page }
      nextnewline := i;
      oldnexntewline := -1;
      while True do begin
        nextnewline2 := -1;
        for j:=nextnewline downto 0 do
          if DrawItems[j].FromNewLine then begin
            if nextnewline2<0 then
              nextnewline2 := j;
            if not (rvpaoKeepLinesTogether in pi.Options)
               or IsDrawItemParaStart(j) then begin
              nextnewline := j;
              break;
            end;
          end;
        if (StartAt<=0) and (nextnewline2>=0) and
          (nextnewline <= Pages.Items[nPages-1].StartDrawItemNo) then begin
          if oldnexntewline>=0 then begin
            nextnewline := oldnexntewline;
            break;
          end;
          nextnewline := nextnewline2;
        end;
        if (StartAt<=0) and
           ((nextnewline <= Pages.Items[nPages-1].StartDrawItemNo) or (nextnewline<=0)) then
          break;
        pi := GetRVStyle.ParaStyles[GetItemPara(DrawItems[nextnewline-1].ItemNo)];
        if not IsDrawItemParaStart(nextnewline) or
           not (rvpaoKeepWithNext in pi.Options) then
          break;
        if oldnexntewline<0 then
          oldnexntewline := nextnewline;
        dec(nextnewline);
      end;
      { page must contain one line at least}
      if (StartAt<=0) and
        (nextnewline <= Pages.Items[nPages-1].StartDrawItemNo) then begin
        tmp := nextnewline;
        nextnewline := DrawItems.Count;
        for j := tmp+1 to DrawItems.Count-1 do
          if DrawItems[j].FromNewLine then begin
            nextnewline := j;
            break;
          end;
        if (nextnewline<i) and not IsDrawItemParaStart(nextnewline) then
          for j := nextnewline+1 to i do
            if IsDrawItemParaStart(j) then begin
              nextnewline := j-1;
              break;
            end;
      end;
      if nextnewline<>DrawItems.Count then begin
        { searching min y of first line in new page }
        dli2 := DrawItems[nextnewline];
        StartY := dli2.Top;
        for j := nextnewline+1 to DrawItems.Count-1 do begin
          dli3 := DrawItems[j];
          if dli3.FromNewLine then
            break;
          if dli3.Top<StartY then
            StartY := dli3.Top;
        end;
        rvpi             := Pages.Add;
        rvpi.StartDrawItemNo := nextnewline;
        dli              := DrawItems[nextnewline];
        pi := GetRVStyle.ParaStyles[GetItemPara(dli.ItemNo)];
        dec(StartY, GetDevY(pi.SpaceBefore));
        rvpi.StartY      := StartY;
        DoFormatting(nPages, rvpsProceeding);
        StartAt := 0;
      end;
      i := nextnewline;
      exit;
      end
    else
      inc(i);
   end;
   j := DrawItems.Count-1;
   dli := DrawItems[j];
   pi := GetRVStyle.ParaStyles[GetItemPara(dli.ItemNo)];
   StartAt := dli.Top+dli.Height;
   for j := DrawItems.Count-1  downto 0 do begin
     if DrawItems[j].Top+DrawItems[j].Height>StartAt then
       StartAt := DrawItems[j].Top+DrawItems[j].Height;
     if DrawItems[j].FromNewLine then
       break;
   end;
   inc(StartAt, GetDevY(pi.SpaceAfter));
   rvpi := Pages[nPages-1];
   if rvpi.StartPart=0 then
     TPrintableRV(FRichView).RVPrint.EndAt := StartAt-rvpi.StartY
   else
     TPrintableRV(FRichView).RVPrint.EndAt := StartAt-rvpi.StartY2;
   if nPages=1 then
     inc(TPrintableRV(FRichView).RVPrint.EndAt, TPrintableRV(FRichView).RVPrint.StartAt);
   DoFormatting(nPages,rvpsProceeding);
end;
{------------------------------------------------------------------------------}
procedure TCustomMainPtblRVData.DrawPage(pgNo: Integer; Canvas: TCanvas;
  Preview, Correction: Boolean);
begin
  if (pgNo>0) and (pgNo<=Pages.Count) then
    inherited
end;
{------------------------------------------------------------------------------}
function TCustomMainPtblRVData.FormatPages: Integer;
var i, StartAt, StartY,y: Integer;
    Splitting: Boolean;
    MaxHeight: Integer;
begin
   DoFormatting(0, rvpsStarting);
   PrinterCanvas := InitPrinterCanvas;
   FIsDestinationReady := PrinterCanvas<>nil;
   if FIsDestinationReady then begin
     Prepare;
     TCustomRichView(FRichView).MaxTextWidth :=
       RV_XToScreen(FRichView.Width, PrnSaD)-GetLeftMargin-GetRightMargin;
     Format_(False, True, False, 0, PrinterCanvas, False, False);
     DonePrinterCanvas(PrinterCanvas);
     PrinterCanvas := nil;
     end
   else
     ClearTemporal;
   Pages.Clear;
   Result := 0;
   if DrawItems.Count = 0 then exit;
   MaxHeight := FRichView.Height - (TmpTMPix+TmpBMPix);
   DoFormatting(0, rvpsProceeding);
   i := 0;
   while i<DrawItems.Count do
     FormatNextPage(i, StartAt, StartY, Y, Splitting, MaxHeight);
   Result := Pages.Count;
   DoFormatting(Result, rvpsFinished);
end;
{------------------------------------------------------------------------------}
procedure TCustomMainPtblRVData.DoFormatting(PageCompleted: Integer; Step: TRVPrintingStep);
begin
end;
{------------------------------------------------------------------------------}
function TCustomMainPtblRVData.GetPrintableAreaLeft(PageNo: Integer): Integer;
begin
  Result := TmpLM;
end;
{------------------------------------------------------------------------------}
function TCustomMainPtblRVData.GetPrintableAreaTop: Integer;
begin
  Result := TmpTM;
end;
{------------------------------------------------------------------------------}
function TCustomMainPtblRVData.GetBack: TRVBackground;
begin
  Result := TCustomRichView(FRichView).Background;
end;
{------------------------------------------------------------------------------}
function TCustomMainPtblRVData.GetTopCoord(PageNo: Integer): Integer;
begin
  Result := Pages[PageNo-1].StartY-TmpTM-TmpTMPix;
  if PageNo=1 then
    dec(Result,TPrintableRV(FRichView).RVPrint.StartAt);
end;
{------------------------------------------------------------------------------}
function TCustomMainPtblRVData.GetTopCoord2(PageNo: Integer): Integer;
begin
  Result := Pages[PageNo-1].StartY2-TmpTM-TmpTMPix;
  if PageNo=1 then
    dec(Result,TPrintableRV(FRichView).RVPrint.StartAt);
end;
{------------------------------------------------------------------------------}
procedure TCustomMainPtblRVData.GetSADForFormatting(Canvas: TCanvas; var sad: TRVScreenAndDevice);
begin
  sad := PrnSaD;
end;
{------------------------------------------------------------------------------}
procedure TCustomMainPtblRVData.CheckPageNo(PageNo: Integer);
begin
  if (PageNo<1) or (PageNo>Pages.Count) then
    raise ERichViewError.Create(errInvalidPageNo);
end;
{------------------------------------------------------------------------------}
procedure TCustomMainPtblRVData.Prepare;
begin
  RV_InfoAboutSaD(PrnSaD, PrinterCanvas);
  GetSADForFormatting(PrinterCanvas, PrnSad);
  TmpTMPix := MulDiv(FTopMarginPix,    PrnSaD.ppiyDevice, PrnSaD.ppiyScreen);
  TmpBMPix := MulDiv(FBottomMarginPix, PrnSaD.ppiyDevice, PrnSaD.ppiyScreen);
end;
{------------------------------------------------------------------------------}
function TCustomMainPtblRVData.GetColorMode: TRVColorMode;
begin
  Result := ColorMode;
end;
{------------------------------------------------------------------------------}
procedure TCustomMainPtblRVData.RVFGetLimits(SaveScope: TRVFSaveScope;
                             var StartItem, EndItem, StartOffs, EndOffs: Integer;
                             var StartPart, EndPart: TRVMultiDrawItemPart);
var StartPartIndex, EndPartIndex: Integer;
begin
  if SaveScope=rvfss_Page then begin
    StartPartIndex := -1;
    EndPartIndex   := -1;
    GetDrawItemsRange(StreamSavePage, StartItem, EndItem, StartPartIndex);
    StartOffs := GetOffsBeforeDrawItem(StartItem);
    EndOffs   := GetOffsAfterDrawItem(EndItem);
    if (EndItem>StartItem) and
       (StreamSavePage<Pages.Count) and
       (Pages[StreamSavePage].StartPart>1) then
      EndPartIndex := 0;
    if StartPartIndex<0 then
      StartPart := nil
    else
      StartPart := (DrawItems[StartItem] as TRVMultiDrawItemInfo).PartsList[StartPartIndex];
    if EndPartIndex<0 then
      EndPart := nil
    else
      EndPart := (DrawItems[EndItem] as TRVMultiDrawItemInfo).PartsList[EndPartIndex];
    DrawItem2Item(StartItem, StartOffs, StartItem, StartOffs);
    DrawItem2Item(EndItem, EndOffs, EndItem, EndOffs);
    end
  else
    inherited RVFGetLimits(SaveScope, StartItem, EndItem, StartOffs, EndOffs,
                           StartPart, EndPart);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERVF}
function TCustomMainPtblRVData.SavePageAsRVF(Stream: TStream; PageNo: Integer;
  Color: TColor; Background: TRVBackground; Layout: TRVLayoutInfo): Boolean;
begin
  StreamSavePage := PageNo;
  Result := SaveRVFToStreamEx(Stream, rvfss_Page, Color, Background, Layout)
end;
{$ENDIF}
{============================== TPrintableRVData ==============================}
constructor TPrintableRVData.Create(RichView: TRVScroller);
begin
  inherited Create(RichView);
  HeaderY := 10;
  FooterY := 10;
end;
{------------------------------------------------------------------------------}
destructor TPrintableRVData.Destroy;
begin
  Footer.Free;
  Header.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TPrintableRVData.DoPagePrepaint(Canvas: TCanvas; PageNo:Integer; Preview, Correction: Boolean);
var r: TRect;
begin
  r := Bounds(GetPrintableAreaLeft(PageNo), GetPrintableAreaTop,GetWidth,GetHeight);
  if Assigned(TRVPrint(TPrintableRV(FRichView).RVPrint).OnPagePrepaint) then
    TRVPrint(TPrintableRV(FRichView).RVPrint).OnPagePrepaint(
      TRVPrint(TPrintableRV(FRichView).RVPrint), PageNo, Canvas, Preview,
        Rect(0, 0, TmpLM+TmpRM+GetWidth, TmpTM+TmpBM+GetHeight),
        r
      );
  if Header<>nil then
    Header.DrawPage(1, Canvas, Preview, Correction);
  if Footer<>nil then
    Footer.DrawPage(1, Canvas, Preview, Correction);

  if TRVPrint(TPrintableRV(FRichView).RVPrint).ClipMargins then
    with r do
      IntersectClipRect(Canvas.Handle,Left,Top,Right,Bottom);
end;
{------------------------------------------------------------------------------}
procedure TPrintableRVData.DoPagePostpaint(Canvas: TCanvas; PageNo:Integer; Preview: Boolean);
var r: TRect;
begin
  r := Bounds(GetPrintableAreaLeft(PageNo), GetPrintableAreaTop,GetWidth,GetHeight);
  if Assigned(TRVPrint(TPrintableRV(FRichView).RVPrint).OnPagePostpaint) then
    TRVPrint(TPrintableRV(FRichView).RVPrint).OnPagePostpaint(
      TRVPrint(TPrintableRV(FRichView).RVPrint), PageNo, Canvas, Preview,
        Rect(0, 0, TmpLM+TmpRM+GetWidth, TmpTM+TmpBM+GetHeight),
        r
      );
end;
{------------------------------------------------------------------------------}
function TPrintableRVData.InitPrinterCanvas: TCanvas;
var HDC: THandle;
begin
  HDC := RV_GetPrinterDC;
  if HDC=0 then
    Result := nil
  else begin
    Result := TCanvas.Create;
    Result.Handle := HDC;
  end;
end;
{------------------------------------------------------------------------------}
procedure TPrintableRVData.DonePrinterCanvas(Canvas: TCanvas);
var PHDC: HDC;
begin
  PHDC := Canvas.Handle;
  Canvas.Handle := 0;
  Canvas.Free;
  DeleteDC(PHDC);
end;
{------------------------------------------------------------------------------}
procedure TPrintableRVData.DoFormatting(PageCompleted: Integer;
  Step: TRVPrintingStep);
begin
  if Assigned(TRVPrint(TPrintableRV(FRichView).RVPrint).OnFormatting) then
    TRVPrint(TPrintableRV(FRichView).RVPrint).OnFormatting(TPrintableRV(FRichView), PageCompleted, Step);
end;
{------------------------------------------------------------------------------}
function TPrintableRVData.GetPrintableAreaLeft(PageNo: Integer): Integer;
begin
  if not TPrintableRV(FRichView).FMirrorMargins or ((PageNo mod 2)=1) then
    Result := TmpLM
  else
    Result := TmpLMMir;
end;
{------------------------------------------------------------------------------}
function TPrintableRVData.GetPageHeight: Integer;
begin
  Result := Printer.PageHeight;
end;
{------------------------------------------------------------------------------}
function TPrintableRVData.GetPageWidth: Integer;
begin
  Result := Printer.PageWidth;
end;
{------------------------------------------------------------------------------}
procedure TPrintableRVData.Prepare;
var phoX, phoY, phW, phH, lpy, lpx, footeroffs: Integer;
    PHDC: HDC;
begin
  inherited Prepare;

  PrinterCanvas.Font.PixelsPerInch := PrnSaD.ppiyDevice;

  PHDC := PrinterCanvas.Handle;

  lpy := GetDeviceCaps(PHDC, LOGPIXELSY);
  lpx := GetDeviceCaps(PHDC, LOGPIXELSX);

  phoX := GetDeviceCaps(PHDC, PHYSICALOFFSETX);
  phoY := GetDeviceCaps(PHDC, PHYSICALOFFSETY);
  phW  := GetDeviceCaps(PHDC, PHYSICALWIDTH);
  phH  := GetDeviceCaps(PHDC, PHYSICALHEIGHT);

  {
  if phW>phoX+GetPageWidth then
    phW := phoX+GetPageWidth;
  if phH>phoY+GetPageHeight then
    phH := phoY+GetPageHeight;
  }

  with TPrintableRV(FRichView) do begin
    TmpLM := MulDiv(FLeftMarginMM,   5*lpx, 127)- phoX;
    TmpTM := MulDiv(FTopMarginMM,    5*lpy, 127)- phoY;
    TmpRM := MulDiv(FRightMarginMM,  5*lpx, 127)- (phW-(phoX+GetPageWidth));
    TmpBM := MulDiv(FBottomMarginMM, 5*lpy, 127)- (phH-(phoY+GetPageHeight));

    TmpLMMir := MulDiv(FRightMarginMM,   5*lpx, 127)- phoX;
  end;

  if TmpLM<0 then TmpLM := 0;
  if TmpTM<0 then TmpTM := 0;
  if TmpRM<0 then TmpRM := 0;
  if TmpBM<0 then TmpBM := 0;
  if TmpLMMir<0 then TmpLMMir := 0;

  if Header<>nil then begin
    Header.Left := TmpLM;
    Header.Width := GetPageWidth - (TmpLM+TmpRM);
    Header.Top := MulDiv(HeaderY,  5*lpy, 127)- phoY;
    if Header.Top<0 then
      Header.Top := 0;
    Header.Height := 1;
    Header.Format(True);
    if TmpTM<Header.Top+Header.DocumentHeight then
      TmpTM := Header.Top+Header.DocumentHeight;
  end;

  if Footer<>nil then begin
    Footer.Left := TmpLM;
    Footer.Width := GetPageWidth - (TmpLM+TmpRM);
    Footer.Top := 0;
    Footer.Height := 1;
    Footer.Format(True);
    footeroffs := MulDiv(FooterY,  5*lpy, 127)-(phH-(phoY+GetPageHeight))+Footer.DocumentHeight;
    Footer.Top := GetPageHeight-footeroffs;
    if TmpBM<footeroffs then
      TmpBM := footeroffs;
  end;

  State := State+[rvstSkipformatting];
  try
    FRichView.ClientWidth := GetPageWidth - (TmpLM+TmpRM);
    FRichView.ClientHeight:= GetPageHeight - (TmpTM+TmpBM);
  finally
    State := State-[rvstSkipformatting];
  end;
end;
{================================ TRectPtblRVData =============================}
constructor TRectPtblRVData.Create(RichView: TRVScroller;
  SourceDataForPrinting: TCustomRVFormattedData;
  ParentPrintData: TCustomPrintableRVData);
begin
  inherited Create(RichView);
  FSourceDataForPrinting := SourceDataForPrinting;
  FParentPrintData := ParentPrintData;
  ShareItemsFrom(SourceDataForPrinting);
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetPrintableAreaLeft(PageNo: Integer): Integer;
begin
  Result := Left+DX;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetPrintableAreaTop: Integer;
begin
  Result := Top+DY;
end;
{------------------------------------------------------------------------------}
procedure TRectPtblRVData.GetSADForFormatting(Canvas: TCanvas; var sad: TRVScreenAndDevice);
begin
  sad := TCustomMainPtblRVData(TPrintableRV(FRichView).RVData).PrnSaD;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetTopCoord(PageNo: Integer): Integer;
begin
  Result := -(Top+DY);
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetWidth: Integer;
begin
  Result := Width;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetHeight: Integer;
begin
  Result := Height;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.InitPrinterCanvas: TCanvas;
begin
  Result := GetCanvas;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetColorMode: TRVColorMode;
begin
  Result := TCustomMainPtblRVData(TPrintableRV(FRichView).RVData).ColorMode;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetCanvas: TCanvas;
begin
  Result := TCustomMainPtblRVData(TPrintableRV(FRichView).RVData).PrinterCanvas;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetColor: TColor;
begin
  Result := FColor;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetBottomMargin: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetLeftMargin: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetRightMargin: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetTopMargin: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
procedure TRectPtblRVData.DoOnHyperlink(RVData: TCustomRVData;
  ItemNo: Integer; const R: TRect);
begin
  FParentPrintData.DoOnHyperlink(RVData, ItemNo, R);
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetSourceRVDataForPrinting: TCustomRVData;
begin
  Result := FSourceDataForPrinting;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetParentData: TCustomRVData;
begin
  Result := FParentPrintData; // TCustomRichView(FRichView).RVData;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetRootData: TCustomRVData;
begin
  Result := GetParentData.GetRootData;
end;
{------------------------------------------------------------------------------}
procedure TRectPtblRVData.DrawBackToBitmap(Left, Top: Integer;
  bmp: TBitmap; const sad: TRVScreenAndDevice; ItemBackgroundLayer: Integer;
  RelativeToItem: Boolean);
var r, r2: TRect;
    l,t: Integer;
    pi: TParaInfo;
    item: TCustomRVItemInfo;
begin
  if RelativeToItem then begin
    inc(Left, RV_XToScreen(FDrawItem.Left, sad));
    inc(Top, RV_YToScreen(FItemTop-GetPrintableAreaTop, sad));
  end;
  item := GetItem(FDrawItem.ItemNo);
  pi := GetRVStyle.ParaStyles[item.ParaNo];
  r := Rect(0,0, bmp.Width, bmp.Height);
  if pi.Background.Color<>clNone then begin
    bmp.Canvas.Brush.Color := pi.Background.Color;
    bmp.Canvas.FillRect(r);
    end
  else if Transparent then begin
    l := Left+
      RV_XToScreen(GetPrintableAreaLeft(FPageNo)-
        FParentPrintData.GetPrintableAreaLeft(FParentPrintData.FPageNo), sad);
    t := Top+
      RV_YToScreen(GetPrintableAreaTop-FParentPrintData.GetPrintableAreaTop, sad);
    FParentPrintData.DrawBackToBitmap(l, t, bmp, sad, -1, False);
    end
  else
    inherited;
  if ItemBackgroundLayer<>0 then begin
    r2 := Bounds(RV_XToScreen(FDrawItem.Left, sad)-Left,
      RV_YToScreen(FItemTop-GetPrintableAreaTop, sad)-Top,
      RV_XToScreen(FDrawItem.Width,sad),
      RV_YToScreen(FDrawItem.Height,sad));
    GetItem(FDrawItem.ItemNo).DrawBackgroundForPrinting(bmp.Canvas, r, r2, GetColorMode, ItemBackgroundLayer);
  end;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetMaxTextWidth: Integer;
begin
  Result := 0;
end;
{------------------------------------------------------------------------------}
function TRectPtblRVData.GetMinTextWidth: Integer;
begin
  Result := 0;
end;
{========================= TRVHeaderFooterRVData ==============================}
constructor TRVHeaderFooterRVData.Create(RichView: TRVScroller;
  SourceDataForPrinting: TCustomRVFormattedData;
  ParentPrintData: TCustomPrintableRVData);
begin
  inherited;
  FColor := clNone;
end;
{------------------------------------------------------------------------------}
function TRVHeaderFooterRVData.GetRVStyle: TRVStyle;
begin
  Result := FSourceDataForPrinting.GetRVStyle;
end;

end.
