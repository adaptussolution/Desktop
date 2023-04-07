
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRVBackground: background for RichView,         }
{       table, or table cell.                           }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVBack;

{$I RV_Defs.inc}

interface
uses SysUtils, Windows, Classes, Graphics,
     {$IFNDEF RVDONOTUSEJPEGIMAGE}
     Jpeg,
     {$ENDIF}
     RVStyle, RVScroll, RVFuncs;

type
  TRVBackground = class
    private
      ImageCopy: TGraphic;
      function GetBitmap: TBitmap;
      function GetItemBackStyle: TRVItemBackgroundStyle;
      procedure SetItemBackStyle(const Value: TRVItemBackgroundStyle);
    public
      Style: TBackgroundStyle;
      Image: TGraphic;
      constructor Create(CreateBitmap: Boolean);
      destructor Destroy; override;
      function ScrollRequiresFullRedraw: Boolean;
      procedure UpdatePaletted(PaletteAction: TRVPaletteAction;Palette: HPALETTE; LogPalette: PLogPalette);
      procedure Draw(Canvas: TCanvas; Rect: TRect;
        HOffs, VOffs, Left, Top, Width,Height: Integer; Color: TColor;
        Clipping: Boolean);
      procedure Print(Canvas: TCanvas; ARect, AFullRect: TRect; const sad: TRVScreenAndDevice;
        Color: TColor; Preview: Boolean; LogPalette: PLogPalette;
        PrintingRVData: TPersistent; ItemBackgroundLayer: Integer);
      function Empty: Boolean;
      function Visible: Boolean;
      function IsSemitransparent: Boolean;
      procedure FreeImage;
      procedure AssignImage(AImage: TGraphic; ARVData: TPersistent; Copy: Boolean);
      property Bitmap: TBitmap read GetBitmap;
      property ItemBackStyle: TRVItemBackgroundStyle read GetItemBackStyle write SetItemBackStyle;
  end;

implementation

uses CRVData, PtRVData;

{============================== TRVBackground =================================}
constructor TRVBackground.Create(CreateBitmap: Boolean);
begin
  inherited Create;
  if CreateBitmap then
    Image := TBitmap.Create;
  Style  := bsNoBitmap;
end;
{------------------------------------------------------------------------------}
destructor TRVBackground.Destroy;
begin
  Image.Free;
  ImageCopy.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
function TRVBackground.ScrollRequiresFullRedraw: Boolean;
begin
  if Image.Empty then
    Result := False
  else begin
    case Style of
      bsNoBitmap, bsTiledAndScrolled:
        Result := False;
      //bsStretched, bsTiled, bsCentered:
      else
        Result := True;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVBackground.UpdatePaletted(PaletteAction: TRVPaletteAction;
                             Palette: HPALETTE; LogPalette: PLogPalette);
begin
  ImageCopy.Free;
  ImageCopy := nil;
  if Image=nil then
    exit;
  case PaletteAction of
    rvpaAssignPalette:
      if (LogPalette<>nil) and not Image.Empty then
        RV_SetPaletteToPicture(Image,LogPalette);
    rvpaCreateCopies,rvpaCreateCopiesEx:
      if (LogPalette<>nil) and not Image.Empty then begin
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
{------------------------------------------------------------------------------}
procedure TRVBackground.Print(Canvas: TCanvas; ARect, AFullRect: TRect;
  const sad: TRVScreenAndDevice; Color: TColor; Preview: Boolean; LogPalette: PLogPalette;
  PrintingRVData: TPersistent; ItemBackgroundLayer: Integer);
var i, j: Integer;
    hbr: HBRUSH;
    OffsRect: TRect;
    bmp: TBitmap;
    gr: TGraphic;
    DC: HDC;
    DCIdx, BmpWidth, BmpHeight, DX, DY: Integer;
    pt: TPoint;
    RVData: TCustomPrintableRVData;

    procedure DrawBitmapAt(Left, Top: Integer);
    begin
      if RV_IsGraphicTransparent(gr) and (Color=clNone) then begin
        RVData.DrawBackToBitmap( RV_XToScreen(Left+DX, sad), RV_YToScreen(Top+DY, sad),
          bmp, sad, ItemBackgroundLayer, True);
        bmp.Canvas.Draw(0,0,gr);
      end;
      RV_PictureToDevice(Canvas, Left, Top, bmp.Width, bmp.Height, sad, bmp, Preview);
    end;

    procedure DrawBitmapAtEx(Left, Top, Width, Height: Integer);
    begin
      if RV_IsGraphicTransparent(gr) and (Color=clNone) then begin
        bmp.Width := RV_XToScreen(Width, sad);
        bmp.Height := RV_YToScreen(Height, sad);
        RVData.DrawBackToBitmap(
          RV_XToScreen(Left+DX, sad), RV_YToScreen(Top+DY, sad),
          bmp, sad, ItemBackgroundLayer, True);
        bmp.Canvas.StretchDraw(Rect(0,0,bmp.Width,bmp.Height),gr);
        RV_PictureToDevice(Canvas, Left, Top, bmp.Width, bmp.Height, sad, bmp, Preview);
        end
      else
        RV_PictureToDevice(Canvas, Left, Top,
          RV_XToScreen(Width, sad), RV_YToScreen(Height, sad), sad, bmp, Preview);
    end;

begin
  DC := Canvas.Handle;
  DCIdx := SaveDC(DC);
  DX := ARect.Left-AFullRect.Left;
  DY := ARect.Top -AFullRect.Top;
  with ARect do
    IntersectClipRect(Canvas.Handle, Left, Top, Right, Bottom);
  SetWindowOrgEx(DC, -ARect.Left, -ARect.Top, @pt);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  try
    if ImageCopy=nil then
      gr := Image
    else
      gr := ImageCopy;
    RVData := PrintingRVData as TCustomPrintableRVData;
    if (Color<>clNone) and
      ((gr=nil) or gr.Empty or (Style in [bsNoBitmap, bsCentered])) then begin
       hbr := CreateSolidBrush(ColorToRGB(Color));
       OffsRect := ARect;
       OffsetRect(OffsRect, -ARect.Left, -ARect.Top);
       FillRect(DC, OffsRect, hbr);
       DeleteObject(hbr);
    end;
    if (gr<>nil) and not gr.Empty then begin
      bmp := TBitmap.Create;
      try
        bmp.Width := gr.Width;
        bmp.Height := gr.Height;
        BmpWidth  := RV_XToDevice(bmp.Width, sad);
        BmpHeight := RV_YToDevice(bmp.Height, sad);
        if LogPalette<>nil then
          bmp.Palette := CreatePalette(LogPalette^);
        if Color<>clNone then begin
          bmp.Canvas.Brush.Color := Color;
          bmp.Canvas.FillRect(Rect(0,0,bmp.Width,bmp.Height));
        end;
        if not RV_IsGraphicTransparent(gr) or (Color<>clNone) then
          bmp.Canvas.Draw(0,0,gr);
        case Style of
          bsCentered:
            DrawBitmapAt(-ARect.Left+((AFullRect.Right-AFullRect.Left)-bmpWidth) div 2,
                         -ARect.Top+((AFullRect.Bottom-AFullRect.Top)-bmpHeight) div 2);
          bsTiled, bsTiledAndScrolled:
            for i:= ARect.Top div bmpHeight to ARect.Bottom div bmpHeight do
              for j:= ARect.Left div bmpWidth to ARect.Right div bmpWidth do
                DrawBitmapAt(j*bmpWidth-ARect.Left,i*bmpHeight-ARect.Top);
          bsStretched:
            DrawBitmapAtEx(-ARect.Left, -ARect.Top,
              AFullRect.Right-AFullRect.Left, AFullRect.Bottom-AFullRect.Top);
        end;
      finally
        bmp.Free;
      end;
    end;
  finally
    SetWindowOrgEx(DC, pt.x, pt.y, nil);
    RestoreDC(DC, DCIdx);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVBackground.Draw(Canvas: TCanvas; Rect: TRect;
  HOffs, VOffs, Left, Top, Width, Height: Integer;
  Color: TColor; Clipping: Boolean);
var i, j: Integer;
    hbr: HBRUSH;
    OffsRect: TRect;
    bmp: TBitmap;
    gr: TGraphic;
    DC: HDC;
    DCIdx: Integer;
    pt: TPoint;
begin
 if ImageCopy=nil then
   gr := Image
 else
   gr := ImageCopy;
 DC := Canvas.Handle;
 if Clipping then begin
   DCIdx := SaveDC(DC);
   with Rect do
     IntersectClipRect(DC, Left, Top, Right, Bottom);
   SetWindowOrgEx(DC, -Rect.Left, -Rect.Top, @pt);
   end
 else
   DCIdx := 0;
 try
   OffsetRect(Rect, -Left, -Top);
   if (Color<>clNone) and
     ((gr=nil) or gr.Empty or RV_IsGraphicTransparent(gr) or (Style in [bsNoBitmap, bsCentered])) then begin
     hbr := CreateSolidBrush(ColorToRGB(Color));
     OffsRect := Rect;
     OffsetRect(OffsRect, -Rect.Left, -Rect.Top);
     FillRect(DC, OffsRect, hbr);
     DeleteObject(hbr);
   end;
   if (gr<>nil) and not gr.Empty then begin
     if gr is TBitmap then begin
       bmp := TBitmap(gr);
       case Style of
         bsCentered:
         BitBlt(DC, -Rect.Left+(Width-bmp.Width) div 2,
                    -Rect.Top+(Height-bmp.Height) div 2,
                    bmp.Width, bmp.Height,
                    bmp.Canvas.Handle, 0, 0, SRCCOPY);
         bsTiled:
          for i:= Rect.Top div bmp.Height to Rect.Bottom div bmp.Height do
            for j:= Rect.Left div bmp.Width to Rect.Right div bmp.Width do
              BitBlt(DC, j*bmp.Width-Rect.Left,i*bmp.Height-Rect.Top, bmp.Width,
                     bmp.Height, bmp.Canvas.Handle, 0, 0, SRCCOPY);
         bsStretched:
              StretchBlt(DC, -Rect.Left, -Rect.Top, Width, Height,
                         bmp.Canvas.Handle, 0, 0, bmp.Width, bmp.Height,
                         SRCCOPY);
         bsTiledAndScrolled:
          for i:= (Rect.Top+VOffs) div bmp.Height to
                  (Rect.Bottom+VOffs) div bmp.Height do
            for j:= (Rect.Left+HOffs) div bmp.Width to
                    (Rect.Right+HOffs) div bmp.Width do
              BitBlt(DC, j*bmp.Width-HOffs-Rect.Left,i*bmp.Height-VOffs-Rect.Top, bmp.Width,
                     bmp.Height, bmp.Canvas.Handle, 0, 0, SRCCOPY);
       end
       end
     else begin
       case Style of
         bsCentered:
           Canvas.Draw(-Rect.Left+(Width-gr.Width) div 2,
           -Rect.Top+(Height-gr.Height) div 2, gr);
         bsTiled:
          for i:= Rect.Top div gr.Height to Rect.Bottom div gr.Height do
            for j:= Rect.Left div gr.Width to Rect.Right div gr.Width do
              Canvas.Draw(j*gr.Width-Rect.Left,i*gr.Height-Rect.Top, gr);
         bsStretched:
           Canvas.StretchDraw(Bounds(-Rect.Left, -Rect.Top, Width, Height), gr);
         bsTiledAndScrolled:
          for i:= (Rect.Top+VOffs) div gr.Height to
                  (Rect.Bottom+VOffs) div gr.Height do
            for j:= (Rect.Left+HOffs) div gr.Width to
                    (Rect.Right+HOffs) div gr.Width do
              Canvas.Draw(j*gr.Width-HOffs-Rect.Left,i*gr.Height-VOffs-Rect.Top, gr);
       end;
     end;
   end;
 finally
   if Clipping then begin
     SetWindowOrgEx(DC, pt.x, pt.y, nil);
     RestoreDC(DC, DCIdx);
   end;
 end;
end;
{------------------------------------------------------------------------------}
function TRVBackground.GetBitmap: TBitmap;
begin
  Result := TBitmap(Image);
end;
{------------------------------------------------------------------------------}
function TRVBackground.GetItemBackStyle: TRVItemBackgroundStyle;
begin
  case Style of
    bsNoBitmap:
      Result := rvbsColor;
    bsStretched:
      Result := rvbsStretched;
    bsCentered:
      Result := rvbsCentered;
    else
      Result := rvbsTiled;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVBackground.SetItemBackStyle(
  const Value: TRVItemBackgroundStyle);
begin
  case Value of
    rvbsColor:
      Style := bsNoBitmap;
    rvbsStretched:
      Style := bsStretched;
    rvbsCentered:
      Style := bsCentered;
    else
      Style := bsTiled;
  end;
end;
{------------------------------------------------------------------------------}
function TRVBackground.Empty: Boolean;
begin
  Result := (Style=bsNoBitmap) and ((Image=nil) or Image.Empty);
end;
{------------------------------------------------------------------------------}
function TRVBackground.Visible: Boolean;
begin
  Result := (Style<>bsNoBitmap) and (Image<>nil) and not Image.Empty;
end;
{------------------------------------------------------------------------------}
procedure TRVBackground.FreeImage;
begin
  Image.Free;
  ImageCopy.Free;
  Image := nil;
  ImageCopy := nil;
end;
{------------------------------------------------------------------------------}
procedure TRVBackground.AssignImage(AImage: TGraphic; ARVData: TPersistent;
  Copy: Boolean);
var RVData: TCustomRVData;
begin
  if AImage=Image then
    exit;
  if Copy then begin
    FreeImage;
    Image := RV_CreateGraphics(TGraphicClass(AImage.ClassType));
    Image.Assign(AImage);
    end
  else begin
    Image := AImage;
  end;
  RVData := TCustomRVData(ARVData);
  UpdatePaletted(RVData.GetDoInPaletteMode, RVData.GetRVPalette,
    RVData.GetRVLogPalette);
end;
{------------------------------------------------------------------------------}
function TRVBackground.IsSemitransparent: Boolean;
begin
  // assuming that Color=clNone
  Result := (Image<>nil) and not Image.Empty and
    (RV_IsGraphicTransparent(Image) or (Style=bsCentered));
end;
{------------------------------------------------------------------------------}

end.
