
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRVMarkerItemInfo: RichView item type           }
{       representing paragraph markers.                 }
{       TRVMarkerList: list of markers in the document  }
{       and all its subdocuments.                       }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVMarker;

interface
{$I RV_Defs.inc}
uses SysUtils, Windows, Classes, Controls, Graphics, Forms,
     RVFuncs, RVItem, RVStyle, DLines, RVFMisc, RVScroll, RVUni, RVClasses;

{$IFNDEF RVDONOTUSELISTS}

type

  TRVMarkerList = class;

  TRVMarkerItemInfo = class (TRVRectItemInfo)
    private
      FWidth, FHeight, FDescent, FOverhang: Integer;
      FCachedIndexInList: Integer;
      procedure DoPaint(x,y: Integer; Canvas: TCanvas; State: TRVItemDrawStates;
        Style: TRVStyle; dli: TRVDrawLineInfo; ColorMode: TRVColorMode);
    protected
      function SaveRVFHeaderTail(RVData: TPersistent): String; override;
      procedure CalcSize(Canvas: TCanvas; RVData: TPersistent;
        var Width, Height, Desc, Overhang: Integer;
        sad: PRVScreenAndDevice; ForMinWidth: Boolean;
        var HShift, SpaceBefore: Integer);
      procedure CalcDisplayString(RVStyle: TRVStyle; List: TRVMarkerList;
        Index: Integer);
      function GetHeight: Integer; override;
      function GetWidth: Integer; override;
      function GetLevelInfoEx(RVStyle: TRVStyle; LevelNo: Integer): TRVListLevel;
      function GetDescent: Integer; override;
      function GetRVFExtraPropertyCount: Integer; override;
      procedure SaveRVFExtraProperties(Stream: TStream); override;
    public
      ListNo, Level: Integer;
      Counter: Integer;
      Reset: Boolean;
      StartFrom: Integer;
      DisplayString: String;
      NoHTMLImageSize: Boolean;
      constructor CreateEx(RVData: TPersistent;
        AListNo, ALevel, AStartFrom: Integer; AReset: Boolean);
      constructor Create(RVData: TPersistent); override;
      procedure Assign(Source: TCustomRVItemInfo); override;
      function GetLevelInfo(RVStyle: TRVStyle): TRVListLevel;
      function GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas;
        RVData: TPersistent): Integer; override;
      function GetBoolValue(Prop: TRVItemBoolProperty): Boolean; override;
      function GetBoolValueEx(Prop: TRVItemBoolPropertyEx;
        RVStyle: TRVStyle): Boolean; override;
      procedure OnDocWidthChange(DocWidth: Integer; dli: TRVDrawLineInfo; Printing: Boolean;
        Canvas: TCanvas; RVData: TPersistent; sad: PRVScreenAndDevice;
        var HShift, Desc: Integer; NoCaching: Boolean); override;
      procedure Paint(x,y: Integer; Canvas: TCanvas; State: TRVItemDrawStates;
        Style: TRVStyle; dli: TRVDrawLineInfo); override;
      procedure Print(Canvas: TCanvas; x,y,x2: Integer; Preview, Correction: Boolean;
        const sad: TRVScreenAndDevice; RichView: TRVScroller; dli: TRVDrawLineInfo;
        Part: Integer; ColorMode: TRVColorMode; RVData: TPersistent); override;
      function PrintToBitmap(Bkgnd: TBitmap; Preview: Boolean; RichView: TRVScroller;
          dli: TRVDrawLineInfo; Part: Integer; ColorMode: TRVColorMode):Boolean; override;
      function ReadRVFHeader(var P: PChar; RVData: TPersistent): Boolean; override;
      procedure SaveRVF(Stream: TStream; RVData: TPersistent;
        ItemNo, ParaNo: Integer; const Name: String; Part: TRVMultiDrawItemPart;
        ForceSameAsPrev: Boolean); override;
      procedure MovingToUndoList(ItemNo: Integer; RVData, AContainerUndoItem: TObject); override;
      procedure MovingFromUndoList(ItemNo: Integer; RVData: TObject); override;
      function GetImageWidth(RVStyle: TRVStyle): Integer; override;
      function GetImageHeight(RVStyle: TRVStyle): Integer; override;
      function GetLeftOverhang: Integer; override;
      procedure HTMLOpenOrCloseTags(Stream: TStream;
        OldLevelNo, NewLevelNo: Integer; RVStyle: TRVStyle; UseCSS: Boolean);
      procedure SaveHTMLSpecial(Stream: TStream; Prev: TRVMarkerItemInfo;
        RVStyle: TRVStyle; UseCSS: Boolean);
      procedure SaveToHTML(Stream: TStream; RVData: TPersistent;
        ItemNo: Integer; const Text, Path: String;
        const imgSavePrefix: String; var imgSaveNo: Integer;
        CurrentFileColor: TColor; SaveOptions: TRVSaveOptions;
        UseCSS: Boolean; Bullets: TRVList); override;
      function GetLICSS(RVData: TPersistent; ItemNo: Integer; const Path,
        imgSavePrefix: String; var imgSaveNo: Integer; CurrentFileColor: TColor;
        SaveOptions: TRVSaveOptions; Bullets: TRVList): String;
      {$IFNDEF RVDONOTUSERTF}
      procedure FillRTFTables(ColorList: TRVColorList;
        ListOverrideCountList: TRVIntegerList; RVData: TPersistent); override;
      procedure SaveRTF(Stream: TStream; RVData: TPersistent; ItemNo: Integer;
        const Name: String; TwipsPerPixel: Double; Level: Integer;
        ColorList: TRVColorList; StyleToFont, ListOverrideOffsetsList1,
        ListOverrideOffsetsList2: TRVIntegerList; FontTable: TRVList); override;
      {$ENDIF}
      procedure MarkStylesInUse(UsedTextStyles, UsedParaStyles,
        UsedListStyles: TRVIntegerList); override;
      procedure UpdateStyles(TextStylesShift, ParaStylesShift,
        ListStylesShift: TRVIntegerList); override;
      function AsText(LineWidth: Integer; RVData: TPersistent;
        const Text, Path: String; TextOnly,Unicode: Boolean): String; override;
      function GetIndexInList(List: TList): Integer;
      function SetExtraIntProperty(Prop: TRVExtraItemProperty;
        Value: Integer): Boolean; override;
      function GetExtraIntProperty(Prop: TRVExtraItemProperty;
        var Value: Integer): Boolean; override;
  end;

  TRVMarkerList = class (TList)
  public
    PrevMarkerList: TRVMarkerList;
    function InsertAfter(InsertMe, AfterMe: TRVMarkerItemInfo): Integer;
    procedure RecalcCounters(StartFrom: Integer; RVStyle: TRVStyle);
    function FindParentMarker(Index: Integer; Marker: TRVMarkerItemInfo;
      var ParentList: TRVMarkerList; var ParentIndex: Integer): Boolean;
    procedure RecalcDisplayStrings(RVStyle: TRVStyle);
    procedure SaveToStream(Stream: TStream; Count: Integer; IncludeSize: Boolean);
    procedure LoadFromStream(Stream: TStream; RVData: TPersistent;
      IncludeSize: Boolean);
    procedure SaveTextToStream(Stream: TStream; Count: Integer);
    procedure LoadText(const s: String; RVData: TPersistent);
    procedure LoadBinary(const s: String; RVData: TPersistent);
  end;

  function RVGetLevelInfo(RVStyle: TRVStyle; ListNo, Level: Integer): TRVListLevel;

{$ENDIF}

implementation
{$IFNDEF RVDONOTUSELISTS}
uses CRVData, CRVFData, RichView;

{============================= TRVMarkerItemInfo ==============================}
constructor TRVMarkerItemInfo.CreateEx(RVData: TPersistent; AListNo,
  ALevel, AStartFrom: Integer; AReset: Boolean);
begin
  inherited Create(RVData);
  StyleNo   := rvsListMarker;
  ListNo    := AListNo;
  Level     := ALevel;
  StartFrom := AStartFrom;
  Reset     := AReset;
  SameAsPrev := False;
  Counter   := 1;
  FCachedIndexInList := -1;
end;
{------------------------------------------------------------------------------}
constructor TRVMarkerItemInfo.Create(RVData: TPersistent);
begin
  inherited Create(RVData);
  SameAsPrev := False;
  Counter   := 1;
  FCachedIndexInList := -1;  
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.Assign(Source: TCustomRVItemInfo);
begin
  if Source is TRVMarkerItemInfo then begin
    ListNo    := TRVMarkerItemInfo(Source).ListNo;
    Level     := TRVMarkerItemInfo(Source).Level;
    StartFrom := TRVMarkerItemInfo(Source).StartFrom;
    Reset     := TRVMarkerItemInfo(Source).Reset;
    NoHTMLImageSize := TRVMarkerItemInfo(Source).NoHTMLImageSize;
  end;
  inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.MovingToUndoList(ItemNo: Integer; RVData,
  AContainerUndoItem: TObject);
begin
  inherited;
  TCustomRVData(RVData).DeleteMarkerFromList(TCustomRVData(RVData).GetItem(ItemNo), False);
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.MovingFromUndoList(ItemNo: Integer; RVData: TObject);
begin
  inherited;
  TCustomRVData(RVData).AddMarkerInList(ItemNo);
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.ReadRVFHeader(var P: PChar;
  RVData: TPersistent): Boolean;
var v: Integer;
begin
  Result :=  RVFReadInteger(P,ListNo) and
             RVFReadInteger(P,Level) and
             RVFReadInteger(P,StartFrom) and
             RVFReadInteger(P,v);
  Reset := v<>0;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.SaveRVFHeaderTail(RVData: TPersistent): String;
begin
  Result := Format('%d %d %d %d', [ListNo, Level, StartFrom, ord(Reset)]);
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.SaveRVF(Stream: TStream; RVData: TPersistent;
  ItemNo, ParaNo: Integer; const Name: String; Part: TRVMultiDrawItemPart;
  ForceSameAsPrev: Boolean);
begin
  RVFWriteLine(Stream, Format('%d %d %s %d %d %s %s',
    [StyleNo, GetRVFExtraPropertyCount,
     RVFItemSavePara(ParaNo, TCustomRVData(RVData), False),
     Byte(ItemOptions) and RVItemOptionsMask,
     0, RVFSaveTag(rvoTagsArePChars in TCustomRVData(RVData).Options,Tag),
     SaveRVFHeaderTail(RVData)]));
  SaveRVFExtraProperties(Stream);
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetLevelInfoEx(RVStyle: TRVStyle; LevelNo: Integer): TRVListLevel;
begin
  if LevelNo>=RVStyle.ListStyles[ListNo].Levels.Count then
    LevelNo :=RVStyle.ListStyles[ListNo].Levels.Count-1;
  Result := RVStyle.ListStyles[ListNo].Levels[LevelNo];
end;
{------------------------------------------------------------------------------}
function RVGetLevelInfo(RVStyle: TRVStyle; ListNo, Level: Integer): TRVListLevel;
var LevelNo: Integer;
begin
  Result := nil;
  if ListNo<0 then
    exit;
  LevelNo := Level;
  if LevelNo>=RVStyle.ListStyles[ListNo].Levels.Count then
    LevelNo :=RVStyle.ListStyles[ListNo].Levels.Count-1;
  if LevelNo<RVStyle.ListStyles[ListNo].Levels.Count then
    Result := RVStyle.ListStyles[ListNo].Levels[LevelNo];
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetLevelInfo(RVStyle: TRVStyle): TRVListLevel;
var LevelNo: Integer;
begin
  Result := nil;
  if ListNo<0 then
    exit;
  LevelNo := Level;
  if LevelNo>=RVStyle.ListStyles[ListNo].Levels.Count then
    LevelNo :=RVStyle.ListStyles[ListNo].Levels.Count-1;
  if LevelNo<RVStyle.ListStyles[ListNo].Levels.Count then
    Result := RVStyle.ListStyles[ListNo].Levels[LevelNo];
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.CalcSize(Canvas: TCanvas; RVData: TPersistent;
                                     var Width, Height, Desc, Overhang: Integer;
                                     sad: PRVScreenAndDevice;
                                     ForMinWidth: Boolean;
                                     var HShift, SpaceBefore: Integer);
var sz: TSize;
    LevelInfo: TRVListLevel;
    TextMetric: TTextMetric;
    RVStyle: TRVStyle;
    {.........................................................}
    procedure CountLR(var LeftWidth, RightWidth: Integer);
    begin
      case LevelInfo.MarkerAlignment of
        rvmaLeft:
          begin
            LeftWidth := 0;
            RightWidth := Width;
          end;
        rvmaRight:
          begin
            LeftWidth := Width;
            RightWidth := 0;
          end;
        rvmaCenter:
          begin
            RightWidth := Width div 2;
            LeftWidth := Width - RightWidth;
          end;
      end;
    end;
    {.........................................................}
    procedure CountWidth(UseSad: Boolean);
    var  LeftWidth, RightWidth, w: Integer;
    begin
      CountLR(LeftWidth, RightWidth);
      if UseSaD and (sad<>nil) then
        w := MulDiv(LevelInfo.FirstIndent+LevelInfo.LeftIndent-LevelInfo.MarkerIndent, sad.ppixDevice, sad.ppixScreen)
      else
        w := LevelInfo.FirstIndent+LevelInfo.LeftIndent-LevelInfo.MarkerIndent;
      if ForMinWidth then begin
        Width := RightWidth;
        if Width<w then
          Width := w;
        end
      else begin
        if RightWidth<w then
          RightWidth := w;
        if TCustomRVData(RVData).GetParaBiDiMode(ParaNo)=rvbdRightToLeft then begin
          HShift :=  LeftWidth;
          SpaceBefore := LeftWidth+RightWidth-Width;
          end
        else begin
          HShift := - LeftWidth;
          SpaceBefore := 0;
        end;
        Width := LeftWidth+RightWidth;
        Overhang := HShift;
      end;
    end;
    {.........................................................}
begin
  if (ListNo<0) or (Level<0) then begin
    Width := 0;
    Height := 0;
    HShift := 0;
    Desc := 0;
    Overhang := 0;
    SpaceBefore := 0;
    exit;
  end;
  RVStyle := TCustomRVData(RVData).GetRVStyle;
  LevelInfo := GetLevelInfo(RVStyle);
  Desc := 0;
  case LevelInfo.ListType of
    rvlstPicture:
      begin
        if LevelInfo.HasPicture then begin
          Width := LevelInfo.Picture.Graphic.Width;
          Height := LevelInfo.Picture.Graphic.Height;
          if sad<>nil then begin
            Width := MulDiv(Width, sad.ppixDevice, sad.ppixScreen);
            Height := MulDiv(Height, sad.ppiyDevice, sad.ppiyScreen);
          end;
          end
        else begin
          Width := 0;
          Height := 0;
        end;
        CountWidth(True);
      end;
    rvlstImageList, rvlstImageListCounter:
      begin
        if LevelInfo.ImageList<>nil then begin
          Width := TImageList(LevelInfo.ImageList).Width;
          Height := TImageList(LevelInfo.ImageList).Height;
          if sad<>nil then begin
            Width := MulDiv(Width, sad.ppixDevice, sad.ppixScreen);
            Height := MulDiv(Height, sad.ppiyDevice, sad.ppiyScreen);
          end;
          end
        else begin
          Width := 0;
          Height := 0;
        end;
        CountWidth(True);
      end;
    {$IFNDEF RVDONOTUSEUNICODE}
    {$IFDEF RICHVIEWCBDEF3}
    rvlstUnicodeBullet:
      begin
        Canvas.Font := LevelInfo.Font;
        if (RVStyle.TextStyles.PixelsPerInch<>0) and (LevelInfo.Font.Size>0) then
          Canvas.Font.Height := - MulDiv(LevelInfo.Font.Size, RVStyle.TextStyles.PixelsPerInch, 72);
        {$IFNDEF RVDONOTUSECHARSPACING}
        SetTextCharacterExtra(Canvas.Handle, 0);
        {$ENDIF}
        SetTextAlign(Canvas.Handle, TA_LEFT);
        GetTextExtentPoint32W(Canvas.Handle, Pointer(LevelInfo.FormatStringW),
           Length(LevelInfo.FormatStringW), sz);
        GetTextMetrics(Canvas.Handle, TextMetric);
        Desc := TextMetric.tmDescent;
        Width := sz.cx;
        Height := sz.cy;
        CountWidth(True);
      end;
    {$ENDIF}
    {$ENDIF}
    else
      begin
        Canvas.Font := LevelInfo.Font;
        if (RVStyle.TextStyles.PixelsPerInch<>0) and (LevelInfo.Font.Size>0) then
          Canvas.Font.Height := - MulDiv(LevelInfo.Font.Size, RVStyle.TextStyles.PixelsPerInch, 72);
        {$IFNDEF RVDONOTUSECHARSPACING}
        SetTextCharacterExtra(Canvas.Handle, 0);
        {$ENDIF}
        SetTextAlign(Canvas.Handle, TA_LEFT);
        GetTextExtentPoint32(Canvas.Handle, PChar(DisplayString),
           Length(DisplayString), sz);
        GetTextMetrics(Canvas.Handle, TextMetric);
        Desc := TextMetric.tmDescent;
        Width := sz.cx;
        Height := sz.cy;
        CountWidth(True);
      end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetBoolValue(Prop: TRVItemBoolProperty): Boolean;
begin
  case Prop of
    rvbpDrawingChangesFont, rvbpCanSaveUnicode {, rvbpAlwaysInText}:
      Result := True;
    else
      Result := inherited GetBoolValue(Prop);
  end;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetBoolValueEx(Prop: TRVItemBoolPropertyEx; RVStyle: TRVStyle): Boolean;
var LevelInfo: TRVListLevel;
begin
  case Prop of
    rvbpActualPrintSize:
      begin
        (*
        LevelInfo := GetLevelInfo(RVStyle);
        if LevelInfo<>nil then
          Result := LevelInfo.ListType in [rvlstDecimal, rvlstLowerAlpha, rvlstUpperAlpha,
            rvlstBullet,
            rvlstLowerRoman, rvlstUpperRoman {$IFNDEF RVDONOTUSEUNICODE}, rvlstUnicodeBullet{$ENDIF}]
        else
        *)
          Result := True;
      end;
    rvbpPrintToBMP:
      begin
        LevelInfo := GetLevelInfo(RVStyle);
        if LevelInfo<>nil then
          Result := (LevelInfo.ListType in [rvlstImageList, rvlstImageListCounter]) or
                  ((LevelInfo.ListType=rvlstPicture) and LevelInfo.HasPicture and
                    not (LevelInfo.Picture.Graphic is TMetafile))
        else
          Result := False;
      end;
    else
      Result := inherited GetBoolValueEx(Prop,RVStyle);
  end;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetHeight: Integer;
begin
  Result := FHeight;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetWidth: Integer;
begin
  Result := FWidth;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetLeftOverhang: Integer;
begin
  Result := FOverhang;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetDescent: Integer;
begin
  Result := FDescent;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetMinWidth(sad: PRVScreenAndDevice;
  Canvas: TCanvas; RVData: TPersistent): Integer;
var h,d,s,o,sb: Integer;
begin
  CalcSize(Canvas, RVData, Result, h, d, o, sad, True, s, sb);
  if not GetBoolValueEx(rvbpActualPrintSize, TCustomRVData(RVData).GetRVStyle) and
     (sad<>nil) then
    Result := MulDiv(Result, sad.ppixDevice, sad.ppixScreen);
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.OnDocWidthChange(DocWidth: Integer;
  dli: TRVDrawLineInfo; Printing: Boolean; Canvas: TCanvas;
  RVData: TPersistent; sad: PRVScreenAndDevice;
  var HShift, Desc: Integer; NoCaching: Boolean);
var Oh: Integer;
begin
  CalcSize(Canvas, RVData, dli.Width, dli.Height, Desc, Oh, sad, False, HShift, dli.SpaceBefore);
  if not Printing then begin
    FWidth := dli.Width;
    FHeight := dli.Height;
    FDescent  := Desc;
    FOverhang := Oh;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.CalcDisplayString(RVStyle: TRVStyle;
   List: TRVMarkerList; Index: Integer);
var LevelInfo: TRVListLevel;
  {.......................................................}
  function IntToRoman(Value: Integer): String;
  const
    Arabics: Array[0..12] of Integer =
    (1,4,5,9,10,40,50,90,100,400,500,900,1000);
    Romans:  Array[0..12] of String =
    ('I','IV','V','IX','X','XL','L','XC','C','CD','D','CM','M');
  var i: Integer;
  begin
    if Value<1 then begin
      Result := '?';
      exit;
    end;
    Result := '';
    for i := 12 downto 0 do
      while (Value >= Arabics[i]) do begin
        Value := Value - Arabics[i];
        Result := Result + Romans[i];
      end;
  end;
  {.......................................................}
  function Number2Str(Value: Integer; ListType: TRVListType): String;
  const CharCount = ord('z')-ord('a')+1;
  var FirstCharCode: Integer;
  begin
    case ListType of
      rvlstDecimal, rvlstImageListCounter:
        Result := IntToStr(Value);
      rvlstLowerAlpha, rvlstUpperAlpha:
        begin
           Result := '';
           if ListType=rvlstLowerAlpha then
             FirstCharCode := ord('a')
           else
             FirstCharCode := ord('A');
           while Value>0 do begin
             Result := Chr((Value-1) mod CharCount+FirstCharCode)+Result;
             Value := (Value-1) div CharCount;
           end;
        end;
      rvlstUpperRoman:
        Result := IntToRoman(Value);
      rvlstLowerRoman:
        Result := LowerCase(IntToRoman(Value));
      else
        Result := '';
    end;
  end;

  {.......................................................}
  function MultiLevelList : String;
  var CountersVal: array [0..255] of TVarRec;
      CountersStr: array [0..255] of String;
      ParentIndex, CurIndex, i: Integer;
      ALevelInfo: TRVListLevel;
      ParentLevelNo, CurLevelNo : Integer;
      ParentList, CurList: TRVMarkerList;
      Marker: TRVMarkerItemInfo;
      LegalStyle : Boolean;
      ListType : TRVListType;
  begin
    LegalStyle := rvloLegalStyleNumbering in LevelInfo.Options;

    CurLevelNo := Level;
    Marker  := Self;
    ALevelInfo := LevelInfo;
    CurIndex := Index;
    CurList  := List;

    while True do begin
      ListType := ALevelInfo.ListType;
      if (CurLevelNo<Level) and (ListType in [rvlstLowerRoman, rvlstUpperRoman, rvlstLowerAlpha, rvlstUpperAlpha]) and
         LegalStyle then
        ListType := rvlstDecimal;
      CountersStr[CurLevelNo]             := Number2Str(Marker.Counter, ListType);
      if CountersStr[CurLevelNo]<>'' then
        CountersVal[CurLevelNo].VAnsiString := PChar(CountersStr[CurLevelNo])
      else
        CountersVal[CurLevelNo].VAnsiString := nil;
      CountersVal[CurLevelNo].VType       := vtAnsiString;

      if CurLevelNo=0 then
        break;

      if CurList.FindParentMarker(CurIndex, nil, ParentList, ParentIndex) then begin
        Marker     := TRVMarkerItemInfo(ParentList.Items[ParentIndex]);
        ALevelInfo := Marker.GetLevelInfo(RVStyle);
        ParentLevelNo := Marker.Level;
        end
      else begin
        Marker     := nil;
        ALevelInfo := nil;
        ParentLevelNo := -1;
      end;
      for i := CurLevelNo-1 downto ParentLevelNo+1 do begin
        with GetLevelInfoEx(RVStyle,i) do
          CountersStr[i]  := Number2Str(StartFrom, ListType);
        if CountersStr[i]<>'' then
          CountersVal[i].VAnsiString := PChar(CountersStr[i])
        else
          CountersVal[i].VAnsiString := nil;
        CountersVal[i].VType       := vtAnsiString;
      end;
      if ParentLevelNo<0 then
        break;
      CurLevelNo := ParentLevelNo;
      CurIndex   := ParentIndex;
      CurList    := ParentList;
    end;
    Result := Format(LevelInfo.FormatString, CountersVal);
  end;
  {.......................................................}
begin
  LevelInfo := GetLevelInfo(RVStyle);
  case LevelInfo.ListType of
    rvlstBullet:
      DisplayString := LevelInfo.FormatString;
    {$IFNDEF RVDONOTUSEUNICODE}
    rvlstUnicodeBullet:
      DisplayString := ''; // RVU_UnicodeToAnsi(RVStyle.DefCodePage, PChar(Pointer(LevelInfo.FormatStringW)))
    {$ENDIF}
    rvlstPicture, rvlstImageList, rvlstImageListCounter:
      DisplayString := '';
    else
      DisplayString := MultiLevelList;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.Paint(x, y: Integer; Canvas: TCanvas;
  State: TRVItemDrawStates; Style: TRVStyle; dli: TRVDrawLineInfo);
begin
  DoPaint(x, y, Canvas, State, Style, dli, rvcmColor);
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.DoPaint(x, y: Integer; Canvas: TCanvas;
  State: TRVItemDrawStates; Style: TRVStyle; dli: TRVDrawLineInfo;
  ColorMode: TRVColorMode);
var LevelInfo: TRVListLevel;
    Index: Integer;
begin
  if (ListNo<0) or (Level<0) then
    exit;
  Canvas.Pen.Color := clRed;
  if dli<>nil then
    inc(x, dli.SpaceBefore);
  LevelInfo := GetLevelInfo(TRVStyle(Style));
  case LevelInfo.ListType of
    rvlstPicture:
      begin
        if LevelInfo.HasPicture then
          Canvas.Draw(X,Y, LevelInfo.Picture.Graphic);
      end;
    rvlstImageList, rvlstImageListCounter:
      begin
        Index := LevelInfo.ImageIndex;
        if LevelInfo.ListType = rvlstImageListCounter then
          inc(Index, Counter-1);
        if (LevelInfo.ImageList<>nil) and (Index>=0) and (Index<LevelInfo.ImageList.Count) then
          LevelInfo.ImageList.Draw(Canvas, X,Y, Index);
      end;
    {$IFNDEF RVDONOTUSEUNICODE}
    {$IFDEF RICHVIEWCBDEF3}
    rvlstUnicodeBullet:
      begin
        Canvas.Font := LevelInfo.Font;
        Canvas.Font.Color := RV_GetColor(Canvas.Font.Color, ColorMode);
        if (Style.TextStyles.PixelsPerInch<>0) and (LevelInfo.Font.Size>0) then
          Canvas.Font.Height := - MulDiv(LevelInfo.Font.Size, Style.TextStyles.PixelsPerInch, 72);
        {$IFNDEF RVDONOTUSECHARSPACING}
        SetTextCharacterExtra(Canvas.Handle, 0);
        {$ENDIF}
        SetTextAlign(Canvas.Handle, TA_LEFT);
        Canvas.Brush.Style := bsClear;
        TextOutW(Canvas.Handle, X,Y, Pointer(LevelInfo.FormatStringW), Length(LevelInfo.FormatStringW));
      end;
    {$ENDIF}
    {$ENDIF}
    else
      begin
        Canvas.Font := LevelInfo.Font;
        Canvas.Font.Color := RV_GetColor(Canvas.Font.Color, ColorMode);        
        if (Style.TextStyles.PixelsPerInch<>0) and (LevelInfo.Font.Size>0) then
          Canvas.Font.Height := - MulDiv(LevelInfo.Font.Size, Style.TextStyles.PixelsPerInch, 72);
        {$IFNDEF RVDONOTUSECHARSPACING}
        SetTextCharacterExtra(Canvas.Handle, 0);
        {$ENDIF}
        SetTextAlign(Canvas.Handle, TA_LEFT);
        Canvas.Brush.Style := bsClear;
        Canvas.TextOut(X,Y, DisplayString);
      end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.Print(Canvas: TCanvas; x, y, x2: Integer;
  Preview, Correction: Boolean; const sad: TRVScreenAndDevice;
  RichView: TRVScroller; dli: TRVDrawLineInfo; Part: Integer;
  ColorMode: TRVColorMode; RVData: TPersistent);
begin
  DoPaint(x, y, Canvas, [], TCustomRichView(RichView).Style, dli, ColorMode);
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.PrintToBitmap(Bkgnd: TBitmap; Preview: Boolean; RichView: TRVScroller;
  dli: TRVDrawLineInfo; Part: Integer; ColorMode: TRVColorMode):Boolean;
begin
  DoPaint(0, 0, Bkgnd.Canvas, [], TCustomRichView(RichView).Style, dli, ColorMode);
  Result := True;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetImageHeight(RVStyle: TRVStyle): Integer;
var LevelInfo: TRVListLevel;
begin
  Result := 0;
  LevelInfo := GetLevelInfo(RVStyle);
  case LevelInfo.ListType of
    rvlstImageList, rvlstImageListCounter:
      if LevelInfo.ImageList<>nil then
        Result := TImageList(LevelInfo.ImageList).Height;
    rvlstPicture:
      if LevelInfo.HasPicture then
        Result := LevelInfo.Picture.Graphic.Height;
  end;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetImageWidth(RVStyle: TRVStyle): Integer;
var LevelInfo: TRVListLevel;
begin
  Result := 0;
  LevelInfo := GetLevelInfo(RVStyle);
  case LevelInfo.ListType of

    rvlstImageList, rvlstImageListCounter:
      if LevelInfo.ImageList<>nil then
        Result := TImageList(LevelInfo.ImageList).Width;
        // Result := FWidth;
    rvlstPicture:
      if LevelInfo.HasPicture then
        Result := LevelInfo.Picture.Graphic.Width;
        //Result := FWidth;
  end;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERTF}
procedure TRVMarkerItemInfo.FillRTFTables(ColorList: TRVColorList;
  ListOverrideCountList: TRVIntegerList; RVData: TPersistent);
var LevelInfo: TRVListLevel;
begin
  if not Reset or (ListNo<0) or (Level<0) then
    exit;
  LevelInfo := GetLevelInfo(TCustomRVData(RVData).GetRVStyle);
  if LevelInfo=nil then
    exit;
  if LevelInfo.HasNumbering and (LevelInfo.ListType<>rvlstImageListCounter) then
    ListOverrideCountList[ListNo] := ListOverrideCountList[ListNo]+1;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.SaveRTF(Stream: TStream; RVData: TPersistent;
  ItemNo: Integer; const Name: String; TwipsPerPixel: Double;
  Level: Integer; ColorList: TRVColorList;
  StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
  FontTable: TRVList);
var LevelInfo: TRVListLevel;
   {$IFNDEF RVDONOTUSEUNICODE}
   {$IFDEF RICHVIEWCBDEF3}
    s: String;
   {$ENDIF}
   {$ENDIF}
   ListOverrideNo: Integer;
begin
  LevelInfo := GetLevelInfo(TCustomRVData(RVData).GetRVStyle);
  if LevelInfo=nil then
    exit;
  case LevelInfo.ListType of
    rvlstPicture:
      RVSaveImageToRTF(Stream,TwipsPerPixel, LevelInfo.Picture.Graphic,
        0, 0, TCustomRVData(RVData).RTFOptions);
    rvlstImageList:
      RVSaveImageListImageToRTF(Stream, TwipsPerPixel, LevelInfo.ImageList,
        LevelInfo.ImageIndex, TCustomRVData(RVData).RTFOptions);
    rvlstImageListCounter:
      RVSaveImageListImageToRTF(Stream, TwipsPerPixel, LevelInfo.ImageList,
        LevelInfo.ImageIndex+Counter-1, TCustomRVData(RVData).RTFOptions);
    else
      begin
        RVFWrite(Stream, '{\listtext\pard\plain');
        RVSaveFontToRTF(Stream, LevelInfo.Font, ColorList, TRVRTFFontTable(FontTable),
          TCustomRVData(RVData).GetRVStyle);
        RVFWrite(Stream, ' ');
        {$IFNDEF RVDONOTUSEUNICODE}
        {$IFDEF RICHVIEWCBDEF3}
        if LevelInfo.ListType=rvlstUnicodeBullet then begin
          SetLength(s, Length(LevelInfo.FormatStringW)*2);
          Move(Pointer(LevelInfo.FormatStringW)^, Pointer(s)^, Length(s));
          RVWriteUnicodeRTFStr(Stream, s, TCustomRVData(RVData).GetRVStyle.DefCodePage,
          rvrtfDuplicateUnicode in TCustomRVData(RVData).RTFOptions, False);
          end
        else
        {$ENDIF}
        {$ENDIF}
        begin
          RVFWrite(Stream, RVMakeRTFStr(DisplayString, False))
        end;
        if Reset and LevelInfo.HasNumbering then begin
          ListOverrideOffsetsList1[ListNo] := ListOverrideOffsetsList1[ListNo]+1;
          ListOverrideNo := ListOverrideOffsetsList1[ListNo];
          end
        else
          ListOverrideNo := ListOverrideOffsetsList2[ListNo];
        RVFWrite(Stream, Format('\tab}\ls%d\ilvl%d', [ListOverrideNo, Self.Level]));
      end;
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.HTMLOpenOrCloseTags(Stream: TStream;
  OldLevelNo, NewLevelNo: Integer; RVStyle: TRVStyle; UseCSS: Boolean);
var i: Integer;
    LevelInfo: TRVListLevel;
begin
  for i := OldLevelNo downto NewLevelNo+1 do begin
    LevelInfo := GetLevelInfoEx(RVStyle,i);
    LevelInfo.HTMLCloseTag(Stream, UseCSS);
  end;
  for i := OldLevelNo+1 to NewLevelNo do begin
    LevelInfo := GetLevelInfoEx(RVStyle,i);
    LevelInfo.HTMLOpenTag(Stream, UseCSS);
  end;
  if OldLevelNo<>NewLevelNo then
    RVFWriteLine(Stream,'');  
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.SaveHTMLSpecial(Stream: TStream;
  Prev: TRVMarkerItemInfo; RVStyle: TRVStyle; UseCSS: Boolean);
begin
  if Prev<>nil then
    if Prev.ListNo<>ListNo then begin
      Prev.HTMLOpenOrCloseTags(Stream, Prev.Level, -1, RVStyle, UseCSS);
      HTMLOpenOrCloseTags(Stream, -1, Level, RVStyle, UseCSS)
      end
    else
      Prev.HTMLOpenOrCloseTags(Stream, Prev.Level, Level, RVStyle, UseCSS)
  else
    HTMLOpenOrCloseTags(Stream, -1, Level, RVStyle, UseCSS);
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.SaveToHTML(Stream: TStream; RVData: TPersistent;
                     ItemNo: Integer;
                     const Text, Path: String;
                     const imgSavePrefix: String;
                     var imgSaveNo: Integer;
                     CurrentFileColor: TColor;
                     SaveOptions: TRVSaveOptions;
                     UseCSS: Boolean;
                     Bullets: TRVList);
var LevelInfo: TRVListLevel;
    DoDefault : Boolean;
    Location: String;
    ImageIndex, W,H: Integer;
begin
  if not (rvsoMarkersAsText in SaveOptions) then
    exit;
  LevelInfo := GetLevelInfo(TCustomRVData(RVData).GetRVStyle);
  if LevelInfo=nil then
    exit;
  case LevelInfo.ListType of
    rvlstBullet, rvlstDecimal, rvlstLowerAlpha, rvlstUpperAlpha, rvlstLowerRoman,
    rvlstUpperRoman {$IFNDEF RVDONOTUSEUNICODE}, rvlstUnicodeBullet{$ENDIF}:
      begin
        if not UseCSS then
          RVFWrite(Stream, RV_HTMLOpenFontTag2(LevelInfo.Font, TCustomRVData(RVData).GetRVStyle.TextStyles[0]))
        else begin
          RVFWrite(Stream, Format('<SPAN style="{%s', [RV_GetHTMLFontCSS(LevelInfo.Font)]));
          if LevelInfo.MarkerIndent>=LevelInfo.LeftIndent then
            RVFWrite(Stream, Format(' width: %dpx;', // workaround for IE buggy rendering: not saving width for hanging indents
              [LevelInfo.LeftIndent+LevelInfo.FirstIndent-LevelInfo.MarkerIndent]));
          RVFWrite(Stream, '}">');
        end;
        {$IFNDEF RVDONOTUSEUNICODE}
        if LevelInfo.ListType=rvlstUnicodeBullet then begin
          {$IFDEF RICHVIEWCBDEF3}
          RVU_GetHTMLEncodedUnicode(RVU_GetRawUnicode(LevelInfo.FormatStringW), False, False)
          {$ENDIF}
          end
        else
        {$ENDIF}
          RVFWrite(Stream, DisplayString);
        if not UseCSS then
          RVFWriteLine(Stream,RV_HTMLCloseFontTag2(LevelInfo.Font, TCustomRVData(RVData).GetRVStyle.TextStyles[0]))
        else
          RVFWriteLine(Stream,'</SPAN>');
      end;
    rvlstImageList,rvlstImageListCounter:
      begin
        TCustomRVData(RVData).HTMLSaveImage(TCustomRVData(RVData), ItemNo, Path, CurrentFileColor, Location, DoDefault);
        if DoDefault then begin
          ImageIndex := LevelInfo.ImageIndex;
          if LevelInfo.ListType=rvlstImageListCounter then
            inc(ImageIndex, Counter-1);
          if (ImageIndex>=0) and (ImageIndex<LevelInfo.ImageList.Count) and
             (LevelInfo.ImageList<>nil) then begin
            RVSaveImageSharedImageInHTML(LevelInfo.ImageList, ImageIndex, nil, Location, RVData, Path,
              imgSavePrefix, imgSaveNo, CurrentFileColor, SaveOptions, Bullets);
          end;
        end;
        if UseCSS and (LevelInfo.MarkerIndent>=LevelInfo.LeftIndent) then
          RVFWrite(Stream, Format('<SPAN style="width:%dpx">',
            [LevelInfo.LeftIndent+LevelInfo.FirstIndent-LevelInfo.MarkerIndent]));
        if Location<>'' then begin
          if LevelInfo.ImageList=nil then begin
            W := 0;
            H := 0;
            Exclude(SaveOptions, rvsoImageSizes);
            end
          else begin
            W := TImageList(LevelInfo.ImageList).Width;
            H := TImageList(LevelInfo.ImageList).Height;
          end;
          RVFWriteLine(Stream,
            Format('<IMG%ssrc="%s">',[RV_GetExtraIMGStr(SaveOptions, W, H, NoHTMLImageSize),Location]));
        end;
        if UseCSS and (LevelInfo.MarkerIndent>=LevelInfo.LeftIndent) then
          RVFWrite(Stream, Format('</SPAN>',
            [LevelInfo.LeftIndent+LevelInfo.FirstIndent-LevelInfo.MarkerIndent]));
      end;
    rvlstPicture:
      begin
        TCustomRVData(RVData).HTMLSaveImage(TCustomRVData(RVData), ItemNo, Path, CurrentFileColor, Location, DoDefault);
        if DoDefault and (LevelInfo.Picture.Graphic<>nil) then
          RVSaveImageSharedImageInHTML(nil, -1, LevelInfo.Picture.Graphic, Location, RVData, Path,
            imgSavePrefix, imgSaveNo, CurrentFileColor, SaveOptions, Bullets);
        if Location<>'' then begin
          if LevelInfo.Picture.Graphic=nil then begin
            W := 0;
            H := 0;
            Exclude(SaveOptions, rvsoImageSizes);
            end
          else begin
            W := LevelInfo.Picture.Graphic.Width;
            H := LevelInfo.Picture.Graphic.Height;
          end;
          RVFWriteLine(Stream,
            Format('<IMG%ssrc="%s">',
              [RV_GetExtraIMGStr(SaveOptions, W, H, NoHTMLImageSize),Location]));
        end;
      end;
  end
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetLICSS(RVData: TPersistent; ItemNo: Integer; const Path,
        imgSavePrefix: String; var imgSaveNo: Integer; CurrentFileColor: TColor;
        SaveOptions: TRVSaveOptions; Bullets: TRVList): String;
var LevelInfo: TRVListLevel;
    DoDefault : Boolean;
    Location: String;
    ImageIndex: Integer;
begin
  Result := '';
  LevelInfo := GetLevelInfo(TCustomRVData(RVData).GetRVStyle);
  case LevelInfo.ListType of
    rvlstImageList,rvlstImageListCounter:
      begin
        TCustomRVData(RVData).HTMLSaveImage(TCustomRVData(RVData), ItemNo, Path, CurrentFileColor, Location, DoDefault);
        if DoDefault then begin
          ImageIndex := LevelInfo.ImageIndex;
          if LevelInfo.ListType=rvlstImageListCounter then
            inc(ImageIndex, Counter-1);
          if (ImageIndex>=0) and (ImageIndex<LevelInfo.ImageList.Count) and
             (LevelInfo.ImageList<>nil) then begin
            RVSaveImageSharedImageInHTML(LevelInfo.ImageList, ImageIndex, nil, Location, RVData, Path,
              imgSavePrefix, imgSaveNo, CurrentFileColor, SaveOptions, Bullets);
          end;
        end;
        if Location<>'' then
          Result := Format('list-style: url(''%s'')',[Location]);
      end;
    rvlstPicture:
      begin
        TCustomRVData(RVData).HTMLSaveImage(TCustomRVData(RVData), ItemNo, Path, CurrentFileColor, Location, DoDefault);
        if DoDefault and (LevelInfo.Picture.Graphic<>nil) then
            RVSaveImageSharedImageInHTML(nil, -1, LevelInfo.Picture.Graphic, Location, RVData, Path,
              imgSavePrefix, imgSaveNo, CurrentFileColor, SaveOptions, Bullets);
        if Location<>'' then
          Result := Format('list-style: url(''%s'')',[Location]);
      end;
  end;
  if Result<>'' then
    Result := Format(' style="{ %s }"', [Result]);
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.MarkStylesInUse(UsedTextStyles, UsedParaStyles,
  UsedListStyles: TRVIntegerList);
begin
  inherited;
  if ListNo>=0 then
    UsedListStyles[ListNo] := 1;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.UpdateStyles(TextStylesShift, ParaStylesShift,
  ListStylesShift: TRVIntegerList);
begin
  inherited;
  if ListNo>=0 then
    dec(ListNo,ListStylesShift[ListNo]-1);
end;
{------------------------------------------------------------------------------}
function ProcessSymbolString(const s: String): String;
var i: Integer;
begin
  Result := s;
  for i := 1 to Length(s) do begin
    if Result[i] in [#$A7..#$AA,#$B7,#$D7,#$E0] then
      Result[i] := '*';
  end;
  RV_ReplaceStr(Result,#$DB,'<=>');
  RV_ReplaceStr(Result,#$DC,'<=');
  RV_ReplaceStr(Result,#$DE,'=>');
  RV_ReplaceStr(Result,#$AB,'<->');
  RV_ReplaceStr(Result,#$AC,'<-');
  RV_ReplaceStr(Result,#$AE,'->');
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.AsText(LineWidth: Integer; RVData: TPersistent;
  const Text, Path: String; TextOnly, Unicode: Boolean): String;
var LevelInfo: TRVListLevel;
  {$IFNDEF RVDONOTUSEUNICODE}
    i: Integer;
  {$ENDIF}
begin
  Result := '';
  LevelInfo := GetLevelInfo(TCustomRVData(RVData).GetRVStyle);
  case LevelInfo.ListType of
    rvlstBullet,rvlstDecimal,rvlstLowerAlpha,rvlstUpperAlpha,
    rvlstLowerRoman,rvlstUpperRoman:
      begin
        if (AnsiCompareText(LevelInfo.Font.Name,'Symbol')=0) then
          Result := ProcessSymbolString(DisplayString)
        else
          Result := DisplayString;
        Result := Result+' ';
        if Unicode then
          Result := RVU_AnsiToUnicode(CP_ACP, Result)
      end;
    rvlstPicture,rvlstImageList,rvlstImageListCounter:
      begin
        Result := '* ';
        if Unicode then
          Result := RVU_AnsiToUnicode(CP_ACP, Result)
      end;
    {$IFNDEF RVDONOTUSEUNICODE}
    {$IFDEF RICHVIEWCBDEF3}
    rvlstUnicodeBullet:
      begin
        Result := RVU_GetRawUnicode(LevelInfo.FormatStringW+' ');
        if not Unicode then begin
          Result := RVU_UnicodeToAnsi(CP_ACP, Result);
          for i := 1 to Length(Result) do begin
            if i>Length(LevelInfo.FormatStringW) then
              break;
            if (Result[i]='?') and (LevelInfo.FormatStringW[i]<>'?') then
              Result[i] := '*';
          end;
        end;
      end;
    {$ENDIF}
    {$ENDIF}
  end;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetIndexInList(List: TList): Integer;
begin
  if List=nil then begin
    Result := -1;
    exit;
  end;
  if (FCachedIndexInList<0) or (FCachedIndexInList>=List.Count) or
     (List.Items[FCachedIndexInList]<>Self) then
    FCachedIndexInList := List.IndexOf(Self);
  Result := FCachedIndexInList;
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetRVFExtraPropertyCount: Integer;
begin
  Result := inherited GetRVFExtraPropertyCount;
  if NoHTMLImageSize then
    inc(Result);
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerItemInfo.SaveRVFExtraProperties(Stream: TStream);
begin
  inherited SaveRVFExtraProperties(Stream);
  if NoHTMLImageSize then
    WriteRVFExtraIntPropertyStr(Stream, rvepNoHTMLImageSize, 1);
end;
{------------------------------------------------------------------------------}
function TRVMarkerItemInfo.GetExtraIntProperty(Prop: TRVExtraItemProperty;
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
function TRVMarkerItemInfo.SetExtraIntProperty(Prop: TRVExtraItemProperty;
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
{=============================== TRVMarkerList ================================}
function TRVMarkerList.FindParentMarker(Index: Integer;
  Marker: TRVMarkerItemInfo; var ParentList: TRVMarkerList;
  var ParentIndex: Integer): Boolean;
var Marker2: TRVMarkerItemInfo;
    i: Integer;
begin
  if Marker=nil then
    Marker := TRVMarkerItemInfo(Items[Index]);
  for i := Index-1 downto 0 do begin
    Marker2 := TRVMarkerItemInfo(Items[i]);
    if (Marker2.ListNo = Marker.ListNo) and
       (Marker2.Level<Marker.Level) then begin
      ParentIndex := i;
      ParentList  := Self;
      Result := True;
      exit;
    end;
  end;
  if PrevMarkerList<>nil then
    Result := PrevMarkerList.FindParentMarker(PrevMarkerList.Count, Marker,
      ParentList, ParentIndex)
  else begin
    Result := False;
    ParentIndex := -1;
    ParentList  := nil;
  end;
end;
{------------------------------------------------------------------------------}
function TRVMarkerList.InsertAfter(InsertMe, AfterMe: TRVMarkerItemInfo): Integer;
begin
  if AfterMe = nil then
    Result := 0
  else
    Result := AfterMe.GetIndexInList(Self)+1;
  Insert(Result, InsertMe);
  InsertMe.FCachedIndexInList := Result;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerList.RecalcCounters(StartFrom: Integer; RVStyle: TRVStyle);
var i,j, ListNo: Integer;
    LevelReset: Boolean;
    Marker, Markerj: TRVMarkerItemInfo;
    LevelInfo: TRVListLevel;
begin
  if StartFrom>=Count then
    exit;
  ListNo := TRVMarkerItemInfo(Items[StartFrom]).ListNo;
  if ListNo<0 then
    exit;
  for i := StartFrom to Count-1 do begin
    Marker := TRVMarkerItemInfo(Items[i]);
    if Marker.ListNo=ListNo then begin
      if Marker.Reset then
        Marker.Counter := Marker.StartFrom
      else begin
        LevelInfo := TRVMarkerItemInfo(Items[i]).GetLevelInfo(RVStyle);
        Marker.Counter := LevelInfo.StartFrom;
        LevelReset := rvloLevelReset in LevelInfo.Options;
        for j := i-1 downto 0 do begin
          Markerj := TRVMarkerItemInfo(Items[j]);
          if Markerj.ListNo=ListNo then
            if Markerj.Level=Marker.Level then begin
              Marker.Counter := Markerj.Counter+1;
              break;
              end
            else if LevelReset and (Markerj.Level<Marker.Level) then begin
              Marker.Counter := LevelInfo.StartFrom;
              break;
              end
            else if (Markerj.Level>Marker.Level) then
              Marker.Counter := LevelInfo.StartFrom+1;
        end;
      end;
      Marker.CalcDisplayString(RVStyle, Self, i);
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerList.RecalcDisplayStrings(RVStyle: TRVStyle);
var i: Integer;
begin
  for i := 0 to Count-1 do
    TRVMarkerItemInfo(Items[i]).CalcDisplayString(RVStyle, Self, i);
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerList.LoadFromStream(Stream: TStream; RVData: TPersistent;
 IncludeSize: Boolean);
var i,c: Integer;
    ListNo, Level: Integer;
    Counter: Integer;
    Reset: Boolean;
    StartFrom: Integer;
    Marker: TRVMarkerItemInfo;
begin
  Clear;
  if IncludeSize then begin
    Stream.ReadBuffer(c, sizeof(c));
  end;
  Stream.ReadBuffer(c, sizeof(c));
  Stream.ReadBuffer(c, sizeof(c));
  Capacity := c;
  for i := 0 to c-1 do begin
    Stream.ReadBuffer(ListNo, sizeof(ListNo));
    Stream.ReadBuffer(Level, sizeof(Level));
    Stream.ReadBuffer(Reset, sizeof(Reset));
    Stream.ReadBuffer(StartFrom, sizeof(StartFrom));
    Stream.ReadBuffer(Counter, sizeof(Counter));
    Marker := TRVMarkerItemInfo.CreateEx(RVData, ListNo, Level, StartFrom, Reset);
    Marker.Counter := Counter;
    Add(Marker);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerList.SaveToStream(Stream: TStream; Count: Integer;
 IncludeSize: Boolean);
var i: Integer;
begin
  if IncludeSize then begin
    i := sizeof(Integer)*(1+1+Count*4)+sizeof(Boolean)*Count;
    Stream.WriteBuffer(i, sizeof(i));
  end;
  i := 0;
  Stream.WriteBuffer(i, sizeof(i));
  Stream.WriteBuffer(Count, sizeof(Count));
  for i := 0 to Count-1 do
    with TRVMarkerItemInfo(Items[i]) do begin
      Stream.WriteBuffer(ListNo, sizeof(ListNo));
      Stream.WriteBuffer(Level, sizeof(Level));
      Stream.WriteBuffer(Reset, sizeof(Reset));
      Stream.WriteBuffer(StartFrom, sizeof(StartFrom));
      Stream.WriteBuffer(Counter, sizeof(Counter));
    end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerList.LoadBinary(const s: String; RVData: TPersistent);
var TmpStream: TMemoryStream;
begin
   TmpStream := TMemoryStream.Create;
   try
     TmpStream.WriteBuffer(PChar(s)^, Length(s));
     TmpStream.Position := 0;
     LoadFromStream(TmpStream, RVData, False);
   finally
     TmpStream.Free;
   end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerList.LoadText(const s: String; RVData: TPersistent);
var TmpStream: TMemoryStream;
begin
   TmpStream := TMemoryStream.Create;
   try
     RVFTextString2Stream(s, TmpStream);
     TmpStream.Position := 0;
     LoadFromStream(TmpStream, RVData, False);
   finally
     TmpStream.Free;
   end;
end;
{------------------------------------------------------------------------------}
procedure TRVMarkerList.SaveTextToStream(Stream: TStream; Count: Integer);
var TmpStream: TMemoryStream;
    s: String;
begin
   TmpStream := TMemoryStream.Create;
   try
     SaveToStream(TmpStream, Count, False);
     TmpStream.Position := 0;
     s := RVFStream2TextString(TmpStream);
     RVFWriteLine(Stream, s);
   finally
     TmpStream.Free;
   end;
end;
{$ENDIF}


end.
