
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRVRTFReaderProperties:                         }
{       - property of TRichView                         }
{         (RichView.RTFReadProperties)                  }
{       - intermediate class between RichView and       }
{         RTF parser (TRVRTFReader)                     }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVRTFProps;

interface
{$I RV_Defs.inc}
uses SysUtils, Windows, Classes, Forms, Math,
     {$IFNDEF RVDONOTUSERTFIMPORT}
     RVRTF,
     {$ENDIF}
     {$IFNDEF RVDONOTUSELISTS}
     RVMarker,
     {$ENDIF}
     {$IFNDEF RVDONOTUSEOLECONTAINER}
     (*
     OleCtnrs, ActiveX, ComObj,
     *)
     {$ENDIF}
     RVScroll, CRVData, CRVFData, RVStyle, Graphics, RVRTFErr, RVUni, RVItem,
     RVMapWht, RVFuncs,
     RVClasses;

{$IFNDEF RICHVIEWCBDEF3}
{$DEFINE RVDONOTUSEUNICODE}
{$ENDIF}
{$IFDEF RVDONOTUSERVF}
{$DEFINE RVDONOTUSETABLES}
{$ENDIF}

const
  RV_TableGridEps: Integer = 1;

type

  TRVAllowUseStyleEvent = procedure (StyleNo: Integer; TextStyle: Boolean;
                                     var Allow: Boolean) of object;
  {$IFNDEF RVDONOTUSERTFIMPORT}
  TRVCustomImageItemEvent = procedure (RVData: TCustomRVData;
    Graphic: TGraphic; Hypertext: Boolean; var item: TCustomRVItemInfo;
    var FreeGraphic: Boolean; RTFPicture: TRVRTFPicture;
    var  Name: String) of object;
  {$ENDIF}


  TRVRTFReaderProperties = class (TPersistent)
    private
      FUnicodeMode: TRVReaderUnicode;
      FTextStyleMode: TRVReaderStyleMode;
      FParaStyleMode: TRVReaderStyleMode;
      FIgnorePictures: Boolean;
      FUseHypertextStyles: Boolean;
      FParaNo: Integer;
      FStyleNo: Integer;
      FAdjustVisibleBorders: Boolean;
      FExplicitTableWidth: Boolean;
      FSkipHiddenText: Boolean;
      FHideTableGridLines: Boolean;
      FLineBreaksAsParagraphs: Boolean;
      {$IFNDEF RVDONOTUSERTFIMPORT}
      FEmptyPara: Integer;
      RVData, CurrentRVData: TCustomRVData;
      RVStyle: TRVStyle;
      PageBreak: Boolean;
      PageBreakRVData: TCustomRVData;
      PixelsPerTwip: Double;
      Reader: TRVRTFReader;
      FirstTime: Boolean;
      InsertPoint, CurrentRow, CurrentCol: Integer;
      Tables: TRVList;
      FHeaderRVData, FFooterRVData: TCustomRVData;
      FHeaderYMM, FFooterYMM: Integer;
      FConvertHighlight: TRVRTFHighlight;
      HFType: TRVRTFHeaderFooterType;
      FBasePath: String;
      FExtractMetafileBitmaps: Boolean;
      {$IFNDEF RVDONOTUSELISTS}
      IgnoreLists: Boolean;
      LastMarkerIndex: Integer;
      IsLastMarkerWord97: Boolean;
      LevelToListNo: TRVIntegerList;
      ListTableMap97: TRVIntegerList;
      ListStylesCountBefore: Integer;
      {$ENDIF}
      procedure InitReader;
      procedure DoneReader;
      function ReturnParaNo(Position: TRVRTFPosition): Integer;
      function CreateTextItem(const Text: String;
        {$IFDEF RICHVIEWCBDEF3}const WideText: WideString;{$ENDIF}
        StyleNo, ParaNo: Integer; UseUnicode: Boolean;
        var ResText: String): TRVTextItemInfo;
      {$IFNDEF RVDONOTUSELISTS}
      function GetMarkerIndex(RTFMarker: TRVRTFMarkerProperties): Integer;
      function InsertMarker(ParaNo: Integer): Boolean;
      procedure InsertMarker97(ParaNo: Integer);
      procedure ReaderUpdateMarker(Sender: TObject);
      procedure MergeListTable97;
      function AreListStylesEqual97(RVList: TRVListInfo;
        RTFList: TRVRTFList97): Boolean;
      function FindListStyle97(RTFList: TRVRTFList97;
        ForbiddenStyles: TRVIntegerList): Integer;
      {$ENDIF}
      procedure InsertItem(var Text: String; item: TCustomRVItemInfo;
        Position: TRVRTFPosition);
      procedure CurrentBorder(var RVBorderStyle: TRVBorderStyle;
        var RVBorderWidth, RVBorderIntWidth: Integer; var RVBorderColor: TColor;
        var RVBorderOffs: TRVRect);
      function FindParaNo(RVBorderStyle: TRVBorderStyle;
        RVBorderWidth, RVBorderIntWidth: Integer; RVBorderColor: TColor;
        RVBorderOffs: TRVRect): Integer;
      {$IFNDEF RVDONOTUSETABLES}
      function GetEmptyParaNo(Alignment: TRVAlignment): Integer;
      function GetEmptyStyleNo: Integer;
      {$ENDIF}
      function FindBestParaNo: Integer;
      function FindStyleNo(AUnicode, AHypertext: Boolean): Integer;
      function FindBestStyleNo(AUnicode, AHypertext: Boolean): Integer;
      procedure AddPara(RVBorderStyle: TRVBorderStyle;
        RVBorderWidth, RVBorderIntWidth: Integer; RVBorderColor: TColor;
        RVBorderOffs: TRVRect);
      procedure AddStyle(AUnicode, AHypertext: Boolean);
      function ReturnParaNo_: Integer;
      function ReturnStyleNo(AUnicode: Boolean): Integer;
      function IsHypertext_: Boolean;
      function IsHypertext(var Target, Hint, Extras: String): Boolean;
      function AllowUseStyle(StyleNo: Integer; TextStyle: Boolean): Boolean;
      {$ENDIF}
      procedure SetParaNo(const Value: Integer);
      procedure SetStyleNo(const Value: Integer);
    protected
      {$IFNDEF RVDONOTUSERTFIMPORT}
      procedure ReaderProgress(Sender: TRVRTFReader; Stage: TRVRTFProgressStage;
        PercentDone: Byte);
      procedure NewReaderText(Sender: TRVRTFReader; const Text: String;
        Position: TRVRTFPosition);
      procedure ReaderEndParsing(Sender: TObject);
      {$IFDEF RICHVIEWCBDEF3}
      procedure NewReaderUnicodeText(Sender: TRVRTFReader;
        const Text: WideString; Position: TRVRTFPosition);
      {$ENDIF}
      procedure NewReaderPicture(Sender: TRVRTFReader; RTFPicture: TRVRTFPicture;
        Graphic: TGraphic; Position: TRVRTFPosition; var Inserted: Boolean);
      procedure ReaderImportPicture(Sender: TRVRTFReader; const Location: String;
        var Graphic: TGraphic);
      {$IFNDEF RVDONOTUSEOLECONTAINER}
      (*
      procedure NewReaderObject(Sender: TRVRTFReader; RTFObject: TRVRTFObject;
        Position: TRVRTFPosition; var Inserted: Boolean);
      *)
      {$ENDIF}
      {$IFNDEF RVDONOTUSETABLES}
      procedure AssignRowProperties;
      procedure ReaderTable(Sender: TRVRTFReader; WhatHappens: TRVRTFTableEventKind);
      {$ENDIF}
      procedure ReaderPageBreak(Sender: TObject);
      procedure ReaderHeaderFooter(Sender: TRVRTFReader; HFType: TRVRTFHeaderFooterType;
        Starting: Boolean; var Supported: Boolean);
      {$ENDIF}
    public
      EditFlag: Boolean;
      ErrorCode: TRVRTFErrorCode;
      FailedBecauseOfProtect, FullReformat: Boolean;
      NonFirstItemsAdded: Integer;
      AllowNewPara: Boolean;
      OnAllowUseStyle:   TRVAllowUseStyleEvent;
      {$IFNDEF RVDONOTUSERTFIMPORT}
      OnCustomImageItem: TRVCustomImageItemEvent;
      {$ENDIF}
      Index: Integer;
      constructor Create;
      procedure Assign(Source: TPersistent); override;
      function ReadFromFile(const AFileName: String; ARVData: TCustomRVData): TRVRTFErrorCode;
      function ReadFromStream(AStream: TStream; ARVData: TCustomRVData): TRVRTFErrorCode;
      function InsertFromStreamEd(AStream: TStream; ARVData: TCustomRVData; var AIndex: Integer): TRVRTFErrorCode;
      property AdjustVisibleBorders: Boolean read FAdjustVisibleBorders write FAdjustVisibleBorders;
      property ExplicitTableWidth: Boolean read FExplicitTableWidth write FExplicitTableWidth default False;
      {$IFNDEF RVDONOTUSERTFIMPORT}
      procedure SetHeader(RVData: TCustomRVData);
      procedure SetFooter(RVData: TCustomRVData);
      property HeaderYMM: Integer read FHeaderYMM;
      property FooterYMM: Integer read FFooterYMM;
      property BasePath: String read FBasePath write FBasePath;
      {$ENDIF}
    published
      property UnicodeMode: TRVReaderUnicode read FUnicodeMode write FUnicodeMode default rvruNoUnicode;
      property TextStyleMode: TRVReaderStyleMode read FTextStyleMode write FTextStyleMode default rvrsUseClosest;
      property ParaStyleMode: TRVReaderStyleMode read FParaStyleMode write FParaStyleMode default rvrsUseClosest;
      property IgnorePictures: Boolean read FIgnorePictures write FIgnorePictures default False;
      property UseHypertextStyles: Boolean read FUseHypertextStyles write FUseHypertextStyles default False;
      property TextStyleNo: Integer read FStyleNo write SetStyleNo default 0;
      property ParaStyleNo: Integer read FParaNo write SetParaNo default 0;
      property SkipHiddenText: Boolean read FSkipHiddenText write FSkipHiddenText default True;
      property AutoHideTableGridLines: Boolean read FHideTableGridLines write FHideTableGridLines default False;
      property LineBreaksAsParagraphs: Boolean read FLineBreaksAsParagraphs write FLineBreaksAsParagraphs default False;
      property ExtractMetafileBitmaps: Boolean read FExtractMetafileBitmaps write FExtractMetafileBitmaps default True;
      {$IFNDEF RVDONOTUSERTFIMPORT}
      property ConvertHighlight: TRVRTFHighlight read FConvertHighlight write FConvertHighlight default rtfhlColorTable;
      {$ENDIF}
  end;

implementation
uses RVStr
  {$IFNDEF RVDONOTUSETABLES}
  , RVTable
  {$ENDIF}
  ;

{$IFNDEF RVDONOTUSERTFIMPORT}

{$IFNDEF RVDONOTUSETABLES}
{=============================== TTableInfo ===================================}
type
  TTableInfo = class
    public
      HRules: TRVIntegerList;
      ParentRow, ParentCol, PrevColCount: Integer;
      table, lastrow: TRVTableItemInfo;
      VSpacingTw, VBorderSpacing1Tw,VBorderSpacing2Tw: Integer;
      HSpacingTw, CellPaddingTw, CellPaddingCount, BestWidth: Integer;
      BorderWidthTw, CellBorderWidthTw, CellCount, BorderCount, CellBorderCount: Integer;
      UseHSpacing, UseVSpacing, Word2000, CellFlatBorder, RichViewSpecial: Boolean;
      RowFinished: Boolean;
      constructor Create;
      destructor Destroy; override;
      procedure Finalize(PixelsPerTwip: Double; AdjustVisibleBorders: Boolean; DefStyleNo, DefParaNo: Integer;
                         ExplicitTableWidth, AutoHideGridLines: Boolean);
  end;
{------------------------------------------------------------------------------}
constructor TTableInfo.Create;
begin
  inherited Create;
  HRules := TRVIntegerList.Create;
  CellFlatBorder := True;
end;
{------------------------------------------------------------------------------}
destructor TTableInfo.Destroy;
begin
  lastrow.Free;
  HRules.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TTableInfo.Finalize(PixelsPerTwip: Double; AdjustVisibleBorders: Boolean; DefStyleNo, DefParaNo: Integer;
                              ExplicitTableWidth, AutoHideGridLines: Boolean);
var r,c,r2,c2: Integer;
    Cell1, Cell2: TRVTableCellData;
begin
  if CellFlatBorder then
    table.CellBorderStyle := rvtbColor;
  if table.BorderColor=table.BorderLightColor then
    table.BorderStyle := rvtbColor;
  if CellPaddingCount>0 then
    table.CellPadding := Round(PixelsPerTwip*CellPaddingTw / CellPaddingCount)
  else
    table.CellPadding := 0;
  for r := 0 to table.Rows.Count-1 do
    for c := 0 to table.Rows[r].Count-1 do begin
      Cell1 := table.Cells[r,c];
      if (Cell1<>nil) then begin
        if not RichViewSpecial and (Cell1.BestWidth>table.CellPadding*2) then
          Cell1.BestWidth := Cell1.BestWidth-table.CellPadding*2;
        if (Cell1.Items.Count=0) then
         Cell1.AddNL('', DefStyleNo, DefParaNo);
      end;
    end;

  if (not UseVSpacing or not UseHSpacing) and AdjustVisibleBorders then
    for r := 0 to table.Rows.Count-1 do
      for c := 0 to table.Rows[r].Count-1 do begin
        Cell1 := table.Cells[r,c];
        if (Cell1<>nil) and Cell1.VisibleBorders.Left and not UseHSpacing and (c>0) then begin
          Cell2 := table.Rows.GetMainCell(r,c-1,r2,c2);
          if Cell2.VisibleBorders.Right then
            Cell1.VisibleBorders.Left := False;
        end;
        if (Cell1<>nil) and Cell1.VisibleBorders.Top and not UseVSpacing and (r>0) then begin
          Cell2 := table.Rows.GetMainCell(r-1,c,r2,c2);
          if Cell2.VisibleBorders.Bottom then
            Cell1.VisibleBorders.Top := False;
        end;
      end;
  if UseVSpacing then begin
    if table.Rows.Count>1 then
      table.CellVSpacing := Round(PixelsPerTwip*VSpacingTw / (table.Rows.Count-1));
      table.BorderVSpacing := Round(PixelsPerTwip*(VBorderSpacing1Tw+VBorderSpacing2Tw));
    end
  else begin
    table.CellVSpacing := -1;
    table.BorderVSpacing := -1;
  end;
  if UseHSpacing then
    table.CellHSpacing := Round(PixelsPerTwip*HSpacingTw / table.Rows.Count)
  else begin
    table.CellHSpacing := -1;
  end;
  table.BorderHSpacing := table.CellHSpacing;
  if RichViewSpecial then
    table.BestWidth := BestWidth
  else
    if BestWidth>0 then begin
      if BorderCount>0 then
        table.BestWidth := Round(PixelsPerTwip*(BestWidth-BorderWidthTw/BorderCount))
      else
        table.BestWidth := Round(PixelsPerTwip*BestWidth);
      end
    else if BestWidth<0 then
      table.BestWidth := BestWidth div 50
    else if ExplicitTableWidth then
      table.BestWidth := Round((HRules[HRules.Count-1]-HRules[0])*PixelsPerTwip);
  if CellBorderCount>0 then begin
    table.CellBorderWidth :=  Round(PixelsPerTwip*CellBorderWidthTw/CellBorderCount);
    if (table.CellBorderWidth=0) and (Round(CellBorderWidthTw/CellBorderCount)>=5) then
      table.CellBorderWidth := 1;
  end;
  if BorderCount>0 then begin
    table.BorderWidth :=  Round(PixelsPerTwip*BorderWidthTw/BorderCount);
    if (table.BorderWidth=0) and (Round(BorderWidthTw/BorderCount)>=5) then
      table.BorderWidth := 1;
  end;
  if not UseVSpacing and not UseHSpacing and (table.BorderWidth=table.CellBorderWidth) then begin
    table.BorderWidth := 0;
    table.BorderHSpacing := 0;
    table.BorderVSpacing := 0;
  end;
  if AutoHideGridLines then
    table.Options := table.Options + [rvtoHideGridLines];
end;
{$ENDIF}
{==============================================================================}
procedure BorderRTF2RV(StyleRTF:TRVRTFBorderType; WidthRTF: Integer;
  var StyleRV: TRVBorderStyle; var WidthRV, IntWidthRV: Integer;
  PixelPerTwips: Double; InvertSides: Boolean);
begin
  IntWidthRV := 0;
  case StyleRTF of
    rtf_brdr_None:
      begin
        StyleRV := rvbNone;
        WidthRV := 0;
      end;
    rtf_brdr_SingleThickness,
    rtf_brdr_Inset, rtf_brdr_Outset, rtf_brdr_Shadow,
    rtf_brdr_Dot, rtf_brdr_Dash, rtf_brdr_DashSmall,
    rtf_brdr_DotDash, rtf_brdr_DotDotDash,
    rtf_brdr_Wavy, rtf_brdr_Striped,
    rtf_brdr_Emboss, rtf_brdr_Engrave:
      begin
       // WidthRTF = width of line
        StyleRV := rvbSingle;
        WidthRV    := Round(WidthRTF*PixelPerTwips);
      end;
    rtf_brdr_DoubleThickness:
      begin
        // WidthRTF = width of line / 2
        StyleRV := rvbSingle;
        WidthRV    := Round(WidthRTF*PixelPerTwips*2);
      end;
    rtf_brdr_Double, rtf_brdr_DoubleWavy:
      begin
        // WidthRTF = width of line = width of space
        StyleRV := rvbDouble;
        WidthRV    := Round(WidthRTF*PixelPerTwips);
        IntWidthRV := WidthRV*2-1;
      end;
    rtf_brdr_Triple:
      begin
        // WidthRTF = width of line = width of space
        StyleRV := rvbTriple;
        WidthRV    := Round(WidthRTF*PixelPerTwips);
        IntWidthRV := WidthRV*2-1;
      end;
    rtf_brdr_Hairline:
      begin
        StyleRV := rvbSingle;
        WidthRV := 1;
      end;
    rtf_brdr_ThickThinSmall:
      begin
        // original: one-pixel thin line, one-pixel space, WidthRTF thick line
        if InvertSides then
          StyleRV := rvbThickInside
        else
          StyleRV    := rvbThickOutside;
        WidthRV    := Round(WidthRTF*PixelPerTwips/2);
        IntWidthRV := WidthRV+(WidthRV+1) div 2;
      end;
    rtf_brdr_ThickThinMed:
      begin
        // original: WidthRTF thick line, thin line - half (?), space = thin line,
        if InvertSides then
          StyleRV := rvbThickInside
        else
          StyleRV    := rvbThickOutside;
        WidthRV    := Round(WidthRTF*PixelPerTwips/2);
        IntWidthRV := WidthRV*2+(WidthRV+1) div 2;
      end;
    rtf_brdr_ThickThinLarge:
      begin
        // original: one-pixel thin line, two-pixel thick line (?), WidthRTF space
        if InvertSides then
          StyleRV := rvbThickInside
        else
          StyleRV    := rvbThickOutside;
        WidthRV    := 1;
        IntWidthRV := Round(WidthRTF*PixelPerTwips)+1;
      end;
    rtf_brdr_ThinThickSmall:
      begin
        if not InvertSides then
          StyleRV := rvbThickInside
        else
          StyleRV    := rvbThickOutside;
        IntWidthRV := 1+Round(WidthRTF*PixelPerTwips*3/8);
        IntWidthRV := WidthRV+(WidthRV+1) div 2;
      end;
    rtf_brdr_ThinThickMed:
      begin
        if not InvertSides then
          StyleRV := rvbThickInside
        else
          StyleRV    := rvbThickOutside;
        WidthRV    := Round(WidthRTF*PixelPerTwips/2);
        IntWidthRV := WidthRV*2+(WidthRV+1) div 2;
      end;
    rtf_brdr_ThinThickLarge:
      begin
        if not InvertSides then
          StyleRV := rvbThickInside
        else
          StyleRV    := rvbThickOutside;
        WidthRV    := 1;
        IntWidthRV := Round(WidthRTF*PixelPerTwips)+1;
      end;
    rtf_brdr_ThinThickThinSmall:
      begin
        StyleRV    := rvbTriple;
        WidthRV    := Round(WidthRTF*PixelPerTwips/3);
        IntWidthRV := WidthRV*2-1;;
      end;
    rtf_brdr_ThinThickThinMed:
      begin
        StyleRV    := rvbTriple;
        WidthRV    := Round(WidthRTF*PixelPerTwips/2);
        IntWidthRV := WidthRV*3-1;
      end;
    rtf_brdr_ThinThickThinLarge:
      begin
        StyleRV    := rvbTriple;
        WidthRV    := 1;
        IntWidthRV := Round(WidthRTF*PixelPerTwips);
      end;
  end;
end;
{------------------------------------------------------------------------------}
function GetStyleSSType(VShift: Integer): TRVRTFSScriptType;
begin
  if VShift=0 then
    Result := rtf_ss_Normal
  else if VShift<0 then
    Result := rtf_ss_Subscript
  else
    Result := rtf_ss_Superscript;
end;
{$ENDIF}
{=========================== TRVRTFReaderProperties ===========================}
constructor TRVRTFReaderProperties.Create;
begin
  inherited Create;
  FUnicodeMode   := rvruNoUnicode;
  FTextStyleMode := rvrsUseClosest;
  FParaStyleMode := rvrsUseClosest;
  FIgnorePictures  := False;
  FSkipHiddenText  := True;
  AdjustVisibleBorders := True;
  FHideTableGridLines := False;
  AllowNewPara   := True;
  {$IFNDEF RVDONOTUSERTFIMPORT}
  FConvertHighlight := rtfhlColorTable;
  {$ENDIF}
  FExtractMetafileBitmaps := True;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.Assign(Source: TPersistent);
begin
  if Source is TRVRTFReaderProperties then begin
    FUnicodeMode   := TRVRTFReaderProperties(Source).FUnicodeMode;
    FTextStyleMode := TRVRTFReaderProperties(Source).FTextStyleMode;
    FParaStyleMode := TRVRTFReaderProperties(Source).FParaStyleMode;
    FIgnorePictures  := TRVRTFReaderProperties(Source).FIgnorePictures;
    FParaNo        := TRVRTFReaderProperties(Source).FParaNo;
    FStyleNo        := TRVRTFReaderProperties(Source).FStyleNo;
    FUseHypertextStyles := TRVRTFReaderProperties(Source).FUseHypertextStyles;
    end
  else
    inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.ReadFromFile(const AFileName: String;
  ARVData: TCustomRVData): TRVRTFErrorCode;
begin
{$IFNDEF RVDONOTUSERTFIMPORT}
  try
    RVData := ARVData;
    EditFlag := False;
    InsertPoint := RVData.Items.Count;
    Index := -1;
    Reader := TRVRTFReader.Create(nil);
    try
      InitReader;
      Result := Reader.ReadFromFile(AFileName);
      ErrorCode := Result;
    finally
      DoneReader;
      Reader.Free;
    end;
  except
    Result := rtf_ec_Exception;
  end;
{$ELSE}
  Result := rtf_ec_OK;
{$ENDIF}
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.ReadFromStream(AStream: TStream;
  ARVData: TCustomRVData): TRVRTFErrorCode;
begin
{$IFNDEF RVDONOTUSERTFIMPORT}
  try
    RVData := ARVData;
    Reader := TRVRTFReader.Create(nil);
    EditFlag := False;
    InsertPoint := RVData.Items.Count;
    Index := -1;
    try
      InitReader;
      Result := Reader.ReadFromStream(AStream);
      ErrorCode := Result;
    finally
      DoneReader;
      Reader.Free;
    end;
  except
    Result := rtf_ec_Exception;
  end;
{$ELSE}
  Result := rtf_ec_OK;
{$ENDIF}
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.InsertFromStreamEd(AStream: TStream;
  ARVData: TCustomRVData; var AIndex: Integer): TRVRTFErrorCode;
begin
{$IFNDEF RVDONOTUSERTFIMPORT}
  RVData := ARVData;
  Reader := TRVRTFReader.Create(nil);
  EditFlag := True;
  InsertPoint := AIndex;
  Index := -1;
  try
    InitReader;
    Result := Reader.ReadFromStream(AStream);
    ErrorCode := Result;
  finally
    Reader.Free;
    DoneReader;
    AIndex := Index;
  end;
{$ELSE}
  Result := rtf_ec_OK;
{$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.SetParaNo(const Value: Integer);
begin
  if Value<0 then
    raise Exception.Create(errNegative);
  FParaNo := Value;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.SetStyleNo(const Value: Integer);
begin
  if Value<0 then
    raise Exception.Create(errNegative);
  FStyleNo := Value;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERTFIMPORT}
procedure TRVRTFReaderProperties.SetFooter(RVData: TCustomRVData);
begin
  FFooterRVData := RVData;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.SetHeader(RVData: TCustomRVData);
begin
  FHeaderRVData := RVData;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.NewReaderPicture(Sender: TRVRTFReader;
  RTFPicture: TRVRTFPicture; Graphic: TGraphic; Position: TRVRTFPosition;
  var Inserted: Boolean);
var item: TCustomRVItemInfo;
    Target, Hint, Extras: String;
    StyleNo, ItemTag,v: Integer;
    ItemName: String;
    Hyp, FreeGraphic: Boolean;
begin
  Inserted := False;
  ItemName := '';
  Hint := '';
  if Assigned(OnCustomImageItem) then begin
    Hyp := IsHypertext(Target, Hint, Extras);
    item := nil;
    FreeGraphic := False;
    OnCustomImageItem(CurrentRVData, Graphic, Hyp, item, FreeGraphic, RTFPicture,
      ItemName);
    if FreeGraphic then
      Graphic.Free;
    if item<>nil then
      RVData.ReadHyperlink(Target, Extras, rvlfRTF, item.StyleNo, item.Tag, ItemName);
    end
  else begin
    if Graphic=nil then begin
      if RTFPicture.ShpPict then
        exit;
      Graphic := RV_CreateGraphics(TGraphicClass(RVStyle.InvalidPicture.Graphic.ClassType));
      Graphic.Assign(RVStyle.InvalidPicture.Graphic);
    end;
    StyleNo  := rvsPicture;
    ItemTag  := 0;
    if IsHypertext(Target, Hint, Extras) then begin
      RVData.ReadHyperlink(Target, Extras, rvlfRTF, StyleNo, ItemTag, ItemName);
      item := CreateRichViewItem(StyleNo,RVData) as TRVGraphicItemInfo;
      TRVGraphicItemInfo(item).Image := Graphic;
      TRVGraphicItemInfo(item).Tag := ItemTag;
      end
    else
      item := TRVGraphicItemInfo.CreateEx(CurrentRVData, Graphic, rvvaBaseline);
  end;
  if item<>nil then begin
    if (item is TRVGraphicItemInfo) and (RTFPicture<>nil) then begin
      if (TRVGraphicItemInfo(item).Image is TMetafile) or (RTFPicture.PicScaleX<>100) then begin
        if RTFPicture.SuggestedWidth>0 then
          v := RTFPicture.SuggestedWidth
        else
          v := TRVGraphicItemInfo(item).Image.Width;
        TRVGraphicItemInfo(item).ImageWidth := Round(v/100*RTFPicture.PicScaleX);
      end;
      if (TRVGraphicItemInfo(item).Image is TMetafile) or (RTFPicture.PicScaleY<>100) then begin
        if RTFPicture.SuggestedHeight>0 then
          v := RTFPicture.SuggestedHeight
        else
          v := TRVGraphicItemInfo(item).Image.Height;
        TRVGraphicItemInfo(item).ImageHeight := Round(v/100*RTFPicture.PicScaleY);
      end;
    end;
    item.ParaNo := ReturnParaNo(Position);
    {$IFNDEF RVDONOTUSEITEMHINTS}
    item.Hint := Hint;
    {$ENDIF}
    item.BeforeLoading(rvlfRTF);
    InsertItem(ItemName, item, Position);
    Inserted := True;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.ReaderImportPicture(Sender: TRVRTFReader;
  const Location: String; var Graphic: TGraphic);
begin
  Graphic := CurrentRVData.ImportPicture(Location, 0, 0);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEOLECONTAINER}
(*
type
  TStreamHeader = record
    case Integer of
      0: ( { New }
        Signature: Integer;
        DrawAspect: Integer;
        DataSize: Integer);
      1: ( { Old }
        PartRect: TSmallRect);
  end;

  TRTFObjectHeader = record
    Signature: Integer;
    DrawAspect: Integer;
    ProgIdSize: Integer;
  end;

const
  StreamSignature = $434F4442; {'BDOC'}

procedure TRVRTFReaderProperties.NewReaderObject(Sender: TRVRTFReader;
  RTFObject: TRVRTFObject; Position: TRVRTFPosition; var Inserted: Boolean);
var ole: TOLEContainer;
    item: TRVControlItemInfo;
    s: String;
    Size: Integer;
    Stream: TStream;
    header: TStreamHeader;
    rtfheader: TRTFObjectHeader;
    StreamAdapter: TStreamAdapter;
    OleObject: IOleObject;

    TempLockBytes: ILockBytes;
    TempStorage: IStorage;
    DataHandle: HGlobal;
    Buffer: Pointer;

begin
  case RTFObject.ObjType of
    rtf_obj_Emb:
      begin
        ole := TOLEContainer.Create(nil);
        try
          ole.Visible := False;
          ole.Parent := RVData.GetParentControl;
          ole.Width := Round(RTFObject.WidthTw*PixelsPerTwip);
          ole.Height := Round(RTFObject.HeightTw*PixelsPerTwip);
          RTFObject.Data.ReadBuffer(rtfheader, sizeof(rtfheader));
          RTFObject.Data.Seek(rtfheader.ProgIdSize+12, soFromCurrent);
          header.Signature := StreamSignature;
          header.DrawAspect := 1;//rtfheader.DrawAspect;
          Header.DataSize := RTFObject.Data.Size-RTFObject.Data.Position;
          Stream := TMemoryStream.Create;
          Stream.WriteBuffer(header, sizeof(header));
          Stream.CopyFrom(RTFObject.Data, Header.DataSize);
          Stream.Position := 0;
          ole.LoadFromStream(Stream);
          Stream.Free;
          item := TRVControlItemInfo.CreateEx(CurrentRVData, ole, rvvaBaseline);
          s := '';
          InsertItem(s, item, Position);
          Inserted := True;
        except
          ole.Free;
        end;
      end;
  end;
end;
*)
{$ENDIF}
{------------------------------------------------------------------------------}
function GetIndex(List: TRVIntegerList; Value: Integer): Integer;
var i: Integer;
begin
  for i := 0 to List.Count-1 do
    if List[i]>=Value then begin
      Result := i;
      exit;
    end;
  Result := List.Count;
end;
{$IFNDEF RVDONOTUSETABLES}
{------------------------------------------------------------------------------}
procedure MergeCol(table: TRVTableItemInfo; c: Integer);
var r: Integer;
begin
  if c>=table.Rows[0].Count-2 then
    exit;
  for r := 0 to table.Rows.Count-2 do
    if table.Cells[r,c]<>nil then
      table.Cells[r,c].Clear;
  for r := 0 to table.Rows.Count-2 do
    if (table.Cells[r,c]<>nil) and (table.Cells[r,c+1]<>nil) then begin
      table.Cells[r,c].AssignAttributesFrom(table.Cells[r,c+1], True,1,1);
      table.Rows.MergeCells(r,c, table.Cells[r,c+1].ColSpan+1,table.Cells[r,c+1].RowSpan,True,False);
    end;
end;
{------------------------------------------------------------------------------}
procedure GetBorderColors(Border: TRVRTFParaBorder; var c1, c2: TColor);
begin
  c1 := clNone;
  c2 := clNone;
  if Border.Sides[rtf_side_Left].BorderType<>rtf_brdr_None then
    c1 := Border.Sides[rtf_side_Left].Color
  else if Border.Sides[rtf_side_Top].BorderType<>rtf_brdr_None then
    c1 := Border.Sides[rtf_side_Top].Color;
  if Border.Sides[rtf_side_Right].BorderType<>rtf_brdr_None then
    c2 := Border.Sides[rtf_side_Right].Color
  else if Border.Sides[rtf_side_Bottom].BorderType<>rtf_brdr_None then
    c2 := Border.Sides[rtf_side_Bottom].Color;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.AssignRowProperties;
var info: TTableInfo;
    i,span,r,c,idx,val,strt: Integer;
    RowProps: TRVRTFRowProperties;
    side: TRVRTFSide;
    c1,c2: TColor;
    {............................................................}
    procedure AssignCellProperties(Cell: TRVTableCellData; Props: TRVRTFCellProperties; RuleIndex1,RuleIndex2: Integer);
    var side: TRVRTFSide;
        w: Integer;
        c1, c2: TColor;
    begin
       inc(info.CellCount);
       Cell.Color := Props.Color;
       if RowProps.RichViewSpecial then
         Cell.BestHeight := Props.BestHeight
       else if RowProps.HeightTw>0 then
         Cell.BestHeight := Round(RowProps.HeightTw*PixelsPerTwip);
       for side := Low(TRVRTFSide) to High(TRVRTFSide) do begin
         if Props.Border.Sides[side].BorderType<>rtf_brdr_None then begin
           inc(info.CellBorderCount);
           inc(info.CellBorderWidthTw, Props.Border.Sides[side].WidthTw);
         end;
       end;
       Cell.VisibleBorders.Left := Props.Border.Sides[rtf_side_Left].BorderType<>rtf_brdr_None;
       Cell.VisibleBorders.Top := Props.Border.Sides[rtf_side_Top].BorderType<>rtf_brdr_None;
       Cell.VisibleBorders.Right := Props.Border.Sides[rtf_side_Right].BorderType<>rtf_brdr_None;
       Cell.VisibleBorders.Bottom := Props.Border.Sides[rtf_side_Bottom].BorderType<>rtf_brdr_None;
       GetBorderColors(Props.Border,c1,c2);
       Cell.BorderColor := c1;
       Cell.BorderLightColor := c2;
       info.CellFlatBorder := info.CellFlatBorder and ((c1=c2) or (c1=clNone) or (c2=clNone));
       if RowProps.RichViewSpecial then
         Cell.BestWidth := Props.BestWidth
       else begin
         info.Word2000 := info.Word2000 or (Props.BestWidth<>0);
         if not info.Word2000 then begin
           w := info.HRules[RuleIndex2]-info.HRules[RuleIndex1];
           Cell.BestWidth := Round(w*PixelsPerTwip)
           end
         else
           if Props.BestWidth>0 then begin
             w := Props.BestWidth;
             Cell.BestWidth := Round(w*PixelsPerTwip)
             end
           else
             Cell.BestWidth := Props.BestWidth div 50;
       end;
       case Props.VAlign of
         rtf_val_Top:
           Cell.VAlign := rvcTop;
         rtf_val_Bottom:
           Cell.VAlign := rvcBottom;
         rtf_val_Center:
           Cell.VAlign := rvcMiddle;
       end;
    end;
    {............................................................}
    procedure HideBorders(Row,Col: Integer);
    var Cell: TRVTableCellData;
    begin
      Cell := info.table.Cells[Row,Col];
      if Cell<>nil then begin
        Cell.VisibleBorders.SetAll(False);
        Cell.Clear;
        Cell.AddNL('',GetEmptyStyleNo,GetEmptyParaNo(rvaLeft));
      end;
    end;
    {............................................................}
begin
  info := TTableInfo(Tables[Tables.Count-1]);
  info.table.InsertRows(info.table.Rows.Count,1,-1);
  if info.table.Rows[0].Count<info.lastrow.Rows[0].Count then begin
    c := info.table.Rows[0].Count;
    span := info.lastrow.Rows[0].Count-info.table.Rows[0].Count+1;
    info.table.InsertCols(c,span,-1);
    if CurrentRow>1 then begin
      info.table.MergeCells(0,c,span,info.table.Rows.Count,True);
      HideBorders(0,c);
    end;
  end;
  RowProps := Reader.RTFState.RowProps;
  if RowProps.Heading then
    info.table.HeadingRowCount := info.table.Rows.Count;

  //assigning table attributes
  if RowProps.UseSpacing[rtf_side_Top] then
    if CurrentRow=0 then
      info.VBorderSpacing1Tw := RowProps.SpacingTw[rtf_side_Top]
    else
      inc(info.VSpacingTw, RowProps.SpacingTw[rtf_side_Top]);
  inc(info.VSpacingTw, info.VBorderSpacing2Tw);
  info.UseHSpacing := info.UseHSpacing or RowProps.UseSpacing[rtf_side_Left] or
                      RowProps.UseSpacing[rtf_side_Right];
  info.UseVSpacing := info.UseVSpacing or RowProps.UseSpacing[rtf_side_Top] or
                      RowProps.UseSpacing[rtf_side_Bottom];
  if RowProps.UseSpacing[rtf_side_Bottom] then
    info.VBorderSpacing2Tw := RowProps.SpacingTw[rtf_side_Bottom];
  if RowProps.UseSpacing[rtf_side_Left] then
    inc(info.HSpacingTw, RowProps.SpacingTw[rtf_side_Left])
  else begin
    inc(info.HSpacingTw, RowProps.GapHTw);
    if RowProps.UsePadding[rtf_side_Left] then
      dec(info.HSpacingTw, RowProps.PaddingTw[rtf_side_Left]);
  end;
  if RowProps.UseSpacing[rtf_side_Right] then
    inc(info.HSpacingTw, RowProps.SpacingTw[rtf_side_Right])
  else begin
    inc(info.HSpacingTw, RowProps.GapHTw);
    if RowProps.UsePadding[rtf_side_Right] then
      dec(info.HSpacingTw, RowProps.PaddingTw[rtf_side_Right]);
  end;
  for side := Low(TRVRTFSide) to High(TRVRTFSide) do begin
    if RowProps.Border.Sides[side].BorderType<>rtf_brdr_None then begin
      inc(info.BorderWidthTw, RowProps.Border.Sides[side].WidthTw);
      inc(info.BorderCount);
    end;
    if RowProps.UsePadding[side] then
      inc(info.CellPaddingTw, RowProps.PaddingTw[side]);
    inc(info.CellPaddingCount);
  end;
  if RowProps.RichViewSpecial then begin
    info.BestWidth := RowProps.BestWidth;
    info.RichViewSpecial := True;
    end
  else
    if ((info.BestWidth<=0)  and (RowProps.BestWidth<info.BestWidth)) or
       ((info.BestWidth>=0) and (RowProps.BestWidth>info.BestWidth)) then
      info.BestWidth := RowProps.BestWidth;
  GetBorderColors(RowProps.Border,c1,c2);
  if c1<>clNone then
    info.table.BorderLightColor := c1;
  if c2<>clNone then
    info.table.BorderColor := c2;
  // assigning cells


  if CurrentRow=0 then begin
    info.HRules.Add(RowProps.LeftTw);
    for i := 0 to RowProps.CellProps.Count-1 do
      info.HRules.Add(RowProps.CellProps[i].RightBoundaryTw);
  end;

  strt := 0;
  if CurrentRow>0 then begin
    val := RowProps.LeftTw;
    idx := GetIndex(info.HRules, val);
    if idx=0 then begin
      if abs(info.HRules[idx]-val)>RV_TableGridEps then begin
        info.HRules.Insert(0, val);
        info.table.InsertCols(0,1,-1);
        info.table.Cells[CurrentRow,0].Clear;
        info.table.Cells[CurrentRow,0].AssignAttributesFrom(info.table.Cells[CurrentRow,1],True,1,1);
        info.table.Rows.MergeCells(0,0,1,CurrentRow,True,False);
        HideBorders(0,0);
        strt := 0;
      end;
     end
    else if idx=info.HRules.Count then begin
      info.HRules.Add(val);
      info.table.InsertCols(idx-1,2,-1);
      info.table.Rows.MergeCells(0,idx,1,CurrentRow,True,False);
      HideBorders(0,idx);
      MergeCol(info.table,idx-1);
      info.table.Rows.MergeCells(CurrentRow,0,idx,1,True,False);
      HideBorders(CurrentRow,0);
      if info.table.Cells[CurrentRow,0]<>nil then
        info.table.Cells[CurrentRow,0].BestWidth := Round((info.HRules[idx]-info.HRules[0]-RowProps.GapHTw*2)*PixelsPerTwip);
      strt := idx;
      end
    else begin
      if abs(info.HRules[idx]-val)>RV_TableGridEps then begin
        info.HRules.Insert(idx, val);
        info.HRules.Sort;
        info.table.InsertCols(idx-1,1,-1);
        HideBorders(CurrentRow,idx-1);
        MergeCol(info.table,idx-1);
      end;
      info.table.Rows.MergeCells(CurrentRow,0,idx,1,True,False);
      if idx>0 then
        HideBorders(CurrentRow,0);
      strt := idx;
    end;

    for i := 0 to RowProps.CellProps.Count-1 do begin
      val := RowProps.CellProps[i].RightBoundaryTw;
      if RowProps.CellProps[i].HMerge=rtf_cm_First then
        if strt<info.HRules.Count-1 then begin
          val := info.HRules[strt+1];
          if val>RowProps.CellProps[i+1].RightBoundaryTw then
            val := RowProps.CellProps[i+1].RightBoundaryTw;
          end
        else
          val := RowProps.CellProps[i+1].RightBoundaryTw;
      idx := GetIndex(info.HRules, val);
      if (idx=info.HRules.Count) then begin
        //if strt<info.HRules.Count-1 then begin
          info.table.InsertCols(idx-1,1,-1);
        //end;
        info.HRules.Add(val);
        info.table.Rows.MergeCells(0,idx-1,1,CurrentRow,True,False);
        HideBorders(0,idx-1);
        end
      else if abs(info.HRules[idx]-val)>RV_TableGridEps then begin
        info.HRules.Insert(idx, val);
        info.HRules.Sort;
        info.table.InsertCols(idx-1,1,-1);
        MergeCol(info.table,idx-1);
      end;
      info.table.Rows.MergeCells(CurrentRow,strt,idx-strt,1,True,False);
      if (strt>0) and (RowProps.CellProps[i].HMerge=rtf_cm_Merged) then begin
        info.table.Rows.GetMainCell(CurrentRow,strt-1,r,c);
        info.table.Rows.MergeCells(r,c,idx-c,CurrentRow-r+1,True,False);
      end;
      if RowProps.CellProps[i].VMerge=rtf_cm_Merged then begin
        info.table.Rows.GetMainCell(CurrentRow-1,strt,r,c);
        span := idx-c;
        if info.table.Cells[r,c].ColSpan>span then
          span := info.table.Cells[r,c].ColSpan;
        info.table.Rows.MergeCells(r,c,span,CurrentRow-r+1,True,False);
      end;
      if info.table.Cells[CurrentRow,strt]<>nil then begin
        AssignCellProperties(info.table.Cells[CurrentRow,strt],RowProps.CellProps[i],strt,idx);
        info.table.Cells[CurrentRow,strt].Clear;
        info.table.Cells[CurrentRow,strt].DrainFrom(info.lastrow.Cells[0,i]);
      end;
      strt := idx;
      //if strt>=info.table.Rows.Count then
      //  dec(strt);
    end;
    for i := strt to info.table.Rows[CurrentRow].Count-1 do
      HideBorders(CurrentRow,i);
    end
  else // CurrentRow=0
    for i := 0 to RowProps.CellProps.Count-1 do begin
      if RowProps.CellProps[i].HMerge=rtf_cm_Merged then begin
        info.table.Rows.GetMainCell(CurrentRow,i-1,r,c);
        info.table.Rows.MergeCells(r,c,i-c+1,1,True,False);
      end;
      if info.table.Cells[CurrentRow,i]<>nil then begin
        AssignCellProperties(info.table.Cells[CurrentRow,i], RowProps.CellProps[i],i,i+1);
        info.table.Cells[CurrentRow,i].Clear;
        info.table.Cells[CurrentRow,i].DrainFrom(info.lastrow.Cells[0,i]);
      end;
    end;
  info.PrevColCount := info.table.Rows[0].Count-1;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.ReaderTable(Sender: TRVRTFReader;
  WhatHappens: TRVRTFTableEventKind);
var info: TTableInfo;
    i: Integer;
    PrevRVData:TCustomRVData;
    s: String;
    APageBreak: Boolean;
    APageBreakRVData: TCustomRVData;
    Alignment: TRVAlignment;
begin
  case WhatHappens of
    rvf_tbl_TableStart:
      begin
        if Tables=nil then
          Tables := TRVList.Create;
        info := TTableInfo.Create;
        if PageBreak and (PageBreakRVData=CurrentRVData) then begin
          info.table.PageBreakBefore := True;
          PageBreak := False;
          PageBreakRVData := nil;
        end;
        info.table := TRVTableItemInfo.CreateEx(0,0,CurrentRVData);
        info.table.BeforeLoading(rvlfRTF);
        info.table.Color := clNone;
        info.table.BorderStyle := rvtbRaisedColor;
        info.table.CellBorderStyle := rvtbLoweredColor;
        info.table.CellBorderColor := clWindowText;
        info.table.BorderColor := clWindowText;
        info.table.BorderLightColor := clWindowText;
        info.table.CellBorderLightColor := clWindowText;
        info.lastrow := TRVTableItemInfo.CreateEx(1,1,CurrentRVData);
        info.lastrow.BeforeLoading(rvlfRTF);
        info.ParentRow := CurrentRow;
        info.ParentCol := CurrentCol;
        CurrentRow := 0;
        CurrentCol := 0;
        CurrentRVData := info.lastrow.Cells[0,0];
        CurrentRVData.Clear;
        Tables.Add(info);
      end;
    rvf_tbl_TableEnd:
      begin
        info := TTableInfo(Tables[Tables.Count-1]);
        if Tables.Count=1 then
          case HFType of
            rtf_hf_Header:
              PrevRVData := FHeaderRVData;
            rtf_hf_Footer:
              PrevRVData := FFooterRVData;
            else
              PrevRVData := RVData;
          end
        else
          PrevRVData := TTableInfo(Tables[Tables.Count-2]).lastrow.Cells[0,info.ParentCol];
        info.Finalize(PixelsPerTwip, AdjustVisibleBorders, GetEmptyStyleNo, GetEmptyParaNo(rvaLeft),
                      ExplicitTableWidth, AutoHideTableGridLines);
        if PageBreak and (PageBreakRVData=CurrentRVData) then
          PageBreakRVData := PrevRVData;
        APageBreak := PageBreak;
        APageBreakRVData := PageBreakRVData;
        PageBreak := False;
        PageBreakRVData := nil;
        CurrentRVData := PrevRVData;
        CurrentRow := info.ParentRow;
        CurrentCol := info.ParentCol;
        info.table.DeleteCols(info.HRules.Count-1,info.table.Rows[0].Count-info.HRules.Count+1,False);
        if Reader.FTableAlignmentDefined then
          Alignment := TRVAlignment(ord(Reader.FTableAlignment))
        else
          Alignment := TRVAlignment(ord(Reader.RTFState.ParaProps.Alignment));        
        info.table.ParaNo := GetEmptyParaNo(Alignment);
        s := '';
        if (info.table.Rows.Count=0) or (info.table.Rows[0].Count=0) then begin
          info.table.Free;
          info.table := nil
          end
        else begin
          info.table.DeleteEmptyRows;
          InsertItem(s, info.table, rtf_ts_NewPara);
        end;
        if not info.RowFinished then begin
          {$IFNDEF RVDONOTUSELISTS}
          IgnoreLists := True;
          {$ENDIF}
          try
            for i := 0 to info.lastrow.Cells[0,0].Items.Count-1 do begin
              s := info.lastrow.Cells[0,0].Items[i];
              InsertItem(s, info.lastrow.Cells[0,0].GetItem(i), rtf_ts_NewPara);
              info.lastrow.Cells[0,0].Items.Objects[i] := nil;
            end;
          finally
            {$IFNDEF RVDONOTUSELISTS}
            IgnoreLists := False;
            LastMarkerIndex := -1;
            {$ENDIF}
          end;
        end;
        Tables.Delete(Tables.Count-1);
        PageBreak := APageBreak;
        PageBreakRVData := APageBreakRVData;
      end;
    rvf_tbl_CellEnd:
      begin
        info := TTableInfo(Tables[Tables.Count-1]);
        if info.lastrow.Cells[0,CurrentCol].Items.Count=0 then begin
           NewReaderText(Sender,'',rtf_ts_NewPara);
           {$IFNDEF RVDONOTUSELISTS}
           ReaderUpdateMarker(Sender);
           {$ENDIF}
        end;
        inc(CurrentCol);
        if CurrentCol=info.lastrow.Rows[0].Count then
          info.lastrow.InsertCols(CurrentCol,1,-1);
        CurrentRVData := info.lastrow.Cells[0,CurrentCol];
        CurrentRVData.Clear;
      end;
    rvf_tbl_RowEnd:
      begin
        AssignRowProperties;
        info := TTableInfo(Tables[Tables.Count-1]);
        info.RowFinished := True;
        inc(CurrentRow);
        CurrentCol := 0;
        info.lastrow.Rows.Reset(1,1);;
        CurrentRVData := info.lastrow.Cells[0,CurrentCol];
        CurrentRVData.Clear;
      end;
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.CreateTextItem(const Text: String;
                                               {$IFDEF RICHVIEWCBDEF3}
                                               const WideText: WideString;
                                               {$ENDIF}
                                               StyleNo, ParaNo: Integer;
                                               UseUnicode: Boolean;
                                               var ResText: String): TRVTextItemInfo;
var
  Target, Hint, Extras: String;
  AStyleNo,ItemTag: Integer;
begin
  Hint := '';
  Result := RichViewTextItemClass.Create(CurrentRVData);
  Result.BeforeLoading(rvlfRTF);
  {$IFNDEF RVDONOTUSEUNICODE}
  if RVStyle.TextStyles[StyleNo].Unicode then
    Include(Result.ItemOptions, rvioUnicode);
  if not UseUnicode then
    if RVStyle.TextStyles[StyleNo].Unicode then
      ResText := RVU_AnsiToUnicode(
        RVU_Charset2CodePage(
          Reader.FontTable[Reader.RTFState.CharProps.FontIndex].Charset), Text)
    else
      ResText := Text
  else
    if RVStyle.TextStyles[StyleNo].Unicode then begin
      SetLength(ResText, Length(WideText)*2);
      Move(Pointer(WideText)^, PChar(ResText)^, Length(ResText));
      end
    else
      ResText := Reader.UnicodeToAnsi(WideText);
  {$ELSE}
  ResText := Text;
  {$ENDIF}
  ResText := CurrentRVData.ReplaceTabs(ResText, StyleNo, False);
  ItemTag := 0;
  if IsHypertext(Target, Hint, Extras) then begin
    AStyleNo := StyleNo;
    CurrentRVData.ReadHyperlink(Target, Extras, rvlfRTF, AStyleNo, ItemTag, ResText);
    if AStyleNo>=0 then
      StyleNo := AStyleNo;
  end;
  Result.StyleNo := StyleNo;
  Result.ParaNo  := ParaNo;
  Result.Tag     := ItemTag;
  {$IFNDEF RVDONOTUSEITEMHINTS}
  Result.Hint    := Hint;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.InsertItem(var Text: String;
  item: TCustomRVItemInfo; Position: TRVRTFPosition);
var Dummy: Integer;
begin
  case Position of
     rtf_ts_ContinuePara:
         item.SameAsPrev := True;
     rtf_ts_NewLine,
     rtf_ts_NewPara:
       begin
         if (Position = rtf_ts_NewLine) and not LineBreaksAsParagraphs then
           item.BR := True
         else begin
           if not AllowNewPara then
             item.BR := True
           else begin
             {$IFNDEF RVDONOTUSELISTS}
             if (item.StyleNo<>rvsListMarker) and not IgnoreLists {$IFNDEF RVDONOTUSETABLES} and (item.StyleNo<>rvsTable){$ENDIF} then
               if InsertMarker(item.ParaNo) then
                 item.SameAsPrev := True;
             {$ENDIF}
           end;
         end;
       end;
  end;
  if PageBreak and (PageBreakRVData=CurrentRVData) then begin
    item.PageBreakBefore := True;
    PageBreak := False;
    PageBreakRVData := nil;
  end;
  if RVData=CurrentRVData then begin
    if FirstTime then begin
      FirstTime := False;
      if not EditFlag and ((InsertPoint=0) or
       (CurrentRVData.GetItem(InsertPoint-1).GetBoolValue(rvbpFullWidth))) then
        item.SameAsPrev := False;
      if not RVData.InsertFirstRVFItem(InsertPoint, Text, Item, EditFlag, FullReformat, Dummy) then begin
        FailedBecauseOfProtect := True;
        abort;
      end;
      if item<>nil then begin
        Item.AfterLoading(rvlfRTF);
        inc(InsertPoint);
        Index := InsertPoint-1;
        end
      else
        FirstTime := True;
      {$IFNDEF RVDONOTUSELISTS}
      if (item<>nil) and (item.StyleNo=rvsListMarker) then
        LastMarkerIndex := Index;
      {$ENDIF}
      end
    else begin
      if (InsertPoint=0) or
       (CurrentRVData.GetItem(InsertPoint-1).GetBoolValue(rvbpFullWidth)) then
        item.SameAsPrev := False;
      Item.Inserting(RVData, Text, False);
      RVData.Items.InsertObject(InsertPoint, Text, Item);
      Item.Inserted(RVData, InsertPoint);
      Item.AfterLoading(rvlfRTF);
      {$IFNDEF RVDONOTUSELISTS}
      RVData.AddMarkerInList(InsertPoint);
      if item.StyleNo=rvsListMarker then
        LastMarkerIndex := InsertPoint;
      {$ENDIF}
      inc(InsertPoint);
      inc(NonFirstItemsAdded);
    end;
    end
  else begin
    if (CurrentRVData.Items.Count=0) or
       (CurrentRVData.GetItem(CurrentRVData.Items.Count-1).GetBoolValue(rvbpFullWidth)) then
      item.SameAsPrev := False;
    Item.Inserting(CurrentRVData, Text, False);
    CurrentRVData.Items.AddObject(Text, Item);
    Item.Inserted(CurrentRVData, CurrentRVData.Items.Count-1);
    if (CurrentRVData=FHeaderRVData) or (CurrentRVData=FFooterRVData) then
      Item.AfterLoading(rvlfRTF);
    {$IFNDEF RVDONOTUSELISTS}
    CurrentRVData.AddMarkerInList(CurrentRVData.Items.Count-1);
    if item.StyleNo=rvsListMarker then
      LastMarkerIndex := CurrentRVData.Items.Count-1;
    {$ENDIF}
    {$IFNDEF RVDONOTUSETABLES}
    if (CurrentRVData<>FHeaderRVData) and (CurrentRVData<>FFooterRVData) then
      TTableInfo(Tables[Tables.Count-1]).RowFinished := False;
    {$ENDIF}
  end;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSELISTS}
procedure ConvMarker(RTFMarker: TRVRTFMarkerProperties;
                     var RVType: TRVListType;
                     var FormatStr1,FormatStr2: String);
begin
  if (RTFMarker.Level=11) or
     ((RTFMarker.Level=10) and (RTFMarker.ListType=rtf_pn_Default)) then begin
    RVType := rvlstBullet;
    FormatStr1 := RTFMarker.TextBefore;
    FormatStr2 := RTFMarker.TextBefore;
    end
  else begin
    case RTFMarker.ListType of
      rtf_pn_Default, rtf_pn_Decimal:
        RVType := rvlstDecimal;
      rtf_pn_LowerLetter:
        RVType := rvlstLowerAlpha;
      rtf_pn_UpperLetter:
        RVType := rvlstUpperAlpha;
      rtf_pn_LowerRoman:
        RVType := rvlstLowerRoman;
      rtf_pn_UpperRoman:
        RVType := rvlstUpperRoman;
    end;
    FormatStr1 := RTFMarker.TextBefore+'%s'+RTFMarker.TextAfter;
    FormatStr2 := RTFMarker.TextBefore+'%0:s'+RTFMarker.TextAfter;
  end;
end;
{------------------------------------------------------------------------------}
function GetMarkerFontName(Reader: TRVRTFReader;
  RTFMarker: TRVRTFCustomMarkerProperties): String;
begin
  if RTFMarker.FontIndex<0 then
    Result := 'Symbol'
  else
    Result := Reader.FontTable[RTFMarker.FontIndex].Name;
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
function GetMarkerCharset(Reader: TRVRTFReader;
  RTFMarker: TRVRTFCustomMarkerProperties): TFontCharset;
begin
  if RTFMarker.FontIndex<0 then
    Result := SYMBOL_CHARSET
  else
    Result := Reader.FontTable[RTFMarker.FontIndex].Charset;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
{$IFNDEF RICHVIEWCBDEF4}
function max(a,b:Integer): Integer;
begin
  if a>b then
    Result := a
  else
    Result := b;
end;
function min(a,b:Integer): Integer;
begin
  if a<b then
    Result := a
  else
    Result := b;
end;
{$ENDIF}

function TRVRTFReaderProperties.GetMarkerIndex(RTFMarker: TRVRTFMarkerProperties): Integer;
var i: Integer;
    Level : TRVListLevel;
    FormatStr1,FormatStr2, FontName: String;
    ListType: TRVListType;
    FirstIndent, LeftIndent, PNIndent, PNSpace: Integer;
    MarkerAlign: TRVRTFAlignment;
    {..............................................}
    function IsEqualIndents(Level : TRVListLevel): Boolean;
    begin
      if RTFMarker.Hanging then
        Result :=
          (Level.LeftIndent=LeftIndent) and
          (Level.FirstIndent=max(PNSpace, FirstIndent+PNIndent))
      else
        Result :=
          (Level.LeftIndent=LeftIndent) and
          (Level.FirstIndent= max(0, FirstIndent+max(PNIndent,PNSpace)));
      if not Result then
        exit;
      case MarkerAlign of
        rtf_al_Left:
          Result := Level.MarkerIndent=LeftIndent+FirstIndent;
        rtf_al_Right:
          Result := Level.MarkerIndent=Level.FirstIndent+Level.LeftIndent;
        rtf_al_Center:
          Result := Level.MarkerIndent=(LeftIndent+FirstIndent+Level.FirstIndent+Level.LeftIndent) div 2;
      end;
    end;
    {..............................................}
begin
  Result := -1;
  if ParaStyleMode<>rvrsAddIfNeeded then
    exit;  
  if (RTFMarker=nil) or (RTFMarker.FontIndex>=Reader.FontTable.Count) or (RTFMarker.Level<=0) then
    exit;
  if LevelToListNo.Count=0 then
    LevelToListNo.InitWith(-1, 11);
  if (RTFMarker.Level<11) and (LevelToListNo[RTFMarker.Level-1]>=0) then begin
    Result := LevelToListNo[RTFMarker.Level-1];
    exit;
  end;
  FontName := GetMarkerFontName(Reader, RTFMarker);
  ConvMarker(RTFMarker, ListType, FormatStr1,FormatStr2);
  MarkerAlign := RTFMarker.Alignment;
  if RTFMarker.Level=11 then
    MarkerAlign := rtf_al_Left;
  LeftIndent := Round(Reader.RTFState.ParaProps.LeftIndentTw*PixelsPerTwip);
  FirstIndent := Round(Reader.RTFState.ParaProps.FirstIndentTw*PixelsPerTwip);
  PNIndent := Round(RTFMarker.IndentTw*PixelsPerTwip);
  PNSpace  := Round(RTFMarker.SpaceTw*PixelsPerTwip);
  for i := 0 to RVStyle.ListStyles.Count-1 do begin
    if RVStyle.ListStyles[i].Levels.Count=0 then
      continue;
    Level := RVStyle.ListStyles[i].Levels[0];
    if (Level.ListType=ListType) and
       (Level.MarkerAlignment=TRVMarkerAlignment(MarkerAlign)) and
       IsEqualIndents(Level) and
       (Level.Font.Size=RTFMarker.FontSize) and
       (Level.Font.Style=RTFMarker.FontStyle) and
       (Level.Font.Color=RTFMarker.Color) and
       (AnsiCompareText(Level.Font.Name,FontName)=0) and
       ((Level.FormatString=FormatStr1) or (Level.FormatString=FormatStr2)) then begin
      Result := i;
      LevelToListNo[RTFMarker.Level-1] := Result;
      exit;
    end;
  end;
  Result := RVStyle.ListStyles.Count;
  Level := RVStyle.ListStyles.Add.Levels.Add;
  RVStyle.ListStyles[RVStyle.ListStyles.Count-1].Standard := False;
  Level.ListType   := ListType;
  Level.LeftIndent := LeftIndent;
  Level.MarkerIndent := LeftIndent+FirstIndent;
  if RTFMarker.Hanging then
    Level.FirstIndent := max(PNSpace, FirstIndent+PNIndent)
  else
    Level.FirstIndent := max(0, FirstIndent+max(PNIndent,PNSpace));
  Level.MarkerAlignment := TRVMarkerAlignment(MarkerAlign);
  case MarkerAlign of
    rtf_al_Right:
      begin
        Level.MarkerIndent := Level.FirstIndent+Level.LeftIndent;
      end;
    rtf_al_Center:
      begin
        Level.MarkerIndent := (Level.FirstIndent+Level.LeftIndent+Level.MarkerIndent) div 2;
      end;
  end;
  Level.Font.Size  := RTFMarker.FontSize;
  Level.Font.Style := RTFMarker.FontStyle;
  Level.Font.Color := RTFMarker.Color;
  Level.Font.Name  := FontName;
  Level.FormatString := FormatStr1;
  LevelToListNo[RTFMarker.Level-1] := Result;
end;
{------------------------------------------------------------------------------}
// Old-style numbering has higher priority
function TRVRTFReaderProperties.InsertMarker(ParaNo: Integer): Boolean;
var marker: TRVMarkerItemInfo;
    s: String;
begin
  if ParaStyleMode<>rvrsAddIfNeeded then begin
    Result := False;
    exit;
  end;
  if not Reader.RTFState.ParaProps.HasMarker then
    LevelToListNo.Clear;
  Result := Reader.RTFState.ParaProps.HasMarker and
    (Reader.RTFState.ParaProps.MarkerProps.Level>0);
  if not Result then begin
    if Reader.RTFState.ParaProps.ListOverrideIndex>=0 then begin
      MergeListTable97;
      InsertMarker97(ParaNo);
      Result := True;
    end;
    exit;
  end;
  if LevelToListNo.Count=0 then
    LevelToListNo.InitWith(-1, 11);
  marker := TRVMarkerItemInfo.CreateEx(CurrentRVData,
    -1, -1, Reader.RTFState.ParaProps.MarkerProps.Start,
    LevelToListNo[Reader.RTFState.ParaProps.MarkerProps.Level-1]<0);
  marker.ParaNo := ParaNo;
  marker.BeforeLoading(rvlfRTF);
  s := '';
  InsertItem(s, marker, rtf_ts_NewPara);
  IsLastMarkerWord97 := False;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.InsertMarker97(ParaNo: Integer);
var marker: TRVMarkerItemInfo;
    s: String;
    ListLevel, ListNo, ListMappedNo: Integer;
    ListOverride: TRVRTFListOverride97;
    Start: Integer;
    UseStart: Boolean;
begin
  if ParaStyleMode<>rvrsAddIfNeeded then
    exit;
  ListLevel := Reader.RTFState.ParaProps.ListLevel;
  ListOverride := Reader.ListOverrideTable[Reader.RTFState.ParaProps.ListOverrideIndex];
  if ListLevel<ListOverride.Count then begin
    Start := ListOverride[ListLevel].Start;
    UseStart := ListOverride[ListLevel].UseStart;
    end
  else begin
    Start := 1;
    UseStart := False;
  end;
  ListNo := ListOverride.ListIndex;
  ListMappedNo := ListTableMap97[ListNo];
  marker := TRVMarkerItemInfo.CreateEx(CurrentRVData,
    ListMappedNo, Reader.RTFState.ParaProps.ListLevel, Start, UseStart);
  marker.ParaNo := ParaNo;
  marker.BeforeLoading(rvlfRTF);
  s := '';
  InsertItem(s, marker, rtf_ts_NewPara);
  IsLastMarkerWord97 := True;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.ReaderUpdateMarker(Sender: TObject);
var Marker: TRVMarkerItemInfo;
    ListLevel, ListNo: Integer;
    ListOverride: TRVRTFListOverride97;
begin
  if ParaStyleMode<>rvrsAddIfNeeded then
    exit;
  if LastMarkerIndex>=0 then begin
    if not IsLastMarkerWord97 then begin
      Marker := CurrentRVData.GetItem(LastMarkerIndex) as TRVMarkerItemInfo;
      Marker.ListNo := GetMarkerIndex(Reader.RTFState.ParaProps.MarkerProps);
      if Marker.ListNo>=0 then begin
        Marker.Level := 0;
        CurrentRVData.RecalcMarker(LastMarkerIndex, True);
      end;
      end
    else begin
      ListLevel := Reader.RTFState.ParaProps.ListLevel;
      if Reader.RTFState.ParaProps.ListOverrideIndex>=0 then begin
        ListOverride := Reader.ListOverrideTable[Reader.RTFState.ParaProps.ListOverrideIndex];
        ListNo := ListOverride.ListIndex;
        if not Reader.ListTable[ListNo].Items[ListLevel].FFontSizeDefined then begin
          Marker := CurrentRVData.GetItem(LastMarkerIndex) as TRVMarkerItemInfo;
          if (Marker.ListNo>=ListStylesCountBefore) then begin
            RVStyle.ListStyles[Marker.ListNo].Levels[Marker.Level].Font.Size :=
              Reader.RTFState.CharProps.Size;
            Reader.ListTable[ListNo].Items[ListLevel].FFontSize :=
              Reader.RTFState.CharProps.Size;
            Reader.ListTable[ListNo].Items[ListLevel].FFontSizeDefined :=
              True;
          end;
        end;
      end;
    end;
    LastMarkerIndex := -1;
  end;
end;
{------------------------------------------------------------------------------}
procedure ConvMarker97(Reader: TRVRTFReader;
                     RTFMarker: TRVRTFListLevel97;
                     var RVType: TRVListType;
                     var FormatStr1,FormatStr2: String;
                     var FormatStrW:
                     {$IFDEF RICHVIEWCBDEF3}WideString{$ELSE}String{$ENDIF}
                     );
var i: Integer;
    s1,s2: String;
{$IFNDEF RVDONOTUSEUNICODE}
    FontName: String;
{$ENDIF}
begin
  case RTFMarker.ListType of
    rtf_pn_Decimal, rtf_pn_Default:
      RVType := rvlstDecimal;
    rtf_pn_LowerLetter:
      RVType := rvlstLowerAlpha;
    rtf_pn_UpperLetter:
      RVType := rvlstUpperAlpha;
    rtf_pn_LowerRoman:
      RVType := rvlstLowerRoman;
    rtf_pn_UpperRoman:
      RVType := rvlstUpperRoman;
    rtf_pn_Bullet:
      RVType := rvlstBullet;
  end;
  FormatStr1 := Copy(RTFMarker.Text, 2, Length(RTFMarker.Text)-1);
  FormatStr2 := FormatStr1;
  for i := Length(FormatStr1) downto 1 do
    if FormatStr1[i] in [#0..#9] then begin
      s2 := '%'+IntToStr(ord(FormatStr1[i]))+':s';
      if FormatStr1[i]=#0 then
        s1 := '%s'
      else
        s1 := s2;
      Delete(FormatStr1,i,1);
      Insert(s1, FormatStr1, i);
      Delete(FormatStr2,i,1);
      Insert(s2, FormatStr2, i);
    end;
  FormatStrW := '';
  {$IFNDEF RVDONOTUSEUNICODE}
  if (RTFMarker.TextW<>'') and (RTFMarker.ListType=rtf_pn_Bullet) then begin
    RVType := rvlstUnicodeBullet;
    FormatStrW := Copy(RTFMarker.TextW, 2, Length(RTFMarker.TextW)-1);
    if (Length(FormatStrW)=1) then begin
      FontName := GetMarkerFontName(Reader, RTFMarker);
      if (Word(FormatStrW[1])>=61601) and (Word(FormatStrW[1])<=61694) and
         (CompareText(FontName, 'Symbol')=0) then begin
        FormatStr1 := chr(Word(FormatStrW[1])-61601+161);
        FormatStr2 := FormatStr1;
        RVType := rvlstBullet;
      end;
      if (Word(FormatStrW[1])>=61473) and (Word(FormatStrW[1])<=61695) and
         (CompareText(FontName, 'Wingdings')=0) then begin
        FormatStr1 := chr(Word(FormatStrW[1])-61473+33);
        FormatStr2 := FormatStr1;
        RVType := rvlstBullet;
      end;
    end;
  end;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
function AreLevelsEqual97(RVLevel: TRVListLevel; RTFLevel: TRVRTFListLevel97;
                     RVType: TRVListType;
                     const FontName, FormatStr1, FormatStr2: String;
                     const FormatStrW: {$IFDEF RICHVIEWCBDEF3}WideString{$ELSE}String{$ENDIF};
                     PixelsPerTwip: Double): Boolean;
begin
  Result :=  (RVLevel.ListType = RVType) and
            (RVLevel.StartFrom = RTFLevel.Start) and
            (RVLevel.MarkerAlignment = TRVMarkerAlignment(RTFLevel.Alignment)) and
            (RVLevel.LeftIndent = Round(RTFLevel.LeftIndentTw*PixelsPerTwip)) and
            (RVLevel.MarkerIndent = Round(RTFLevel.FirstIndentTw*PixelsPerTwip)+RVLevel.LeftIndent) and
            (RVLevel.FirstIndent = Round(RTFLevel.TabPosTw*PixelsPerTwip)-RVLevel.LeftIndent) and
            (RTFLevel.NoRestart = not (rvloLevelReset in RVLevel.Options)) and
            (RTFLevel.Legal = (rvloLegalStyleNumbering in RVLevel.Options)) and
            (RVLevel.Font.Style = RTFLevel.FontStyle) and
            (RVLevel.Font.Color = RTFLevel.Color) and
            (RVLevel.Font.Size  = RTFLevel.FontSize) and
            (CompareText(RVLevel.Font.Name, FontName) = 0);
  if not Result then
    exit;
  case RVType of
    rvlstDecimal,rvlstLowerAlpha,
    rvlstUpperAlpha,rvlstLowerRoman,
    rvlstUpperRoman,rvlstBullet:
      Result := (RVLevel.FormatString=FormatStr1) or
                (RVLevel.FormatString=FormatStr2);
    {$IFNDEF RVDONOTUSEUNICODE}
    rvlstUnicodeBullet:
      Result := (RVLevel.FormatStringW=FormatStrW);
    {$ENDIF}
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.AreListStylesEqual97(RVList: TRVListInfo; RTFList: TRVRTFList97): Boolean;
var
  RVType: TRVListType;
  FontName, FormatStr1, FormatStr2: String;
  FormatStrW: {$IFDEF RICHVIEWCBDEF3}WideString{$ELSE}String{$ENDIF};
  RTFLevel: TRVRTFListLevel97;
  i: Integer;
begin
  Result := RVList.Levels.Count>=RTFList.Count;
  if not Result then
    exit;
  for i := 0 to RTFList.Count-1 do begin
     RTFLevel := RTFList.Items[i];
      ConvMarker97(Reader, RTFLevel, RVType, FormatStr1, FormatStr2, FormatStrW);
      FontName := GetMarkerFontName(Reader, RTFLevel);
      Result := AreLevelsEqual97(RVList.Levels[i], RTFLevel, RVType,
        FontName,FormatStr1,FormatStr2,FormatStrW,PixelsPerTwip);
      if not Result then
        exit;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.FindListStyle97(RTFList: TRVRTFList97;
  ForbiddenStyles: TRVIntegerList): Integer;
var i: Integer;
begin
  if ParaStyleMode=rvrsAddIfNeeded then
    for i := 0 to RVStyle.ListStyles.Count-1 do
      if (ForbiddenStyles.IndexOf(Pointer(i))<0) and
         AreListStylesEqual97(RVStyle.ListStyles[i], RTFList) then begin
        Result := i;
        exit;
      end;
  Result := -1;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.MergeListTable97;
var i,j: Integer;
    RVLevel : TRVListLevel;
    RTFLevel: TRVRTFListLevel97;
    FormatStr1,FormatStr2,FontName: String;
    FormatStrW: {$IFDEF RICHVIEWCBDEF3}WideString{$ELSE}String{$ENDIF};
    {$IFDEF RICHVIEWCBDEF3}
    Charset: TFontCharset;
    {$ENDIF}
    RVType: TRVListType;
    ListNo: Integer;
    ForbiddenStyles: TRVIntegerList;
begin
  if ParaStyleMode<>rvrsAddIfNeeded then
    exit;
  if ListTableMap97<>nil then
    exit;
  ListStylesCountBefore := RVStyle.ListStyles.Count;
  ForbiddenStyles := TRVIntegerList.Create;
  try
    ListTableMap97 := TRVIntegerList.Create;
    for i := 0 to Reader.ListTable.Count-1 do begin
      ListNo := FindListStyle97(Reader.ListTable[i], ForbiddenStyles);
      if ListNo>=0 then begin
        ListTableMap97.Add(ListNo);
        ForbiddenStyles.Add(ListNo);
        end
      else begin
        RVStyle.ListStyles.Add;
        RVStyle.ListStyles[RVStyle.ListStyles.Count-1].Standard := False;
        ListTableMap97.Add(RVStyle.ListStyles.Count-1);
        ForbiddenStyles.Add(RVStyle.ListStyles.Count-1);
        for j := 0 to Reader.ListTable[i].Count-1 do begin
          RTFLevel := Reader.ListTable[i].Items[j];
          ConvMarker97(Reader, RTFLevel, RVType, FormatStr1, FormatStr2, FormatStrW);
          FontName := GetMarkerFontName(Reader, RTFLevel);
          {$IFDEF RICHVIEWCBDEF3}
          Charset  := GetMarkerCharset(Reader, RTFLevel);
          {$ENDIF}
          RVLevel := RVStyle.ListStyles[RVStyle.ListStyles.Count-1].Levels.Add;
          RVLevel.ListType := RVType;
          RVLevel.FormatString := FormatStr1;
          {$IFNDEF RVDONOTUSEUNICODE}
          RVLevel.FormatStringW := FormatStrW;
          {$ENDIF}
          RVLevel.MarkerAlignment := TRVMarkerAlignment(RTFLevel.Alignment);
          RVLevel.Font.Name := FontName;
          {$IFDEF RICHVIEWCBDEF3}
          RVLevel.Font.Charset  := Charset;
          {$ENDIF}
          RVLevel.Font.Style := RTFLevel.FontStyle;
          RVLevel.Font.Color := RTFLevel.Color;
          RVLevel.Font.Size  := RTFLevel.FontSize;
          RVLevel.LeftIndent := Round(RTFLevel.LeftIndentTw*PixelsPerTwip);
          RVLevel.MarkerIndent := Round(RTFLevel.FirstIndentTw*PixelsPerTwip)+RVLevel.LeftIndent;
          RVLevel.FirstIndent := Round(RTFLevel.TabPosTw*PixelsPerTwip)-RVLevel.LeftIndent;
          RVLevel.StartFrom   := RTFLevel.Start;
          if RTFLevel.NoRestart then
            RVLevel.Options := RVLevel.Options - [rvloLevelReset];
          if RTFLevel.Legal then
            RVLevel.Options := RVLevel.Options + [rvloLegalStyleNumbering];
        end;
      end;
    end;
  finally
    ForbiddenStyles.Free;
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.ReaderProgress(Sender: TRVRTFReader;
  Stage: TRVRTFProgressStage; PercentDone: Byte);
begin
  RVData.DoProgress(rvloLoading, TRVProgressStage(Stage), PercentDone);
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.NewReaderText(Sender: TRVRTFReader;
  const Text: String; Position: TRVRTFPosition);
var StyleNo : Integer;
    item    : TRVTextItemInfo;
    s: String;
begin
  if FSkipHiddenText and Reader.RTFState.CharProps.Hidden then
    exit;
  StyleNo := ReturnStyleNo(UnicodeMode=rvruOnlyUnicode);
  item := CreateTextItem(Text,
                         {$IFDEF RICHVIEWCBDEF3} '', {$ENDIF}
                         StyleNo, ReturnParaNo(Position), False, s);
  InsertItem(s, item, Position);
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.ReaderEndParsing(Sender: TObject);
begin
  {$IFDEF RICHVIEW_DPMARGINS}
  if not EditFlag then begin
    if Reader.RTFState.DocProps.LeftMarginTw>0 then
      RVData.GetAbsoluteRootData.GetDocProperties.Values['LeftMarginMM'] :=
        IntToStr(Round(Reader.RTFState.DocProps.LeftMarginTw*127/(1440*5)));
    if Reader.RTFState.DocProps.TopMarginTw>0 then
      RVData.GetAbsoluteRootData.GetDocProperties.Values['TopMarginMM'] :=
        IntToStr(Round(Reader.RTFState.DocProps.TopMarginTw*127/(1440*5)));
    if Reader.RTFState.DocProps.RightMarginTw>0 then
      RVData.GetAbsoluteRootData.GetDocProperties.Values['RightMarginMM'] :=
        IntToStr(Round(Reader.RTFState.DocProps.RightMarginTw*127/(1440*5)));
    if Reader.RTFState.DocProps.BottomMarginTw>0 then
      RVData.GetAbsoluteRootData.GetDocProperties.Values['BottomMarginMM'] :=
        IntToStr(Round(Reader.RTFState.DocProps.BottomMarginTw*127/(1440*5)));
  end;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
procedure TRVRTFReaderProperties.NewReaderUnicodeText(Sender: TRVRTFReader;
  const Text: WideString; Position: TRVRTFPosition);
var StyleNo : Integer;
    item    : TRVTextItemInfo;
    s: String;
begin
  if FSkipHiddenText and Reader.RTFState.CharProps.Hidden then
    exit;
  StyleNo := ReturnStyleNo(UnicodeMode in [rvruOnlyUnicode, rvruMixed]);
  item := CreateTextItem('', Text, StyleNo, ReturnParaNo(Position), True, s);
  InsertItem(s, item, Position);
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.ReaderHeaderFooter(Sender: TRVRTFReader;
  HFType: TRVRTFHeaderFooterType; Starting: Boolean;
  var Supported: Boolean);
begin
  // 1 twip = 20 points = 1/1440 inch
  Supported := False;
  if Starting then begin
    case HFType of
      rtf_hf_Header:
        if FHeaderRVData<>nil then begin
          CurrentRVData := FHeaderRVData;
          CurrentRVData.Clear;
          FHeaderYMM := Round(Reader.RTFState.SectProps.HeaderYTw*127/(1440*5));
          Supported := True;
          Self.HFType := HFType;
        end;
      rtf_hf_Footer:
        if FFooterRVData<>nil then begin
          CurrentRVData := FFooterRVData;
          CurrentRVData.Clear;
          FFooterYMM := Round(Reader.RTFState.SectProps.FooterYTw*127/(1440*5));
          Supported := True;
          Self.HFType := HFType;
        end;
    end;
    end
  else begin
    CurrentRVData := RVData;
    Self.HFType := rtf_hf_MainText;
  end;
  RVStyle := CurrentRVData.GetRVStyle;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.ReaderPageBreak(Sender: TObject);
begin
  PageBreakRVData := CurrentRVData;
  PageBreak := True;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.InitReader;
begin
  PixelsPerTwip := Screen.PixelsPerInch / (72*20);
  Reader.ConvertHighlight := TRVRTFHighlightConvert(ConvertHighlight);
  Reader.BasePath := BasePath;
  Reader.ExtractMetafileBitmaps := ExtractMetafileBitmaps;
  HFType := rtf_hf_MainText;
  {$IFNDEF RVDONOTUSELISTS}
  Reader.OnUpdateMarker := ReaderUpdateMarker;
  LastMarkerIndex := -1;
  LevelToListNo := TRVIntegerList.Create;
  {$ENDIF}
  Reader.OnNewText := NewReaderText;
  {$IFNDEF RVDONOTUSEUNICODE}
  if UnicodeMode in [rvruMixed, rvruOnlyUnicode] then
    Reader.OnNewUnicodeText := NewReaderUnicodeText;
  {$ENDIF}
  if not IgnorePictures then begin
    Reader.OnNewPicture := NewReaderPicture;
    Reader.OnImportPicture := ReaderImportPicture;
  end;
  if RVData.IsAssignedOnProgress then
    Reader.OnProgress := ReaderProgress
  else
    Reader.OnProgress := nil; 
  {$IFNDEF RVDONOTUSEOLECONTAINER}
  (*
  Reader.OnNewObject := NewReaderObject;
  *)
  {$ENDIF}
  Reader.DefCodePage := RVData.GetDefaultCodePage;
  Reader.OnRequiredPageBreak := ReaderPageBreak;
  Reader.OnEndParsing := ReaderEndParsing;
  {$IFNDEF RVDONOTUSETABLES}
  Reader.OnTable     := ReaderTable;
  {$ENDIF}
  RVStyle := RVData.GetRVStyle;
  PageBreak := False;
  PageBreakRVData := nil;
  NonFirstItemsAdded := 0;
  FirstTime := True;
  FailedBecauseOfProtect := False;
  FullReformat := False;
  Tables    := nil;
  CurrentRVData := RVData;
  CurrentRow := -1;
  CurrentCol := -1;
  FEmptyPara := -1;
  FHeaderYMM := -1;
  FFooterYMM := -1;
  if (FHeaderRVData<>nil) or (FFooterRVData<>nil) then
    Reader.OnHeaderFooter := ReaderHeaderFooter;
  if (FHeaderRVData<>nil) then
    FHeaderRVData.Clear;
  if (FFooterRVData<>nil) then
    FFooterRVData.Clear;
  {$IFNDEF RVDONOTUSELISTS}
  ListStylesCountBefore := 0;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.DoneReader;
begin
  {$IFNDEF RVDONOTUSELISTS}
  ReaderUpdateMarker(nil);
  {$ENDIF}
  Tables.Free;
  Tables := nil;
  {$IFNDEF RVDONOTUSELISTS}
  LevelToListNo.Free;
  LevelToListNo := nil;
  ListTableMap97.Free;
  ListTableMap97 := nil;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.CurrentBorder(
  var RVBorderStyle: TRVBorderStyle; var RVBorderWidth,
  RVBorderIntWidth: Integer; var RVBorderColor: TColor;
  var RVBorderOffs: TRVRect);
var side: TRVRTFSide;
begin
  with Reader.RTFState.ParaProps do begin
    if HasBorder then
      with Border do begin
        RVBorderOffs := TRVRect.Create;
        RVBorderOffs.Left   := Round(Sides[rtf_side_Left  ].SpaceTw*PixelsPerTwip);
        RVBorderOffs.Top    := Round(Sides[rtf_side_Top   ].SpaceTw*PixelsPerTwip);
        RVBorderOffs.Right  := Round(Sides[rtf_side_Right ].SpaceTw*PixelsPerTwip);
        RVBorderOffs.Bottom := Round(Sides[rtf_side_Bottom].SpaceTw*PixelsPerTwip);
        if Sides[rtf_side_Top].BorderType<>rtf_brdr_None then
          side := rtf_side_Top
        else if Sides[rtf_side_Left].BorderType<>rtf_brdr_None then
          side := rtf_side_Left
        else   if Sides[rtf_side_Bottom].BorderType<>rtf_brdr_None then
          side := rtf_side_Bottom
        else
          side := rtf_side_Right;
        RVBorderColor := Sides[side].Color;
        BorderRTF2RV(Sides[side].BorderType, Sides[side].WidthTw,
          RVBorderStyle, RVBorderWidth, RVBorderIntWidth,
          PixelsPerTwip, side in [rtf_side_Bottom, rtf_side_Right]);
      end
    else begin
      RVBorderStyle := rvbNone;
      RVBorderWidth := 0;
      RVBorderIntWidth := 0;
      RVBorderColor   := clNone;
      RVBorderOffs := nil;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.FindBestParaNo: Integer;
var i: Integer;
    Para: TParaInfo;
    LeftIndent, RightIndent, FirstIndent, SpaceBefore, SpaceAfter: Integer;
    w, bw, maxw,LS: Integer;
    RVBorderStyle: TRVBorderStyle; RVBorderWidth, RVBorderIntWidth: Integer;
    RVBorderOffs: TRVRect;
    {..........................................................}
    function CompareBorderSide( RVBorder:TRVBorder;
      RVBorderStyle: TRVBorderStyle; RVBorderWidth,RVBorderIntWidth: Integer;
      RVBorderColor: TColor; SideVisible: Boolean): Integer;

    begin
      Result := 0;
      with Reader.RTFState.ParaProps do
        if HasBorder and (RVBorderStyle<>rvbNone) then begin
          if (RVBorder.Style=rvbNone) or not SideVisible then
            exit;
          inc(Result, RVSMW_BORDERSIDE);
          inc(Result, RV_CompareInts(RVBorderWidth, RVBorder.Width, RVSMW_WIDTH));
          inc(Result, RV_CompareInts(RVBorderIntWidth, RVBorder.InternalWidth, RVSMW_WIDTH));
          inc(Result, RV_CompareColors(RVBorder.Color, RVBorderColor, RVSMW_EACHRGBCOLOR, RVSMW_COLORSET));
          if RVBorderStyle=RVBorder.Style then
            inc(Result, RVSMW_BORDERSTYLE);
          end
        else
          if (RVBorder.Style=rvbNone) or not SideVisible then
            inc(Result, RVSMW_BORDERNOSIDE);
    end;
    {..........................................................}
    function CompareLS: Integer;
    var v: Integer;
    begin
      Result := 0;
      if not Reader.RTFState.ParaProps.LineSpacingMulti then
        exit;
      if Para.LineSpacingType=rvlsSpaceBetween then begin
        if Para.LineSpacing>1 then
          Result := - 4*RVSMW_LINESPACING
        else if (LS<=100) then
          Result := RVSMW_LINESPACING;
        exit;
      end;
      v := Para.LineSpacing;
      if v<100 then
        v := 100;
      Result := RV_CompareInts(LS, v, RVSMW_LINESPACING);
    end;
    {..........................................................}
begin
  Result := 0;
  maxw   := 0;

  with Reader.RTFState.ParaProps do begin
    LeftIndent   := Round(LeftIndentTw *PixelsPerTwip);
    RightIndent  := Round(RightIndentTw*PixelsPerTwip);
    FirstIndent  := Round(FirstIndentTw*PixelsPerTwip);
    SpaceBefore  := Round(SpaceBeforeTw*PixelsPerTwip);
    SpaceAfter   := Round(SpaceAfterTw *PixelsPerTwip);
    LS           := abs(LineSpacing) * 100 div 240;
    if LS<100 then
      LS := 100;
    if HasBorder then begin
        RVBorderOffs := TRVRect.Create;
        RVBorderOffs.Left   := Round(Border.Sides[rtf_side_Left  ].SpaceTw*PixelsPerTwip);
        RVBorderOffs.Top    := Round(Border.Sides[rtf_side_Top   ].SpaceTw*PixelsPerTwip);
        RVBorderOffs.Right  := Round(Border.Sides[rtf_side_Right ].SpaceTw*PixelsPerTwip);
        RVBorderOffs.Bottom := Round(Border.Sides[rtf_side_Bottom].SpaceTw*PixelsPerTwip);
      end
    else
      RVBorderOffs := nil;
    for i := 0 to RVStyle.ParaStyles.Count-1 do begin
      if not AllowUseStyle(i, False) then
        continue;
      Para := RVStyle.ParaStyles[i];
      bw := Para.Border.GetTotalWidth;
      w := 0;
      if ord(Para.Alignment)=ord(Alignment) then
        inc(w, RVSMW_ALIGNMENT);
      inc(w, RV_CompareInts(LeftIndent+bw,  Para.LeftIndent,  RVSMW_INDENT));
      inc(w, RV_CompareInts(RightIndent+bw, Para.RightIndent, RVSMW_INDENT));
      inc(w, RV_CompareInts(FirstIndent, Para.FirstIndent, RVSMW_INDENT));
      inc(w, RV_CompareInts(SpaceBefore+bw, Para.SpaceBefore, RVSMW_INDENT));
      inc(w, RV_CompareInts(SpaceAfter+bw,  Para.SpaceAfter,  RVSMW_INDENT));
      inc(w, RV_CompareColors(Para.Background.Color, Color, RVSMW_EACHRGBBCOLOR, RVSMW_BCOLORSET));
      if (rvpaoKeepLinesTogether in Para.Options) = KeepLinesTogether then
        inc(w, RVSMW_KEEPLINESTOGETHER);
      if (rvpaoKeepWithNext in Para.Options) = KeepWithNext then
        inc(w, RVSMW_KEEPWITHNEXT);
      inc(w, CompareLS);
      if HasBorder then begin
        inc(w, RV_CompareInts(RVBorderOffs.Left,   Para.Border.BorderOffsets.Left,   RVSMW_INDENT));
        inc(w, RV_CompareInts(RVBorderOffs.Top,    Para.Border.BorderOffsets.Top,    RVSMW_INDENT));
        inc(w, RV_CompareInts(RVBorderOffs.Right,  Para.Border.BorderOffsets.Right,  RVSMW_INDENT));
        inc(w, RV_CompareInts(RVBorderOffs.Bottom, Para.Border.BorderOffsets.Bottom, RVSMW_INDENT));
        BorderRTF2RV(Border.Sides[rtf_side_Left].BorderType, Border.Sides[rtf_side_Left].WidthTw,
                     RVBorderStyle, RVBorderWidth, RVBorderIntWidth,
                     PixelsPerTwip, False);
        inc(w, CompareBorderSide(Para.Border, RVBorderStyle, RVBorderWidth,
                                 RVBorderIntWidth,
                                 Border.Sides[rtf_side_Left].Color,
                                 Para.Border.VisibleBorders.Left));
        BorderRTF2RV(Border.Sides[rtf_side_Top].BorderType, Border.Sides[rtf_side_Top].WidthTw,
                     RVBorderStyle, RVBorderWidth, RVBorderIntWidth,
                     PixelsPerTwip, False);
        inc(w, CompareBorderSide(Para.Border, RVBorderStyle, RVBorderWidth,
                                 RVBorderIntWidth,
                                 Border.Sides[rtf_side_Top].Color,
                                 Para.Border.VisibleBorders.Top));
        BorderRTF2RV(Border.Sides[rtf_side_Right].BorderType, Border.Sides[rtf_side_Right].WidthTw,
                     RVBorderStyle, RVBorderWidth, RVBorderIntWidth,
                     PixelsPerTwip, True);
        inc(w, CompareBorderSide(Para.Border, RVBorderStyle, RVBorderWidth,
                                 RVBorderIntWidth,
                                 Border.Sides[rtf_side_Right].Color,
                                 Para.Border.VisibleBorders.Right));
        BorderRTF2RV(Border.Sides[rtf_side_Bottom].BorderType, Border.Sides[rtf_side_Bottom].WidthTw,
                     RVBorderStyle, RVBorderWidth, RVBorderIntWidth,
                     PixelsPerTwip, True);
        inc(w, CompareBorderSide(Para.Border, RVBorderStyle, RVBorderWidth,
                                 RVBorderIntWidth,
                                 Border.Sides[rtf_side_Bottom].Color,
                                 Para.Border.VisibleBorders.Bottom));
        end
      else
        if Para.Border.Style=rvbNone then
          inc(w, RVSMW_BORDERNOSIDE);
      if w>maxw then begin
        Result := i;
        maxw := w;
      end;
    end;
    RVBorderOffs.Free;
  end;
end;
{------------------------------------------------------------------------------}
function GetRVBorderTotalWidth(RVBorderStyle: TRVBorderStyle;
  RVBorderWidth, RVBorderIntWidth: Integer): Integer;
begin
  case RVBorderStyle of
    rvbSingle:
      Result := RVBorderWidth;
    rvbDouble:
      Result := 2*RVBorderWidth+RVBorderIntWidth;
    rvbTriple:
      Result := 3*RVBorderWidth+2*RVBorderIntWidth;
    rvbThickInside, rvbThickOutside:
      Result := 3*RVBorderWidth+RVBorderIntWidth;
    else
      Result := 0;
  end;
end;
{------------------------------------------------------------------------------}
procedure AdjustIndents(RVBorderStyle: TRVBorderStyle;
  RVBorderWidth, RVBorderIntWidth: Integer; RVBorderOffs: TRVRect;
   var LeftIndent, RightIndent, SpaceBefore, SpaceAfter: Integer);
var w: Integer;
begin
 if RVBorderOffs<>nil then begin
   w := GetRVBorderTotalWidth(RVBorderStyle, RVBorderWidth, RVBorderIntWidth)-1;
   if LeftIndent<RVBorderOffs.Left+w then
     LeftIndent := RVBorderOffs.Left+w;
   if SpaceBefore<RVBorderOffs.Top+w then
     SpaceBefore := RVBorderOffs.Top+w;
   if RightIndent<RVBorderOffs.Right+w then
     RightIndent := RVBorderOffs.Right+w;
   if SpaceAfter<RVBorderOffs.Bottom+w then
     SpaceAfter := RVBorderOffs.Bottom+w;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.FindParaNo(RVBorderStyle: TRVBorderStyle;
  RVBorderWidth, RVBorderIntWidth: Integer; RVBorderColor: TColor;
  RVBorderOffs: TRVRect): Integer;
    {..........................................................}
    function EqualBorders(RVBorder:TRVBorder; RVBorderStyle: TRVBorderStyle;
      RVBorderWidth,RVBorderIntWidth: Integer; RVBorderColor: TColor;
      RVBorderOffs: TRVRect):Boolean;

    begin
      with Reader.RTFState.ParaProps do
        if HasBorder then
        Result := (RVBorder.Style=RVBorderStyle) and
                  (RVBorder.Width=RVBorderWidth) and
                  ((RVBorder.InternalWidth=RVBorderIntWidth) or (RVBorderStyle=rvbSingle)) and
                  (RVBorder.Color=RVBorderColor) and
                  (RVBorder.VisibleBorders.Left   = (Border.Sides[rtf_side_Left].BorderType<>rtf_brdr_None))  and
                  (RVBorder.VisibleBorders.Top    = (Border.Sides[rtf_side_Top].BorderType<>rtf_brdr_None))   and
                  (RVBorder.VisibleBorders.Right  = (Border.Sides[rtf_side_Right].BorderType<>rtf_brdr_None)) and
                  (RVBorder.VisibleBorders.Bottom = (Border.Sides[rtf_side_Bottom].BorderType<>rtf_brdr_None)) and
                  (RVBorderOffs<>nil) and RVBorder.BorderOffsets.IsEqual(RVBorderOffs)
      else
        Result := RVBorder.Style=rvbNone;
    end;
    {..........................................................}
    function EqualLS(Para: TParaInfo; LS: Integer): Boolean;
    begin
      if not Reader.RTFState.ParaProps.LineSpacingMulti then begin
        Result := True;
        exit;
      end;
      if (Para.LineSpacingType=rvlsSpaceBetween) then begin
         if (Para.LineSpacing>1) then begin
           Result := False;
           exit;
         end;
         Result := LS<=100;
         exit;
      end;
      Result := (LS = Para.LineSpacing) or
                ((LS<100) and (Para.LineSpacing<100));
    end;
    {..........................................................}
var i: Integer;
    Para: TParaInfo;
    LeftIndent, RightIndent, FirstIndent, SpaceBefore, SpaceAfter, LS: Integer;
begin
  Result := -1;
  with Reader.RTFState.ParaProps do begin
   LeftIndent   := Round(LeftIndentTw *PixelsPerTwip);
   RightIndent  := Round(RightIndentTw*PixelsPerTwip);
   FirstIndent  := Round(FirstIndentTw*PixelsPerTwip);
   SpaceBefore  := Round(SpaceBeforeTw*PixelsPerTwip);
   SpaceAfter   := Round(SpaceAfterTw *PixelsPerTwip);
   AdjustIndents(RVBorderStyle, RVBorderWidth, RVBorderIntWidth, RVBorderOffs,
     LeftIndent, RightIndent, SpaceBefore, SpaceAfter);
   LS           := abs(LineSpacing) * 100 div 240;
   for i := 0 to RVStyle.ParaStyles.Count-1 do begin
     Para := RVStyle.ParaStyles[i];
     if (ord(Para.Alignment)=ord(Alignment)) and
        (Para.LeftIndent = LeftIndent) and
        (Para.RightIndent = RightIndent) and
        (Para.FirstIndent = FirstIndent) and
        (Para.SpaceBefore = SpaceBefore) and
        (Para.SpaceAfter  = SpaceAfter) and
        (Para.Background.Color = Color) and
        ((rvpaoKeepLinesTogether in Para.Options) = KeepLinesTogether) and
        ((rvpaoKeepWithNext in Para.Options) = KeepWithNext) and
        EqualLS(Para,LS) and
        EqualBorders(Para.Border, RVBorderStyle, RVBorderWidth,
          RVBorderIntWidth, RVBorderColor, RVBorderOffs)
       then begin
         Result := i;
         break;
       end;
     end;
  end;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSETABLES}
function TRVRTFReaderProperties.GetEmptyStyleNo: Integer;
begin
  case TextStyleMode of
    rvrsUseSpecified:
      Result := TextStyleNo;
    else
      Result := 0;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.GetEmptyParaNo(Alignment: TRVAlignment): Integer;
var pi: TParaInfo;
begin
  if (Alignment=rvaLeft) and  (FEmptyPara>=0) then begin
    Result := FEmptyPara;
    exit;
  end;
  case ParaStyleMode of
    rvrsUseSpecified:
      Result := ParaStyleNo;
    rvrsUseClosest:
      Result := 0;
    else
      begin
        pi := RVStyle.ParaStyles[0];
        if (pi.LeftIndent=0) and
           (pi.RightIndent=0) and
           (pi.FirstIndent=0) and
           (pi.Alignment=Alignment) then
          Result := 0
        else begin
          pi := TParaInfo.Create(nil);
          try
            pi.Assign(RVStyle.ParaStyles[0]);
            pi.LeftIndent  := 0;
            pi.RightIndent := 0;
            pi.FirstIndent := 0;
            pi.Alignment   := Alignment;
            Result := RVStyle.ParaStyles.FindSuchStyle(0,pi,RVAllParaInfoProperties);
            if Result=-1 then begin
              RVStyle.ParaStyles.Add.Assign(pi);
              Result := RVStyle.ParaStyles.Count-1;
            end;
          finally
            pi.Free;
          end;
        end;
      end;
  end;
  if (Alignment=rvaLeft) then
    FEmptyPara := Result;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.AddPara(RVBorderStyle: TRVBorderStyle;
  RVBorderWidth, RVBorderIntWidth: Integer; RVBorderColor: TColor;
  RVBorderOffs: TRVRect);
var Para: TParaInfo;
    LeftIndent, RightIndent, SpaceBefore, SpaceAfter: Integer;
begin
  Para := TParaInfo(RVStyle.ParaStyles.Add);
  Para.Standard := False;
  with Reader.RTFState.ParaProps do begin
    Para.Alignment   := TRVAlignment(Alignment);
    Para.FirstIndent := Round(FirstIndentTw*PixelsPerTwip);
    LeftIndent  := Round(LeftIndentTw *PixelsPerTwip);
    RightIndent := Round(RightIndentTw*PixelsPerTwip);
    SpaceBefore := Round(SpaceBeforeTw*PixelsPerTwip);
    SpaceAfter  := Round(SpaceAfterTw *PixelsPerTwip);
    AdjustIndents(RVBorderStyle, RVBorderWidth, RVBorderIntWidth, RVBorderOffs,
      LeftIndent, RightIndent, SpaceBefore, SpaceAfter);
    Para.LeftIndent := LeftIndent;
    Para.RightIndent := RightIndent;
    Para.SpaceBefore := SpaceBefore;
    Para.SpaceAfter := SpaceAfter;
    if LineSpacingMulti then begin
      Para.LineSpacing := LineSpacing*100 div 240;
    end;
    Para.Background.Color := Color;
    if KeepLinesTogether then
      Para.Options := Para.Options + [rvpaoKeepLinesTogether];
    if KeepWithNext then
      Para.Options := Para.Options + [rvpaoKeepWithNext];
    if HasBorder then begin
      Para.Border.Style         := RVBorderStyle;
      Para.Border.Width         := RVBorderWidth;
      Para.Border.InternalWidth := RVBorderIntWidth;
      with Border do begin
        Para.Border.Color         := RVBorderColor;
        Para.Border.VisibleBorders.Left   := Sides[rtf_side_Left].BorderType  <>rtf_brdr_None;
        Para.Border.VisibleBorders.Top    := Sides[rtf_side_Top].BorderType   <>rtf_brdr_None;
        Para.Border.VisibleBorders.Right  := Sides[rtf_side_Right].BorderType <>rtf_brdr_None;
        Para.Border.VisibleBorders.Bottom := Sides[rtf_side_Bottom].BorderType<>rtf_brdr_None;
      end;
      if RVBorderOffs<>nil then
        Para.Border.BorderOffsets.Assign(RVBorderOffs);
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.ReturnParaNo_: Integer;
var RVBorderStyle: TRVBorderStyle;
    RVBorderWidth, RVBorderIntWidth: Integer;
    RVBorderColor: TColor;
    RVBorderOffs: TRVRect;
begin
  case ParaStyleMode of
    rvrsUseSpecified:
      Result := ParaStyleNo;
    rvrsUseClosest:
      Result := FindBestParaNo;
    else
      begin
        CurrentBorder(RVBorderStyle, RVBorderWidth, RVBorderIntWidth, RVBorderColor,
          RVBorderOffs);
        Result := FindParaNo(RVBorderStyle, RVBorderWidth, RVBorderIntWidth,
          RVBorderColor, RVBorderOffs);
        if Result<0 then begin
          AddPara(RVBorderStyle, RVBorderWidth, RVBorderIntWidth, RVBorderColor,
            RVBorderOffs);
          Result := RVStyle.ParaStyles.Count-1;
        end;
        RVBorderOffs.Free;
      end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.ReturnParaNo(Position: TRVRTFPosition): Integer;
begin
  case Position of
  rtf_ts_NewPara:
    begin
      Result := ReturnParaNo_;
    end;
  else
    begin
      if CurrentRVData=RVData then begin
        if (InsertPoint-1<RVData.Items.Count) and (InsertPoint>0) then begin
          Result := RVData.GetItemPara(InsertPoint-1);
          end
        else
          Result := 0;
        end
      else begin
        if CurrentRVData.Items.Count=0 then
          Result := 0
        else
          Result := CurrentRVData.GetItemPara(CurrentRVData.Items.Count-1);
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.FindBestStyleNo(AUnicode, AHypertext: Boolean): Integer;
var i: Integer;
    FontStyle: TFontInfo;
    FN: String;
    {$IFDEF RICHVIEWCBDEF3}
    Charset: TFontCharset;
    {$ENDIF}
    w, maxw, FontSize: Integer;
    fs: TFontStyle;
    CharSpacing: Integer;
begin
  Result := 0;
  maxw   := 0;
  with Reader.RTFState.CharProps do begin
    if FontName='' then
      FN := AnsiLowerCase(Reader.FontTable[FontIndex].Name)
    else
      FN := AnsiLowerCase(FontName);
    {$IFDEF RICHVIEWCBDEF3}
    Charset := Reader.FontTable[FontIndex].Charset;
    if (FontName='Symbol') or (FontName='Wingdings') then
      Charset := SYMBOL_CHARSET;
    {$ENDIF}
    CharSpacing  := Round(CharSpacingTw*PixelsPerTwip);
    if SScriptType = rtf_ss_Normal then
      FontSize := Size
    else
      FontSize := Round(Size/2);
    for i := 0 to RVStyle.TextStyles.Count-1 do begin
      if not AllowUseStyle(i, True) then
        continue;
      FontStyle := RVStyle.TextStyles[i];
      w := 0;
      if {$IFNDEF RVDONOTUSEUNICODE}
        (FontStyle.Unicode = AUnicode) and
        {$ENDIF}
        ((AHypertext and FontStyle.Jump) or
         (not AHypertext and (UseHypertextStyles or not FontStyle.Jump))) then begin
        if (AnsiLowerCase(FontStyle.FontName)=FN) then
          inc(w,RVSMW_FONTNAME);
        {$IFDEF RICHVIEWCBDEF3}
        if Charset=FontStyle.Charset then
          inc(w, RVSMW_FONTCHARSET)
        else
          if (Charset=DEFAULT_CHARSET) or
             (FontStyle.Charset=DEFAULT_CHARSET) then
            inc(w, RVSMW_FONTCHARSET div 4);
        {$ENDIF}
        {$IFDEF RVLANGUAGEPROPERTY}
        if Language = FontStyle.Language then
          inc(w, RVSMW_LANGUAGE);
        {$ENDIF}
        inc(w, RV_CompareInts(FontSize, FontStyle.Size, RVSMW_FONTSIZE));
        inc(w, RV_CompareColors(FontStyle.Color, Color, RVSMW_EACHRGBCOLOR, RVSMW_COLORSET));
        inc(w, RV_CompareColors(FontStyle.BackColor, BackColor, RVSMW_EACHRGBBCOLOR, RVSMW_BCOLORSET));
        if (rvfsAllCaps in FontStyle.StyleEx) = (rtf_fs_AllCaps in StyleEx) then
          inc(w, RVSMW_ALLCAPS);
        for fs := Low(TFontStyle) to High(TFontStyle) do
          if (fs in FontStyle.Style) = (fs in Style) then
            inc(w, RVSMW_FONTEACHSTYLE);
        if (FontStyle.Style=[])=(Style=[]) then
          inc(w, RVSMW_FONTSTYLESET);
        inc(w, Round((1 - abs(FontStyle.CharScale-CharScaleX) / 100)*RVSMW_CHARSCALE));
        inc(w, RV_CompareInts(CharSpacing, FontStyle.CharSpacing, RVSMW_FONTSIZE));
        if GetStyleSSType(FontStyle.VShift)=SScriptType then
          inc(w, RVSMW_VSHIFT);
        if FontStyle.StyleEx<>[] then
          w := w div 4;
        if w>maxw then begin
          maxw := w;
          Result := i;
        end;
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.FindStyleNo(AUnicode, AHypertext: Boolean): Integer;
var i: Integer;
    FontStyle: TFontInfo;
    FontSize: Integer;
    FN: String;
    CharSpacing: Integer;
    {$IFDEF RICHVIEWCBDEF3}
    Charset: TFontCharset;
    {$ENDIF}

begin
  Result := -1;
  {$IFDEF RVTEXTFOOTNOTES}
  if Reader.RTFState.CharProps.FootNote<>'' then
    exit;
  {$ENDIF}
  with Reader.RTFState.CharProps do begin
    if Reader.FontTable.Count=0 then begin
      Result := 0;
      exit;
    end;
    if FontName='' then
      FN := AnsiLowerCase(Reader.FontTable[FontIndex].Name)
    else
      FN := AnsiLowerCase(FontName);
    {$IFDEF RICHVIEWCBDEF3}
    Charset := Reader.FontTable[FontIndex].Charset;
    if (FontName='Symbol') or (FontName='Wingdings') then
      Charset := SYMBOL_CHARSET;
    {$ENDIF}
    CharSpacing  := Round(CharSpacingTw*PixelsPerTwip);
    if SScriptType = rtf_ss_Normal then
      FontSize := Size
    else
      FontSize := Round(Size/2);    
    for i := 0 to RVStyle.TextStyles.Count-1 do begin
      FontStyle := RVStyle.TextStyles[i];
      if (AnsiLowerCase(FontStyle.FontName)=FN) and
         (FontStyle.Size=FontSize) and
         (FontStyle.Color=Color) and
         (FontStyle.BackColor=BackColor) and
         (FontStyle.Style=Style) and
         {$IFDEF RVLANGUAGEPROPERTY}
         (FontStyle.Language = Language) and
         {$ENDIF}
         {$IFDEF RICHVIEWCBDEF3}
         ((FontStyle.Charset=Charset)
          or (FontStyle.Charset=DEFAULT_CHARSET)) // <-- not very good solution
          and
         {$ENDIF}
         (FontStyle.CharScale=CharScaleX) and
         (FontStyle.CharSpacing=CharSpacing) and
         ((rvfsAllCaps in FontStyle.StyleEx)=(rtf_fs_AllCaps in StyleEx)) and
         (GetStyleSSType(FontStyle.VShift)=SScriptType) and
         {$IFNDEF RVDONOTUSEUNICODE}
         (FontStyle.Unicode=AUnicode) and
         {$ENDIF}
         (FontStyle.StyleEx = []) and
         ((AHypertext and FontStyle.Jump) or
          (not AHypertext and (UseHypertextStyles or not FontStyle.Jump))) then begin
        Result := i;
        break;
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderProperties.AddStyle(AUnicode, AHypertext: Boolean);
var FontStyle: TFontInfo;
begin
  FontStyle := TFontInfo(RVStyle.TextStyles.Add);
  FontStyle.Standard := False;
  with Reader.RTFState.CharProps do begin

    FontStyle.Jump      := AHypertext;
    {$IFNDEF RVDONOTUSEUNICODE}
    FontStyle.Unicode   := AUnicode;
    {$ENDIF}
    FontStyle.Style     := Style;
    if SScriptType = rtf_ss_Normal then
      FontStyle.Size := Size
    else
      FontStyle.Size := Round(Size/2);
    FontStyle.Color     := Color;
    FontStyle.BackColor := BackColor;
    if FontName='' then
      FontStyle.FontName  := Reader.FontTable[FontIndex].Name
    else begin
      FontStyle.FontName  := FontName;
      // FontStyle.Protection := FontStyle.Protection+[rvprStyleProtect,rvprDoNotAutoSwitch];
    end;
    {$IFDEF RICHVIEWCBDEF3}
    FontStyle.Charset   := Reader.FontTable[FontIndex].Charset;
    if (FontName='Symbol') or (FontName='Wingdings') then
      FontStyle.Charset := SYMBOL_CHARSET;
    {$ENDIF}
    FontStyle.CharScale := CharScaleX;
    FontStyle.CharSpacing := Round(CharSpacingTw*PixelsPerTwip);
    case SScriptType of
      rtf_ss_Normal:
        FontStyle.VShift := 0;
      rtf_ss_Superscript:
        FontStyle.VShift := 45;
      rtf_ss_Subscript:
        FontStyle.VShift := -25;
    end;
    if rtf_fs_AllCaps in StyleEx then
      FontStyle.StyleEx := FontStyle.StyleEx+[rvfsAllCaps];
    {$IFDEF RVLANGUAGEPROPERTY}
    FontStyle.Language := Language;
    {$ENDIF}
    {$IFDEF RVTEXTFOOTNOTES}
    if rtf_fs_FootNote in StyleEX then begin
      FontStyle.StyleEx := FontStyle.StyleEx+[rvfsFootNotes];
      FontStyle.FootNote := FootNote;
    end;    
    {$ENDIF}
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.ReturnStyleNo(AUnicode: Boolean): Integer;
var ht: Boolean;
begin
  case TextStyleMode of
    rvrsUseSpecified:
      Result := TextStyleNo;
    rvrsUseClosest:
      Result := FindBestStyleNo(AUnicode, IsHypertext_);
    else
      begin
        ht := IsHypertext_;
        Result := FindStyleNo(AUnicode, ht);
        if Result<0 then begin
          AddStyle(AUnicode, ht);
          Result := RVStyle.TextStyles.Count-1;
        end;
      end;
  end;
end;
const HYPERLINK = 'HYPERLINK';
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.IsHypertext(var Target, Hint,
  Extras: String): Boolean;

var p: Integer;
    s: String;
    removed, quoted: Boolean;
    i, state: Integer;
begin
  if Reader.RTFState.FieldCode='' then begin
    Result := False;
    exit;
  end;
  s := Trim(Reader.RTFState.FieldCode);
  if s[1]='\' then
    s := Copy(s, 2, Length(s));
  p := Pos(HYPERLINK, UpperCase(s));
  Result := p=1;
  if not Result then
    exit;
  Target := Reader.GetFieldCommandValueEx(s, p);
  if p<=Length(s) then
    Extras := Copy(s, p, Length(s)-p+1)
  else
    Extras := '';
  removed := False;
  for p := Length(Target) downto 2 do begin
    removed := not removed and (Target[p]='\') and (Target[p-1]='\');
    if removed then
      Delete(Target, p, 1);
  end;
  quoted := False;
  state := 0;
  Hint := '';
  for i := 1 to Length(Extras) do
    case state of
    0:
      begin
        if not quoted then begin
          if Extras[i]='\' then
           state := 1
          else if Extras[i]='"' then
            quoted := True;
          end
        else if Extras[i]='"' then
          quoted := False;
      end;
    1:
      if Extras[i]='o' then
        state := 2
      else
        state := 0;
    2:
      if Extras[i]=' ' then
        state := 3
      else
        state := 0;
    3:
      begin
        if Extras[i]='"' then
          quoted := True
        else
          Hint := Hint+Extras[i];
        state := 4;
      end;
    4:
      begin
        if (quoted and (Extras[i]='"')) or
           (not quoted and (Extras[i]=' ')) then
          break;
        Hint := Hint+Extras[i];
      end;
    end;
  if (BasePath<>'') and (Pos(':', Target)=0) then
    Target := BasePath+Target;
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.IsHypertext_: Boolean;
begin
  if Reader.RTFState.FieldCode='' then
    Result := False
  else
    Result := Pos(HYPERLINK, Reader.RTFState.FieldCode)>0;
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderProperties.AllowUseStyle(StyleNo: Integer; TextStyle: Boolean): Boolean;
begin
  Result := True;
  if Assigned(OnAllowUseStyle) then
    OnAllowUseStyle(StyleNo, TextStyle, Result);
end;
{$ENDIF}

end.
