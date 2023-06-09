
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRichViewRVData represents RichView document.   }
{       This is a type of TRichView.RVData.             }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVRVData;

interface

{$I RV_Defs.inc}

uses Windows, Classes, Controls, Graphics, Forms, StdCtrls,
     RVItem, RVBack, RVStyle, CRVData, CRVFData, RVCtrlData, RVScroll,
    {$IFDEF RICHVIEWDEF4}
     ImgList,
    {$ENDIF}
    {$IFNDEF RVDONOTUSELISTS}
     RVMarker,
    {$ENDIF}
    {$IFNDEF RVDONOTUSEDRAGDROP}
     ActiveX, RVDragDrop,
    {$ENDIF}
     RVClasses, RVFuncs
     ;

type
  TRichViewRVData = class (TRVControlData)
  protected
    FRichView: TRVScroller;
    RVFTextStylesMapping, RVFParaStylesMapping, RVFListStyleMapping: TRVIntegerList;
    {$IFNDEF RVDONOTUSELISTS}
    FMarkers: TRVMarkerList;
    procedure DestroyMarkers; override;
    {$ENDIF}
    function GetInplaceEditor: TControl; override;
    procedure DestroyInplaceEditor; override;
    function GetRTFProperties: TPersistent{TRVRTFReaderProperties}; override;
    procedure InitStyleMappings(var PTextStylesMapping,
      PParaStylesMapping, PListStylesMapping: PRVIntegerList); override;
    procedure DoneStyleMappings(PTextStylesMapping,
      PParaStylesMapping, PListStylesMapping: PRVIntegerList); override;
    {$IFNDEF RVDONOTUSEDRAGDROP}
    { Drag & drop: drop from }
    function CanStartDragging: Boolean; override;
    function InitDragging(var DropSource: TRVDropSource;
      var OKEffect: Integer): Boolean; override;
    procedure DoneDragging(FDeleteSelection: Boolean); override;
    {$ENDIF}
    function GetExtraVOffs: Integer;
  public
    {$IFNDEF RVDONOTUSEDRAGDROP}
    FDropSource: TRVDropSource; // object implemeting IDropSource and IDataObject
    {$ENDIF}
    function CanLoadLayout: Boolean; override;
    function GetExtraRTFCode(Area: TRVRTFSaveArea; Obj: TObject;
      Index1, Index2: Integer; InStyleSheet: Boolean): String; override;
    function GetExtraHTMLCode(Area: TRVHTMLSaveArea; CSSVersion: Boolean): String; override;
    function GetParaHTMLCode(RVData: TCustomRVData; ItemNo: Integer;
      ParaStart, CSSVersion: Boolean): String; override;
    function GetChosenRVData: TCustomRVData; override;
    function GetChosenItem: TCustomRVItemInfo; override;
    procedure AssignChosenRVData(RVData: TCustomRVFormattedData;
                                 Item: TCustomRVItemInfo); override;
    procedure SilentReplaceChosenRVData(RVData: TCustomRVFormattedData); override;
    procedure UnassignChosenRVData(RVData: TCustomRVData); override;
    function GetParentControl: TWinControl; override;
    function GetDoInPaletteMode: TRVPaletteAction; override;
    function GetRVPalette: HPALETTE; override;
    function GetRVStyle: TRVStyle; override;
    function GetRVLogPalette: PLogPalette; override;
    function GetURL(id: Integer): String; override;
    procedure ReadHyperlink(const Target, Extras: String; DocFormat: TRVLoadFormat;
      var StyleNo, ItemTag: Integer; var ItemName: String); override;
    procedure WriteHyperlink(id: Integer; RVData: TCustomRVData; ItemNo: Integer;
       SaveFormat: TRVSaveFormat; var Target, Extras: String); override;
    function GetOptions: TRVOptions; override;
    procedure SetOptions(const Value: TRVOptions); override;
    function GetDocProperties: TStringList; override;    
    function GetRVFOptions: TRVFOptions; override;
    procedure SetRVFOptions(const Value: TRVFOptions); override;
    function GetRVFWarnings: TRVFWarnings; override;
    procedure SetRVFWarnings(const Value: TRVFWarnings); override;
    function GetRTFOptions: TRVRTFOptions; override;
    procedure SetRTFOptions(const Value: TRVRTFOptions); override;
    function GetAreaWidth: Integer; override;
    procedure GetOrigin(var ALeft, ATop: Integer); override;
    procedure GetOriginEx(var ALeft, ATop: Integer); override;
    function GetMinTextWidth: Integer; override;
    function GetMaxTextWidth: Integer; override;
    function GetLeftMargin: Integer; override;
    function GetRightMargin: Integer; override;
    function GetTopMargin: Integer; override;
    function GetBottomMargin: Integer; override;
    function GetFlags: TRVFlags; override;
    procedure SetFlags(const Value: TRVFlags); override;
    procedure AdjustVScrollUnits; override;
    procedure SetDocumentAreaSize(Width,Height: Integer; UpdateH: Boolean); override;
    procedure ScrollTo(Y: Integer; Redraw: Boolean); override;
    procedure HScrollTo(X: Integer); override;
    function GetVSmallStep: Integer; override;
    function GetBackground: TRVBackground; override;

    function IsAssignedCopy: Boolean; override;
    function IsAssignedRVMouseDown: Boolean; override;
    function IsAssignedRVMouseUp: Boolean; override;
    function IsAssignedRVRightClick: Boolean; override;
    function IsAssignedJump: Boolean; override;
    function IsAssignedRVDblClick: Boolean; override;
    function IsAssignedCheckpointVisible: Boolean; override;
    function IsAssignedOnProgress: Boolean; override;

    procedure DoProgress(Operation: TRVLongOperation; Stage: TRVProgressStage;
      PercentDone: Byte); override;
    procedure DoCopy; override;
    procedure DoRVMouseMove(id: Integer); override;
    procedure DoRVMouseDown(Button: TMouseButton; Shift: TShiftState;
      ItemNo, X, Y: Integer); override;
    procedure DoRVMouseUp(Button: TMouseButton; Shift: TShiftState;
      ItemNo, X, Y: Integer); override;
    procedure DoRVRightClick(const ClickedWord: String; StyleNo,
      X, Y: Integer); override;
    procedure DoRVDblClick(const ClickedWord: String; StyleNo: Integer); override;
    procedure DoCheckpointVisible(CheckpointData: TCheckpointData); override;

    procedure DoJump(id: Integer); override;
    procedure DoSelect; override;
    function GetNormalCursor: TCursor; override;
    function GetCPEventKind: TCPEventKind; override;
    procedure HTMLSaveImage(RVData: TCustomRVData; ItemNo: Integer;
      const Path: String; BackgroundColor: TColor; var Location: String;
      var DoDefault: Boolean); override;
    procedure SaveImage2(Graphic: TGraphic; SaveFormat: TRVSaveFormat;
      const Path, ImagePrefix: String; var ImageSaveNo: Integer;
      var Location: String; var DoDefault: Boolean); override;
    constructor Create(RichView: TRVScroller); virtual;
    destructor Destroy; override;
    function GetHOffs: Integer; override;
    function GetVOffs: Integer; override;
    function GetCanvas: TCanvas; override;
    function GetWidth: Integer; override;
    function GetHeight: Integer; override;
    function GetColor: TColor; override;
    function SaveComponentToFile(const Path: String; SaveMe: TComponent;
      SaveFormat: TRVSaveFormat): String; override;
    function SaveItemToFile(const Path: String; RVData: TCustomRVData;
      ItemNo: Integer; SaveFormat: TRVSaveFormat; Unicode: Boolean;
      var Text: String): Boolean; override;
    function ImportPicture(const Location: String;
      Width, Height: Integer): TGraphic; override;
    function GetItemHint(RVData: TCustomRVData; ItemNo: Integer): String; override;
    function RVFPictureNeeded(const ItemName: String;
      ItemTag: Integer): TGraphic; override;
    function RVFControlNeeded(const ItemName: String;
      ItemTag: Integer): TControl; override;
    function RVFImageListNeeded(ImageListTag: Integer): TCustomImageList; override;
    function GetDelimiters: String; override;
    function GetRVFTextStylesReadMode: TRVFReaderStyleMode; override;
    function GetRVFParaStylesReadMode: TRVFReaderStyleMode; override;
    function GetBiDiMode: TRVBiDiMode; override;
    procedure ControlAction2(ControlAction: TRVControlAction; ItemNo: Integer;
                            var Control:TControl); override;
    procedure ItemAction(ItemAction: TRVItemAction; Item: TCustomRVItemInfo;
                             var Text: String; RVData: TCustomRVData); override;
    {$IFNDEF RVDONOTUSEDRAGDROP}
    { Drag & drop: drop to }
    function GetDragDropCaretInfo: TRVDragDropCaretInfo; override;
    { Drag & drop: drop from }    
    function IsDragging: Boolean; override;
    {$ENDIF}
    {$IFNDEF RVDONOTUSELISTS}
    function GetMarkers(AllowCreate: Boolean): TRVMarkerList; override;
    {$ENDIF}
    property RichView: TRVScroller read FRichView;
  end;

  TRichViewRVDataClass = class of TRichViewRVData;

implementation
uses RichView;
{============================ TRichViewRVData =================================}
constructor TRichViewRVData.Create(RichView: TRVScroller);
begin
  inherited Create;
  FRichView := RichView;
end;
{------------------------------------------------------------------------------}
destructor TRichViewRVData.Destroy;
begin
  {$IFNDEF RVDONOTUSELISTS}
  FMarkers.Free;
  FMarkers := nil;
  {$ENDIF}
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetDoInPaletteMode: TRVPaletteAction;
begin
  Result := TCustomRichView(FRichView).DoInPaletteMode;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetOptions: TRVOptions;
begin
  Result := TCustomRichView(FRichView).Options;
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.SetOptions(const Value: TRVOptions);
begin
  TCustomRichView(FRichView).Options := Value;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetParentControl: TWinControl;
begin
  Result := TWinControl(FRichView);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetRVFOptions: TRVFOptions;
begin
  Result := TCustomRichView(FRichView).RVFOptions;
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.SetRVFOptions(const Value: TRVFOptions);
begin
  TCustomRichView(FRichView).RVFOptions := Value;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetRTFOptions: TRVRTFOptions;
begin
  Result := TCustomRichView(FRichView).RTFOptions;
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.SetRTFOptions(const Value: TRVRTFOptions);
begin
  TCustomRichView(FRichView).RTFOptions := Value;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetRVFWarnings: TRVFWarnings;
begin
  Result := TCustomRichView(FRichView).RVFWarnings;
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.SetRVFWarnings(const Value: TRVFWarnings);
begin
  TCustomRichView(FRichView).RVFWarnings := Value;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetRVLogPalette: PLogPalette;
begin
  Result := TCustomRichView(FRichView).PRVLogPalette;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetRVPalette: HPALETTE;
begin
  Result := TCustomRichView(FRichView).RVPalette;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetURL(id: Integer): String;
begin
  Result := '';
  if Assigned(TCustomRichView(FRichView).OnURLNeeded) then
    TCustomRichView(FRichView).OnURLNeeded(TCustomRichView(FRichView), id, Result);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.CanLoadLayout: Boolean;
begin
  Result := True;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetExtraRTFCode(Area: TRVRTFSaveArea;
  Obj: TObject; Index1, Index2: Integer; InStyleSheet: Boolean): String;
begin
  Result := '';
  if Assigned(TCustomRichView(FRichView).OnSaveRTFExtra) then
    TCustomRichView(FRichView).OnSaveRTFExtra(TCustomRichView(FRichView),
      Area, Obj, Index1, Index2, InStyleSheet, Result);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetExtraHTMLCode(Area: TRVHTMLSaveArea; CSSVersion: Boolean): String; 
begin
  Result := '';
  if Assigned(TCustomRichView(FRichView).OnSaveHTMLExtra) then
    TCustomRichView(FRichView).OnSaveHTMLExtra(TCustomRichView(FRichView),
      Area, CSSVersion, Result);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetParaHTMLCode(RVData: TCustomRVData;
  ItemNo: Integer; ParaStart, CSSVersion: Boolean): String;
begin
  Result := '';
  if Assigned(TCustomRichView(FRichView).OnSaveParaToHTML) then
    TCustomRichView(FRichView).OnSaveParaToHTML(TCustomRichView(FRichView),
      RVData, ItemNo, ParaStart, CSSVersion, Result);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.ReadHyperlink(const Target, Extras: String;
  DocFormat: TRVLoadFormat; var StyleNo, ItemTag: Integer;
  var ItemName: String);
begin
  case StyleNo of
    rvsPicture:
      StyleNo := rvsHotPicture;
    rvsBullet:
      StyleNo := rvsHotspot;
  end;
  if Assigned(TCustomRichView(FRichView).OnReadHyperlink) then
    TCustomRichView(FRichView).OnReadHyperlink(TCustomRichView(FRichView),
      Target, Extras, DocFormat, StyleNo, ItemTag, ItemName);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.WriteHyperlink(id: Integer; RVData: TCustomRVData;
  ItemNo: Integer; SaveFormat: TRVSaveFormat;
  var Target, Extras: String);
begin
  Target := '';
  Extras := '';
  if rvflRoot in Flags then begin
    {$IFNDEF RVDONOTUSEITEMHINTS}
    RVData.GetItem(ItemNo).GetExtraStrProperty(rvespHint, Extras);
    Extras := RV_GetHintStr(SaveFormat, Extras);
    {$ENDIF}
    if Assigned(TCustomRichView(FRichView).OnWriteHyperlink) then
      TCustomRichView(FRichView).OnWriteHyperlink(TCustomRichView(FRichView),
        id, RVData, ItemNo, SaveFormat, Target, Extras)
    else
      Target := GetURL(id)
    end
  else
    inherited;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.RVFControlNeeded(const ItemName: String; ItemTag: Integer): TControl;
begin
  Result := nil;
  if Assigned(  TCustomRichView(FRichView).OnRVFControlNeeded) then
      TCustomRichView(FRichView).OnRVFControlNeeded(TCustomRichView(FRichView),
                                              ItemName, ItemTag, Result);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.RVFImageListNeeded(ImageListTag: Integer): TCustomImageList;
begin
  Result := nil;
  if Assigned(TCustomRichView(FRichView).OnRVFImageListNeeded) then
    TCustomRichView(FRichView).OnRVFImageListNeeded(TCustomRichView(FRichView),
                                              ImageListTag, Result);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.RVFPictureNeeded(const ItemName: String;
  ItemTag: Integer): TGraphic;
begin
  Result := nil;
  if Assigned(TCustomRichView(FRichView).OnRVFPictureNeeded) then
    TCustomRichView(FRichView).OnRVFPictureNeeded(TCustomRichView(FRichView),
                                            ItemName, ItemTag, Result);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.SaveComponentToFile(const Path: String;
  SaveMe: TComponent; SaveFormat: TRVSaveFormat): String;
begin
  Result := '';
  if Assigned(TCustomRichView(FRichView).OnSaveComponentToFile) then
    TCustomRichView(FRichView).OnSaveComponentToFile(
      TCustomRichView(FRichView), Path, SaveMe, SaveFormat, Result);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.SaveItemToFile(const Path: String; 
  RVData: TCustomRVData; ItemNo: Integer; SaveFormat: TRVSaveFormat;
  Unicode: Boolean; var Text: String): Boolean;
begin
  if Assigned(TCustomRichView(FRichView).OnSaveItemToFile) then begin
    Result := True;
    TCustomRichView(FRichView).OnSaveItemToFile(TCustomRichView(FRichView),
      Path, RVData, ItemNo, SaveFormat, Unicode, Text, Result);
    Result := not Result;
    end
  else
    Result := False;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.ImportPicture(const Location: String; Width,
  Height: Integer): TGraphic;
begin
  Result := nil;
  if Assigned(TCustomRichView(FRichView).OnImportPicture) then
    TCustomRichView(FRichView).OnImportPicture(TCustomRichView(FRichView),
      Location, Width, Height, Result);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetItemHint(RVData: TCustomRVData;
  ItemNo: Integer): String;
begin
  {$IFNDEF RVDONOTUSEITEMHINTS}
  RVData.GetItem(ItemNo).GetExtraStrProperty(rvespHint, Result);
  {$ELSE}
  Result := '';
  {$ENDIF}
  if Assigned(TCustomRichView(FRichView).OnItemHint) then
    TCustomRichView(FRichView).OnItemHint(TCustomRichView(FRichView),
      RVData, ItemNo, Result);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.IsAssignedCopy: Boolean;
begin
  Result := Assigned(TCustomRichView(FRichView).OnCopy);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.DoCopy;
begin
  if Assigned(TCustomRichView(FRichView).OnCopy) then
    TCustomRichView(FRichView).OnCopy(TCustomRichView(FRichView));
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.DoRVMouseMove(id: Integer);
begin
  if Assigned(TCustomRichView(FRichView).OnRVMouseMove) then
    TCustomRichView(FRichView).OnRVMouseMove(TCustomRichView(FRichView),id);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.DoRVMouseDown(Button: TMouseButton;
  Shift: TShiftState; ItemNo, X, Y: Integer);
begin
  if Assigned(TCustomRichView(FRichView).OnRVMouseDown) then
    TCustomRichView(FRichView).OnRVMouseDown(TCustomRichView(FRichView),Button,Shift,ItemNo,X,Y);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.IsAssignedRVMouseUp: Boolean;
begin
  Result := Assigned(TCustomRichView(FRichView).OnRVMouseUp);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.IsAssignedRVMouseDown: Boolean;
begin
  Result := Assigned(TCustomRichView(FRichView).OnRVMouseDown);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.IsAssignedRVRightClick: Boolean;
begin
  Result := Assigned(TCustomRichView(FRichView).OnRVRightClick);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.IsAssignedJump: Boolean;
begin
  Result := Assigned(TCustomRichView(FRichView).OnJump);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.IsAssignedRVDblClick: Boolean;
begin
  Result := Assigned(TCustomRichView(FRichView).OnRVDblClick);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.IsAssignedCheckpointVisible: Boolean;
begin
  Result := Assigned(TCustomRichView(FRichView).OnCheckpointVisible);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.DoRVMouseUp(Button: TMouseButton;
  Shift: TShiftState; ItemNo, X, Y: Integer);
begin
  if IsAssignedRVMouseUp then
    TCustomRichView(FRichView).OnRVMouseUp(TCustomRichView(FRichView),Button,Shift,ItemNo,X,Y);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.DoRVRightClick(const ClickedWord: String;
  StyleNo, X, Y: Integer);
begin
  if Assigned(TCustomRichView(FRichView).OnRVRightClick) then
    TCustomRichView(FRichView).OnRVRightClick(TCustomRichView(FRichView), ClickedWord,
                                        StyleNo, X, Y);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.DoJump(id: Integer);
begin
  if Assigned(TCustomRichView(FRichView).OnJump) then
    TCustomRichView(FRichView).OnJump(TCustomRichView(FRichView), id);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.HTMLSaveImage(RVData: TCustomRVData; ItemNo: Integer;
  const Path: String; BackgroundColor: TColor;
  var Location: String; var DoDefault: Boolean);
begin
  DoDefault := True;
  if Assigned(TCustomRichView(FRichView).OnHTMLSaveImage) then
    TCustomRichView(FRichView).OnHTMLSaveImage(TCustomRichView(FRichView),
      RVData, ItemNo, Path, BackgroundColor, Location, DoDefault);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.SaveImage2(Graphic: TGraphic; SaveFormat: TRVSaveFormat;
  const Path, ImagePrefix: String; var ImageSaveNo: Integer;
  var Location: String; var DoDefault: Boolean); 
begin
  DoDefault := True;
  if Assigned(TCustomRichView(FRichView).OnSaveImage2) then
    TCustomRichView(FRichView).OnSaveImage2(TCustomRichView(FRichView),
      Graphic, SaveFormat, Path, ImagePrefix, ImageSaveNo, Location, DoDefault);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.DoRVDblClick(const ClickedWord: String; StyleNo: Integer);
begin
  if Assigned(TCustomRichView(FRichView).OnRVDblClick) then
    TCustomRichView(FRichView).OnRVDblClick(TCustomRichView(FRichView), ClickedWord, StyleNo);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.DoSelect;
begin
  if Assigned(TCustomRichView(FRichView).OnSelect) and
     not (csDestroying in FRichView.ComponentState) and
     FRichView.HandleAllocated then
    TCustomRichView(FRichView).OnSelect(TCustomRichView(FRichView));
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.DoCheckpointVisible(CheckpointData: TCheckpointData);
begin
  if Assigned(TCustomRichView(FRichView).OnCheckpointVisible) then
    TCustomRichView(FRichView).OnCheckpointVisible(TCustomRichView(FRichView), CheckpointData);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.ControlAction2(ControlAction: TRVControlAction;
  ItemNo: Integer; var Control: TControl);
begin
  if Assigned(TCustomRichView(FRichView).OnControlAction) then
    TCustomRichView(FRichView).OnControlAction(TCustomRichView(FRichView), ControlAction,
                                         ItemNo, Control);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.ItemAction(ItemAction: TRVItemAction; Item: TCustomRVItemInfo;
                                     var Text: String; RVData: TCustomRVData);
begin
  if Assigned(TCustomRichView(FRichView).OnItemAction) then
    TCustomRichView(FRichView).OnItemAction(TCustomRichView(FRichView), ItemAction,
                                         Item, Text, RVData);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.DoProgress(Operation: TRVLongOperation;
  Stage: TRVProgressStage; PercentDone: Byte);
begin
  if Assigned(TCustomRichView(FRichView).OnProgress) then
    TCustomRichView(FRichView).OnProgress(TCustomRichView(FRichView),
      Operation, Stage, PercentDone);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.IsAssignedOnProgress: Boolean;
begin
  Result := Assigned(TCustomRichView(FRichView).OnProgress);
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetWidth: Integer;
begin
  Result := FRichView.ClientWidth;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetHeight: Integer;
begin
  Result := FRichView.ClientHeight;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetAreaWidth: Integer;
begin
  Result := FRichView.AreaWidth;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetFlags: TRVFlags;
begin
  Result := TCustomRichView(FRichView).Flags;
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.SetFlags(const Value: TRVFlags);
begin
  TCustomRichView(FRichView).Flags := Value;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetHOffs: Integer;
begin
  Result := TCustomRichView(FRichView).HScrollPos;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetVOffs: Integer;
begin
  Result := TCustomRichView(FRichView).VScrollPos*TCustomRichView(FRichView).VSmallStep
    -GetExtraVOffs;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetLeftMargin: Integer;
begin
  Result := TCustomRichView(FRichView).LeftMargin;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetRightMargin: Integer;
begin
  Result := TCustomRichView(FRichView).RightMargin;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetBottomMargin: Integer;
begin
  Result := TCustomRichView(FRichView).BottomMargin;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetTopMargin: Integer;
begin
  Result := TCustomRichView(FRichView).TopMargin;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetMaxTextWidth: Integer;
begin
  Result := TCustomRichView(FRichView).MaxTextWidth;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetMinTextWidth: Integer;
begin
  Result := TCustomRichView(FRichView).MinTextWidth;
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.AdjustVScrollUnits;
var v: Integer;
begin
  if  TCustomRichView(FRichView).VScrollVisible and
     (DocumentHeight div TCustomRichView(FRichView).VSmallStep > 32000) then begin
    v := DocumentHeight div 32000;
    if DocumentHeight div v > 32000 then
      inc(v);
    TCustomRichView(FRichView).VSmallStep := v;
    end
  {$IFNDEF DONOTDECVSCROLLUNITS}
  else if (TCustomRichView(FRichView).VSmallStep>10) and
          (DocumentHeight div 10 <= 32000) then
    TCustomRichView(FRichView).VSmallStep := 10;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.SetDocumentAreaSize(Width, Height: Integer;
                                              UpdateH: Boolean);
var h: Integer;
begin
  //Width := MulDiv(Width,ZoomPercent,100);
  //Height := MulDiv(Height,ZoomPercent,100);
  {$IFDEF RVDEBUG}{$I Debug\c.inc}{$ENDIF}
  h := Height div TCustomRichView(FRichView).VSmallStep;
  if (Height mod TCustomRichView(FRichView).VSmallStep)>0 then
    inc(h);
  FRichView.UpdateScrollBars(Width, h, UpdateH,True);
  {$IFDEF RV_ODHC}
  if Assigned(TCustomRichView(FRichView).OnDocumentHeightChange) then
    TCustomRichView(FRichView).OnDocumentHeightChange(FRichView);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.ScrollTo(Y: Integer; Redraw: Boolean);
begin
  if rvstNoScroll in State then
    exit;
  if Redraw then
    FRichView.ScrollTo(Y)
  else
    FRichView.ScrollToNoRedraw(Y)
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.HScrollTo(X: Integer);
begin
  if rvstNoScroll in State then
    exit;
  TCustomRichView(FRichView).HScrollPos := X;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetVSmallStep: Integer;
begin
  Result := TCustomRichView(FRichView).VSmallStep;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetBackground: TRVBackground;
begin
  Result := TCustomRichView(FRichView).Background;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetColor: TColor;
begin
  if TCustomRichView(FRichView).Color<>clNone then
    Result := TCustomRichView(FRichView).Color
  else
    Result :=  TCustomRichView(FRichView).Style.Color;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetNormalCursor: TCursor;
begin
  Result := TCustomRichView(FRichView).Cursor;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetDelimiters: String;
begin
  Result := TCustomRichView(FRichView).Delimiters;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetCPEventKind: TCPEventKind;
begin
  Result := TCustomRichView(FRichView).CPEventKind;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetCanvas: TCanvas;
begin
  Result := TCustomRichView(FRichView).Canvas;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetRVStyle: TRVStyle;
begin
  Result := TCustomRichView(FRichView).Style;
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.AssignChosenRVData(RVData: TCustomRVFormattedData;
                                             Item: TCustomRVItemInfo);
begin
  FRichView.AssignChosenRVData(RVData, Item);
  inherited AssignChosenRVData(RVData, Item);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.SilentReplaceChosenRVData(
  RVData: TCustomRVFormattedData);
begin
  FRichView.SilentReplaceChosenRVData(RVData);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.UnassignChosenRVData(RVData: TCustomRVData);
begin
  if rvstUnAssigningChosen in State then
    exit;
  State := State+[rvstUnAssigningChosen];
  try
    inherited UnassignChosenRVData(RVData);
    FRichView.UnassignChosenRVData(RVData);
  finally
    State := State-[rvstUnAssigningChosen]
  end;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetInplaceEditor: TControl;
begin
  Result := TCustomRichView(FRichView).InplaceEditor;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetChosenRVData: TCustomRVData;
begin
  Result := TCustomRichView(FRichView).ChosenRVData as TCustomRVData;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetChosenItem: TCustomRVItemInfo;
begin
  Result := TCustomRichView(FRichView).ChosenItem as TCustomRVItemInfo;
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.DestroyInplaceEditor;
begin
  FRichView.DestroyInplace;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetRTFProperties: TPersistent;
begin
  Result := TCustomRichView(FRichView).RTFReadProperties
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetRVFParaStylesReadMode: TRVFReaderStyleMode;
begin
  Result := TCustomRichView(FRichView).RVFParaStylesReadMode;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetRVFTextStylesReadMode: TRVFReaderStyleMode;
begin
  Result := TCustomRichView(FRichView).RVFTextStylesReadMode;
end;
{------------------------------------------------------------------------------}
function TRichViewRVData.GetBiDiMode: TRVBiDiMode;
begin
  Result := TCustomRichView(FRichView).BiDiMode;
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.InitStyleMappings(var PTextStylesMapping,
  PParaStylesMapping, PListStylesMapping: PRVIntegerList);
begin
  PTextStylesMapping := @RVFTextStylesMapping;
  PParaStylesMapping := @RVFParaStylesMapping;
  PListStylesMapping := @RVFListStyleMapping;
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.DoneStyleMappings(PTextStylesMapping,
  PParaStylesMapping, PListStylesMapping: PRVIntegerList);
begin
  (PTextStylesMapping^).Free;
  (PParaStylesMapping^).Free;
  (PListStylesMapping^).Free;  
  PTextStylesMapping^ := nil;
  PParaStylesMapping^ := nil;
  PListStylesMapping^ := nil;  
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSELISTS}
function TRichViewRVData.GetMarkers(AllowCreate: Boolean): TRVMarkerList;
begin
  if rvflRoot in Flags then begin
    if (FMarkers=nil) and AllowCreate then begin
      FMarkers := TRVMarkerList.Create;
      FMarkers.PrevMarkerList := GetPrevMarkers;
    end;
    Result := FMarkers;
    end
  else
    Result := inherited GetMarkers(AllowCreate);
end;
{------------------------------------------------------------------------------}
procedure TRichViewRVData.DestroyMarkers;
begin
  if rvflRoot in Flags then begin
    FMarkers.Free;
    FMarkers := nil;
    end
  else
    inherited DestroyMarkers;
end;
{$ENDIF}

function TRichViewRVData.GetDocProperties: TStringList;
begin
  if rvflRoot in Flags then
    Result := TCustomRichView(RichView).DocProperties
  else
    Result := nil;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEDRAGDROP}
{------------------------------------------------------------------------------}
{ Drag&Drop: IDropTarget related                                               }
{------------------------------------------------------------------------------}
{ Returns information about drag&drop caret location.
  D&d caret is not allowed in TRichView.
  This method is overriden in TRVEditRVData.                                   }
function TRichViewRVData.GetDragDropCaretInfo: TRVDragDropCaretInfo;
begin
  Result := nil;
end;
{------------------------------------------------------------------------------}
{ Drag&Drop: IDropSource related                                               }
{------------------------------------------------------------------------------}
{ Can dragging from this TRichView be started?                                 } 
function TRichViewRVData.CanStartDragging: Boolean;
begin
  if not (rvflRoot in Flags) then begin
    Result := inherited CanStartDragging;
    exit;
  end;
  if rvoDisallowDrag in GetOptions then
    Result := False
  else
    Result := FDropSource=nil;
end;
{------------------------------------------------------------------------------}
{ Is dragging from this TRichView in process? Overriden in TRichViewRVData.    }
function TRichViewRVData.IsDragging: Boolean;
begin
  if not (rvflRoot in Flags) then
    Result := inherited IsDragging
  else
    Result := FDropSource<>nil;
end;
{------------------------------------------------------------------------------}
{ Initializing dragging. Overriden in TRVEditRVData.
  Returns True on success.
  Returns DropSource and OKEffect for call of DoDragDrop.                      }
function TRichViewRVData.InitDragging(var DropSource: TRVDropSource;
  var OKEffect: Integer): Boolean;
begin
  if not (rvflRoot in Flags) then begin
    Result := inherited InitDragging(DropSource, OKEffect);
    exit;
  end;
  if rvoDisallowDrag in GetOptions then
    Result := False
  else begin
    Result := FDropSource=nil;
    if Result then begin
      FDropSource := TRVDropSource.Create(FRichView);
      OKEffect := DROPEFFECT_COPY;
    end;
    DropSource := FDropSource;
  end;
end;
{------------------------------------------------------------------------------}
{ Finalizing dragging.
  Note: if FDropSource<>nil, this function loses the object.                   }
procedure TRichViewRVData.DoneDragging(FDeleteSelection: Boolean);
begin
  FDropSource := nil;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRichViewRVData.GetExtraVOffs: Integer;
begin
  {$IFDEF RICHVIEWDEF3}
  case TCustomRichView(RichView).VAlign of
    tlBottom:
      begin
        Result := GetHeight-DocumentHeight;
        if Result<0 then
          Result := 0;
      end;
    tlCenter:
      begin
        Result := (GetHeight-DocumentHeight) div 2;
        if Result<0 then
          Result := 0;
      end;
    else
      Result := 0;
  end;
  {$ELSE}
  Result := 0;
  {$ENDIF}
end;

procedure TRichViewRVData.GetOrigin(var ALeft, ATop: Integer);
begin
  ALeft := 0;
  ATop := GetExtraVOffs;
end;

procedure TRichViewRVData.GetOriginEx(var ALeft, ATop: Integer);
begin
  ALeft := 0;
  ATop := GetExtraVOffs;
end;

end.
