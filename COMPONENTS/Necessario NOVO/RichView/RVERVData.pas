
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRichViewRVData represents RichViewEdit         }
{       document.                                       }
{       This is a type of TRichViewEdit.RVData.         }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVERVData;

{$I RV_Defs.inc}
interface
uses SysUtils, Windows, Graphics, Classes, Controls, Forms,
     CRVData, CRVFData, RVRVData, RVItem, RVUndo, RVBack,
     DLines, RVFuncs, RVEdit, RVUni,
     RVScroll, RichView, RVStyle, ComCtrls, TypInfo, RVClasses,
     RVRTFProps, RVResize,
     {$IFNDEF RVDONOTUSEDRAGDROP}
     ActiveX, RVDragDrop,
     {$ENDIF}
     RVRTFErr;

type
  TRVCharPos = class (TCollectionItem)
    public
      X, DrawItemNo, Offset, MoveRightTo: Integer;
      constructor Create(Owner: TCollection); override;
      procedure Assign(Source: TPersistent); override;
  end;

  TRVCharPosCollection = class (TCollection)
    private
      function GetItem(Index: Integer): TRVCharPos;
      procedure SetItem(Index: Integer; const Value: TRVCharPos);
    public
      property Items[Index: Integer]: TRVCharPos read GetItem write SetItem;
  end;

  TRVParaListOperation = (rvplopChange, rvplopRemove, rvplopLevel);

  TRVUndoDest = (udNone, udUndo, udRedo);

  TRVEditRVData = class (TRichViewRVData)
    private
      FPrevTextStyleNo: Integer;
      FCurTag: Integer;
      FResizer: TRVItemResizer;
      ResizingControl: TControl;
      FRVFInserted: Boolean;
      function InsSomething(var info: TCustomRVItemInfo; var s: String;
        AutoTag: Boolean; var InsertPoint, ItemsAdded: Integer;
        var FullReformat: Boolean; var NewListNo: Integer): Boolean;
      function InsEmptyString(Index, Tag, ParaStyle, FontStyle: Integer;
        SameAsPrev, BR: Boolean):TCustomRVItemInfo;
      function InsString(s: String; Index, Tag, ParaStyle, FontStyle: Integer;
        SameAsPrev, BR: Boolean; var FullReformat: Boolean): TCustomRVItemInfo;
      function InsString2(s: String; Index, Tag: Integer;
        Source: TCustomRVItemInfo; SameAsPrev, BR: Boolean;
        var FullReformat: Boolean): TCustomRVItemInfo;
      function CreateTextItem(Tag, ParaStyle, FontStyle: Integer;
        SameAsPrev, BR: Boolean): TCustomRVItemInfo;
      function CreateTextItem2(Tag: Integer; Source: TCustomRVItemInfo;
        SameAsPrev, BR: Boolean): TCustomRVItemInfo;
      procedure InsertString(var s: String; StyleNo: Integer; AutoTag,
        CaretBefore: Boolean);
      procedure DoResizeControl(ItemNo, OldWidth, OldHeight,
        NewWidth, NewHeight: Integer);

      function GetParaEndItemNo(ItemNo: Integer): Integer;
      procedure SetParaStyle(StartItemNo,EndItemNo, ParaNo: Integer;
        var FullReformat: Boolean);
      procedure AfterAddingText(StartItemNo,EndItemNo, ItemsAdded,
        DIStartNo, DIEndNo: Integer; FullReformat, CaretBefore: Boolean);
      function ItemHasPersistentCheckpoint(ItemNo: Integer): Boolean;
      function ParaHasPersistentCheckpoint(ItemNo: Integer): Boolean;
      function MovePersistentCheckpoint(ItemNo: Integer; OnlyToPrev: Boolean): Boolean;
      procedure ClearCurTag;
      function GetOneSelectedItemNo: Integer;
    protected
      CaretHeight: Integer;
      CharEnds: TRVCharPosCollection;
      procedure DeselectPartiallySelectedItem(NewPartiallySelected: TCustomRVItemInfo); override;
      procedure SetPartialSelectedItem(Item: TCustomRVItemInfo); override;
      procedure AfterDeleteStyles(TextStylesShift, ParaStylesShift, ListStylesShift: TRVIntegerList); override;
      {$IFNDEF RVDONOTUSELISTS}
      function ReplicateMarker(ReferenceItemNo, InsertItemNo: Integer;
        var FullReformat: Boolean; EditFlag: Boolean): Boolean;
      procedure AdjustMarkerCaret(Right: Boolean; var Offs: Integer);
      procedure AdjustMarkerPos(var ItemNo, Offs: Integer; DefRight: Boolean);
      {$ENDIF}
      function CaretAtTheBeginningOfParaSection: Boolean;
      function CaretAtTheEndOfParaSection: Boolean;
      function CaretInTheLastLine: Boolean;
      function CaretAtTheBeginningOfLine: Boolean;
      function CaretAtTheEndOfLine: Boolean;
      procedure PostPaintTo(Canvas: TCanvas;
        XShift, YShift, FirstDrawItemNo, LastDrawItemNo: Integer); override;
      function GetResizeHandleAt(X, Y: Integer; var Index: TRVResizeHandleIndex): Boolean;
      {$IFNDEF RVDONOTUSEDRAGDROP}
      { Drag & drop: drop from }
      function InitDragging(var DropSource: TRVDropSource;
        var OKEffect: Integer): Boolean; override;
      procedure DoneDragging(FDeleteSelection: Boolean); override;
      {$ENDIF}
    public
      {$IFNDEF RVDONOTUSEDRAGDROP}
      FDragDropCaretInfo: TRVDragDropCaretInfo; // info about drag&drop caret location
      {$ENDIF}
      FCurTextStyleNo, FCurParaStyleNo: Integer;
      UndoList, RedoList: TRVUndoList;
      UndoDest: TRVUndoDest;
      CaretDrawItemNo: Integer;
      CaretOffs: Integer;
      {$IFNDEF RVDONOTUSEDRAGDROP}
      procedure CreateDragDropCaretInfo;
      procedure ReleaseDragDropCaretInfo;
      function GetDragDropCaretInfo: TRVDragDropCaretInfo; override;
      {$ENDIF}
      procedure AssignCurTag;      
      function GetActualCurStyleNo: Integer;
      procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
      procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
      procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);override;
      function CancelResize: Boolean;
      procedure AdjustMouseUpSelection; override;
      procedure ResizeItem(ItemNo, Width, Height: Integer);
      procedure XorDrawing; override;
      procedure MarkStylesInUse(UsedTextStyles, UsedParaStyles, UsedListStyles: TRVIntegerList); override;
      function InsertFirstRVFItem(var Index: Integer; var s: String;
        var li: TCustomRVItemInfo; EditFlag: Boolean; var FullReformat: Boolean;
        var NewListNo: Integer): Boolean; override;
      function GetUndoList: TRVUndoList;
      procedure Change;
      procedure ChangeEx(ClearRedo: Boolean);
      procedure DoOnSelection(AllowScrolling: Boolean); override;
      procedure CreateResizer;
      procedure UpdateResizer;
      procedure Do_ReformateRange(StartNo, EndNo: Integer; SuperReformat: Boolean);
      procedure Do_DeleteItem(ItemNo: Integer; var FullReformat: Boolean);
      procedure Do_InsertItem(ItemNo: Integer; var s: String; item: TCustomRVItemInfo; FromUndo: Boolean;
                              var FullReformat: Boolean);
      procedure Do_ReplaceItem(ItemNo: Integer; item: TCustomRVItemInfo);
      procedure Do_DeleteItems(StartItemNo, EndItemNo: Integer; var FullReformat: Boolean);
      procedure Do_InsertItems(ItemNo: Integer; sl: TStringList; FromUndo: Boolean;
                               var FullReformat: Boolean);
      procedure Do_ModifyItem(ItemNo: Integer; const s: String; Item: TCustomRVItemInfo);
      function Do_InsertItems_1(ItemNo,Count: Integer): TRVUndoInsertItemsInfo;
      procedure Do_InsertItems_2(ItemNo,Count: Integer; ui: TRVUndoInsertItemsInfo;
                                 var FullReformat: Boolean);
      procedure Do_DeleteSubstring(ItemNo, Index, ALength: Integer);
      procedure Do_InsertSubstring(ItemNo, Index: Integer; const s: String);
      procedure Do_NewLine(ItemNo: Integer; SameAsPrev: Boolean; ParaNo: Integer;
                           var FullReformat: Boolean);
      procedure Do_BR(ItemNo: Integer; BR: Boolean; var FullReformat: Boolean);
      procedure Do_PageBreak(ItemNo: Integer; PageBreak: Boolean);
      procedure Do_ExtraIntProperty(ItemNo: Integer; Prop: TRVExtraItemProperty;
        Value: Integer);
      procedure Do_ExtraStrProperty(ItemNo: Integer; Prop: TRVExtraItemStrProperty;
        const Value: String);
      procedure Do_Concate(FirstItemNo: Integer);
      procedure Do_MoveCP(SrcItemNo, DestItemNo: Integer);
      procedure Do_Para(FirstItemNo, EndItemNo, ParaNo: Integer; var FullReformat: Boolean);
      procedure Do_ParaList(FirstItemNo: Integer; ParaList: TRVIntegerList; var FullReformat: Boolean);
      procedure Do_StyleNo(ItemNo, StyleNo: Integer);
      procedure Do_Tag(ItemNo, Tag: Integer; AssignAsIs: Boolean);
      procedure Do_AddCP(ItemNo: Integer; Checkpoint: TRVCPInfo);
      procedure Do_DeleteCP(ItemNo: Integer);
      procedure Do_ChangeText(ItemNo: Integer; const s: String);
      procedure Do_ChangeVAlign(ItemNo: Integer; VAlign: TRVVAlign);
      procedure Do_Resize(ItemNo: Integer; Width, Height: Integer;Reformat: Boolean);
      procedure Do_ItemModifyTerminator(ItemNo: Integer; Opening: Boolean);
      function Do_ModifyItemIntProperty(ItemNo: Integer;
                                         SubObject: TObject;
                                         const PropertyName: String;
                                         Value:  LongInt;
                                         AffectSize, AffectWidth: Boolean;
                                         UndoInfoClass: TRVUndoInfoClass):TRVUndoModifyItemProps;
      function Do_ModifyItemIntProperties(ItemNo: Integer;
                                         SubObject: TObject;
                                         PropList: TStringList;
                                         AffectSize, AffectWidth: Boolean;
                                         UndoInfoClass: TRVUndoInfoClass):TRVUndoModifyItemProps;

      procedure BeginUndoSequence(UndoType: TRVUndoType; AllowFinalize: Boolean);
      procedure SetUndoGroupMode(GroupUndo: Boolean);
      procedure FinalizeUndoGroup;
      procedure BeginNamedUndoSequence(UndoType: TRVUndoType; const Caption: String;
        AllowFinalize: Boolean);
      procedure EndUndoSequence;
      procedure BeginRedoSequence(UndoType: TRVUndoType; const Caption: String);

      procedure GetSelectionBoundsEx(var StartItemNo, StartItemOffs, EndItemNo,
                 EndItemOffs: Integer; Normalize: Boolean); override;
      procedure Clear; override;
      procedure PrepareForEdit;
      function DeleteSelection_: Boolean;
      function  CanDelete: Boolean;
      procedure InsertTextTyping(text: String);
      procedure InsertText_(const text: String; AutoTag, CaretBefore: Boolean);
      procedure InsertTextW_(const text: String; AutoTag, CaretBefore: Boolean);
      function OnEnterPress_(Shift: Boolean; Recursive: Boolean): Boolean;
      procedure OnDeletePress_(Ctrl: Boolean; MovedFromLineEnd: Boolean);
      procedure OnBackSpacePress_(Ctrl: Boolean; MultiDelete, FromNextLine: Boolean);
      procedure SetCurTextStyleNo(Value: Integer);
      procedure SetCurParaStyleNo(Value: Integer);
      procedure ClearTemporal; override;
      procedure ApplyParaStyle(ParaStyleNo: Integer; UseConversion: Boolean);
      function  OnHomePress(Ctrl: Boolean): Boolean;
      function  OnDownPress(Shift, Ctrl: Boolean): Boolean;
      function  OnEndPress(Ctrl: Boolean): Boolean;
      function  OnLeftPress(Shift, Ctrl: Boolean): Boolean;
      function  OnPgDownPress: Boolean;
      function  OnPgUpPress: Boolean;
      function  OnRightPress(Shift, Ctrl: Boolean): Boolean;
      function  OnUpPress(Shift,Ctrl: Boolean): Boolean;
      procedure MoveCaretToTheBeginningOfThePrevParagraph;
      procedure MoveCaretToTheEndOfTheNextParagraph;
      procedure ChangeCaret(ForceCreate, ScrollToCaret, DontChangeStyle, RefreshBefore: Boolean);
      function BuildJumpsCoords(IgnoreReadOnly: Boolean): Integer; dynamic;
      procedure ClearJumpsCoords; dynamic;
      procedure Format_(OnlyResized,ForceFormat,NoScroll: Boolean; depth: Integer; Canvas: TCanvas; OnlyTail, NoCaching: Boolean); override;
      procedure GetSelStart(var DINo, DIOffs: Integer);override;
      procedure SrchSelectIt(strt, offs, len: Integer; Invert: Boolean);override;
      procedure SrchStart(Down: Boolean; var strt, offs: Integer);override;
      function GetCurItemNo: Integer;
      function GetOffsetInCurItem: Integer;
      function InsertSomething(info: TCustomRVItemInfo; var s: String;
        AutoTag, CaretBefore: Boolean): Boolean;
      function NotFormatted: Boolean;
      procedure StartShiftMoving;
      procedure EndShiftMoving;
      procedure ApplyStyleConversion_(UserData: Integer);
      function InsertRVFFromStreamEd_(Stream: TStream): Boolean;
      procedure OnChangeCaretLine(DLOffs: Integer);
      procedure ConcateAfterAdding(var InsertPoint, LastInserted, ItemsAdded, Offs: Integer);

      {$IFNDEF RVDONOTUSERTFIMPORT}
      function InsertRTFFromStreamEd_(Stream: TStream): Boolean;
      {$ENDIF}

      {$IFNDEF RVDONOTUSERTF}
      function SaveRTFToStream(Stream: TStream; SelectionOnly: Boolean;
        Level: Integer; Color: TColor; Background: TRVBackground; ColorList: TRVColorList;
        StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
        FontTable: TRVRTFFontTable; tpp: Double):Boolean; override;
      {$ENDIF}

      function InsertTextFromFile(const FileName: String; OEM, AutoTag: Boolean): Boolean;
      function InsertTextFromFileW(const FileName: String; AutoTag: Boolean): Boolean;
      procedure KeyPress(var Key: Char);
      procedure AdjustControlPlacement(ItemNo: Integer);
      procedure ResizeControl(ItemNo, NewWidth, NewHeight: Integer; Reformat: Boolean);
      procedure Reformat(FullFormat,ForceFormat, NoScroll: Boolean; ItemNo: Integer; UpdateView:  Boolean);
      procedure Reformat_(FullFormat: Boolean; StartDrawItem, EndDrawItem, ItemsAdded: Integer);
      procedure BeginItemModify(ItemNo: Integer; var ModifyData: Integer);
      procedure EndItemModify(ItemNo: Integer; ModifyData: Integer);
      procedure SelectCurrentWord;
      procedure InsertPageBreak;

      {$IFNDEF RVDONOTUSEHTML}
      function SaveHTMLToStreamEx(Stream: TStream;
        const Path, Title, ImagesPrefix, ExtraStyles, ExternalCSS, CPPrefix: String;
        Options: TRVSaveOptions; Color: TColor; var CurrentFileColor: TColor;
        var imgSaveNo: Integer;
        LeftMargin, TopMargin, RightMargin, BottomMargin: Integer;
        Background: TRVBackground; Bullets: TRVList): Boolean; override;
      function SaveHTMLToStream(Stream: TStream; const Path, Title,
        ImagesPrefix: String; Options: TRVSaveOptions; Color: TColor;
        var imgSaveNo: Integer;
         LeftMargin, TopMargin, RightMargin, BottomMargin: Integer;
         Background: TRVBackground; Bullets: TRVList): Boolean; override;
      {$ENDIF}
      function GetIMEWinCoord: TPoint;
      {$IFNDEF RVDONOTUSELISTS}
      procedure UpdateAfterMarkers(FirstItemNo, LastMarkerIndex: Integer;
        ListNos: TRVIntegerList; ListNo: Integer);
      procedure PrepareForUpdateRangeAfterMarkers(StartNo, EndNo: Integer;
        ForDeletion: Boolean;
        var FirstItemNo, LastMarkerIndex: Integer; var ListNos: TRVIntegerList);
      procedure UpdateRangeAfterMarkers(StartNo, EndNo: Integer);
      procedure ApplyListStyle_(AListNo, AListLevel, AStartFrom: Integer;
        AUseStartFrom, ARecursive: Boolean; Operation: TRVParaListOperation;
        var ItemsAdded, StartNo, EndNo, SelStartNo, SelEndNo: Integer;
        ListNos: TRVIntegerList; var LastVWMarkerIndex: Integer);
      procedure ApplyListStyle(AListNo, AListLevel, AStartFrom: Integer;
        AUseStartFrom, ARecursive: Boolean; Operation: TRVParaListOperation);
      {$ENDIF}

      constructor Create(RichView: TRVScroller); override;
      destructor Destroy; override;
  end;

const RichViewEditCaretWidth: Integer = 1;
      RichViewEditCaretHeightExtra: Integer = 0;
      RichViewEditMaxCaretHeight: Integer = 1000;
      RichViewEditDefaultProportionalResize: Boolean = True;

implementation
uses RVStr
     {$IFNDEF RVDONOTUSELISTS}
     , RVMarker
     {$ENDIF}
      ;
{==============================================================================}
function RV_CreateTextCopy(li: TRVTextItemInfo;
                           RVData: TCustomRVData): TRVTextItemInfo;
begin
  Result := RichViewTextItemClass.Create(RVData);
  with Result do begin
    Assign(li);
    ItemOptions := [];  
    SameAsPrev := li.SameAsPrev;
    {$IFNDEF RVDONOTUSEUNICODE}
    if rvioUnicode in li.ItemOptions then
      Include(ItemOptions,rvioUnicode);
    {$ENDIF}
    Tag := RV_CopyTag(li.Tag, rvoTagsArePChars in RVData.Options);
    Checkpoint := nil;
  end;
end;
{================================== TRVCharPos ================================}
constructor TRVCharPos.Create(Owner: TCollection);
begin
  inherited Create(Owner);
  X := 0;
  DrawItemNo := -1;
  Offset := -1;
end;
{------------------------------------------------------------------------------}
procedure TRVCharPos.Assign(Source: TPersistent);
begin
  if Source is TRVCharPos then begin
    X          := TRVCharPos(Source).X;
    DrawItemNo := TRVCharPos(Source).DrawItemNo;
    Offset     := TRVCharPos(Source).Offset;
  end;
end;
{============================= TRVCharPosCollection ===========================}
function TRVCharPosCollection.GetItem(Index: Integer): TRVCharPos;
begin
  Result := TRVCharPos(inherited GetItem(Index));
end;
{------------------------------------------------------------------------------}
procedure TRVCharPosCollection.SetItem(Index: Integer;
  const Value: TRVCharPos);
begin
  inherited SetItem(Index, Value);
end;
{================================= TRVEditRVData ==============================}
constructor TRVEditRVData.Create(RichView: TRVScroller);
begin
  inherited Create(RichView);
  CharEnds := TRVCharPosCollection.Create(TRVCharPos);
  CaretDrawItemNo  := -1;
  CaretOffs        := -1;
  CaretHeight      := 0;
  FCurTextStyleNo  := 0;
  FCurParaStyleNo  := 0;
  FPrevTextStyleNo := 0;
  UndoList         := TRVUndoList.Create(TCustomRVFormattedData(Self.GetAbsoluteRootData));
  RedoList         := TRVUndoList.Create(TCustomRVFormattedData(Self.GetAbsoluteRootData));
  TabNavigation    := rvtnNone;
end;
{------------------------------------------------------------------------------}
destructor TRVEditRVData.Destroy;
begin
  CharEnds.Free;
  UndoList.Free;
  RedoList.Free;
  CharEnds := nil;
  UndoList := nil;
  RedoList := nil;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.ClearTemporal;
begin
  if CharEnds<>nil then
    CharEnds.Clear;
  CaretDrawItemNo := -1;
  CaretOffs   := -1;
  FResizer.Free;
  FResizer := nil;
  ClearCurTag;
  Exclude(State, rvstDoNotClearCurTag);
  inherited ClearTemporal;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Clear;
begin
  inherited Clear;
  if RichView<>nil then
    TCustomRichViewEdit(RichView).Modified := False;
  if UndoList<>nil then
    UndoList.Clear;
  if RedoList<>nil then
    RedoList.Clear;
  FPrevTextStyleNo := 0;
  SetCurParaStyleNo(0);
  SetCurTextStyleNo(0);
  Include(State, rvstEditorUnformatted);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.GetUndoList: TRVUndoList;
begin
  case UndoDest of
    udUndo:
      begin
        if UndoList.Limit<>0 then
          Result := UndoList
        else
          Result := nil;
      end;
    udRedo:
      Result := RedoList;
    else
      Result := nil;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.BeginRedoSequence(UndoType: TRVUndoType; const Caption: String);
begin
  if RedoList.BeginItem(UndoType, Caption, GetCurItemNo, GetOffsetInCurItem) then
    UndoDest := udRedo
  else
    UndoDest := udNone;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.FinalizeUndoGroup;
begin
  if not (rvstFinalizingUndo in State) then begin
    State := State + [rvstFinalizingUndo];
    try
      if FPartialSelectedItem<>nil then
        FPartialSelectedItem.FinalizeUndoGroup
      else if GetChosenItem<>nil then
        GetChosenItem.FinalizeUndoGroup;
    finally
      State := State - [rvstFinalizingUndo];
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.BeginUndoSequence(UndoType: TRVUndoType; AllowFinalize: Boolean);
begin
  if AllowFinalize then
    FinalizeUndoGroup;
  if UndoList.BeginItem(UndoType, '', GetCurItemNo, GetOffsetInCurItem) then
    UndoDest := udUndo
  else
    UndoDest := udNone;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.BeginNamedUndoSequence(UndoType: TRVUndoType; const Caption: String;
                                               AllowFinalize: Boolean);
begin
  if AllowFinalize then
    FinalizeUndoGroup;
  if UndoList.BeginItem(UndoType, Caption, GetCurItemNo, GetOffsetInCurItem) then
    UndoDest := udUndo
  else
    UndoDest := udNone;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.SetUndoGroupMode(GroupUndo: Boolean);
begin
  if GroupUndo then
    inc(UndoList.GroupModeCount)
  else
    dec(UndoList.GroupModeCount);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.EndUndoSequence;
begin
  UndoList.EndItem;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.GetParaEndItemNo(ItemNo: Integer): Integer;
begin
  Result := ItemNo+1;
  while Result<Items.Count do begin
    if GetItem(Result).CanBeBorderStart then break;
    inc(Result)
  end;
  dec(Result);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.SetParaStyle(StartItemNo, EndItemNo, ParaNo: Integer;
                                     var FullReformat: Boolean);
var i: Integer;
    NewWidth, OldWidth: Integer;
    item : TCustomRVItemInfo;
begin
  OldWidth := CalculateParaSectionsMinWidthDef(StartItemNo, EndItemNo);
  for i := StartItemNo to EndItemNo do begin
    item := GetItem(i);
    if not (rvpaoStyleProtect in GetRVStyle.ParaStyles[item.ParaNo].Options) or
       ((i>0) and (item.SameAsPrev or item.BR) and (GetItemPara(i-1)=ParaNo))
     then
      item.ParaNo := ParaNo;
  end;
  NewWidth := CalculateParaSectionsMinWidthDef(StartItemNo, EndItemNo);
  FullReformat := {(OldWidth<>NewWidth) and}
                  ((NewWidth>DocumentWidth) or
                   (OldWidth>=DocumentWidth));
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_ItemModifyTerminator(ItemNo: Integer; Opening: Boolean);
var ui: TRVUndoModifyItemTerminator;
    List: TRVUndoList;
begin
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoModifyItemTerminator.Create;
    ui.ItemNo := ItemNo;
    ui.Opening := Opening;
    List.AddInfo(ui);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_ReformateRange(StartNo, EndNo: Integer;
  SuperReformat: Boolean);
var ui: TRVUndoReformateRange;
    List: TRVUndoList;
begin
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoReformateRange.Create;
    ui.Action := rvuMisc;
    ui.ItemNo := StartNo;
    ui.LastAffectedItemNo := EndNo;
    ui.SuperReformat := SuperReformat;
    List.AddInfo(ui);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_DeleteItem(ItemNo: Integer; var FullReformat: Boolean);
var ui: TRVUndoDeleteItemInfo;
    item: TCustomRVItemInfo;
    List: TRVUndoList;
    LastAffectedItemNo: Integer;
    FR: Boolean;
begin
  item := GetItem(ItemNo);
  FullReformat := CalculateMinItemWidthPlusEx(ItemNo)=DocumentWidth;
  if item.Checkpoint<>nil then
    Do_DeleteCP(ItemNo);
  if False and (ItemNo>0) and item.CanBeBorderStart then
    LastAffectedItemNo := GetParaEndItemNo(ItemNo)
  else
    LastAffectedItemNo := -1;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoDeleteItemInfo.Create;
    ui.Action := rvuDeleteItem;
    ui.Item   := item;
    //!!ui.RVData := Self;
    ui.ItemNo := ItemNo;
    ui.LastAffectedItemNo := LastAffectedItemNo;
    ui.s      := Items[ItemNo];
    List.AddInfo(ui);
    item.MovingToUndoList(ItemNo, Self, ui);
    end
  else begin
    FreeItem(ItemNo, False);
  end;
  Items.Delete(ItemNo);
  if (LastAffectedItemNo<>-1) then begin
    Do_Para(ItemNo, LastAffectedItemNo-1,
            GetItem(ItemNo-1).ParaNo, FR);
    FullReformat := FR or FullReformat;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_ModifyItem(ItemNo: Integer; const s: String;
                                      Item: TCustomRVItemInfo);
var ui: TRVUndoModifyItemInfo;
    OldItem: TCustomRVItemInfo;
    List: TRVUndoList;
begin
  OldItem := GetItem(ItemNo);
  Item.Checkpoint := OldItem.Checkpoint;
  if Item.Checkpoint<>nil then
    Item.Checkpoint.ItemInfo := Item;
  OldItem.Checkpoint := nil;
  if (rvoTagsArePChars in Options) and (Item.Tag=OldItem.Tag) then
    Item.Tag := RV_CopyTag(OldItem.Tag,rvoTagsArePChars in Options);

  Item.ParaNo      := OldItem.ParaNo;
  Item.ItemOptions := OldItem.ItemOptions;
  Item.JumpID      := OldItem.JumpID;
  Item.DrawItemNo  := OldItem.DrawItemNo;

  if FActiveItem=OldItem then
    FActiveItem := Item;
  if FPartialSelectedItem=OldItem then
    FPartialSelectedItem := Item;

  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoModifyItemInfo.Create;
    ui.Action := rvuModifyItem;
    ui.Item   := OldItem;
    //!!ui.RVData := Self;
    ui.ItemNo := ItemNo;
    ui.s      := Items[ItemNo];
    //OldItem.MovingToUndoList;
    List.AddInfo(ui);
    end
  else begin
    InternalFreeItem(OldItem, False); // never called for controls
  end;
  Items[ItemNo] := s;
  Items.Objects[ItemNo] := Item;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_DeleteItems(StartItemNo, EndItemNo: Integer; var FullReformat: Boolean);
var i: Integer;
    item: TCustomRVItemInfo;
    List: TRVUndoList;
    ui: TRVUndoDeleteItemsInfo;
    LastAffectedItemNo: Integer;
    FR: Boolean;
begin
  FullReformat := False;
  if StartItemNo>EndItemNo then exit;
  FullReformat := CalculateMinItemsWidthPlusEx(StartItemNo, EndItemNo)=DocumentWidth;
  for i := EndItemNo downto StartItemNo do begin
    item := GetItem(i);
    if item.Checkpoint<>nil then
      Do_DeleteCP(i);
  end;

  if False and (StartItemNo>0)  then
    LastAffectedItemNo := GetParaEndItemNo(EndItemNo)
  else
    LastAffectedItemNo := -1;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoDeleteItemsInfo.Create;
    ui.Action := rvuDeleteItems;
    ui.StartItemNo := StartItemNo;
    ui.LastAffectedItemNo := LastAffectedItemNo;
    List.AddInfo(ui);
    for i := EndItemNo downto StartItemNo do begin
      item := GetItem(i);
      ui.List.AddObject(Items[i],item);
      item.MovingToUndoList(i, Self, ui);
      Items.Delete(i);
    end;
    end
  else begin
    for i := EndItemNo downto StartItemNo do begin
      FreeItem(i, False);
      Items.Delete(i);
    end;
  end;
  if (LastAffectedItemNo<>-1) then begin
    Do_Para(StartItemNo,LastAffectedItemNo-(EndItemNo-StartItemNo+1),
            GetItemPara(StartItemNo-1), FR);
    FullReformat := FullReformat or FR;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_DeleteSubstring(ItemNo, Index,ALength: Integer);
var s: String;
    List: TRVUndoList;
    ui: TRVUndoDeleteSubstringInfo;
begin
  s := Items[ItemNo];
  if ALength=-1 then
    ALength := RVU_Length(s,GetItemOptions(ItemNo))-Index+1;
  if ALength=0 then exit;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoDeleteSubstringInfo.Create;
    ui.Action := rvuDeleteSubstring;
    ui.ItemNo := ItemNo;
    ui.Index  := Index;
    ui.s      := RVU_Copy(Items[ItemNo],Index, ALength, GetItemOptions(ItemNo));
    List.AddInfo(ui);
  end;
  RVU_Delete(s, Index, ALength, GetItemOptions(ItemNo));
  ItemAction(rviaTextModifying, GetItem(ItemNo), s, Self); 
  Items[ItemNo] := s;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_Para(FirstItemNo, EndItemNo, ParaNo: Integer;
                                var FullReformat: Boolean);
var List: TRVUndoList;
    ui: TRVUndoParaListInfo;
    i: Integer;
begin
  FullReformat := False;
  if EndItemNo<FirstItemNo then
    exit;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoParaListInfo.Create;
    ui.Action := rvuPara;
    ui.StartItemNo := FirstItemNo;
    for i := FirstItemNo to EndItemNo do
      ui.List.Add(GetItemPara(i));
    List.AddInfo(ui);
  end;
  SetParaStyle(FirstItemNo, EndItemNo, ParaNo, FullReformat);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_ParaList(FirstItemNo: Integer; ParaList: TRVIntegerList;
                                    var FullReformat: Boolean);
var List: TRVUndoList;
    ui: TRVUndoParaInfo;
    i: Integer;
    NewWidth, OldWidth: Integer;
begin
  FullReformat := False;
  if ParaList.Count=0 then
    exit;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoParaInfo.Create;
    ui.Action := rvuPara;
    ui.ItemNo := FirstItemNo;
    ui.Count  := ParaList.Count;
    ui.ParaNo := GetItemPara(FirstItemNo);
    for i := 0 to ParaList.Count-1 do
      if not GetItem(FirstItemNo+i).GetBoolValue(rvbpFullWidth) then begin
        ui.ParaNo := GetItemPara(FirstItemNo+i);
        break;
      end;
    List.AddInfo(ui);
  end;
  OldWidth := CalculateMinItemsWidthPlusEx(FirstItemNo, FirstItemNo+ParaList.Count-1);
  for i := 0 to ParaList.Count-1 do
    GetItem(FirstItemNo+i).ParaNo := ParaList[i];
  NewWidth := CalculateMinItemsWidthPlusEx(FirstItemNo, FirstItemNo+ParaList.Count-1);
  FullReformat := (OldWidth<>NewWidth) and
                  ((NewWidth>DocumentWidth) or
                   (OldWidth>=DocumentWidth));
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_StyleNo(ItemNo, StyleNo: Integer);
var List: TRVUndoList;
    ui: TRVUndoStyleNoInfo;
    item :TCustomRVItemInfo;
begin
  item := GetItem(ItemNo);
  if (item.StyleNo<0) or (item.StyleNo=StyleNo) then
    exit;

  List := GetUndoList;
  if List<>nil then begin
    ui             := TRVUndoStyleNoInfo.Create;
    ui.Action      := rvuStyleNo;
    ui.ItemNo      := ItemNo;
    ui.WasStyleNo  := item.StyleNo;
    List.AddInfo(ui);
  end;
  {$IFNDEF RVDONOTUSEUNICODE}
  if GetRVStyle.TextStyles[GetActualStyle(item)].Unicode then begin
    if not GetRVStyle.TextStyles[GetActualStyle2(StyleNo, item.ParaNo)].Unicode then
      Exclude(item.ItemOptions, rvioUnicode)
    end
  else begin
    if GetRVStyle.TextStyles[GetActualStyle2(StyleNo,item.ParaNo)].Unicode then
      Include(item.ItemOptions, rvioUnicode)
  end;
  {$ENDIF}
  item.StyleNo := StyleNo;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_InsertItem(ItemNo: Integer; var s: String;
                                     item: TCustomRVItemInfo; FromUndo: Boolean;
                                     var FullReformat: Boolean);
var  List: TRVUndoList;
     ui: TRVUndoInsertItemInfo;
     LastAffectedItemNo: Integer;
     Checkpoint: TRVCPInfo;
begin
  FullReformat := False;
  if item.CanBeBorderStart and (ItemNo<Items.Count) and
     not GetItem(ItemNo).CanBeBorderStart then
    LastAffectedItemNo := GetParaEndItemNo(ItemNo)
  else
    LastAffectedItemNo := -1;

  Checkpoint := item.Checkpoint;
  item.Checkpoint := nil;

  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoInsertItemInfo.Create;
    ui.Action :=  rvuInsertItem;
    ui.ItemNo := ItemNo;
    ui.LastAffectedItemNo := LastAffectedItemNo;
    List.AddInfo(ui);
  end;
  item.Inserting(Self, s, False);
  Items.InsertObject(ItemNo,s,item);
  item.Inserted(Self, ItemNo);
  {$IFNDEF RVDONOTUSELISTS}
  AddMarkerInList(ItemNo);
  {$ENDIF}
  if FromUndo then
    item.MovingFromUndoList(ItemNo, Self);  
  item.UpdatePaletteInfo(GetDoInPaletteMode, False, GetRVPalette, GetRVLogPalette);
  if LastAffectedItemNo<>-1 then
     Do_Para(ItemNo+1,LastAffectedItemNo+1,item.ParaNo,FullReformat);
     //SetParaStyle(ItemNo+1,LastAffectedItemNo+1,item.ParaNo);
  if Checkpoint<>nil then
    Do_AddCP(ItemNo, Checkpoint);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_ReplaceItem(ItemNo: Integer; item: TCustomRVItemInfo);
var List: TRVUndoList;
    ui: TRVUndoReplaceItemInfo;
    olditem :TCustomRVItemInfo;
    s: String;
    StyleNo: Integer;
begin
  s := Items[ItemNo];
  olditem := GetItem(ItemNo);
  StyleNo := item.StyleNo;
  item.Assign(olditem);
  item.StyleNo := StyleNo;
  item.DrawItemNo := olditem.DrawItemNo;
  item.Tag        := olditem.Tag;
  item.ItemOptions:= olditem.ItemOptions;
  item.JumpID     := olditem.JumpID;
  item.Checkpoint := olditem.Checkpoint;
  if item.Checkpoint<>nil then
    item.Checkpoint.ItemInfo := item;
  olditem.Checkpoint := nil;
  olditem.Tag        := 0;
  if olditem=CaptureMouseItem then
    ReleaseMouseCapture(olditem);
  if olditem=FActiveItem then
    FActiveItem := item;
  List := GetUndoList;
  if List<>nil then begin
    ui             := TRVUndoReplaceItemInfo.Create;
    ui.Action      := rvuModifyItem;
    ui.ItemNo      := ItemNo;
    ui.item        := olditem;
    olditem.MovingToUndoList(ItemNo, Self, ui);
    List.AddInfo(ui);
    end
  else
    FreeItem(ItemNo, False);
  item.Inserting(Self, s, False);
  Items.Objects[ItemNo] := item;
  item.Inserted(Self, ItemNo);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.Do_InsertItems_1(ItemNo,Count: Integer):TRVUndoInsertItemsInfo;
var List: TRVUndoList;
    ui: TRVUndoInsertItemsInfo;
begin
  Result   := nil;

  if Count=0 then
    exit;

  List := GetUndoList;
  if List<>nil then begin
    ui :=  TRVUndoInsertItemsInfo.Create;
    ui.Action := rvuInsertItems;
    ui.ItemNo := ItemNo;
    ui.Count  := Count;
    List.AddInfo(ui);
    end
  else
    ui := nil;
  Result := ui;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_InsertItems_2(ItemNo,Count: Integer; ui: TRVUndoInsertItemsInfo;
                                         var FullReformat: Boolean);
var i: Integer;
    Checkpoint: TRVCPInfo;
    item: TCustomRVItemInfo;
    LastAffectedItemNo: Integer;    
begin
  FullReformat := False;
  if Count=0 then exit;
  if (ItemNo+Count<Items.Count) and
     not GetItem(ItemNo+Count).CanBeBorderStart then
    LastAffectedItemNo := GetParaEndItemNo(ItemNo+Count-1)
  else
    LastAffectedItemNo := -1;

  if ui<>nil then begin
    ui.LastAffectedItemNo := LastAffectedItemNo;
  end;

  if LastAffectedItemNo<>-1 then
    Do_Para(ItemNo+Count-1,LastAffectedItemNo,
            GetItemPara(ItemNo+Count-1), FullReformat);

  for i := ItemNo to ItemNo+Count-1 do begin
    item := GetItem(i);
    if item.Checkpoint<>nil then begin
      Checkpoint := item.Checkpoint;
      item.Checkpoint := nil;
      Do_AddCP(i, Checkpoint);
    end;
  end;
end;
{------------------------------------------------------------------------------}
// in rev. order
procedure TRVEditRVData.Do_InsertItems(ItemNo: Integer; sl: TStringList; FromUndo: Boolean;
                                       var FullReformat: Boolean);
var  i: Integer;
     item: TCustomRVItemInfo;
     ui: TRVUndoInsertItemsInfo;
     s: String;
begin
  FullReformat := False;
  if sl.Count=0 then exit;

  ui := Do_InsertItems_1(ItemNo, sl.Count);

  for i := 0 to sl.Count-1 do begin
    item := TCustomRVItemInfo(sl.Objects[i]);
    s := sl[i];
    item.Inserting(Self, s, False);
    Items.InsertObject(ItemNo, s, item);
    item.Inserted(Self, ItemNo);
    {$IFNDEF RVDONOTUSELISTS}
    AddMarkerInList(ItemNo);
    {$ENDIF}
    if FromUndo then
      item.MovingFromUndoList(ItemNo, Self);
    item.UpdatePaletteInfo(GetDoInPaletteMode, False, GetRVPalette, GetRVLogPalette);
  end;

  Do_InsertItems_2(ItemNo, sl.Count, ui, FullReformat);

end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_InsertSubstring(ItemNo, Index: Integer; const s: String);
var str: String;
    List: TRVUndoList;
    ui: TRVUndoInsertSubstringInfo;
begin
  if (Length(s)=0) then exit;
  if (ItemNo<0) or (ItemNo>=Items.Count) or
     (GetItemStyle(ItemNo)<0) then
    raise Exception.Create(errRVUndo);
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoInsertSubstringInfo.Create;
    ui.Action := rvuInsertSubstring;
    ui.ItemNo := ItemNo;
    ui.Index  := Index;
    ui.Length := RVU_Length(s, GetItemOptions(ItemNo));
    List.AddInfo(ui);
  end;
  str := Items[ItemNo];
  RVU_Insert(s, str, Index, GetItemOptions(ItemNo));
  ItemAction(rviaTextModifying, GetItem(ItemNo), str, Self);
  Items[ItemNo] := str;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_ChangeText(ItemNo: Integer; const s: String);
var List: TRVUndoList;
    ui: TRVUndoChangeTextInfo;
    str: String;
begin
  if Items[ItemNo]=s then exit;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoChangeTextInfo.Create;
    ui.Action := rvuChangeText;
    ui.ItemNo := ItemNo;
    ui.s      := Items[ItemNo];
    List.AddInfo(ui);
  end;
  str := s;
  ItemAction(rviaTextModifying, GetItem(ItemNo), str, Self);
  Items[ItemNo] := str;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_ChangeVAlign(ItemNo: Integer; VAlign: TRVVAlign);
var List: TRVUndoList;
    ui: TRVUndoChangeVAlignInfo;
    item: TCustomRVItemInfo;
begin
  item := GetItem(ItemNo);
  if (item as TRVRectItemInfo).VAlign = VAlign then exit;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoChangeVAlignInfo.Create;
    ui.Action := rvuChangeText;
    ui.ItemNo := ItemNo;
    ui.VAlign := TRVRectItemInfo(item).VAlign;
    List.AddInfo(ui);
  end;
  TRVRectItemInfo(item).Valign := VAlign;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_Resize(ItemNo, Width, Height: Integer;Reformat: Boolean);
var List: TRVUndoList;
    ui: TRVUndoResizeInfo;
    item: TRVControlItemInfo;
begin
  // for controls only
  item := TRVControlItemInfo(GetItem(ItemNo));
  if (item.Control.Width  = Width) and
     (item.Control.Height = Height) then exit;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoResizeInfo.Create;
    ui.Action := rvuModifyItem;
    ui.ItemNo := ItemNo;
    ui.Width  := item.Control.Width;
    ui.Height := item.Control.Height;
    List.AddInfo(ui);
  end;
  ResizeControl(ItemNo, Width, Height, Reformat);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.Do_ModifyItemIntProperty(ItemNo: Integer;
                        SubObject: TObject;
                        const PropertyName: String;
                        Value: LongInt;
                        AffectSize, AffectWidth: Boolean;
                        UndoInfoClass: TRVUndoInfoClass):TRVUndoModifyItemProps;
var List: TRVUndoList;
    ui: TRVUndoModifyItemIntProperty;
    item: TCustomRVItemInfo;
    propinfo: PPropInfo;
    OldValue:LongInt;
begin
  Result := nil;
  item := GetItem(ItemNo);
  if SubObject=nil then
    SubObject := item;
  propinfo := GetPropInfo(SubObject.ClassInfo, PropertyName);
  OldValue := GetOrdProp(SubObject, propinfo);
  if OldValue=Value then exit;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoModifyItemIntProperty(UndoInfoClass.Create);
    ui.Action := rvuModifyItem;
    ui.ItemNo := ItemNo;
    ui.PropertyName := PropertyName;
    ui.Value        := OldValue;
    ui.AffectSize   := AffectSize;
    ui.AffectWidth  := AffectWidth;
    ui.SubObject    := SubObject;
    List.AddInfo(ui);
    end
  else
    ui := nil;
  item.BeforeUndoChangeProperty;
  try
    SetOrdProp(SubObject, propinfo,Value);
  finally
    item.AfterUndoChangeProperty;
  end;
  Result := ui;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.Do_ModifyItemIntProperties(ItemNo: Integer;
  SubObject: TObject; PropList: TStringList; AffectSize,
  AffectWidth: Boolean;
  UndoInfoClass: TRVUndoInfoClass):TRVUndoModifyItemProps;
var List: TRVUndoList;
    ui: TRVUndoModifyItemIntProperties;
    item: TCustomRVItemInfo;
    propinfo: PPropInfo;
    i: Integer;
    OldValue, NewValue: LongInt;
begin
  item := GetItem(ItemNo);
  if SubObject=nil then
    SubObject := item;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoModifyItemIntProperties(UndoInfoClass.Create);
    ui.Action := rvuModifyItem;
    ui.ItemNo := ItemNo;
    ui.AffectSize   := AffectSize;
    ui.AffectWidth  := AffectWidth;
    ui.SubObject    := SubObject;
    end
  else
    ui := nil;
  item.BeforeUndoChangeProperty;
  try
    for i := PropList.Count-1 downto 0 do begin
      propinfo := GetPropInfo(SubObject.ClassInfo, PropList[i]);
      OldValue := GetOrdProp(SubObject, propinfo);
      NewValue := LongInt(PropList.Objects[i]);
      if OldValue<>NewValue then begin
        if ui<>nil then
          ui.PropList.AddObject(PropList[i], TObject(OldValue));
        SetOrdProp(SubObject, propinfo, NewValue);
      end;
    end;
  finally
    item.AfterUndoChangeProperty;
  end;
  if ui<>nil then begin
    if ui.PropList.Count=0 then begin
      ui.Free;
      ui := nil;
      end
    else
      List.AddInfo(ui);
  end;
  Result := ui;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_NewLine(ItemNo: Integer; SameAsPrev: Boolean;
                                   ParaNo: Integer;
                                   var FullReformat: Boolean);
var item: TCustomRVItemInfo;
    List: TRVUndoList;
    ui: TRVUndoNewLineInfo;
    LastAffectedItemNo: Integer;
    FR: Boolean;
begin
  FullReformat := False;
  item := GetItem(ItemNo);
  if Item.GetBoolValue(rvbpFullWidth) then
    exit;
  if not SameAsPrev and (item.SameAsPrev = SameAsPrev) then exit;
  if SameAsPrev then begin
    Do_BR(ItemNo, False, FR);
    FullReformat := FullReformat or FR;
    Do_PageBreak(ItemNo, False);
  end;
  if (ItemNo<Items.Count-1) and
     not GetItem(ItemNo+1).CanBeBorderStart then
    LastAffectedItemNo := GetParaEndItemNo(ItemNo+1)
  else
    LastAffectedItemNo := -1;

  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoNewLineInfo.Create;
    ui.Action := rvuNewLine;
    ui.ItemNo := ItemNo;
    ui.WasSameAsPrev  := item.SameAsPrev;
    ui.WasParaNo      := item.ParaNo;
    ui.LastAffectedItemNo := LastAffectedItemNo;
    List.AddInfo(ui);
  end;

  if LastAffectedItemNo=-1 then
    LastAffectedItemNo := ItemNo;

  if SameAsPrev then begin
    if ItemNo>0 then begin
      Do_Para(ItemNo,LastAffectedItemNo,
              GetItemPara(ItemNo-1), FR);
      FullReformat := FullReformat or FR;
    end
    end
  else begin
    if ParaNo<>-1 then begin
      Do_Para(ItemNo,LastAffectedItemNo, ParaNo, FR);
      FullReformat := FullReformat or FR;      
    end;
  end;
  item.SameAsPrev := SameAsPrev;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_BR(ItemNo: Integer; BR: Boolean; var FullReformat: Boolean);
var item: TCustomRVItemInfo;
    List: TRVUndoList;
    ui: TRVUndoBRInfo;
    OldWidth, NewWidth: Integer;
begin
  FullReformat := False;
  item := GetItem(ItemNo);
  if item.SameAsPrev or item.BR=BR then
    exit;
  OldWidth := CalculateMinItemWidthPlusEx(ItemNo);
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoBRInfo.Create;
    ui.Action := rvuBR;
    ui.ItemNo := ItemNo;
    ui.WasBR  := item.BR;
    List.AddInfo(ui);
  end;
  item.BR := BR;
  NewWidth := CalculateMinItemWidthPlusEx(ItemNo);
  FullReformat := (OldWidth<>NewWidth) and
                  ((NewWidth>DocumentWidth) or
                   (OldWidth>=DocumentWidth));
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_PageBreak(ItemNo: Integer; PageBreak: Boolean);
var item: TCustomRVItemInfo;
    List: TRVUndoList;
    ui: TRVUndoPageBreakInfo;
begin
  {$IFNDEF RVDONOTUSELISTS}
  if (ItemNo>0) and (GetItemStyle(ItemNo-1)=rvsListMarker) then
    dec(ItemNo);
  {$ENDIF}
  item := GetItem(ItemNo);
  if item.SameAsPrev or item.PageBreakBefore=PageBreak then
    exit;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoPageBreakInfo.Create;
    ui.Action := rvuPageBreak;
    ui.ItemNo := ItemNo;
    ui.WasPageBreak  := item.PageBreakBefore;
    List.AddInfo(ui);
  end;
  item.PageBreakBefore := PageBreak;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_ExtraIntProperty(ItemNo: Integer;
  Prop: TRVExtraItemProperty; Value: Integer);
var item: TCustomRVItemInfo;
    List: TRVUndoList;
    ui: TRVUndoExtraIntProperty;
    OldValue: Integer;
begin
  item := GetItem(ItemNo);
  if not item.GetExtraIntProperty(Prop,OldValue) or (OldValue=Value) then
    exit;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoExtraIntProperty.Create;
    ui.Action := rvuModifyItem;
    ui.ItemNo := ItemNo;
    ui.OldValue := OldValue;
    ui.Prop   := Prop;
    List.AddInfo(ui);
  end;
  item.SetExtraIntProperty(Prop, Value);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_ExtraStrProperty(ItemNo: Integer;
  Prop: TRVExtraItemStrProperty; const Value: String);
var item: TCustomRVItemInfo;
    List: TRVUndoList;
    ui: TRVUndoExtraStrProperty;
    OldValue: String;
begin
  item := GetItem(ItemNo);
  if not item.GetExtraStrProperty(Prop, OldValue) or (OldValue=Value) then
    exit;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoExtraStrProperty.Create;
    ui.Action := rvuModifyItem;
    ui.ItemNo := ItemNo;
    ui.OldValue := OldValue;
    ui.Prop   := Prop;
    List.AddInfo(ui);
  end;
  item.SetExtraStrProperty(Prop, Value);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_Tag(ItemNo, Tag: Integer; AssignAsIs: Boolean);
var item: TCustomRVItemInfo;
    List: TRVUndoList;
    ui: TRVUndoTagInfo;
begin
  item := GetItem(ItemNo);
  if Tag=item.Tag then exit;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoTagInfo.Create;
    ui.Action := rvuTag;
    ui.ItemNo := ItemNo;
    ui.WasTag  := RV_CopyTag(item.Tag, rvoTagsArePChars in Options);
    ui.TagsArePChars := rvoTagsArePChars in Options;
    List.AddInfo(ui);
  end;
  if rvoTagsArePChars in Options then
    StrDispose(PChar(item.Tag));
  if AssignAsIs then
    item.Tag := Tag
  else
    item.Tag := RV_CopyTag(Tag, rvoTagsArePChars in Options);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_AddCP(ItemNo: Integer; Checkpoint: TRVCPInfo);
var item: TCustomRVItemInfo;
    List: TRVUndoList;
    ui: TRVUndoAddCPInfo;
begin
  if ItemNo<>-1 then begin
    item := GetItem(ItemNo);
    if item.Checkpoint<>nil then
      raise ERichViewError.Create(errRVCP);
    end
  else begin
    item := nil;
    if NotAddedCP<>nil then
      raise ERichViewError.Create(errRVCP);
  end;
  if Checkpoint=nil then exit;
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoAddCPInfo.Create;
    ui.Action := rvuCheckpoint;
    ui.ItemNo := ItemNo;
    List.AddInfo(ui);
  end;
  if item<>nil then begin
    item.Checkpoint := Checkpoint;
    Checkpoint.ItemInfo := item;
    end
  else begin
    NotAddedCP := Checkpoint;
    Checkpoint.ItemInfo := nil;
  end;
  inc(CPCount);
  UpdateCPPos(Checkpoint, ItemNo);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_DeleteCP(ItemNo: Integer);
var item: TCustomRVItemInfo;
    List: TRVUndoList;
    ui: TRVUndoDeleteCPInfo;
    CP: TRVCPInfo;
begin
  if ItemNo<>-1 then begin
    item := GetItem(ItemNo);
    CP := item.Checkpoint;
    end
  else begin
    item := nil;
    CP := NotAddedCP;
  end;
  if CP=nil then
    raise ERichViewError.Create(errRVCP);
  List := GetUndoList;
  if List<>nil then begin
    ui := TRVUndoDeleteCPInfo.Create;
    ui.Action := rvuCheckpoint;
    ui.ItemNo := ItemNo;
    ui.Checkpoint := CP;
    ui.TagsArePChars := rvoTagsArePChars in Options;
    List.AddInfo(ui);
    UnlinkCheckpoint(CP,True);
    end
  else
    FreeCheckpoint(CP,True,True);
  if item<>nil then
    item.Checkpoint := nil
  else
    NotAddedCP := nil;
end;
{------------------------------------------------------------------------------}
{ Moving checkpoint (by copying-deleting) from the SrcItemNo-th item to
  the DestItemNo-th item                                                       }
procedure TRVEditRVData.Do_MoveCP(SrcItemNo, DestItemNo: Integer);
var CP: TRVCPInfo;
begin
  if GetItemCheckpoint(SrcItemNo)<>nil then begin    
    if GetItemCheckpoint(DestItemNo)<>nil then
      Do_DeleteCP(DestItemNo);
    CP := TRVCPInfo.Create;
    CP.Assign(GetItemCheckpoint(SrcItemNo), rvoTagsArePChars in Options);
    Do_AddCP(DestItemNo, CP);
    Do_DeleteCP(SrcItemNo);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Do_Concate(FirstItemNo: Integer);
var // FullReformat,
    FR: Boolean;
    StyleNo: Integer;
    SAP, BR: Boolean;
    item: TCustomRVItemInfo;
    CP: TRVCPInfo;
begin
  // FullReformat := False;
  if ((rvioUnicode in GetItemOptions(FirstItemNo))<>
     (rvioUnicode in GetItemOptions(FirstItemNo+1))) and
     (Items[FirstItemNo+1]<>'') then begin
    item := GetItem(FirstItemNo);
    BR  := item.BR;
    SAP := item.SameAsPrev;
    if not SAP then begin
      Do_NewLine(FirstItemNo+1, False, item.ParaNo, FR);
      // FullReformat := FullReformat or FR;
    end;
    if BR then begin
      Do_BR(FirstItemNo+1, True, FR);
      // FullReformat := FullReformat or FR;
    end;
    if (GetItemCheckpoint(FirstItemNo)<>nil) and
       (GetItemCheckpoint(FirstItemNo+1)=nil) then begin
       CP := TRVCPInfo.Create;
       CP.Assign(GetItemCheckpoint(FirstItemNo), rvoTagsArePChars in Options);
       Do_AddCP(FirstItemNo+1, CP);
    end;
    Do_DeleteItem(FirstItemNo,FR);
    // FullReformat := FullReformat or FR;
    end
  else begin
    if Items[FirstItemNo]='' then begin
      StyleNo := GetItem(FirstItemNo+1).StyleNo;
      Do_Tag(FirstItemNo, GetItemTag(FirstItemNo+1), False);
      end
    else begin
      StyleNo := -1;
    end;
    Do_InsertSubstring(FirstItemNo, ItemLength(FirstItemNo)+1,Items[FirstItemNo+1]);
    if (GetItemCheckpoint(FirstItemNo+1)<>nil) and
       (GetItemCheckpoint(FirstItemNo)=nil) then begin
       CP := TRVCPInfo.Create;
       CP.Assign(GetItemCheckpoint(FirstItemNo+1), rvoTagsArePChars in Options);
       Do_AddCP(FirstItemNo, CP);
    end;
    Do_DeleteItem(FirstItemNo+1, FR);
    // FullReformat := FullReformat or FR;
    if StyleNo<>-1 then
      Do_StyleNo(FirstItemNo, StyleNo);
  end;
  // assuming FullReformat=False for text
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.InsSomething(var info: TCustomRVItemInfo;
  var s: String; AutoTag: Boolean; var InsertPoint, ItemsAdded: Integer;
  var FullReformat: Boolean; var NewListNo: Integer): Boolean;
  {.............................................................}
  procedure AdjustTag(cur: TCustomRVItemInfo);
  begin
    if AutoTag and (info.Tag=0) and (info.StyleNo>=0) and (info.StyleNo=cur.StyleNo) then
      info.Tag := RV_CopyTag(cur.Tag, rvoTagsArePChars in Options)
  end;
  {.............................................................}
  procedure SetTagToCurTag;
  begin
    if AutoTag and (info.Tag=0) and (info.StyleNo>=0) and
      (info.StyleNo=FCurTextStyleNo) then
      info.Tag := RV_CopyTag(FCurTag, rvoTagsArePChars in Options)
  end;
  {.............................................................}
  // Inserting before or after non-text item.
  // If inserting a full-line item before the first item in bulleted paragraph,
  // inserting a empty string before, so result will look like
  // {bullet} {empty string (added)}
  // {new full-line item}
  // Special case: marker can be inserted only in place of empty line,
  // not here
  procedure IS_InsertNearNonText(CharPos: TRVCharPos; dli: TRVDrawLineInfo;
    var ItemParaNo: Integer; var InsertPoint: Integer; var ItemsAdded: Integer;
    var FullReformat: Boolean);
  var FR: Boolean;
  begin
    {$IFNDEF RVDONOTUSELISTS}
    if info.StyleNo=rvsListMarker then begin
      Result := True;
      info.Free;
      info := nil;
      exit;
    end;
    {$ENDIF}
    if CharPos.Offset=0 then begin // inserting before nontext
      InsertPoint := dli.ItemNo;
      if not GetItem(InsertPoint).SameAsPrev then begin
        info.SameAsPrev := False;
        info.BR := GetItem(InsertPoint).BR;
        if info.BR then
          ItemParaNo := -1;
        if GetItem(InsertPoint).PageBreakBefore then
          info.PageBreakBefore := True;
        Do_NewLine(InsertPoint, True, -1, FR);
        FullReformat := FullReformat or FR;
        end
      else begin
        ItemParaNo := -1;
        {$IFNDEF RVDONOTUSELISTS}
        if (InsertPoint>0) and (GetItemStyle(InsertPoint-1)=rvsListMarker) and
           (info.GetBoolValue(rvbpFullWidth) or
            ((info.Checkpoint<>nil) and info.Checkpoint.Persistent and
              ItemHasPersistentCheckpoint(InsertPoint-1))) then begin
          InsEmptyString(InsertPoint, 0, GetItemPara(InsertPoint),
            FCurTextStyleNo, True, False);
          inc(InsertPoint);
          inc(ItemsAdded);
        end;
        {$ENDIF}
      end;
      end
    else begin  // inserting after nontext
      ItemParaNo := -1;
      InsertPoint := dli.ItemNo+1;
      if GetItem(dli.ItemNo).GetBoolValue(rvbpFullWidth) then
        info.SameAsPrev := False;
    end;
  end;
  {.............................................................}
  // Replacing an empty line with new item
  // Exception: If inserting a full-line item in bulleted paragraph,
  // we do not replace empty string, but add new item after,
  // so result will look like:
  // {bullet} {old empty string}
  // {new full-line item}
  // Special case: if inserting marker, it deletes empty string and
  // replaces a marker before it (if exists)
  function IS_ReplaceEmptyText(CharPos: TRVCharPos;
                                dli: TRVDrawLineInfo;
                                var ItemParaNo: Integer;
                                var InsertPoint: Integer;
                                var ItemsAdded: Integer;
                                var FullReformat: Boolean): Boolean;
  var curitem: TCustomRVItemInfo;
      FR: Boolean;
      {$IFNDEF RVDONOTUSELISTS}
      PrevMarkerItemNo: Integer;
      {$ENDIF}
  begin
    Result := False;
    if IsSticking(dli.ItemNo-1, False) or
       IsSticking(dli.ItemNo, False) then begin
      info.Free;
      info := nil;
      exit;
    end;
    {$IFNDEF RVDONOTUSELISTS}
    if (dli.ItemNo>0) and (GetItemStyle(dli.ItemNo-1)=rvsListMarker) and
       (info.GetBoolValue(rvbpFullWidth) or
       ((info.Checkpoint<>nil) and info.Checkpoint.Persistent and
        ParaHasPersistentCheckpoint(dli.ItemNo)))
       then
      InsertPoint := dli.ItemNo+1
    else
    {$ENDIF}
    begin
      curitem := GetItem(dli.ItemNo);
      {$IFNDEF RVDONOTUSELISTS}
      if (info.StyleNo=rvsListMarker) and (TRVMarkerItemInfo(info).ListNo>=0) and
         GetRVStyle.ListStyles[TRVMarkerItemInfo(info).ListNo].HasNumbering then begin
        if dli.ItemNo=rvsListMarker then
          PrevMarkerItemNo := dli.ItemNo
        else begin
          PrevMarkerItemNo := dli.ItemNo-1;
          if PrevMarkerItemNo>=0 then begin
            PrevMarkerItemNo := GetFirstParaItem(PrevMarkerItemNo);
            if (GetItemStyle(PrevMarkerItemNo)<>rvsListMarker) then
              PrevMarkerItemNo := -1;
          end;
        end;
        if (PrevMarkerItemNo>=0) and (TRVMarkerItemInfo(GetItem(PrevMarkerItemNo)).ListNo>=0) and
          GetRVStyle.ListStyles[TRVMarkerItemInfo(GetItem(PrevMarkerItemNo)).ListNo].IsSimpleEqual(
            GetRVStyle.ListStyles[TRVMarkerItemInfo(info).ListNo], True, True) then begin
          NewListNo := TRVMarkerItemInfo(GetItem(PrevMarkerItemNo)).ListNo;
          TRVMarkerItemInfo(info).ListNo := NewListNo;
        end;
      end;
      {$ENDIF}
      if not curitem.SameAsPrev then begin
        info.SameAsPrev := False;
        {$IFNDEF RVDONOTUSELISTS}
        if info.StyleNo<>rvsListMarker then
        {$ENDIF}
          info.BR := curitem.BR;
      end;
      if info.BR then
        ItemParaNo := -1;
      if curitem.PageBreakBefore then
        info.ItemOptions := info.ItemOptions+[rvioPageBreakBefore];
      Do_DeleteItem(dli.ItemNo, FR);
      FullReformat := FullReformat or FR;
      InsertPoint := dli.ItemNo;
      dec(ItemsAdded);
      info.ParaNo := FCurParaStyleNo;
      {$IFNDEF RVDONOTUSELISTS}
      if (info.StyleNo=rvsListMarker) and (InsertPoint>0) and
         (GetItemStyle(InsertPoint-1)=rvsListMarker) then begin
        dec(InsertPoint);
        Do_DeleteItem(InsertPoint, FR);
        FullReformat := FullReformat or FR;
        dec(ItemsAdded);
      end;
      {$ENDIF}
    end;
    Result := True;
  end;
  {.............................................................}
  // Inserting in text (before, after, or between)
  // Special case: a full-line item is never inserted before the first text
  // item in bulleted paragraph. In this case, it splits this text item into
  // two parts, one of them is empty:
  // {bullet} {empty string (added) }
  // {new full-line item} { string } { rest of paragraph }
  // Another special case: marker can be inserted only in place of empty line,
  // not here
  function IS_InsertInText(CharPos: TRVCharPos;
                           dli: TRVDrawLineInfo;
                           var ItemParaNo: Integer;
                           var InsertPoint: Integer;
                           var ItemsAdded: Integer;
                           var FullReformat: Boolean): Boolean;
  var s2: String;
      newsubstr: TCustomRVItemInfo;
      FR: Boolean;
  begin
    {$IFNDEF RVDONOTUSELISTS}
    if info.StyleNo=rvsListMarker then begin
      Result := True;
      info.Free;
      info := nil;
      exit;
    end;
    {$ENDIF}
    Result := False;
    if (dli.Offs+CharPos.Offset-1<=1)
       {$IFNDEF RVDONOTUSELISTS}
       and not
       (((dli.ItemNo>0) and (GetItemStyle(dli.ItemNo-1)=rvsListMarker)) and
        (info.GetBoolValue(rvbpFullWidth) or
         ((info.Checkpoint<>nil) and info.Checkpoint.Persistent and
           ItemHasPersistentCheckpoint(dli.ItemNo-1))))
       {$ENDIF} then begin // inserting before text
      InsertPoint := dli.ItemNo;
      if IsSticking(InsertPoint-1, False) then begin
        info.Free;
        info := nil;
        exit;
      end;
      if not info.GetBoolValue(rvbpFullWidth) and  not GetItem(InsertPoint).SameAsPrev then begin
        info.SameAsPrev := False;
        info.BR := GetItem(InsertPoint).BR;
        if info.BR then
          ItemParaNo := -1;
        Do_NewLine(InsertPoint, True, -1, FR);
        FullReformat := FullReformat or FR;
        end
      else
        ItemParaNo := -1;
      AdjustTag(GetItem(InsertPoint));
      end
    else if dli.Offs+CharPos.Offset-1>ItemLength(dli.ItemNo) then begin // inserting after text
      ItemParaNo := -1;
      InsertPoint := dli.ItemNo+1;
      if IsSticking(InsertPoint-1, False) then begin
        info.Free;
        info := nil;
        exit;
      end;
      AdjustTag(GetItem(InsertPoint-1));
      end
    else begin // inserting in text
      if IsProtected(dli.ItemNo, rvprModifyProtect) then begin
        info.Free;
        info := nil;
        exit;
      end;
      ItemParaNo := -1;
      inc(ItemsAdded);
      InsertPoint := dli.ItemNo+1;
      s2 := RVU_Copy(Items[dli.ItemNo], dli.Offs+CharPos.Offset-1,
        RVU_Length(Items[dli.ItemNo], GetItemOptions(dli.ItemNo)),
        GetItemOptions(dli.ItemNo));
      Do_DeleteSubstring(dli.ItemNo, dli.Offs+CharPos.Offset-1, -1);
      newsubstr := RV_CreateTextCopy(TRVTextItemInfo(GetItem(dli.ItemNo)), Self);
      newsubstr.SameAsPrev := True;
      Do_InsertItem(InsertPoint,s2,newsubstr,False, FR);
      AdjustTag(GetItem(InsertPoint));
      FullReformat := FullReformat or FR;
    end;
    Result := True;
  end;
  {.............................................................}
var dli: TRVDrawLineInfo;
    FR: Boolean;
    ItemParaNo: Integer;
    ItemNo1, ItemNo2: Integer;
begin
  NewListNo := -1;
  FullReformat := False;
  Result := True;
  ItemsAdded := 1;
  ItemParaNo := info.ParaNo;
  if info.SameAsPrev or (ItemParaNo>=GetRVStyle.ParaStyles.Count) then
    ItemParaNo := -1;
  if (DrawItems.Count<>0) and not info.GetBoolValue(rvbpFullWidth) and
     ((info.Checkpoint=nil) or not info.Checkpoint.Persistent)
     {$IFNDEF RVDONOTUSELISTS}
     and (info.StyleNo<>rvsListMarker)
     {$ENDIF}
   then begin
    info.SameAsPrev := True;
    info.ParaNo := GetItemPara(DrawItems[CaretDrawItemNo].ItemNo);
    end
  else begin
    info.SameAsPrev := False;
    info.ParaNo := FCurParaStyleNo;
  end;
  if IsParaProtected(info.ParaNo,rvpaoReadOnly) then begin
    info.Free;
    info := nil;
    Result := False;
    exit;
  end;
  SetTagToCurTag;
  dli := DrawItems[CharEnds.Items[CaretOffs].DrawItemNo];
  if GetItemStyle(dli.ItemNo)<0 then
    IS_InsertNearNonText(CharEnds.Items[CaretOffs], dli, ItemParaNo, InsertPoint, ItemsAdded, FullReformat)
  else if (Items[dli.ItemNo]='') then
    Result := IS_ReplaceEmptyText(CharEnds.Items[CaretOffs], dli, ItemParaNo, InsertPoint, ItemsAdded, FullReformat)
  else
    Result := IS_InsertInText(CharEnds.Items[CaretOffs], dli, ItemParaNo, InsertPoint, ItemsAdded, FullReformat);
  if not Result or (info=nil) then
    exit;
  {$IFNDEF RVDONOTUSELISTS}
  if (InsertPoint-1>=0) and (GetItemStyle(InsertPoint-1)=rvsListMarker) then
    info.SameAsPrev := True;
  {$ENDIF}
  if info.GetBoolValue(rvbpFullWidth) then begin
    if InsertPoint<>Items.Count then begin
       Do_NewLine(InsertPoint, False, -1, FR);
       FullReformat := FullReformat or FR;
       Do_BR(InsertPoint, False, FR);
       FullReformat := FullReformat or FR;
    end;
    if info.StyleNo=rvsBreak then
      info.ParaNo := 0;
  end;
  FullReformat := FullReformat or FR;
  Do_InsertItem(InsertPoint, s, info, False, FR);
  if InsertPoint+1<Items.Count then
    MovePersistentCheckpoint(InsertPoint+1, True);
  if (ItemParaNo>=0) and (ItemParaNo<>info.ParaNo) and (info.StyleNo<>rvsBreak) then begin
    ExpandToPara(InsertPoint, InsertPoint, ItemNo1, ItemNo2);
    Do_Para(ItemNo1, ItemNo2, ItemParaNo, FR);
    FullReformat := FullReformat or FR;
  end;
  if (InsertPoint+1<Items.Count) and not IsParaStart(InsertPoint+1) and
     ItemHasPersistentCheckpoint(InsertPoint+1) then begin
    Do_NewLine(InsertPoint+1, False, -1, FR);
    FullReformat := FullReformat or FR;
  end;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.InsertSomething(info: TCustomRVItemInfo; var s: String;
  AutoTag, CaretBefore: Boolean): Boolean;
var
  InsertPoint, Dummy : Integer;
  Offs, ItemsAdded: Integer;
  FullReformat : Boolean;
begin
  if (info.StyleNo<>rvsBreak) and ((info.Checkpoint=nil) or not (info.Checkpoint.Persistent)) then
    info.ParaNo := FCurParaStyleNo;
  if (FPartialSelectedItem<>nil) or
     not InsSomething(info, s, AutoTag, InsertPoint, ItemsAdded, FullReformat, Dummy) then begin
    Beep;
    Result := False;
    exit;
  end;
  if info=nil then begin
    // only possible if info was a marker, and it was not inserted, but this is
    // not error
    Result := True;
    exit;
  end;
  if (FullReformat or (CalculateMinItemWidthPlusEx(InsertPoint)>DocumentWidth)) and
     (DocumentWidth<>CalculateMinDocWidthPlus(nil,nil)) then begin
    Format_(False,True,False,0,GetCanvas,False,False);
    Invalidate;
    ChangeCaret(False,True,True,False);
    end
  else
    FormatParas(CharEnds.Items[0].DrawItemNo, CharEnds.Items[CharEnds.Count-1].DrawItemNo,ItemsAdded);
  if CaretBefore then
    if info.StyleNo<0 then
      Offs := 0
    else
      Offs := 1
  else
    if info.StyleNo<0 then
      Offs := 1
    else
      Offs := RVU_Length(S, GetItemOptions(InsertPoint))+1;
  Item2DrawItem(InsertPoint, Offs, {->} CaretDrawItemNo, Offs);
  OnChangeCaretLine(Offs-2);
  ChangeCaret(False, True, False, False);
  Result := True;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.CreateTextItem(Tag, ParaStyle, FontStyle: Integer;
  SameAsPrev, BR: Boolean): TCustomRVItemInfo;
begin
  Result            := RichViewTextItemClass.Create(Self);
  Result.StyleNo    := FontStyle;
  Result.ParaNo     := ParaStyle;
  {$IFNDEF RVDONOTUSEUNICODE}
  if GetRVStyle.TextStyles[GetActualStyle(Result)].Unicode then
    Include(Result.ItemOptions,rvioUnicode);
  {$ENDIF}
  Result.SameAsPrev := SameAsPrev;
  Result.BR         := BR;
  Result.Tag        := Tag;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.CreateTextItem2(Tag: Integer; Source: TCustomRVItemInfo;
  SameAsPrev, BR: Boolean): TCustomRVItemInfo;
begin
  Result            := RichViewTextItemClass.Create(Self);
  Result.Assign(Source);
  Result.ItemOptions := [];
  {$IFNDEF RVDONOTUSEUNICODE}
  if GetRVStyle.TextStyles[Result.StyleNo].Unicode then
    Include(Result.ItemOptions,rvioUnicode);
  {$ENDIF}
  Result.SameAsPrev := SameAsPrev;
  Result.BR         := BR;
  Result.Tag        := Tag;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.InsString(s: String;
                                 Index, Tag: Integer;
                                 ParaStyle, FontStyle: Integer;
                                 SameAsPrev, BR: Boolean;
                                 var FullReformat: Boolean): TCustomRVItemInfo;
begin
  Result := CreateTextItem(Tag, ParaStyle, FontStyle, SameAsPrev, BR);
  Do_InsertItem(Index, s, Result, False, FullReformat);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.InsString2(s: String; Index, Tag: Integer; Source: TCustomRVItemInfo; SameAsPrev, BR: Boolean;
                                  var FullReformat: Boolean): TCustomRVItemInfo;
begin
  Result := CreateTextItem2(Tag, Source, SameAsPrev, BR);
  Do_InsertItem(Index, s, Result, False, FullReformat);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.InsEmptyString(Index, Tag, ParaStyle, FontStyle: Integer; SameAsPrev, BR: Boolean): TCustomRVItemInfo;
var FullReformat: Boolean;
begin
  Result := InsString('', Index, Tag, ParaStyle, FontStyle, SameAsPrev, BR, FullReformat);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.CanDelete: Boolean;
var i, StyleNo: Integer;
    StartNo, EndNo, StartOffs, EndOffs: Integer;
    {................................................}
    function CompletelySelected(a,b, ItemNo: Integer): Boolean;
    begin
      Result := (a<=GetOffsBeforeItem(ItemNo)) and
                (b>=GetOffsAfterItem(ItemNo));
    end;
    {................................................}
    function NotSelected(a,b, ItemNo: Integer): Boolean;
    begin
      Result := (a=b) or (b<=GetOffsBeforeItem(ItemNo)) or
                (a>=GetOffsAfterItem(ItemNo));
    end;
    {................................................}
    function CD(a,b, ItemNo, StyleNo: Integer): Boolean;
    var Protection : TRVProtectOptions;
    begin
      if NotSelected(a, b, ItemNo) then
        Result := True
      else begin
        if StyleNo<0 then begin
          Result := not IsProtected(ItemNo, rvprDeleteProtect);
          end
        else begin
          Protection := GetRVStyle.TextStyles[StyleNo].Protection;
          Result := False;
          if (rvprDeleteProtect in Protection) and
             CompletelySelected(a,b, ItemNo) then exit;
          if (rvprModifyProtect in Protection) and
             not CompletelySelected(a,b, ItemNo) then exit;
          Result := True;
        end;
      end;
    end;
    {................................................}
    // Searching "persistent" checkpoints in the selected paragraphs
    // (not including the first one)
    function HasNonFirstPersistentCheckpoints: Boolean;
    var nonfirst: Boolean;
        i: Integer;
    begin
       Result := True;
       nonfirst := False;
       for i := StartNo+1 to EndNo do begin
         if not nonfirst and IsParaStart(i) then
           nonfirst := True;
         if nonfirst and ItemHasPersistentCheckpoint(i) then
           exit;
       end;
       if nonfirst then
         for i := EndNo+1 to ItemCount-1 do begin
           if IsParaStart(i) then
             break;
           if ItemHasPersistentCheckpoint(i) then
             exit;
         end;
       Result := False;
    end;
    {................................................}
begin
  if not SelectionExists(True, False) then begin
    if FPartialSelectedItem<>nil then
      Result := FPartialSelectedItem.CanDeletePartiallySelected
    else
      Result := True;
    exit;
  end;
  {$IFNDEF RVDONOTUSEINPLACE}
  if (GetChosenRVData<>nil) then begin
    if (GetChosenRVData is TRVEditRVData) then
      Result := TRVEditRVData(GetChosenRVData).CanDelete
    else
      Result := False;
    exit;
  end;
  {$ENDIF}
  Result := False;
  StoreSelBounds(StartNo, EndNo, StartOffs, EndOffs, True);
  for i := StartNo+1 to EndNo-1 do begin
    if IsProtected(i, rvprDeleteProtect) then
      exit;
  end;
  if (StartNo<>EndNo) then begin
    StyleNo := GetItemStyle(StartNo);
    if not CD(StartOffs, GetOffsAfterItem(StartNo), StartNo, StyleNo) then exit;
    StyleNo := GetItemStyle(EndNo);
    if not CD(GetOffsBeforeItem(EndNo), EndOffs, EndNo, StyleNo) then exit;
    end
  else begin
    StyleNo := GetItemStyle(StartNo);
    if not CD(StartOffs, EndOffs, StartNo, StyleNo) then exit;
  end;
  if IsItemParaProtected(StartNo) and
       (not IsParaStart(StartNo) or
        (StartOffs>GetOffsBeforeItem(StartNo))) then
      exit;
  if IsItemParaProtected(EndNo) and
       ((EndOffs<GetOffsAfterItem(EndNo)) or
        ((EndNo<Items.Count-1) and
         not IsParaStart(EndNo+1))
        ) then
      exit;
  if IsItemParaProtected(StartNo) and (StartNo=EndNo) and (StartOffs=EndOffs) then
    exit;
  Result := not IsParaProtected(FCurParaStyleNo,rvpaoReadOnly) and
    not HasNonFirstPersistentCheckpoints;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Reformat_(FullFormat: Boolean; StartDrawItem, EndDrawItem,
  ItemsAdded: Integer);
begin
  if FullFormat and (DocumentWidth<>CalculateMinDocWidthPlus(nil,nil)) then begin
    Format_(False, True, False, 0, GetCanvas, False, False);
    Invalidate;
    end
  else
    FormatParasExact(StartDrawItem, EndDrawItem, ItemsAdded, False);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.DeleteSelection_: Boolean;
var  StartNo, EndNo, StartOffs, EndOffs, TagForEmptyItem: Integer;
     {$IFNDEF RVDONOTUSEDRAGDROP}
     DragDropCaretInfo: TRVDragDropCaretInfo;
     {$ENDIF}
    {........................................................}
    // Searching the checkpoint with "persistent" flag in the
    // first selected paragraph; if found, moving it to the
    // first selected item or (if this item is not the first
    // in paragraph and selected completely) to the item
    // before the first selected one
    procedure MovePersistentCP;
    var i: Integer;
        CPItemNo: Integer;
        MultiParagraphSelection: Boolean;
    begin
      CPItemNo := -1;
      MultiParagraphSelection := False;
      if ItemHasPersistentCheckpoint(StartNo) and
         (StartOffs<GetOffsAfterItem(StartNo)) then
        CPItemNo := StartNo
      else begin
        for i := StartNo+1 to EndNo-1 do begin
          if IsParaStart(i) then begin
            MultiParagraphSelection := True;
            break;
          end;
          if ItemHasPersistentCheckpoint(i) then begin
            CPItemNo := i;
            break
          end;
        end;
        if not MultiParagraphSelection and (EndNo>StartNo) and
           not IsParaStart(EndNo) and (EndOffs>=GetOffsAfterItem(EndNo)) and
           ItemHasPersistentCheckpoint(EndNo) then
          CPItemNo := EndNo;
      end;
      if CPItemNo>=0 then begin
        if (StartOffs<=GetOffsBeforeItem(StartNo)) and
           (StartNo>0) and not IsParaStart(StartNo) then
          i := StartNo-1
        else
          i := StartNo;
        if i<CPItemNo then
          Do_MoveCP(CPItemNo, i);
      end;
    end;
    {........................................................}
    // Deleting selection consisting of multiple items.
    // Step 1: Deleting from last item (which is selected completely or partially)
    procedure DelSel_DelFromLastItem(var FullReformat, ShouldAddEmpty,
      SubstringDeleted: Boolean; var ItemsDeleted, DeletedWidth: Integer);
    var FR: Boolean;
    begin
      DeletedWidth := CalculateMinItemWidthPlusEx(EndNo);
      SubstringDeleted := False;
      FR := False;
      if EndOffs>=GetOffsAfterItem(EndNo) then begin
        // last item is selected completely - deleting it
        ShouldAddEmpty := (EndNo=Items.Count-1) or
                          not GetItem(EndNo+1).SameAsPrev;
        Do_DeleteItem(EndNo,FR);
        FullReformat := FullReformat or FR;
        inc(ItemsDeleted);
        {$IFNDEF RVDONOTUSEDRAGDROP}
        if (DragDropCaretInfo<>nil) then begin
          if DragDropCaretInfo.ItemNo=EndNo then
            DragDropCaretInfo.ItemOffs := GetOffsAfterItem(EndNo-1);
          dec(DragDropCaretInfo.ItemNo);
        end;
        {$ENDIF}
        end
      else begin
        // deleting substring from last item }
        if GetItemStyle(EndNo)>=0 then
          Do_DeleteSubstring(EndNo, 1, EndOffs-1);
        {$IFNDEF RVDONOTUSEDRAGDROP}
        if (DragDropCaretInfo<>nil) and (DragDropCaretInfo.ItemNo=EndNo) then
          dec(DragDropCaretInfo.ItemOffs, EndOffs-1);
        {$ENDIF}
        SubstringDeleted := True;
        ShouldAddEmpty := False;
      end;
    end;
    {........................................................}
    // Deleting selection consisting of multiple items.
    // Step 2: Deleting all items except from the first and the last one
    procedure DelSel_DelMiddleItems(var FullReformat: Boolean;
                                    var ItemsDeleted, DeletedWidth: Integer);
    var DeletedWidth1: Integer;
        FR: Boolean;
    begin
      FR := False;
      DeletedWidth1 := CalculateMinItemsWidthPlusEx(StartNo+1,EndNo-1);
      if DeletedWidth<DeletedWidth1 then
        DeletedWidth := DeletedWidth1;
      inc(ItemsDeleted,EndNo-StartNo-1);
      Do_DeleteItems(StartNo+1,EndNo-1,FR);
      {$IFNDEF RVDONOTUSEDRAGDROP}
      if (DragDropCaretInfo<>nil) then begin
        if (DragDropCaretInfo.ItemNo=EndNo-1) and (EndNo-StartNo-1>0) then
          DragDropCaretInfo.ItemOffs := GetOffsAfterItem(StartNo);
        dec(DragDropCaretInfo.ItemNo, EndNo-StartNo-1);
      end;
      {$ENDIF}
      FullReformat := FullReformat or FR;
    end;
    {........................................................}
    // Deleting selection consisting of multiple items.
    // Step 3: Deleting from the first item (which is selected completely or partially)
    // or
    // Deleting selection consisting of one item.
    procedure DelSel_DelFromFirstItem(StartOffs, EndOffs: Integer;
      var FullReformat, ShouldAddEmpty, SubstringDeleted: Boolean;
      var ItemsDeleted, DeletedWidth, ItemNo, ItemOffs: Integer);
    var item: TCustomRVItemInfo;
        FR, DeletedBR, DeletedSAP, NewPara: Boolean;
        DeletedWidth1, ParaNo: Integer;
        CPForEmptyString: TRVCPInfo;
    begin
      CPForEmptyString := nil;
      DeletedWidth1 := CalculateMinItemWidthPlusEx(StartNo);
      if DeletedWidth<DeletedWidth1 then
        DeletedWidth := DeletedWidth1;
      item := GetItem(StartNo);
      if (StartOffs<=GetOffsBeforeItem(StartNo)) and
         (EndOffs  >=GetOffsAfterItem(StartNo)) then begin
        // first item is selected completely
        NewPara := not item.SameAsPrev;
        if ShouldAddEmpty then begin
          if not NewPara then
            ShouldAddEmpty := False;
//          if (ItemCount>1) and (GetRVStyle.SelectionMode=rvsmParagraph) then
//            ShouldAddEmpty := False;
          {$IFNDEF RVDONOTUSELISTS}
          if not ShouldAddEmpty and (StartNo>0) and (GetItemStyle(StartNo-1)=rvsListMarker) then
            ShouldAddEmpty := True;
          {$ENDIF}
          if ShouldAddEmpty and ItemHasPersistentCheckpoint(StartNo) then
            CPForEmptyString := item.Checkpoint.CreateCopy(rvoTagsArePChars in Options);
        end;
        DeletedBR := item.BR;
        DeletedSAP := item.SameAsPrev;
        ParaNo    := item.ParaNo;
        if not ShouldAddEmpty then
          MovePersistentCheckpoint(StartNo, False);
        Do_DeleteItem(StartNo,FR);
        inc(ItemsDeleted);
        FullReformat := FullReformat or FR;
        if ShouldAddEmpty then begin
          InsEmptyString(StartNo, TagForEmptyItem, ParaNo, FCurTextStyleNo, DeletedSAP, DeletedBR);
          TagForEmptyItem := 0;
          if CPForEmptyString<>nil then
            Do_AddCP(StartNo, CPForEmptyString);
          dec(ItemsDeleted);
          ItemNo := StartNo;
          ItemOffs := 1;
          end
        else if NewPara and (StartNo<Items.Count)  then begin
          if not ((StartNo>0) and IsProtected(StartNo, rvprParaStartProtect)) then begin
            Do_NewLine(StartNo,False,-1,FR);
            FullReformat := FullReformat or FR;
            if DeletedBR then begin
              Do_BR(StartNo, True, FR);
              FullReformat := FullReformat or FR;
            end;
          end;
          ItemNo := StartNo;
          ItemOffs := GetOffsBeforeItem(ItemNo);
          end
        else begin
          ItemNo := StartNo-1;
          ItemOffs := GetOffsAfterItem(ItemNo);
          if (StartNo<Items.Count) and SubstringDeleted then begin
            Do_NewLine(StartNo,True,-1,FR);
            FullReformat := FullReformat or FR;
          end;
        end;
        {$IFNDEF RVDONOTUSEDRAGDROP}
        if (DragDropCaretInfo<>nil) then begin
          if DragDropCaretInfo.ItemNo=StartNo then begin
            DragDropCaretInfo.ItemNo := ItemNo;
            DragDropCaretInfo.ItemOffs := ItemOffs;
            end
          else if not ShouldAddEmpty then
            dec(DragDropCaretInfo.ItemNo);
        end;
        {$ENDIF}
        end
      else begin
        // deleting substring from the first item
        if GetItemStyle(StartNo)>=0 then begin
          Do_DeleteSubstring(StartNo, StartOffs, EndOffs-StartOffs);
          {$IFNDEF RVDONOTUSEDRAGDROP}
          if (DragDropCaretInfo<>nil) then begin
            if DragDropCaretInfo.ItemNo=StartNo then
              dec(DragDropCaretInfo.ItemOffs, EndOffs-StartOffs);
          end;
          {$ENDIF}
        end;
        ItemNo := StartNo;
        ItemOffs := StartOffs;
        if (StartNo+1<Items.Count) and SubstringDeleted then begin
          Do_NewLine(StartNo+1,True,-1,FR);
          FullReformat := FullReformat or FR;
        end;
      end;
    end;
    {........................................................}
var DIStartNo, DIEndNo, DIStartOffs, DIEndOffs: Integer;
    ItemsDeleted, DeletedWidth, ACaretItemNo, ACaretOffs: Integer;
    FullReformat, ShouldAddEmpty, SubstringDeleted: Boolean;
    {$IFNDEF RVDONOTUSELISTS}
    M_FirstItemNo, M_LastMarkerIndex: Integer;
    M_ListNos: TRVIntegerList;
    {$ENDIF}
begin
  Result := True;
  if not SelectionExists(True, False) then begin
    if (FPartialSelectedItem<>nil) then begin
      Result := FPartialSelectedItem.CanDeletePartiallySelected;
      if Result then
        FPartialSelectedItem.DeletePartiallySelected
      else
        Beep;
      end
    else
      Result := True;
    exit;
  end;
  if not CanDelete or (GetChosenRVData<>nil) then begin
    Beep;
    Result := False;
    exit;
  end;
  {$IFNDEF RVDONOTUSEDRAGDROP}
  DragDropCaretInfo := GetDragDropCaretInfo;
  if DragDropCaretInfo<>nil then
    if DragDropCaretInfo.RVData<>Self then
      DragDropCaretInfo := nil
    else begin
      if Item_InsideSelection(DragDropCaretInfo.ItemNo, DragDropCaretInfo.ItemOffs) then begin
        Beep;
        Result := False;
        exit;
      end;
    end;
  {$ENDIF}
  BeginUndoSequence(rvutDelete, True);
  GetSelBounds(DIStartNo, DIEndNo, DIStartOffs, DIEndOffs, True);
  GetParaBounds(DIStartNo,DIEndNo,DIStartNo,DIEndNo);
  StoreSelBounds(StartNo, EndNo, StartOffs, EndOffs, False);
  if FCurTextStyleNo = GetItemStyle(EndNo) then
    TagForEmptyItem := RV_CopyTag(GetItemTag(EndNo), rvoTagsArePChars in Options)
  else
    TagForEmptyItem := 0;
  StoreSelBounds(StartNo, EndNo, StartOffs, EndOffs, True);
  {$IFNDEF RVDONOTUSEDRAGDROP}
  if (DragDropCaretInfo<>nil) and
    ((DragDropCaretInfo.ItemNo<EndNo) or
    ((DragDropCaretInfo.ItemNo=EndNo) and
     (DragDropCaretInfo.ItemOffs<=EndOffs))) then
    DragDropCaretInfo := nil;
  {$ENDIF}
  Deselect(nil, False);

  if (StartNo>0) and GetItem(StartNo).SameAsPrev and
     (StartOffs<=GetOffsBeforeItem(StartNo)) then begin
     dec(StartNo);
     StartOffs := GetOffsAfterItem(StartNo);
  end;
  {$IFNDEF RVDONOTUSELISTS}
  AdjustMarkerPos(StartNo, StartOffs, True);
  AdjustMarkerPos(EndNo,   EndOffs, True);
  {$ENDIF}
  ItemsDeleted := 0;
  DeletedWidth := 0;
  ShouldAddEmpty := True;
  FullReformat := False;
  SubstringDeleted := False;
  {$IFNDEF RVDONOTUSELISTS}
  PrepareForUpdateRangeAfterMarkers(StartNo, EndNo, True, M_FirstItemNo, M_LastMarkerIndex, M_ListNos);
  {$ENDIF}
  MovePersistentCP;
  if StartNo=EndNo then begin
    // one item is selected
    ShouldAddEmpty := (EndNo+1>=Items.Count) or not GetItem(EndNo+1).SameAsPrev;
    DelSel_DelFromFirstItem(StartOffs, EndOffs, FullReformat, ShouldAddEmpty,
      SubstringDeleted, ItemsDeleted, DeletedWidth, ACaretItemNo, ACaretOffs)
    end
  else begin { delete selected itemS }
    DelSel_DelFromLastItem(FullReformat, ShouldAddEmpty, SubstringDeleted,
                           ItemsDeleted, DeletedWidth);
    DelSel_DelMiddleItems(FullReformat, ItemsDeleted, DeletedWidth);
    DelSel_DelFromFirstItem(StartOffs, GetOffsAfterItem(StartNo), FullReformat,
      ShouldAddEmpty, SubstringDeleted,  ItemsDeleted, DeletedWidth,
      ACaretItemNo, ACaretOffs);
  end;
  if (ACaretItemNo+1<Items.Count) and
    RV_CanConcateItems(ACaretItemNo, GetItem(ACaretItemNo), GetItem(ACaretItemNo+1), False) then begin
    {$IFNDEF RVDONOTUSEDRAGDROP}
    if (DragDropCaretInfo<>nil) then begin
      if DragDropCaretInfo.ItemNo=ACaretItemNo+1 then begin
        dec(DragDropCaretInfo.ItemNo);
        inc(DragDropCaretInfo.ItemOffs, ItemLength(ACaretItemNo));
        end
      else if DragDropCaretInfo.ItemNo>ACaretItemNo+1 then
        dec(DragDropCaretInfo.ItemNo);
    end;
    {$ENDIF}
    Do_Concate(ACaretItemNo);
    inc(ItemsDeleted);
  end;
  Reformat_(FullReformat or (DeletedWidth>=DocumentWidth), DIStartNo, DIEndNo, -ItemsDeleted);
  Item2DrawItem(ACaretItemNo, ACaretOffs, CaretDrawItemNo, CaretOffs);
  {$IFNDEF RVDONOTUSELISTS}
  AdjustMarkerCaret(True, CaretOffs);
  {$ENDIF}
  OnChangeCaretLine(CaretOffs-2);
  ChangeCaret(False,True,True,False);
  ApplyParaStyle(GetItemPara(ACaretItemNo), False);
  {$IFNDEF RVDONOTUSELISTS}
  if (M_ListNos<>nil) and (M_ListNos.Count>0) then
    UpdateAfterMarkers(GetFirstParaItem(StartNo), M_LastMarkerIndex, M_ListNos, -1);
  M_ListNos.Free;
  {$ENDIF}
  if (TagForEmptyItem<>0) and (rvoTagsArePChars in Options) then
    StrDispose(PChar(TagForEmptyItem));
  Change;
  DoSelect;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.OnEnterPress_(Shift: Boolean; Recursive: Boolean): Boolean;
    {................................................................}
    procedure UpdateCaretAtTheBeginningOfLine;
    begin
      CaretOffs := GetOffsBeforeDrawItem(CaretDrawItemNo);
      {$IFNDEF RVDONOTUSELISTS}
      AdjustMarkerCaret(True, CaretOffs);
      {$ENDIF}
      OnChangeCaretLine(CaretOffs-2);
      ChangeCaret(False,True,False,False);
    end;
    {................................................................}
    // Adding an empty line before or after the current paragraph
    function AddEmptyLine(dli: TRVDrawLineInfo): Boolean;
    var li, srcli: TCustomRVItemInfo;
        ItemsAdded, InsertPoint, SrcItemNo, Last: Integer;
        //FullReformat,
        FR: Boolean;
        s: String;
        {$IFNDEF RVDONOTUSELISTS}
        AddedMarkerItemNo : Integer;
        {$ENDIF}
    begin
      Result := False;
      //FullReformat := False;
      if IsProtected(dli.ItemNo, rvprParaStartProtect) then begin
        Beep;
        exit;
      end;
      Result := True;
      ItemsAdded := 1;
      {$IFNDEF RVDONOTUSELISTS}
      AddedMarkerItemNo := -1;
      {$ENDIF}
      srcli := GetItem(dli.ItemNo);
      {$IFNDEF RVDONOTUSELISTS}
      if not Shift and not Recursive and (srcli.StyleNo>=0) and
        (GetItemText(dli.ItemNo)='') and (dli.ItemNo>0) and
        (GetItemStyle(dli.ItemNo-1)=rvsListMarker) then begin
        OnBackSpacePress_(False, False, False);
        OnEnterPress_(Shift, True);
        exit;
      end;
      {$ENDIF}
      li := RichViewTextItemClass.Create(Self);
      if (CaretOffs=CharEnds.Count-1) and
         (srcli.StyleNo=FCurTextStyleNo) and
         (GetRVStyle.TextStyles[GetActualCurStyleNo].NextStyleNo<>-1) then
         SetCurTextStyleNo(GetRVStyle.TextStyles[GetActualCurStyleNo].NextStyleNo);
      if not Shift and ((dli.ItemNo+1>=Items.Count) or not GetItem(dli.ItemNo+1).BR) then
        if IsParaProtected(srcli.ParaNo,rvpaoReadOnly) or
           ((CaretOffs=CharEnds.Count-1) and
           (GetRVStyle.ParaStyles[srcli.ParaNo].NextParaNo<>-1)) then
          if GetRVStyle.ParaStyles[srcli.ParaNo].NextParaNo>=0 then
             SetCurParaStyleNo(GetRVStyle.ParaStyles[srcli.ParaNo].NextParaNo)
          else
             SetCurParaStyleNo(0);
      if rvprModifyProtect in GetRVStyle.TextStyles[GetActualCurStyleNo].Protection then
        li.StyleNo := 0
      else
        li.StyleNo := FCurTextStyleNo;
      {$IFNDEF RVDONOTUSEUNICODE}
      if GetRVStyle.TextStyles[li.StyleNo].Unicode then
        Include(li.ItemOptions,rvioUnicode);
      {$ENDIF}
      li.ParaNo     := FCurParaStyleNo;
      li.SameAsPrev := False;
      if Shift then begin
        li.ParaNo := GetItemPara(dli.ItemNo);
        if (CaretOffs>0) or (CharEnds.Count=1) then // inserting after...
          li.BR := True
        else begin// inserting before...
          li.BR := GetItem(dli.ItemNo).BR;
          Do_BR(dli.ItemNo,True,FR);
          //FullReformat := FullReformat or FR;
        end;
      end;
      if GetItemStyle(dli.ItemNo)=li.StyleNo then begin
        li.Tag := RV_CopyTag(GetItem(dli.ItemNo).Tag, rvoTagsArePChars in Options);
        {$IFNDEF RVDONOTUSEITEMHINTS}
        li.Hint := GetItem(dli.ItemNo).Hint;
        {$ENDIF}
      end;
      if CaretOffs=CharEnds.Count-1 then begin
        InsertPoint := dli.ItemNo+1;
        SrcItemNo   := dli.ItemNo;
        {$IFNDEF RVDONOTUSELISTS}
        if not Shift and (li.ParaNo=srcli.ParaNo) and
           ReplicateMarker(dli.ItemNo, InsertPoint, FR,True) then begin
          //FullReformat := FullReformat or FR;
          AddedMarkerItemNo := InsertPoint;
          inc(ItemsAdded);
          inc(InsertPoint);
          li.SameAsPrev := True;
        end;
        {$ENDIF}
        Last := InsertPoint;
        end
      else begin
        InsertPoint := dli.ItemNo;
        Last := InsertPoint+1;
        {$IFNDEF RVDONOTUSELISTS}
        if Shift then begin
          if GetItemStyle(GetFirstParaSectionItem(dli.ItemNo))=rvsListMarker then begin
            li.SameAsPrev := True;
            Do_NewLine(dli.ItemNo,False,-1, FR);
            //FullReformat := FullReformat or FR;
            Do_BR(dli.ItemNo, True, FR);
            //FullReformat := FullReformat or FR;
          end;
          end
        else begin
          if GetItemStyle(GetFirstParaSectionItem(dli.ItemNo))=rvsListMarker then
            li.SameAsPrev := True;
          if ReplicateMarker(dli.ItemNo, dli.ItemNo, FR, True) then begin
            //FullReformat := FullReformat or FR;
            AddedMarkerItemNo := dli.ItemNo+1;
            Do_NewLine(dli.ItemNo+1,True,-1,FR);
            //FullReformat := FullReformat or FR;
            inc(ItemsAdded);
            inc(Last);
          end;
        end;
        {$ENDIF}
        SrcItemNo := Last;
      end;
      s := '';
      Do_InsertItem(InsertPoint, s, li, False, FR);
      if InsertPoint<SrcItemNo then
        Do_MoveCP(SrcItemNo, InsertPoint);
      // FullReformat := FullReformat or FR;
      FormatParas(CharEnds.Items[0].DrawItemNo, CharEnds.Items[CharEnds.Count-1].DrawItemNo, ItemsAdded);
      Item2FirstDrawItem(Last, CaretDrawItemNo);
      UpdateCaretAtTheBeginningOfLine;
      {$IFNDEF RVDONOTUSELISTS}
      UpdateRangeAfterMarkers(AddedMarkerItemNo,AddedMarkerItemNo);
      {$ENDIF}
    end;
    {................................................................}
    // Moving the item to the right of caret to the new line
    // (or adding a marker instead)
    // This procedure is rarely executed, usually the item
    // to the right of the caret is not a current
    function WrapCurrentItem(dli: TRVDrawLineInfo; OldWidth: Integer;
      NoWrap: Boolean): Boolean;
    var PItem1, PItem2, Last, ItemsAdded: Integer;
        FullReformat, FR: Boolean;
        {$IFNDEF RVDONOTUSELISTS}
        AddedMarkerItemNo : Integer;
        {$ENDIF}
    begin
      Result := False;
      if IsProtected(dli.ItemNo, rvprParaStartProtect) then begin
        Beep;
        exit;
      end;
      Result := True;
      {$IFNDEF RVDONOTUSELISTS}
      AddedMarkerItemNo := -1;
      {$ENDIF}
      FullReformat := False;
      GetParaBounds(CharEnds.Items[0].DrawItemNo,
                   CharEnds.Items[CharEnds.Count-1].DrawItemNo,
                   PItem1, PItem2);
      ItemsAdded := 0;
      if Shift then begin
        Do_NewLine(dli.ItemNo,False,-1, FR);
        FullReformat := FullReformat or FR;
        Do_BR(dli.ItemNo,True, FR);
        FullReformat := FullReformat or FR;
        end
      else begin
       {$IFNDEF RVDONOTUSELISTS}
       if ReplicateMarker(dli.ItemNo-1, dli.ItemNo, FR, True) then begin
         AddedMarkerItemNo := dli.ItemNo;
         FullReformat := FullReformat or FR;
         inc(ItemsAdded);
         end
       else
       {$ENDIF}
       begin
         Do_NewLine(dli.ItemNo,False,-1, FR);
         FullReformat := FullReformat or FR;
       end;
      end;
      Last := dli.ItemNo;
      Reformat_((NoWrap and (OldWidth>=DocumentWidth)) or FullReformat, PItem1, PItem2,ItemsAdded);
      Item2FirstDrawItem(Last, CaretDrawItemNo);
      UpdateCaretAtTheBeginningOfLine;
      {$IFNDEF RVDONOTUSELISTS}
      UpdateRangeAfterMarkers(AddedMarkerItemNo,AddedMarkerItemNo);
      {$ENDIF}
    end;
    {................................................................}
    // Moving the item to the right of caret to the new line
    // (or adding a marker instead)
    function WrapNextItem(dli: TRVDrawLineInfo; OldWidth: Integer;
      NoWrap: Boolean): Boolean;
    var PItem1, PItem2, Last, ItemsAdded: Integer;
        FullReformat, FR: Boolean;
        {$IFNDEF RVDONOTUSELISTS}
        AddedMarkerItemNo : Integer;
        {$ENDIF}
    begin
      Result := False;
      GetParaBounds(CharEnds.Items[0].DrawItemNo,
                   CharEnds.Items[CharEnds.Count-1].DrawItemNo,
                   PItem1, PItem2);
      if IsProtected(dli.ItemNo+1, rvprParaStartProtect) then begin
         Beep;
         exit;
      end;
      Result := True;
      {$IFNDEF RVDONOTUSELISTS}
      AddedMarkerItemNo := -1;
      {$ENDIF}
      FullReformat := False;
      Last := dli.ItemNo+1;
      ItemsAdded := 0;
      if Shift then begin
         Do_NewLine(Last,False,-1, FR);
         FullReformat := FullReformat or FR;
         Do_BR(Last,True,FR);
         FullReformat := FullReformat or FR;
       end
      else begin
        {$IFNDEF RVDONOTUSELISTS}
        if ReplicateMarker(Last-1, Last, FR, True) then begin
          AddedMarkerItemNo := Last;
          FullReformat := FullReformat or FR;
          inc(ItemsAdded);
          end
        else
        {$ENDIF}
        begin
          Do_NewLine(Last,False,-1, FR);
          FullReformat := FullReformat or FR;
        end;
      end;
      inc(CaretDrawItemNo);
      if CaretDrawItemNo>PItem2 then
        PItem2 := CaretDrawItemNo;
      Reformat_((NoWrap and (OldWidth>=DocumentWidth)) or FullReformat, PItem1, PItem2,ItemsAdded);
      Item2FirstDrawItem(Last, CaretDrawItemNo);
      UpdateCaretAtTheBeginningOfLine;
      {$IFNDEF RVDONOTUSELISTS}
      UpdateRangeAfterMarkers(AddedMarkerItemNo,AddedMarkerItemNo);
      {$ENDIF}
    end;
    {................................................................}
    // Breaking a text line (removing part of text after caret,
    // adding it as a new item from new line; adding marker if needbe)
    function BreakTextItem(dli: TRVDrawLineInfo; OldWidth: Integer;
      NoWrap: Boolean): Boolean;
    var PItem1, PItem2, InsertPoint, ItemsAdded: Integer;
        li: TCustomRVItemInfo;
        FullReformat1, FullReformat2: Boolean;
        s: String;
        {$IFNDEF RVDONOTUSELISTS}
        AddedMarkerItemNo : Integer;
        {$ENDIF}
    begin
      Result := False;
      if IsProtected(dli.ItemNo, rvprParaStartProtect) then begin
        Beep;
        exit;
      end;
      {$IFNDEF RVDONOTUSELISTS}
      AddedMarkerItemNo := -1;
      {$ENDIF}
      with CharEnds.Items[CaretOffs] do begin
        if IsProtected(dli.ItemNo, rvprModifyProtect) then begin
          Beep;
          exit;
        end;
        Result := True;
        GetParaBounds(CharEnds.Items[0].DrawItemNo,
                       CharEnds.Items[CharEnds.Count-1].DrawItemNo,
                       PItem1, PItem2);
        InsertPoint := dli.ItemNo+1;
        s := RVU_Copy(Items[dli.ItemNo], dli.Offs+Offset-1,
                      RVU_Length(Items[dli.ItemNo], GetItemOptions(dli.ItemNo)),
                      GetItemOptions(dli.ItemNo));
        Do_DeleteSubstring(dli.ItemNo, dli.Offs+Offset-1,-1);
        ItemsAdded := 1;
        FullReformat1 := False;
        FullReformat2 := False;
        li := RV_CreateTextCopy(TRVTextItemInfo(Items.Objects[dli.ItemNo]), Self);
        li.SameAsPrev := False;
        if Shift then
          li.BR := True
        else begin
           {$IFNDEF RVDONOTUSELISTS}
           if ReplicateMarker(InsertPoint-1, InsertPoint, FullReformat1, True) then begin
             AddedMarkerItemNo := InsertPoint;
             inc(ItemsAdded);
             inc(InsertPoint);
             li.SameAsPrev := True
           end;
           {$ENDIF}
        end;
        Do_InsertItem(InsertPoint, s, li, False, FullReformat2);
        Reformat_((NoWrap and (OldWidth>=DocumentWidth)) or FullReformat1 or FullReformat2, PItem1, PItem2,ItemsAdded);
        Item2FirstDrawItem(InsertPoint,CaretDrawItemNo);
        UpdateCaretAtTheBeginningOfLine;
        {$IFNDEF RVDONOTUSELISTS}
        UpdateRangeAfterMarkers(AddedMarkerItemNo,AddedMarkerItemNo);
        {$ENDIF}
      end;
    end;
    {................................................................}
var dli: TRVDrawLineInfo;
    li: TCustomRVItemInfo;
    OldWidth: Integer;
    NoWrap: Boolean;
begin
  Result := False;
  CaretDrawItemNo := CharEnds.Items[CaretOffs].DrawItemNo;
  dli := DrawItems[CaretDrawItemNo];
  li  := GetItem(dli.ItemNo);
  if IsParaProtected(li.ParaNo,rvpaoDoNotWantReturns) then begin
    Beep;
    exit;
  end;
  BeginUndoSequence(rvutMiscTyping, True);

  if CaretAtTheBeginningOfParaSection or CaretAtTheEndOfParaSection then begin
    Result := AddEmptyLine(dli);
    exit;
  end;

  if IsParaProtected(li.ParaNo,rvpaoReadOnly) then begin
    Result := False;
    Beep;
    exit;
  end;

  { breaking line }
  OldWidth := CalculateParaSectionMinWidthDef(dli.ItemNo);
  NoWrap := rvpaoNoWrap in GetRVStyle.ParaStyles[li.ParaNo].Options;

  if ((li.StyleNo<0) and (CharEnds.Items[CaretOffs].Offset=0)) or
     ((li.StyleNo>=0) and (dli.Offs+CharEnds.Items[CaretOffs].Offset-1<=GetOffsBeforeItem(dli.ItemNo))) then begin
     Result := WrapCurrentItem(dli, OldWidth, NoWrap);
     exit;
  end;

  if (li.StyleNo<0) or
     (ItemLength(dli.ItemNo)<dli.Offs+CharEnds.Items[CaretOffs].Offset-1) then begin
     Result := WrapNextItem(dli, OldWidth, NoWrap);
     exit;
  end;
  Result := BreakTextItem(dli, OldWidth, NoWrap);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.OnChangeCaretLine(DLOffs: Integer);
var First,Last,i: Integer;
    Canvas: TCanvas;
const
    VARGE_VALUE = 100000;
    {........................................................}
    procedure SimpleBuildCaretPositions(Canvas: TCanvas; i: Integer);
    var
      item: TCustomRVItemInfo;
      ditem: TRVDrawLineInfo;
      arr: PRVIntegerArray;
      j, max: Integer;
      s: String;
      BiDiMode: TRVBiDiMode;
      {$IFNDEF RVDONOTUSEUNICODE}
      PStart, PCur: Pointer;
      prevj: Integer;
      {$ENDIF}
    begin
      ditem := DrawItems[i];
      item  := GetItem(ditem.ItemNo);
      BiDiMode := GetParaBiDiMode(item.ParaNo);
      if (i=First) and ((item.StyleNo<0) or (Items[ditem.ItemNo]<>'')) then
        with TRVCharPos(CharEnds.Add) do begin
          if item.StyleNo<0 then
            Offset := 0
          else
            Offset := 1;
          if (item.StyleNo<0) and (BiDiMode=rvbdRightToLeft) then begin
            MoveRightTo := 2;
            X := ditem.Left+ditem.Width;
            end
          else begin
            MoveRightTo := 1;
            X := ditem.Left;
          end;
          DrawItemNo := i;
          if (i=CaretDrawItemNo) and (DLOffs<0) then
            CaretOffs := CharEnds.Count-1;
        end;
      max := 0;
      if (item.StyleNo>=0) and (Items[ditem.ItemNo]<>'') then begin
        GetRVStyle.ApplyStyle(Canvas, item.StyleNo, GetParaBiDiMode(item.ParaNo));
        s := DrawItems.GetString(i,Items);
        {$IFNDEF RVDONOTUSEALLCAPS}
        s := RV_ReturnCapitalized(s, GetRVStyle.TextStyles[item.StyleNo]);
        {$ENDIF}
        GetMem(arr, (ditem.Length+2)*sizeof(Integer));
        try
          RVU_GetTextExtentExPoint(Canvas, s,
                                   ditem.Width*10, max, arr, item.ItemOptions);
          {$IFNDEF RVDONOTUSEUNICODE}
          if RVNT and (rvioUnicode in item.ItemOptions) then begin
            SetLength(s, Length(s)+1);
            s[Length(s)] := #0;
            PStart := Pointer(s);
            PCur := PStart;
            prevj := 0;
            while PWord(PCur)^<>0 do begin
              with TRVCharPos(CharEnds.Add) do begin
                j := (PChar(PCur)-PChar(PStart)) div 2;
                Offset :=  j+2;
                MoveRightTo := 1;
                X := ditem.Left+arr[j]
                {$IFNDEF RVDONOTUSEJUSTIFY}+ditem.SpaceBefore{$ENDIF}
                ;
                DrawItemNo := i;
                if j>prevj+1 then begin
                  TRVCharPos(CharEnds.Items[CharEnds.Count-2]).X := ditem.Left+arr[j-1]
                  {$IFNDEF RVDONOTUSEJUSTIFY}+ditem.SpaceBefore{$ENDIF}
                  ;
                  TRVCharPos(CharEnds.Items[CharEnds.Count-2]).Offset :=  j-1+2;
                end;
                if (i=CaretDrawItemNo) then begin
                  if (j=DLOffs) then
                    CaretOffs := CharEnds.Count-1
                  else if (j>DLOffs) and (prevj<DLOffs) then
                    CaretOffs := CharEnds.Count-2;
                end;
                prevj := j;
              end;
              PCur := CharNextW(PCur);
            end;
            CharEnds.Items[CharEnds.Count-1].Free;
            end
          else
          {$ENDIF}
            for j := 0 to max-2 do
              with TRVCharPos(CharEnds.Add) do begin
                Offset := j+2;
                MoveRightTo := 1;
                X := ditem.Left+arr[j]
                {$IFNDEF RVDONOTUSEJUSTIFY}+ditem.SpaceBefore{$ENDIF}
                ;
                DrawItemNo := i;
                if (i=CaretDrawItemNo) and (j=DLOffs) then
                  CaretOffs := CharEnds.Count-1;
              end;
        finally
          FreeMem(arr);
        end;
      end;
      with TRVCharPos(CharEnds.Add) do begin
        DrawItemNo := i;
        Offset := GetOffsAfterDrawItem(DrawItemNo);
        if (item.StyleNo<0) and (BiDiMode=rvbdRightToLeft) then begin
          X := ditem.Left;
          MoveRightTo := 1;
          end
        else begin
          X := ditem.Left+ditem.Width;
          if item.StyleNo<0 then
            MoveRightTo := 2
          else
            MoveRightTo := 1;
        end;
        if (i=CaretDrawItemNo-1) and (DLOffs<0) or
           (i=CaretDrawItemNo) and (DLOffs>max-2) then
          CaretOffs := CharEnds.Count-1;
      end;
    end;
    {........................................................}
    function AdvancedBuildCaretPositions(Canvas: TCanvas; i: Integer): Boolean;
    var
      item: TCustomRVItemInfo;
      ditem: TRVDrawLineInfo;
      arr: PRVIntegerArray;
      strt, j: Integer;
      s: String;
      {$IFNDEF RVDONOTUSEUNICODE}
      PStart, PCur: Pointer;
      prevj: Integer;
      {$ENDIF}
    begin
      Result := True;
      ditem := DrawItems[i];
      item  := GetItem(ditem.ItemNo);
      if (item.StyleNo<0) or (Items[ditem.ItemNo]='') then begin
        SimpleBuildCaretPositions(Canvas, i);
        exit;
      end;
      s := DrawItems.GetString(i,Items);
      {$IFNDEF RVDONOTUSEALLCAPS}
      s := RV_ReturnCapitalized(s, GetRVStyle.TextStyles[item.StyleNo]);
      {$ENDIF}
      GetMem(arr, (ditem.Length+1)*sizeof(Integer));
      try
        GetRVStyle.ApplyStyle(Canvas, item.StyleNo, GetParaBiDiMode(item.ParaNo));
        Result := RVU_GetTextCaretPos(Canvas, s, arr, item.ItemOptions, ditem.Width-ditem.SpaceBefore);
        if Result then begin
          {$IFNDEF RVDONOTUSEUNICODE}
          if RVNT and (rvioUnicode in item.ItemOptions) then begin
            SetLength(s, Length(s)+1);
            s[Length(s)] := #0;
            PStart := Pointer(s);
            PCur := PStart;
            prevj := 0;
            if i>First then
              PCur := CharNextW(Pointer(PCur));
            while True do
              with TRVCharPos(CharEnds.Add) do begin
                j := (PChar(PCur)-PChar(PStart)) div 2;
                Offset := j+1;
                MoveRightTo := 0;
                X := ditem.Left+arr[j];
                {$IFNDEF RVDONOTUSEJUSTIFY}inc(X,ditem.SpaceBefore);{$ENDIF}
                DrawItemNo := i;
                if ((i=CaretDrawItemNo) and (DLOffs<0) and (j=0)) or
                   ((i=CaretDrawItemNo) and ((j-1=DLOffs))) or
                   ((i=CaretDrawItemNo-1) and (DLOffs<0) and (j=ditem.Length)) or
                   ((i=CaretDrawItemNo) and (DLOffs>ditem.Length-2) and (j=ditem.Length)) then
                  CaretOffs := CharEnds.Count-1
                else if (i=CaretDrawItemNo) and (CharEnds.Count>1) and (j-1>DLOffs) and (prevj-1<DLOffs) then
                  CaretOffs := CharEnds.Count-2;
                 prevj := j;
                if PWord(PCur)^=0 then
                  break;
                PCur := CharNextW(PCur);
              end;
            end
          else
          {$ENDIF}
          begin
            strt := 0;
            if i>First then
              inc(strt);
            for j := strt to ditem.Length do
              with TRVCharPos(CharEnds.Add) do begin
                Offset := j+1;
                MoveRightTo := 0;
                X := ditem.Left+arr[j];
                {$IFNDEF RVDONOTUSEJUSTIFY}inc(X,ditem.SpaceBefore);{$ENDIF}
                DrawItemNo := i;
                if ((i=CaretDrawItemNo) and (DLOffs<0) and (j=0)) or
                   ((i=CaretDrawItemNo) and (j-1=DLOffs)) or
                   ((i=CaretDrawItemNo-1) and (DLOffs<0) and (j=ditem.Length)) or
                   ((i=CaretDrawItemNo) and (DLOffs>ditem.Length-2) and (j=ditem.Length)) then
                  CaretOffs := CharEnds.Count-1;
              end;
          end;
        end;
      finally
        FreeMem(arr);
      end;
    end;
    {........................................................}
begin
  {$IFDEF RVDEBUG}{$I Debug\h.inc}{$ENDIF}
  Canvas := GetCanvas;
  CharEnds.Clear;
  if CaretDrawItemNo = -1 then exit;
  CaretOffs := VARGE_VALUE;
  GetScreenLineBounds(CaretDrawItemNo, First, Last);
  {$IFNDEF RVDONOTUSELISTS}
  if GetItemStyle(DrawItems[First].ItemNo)=rvsListMarker then begin

    if CaretDrawItemNo=First then begin
      inc(CaretDrawItemNo);
      DLOffs := GetOffsBeforeDrawItem(CaretDrawItemNo)-2;
    end;
  
    inc(First);
  end;
  {$ENDIF}
  for i := First to Last do
    if (GetItemBiDiMode(DrawItems[i].ItemNo)=rvbdUnspecified) or not AdvancedBuildCaretPositions(Canvas, i) then
      SimpleBuildCaretPositions(Canvas, i);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.ChangeCaret(ForceCreate, ScrollToCaret, DontChangeStyle, RefreshBefore: Boolean);
var NewHeight: Integer;
    cx, cy: Integer;
    item: TCustomRVItemInfo;
    {...........................................................}
    procedure MakeHot(JumpIndex: Integer);
    begin
      if not (rvoNoCaretHighlightJumps in
        TCustomRichViewEdit(FRichView).EditorOptions) then begin
        LastJumpMovedAbove := JumpIndex;
        State := State + [rvstDrawHover];
        Invalidate;
      end;
    end;
    {...........................................................}
    procedure MakeCold;
    begin
      if rvstDrawHover in State then begin
        LastJumpMovedAbove := -1;
        State := State - [rvstDrawHover];
        Invalidate;
      end;
    end;
   {...........................................................}
   procedure SwitchStyle(item: TCustomRVItemInfo);
   begin
     if not DontChangeStyle then
       if rvprDoNotAutoSwitch in GetRVStyle.TextStyles[item.StyleNo].Protection then begin
         if not (rvprDoNotAutoSwitch in GetRVStyle.TextStyles[FPrevTextStyleNo].Protection) then
           SetCurTextStyleNo(FPrevTextStyleNo)
         end
       else begin
         SetCurTextStyleNo(item.StyleNo);
         FPrevTextStyleNo := item.StyleNo;
       end;
   end;
   {...........................................................}
begin
 if rvstSkipformatting in State then exit;
 if RefreshBefore then begin
   UpdateResizer;
   UpdateView;
 end;
 if (FPartialSelectedItem<>nil) or
    (TCustomRichView(FRichView).InplaceEditor<>nil) or
    (rvstCompletelySelected in State)
 then begin
   HideCaret(GetParentControl.Handle);
   TCustomRichViewEdit(FRichView).AfterCaretMove;
   exit;
 end;
 cx := 0; cy := 0; {<- avoiding warnings}
 if FResizer=nil then begin
   if CaretDrawItemNo=-1 then
     NewHeight := 0
   else begin
     if (CaretOffs<0) or (CaretOffs>=CharEnds.Count) then begin
       CaretDrawItemNo := 0;
       CaretOffs := 0;
       {$IFNDEF RVDONOTUSELISTS}
       AdjustMarkerCaret(True, CaretOffs);
       {$ENDIF}
       OnChangeCaretLine(CaretOffs-2);
       raise ERichViewError.Create(errCaretPosition);
     end;
     CaretDrawItemNo := CharEnds.Items[CaretOffs].DrawItemNo;
     NewHeight := DrawItems[CaretDrawItemNo].Height+2;
     if NewHeight>RichViewEditMaxCaretHeight then
       NewHeight := RichViewEditMaxCaretHeight;
     with CharEnds.Items[CaretOffs] do begin
       cx := X-1+MoveRightTo;
       cy := DrawItems[CaretDrawItemNo].Top-1;
     end;
   end;
   if (CaretHeight<>NewHeight) or ForceCreate or True {!!!} then begin
     if (CaretHeight<>0) and GetParentControl.Focused then begin
       HideCaret(GetParentControl.Handle);
       DestroyCaret;
     end;
     if (NewHeight<>0) and
        {$IFNDEF RVDONOTUSEDRAGDROP}
        (GetDragDropCaretInfo=nil) and
        {$ENDIF}
        not (TCustomRichViewEdit(FRichView).ReadOnly and
         (rvoHideReadOnlyCaret in TCustomRichViewEdit(FRichView).EditorOptions)) and
        GetParentControl.Focused then begin
       CreateCaret(GetParentControl.Handle, 0,
                  {MulDiv(}RichViewEditCaretWidth{,ZoomPercent,100)},
                  {MulDiv(}NewHeight+RichViewEditCaretHeightExtra*2{,ZoomPercent,100)});
       SetCaretPos({MulDiv(}cx-GetHOffs-RichViewEditCaretWidth div 2{, ZoomPercent,100)},
                   {MulDiv(}cy-GetVOffs+DrawItems[CaretDrawItemNo].Height+1-RichViewEditCaretHeightExtra-NewHeight{,ZoomPercent,100)});
       ShowCaret(GetParentControl.Handle);
     end;
     CaretHeight := NewHeight;
     end
   else begin
     if (NewHeight<>0) {and Focused} then begin
       if GetParentControl.Focused then SetCaretPos({MulDiv(}cx-GetHOffs-RichViewEditCaretWidth div 2{,ZoomPercent,100)},
                                                    {MulDiv(}cy-GetVOffs+DrawItems[CaretDrawItemNo].Height+1+RichViewEditCaretHeightExtra-NewHeight{,ZoomPercent,100)});
     end;
   end;
   end
 else begin
   HideCaret(GetParentControl.Handle);
   TCustomRichViewEdit(FRichView).AfterCaretMove;
 end;
 if (CaretDrawItemNo<>-1) then begin
   item := GetItem(DrawItems[CaretDrawItemNo].ItemNo);
   if not DontChangeStyle then
     SetCurParaStyleNo(item.ParaNo);
   if item.GetBoolValue(rvbpDisplayActiveState) then begin
     FActiveItem := item;
     Invalidate;
     end
   else
     if FActiveItem<>nil then begin
       FActiveItem := nil;
       invalidate;
     end;
   if (FSelStartNo>=0) and (FSelEndNo>=0) and
      ((FSelStartNo>FSelEndNo) or ((FSelStartNo=FSelEndNo)and(FSelStartOffs>FSelEndOffs))) and
      (FSelEndOffs>=GetOffsAfterDrawItem(FSelEndNo)) and (FSelEndNo+1<DrawItems.Count) and
      GetItem(DrawItems[FSelEndNo+1].ItemNo).SameAsPrev then
     item := GetItem(DrawItems[FSelEndNo+1].ItemNo);
   if (item.StyleNo>=0) then begin
     SwitchStyle(item);
     if item.GetBoolValueEx(rvbpHotColdJump, GetRVStyle) then
       MakeHot(item.JumpID)
     else
       MakeCold;
     end
   else begin
     if item.GetBoolValueEx(rvbpHotColdJump, GetRVStyle) then
       MakeHot(item.JumpID)
     else
       MakeCold;
   end;
 end;
  if (FResizer=nil) and ScrollToCaret and {Focused and} (CaretHeight<>0) and (CaptureMouseItem=nil) then begin { scrolling to caret pos }
    ShowRectangle(cx, cy, 5, CaretHeight);
  end;
  TCustomRichViewEdit(FRichView).AfterCaretMove;
end;
{------------------------------------------------------------------------------}
// Ctrl is not used
procedure TRVEditRVData.OnDeletePress_(Ctrl: Boolean; MovedFromLineEnd: Boolean);
    {.....................................................................}
    procedure AdjustCaret(SavedCaretNo, SavedCaretOffs: Integer; ApplyPara: Boolean);
    begin
      Item2DrawItem(SavedCaretNo, SavedCaretOffs, CaretDrawItemNo, SavedCaretOffs);
      {$IFNDEF RVDONOTUSELISTS}
      AdjustMarkerCaret(True, SavedCaretOffs);
      {$ENDIF}      
      OnChangeCaretLine(SavedCaretOffs-2);
      ChangeCaret(False,True,False,True);
      if ApplyPara then
        ApplyParaStyle(FCurParaStyleNo, False);
    end;
    {.....................................................................}
    // Deleting current empty string
    // This string can be at the position of caret (CaretToTheNext=True,
    // because caret will be moved to the next item after deleting)
    // or the next line (CaretToTheNext=False)
    // If (ItemNo-1)-th item is a marker, this function deletes it as well.
    procedure Del_DeleteEmptyString(ItemNo, First, Last: Integer;
      CaretToTheNext: Boolean);
    var FullReformat,FR: Boolean;
        ItemsAdded : Integer;
        SavedCaretNo, SavedCaretOffs: Integer;
        {$IFNDEF RVDONOTUSELISTS}
        M_FirstItemNo, M_MarkerIndex: Integer;
        M_ListNos: TRVIntegerList;
        {$ENDIF}
    begin
      if IsProtected(ItemNo, rvprDeleteProtect) or
        IsItemParaProtected(ItemNo) or
        ParaHasPersistentCheckpoint(ItemNo) then begin
        Beep;
        exit;
      end;
      {$IFNDEF RVDONOTUSELISTS}
      if (ItemNo>0) and (GetItemStyle(ItemNo-1)=rvsListMarker) and
         IsProtected(ItemNo-1, rvprDeleteProtect) then begin
        Beep;
        exit;
      end;
      {$ENDIF}
      FullReformat := False;
      if CaretToTheNext and (ItemNo<Items.Count-1) and not GetItem(ItemNo).BR then begin
        Do_BR(ItemNo+1,False,FR);
        FullReformat := FullReformat or FR;
      end;
      ItemsAdded := -1;
      {$IFNDEF RVDONOTUSELISTS}
      M_FirstItemNo := -1;
      M_MarkerIndex := -1;
      M_ListNos     := nil;
      if (ItemNo>0) and (GetItemStyle(ItemNo-1)=rvsListMarker) then begin
        {$IFNDEF RVDONOTUSELISTS}
        PrepareForUpdateRangeAfterMarkers(ItemNo-1, ItemNo-1, True, M_FirstItemNo, M_MarkerIndex, M_ListNos);
        {$ENDIF}
        Do_DeleteItem(ItemNo, FR);
        FullReformat := FullReformat or FR;
        dec(ItemsAdded);
        dec(ItemNo);
      end;
      {$ENDIF}
      Do_DeleteItem(ItemNo, FR);
      FullReformat := FullReformat or FR;
      if CaretToTheNext then begin
        SavedCaretNo := ItemNo;
        if SavedCaretNo=Items.Count then begin
          dec(SavedCaretNo);
          SavedCaretOffs := GetOffsAfterItem(SavedCaretNo);
          end
        else begin
          {$IFNDEF RVDONOTUSELISTS}
          if GetItemStyle(SavedCaretNo)=rvsListMarker then
            inc(SavedCaretNo);
          {$ENDIF}
          SavedCaretOffs := GetOffsBeforeItem(SavedCaretNo);
        end;
        end
      else begin
        SavedCaretNo := ItemNo-1;
        SavedCaretOffs := GetOffsAfterItem(SavedCaretNo);
      end;
      Reformat_(FullReformat, First, Last, ItemsAdded);
      AdjustCaret(SavedCaretNo, SavedCaretOffs, not CaretToTheNext);
      {$IFNDEF RVDONOTUSELISTS}
      if not FullReformat then
        UpdateAfterMarkers(M_FirstItemNo, M_MarkerIndex, M_ListNos, -1);
      {$ENDIF}
    end;
    {.................................................................}
    // Concatenating two text items into one
    // This function is called only if caret is at the end of line,
    // and the last item in this line and the first item in the next line
    // can be combined.
    procedure Del_ConcateItems(ItemNo, First, Last: Integer);
    var ItemsAdded, OldWidth : Integer;
        SavedCaretOffs: Integer;
    begin
      if IsItemParaProtected(ItemNo) or IsItemParaProtected(ItemNo+1) or
         IsProtected(ItemNo+1, rvprParaStartProtect) or
         ParaHasPersistentCheckpoint(ItemNo+1) then begin
        Beep;
        exit;
      end;
      OldWidth := CalculateParaSectionMinWidthDef(ItemNo);
      SavedCaretOffs := GetOffsAfterItem(ItemNo);
      Do_Concate(ItemNo);
      ItemsAdded := -1;
      Reformat_((OldWidth>=DocumentWidth), First, Last, ItemsAdded);
      AdjustCaret(ItemNo, SavedCaretOffs, True);
    end;
    {.................................................................}
    // Moving the next item to the end of current paragraph
    // This function is called only if caret is at the end of line
    // If next item is a marker, it is deleted
    procedure Del_MoveToThis(ItemNo, First, Last: Integer);
    var ItemsAdded : Integer;
        SavedCaretOffs: Integer;
        FullReformat1,FullReformat2: Boolean;
        {$IFNDEF RVDONOTUSELISTS}
        M_FirstItemNo, M_MarkerIndex: Integer;
        M_ListNos: TRVIntegerList;
        {$ENDIF}
    begin

      if IsItemParaProtected(ItemNo) or IsItemParaProtected(ItemNo+1) or
         IsProtected(ItemNo+1, rvprParaStartProtect) or
         ParaHasPersistentCheckpoint(ItemNo+1) then begin
        Beep;
        exit;
      end;
      {$IFNDEF RVDONOTUSELISTS}
      if (GetItemStyle(ItemNo+1)=rvsListMarker) and
        IsProtected(ItemNo+1, rvprDeleteProtect) then begin
        Beep;
        exit;        
      end;
      {$ENDIF}
      FullReformat2 := False;
      Do_NewLine(ItemNo+1, True,-1, FullReformat1);
      ItemsAdded := 0;
      SavedCaretOffs := GetOffsAfterItem(ItemNo);
      {$IFNDEF RVDONOTUSELISTS}
      M_FirstItemNo := -1;
      M_MarkerIndex := -1;
      M_ListNos     := nil;
      if GetItemStyle(ItemNo+1)=rvsListMarker then begin
        {$IFNDEF RVDONOTUSELISTS}
        PrepareForUpdateRangeAfterMarkers(ItemNo+1, ItemNo+1, True, M_FirstItemNo, M_MarkerIndex, M_ListNos);
        {$ENDIF}
        Do_DeleteItem(ItemNo+1, FullReformat2);
        dec(ItemsAdded);
        if (ItemNo+1<Items.Count) and
           RV_CanConcateItems(ItemNo, GetItem(ItemNo), GetItem(ItemNo+1), False) then begin
          Do_Concate(ItemNo);
          dec(ItemsAdded);
        end
      end;
      {$ENDIF}
      Reformat_(FullReformat1 or FullReformat2, First, Last, ItemsAdded);
      AdjustCaret(ItemNo, SavedCaretOffs, True);
      {$IFNDEF RVDONOTUSELISTS}
      if not (FullReformat1 or FullReformat2)  then
        UpdateAfterMarkers(M_FirstItemNo, M_MarkerIndex, M_ListNos, -1);
      {$ENDIF}
    end;
    {.................................................................}
    // Actions at the end of line.
    // This procedure is never called at the end of document, so next item
    // must exist.
    procedure Del_AtTheEndOfParaSection(ItemNo, First, Last: Integer);
    begin

     if (GetItemStyle(ItemNo)>=0) and (ItemLength(ItemNo)=0) then // delete current empty string
       Del_DeleteEmptyString(ItemNo, First, Last, True)
     else if (GetItemStyle(ItemNo+1)>=0) and
             (ItemLength(ItemNo+1)=0) then // delete next empty string
       Del_DeleteEmptyString(ItemNo+1, First, Last, False)
     else if RV_CanConcateItems(ItemNo, GetItem(ItemNo), GetItem(ItemNo+1), True) then
       Del_ConcateItems(ItemNo, First, Last)
     else // move next item to the previous line
       Del_MoveToThis(ItemNo, First, Last)
    end;
    {.................................................................}
    // This procedure is called when caret is not at the end of line (i.e. paragraph section)
    // It can be at the end of screen line, though
    procedure Del_AtTheMiddle(dli: TRVDrawLineInfo; First, Last: Integer);
    begin
      if (CaretOffs=CharEnds.Count-1) then begin // we are at the end of screen line, but not item
        if (dli.ItemNo <> DrawItems[CaretDrawItemNo+1].ItemNo) or
           (DrawItems[CaretDrawItemNo+1].Offs>dli.Offs+dli.Length) then begin
          inc(CaretDrawItemNo); // space character was "eaten" between screen Items
          OnChangeCaretLine(0);
          CaretOffs := 0;
          OnBackspacePress_(False, True, False);
          ChangeCaret(False,True,False,True);
          end
        else begin
          inc(CaretDrawItemNo);
          OnChangeCaretLine(0);
          CaretOffs := 0;
          OnDeletePress_(False, True);
          ChangeCaret(False,True,False,True);
       end;
       end
     else begin
       // "if" checks a special case when space char was "eaten" at the beginning of line
       if not (MovedFromLineEnd and
          (dli.ItemNo<>DrawItems[CaretDrawItemNo-1].ItemNo) and
          (dli.Offs>1)) then
         CaretOffs := CaretOffs+1;
       OnBackSpacePress_(False, True, False);
       ChangeCaret(False,True,False,True);
     end;
    end;
    {.................................................................}
var dli: TRVDrawLineInfo;
    First, Last: Integer;
begin
  CaretDrawItemNo := CharEnds.Items[CaretOffs].DrawItemNo;
  dli := DrawItems[CaretDrawItemNo];
  with Items.Objects[dli.ItemNo] as TCustomRVItemInfo do begin
    if IsParaProtected(ParaNo,rvpaoReadOnly) then begin
      Beep;
      exit;
    end;
    if (CaretOffs=CharEnds.Count-1) and
       (
        (StyleNo<0) or
        (dli.Offs+CharEnds.Items[CaretOffs].Offset-1>ItemLength(dli.ItemNo))
       ) then begin
       { we are at the end of item and screen line }
       if GetBoolValue(rvbpFullWidth) or
          (
          (dli.ItemNo+1<Items.Count) and
          (GetItem(dli.ItemNo+1).GetBoolValue(rvbpFullWidth)) and
          not ((StyleNo>=0) and (ItemLength(dli.ItemNo)=0))
          )
       then begin
         OnRightPress(False,False);
         exit;
       end;
       if dli.ItemNo=Items.Count-1 then begin
         if ((StyleNo>=0) and (ItemLength(dli.ItemNo)=0)) then
           OnBackSpacePress_(False,False,False);
         exit;
       end;
    end;
  end;
  First := CharEnds.Items[0].DrawItemNo;
  Last  := CharEnds.Items[CharEnds.Count-1].DrawItemNo;
  GetParaBounds(First,Last,First,Last);

  with GetItem(dli.ItemNo) do begin
    if (CaretOffs=CharEnds.Count-1) and
       ((StyleNo<0) or
        (dli.Offs+CharEnds.Items[CaretOffs].Offset-1>ItemLength(dli.ItemNo))) and
       (GetItem(dli.ItemNo+1).SameAsPrev)
       then begin { we at the end of screen line, item, but not paragraph }
      inc(CaretDrawItemNo);
      OnChangeCaretLine(0);
      CaretOffs := 0;
      OnDeletePress_(Ctrl, True);
      ChangeCaret(False,True,False,True);
      exit;
    end;
  end;

  BeginUndoSequence(rvutDelete, True);

  if (CaretOffs=CharEnds.Count-1) and
     ((GetItemStyle(dli.ItemNo)<0) or
      (dli.Offs+CharEnds.Items[CaretOffs].Offset-1>ItemLength(dli.ItemNo))) and
      not GetItem(dli.ItemNo+1).SameAsPrev then
      Del_AtTheEndOfParaSection(dli.ItemNo, First, Last)
    else
      Del_AtTheMiddle(dli, First, Last);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.OnBackSpacePress_(Ctrl: Boolean; MultiDelete, FromNextLine: Boolean);
    {.....................................................................}
    function GetPrevP(dli: TRVDrawLineInfo; CaretOffs: Integer): Integer;
    begin
      if DrawItems[CharEnds.Items[CaretOffs-1].DrawItemNo].ItemNo<>dli.ItemNo then
        Result := 0
      else
        Result := dli.Offs+CharEnds.Items[CaretOffs-1].Offset-2;
    end;
    {.....................................................................}
    function DeleteCount(s: String; p, ItemNo: Integer): Integer;
    var ItemOptions: TRVItemOptions;
    begin
      if Ctrl then begin
        ItemOptions := GetItemOptions(ItemNo);
        Result := 1;
        for p := p-1 downto 1 do begin
          if RVU_IsSpace(s, p, ItemOptions) and
             not RVU_IsSpace(s, p+1, ItemOptions) then break;
          inc(Result);
        end;
        end
      else if MultiDelete then
        Result := (p+1)-PrevCharStr(s,ItemNo,p+1)
      else
        Result := 1;
    end;
    {.....................................................................}
    procedure AdjustCaret(SavedCaretNo, SavedCaretOffs: Integer; ApplyPara: Boolean);
    begin
      Item2DrawItem(SavedCaretNo, SavedCaretOffs, CaretDrawItemNo, SavedCaretOffs);
      {$IFNDEF RVDONOTUSELISTS}
      AdjustMarkerCaret(True, SavedCaretOffs);
      {$ENDIF}
      OnChangeCaretLine(SavedCaretOffs-2);
      ChangeCaret(False,True,False,True);
      if ApplyPara then
        ApplyParaStyle(FCurParaStyleNo, False);
    end;
    {.....................................................................}
    // Deleting current empty line
    // This function is called only if this is an empty line without marker,
    // and this line is not the first
    procedure Backspace_DeleteCurrentEmptyString(dli: TRVDrawLineInfo; First, Last: Integer);
    var SavedCaretNo, SavedCaretOffs, ItemsAdded: Integer;
        FullReformat: Boolean;
    begin
      if IsProtected(dli.ItemNo, rvprDeleteProtect) or
         IsItemParaProtected(dli.ItemNo) or
         ItemHasPersistentCheckpoint(dli.ItemNo) then begin
        Beep;
        exit;
      end;
      SavedCaretNo := dli.ItemNo-1;
      SavedCaretOffs := GetOffsAfterItem(SavedCaretNo);
      Do_DeleteItem(dli.ItemNo, FullReformat);
      ItemsAdded := -1;
      Reformat_(FullReformat, First, Last, ItemsAdded);
      AdjustCaret(SavedCaretNo, SavedCaretOffs, True);
    end;
    {.....................................................................}
    // Deleting previous empty line
    // If this line is after marker, moving current item to this marker's paragraph
    // This function is called only if caret is at the beginning of line,
    // end previous line if finished by an empty text item
    procedure Backspace_DeletePrevEmptyString(dli: TRVDrawLineInfo; First, Last: Integer);
    var SavedCaretNo, SavedCaretOffs, ItemsAdded: Integer;
        FullReformat, FR: Boolean;
    begin
      if IsProtected(dli.ItemNo-1, rvprDeleteProtect) or
         IsItemParaProtected(dli.ItemNo-1) or
         IsItemParaProtected(dli.ItemNo) or
         ParaHasPersistentCheckpoint(dli.ItemNo-1) then begin
        Beep;
        exit;
      end;
      {$IFNDEF RVDONOTUSELISTS}
      if (dli.ItemNo>1) and (GetItemStyle(dli.ItemNo-2)=rvsListMarker) and
         ParaHasPersistentCheckpoint(dli.ItemNo) then begin
        Beep;
        exit;
      end;
      {$ENDIF}
      FullReformat := False;
      if not GetItem(dli.ItemNo-1).BR then begin
        Do_BR(dli.ItemNo, False, FR);
        FullReformat := FullReformat or FR;
      end;
      {$IFNDEF RVDONOTUSELISTS}
      if (dli.ItemNo>1) and (GetItemStyle(dli.ItemNo-2)=rvsListMarker) then begin
        Do_NewLine(dli.ItemNo, True, -1, FR);
        FullReformat := FullReformat or FR;
      end;
      {$ENDIF}
      SavedCaretNo := dli.ItemNo;
      SavedCaretOffs := GetOffsBeforeItem(SavedCaretNo);
      dec(SavedCaretNo);
      Do_DeleteItem(dli.ItemNo-1, FR);
      FullReformat := FullReformat or FR;
      ItemsAdded := -1;
      Reformat_(FullReformat, First, Last, ItemsAdded);
      AdjustCaret(SavedCaretNo, SavedCaretOffs, False);
    end;
    {.....................................................................}
    // Concatenating two text items into one
    // This function is called only if caret is at the beginning of line,
    // and the first item in this line and the last item in previous line
    // can be combined.
    procedure Backspace_ConcateItems(dli: TRVDrawLineInfo; First, Last: Integer);
    var SavedCaretNo, SavedCaretOffs, ItemsAdded: Integer;
    begin
      if IsItemParaProtected(dli.ItemNo-1) or IsItemParaProtected(dli.ItemNo) or
         IsProtected(dli.ItemNo, rvprParaStartProtect) or
         ParaHasPersistentCheckpoint(dli.ItemNo) then begin
        Beep;
        exit;
      end;
      SavedCaretOffs := ItemLength(dli.ItemNo-1)+1;
      Do_Concate(dli.ItemNo-1);
      ItemsAdded := -1;
      SavedCaretNo := dli.ItemNo-1;
      FormatParasExact(First, Last, ItemsAdded, False);
      AdjustCaret(SavedCaretNo, SavedCaretOffs, True);
    end;
    {.....................................................................}
    // Moving current item to the end of previous paragraph
    // This function is called only if caret is at the beginning of line
    procedure Backspace_MoveToPrev(dli: TRVDrawLineInfo; First, Last: Integer);
    var SavedCaretNo, SavedCaretOffs, ItemsAdded: Integer;
        FullReformat: Boolean;
    begin
      if IsProtected(dli.ItemNo, rvprParaStartProtect) or
         IsItemParaProtected(dli.ItemNo) or IsItemParaProtected(dli.ItemNo-1) or
         ParaHasPersistentCheckpoint(dli.ItemNo) then begin
        Beep;
        exit;
      end;
      Do_NewLine(dli.ItemNo, True,-1, FullReformat);
      ItemsAdded := 0;
      SavedCaretNo := dli.ItemNo;
      SavedCaretOffs := GetOffsBeforeItem(SavedCaretNo);
      Reformat_(FullReformat, First, Last, ItemsAdded);
      AdjustCaret(SavedCaretNo, SavedCaretOffs, True);
    end;
    {.....................................................................}
    // Deleting the ItemNo-th item completely.
    // May be called for current item or previous item (marker).
    procedure Backspace_DeleteItem(ItemNo, First, Last: Integer);
    var Prev, SavedCaretNo, SavedCaretOffs, ItemsAdded, DeletedWidth: Integer;
        FullReformat, FR: Boolean;
        ASameAsPrev, ABR, APB: Boolean;
        Item: TCustomRVItemInfo;
        {$IFNDEF RVDONOTUSELISTS}
        M_FirstItemNo, M_MarkerIndex: Integer;
        M_ListNos: TRVIntegerList;
        {$ENDIF}
    begin
      if IsItemParaProtected(ItemNo) or
         IsProtected(ItemNo, rvprDeleteProtect) or
         not MovePersistentCheckpoint(ItemNo, False) then begin
        Beep;
        exit;
      end;
      FullReformat := False;
      ItemsAdded := -1;
      Prev := ItemNo-1;
      if Prev>=0 then begin
        SavedCaretNo   := Prev;
        SavedCaretOffs := GetOffsAfterItem(Prev)
        end
      else begin
       SavedCaretNo    := -1; { avoiding warnings}
       SavedCaretOffs  := -1;
      end;
      Item := GetItem(ItemNo);
      {$IFNDEF RVDONOTUSELISTS}
      PrepareForUpdateRangeAfterMarkers(ItemNo, ItemNo, True, M_FirstItemNo, M_MarkerIndex, M_ListNos);
      {$ENDIF}
      DeletedWidth := CalculateMinItemWidthPlusEx(ItemNo);
      ASameAsPrev := Item.SameAsPrev;
      ABR         := Item.BR;
      APB         := Item.PageBreakBefore;
      Do_DeleteItem(ItemNo, FR);
      FullReformat := FullReformat or FR;
      if (ItemNo<Items.Count) and GetItem(ItemNo).SameAsPrev then
        if (not ASameAsPrev) or (ItemNo=0) then begin
          { move next item to the new line }
          Do_NewLine(ItemNo, False, -1, FR);
          FullReformat := FullReformat or FR;
          Do_BR(ItemNo, ABR, FR);
          FullReformat := FullReformat or FR;
          Do_PageBreak(ItemNo, APB);
          SavedCaretNo := ItemNo;
          SavedCaretOffs := GetOffsBeforeItem(ItemNo);
          end
        else begin
          if RV_CanConcateItems(ItemNo-1, GetItem(ItemNo-1), GetItem(ItemNo), False) then begin
            { concate items before and after deleted }
            Do_Concate(ItemNo-1);
            dec(ItemsAdded);
          end;
        end;
      Reformat_(FullReformat or (DeletedWidth>=DocumentWidth), First, Last, ItemsAdded);
      AdjustCaret(SavedCaretNo, SavedCaretOffs, False);
      {$IFNDEF RVDONOTUSELISTS}
      if not FullReformat then
        UpdateAfterMarkers(M_FirstItemNo, M_MarkerIndex, M_ListNos, -1);
      {$ENDIF}
    end;
    {.....................................................................}
    // Actions on backspace at the beginning of line
    procedure Backspace_AtTheBeginningOfParaSection(dli: TRVDrawLineInfo; First, Last: Integer);
    begin
      if PageBreaksBeforeItems[dli.ItemNo] then begin
        Do_PageBreak(dli.ItemNo, False);
        Invalidate;
        Change;
        end
      {$IFNDEF RVDONOTUSELISTS}
      else if (dli.ItemNo>0) and (GetItemStyle(dli.ItemNo-1)=rvsListMarker) then
        Backspace_DeleteItem(dli.ItemNo-1, First, Last)
      {$ENDIF}
      else if (GetItemStyle(dli.ItemNo)>=0) and (ItemLength(dli.ItemNo)=0) then
        BackSpace_DeleteCurrentEmptyString(dli, First, Last)
      else if (GetItemStyle(dli.ItemNo-1)>=0) and
              (ItemLength(dli.ItemNo-1)=0) then
        BackSpace_DeletePrevEmptyString(dli, First, Last)
      else if RV_CanConcateItems(dli.ItemNo-1, GetItem(dli.ItemNo-1), GetItem(dli.ItemNo), True) then
        BackSpace_ConcateItems(dli, First, Last)
      else
        Backspace_MoveToPrev(dli, First, Last);
    end;
    {.....................................................................}
    // Deleting the ItemNo-th item and inserting an empty text instead
    procedure Backspace_ReplaceWithEmptyText(ItemNo, First, Last: Integer);
    var SavedCaretNo, SavedCaretOffs, ItemsAdded, DeletedWidth: Integer;
        FullReformat1, FullReformat2: Boolean;
        s: String;
        TextItem, Item: TCustomRVItemInfo;
    begin
      if IsItemParaProtected(ItemNo) or
         IsProtected(ItemNo, rvprDeleteProtect) then begin
        Beep;
        exit;
      end;
      Item := GetItem(ItemNo);
      TextItem := CreateTextItem(0, FCurParaStyleNo, FCurTextStyleNo, Item.SameAsPrev, Item.BR);
      if Item.Checkpoint<>nil then begin
        TextItem.Checkpoint := Item.Checkpoint.CreateCopy(rvoTagsArePChars in Options);
        Do_DeleteCP(ItemNo);
      end;
      if Item.StyleNo>=0 then
        TextItem.Tag := RV_CopyTag(Item.Tag, rvoTagsArePChars in Options);
      ItemsAdded := 0;
      SavedCaretNo := ItemNo;
      SavedCaretOffs := 1;
      DeletedWidth := CalculateMinItemWidthPlusEx(ItemNo);
      Do_DeleteItem(ItemNo, FullReformat1);
      s := '';
      Do_InsertItem(ItemNo, s, TextItem, False, FullReformat2);
      Reformat_(FullReformat1 or FullReformat2 or (DeletedWidth>=DocumentWidth), First, Last, ItemsAdded);
      AdjustCaret(SavedCaretNo, SavedCaretOffs, False);
    end;
    {.....................................................................}
    // Deleting characters from item (but not item itself)
    procedure Backspace_DeleteSubstring(dli: TRVDrawLineInfo; First, Last, Offset: Integer);
    var DeletedWidth, SavedCaretNo, SavedCaretOffs, CD: Integer;
        s: String;
    begin
      if IsProtected(dli.ItemNo, rvprModifyProtect) then begin
        Beep;
        exit;
      end;
      DeletedWidth := CalculateParaSectionMinWidthDef(dli.ItemNo);
      s := Items[dli.ItemNo];
      SavedCaretOffs := dli.Offs+Offset-2;
      CD := DeleteCount(s, SavedCaretOffs, dli.ItemNo);
      SavedCaretOffs := SavedCaretOffs-CD+1;
      Do_DeleteSubstring(dli.ItemNo, SavedCaretOffs, CD);
      SavedCaretNo := dli.ItemNo;
      Reformat_((DeletedWidth>=DocumentWidth), First, Last, 0);
      AdjustCaret(SavedCaretNo, SavedCaretOffs, False);
    end;
    {.....................................................................}
    // Actions on backspace when caret is not at the beginning of line
    procedure Backspace_NotAtTheBeginning(dli: TRVDrawLineInfo; First, Last, Offset: Integer);
    var StyleNo: Integer;
    begin
      StyleNo := GetItemStyle(dli.ItemNo);
      if ((StyleNo<0) and (Offset=1))
         or
         ((StyleNo>=0) and
         (ItemLength(dli.ItemNo)= DeleteCount(Items[dli.ItemNo], dli.Offs+Offset-2, dli.ItemNo))) then begin
        if (not GetItem(dli.ItemNo).SameAsPrev
         {$IFNDEF RVDONOTUSELISTS}
           or ((dli.ItemNo>0) and (GetItemStyle(dli.ItemNo-1)=rvsListMarker))
         {$ENDIF}
           )
          and
          ((dli.ItemNo>=Items.Count-1) or (not GetItem(dli.ItemNo+1).SameAsPrev)) then
         Backspace_ReplaceWithEmptyText(dli.ItemNo, First, Last)
       else
         Backspace_DeleteItem(dli.ItemNo, First, Last)
       end
     else
       Backspace_DeleteSubstring(dli, First, Last, Offset);
    end;
    {.....................................................................}
var dli: TRVDrawLineInfo;
    First, Last, Offset: Integer;
begin
  CaretDrawItemNo := CharEnds.Items[CaretOffs].DrawItemNo;
  dli := DrawItems[CaretDrawItemNo];
  Offset := CharEnds.Items[CaretOffs].Offset;
  if FromNextLine and (CaretDrawItemNo+1<DrawItems.Count) then begin
    // may be space was eaten?
    if DrawItems[CaretDrawItemNo].ItemNo<>DrawItems[CaretDrawItemNo+1].ItemNo then
      Offset := GetOffsAfterItem(DrawItems[CaretDrawItemNo].ItemNo)-DrawItems[CaretDrawItemNo].Offs+1
  end;

  with GetItem(dli.ItemNo) do begin
    if IsParaProtected(ParaNo,rvpaoReadOnly) then begin
      Beep;
      exit;
    end;
    if (CaretOffs=0) and
       (
        (StyleNo<0) or
        (dli.Offs+CharEnds.Items[CaretOffs].Offset-1<=1)
       ) then begin
      if (dli.ItemNo=0) then
        exit;
      if GetBoolValue(rvbpFullWidth) or
        (GetItem(dli.ItemNo-1).GetBoolValue(rvbpFullWidth) and not ((StyleNo>=0) and (ItemLength(dli.ItemNo)=0))) then begin
        OnLeftPress(False,False);
        exit;
      end;
    end;
  end;
  First := CharEnds.Items[0].DrawItemNo;
  Last  := CharEnds.Items[CharEnds.Count-1].DrawItemNo;
  GetParaBounds(First,Last,First,Last);

  with GetItem(dli.ItemNo) do begin
    if (CaretOffs=0) and
       (SameAsPrev) and
       (((StyleNo<0) and (CharEnds.Items[CaretOffs].Offset=0)) or
        ((StyleNo>=0) and (dli.Offs+CharEnds.Items[CaretOffs].Offset-1<=GetOffsBeforeItem(dli.ItemNo))))
       {$IFNDEF RVDONOTUSELISTS}
       and
       not ((dli.ItemNo>0) and (GetItemStyle(dli.ItemNo-1)=rvsListMarker))
       {$ENDIF}
        then begin { we at the beginning of screen line, item, but not paragraph }
      dec(CaretDrawItemNo);
      OnChangeCaretLine(0);
      CaretOffs := CharEnds.Count-1;
      OnBackSpacePress_(Ctrl, MultiDelete, True);
      ChangeCaret(False,True,False,True);
      exit;
    end;
  end;

  BeginUndoSequence(rvutDelete, True);

  if CaretAtTheBeginningOfParaSection then
    Backspace_AtTheBeginningOfParaSection(dli, First, Last)
  else
    Backspace_NotAtTheBeginning(dli, First, Last, Offset);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.ApplyParaStyle(ParaStyleNo: Integer; UseConversion: Boolean);
var i, StartNo,EndNo, SelStartNo, SelEndNo : Integer;
    DIStartNo, DIEndNo, DIStartOffs, DIEndOffs: Integer;
    CIN, CIOffs, SSIN, SSIOffs, SEIN, SEIOffs: Integer;
    FullReformat: Boolean;
    ParaList: TRVIntegerList;
    {..................................................................}
    function GetNewStyleNo(OldStyleNo: Integer): Integer;
    begin
      Result := OldStyleNo;
      TCustomRichViewEdit(FRichView).FCurStyleConversion(TCustomRichViewEdit(FRichView),
        OldStyleNo, ParaStyleNo, False, Result);
    end;
    {...................................................................}
begin
  if FPartialSelectedItem<>nil then begin
     if UseConversion then
       FPartialSelectedItem.ApplyParaStyleConversionToSubRVDatas(ParaStyleNo, True)
     else
       FPartialSelectedItem.ApplyParaStyleToSubRVDatas(ParaStyleNo, True);
     exit;
  end;
  if SelectionExists(True, False) then
    GetSelBounds(DIStartNo, DIEndNo, DIStartOffs, DIEndOffs, True)
  else begin
    DIStartNo := CaretDrawItemNo;
    DIEndNo   := CaretDrawItemNo;
  end;
  StartNo := DrawItems[DIStartNo].ItemNo;
  EndNo   := DrawItems[DIEndNo].ItemNo;
  with CharEnds.Items[CaretOffs] do begin
    CaretDrawItemNo := DrawItemNo;
    CIOffs            := Offset;
  end;
  SelStartNo := StartNo;
  SelEndNo := EndNo;
  if DIStartOffs>=GetOffsAfterDrawItem(DIStartNo) then
    inc(SelStartNo);
  if DIEndOffs<=GetOffsBeforeDrawItem(DIEndNo) then
    dec(SelEndNo);
  DrawItem2Item(CaretDrawItemNo, CIOffs, CIN, CIOffs);
  StoreSelBounds(SSIN,SEIN,SSIOffs,SEIOffs, True);

  while StartNo>0 do begin
    if GetItem(StartNo).CanBeBorderStart then
      break;
    dec(StartNo)
  end;

  inc(EndNo);
  while EndNo<Items.Count do begin
    if GetItem(EndNo).CanBeBorderStart then break;
    inc(EndNo)
  end;
  dec(EndNo);

  if UseConversion then begin
    ParaList := TRVIntegerList.Create;
    try
      ParaList.Capacity := EndNo-StartNo+1;
      for i := StartNo to EndNo do
        ParaList.Add(GetNewStyleNo(GetItemPara(i)));
      Do_ParaList(StartNo, ParaList, FullReformat);
    finally
      ParaList.Free;
    end;
    end
  else
    Do_Para(StartNo, EndNo, ParaStyleNo, FullReformat);
  for i := SelStartNo to SelEndNo do
    if UseConversion then
      GetItem(i).ApplyParaStyleConversionToSubRVDatas(ParaStyleNo, False)
    else
      GetItem(i).ApplyParaStyleToSubRVDatas(ParaStyleNo, False);

  Item2DrawItem(StartNo, GetOffsBeforeItem(StartNo), DIStartNo, DIStartOffs);
  Item2DrawItem(EndNo,   GetOffsAfterItem(EndNo),    DIEndNo,   DIEndOffs);

  try
    Include(State, rvstInvalidSelection);
    if FullReformat then
      Format_(False, True, False, 0, GetCanvas, False, False)
    else
      FormatParas(DIStartNo, DIEndNo, 0);
  finally
    Exclude(State, rvstInvalidSelection);
  end;
  RestoreSelBounds(SSIN,SEIN,SSIOffs,SEIOffs);
  Invalidate;
  Item2DrawItem(CIN, CIOffs,CaretDrawItemNo, CaretOffs);
  OnChangeCaretLine(CaretOffs-2);
  ChangeCaret(False,True,False,True);
  Change;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.OnHomePress(Ctrl: Boolean): Boolean;
begin
  Result := Ctrl;
  if Ctrl then begin
    CaretDrawItemNo := 0;
    OnChangeCaretLine(GetOffsBeforeItem(0)-2);
    CaretOffs := 0;
    end
  else begin
    CaretOffs := 0;
    CaretDrawItemNo := CharEnds.Items[CaretOffs].DrawItemNo;
  end;
  ChangeCaret(False,True,False,False);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.OnEndPress(Ctrl: Boolean): Boolean;
begin
  Result := Ctrl;
  if Ctrl then begin
    CaretDrawItemNo := DrawItems.Count-1;
    OnChangeCaretLine(0);
    CaretOffs := CharEnds.Count-1;
    end
  else begin
    CaretOffs := CharEnds.Count-1;
    CaretDrawItemNo := CharEnds.Items[CaretOffs].DrawItemNo;
  end;
  ChangeCaret(False,True,False,False);
end;
{------------------------------------------------------------------------------}
{ Moving caret to the beginning of the previous paragraph.
  Used in paragraph selection mode, for arrow keys movement.                   }
procedure TRVEditRVData.MoveCaretToTheBeginningOfThePrevParagraph;
var DrawItemNo, DrawItemOffs: Integer;
begin
  DrawItemNo := CharEnds.Items[CaretOffs].DrawItemNo;
  while (DrawItemNo>0) and not IsDrawItemParaStart(DrawItemNo) do
    dec(DrawItemNo);
  if DrawItemNo>0 then
    dec(DrawItemNo);
  while (DrawItemNo>0) and not IsDrawItemParaStart(DrawItemNo) do
    dec(DrawItemNo);
  DrawItemOffs := GetOffsBeforeDrawItem(DrawItemNo);
  CaretDrawItemNo := DrawItemNo;
  OnChangeCaretLine(DrawItemOffs-2);
  ChangeCaret(False,True,False,False);
end;
{------------------------------------------------------------------------------}
{ Moving caret to the end of the next paragraph.
  Used in paragraph selection mode, for arrow keys movement.                   }
procedure TRVEditRVData.MoveCaretToTheEndOfTheNextParagraph;
var DrawItemNo, DrawItemOffs: Integer;
begin
  DrawItemNo := CharEnds.Items[CaretOffs].DrawItemNo;
  while (DrawItemNo+1<DrawItems.Count-1) and
    not IsDrawItemParaStart(DrawItemNo+1) do
    inc(DrawItemNo);
  if DrawItemNo<DrawItems.Count-1 then
    inc(DrawItemNo);
  while (DrawItemNo+1<DrawItems.Count-1) and
    not IsDrawItemParaStart(DrawItemNo+1) do
    inc(DrawItemNo);
  DrawItemOffs := GetOffsAfterDrawItem(DrawItemNo);
  CaretDrawItemNo := DrawItemNo;
  OnChangeCaretLine(DrawItemOffs-2);
  ChangeCaret(False,True,False,False);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.OnLeftPress(Shift, Ctrl: Boolean): Boolean;
var no,no2,offs: Integer;
    dli: TRVDrawLineInfo;
    li: TCustomRVItemInfo;
begin
  Result := False;
  dli := DrawItems[CharEnds.Items[CaretOffs].DrawItemNo];
  li  := GetItem(dli.ItemNo);
  offs := TRVCharPos(CharEnds.Items[CaretOffs]).Offset+dli.Offs-2;
  if Shift and (GetRVStyle.SelectionMode=rvsmParagraph) and
     IsMultiParagraphSelection then begin
    MoveCaretToTheBeginningOfThePrevParagraph;
    exit;
  end;
  if not Ctrl or (li.StyleNo<0) or (Offs<=0) then begin
    if not CaretAtTheBeginningOfLine then begin
      if Shift or Ctrl or not li.EnterItem(rvedRight,0) then
        dec(CaretOffs)
      end
    else begin
      if CaretDrawItemNo<>0 then begin
        dec(CaretDrawItemNo);
        {$IFNDEF RVDONOTUSELISTS}
        if (CaretDrawItemNo=0) and (GetItemStyle(0)=rvsListMarker) then
          Result := True;
        {$ENDIF}
        CaretOffs := GetOffsAfterDrawItem(CaretDrawItemNo);
        {$IFNDEF RVDONOTUSELISTS}
        AdjustMarkerCaret(False, CaretOffs);
        {$ENDIF}
        OnChangeCaretLine(CaretOffs-2);
        end
      else
        Result := True;
    end
    end
  else begin
    no2 := 0;
    for no := offs downto 1 do begin
      if (no2<>0) and RVU_IsSpace(Items[dli.ItemNo], no, GetItemOptions(dli.ItemNo)) and
         not RVU_IsSpace(Items[dli.ItemNo], no+1, GetItemOptions(dli.ItemNo)) then break;
        inc(no2);
    end;
    if no2<=CaretOffs then
      dec(CaretOffs, no2)
    else begin
      Item2DrawItem(dli.ItemNo, offs-no2+1, CaretDrawItemNo, CaretOffs);
      {$IFNDEF RVDONOTUSELISTS}
      AdjustMarkerCaret(False, CaretOffs);
      {$ENDIF}
      OnChangeCaretLine(CaretOffs-2);
    end;
  end;
  ChangeCaret(False,True,False,False);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.OnRightPress(Shift, Ctrl: Boolean): Boolean;
var no,no2,offs: Integer;
    dli: TRVDrawLineInfo;
    li: TCustomRVItemInfo;
begin
  Result := False;
  if Shift and (GetRVStyle.SelectionMode=rvsmParagraph) and
     IsMultiParagraphSelection then begin
    MoveCaretToTheEndOfTheNextParagraph;
    exit;
  end;
  if not (Ctrl) then begin
    if not CaretAtTheEndOfLine then begin
      dli := DrawItems[CharEnds.Items[CaretOffs+1].DrawItemNo];
      li  := GetItem(dli.ItemNo);
      if Shift or Ctrl or not li.EnterItem(rvedLeft,0) then
        inc(CaretOffs)
      end
    else begin
      if CaretDrawItemNo<>DrawItems.Count-1 then begin
        inc(CaretDrawItemNo);
        CaretOffs := GetOffsBeforeDrawItem(CaretDrawItemNo);
        {$IFNDEF RVDONOTUSELISTS}
        AdjustMarkerCaret(True, CaretOffs);
        {$ENDIF}
        OnChangeCaretLine(CaretOffs-2);
        end
      else
        Result := True;
    end
    end
  else begin
    inc(CaretOffs);
    if CaretOffs=CharEnds.Count then
      if CaretDrawItemNo=DrawItems.Count-1 then begin
        dec(CaretOffs);
        Result := True;
        exit;
        end
      else begin
        inc(CaretDrawItemNo);
        CaretOffs := GetOffsBeforeDrawItem(CaretDrawItemNo);
        {$IFNDEF RVDONOTUSELISTS}
        AdjustMarkerCaret(True, CaretOffs);
        {$ENDIF}
        OnChangeCaretLine(CaretOffs-2);
      end;
      dli := DrawItems[CharEnds.Items[CaretOffs].DrawItemNo];
      li  := GetItem(dli.ItemNo);
      if li.StyleNo>=0 then begin
        offs := TRVCharPos(CharEnds.Items[CaretOffs]).Offset+dli.Offs-1;
        no2 := 0;
        for no := offs to ItemLength(dli.ItemNo) do begin
          if (no=ItemLength(dli.ItemNo)) or
             (RVU_IsSpace(Items[dli.ItemNo],no, li.ItemOptions) and
              not RVU_IsSpace(Items[dli.ItemNo],no+1, li.ItemOptions)) then break;
          inc(no2);
        end;
        if no2+CaretOffs+1<CharEnds.Count then
          inc(CaretOffs, no2+1)
        else begin
          Item2DrawItem(dli.ItemNo, offs+no2+1, CaretDrawItemNo, CaretOffs);
          {$IFNDEF RVDONOTUSELISTS}
          AdjustMarkerCaret(True, CaretOffs);
          {$ENDIF}
          OnChangeCaretLine(CaretOffs-2);
        end;
      end;
  end;
  ChangeCaret(False,True,False,False);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.OnUpPress(Shift,Ctrl: Boolean): Boolean;
var offs: Integer;
    item: TCustomRVItemInfo;
    ditem : TRVDrawLineInfo;
    x, DrawItemNo: Integer;
    r: Boolean;
begin
  DrawItemNo := CharEnds.Items[0].DrawItemNo;
  Result := DrawItemNo=0;
  if not Result then begin
    if Shift and (GetRVStyle.SelectionMode=rvsmParagraph) and
       IsMultiParagraphSelection then begin
      MoveCaretToTheBeginningOfThePrevParagraph;
      exit;      
    end;
    x := CharEnds.Items[CaretOffs].X;
    dec(DrawItemNo);
    {$IFNDEF RVDONOTUSELISTS}
    if GetItemStyle(DrawItems[DrawItemNo].ItemNo)=rvsListMarker then begin
      Result := DrawItemNo=0;
      if not Result then
        dec(DrawItemNo)
      else begin
        if Shift and not CaretAtTheBeginningOfLine then
          CaretOffs := 0
        else
          ScrollTo(0,True);
        exit;
      end;
    end;
    {$ENDIF}
    FindDrawItemForSel(x, DrawItems[DrawItemNo].Top+1, CaretDrawItemNo, offs, False);
    ditem := DrawItems[CaretDrawItemNo];
    r := False;
    if (x>=ditem.Left) and (x<=ditem.Left+ditem.Width) then begin
      item := GetItem(ditem.ItemNo);
      if not Shift and not Ctrl and item.EnterItem(rvedBottom, x-ditem.Left) then
        r := True;
    end;
    if not r then begin
      OnChangeCaretLine(offs-2);
      ChangeCaret(False,True,False,False);
    end;
    end
  else if Shift and not CaretAtTheBeginningOfLine then
    CaretOffs := 0
  else
    ScrollTo(0,True);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.OnDownPress(Shift,Ctrl: Boolean): Boolean;
var offs: Integer;
    item: TCustomRVItemInfo;
    ditem : TRVDrawLineInfo;
    x: Integer;
    r: Boolean;
begin
  Result := CaretInTheLastLine;
  if not Result then begin
    if Shift and (GetRVStyle.SelectionMode=rvsmParagraph) and
       IsMultiParagraphSelection then begin
      MoveCaretToTheEndOfTheNextParagraph;
      exit;
    end;
    x := CharEnds.Items[CaretOffs].X;
    FindDrawItemForSel(x,
                   DrawItems[CharEnds.Items[CharEnds.Count-1].DrawItemNo+1].Top+1,
                   CaretDrawItemNo, offs, False);
    ditem := DrawItems[CaretDrawItemNo];
    r := False;
    if (x>=ditem.Left) and (x<=ditem.Left+ditem.Width) then begin
      item := GetItem(ditem.ItemNo);
      if not Shift and not Ctrl and item.EnterItem(rvedTop, x-ditem.Left) then
        r := True;
    end;
    if not r then begin
      OnChangeCaretLine(offs-2);
      ChangeCaret(False,True,False,False);
    end;
    end
  else begin
    if Shift and not CaretAtTheEndOfLine then begin
      CaretOffs := CharEnds.Count-1;
      end
    else
      ScrollTo(DocumentHeight,True);
  end;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.OnPgUpPress: Boolean;
var offs,x,y: Integer;
    NewDrawLineNo: Integer;
begin
  Result := False;
  x := TRVCharPos(CharEnds.Items[CaretOffs]).X;
  y := GetVOffs-(GetHeight * 2 div 3);
  while y>0 do begin
    FindDrawItemForSel(x,y, NewDrawLineNo, offs, False);
    if (NewDrawLineNo<CaretDrawItemNo) then begin
      CaretDrawItemNo := NewDrawLineNo;
      OnChangeCaretLine(offs-2);
      ChangeCaret(False,True,False,False);
      exit;
    end;
    dec(y, GetHeight div 2);
  end;
  OnHomePress(True)
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.OnPgDownPress: Boolean;
var offs,x,y: Integer;
    NewDrawLineNo: Integer;
begin
  Result := False;
  x := TRVCharPos(CharEnds.Items[CaretOffs]).X;
  y := GetVOffs+(GetHeight * 2 div 3);
  while y<DocumentHeight do begin
    FindDrawItemForSel(x,y, NewDrawLineNo, offs, False);
    if (NewDrawLineNo>CaretDrawItemNo) then begin
      CaretDrawItemNo := NewDrawLineNo;
      OnChangeCaretLine(offs-2);
      ChangeCaret(False,False,False,False);
      ScrollTo(DrawItems[CaretDrawItemNo].Top,True);
      exit;
    end;
    inc(y, GetHeight div 2);
  end;
  OnEndPress(True)
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.BuildJumpsCoords(IgnoreReadOnly: Boolean): Integer;
begin
  if not IgnoreReadOnly and TCustomRichViewEdit(FRichView).ReadOnly then
    exit;
  if CaptureMouseItem<>nil then exit;
  ClearJumps;
  Result := 0;
  inherited BuildJumpsCoords(Result,jumps);
  Flags := Flags + [rvflUseJumps];
  State := State - [rvstDrawHover];
  Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.ClearJumpsCoords;
begin
  if TCustomRichViewEdit(FRichView).ReadOnly then
    exit;
  if CaptureMouseItem<>nil then exit;
  if rvflUseJumps in Flags then begin
    ClearJumps;
    Flags := Flags - [rvflUseJumps];
    State := State - [rvstDrawHover];
    ChangeCaret(False,False,True,False);
    Invalidate;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.PrepareForEdit;
begin
  if (Items.Count=0) and (GetRVStyle<>nil) then begin
    AddNL('',FCurTextStyleNo,FCurParaStyleNo);
    Format_(False, True, False, 0, GetCanvas, False, False);
    if DrawItems.Count=0 then exit;
    CaretDrawItemNo := 0;
    OnChangeCaretLine(1-2);
    CaretOffs := 0;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Format_(OnlyResized,ForceFormat,NoScroll:Boolean; depth: Integer; Canvas: TCanvas; OnlyTail, NoCaching: Boolean);
var SavedCaretDrawItemNo, SavedCaretOffs, Offs: Integer;
    saved: Boolean;
begin
  PrepareForEdit;
  if AlreadyFormatted then begin
    AlreadyFormatted := False;
    exit;
  end;
  saved := (depth=0) and
           (GetRVStyle<>nil) and
           not (rvstSkipformatting in State);
  if saved and (OnlyResized or OnlyTail) then begin
    if CaretDrawItemNo<>-1 then
      with CharEnds.Items[CaretOffs] do begin
        CaretDrawItemNo := DrawItemNo;
        Offs            := Offset;
      end;
    DrawItem2Item(CaretDrawItemNo, Offs, {->} SavedCaretDrawItemNo, SavedCaretOffs);
  end;
  inherited;
  if saved then begin
    if (OnlyResized or OnlyTail) then begin
      Item2DrawItem(SavedCaretDrawItemNo, SavedCaretOffs, {->} CaretDrawItemNo, Offs);
      OnChangeCaretLine(Offs-2);
      end
    else
      if DrawItems.Count=0 then begin
          CaretDrawItemNo := -1;
          CaretOffs   := -1;
        end
      else begin
        CaretDrawItemNo := 0;
        OnChangeCaretLine(GetOffsBeforeItem(0)-2);
        CaretOffs   := 0;
      end;
    CreateResizer;
    ChangeCaret(False,False,not (rvstEditorUnformatted in State),False);
  end;
  Exclude(State, rvstEditorUnformatted);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.ApplyStyleConversion_(UserData: Integer);
var StartNo,EndNo, StartOffs, EndOffs : Integer;
    SL, EL, SO, EO: Integer;
    DIStartNo, DIEndNo, DLStartOffs, DLEndOffs, i, ConvertedCurStyle: Integer;
    CLN, CLOffs: Integer;
    ItemsAdded, OldWidth, NewWidth: Integer;
    inverted: Boolean;
    {..................................................................}
    function GetNewStyleNo(OldStyleNo: Integer): Integer;
    begin
      Result := OldStyleNo;
      TCustomRichViewEdit(FRichView).FCurStyleConversion(
        TCustomRichViewEdit(FRichView), OldStyleNo, UserData, True, Result);
    end;
    {...................................................................}
    function NeedClearTag(li: TCustomRVItemInfo; NewStyleNo: Integer): Boolean;
    begin
      Result :=
        (rvoClearTagOnStyleApp in TCustomRichViewEdit(FRichView).EditorOptions)
        and (li.StyleNo<>NewStyleNo);
    end;
    {...................................................................}
    procedure ClrTag(ItemNo, NewStyleNo: Integer);
    var li: TCustomRVItemInfo;
    begin
      li := GetItem(ItemNo);
      if NeedClearTag(li, NewStyleNo) then
        Do_Tag(ItemNo, 0, False);
    end;
    {...................................................................}
    procedure DoSetStyleNo(li: TCustomRVItemInfo; ItemNo, NewStyleNo: Integer);
    {$IFNDEF RVDONOTUSEUNICODE}
    var ActualNewStyleNo: Integer;
    {$ENDIF}
    begin
      {$IFNDEF RVDONOTUSEUNICODE}
      ActualNewStyleNo := GetActualStyle2(NewStyleNo,li.ParaNo);
       if GetRVStyle.TextStyles[GetActualStyle(li)].Unicode then begin
         if not GetRVStyle.TextStyles[ActualNewStyleNo].Unicode then begin
           Do_ChangeText(ItemNo, RVU_UnicodeToAnsi(GetStyleCodePage(ActualNewStyleNo), Items[ItemNo]));
         end;
         end
      else begin
        if GetRVStyle.TextStyles[NewStyleNo].Unicode then begin
          Do_ChangeText(ItemNo, RVU_AnsiToUnicode(GetActualStyle(li), Items[ItemNo]));
        end;
      end;
      {$ENDIF}
      Do_StyleNo(ItemNo, NewStyleNo);
    end;
    {...................................................................}
    procedure ApplyToWhole(ItemNo: Integer);
    var NewStyleNo: Integer;
        li: TCustomRVItemInfo;
    begin
      li := GetItem(ItemNo);
      if (li.StyleNo<0) then begin
        li.ApplyStyleConversion(Self, UserData);
        exit;
      end;
      NewStyleNo := GetNewStyleNo(GetActualStyle(li));
      if (li.StyleNo<>NewStyleNo) then begin
        ClrTag(ItemNo, NewStyleNo);
        DoSetStyleNo(li, ItemNo, NewStyleNo);
      end;
    end;
    {...................................................................}
    function GetStr(const s: String; OldStyleNo, NewStyleNo, ParaNo: Integer): String;
    {$IFNDEF RVDONOTUSEUNICODE}
    var oldU, newU: Boolean;
    {$ENDIF}
    begin
      {$IFNDEF RVDONOTUSEUNICODE}
      oldU := GetRVStyle.TextStyles[GetActualStyle2(OldStyleNo,ParaNo)].Unicode;
      newU := GetRVStyle.TextStyles[GetActualStyle2(NewStyleNo,ParaNo)].Unicode;
      if oldU then
        if not newU then
          Result := RVU_UnicodeToAnsi(GetStyleCodePage(GetActualStyle2(NewStyleNo,ParaNo)), s)
        else
          Result := s
      else
        if newU then
          Result := RVU_AnsiToUnicode(GetStyleCodePage(GetActualStyle2(OldStyleNo,ParaNo)), s)
        else
      {$ENDIF}
          Result := s;
     end;
    {...................................................................}
    procedure ApplyToItemStart(ItemNo: Integer;
                               StartOffs, EndOffs: Integer);
    var  s1: String;
         tag: Integer;
         curitem, newitem: TCustomRVItemInfo;
         NewStyleNo: Integer;
         Dummy: Boolean;
    begin
      curitem := GetItem(ItemNo);
      NewStyleNo := GetNewStyleNo(GetActualStyle(curitem));
      if NewStyleNo=curitem.StyleNo then
        exit;
      s1 := RVU_Copy(Items[ItemNo], 1, EndOffs-1, curitem.ItemOptions);
      Do_DeleteSubstring(ItemNo, 1, EndOffs-1);
      if s1='' then
        exit;
      if NeedClearTag(curitem, NewStyleNo) then
        tag := 0
      else
        tag := RV_CopyTag(curitem.Tag, rvoTagsArePChars in Options);

      newitem := InsString(
        GetStr(s1, curitem.StyleNo, NewStyleNo, curitem.ParaNo), ItemNo, tag,
        curitem.ParaNo, NewStyleNo, curitem.SameAsPrev, curitem.BR, Dummy);
      if curitem.Checkpoint<>nil then begin
        Do_AddCP(ItemNo, curitem.Checkpoint.CreateCopy(rvoTagsArePChars in Options));
        Do_DeleteCP(ItemNo+1);
      end;
      if curitem.PageBreakBefore then begin
        newitem.PageBreakBefore := True;
      end;
      inc(ItemsAdded);
      Do_NewLine(ItemNo+1, True, -1, Dummy);
      EL := ItemNo;
      EO := GetOffsAfterItem(EL);
    end;
    {...................................................................}
    procedure ApplyToItemEnd(ItemNo, StartOffs, EndOffs: Integer);
    var  s2: String;
         tag, NewStyleNo: Integer;
         curitem: TCustomRVItemInfo;
         Dummy: Boolean;
    begin
      curitem := GetItem(ItemNo);
      NewStyleNo := GetNewStyleNo(GetActualStyle(curitem));
      if NewStyleNo=curitem.StyleNo then
        exit;
      s2 := RVU_Copy(Items[ItemNo], StartOffs, RVU_Length(Items[ItemNo], curitem.ItemOptions), curitem.ItemOptions);
      Do_DeleteSubstring(ItemNo, StartOffs, -1);
      if s2='' then
        exit;
      if NeedClearTag(curitem, NewStyleNo) then
        tag := 0
      else
        tag := RV_CopyTag(curitem.Tag, rvoTagsArePChars in Options);
      s2 := GetStr(s2, curitem.StyleNo, NewStyleNo, curitem.ParaNo);
      InsString(s2, ItemNo+1, tag, curitem.ParaNo, NewStyleNo, True, False, Dummy);
      inc(ItemsAdded);
      SL := ItemNo;
      SO := GetOffsAfterItem(SL);
      if StartNo=EndNo then begin
        EL := ItemNo+1;
        EO := GetOffsAfterItem(EL);
        end
      else
        inc(EL);
    end;
    {...................................................................}
    procedure ApplyToItemMid(ItemNo, StartOffs, EndOffs: Integer);
    var  s1, s2, s3: String;
         curitem, newitem: TCustomRVItemInfo;
         Tag, NewStyleNo: Integer;
         Dummy: Boolean;
    begin
      curitem := GetItem(ItemNo);
      NewStyleNo := GetNewStyleNo(GetActualStyle(curitem));
      if NewStyleNo=curitem.StyleNo then
        exit;
      s1 := RVU_Copy(Items[ItemNo], 1, StartOffs-1, curitem.ItemOptions);
      s2 := RVU_Copy(Items[ItemNo], StartOffs, EndOffs-StartOffs, curitem.ItemOptions);
      s3 := RVU_Copy(Items[ItemNo], EndOffs, RVU_Length(Items[ItemNo], curitem.ItemOptions), curitem.ItemOptions);
      if s1<>'' then begin
        Tag := RV_CopyTag(curitem.Tag, rvoTagsArePChars in Options);
        newitem := InsString2(s1, ItemNo, Tag, curitem, curitem.SameAsPrev, curitem.BR, Dummy);
        if curitem.Checkpoint<>nil then begin
          Do_AddCP(ItemNo, curitem.Checkpoint.CreateCopy(rvoTagsArePChars in Options));
          Do_DeleteCP(ItemNo+1);
        end;
        if curitem.PageBreakBefore then
          newitem.PageBreakBefore := True;
        inc(ItemsAdded);
        inc(ItemNo);
        Do_NewLine(ItemNo, True, -1, Dummy);
      end;
      if s3<>'' then begin
        tag :=RV_CopyTag(curitem.Tag, rvoTagsArePChars in Options);
        InsString2(s3, ItemNo+1, tag, curitem, True, False, Dummy);
        if (s1='') and (s2='') and (curitem.Checkpoint<>nil) then begin
          Do_AddCP(ItemNo+1, curitem.Checkpoint.CreateCopy(rvoTagsArePChars in Options));
          Do_DeleteCP(ItemNo);
        end;
        inc(ItemsAdded);
      end;

      ClrTag(ItemNo,NewStyleNo);
      Do_DeleteSubstring(ItemNo, EndOffs, -1);
      Do_DeleteSubstring(ItemNo, 1, StartOffs-1);
      DoSetStyleNo(curitem, ItemNo, NewStyleNo);
      SL := ItemNo-1;
      EL := ItemNo;
      SO := GetOffsAfterItem(SL);
      EO := GetOffsAfterItem(EL);

    end;
    {..................................................................}
    procedure ASC_Concate(StartNo, EndNo: Integer);
    var i: Integer;
    begin
      if StartNo>0 then
        dec(StartNo);
      if EndNo<Items.Count-1 then
        inc(EndNo);
      for i := EndNo downto StartNo+1 do
        if RV_CanConcateItems(i-1, GetItem(i-1), GetItem(i), False) then begin
          dec(ItemsAdded);
          if i=SL then begin
            dec(SL);
            inc(SO, RVU_Length(Items[i-1], GetItemOptions(i-1)));
          end;
          if i<=EL then begin
            if i=EL then
              inc(EO, RVU_Length(Items[i-1], GetItemOptions(i-1)));
            dec(EL);
          end;
          Do_Concate(i-1);
        end;
    end;
    {..................................................................}
begin
  if FPartialSelectedItem<>nil then begin
     FPartialSelectedItem.ApplyStyleConversionToSubRVDatas(UserData, True);
     exit;
  end;
  if not (rvprDoNotAutoSwitch in
     GetRVStyle.TextStyles[GetActualCurStyleNo].Protection) then
    FPrevTextStyleNo := FCurTextStyleNo;
  ConvertedCurStyle := FCurTextStyleNo;
  TCustomRichViewEdit(FRichView).FCurStyleConversion(TCustomRichViewEdit(FRichView),
    FCurTextStyleNo, UserData, False, ConvertedCurStyle);
  if not SelectionExists(True, False) then begin
    if (GetItemStyle(GetCurItemNo)>=0) and (Items[GetCurItemNo]='') then begin
      BeginUndoSequence(rvutStyleNo, True);
      ApplyToWhole(GetCurItemNo);
      FormatParas(CaretDrawItemNo, CaretDrawItemNo, 0);
      SetCurTextStyleNo(ConvertedCurStyle);
      ChangeCaret(False,True,True,True);
      Change;
    end;
    SetCurTextStyleNo(ConvertedCurStyle);
    exit;
  end;
  BeginUndoSequence(rvutStyleNo, True);
  GetSelBounds(DIStartNo, DIEndNo, DLStartOffs, DLEndOffs, True);
  StoreSelBounds(StartNo, EndNo, StartOffs, EndOffs, True);
  OldWidth := CalculateParaSectionsMinWidthDef(StartNo, EndNo);
  StoreSelBounds(SL, EL, SO, EO, False);
  inverted := (SL<>StartNo) or (SO<>StartOffs);
  StoreSelBounds(SL, EL, SO, EO, True);
  StartNo := DrawItems[DIStartNo].ItemNo;
  EndNo   := DrawItems[DIEndNo].ItemNo;
  GetParaBounds(DIStartNo, DIEndNo,DIStartNo, DIEndNo);
  with CharEnds.Items[CaretOffs] do begin
    CaretDrawItemNo := DrawItemNo;
    CLOffs            := Offset;
  end;
  DrawItem2Item(CaretDrawItemNo, CLOffs, CLN, CLOffs);
  ItemsAdded := 0;

  if StartNo=EndNo then begin // (1) one item is selected
    if (GetItemStyle(StartNo)>=0) then begin
      if (StartOffs<=GetOffsBeforeItem(StartNo)) and (EndOffs>=GetOffsAfterItem(StartNo)) then
        ApplyToWhole(StartNo)
      else if StartOffs<=GetOffsBeforeItem(StartNo) then
        ApplyToItemStart(StartNo, StartOffs, EndOffs)
      else if EndOffs>=GetOffsAfterItem(StartNo) then
        ApplyToItemEnd(StartNo, StartOffs, EndOffs)
      else
        ApplyToItemMid(StartNo, StartOffs, EndOffs);
      end
    else
      GetItem(StartNo).ApplyStyleConversion(Self, UserData);
    end
  else begin // (2) 2 or more items are selected
    if GetItemStyle(EndNo)>=0 then begin
      if EndOffs>=GetOffsAfterItem(EndNo) then
        ApplyToWhole(EndNo)
      else
        ApplyToItemStart(EndNo, 1, EndOffs);
      end
    else
      GetItem(EndNo).ApplyStyleConversion(Self, UserData);
    for i := EndNo-1 downto StartNo+1 do
      ApplyToWhole(i);
    if GetItemStyle(StartNo)>=0 then begin
      if StartOffs<=GetOffsBeforeItem(StartNo) then
        ApplyToWhole(StartNo)
      else
        ApplyToItemEnd(StartNo, StartOffs, GetOffsAfterItem(StartNo));
      end
    else
      GetItem(StartNo).ApplyStyleConversion(Self, UserData);
  end;

  ASC_Concate(SL, EL);

  if SL<EL then begin
    StartNo := SL;
    EndNo   := EL;
    end
  else begin
    StartNo := EL;
    EndNo   := SL;
  end;
  NewWidth := CalculateParaSectionsMinWidthDef(StartNo,EndNo);
  try
    Include(State, rvstInvalidSelection);
    Reformat_((OldWidth<>NewWidth) and ((OldWidth>=DocumentWidth) or (NewWidth>=DocumentWidth)),
            DIStartNo, DIEndNo, ItemsAdded);
  finally
    Exclude(State, rvstInvalidSelection);
  end;
  if not Inverted then begin
    Item2DrawItem(SL,SO, FSelStartNo, FSelStartOffs);
    Item2DrawItem(EL,EO, FSelEndNo, FSelEndOffs);
    end
  else begin
    Item2DrawItem(EL,EO, FSelStartNo, FSelStartOffs);
    Item2DrawItem(SL,SO, FSelEndNo, FSelEndOffs);
  end;
  CaretDrawItemNo := FSelEndNo;
  OnChangeCaretLine(FSelEndOffs-2);
  SetCurTextStyleNo(ConvertedCurStyle);
  ChangeCaret(False,True,True,True);
  Invalidate;
  Change;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.InsertTextFromFileW(const FileName: String; AutoTag: Boolean): Boolean;
var Stream: TFileStream;
    s: String;
    SwapBytes: Boolean;
    FirstChar: Word;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      RVCheckUni(Stream.Size);
      SwapBytes := False;
      if Stream.Size>0 then begin
        Stream.ReadBuffer(FirstChar, 2);
        if (FirstChar=UNI_LSB_FIRST) or
           (FirstChar=UNI_MSB_FIRST) then
          SwapBytes := FirstChar=UNI_MSB_FIRST
        else
          Stream.Position := 0;
      end;
      SetLength(s,Stream.Size-Stream.Position);
      Stream.ReadBuffer(PChar(s)^,Stream.Size-Stream.Position);
    finally
      Stream.Free;
    end;
    if SwapBytes then
      RVU_SwapWordBytes(PWord(s), Length(s) div 2);
    InsertTextW_(s, AutoTag, False);
    Result := True;
  except
    Result := False;
  end;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.InsertTextFromFile(const FileName: String; OEM, AutoTag: Boolean): Boolean;
var F: TextFile;
    s: String;
    FullReformat, FirstItem: Boolean;
    InsertPoint, FirstIP, ItemsAdded, P, Dummy: Integer;
    DIStartNo, DIEndNo: Integer;
    li: TCustomRVItemInfo;
    PageBreak: Boolean;
    {$IFNDEF RVDONOTUSEUNICODE}
    ToUnicode: Boolean;
    CodePage: Cardinal;
    {$ENDIF}
    {$IFNDEF RVDONOTUSELISTS}
    MarkerItemNo: Integer;
    {$ENDIF}
    {.............................................}
    procedure InsertString(const s_: String);
    var s: String;
        {$IFNDEF RVDONOTUSELISTS}
        FR: Boolean;
        {$ENDIF}
    begin
      {$IFNDEF RVDONOTUSEUNICODE}
      if ToUnicode then
        s := RVU_AnsiToUnicode(CodePage, s_)
      else
     {$ENDIF}
        s := s_;
      if FirstItem then begin
        li := CreateTextItem(0, FCurParaStyleNo, FCurTextStyleNo, False, False);
        if PageBreak then
          li.PageBreakBefore := True;
        if not InsSomething(li, s, AutoTag, InsertPoint,ItemsAdded, FullReformat, Dummy) then begin
          Beep;
          exit;
        end;
        FirstIP := InsertPoint;
        {$IFNDEF RVDONOTUSELISTS}
        MarkerItemNo := GetFirstParaItem(InsertPoint);
        if GetItemStyle(MarkerItemNo)<>rvsListMarker then
          MarkerItemNo := -1;
        {$ENDIF}
        FirstItem := False;
        end
      else begin
        inc(InsertPoint);
        inc(ItemsAdded);
        {$IFNDEF RVDONOTUSELISTS}
        if MarkerItemNo>=0 then begin
          ReplicateMarker(MarkerItemNo, InsertPoint, FR, False);
          FullReformat := FullReformat or FR;
          if PageBreak then
            GetItem(InsertPoint).PageBreakBefore := True;
          inc(InsertPoint);
          inc(ItemsAdded);
          li := CreateTextItem(0, FCurParaStyleNo, FCurTextStyleNo, True, False);
          end
        else
        {$ENDIF}
        begin
          li := CreateTextItem(0, FCurParaStyleNo, FCurTextStyleNo, False, False);
          if PageBreak then
            li.PageBreakBefore := True;
        end;
        li.Inserting(Self,s,False);
        Items.InsertObject(InsertPoint, s, li);
        li.Inserted(Self,InsertPoint);
      end;
      PageBreak := False;
    end;
    {.............................................}
    procedure InsertStringFF(var s: String);
    begin
      if s<>'' then
        while s<>'' do begin
          P := RV_CharPos(PChar(s), #$0C, Length(s));
          if P<>0 then begin
            if P>1 then
              InsertString(System.Copy(s,1,P-1));
            s := System.Copy(s, P+1, Length(s));
            PageBreak := True;
            end
          else begin
            if s<>'' then
              InsertString(s);
            break;
          end;
        end
      else
        InsertString(s);
    end;
    {.............................................}
    {$IFNDEF RICHVIEWDEF6}
    procedure InsertUnixString(var s: String);
    var PrevCRLoc, CRLoc, StartLoc: PChar;
        s2: String;
        Len,Pos: Integer;
    begin
      Len       := Length(s);
      StartLoc  := PChar(s);
      PrevCRLoc := StartLoc;
      repeat
        Pos := RV_CharPos(PrevCRLoc, #10, Len);
        if Pos <> 0 then begin
          CRLoc := PrevCRLoc+Pos-1;
          s2 := System.Copy(s, PrevCRLoc-StartLoc+1, CRLoc-PrevCRLoc);
          dec(Len, Length(s2)+1);
          PrevCRLoc := CRLoc+1;
          end
        else
          s2 := System.Copy(s, PrevCRLoc-StartLoc+1, Length(s));
        InsertStringFF(s2);
      until Pos=0;
    end;
    {$ENDIF}
    {.............................................}
begin
  FullReformat := False;
  GetParaBounds(CaretDrawItemNo,CaretDrawItemNo,DIStartNo,DIEndNo);
  FirstItem := True;
  InsertPoint := -1;
  FirstIP     := -1;
  ItemsAdded  := 0;
  PageBreak   := False;
  {$IFNDEF RVDONOTUSEUNICODE}
  ToUnicode   := GetRVStyle.TextStyles[GetActualCurStyleNo].Unicode;
  CodePage    := GetStyleCodePage(GetActualCurStyleNo);
  {$ENDIF}
  {$IFNDEF RVDONOTUSELISTS}
  MarkerItemNo := -1;
  {$ENDIF}
  Result := True;
  try
    AssignFile(F, FileName);
    Reset(F);
    BeginUndoSequence(rvutInsert, True);
    try
      while not EOF(F) do begin
        ReadLn(F,s);
        if OEM and (s<>'') then
          OEMToAnsi(PChar(s),PChar(s));
        Replace0(s);
        s := RV_ReplaceTabsA(s, GetRVStyle.SpacesInTab);
        {$IFDEF RICHVIEWDEF6}
        InsertStringFF(s);
        {$ELSE}
        InsertUnixString(s);
        {$ENDIF}
      end;
    finally
      CloseFile(F);
    end;
  except
    Result := False;
  end;
  if InsertPoint=-1 then exit;
  AfterAddingText(FirstIP, InsertPoint, ItemsAdded, DIStartNo,DIEndNo,FullReformat,False);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.AfterAddingText(StartItemNo, EndItemNo, ItemsAdded,
                                        DIStartNo, DIEndNo: Integer;
                                        FullReformat, CaretBefore: Boolean);
var Offs, CDIOffs: Integer;
    FR: Boolean;
begin
  Do_InsertItems_2(StartItemNo+1, EndItemNo-StartItemNo,
                   Do_InsertItems_1(StartItemNo+1, EndItemNo-StartItemNo),
                   FR);
  if FR then
    FullReformat := True;
  // concating Items
  if CaretBefore then
    Offs := 1
  else
    Offs := ItemLength(EndItemNo)+1;
  if (EndItemNo<>Items.Count-1) and
     RV_CanConcateItems(EndItemNo, GetItem(EndItemNo), GetItem(EndItemNo+1), False) then begin
    Do_Concate(EndItemNo);
    dec(ItemsAdded);
  end;
  if (StartItemNo<>0) and
     RV_CanConcateItems(StartItemNo-1, GetItem(StartItemNo-1), GetItem(StartItemNo), False) then begin
    if CaretBefore or (StartItemNo=EndItemNo) then
      inc(Offs, ItemLength(StartItemNo-1));
    Do_Concate(StartItemNo-1);
    dec(ItemsAdded);
    dec(EndItemNo);
    dec(StartItemNo);
  end;
  AdjustInItemsRange(StartItemNo);
  AdjustInItemsRange(EndItemNo);
  if CalculateMinItemsWidthPlusEx(StartItemNo,EndItemNo)>DocumentWidth then
    FullReformat := True;
  // formatting
  Reformat_(FullReformat, DIStartNo,DIEndNo,ItemsAdded);
  if CaretBefore then
    Item2DrawItem(StartItemNo,Offs, CaretDrawItemNo, CDIOffs)
  else
    Item2DrawItem(EndItemNo,Offs, CaretDrawItemNo, CDIOffs);
  OnChangeCaretLine(CDIOffs-2);
  ChangeCaret(False,True,False,True);
  {$IFNDEF RVDONOTUSELISTS}
  if not FullReformat and (GetItemStyle(GetFirstParaItem(StartItemNo))=rvsListMarker) then
    UpdateRangeAfterMarkers(StartItemNo, EndItemNo);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.InsertText_(const text: String; AutoTag, CaretBefore: Boolean);
var DIStartNo, DIEndNo, Dummy: Integer;
    text2, s: String;
    TextPtr, EndPtr: PChar;
    Pos, ItemsAdded, FirstIP,InsertPoint, TextLength: Integer;
    FirstItem, CRLFAtEnd: Boolean;
    li: TCustomRVItemInfo;
    {$IFNDEF RVDONOTUSEUNICODE}
    ToUnicode: Boolean;
    CodePage: Cardinal;
    {$ENDIF}
    {$IFNDEF RVDONOTUSELISTS}
    MarkerItemNo: Integer;
    FR: Boolean;
    {$ENDIF}
    FullReformat: Boolean;
begin
  if Text='' then begin
    Invalidate;
    exit;
  end;
  GetParaBounds(CaretDrawItemNo,CaretDrawItemNo,DIStartNo,DIEndNo);
  FullReformat := False;
  {$IFNDEF RVDONOTUSELISTS}
  FR           := False;
  {$ENDIF}
  Text2 := AdjustLineBreaks(Text);
  FirstItem := True;
  InsertPoint := -1;
  FirstIP     := -1;
  ItemsAdded := 0;
  CRLFAtEnd := False;
  TextPtr := PChar(Text2);
  {$IFNDEF RVDONOTUSEUNICODE}
  ToUnicode   := GetRVStyle.TextStyles[GetActualCurStyleNo].Unicode;
  CodePage    := GetStyleCodePage(GetActualCurStyleNo);
  {$ENDIF}
  {$IFNDEF RVDONOTUSELISTS}
  MarkerItemNo := -1;
  {$ENDIF}
  TextLength := Length(Text2);
  while (TextPtr[0]<>#0) or CRLFAtEnd do begin
    //EndPtr := StrPos(TextPtr, #13#10);
    Pos := RV_CharPos(TextPtr, #13, TextLength);
    if Pos=0 then begin
      s := TextPtr;
      CRLFAtEnd := False;
      inc(TextPtr, Length(s));
      end
    else begin
      EndPtr := TextPtr+Pos-1;
      SetLength(s, EndPtr-TextPtr);
      Move(TextPtr^, PChar(s)^, EndPtr-TextPtr);
      CRLFAtEnd := True;
      inc(TextPtr, Length(s)+2);
      dec(TextLength, Length(s)+2);
    end;
    s := RV_ReplaceTabsA(s, GetRVStyle.SpacesInTab);
    {$IFNDEF RVDONOTUSEUNICODE}
    if ToUnicode then
      s := RVU_AnsiToUnicode(CodePage, s);
    {$ENDIF}
    if FirstItem then begin
      li := CreateTextItem(0, FCurParaStyleNo, FCurTextStyleNo, False, False);
      if not InsSomething(li, s, AutoTag, InsertPoint,ItemsAdded,FullReformat, Dummy) then begin
        Beep;
        exit;
      end;
      FirstIP := InsertPoint;
      {$IFNDEF RVDONOTUSELISTS}
      MarkerItemNo := GetFirstParaItem(InsertPoint);
      if GetItemStyle(MarkerItemNo)<>rvsListMarker then
        MarkerItemNo := -1;
      {$ENDIF}
      FirstItem := False;
      end
    else begin
      inc(InsertPoint);
      inc(ItemsAdded);
      {$IFNDEF RVDONOTUSELISTS}
      if MarkerItemNo>=0 then begin
        ReplicateMarker(MarkerItemNo, InsertPoint, FR, False);
        FullReformat := FullReformat or FR;
        inc(InsertPoint);
        inc(ItemsAdded);
        li := CreateTextItem(0, FCurParaStyleNo, FCurTextStyleNo, True, False);
        end
      else
      {$ENDIF}
        li := CreateTextItem(0, FCurParaStyleNo, FCurTextStyleNo, False, False);
      li.Inserting(Self, s, False);
      Items.InsertObject(InsertPoint, s, li);
      li.Inserted(Self, InsertPoint);
    end;
  end;

  if InsertPoint=-1 then exit;
  AfterAddingText(FirstIP, InsertPoint, ItemsAdded, DIStartNo,DIEndNo,FullReformat, CaretBefore);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.InsertTextTyping(text: String);
var ditem: TRVDrawLineInfo;
    item: TCustomRVItemInfo;
    ItemNo, Offs, Len : Integer;
    s: String;
    Minus,SavedCaretOffs: Integer;
    {.....................................................}
    procedure DoInsert;
    begin
      BeginUndoSequence(rvutInsert, True);
      InsertString(Text,FCurTextStyleNo,True,False);  { inserting character in new item }
      Refresh;
      Change;
    end;
    {.....................................................}
begin
  if (GetRVStyle=nil) or (PartialSelectedItem<>nil) or not CanDelete then begin
    Beep;
    exit;
  end;
  Len := Length(text);
  {$IFNDEF RVDONOTUSEUNICODE}
  if GetRVStyle.TextStyles[GetActualCurStyleNo].Unicode then
    Len := Length(text) div 2;
  {$ENDIF}
  CaretDrawItemNo := CharEnds.Items[CaretOffs].DrawItemNo;
  ditem := DrawItems[CaretDrawItemNo];
  Minus := 1;
  item := GetItem(ditem.ItemNo);
  if IsParaProtected(item.ParaNo,rvpaoReadOnly) then begin
   Beep;
   exit;
  end;
  Offs := CharEnds.Items[CaretOffs].Offset-1;
  if item.StyleNo<>FCurTextStyleNo then
    if (
      ((item.StyleNo<0)  and (CharEnds.Items[CaretOffs].Offset=1)) or
      ((item.StyleNo>=0) and (ditem.Offs+CharEnds.Items[CaretOffs].Offset-1>ItemLength(ditem.ItemNo)))
       ) and
       (CaretDrawItemNo<>DrawItems.Count-1) then begin
      ditem := DrawItems[CaretDrawItemNo+1];
      item := GetItem(ditem.ItemNo);
      Minus := 2;
      Offs := 1;
      if (not item.SameAsPrev) or
         (item.StyleNo<>FCurTextStyleNo) then begin
        DoInsert;
        exit;
      end;
      inc(CaretOffs);
      end
    else begin
       DoInsert;
       exit;
    end;
  { inserting in existing item }

  if item.StyleNo>=0 then begin
    if (rvprConcateProtect in GetRVStyle.TextStyles[GetActualStyle(item)].Protection) or
       (
       (rvprModifyProtect in GetRVStyle.TextStyles[GetActualStyle(item)].Protection) and
       ((ditem.Offs+Offs+1-Minus <= GetOffsBeforeItem(ditem.ItemNo)) or
        (ditem.Offs+Offs+1-Minus >= GetOffsAfterItem(ditem.ItemNo)))
       )
     then begin
      if Minus=2 then
        dec(CaretOffs);
      DoInsert;
      exit;
    end;
    if rvprModifyProtect in GetRVStyle.TextStyles[GetActualStyle(item)].Protection then begin
      if Minus=2 then
        dec(CaretOffs);
      Beep;
      exit;
    end;
  end;
  SavedCaretOffs := ditem.Offs+Offs;
  ItemNo := ditem.ItemNo;  
  if Len>1 then begin
    BeginUndoSequence(rvutMiscTyping, True);
    Do_InsertSubstring(ItemNo, ditem.Offs+Offs+1-Minus, text);
    end
  else begin
    UndoList.AddTyping(ItemNo, ditem.Offs+Offs+1-Minus, rvioUnicode in GetItemOptions(ditem.ItemNo));
    s := Items[ItemNo];
    if Length(s)=0 then
      GetItem(ItemNo).ParaNo := FCurParaStyleNo;
    RVU_Insert(Text, s, SavedCaretOffs+1-Minus, GetItemOptions(ditem.ItemNo));
    ItemAction(rviaTextModifying, GetItem(ItemNo), s, Self);
    Items[ItemNo] := s;
  end;
  if CalculateParaSectionMinWidthDef(ItemNo)>DocumentWidth then begin
    Format_(False, True, False, 0, GetCanvas, False, False);
    Invalidate;
    end
  else
    FormatParas(CharEnds.Items[0].DrawItemNo, CharEnds.Items[CharEnds.Count-1].DrawItemNo,0);
  Item2DrawItem(ItemNo, SavedCaretOffs+1, CaretDrawItemNo, CaretOffs);
  OnChangeCaretLine(CaretOffs-Minus-1);
  ChangeCaret(False,True,False,False);
  Change;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.InsertTextW_(const text: String; AutoTag, CaretBefore: Boolean);
var DIStartNo, DIEndNo,Dummy: Integer;
    startptr,ptr,endptr: PWord;
    SkipIfEqual: Word;
    ItemsAdded, FirstIP,InsertPoint: Integer;
    FirstItem: Boolean;
    ANP: Boolean;
    ConvertToAnsi: Boolean;
    CodePage: Cardinal;
    FullReformat: Boolean;
    PageBreak: Boolean;
    {$IFNDEF RVDONOTUSELISTS}
    MarkerItemNo: Integer;
    {$ENDIF}    
    {................................................}
    function AddS: Boolean;
    var s: String;
        li: TCustomRVItemInfo;
        {$IFNDEF RVDONOTUSELISTS}
        FR: Boolean;
        {$ENDIF}
    begin
      if PageBreak and (startptr=ptr) then begin
        Result := True;
        exit;
      end;
      s := System.Copy(text, PChar(startptr)-PChar(text)+1, PChar(ptr)-PChar(startptr));
      s := RV_ReplaceTabsW(s, GetRVStyle.SpacesInTab);
      if ConvertToAnsi then
        s := RVU_UnicodeToAnsi(CodePage, s);
      if FirstItem then begin
        li := CreateTextItem(0, FCurParaStyleNo, FCurTextStyleNo, False, False);
        if PageBreak then begin
          li.PageBreakBefore := True;
          PageBreak := False;
        end;
        if not InsSomething(li, s, AutoTag, InsertPoint,ItemsAdded,FullReformat,Dummy) then begin
          Beep;
          Result := False;
          exit;
        end;
        FirstIP := InsertPoint;
        {$IFNDEF RVDONOTUSELISTS}
        MarkerItemNo := GetFirstParaItem(InsertPoint);
        if GetItemStyle(MarkerItemNo)<>rvsListMarker then
          MarkerItemNo := -1;
        {$ENDIF}
        FirstItem := False;
        end
      else begin
        inc(InsertPoint);
        inc(ItemsAdded);
        {$IFNDEF RVDONOTUSELISTS}
        if (MarkerItemNo>=0) and ANP then begin
          ReplicateMarker(MarkerItemNo, InsertPoint, FR, False);
          FullReformat := FullReformat or FR;
          if PageBreak then
            GetItem(InsertPoint).PageBreakBefore := True;        
          inc(InsertPoint);
          inc(ItemsAdded);
          li := CreateTextItem(0, FCurParaStyleNo, FCurTextStyleNo, True, False);
          end
        else
        {$ENDIF}
        begin
          li := CreateTextItem(0, FCurParaStyleNo, FCurTextStyleNo, False, not ANP);
          if PageBreak then
            li.PageBreakBefore := True;
        end;
        li.Inserting(Self, s, False);
        Items.InsertObject(InsertPoint, s, li);
        li.Inserted(Self, InsertPoint);
      end;
      PageBreak := False;      
      startptr := ptr;
      inc(PChar(startptr),2);
      Result := True;
    end;
    {................................................}
begin
  if Text='' then begin
    Invalidate;
    exit;
  end;
  FullReformat := False;
  RVCheckUni(Length(text));

  {$IFNDEF RVDONOTUSEUNICODE}
  ConvertToAnsi := not GetRVStyle.TextStyles[GetActualCurStyleNo].Unicode;
  CodePage := GetStyleCodePage(GetActualCurStyleNo);
  {$ENDIF}
  {$IFNDEF RVDONOTUSELISTS}
  MarkerItemNo := -1;
  {$ENDIF}  

  GetParaBounds(CaretDrawItemNo,CaretDrawItemNo,DIStartNo,DIEndNo);

  FirstItem := True;
  InsertPoint := -1;
  FirstIP     := -1;
  ItemsAdded  := 0;
  StartPtr    := PWord(Text);
  Ptr         := StartPtr;
  EndPtr      := PWord(PChar(Text)+Length(Text));
  SkipIfEqual := 0;
  ANP         := True;
  PageBreak   := False;

  while PChar(ptr)<PChar(endptr) do begin
    if (ptr^=UNI_LineSeparator) or
       (ptr^=UNI_ParagraphSeparator) or
       (ptr^=UNI_VerticalTab) or
       (ptr^=0) then begin
      ANP := ptr^=UNI_ParagraphSeparator;
      if not AddS then exit;
      SkipIfEqual := 0;
      end
    else if (ptr^=SkipIfEqual) then begin
      SkipIfEqual := 0;
      inc(PChar(startptr),2);
      end
    else if (ptr^=UNI_CR) then begin
      ANP := True;
      if not AddS then exit;
      SkipIfEqual := UNI_LF;
      end
    else if (ptr^=UNI_LF) then begin
      ANP := True;
      if not AddS then exit;
      SkipIfEqual := UNI_CR;
      end
    else if (ptr^=UNI_FF) then begin
      ANP := True;
      if startptr<>ptr then
        if not AddS then exit;
      PChar(startptr) := PChar(ptr)+2;
      PageBreak := True;
      SkipIfEqual := 0;
      end
    else begin
      ;
    end;
    inc(PChar(ptr), 2);
  end;
  if not AddS then exit;
  if InsertPoint=-1 then exit;
  AfterAddingText(FirstIP, InsertPoint, ItemsAdded, DIStartNo,DIEndNo,FullReformat, CaretBefore);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.InsertRVFFromStreamEd_(Stream: TStream):Boolean;
var
    Offs,CDIOffs : Integer;
    DIStartNo, DIEndNo: Integer;
    InsertPoint, LastInserted: Integer;
    dli: TRVDrawLineInfo;
    li: TCustomRVItemInfo;
    FullFormat, FR: Boolean;
    Color : TColor;
    NonFirstItemsAdded, ItemsAdded: Integer;
    ui: TRVUndoInsertItemsInfo;
    FailedBecauseOfProtect: Boolean;
begin
  GetParaBounds(CaretDrawItemNo,CaretDrawItemNo,DIStartNo,DIEndNo);
  with CharEnds.Items[CaretOffs] do begin
    dli := DrawItems[DrawItemNo];
    li := GetItem(dli.ItemNo);
    InsertPoint := dli.ItemNo;
    if ((li.StyleNo>=0) and (dli.Offs+Offset-1>=GetOffsAfterItem(dli.ItemNo))) or
       ((li.StyleNo<0) and (Offset=1)) then
      inc(InsertPoint);
  end;
  BeginUndoSequence(rvutInsert, True);
  ItemsAdded := Items.Count;
  Color := TCustomRichView(FRichView).Color;
  FRVFInserted := False;
  Result := InsertRVFFromStream_(Stream, InsertPoint, -1, False, False, True,
              Color, TCustomRichView(FRichView).Background, nil, NonFirstItemsAdded,
              FailedBecauseOfProtect, FullFormat);
  if Result and FailedBecauseOfProtect or not FRVFInserted then
    exit;
  if not Result then begin
    TCustomRichViewEdit(FRichView).Format;
    Invalidate;
    Beep;
    exit;
  end;
  TCustomRichView(FRichView).Color := Color;
  ItemsAdded := Items.Count - ItemsAdded;
  LastInserted := InsertPoint+NonFirstItemsAdded;
  if LastInserted>=Items.Count then
    exit;
  Offs := GetOffsAfterItem(LastInserted);
  ui := Do_InsertItems_1(InsertPoint+1, NonFirstItemsAdded);
  Do_InsertItems_2(InsertPoint+1, NonFirstItemsAdded, ui, FR);

  if InsertPoint<0 then
    InsertPoint := 0;
  FullFormat := FullFormat or FR or (CalculateMinItemsWidthPlusEx(InsertPoint,LastInserted) > DocumentWidth);

  ConcateAfterAdding(InsertPoint, LastInserted, ItemsAdded, Offs);

  // formatting
  if FullFormat then begin
    Format_(False,True, False, 0,GetCanvas,False, False);
    Invalidate;
    end
  else
    FormatParasExact(DIStartNo,DIEndNo,ItemsAdded, False);
  Item2DrawItem(LastInserted, Offs, CaretDrawItemNo, CDIOffs);
  OnChangeCaretLine(CDIOffs-2);
  ChangeCaret(False,True,False,True);
  {$IFNDEF RVDONOTUSELISTS}
  if not FullFormat then
    UpdateRangeAfterMarkers(InsertPoint,LastInserted);
  {$ENDIF}  
  Change;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERTFIMPORT}
function TRVEditRVData.InsertRTFFromStreamEd_(Stream: TStream): Boolean;
var
    Offs,CDIOffs : Integer;
    DIStartNo, DIEndNo: Integer;
    ItemsAdded, InsertPoint, LastInserted: Integer;
    dli: TRVDrawLineInfo;
    li: TCustomRVItemInfo;
    FullFormat, FR: Boolean;
    ui: TRVUndoInsertItemsInfo;
    rp: TRVRTFReaderProperties;
begin
  if Stream.Position=Stream.Size then begin
    Result := True;
    exit;
  end;
  Result := False;
  rp := TRVRTFReaderProperties(GetRTFProperties);
  if rp=nil then
    exit;
  GetParaBounds(CaretDrawItemNo,CaretDrawItemNo,DIStartNo,DIEndNo);
  with CharEnds.Items[CaretOffs] do begin
    dli := DrawItems[DrawItemNo];
    li := GetItem(dli.ItemNo);
    InsertPoint := dli.ItemNo;
    if ((li.StyleNo>=0) and (dli.Offs+Offset-1>=GetOffsAfterItem(dli.ItemNo))) or
       ((li.StyleNo<0) and (Offset=1)) then
      inc(InsertPoint);
  end;
  BeginUndoSequence(rvutInsert, True);
  ItemsAdded := Items.Count;
  Result := rp.InsertFromStreamEd(Stream, Self, InsertPoint)=rtf_ec_OK;
  if Result and rp.FailedBecauseOfProtect or (InsertPoint<0) then
    exit;
  if not Result then begin
    TCustomRichViewEdit(FRichView).Format;
    Invalidate;
    Beep;
    exit;
  end;
  ItemsAdded := Items.Count-ItemsAdded;
  LastInserted := InsertPoint+rp.NonFirstItemsAdded;
  Offs := GetOffsAfterItem(LastInserted);

  ui := Do_InsertItems_1(InsertPoint+1, rp.NonFirstItemsAdded);
  Do_InsertItems_2(InsertPoint+1, rp.NonFirstItemsAdded, ui, FR);

  FullFormat := rp.FullReformat or FR or (CalculateMinItemsWidthPlusEx(InsertPoint,LastInserted) > DocumentWidth);

  ConcateAfterAdding(InsertPoint, LastInserted, ItemsAdded, Offs);

  // formatting
  if FullFormat then begin
    Format_(False,True,False, 0,GetCanvas,False, False);
    Invalidate;
    end
  else
    FormatParasExact(DIStartNo,DIEndNo,ItemsAdded, False);
  Item2DrawItem(LastInserted, Offs, CaretDrawItemNo, CDIOffs);
  OnChangeCaretLine(CDIOffs-2);
  ChangeCaret(False,True,False,True);
  {$IFNDEF RVDONOTUSELISTS}
  if not FullFormat then
    UpdateRangeAfterMarkers(InsertPoint,LastInserted);
  {$ENDIF}
  Change;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERTF}
function TRVEditRVData.SaveRTFToStream(Stream: TStream; SelectionOnly: Boolean;
  Level: Integer; Color: TColor; Background: TRVBackground; ColorList: TRVColorList;
  StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
  FontTable: TRVRTFFontTable; tpp: Double):Boolean;
begin
  if rvflRootEditor in Flags then
    BuildJumpsCoords(False);
    Result := inherited SaveRTFToStream(Stream, SelectionOnly, Level, Color,
      Background, ColorList, StyleToFont,
      ListOverrideOffsetsList1, ListOverrideOffsetsList2, FontTable, tpp);
  if rvflRootEditor in Flags then
    ClearJumpsCoords;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TRVEditRVData.ConcateAfterAdding(var InsertPoint, LastInserted, ItemsAdded, Offs: Integer);
var li, li2: TCustomRVItemInfo;
    i: Integer;
    atend: Boolean;
    fr: Boolean;
begin
  if LastInserted<>Items.Count-1 then begin
    li := GetItem(LastInserted);
    li2 := GetItem(LastInserted+1);
    if RV_CanConcateItems(LastInserted, li,li2,False) then begin
      Do_Concate(LastInserted);
      dec(ItemsAdded);
      end
    else if li.GetBoolValue(rvbpFullWidth) and li2.SameAsPrev then
      Do_NewLine(LastInserted+1,False,li2.ParaNo,fr);
  end;
  atend := True;
  for i := LastInserted downto InsertPoint+1 do begin
    li := GetItem(i-1);
    li2 := GetItem(i);
    if RV_CanConcateItems(i-1, li,li2,False) then begin
      if atend then
        inc(Offs, RVU_Length(Items[i-1], GetItemOptions(i-1)));
      Do_Concate(i-1);
      dec(ItemsAdded);
      dec(LastInserted);
      end
    else
      atend := False;
  end;
  if InsertPoint<>0 then begin
    li := GetItem(InsertPoint-1);
    li2 := GetItem(InsertPoint);
    if RV_CanConcateItems(InsertPoint-1, li,li2,False) then begin
      if atend then
        inc(Offs, RVU_Length(Items[InsertPoint-1], GetItemOptions(InsertPoint-1)));
      Do_Concate(InsertPoint-1);
      dec(ItemsAdded);
      dec(LastInserted);
      end
    else begin
      if (li2.StyleNo>=0) and li2.SameAsPrev and (Items[InsertPoint]='')
      {$IFNDEF RVDONOTUSELISTS}
      and (li.StyleNo<>rvsListMarker)
      {$ENDIF}
      then begin
        Do_DeleteItem(InsertPoint, fr);
        dec(ItemsAdded);
        dec(LastInserted);
      end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.GetSelStart(var DINo, DIOffs: Integer);
begin
  inherited GetSelStart(DINo,DIOffs);
  if (DINo=-1) and (CaretDrawItemNo<>-1) then
    with CharEnds.Items[CaretOffs] do begin
      DINo   := DrawItemNo;
      DIOffs := Offset;
    end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.SrchSelectIt(strt, offs, len: Integer; Invert: Boolean);
begin
  DeselectPartiallySelectedItem(nil);
  if not Invert then
    RestoreSelBounds(strt,strt,offs,offs+len)
  else
    RestoreSelBounds(strt,strt,offs+len,offs);
  Invalidate;
  DoOnSelection(True);
  DoSelect;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.SrchStart(Down: Boolean; var strt, offs: Integer);
begin
  strt := GetCurItemNo;
  offs := GetOffsetInCurItem;
  if Down and (offs>=GetOffsAfterItem(strt)) then begin
    inc(strt);
    if strt<Items.Count then
      offs := GetOffsBeforeItem(strt);
    end
  else if not Down and (offs<=GetOffsBeforeItem(strt)) then begin
    dec(strt);
    if strt>=0 then
      offs := GetOffsAfterItem(strt);
  end;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.GetCurItemNo: Integer;
  {........................................}
  function IndexOf(obj: TObject): Integer;
  begin
    if (CaretDrawItemNo>=0) and (Items.Objects[DrawItems[CaretDrawItemNo].ItemNo]=obj) then
      Result := DrawItems[CaretDrawItemNo].ItemNo
    else
      Result := Items.IndexOfObject(obj);
  end;
  {........................................}
begin
  PrepareForEdit;
  if FPartialSelectedItem<>nil then
    Result := IndexOf(FPartialSelectedItem)
  else if GetChosenItem<>nil then
    Result := IndexOf(GetChosenItem)
  else if CaretDrawItemNo=-1 then
    Result := -1
  else
    Result := DrawItems[CaretDrawItemNo].ItemNo
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.GetOffsetInCurItem: Integer;
begin
  PrepareForEdit;
  if FPartialSelectedItem<>nil then
    Result := 1
  else if CaretDrawItemNo=-1 then
    Result := -1
  else begin
    if GetItemStyle(DrawItems[CaretDrawItemNo].ItemNo)>=0 then
      Result := DrawItems[CaretDrawItemNo].Offs+CharEnds.Items[CaretOffs].Offset-1
    else
      Result := CharEnds.Items[CaretOffs].Offset;
  end;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.NotFormatted: Boolean;
begin
  Result := DrawItems.Count=0;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.StartShiftMoving;
begin
 with CharEnds.Items[CaretOffs] do
   if not SelectionExists(True, False) then begin
     FSelStartNo   := DrawItemNo;
     FSelStartOffs := Offset;
     FSelEndNo     := DrawItemNo;
     FSelEndOffs   := Offset;
   end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.EndShiftMoving;
begin
  with CharEnds.Items[CaretOffs] do begin
    FSelEndNo     := DrawItemNo;
    FSelEndOffs   := Offset;
  end;
  if GetRVStyle.SelectionMode=rvsmParagraph then
    ExpandSelectionToParagraph(True);
  DoOnSelection(True);
  DoSelect;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.InsertString(var s: String; StyleNo: Integer;
  AutoTag, CaretBefore: Boolean);
var info: TCustomRVItemInfo;
begin
  info         := RichViewTextItemClass.Create(Self);
  {$IFNDEF RVDONOTUSEUNICODE}
  if GetRVStyle.TextStyles[GetActualStyle2(StyleNo, FCurParaStyleNo)].Unicode then
    Include(info.ItemOptions,rvioUnicode);
  {$ENDIF}
  info.StyleNo := StyleNo;
  InsertSomething(info, s, AutoTag, CaretBefore);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.KeyPress(var Key: Char);
var Text: String;
begin
  if (GetRVStyle=nil) or (PartialSelectedItem<>nil) or not CanDelete then begin
    Beep;
    exit;
  end;
  {$IFNDEF RVDONOTUSEUNICODE}
  if GetRVStyle.TextStyles[GetActualCurStyleNo].Unicode then begin
    Text := RVU_KeyToUnicode(Key);
    if Length(Text)=0 then exit;
    end
  else
  {$ENDIF}
    Text := Key;
  InsertTextTyping(Text);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.CreateResizer;
  {..............................................}
  function GetResizerDrawItemNo: Integer;
  begin
    Result := GetOneSelectedItemNo;
    if (Result>=0) and (GetItemStyle(Result)<0) and
       GetItem(Result).GetBoolValue(rvbpResizable) then
      Item2FirstDrawItem(Result, Result)
    else
      Result := -1;
  end;
  {..............................................}
var ResizerDrawItemNo: Integer;
begin
  if rvstInvalidSelection in State then
    exit;
  FResizer.Free;
  FResizer := nil;
  if (TCustomRichViewEdit(RichView).ReadOnly and not (rvflDBRichViewEdit in Flags)) or
     (rvoNoImageResize in TCustomRichViewEdit(RichView).EditorOptions) then
    exit;
  ResizerDrawItemNo := GetResizerDrawItemNo;
  if (ResizerDrawItemNo>=0) and
    not IsItemParaProtected(DrawItems[ResizerDrawItemNo].ItemNo) then begin
    FResizer := TRVItemResizer.Create(DrawItems[ResizerDrawItemNo],
      GetItem(DrawItems[ResizerDrawItemNo].ItemNo), ResizerDrawItemNo);
    if GetItem(DrawItems[ResizerDrawItemNo].ItemNo).GetBoolValue(rvbpResizeHandlesOutside) then
      FResizer.Position := rvhpOutside;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.UpdateResizer;
begin
  if (FResizer<>nil) and
     ((FResizer.DrawItemNo>=DrawItems.Count) or
      (DrawItems[FResizer.DrawItemNo]<>FResizer.DrawItem)) then
    CreateResizer;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.DoOnSelection(AllowScrolling: Boolean);

begin
  TCustomRichViewEdit(FRichView).Selecting;
  {$IFNDEF RVDONOTUSELISTS}
  if (FSelEndNo>=0) and (GetItemStyle(DrawItems[FSelEndNo].ItemNo)=rvsListMarker) then begin
    inc(FSelEndNo);
    FSelEndOffs := GetOffsBeforeDrawItem(FSelEndNo);
  end;
  {$ENDIF}
  if (FSelEndNo>=0) then begin
    CaretDrawItemNo := FSelEndNo;
    OnChangeCaretLine(FSelEndOffs-2);
  end;
  CreateResizer;
  {$IFDEF RVDEBUG}{$I Debug\i.inc}{$ENDIF}
  ChangeCaret(False,AllowScrolling,False,False);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Reformat(FullFormat,ForceFormat,NoScroll: Boolean; ItemNo: Integer; UpdateView:  Boolean);
var ln, lo, dlo: Integer;
    curdlno,dummy: Integer;
begin
  DrawItem2Item(CaretDrawItemNo, CharEnds.Items[CaretOffs].Offset, ln, lo);
  if FullFormat then begin
    Format_(True,ForceFormat,NoScroll, 0,GetCanvas,False, False);
    Invalidate;
    end
  else begin
    if ln=ItemNo then
      curdlno := CaretDrawItemNo
    else
      Item2DrawItem(ItemNo,0,curdlno,dummy);
    FormatParas(curdlno,curdlno,0);
  end;
  Item2DrawItem(ln, lo, CaretDrawItemNo, dlo);
  OnChangeCaretLine(dlo-2);
  ChangeCaret( False,UpdateView,False,UpdateView);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.InsertFirstRVFItem(var Index: Integer;
  var s: String; var li: TCustomRVItemInfo; EditFlag: Boolean;
  var FullReformat: Boolean;
  var NewListNo: Integer):Boolean;
var ItemsAdded: Integer;
begin
  if EditFlag then begin
    Result := InsSomething(li, s, False, Index, ItemsAdded, FullReformat, NewListNo);
    FRVFInserted := True;
    if not Result then Beep;
    end
  else
    Result := (inherited InsertFirstRVFItem(Index, s, li, EditFlag, FullReformat, NewListNo));
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.AdjustControlPlacement(ItemNo: Integer);
var dlno: Integer;
    NewWidth, NewHeight, OldWidth, OldHeight: Integer;
begin
  CheckItemClass(ItemNo, TRVControlItemInfo);
  dlno := FindDrawItemByItem(ItemNo);
  if dlno<0 then
    exit;
  with Items.Objects[ItemNo] as TRVControlItemInfo do
    if (Control<>ResizingControl) then begin
      OldWidth  := DrawItems[dlno].Width-2;
      OldHeight := DrawItems[dlno].Height-2;
      NewWidth  := Control.Width;
      NewHeight := Control.Height;
      if (NewWidth<>OldWidth) or
         (NewHeight<>OldHeight) then
        DoResizeControl(ItemNo,OldWidth,OldHeight,NewWidth,NewHeight)
      else begin
        Control.Left := DrawItems[dlno].Left+1-GetHOffs;
        Control.Tag := DrawItems[dlno].Top+1-GetVOffs;
        RV_Tag2Y(Control);
      end;
    end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.ResizeControl(ItemNo, NewWidth, NewHeight: Integer;Reformat: Boolean);
var OldWidth, OldHeight:Integer;
begin
  CheckItemClass(ItemNo, TRVControlItemInfo);
  with Items.Objects[ItemNo] as TRVControlItemInfo do begin
    OldWidth  := Control.Width;
    OldHeight := Control.Height;
    if (OldWidth=NewWidth) and (OldHeight=NewHeight) then exit;
    ResizingControl := Control;
    try
      Control.SetBounds(Control.Left, Control.Top, NewWidth, NewHeight);
    finally
      ResizingControl := nil;
    end;
  end;
  if Reformat then
    DoResizeControl(ItemNo,OldWidth,OldHeight,NewWidth,NewHeight);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.DoResizeControl(ItemNo, OldWidth,OldHeight,NewWidth,NewHeight: Integer);
var FullReformat: Boolean;
    NewFWidth, OldFWidth: Integer;
begin
  NewFWidth := CalculateMinItemWidthPlusEx(ItemNo);
  OldFWidth := NewFWidth-NewWidth+OldWidth;
  FullReformat := (OldFWidth<>NewFWidth) and
                  ((OldFWidth>=DocumentWidth) or
                   (NewFWidth> DocumentWidth));
  Reformat(FullReformat,False,False,ItemNo,True);
  Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.SelectCurrentWord;
var first, last, ItemNo, Offs, Len: Integer;
    ItemOptions: TRVItemOptions;
    s: String;
begin
  PrepareForEdit;
  if GetItemStyle(GetCurItemNo)<0 then exit;
  Offs   := GetOffsetInCurItem;
  ItemNo := GetCurItemNo;
  Last   := Offs;
  First  := Offs;
  s      := Items[ItemNo];
  Len    := ItemLength(ItemNo);
  ItemOptions := GetItemOptions(ItemNo);
  while (Last<=Len) do begin
    if IsDelimiter(s, Last, ItemOptions) then
      break;
    inc(Last);
  end;
  dec(First);
  while (First>0) do begin
    if IsDelimiter(s, First, ItemOptions) then begin
      inc(First);
      break;
    end;
    dec(First);
  end;
  if First=0 then
    inc(First);
  SetSelectionBounds(ItemNo, First, ItemNo, Last);
  Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.InsertPageBreak;
var NeedReformat, FullReformat: Boolean;
begin
  FullReformat := False;
  Deselect(nil, True);
  BeginUndoSequence(rvutInsertPageBreak, True);
  TCustomRichViewEdit(FRichView).SetUndoGroupMode(True);
  try
    if (GetOffsetInCurItem>GetOffsBeforeItem(GetCurItemNo)) or
       (
       GetItem(GetCurItemNo).SameAsPrev
       {$IFNDEF RVDONOTUSELISTS}
       and not ((GetCurItemNo>0) and (GetItemStyle(GetCurItemNo-1)=rvsListMarker))
      {$ENDIF}
       ) then
      OnEnterPress_(False, False);
    NeedReformat := GetItem(GetCurItemNo).BR;
    Do_BR(GetCurItemNo,False, FullReformat);
    Do_PageBreak(GetCurItemNo,True);
  finally
    TCustomRichViewEdit(FRichView).SetUndoGroupMode(False);
  end;
  if NeedReformat then
    Reformat(FullReformat, True, False, GetCurItemNo,True);
  Refresh;
  Change;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.Change;
begin
  ChangeEx(True);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.ChangeEx(ClearRedo: Boolean);
begin
  TCustomRichViewEdit(FRichView).Modified := True;
  if not (rvstDoNotClearCurTag in State) then
    ClearCurTag;
  TCustomRichViewEdit(FRichView).DoChange(ClearRedo);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.SetCurParaStyleNo(Value: Integer);
begin
  TCustomRichViewEdit(FRichView).CurParaStyleNo := Value;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.SetCurTextStyleNo(Value: Integer);
begin
  TCustomRichViewEdit(FRichView).CurTextStyleNo := Value;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEHTML}
function TRVEditRVData.SaveHTMLToStreamEx(Stream: TStream;
                       const Path, Title, ImagesPrefix, ExtraStyles,
                       ExternalCSS, CPPrefix: String;
                       Options: TRVSaveOptions; Color: TColor;
                       var CurrentFileColor: TColor;
                       var imgSaveNo: Integer;
                       LeftMargin, TopMargin, RightMargin, BottomMargin: Integer;
                       Background: TRVBackground;
                       Bullets: TRVList): Boolean;
begin
  if rvflRootEditor in Flags then
    BuildJumpsCoords(False);
  Result := inherited SaveHTMLToStreamEx(Stream, Path, Title, ImagesPrefix, ExtraStyles,
                       ExternalCSS, CPPrefix,
                       Options, Color, CurrentFileColor, imgSaveNo,
                       LeftMargin, TopMargin, RightMargin, BottomMargin,
                       Background,Bullets);
  if rvflRootEditor in Flags then
    ClearJumpsCoords;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.SaveHTMLToStream(Stream: TStream;
                       const Path, Title,ImagesPrefix: String;
                       Options: TRVSaveOptions; Color: TColor;
                       var imgSaveNo: Integer;
                       LeftMargin, TopMargin, RightMargin, BottomMargin: Integer;
                       Background: TRVBackground;
                       Bullets: TRVList): Boolean;
begin
  if rvflRootEditor in Flags then
    BuildJumpsCoords(False);
  Result := inherited SaveHTMLToStream(Stream, Path, Title,ImagesPrefix,
                       Options, Color, imgSaveNo,
                       LeftMargin, TopMargin, RightMargin, BottomMargin,
                       Background,Bullets);
  if rvflRootEditor in Flags then
    ClearJumpsCoords;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRVEditRVData.GetIMEWinCoord: TPoint;
begin
   with CharEnds.Items[CaretOffs] do begin
     Result.x := X-1+MoveRightTo-GetHOffs;
     Result.y := DrawItems[CaretDrawItemNo].Top-1-GetVOffs;
   end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.GetSelectionBoundsEx(var StartItemNo,
  StartItemOffs, EndItemNo, EndItemOffs: Integer; Normalize: Boolean);
begin
  if SelectionExists(False,False) then
    inherited
  else begin
    StartItemNo   := GetCurItemNo;
    EndItemNo     := StartItemNo;
    StartItemOffs := GetOffsetInCurItem;
    EndItemOffs   := StartItemOffs;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.BeginItemModify(ItemNo: Integer; var ModifyData: Integer);
begin
  ModifyData := CalculateMinItemWidthPlusEx(ItemNo);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.EndItemModify(ItemNo, ModifyData: Integer);
var NewW: Integer;
    FullReformat: Boolean;
begin
  NewW := CalculateMinItemWidthPlusEx(ItemNo);
  FullReformat := (NewW<>ModifyData) and
                  ((ModifyData>=DocumentWidth) or (NewW>DocumentWidth));
  Reformat(FullReformat, True, False, ItemNo, True);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.DeselectPartiallySelectedItem(NewPartiallySelected: TCustomRVItemInfo);
var r: Boolean;
begin
  r := (FPartialSelectedItem<>nil) ;
  inherited DeselectPartiallySelectedItem(NewPartiallySelected);
  if r then
    TCustomRichViewEdit(FRichView).AfterCaretMove;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.SetPartialSelectedItem(Item: TCustomRVItemInfo);
var r: Boolean;
begin
  r := FPartialSelectedItem<>Item;
  inherited SetPartialSelectedItem(Item);
  if r then
    TCustomRichViewEdit(FRichView).AfterCaretMove;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.MarkStylesInUse(UsedTextStyles,
  UsedParaStyles, UsedListStyles: TRVIntegerList);
begin
  inherited MarkStylesInUse(UsedTextStyles, UsedParaStyles, UsedListStyles);
  if FCurTextStyleNo<>-1 then begin
    FCurTextStyleNo := GetActualCurStyleNo;
    UsedTextStyles[FCurTextStyleNo] := 1;
  end;
  if FCurParaStyleNo<>-1 then
    UsedParaStyles[FCurParaStyleNo] := 1;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.AfterDeleteStyles(TextStylesShift,
  ParaStylesShift, ListStylesShift: TRVIntegerList);
{$IFNDEF RVDONOTUSEINPLACE}
var inplace: TControl;
{$ENDIF}
begin
  TCustomRichViewEdit(FRichView).ClearUndo;
  if FCurTextStyleNo<>-1 then
    dec(FCurTextStyleNo, TextStylesShift[FCurTextStyleNo]-1);
  if TextStylesShift[FPrevTextStyleNo]<>0 then
    dec(FPrevTextStyleNo, TextStylesShift[FPrevTextStyleNo]-1)
  else
    FPrevTextStyleNo := 0;
  if FCurParaStyleNo<>-1 then
    dec(FCurParaStyleNo, ParaStylesShift[FCurParaStyleNo]-1);
  {$IFNDEF RVDONOTUSEINPLACE}
  inplace := GetInplaceEditor;
  if inplace=nil then
  {$ENDIF}
  begin
    State := State + [rvstForceStyleChangeEvent];
    SetCurTextStyleNo(FCurTextStyleNo);
    State := State + [rvstForceStyleChangeEvent];
    SetCurParaStyleNo(FCurParaStyleNo);
  end;
  inherited AfterDeleteStyles(TextStylesShift, ParaStylesShift, ListStylesShift);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSELISTS}
function TRVEditRVData.ReplicateMarker(ReferenceItemNo,
  InsertItemNo: Integer; var FullReformat: Boolean;
  EditFlag: Boolean): Boolean;
var Marker: TRVMarkerItemInfo;
    s: String;
begin
  Result := False;
  if ReferenceItemNo<0 then
    exit;
  ReferenceItemNo := GetFirstParaItem(ReferenceItemNo);
  if GetItemStyle(ReferenceItemNo)<>rvsListMarker then
    exit;
  Marker := TRVMarkerItemInfo(RV_DuplicateItem(GetItem(ReferenceItemNo), Self,
    False));
  Marker.DeleteProtect := False;
  Marker.Reset := False;
  s := Items[ReferenceItemNo];
  if EditFlag then
    Do_InsertItem(InsertItemNo, s, Marker, False, FullReformat)
  else begin
    Marker.Inserting(Self, s, False);
    Items.InsertObject(InsertItemNo,s,Marker);
    Marker.Inserted(Self, InsertItemNo);
    AddMarkerInList(InsertItemNo);
  end;
  Result := True;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.AdjustMarkerPos(var ItemNo, Offs: Integer;
  DefRight: Boolean);
var Right: Boolean;
begin
  if GetItemStyle(ItemNo)<>rvsListMarker then
    exit;
  Right := (Offs=1) or DefRight;
  if ItemNo=0 then
    Right := True;
  if Right and (ItemNo+1>=Items.Count) then
    exit;
  if Right then begin
    inc(ItemNo);
    Offs := GetOffsBeforeItem(ItemNo);
    end
  else begin
    dec(ItemNo);
    Offs := GetOffsAfterItem(ItemNo);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.AdjustMarkerCaret(Right: Boolean; var Offs: Integer);
begin
  if GetItemStyle(DrawItems[CaretDrawItemNo].ItemNo)<>rvsListMarker then
    exit;
  if CaretDrawItemNo=0 then
    Right := True;
  if Right then begin
    inc(CaretDrawItemNo);
    Offs := GetOffsBeforeDrawItem(CaretDrawItemNo);
    end
  else begin
    dec(CaretDrawItemNo);
    Offs := GetOffsAfterDrawItem(CaretDrawItemNo);
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRVEditRVData.CaretAtTheBeginningOfParaSection: Boolean;
var item: TCustomRVItemInfo;
    dli: TRVDrawLineInfo;
begin
  CaretDrawItemNo := CharEnds.Items[CaretOffs].DrawItemNo;
  dli := DrawItems[CaretDrawItemNo];
  item := GetItem(dli.ItemNo);
  if CaretOffs=0 then begin
    Result := (dli.Offs+CharEnds.Items[CaretOffs].Offset-1<=GetOffsBeforeItem(dli.ItemNo));
    if Result then
      Result := not item.SameAsPrev
      {$IFNDEF RVDONOTUSELISTS}
      or (item.SameAsPrev and (dli.ItemNo>0) and (GetItemStyle(dli.ItemNo-1)=rvsListMarker))
      {$ENDIF}
      ;
    end
  else
    Result := False;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.CaretAtTheEndOfParaSection: Boolean;
var item: TCustomRVItemInfo;
    dli: TRVDrawLineInfo;
begin
  CaretDrawItemNo := CharEnds.Items[CaretOffs].DrawItemNo;
  dli := DrawItems[CaretDrawItemNo];
  item := GetItem(dli.ItemNo);
  Result := (CaretOffs=CharEnds.Count-1) and
      ((dli.ItemNo+1=Items.Count) or
       (not GetItem(dli.ItemNo+1).SameAsPrev)) and
      ((item.StyleNo<0) or
       (dli.Offs+CharEnds.Items[CaretOffs].Offset-1>ItemLength(dli.ItemNo))
      )
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.CaretInTheLastLine: Boolean;
begin
  Result := CharEnds.Items[CharEnds.Count-1].DrawItemNo = DrawItems.Count-1;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.CaretAtTheBeginningOfLine: Boolean;
begin
  Result := CaretOffs=0;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.CaretAtTheEndOfLine: Boolean;
begin
  Result := CaretOffs=CharEnds.Count-1;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSELISTS}

procedure TRVEditRVData.ApplyListStyle_(AListNo, AListLevel, AStartFrom: Integer;
  AUseStartFrom, ARecursive: Boolean; Operation: TRVParaListOperation;
  var ItemsAdded, StartNo, EndNo, SelStartNo, SelEndNo: Integer;
  ListNos: TRVIntegerList; var LastVWMarkerIndex: Integer);
var i: Integer;
    FR: Boolean;
    OldMarker, Marker: TRVMarkerItemInfo;
    OldListNo, OldLevel, OldStartFrom: Integer;
    OldUseStartFrom: Boolean;
    OldMarkerCp: TRVCPInfo;
    OldMarkerTag: Integer;
    OldMarkerDeleteProtect: Boolean;
    s: String;
    Markers: TRVMarkerList;
begin
  ExpandToPara(StartNo, EndNo, StartNo, EndNo);

  ItemsAdded := 0;
  OldListNo       := -1;
  OldLevel        := -1;
  OldStartFrom    := -1;
  OldUseStartFrom := False;

  Markers := GetMarkers(False);
  LastVWMarkerIndex := -1;
  OldMarkerCp     := nil;
  OldMarkerTag    := 0;
  OldMarkerDeleteProtect := False;

  for i := EndNo downto StartNo do
    if IsParaStart(i) and not GetItem(i).GetBoolValue(rvbpFullWidth) then begin
       OldMarker := nil;
       if GetItemStyle(i)=rvsListMarker then begin
         OldListNo := TRVMarkerItemInfo(GetItem(i)).ListNo;
         if (OldListNo>=0) and (GetRVStyle.ListStyles[OldListNo].HasVariableWidth) then begin
           ListNos.AddUnique(OldListNo);
           LastVWMarkerIndex := TRVMarkerItemInfo(GetItem(i)).GetIndexInList(Markers);
         end;
         if (Operation in [rvplopLevel,rvplopChange]) then begin
           OldMarker       := TRVMarkerItemInfo(GetItem(i));
           OldListNo       := OldMarker.ListNo;
           OldLevel        := OldMarker.Level;
           OldStartFrom    := OldMarker.StartFrom;
           OldUseStartFrom := OldMarker.Reset;
           if OldMarker.Checkpoint<>nil then
             OldMarkerCp :=
               OldMarker.Checkpoint.CreateCopy(rvoTagsArePChars in Options)
           else
             OldMarkerCp := nil;
           OldMarkerTag := RV_CopyTag(OldMarker.Tag, rvoTagsArePChars in Options);
           OldMarkerDeleteProtect := OldMarker.DeleteProtect;
         end;
         if (Operation=rvplopRemove) and (i+1<Items.Count) then
           Do_NewLine(i+1, False, -1, FR);
         Do_DeleteItem(i, FR);
         dec(ItemsAdded);
         if SelStartNo>=i then
           dec(SelStartNo);
         if SelEndNo>=i then
           dec(SelEndNo);
         dec(EndNo);
       end;
       if (Operation<>rvplopRemove) then begin
         s := '';
         if (Operation = rvplopLevel) and (OldMarker<>nil) then begin
           Marker := TRVMarkerItemInfo.CreateEx(Self, OldListNo,
             OldLevel+AListLevel, OldStartFrom, OldUseStartFrom);
           if Marker.Level>=GetRVStyle.ListStyles[Marker.ListNo].Levels.Count then
             Marker.Level := GetRVStyle.ListStyles[Marker.ListNo].Levels.Count-1;
           if Marker.Level<0 then
             Marker.Level := 0;
           end
         else if Operation=rvplopChange then begin
           if AListLevel<0 then begin
             if OldMarker=nil then
               OldLevel := 0
             end
           else
             OldLevel := AListLevel;
           Marker := TRVMarkerItemInfo.CreateEx(Self, AListNo, OldLevel, AStartFrom, AUseStartFrom and (i=StartNo));
           end
         else
           Marker := nil;
         if Marker<>nil then begin
           if OldMarker<>nil then begin
             Marker.Checkpoint := OldMarkerCP;
             Marker.Tag := OldMarkerTag;
             Marker.DeleteProtect := OldMarkerDeleteProtect;
           end;
           Marker.ParaNo := GetItemPara(i);
           if SelStartNo>=i then
             inc(SelStartNo);
           if SelEndNo>=i then
             inc(SelEndNo);
           inc(EndNo);
           Do_InsertItem(i, s, marker, False, FR);
           Do_NewLine(i+1, True, Marker.ParaNo, FR);
           inc(ItemsAdded);
         end;
       end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.ApplyListStyle(AListNo, AListLevel, AStartFrom: Integer;
                               AUseStartFrom, ARecursive: Boolean;
                               Operation: TRVParaListOperation);
var StartNo, EndNo, StartOffs, EndOffs: Integer;
    OldWidth, NewWidth, ItemsAdded: Integer;
    DIStartNo, DIEndNo: Integer;
    SL, EL, SO, EO: Integer;
    LastVWMarkerIndex: Integer;
    ListNos: TRVIntegerList;
begin
  LastVWMarkerIndex := -1;
  ListNos := TRVIntegerList.Create;
  try
    BeginUndoSequence(rvutList, True);
    SetUndoGroupMode(True);
    try
      GetSelectionBoundsEx(StartNo, StartOffs, EndNo, EndOffs, True);
      GetSelectionBoundsEx(SL, SO, EL, EO, False);
      Item2FirstDrawItem(StartNo,DIStartNo);
      Item2FirstDrawItem(EndNo,DIEndNo);
      GetParaBounds(DIStartNo, DIEndNo,DIStartNo, DIEndNo);

      ExpandToPara(StartNo, EndNo, StartNo, EndNo);
      OldWidth := CalculateParaSectionsMinWidthDef(StartNo, EndNo);
      ApplyListStyle_(AListNo, AListLevel, AStartFrom,
        AUseStartFrom, ARecursive, Operation, ItemsAdded,StartNo, EndNo, SL, EL, ListNos, LastVWMarkerIndex);
      NewWidth := CalculateParaSectionsMinWidthDef(StartNo, EndNo);
      try
        Include(State, rvstInvalidSelection);
        Reformat_((OldWidth<>NewWidth) and ((NewWidth>DocumentWidth) or (OldWidth>=DocumentWidth)),
                DIStartNo, DIEndNo, ItemsAdded);
      finally
        Exclude(State, rvstInvalidSelection);
      end;
      Item2DrawItem(SL,SO, FSelStartNo, FSelStartOffs);
      Item2DrawItem(EL,EO, FSelEndNo, FSelEndOffs);
      CaretDrawItemNo := FSelEndNo;
      OnChangeCaretLine(FSelEndOffs-2);
      ChangeCaret(False, True, False, False);
    finally
      SetUndoGroupMode(False);
      Change;
    end;
    UpdateAfterMarkers(EndNo, LastVWMarkerIndex, ListNos, -1);
  finally
    ListNos.Free;
  end;
  GetParentControl.Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.UpdateAfterMarkers(FirstItemNo,
  LastMarkerIndex: Integer; ListNos: TRVIntegerList; ListNo: Integer);
var ListNosCreated: Boolean;
    LastMarkerItemNo, StartNo, EndNo, Dummy: Integer;
    Markers : TRVMarkerList;
begin
  if (ListNos=nil) and (ListNo<0) then
    exit;
  Markers := GetMarkers(False);
  if Markers=nil then
    exit;
  ListNosCreated := ListNos=nil;
  if ListNosCreated then
    ListNos := TRVIntegerList.CreateEx(1, ListNo);
  if (ListNos.Count>0) then begin
    LastMarkerIndex := FindLastMarkerIndex(LastMarkerIndex, ListNos);
    if LastMarkerIndex>=0 then begin
      LastMarkerItemNo := FindMarkerLocalLocationFrom(FirstItemNo+1, TRVMarkerItemInfo(Markers[LastMarkerIndex]));
      if LastMarkerItemNo>=0 then begin
        ExpandToPara(FirstItemNo+1, LastMarkerItemNo, StartNo, EndNo);
        Item2FirstDrawItem(StartNo, StartNo);
        Item2DrawItem(EndNo, GetOffsAfterItem(EndNo), EndNo, Dummy);
        Do_ReformateRange(StartNo, EndNo, False);
        FormatParasExact(StartNo, EndNo, 0, True);
        end
      else begin
        (GetAbsoluteRootData as TRichViewRVData).Format_(True, True, True, 0, nil, False, True);
        Do_ReformateRange(-1, -1, True);
      end;
    end;
  end;
  if ListNosCreated then
    ListNos.Free;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.PrepareForUpdateRangeAfterMarkers(StartNo,
  EndNo: Integer; ForDeletion: Boolean;
  var FirstItemNo, LastMarkerIndex: Integer;
  var ListNos: TRVIntegerList);
var i, ListNo: Integer;
    Style: TRVStyle;
    Markers : TRVMarkerList;
begin
  ListNos := nil;
  Markers := GetMarkers(False);
  if Markers=nil then
    exit;
  if StartNo<0 then
    exit;
  AdjustInItemsRange(StartNo);
  AdjustInItemsRange(EndNo);
  ExpandToPara(StartNo, EndNo, StartNo, EndNo);
  ListNos := TRVIntegerList.Create;
  LastMarkerIndex := -1;
  Style := GetRVStyle;
  for i := StartNo to EndNo do
    if (GetItemStyle(i)=rvsListMarker) then begin
      ListNo := TRVMarkerItemInfo(GetItem(i)).ListNo;
      if (ListNo>=0) and Style.ListStyles[ListNo].HasVariableWidth then begin
        ListNos.AddUnique(ListNo);
        if not ForDeletion or (LastMarkerIndex<0) then
          LastMarkerIndex := TRVMarkerItemInfo(GetItem(i)).GetIndexInList(Markers);
      end;
    end;
  if ForDeletion and (LastMarkerIndex>=0) then
    dec(LastMarkerIndex);
  FirstItemNo := EndNo;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.UpdateRangeAfterMarkers(StartNo, EndNo: Integer);
var ListNos: TRVIntegerList;
    LastMarkerIndex, FirstItemNo: Integer;
begin
  PrepareForUpdateRangeAfterMarkers(StartNo, EndNo, False, FirstItemNo, LastMarkerIndex, ListNos);
  if (ListNos<>nil) and (ListNos.Count>0) then
    UpdateAfterMarkers(FirstItemNo, LastMarkerIndex, ListNos, -1);
  ListNos.Free;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TRVEditRVData.PostPaintTo(Canvas: TCanvas; XShift, YShift,
  FirstDrawItemNo, LastDrawItemNo: Integer);
begin
  if FResizer<>nil then
    FResizer.Draw(Canvas, XShift, YShift);
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.GetResizeHandleAt(X, Y: Integer;
  var Index: TRVResizeHandleIndex): Boolean;
begin
  Result := (FResizer<>nil) and FResizer.GetResizeHandleAt(X, Y, GetHOffs, GetVOffs, Index);
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.MouseMove(Shift: TShiftState; X, Y: Integer);
var Index: TRVResizeHandleIndex;
begin
  if (FResizer<>nil) and FResizer.Dragging then begin
    XorDrawing;
    FResizer.DragTo(Shift, X,Y, GetHOffs, GetVOffs);
    XorDrawingEx(X,Y);
    end
  else if GetResizeHandleAt(X, Y, Index) then begin
    SetCursor(FResizer.GetResizeHandleCursor(Index));
    end
  else
    inherited;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (FResizer<>nil) and FResizer.MouseDown(X,Y, GetHOffs, GetVOffs) then begin
    XorDrawing;
    SetCursor(crCross);
    Windows.SetCursor(Screen.Cursors[crCross]);
    end
  else
    inherited;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var Dragging: Boolean;
begin
  if (FResizer<>nil) and (FResizer.Dragging or FResizer.DragCancelled) then begin
    Dragging := FResizer.Dragging;
    if Dragging then
      ClearXorDrawing;
    FResizer.MouseUp(X,Y, GetHOffs, GetVOffs);
    if Dragging then begin
      SetCursor(GetNormalCursor);
      ResizeItem(FResizer.ItemNo, FResizer.Width, FResizer.Height);
    end;
    end
  else
    inherited;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.XorDrawing;
begin
  if (FResizer<>nil) and (FResizer.Dragging) then begin
    FResizer.XorDrawing(GetCanvas, GetHOffs, GetVOffs);
    XorImageDrawn := not XorImageDrawn;
    end
  else
    inherited;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.CancelResize: Boolean;
begin
  Result := (FResizer<>nil) and FResizer.Dragging;
  if Result then begin
    ClearXorDrawing;
    FResizer.CancelDrag;
    SetCursor(GetNormalCursor);
    Windows.SetCursor(Screen.Cursors[GetNormalCursor]);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.AdjustMouseUpSelection;
begin
  if (FClickedDrawItemNo=LastDIMovedAbove) and (FClickedDrawItemNo>=0) and
     (GetItem(DrawItems[FClickedDrawItemNo].ItemNo).GetBoolValue(rvbpResizable) or
      GetItem(DrawItems[FClickedDrawItemNo].ItemNo).GetBoolValue(rvbpClickSelect)) and
     not SelectionExists(False, False) then begin
    FSelStartNo := FClickedDrawItemNo;
    FSelEndNo   := FClickedDrawItemNo;
    FSelStartOffs := 0;
    FSelEndOffs   := 1;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.ResizeItem(ItemNo, Width, Height: Integer);
var item: TCustomRVItemInfo;
    edit: TCustomRichViewEdit;
    Data: Integer;
begin
  item := GetItem(ItemNo);
  if item is TRVRectItemInfo then begin
    dec(Width, TRVRectItemInfo(item).Spacing*2);
    dec(Height, TRVRectItemInfo(item).Spacing*2);
  end;
  if Width<1 then
    Width := 1;
  if Height<1 then
    Height := 1;
  edit := TCustomRichViewEdit(RichView);
  if not edit.BeforeChange(False) then
    exit;
  BeginUndoSequence(rvutModifyItem, True);
  SetUndoGroupMode(True);
  try
    if item is TRVControlItemInfo then begin
      Do_Resize(ItemNo, Width, Height, True);
      end
    else begin
      BeginItemModify(ItemNo, Data);
      if item is TRVRectItemInfo then begin
        edit.SetItemExtraIntPropertyEd(ItemNo, rvepImageHeight, Height, False);
        edit.SetItemExtraIntPropertyEd(ItemNo, rvepImageWidth, Width, False);
      end;
      EndItemModify(ItemNo, Data);
    end;
  finally
    SetUndoGroupMode(False);
  end;
  Change;
end;
{------------------------------------------------------------------------------}
function TRVEditRVData.GetActualCurStyleNo: Integer;
begin
  Result := GetActualStyle2(FCurTextStyleNo, FCurParaStyleNo);
end;
{------------------------------------------------------------------------------}
{ Returns True if the ItemNo-th item has a checkpoint with "Persistent" flag   }
function TRVEditRVData.ItemHasPersistentCheckpoint(ItemNo: Integer): Boolean;
begin
  Result := (GetItem(ItemNo).Checkpoint<>nil) and
    GetItem(ItemNo).Checkpoint.Persistent;
end;
{------------------------------------------------------------------------------}
{ Returns True if the paragraph containing the ItemNo-th item has a checkpoint
  with "Persistent" flag                                                       }
function TRVEditRVData.ParaHasPersistentCheckpoint(ItemNo: Integer): Boolean;
var i: Integer;
begin
  Result := False;
  i := ItemNo;
  while i>=0 do begin
    Result := ItemHasPersistentCheckpoint(i);
    if Result then
      exit;
    if IsParaStart(i) then
      break;
    dec(i);
  end;
  i := ItemNo+1;
  while i<ItemCount do begin
    if IsParaStart(i) then
      break;
    Result := ItemHasPersistentCheckpoint(i);
    if Result then
      exit;
    inc(i);
  end;
end;
{------------------------------------------------------------------------------}
{ Moves "persistent" checkpoint from the ItemNo-th item to the adjacent item
  in the same paragraph. If possible (and not OnlyToPrev), to the next item.
  If not, to the previous one. If moving is impossible, returns False.
  If the ItemNo-th item does not have "persistent" checkpoint, returns True.   }
function TRVEditRVData.MovePersistentCheckpoint(ItemNo: Integer;
  OnlyToPrev: Boolean): Boolean;
begin
  Result := True;
  if not ItemHasPersistentCheckpoint(ItemNo) then
    exit;
  if not OnlyToPrev and (ItemNo+1<ItemCount) and not IsParaStart(ItemNo+1) and
     not ItemHasPersistentCheckpoint(ItemNo+1) then
    Do_MoveCP(ItemNo, ItemNo+1)
  else if (ItemNo-1>=0) and not IsParaStart(ItemNo) and
     not ItemHasPersistentCheckpoint(ItemNo-1) then
    Do_MoveCP(ItemNo, ItemNo-1)
  else
    Result := False;
end;
{------------------------------------------------------------------------------}
{ Assigns 0 to FCurTag field. Frees memory.                                    }
procedure TRVEditRVData.ClearCurTag;
begin
  if rvoTagsArePChars in Options then
    StrDispose(PChar(FCurTag));
  FCurTag := 0;
end;
{------------------------------------------------------------------------------}
{ Assigns new value to FCurTag field.
  If one text item is selected, and its style is a current text style,
  assigns a copy of tag of this item.
  Otherwise, assigns 0.                                                        }
procedure TRVEditRVData.AssignCurTag;
var ItemNo: Integer;
begin
  ClearCurTag;
  ItemNo := GetOneSelectedItemNo;
  if (ItemNo>=0) and (GetItemStyle(ItemNo)=FCurTextStyleNo) then
    FCurTag := RV_CopyTag(GetItemTag(ItemNo), rvoTagsArePChars in Options);
end;
{------------------------------------------------------------------------------}
{ If only one item is completely selected, returns its index.
  Otherwise, returns -1.                                                       }
function TRVEditRVData.GetOneSelectedItemNo: Integer;
var itemno1, itemno2, itemoffs1, itemoffs2: Integer;
begin
  Result := -1;
  if (FSelStartNo<0) or (FSelEndNo<0) then
    exit;
  GetSelectionBoundsEx(itemno1, itemoffs1, itemno2, itemoffs2, True);
  if itemoffs1>=GetOffsAfterItem(itemno1) then begin
    inc(itemno1);
    if itemno1>=Items.Count then
      exit;
    itemoffs1 := GetOffsBeforeItem(itemno1);
  end;
  if itemoffs2<=GetOffsBeforeItem(itemno2) then begin
    dec(itemno2);
    if itemno2<0 then
      exit;
    itemoffs2 := GetOffsAfterItem(itemno2);
  end;
  if (itemno1=itemno2) and (itemoffs1<=GetOffsBeforeItem(itemno1)) and
     (itemoffs2>=GetOffsAfterItem(itemno2)) then
    Result := itemno1;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEDRAGDROP}
{------------------------------------------------------------------------------}
{ Drag&Drop: IDropTarget related                                               }
{------------------------------------------------------------------------------}
{ Returns information about drag&drop caret location.
  It is used by this RVData and all its children RVDatas (including inplace
  editors)                                                                     }
function TRVEditRVData.GetDragDropCaretInfo: TRVDragDropCaretInfo;
begin
  if rvflRoot in Flags then
    Result := FDragDropCaretInfo
  else
    Result := TCustomRVFormattedData(GetAbsoluteRootData).GetDragDropCaretInfo;
end;
{------------------------------------------------------------------------------}
{ Creates FDragDropCaretInfo (if not created) and increase reference count.
  Only for root editors.                                                       }
procedure TRVEditRVData.CreateDragDropCaretInfo;
begin
  if rvflRoot in Flags then begin
    if FDragDropCaretInfo=nil then
      FDragDropCaretInfo := TRVDragDropCaretInfo.Create;
    inc(FDragDropCaretInfo.RefCount);
  end;
end;
{------------------------------------------------------------------------------}
{ Decrease reference count. If zero, frees FDragDropCaretInfo.
  Only for root editors.                                                       }
procedure TRVEditRVData.ReleaseDragDropCaretInfo;
begin
  if rvflRoot in Flags then
    if FDragDropCaretInfo<>nil then begin
      dec(FDragDropCaretInfo.RefCount);
      if FDragDropCaretInfo.RefCount=0 then begin
        FDragDropCaretInfo.Free;
        FDragDropCaretInfo := nil;
      end;
    end;
end;
{------------------------------------------------------------------------------}
{ Drag&Drop: IDropSource related                                               }
{------------------------------------------------------------------------------}
{ Initializing dragging. Overrides in TRichViewRVData.InitDragging.
  Returns True on success.
  Returns DropSource and OKEffect for call of DoDragDrop.                      }
function TRVEditRVData.InitDragging(var DropSource: TRVDropSource;
  var OKEffect: Integer): Boolean;
begin
  Result := inherited InitDragging(DropSource, OKEffect);
  if Result and (rvflRoot in Flags) and
    not TCustomRichViewEdit(FRichView).ReadOnly and CanDelete then
    OKEffect := OKEffect or DROPEFFECT_MOVE;
end;
{------------------------------------------------------------------------------}
procedure TRVEditRVData.DoneDragging(FDeleteSelection: Boolean);
begin
  inherited DoneDragging(FDeleteSelection);
  if FDeleteSelection then
    TCustomRichViewEdit(RichView).DeleteSelection;
end;
{$ENDIF}

end.
