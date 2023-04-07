
{*******************************************************}
{                                                       }
{       RichView                                        }
{       A set of procedures implementing RichEdit-like  }
{       selection (SelStart and SelLength)              }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

{
  v1.1:
  chg: linear position is counted from 0, like in RichEdit.
    If you need to count it from 1 (for compatibility reasons), remove the
    dot from the define below.
}

{.$DEFINE RVLIN_STARTFROM1}

unit RVLinear;

interface
uses RichView, RVEdit, CRVData, CRVFData, RVUni, RVItem;

function RVGetLinearCaretPos(rve: TCustomRichViewEdit): Integer;
procedure RVSetLinearCaretPos(rve: TCustomRichViewEdit; LinearPos: Integer);
procedure RVGetSelection(rv: TCustomRichView; var SelStart, SelLength: Integer);
procedure RVSetSelection(rv: TCustomRichView; SelStart, SelLength: Integer);

implementation

function GetAbstractCharCountInItem(item: TCustomRVItemInfo; const text: String): Integer; forward;

function GetAbstractCharCountInRVData(RVData: TCustomRVData): Integer;
var i: Integer;
begin
  Result := 0;
  for i := 0 to RVData.Items.Count-1 do
    inc(Result, GetAbstractCharCountInItem(RVData.GetItem(i), RVData.Items[i]));
end;

function GetAbstractCharCountInItem(item: TCustomRVItemInfo; const text: String): Integer;
var StoreSub: TRVStoreSubRVData;
    SubRVData: TCustomRVData;
begin
  if not item.SameAsPrev then
    Result := 1
  else
    Result := 0;
  if item.StyleNo>=0 then begin
    inc(Result, RVU_Length(text, item.ItemOptions));
    exit;
  end;
  inc(Result);
  SubRVData := TCustomRVData(item.GetSubRVData(StoreSub, rvdFirst));
  if SubRVData<>nil then begin
    repeat
      inc(Result, GetAbstractCharCountInRVData(SubRVData.GetRVData));
      SubRVData := TCustomRVData(item.GetSubRVData(StoreSub, rvdNext));
    until SubRVData=nil;
    StoreSub.Free;
  end;
end;

function RichViewToLinear(rv: TCustomRichView; CurRVData, RVData: TCustomRVData; ItemNo, ItemOffs: Integer;
  var LinearPos: Integer): Boolean;
var i, SubLinPos: Integer;
    StoreSub: TRVStoreSubRVData;
    SubRVData: TCustomRVData;
    item: TCustomRVItemInfo;
begin
  Result := False;
  LinearPos := 0;
  if CurRVData=RVData then begin
    for i := 0 to ItemNo-1 do
      inc(LinearPos, GetAbstractCharCountInItem(CurRVData.GetItem(i), CurRVData.Items[i]));
    if CurRVData.GetItemStyle(ItemNo)>=0 then begin
      if CurRVData.IsFromNewLine(ItemNo) then
        inc(LinearPos);
      inc(LinearPos, ItemOffs-1)
      end
    else if ItemOffs>0 then
      inc(LinearPos, GetAbstractCharCountInItem(CurRVData.GetItem(ItemNo), CurRVData.Items[ItemNo]))
    else if CurRVData.IsFromNewLine(ItemNo) then
      inc(LinearPos);
    Result := True;
    end
  else begin
    for i := 0 to CurRVData.Items.Count-1 do begin
      item := CurRVData.GetItem(i);
      SubRVData := TCustomRVData(item.GetSubRVData(StoreSub, rvdFirst));
      if SubRVData<>nil then begin
         if not item.SameAsPrev then
           inc(LinearPos);
         repeat
           Result := RichViewToLinear(rv, SubRVData.GetRVData, RVData, ItemNo, ItemOffs, SubLinPos);
           inc(LinearPos, SubLinPos);
           if Result then
             break;
           SubRVData := TCustomRVData(item.GetSubRVData(StoreSub, rvdNext));
         until SubRVData=nil;
         StoreSub.Free;
         if Result then
           exit;
         inc(LinearPos);
        end
      else
        inc(LinearPos, GetAbstractCharCountInItem(item, CurRVData.Items[i]))
    end;
  end;
end;

function LinearToRichView(rv: TCustomRichView; CurRVData: TCustomRVData;
  var LinearPos: Integer; var RVData: TCustomRVData; var ItemNo, ItemOffs: Integer): Boolean;
var i, SubLinPos: Integer;
    StoreSub: TRVStoreSubRVData;
    SubRVData: TCustomRVData;
    item: TCustomRVItemInfo;
begin
  Result := False;
  for i := 0 to CurRVData.Items.Count-1 do begin
    item := CurRVData.GetItem(i);
    SubRVData := TCustomRVData(item.GetSubRVData(StoreSub, rvdFirst));
    if SubRVData<>nil then begin
      if not item.SameAsPrev then
        dec(LinearPos);
      if LinearPos=0 then begin
        RVData := CurRVData;
        ItemNo := i;
        ItemOffs := 0;
        Result := True;
        exit;
      end;
      repeat
        Result := LinearToRichView(rv, SubRVData.GetRVData, LinearPos, RVData, ItemNo, ItemOffs);
        if Result then
          break;
        SubRVData := TCustomRVData(item.GetSubRVData(StoreSub, rvdNext));
      until SubRVData=nil;
      StoreSub.Free;
      if Result then
        exit;
      dec(LinearPos);
      if LinearPos=0 then begin
        RVData := CurRVData;
        ItemNo := i;
        ItemOffs := 1;
        Result := True;
        exit;
      end;
      end
    else begin
      SubLinPos := GetAbstractCharCountInItem(item, CurRVData.Items[i]);
      if SubLinPos>=LinearPos then begin
        if not item.SameAsPrev then
          dec(LinearPos);
        RVData := CurRVData;
        ItemNo := i;
        ItemOffs := LinearPos;
        if item.StyleNo>=0 then
          inc(ItemOffs);
        Result := True;
        exit;
      end;
      dec(LinearPos, SubLinPos);
    end;
  end;
end;

function RVGetLinearCaretPos(rve: TCustomRichViewEdit): Integer;
var tle: TCustomRichViewEdit;
begin
  tle := rve;
  while tle.InplaceEditor<>nil do
    tle := TCustomRichViewEdit(tle.InplaceEditor);
  if tle.CurItemNo<0 then
    Result := 1
  else
    RichViewToLinear(rve, rve.RVData, tle.RVData, tle.CurItemNo, tle.OffsetInCurItem, Result);
  {$IFNDEF RVLIN_STARTFROM1}
  dec(Result);
  {$ENDIF}
end;

procedure RVSetLinearCaretPos(rve: TCustomRichViewEdit; LinearPos: Integer);
var RVData: TCustomRVData;
    ItemNo, ItemOffs: Integer;
begin
  {$IFNDEF RVLIN_STARTFROM1}
  inc(LinearPos);
  {$ENDIF}
  if LinearToRichView(rve, rve.RVData, LinearPos, RVData, ItemNo, ItemOffs) then begin
    RVData := RVData.Edit;
    TCustomRVFormattedData(RVData).SetSelectionBounds(ItemNo, ItemOffs, ItemNo, ItemOffs);
    TCustomRVFormattedData(RVData).Invalidate;
  end;
end;

procedure RVGetSelection(rv: TCustomRichView; var SelStart, SelLength: Integer);
var ItemNo1, ItemNo2, ItemOffs1, ItemOffs2: Integer;
    tle: TCustomRichView;
begin
  tle := rv;
  while tle.InplaceEditor<>nil do
    tle := TCustomRichView(tle.InplaceEditor);
  tle.RVData.GetSelectionBoundsEx(ItemNo1, ItemOffs1, ItemNo2, ItemOffs2, False);
  if ItemNo1<0 then begin
    SelStart := 1;
    SelLength := 0;
    end
  else begin
    RichViewToLinear(rv, rv.RVData, tle.RVData, ItemNo1, ItemOffs1, SelStart);
    RichViewToLinear(rv, rv.RVData, tle.RVData, ItemNo2, ItemOffs2, SelLength);
    SelLength := SelLength-SelStart;
    {$IFNDEF RVLIN_STARTFROM1}
    dec(SelStart);
    {$ENDIF}
  end;
end;

procedure RVSetSelection(rv: TCustomRichView; SelStart, SelLength: Integer);
var ItemNo1, ItemNo2, ItemOffs1, ItemOffs2: Integer;
  RVData1, RVData2: TCustomRVData;
begin
  {$IFNDEF RVLIN_STARTFROM1}
  inc(SelStart);
  {$ENDIF}
  inc(SelLength, SelStart);
  if LinearToRichView(rv, rv.RVData, SelStart, RVData1, ItemNo1, ItemOffs1) and
     LinearToRichView(rv, rv.RVData, SelLength, RVData2, ItemNo2, ItemOffs2) and
     (RVData1=RVData2)
  then begin
    RVData1 := RVData1.Edit;
    TCustomRVFormattedData(RVData1).SetSelectionBounds(ItemNo1, ItemOffs1, ItemNo2, ItemOffs2);
    TCustomRVFormattedData(RVData1).Invalidate;
  end;
end;


end.
