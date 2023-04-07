
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRVSerializer - class converting RichView       }
{       coordinates (RVData, ItemNo) to linear          }
{       coordinates (AbsoluteItemNo) and vice versa.    }
{       Used in parsers for spell-checkers.             }
{       See RVLinear.pas for converting                 }
{       (RVData, ItemNo, Offset) to AbsoluteItemNo'.    }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVSer;

interface
uses SysUtils, RVClasses,
     RichView, CRVData, CRVFData, RVERVData, RVItem, RVEdit, RVRVData;

type
  TRVSerEntry = class
    public
      AbsoluteItemNo: Integer;
      RVData: TCustomRVFormattedData;
      ParentEntryNo, PrevEntryNo: Integer;
      ParentItemNo: Integer;
      ParentSubPos: TRVStoreSubRVData;
      FirstItemNo,LastItemNo : Integer;
      constructor Create(ARVData: TCustomRVFormattedData; AAbsoluteItemNo: Integer);
      destructor Destroy; override;
  end;

  TRVSerializer = class (TRVList)
    private
      RootRVData: TCustomRVFormattedData;
      CurEntryNo: Integer;
      FBeyondTheEnd: Boolean;
    public
      constructor Create(RVData: TCustomRVFormattedData);
      function Expand: Boolean;
      function Shrink: Boolean;
      function ExpandToRV(RVData: TCustomRVFormattedData; ItemNo: Integer): Boolean;
      function ExpandTo(AbsoluteItemNo: Integer): Boolean;
      function RollBackTo(AbsoluteItemNo: Integer): Boolean;
      function RollBackToRV(RVData: TCustomRVFormattedData; ItemNo: Integer): Boolean;
      procedure GoToCaret;
      procedure GoToSelEnd(var AbsItemNo1, Offs1, AbsItemNo2, Offs2: Integer);
      procedure AbsoluteToRV(AbsoluteItemNo: Integer;
                             var RVData: TCustomRVFormattedData;
                             var ItemNo: Integer);
      procedure RVToAbsolute(RVData: TCustomRVFormattedData;
                             ItemNo: Integer;
                             var AbsoluteItemNo: Integer);
      procedure GoToPosRV(RVData: TCustomRVFormattedData;
                             ItemNo: Integer);
      procedure GoToPos(AbsoluteItemNo: Integer);
      procedure CurPosToRV(var RVData: TCustomRVFormattedData;
                           var ItemNo: Integer);
      function CurPos: Integer;
      procedure EditAtCurPos;
      property BeyondTheEnd: Boolean read FBeyondTheEnd;
  end;

implementation

{============================== TRVSerEntry ===================================}
constructor TRVSerEntry.Create(ARVData: TCustomRVFormattedData; AAbsoluteItemNo: Integer);
begin
  inherited Create;
  RVData        := ARVData;
  AbsoluteItemNo := AAbsoluteItemNo;
  ParentEntryNo := -1;
  ParentItemNo  := -1;
  PrevEntryNo   := -1;
  LastItemNo    := -1;
end;
{------------------------------------------------------------------------------}
destructor TRVSerEntry.Destroy;
begin
  if PrevEntryNo>=0 then
    ParentSubPos.Free;
  inherited;
end;
{============================== TRVSerializer =================================}
constructor TRVSerializer.Create(RVData: TCustomRVFormattedData);
var Entry: TRVSerEntry;
begin
  inherited Create;
  RootRVData := RVData;
  Entry := TRVSerEntry.Create(RootRVData,0);
  Add(Entry);
  CurEntryNo := 0;
end;
{------------------------------------------------------------------------------}
function TRVSerializer.Expand: Boolean;
var NewEntry,ParentEntry,Entry, LastEntry: TRVSerEntry;
    RVData: TCustomRVFormattedData;
    SubPos: TRVStoreSubRVData;
begin
  Result := True;
  Entry := TRVSerEntry(Items[CurEntryNo]);
  if Entry.LastItemNo<Entry.RVData.GetRVData.Items.Count-1 then begin
    inc(Entry.LastItemNo);
    RVData := TCustomRVFormattedData(Entry.RVData.GetRVData.GetItem(Entry.LastItemNo).GetSubRVData(SubPos, rvdFirst));
    if RVData<>nil then begin
      // if new item is a table, adding first cell
      LastEntry := TRVSerEntry(Items[Count-1]);
      NewEntry := TRVSerEntry.Create(RVData, LastEntry.AbsoluteItemNo+LastEntry.LastItemNo-LastEntry.FirstItemNo+1);
      NewEntry.ParentEntryNo := CurEntryNo;
      NewEntry.ParentItemNo  := Entry.LastItemNo;
      NewEntry.ParentSubPos := SubPos;
      Add(NewEntry);
      CurEntryNo := Count-1;
      Result := Expand;
    end;
    end
  else begin
    if Entry.ParentSubPos<>nil then begin
      // if last entry is a cell, trying to add next cell
      SubPos := Entry.ParentSubPos.Duplicate;
      ParentEntry := TRVSerEntry(Items[Entry.ParentEntryNo]);
      RVData := TCustomRVFormattedData(ParentEntry.RVData.GetRVData.GetItem(Entry.ParentItemNo).GetSubRVData(SubPos, rvdNext));
      if RVData<>nil then begin
        // adding a new cell
        LastEntry := TRVSerEntry(Items[Count-1]);
        NewEntry := TRVSerEntry.Create(RVData, LastEntry.AbsoluteItemNo+LastEntry.LastItemNo-LastEntry.FirstItemNo+1);
        NewEntry.ParentEntryNo := Entry.ParentEntryNo;
        NewEntry.ParentItemNo  := Entry.ParentItemNo;
        NewEntry.ParentSubPos  := SubPos;
        Add(NewEntry);
        CurEntryNo := Count-1;
        Result := Expand;
        end
      else begin
        SubPos.Free;
        CurEntryNo := Entry.ParentEntryNo;
        Entry := TRVSerEntry(Items[CurEntryNo]);
        if Entry.LastItemNo<Entry.RVData.GetRVData.Items.Count-1 then begin
          // resuming parent entry
          LastEntry := TRVSerEntry(Items[Count-1]);
          NewEntry := TRVSerEntry.Create(Entry.RVData, LastEntry.AbsoluteItemNo+LastEntry.LastItemNo-LastEntry.FirstItemNo+1);
          NewEntry.PrevEntryNo := CurEntryNo;
          NewEntry.ParentEntryNo := Entry.ParentEntryNo;
          NewEntry.ParentItemNo := Entry.ParentItemNo;
          if Entry.ParentSubPos=nil then
            NewEntry.ParentSubPos := nil
          else
            NewEntry.ParentSubPos := Entry.ParentSubPos.Duplicate;
          NewEntry.FirstItemNo := Entry.LastItemNo+1;
          NewEntry.LastItemNo := Entry.LastItemNo;
          Add(NewEntry);
          CurEntryNo := Count-1;
          Result := Expand;
          end
        else begin
          Result := Expand;
        end;
      end;
      end
    else begin
      Result := False; // that's all, folks
      FBeyondTheEnd := True;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVSerializer.Shrink: Boolean;
var Entry: TRVSerEntry;
begin
  if FBeyondTheEnd then begin
    FBeyondTheEnd := False;
    Result := True;
    exit;
  end;
  Result := True;
  if CurEntryNo<>Count-1 then
    raise ERichViewError.Create('Internal error 1');
  Entry := TRVSerEntry(Items[CurEntryNo]);
  dec(Entry.LastItemNo);
  if Entry.LastItemNo<Entry.FirstItemNo then begin
    if CurEntryNo=0 then begin
      Entry.LastItemNo := -1;
      Result := False;
      exit;
    end;
    Delete(CurEntryNo);
    dec(CurEntryNo);
  end;
end;
{------------------------------------------------------------------------------}
function TRVSerializer.ExpandToRV(RVData: TCustomRVFormattedData;
  ItemNo: Integer): Boolean;
var Entry: TRVSerEntry;
    Index: Integer;
begin
  FBeyondTheEnd := False;
  repeat
    Index := Count-1;
    while True do begin
      Entry := TRVSerEntry(Items[Index]);
      if (Entry.RVData=RVData) and (ItemNo<=Entry.LastItemNo) then begin
        CurEntryNo := Index;
        Result := True;
        exit;
      end;
      if Entry.ParentEntryNo<0 then
        break;
      Index := Entry.ParentEntryNo;
    end;
  until not Expand;
  Result := False;
end;
{------------------------------------------------------------------------------}
function TRVSerializer.ExpandTo(AbsoluteItemNo: Integer): Boolean;
begin
  FBeyondTheEnd := False;
  repeat
    if AbsoluteItemNo=CurPos then begin
      Result := True;
      exit;
    end;
  until not Expand;
  Result := False;
end;
{------------------------------------------------------------------------------}
procedure TRVSerializer.GoToCaret;
var RVData: TCustomRVFormattedData;
    ItemNo: Integer;
begin
  RVData := RootRVData;
  while RVData.GetChosenRVData <>nil do
    RVData := TCustomRVFormattedData(RVData.GetChosenRVData);
  ItemNo := (RVData as TRVEditRVData).GetCurItemNo;
  RVData := TCustomRVFormattedData(RVData.GetSourceRVData);
  GoToPosRV(RVData, ItemNo);
end;
{------------------------------------------------------------------------------}
procedure TRVSerializer.GoToSelEnd(var AbsItemNo1, Offs1, AbsItemNo2, Offs2: Integer);
var RVData: TCustomRVFormattedData;
    ItemNo1,ItemNo2: Integer;
begin
  RVData := RootRVData;
  while RVData.GetChosenRVData <>nil do
    RVData := TCustomRVFormattedData(RVData.GetChosenRVData);
  RVData.GetSelectionBoundsEx(ItemNo1,Offs1,ItemNo2,Offs2,True);
  if RVData.GetItemStyle(ItemNo1)>=0 then
    dec(Offs1);
  if RVData.GetItemStyle(ItemNo2)>=0 then
    dec(Offs2);
  RVData := TCustomRVFormattedData(RVData.GetSourceRVData);
  GoToPosRV(RVData, ItemNo2);
  RVToAbsolute(RVData, ItemNo1, AbsItemNo1);
  RVToAbsolute(RVData, ItemNo2, AbsItemNo2);
end;
{------------------------------------------------------------------------------}
function TRVSerializer.RollBackTo(AbsoluteItemNo: Integer): Boolean;
var i,j: Integer;
    Entry: TRVSerEntry;
begin
  if AbsoluteItemNo=-1 then begin
    for j := Count-1 downto 1 do
      Delete(j);
    Entry := TRVSerEntry(Items[0]);
    Entry.LastItemNo := -1;
    CurEntryNo := 0;
    Result := False;
    exit;
  end;
  Result := True;
  for i := Count-1 downto 0 do begin
    Entry := TRVSerEntry(Items[i]);
    if (Count=1) and (Entry.LastItemNo<0) then begin
      Result := False;
      exit;
    end;
    if (AbsoluteItemNo>=Entry.AbsoluteItemNo) then begin
      if (AbsoluteItemNo>Entry.AbsoluteItemNo+(Entry.LastItemNo-Entry.FirstItemNo)) then begin
        Result := False;
        exit;
      end;
      for j := Count-1 downto i+1 do
        Delete(j);
      Entry.LastItemNo := Entry.FirstItemNo+AbsoluteItemNo-Entry.AbsoluteItemNo;
      CurEntryNo := i;
      dec(Entry.LastItemNo);
      Expand;
      exit;
    end;
  end;
  Result := False;
end;
{------------------------------------------------------------------------------}
function TRVSerializer.RollBackToRV(RVData: TCustomRVFormattedData; ItemNo: Integer): Boolean;
var i,j: Integer;
    Entry: TRVSerEntry;
begin
  Result := True;
  for i := Count-1 downto 0 do begin
    Entry := TRVSerEntry(Items[i]);
    if (Count=1) and (Entry.LastItemNo<0) then begin
      Result := False;
      exit;
    end;
    if (RVData=Entry.RVData) and
      (ItemNo>=Entry.FirstItemNo) and (ItemNo<=Entry.LastItemNo) then begin
      for j := Count-1 downto i+1 do
        Delete(j);
      Entry.LastItemNo := ItemNo;
      CurEntryNo := i;
      exit;
    end;
  end;
  Result := False;
end;
{------------------------------------------------------------------------------}
procedure TRVSerializer.AbsoluteToRV(AbsoluteItemNo: Integer;
  var RVData: TCustomRVFormattedData; var ItemNo: Integer);
var i: Integer;
    Entry: TRVSerEntry;
begin
  for i := Count-1 downto 0 do begin
    Entry := TRVSerEntry(Items[i]);
    if (AbsoluteItemNo>=Entry.AbsoluteItemNo) then begin
      if (AbsoluteItemNo>Entry.AbsoluteItemNo+(Entry.LastItemNo-Entry.FirstItemNo)) then
        raise ERichViewError.Create('Internal error 2');
      RVData := Entry.RVData;
      ItemNo := AbsoluteItemNo-Entry.AbsoluteItemNo+Entry.FirstItemNo;
      exit;
    end;
  end;
  raise ERichViewError.Create('Internal error 3');
end;
{------------------------------------------------------------------------------}
procedure TRVSerializer.RVToAbsolute(RVData: TCustomRVFormattedData;
  ItemNo: Integer; var AbsoluteItemNo: Integer);
var i: Integer;
    Entry: TRVSerEntry;
begin
  AbsoluteItemNo := -1;
  for i := Count-1 downto 0 do begin
    Entry := TRVSerEntry(Items[i]);
    if (Count=1) and (Entry.LastItemNo<0) then begin
      exit;
    end;
    if (RVData=Entry.RVData) and
      (ItemNo>=Entry.FirstItemNo) then begin
      AbsoluteItemNo := Entry.AbsoluteItemNo-Entry.FirstItemNo+ItemNo;
      exit;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVSerializer.GoToPosRV(RVData: TCustomRVFormattedData;
                               ItemNo: Integer);
begin
  if not RollBackToRV(RVData, ItemNo) then
    ExpandToRV(RVData, ItemNo);
end;
{------------------------------------------------------------------------------}
procedure TRVSerializer.GoToPos(AbsoluteItemNo: Integer);
begin
  if not RollBackTo(AbsoluteItemNo) then
    ExpandTo(AbsoluteItemNo);
end;
{------------------------------------------------------------------------------}
procedure TRVSerializer.CurPosToRV(var RVData: TCustomRVFormattedData;
  var ItemNo: Integer);
var Entry: TRVSerEntry;
begin
  Entry := TRVSerEntry(Items[CurEntryNo]);
  RVData := Entry.RVData;
  ItemNo := Entry.LastItemNo;
end;
{------------------------------------------------------------------------------}
function TRVSerializer.CurPos: Integer;
var Entry: TRVSerEntry;
begin
  Entry := TRVSerEntry(Items[CurEntryNo]);
  Result := Entry.AbsoluteItemNo+(Entry.LastItemNo-Entry.FirstItemNo);
end;
{------------------------------------------------------------------------------}
procedure TRVSerializer.EditAtCurPos;
var ParentEntry, Entry: TRVSerEntry;
begin
  Entry := TRVSerEntry(Items[CurEntryNo]);
  if Entry.ParentItemNo=-1 then
    exit;
  ParentEntry := TRVSerEntry(Items[Entry.ParentEntryNo]);
  ParentEntry.RVData.GetRVData.GetItem(Entry.ParentItemNo).ChooseSubRVData(Entry.ParentSubPos);
end;

end.
