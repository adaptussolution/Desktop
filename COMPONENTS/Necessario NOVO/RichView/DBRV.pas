
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TDBRichView: displays RVF/RTF/text field in     }
{       a dataset.                                      }
{       TDBRichViewEdit: edits RVF/RTF/text field in    }
{       a dataset.                                      }
{       (registered on "RichView" page of               }
{       the Component Palette)                          }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit DBRV;

interface
{$I RV_Defs.inc}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  RVScroll, RichView, RVEdit, DB, DBCtrls, CRVData, CRVFData
  {$IFNDEF RICHVIEWCBDEF3}
  , DBTables
  {$ENDIF}
  ;
type
  TRVDBFieldFormat = (rvdbRVF, rvdbRTF, rvdbText);


  TDBRichView = class(TCustomRichView)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FAutoDisplay: Boolean;
    FFocused: Boolean;
    FMemoLoaded: Boolean;
    FOnNewDocument: TNotifyEvent;
    FOnLoadDocument: TNotifyEvent;
    procedure DataChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure SetAutoDisplay(Value: Boolean);
  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DblClick; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadField;
    property Field: TField read GetField;
  published
    { Published declarations: new for TDBRichView }
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property OnLoadDocument: TNotifyEvent read FOnLoadDocument write FOnLoadDocument;
    property OnNewDocument: TNotifyEvent read FOnNewDocument write FOnNewDocument;    
    { Published standard properties }
    property Align;
    {$IFDEF RICHVIEWDEF4}
    property Anchors;
    property Constraints;
    {$ENDIF}
    property Color default clNone;
    property Ctl3D;
    {$IFDEF RICHVIEWDEF4}
    property DragKind;
    {$ENDIF}    
    property DragMode;
    property Enabled;
    property HelpContext;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;    
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property Visible;
    { Published standard events }
    property OnClick;
    {$IFDEF RICHVIEWDEF5}
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseMove;
    {$IFDEF RICHVIEWDEF4}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;    
    {$ENDIF}
    property OnStartDrag;
    { Published RichView properties }
    property BackgroundBitmap;
    property BackgroundStyle default bsNoBitmap;
    property BiDiMode;
    property BorderStyle default bsSingle;
    property BottomMargin;
    property CPEventKind default cpeNone;
    property Cursor default crDefault;
    property Delimiters;
    //property DocProperties;
    property DoInPaletteMode;
    property FirstJumpNo;
    property HScrollVisible;
    property LeftMargin;
    property MaxTextWidth;
    property MinTextWidth;
    property Options;
    property RightMargin;
    property RTFOptions;
    property RTFReadProperties;
    property RVFOptions;
    property RVFParaStylesReadMode;
    property RVFTextStylesReadMode;
    {$IFDEF RVFLATSCROLLBARS}
    property ScrollBarColor;
    property ScrollBarStyle;
    {$ENDIF}
    property Style;
    property TabNavigation;
    property TopMargin;
    property Tracking;
    property UseXPThemes;
    {$IFDEF RICHVIEWDEF3}
    property VAlign;
    {$ENDIF}
    property VScrollVisible;
    {$IFDEF RICHVIEWDEF4}
    property WheelStep;
    {$ENDIF}
    { Published RichView events }
    property OnCheckpointVisible;
    property OnControlAction;
    property OnCopy;
    {$IFDEF RV_ODHC}
    property OnDocumentHeightChange;
    {$ENDIF}
    property OnImportPicture;
    property OnItemAction;
    property OnItemHint;
    property OnJump;
    property OnHScrolled;    
    property OnHTMLSaveImage;
    property OnPaint;
    property OnProgress;
    property OnReadHyperlink;    
    property OnRVDblClick;
    property OnRVFImageListNeeded;
    property OnRVFControlNeeded;
    property OnRVFPictureNeeded;
    property OnRVMouseDown;
    property OnRVMouseMove;
    property OnRVMouseUp;
    property OnRVRightClick;
    property OnSaveComponentToFile;
    property OnSaveHTMLExtra;
    property OnSaveImage2;
    property OnSaveItemToFile;    
    property OnSaveRTFExtra;    
    property OnSelect;
    property OnVScrolled;
    property OnWriteHyperlink;
    { obsolete properties }
    property AllowSelection;
    property SingleClick;
    property OnURLNeeded;
  end;
{-----------------------------------------------------------------------}
  TDBRichViewEdit = class(TCustomRichViewEdit)
  private
    { Private declarations }
    FDataLink: TFieldDataLink;
    FAutoDisplay: Boolean;
    FFocused: Boolean;
    FMemoLoaded: Boolean;
    FDataSaveStream: TMemoryStream;
    FFieldFormat: TRVDBFieldFormat;
    FAutoDeleteUnusedStyles: Boolean;
    FOnNewDocument: TNotifyEvent;
    FIgnoreEscape: Boolean;
    FOnLoadDocument: TNotifyEvent;
    procedure DataChange(Sender: TObject);
    procedure EditingChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    function DBGetReadOnly: Boolean;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    procedure SetFocused(Value: Boolean);
    procedure DBSetReadOnly(Value: Boolean);
    procedure SetAutoDisplay(Value: Boolean);
    procedure UpdateData(Sender: TObject);
    procedure CMEnter(var Message: TCMEnter); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMGetDataLink(var Message: TMessage); message CM_GETDATALINK;
    procedure BeginEditing;
  protected
    { Protected declarations }
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure DblClick; override;
    procedure KeyPress(var Key: Char); override;    
  public
    procedure DoChange(ClearRedo: Boolean); override;
    function BeforeChange(FromOutside: Boolean): Boolean; override;
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadField(Check: Boolean);
    property Field: TField read GetField;
  published
    { Published declarations: new for TDBRichViewEdit }
    property IgnoreEscape: Boolean read FIgnoreEscape write FIgnoreEscape default False;
    property AutoDeleteUnusedStyles: Boolean read FAutoDeleteUnusedStyles write FAutoDeleteUnusedStyles default False;
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ReadOnly: Boolean read DBGetReadOnly write DBSetReadOnly;
    property AutoDisplay: Boolean read FAutoDisplay write SetAutoDisplay default True;
    property FieldFormat: TRVDBFieldFormat read FFieldFormat write FFieldFormat default rvdbRVF;
    property OnLoadDocument: TNotifyEvent read FOnLoadDocument write FOnLoadDocument;
    property OnNewDocument: TNotifyEvent read FOnNewDocument write FOnNewDocument;
    { Published declarations: new for TRichViewEdit }
    property AcceptDragDropFormats;
    property EditorOptions;
    property UndoLimit;

    property OnCaretGetOut;
    property OnCaretMove;    
    property OnChange;
    property OnChanging;
    property OnCurParaStyleChanged;
    property OnCurTextStyleChanged;
    {$IFDEF RVONCUT}
    property OnCut;
    {$ENDIF}    
    property OnDropFiles;
    property OnParaStyleConversion;
    property OnPaste;
    property OnStyleConversion;
    property TabNavigation;
    { Published standard properties }
    property Align;
    {$IFDEF RICHVIEWDEF4}
    property Anchors;
    property Constraints;
    {$ENDIF}
    property Color default clNone;
    property Ctl3D;
    {$IFDEF RICHVIEWDEF4}
    property DragKind;
    {$ENDIF}    
    property DragMode;
    property Enabled;
    property HelpContext;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;    
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property UseXPThemes;
    property Visible;
    { Published standard events }
    property OnClick;
    {$IFDEF RICHVIEWDEF5}
    property OnContextPopup;
    {$ENDIF}
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseMove;
    {$IFDEF RICHVIEWDEF4}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;    
    {$ENDIF}
    property OnStartDrag;
    { Published RichView properties }
    property BackgroundBitmap;
    property BackgroundStyle default bsNoBitmap;
    property BiDiMode;
    property BorderStyle default bsSingle;
    property BottomMargin;
    //property CPEventKind;
    property Cursor default crIBeam;
    property Delimiters;
    //property DocProperties;
    property DoInPaletteMode;
    property FirstJumpNo;
    property HScrollVisible;
    property LeftMargin;
    property MaxTextWidth;
    property MinTextWidth;
    property Options;
    property RightMargin;
    property RTFOptions;
    property RTFReadProperties;
    property RVFOptions;
    property RVFParaStylesReadMode;
    property RVFTextStylesReadMode;
    {$IFDEF RVFLATSCROLLBARS}
    property ScrollBarColor;
    property ScrollBarStyle;
    {$ENDIF}
    property Style;
    //property TabNavigation;
    property TopMargin;
    property Tracking;
    {$IFDEF RICHVIEWDEF3}
    property VAlign;
    {$ENDIF}
    property VScrollVisible;
    {$IFDEF RICHVIEWDEF4}
    property WheelStep;
    {$ENDIF}
    { Published RichView events }
    //property OnCheckpointVisible;
    property OnControlAction;
    property OnCopy;
    {$IFDEF RV_ODHC}
    property OnDocumentHeightChange;
    {$ENDIF}
    property OnImportPicture;
    property OnItemAction;
    property OnItemHint;    
    property OnJump;
    property OnHScrolled;    
    property OnHTMLSaveImage;
    property OnPaint;
    property OnProgress;    
    property OnReadHyperlink;
    property OnRVDblClick;
    property OnRVFImageListNeeded;
    property OnRVFControlNeeded;
    property OnRVFPictureNeeded;
    property OnRVMouseDown;
    property OnRVMouseMove;
    property OnRVMouseUp;
    property OnRVRightClick;
    property OnSaveComponentToFile;
    property OnSaveHTMLExtra;
    property OnSaveImage2;
    property OnSaveItemToFile;
    property OnSaveRTFExtra;
    property OnSelect;
    property OnVScrolled;
    property OnWriteHyperlink;
    { obsolete properties }
    property AllowSelection;
    property SingleClick;
    property OnURLNeeded;    
  end;

procedure Register;

implementation
{$IFNDEF RICHVIEWDEF3}
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,P1
        MOV     EDI,P2
        MOV     EDX,ECX
        XOR     EAX,EAX
        AND     EDX,3
        SHR     ECX,2
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
@@1:    INC     EAX
@@2:    POP     EDI
        POP     ESI
end;
{$ENDIF}
{============================DBRichView=================================}
constructor TDBRichView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FAutoDisplay := True;
end;
{-----------------------------------------------------------------------}
destructor TDBRichView.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;
{-----------------------------------------------------------------------}
procedure TDBRichView.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;
{-----------------------------------------------------------------------}
procedure TDBRichView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;
{-----------------------------------------------------------------------}
function TDBRichView.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;
{-----------------------------------------------------------------------}
procedure TDBRichView.SetDataSource(Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;
{-----------------------------------------------------------------------}
function TDBRichView.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;
{-----------------------------------------------------------------------}
procedure TDBRichView.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;
{-----------------------------------------------------------------------}
function TDBRichView.GetField: TField;
begin
  Result := FDataLink.Field;
end;
{-----------------------------------------------------------------------}
procedure TDBRichView.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then LoadField;
  end;
end;
{-----------------------------------------------------------------------}
procedure TDBRichView.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;
{-----------------------------------------------------------------------}
procedure TDBRichView.DblClick;
begin
  if not FMemoLoaded then
    LoadField
  else
    inherited;
end;
{-----------------------------------------------------------------------}
procedure LoadXXXFromStream(Stream: TStream; rv: TCustomRichView);
var s: String;
const RTF_START = '{\rtf';
  {...................................................}
  function IsRTF(Stream: TStream): Boolean;
  var DataStart: String;
  begin
    Result := (Stream.Size>5);
    if not Result then
      exit;
    SetLength(DataStart,5);
    Stream.ReadBuffer(PChar(DataStart)^,5);
    Result := DataStart=RTF_START;
    Stream.Position := 0;
  end;
  {...................................................}
  function AllZero(const s: String):Boolean;
  var i: Integer;
  begin
    Result := False;
    for i := 1 to Length(s) do
      if s[i]<>#0 then
        exit;
    Result := True;
  end;
  {...................................................}
begin
  Stream.Position := 0;
  if not rv.LoadRVFFromStream(Stream) then begin
    Stream.Position := 0;
    if not (IsRTF(Stream) and rv.LoadRTFFromStream(Stream)) then begin
      Stream.Position := 0;
      SetLength(s, Stream.Size);
      Stream.ReadBuffer(PChar(s)^,Stream.Size);
      rv.Clear;
      if AllZero(s) then
        s := '';
      rv.AddTextNL(s, 0,0,0);
    end;
  end;
end;
{-----------------------------------------------------------------------}
procedure TDBRichView.LoadField;
var Stream: TMemoryStream;
begin
  if not FMemoLoaded and Assigned(FDataLink.Field) then
  begin
    Clear;
    try
      Stream := TMemoryStream.Create;
      try
        (FDataLink.Field as TBlobField).SaveToStream(Stream);
        if Assigned(FOnNewDocument) then
          FOnNewDocument(Self);
        LoadXXXFromStream(Stream, Self);
        if Assigned(FOnLoadDocument) then
          FOnLoadDocument(Self);
      finally
        Stream.Free;
      end;
      if RVData.Items.Count = 0 then AddNL('',0,0);
      FMemoLoaded := True;
    except
      on E:EInvalidOperation do
        AddNL(SysUtils.Format('(%s)', [E.Message]),0,0);
    end;
    Format;
    Invalidate;
  end;
end;
{-----------------------------------------------------------------------}
procedure TDBRichView.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
    if {FDataLink.Field.IsBlob} True then
      if FAutoDisplay or (FDataLink.Editing and FMemoLoaded) then begin
        FMemoLoaded := False;
        LoadField;
        end
      else begin
        FMemoLoaded := False;
        Clear;
        AddNL(SysUtils.Format('(%s)', [FDataLink.Field.DisplayLabel]),0,0);
        Format;
        Invalidate;
      end
    else begin
      Clear;
      if FFocused and FDataLink.CanModify then
        AddNL(FDataLink.Field.Text,0,0)
      else
        AddNL(FDataLink.Field.DisplayText,0,0);
      if RVData.Items.Count = 0 then AddNL('',0,0);
      Format;
      Invalidate;
      FMemoLoaded := True;
    end
  else begin
    Clear;
    Format;
    Invalidate;
    FMemoLoaded := False;
  end;
end;
{==========================DBRichViewEdit===============================}
constructor TDBRichViewEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited ReadOnly := True;
  FieldFormat := rvdbRVF;
  FAutoDisplay := True;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FDataLink.OnEditingChange := EditingChange;
  FDataLink.OnUpdateData := UpdateData;
  FDataSaveStream := nil;
  RVData.Flags := RVData.Flags + [rvflDBRichViewEdit];
end;
{-----------------------------------------------------------------------}
destructor TDBRichViewEdit.Destroy;
begin
  FDataSaveStream.Free;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) then DataChange(Self);
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FDataLink <> nil) and
    (AComponent = DataSource) then DataSource := nil;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.DoChange(ClearRedo: Boolean);
begin
  if FMemoLoaded then FDataLink.Modified;
  inherited DoChange(ClearRedo);
end;
{-----------------------------------------------------------------------}
function TDBRichViewEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
  if Value <> nil then Value.FreeNotification(Self);
end;
{-----------------------------------------------------------------------}
function TDBRichViewEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;
{-----------------------------------------------------------------------}
function TDBRichViewEdit.DBGetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.DBSetReadOnly(Value: Boolean);
begin
  FDataLink.ReadOnly := Value;
end;
{-----------------------------------------------------------------------}
function TDBRichViewEdit.GetField: TField;
begin
  Result := FDataLink.Field;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.LoadField(Check: Boolean);
var Stream, Stream2: TMemoryStream;
    sf, equal: Boolean;
    {..............................}
    function HasFocus: Boolean;
    var ctrl: TWinControl;
    begin
      Result := True;
      ctrl := Self;
      while ctrl<>nil do begin
        if ctrl.Focused then
          exit;
        if ctrl is TCustomRichViewEdit then
          ctrl := TCustomRichViewEdit(ctrl).InplaceEditor
        else
          ctrl := nil;
      end;
      Result := False;;
    end;
    {..............................}
begin
  if not FMemoLoaded and Assigned(FDataLink.Field) {and FDataLink.Field.IsBlob} then
  begin
    equal := False;
    sf := HasFocus;
    try
      Stream := TMemoryStream.Create;
      try
        (FDataLink.Field as TBlobField).SaveToStream(Stream);
        Stream.Position := 0;
        if Check and (FieldFormat=rvdbRVF) then begin
          Stream2 := TMemoryStream.Create;
          try
            SaveRVFToStream(Stream2, False);
            equal := (Stream.Size=Stream2.Size) and
              CompareMem(Stream.Memory, Stream2.Memory, Stream.Size);
          finally
            Stream2.Free;
          end;
        end;
        if not equal then begin
          Clear;
          if FAutoDeleteUnusedStyles then
            DeleteUnusedStyles(True, True, True);
          if Assigned(FOnNewDocument) then
            FOnNewDocument(Self);
          LoadXXXFromStream(Stream, Self);
          if Assigned(FOnLoadDocument) then
            FOnLoadDocument(Self);
        end;
      finally
        Stream.Free;
      end;
      if RVData.Items.Count = 0 then AddNL('',0,0);
      FMemoLoaded := True;
    except
      on E:EInvalidOperation do
        AddNL(SysUtils.Format('(%s)', [E.Message]),0,0);
    end;
    if not equal then begin
      Format;
      if sf then
        Windows.SetFocus(Handle);
      Invalidate;
    end;
    EditingChange(Self);
  end;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.BeginEditing;
begin
  if not FDataLink.Editing then
  try
    if {FDataLink.Field.IsBlob} True then begin
      if FDataSaveStream=nil then FDataSaveStream := TMemoryStream.Create;
      SaveRVFToStream(FDataSaveStream, False);
    end;
    FDataLink.Edit;
  finally
    FDataSaveStream.Free;
    FDataSaveStream := nil;
  end;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.DataChange(Sender: TObject);
var Stream: TMemoryStream;
    equal, ml: Boolean;
begin
  if FDataLink.Field <> nil then
    if {FDataLink.Field.IsBlob} True then
      if FAutoDisplay or (FDataLink.Editing and FMemoLoaded) then begin
        if (FDataSaveStream <> nil) then begin
          Stream := TMemoryStream.Create;
          try
            SaveRVFToStream(Stream, False);
            equal := (Stream.Size=FDataSaveStream.Size) and
              CompareMem(Stream.Memory, FDataSaveStream.Memory, FDataSaveStream.Size);
          finally
            Stream.Free;
          end;
          if equal then exit;
        end;
        ml := FMemoLoaded;
        FMemoLoaded := False;
        LoadField(ml);
        end
      else begin
        FMemoLoaded := False;
        Clear;
        AddNL(SysUtils.Format('(%s)', [FDataLink.Field.DisplayLabel]),0,0);
        Format;
        Invalidate;
      end
    else begin
      Clear;
      if FFocused and FDataLink.CanModify then
        AddNL(FDataLink.Field.Text,0,0)
      else
        AddNL(FDataLink.Field.DisplayText,0,0);
      if RVData.Items.Count = 0 then AddNL('',0,0);
      Format;
      Invalidate;
      FMemoLoaded := True;
    end
  else begin
    Clear;
    Format;
    Invalidate;
    FMemoLoaded := False;
  end;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.EditingChange(Sender: TObject);
begin
  inherited ReadOnly := not (FDataLink.Editing {FDataLink.CanModify} and FMemoLoaded);
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.UpdateData(Sender: TObject);
var Stream: TMemoryStream;
begin
  if {FDataLink.Field.IsBlob} True then begin
    Stream := TMemoryStream.Create;
    try
      case FieldFormat of
        rvdbRVF:
         begin
           if FAutoDeleteUnusedStyles then
             DeleteUnusedStyles(True, True, True);
           SaveRVFToStream(Stream, False);
         end;
        rvdbRTF:
         begin
           if FAutoDeleteUnusedStyles then
             DeleteUnusedStyles(True, True, True);
           SaveRTFToStream(Stream, False);
         end;
        rvdbText:
          SaveTextToStream('',Stream,80,False,True)
      end;
      Stream.Position := 0;
      (FDataLink.Field as TBlobField).LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  end;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.SetFocused(Value: Boolean);
begin
  if FFocused <> Value then
  begin
    FFocused := Value;
    if not (rvstClearing in RVData.State) and  not Assigned(FDataLink.Field) {or not FDataLink.Field.IsBlob} then
      FDataLink.Reset;
  end;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.CMEnter(var Message: TCMEnter);
begin
//  if not FMemoLoaded then LoadField;
  SetFocused(True);
  inherited;
  if {$IFDEF RICHVIEWCBDEF3}SysLocale.FarEast and{$ENDIF}
     FDataLink.CanModify then
    inherited ReadOnly := False;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.CMExit(var Message: TCMExit);
begin
  try
    FDataLink.UpdateRecord;
  except
    SetFocus;
    raise;
  end;
  SetFocused(False);
  inherited;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.SetAutoDisplay(Value: Boolean);
begin
  if FAutoDisplay <> Value then
  begin
    FAutoDisplay := Value;
    if Value then
      LoadField(FMemoLoaded);
  end;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.DblClick;
begin
  if not FMemoLoaded then
    LoadField(FMemoLoaded)
  else
    inherited;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  if FMemoLoaded and (Key=#27) and not IgnoreEscape then
    FDataLink.Reset;
  if not FMemoLoaded and (Key=#13) then begin
    LoadField(FMemoLoaded);
    Key := #0;
  end;
end;
{-----------------------------------------------------------------------}
procedure TDBRichViewEdit.CMGetDataLink(var Message: TMessage);
begin
  Message.Result := Integer(FDataLink);
end;
{-----------------------------------------------------------------------}
function TDBRichViewEdit.BeforeChange(FromOutside: Boolean): Boolean;
begin
  if FMemoLoaded then BeginEditing;
  Result := inherited BeforeChange(FromOutside);
end;
{=======================================================================}
procedure Register;
begin
  RegisterComponents('RichView', [TDBRichView, TDBRichViewEdit]);
end;

end.
