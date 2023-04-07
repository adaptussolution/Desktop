
{*******************************************************}
{                                                       }
{       RichView                                        }
{       Assistant classes for drag & drop.              }
{       TRVDropTarget: implements IDropTarget           }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}


unit RVDragDrop;

interface

{$I RV_Defs.inc}

uses SysUtils, Windows, ActiveX, Classes, Graphics,
     RVStr, RVUni, RVScroll, ShlObj;

  {$IFNDEF RVDONOTUSEDRAGDROP}

{$IFDEF BCB}
{$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDropTarget)'}
{$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IDropSource)'}
{$HPPEMIT 'DECLARE_DINTERFACE_TYPE(IEnumFORMATETC)'}
{$ENDIF}
  
type
  { ----------------------------------------------------------------------------
    TRVDropTarget: implementation of IDropTarget.
    Used as TCustomRichViewEdit.FDropTarget
  }
  TRVDropTarget = class(TRVScrollerInternalIfcObject, IDropTarget)
  private
    FAccepted: Boolean;
    function HasAcceptableFormats(const DataObj: IDataObject): Boolean;
    function AllowMoving(KeyState: LongInt; Effect: LongInt): Boolean;
    function GetEffect(KeyState: LongInt; Effect: LongInt): LongInt;
    procedure FillFormatEtc(var FmtEtc: TFormatEtc; Format: Word);
    function GetFiles(const Handle: HGlobal; Files: TStrings): Boolean;    
  public
    { IDropTarget }
    function DragEnter(const DataObj: IDataObject; KeyState: Longint;
      pt: TPoint; var Effect: Longint): HResult; stdcall;
    function DragOver(KeyState: Longint; pt: TPoint;
      var Effect: Longint): HResult; stdcall;
    function DragLeave: HResult; stdcall;
    function Drop(const DataObj: IDataObject; KeyState: Longint; pt: TPoint;
      var Effect: Longint): HResult; stdcall;
    { Get data as ... }
    function GetMedium(const DataObj: IDataObject; Format: Word;
      var StgMedium: TStgMedium): Boolean;
    function GetAsStream(const DataObj: IDataObject; Format: Word): TMemoryStream;
    function GetAsText(const DataObj: IDataObject; Format: Word;
      var s: String): Boolean;
    {$IFNDEF RVDONOTUSEUNICODE}
    function GetAsTextW(const DataObj: IDataObject; var s: String): Boolean;
    {$ENDIF}
    function GetAsBitmap(const DataObj: IDataObject; TryDIBFirst: Boolean): TBitmap;
    function GetAsMetafile(const DataObj: IDataObject): TMetafile;
    function GetAsFiles(const DataObj: IDataObject): TStringList;
    function HasFormat(const DataObj: IDataObject; Format: Word): Boolean;
    { Other methods }
    destructor Destroy; override;
    function RegisterDragDropForOwner: Boolean;
    procedure UnRegisterDragDropForOwner;
  end;
  { ----------------------------------------------------------------------------
    TRVEnumFormatEtc: implementation of IEnumFormatEtc
  }
  TRVEnumFormatEtc = class(TInterfacedObject, IEnumFormatEtc)
  private
    FGraphic: TGraphic;
    FIndex: Integer;
    function GetCurFormat: Word;
  public
    constructor Create(Graphic: TGraphic; Index: Integer);
    { IEnumFormatEtc }
    function Next(Celt: LongInt; out Elt; PCeltFetched: PLongInt): HResult; stdcall;
    function Skip(Celt: LongInt): HResult; stdcall;
    function Reset: HResult; stdcall;
    function Clone(out Enum: IEnumFormatEtc): HResult; stdcall;
  end;

  { ----------------------------------------------------------------------------
    TRVDropSource: implementation of IDropSource and IDataObject.
    Used as TCustomRichView.FDropSource.
  }
  TRVDropSource = class(TRVScrollerInternalIfcObject, IDropSource, IDataObject)
  private
    FMedium: TStgMedium;
    FUseMedium: Boolean;
    function IsAvailableFormat(Format: Word): Boolean;
    function GetAsStream(Format: Word): TMemoryStream;
    function GetAsHandle(Format: Word; var Handle: HGlobal): HResult;
    function SaveToHandle(Format: Word; Handle: HGlobal): HResult;
  public
    { IDropSource }
    function QueryContinueDrag(FEscapePressed: Bool;
      KeyState: LongInt): HResult; stdcall;
    function GiveFeedback(Effect: LongInt): HResult; stdcall;
    { IDataObject }
    function GetData(const FormatEtcIn: TFormatEtc;
      out Medium: TStgMedium): HResult; stdcall;
    function GetDataHere(const FormatEtcIn: TFormatEtc;
      out Medium: TStgMedium): HResult; stdcall;
    function QueryGetData(const FormatEtc: TFormatEtc): HResult; stdcall;
    function GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
      out FormatEtcOut: TFormatEtc): HResult; stdcall;
    function SetData(const FormatEtc: TFormatEtc; var Medium: TStgMedium;
      FRelease: Bool): HResult; stdcall;
    function EnumFormatEtc(Direction: LongInt;
      out EnumFormatEtc: IEnumFormatEtc): HResult; stdcall;
    function DAdvise(const FormatEtc: TFormatEtc; advf: LongInt;
      const advsink: IAdviseSink; out Connection: LongInt): HResult; stdcall;
    function DUnadvise(Connection: LongInt): HResult; stdcall;
    function EnumDAdvise(out EnumAdvise: IEnumStatData): HResult; stdcall;
    { Other methods }
    destructor Destroy; override;
    function StoreData(Format: Word): Boolean;
  end;
  {$ENDIF}

implementation
{$IFNDEF RVDONOTUSEDRAGDROP}
uses RichView, RVRVData, CRVData;
{=============================== TRVDropTarget ================================}
destructor TRVDropTarget.Destroy;
begin
  OwnerReleaseDropTargetObject;
  inherited;
end;
{------------------------------------------------------------------------------}
{ DROPEFFECT_MOVE is possible                                                  }
function TRVDropTarget.AllowMoving(KeyState, Effect: Integer): Boolean;
begin
  Result := ((KeyState and MK_CONTROL)=0) and ((Effect and DROPEFFECT_MOVE)<>0)
    and TCustomRichView(FOwner).RVData.IsDragging;
end;
{------------------------------------------------------------------------------}
function TRVDropTarget.GetEffect(KeyState: LongInt; Effect: LongInt): LongInt;
begin
  if AllowMoving(KeyState, Effect) then
    Result := DROPEFFECT_MOVE
  else if (Effect and DROPEFFECT_COPY)<>0 then
    Result := DROPEFFECT_COPY
  else
    Result := DROPEFFECT_LINK;
end;
{------------------------------------------------------------------------------}
{ IDropTarget.DragEnter                                                        }
function TRVDropTarget.DragEnter(const DataObj: IDataObject;
  KeyState: Integer; pt: TPoint; var Effect: Integer): HResult;
begin
  pt := FOwner.ScreenToClient(pt);
  FAccepted := HasAcceptableFormats(DataObj) and OwnerDragEnter(pt.X, pt.Y);
  if FAccepted then
    Effect := GetEffect(KeyState, Effect)
  else
    Effect := DROPEFFECT_NONE;
  Result := S_OK;
end;
{------------------------------------------------------------------------------}
{ IDropTarget.DragOver                                                         }
function TRVDropTarget.DragOver(KeyState: Integer; pt: TPoint;
  var Effect: Integer): HResult;
begin
  pt := FOwner.ScreenToClient(pt);
  if FAccepted and OwnerDragOver(pt.X, pt.Y) then
    Effect := GetEffect(KeyState, Effect)
  else
    Effect := DROPEFFECT_NONE;
  Result := S_OK;
end;
{------------------------------------------------------------------------------}
// IDropTarget.DragOver
function TRVDropTarget.DragLeave: HResult;
begin
  OwnerDragLeave;
  Result := S_OK;
end;
{------------------------------------------------------------------------------}
// IDropTarget.Drop
function TRVDropTarget.Drop(const DataObj: IDataObject;
  KeyState: Integer; pt: TPoint; var Effect: Integer): HResult;
var NewEffect: LongInt;
begin
  NewEffect := OwnerDrop(DataObj, AllowMoving(KeyState, Effect));
  if (NewEffect=DROPEFFECT_COPY) and ((Effect and DROPEFFECT_COPY)=0) then
    Effect := DROPEFFECT_LINK
  else
    Effect := NewEffect;  
  Result := S_OK;
end;
{------------------------------------------------------------------------------}
{ Returns true if DataObj contains one of: text, RVF, RTF, Bitmap...           }
function TRVDropTarget.HasAcceptableFormats(const DataObj: IDataObject): Boolean;
var  FormatEtc : IEnumFormatEtc;
     Fmt : TFormatEtc;
     s: String;
     l: Integer;
begin
  Result := False;
  if DataObj.EnumFormatEtc(DATADIR_GET, FormatEtc) = S_OK then
    while FormatEtc.Next(1, Fmt, nil) = S_OK do begin
      SetLength(s, 1000);
      l := GetClipboardFormatName(Fmt.cfFormat, PChar(s), 1000);
      SetLength(s, l);
      if OwnerCanAcceptFormat(Fmt.cfFormat) then begin
        Result := True;
        //exit;
      end;
    end;
end;
{------------------------------------------------------------------------------}
{ Returns tymed value for the data format                                      }
function GetTymed(Format: Word): LongInt;
begin
  case Format of
    CF_BITMAP:
      Result := TYMED_GDI;
    CF_ENHMETAFILE:
      Result := TYMED_ENHMF;
    CF_METAFILEPICT:
      Result := TYMED_MFPICT;
    else
      Result := TYMED_HGLOBAL;
  end;
end;
{------------------------------------------------------------------------------}
{ Fills TFormatEtc for the given Format.                                       }
procedure TRVDropTarget.FillFormatEtc(var FmtEtc: TFormatEtc; Format: Word);
begin
  FillChar(FmtEtc, sizeof(FmtEtc), 0);
  FmtEtc.cfFormat := Format;
  FmtEtc.dwAspect := DVASPECT_CONTENT;
  FmtEtc.lindex := -1;
  FmtEtc.tymed := GetTymed(Format);
end;
{------------------------------------------------------------------------------}
{ Returns true if DataObj contains Format                                      }
function TRVDropTarget.HasFormat(const DataObj: IDataObject;
  Format: Word): Boolean;
var FmtEtc: TFormatEtc;
begin
  FillFormatEtc(FmtEtc, Format);
  Result := DataObj.QueryGetData(FmtEtc)=S_OK;
end;
{------------------------------------------------------------------------------}
{ Returns StgMedium. The caller must call ReleaseStgMedium                     }
function TRVDropTarget.GetMedium(const DataObj: IDataObject; Format: Word;
  var StgMedium: TStgMedium): Boolean;
var FmtEtc: TFormatEtc;  
begin
  FillChar(StgMedium, sizeof(StgMedium), 0);
  FillFormatEtc(FmtEtc, Format);
  Result := DataObj.GetData(FmtEtc, StgMedium)=S_OK;
end;
{------------------------------------------------------------------------------}
{ Creates stream, copies data to it. Returns nil if failed.                    }
function TRVDropTarget.GetAsStream(const DataObj: IDataObject;
  Format: Word): TMemoryStream;
var FmtEtc: TFormatEtc;
  StgMedium: TStgMedium;
  ptr: Pointer;
  Size: Integer;
begin
  FillChar(StgMedium, sizeof(StgMedium), 0);
  FillFormatEtc(FmtEtc, Format);
  Result := nil;
  if DataObj.GetData(FmtEtc, StgMedium)<>S_OK then
    exit;
  if StgMedium.tymed=GetTymed(Format) then begin
    ptr := GlobalLock(StgMedium.HGlobal);
    Result := TMemoryStream.Create;
    if Format<>CFRV_RVF then
      Result.WriteBuffer(ptr^, GlobalSize(StgMedium.HGlobal))
    else begin
      if GlobalSize(StgMedium.HGlobal)<4 then
        exit;
      Size := PInteger(ptr)^;
      Result.WriteBuffer((PChar(ptr)+sizeof(Integer))^, Size);
    end;
    Result.Position := 0;
    GlobalUnlock(StgMedium.HGlobal);
  end;
  ReleaseStgMedium(StgMedium);
end;
{------------------------------------------------------------------------------}
{ Returns data as string.                                                      }
function TRVDropTarget.GetAsText(const DataObj: IDataObject;
  Format: Word; var s: String): Boolean;
var Stream: TMemoryStream;
begin
  Stream := GetAsStream(DataObj, Format);
  Result := Stream<>nil;
  if Result then begin
    s := StrPas(Stream.Memory);
    Stream.Free;
  end;
end;
{------------------------------------------------------------------------------}
{ Returns CF_UNICODETEXT data as "raw unicode" string.                         }
{$IFNDEF RVDONOTUSEUNICODE}
function TRVDropTarget.GetAsTextW(const DataObj: IDataObject;
  var s: String): Boolean;
var Stream: TMemoryStream;
   Len: Integer;
   Ptr: Pointer;
begin
  Stream := GetAsStream(DataObj, CF_UNICODETEXT);
  Result := Stream<>nil;
  if Result then begin
    Len := Stream.Size;
    SetLength(s, Len);
    Stream.ReadBuffer(PChar(s)^, Len);
    Stream.Free;
    ptr := RVU_StrScanW(Pointer(s), 0, Length(s) div 2);
    if ptr<>nil then
      SetLength(s, PChar(ptr)-PChar(s));
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
{ Returns a number of colors in TBitmapInfoHeader                              }
function GetDInColors(BitCount: Word): Integer;
begin
  case BitCount of
    1, 4, 8: Result := 1 shl BitCount;
  else
    Result := 0;
  end;
end;
{------------------------------------------------------------------------------}
{ Returns data as a bitmap. First tries to read CF_DIB. If failed, tries
  CF_BITMAP. If failed, returns nil.                                           }
function TRVDropTarget.GetAsBitmap(const DataObj: IDataObject;
  TryDIBFirst: Boolean): TBitmap;
var FmtEtc: TFormatEtc;
  StgMedium: TStgMedium;
  ColorCount: Integer;
  bits:Pointer;
  BI: PBitmapInfoHeader;
  Handle: HBitmap;
  DC: HDC;
begin
  // 1. CF_DIB
  FillFormatEtc(FmtEtc, CF_DIB);
  Result := nil;
  if TryDIBFirst then begin
    if DataObj.GetData(FmtEtc, StgMedium)<>S_OK then
      exit;
    if StgMedium.tymed=GetTymed(CF_DIB) then begin
      BI := GlobalLock(StgMedium.hGlobal);
      ColorCount := GetDInColors(BI.biBitCount);
      Bits := Pointer(Longint(BI) + SizeOf(TBitmapInfoHeader) + ColorCount * SizeOf(TRGBQuad));
      try
        DC := GetDC(0);
        try
          Handle := CreateDIBitmap(DC, BI^, CBM_INIT, Bits, PBitmapInfo(BI)^, DIB_RGB_COLORS);
          if Handle<>0 then begin
             Result := TBitmap.Create;
             Result.Handle := Handle;
          end;
        finally
          ReleaseDC(0, DC);
        end;
      finally
        GlobalUnlock(StgMedium.hGlobal);
      end;
    end;
    ReleaseStgMedium(StgMedium);
  end;
  // 2. CF_BITMAP
  if Result=nil then begin
    FillFormatEtc(FmtEtc, CF_BITMAP);
    Result := nil;
    if DataObj.GetData(FmtEtc, StgMedium)<>S_OK then
      exit;
    if StgMedium.tymed=GetTymed(CF_BITMAP) then begin
      Result := TBitmap.Create;
      // Palette?
      Result.LoadFromClipboardFormat(CF_BITMAP, StgMedium.hBitmap, 0);
    end;
    ReleaseStgMedium(StgMedium);
  end;
end;
{------------------------------------------------------------------------------}
{ Returns data as a metafile (CF_ENHMETAFILE).
  CF_METAFILEPICT is not supported yet.                                        }
function TRVDropTarget.GetAsMetafile(const DataObj: IDataObject): TMetafile;
var FmtEtc: TFormatEtc;
  StgMedium: TStgMedium;
  Handle: HEnhMetafile;
begin
  // 1. CF_ENHMETAFILE
  FillFormatEtc(FmtEtc, CF_ENHMETAFILE);
  Result := nil;
  if DataObj.GetData(FmtEtc, StgMedium)<>S_OK then
    exit;
  if StgMedium.tymed=GetTymed(CF_ENHMETAFILE) then begin
    Handle := CopyEnhMetaFile(StgMedium.hEnhMetaFile, nil);
    if Handle<>0 then begin
      Result := TMetafile.Create;
      Result.Handle := Handle;
    end;
  end;
  ReleaseStgMedium(StgMedium);
end;
{------------------------------------------------------------------------------}
{ Returns a list of files from the Handle of CF_HDROP format                   }
function TRVDropTarget.GetFiles(const Handle: HGlobal;
  Files: TStrings): Boolean;
var
  DropFiles: PDropFiles;
  FileName: PChar;
begin
  Files.Clear;
  DropFiles := PDropFiles(GlobalLock(Handle));
  try
    FileName := PChar(DropFiles) + DropFiles^.pFiles;
    while (FileName^ <> #0) do begin
      if (DropFiles^.fWide) then begin
        Files.Add(PWideChar(FileName));
        inc(FileName, (Length(PWideChar(FileName)) + 1) * 2);
      end else
      begin
        Files.Add(Filename);
        inc(FileName, Length(FileName) + 1);
      end;
    end;
  finally
    GlobalUnlock(Handle);
  end;
  Result := Files.count>0;
end;
{------------------------------------------------------------------------------}
function TRVDropTarget.GetAsFiles(const DataObj: IDataObject): TStringList;
var FmtEtc: TFormatEtc;
  StgMedium: TStgMedium;
begin
  FillFormatEtc(FmtEtc, CF_HDROP);
  Result := nil;
  if DataObj.GetData(FmtEtc, StgMedium)<>S_OK then
    exit;
  if StgMedium.tymed=GetTymed(CF_HDROP) then begin
    Result := TStringList.Create;
    GetFiles(StgMedium.hGlobal, Result);
  end;
  ReleaseStgMedium(StgMedium);
end;
{------------------------------------------------------------------------------}
{ Allows owner to be a drop targer.                                            }
function TRVDropTarget.RegisterDragDropForOwner: Boolean;
begin
  Result := FOwner.HandleAllocated and
    (RegisterDragDrop(FOwner.Handle, Self as IDropTarget)=S_OK);
end;
{------------------------------------------------------------------------------}
{ Reverse to RegisterDragDropForOwner.                                         }
procedure TRVDropTarget.UnRegisterDragDropForOwner;
begin
  if FOwner.HandleAllocated then
    RevokeDragDrop(FOwner.Handle);
end;
{============================ TRVEnumFormatEtc ================================}
function GetTymeds(Format: Word): LongInt;
begin
  case Format of
    CF_BITMAP:
      Result := TYMED_GDI;
    CF_ENHMETAFILE:
      Result := TYMED_ENHMF;
    CF_METAFILEPICT:
      Result := TYMED_MFPICT;
    {$IFNDEF RVDONOTUSEUNICODE}
    CF_UNICODETEXT:
      Result := TYMED_HGLOBAL {or TYMED_ISTREAM};    
    {$ENDIF}
    CF_TEXT:
      Result := TYMED_HGLOBAL {or TYMED_ISTREAM};
    else
      {$IFNDEF RVDONOTUSERVF}
      if (Format=CFRV_RVF) then
        Result := TYMED_HGLOBAL
      else
      {$ENDIF}
      {$IFNDEF RVDONOTUSERTF}
         if (Format=CFRV_RTF) then
           Result := TYMED_HGLOBAL {or TYMED_ISTREAM}

      else
      {$ENDIF}      
        Result := 0;
  end;
end;
{------------------------------------------------------------------------------}
constructor TRVEnumFormatEtc.Create(Graphic: TGraphic; Index: Integer);
begin
  inherited Create;
  FGraphic := Graphic;
  FIndex := Index;
end;
{------------------------------------------------------------------------------}
function TRVEnumFormatEtc.GetCurFormat: Word;
begin
  case FIndex of
    0:
    {$IFNDEF RVDONOTUSERVF}
      Result := CFRV_RVF;
    {$ELSE}
      Result := CF_TEXT;
    {$ENDIF}
    1:
    {$IFNDEF RVDONOTUSERTF}
      Result := CFRV_RTF;
    {$ELSE}
      Result := CF_TEXT;
    {$ENDIF}
    2:
      Result := CF_TEXT;
    3:
   {$IFNDEF RVDONOTUSEUNICODE}
      Result := CF_UNICODETEXT;
    4:
    {$ENDIF}
      begin
        if FGraphic=nil then
          Result := 0
        else if FGraphic is TBitmap then
          Result := CF_BITMAP
        else if FGraphic is TMetafile then
          Result := CF_ENHMETAFILE
        else
          Result := 0;
      end
    else
      Result := 0;
  end;
end;
{------------------------------------------------------------------------------}
type
  TFormatList = array[0..255] of TFormatEtc;
  PFormatList = ^TFormatList;
{ IEnumFormatEtc.Next                                                          }  
function TRVEnumFormatEtc.Next(Celt: Integer; out Elt;
  PCeltFetched: PLongInt): HResult;
var
  i: Integer;
  Format: Word;
  List: TFormatList absolute Elt;
begin
  i := 0;
  while (i<Celt) do begin
    Format := GetCurFormat;
    if Format=0 then
      break;
    List[i].cfFormat := Format;
    List[i].ptd := nil;
    List[i].dwAspect := DVASPECT_CONTENT;
    List[i].lindex := -1;
    List[i].tymed := GetTymeds(Format);
    Inc(FIndex);
    Inc(i);
  end;
  if PCeltFetched<>nil then
    PCeltFetched^ := i;
  if i = Celt then
    Result := S_OK
  else
    Result := S_FALSE;
end;
{------------------------------------------------------------------------------}
{ IEnumFormatEtc.Skip                                                          }
function TRVEnumFormatEtc.Skip(Celt: Integer): HResult;
var
  i: Integer;
  Format: Word;
begin
  i := 0;
  while (i<Celt) do begin
    Format := GetCurFormat;
    if Format=0 then
      break;
    Inc(FIndex);
    Inc(i);
  end;
  if i = Celt then
    Result := S_OK
  else
    Result := S_FALSE;
end;
{------------------------------------------------------------------------------}
{ IEnumFormatEtc.Reset                                                         }
function TRVEnumFormatEtc.Reset: HResult;
begin
  FIndex := 0;
  Result := S_OK;
end;
{------------------------------------------------------------------------------}
{ IEnumFormatEtc.Clone                                                         }
function TRVEnumFormatEtc.Clone(out Enum: IEnumFormatEtc): HResult;
begin
  Enum := TRVEnumFormatEtc.Create(FGraphic, FIndex);
  Result := S_OK;
end;
{============================== TRVDropSource =================================}
destructor TRVDropSource.Destroy;
begin
  TRichViewRVData(TCustomRichView(FOwner).RVData).FDropSource := nil;
  if FUseMedium then
    ReleaseStgMedium(FMedium);
  inherited;
end;
{------------------------------------------------------------------------------}
function TRVDropSource.IsAvailableFormat(Format: Word): Boolean;
var Graphic: TGraphic;
begin
  Result :=
    (Format=CF_TEXT)
    {$IFNDEF RVDONOTUSEUNICODE}
    or (Format=CF_UNICODETEXT)
    {$ENDIF}
    {$IFNDEF RVDONOTUSERVF}
    or (Format=CFRV_RVF)
    {$ENDIF}
    {$IFNDEF RVDONOTUSERTF}
    or (Format=CFRV_RTF)
    {$ENDIF};
  if not Result then begin
    Graphic := TCustomRichView(FOwner).GetSelectedImage;
    if Graphic is TBitmap then
      Result := Format=CF_BITMAP
    else if Graphic is TMetafile then
      Result := Format=CF_ENHMETAFILE
  end;
end;
{------------------------------------------------------------------------------}
function TRVDropSource.GetAsStream(Format: Word): TMemoryStream;
var Size: Integer;
begin
   Result := TMemoryStream.Create;
   case Format of
     CF_TEXT:
       begin
         TCustomRichView(FOwner).SaveTextToStream('', Result, 80, True, True);
         Size := 0;
         Result.WriteBuffer(Size, 1);
       end;
     {$IFNDEF RVDONOTUSEUNICODE}
     CF_UNICODETEXT:
       begin
         TCustomRichView(FOwner).SaveTextToStreamW('', Result, 80, True, True);
         Size := 0;
         Result.WriteBuffer(Size, 2);
       end;
     {$ENDIF}
     else
       begin
         {$IFNDEF RVDONOTUSERVF}
         if Format=CFRV_RVF then begin
           Size := 0;
           Result.WriteBuffer(Size, sizeof(Size));
           TCustomRichView(FOwner).SaveRVFToStream(Result, True);
           Size := Result.Size-sizeof(Size);
           Result.Position := 0;
           Result.WriteBuffer(Size, sizeof(Size));
           end
         else
         {$ENDIF}
         {$IFNDEF RVDONOTUSERTF}
         if Format=CFRV_RTF then
           TCustomRichView(FOwner).SaveRTFToStream(Result, True)
         else
         {$ENDIF}
         begin
           Result.Free;
           Result := nil;
         end;
       end;
   end;
   if Result<>nil then
     Result.Position := 0;
end;
{------------------------------------------------------------------------------}
function TRVDropSource.GetAsHandle(Format: Word; var Handle: HGlobal): HResult;
var Stream: TMemoryStream;
    ptr: Pointer;
begin
  Handle := 0;
  Stream := GetAsStream(Format);
  if Stream=nil then begin
    Result := DV_E_FORMATETC;
    exit;
  end;
  Handle := GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT, Stream.Size);
  if Handle=0 then begin
    Result := E_OUTOFMEMORY;
    Stream.Free;
    exit;
  end;
  ptr := GlobalLock(Handle);
  Stream.ReadBuffer(ptr^, Stream.Size);
  GlobalUnlock(Handle);
  Stream.Free;
  Result := S_OK;
end;
{------------------------------------------------------------------------------}
function TRVDropSource.SaveToHandle(Format: Word; Handle: HGlobal): HResult;
var Stream: TMemoryStream;
    ptr: Pointer;
begin
  Stream := GetAsStream(Format);
  if Stream=nil then begin
    Result := DV_E_FORMATETC;
    exit;
  end;
  if GlobalSize(Handle)<DWORD(Stream.Size) then begin
    Result := STG_E_MEDIUMFULL;
    Stream.Free;
    exit;
  end;
  ptr := GlobalLock(Handle);
  Stream.ReadBuffer(ptr^, Stream.Size);
  GlobalUnlock(Handle);
  Stream.Free;
  Result := S_OK;
end;
{------------------------------------------------------------------------------}
{
function TRVDropSource.SaveToStream(Format: Word; const Stream: IStream): HResult;
var LStream: TMemoryStream;
    cb: LongInt;
begin
  LStream := GetAsStream(Format);
  if LStream=nil then begin
    Result := DV_E_FORMATETC;
    exit;
  end;
  Result := Stream.Write(LStream.Memory, LStream.Size, @cb);
  LStream.Free;
end;
}
{------------------------------------------------------------------------------}
{ IDropSource.GiveFeedback                                                     }
function TRVDropSource.GiveFeedback(Effect: Integer): HResult;
begin
  Result := DRAGDROP_S_USEDEFAULTCURSORS;
end;
{------------------------------------------------------------------------------}
{ IDropSource.QueryContinueDrag                                                }
function TRVDropSource.QueryContinueDrag(FEscapePressed: Bool;
  KeyState: Integer): HResult;
begin
  if (MK_LBUTTON and KeyState)<>0 then
    TCustomRichView(FOwner).RVData.State :=
      TCustomRichView(FOwner).RVData.State-[rvstDragDropCursorNotMoved];
  if FEscapePressed then
    Result := DRAGDROP_S_CANCEL
  else if (MK_LBUTTON and KeyState)=0 then
    Result := DRAGDROP_S_DROP
  else
    Result := S_OK;
end;
{------------------------------------------------------------------------------}
{ IDataObject.EnumFormatEtc                                                    }
function TRVDropSource.EnumFormatEtc(Direction: Integer;
  out EnumFormatEtc: IEnumFormatEtc): HResult;
begin
  case Direction of
    DATADIR_GET:
      begin
        EnumFormatEtc := TRVEnumFormatEtc.Create(
          TCustomRichView(FOwner).GetSelectedImage, 0) as IEnumFormatEtc;
        Result := S_OK;
      end;
    DATADIR_SET:
      Result := E_NOTIMPL;
    else
      Result := E_INVALIDARG;
  end;
end;
{------------------------------------------------------------------------------}
{ Stores data as Format in FMedium. This medium will be used in the next call
  of GetData. This method is used when dropping data to itself.                }
function TRVDropSource.StoreData(Format: Word): Boolean;
var FmtEtc: TFormatEtc;
begin
  FillChar(FmtEtc, sizeof(FmtEtc), 0);
  FmtEtc.cfFormat := Format;
  FmtEtc.dwAspect := DVASPECT_CONTENT;
  FmtEtc.lindex := -1;
  FmtEtc.tymed := GetTymed(Format);
  FUseMedium := GetData(FmtEtc, FMedium)=S_OK;
  Result := FUseMedium;
end;
{------------------------------------------------------------------------------}
{ IDataObject.GetData                                                          }
function TRVDropSource.GetData(const FormatEtcIn: TFormatEtc;
  out Medium: TStgMedium): HResult;
var {StreamAdapter : TStreamAdapter;
    Stream        : TStream;}
    Format: Word;
    Palette: HPalette;
    Handle: THandle;
begin
  Medium.tymed := 0;
  Medium.UnkForRelease := nil;
  Medium.hGlobal := 0;
  Medium.hBitmap := 0;
  Medium.hMetaFilePict := 0;
  Medium.hEnhMetaFile := 0;
  Medium.stm := nil;
  if FUseMedium then begin
    Medium := FMedium;
    FUseMedium := False;
    Result := S_OK;
    exit;
  end;
  if IsAvailableFormat(FormatEtcIn.cfFormat) then
    if FormatEtcIn.dwAspect = DVASPECT_CONTENT then
      if (FormatEtcIn.tymed and GetTymeds(FormatEtcIn.cfFormat))<>0 then begin
        if (FormatEtcIn.cfFormat=CF_TEXT)
           {$IFNDEF RVDONOTUSEUNICODE}
           or (FormatEtcIn.cfFormat=CF_UNICODETEXT)
           {$ENDIF}
           {$IFNDEF RVDONOTUSERVF}
           or (FormatEtcIn.cfFormat=CFRV_RVF)
           {$ENDIF}
           {$IFNDEF RVDONOTUSERTF}
           or (FormatEtcIn.cfFormat=CFRV_RTF)
           {$ENDIF} then begin
          if ((FormatEtcIn.tymed and TYMED_HGLOBAL)=TYMED_HGLOBAL) then begin
            Result := GetAsHandle(FormatEtcIn.cfFormat, Medium.HGlobal);
            Medium.tymed := TYMED_HGLOBAL;
            end
          {else if (FormatEtcIn.tymed and TYMED_ISTREAM)=TYMED_ISTREAM then begin
            Stream := GetAsStream(FormatEtcIn.cfFormat);
            StreamAdapter := TStreamAdapter.Create(Stream, soOwned);
            Medium.stm := Pointer(StreamAdapter as IStream);
            Medium.tymed := TYMED_ISTREAM;
            Medium.unkForRelease := Pointer(StreamAdapter as IUnknown);
            Result := S_OK;
            end}
          else
            Result := DV_E_TYMED;
          end
        else begin
          case FormatEtcIn.cfFormat of
            CF_BITMAP:
              begin
                if TRichView(FOwner).GetSelectedImage is TBitmap then begin
                  Format := 0;
                  Handle := 0;
                  TRichView(FOwner).GetSelectedImage.SaveToClipboardFormat(
                    Format, Handle, Palette);
                  if Format=CF_BITMAP then begin
                    Medium.hBitmap := Handle;
                    if (Palette <> 0) and (Palette <> SystemPalette16) then
                      DeleteObject(Palette);
                    Medium.tymed := TYMED_GDI;
                    Result := S_OK;
                    end
                  else
                    Result := DV_E_FORMATETC;
                  end
                else
                  Result := DV_E_FORMATETC;
              end;
            CF_ENHMETAFILE:
              begin
                if TRichView(FOwner).GetSelectedImage is TMetafile then begin
                  Format := 0;
                  Handle := 0;
                  TRichView(FOwner).GetSelectedImage.SaveToClipboardFormat(
                    Format, Handle, Palette);
                  if Format=CF_ENHMETAFILE then begin
                    Medium.hEnhMetaFile := Handle;
                    Medium.tymed := TYMED_ENHMF;
                    Result := S_OK;
                    end
                  else
                    Result := DV_E_FORMATETC;
                  end
                else
                  Result := DV_E_FORMATETC;
              end;
            else
              Result := DV_E_FORMATETC;
          end;
        end
        end
      else
        Result := DV_E_TYMED
    else
      Result := DV_E_DVASPECT
  else
    Result := DV_E_FORMATETC;
end;
{------------------------------------------------------------------------------}
{ IDataObject.GetDataHere                                                      }
function TRVDropSource.GetDataHere(const FormatEtcIn: TFormatEtc;
  out Medium: TStgMedium): HResult;
begin
  if IsAvailableFormat(FormatEtcIn.cfFormat) then
    if FormatEtcIn.dwAspect = DVASPECT_CONTENT then
      if (FormatEtcIn.tymed and GetTymeds(FormatEtcIn.cfFormat))<>0 then begin
        if (FormatEtcIn.cfFormat=CF_TEXT)
           {$IFNDEF RVDONOTUSERVF}
           or (FormatEtcIn.cfFormat=CFRV_RVF)
           {$ENDIF}
           {$IFNDEF RVDONOTUSERTF}
           or (FormatEtcIn.cfFormat=CFRV_RTF)
           {$ENDIF} then begin
          if (FormatEtcIn.tymed and TYMED_HGLOBAL)=TYMED_HGLOBAL then
            Result := SaveToHandle(FormatEtcIn.cfFormat, Medium.HGlobal)
          {else if (FormatEtcIn.tymed and TYMED_ISTREAM)=TYMED_ISTREAM then
            Result := SaveToStream(FormatEtcIn.cfFormat, IStream(Medium.stm))}
          else
            Result := DV_E_TYMED;
          end
        else begin
          Result := DV_E_FORMATETC;
        end
        end
      else
        Result := DV_E_TYMED
    else
      Result := DV_E_DVASPECT
  else
    Result := DV_E_FORMATETC;
end;
{------------------------------------------------------------------------------}
{ IDataObject.QueryGetData                                                     }
function TRVDropSource.QueryGetData(const FormatEtc: TFormatEtc): HResult;
begin
  if FUseMedium then begin
    Result := S_OK;
    exit;
  end;
  if IsAvailableFormat(FormatEtc.cfFormat) then
    if FormatEtc.dwAspect = DVASPECT_CONTENT then
      if (FormatEtc.tymed and GetTymeds(FormatEtc.cfFormat))<>0 then
        Result := S_OK
      else
        Result := DV_E_TYMED
    else
      Result := DV_E_DVASPECT
  else
    Result := DV_E_FORMATETC;
end;
{------------------------------------------------------------------------------}
{ IDataObject.GetCanonicalFormatEtc                                            }
function TRVDropSource.GetCanonicalFormatEtc(const FormatEtc: TFormatEtc;
  out FormatEtcOut: TFormatEtc): HResult;
begin
  FormatEtcOut.ptd := nil;
  Result := E_NOTIMPL;
end;
{------------------------------------------------------------------------------}
{ IDataObject.SetData                                                          }
function TRVDropSource.SetData(const FormatEtc: TFormatEtc;
  var Medium: TStgMedium; FRelease: Bool): HResult;
begin
  Result := E_NOTIMPL;
end;
{------------------------------------------------------------------------------}
{ IDataObject.dAdvise                                                          }
function TRVDropSource.DAdvise(const FormatEtc: TFormatEtc; advf: Integer;
  const advsink: IAdviseSink; out Connection: Integer): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;
{------------------------------------------------------------------------------}
{ IDataObject.dUnadvise                                                        }
function TRVDropSource.DUnadvise(Connection: Integer): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;
{------------------------------------------------------------------------------}
{ IDataObject.EnumdAdvise                                                      }
function TRVDropSource.EnumDAdvise(out EnumAdvise: IEnumStatData): HResult;
begin
  Result := OLE_E_ADVISENOTSUPPORTED;
end;
{$ENDIF}

end.
