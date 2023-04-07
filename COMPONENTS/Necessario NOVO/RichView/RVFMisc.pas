
{*******************************************************}
{                                                       }
{       RichView                                        }
{       Miscellaneous procedures related to             }
{       RichView Format (RVF).                          }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVFMisc;

interface
{$I RV_Defs.inc}
uses Classes, SysUtils, Graphics, Controls,
     RVStyle, RVUni;

procedure RVFWrite(Stream: TStream; const s: String);
procedure RVFWriteLine(Stream: TStream; s: String);
procedure RVFWriteLineX(Stream: TStream; const s: String;
  Unicode, HexUnicode: Boolean);
function RVFStream2TextString(Stream: TMemoryStream): String;
function RVFTextString2Stream(const str: String; Stream: TMemoryStream): Boolean;

function RVEncodeString(const str: String): String;
function RVDecodeString(const str: String): String;

{$IFDEF RICHVIEWCBDEF3}
function RVEncodeWideString(const str: WideString): String;
function RVDecodeWideString(const str: String): WideString;
{$ENDIF}

function RVFLoadPicture(const s: String; gr: TGraphic): Boolean;
function RVFSavePicture(gr: TGraphic): String;
procedure RVFLoadPictureBinary(const Data: String; gr: TGraphic);
procedure RVFLoadPictureBinary2(AStream: TStream; gr: TGraphic);
procedure RVFSavePictureBinary(Stream: TStream; gr: TGraphic);
function RVFLoadControl(const s: String; var ctrl: TComponent;
                        const ClassName: String;
                        ParentControl: TWinControl): Boolean;
function RVFSaveControl(ctrl: TComponent): String;
procedure RVFLoadControlBinary(const Data: String; var ctrl: TComponent;
                               const ClassName: String;
                               ParentControl: TWinControl);
procedure RVFSaveControlBinary(Stream: TStream; ctrl: TComponent);

function RVFReadString(var P: PChar; var s: String): Boolean;
function RVFReadInteger(var P: PChar; var V: Integer): Boolean;
{$IFDEF RICHVIEWCBDEF3}
function RVFReadText(var P: PChar): String;
{$ENDIF}
function RVFReadTag(var P: PChar; TagsArePChars, Quoted: Boolean;
  var Tag: Integer): Boolean;
function RVFReadParaStyle(RVStyle: TRVStyle; var P: PChar; var V: Integer): Boolean;
function RVFReadTextStyle(RVStyle: TRVStyle; var P: PChar; var V: Integer): Boolean;

function RVFSaveTag(TagsArePChars:Boolean; Tag: Integer): String;
function RVFSaveText(RVStyle:TRVStyle; UseStyleNames: Boolean; TextIdx: Integer): String;
function RVFSavePara(RVStyle:TRVStyle; UseStyleNames: Boolean; TextIdx: Integer): String;
function RVFItemSavePara(ParaNo: Integer; RVData: TPersistent;
                         ForceSameAsPrev: Boolean): String;
function RVFEncodeLineBreaks(const s: String): String;
function RVFDecodeLineBreaks(const s: String): String;

implementation
uses RVStr, RVFuncs, CRVData;
const
  crlf = #13#10;
{-----------------------------------------------------------------------}
procedure RVFWrite(Stream: TStream; const s: String);
begin
  Stream.WriteBuffer(PChar(s)^, Length(s));
end;
{-----------------------------------------------------------------------}
procedure RVFWriteLine(Stream: TStream; s: String);
begin
  s := s+crlf;
  Stream.WriteBuffer(PChar(s)^, Length(s));
end;
{-----------------------------------------------------------------------}
procedure RVFWriteLineX(Stream: TStream; const s: String;
  Unicode, HexUnicode: Boolean);
var sep: String;

begin
  {$IFDEF RICHVIEWCBDEF3}
  if Unicode and HexUnicode then begin
    sep := RVEncodeString(s);
    Stream.WriteBuffer(PChar(sep)^, Length(sep));
    end
  else
  {$ENDIF}
    Stream.WriteBuffer(PChar(s)^, Length(s));
  if Unicode and not HexUnicode then
    sep := String(Chr(Lo(UNI_ParagraphSeparator)))+Chr(Hi(UNI_ParagraphSeparator))
  else
    sep := crlf;
  Stream.WriteBuffer(PChar(sep)^, Length(sep));
end;
{-----------------------------------------------------------------------}
function RVFStream2TextString(Stream: TMemoryStream): String;
var i: Integer;
    hex: String;
begin
 SetLength(Result, Stream.Size*2);
 for i := 0 to Stream.Size-1 do begin
   hex := IntToHex(Ord(PChar(Stream.Memory)[i]),2);
   Result[i*2+1] := hex[1];
   Result[i*2+2] := hex[2];
 end;
end;
{-----------------------------------------------------------------------}
function RVEncodeString(const str: String): String;
var i: Integer;
    hex: String;
begin
 SetLength(Result, Length(str)*2);
 for i := 0 to Length(str)-1 do begin
   hex := IntToHex(Ord(str[i+1]),2);
   Result[i*2+1] := hex[1];
   Result[i*2+2] := hex[2];
 end;
end;
{-----------------------------------------------------------------------}
function RVFTextString2Stream(const str: String; Stream: TMemoryStream): Boolean;
var i,d1,d2, idx1, idx2: Integer;
    s: String;
begin
 Result := False;
 if (Length(str) mod 2)<>0 then exit;
 Stream.SetSize(Length(str) div 2);
 s := UpperCase(str);
 for i := 0 to (Length(s) div 2)-1 do begin
   idx1 := i*2+1;
   idx2 := i*2+2;
   if not (s[idx1] in ['0'..'9','A'..'F']) or
     not (s[idx2] in ['0'..'9','A'..'F']) then exit;
   if s[idx1] in ['0'..'9'] then
     d1 := Ord(s[idx1])-Ord('0')
   else
     d1 := Ord(s[idx1])-Ord('A')+10;
   if s[idx2] in ['0'..'9'] then
     d2 := Ord(s[idx2])-Ord('0')
   else
     d2 := Ord(s[idx2])-Ord('A')+10;
   PChar(Stream.Memory)[i] := Chr(d1*16+d2);
 end;
 Result := True;
end;
{-----------------------------------------------------------------------}
function RVDecodeString(const str: String): String;
var i,d1,d2, idx1, idx2: Integer;
    s: String;
begin
 Result := '';
 if (Length(str) mod 2)<>0 then exit;
 SetLength(Result, Length(str) div 2);
 s := UpperCase(str);
 for i := 0 to (Length(s) div 2)-1 do begin
   idx1 := i*2+1;
   idx2 := i*2+2;
   if not (s[idx1] in ['0'..'9','A'..'F']) or
     not (s[idx2] in ['0'..'9','A'..'F']) then exit;
   if s[idx1] in ['0'..'9'] then
     d1 := Ord(s[idx1])-Ord('0')
   else
     d1 := Ord(s[idx1])-Ord('A')+10;
   if s[idx2] in ['0'..'9'] then
     d2 := Ord(s[idx2])-Ord('0')
   else
     d2 := Ord(s[idx2])-Ord('A')+10;
   Result[i+1] := Chr(d1*16+d2);
 end;
end;
{-----------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
function RVEncodeWideString(const str: WideString): String;
var i: Integer;
    hex: String;
begin
 SetLength(Result, Length(str)*4);
 for i := 0 to Length(str)-1 do begin
   hex := IntToHex(Word(str[i+1]),4);
   Result[i*4+1] := hex[1];
   Result[i*4+2] := hex[2];
   Result[i*4+3] := hex[3];
   Result[i*4+4] := hex[4];
 end;
end;
{-----------------------------------------------------------------------}
function RVDecodeWideString(const str: String): WideString;
var i,d1,d2,d3,d4, idx1, idx2, idx3, idx4: Integer;
    s: String;
begin
 Result := '';
 if (Length(str) mod 4)<>0 then exit;
 SetLength(Result, Length(str) div 4);
 s := UpperCase(str);
 for i := 0 to (Length(s) div 4)-1 do begin
   idx1 := i*4+1;
   idx2 := idx1+1;
   idx3 := idx2+1;
   idx4 := idx3+1;
   if not (s[idx1] in ['0'..'9','A'..'F']) or
     not (s[idx2] in ['0'..'9','A'..'F']) or
     not (s[idx3] in ['0'..'9','A'..'F']) or
     not (s[idx4] in ['0'..'9','A'..'F']) then exit;
   if s[idx1] in ['0'..'9'] then
     d1 := Ord(s[idx1])-Ord('0')
   else
     d1 := Ord(s[idx1])-Ord('A')+10;
   if s[idx2] in ['0'..'9'] then
     d2 := Ord(s[idx2])-Ord('0')
   else
     d2 := Ord(s[idx2])-Ord('A')+10;
   if s[idx3] in ['0'..'9'] then
     d3 := Ord(s[idx3])-Ord('0')
   else
     d3 := Ord(s[idx3])-Ord('A')+10;
   if s[idx4] in ['0'..'9'] then
     d4 := Ord(s[idx4])-Ord('0')
   else
     d4 := Ord(s[idx4])-Ord('A')+10;
   Result[i+1] := WideChar(((d1*16+d2)*16+d3)*16+d4);
 end;
end;
{$ENDIF}
{-----------------------------------------------------------------------}
function RVFLoadPicture(const s: String; gr: TGraphic): Boolean;
var Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Result := RVFTextString2Stream(s,Stream);
    Stream.Position := 0;
    gr.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;
{-----------------------------------------------------------------------}
function RVFSavePicture(gr: TGraphic): String;
var Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    gr.SaveToStream(Stream);
    Result := RVFStream2TextString(Stream);
  finally
    Stream.Free;
  end;
end;
{-----------------------------------------------------------------------}
procedure RVFLoadPictureBinary(const Data: String; gr: TGraphic);
var Stream: TMemoryStream;
begin
  Stream  := TMemoryStream.Create;
  try
    Stream.SetSize(Length(Data));
    Move(PChar(Data)^, Stream.Memory^, Length(Data));
    Stream.Position := 0;
    gr.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;
{-----------------------------------------------------------------------}
procedure RVFLoadPictureBinary2(AStream: TStream; gr: TGraphic);
var Stream: TMemoryStream;
    v: Integer;
begin
  Stream  := TMemoryStream.Create;
  try
    AStream.ReadBuffer(v, sizeof(v));
    Stream.SetSize(v);
    AStream.ReadBuffer(Stream.Memory^, v);
    Stream.Position := 0;
    gr.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;
{-----------------------------------------------------------------------}
procedure RVFSavePictureBinary(Stream: TStream; gr: TGraphic);
var p, newp: Integer;
begin
  // writes size of picture body, then picture body
  p := Stream.Position;
  Stream.WriteBuffer(p, SizeOf(p));
  gr.SaveToStream(Stream);
  newp := Stream.Position;
  Stream.Position := p;
  p := newp - p - SizeOf(p);
  Stream.WriteBuffer(p, SizeOf(p));
  Stream.Position := newp;
end;
{-----------------------------------------------------------------------}
function RVFLoadControl(const s: String; var ctrl: TComponent;
                        const ClassName: String;
                        ParentControl: TWinControl): Boolean;
var Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Result := RVFTextString2Stream(s,Stream);
    if ClassName<>'' then
      ctrl := TComponentClass(GetClass(ClassName)).Create(nil);
    if (ctrl<>nil) and (ctrl is TControl) then
      TControl(ctrl).Parent := ParentControl;
    Stream.Position := 0;
    try
      ctrl := Stream.ReadComponent(ctrl);
    except
      ctrl := nil;
    end;
    if (ParentControl<>nil) and (ctrl<>nil) and (ctrl is TControl) then
      TControl(ctrl).Parent := ParentControl;    
  finally
    Stream.Free;
  end;
end;
{-----------------------------------------------------------------------}
function RVFSaveControl(ctrl: TComponent): String;
var Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.WriteComponent(ctrl);
    Result := RVFStream2TextString(Stream);
  finally
    Stream.Free;
  end;
end;
{-----------------------------------------------------------------------}
procedure RVFLoadControlBinary(const Data: String; var ctrl: TComponent;
                               const ClassName: String;
                               ParentControl: TWinControl);
var Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    Stream.SetSize(Length(Data));
    Move(PChar(Data)^, Stream.Memory^, Length(Data));
    if ClassName<>'' then
      ctrl := TComponentClass(GetClass(ClassName)).Create(nil);
    if (ctrl<>nil) and (ctrl is TControl) then
      TControl(ctrl).Parent := ParentControl;
    Stream.Position := 0;
    try
      ctrl := Stream.ReadComponent(ctrl);
    except
      ctrl := nil;
    end;
    if (ParentControl<>nil) and (ctrl<>nil) and (ctrl is TControl) then
      TControl(ctrl).Parent := ParentControl;
  finally
    Stream.Free;
  end;
end;
{-----------------------------------------------------------------------}
procedure RVFSaveControlBinary(Stream: TStream; ctrl: TComponent);
var p, newp: Integer;
begin
  p := Stream.Position;
  Stream.WriteBuffer(p, SizeOf(p));
  Stream.WriteComponent(ctrl);
  newp := Stream.Position;
  Stream.Position := p;
  p := newp - p - SizeOf(p);
  Stream.WriteBuffer(p, SizeOf(p));
  Stream.Position := newp;
end;
{-----------------------------------------------------------------------}
function RVFReadString(var P: PChar; var s: String): Boolean;
begin
  s := '';
  while not (P[0] in [#0,' ']) do begin
    s := s + P[0];
    inc(P);
  end;
  if P[0]=' ' then inc(P);
  Result := s<>'';
end;
{-----------------------------------------------------------------------}
function RVFReadInteger(var P: PChar; var V: Integer): Boolean;
var minus: ByteBool;
begin
  if not (P[0] in ['-','0'..'9']) then begin
    Result := False;
    exit;
  end;
  V:=0;
  minus := (P[0]='-');
  if minus then inc(P);
  while not (P[0] in [#0,' ']) do
    if P[0] in ['0'..'9'] then begin
      V := V*10+(Ord(P[0])-Ord('0'));
      inc(P);
      end
    else begin
      Result := False;
      exit;
    end;
    if P[0]=' ' then inc(P);
    if minus then V := -V;
    Result := True;
end;
{--------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
function RVFReadText(var P: PChar): String;
begin
  Result := AnsiExtractQuotedStr(P, '"');
  if (P^ = ' ') then inc(P);
end;
{$ENDIF}
{--------------------------------------------------------------------}
function RVFReadTag(var P: PChar; TagsArePChars, Quoted: Boolean;
  var Tag: Integer): Boolean;
var s: String;
begin
  {$IFDEF RICHVIEWCBDEF3}
  Quoted := Quoted and (P^ = '"');
  if Quoted then begin
    s := AnsiExtractQuotedStr(P, '"');
    if (P^ = ' ') then
      inc(P);
    end
  else
  {$ENDIF}
  if not RVFReadString(P,s) then begin
    Result := False;
    exit;
  end;
  Result := True;
  if TagsArePChars then
    if (s=RVFTagEmptyStr) and not Quoted then
      Tag := 0
    else
      Tag := Integer(StrNew(PChar(s)))
  else
    try
      Tag := StrToInt(s);
    except
      Result := False;
    end;
end;
{------------------------------------------------------------------------------}
function RVFReadParaStyle(RVStyle: TRVStyle; var P: PChar; var V: Integer): Boolean;
   {$IFDEF RICHVIEWCBDEF3}
    function ParaNameToIndex(const aParaName: String): Integer;
    begin
      with RVStyle.ParaStyles do
        for Result := 0 to Count-1 do
          if Items[result].StyleName = aParaName then Exit;
      Result := 0;
    end;
    {$ENDIF}
begin
  Result := RVFReadInteger(P, V);
  {$IFDEF RICHVIEWCBDEF3}
  if not Result then begin
    V := ParaNameToIndex(RVFReadText(P));
    Result := True;
  end;
  {$ENDIF}
end;
{-----------------------------------------------------------------------}
function RVFReadTextStyle(RVStyle: TRVStyle; var P: PChar; var V: Integer): Boolean;
    {$IFDEF RICHVIEWCBDEF3}
    function TextNameToIndex(const aParaName: String): Integer;
    begin
      with RVStyle.TextStyles do
        for Result := 0 to Count-1 do
          if Items[result].StyleName = aParaName then Exit;
      Result := 0;
    end;
    {$ENDIF}
begin
  Result := RVFReadInteger(P, V);
  {$IFDEF RICHVIEWCBDEF3}
  if not Result then begin
    V := TextNameToIndex(RVFReadText(P));
    Result := True;
  end;
  {$ENDIF}
end;
{-----------------------------------------------------------------------}
function RVFSaveTag(TagsArePChars:Boolean; Tag: Integer): String;
begin
  if TagsArePChars then
    if (Tag=0) or (PChar(Tag)[0]=#0) then
      Result := RVFTagEmptyStr
    else
     {$IFDEF RICHVIEWCBDEF3}
      Result := AnsiQuotedStr(PChar(Tag), '"')
     {$ELSE}
     Result := PChar(Tag)
     {$ENDIF}
  else
    Result := IntToStr(Tag)
end;
{-----------------------------------------------------------------------}
function RVFSaveText(RVStyle:TRVStyle; UseStyleNames: Boolean; TextIdx: Integer): String;
begin
  {$IFDEF RICHVIEWCBDEF3}
  with RVStyle.TextStyles do
    if (TextIdx>=0) and UseStyleNames then
      Result := AnsiQuotedStr(Items[TextIdx].StyleName, '"')
    else
  {$ENDIF}
      Result := IntToStr(TextIdx);
end;
{-----------------------------------------------------------------------}
function RVFSavePara(RVStyle:TRVStyle; UseStyleNames: Boolean; TextIdx: Integer): String;
begin
  {$IFDEF RICHVIEWCBDEF3}
  with RVStyle.ParaStyles do
    if (TextIdx>=0) and UseStyleNames then
      Result := AnsiQuotedStr(Items[TextIdx].StyleName, '"')
    else
  {$ENDIF}
      Result := IntToStr(TextIdx);
end;
{-----------------------------------------------------------------------}
function RVFItemSavePara(ParaNo: Integer; RVData: TPersistent;
                         ForceSameAsPrev: Boolean): String;
begin
  if ForceSameAsPrev then
    ParaNo := -1;
  Result := RVFSavePara(TCustomRVData(RVData).GetRVStyle,
                        rvfoUseStyleNames in TCustomRVData(RVData).RVFOptions,
                        ParaNo);
end;
{-----------------------------------------------------------------------}
function RVFEncodeLineBreaks(const s: String): String;
var i: Integer;
begin
  Result := s;
  for i := 1 to Length(Result) do
    case Result[i] of
      #13:
        Result[i] := #1;
      #10:
        Result[i] := #2;
    end;
end;
{-----------------------------------------------------------------------}
function RVFDecodeLineBreaks(const s: String): String;
var i: Integer;
begin
  Result := s;
  for i := 1 to Length(Result) do
    case Result[i] of
      #1:
        Result[i] := #13;
      #2:
        Result[i] := #10;
    end;
end;

end.
