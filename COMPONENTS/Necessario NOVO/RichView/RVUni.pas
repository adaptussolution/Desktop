
{*******************************************************}
{                                                       }
{       RichView                                        }
{       Unicode-related procedures.                     }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVUni;

interface
{$I RV_Defs.inc}

uses SysUtils, Windows, Classes, Graphics,
     RVItem, RVStyle, RVScroll;

type TRVIntegerArray = array[0..100000] of Integer;
     PRVIntegerArray = ^TRVIntegerArray;
type TRVUnsignedArray = array[0..100000] of Cardinal;
     PRVUnsignedArray = ^TRVUnsignedArray;
type TRVWordArray = array[0..100000] of Word;
     PRVWordArray = ^TRVWordArray;

{$IFNDEF RVDONOTUSEUNICODE}
function RVU_FindLineBreak(Str: PRVWordArray; Length: Integer): Pointer;
{$ENDIF}
function RVU_Copy(const s: String; Index, Count: Integer;
  ItemOptions: TRVItemOptions): String;
procedure RVU_Delete(var s: String; Index, Count: Integer;
  ItemOptions: TRVItemOptions);
procedure RVU_Insert(const Source: String; var s: String; Index: Integer;
  ItemOptions: TRVItemOptions);
procedure RVU_GetTextExtentExPoint(Canvas: TCanvas; const s: String;
  MaxExtent: Integer; var Fit: Integer; PDx: PRVIntegerArray;
  ItemOptions: TRVItemOptions);
procedure RVU_GetTextExtentExPointPC(Canvas: TCanvas; pc: PChar; Length: Integer;
  MaxExtent: Integer; var Fit: Integer; PDx: PRVIntegerArray;
  ItemOptions: TRVItemOptions; var sz: TSize);
function RVU_GetTextCaretPos(Canvas: TCanvas; const s: String;
  PCP: PRVIntegerArray; ItemOptions: TRVItemOptions;
  Width: Integer): Boolean;
function RVU_Length(const s: String; ItemOptions: TRVItemOptions): Integer;
function RVU_TextWidth(const s: String; Canvas: TCanvas;
  ItemOptions: TRVItemOptions): Integer;
function RVU_IsSpace(const s: String; Index: Integer;
  ItemOptions: TRVItemOptions): Boolean;
function RVU_OffsInPChar(Offs: Integer; ItemOptions: TRVItemOptions): Integer;
{$IFDEF RICHVIEWCBDEF3}
function RVU_Charset2CodePage(Charset: TFontCharset): TRVCodePage;
function RVU_Charset2Language(Charset: TFontCharset): Cardinal;
function RVU_GetRawUnicode(const s: WideString):String;
function RVU_RawUnicodeToWideString(const s: String):WideString;
{$ELSE}
function RVU_GetRawUnicode(const s: String):String;
{$ENDIF}
procedure RVU_SwapWordBytes(arr: PWord; Count: Integer);

function RVU_AnsiToUnicode(CodePage: TRVCodePage; const s: String): String;
function RVU_UnicodeToAnsi(CodePage: TRVCodePage; const s: String): String;
function RVU_StrScanW(Str: Pointer; Ch: Word; Length: Integer): Pointer;
function RVU_StrLenW(Str: Pointer): Cardinal;

type TRVUnicodeTestResult = (rvutNo, rvutYes, rvutProbably, rvutEmpty, rvutError);
function RV_TestFileUnicode(const FileName: String): TRVUnicodeTestResult;

function RVU_GetKeyboardCodePage: TRVCodePage;
function RVU_KeyToUnicode(const Key: String): String;

procedure RVU_WriteHTMLEncodedUnicode(Stream: TStream; const s: String;
  NoEmptyLines, SpecialCode: Boolean);
function RVU_GetHTMLEncodedUnicode(const s: String;
  NoEmptyLines, SpecialCode:Boolean): String;

function RV_ReturnCapitalized(const s: String; TextStyle: TFontInfo): String;

function RVU_DrawSelectedTextEx(Left, Top, Width, Height: Integer; const s: String;
  Canvas: TCanvas; Index1,Index2: Integer; ItemOptions: TRVItemOptions;
  BiDiMode: TRVBiDiMode): Boolean;
{$IFNDEF RICHVIEWDEF6}
{$IFDEF RICHVIEWCBDEF3}
function Utf8Decode(const S: String): WideString;
{$ENDIF}
{$ENDIF}

const
  UNI_LF                 = Word($000A);
  UNI_CR                 = Word($000D);
  UNI_LineSeparator      = Word($2028);
  UNI_ParagraphSeparator = Word($2029);
  UNI_VerticalTab        = Word($000B);
  UNI_FormFeed           = Word($000C);
  UNI_LSB_FIRST          = Word($FEFF);
  UNI_MSB_FIRST          = Word($FFFE);
  UNI_FF                 = Word($000C);
  UNI_HYPHEN             = Word($002D);
  UNI_Space              = Word(ord(' '));
  UNI_ZeroWidthSpace     = Word($200B);

  UNI_LSB_FIRST1         = #$FF;
  UNI_LSB_FIRST2         = #$FE;

function RVMAKELCID(plgid: Word): Cardinal;  

var RVNT: Boolean;

implementation
uses CRVData, RVStr;

const
  GETCHARACTERPLACEMENTFLAGS = GCP_USEKERNING or GCP_REORDER or GCP_GLYPHSHAPE or GCP_DIACRITIC;

function MAKELCID(lgid, srtid: Word): Cardinal;
begin
  Result := (Cardinal(srtid) shl 16) or Cardinal(lgid);
end;

function MAKELANGID(p, s: Word): Word;
begin
  Result := (s shl 10) or p;
end;

function RVMAKELCID(plgid: Word): Cardinal;
begin
  Result := MAKELCID(MAKELANGID(plgid, SUBLANG_DEFAULT), SORT_DEFAULT);
end;

{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEUNICODE}
type
  TRVLineBreakClass =
  (
  rvu_lb_OP, // Opening Punctuation
  rvu_lb_CL, // Closing Punctuation
  rvu_lb_QU, // Ambiguous Quotation
  rvu_lb_GL, // Non-breaking ("Glue")
  rvu_lb_NS, // Non Starter
  rvu_lb_EX, // Exclamation/Interrogation
  rvu_lb_SY, // Symbols Allowing Breaks
  rvu_lb_IS, // Infix Separator (Numeric)
  rvu_lb_PR, // Prefix (Numeric)
  rvu_lb_PO, // Postfix (Numeric)
  rvu_lb_NU, // Numeric
  rvu_lb_AL, // Ordinary Alphabetic and Symbol Characters
  rvu_lb_ID, // Ideographic
  rvu_lb_IN, // Inseparable
  rvu_lb_HY, // Hyphen
  rvu_lb_BA, // Break Opportunity After
  rvu_lb_BB, // Break Opportunity Before
  rvu_lb_B2, // Break Opportunity Before and After
  rvu_lb_ZW, // Zero Width Space
  rvu_lb_CM // Attached Characters and Combining Marks
  );

  {
  rvu_lb_BK, // Mandatory Break // may not occur
  rvu_lb_CR, // Carriage Return // may not occur
  rvu_lb_LF, // Line Feed       // may not occur

  rvu_lb_SP, // Space           // special processing
  rvu_lb_SG, // Surrogates                            // treated as AL
  rvu_lb_CB, // Contingent Break Opportunity          // treated as AL
  rvu_lb_XX, // Unknown                               // treated as AL
  rvu_lb_SA, // Complex Context (South East Asian)    // treated as AL
  rvu_lb_AI, // Ambiguous (Alphabetic or Ideographic) // treated as AL
  }

  TRVLineBreakAction =
  (
     bk_DBK,  // direct break
     bk_IBK,  // indirect break
     bk_PBK   // prohibited break
  );
const
  BreakPairs : array [TRVLineBreakClass,TRVLineBreakClass] of TRVLineBreakAction =
  (
  (bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_PBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_IBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_IBK,bk_IBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK),
  (bk_PBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_IBK),
  (bk_IBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_IBK,bk_IBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_IBK,bk_IBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_DBK,bk_IBK,bk_DBK,bk_DBK,bk_DBK,bk_IBK,bk_IBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_DBK,bk_IBK,bk_DBK,bk_DBK,bk_DBK,bk_IBK,bk_IBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK),
  (bk_IBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_DBK,bk_IBK,bk_IBK,bk_IBK,bk_DBK,bk_IBK,bk_IBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_IBK,bk_IBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_IBK,bk_IBK,bk_IBK,bk_DBK,bk_IBK,bk_IBK,bk_IBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_DBK,bk_IBK,bk_IBK,bk_DBK,bk_IBK,bk_IBK,bk_IBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_IBK,bk_DBK,bk_DBK,bk_DBK,bk_IBK,bk_IBK,bk_IBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_IBK,bk_IBK,bk_IBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_IBK,bk_IBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_IBK,bk_IBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK),
  (bk_IBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_IBK,bk_IBK,bk_DBK,bk_PBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK),
  (bk_DBK,bk_PBK,bk_IBK,bk_IBK,bk_IBK,bk_PBK,bk_PBK,bk_PBK,bk_DBK,bk_DBK,bk_IBK,bk_IBK,bk_DBK,bk_IBK,bk_IBK,bk_IBK,bk_DBK,bk_DBK,bk_PBK,bk_IBK)
  );
{------------------------------------------------------------------------------}
function GetCharLineBreakClass(Char: Word): TRVLineBreakClass; forward;
{------------------------------------------------------------------------------}
// (We assumes that Str[Length] character exists)
// Returns the last character to leave on the line.
// In case of spaces, returns the space
function RVU_FindLineBreak(Str: PRVWordArray; Length: Integer): Pointer;
var i,j: Integer;
    cls, cls2: TRVLineBreakClass;
    act: TRVLineBreakAction;
begin
  Result := nil;
  if Str[Length]=UNI_Space then begin
    Result := @(Str[Length]);
    exit;
  end;
  cls := GetCharLineBreakClass(Str[Length]);
  for i := Length-1 downto 0 do begin
    if Str[i]=UNI_Space then
      continue;
    cls2 := GetCharLineBreakClass(Str[i]);
    act := BreakPairs[cls2, cls];
    if (act = bk_IBK) then
      if Str[i+1]<>UNI_Space then
        act := bk_PBK;
    if act in [bk_IBK, bk_DBK] then begin
      j := i;
      while (j+1<Length) and (Str[j+1]=UNI_Space) do
        inc(j);
      Result := @(Str[j]);
      exit;
    end;
    cls := cls2;
  end;
end;
{$ENDIF}
type
{$IFDEF RICHVIEWDEF7}
  TGetCharacterPlacementVal = Integer;
{$ELSE}
  TGetCharacterPlacementVal = LongBool;
{$ENDIF}
{------------------------------------------------------------------------------}
function RVU_DrawSelectedTextEx_(Left, Top, Width, Height: Integer; const s: String;
  Canvas: TCanvas; Index1,Index2: Integer; ItemOptions: TRVItemOptions): Boolean;
var res: TGCPResultsA;
    i,j: Integer;
    POrder,POrderRev: PRVUnsignedArray;
    PDX: PRVIntegerArray;
    Selected: PChar;
    SelectedCount: Integer;
    DX, idx, idx1, idx2, Start: Integer;
    Len: Integer;
    r: TRect;
    sz, sz2: TSmallPoint;
    UseSz2: Boolean;
    {..........................................}
    procedure InitStructure;
    begin
      FillChar(res, sizeof(res), 0);
      FillChar(POrder^, Len*sizeof(Cardinal), 0);
      res.lStructSize := sizeof(res);
      res.lpOrder := @(POrder[0]);
      res.lpDx    := @(PDX[0]);
      res.nGlyphs := Len;
    end;
    {..........................................}
begin
  if rvioUnicode in ItemOptions then
    Len := Length(s) div 2
  else
    Len := Length(s);
  r.Top := Top;
  r.Bottom := Top+Height;
  POrder := nil;
  POrderRev := nil;
  PDX       := nil;
  Selected  := nil;
  try
    GetMem(POrder,    Len*sizeof(Cardinal));
    GetMem(POrderRev, Len*sizeof(Cardinal));
    GetMem(PDX,       Len*sizeof(Integer));
    SelectedCount := Index2-Index1+1;
    GetMem(Selected, SelectedCount);
    FillChar(Selected^, SelectedCount, 1);
    InitStructure;
    UseSz2 := (GetFontLanguageInfo(Canvas.Handle) and GCP_LIGATE)<>0;
    if UseSz2 then begin
      if rvioUnicode in ItemOptions then
        sz2 := TSmallPoint(GetCharacterPlacementW(Canvas.Handle, Pointer(s),
          TGetCharacterPlacementVal(Len), TGetCharacterPlacementVal(0), res,
          GETCHARACTERPLACEMENTFLAGS or GCP_LIGATE))
      else
        sz2 := TSmallPoint(GetCharacterPlacementA(Canvas.Handle, PChar(s),
          TGetCharacterPlacementVal(Len), TGetCharacterPlacementVal(0), res,
          GETCHARACTERPLACEMENTFLAGS or GCP_LIGATE));
      InitStructure;
    end;
    if rvioUnicode in ItemOptions then
      sz := TSmallPoint(GetCharacterPlacementW(Canvas.Handle, Pointer(s),
        TGetCharacterPlacementVal(Len), TGetCharacterPlacementVal(0), res,
        GETCHARACTERPLACEMENTFLAGS))
    else
      sz := TSmallPoint(GetCharacterPlacementA(Canvas.Handle, PChar(s),
        TGetCharacterPlacementVal(Len), TGetCharacterPlacementVal(0), res,
        GETCHARACTERPLACEMENTFLAGS));
    if UseSz2 then
      sz := sz2;
    Result := (Abs(sz.x-Width)<2) and (sz.y>0);
    if Result then begin
      for i := 0 to Len-1 do
        POrderRev[POrder[i]] := i;
      while SelectedCount>0 do begin
        Start := 0;
        for i := 0 to Len-1 do begin
          idx := POrderRev[i];
          if  (idx+1>=Index1) and (idx+1<=Index2) and
              (Selected[idx+1-Index1]<>#0) then begin
            idx1 := idx+1-1;
            idx2 := idx+1+1;
            while (idx2<=Index2) and (POrder[idx2-1]>POrder[idx+1-1]) and
                  (Integer(POrder[idx2-1]-POrder[idx+1-1])=idx2-(idx+1)) do
              inc(idx2);
            dec(idx2);
            while (idx1>=Index1) and (POrder[idx+1-1]>POrder[idx1-1]) and
                  (Integer(POrder[idx+1-1]-POrder[idx1-1])=(idx+1)-idx1) do
              dec(idx1);
            inc(idx1);
            r.Left := Left+Start;
            r.Right := r.Left;
            for j := idx1 to idx2 do begin
              //Assert(Selected[j-Index1]<>#0);
              Selected[j-Index1] := #0;
              dec(SelectedCount);
              inc(r.Right, PDX[POrder[j-1]]);
            end;
            if rvioUnicode in ItemOptions then begin
              ExtTextOutW(Canvas.Handle, Left,Top, ETO_CLIPPED or ETO_OPAQUE, @r,
                         Pointer(s), Length(s) div 2, nil);
              end
            else begin
              ExtTextOutA(Canvas.Handle, Left,Top, ETO_CLIPPED or ETO_OPAQUE, @r,
                         PChar(s), Length(s), nil);
            end;
            break;
          end;
          dx  := PDX[i];
          inc(Start,dx);
        end;
      end;
    end;
  finally
    FreeMem(POrder);
    FreeMem(POrderRev);
    FreeMem(PDX);
    FreeMem(Selected);
  end;
end;
{------------------------------------------------------------------------------}
function RVU_GetTextCaretPos(Canvas: TCanvas; const s: String;
  PCP: PRVIntegerArray; ItemOptions: TRVItemOptions;
  Width: Integer): Boolean;
var res: TGCPResultsA;
    i: Integer;
    POrder,POrderRev: PRVUnsignedArray;
    PDX: PRVIntegerArray;
    PClass: PChar;
    DX, idx: Integer;
    cls: Char;
    p: Integer;
    Len: Integer;
    sz, sz2: TSmallPoint;
    UseSz2: Boolean;
    {..........................................}
    procedure InitStructure;
    begin
      FillChar(res, sizeof(res), 0);
      FillChar(POrder^, Len*sizeof(Cardinal), 0);
      FillChar(PDX^,    Len*sizeof(Integer),  0);
      FillChar(PClass^, Length(s),            0);
      res.lStructSize := sizeof(res);
      res.nGlyphs := Len;
      res.lpOrder := @(POrder[0]);
      res.lpClass := PClass;
      res.lpDx    := @(PDX[0]);
    end;
    {..........................................}
begin
  if rvioUnicode in ItemOptions then
    Len := Length(s) div 2
  else
    Len := Length(s);
  POrder := nil;
  POrderRev := nil;
  PClass := nil;
  PDX    := nil;
  try
    GetMem(POrder,    Len*sizeof(Cardinal));
    GetMem(POrderRev, Len*sizeof(Cardinal));
    GetMem(PDX,       Len*sizeof(Integer));
    GetMem(PClass,    Length(s)); // for any case
    InitStructure;
    UseSz2 := (GetFontLanguageInfo(Canvas.Handle) and GCP_LIGATE)<>0;
    if UseSz2 then begin
      if rvioUnicode in ItemOptions then
        sz2 := TSmallPoint(GetCharacterPlacementW(Canvas.Handle, Pointer(s),
          TGetCharacterPlacementVal(Len), TGetCharacterPlacementVal(0), res,
          GETCHARACTERPLACEMENTFLAGS or GCP_LIGATE))
      else
        sz2 := TSmallPoint(GetCharacterPlacementA(Canvas.Handle, PChar(s),
          TGetCharacterPlacementVal(Len), TGetCharacterPlacementVal(0), res,
          GETCHARACTERPLACEMENTFLAGS or GCP_LIGATE));
      InitStructure;
    end;
    if rvioUnicode in ItemOptions then
      sz := TSmallPoint(GetCharacterPlacementW(Canvas.Handle, Pointer(s),
        TGetCharacterPlacementVal(Len), TGetCharacterPlacementVal(0), res,
        GETCHARACTERPLACEMENTFLAGS))
    else
      sz := TSmallPoint(GetCharacterPlacementA(Canvas.Handle, PChar(s),
        TGetCharacterPlacementVal(Len), TGetCharacterPlacementVal(0), res,
        GETCHARACTERPLACEMENTFLAGS));
    if UseSz2 then
      sz := sz2;
    Result := (Abs(sz.x-Width)<2) and (sz.y>0);
    if Result then begin
      p := 0;
      for i := 0 to Len-1 do
        POrderRev[POrder[i]] := i;
      for i := 0 to Len-1 do begin
        idx := POrderRev[i];
        dx  := PDX[i];
        cls := PClass[idx];
        if cls in [chr(GCPCLASS_ARABIC),
                   chr(GCPCLASS_HEBREW)] then begin
          PCP[idx+1] := p;
          if idx=0 then
            PCP[0] := p+dx;
          end
        else begin
          PCP[idx+1] := p+dx+1;
          if idx=0 then
            PCP[0] := p;
        end;
        inc(p,dx);
      end;
    end;
  finally
    FreeMem(POrder);
    FreeMem(POrderRev);
    FreeMem(PClass);
    FreeMem(PDX);
  end;
end;
{$IFNDEF RVDONOTUSEUNICODE}
{------------------------------------------------------------------------------}
function RVU_Copy(const s: String; Index, Count: Integer; ItemOptions: TRVItemOptions): String;
begin
  if not (rvioUnicode in ItemOptions) then
    Result := Copy(s, Index, Count)
  else
    Result := Copy(s, 1+(Index-1)*2, Count*2);
end;
{------------------------------------------------------------------------------}
procedure RVU_GetTextExtentExPoint(Canvas: TCanvas; const s: String;
                                  MaxExtent: Integer; var Fit: Integer;
                                  PDx: PRVIntegerArray;
                                  ItemOptions: TRVItemOptions);
var sz: TSize;
    i: Integer;
  {$IFNDEF RICHVIEWDEF4}
    allocated: Boolean;
  {$ENDIF}
begin
  if Length(s)=0 then begin
    Fit := 0;
    exit;
  end;
  {$IFNDEF RICHVIEWDEF4}
  allocated := False;
  {$ENDIF}
  if not (rvioUnicode in ItemOptions) then begin
    {$IFNDEF RICHVIEWDEF4}
    if PDx=nil then begin
      GetMem(PDx, (Length(s)+1)*sizeof(Integer));
      allocated := True;
    end;
    {$ENDIF}
    GetTextExtentExPointA(Canvas.Handle,  PChar(s), Length(s), MaxExtent,
                            {$IFDEF RICHVIEWDEF4}
                            @Fit, PInteger(PDx),
                            {$ELSE}
                            Fit, PInteger(PDx)^,
                            {$ENDIF}
                            sz)
    end
  else if not (RVNT) then begin
    for i := 1 to Length(s) div 2 do begin
      GetTextExtentPoint32W(Canvas.Handle, Pointer(s), i, sz);
      if sz.cx>MaxExtent then begin
        Fit := i-1;
        exit;
      end;
      if PDx<>nil then begin
        PDx[0] := sz.cx;
        inc(PChar(PDx), sizeof(Integer));
      end;
    end;
    Fit := Length(s) div 2;
    end
  else begin
    {$IFNDEF RICHVIEWDEF4}
    if PDx=nil then begin
      GetMem(PDx, (Length(s) div 2 +1)*sizeof(Integer));
      allocated := True;
    end;
    {$ENDIF}
    GetTextExtentExPointW(Canvas.Handle, Pointer(s), Length(s) div 2, MaxExtent,
                            {$IFDEF RICHVIEWDEF4}
                            @Fit, PInteger(PDx),
                            {$ELSE}
                            Fit, PInteger(PDx)^,
                            {$ENDIF}
                            sz);
  end;
  {$IFNDEF RICHVIEWDEF4}
  if allocated then
    FreeMem(PDx);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure RVU_GetTextExtentExPointPC(Canvas: TCanvas; pc: PChar; Length: Integer;
                                  MaxExtent: Integer; var Fit: Integer;
                                  PDx: PRVIntegerArray;
                                  ItemOptions: TRVItemOptions;
                                  var sz: TSize);
var i: Integer;
  {$IFNDEF RICHVIEWDEF4}
    allocated: Boolean;
  {$ENDIF}
begin
  if Length=0 then begin
    Fit := 0;
    exit;
  end;
  {$IFNDEF RICHVIEWDEF4}
  allocated := False;
  {$ENDIF}  
  if not (rvioUnicode in ItemOptions) then begin
    {$IFNDEF RICHVIEWDEF4}
    if PDx=nil then begin
      GetMem(PDx, (Length+1)*sizeof(Integer));
      allocated := True;
    end;
    {$ENDIF}
    GetTextExtentExPointA(Canvas.Handle,  pc, Length, MaxExtent,
                            {$IFDEF RICHVIEWDEF4}
                            @Fit, PInteger(PDx),
                            {$ELSE}
                            Fit, PInteger(PDx)^,
                            {$ENDIF}
                            sz);
    end
  else if not (RVNT) then begin
    for i := 1 to Length do begin
      GetTextExtentPoint32W(Canvas.Handle, Pointer(pc), i, sz);
      if sz.cx>MaxExtent then begin
        Fit := i-1;
        exit;
      end;
      if PDx<>nil then begin
        PDx[0] := sz.cx;
        inc(PChar(PDx), sizeof(Integer));
      end;
    end;
    Fit := Length;
    end
  else begin
    {$IFNDEF RICHVIEWDEF4}
    if PDx=nil then begin
      GetMem(PDx, (Length+1)*sizeof(Integer));
      allocated := True;
    end;
    {$ENDIF}
    GetTextExtentExPointW(Canvas.Handle, Pointer(pc), Length, MaxExtent,
                            {$IFDEF RICHVIEWDEF4}
                            @Fit, PInteger(PDx),
                            {$ELSE}
                            Fit, PInteger(PDx)^,
                            {$ENDIF}
                            sz);
  end;
  {$IFNDEF RICHVIEWDEF4}
  if allocated then
    FreeMem(PDx);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
function RVU_Length(const s: String; ItemOptions: TRVItemOptions): Integer;
begin
  if not (rvioUnicode in ItemOptions) then
    Result := Length(s)
  else
    Result := Length(s) div 2;
end;
{------------------------------------------------------------------------------}
function RVU_TextWidth(const s: String; Canvas: TCanvas;
                       ItemOptions: TRVItemOptions): Integer;
var Size: TSize;
begin
  if not (rvioUnicode in ItemOptions) then
    GetTextExtentPoint32A(Canvas.Handle, PChar(s), Length(s), Size)
  else
    GetTextExtentPoint32W(Canvas.Handle, Pointer(PChar(s)), Length(s) div 2, Size);
  Result := Size.cx;
end;
{------------------------------------------------------------------------------}
function RVU_IsSpace(const s: String; Index: Integer;
                     ItemOptions: TRVItemOptions): Boolean;
begin
  if not (rvioUnicode in ItemOptions) then
    Result := s[Index]=' '
  else
    Result := (s[(Index-1)*2+1]=' ') and (s[Index*2]=#0);
end;
{------------------------------------------------------------------------------}
procedure RVU_Delete(var s: String; Index, Count: Integer; ItemOptions: TRVItemOptions);
begin
  if not (rvioUnicode in ItemOptions) then
    Delete(s, Index, Count)
  else
    Delete(s, (Index-1)*2+1, Count*2);
end;
{------------------------------------------------------------------------------}
procedure RVU_Insert(const Source: String; var s: String; Index: Integer; ItemOptions: TRVItemOptions);
begin
  if not (rvioUnicode in ItemOptions) then
    Insert(Source, s, Index)
  else
    Insert(Source, s, (Index-1)*2+1);
end;
{------------------------------------------------------------------------------}
function RVU_OffsInPChar(Offs: Integer; ItemOptions: TRVItemOptions): Integer;
begin
  if not (rvioUnicode in ItemOptions) then
    Result := Offs
  else
    Result := Offs*2;
end;
{------------------------------------------------------------------------------}
{$ELSE}
{------------------------------------------------------------------------------}
function RVU_Copy(const s: String; Index, Count: Integer; ItemOptions: TRVItemOptions): String;
begin
  Result := Copy(s, Index, Count);
end;
{------------------------------------------------------------------------------}
procedure RVU_GetTextExtentExPoint(Canvas: TCanvas; const s: String;
                                  MaxExtent: Integer; var Fit: Integer;
                                  PDx: PRVIntegerArray;
                                  ItemOptions: TRVItemOptions);
var sz: TSize;
{$IFNDEF RICHVIEWDEF4}
    allocated: Boolean;
{$ENDIF}
begin
  if Length(s)=0 then begin
    Fit := 0;
    exit;
  end;
  {$IFNDEF RICHVIEWDEF4}
  if PDx=nil then begin
    GetMem(PDx, (Length+1)*sizeof(Integer));
    allocated := True;
    end
  else
    allocated := False;
  {$ENDIF}
  GetTextExtentExPointA(Canvas.Handle,  PChar(s), Length(s), MaxExtent,
                            {$IFDEF RICHVIEWDEF4}
                            @Fit, PInteger(PDx),
                            {$ELSE}
                            Fit, PInteger(PDx)^,
                            {$ENDIF}
                            sz);
  {$IFNDEF RICHVIEWDEF4}
  if allocated then
    FreeMem(PDx);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure RVU_GetTextExtentExPointPC(Canvas: TCanvas; pc: PChar;
                                  Length: Integer;
                                  MaxExtent: Integer; var Fit: Integer;
                                  PDx: PRVIntegerArray;
                                  ItemOptions: TRVItemOptions;
                                  var sz: TSize);

{$IFNDEF RICHVIEWDEF4}
var
    allocated: Boolean;
{$ENDIF}
begin
  if Length=0 then begin
    Fit := 0;
    exit;
  end;
  {$IFNDEF RICHVIEWDEF4}
  if PDx=nil then begin
    GetMem(PDx, (Length+1)*sizeof(Integer));
    allocated := True;
    end
  else
    allocated := False;
  {$ENDIF}
  GetTextExtentExPointA(Canvas.Handle,  pc, Length, MaxExtent,
                            {$IFDEF RICHVIEWDEF4}
                            @Fit, PInteger(PDx),
                            {$ELSE}
                            Fit, PInteger(PDx)^,
                            {$ENDIF}
                            sz);
  {$IFNDEF RICHVIEWDEF4}
  if allocated then
    FreeMem(PDx);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
function RVU_Length(const s: String; ItemOptions: TRVItemOptions): Integer;
begin
  Result := Length(s);
end;
{------------------------------------------------------------------------------}
function RVU_TextWidth(const s: String; Canvas: TCanvas;
                       ItemOptions: TRVItemOptions): Integer;
var Size: TSize;
begin
  GetTextExtentPoint32(Canvas.Handle, PChar(s), Length(s), Size);
  Result := Size.cx;
end;
{------------------------------------------------------------------------------}
function RVU_IsSpace(const s: String; Index: Integer;
                     ItemOptions: TRVItemOptions): Boolean;
begin
  Result := s[Index]=' ';
end;
{------------------------------------------------------------------------------}
procedure RVU_Delete(var s: String; Index, Count: Integer; ItemOptions: TRVItemOptions);
begin
  Delete(s, Index, Count);
end;
{------------------------------------------------------------------------------}
procedure RVU_Insert(const Source: String; var s: String; Index: Integer; ItemOptions: TRVItemOptions);
begin
  Insert(Source, s, Index);
end;
{------------------------------------------------------------------------------}
function RVU_OffsInPChar(Offs: Integer; ItemOptions: TRVItemOptions): Integer;
begin
  Result := Offs;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function RVU_DrawSelectedTextEx(Left, Top, Width, Height: Integer;
  const s: String; Canvas: TCanvas; Index1,Index2: Integer;
  ItemOptions: TRVItemOptions; BiDiMode: TRVBiDiMode): Boolean;
begin
  if BiDiMode=rvbdUnspecified then
    Result := False
  else
    Result := RVU_DrawSelectedTextEx_(Left, Top, Width, Height, s, Canvas,
      Index1, Index2, ItemOptions);
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
function RVU_Charset2CodePage(Charset: TFontCharset): TRVCodePage;
begin
  // PLEASE REPORT ME ABOUT ERRORS IN THIS TABLE
  case Charset of
    DEFAULT_CHARSET:
      Result := CP_ACP;
    OEM_CHARSET:
       Result := CP_OEMCP;
    MAC_CHARSET:
       Result := CP_MACCP;
    SYMBOL_CHARSET:
      Result := CP_ACP; // ???
    VIETNAMESE_CHARSET:
       Result := 1258;
    ANSI_CHARSET:
      Result := 1252;   // Windows 3.1 US (ANSI)
    SHIFTJIS_CHARSET:
       Result := 932;   // Japan
    HANGEUL_CHARSET:
       Result := 949;   // Korean
    JOHAB_CHARSET:
       Result := 1361;  // Korean (Johab)
    GB2312_CHARSET:
       Result := 936;   // Chinese (PRC, Singapore)
    CHINESEBIG5_CHARSET:
       Result := 950;   // Chinese (Taiwan, Hong Kong)
    GREEK_CHARSET:
       Result := 1253;  // Windows 3.1 Greek
    TURKISH_CHARSET:
       Result := 1254;  // Windows 3.1 Turkish
    HEBREW_CHARSET:
       Result := 1255;   // Hebrew
    ARABIC_CHARSET:
       Result := 1256;   // Arabic
    BALTIC_CHARSET:
       Result := 1257;   // Baltic
    RUSSIAN_CHARSET:
       Result := 1251;   // Windows 3.1 Cyrillic
    THAI_CHARSET:
       Result := 874;    // Thai
    EASTEUROPE_CHARSET:
       Result := 1250;   // Windows 3.1 Eastern European
    else
       Result := CP_ACP;
  end;
end;
{------------------------------------------------------------------------------}
function RVU_Charset2Language(Charset: TFontCharset): Cardinal;
begin
  // PLEASE REPORT ME ABOUT ERRORS IN THIS TABLE
  // Note: trying to make a best guess here;
  // one charset can be used by a lots of languages
  case Charset of
    DEFAULT_CHARSET:
      Result := $0000; // default
    OEM_CHARSET:
       Result := $0400; // default
    MAC_CHARSET:
       Result := $0400; // default
    SYMBOL_CHARSET:
      Result := $0400; // default
    VIETNAMESE_CHARSET:
       Result := $042A;  // by experement with MS Word
    ANSI_CHARSET:
      Result := $0409;   // English US
    SHIFTJIS_CHARSET:
       Result := $0411;   // Japanese
    HANGEUL_CHARSET:
       Result := $0412;   // Korean
    JOHAB_CHARSET:
       Result := $0812;  // Korean (Johab)
    GB2312_CHARSET:
       Result := $0804;   // Chinese (PRC; more options possible here)
    CHINESEBIG5_CHARSET:
       Result := $0404;	  // Chinese (Taiwan; more options possible here)
    GREEK_CHARSET:
       Result := $0408;  // Greek
    TURKISH_CHARSET:
       Result := $041F;  // Turkish
    HEBREW_CHARSET:
       Result := $040D;  // Hebrew
    ARABIC_CHARSET:
       Result := $0000;	 // default - too many options
    BALTIC_CHARSET:
       Result := $0000;  // default - too many options
    RUSSIAN_CHARSET:
       Result := $0419;   // Russian
    THAI_CHARSET:
       Result := $041E;    // Thai
    EASTEUROPE_CHARSET:
       Result := $0400;   // default - too many options
    else
       Result := $0400;
  end;
end;
{------------------------------------------------------------------------------}
function RVU_RawUnicodeToWideString(const s: String):WideString;
begin
  RVCheckUni(Length(s));
  SetLength(Result, Length(s) div 2);
  Move(Pointer(s)^, Pointer(Result)^, Length(s));
end;
{------------------------------------------------------------------------------}
function RVU_GetRawUnicode(const s: WideString):String;
begin
  SetLength(Result, Length(s)*2);
  Move(Pointer(s)^, Pointer(Result)^, Length(Result));
end;
{$ELSE}
{------------------------------------------------------------------------------}
function RVU_GetRawUnicode(const s: String):String;
begin
  Result := s;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure RVU_SwapWordBytes(arr: PWord; Count: Integer);
var i: Integer;
begin
  for i := 0 to Count-1 do begin
    arr^ := Swap(Word(arr^));
    inc(PChar(arr),2);
  end;
end;
{------------------------------------------------------------------------------}
function RVU_AnsiToUnicode(CodePage: TRVCodePage; const s: String): String;
var l: Integer;
begin
  if Length(s)=0 then begin
    Result := '';
    exit;
  end;
  l := MultiByteToWideChar(CodePage,MB_PRECOMPOSED or MB_USEGLYPHCHARS, PChar(s), Length(s),
                           nil, 0);
  if (l=0) and (CodePage<>CP_ACP) then begin
    CodePage := CP_ACP;
    l := MultiByteToWideChar(CodePage, MB_PRECOMPOSED or MB_USEGLYPHCHARS, PChar(s), Length(s),
                           nil, 0);
  end;
  if l<>0 then begin
    SetLength(Result, l*2);
    MultiByteToWideChar(CodePage, MB_PRECOMPOSED or MB_USEGLYPHCHARS, PChar(s), Length(s),
                             Pointer(Result), l);
    end
  else begin
    SetLength(Result, Length(s)*2);
    FillChar(PChar(Result)^, Length(Result), 0);
    for l := 0 to Length(s)-1 do
      Result[l*2+1] := RVDEFAULTCHARACTER
  end;
end;
{------------------------------------------------------------------------------}
function RVU_UnicodeToAnsi(CodePage: TRVCodePage; const s: String): String;
var l: Integer;
    DefChar: Char;
    Flags: Integer;
    Len: Integer;
begin
  if Length(s)=0 then begin
    Result := '';
    exit;
  end;
  RVCheckUni(Length(s));
  DefChar := RVDEFAULTCHARACTER;
  Flags := WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR;
  Len := Length(s) div 2;
  l := WideCharToMultiByte(CodePage, Flags, Pointer(s), Len, nil, 0, @DefChar, nil);
  if (l=0) and (CodePage<>CP_ACP) then begin
    CodePage := CP_ACP;
    l := WideCharToMultiByte(CodePage, Flags, Pointer(s), Len, nil, 0, @DefChar, nil);
  end;
  if l<>0 then begin
    SetLength(Result, l);
    WideCharToMultiByte(CodePage, Flags, Pointer(s), Len, PChar(Result), l, @DefChar, nil);
    end
  else begin
    SetLength(Result, Len);
    FillChar(PChar(Result)^, Len, RVDEFAULTCHARACTER);
  end;
end;
{------------------------------------------------------------------------------}
function RV_TestFileUnicode(const FileName: String): TRVUnicodeTestResult;
var Stream: TFileStream;
    FirstChar: Word;
    Len: Integer;
    s: String;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      if Stream.Size=0 then
        Result := rvutEmpty
      else if Stream.Size mod 2 <> 0 then
        Result := rvutNo
      else begin
        Stream.ReadBuffer(FirstChar, 2);
        if (FirstChar=UNI_LSB_FIRST) or
           (FirstChar=UNI_MSB_FIRST) then
          Result := rvutYes
        else begin
          Len := Stream.Size-2;
          if Len>500 then Len := 500;
          SetLength(s, Len);
          Stream.ReadBuffer(PChar(s)^, Len);
          if Pos(#0, s)<>0 then
            Result := rvutYes
          else
            Result := rvutProbably;
        end;
      end;
    finally
      Stream.Free;
    end;
  except
    Result := rvutError;
  end;
end;
{------------------------------------------------------------------------------}
function RVU_GetKeyboardCodePage: TRVCodePage;
var Buf: String;
    Len: Integer;
    Locale: LCID;
    {$IFNDEF RICHVIEWCBDEF3}
const LOCALE_IDEFAULTANSICODEPAGE     = $00001004;
    {$ENDIF}
begin
  Locale := GetKeyboardLayout(0) and $FFFF;
  Len := GetLocaleInfo(Locale, LOCALE_IDEFAULTANSICODEPAGE, nil, 0);
  SetLength(Buf, Len);
  GetLocaleInfo(Locale, LOCALE_IDEFAULTANSICODEPAGE, PChar(Buf), Len);
  Result := StrToIntDef(Buf, GetACP);
end;
{------------------------------------------------------------------------------}
function RVU_KeyToUnicode(const Key: String): String;
begin
  Result :=  RVU_AnsiToUnicode(RVU_GetKeyboardCodePage, Key);
end;
{------------------------------------------------------------------------------}
function RVU_StrScanW(Str: Pointer; Ch: Word; Length: Integer): Pointer;
// in: Str -> EAX, Ch -> EDX, Length -> ECX
// out: Result -> EAX
// Assums Str<>nil
asm
    JCXZ @@RetNil
@@Loop:
    CMP [EAX], DX
    JE @@Done
    INC EAX
    INC EAX
    DEC ECX
    JNZ @@Loop
@@RetNil:
    XOR EAX, EAX
@@Done:
end;
{------------------------------------------------------------------------------}
{ returns number of characters in a string excluding the null terminator       }
function RVU_StrLenW(Str: Pointer): Cardinal;
// in: Str -> EAX
// out: Result -> EAX
asm
    MOV EDX, EDI
    MOV EDI, EAX
    MOV ECX, 0FFFFFFFFH
    XOR AX, AX
    REPNE SCASW
    MOV EAX, 0FFFFFFFEH
    SUB EAX, ECX
    MOV EDI, EDX
end;
{------------------------------------------------------------------------------}
function GetCharHTMLCode(ch: Char; var prevspace, specialcode: Boolean; last: Boolean): String;
begin
  if specialcode then begin
    Result := ch;
    prevspace := False;
    exit;
  end;
  if ch='&' then begin
    Result := '&amp;';
    prevspace := False;
    end
  else if ch='<' then begin
    Result := '&lt;';
    prevspace := False;
    end
  else if ch='>' then begin
    Result := '&gt;';
    prevspace := False;
    end
  else if ch=' ' then begin
    if prevspace or last then begin
      Result := '&nbsp;';
      prevspace := False;
      end
    else begin
      Result := ch;
      prevspace := True;
      end
    end
  else begin
    Result := ch;
    prevspace := False;
  end;
end;
{------------------------------------------------------------------------------}
procedure RVU_WriteHTMLEncodedUnicode(Stream: TStream; const s: String;NoEmptyLines,SpecialCode:Boolean);
var p: PWord;
    chars: String;
    i: Integer;
    prevspace: Boolean;
    Len: Integer;
begin
  if (Length(s)=0) and NoEmptyLines then begin
    chars := '&nbsp;';
    Stream.WriteBuffer(PChar(chars)^,Length(chars));
  end;
  prevspace := True;
  p := PWord(PChar(s));
  Len := Length(s) div 2;
  for i := 1 to Len do begin
    if (p^<128) then
      chars := GetCharHTMLCode(chr(p^), prevspace, SpecialCode, i=Len)
    else begin
      chars := Format('&#%d;',[p^]);
      prevspace := False;
    end;
    Stream.WriteBuffer(PChar(chars)^,Length(chars));
    inc(PChar(p),2);
  end;
end;
{------------------------------------------------------------------------------}
function RVU_GetHTMLEncodedUnicode(const s: String;NoEmptyLines,SpecialCode:Boolean): String;
var p: PWord;
    i, Len: Integer;
    prevspace: Boolean;
begin
  prevspace := True;
  Result := '';
  p := PWord(PChar(s));
  Len := Length(s) div 2;
  for i := 1 to Len do begin
    if (p^<128) then
      Result := Result+GetCharHTMLCode(chr(p^), prevspace,SpecialCode, i=Len)
    else begin
      Result := Result+Format('&#%d;',[p^]);
      prevspace := False;
    end;
    inc(PChar(p),2);
  end;
  if NoEmptyLines and (Length(Result)=0) then
    Result := '&nbsp;';
end;
{------------------------------------------------------------------------------}
function RV_ReturnCapitalized(const s: String; TextStyle: TFontInfo): String;
begin
  if rvfsAllCaps in TextStyle.StyleEx then begin
    {$IFNDEF RVDONOTUSEUNICODE}
    if TextStyle.Unicode then begin
      if RVNT then begin
        SetString(Result, PChar(s), Length(s));
        CharUpperBuffW(Pointer(Result), Length(s) div 2);
        end
      else
        Result := s;
      end
    else
   {$ENDIF}
     Result := AnsiUpperCase(s);
   end
  else
    Result := s;
end;

procedure RVCheckNT;
var vi: TOSVersionInfo;
begin
  vi.dwOSVersionInfoSize := sizeof(vi);
  GetVersionEx(vi);
  RVNT := vi.dwPlatformId=VER_PLATFORM_WIN32_NT;
end;

{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEUNICODE}
{$O+}
function GetCharLineBreakClass(Char: Word): TRVLineBreakClass;
begin
  case Char of
    $002D:
      Result := rvu_lb_HY;
    $002F:
      Result := rvu_lb_SY;
    $200B:
      Result := rvu_lb_ZW;
    $2014:
      Result := rvu_lb_B2;
    $2024..$2026:
      Result := rvu_lb_IN;
    $002C,$002E,$003A..$003B,$0589:
      Result := rvu_lb_IS;
    $00A0,$0F0C,$2007,$2011,$202F,$2060,$FEFF:
      Result := rvu_lb_GL;
    $00B4,$02C8,$02CC,$1806:
      Result := rvu_lb_BB;      
    $0009,$007C,$00AD,$058A,$0F0B,$1361,$1680,$17D5,$2000..$2006,$2008..$200A,
    $2010,$2012..$2013,$2027,$205F:
      Result := rvu_lb_BA;
    $0021,$003F,$2762..$2763,$FE56..$FE57,$FF01,$FF1F:
      Result := rvu_lb_EX;
    $0022,$0027,$00AB,$00BB,$2018..$2019,$201B..$201D,$201F,$2039..$203A,$23B6,
    $275B..$275E:
      Result := rvu_lb_QU;
    $0024,$002B,$005C,$00A3..$00A5,$00B1,$09F2..$09F3,$0E3F,$17DB,$20A0..$20A6,
    $20A8..$20B1,$2116,$2212..$2213,$FE69,$FF04,$FFE1,$FFE5..$FFE6:
      Result := rvu_lb_PR;
    $0025,$00A2,$00B0,$2030..$2037,$20A7,$2103,$2109,$2126,$FDFC,$FE6A,$FF05,
    $FFE0:
      Result := rvu_lb_PO;
    $0028,$005B,$007B,$0F3A,$0F3C,$169B,$201A,$201E,$2045,$207D,$208D,$2329,
    $23B4,$2768,$276A,$276C,$276E,$2770,$2772,$2774,$27E6,$27E8,$27EA,$2983,
    $2985,$2987,$2989,$298B,$298D,$298F,$2991,$2993,$2995,$2997,$29D8,$29DA,
    $29FC,$3008,$300A,$300C,$300E,$3010,$3014,$3016,$3018,$301A,$301D,$FD3E,
    $FE35,$FE37,$FE39,$FE3B,$FE3D,$FE3F,$FE41,$FE43,$FE59,$FE5B,$FE5D,$FF08,
    $FF3B,$FF5B,$FF5F,$FF62:
      Result := rvu_lb_OP;
    $0029,$005D,$007D,$0F3B,$0F3D,$169C,$2046,$207E,$208E,$232A,$23B5,$2769,
    $276B,$276D,$276F,$2771,$2773,$2775,$27E7,$27E9,$27EB,$2984,$2986,$2988,
    $298A,$298C,$298E,$2990,$2992,$2994,$2996,$2998,$29D9,$29DB,$29FD,
    $3001..$3002,$3009,$300B,$300D,$300F,$3011,$3015,$3017,$3019,$301B,
    $301E..$301F,$FD3F,$FE36,$FE38,$FE3A,$FE3C,$FE3E,$FE40,$FE42,$FE44,
    $FE50,$FE52,$FE5A,$FE5C,$FE5E,$FF09,$FF0C,$FF0E,$FF3D,$FF5D,$FF60..$FF61,
    $FF63..$FF64:
      Result := rvu_lb_CL;
    $0030..$0039,$0660..$0669,$06F0..$06F9,$0966..$096F,$09E6..$09EF,
    $0A66..$0A6F,$0AE6..$0AEF,$0B66..$0B6F,$0BE7..$0BEF,$0C66..$0C6F,
    $0CE6..$0CEF,$0D66..$0D6F,$0E50..$0E59,$0ED0..$0ED9,$0F20..$0F29,
    $1040..$1049,$1369..$1371,$17E0..$17E9,$1810..$1819:
      Result := rvu_lb_NU;
    $0E5A..$0E5B,$17D4,$17D6..$17DA,$203C,$2044,$3005,$301C,$303B..$303C,
    $3041,$3043,$3045,$3047,$3049,$3063,$3083,$3085,$3087,$308E,$3095..$3096,
    $309B..$309E,$30A0..$30A1,$30A3,$30A5,$30A7,$30A9,$30C3,$30E3,$30E5,$30E7,
    $30EE,$30F5..$30F6,$30FB,$30FD,$31F0..$31FF,$FE54..$FE55,$FF1A..$FF1B,
    $FF65,$FF67..$FF70,$FF9E..$FF9F:
      Result := rvu_lb_NS;
    $1100..$1159,$115F,$2E80..$2E99,$2E9B..$2EF3,$2F00..$2FD5,$2FF0..$2FFB,
    $3000,$3003..$3004,$3006..$3007,$3012..$3013,$3020..$3029,$3030..$303A,
    $303D..$303F,$3042,$3044,$3046,$3048,$304A..$3062,$3064..$3082,$3084,
    $3086,$3088..$308D,$308F..$3094,$309F,$30A2,$30A4,$30A6,$30A8,$30AA..$30C2,
    $30C4..$30E2,$30E4,$30E6,$30E8..$30ED,$30EF..$30F4,$30F7..$30FA,$30FC,
    $30FE..$30FF,$3105..$312C,$3131..$318E,$3190..$31B7,$3200..$321C,
    $3220..$3243,$3251..$327B,$327F..$32CB,$32D0..$32FE,$3300..$3376,
    $337B..$33DD,$33E0..$33FE,$3400..$4DB5,$4E00..$9FA5,$A000..$A48C,
    $A490..$A4C6,$AC00..$D7A3,$F900..$FA2D,$FA30..$FA6A,$FE30..$FE34,
    $FE45..$FE46,$FE49..$FE4F,$FE51,$FE58,$FE5F..$FE66,$FE68,$FE6B,
    $FF02..$FF03,$FF06..$FF07,$FF0A..$FF0B,$FF0D,$FF0F..$FF19,$FF1C..$FF1E,
    $FF20..$FF3A,$FF3C,$FF3E..$FF5A,$FF5C,$FF5E,$FFE2..$FFE4:
      Result := rvu_lb_ID;
    $0000..$0008,$000B,$000E..$001F,$007F..$009F,$0300..$034F,$0360..$036F,
    $0483..$0486,$0488..$0489,$0591..$05A1,$05A3..$05B9,$05BB..$05BD,$05BF,
    $05C1..$05C2,$05C4,$064B..$0655,$0670,$06D6..$06E4,$06E7..$06E8,
    $06EA..$06ED,$070F,$0711,$0730..$074A,$07A6..$07B0,$0901..$0903,$093C,
    $093E..$094D,$0951..$0954,$0962..$0963,$0981..$0983,$09BC,$09BE..$09C4,
    $09C7..$09C8,$09CB..$09CD,$09D7,$09E2..$09E3,$0A02,$0A3C,$0A3E..$0A42,
    $0A47..$0A48,$0A4B..$0A4D,$0A70..$0A71,$0A81..$0A83,$0ABC,$0ABE..$0AC5,
    $0AC7..$0AC9,$0ACB..$0ACD,$0B01..$0B03,$0B3C,$0B3E..$0B43,$0B47..$0B48,
    $0B4B..$0B4D,$0B56..$0B57,$0B82,$0BBE..$0BC2,$0BC6..$0BC8,$0BCA..$0BCD,
    $0BD7,$0C01..$0C03,$0C3E..$0C44,$0C46..$0C48,$0C4A..$0C4D,$0C55..$0C56,
    $0C82..$0C83,$0CBE..$0CC4,$0CC6..$0CC8,$0CCA..$0CCD,$0CD5..$0CD6,
    $0D02..$0D03,$0D3E..$0D43,$0D46..$0D48,$0D4A..$0D4D,$0D57,$0D82..$0D83,
    $0DCA,$0DCF..$0DD4,$0DD6,$0DD8..$0DDF,$0DF2..$0DF3,$0E31,$0E34..$0E3A,
    $0E47..$0E4E,$0EB1,$0EB4..$0EB9,$0EBB..$0EBC,$0EC8..$0ECD,$0F18..$0F19,
    $0F35,$0F37,$0F39,$0F3E..$0F3F,$0F71..$0F84,$0F86..$0F87,$0F90..$0F97,
    $0F99..$0FBC,$0FC6,$102C..$1032,$1036..$1039,$1056..$1059,$1160..$11A2,
    $11A8..$11F9,$1712..$1714,$1732..$1734,$1752..$1753,$1772..$1773,
    $17B4..$17D3,$180B..$180E,$18A9,$200C..$200F,$202A..$202E,$206A..$206F,
    $20D0..$20EA,$302A..$302F,$3099..$309A,$FB1E,$FE00..$FE0F,$FE20..$FE23,
    $FFF9..$FFFB:
      Result := rvu_lb_CM;
    else
      Result := rvu_lb_AL;
  end;
end;
{$ENDIF}

{$IFNDEF RICHVIEWDEF6}
{$IFDEF RICHVIEWCBDEF3}
function Utf8ToUnicode(Dest: PWideChar; MaxDestChars: Cardinal;
  Source: PChar; SourceBytes: Cardinal): Cardinal;
var
  i, count: Cardinal;
  c: Byte;
  wc: Cardinal;
begin
  if Source = nil then
  begin
    Result := 0;
    Exit;
  end;
  Result := Cardinal(-1);
  count := 0;
  i := 0;
  if Dest <> nil then
  begin
    while (i < SourceBytes) and (count < MaxDestChars) do
    begin
      wc := Cardinal(Source[i]);
      Inc(i);
      if (wc and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        wc := wc and $3F;
        if (wc and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
          wc := (wc shl 6) or (c and $3F);
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte

        Dest[count] := WideChar((wc shl 6) or (c and $3F));
      end
      else
        Dest[count] := WideChar(wc);
      Inc(count);
    end;
    if count >= MaxDestChars then count := MaxDestChars-1;
    Dest[count] := #0;
  end
  else
  begin
    while (i < SourceBytes) do
    begin
      c := Byte(Source[i]);
      Inc(i);
      if (c and $80) <> 0 then
      begin
        if i >= SourceBytes then Exit;          // incomplete multibyte char
        c := c and $3F;
        if (c and $20) <> 0 then
        begin
          c := Byte(Source[i]);
          Inc(i);
          if (c and $C0) <> $80 then Exit;      // malformed trail byte or out of range char
          if i >= SourceBytes then Exit;        // incomplete multibyte char
        end;
        c := Byte(Source[i]);
        Inc(i);
        if (c and $C0) <> $80 then Exit;       // malformed trail byte
      end;
      Inc(count);
    end;
  end;
  Result := count+1;
end;

function Utf8Decode(const S: String): WideString;
var
  L: Integer;
  Temp: WideString;
begin
  Result := '';
  if S = '' then Exit;
  SetLength(Temp, Length(S));

  L := Utf8ToUnicode(PWideChar(Temp), Length(Temp)+1, PChar(S), Length(S));
  if L > 0 then
    SetLength(Temp, L-1)
  else
    Temp := '';
  Result := Temp;
end;

{$ENDIF}
{$ENDIF}

{

type TRVBiDiClass =
 (
   rvu_bd_BN,  // Boundary neutral (type of RLE etc after explicit levels)
   rvu_bd_S,   // Segment Separator (TAB)
   rvu_bd_B,   // Paragraph Separator
   rvu_bd_WS,  // White space
   rvu_bd_ON,  // Other Neutral
   rvu_bd_ET,  // European Terminator (post/prefix e.g. $ and %)
   rvu_bd_CS,  // Common Separator
   rvu_bd_ES,  // European Separator
   rvu_bd_EN,  // European Number
   rvu_bd_L,   // Left Letter
   rvu_bd_NSM, // Non-spacing Mark
   rvu_bd_R,   // Right Letter
   rvu_bd_AL,  // Arabic Letter (Right-to-left)
   rvu_bd_AN,  // Arabic Number
   rvu_bd_LRE,
   rvu_bd_RLE,
   rvu_bd_RLE,
   rvu_bd_PDF,
   rvu_bd_LRO,
   rvu_bd_RLO
 );

function GetBiDiClass(chr: WideChar): TRVBiDiClass;
begin
  case ord(chr) of
    $0000..$0008,$000E..$001B,$007F..$0084,$0086..$009F,$070F,$180B..$180E,
    $200B..$200D,$206A..$206F,$FEFF,$FFF9..$FFFB:
      Result := rvu_bd_BN;
    $0009,$000B,$001F:
      Result := rvu_bd_S;
    $000A,$000D,$001C..$001E,$0085,$2029:
      Result := rvu_bd_B
    $000C,$0020,$1680,$2000..$200A,$2028,$202F,$3000:
      Result := rvu_bd_WS
    $0021..$0022,$0026..$002A,$003B..$0040,$005B..$0060,$007B..$007E,$00A1,
    $00A6..$00A9,$00AB..$00AF,$00B4,$00B6..$00B8,$00BB..$00BF,$00D7,$00F7,
    $02B9..$02BA,$02C2..$02CF,$02D2..$02DF,$02E5..$02ED,$0374..$0375,
    $037E..$0385,$0387,$058A,$06E9,$0F3A..$0F3D,$169B..$169C,$1800..$180A,
    $1FBD,$1FBF..$1FC1,$1FCD..$1FCF,$1FDD..$1FDF,$1FED..$1FEF,$1FFD..$1FFE,
    $2010..$2027,$2035..$204D,$207C..$207E,$208C..$208E,$2100..$2101,
    $2103..$2106,$2108..$2109,$2114,$2116..$2118,$211E..$2123,$2125,$2127,$2129,
    $2132,$213A..$215F,$2190..$2211,$2214..$2335,$237B..$2394,$2396..$244A,
    $2500..$2FFB,$3001..$3004,$3008..$3020,$3030,$3036..$3037,$303E..$303F,
    $309B..$309C,$30FB,$A490..$A4C6,$FD3E..$FD3F,$FE30..$FE4F,$FE51,$FE54,
    $FE56..$FE5E,$FE60..$FE61,$FE64..$FE68,$FE6B,$FF01..$FF02,$FF06..$FF0A,
    $FF1B..$FF20,$FF3B..$FF40,$FF5B..$FF65,$FFE2..$FFE4,$FFE8..$FFEE,
    $FFFC..$FFFD:
      Result := rvu_bd_ON;
    $0023..$0025,$002B,$002D,$00A2..$00A5,$00B0..$00B1,$066A,$09F2..$09F3,$0E3F,
    $17DB,$2030..$2034,$207A..$207B,$208A..$208B,$20A0..$20AF,$212E,
    $2212..$2213,$FB29,$FE5F,$FE62..$FE63,$FE69..$FE6A,$FF03..$FF05,
    $FF0B,$FF0D,$FFE0..$FFE1,$FFE5..$FFE6:
      Result := rvu_bd_ET;
    $002C,$002E,$003A,$00A0,$060C,$FE50,$FE52,$FE55,$FF0C,$FF0E,$FF1A:
      Result := rvu_bd_CS;
    $002F,$FF0F:
      Result := rvu_bd_ES;
    $0030..$0039,$00B2..$00B3,$00B9,$06F0..$06F9,$2070..$2079,$2080..$2089,
    $2460..$249B,$24EA,$FF10..$FF19:
      Result := rvu_bd_EN;
    $0300..$0362,$0483..$0489,$0591..$05BD,$05BF,$05C1..$05C2,$05C4,
    $064B..$0655,$0670,$06D6..$06E4,$06E7..$06E8,$06EA..$06ED,$0711,
    $0730..$074A,$07A6..$0902,$093C,$0941..$0948,$094D,$0951..$0954,
    $0962..$0963,$0981,$09BC,$09C1..$09C4,$09CD,$09E2..$09E3,$0A02,$0A3C,
    $0A41..$0A4D,$0A70..$0A71,$0A81..$0A82,$0ABC,$0AC1..$0AC8,$0ACD,$0B01,$0B3C,
    $0B3F,$0B41..$0B43,$0B4D..$0B56,$0B82,$0BC0,$0BCD,$0C3E..$0C40,$0C46..$0C56,
    $0CBF,$0CC6,$0CCC..$0CCD,$0D41..$0D43,$0D4D,$0DCA,$0DD2..$0DD6,$0E31,
    $0E34..$0E3A,$0E47..$0E4E,$0EB1,$0EB4..$0EBC,$0EC8..$0ECD,$0F18..$0F19,
    $0F35,$0F37,$0F39,$0F71..$0F7E,$0F80..$0F84,$0F86..$0F87,$0F90..$0FBC,$0FC6,
    $102D..$1030,$1032..$1037,$1039,$1058..$1059,$17B7..$17BD,$17C6,
    $17C9..$17D3,$18A9,$20D0..$20E3,$302A..$302F,$3099..$309A,$FB1E,
    $FE20..$FE23:
      Result := rvu_bd_NSM;
    $05BE,$05C0,$05C3,$05D0..$05F4,$200F,$FB1D,$FB1F..$FB28,$FB2A..$FB4F:
      Result := rvu_bd_R;
    $061B..$064A,$066D,$0671..$06D5,$06E5..$06E6,$06FA..$070D,$0710,
    $0712..$072C,$0780..$07A5,$FB50..$FD3D,$FD50..$FDFB,$FE70..$FEFC:
      Result := rvu_bd_AL;
    $0660..$0669,$066B..$066C:
      Result := rvu_bd_AN;
    $202A:
      Result := rvu_bd_LRE;
    $202B:
      Result := rvu_bd_RLE;
    $202C:
      Result := rvu_bd_PDF;
    $202D:
      Result := rvu_bd_LRO;
    $202E:
      Result := rvu_bd_RLO;
    else
      Result :=  rvu_bd_L;
  end;
end;

}

initialization
  RVCheckNT


end.
