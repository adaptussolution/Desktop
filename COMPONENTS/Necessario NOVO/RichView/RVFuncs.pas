
{*******************************************************}
{                                                       }
{       RichView                                        }
{       Miscellaneous procedures.                       }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVFuncs;

interface

{$I RV_Defs.inc}

uses SysUtils, Windows, Classes, RVStyle,
     {$IFNDEF RVDONOTUSEJPEGIMAGE}
     Jpeg,
    {$ENDIF}
     Graphics;

{---------------------------  Text & Tags  ------------------------------------}

function RV_CopyTag(SourceTag: Integer; TagsArePChars: Boolean): Integer;
function RV_CompareTags(Tag1, Tag2: Integer; TagsArePChars: Boolean): Boolean;
function RV_ReplaceTabsA(const s: String; SpacesInTab: Integer): String;
function RV_ReplaceTabsW(const s: String; SpacesInTab: Integer): String;
function RV_CharPos(const Str: PChar; Chr: Char; Length: Integer): Integer; assembler;
procedure RV_ReplaceStr(var str: String; oldstr, newstr: String);
function RV_GetHintStr(DocFormat: TRVSaveFormat; const Hint: String): String;

{--------------------------  HTML functions  ----------------------------------}

function RV_GetHTMLRGBStr(Color: TColor; Quotes: Boolean): String;
function RV_GetCSSBkColor(Color: TColor): String;
function RV_GetHTMLPath(const Path: String): String;
function RV_GetHTMLFontCSS(Font: TFont): String;
function RV_HTMLGetFontSize(pts: Integer): Integer;
function RV_HTMLOpenFontTag(ts, normalts: TFontInfo; Relative: Boolean): String;
function RV_HTMLOpenFontTag2(fnt: TFont; normalts: TFontInfo): String;
function RV_HTMLCloseFontTag(ts: TFontInfo; normalts: TFontInfo; Relative: Boolean):String;
function RV_HTMLCloseFontTag2(fnt: TFont; normalts: TFontInfo):String;
{$IFNDEF RVDONOTUSEHTML}
function RV_MakeHTMLStr(const str:String;NoEmptyLines,SpecialCode:Boolean): String;
{$IFDEF RICHVIEWCBDEF3}
function RV_CharSet2HTMLLang(CharSet: TFontCharset): String;
{$ENDIF}
{$ENDIF}

{--------------------------  RTF functions  -----------------------------------}

{$IFNDEF RVDONOTUSERTF}
function RVMakeRTFStr(const s:String; SpecialCode: Boolean): String;
function MakeRTFIdentifierStr(const s:String): String;
function MakeRTFBookmarkNameStr(const s:String): String;
procedure RVWriteUnicodeRTFStr(Stream: TStream; const s: String;
  CodePage: TRVCodePage; SaveAnsi, SpecialCode: Boolean);
{$ENDIF}

{----------------------  Conversion of coordinates  ---------------------------}

function RV_XToDevice(X: Integer; const sad: TRVScreenAndDevice): Integer;
function RV_YToDevice(Y: Integer; const sad: TRVScreenAndDevice): Integer;
function RV_XToScreen(X: Integer; const sad: TRVScreenAndDevice): Integer;
function RV_YToScreen(Y: Integer; const sad: TRVScreenAndDevice): Integer;
procedure RV_RectToScreen(var R: TRect; const sad: TRVScreenAndDevice);
procedure RV_InfoAboutSaD(var sad:TRVScreenAndDevice; Canvas: TCanvas);
function RV_PointInRect(X,Y: Integer; Left,Top,Width,Height: Integer): Boolean;

{------------------------  Graphics & Colors  ---------------------------------}

function RV_CreateGraphicsDefault(GraphicClass: TGraphicClass): TGraphic;
procedure RV_AfterImportGraphicDefault(Graphic: TGraphic);
function RV_GetLuminance(Color: TColor): Integer;
function RV_GetGray(Color: TColor): TColor;
function RV_GetPrnColor(Color: TColor): TColor;
function RV_GetColor(Color: TColor; ColorMode: TRVColorMode): TColor;
function RV_GetBackColor(Color: TColor; ColorMode: TRVColorMode): TColor;
function RV_IsGraphicTransparent(gr: TGraphic): Boolean;
procedure RV_SetPaletteToPicture(gr: TGraphic; PLogPal: PLogPalette);
procedure RV_PictureToDevice(Canvas: TCanvas; x,y, width, height: Integer;
  sad:TRVScreenAndDevice; gr: TGraphic; ToScreen: Boolean);

{ ---------------------------- Others -----------------------------------------}
{$IFNDEF RICHVIEWCBDEF3}
function ExtractRelativePath(const BaseName, DestName: string): string;
{$ENDIF}

type
  TRV_CreateGraphicsFunction = function (GraphicClass: TGraphicClass): TGraphic;
  TRV_AfterImportGraphicsProc = procedure(Graphic: TGraphic);
var
  { Procedure for creating graphic object by graphic class
    used as a workaround for D2-D5, CB1-CB5 bug (private constructor in
    TGraphic). Assign your own procedure if you use third-party graphic
    classes }
  RV_CreateGraphics: TRV_CreateGraphicsFunction;
  { Procedure for calling after importing external graphics from RTF documents }
  RV_AfterImportGraphic: TRV_AfterImportGraphicsProc;

implementation

uses RVFMisc, RVUni;

type SetOfChar = set of Char;


procedure ReplaceChars(var str: String; Replacer: Char; Replaced: SetOfChar); forward;

{===========================  Text & Tags  ====================================}
{ Returns a copy of SourceTag. If TagsArePChars, it assumes that SourceTag is
  a pointer to ANSIZ string, creates a copy of this string and returns it      }
function RV_CopyTag(SourceTag: Integer; TagsArePChars: Boolean): Integer;
begin
  if (SourceTag<>0) and TagsArePChars then
    Result := Integer(StrNew(PChar(SourceTag)))
  else
    Result := SourceTag;
end;
{------------------------------------------------------------------------------}
{ Returns true if Tag1 is equal to Tag2. If TagsArePChars, tags are compared
  as strings, otherwise as integers                                            }
function RV_CompareTags(Tag1, Tag2: Integer; TagsArePChars: Boolean): Boolean;
begin
  if TagsArePChars then
    if (Tag1=0) then
      if (Tag2=0) then
        Result := True
      else
        Result := False
    else
      if (Tag2=0) then
        Result := False
      else
        Result := StrComp(PChar(Tag1),PChar(Tag2))=0
  else
    Result := Tag1=Tag2;
end;
{------------------------------------------------------------------------------}
{ Replaces all tabs (#9) with a sequence of SpacesInTab space characters       }
function RV_ReplaceTabsA(const s: String; SpacesInTab: Integer): String;
var p: Integer;
    spaces: String;
begin
  Result := s;
  p := Pos(#9,Result);
  if p<>0 then begin
    SetLength(spaces,SpacesInTab);
    FillChar(PChar(spaces)^, SpacesInTab, ' ');
  end;
  while p<>0 do begin
    Delete(Result,p,1);
    Insert(spaces,Result,p);
    p := Pos(#9,Result);
  end;
end;
{------------------------------------------------------------------------------}
{ The same for unicode string (represented as "raw unicode")                   }
function RV_ReplaceTabsW(const s: String; SpacesInTab: Integer): String;
var i,p: Integer;
    spaces: String;
begin
  Result := s;
  p := Pos(#9#0,Result);
  if p<>0 then begin
    SetLength(spaces,SpacesInTab*2);
    FillChar(PChar(spaces)^, SpacesInTab*2, 0);
    for i := 1 to SpacesInTab do
      spaces[(i-1)*2+1] := ' ';
  end;
  while p<>0 do begin
    Delete(Result,p,2);
    Insert(spaces,Result,p);
    p := Pos(#9#0,Result);
  end;
end;
{------------------------------------------------------------------------------}
{ Returns index of character (Chr) in string (Str) having length Length.
  Returns 0 if not found. Otherwise index of the first occurence of the character
  (1-based).                                                                   }
function RV_CharPos(const Str: PChar {EAX}; Chr: Char {DL} ; Length: Integer {ECX}): Integer; assembler;
asm
        TEST    EAX,EAX
        JE      @@2
        PUSH    EDI
        PUSH    EBX
        MOV     EDI,Str
        MOV     EBX,Str
        MOV     AL,Chr
        REPNE   SCASB
        MOV     EAX,0
        JNE     @@1
        MOV     EAX,EDI
        SUB     EAX,EBX
@@1:    POP     EBX
        POP     EDI
@@2:
end;
{------------------------------------------------------------------------------}
{ Replaces in str all substrings oldstr with substring newstr.
  Case insensitive. Newstr CANNOT contain oldstr as a substring.               }
procedure RV_ReplaceStr(var str: String; oldstr, newstr: String);
var p: Integer;
begin
   while true do begin
     p := pos(oldstr, str);
     if p=0 then break;
     Delete(str,p, Length(oldstr));
     Insert(newstr, str, p);
   end;
end;
{------------------------------------------------------------------------------}
{ Replaces in str all substrings oldstr with substring newstr.
  Case insensitive. Newstr can contain oldstr as a substring.                  }
procedure RV_ReplaceStr2(var str: String; oldstr, newstr: String);
var p,ptr: Integer;
    s: String;
begin
   s := str;
   ptr := 1;
   while true do begin
     p := pos(oldstr, s);
     if p=0 then break;
     inc(p, ptr-1);
     Delete(str,p, Length(oldstr));
     Insert(newstr, str, p);
     ptr := p+Length(newstr);
     s := Copy(str, ptr, Length(str)+1-ptr);
   end;
end;
{------------------------------------------------------------------------------}
{ Returns code for inserting hint (tool tip) in HTML and RTF                   }
function RV_GetHintStr(DocFormat: TRVSaveFormat; const Hint: String): String;
begin
  Result := Hint;
  if Result='' then
    exit;
  case DocFormat of
    rvsfHTML:
      begin
        {$IFNDEF RVDONOTUSEHTML}
        ReplaceChars(Result , '''', ['"']);
        Result := 'title="'+Result+'"';
        {$ENDIF}
      end;
    rvsfRTF:
      begin
        {$IFNDEF RVDONOTUSERTF}
        ReplaceChars(Result , '''', ['"']);
        Result := MakeRTFIdentifierStr(Result);
        Result := '\o "'+Result+'"';
        {$ENDIF}
      end;
  end;
end;
{==========================  HTML functions  ==================================}
{ Returns HTML representation of color ('#RRGGBB' string).
  For clNone, returns empty string.
  Processes clWindowText as clBlack.                                           }
function RV_GetHTMLRGBStr(Color: TColor; Quotes: Boolean): String;
begin
  if Color=clWindowText then
    Color := clBlack;
  if Color=clNone then
    Result := ''
  else begin
    Result := IntToHex(ColorToRGB(Color),6);
    Result := '#'+System.Copy(Result,5,2)+System.Copy(Result,3,2)+System.Copy(Result,1,2);
    if Quotes then
      Result := '"'+Result+'"';
  end;
end;
{------------------------------------------------------------------------------}
{ The same as RV_GetHTMLRGBStr, but returns 'transparent' for clNone           }
function RV_GetCSSBkColor(Color: TColor): String;
begin
  if Color=clNone then
    Result := 'transparent'
  else
    Result := RV_GetHTMLRGBStr(Color, False);
end;
{------------------------------------------------------------------------------}
{ Replaces all '\' with '/'                                                    }
function RV_GetHTMLPath(const Path: String): String;
var i: Integer;
begin
  Result := Path;
  for i := 1 to Length(Result) do
    if Result[i]='\' then
      Result[i] := '/';
end;
{------------------------------------------------------------------------------}
{ Special concatenation of two strings                                         }
procedure AddStr(var s1: String; const s2: String);
begin
  if s1<>'' then
    s1 := s1+' '+s2
  else
    s1 := s2;
end;
{------------------------------------------------------------------------------}
{ Returns string describing the given font in CSS format                       }
function RV_GetHTMLFontCSS(Font: TFont): String;
var s: String;
begin
  Result := '';
  if fsBold in Font.Style then
    Result := 'bold';
  if fsItalic in Font.Style then
    AddStr(Result, 'italic');
  if Font.Size>0 then
    AddStr(Result, Format('%dpt',[Font.Size]))
  else
    AddStr(Result, Format('%dpx',[Font.Height]));
  AddStr(Result, Format('''%s''',[Font.Name]));
  Result := Format('font: %s;',[Result]);
  s := '';
  if fsUnderline in Font.Style then
    s := 'underline';
  if fsStrikeOut in Font.Style then
    AddStr(s, 'line-through');
  if s<>'' then
    Result := Format('%s text-decoration: %s;',[Result,s]);
  Result := Format('%s color: %s;',[Result,RV_GetHTMLRGBStr(Font.Color, False)]);
end;
{------------------------------------------------------------------------------}
{ Converts the font size in points to the font size for HTML (without CSS).
  HTML uses 7 font sizes.                                                      }
function RV_HTMLGetFontSize(pts: Integer): Integer;
begin
  if pts<=8 then
    Result := 1
  else
    case pts of
      9..10:  Result := 2;
      11..12: Result := 3;
      13..14: Result := 4;
      15..18: Result := 5;
      19..24: Result := 6;
      else    Result := 7;
    end;
end;
{------------------------------------------------------------------------------}
{ Returns opening HTML tags for formatting of text style ts.
  If Relative, it returns a difference in formatting between ts and normalts   }
function RV_HTMLOpenFontTag(ts, normalts: TFontInfo; Relative: Boolean): String;
var s: String;
begin
  s := '';
  if not Relative or (ts.Size<>normalts.Size) then
    s := s+' size='+IntToStr(RV_HTMLGetFontSize(ts.Size));
  if not Relative or (ts.Color<>normalts.Color) then
    s := s+' color='+RV_GetHTMLRGBStr(ts.Color, True);
  if not Relative or (AnsiCompareText(ts.FontName,normalts.FontName)<>0) then
    s := s+' face="'+ts.FontName+'"';
  if s<>'' then
    s := '<FONT'+s+'>';
  if Relative then begin
    if not (fsStrikeOut in ts.Style) and (fsStrikeOut in normalts.Style) then s := s+'</S>';
    if not (fsUnderline in ts.Style) and (fsUnderline in normalts.Style) then s := s+'</U>';
    if not (fsItalic    in ts.Style) and (fsItalic    in normalts.Style) then s := s+'</I>';
    if not (fsBold      in ts.Style) and (fsBold      in normalts.Style) then s := s+'</B>';
    if (fsBold      in ts.Style) and not (fsBold      in normalts.Style) then s := s+'<B>';
    if (fsItalic    in ts.Style) and not (fsItalic    in normalts.Style) then s := s+'<I>';
    if (fsUnderline in ts.Style) and not (fsUnderline in normalts.Style) then s := s+'<U>';
    if (fsStrikeOut in ts.Style) and not (fsStrikeOut in normalts.Style) then s := s+'<S>';
    end
  else begin
    if (fsBold in ts.Style)      then s := s+'<B>';
    if (fsItalic in ts.Style)    then s := s+'<I>';
    if (fsUnderline in ts.Style) then s := s+'<U>';
    if (fsStrikeOut in ts.Style) then s := s+'<S>';
  end;
  if ts.VShift < 0 then
    s := s+'<SUB>'
  else if ts.VShift > 0 then
    s := s+'<SUP>';
  Result := s;
end;
{------------------------------------------------------------------------------}
{ The same as RV_HTMLOpenFontTag(..., True), but formatting is defined by
  fnt: TFont                                                                   }
function RV_HTMLOpenFontTag2(fnt: TFont; normalts: TFontInfo): String;
var s: String;
begin
  s := '';
  if (fnt.Size<>normalts.Size) then
    s := s+' size='+IntToStr(RV_HTMLGetFontSize(fnt.Size));
  if (fnt.Color<>normalts.Color) then
    s := s+' color='+RV_GetHTMLRGBStr(fnt.Color, True);
  if AnsiCompareText(fnt.Name,normalts.FontName)<>0 then
    s := s+' face="'+fnt.Name+'"';
  if s<>'' then
    s := '<FONT'+s+'>';
  if not (fsStrikeOut in fnt.Style) and (fsStrikeOut in normalts.Style) then s := s+'</S>';
  if not (fsUnderline in fnt.Style) and (fsUnderline in normalts.Style) then s := s+'</U>';
  if not (fsItalic    in fnt.Style) and (fsItalic    in normalts.Style) then s := s+'</I>';
  if not (fsBold      in fnt.Style) and (fsBold      in normalts.Style) then s := s+'</B>';
  if (fsBold      in fnt.Style) and not (fsBold      in normalts.Style) then s := s+'<B>';
  if (fsItalic    in fnt.Style) and not (fsItalic    in normalts.Style) then s := s+'<I>';
  if (fsUnderline in fnt.Style) and not (fsUnderline in normalts.Style) then s := s+'<U>';
  if (fsStrikeOut in fnt.Style) and not (fsStrikeOut in normalts.Style) then s := s+'<S>';
  Result := s;
end;
{------------------------------------------------------------------------------}
{ Closes HTML tags opened in RV_HTMLOpenFontTag                                }
function RV_HTMLCloseFontTag(ts: TFontInfo; normalts: TFontInfo; Relative: Boolean):String;
var s: String;
begin
  if ts.VShift < 0 then
    s := s+'</SUB>'
  else if ts.VShift > 0 then
    s := s+'</SUP>';
  if Relative then begin
    if (fsStrikeOut in ts.Style) and not (fsStrikeOut in normalts.Style) then s := s+'</S>';
    if (fsUnderline in ts.Style) and not (fsUnderline in normalts.Style) then s := s+'</U>';
    if (fsItalic    in ts.Style) and not (fsItalic    in normalts.Style) then s := s+'</I>';
    if (fsBold      in ts.Style) and not (fsBold      in normalts.Style) then s := s+'</B>';
    if not (fsBold      in ts.Style) and (fsBold      in normalts.Style) then s := s+'<B>';
    if not (fsItalic    in ts.Style) and (fsItalic    in normalts.Style) then s := s+'<I>';
    if not (fsUnderline in ts.Style) and (fsUnderline in normalts.Style) then s := s+'<U>';
    if not (fsStrikeOut in ts.Style) and (fsStrikeOut in normalts.Style) then s := s+'<S>';
    end
  else begin
    if (fsStrikeOut in ts.Style) then s := s+'</S>';
    if (fsUnderline in ts.Style) then s := s+'</U>';
    if (fsItalic in ts.Style)    then s := s+'</I>';
    if (fsBold in ts.Style)      then s := s+'</B>';
  end;
  if not Relative or (ts.Size<>normalts.Size) or (ts.Color<>normalts.Color) or
    (AnsiCompareText(ts.FontName,normalts.FontName)<>0) then
    s:= s+'</FONT>';
  Result := s;
end;
{------------------------------------------------------------------------------}
{ Closes HTML tags opened in RV_HTMLOpenFontTag2                               }
function RV_HTMLCloseFontTag2(fnt: TFont; normalts: TFontInfo):String;
var s: String;
begin
  if (fsStrikeOut in fnt.Style) and not (fsStrikeOut in normalts.Style) then s := s+'</S>';
  if (fsUnderline in fnt.Style) and not (fsUnderline in normalts.Style) then s := s+'</U>';
  if (fsItalic    in fnt.Style) and not (fsItalic    in normalts.Style) then s := s+'</I>';
  if (fsBold      in fnt.Style) and not (fsBold      in normalts.Style) then s := s+'</B>';
  if not (fsBold      in fnt.Style) and (fsBold      in normalts.Style) then s := s+'<B>';
  if not (fsItalic    in fnt.Style) and (fsItalic    in normalts.Style) then s := s+'<I>';
  if not (fsUnderline in fnt.Style) and (fsUnderline in normalts.Style) then s := s+'<U>';
  if not (fsStrikeOut in fnt.Style) and (fsStrikeOut in normalts.Style) then s := s+'<S>';
  if (fnt.Size<>normalts.Size) or (fnt.Color<>normalts.Color) or
    (AnsiCompareText(fnt.Name,normalts.FontName)<>0) then
    s:= s+'</FONT>';
  Result := s;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEHTML}
{ Converts str to a string for insertion in HTML.
  If SpecialCode=True, does nothing (returns the original string).
  Replaces:
    '&' --> '&amp;'
    '>' --> '&gt;'
    '<' --> '&lt;'
    '  ' (two spaces) --> '&nbsp; '
  Convert leading and trailing spaces to '&nbsp;'.
  If NoEmptyLines=True, and str='', returns '&nbsp;'.                          }
function RV_MakeHTMLStr(const str:String;NoEmptyLines,SpecialCode:Boolean): String;
begin
  Result := str;
  if not SpecialCode then begin
    RV_ReplaceStr2(Result, '&', '&amp;');
    RV_ReplaceStr(Result, '>', '&gt;');
    RV_ReplaceStr(Result, '<', '&lt;');
    RV_ReplaceStr(Result, '  ', '&nbsp; ');
    if Length(Result)>0 then begin
      if Result[Length(Result)]=' ' then begin
        Result[Length(Result)] := '&';
        Result := Result+'nbsp;';
      end;
      if Result[1]=' ' then begin
        Result[1] := ';';
        Result := '&nbsp'+Result;
      end;
      end
    else if NoEmptyLines then
      Result := '&nbsp;';
  end;
end;
{------------------------------------------------------------------------------}
{ For HTML saving. Returns value for '%s' in
  '<META HTTP-EQUIV="Content-Type"  CONTENT="text/html; CHARSET=%s">'          }
{$IFDEF RICHVIEWCBDEF3}
function RV_CharSet2HTMLLang(CharSet: TFontCharset): String;
begin
  case CharSet of // please report me about errors and holes in this table!
    SHIFTJIS_CHARSET:    Result := 'shift_jis';
    {
    HANGEUL_CHARSET:     Result := ?
    JOHAB_CHARSET:       Result := ?
    }
    GB2312_CHARSET:      Result := 'gb2312';
    CHINESEBIG5_CHARSET: Result := 'big5';
    GREEK_CHARSET:       Result := 'Windows-1253';
    TURKISH_CHARSET:     Result := 'Windows-1254';
    VIETNAMESE_CHARSET:  Result := 'Windows-1258';
    HEBREW_CHARSET:      Result := 'Windows-1255';
    ARABIC_CHARSET:      Result := 'Windows-1256';
    BALTIC_CHARSET:      Result := 'Windows-1257';
    RUSSIAN_CHARSET:     Result := 'Windows-1251';
    THAI_CHARSET:        Result := 'Windows-874';
    EASTEUROPE_CHARSET:  Result := 'Windows-1250';
    OEM_CHARSET:         Result := 'ascii';
    else                 Result := '';
  end;
end;
{$ENDIF}
{$ENDIF}
{------------------------------------------------------------------------------}
{ Replaces all characters from the set Replaced with the character Replacer
  in the string s.                                                             }
procedure ReplaceChars(var str: String; Replacer: Char; Replaced: SetOfChar);
var i: Integer;
begin
  for i := 1 to Length(str) do
    if Str[i] in Replaced then
      Str[i] := Replacer;
end;
{==========================  RTF functions  ===================================}
{$IFNDEF RVDONOTUSERTF}
{ Inserts the character Prefix before all characters from the set Prefixed
  in the string s.                                                                 }
procedure PrecedeCharacters(var s: String; Prefix: Char; Prefixed: SetOfChar);
var i: Integer;
begin
  i := 1;
  while i<=Length(s) do begin
    if s[i] in Prefixed then begin
      Insert(Prefix, s, i);
      inc(i);
    end;
    inc(i);
  end;
end;
{------------------------------------------------------------------------------}
{ Writes hexadecimal value of the character s in the 2nd and 3rd characters
  of the string s. s must have length >=3.                                     } 
procedure ToHex_(c: Char; var s: String);
begin
  s[3] := chr(ord(c) mod 16);
  if s[3]<#10 then
    inc(s[3],ord('0'))
  else
    inc(s[3],ord('a')-10);
  s[2] := chr(ord(c) div 16);
  if s[2]<#10 then
    inc(s[2],ord('0'))
  else
    inc(s[2],ord('a')-10);
end;
{------------------------------------------------------------------------------}
{ Replaces all characters with codes <= $19 and >=$80 with their hexadecimal
  codes, prefixed with '\'''                                                   }
procedure RTFReplaceHex(var str: String);
var i: Integer;
    shex: String;
begin
  shex := '''  ';
  for i := Length(str) downto 1 do
    if str[i] in [#0..#$19,#$80..#$FF] then begin
      ToHex_(str[i],shex);
      Insert(shex, str, i+1);
      str[i] := '\';
    end;
end;
{------------------------------------------------------------------------------}
(* Converts s to a string for insertion in RTF.
  If SpecialCode=True, does nothing (returns the original string).
  Adds '\' before '\','}','{'
  Then replaces all characters with codes <= $19 and >=$80 with their
  hexadecimal codes prefixed with '\'''                                       *)
function RVMakeRTFStr(const s:String; SpecialCode: Boolean): String;
begin
  Result := s;
  if not SpecialCode then
    PrecedeCharacters(Result, '\', ['\','}','{']);
  RTFReplaceHex(Result);
end;
{------------------------------------------------------------------------------}
(* Converts s to a string for insertion in RTF as an identifier or something
  like this.
  Replaces '\','}','{' with '_'.
  Then replaces all characters with codes <= $19 and >=$80 with their
  hexadecimal codes prefixed with '\'''                                       *)
function MakeRTFIdentifierStr(const s:String): String;
begin
  Result := s;
  ReplaceChars(Result, '_',  ['\','}','{']);
  RTFReplaceHex(Result);
end;
{------------------------------------------------------------------------------}
{ Converts s to a string for insertion in RTF as a bookmark name.
  Replaces all non alpha-numeric characters with '_'.
  Note: IsCharAlphaNumeric depends on Windows locale.                          }
function MakeRTFBookmarkNameStr(const s:String): String;
var i: Integer;
begin
  Result := s;
  for i := 1 to Length(Result) do
    if not IsCharAlphaNumeric(Result[i]) then
      Result[i] := '_';
  RTFReplaceHex(Result);
end;
{------------------------------------------------------------------------------}
{ Writes "raw Unicode" string s in RTF Stream.
  If SaveAnsi=True, saves ANSI version of text as well (in this case, assumes
  that the default RTF setting "number of ANSI characters representing 1 Unicode
  character"=1. CodePage is used for conversion Unicode to ANSI.
  Unicode characters with codes <128 are saved only as ANSI characters,
  RVMakeRTFStr(...,SpecialCode) is used for them.                              }
procedure RVWriteUnicodeRTFStr(Stream: TStream;
  const s: String; CodePage: TRVCodePage;
  SaveAnsi, SpecialCode: Boolean);
type
    PSmallInt = ^SmallInt;
var i: Integer;
    ws: PSmallInt;
    uni, ansi: String;
    AnsiLen: Integer;
begin
  ws := PSmallInt(PChar(s));
  SetLength(uni,2);
  AnsiLen := 0;
  for i := 0 to (Length(s) div 2)-1 do begin
    if (ws^>0) and (ws^<128) then begin
      ansi := Chr(ws^);
      ansi := RVMakeRTFStr(ansi,SpecialCode);
      if Length(ansi)>0 then
        RVFWrite(Stream, ansi);
      end
    else begin
      if SaveAnsi then begin
        Move(ws^, PChar(uni)^, 2);
        ansi := RVU_UnicodeToAnsi(CodePage, uni);
        AnsiLen := Length(ansi);
        if AnsiLen<>1 then begin
          RVFWrite(Stream, '\uc');
          RVFWrite(Stream, IntToStr(Length(ansi)));
        end;
      end;
      RVFWrite(Stream, Format('\u%d ',[ws^]));
      if SaveAnsi then begin
        ansi := RVMakeRTFStr(ansi,SpecialCode);
        if Length(ansi)>0 then begin
          RVFWrite(Stream, ansi);
        end;
        if AnsiLen<>1 then
          RVFWrite(Stream, '\uc1 ');
      end;
    end;
    inc(PChar(ws),2);
  end;
end;
{$ENDIF}
{======================  Conversion of coordinates  ===========================}
{ Converts X-coordinate from screen resolution to device resolution            }
function RV_XToDevice(X: Integer; const sad: TRVScreenAndDevice): Integer;
begin
  Result := MulDiv(X, sad.ppixDevice, sad.ppixScreen);
end;
{------------------------------------------------------------------------------}
{ Converts Y-coordinate from screen resolution to device resolution            }
function RV_YToDevice(Y: Integer; const sad: TRVScreenAndDevice): Integer;
begin
  Result := MulDiv(Y, sad.ppiyDevice, sad.ppiyScreen);
end;
{------------------------------------------------------------------------------}
{ Converts X-coordinate from device resolution to screen resolution            }
function RV_XToScreen(X: Integer; const sad: TRVScreenAndDevice): Integer;
begin
  Result := MulDiv(X, sad.ppixScreen, sad.ppixDevice);
end;
{------------------------------------------------------------------------------}
{ Converts Y-coordinate from device resolution to screen resolution            }
function RV_YToScreen(Y: Integer; const sad: TRVScreenAndDevice): Integer;
begin
  Result := MulDiv(Y, sad.ppiyScreen, sad.ppiyDevice);
end;
{------------------------------------------------------------------------------}
{ Converts coordinates in rectangle R from device resolution to screen
  resolution                                                                   }
procedure RV_RectToScreen(var R: TRect; const sad: TRVScreenAndDevice);
begin
  R.Left   := MulDiv(R.Left,   sad.ppixScreen, sad.ppixDevice);
  R.Right  := MulDiv(R.Right,  sad.ppixScreen, sad.ppixDevice);
  R.Top    := MulDiv(R.Top,    sad.ppiyScreen, sad.ppiyDevice);
  R.Bottom := MulDiv(R.Bottom, sad.ppiyScreen, sad.ppiyDevice);
end;
{------------------------------------------------------------------------------}
{ Fills the main properties of sad - information about the screen resolution
  and about the device resolution (device is represented by Canvas)            }
procedure RV_InfoAboutSaD(var sad:TRVScreenAndDevice; Canvas: TCanvas);
var screenDC: HDC;
begin
   sad.ppixDevice := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);
   sad.ppiyDevice := GetDeviceCaps(Canvas.Handle, LOGPIXELSY);
   screenDc := CreateCompatibleDC(0);
   sad.ppixScreen := GetDeviceCaps(screenDC, LOGPIXELSX);
   sad.ppiyScreen := GetDeviceCaps(screenDC, LOGPIXELSY);
   DeleteDC(screenDC);
end;
{------------------------------------------------------------------------------}
{ Returns true, if (X,Y) is inside the given rectangle                         }
function RV_PointInRect(X,Y: Integer; Left,Top,Width,Height: Integer): Boolean;
begin
  Result := (X>=Left) and (X<Left+Width) and
            (Y>=Top)  and (Y<Top+Height);
end;
{========================  Graphics & Colors  =================================}
{ This procedure creates a graphic object by its class.
  This is a workaround for D2-D5 bug (private constructor of TGraphic).
  This procedure is assigned to variable RV_CreateGraphics.                    }
function RV_CreateGraphicsDefault(GraphicClass: TGraphicClass): TGraphic;
begin
  {$IFNDEF RICHVIEWDEF6}
  if GraphicClass=TBitmap then
    Result := TBitmap.Create
  else if GraphicClass=TMetafile then
    Result := TMetafile.Create
  else if GraphicClass=TIcon then
    Result := TIcon.Create
  else
  {$ENDIF}
  Result := GraphicClass.Create;
end;
{------------------------------------------------------------------------------}
{ This is a default procedure called after importing external files by links
  in documents (RTF).
  This procedure is assigned to variable RV_AfterImportGraphic.                }
procedure RV_AfterImportGraphicDefault(Graphic: TGraphic);
begin

end;
{------------------------------------------------------------------------------}
{ Returns luminance of Color                                                   }
function RV_GetLuminance(Color: TColor): Integer;
var
  R, G, B: Word;
begin
  Color := ColorToRGB(Color);
  R := Color and $0000FF;
  G := (Color and $00FF00) shr 8;
  B := (Color and $FF0000) shr 16;
  Result := Round(0.3*R + 0.59*G + 0.11*B);
end;
{------------------------------------------------------------------------------}
{ Converts Color to grayscale.                                                 }
function RV_GetGray(Color: TColor): TColor;
var
  R, G, B, C: Word;
begin
  if Color=clNone then begin
    Result := clNone;
    exit;
  end;
  Color := ColorToRGB(Color);
  R := Color and $0000FF;
  G := (Color and $00FF00) shr 8;
  B := (Color and $FF0000) shr 16;

  C := Round(0.3*R + 0.59*G + 0.11*B);
  if C>255 then
    C := 255;
  Result := RGB(C,C,C);
end;
{------------------------------------------------------------------------------}
{ Returns color as it will be printed, if ColorMode=rvcmPrinterColor
  (converts clWindow to clWhite and clWindowText to clBlack)                   }
function RV_GetPrnColor(Color: TColor): TColor;
begin
  case Color of
    clWindowText:
      Result := clBlack;
    clWindow, clBtnHighlight:
      Result := clWhite;
    clBtnShadow:
      Result := clGray;
    clBtnFace, clScrollbar:
      Result := clSilver;
    else
      Result := Color;
  end;
end;
{------------------------------------------------------------------------------}
{ Returns foreground color as it will be printed                               }
function RV_GetColor(Color: TColor; ColorMode: TRVColorMode): TColor;
begin
  if Color=clNone then begin
    Result := clNone;
    exit;
  end;
  case ColorMode of
    rvcmColor:
      Result := Color;
    rvcmPrinterColor:
      Result := RV_GetPrnColor(Color);
    rvcmGrayScale:
      Result := RV_GetGray(RV_GetPrnColor(Color));
    else
      if Color<>clWhite then
        Result := clBlack
      else
        Result := clWhite;
  end;
end;
{------------------------------------------------------------------------------}
{ Returns background color as it will be printed                               }
function RV_GetBackColor(Color: TColor; ColorMode: TRVColorMode): TColor;
begin
  if Color=clNone then begin
    Result := clNone;
    exit;
  end;
  case ColorMode of
    rvcmColor:
      Result := Color;
    rvcmPrinterColor:
      Result := RV_GetPrnColor(Color);
    rvcmGrayScale:
      Result := RV_GetGray(RV_GetPrnColor(Color));
    else
      Result := clWhite;
  end;
end;
{------------------------------------------------------------------------------}
{ Returns true, if graphic object (gr) can contain transparent areas           }
function RV_IsGraphicTransparent(gr: TGraphic): Boolean;
begin
  {$IFDEF RICHVIEWCBDEF3}
  Result := gr.Transparent
  {$ELSE}
  Result := True;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
{ Assigns the palette (PLogPal) to the picture (gr).
  Does something only on D3+, CB3+                                             }
procedure RV_SetPaletteToPicture(gr: TGraphic; PLogPal: PLogPalette);
{$IFDEF RICHVIEWCBDEF3}
var Palette: HPALETTE;
{$ENDIF}
begin
  if PLogPal<>nil then begin
    {$IFNDEF RVDONOTUSEJPEGIMAGE}
    if gr is TJpegImage then
      TJpegImage(gr).PixelFormat := jf8Bit;
    {$ENDIF}
    {$IFDEF RICHVIEWCBDEF3}
      Palette := CreatePalette(PLogPal^);
      gr.Palette := Palette;
      if gr.Palette<>Palette then
        DeleteObject(Palette);
    {$ENDIF}
  end;
end;
{------------------------------------------------------------------------------}
{ Not used, reserved                                                           }
procedure RV_PictureToDeviceOld(Canvas: TCanvas; x,y, width, height: Integer;
  sad:TRVScreenAndDevice; gr: TGraphic; ToScreen: Boolean);
var
  Info: PBitmapInfo;
  InfoSize: DWORD;
  Image: Pointer;
  ImageSize: DWORD;
  Bits: HBITMAP;
  DIBWidth, DIBHeight: Longint;
  PrintWidth, PrintHeight: Longint;
{
  PrintMaxWidth, PrintMaxHeight: Longint;
  RestWidth, RestHeight, PrintRestWidth, PrintRestHeight: Longint;
  ix,iy: Integer;

const MAXWIDTH  = 400;
      MAXHEIGHT = 400;
}
begin
 if width<0 then
   width := gr.Width;
 if height<0 then
   height := gr.Height;
 if ToScreen then begin
   with sad do
     Canvas.StretchDraw(Bounds(x,y,
                               MulDiv(width, ppixDevice, ppixScreen),
                               MulDiv(height, ppiyDevice, ppiyScreen)),
                         gr);
   exit;
 end;
 if gr is TBitmap then begin
     Bits := TBitmap(gr).Handle;
     GetDIBSizes(Bits, InfoSize, ImageSize);
     Info := AllocMem(InfoSize);
     try
        Image := AllocMem(ImageSize);
        try
          GetDIB(Bits, 0, Info^, Image^);
          with Info^.bmiHeader do
            begin
              DIBWidth := biWidth;
              DIBHeight := biHeight;
            end;

           {
            // Some very large pictures are not printed.
            // Below is the experiment to fix (failed)

            PrintMaxWidth := MulDiv(MAXWIDTH,  sad.ppixDevice, sad.ppixScreen);
            PrintMaxHeight:= MulDiv(MAXHEIGHT, sad.ppiyDevice, sad.ppiyScreen);

            for ix := 0 to DIBWidth div MAXWIDTH-1 do
              for iy := 0 to DIBHeight div MAXHEIGHT-1 do
                StretchDIBits(Canvas.Handle,
                              x+ix*PrintMaxWidth, y+iy*PrintMaxHeight,
                              PrintMaxWidth, PrintMaxHeight,
                              ix*MAXWIDTH, iy*MAXHEIGHT, MAXWIDTH, MAXHEIGHT,
                              Image, Info^, DIB_RGB_COLORS, SRCCOPY);

            RestWidth := DIBWidth mod MAXWIDTH;
            RestHeight := DIBHeight mod MAXHEIGHT;
            if (RestWidth<>0) or (RestHeight<>0) then begin
              PrintRestWidth := MulDiv(RestWidth,  sad.ppixDevice, sad.ppixScreen);
              PrintRestHeight:= MulDiv(RestHeight, sad.ppiyDevice, sad.ppiyScreen);

              PrintWidth := MulDiv(DIBWidth, sad.ppixDevice, sad.ppixScreen);
              PrintHeight:= MulDiv(DIBHeight, sad.ppiyDevice, sad.ppiyScreen);

              if RestWidth<>0 then
                for iy := 0 to DIBHeight div MAXHEIGHT-1 do
                  StretchDIBits(Canvas.Handle,
                              x+PrintWidth-PrintRestWidth, y+iy*PrintMaxHeight,
                              PrintRestWidth, PrintMaxHeight,
                              DIBWidth-RestWidth, iy*MAXHEIGHT, RestWidth, MAXHEIGHT,
                              Image, Info^, DIB_RGB_COLORS, SRCCOPY);

              if RestHeight<>0 then
                for ix := 0 to DIBWidth div MAXWIDTH-1 do
                  StretchDIBits(Canvas.Handle,
                              x+ix*PrintMaxWidth, y+PrintHeight-PrintRestHeight,
                              PrintMaxWidth, PrintRestHeight,
                              ix*MAXWIDTH, DIBHeight-RestHeight, MAXWIDTH, RestHeight,
                              Image, Info^, DIB_RGB_COLORS, SRCCOPY);

              if (RestWidth<>0) and (RestHeight<>0) then
                StretchDIBits(Canvas.Handle,
                            x+PrintWidth-PrintRestWidth, y+PrintHeight-PrintRestHeight,
                            PrintRestWidth, PrintRestHeight,
                            DIBWidth-RestWidth, DIBHeight-RestHeight,
                            RestWidth, RestHeight,
                            Image, Info^, DIB_RGB_COLORS, SRCCOPY);
            end;
            }
            PrintWidth := MulDiv(width, sad.ppixDevice, sad.ppixScreen);
            PrintHeight:= MulDiv(height, sad.ppiyDevice, sad.ppiyScreen);

            StretchDIBits(Canvas.Handle, x, y, PrintWidth, PrintHeight, 0, 0,
              DIBWidth, DIBHeight, Image, Info^, DIB_RGB_COLORS, SRCCOPY);
        finally
          FreeMem(Image, ImageSize);
        end;
     finally
        FreeMem(Info, InfoSize);
     end;
 end;
end;
{------------------------------------------------------------------------------}
{ This function is used for printing and drawing bitmaps.
  Since blitting does not aways work between different devices (such as screen
  and printer), this function uses StretchDIBits.
  Canvas - destination canvas;
  x,y - destination coordinates of the left top corner, in Canvas resolution
  width, height - size of source graphics (may not be equal to the actual size
    of image), in screen resolution
  sad - contains information about screen and destination device resolutions.
    The image will be scaled according to it.
  gr - source graphics (must be bitmap)
  ToScreen - true, if the destination device is a screen (no special processing
    is required)                                                               }
procedure RV_PictureToDevice(Canvas: TCanvas; x,y, width, height: Integer;
  sad:TRVScreenAndDevice; gr: TGraphic; ToScreen: Boolean);
var
  Info: PBitmapInfo;
  InfoSize: DWORD;
  Image: Pointer;
  ImageSize: DWORD;
  Bits: HBITMAP;
  DIBWidth, DIBHeight: Longint;
  PrintWidth, PrintHeight: Longint;
  palHalftone, palOrig: HPALETTE;
  nOldStretchBltMode: Integer;
begin
 if width<0 then
   width := gr.Width;
 if height<0 then
   height := gr.Height;
 if ToScreen then begin
   with sad do
     Canvas.StretchDraw(Bounds(x,y,
                               MulDiv(width, ppixDevice, ppixScreen),
                               MulDiv(height, ppiyDevice, ppiyScreen)),
                         gr);
   exit;
 end;

 if gr is TBitmap then begin
     palHalftone := CreateHalftonePalette(Canvas.Handle);
     palOrig := SelectPalette(Canvas.Handle, palHalftone, False);
     RealizePalette(Canvas.Handle);
     nOldStretchBltMode := GetStretchBltMode(Canvas.Handle);
     SetStretchBltMode(Canvas.Handle, HALFTONE);

     try
       Bits := TBitmap(gr).Handle;
       GetDIBSizes(Bits, InfoSize, ImageSize);
       Info := AllocMem(InfoSize);
       try
          Image := AllocMem(ImageSize);
          try
            GetDIB(Bits, 0, Info^, Image^);
            with Info^.bmiHeader do
              begin
                DIBWidth := biWidth;
                DIBHeight := biHeight;
              end;

              PrintWidth := MulDiv(width, sad.ppixDevice, sad.ppixScreen);
              PrintHeight:= MulDiv(height, sad.ppiyDevice, sad.ppiyScreen);

              StretchDIBits(Canvas.Handle, x, y, PrintWidth, PrintHeight, 0, 0,
                DIBWidth, DIBHeight, Image, Info^, DIB_RGB_COLORS, SRCCOPY);
          finally
            FreeMem(Image, ImageSize);
          end;
       finally
          FreeMem(Info, InfoSize);
       end;
     finally
       SetStretchBltMode(Canvas.Handle, nOldStretchBltMode);
       SelectPalette(Canvas.Handle, palOrig, FALSE);
     end;
 end;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RICHVIEWCBDEF3}
function ExtractRelativePath(const BaseName, DestName: string): string;
var
  BasePath, DestPath: string;
  BaseLead, DestLead: PChar;
  BasePtr, DestPtr: PChar;

  function ExtractFilePathNoDrive(const FileName: string): string;
  begin
    Result := ExtractFilePath(FileName);
    Delete(Result, 1, Length(ExtractFileDrive(FileName)));
  end;

  function Next(var Lead: PChar): PChar;
  begin
    Result := Lead;
    if Result = nil then Exit;
    Lead := StrScan(Lead, '\');
    if Lead <> nil then
    begin
      Lead^ := #0;
      Inc(Lead);
    end;
  end;

begin
  if AnsiCompareStr(ExtractFileDrive(BaseName), ExtractFileDrive(DestName))=0 then
  begin
    BasePath := ExtractFilePathNoDrive(BaseName);
    UniqueString(BasePath);
    DestPath := ExtractFilePathNoDrive(DestName);
    UniqueString(DestPath);
    BaseLead := Pointer(BasePath);
    BasePtr := Next(BaseLead);
    DestLead := Pointer(DestPath);
    DestPtr := Next(DestLead);
    while (BasePtr <> nil) and (DestPtr <> nil) and (AnsiCompareStr(BasePtr, DestPtr)=0) do
    begin
      BasePtr := Next(BaseLead);
      DestPtr := Next(DestLead);
    end;
    Result := '';
    while BaseLead <> nil do
    begin
      Result := Result + '..' + '\';             { Do not localize }
      Next(BaseLead);
    end;
    if (DestPtr <> nil) and (DestPtr^ <> #0) then
      Result := Result + DestPtr + '\';
    if DestLead <> nil then
      Result := Result + DestLead;     // destlead already has a trailing backslash
    Result := Result + ExtractFileName(DestName);
  end
  else
    Result := DestName;
end;
{$ENDIF}
{------------------------------------------------------------------------------}

initialization
  RV_CreateGraphics := RV_CreateGraphicsDefault;
  RV_AfterImportGraphic := RV_AfterImportGraphicDefault;
end.
