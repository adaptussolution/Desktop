
{*******************************************************}
{                                                       }
{       RichView                                        }
{       Text strings for RichView.                      }
{       Non-localizable (except from, may be,           }
{       exception messages).                            }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVStr;

{$I RV_Defs.inc}

interface
uses Windows;

{================================ Error messages ==============================}
// These messages are the texts of exception.
// Should not occur in debugged application.
const
  errRVNil               = 'Parameter can''t be NIL';
  errRVNotFormatted      = 'RichView must be formatted for this operation';
  errRVNoMemory          = 'No memory';
  errRVTypesMismatch     = 'Can''t get or set this kind of information for this item';
  errRVUnicode           = 'Can''t perform Unicode operation';
  errRVCPByCP            = 'Calling AddCheckpoint after AddCheckpoint is not allowed in this version';
  errRVNoSuchCP          = 'Invalid checkpoint index - %d';
  errRVTagsTypesMismatch = 'Can''t assign RichView to RichView - tags types mismatch';
  errRVNoSuchCP2         = 'Invalid checkpoint data';
  errRVItemRangeError    = 'Item index is out of bounds';
  errRVCPExists          = 'Checkpoint already exists';
  errStyleIsNotAssigned  = 'Style of printable TRichView component is not assigned';
  errInvalidPageNo       = 'Invalid page number is specified for printing';
  errTRVItemFormattedDataNS =   'This method is not supported for TRVItemFormattedData';
  errRViewerOnly         = 'Not supported in editor';
  errRVUndo              = 'Undo error';
  errRVCP                = 'Checkpoint error';
  errRVItemReg1          = 'Can''t register RichView item type - system is not initialized yet';
  errRVItemReg2          = 'Can''t register RichView item type - this StyleNo is already registered';
  errRVUndoEmpty         = 'Can''t modify empty undo list';
  errRVUndoAdd           = 'Incorrect adding into undo list';
  errRVUndoEmptyBuffer   = 'Undo buffer is empty';
  errNegative            = 'Value must not be negative';
  errRVFDocProp          = 'Invalid RVF format';
  errCaretPosition       = 'Invalid caret position. Please contact the developer, if you can reproduce this problem';
  errPrint               = 'Internal printing error';
{================================ Resource Names ==============================}
  RVRC_ZOOMIN_CURSOR  = 'RV_ZOOMIN_CURSOR';
  RVRC_ZOOMOUT_CURSOR = 'RV_ZOOMOUT_CURSOR';
  RVRC_JUMP_CURSOR    ='RV_JUMP_CURSOR';
  RVRC_FLIPARROW_CURSOR ='RV_FLIPARROW_CURSOR';
{=================================== INI ======================================}
const
  {$IFNDEF RVDONOTUSEINI}

  RVINIFILEYES = 'Yes';
  RVINIFILENO = 'No';
  RVINIFILEYESU = 'YES';
  RVINIFILENOU  = 'NO';
  RVINIUNKNOWN  = '?';

  RVINI_TEXTSTYLECOUNT   = 'FontsCount';
  RVINI_TEXTSTYLEPREFIX  = 'Font%s';
  RVINI_PARASTYLECOUNT   = 'ParasCount';
  RVINI_PARASTYLEPREFIX  = 'Para%s';
  RVINI_LISTSTYLECOUNT   = 'ListCount';
  RVINI_LISTSTYLEPREFIX  = 'List%s';

  RVINI_STANDARD         = 'Standard';

  RVINI_LEFT             = 'Left';
  RVINI_RIGHT            = 'Right';
  RVINI_TOP              = 'Top';
  RVINI_BOTTOM           = 'Bottom';

  RVINI_WIDTH            = 'Width';
  RVINI_STYLE            = 'Style';
  RVINI_INTERNALWIDTH    = 'InternalWidth';
  RVINI_BOFFSPREFIX      = 'Offsets%s';
  RVINI_VISBPREFIX       = 'Visible%s';

  RVINI_SPACEBEFORE      = 'SpaceBefore';
  RVINI_SPACEAFTER       = 'SpaceAfter';  
  RVINI_LEFTINDENT       = 'LeftIndent';
  RVINI_RIGHTIDENT       = 'RightIndent';
  RVINI_FIRSTINDENT      = 'FirstIndent';
  RVINI_LINESPACING      = 'LineSpacing';
  RVINI_LINESPACINGTYPE  = 'LSType';  
  RVINI_NEXTPARANO       = 'NextParaNo';
  RVINI_DEFSTYLENO       = 'DefStyleNo';  
  RVINI_ALIGNMENT        = 'Alignment';
  RVINI_NOWRAP           = 'NoWrap';
  RVINI_READONLY         = 'ReadOnly';
  RVINI_STYLEPROTECT     = 'StyleProtect';
  RVINI_DONOTWANTRETURNS = 'DoNotWantReturns';
  RVINI_KEEPLINESTOGETHER = 'KeepLinesTogether';
  RVINI_KEEPWITHNEXT     = 'KeepWithNext';  

  RVINI_BORDERPREFIX     = 'Border%s';
  RVINI_BACKGROUNDPREFIX = 'Background%s';

  RVINI_STYLENAME        = 'StyleName';
  RVINI_FONTNAME         = 'Name';
  RVINI_JUMP             = 'Jump';
  {$IFDEF RVLANGUAGEPROPERTY}
  RVINI_LANGUAGE         = 'Language';
  {$ENDIF}
  RVINI_JUMPCURSOR       = 'JumpCursor';
  RVINI_SIZE             = 'Size';
  RVINI_COLOR            = 'Color';
  RVINI_BACKCOLOR        = 'BackColor';
  RVINI_HOVERBACKCOLOR   = 'HoverBackColor';
  RVINI_HOVERCOLOR       = 'HoverColor';
  RVINI_CURRENTITEMCOLOR = 'CurItemColor';
  RVINI_CHARSET          = 'Charset';
  RVINI_CHARSCALE        = 'CharScale';
  RVINI_CHARSPACING      = 'CharSpacing';  
  RVINI_BIDIMODE         = 'BiDiMode';  
  RVINI_BOLD             = 'Bold';
  RVINI_UNDERLINE        = 'Underline';
  RVINI_STRIKEOUT        = 'StrikeOut';
  RVINI_ITALIC           = 'Italic';
  RVINI_OVERLINE         = 'Overline';
  RVINI_ALLCAPS          = 'Caps';  
  RVINI_PROTECTION       = 'Protection';
  RVINI_RTFCODE          = 'RTFCode';
  RVINI_HTMLCODE         = 'HTMLCode';
  RVINI_VSHIFT           = 'VShift';
  RVINI_NEXTSTYLENO      = 'NextStyleNo';
  RVINI_BASESTYLENO      = 'BaseStyleNo';
  RVINI_UNICODE          = 'Unicode';
  RVINI_SELECTIONMODE    = 'SelectionMode';
  RVINI_SELECTIONSTYLE   = 'SelectionStyle';  

  RVINI_SELCOLOR         = 'SelColor';
  RVINI_SELTEXTCOLOR     = 'SelTextColor';
  RVINI_ISELCOLOR        = 'ISelColor';
  RVINI_ISELTEXTCOLOR    = 'ISelTextColor';
  RVINI_CPCOLOR          = 'CheckpointColor';
  RVINI_CPEVCOLOR        = 'CheckpointEvColor';
  RVINI_PAGEBREAKCOLOR   = 'PageBreakColor';
  RVINI_SOFTPAGEBREAKCOLOR = 'SoftPageBreakColor';
  RVINI_USESOUND         = 'UseSound';
  RVINI_DEFUNICODESTYLE  = 'DefUnicodeStyle';
  RVINI_DEFCODEPAGE      = 'DefCodePage';
  RVINI_LINESELECTCURSOR = 'LineSelectCursor';

  RVINI_LISTTYPE         = 'ListType';
  RVINI_IMAGEINDEX       = 'ImageIndex';
  RVINI_FORMATSTRING     = 'FormatString';
  RVINI_MARKERINDENT     = 'MarkerIndent';
  RVINI_MARKERALIGNMENT  = 'MarkerAlignment';
  RVINI_FORMATSTRINGW    = 'FormatStringW';
  RVINI_PICTURE          = 'Picture';
  RVINI_GRAPHICCLASS     = 'GraphicClass';

  RVINI_FONT             = 'Font';
  RVINI_LOCONTINUOUS     = 'Continuous';
  RVINI_LOLEVELRESET     = 'LevelReset';
  RVINI_LEVELSCOUNT      = 'LevelsCount';
  RVINI_ONELEVELPREVIEW  = 'OneLevelPreview';
  RVINI_LEVELPREFIX      = 'Lvl%s';

  RVSTYLE_REG = 'RVStyle';
  {$ENDIF}
  RVINI_SINGLESYMBOLS    = 'SingleSymbols';

  RVWCEDIT = 'E'#0'd'#0'i'#0't'#0#0#0;
{================================== Misc ======================================}
const
  RVDEFAULTCHECKPOINTPREFIX = 'RichViewCheckpoint';
  RVDEFAULTCHARACTER        = '?';
  RVDEFAULTDELEMITERS       = ' .;,:(){}"/\';
const RVAddress = 'http://www.trichview.com';
      RVVersion = 'v1.8.14.1';
      RVPalettePage = 'RichView';
      RVNOSTYLEMSG = 'Style is not defined'#13'Create a TRVStyle object and assign it to %s.Style';
      RVFTagEmptyStr = '0';
  { Names of Clipboard formats }
const
  RVFormatName = 'RichView Format';
  RTFormatName = 'Rich Text Format';
  URLFormatName = 'UniformResourceLocator';
  HTMLFormatName = 'HTML Format';
  { Substring in HTML Clipboard contents }
  HTMLClipboardSourceURL = 'SourceURL:';
const
  { Default names of styles, default font names }
  RVDEFSTYLENAME0 = 'Normal text';
  RVDEFSTYLENAME1 = 'Heading';
  RVDEFSTYLENAME2 = 'Subheading';
  RVDEFSTYLENAME3 = 'Keywords';
  RVDEFSTYLENAME4 = 'Jump 1';
  RVDEFSTYLENAME5 = 'Jump 2';
  RVDEFPARASTYLENAME1 = 'Centered';
  RVDEFAULTDESIGNFONT = 'MS Sans Serif';
  RVDEFAULTSTYLEFONT  = 'Arial';
  RVDEFAULTTEXTSTYLENAME = 'Font Style';
  RVDEFAULTPARASTYLENAME = 'Paragraph Style';
  RVDEFAULTLISTSTYLENAME = 'List Style';

  RVLISTLEVELDISPLAYNAME = 'List Level %d';

  { Not strings but Clipboard formats }
var CFRV_RVF, CFRV_RTF, CFRV_HTML, CFRV_URL: Word;

implementation

initialization
  CFRV_RVF := RegisterClipboardFormat(RVFormatName);
  CFRV_RTF := RegisterClipboardFormat(RTFormatName);
  CFRV_URL := RegisterClipboardFormat(URLFormatName);
  CFRV_HTML := RegisterClipboardFormat(HTMLFormatName);  

end.
