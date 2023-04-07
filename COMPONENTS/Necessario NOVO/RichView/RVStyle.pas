
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRVStyle: settings and formatting for           }
{       RichView.                                       }
{       (registered on "RichView" page of               }
{       the Component Palette)                          }
{       Declarations of types used elsewhere.           }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVStyle;

interface
{$R RVStyle}
{$I RV_Defs.inc}
{$IFDEF RICHVIEWDEF6}
{$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  {$IFNDEF RVDONOTUSEINI}
  IniFiles, Registry,
  {$ENDIF}
  {$IFDEF RICHVIEWDEF4}
  ImgList,
  {$ENDIF}
  RVClasses, RVMapWht, RVScroll;

 {---------------------------------------------------------------------------- }
 
const

  { Cursors }
  crJump = 101; // hand point, used as a default value for TRVStyle.JumpCursor
                // property
  crRVFlipArrow = 106; // arrow to top right, used as a default value for
                // TRVStyle.LineSelectCursor property

  { Indices for TRVStyle.TextStyles in its default state }
  rvsNormal     = 0;
  rvsHeading    = 1;
  rvsSubheading = 2;
  rvsKeyword    = 3;
  rvsJump1      = 4;
  rvsJump2      = 5;

  { Standard item types (styles) }
  rvsBreak      = -1; // "break" - horizontal line
  rvsCheckpoint = -2; // "checkpoint" (not an item, for internal use)
  rvsPicture    = -3; // picture (in class inherited from TGraphic)
  rvsHotspot    = -4; // "hotspot": hypertext image from image-list
  rvsComponent  = -5; // component (inherited from TControl)
  rvsBullet     = -6; // "bullet": image from image-list
  rvsBack       = -7; // background (not an item)
  rvsVersionInfo= -8; // version info (not an item, for internal use)
  rvsDocProperty= -9; // document property (not an item, for internal use)
  rvsHotPicture = -10; // "hot picture": hypertext picture
  rvsListMarker = -11; // list marker (paragraph bullet/numbering)

  LAST_DEFAULT_STYLE_NO = rvsJump2;

  { constant used for representing default text style for paragraph }
  rvsDefStyle = MaxInt;

 {---------------------------------------------------------------------------- }

type
  { Structure containing information for resolutions of screen and some
    device, for internal use }
  TRVScreenAndDevice = record
       ppixScreen, ppiyScreen, ppixDevice, ppiyDevice: Integer;
       LeftMargin, RightMargin: Integer;
   end;
  PRVScreenAndDevice= ^TRVScreenAndDevice;

  TParaInfo = class;

  { Saving format }
  TRVSaveFormat = (rvsfText, rvsfHTML, rvsfRTF, rvsfRVF);
  { Loading format }
  TRVLoadFormat = (rvlfText, rvlfHTML { not implemented }, rvlfRTF, rvlfRVF,
    rvlfURL, rvlfOther);

  { Part of RTF file, used in TCustomRichView.OnSaveRTFExtra }
  TRVRTFSaveArea = (
    rv_rtfs_TextStyle, // in character attributes
    rv_rtfs_ParaStyle, // in paragraph attributes
    rv_rtfs_CellProps, // in table cell
    rv_rtfs_RowProps,  // in table row
    rv_rtfs_Doc);      // at the beginning of document
  { Part of HTML file, used in TCustomRichView.OnSaveHTMLExtra }
  TRVHTMLSaveArea = (
    rv_thms_Head,      // <HEAD>*</HEAD>
    rv_thms_BodyAttribute, // <BODY *>
    rv_thms_Body,      // <BODY>*
    rv_thms_End       // *</BODY>
    );  // </P>*

  { Values for TCustomRichView.RTFReadProperties.UnicodeMode }
  TRVReaderUnicode = (
    rvruMixed,         // Use ANSI text as possible, Unicode if necessary
    rvruNoUnicode,     // Use only ANSI text, ignore Unicode in RTF
    rvruOnlyUnicode);  // Use Unicode text, convert all text from RTF to Unicode

  { Values for TCustomRichView.RTFReadProperties.TextStyleMode and
    .ParaStyleMode }
  TRVReaderStyleMode = (
    rvrsUseSpecified,  // Use the specified style
                       // (TCustomRichView.RTFReadProperties.TextStyleNo or
                       // .ParaStyleNo)
    rvrsUseClosest,    // Use the most similar of existing styles, do not
                       // modify collection of styles
    rvrsAddIfNeeded);  // Add new styles if necessary (result is the most
                       // similar to the original RTF

  { Values for TCustomRichView.RVFTextStylesReadMode and
    .RVFParaStylesReadMode }
  TRVFReaderStyleMode = (
    rvf_sIgnore,       // Ignore styles in RVF.
    rvf_sInsertMap,    // RVF loading: styles from RVF replace previous styles.
                       // RVF inserting: use the most similar of existing
                       // styles, do not modify collection of styles.
    rvf_sInsertMerge); // RVF loading: styles from RVF replace previous styles.
                       // RVF inserting: add new styles if necessary

  { Values for TCustomRVPrint.ColorMode }
  TRVColorMode = (
    rvcmColor,         // Colors are not changed
    rvcmPrinterColor,  // Colors are not changed, except for some system colors
                       // converted to black and white
    rvcmGrayScale,     // Colors are converted to shades of gray
    rvcmBlackAndWhite, // Colors are converted to black and white
    rvcmBlackOnWhite); // Black text on white background

  { Code page, for example TRVStyle.DefCodePage }
  TRVCodePage = type cardinal;

  { Background style of item (for example, of table or table cell }
  TRVItemBackgroundStyle = (
    rvbsColor,         // no image
    rvbsStretched,     // stretched image
    rvbsTiled,         // tiled image
    rvbsCentered);     // image in center

  { Reference to information about "checkpoint" }
  TCheckpointData = type Pointer;

  TRVStyle = class;

  { Text properties, used in TRVStyle.OnDrawStyleText }
  TRVTextDrawState = (
    rvtsSelected,      // selected
    rvtsHover,         // under mouse
    rvtsItemStart,     // starting item
    rvtsItemEnd,       // ending item
    rvtsDrawItemStart, // starting drawing item
    rvtsDrawItemEnd,   // ending drawing item
    rvtsControlFocused, // set if TRichView has input focus
    rvtsSpecialCharacters); // display dots in spaces
  TRVTextDrawStates = set of TRVTextDrawState;

  { Type of page break }
  TRVPageBreakType = (
  rvpbSoftPageBreak,   // "soft" page break (created automatically)
  rvpbPageBreak);      // page break set by user

  { Visual style of "break" (horizontal line), not used }
  TRVBreakStyle =
    (rvbsLine,          // line of the given width
     rvbsRectangle,     // rectangle of the given height (border width=1)
     rvbs3d);           // sunken rectangle of the given height (border width=1)

  { Vertical alignment of item }
  TRVVAlign = (
    rvvaBaseline,      // bottom of picture -> baseline
    rvvaMiddle         // center of picture -> baseline
    // may be in future:
    //rvvaAbsTop,      // top of picture    -> top of line
    //rvvaAbsBottom,   // bottom of picture -> bottom of line
    //rvvaAbsMiddle  // center of picture -> center of line
    );

  { Types of paragraph border, TParaInfo.Border.Style }
  TRVBorderStyle = (rvbNone, rvbSingle, rvbDouble, rvbTriple,
    rvbThickInside, rvbThickOutside);

  { Paragraph list type, TRVListLevel.ListType property }
  TRVListType = (rvlstBullet, rvlstPicture, rvlstImageList,
    rvlstDecimal, rvlstLowerAlpha, rvlstUpperAlpha, rvlstLowerRoman,
    rvlstUpperRoman, rvlstImageListCounter
    {$IFNDEF RVDONOTUSEUNICODE}
    ,rvlstUnicodeBullet
    {$ENDIF});

  { Alignment of paragraph marker, TRVListLevel.MarkerAlignment property }
  TRVMarkerAlignment = (rvmaLeft, rvmaRight, rvmaCenter);

  { Options for paragraph bullets/numbering, TRVListLevel.Options }
  TRVListLevelOption = (
    rvloContinuous, // (reserved for future use, must always be set)
    rvloLevelReset, // Reset numbering on each level - normal behavior
    rvloLegalStyleNumbering); // Use decimal representation of numbering of
                    // other levels
  TRVListLevelOptions = set of TRVListLevelOption;

  TRVMarkerFormatString = type String;

  {$IFNDEF RVDONOTUSEUNICODE}
  {$IFDEF RICHVIEWCBDEF3}
  TRVMarkerFormatStringW = type WideString;
  {$ENDIF}
  {$ENDIF}

  { Options for saving/loading RVF files/streams }
  TRVFOption = (
    rvfoSavePicturesBody, // Save pictures (if not set - images are requested
                          // in event)
    rvfoSaveControlsBody, // Save controls (if not set - controls are
                          // requested in event)
    rvfoIgnoreUnknownPicFmt, // Ignore pictures of unknown types
                             // (if not set - report error)
    rvfoIgnoreUnknownCtrls,  // Ignore controls of unknown types
                             // (if not set - report error)
    rvfoConvUnknownStylesToZero, // Convert unknown text, paragraph or list
                                 // styles to 0-th styke (if not set - report error)
    rvfoConvLargeImageIdxToZero, // Convert too large image indices in "bullets"
                                 // and "hotspots" to 0 (if not set - report error)
    rvfoSaveBinary,       // Binary RVF saving mode
    rvfoUseStyleNames,    // (Obsolete)
    rvfoSaveBack,         // Save background
    rvfoLoadBack,         // Load background
    rvfoSaveTextStyles,   // Save collection of text styles (RVStyle.TextStyles)
    rvfoSaveParaStyles,   // Save collections of paragraph and list styles
                          // (RVStyle.ParaStyles and .ListStyles)
    rvfoSaveLayout,       // Save layout properties (margins, etc.)
    rvfoLoadLayout,       // Load layout properties
    rvfoSaveDocProperties,// Save DocProperties stringlist
    rvfoLoadDocProperties // Load DocProperties stringlist
    );
  TRVFOptions = set of TRVFOption;

  { Operation, see TRichView.OnProgress event. }
  TRVLongOperation = (
    rvloLoading,          // Saving
    rvloConverting);      // Not used by the component. Allows to call


  { Operation progress, see TRichView.OnProgress event. }
  TRVProgressStage = (
    rvpstgStarting,       // The operation is about to begin
    rvpstgRunning,        // The operation is underway and has not yet completed
    rvpstgEnding);        // The operation has just completed


  { Warnings for loading RVF files/streams }
  TRVFWarning = (
    rvfwUnknownPicFmt, // Picture of unknown/unregistered type (use RegisterClass)
    rvfwUnknownCtrls,  // Control of unknown/unregistered type (use RegisterClass)
    rvfwConvUnknownStyles, // Invalid index of text/paragraph/list style
    rvfwConvLargeImageIdx, // Invalid image index in "bullet" or "hotspot"
    rvfwConvToUnicode,     // Mismatched Unicode/ANSI type of text
                           // (was converted to Unicode)
    rvfwConvFromUnicode,   // Mismatched Unicode/ANSI type of text
                           // (was converted to ANSI)
    rvfwInvalidPicture, // Invalid picture data (was replaced with
                         // RVStyle.InvalidPicture.Graphic)
    rvfwUnknownStyleProperties); // Unknown properties of items in the collections
                           // of text/paragraph/lists. Probably, RVF was saved with
                           // newer version of component 
  TRVFWarnings = set of TRVFWarning;

  { Action with controls inserted in TRichView, parameter of
    TCustomRichView.OnControlAction }
  TRVControlAction = (
    rvcaAfterRVFLoad,      // Control is loaded from RVF file or stream
    rvcaDestroy,           // Control is being destroyed (in TCustomRichView)
    rvcaMoveToUndoList,    // Control is moved from editor to undo/redo buffer
    rvcaMoveFromUndoList,  // Control is moved from undo/redo buffer back to editor
    rvcaDestroyInUndoList, // Control is being destroyed (in undo buffer)
    rvcaBeforeRVFSave,     // Before saving control to RVF file or stream
    rvcaAfterRVFSave);     // After saving control to RVF file or stream

  { Action with items, parameter of TCustomRichView.OnItemAction }
  TRVItemAction = (
    rviaInserting,         // Before insertion in TCustomRichView
    rviaInserted,          // After insertion in TCustomRichView
    rviaTextModifying,     // Text of item is being modified as a result of
                           // editing operation
    rviaDestroying,        // Item is being destroyed
    rviaMovingToUndoList); // Item is moved to undo/redo buffer

  { Options for protected text, TFontInfo.Protection property }
  TRVProtectOption = (
    rvprStyleProtect,   // Protect from ApplyTextStyle
    rvprModifyProtect,  // Protect from text modifying (but not from
                        // deletion as a whole)
    rvprDeleteProtect,  // Protect from deletion as a whole
    rvprConcateProtect, // Protect from concatenation with adjacent text
    rvprRVFInsertProtect, // Protect from insertion from RVF
    rvprDoNotAutoSwitch, // TCustomRichViewEdit.CurTextStyleNo will never
                        // be set to the text of this style automatically
    rvprParaStartProtect, // (See the help file)
    rvprSticking,       // Disallows inserting between protected text items
    rvprStickToTop,     // If this text is at the beginning, disallow inserting
                        // before it
    rvprStickToBottom); // If this text is at the end, disallow inserting
                        // after it
  TRVProtectOptions = set of TRVProtectOption;

  { Options for paragraph styles, TParaInfo.Options property }
  TRVParaOption = (
    rvpaoNoWrap,         // Disallow word wrapping
    rvpaoReadOnly,       // Disallow changes in paragraph (but it can be deleted
                         // as a whole
    rvpaoStyleProtect,   // Protect from ApplyParaStyle
    rvpaoDoNotWantReturns, // Ignore ENTER key
    rvpaoKeepLinesTogether, // Print the whole paragraph on one page, if possible
    rvpaoKeepWithNext); // Print this paragraph on the same page as the next one
  TRVParaOptions = set of TRVParaOption;

  { Options for text styles, TTextInfo.Options property }
  TRVTextOption = (
    rvteoHTMLCode,  // Save text to HTML as is
    rvteoRTFCode);  // Save text to RTF as is
  TRVTextOptions = set of TRVTextOption;

  { Options for saving HTML files, TCustomRichView.SaveHTML and SaveHTMLEx methods }
  TRVSaveOption = (
    rvsoOverrideImages, // Overwrite image files (if not set - use unique)
    rvsoFirstOnly,      // Save only heading part of HTML
    rvsoMiddleOnly,     // Save only middle part of HTML (document itself)
    rvsoLastOnly,       // Save only ending part of HTML
    rvsoDefault0Style,  // Do not save properties for the 0-th text style
                        //   (only for SaveHTML)
    rvsoNoHypertextImageBorders, // Supress borders for hypertext images
    rvsoImageSizes,     // Write image size
    rvsoForceNonTextCSS,// Always use CSS for non-text items
    rvsoUseCheckpointsNames, // Use "checkpoint names" instead of indices
    rvsoMarkersAsText,  // Save paragraph bullets/numbering without <UL>/<OL>
    rvsoInlineCSS,      // Write CSS directly in <P> and <SPAN> tags
                        //   (only for SaveHTMLEx)
    rvsoNoDefCSSStyle,  // Use named CSS for all text styles, even for
                        //   TextStyles[0] (by default, properties of
                        //   TextStyles[0] are assigned to BODY and TABLE).
                        //   This option generates larger HTML (not recommended).
                        //   (only for SaveHTMLEx)
    rvsoUseItemImageFileNames); // If set, images having specified
                        //   (in extra string properties) file names will not
                        //   be saved, but their file names will be written
                        //   in HTML (relative to the HTML file path)
  TRVSaveOptions = set of TRVSaveOption;

  { Options for saving RTF files, TCustomRichView.RTFOptions }
  TRVRTFOption = (
    rvrtfSaveStyleSheet,    // Save style sheet
    rvrtfDuplicateUnicode,  // Save optional ANSI representation of Unicode text
    rvrtfSaveEMFAsWMF,      // Save 32-bit metafiles as 16-bit metafiles
                            // (more compatible RTF)
    rvrtfSaveJpegAsJpeg,    // Save TJpegImage as jpeg (less compatible RTF)
    rvrtfSaveBitmapDefault, // Save "exotic" picture types as bitmaps (if not
                            // set - as metafiles)
    rvrtfSaveEMFDefault);   // Save "exotic" picture types as 32-bit metafiles
  TRVRTFOptions = set of TRVRTFOption;

  { Advanced font styles, TFontInfo.StyleEx }
  TRVFontStyle = (
    rvfsOverline,   // Line above text
    rvfsAllCaps    // All capitals
    {$IFDEF RVTEXTFOOTNOTES}
    , rvfsFootnotes
    {$ENDIF}
    );
  TRVFontStyles = set of TRVFontStyle;

  { Paragraph alignment, TParaInfo.Alignment }
  TRVAlignment = (rvaLeft, rvaRight, rvaCenter, rvaJustify);

{$IFNDEF RVDONOTUSEINI}
{$IFDEF RICHVIEWDEF4}
  TRVIniFile = TCustomIniFile;
{$ELSE}
  TRVIniFile = TIniFile;
{$ENDIF}
{$ENDIF}

  { Parameters of TRVStyle.SaveCSS }
  TRVSaveCSSOption = (
    rvcssOnlyDifference,      // do not use
    rvcssIgnoreLeftAlignment, // do not use
    rvcssNoDefCSSStyle);      // see rvsoNoDefCSSStyle
  TRVSaveCSSOptions = set of TRVSaveCSSOption;

  { Enumeration of properties of TFontInfo }
  TRVFontInfoProperty = (
    rvfiFontName, rvfiSize, rvfiCharset, rvfiUnicode,
    rvfiBold, rvfiItalic, rvfiUnderline, rvfiStrikeout,
    rvfiOverline, rvfiAllCaps,
    {$IFDEF RVTEXTFOOTNOTES}
    rvfiFootnotes,
    {$ENDIF}
    rvfiVShift, rvfiColor, rvfiBackColor,
    rvfiJump, rvfiHoverBackColor, rvfiHoverColor, rvfiJumpCursor,
    rvfiNextStyleNo, rvfiProtection, rvfiCharScale, rvfiBaseStyleNo,
    rvfiBiDiMode, rvfiCharSpacing, rvfiHTMLCode, rvfiRTFCode
    {$IFDEF RVLANGUAGEPROPERTY}
    , rvfiLanguage
    {$ENDIF});

  { Enumeration of properies of TParaInfo }
  TRVParaInfoProperty = (
    rvpiFirstIndent, rvpiLeftIndent, rvpiRightIndent,
    rvpiSpaceBefore, rvpiSpaceAfter, rvpiAlignment,
    rvpiNextParaNo, rvpiDefStyleNo, rvpiLineSpacing, rvpiLineSpacingType,
    rvpiBackground_Color,
    rvpiBackground_BO_Left, rvpiBackground_BO_Top,
    rvpiBackground_BO_Right, rvpiBackground_BO_Bottom,
    rvpiBorder_Color, rvpiBorder_Style,
    rvpiBorder_Width, rvpiBorder_InternalWidth,
    rvpiBorder_BO_Left, rvpiBorder_BO_Top,
    rvpiBorder_BO_Right, rvpiBorder_BO_Bottom,
    rvpiBorder_Vis_Left, rvpiBorder_Vis_Top,
    rvpiBorder_Vis_Right, rvpiBorder_Vis_Bottom,
    rvpiNoWrap, rvpiReadOnly, rvpiStyleProtect, rvpiDoNotWantReturns,
    rvpiKeepLinesTogether, rvpiKeepWithNext,
    rvpiBiDiMode);

  TRVFontInfoProperties = set of TRVFontInfoProperty;
  TRVParaInfoProperties = set of TRVParaInfoProperty;

  { Type of line spacing, TParaInfo.LineSpacingType }
  TRVLineSpacingType = (
    rvlsPercent,        // TParaInfo.LineSpacing specifies spacing in percents
    rvlsSpaceBetween    // ... in pixels
    {, rvlsAtLeast});

  { Mode of merging collections of styles, for internal use }
  TRVStyleMergeMode = (
    rvs_merge_SmartMerge, // Reuse styles, add if necessary
    rvs_merge_Map,        // Use the most similar of existing styles. Do not add styles
    rvs_merge_Append);    // Append one collection to another

  { Text selection mode }
  TRVSelectionMode = (
    rvsmChar,       // Select by characters
    rvsmWord,       // Select by word
    rvsmParagraph); // Select by paragraphs

  { Text selection style }
  TRVSelectionStyle = (
    rvssItems,      // Highlighted items
    rvssLines);     // Highlighted lines (like in Word). Not supported,
                    // if BiDiMode<>rvbdUnspecified

  { --------------------- Types for events of TRVStyle ----------------------- }
  TRVDrawTextBackEvent = procedure (Sender: TRVStyle; Canvas: TCanvas;
    StyleNo: Integer; Left, Top, Width, Height: Integer;
    DrawState: TRVTextDrawStates; var DoDefault: Boolean) of object;

  TRVApplyStyleEvent = procedure (Sender: TRVStyle; Canvas: TCanvas;
    StyleNo: Integer; var DoDefault: Boolean) of object;

  TRVApplyStyleColorEvent = procedure (Sender: TRVStyle; Canvas: TCanvas;
    StyleNo: Integer; DrawState: TRVTextDrawStates;
    var DoDefault: Boolean) of object;

  TRVDrawStyleTextEvent = procedure (Sender: TRVStyle; const s: String;
    Canvas: TCanvas; StyleNo: Integer; SpaceBefore,
    Left, Top, Width, Height: Integer;
    DrawState: TRVTextDrawStates; var DoDefault: Boolean) of object;

  TRVStyleHoverSensitiveEvent = procedure (Sender: TRVStyle; StyleNo: Integer;
    var Sensitive: Boolean) of object;

  TRVDrawCheckpointEvent = procedure (Sender: TRVStyle; Canvas: TCanvas;
    X,Y, ItemNo, XShift: Integer; RaiseEvent: Boolean; Control: TControl;
    var DoDefault: Boolean) of object;

  TRVDrawPageBreakEvent = procedure (Sender: TRVStyle; Canvas: TCanvas;
    Y, XShift: Integer; PageBreakType: TRVPageBreakType; Control: TControl;
    var DoDefault: Boolean) of object;

  TRVDrawParaRectEvent = procedure (Sender: TRVStyle; Canvas: TCanvas;
    ParaNo: Integer; ARect: TRect; var DoDefault: Boolean) of object;

  { ---------------------------------------------------------------------------
    TCustomRVInfo: ancestor class for text, paragraph and list styles
    (TFontInfo, TParaInfo, TRVListInfo)
    Properties:
    - BaseStyleNo - index of base style (reserved for future use)
    - StyleName   - name of style
    - Standard    - if True, this is a "real" style; if False, this style
                    represents formatting and can be deleted by
                    TCustomRichView.DeleteUnusedStyles
  }
  TCustomRVInfo = class(TCollectionItem)
    private
      FBaseStyleNo: Integer;
      FName: String;
      FStandard: Boolean;
    protected
      function IsSimpleEqual(Value: TCustomRVInfo; IgnoreReferences: Boolean;
        IgnoreID: Boolean{$IFDEF RICHVIEWDEF4}=True{$ENDIF}): Boolean; dynamic; abstract;
      function IsSimpleEqualEx(Value: TCustomRVInfo; Mapping: TRVIntegerList): Boolean; dynamic; abstract;
      function SimilarityValue(Value: TCustomRVInfo): Integer; dynamic; abstract;
    public
      constructor Create(Collection: TCollection); override;
      procedure Assign(Source: TPersistent); override;
      {$IFDEF RICHVIEWCBDEF3}
      function GetDisplayName: String; override;
      {$ENDIF}
      {$IFNDEF RVDONOTUSEINI}
      procedure SaveToINI(ini: TRVIniFile; const Section, fs: String);
      procedure LoadFromINI(ini: TRVIniFile; const Section, fs, DefName: String);
      {$ENDIF}
    published
      property BaseStyleNo: Integer read FBaseStyleNo write FBaseStyleNo default -1;
      property StyleName: String    read FName        write FName;
      property Standard: Boolean    read FStandard    write FStandard default True;
  end;
  { ---------------------------------------------------------------------------
    TFontInfo: text style, item in the collection TRVStyle.TextStyles
    (collection type is TFontInfos)
    Properties:
    - Charset, FontName, Size, Style, Color - see properties for TFont
      (FontName = Name)
    - VShift - vertical offet of text, % of text height.
      Positive values - up, negative values - down.
    - BackColor - color of text background, clNone for transparent
    - HoverBackColor - color of text background under mouse (only for hypertext),
      clNone for no effect
    - HoverColor - color of text under mouse (only for hypertext), clNone to
      use TRVStyle.HoverColor
    -  StyleEx - advanced visual text styles, see TRVFontStyles
    - Jump - if true, this text is a hypertext
    - JumpCursor - cursor for hypertext
    - CharScale - horizontal character scale value, %
    - CharSpacing - spacing between characters, pixels
    - NextStyleNo - index of text style for the next paragraph, if user
      pressed ENTER at the end of paragraph of this style. -1 for the same style
    - BiDiMode - bi-di mode of text
    - Unicode - if False, this text has ANSI encoding. If True, it is Unicode
    - Language - text language (enabled by RVLANGUAGEPROPERTY compiler define)
    - Protection - protection options, see TRVProtectOptions
    - Options - see TRVTextOptions
  }
  TFontInfo = class(TCustomRVInfo)
  private
    { Private declarations }
    FBiDiMode: TRVBiDiMode;
    FJump: Boolean;
    FJumpCursor: TCursor;
    FFontName: TFontName;
    FSize: Integer;
    FColor, FBackColor, FHoverColor, FHoverBackColor: TColor;
    FStyle: TFontStyles;
    FStyleEx: TRVFontStyles;
    FVShift: Integer;
    FNextStyleNo: Integer;
    {$IFDEF RICHVIEWCBDEF3}
    FCharset: TFontCharset;
    {$ENDIF}
    {$IFDEF RVLANGUAGEPROPERTY}
    FLanguage: Cardinal;
    {$ENDIF}
    {$IFNDEF RVDONOTUSEUNICODE}
    FUnicode: Boolean;
    {$ENDIF}
    FProtection: TRVProtectOptions;
    FOptions: TRVTextOptions;
    FCharScale, FCharSpacing: Integer;
    {$IFDEF RVTEXTFOOTNOTES}
    FFootNote: String;
    {$ENDIF}
    procedure SingleSymbolsReader(reader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler);override;
    function IsSimpleEqual(Value: TCustomRVInfo; IgnoreReferences: Boolean;
      IgnoreID: Boolean{$IFDEF RICHVIEWDEF4}=True{$ENDIF}): Boolean; override;
    function IsSimpleEqualEx(Value: TCustomRVInfo; Mapping: TRVIntegerList): Boolean; override;
    function SimilarityValue(Value: TCustomRVInfo): Integer; override;
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure AssignToLogFont(var LogFont: TLogFont; Canvas: TCanvas);
    procedure Apply(Canvas: TCanvas; DefBiDiMode: TRVBiDiMode);
    procedure ApplyColor(Canvas: TCanvas; RVStyle: TRVStyle;
      DrawState: TRVTextDrawStates; Printing: Boolean; ColorMode: TRVColorMode);
    procedure Draw(const s: String; Canvas: TCanvas; ThisStyleNo: Integer;
      SpaceBefore, Left, Top, Width, Height: Integer; RVStyle: TRVStyle;
      DrawState: TRVTextDrawStates; Printing, PreviewCorrection: Boolean;
      ColorMode: TRVColorMode; DefBiDiMode: TRVBiDiMode);
    procedure DrawVertical(const s: String; Canvas: TCanvas; // <-  do not ask me what is it :)
      ThisStyleNo: Integer; SpaceBefore, Left, Top, Width, Height: Integer;
      RVStyle: TRVStyle; DrawState: TRVTextDrawStates);
    function IsEqual(Value: TFontInfo; IgnoreList: TRVFontInfoProperties): Boolean; dynamic;
    {$IFNDEF RVDONOTUSEINI}
    procedure SaveToINI(ini: TRVIniFile; const Section, fs: String); dynamic;
    procedure LoadFromINI(ini: TRVIniFile; const Section, fs: String;
      JumpByDefault: Boolean; DefJumpCursor: TCursor); dynamic;
    {$ENDIF}
    procedure SaveCSSToStream(Stream: TStream; BaseStyle: TFontInfo;
      Multiline: Boolean);
   published
    { Published declarations }
    {$IFDEF RICHVIEWCBDEF3}
    property Charset: TFontCharset  read FCharset  write FCharset     default DEFAULT_CHARSET;
    {$ENDIF}
    property FontName:  TFontName   read FFontName   write FFontName;
    property Size:      Integer     read FSize       write FSize       default 10;
    property Style:     TFontStyles read FStyle      write FStyle      default [];
    property VShift:    Integer     read FVShift     write FVShift     default 0;
    property Color:     TColor      read FColor      write FColor      default clWindowText;
    property BackColor: TColor      read FBackColor  write FBackColor  default clNone;
    property HoverBackColor: TColor read FHoverBackColor write FHoverBackColor default clNone;
    property HoverColor: TColor     read FHoverColor write FHoverColor default clNone;
    property StyleEx:   TRVFontStyles read FStyleEx  write FStyleEx    default [];
    property Jump:       Boolean    read FJump       write FJump       default False;
    property JumpCursor: TCursor    read FJumpCursor write FJumpCursor default crJump;
    property CharScale: Integer     read FCharScale  write FCharScale  default 100;
    property CharSpacing: Integer   read FCharSpacing write FCharSpacing default 0;
    property NextStyleNo: Integer   read FNextStyleNo write FNextStyleNo default -1;
    property BiDiMode: TRVBiDiMode  read FBiDiMode   write FBiDiMode   default rvbdUnspecified;
    {$IFNDEF RVDONOTUSEUNICODE}
    property Unicode: Boolean       read FUnicode    write FUnicode    default False;
    {$ENDIF}
    {$IFDEF RVLANGUAGEPROPERTY}
    property Language: Cardinal     read FLanguage   write FLanguage  default 0;
    {$ENDIF}
    property Protection: TRVProtectOptions read FProtection write FProtection default [];
    property Options: TRVTextOptions read FOptions write FOptions default [];
    {$IFDEF RVTEXTFOOTNOTES}
    property FootNote: String       read FFootNote   write FFootNote;
    {$ENDIF}
  end;
  { ---------------------------------------------------------------------------
    TCustomRVInfos: ancestor class for collections of styles
    (TFontInfos, TParaInfos, TRVListInfos)
  }
  TCustomRVInfos = class (TCollection)
  protected
    FOwner: TPersistent;
  public
    constructor Create(ItemClass: TCollectionItemClass; Owner: TPersistent);
    {$IFDEF RICHVIEWCBDEF3}
    function GetOwner: TPersistent; override;
    {$ENDIF}
    procedure AssignTo(Dest: TPersistent); override;
    procedure MergeWith(Styles:TCustomRVInfos; Mode:TRVStyleMergeMode;
      Mapping: TRVIntegerList; TextStyleMapping: TRVIntegerList);
  end;
  {----------------------------------------------------------------------------
    TFontInfos: collection of text styles (of TFontInfo), TRVStyle.TextStyles
    Properties:
    - Items[] - items
    - InvalidItem - returned when accessing item with invalid index
  }
  TFontInfos = class (TCustomRVInfos)
  private
    FInvalidItem: TFontInfo;
    function GetItem(Index: Integer): TFontInfo;
    procedure SetItem(Index: Integer; Value: TFontInfo);
    function GetInvalidItem: TFontInfo;
    procedure SetInvalidItem(const Value: TFontInfo);
  public
    PixelsPerInch: Integer;
    destructor Destroy; override;
    {$IFDEF RICHVIEWCBDEF3}
    function FindStyleWithCharset(BaseStyle: Integer;
      Charset: TFontCharset): Integer;
    {$ENDIF}
    function FindStyleWithFontStyle(BaseStyle: Integer; Value,
      Mask: TFontStyles): Integer;
    function FindStyleWithFontSize(BaseStyle: Integer; Size: Integer): Integer;
    function FindStyleWithColor(BaseStyle: Integer;
      Color, BackColor: TColor): Integer;
    function FindStyleWithFontName(BaseStyle: Integer;
      const FontName: TFontName): Integer;
    function FindSuchStyle(BaseStyle: Integer; Style: TFontInfo;
      Mask: TRVFontInfoProperties): Integer;
    function FindStyleWithFont(BaseStyle: Integer; Font: TFont): Integer;
    function Add: TFontInfo;
    function AddFont(Name: TFontName; Size: Integer; Color, BackColor: TColor;
      Style:TFontStyles): TFontInfo;
    {$IFDEF RICHVIEWCBDEF3}
    function AddFontEx(Name: TFontName; Size: Integer; Color, BackColor: TColor;
      Style:TFontStyles; Charset: TFontCharset): TFontInfo;
    {$ENDIF}
    {$IFNDEF RVDONOTUSEINI}
    procedure SaveToINI(ini: TRVIniFile; const Section: String);
    procedure LoadFromINI(ini: TRVIniFile; const Section: String;
      DefJumpCursor: TCursor); 
    {$ENDIF}
    property Items[Index: Integer]: TFontInfo read GetItem write SetItem; default;
    property InvalidItem: TFontInfo read GetInvalidItem write SetInvalidItem;
  end;
  { ---------------------------------------------------------------------------
    TRVRect: rectangle.
    Properties:
    - Left, Top, Right, Bottom
  }
  TRVRect = class (TPersistent)
    private
      FTop: Integer;
      FLeft: Integer;
      FRight: Integer;
      FBottom: Integer;
      function IsEqualEx(Value: TRVRect; IgnL,IgnT,IgnR,IgnB: Boolean): Boolean;
      function SimilarityValue(Value: TRVRect; Weight: Integer): Integer;
    public
      procedure Assign(Source: TPersistent); override;
      procedure SetAll(Value: Integer);
      procedure InflateRect(var Rect: TRect);
      procedure InflateRectSaD(var Rect: TRect; const sad: TRVScreenAndDevice);
      procedure AssignToRect(var Rect: TRect);
      procedure AssignToRectIfGreater(var Rect: TRect);
      function IsEqual(Value: TRVRect): Boolean;
      {$IFNDEF RVDONOTUSEINI}
      procedure SaveToINI(ini: TRVIniFile; const Section, fs: String); 
      procedure LoadFromINI(ini: TRVIniFile; const Section, fs: String);
      {$ENDIF}
    published
      property Left: Integer   read FLeft   write FLeft   default 0;
      property Right: Integer  read FRight  write FRight  default 0;
      property Top: Integer    read FTop    write FTop    default 0;
      property Bottom: Integer read FBottom write FBottom default 0;
  end;
  { ---------------------------------------------------------------------------
    TRVBooleanRect: 4 boolean values
    Properties:
    - Left, Top, Right, Bottom
  }
  TRVBooleanRect = class (TPersistent)
    private
      FTop: Boolean;
      FLeft: Boolean;
      FRight: Boolean;
      FBottom: Boolean;
      function IsEqualEx(Value: TRVBooleanRect; IgnL,IgnT,IgnR,IgnB: Boolean): Boolean;
    public
      constructor Create(DefValue: Boolean);
      procedure SetAll(Value: Boolean);
      procedure SetValues(ALeft, ATop, ARight, ABottom: Boolean);
      procedure Assign(Source: TPersistent); override;
      function IsEqual(Value: TRVBooleanRect): Boolean;
      function IsEqual2(ALeft, ATop, ARight, ABottom: Boolean): Boolean;
      function IsAllEqual(Value: Boolean): Boolean;
      {$IFNDEF RVDONOTUSEINI}
      procedure SaveToINI(ini: TRVIniFile; const Section, fs: String);
      procedure LoadFromINI(ini: TRVIniFile; const Section, fs: String);
      {$ENDIF}
    published
      property Left: Boolean   read FLeft   write FLeft   default True;
      property Right: Boolean  read FRight  write FRight  default True;
      property Top: Boolean    read FTop    write FTop    default True;
      property Bottom: Boolean read FBottom write FBottom default True;
  end;
  { ---------------------------------------------------------------------------
    TRVBorder: paragraph border
    Properties:
    - Width - [thin] line width
    - InternalWidth - spacing between border lines (for double or triple borders)
    - Color - border color
    - Style - border type, see TRVBorderStyle
    - VisibleBorders - turn on/off border sides
    - BorderOffsets - padding between text and border
  }
  TRVBorder = class (TPersistent)
    private
      FColor: TColor;
      FStyle: TRVBorderStyle;
      FWidth: Integer;
      FInternalWidth: Integer;
      FVisibleBorders: TRVBooleanRect;
      FBorderOffsets: TRVRect;
      procedure SetBorderOffsets(const Value: TRVRect);
      procedure SetVisibleBorders(const Value: TRVBooleanRect);
      function SimilarityValue(Value: TRVBorder): Integer;
    protected
      procedure DoDraw(Rect: TRect; Canvas: TCanvas;
        Width, InternalWidth, OnePixelWidth: Integer;
        ColorMode: TRVColorMode);
    public
      constructor Create;
      destructor Destroy; override;
      procedure Draw(Rect: TRect; Canvas: TCanvas);
      procedure DrawSaD(Rect: TRect; Canvas: TCanvas; const sad: TRVScreenAndDevice;
        ColorMode: TRVColorMode);
      procedure Assign(Source: TPersistent); override;
      function IsEqual(Value: TRVBorder): Boolean;
      function IsEqual_Para(Value: TRVBorder; IgnoreList: TRVParaInfoProperties): Boolean;
      {$IFNDEF RVDONOTUSEINI}
      procedure SaveToINI(ini: TRVIniFile; const Section, fs: String);
      procedure LoadFromINI(ini: TRVIniFile; const Section, fs: String);
      {$ENDIF}
      function GetTotalWidth: Integer;
    published
      property Width:         Integer         read FWidth          write FWidth         default 1;
      property InternalWidth: Integer         read FInternalWidth  write FInternalWidth default 1;
      property Color:         TColor          read FColor          write FColor         default clWindowText;
      property Style:         TRVBorderStyle  read FStyle          write FStyle         default rvbNone;
      property VisibleBorders: TRVBooleanRect read FVisibleBorders write SetVisibleBorders;
      property BorderOffsets: TRVRect read FBorderOffsets write SetBorderOffsets;
  end;
  { ---------------------------------------------------------------------------
    TRVBackgroundRect: properties for paragraph background
    Properties:
    - Color - background color (clNone for transparent)
    - BorderOffsets - padding (widths of colored area around paragraph text)
  }
  TRVBackgroundRect = class (TPersistent)
  private
      FBorderOffsets: TRVRect;
      FColor: TColor;
      procedure SetBorderOffsets(const Value: TRVRect);
      function SimilarityValue(Value: TRVBackgroundRect): Integer;
    public
      constructor Create;
      destructor Destroy; override;
      procedure Assign(Source: TPersistent); override;
      procedure PrepareDraw(var Rect: TRect);
      procedure PrepareDrawSaD(var Rect: TRect; const sad: TRVScreenAndDevice);
      procedure Draw(Rect: TRect; Canvas: TCanvas; Printing: Boolean; ColorMode: TRVColorMode);
      function IsEqual(Value: TRVBackgroundRect): Boolean;
      function IsEqual_Para(Value: TRVBackgroundRect; IgnoreList: TRVParaInfoProperties): Boolean;
      {$IFNDEF RVDONOTUSEINI}
      procedure SaveToINI(ini: TRVIniFile; const Section, fs: String);
      procedure LoadFromINI(ini: TRVIniFile; const Section, fs: String);
      {$ENDIF}
    published
      property Color: TColor read FColor write FColor default clNone;
      property BorderOffsets: TRVRect read FBorderOffsets write SetBorderOffsets;
  end;
  {----------------------------------------------------------------------------
    TParaInfo: paragraph style, item in the collection TRVStyle.ParaStyles
    (collection type is TParaInfos)
    Properties:
    - FirstIndent - first line indent, pixels (added to LeftIndent, can be negative)
    - LeftIndent, RightIndent, SpaceBefore, SpaceAfter - indents to the left,
      right, top, bottom of the paragraph, pixels
    - Alignment - paragraph alignmentm see TRVAlignment
    - Border - paragraph border, see TRVBorder
    - Background - paragraph background, see TRVBackgroundRect
    - NextParaNo - index of paragraph style for the next paragraph, if user
      pressed ENTER at the end of paragraph of this style. -1 for the same style
    - DefStyleNo - index of text style used for this paragraph by default
    - LineSpacing - line spacing value, pixels or percents
    - LineSpacingType - line spacing type, see TRVLineSpacingType
    - Options - see TRVParaOptions
    - BiDiMode - paragraph bi-di mode
  }
  TParaInfo = class (TCustomRVInfo)
  private
    FFirstIndent: Integer;
    FLeftIndent: Integer;
    FRightIndent: Integer;
    FSpaceBefore: Integer;
    FSpaceAfter: Integer;
    FLineSpacing: Integer;
    FLineSpacingType: TRVLineSpacingType;
    FAlignment: TRVAlignment;
    FBorder: TRVBorder;
    FNextParaNo: Integer;
    FBackground: TRVBackgroundRect;
    FOptions: TRVParaOptions;
    FBiDiMode: TRVBiDiMode;
    FDefStyleNo: Integer;
    procedure SetBorder(const Value: TRVBorder);
    procedure SetBackground(const Value: TRVBackgroundRect);
    function ExtraLineSpacing: Boolean;
  protected
    function IsSimpleEqual(Value: TCustomRVInfo; IgnoreReferences: Boolean;
      IgnoreID: Boolean{$IFDEF RICHVIEWDEF4}=True{$ENDIF}): Boolean; override;
    function IsSimpleEqualEx(Value: TCustomRVInfo; Mapping: TRVIntegerList): Boolean; override;
    function SimilarityValue(Value: TCustomRVInfo): Integer; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {$IFNDEF RVDONOTUSEINI}
    procedure SaveToINI(ini: TRVIniFile; const Section, fs: String); dynamic;
    procedure LoadFromINI(ini: TRVIniFile; const Section, fs: String); dynamic;
    {$ENDIF}
    function IsEqual(Value: TParaInfo; IgnoreList: TRVParaInfoProperties): Boolean; dynamic;
    procedure SaveCSSToStream(Stream: TStream; BaseStyle: TParaInfo;
      Multiline, IgnoreLeftAlignment, IgnoreLeftIndents: Boolean);
  published
    property FirstIndent: Integer       read FFirstIndent write FFirstIndent default 0;
    property LeftIndent:  Integer       read FLeftIndent  write FLeftIndent  default 0;
    property RightIndent: Integer       read FRightIndent write FRightIndent default 0;
    property SpaceBefore: Integer       read FSpaceBefore write FSpaceBefore default 0;
    property SpaceAfter:  Integer       read FSpaceAfter  write FSpaceAfter  default 0;
    property Alignment:   TRVAlignment  read FAlignment   write FAlignment   default rvaLeft;
    property Border:      TRVBorder     read FBorder      write SetBorder;
    property Background:  TRVBackgroundRect read FBackground write SetBackground;
    property NextParaNo: Integer        read FNextParaNo  write FNextParaNo default -1;
    property DefStyleNo: Integer        read FDefStyleNo  write FDefStyleNo default -1;
    property LineSpacing: Integer       read FLineSpacing write FLineSpacing default 100;
    property LineSpacingType: TRVLineSpacingType read FLineSpacingType write FLineSpacingType default rvlsPercent;
    property Options: TRVParaOptions    read FOptions     write FOptions;
    property BiDiMode: TRVBiDiMode      read FBiDiMode    write FBidiMode default rvbdUnspecified;
  end;
  { ---------------------------------------------------------------------------
    TParaInfos: collection of paragraph styles (of TParaInfo), TRVStyle.ParaStyles
    Properties:
    - Items[] - items
    - InvalidItem - returned when accessing item with invalid index
  }
  TParaInfos = class(TCustomRVInfos)
  private
    FInvalidItem: TParaInfo;
    function GetItem(Index: Integer): TParaInfo;
    procedure SetItem(Index: Integer; Value: TParaInfo);
    function GetInvalidItem: TParaInfo;
    procedure SetInvalidItem(const Value: TParaInfo);
  public
    function Add: TParaInfo;
    procedure AssignTo(Dest: TPersistent); override;
    {$IFNDEF RVDONOTUSEINI}
    procedure SaveToINI(ini: TRVIniFile; const Section: String);
    procedure LoadFromINI(ini: TRVIniFile; const Section: String);
    {$ENDIF}
    function FindSuchStyle(BaseStyle: Integer; Style: TParaInfo;
      Mask: TRVParaInfoProperties): Integer;
    function FindStyleWithAlignment(BaseStyle: Integer;
      Alignment: TRVAlignment): Integer;
    property Items[Index: Integer]: TParaInfo
      read GetItem write SetItem; default;
    property InvalidItem: TParaInfo read GetInvalidItem write SetInvalidItem;
  end;
  { ---------------------------------------------------------------------------
    TRVMarkerFont: font for paragraph marker.
    Overrides default values of properties (to Arial, 8pt)
  }
  TRVMarkerFont = class (TFont)
  private
    function StoreName: Boolean;
    function StoreHeight: Boolean;
  public
    constructor Create;
    function IsEqual(Font: TFont): Boolean;
    function IsDefault: Boolean;
  published
    {$IFDEF RICHVIEWCBDEF3}
    property Charset default DEFAULT_CHARSET;
    {$ENDIF}
    property Color default clWindowText;
    property Name stored StoreName;
    property Style default [];
    property Height stored StoreHeight;
  end;
  { ---------------------------------------------------------------------------
    TRVListLevel: level of paragraph bullets/numbering. Item of collection
    RVListInfo.Levels (collection type is TRVListLevelCollection)
    Properties:
    - ListType - type of bullets/numbering, see TRVListType
    - StartFrom - level numbering starts from this value
    - ImageList, ImageIndex - used if ListType = rvlstImageList or
      rvlstImageListCounter
    - FormatString - format string for ListType = rvlstBullet or text numbering
    - FormatStringW - text, used if ListType = rvlstUnicodeBullet
    - LeftIndent - left indent (right indent for RTL paragraphs), pixels;
      overrides setting for paragraph
    - FirstIndent - first line indent, pixels; added to left indent,
      overrides setting for paragraph
    - MarkerIndent - indent of list marker, pixels (see also MarkerAlignment)
    - MarkerAlignment - alignment of list marker relative to position specified
      in MarkerIndent
    - Picture - used if ListType = rvlstPicture
    - Font - font of list marker, used for text list types
    - Options - see TRVListLevelOptions
  }
  TRVListLevel = class (TCollectionItem)
  private
    FListType: TRVListType;
    FPicture: TPicture;
    FImageList: TCustomImageList;
    FImageIndex: Integer;
    FFormatString: TRVMarkerFormatString;
    {$IFNDEF RVDONOTUSEUNICODE}
    {$IFDEF RICHVIEWCBDEF3}
    FFormatStringW: TRVMarkerFormatStringW;
    {$ENDIF}
    {$ENDIF}
    FLeftIndent, FFirstIndent, FMarkerIndent: Integer;
    FMarkerAlignment: TRVMarkerAlignment;
    FFont: TRVMarkerFont;
    FOptions: TRVListLevelOptions;
    FStartFrom: Integer;
    function GetPicture: TPicture;
    procedure SetPicture(const Value: TPicture);
    function GetFont: TRVMarkerFont;
    procedure SetFont(const Value: TRVMarkerFont);
    function StoreFont: Boolean;
    function StorePicture: Boolean;
    procedure ImageListTagWriter(Writer: TWriter);
    procedure ImageListTagReader(Reader: TReader);
    {$IFNDEF RVDONOTUSEUNICODE}
    {$IFDEF RICHVIEWCBDEF3}
    procedure FormatStringWCodeWriter(Writer: TWriter);
    procedure FormatStringWCodeReader(Reader: TReader);
    {$ENDIF}
    {$ENDIF}
    procedure FormatStringCodeWriter(Writer: TWriter);
    procedure FormatStringCodeReader(Reader: TReader);
    function StoreImageList: Boolean;
    function GetRVFRVData: TPersistent;
  protected
    {$IFDEF RICHVIEWCBDEF3}
    function GetDisplayName: String; override;
    {$ENDIF}
    function IsSimpleEqual(Value: TRVListLevel): Boolean;
    procedure DefineProperties(Filer: TFiler); override;
    function SimilarityValue(Value: TRVListLevel): Integer;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetHTMLOpenTagForCSS: String;
    function GetIndentCSSForTextVersion: String;
    procedure HTMLOpenTag(Stream: TStream; UseCSS: Boolean);
    procedure HTMLCloseTag(Stream: TStream; UseCSS: Boolean);
    function HasPicture: Boolean;
    function UsesFont: Boolean;
    function HasNumbering: Boolean;
    function HasVariableWidth: Boolean;
    {$IFNDEF RVDONOTUSEINI}
    procedure SaveToINI(ini: TRVIniFile; const Section, fs: String);
    procedure LoadFromINI(ini: TRVIniFile; const Section, fs: String);
    {$ENDIF}
  published
    property ListType: TRVListType read FListType write FListType default rvlstBullet;
    property StartFrom: Integer read FStartFrom write FStartFrom default 1;
    property ImageList: TCustomImageList read FImageList write FImageList stored StoreImageList;
    property ImageIndex: Integer read FImageIndex write FImageIndex default 0;
    property FormatString: TRVMarkerFormatString read FFormatString write FFormatString stored False;
    {$IFNDEF RVDONOTUSEUNICODE}
    {$IFDEF RICHVIEWCBDEF3}
    property FormatStringW: TRVMarkerFormatStringW read FFormatStringW write FFormatStringW stored False;
    {$ENDIF}
    {$ENDIF}
    property LeftIndent: Integer read FLeftIndent write FLeftIndent default 0;
    property FirstIndent: Integer read FFirstIndent write FFirstIndent default 10;
    property MarkerIndent: Integer read FMarkerIndent write FMarkerIndent default 0;
    property MarkerAlignment: TRVMarkerAlignment read FMarkerAlignment write FMarkerAlignment default rvmaLeft;
    property Picture: TPicture read GetPicture write SetPicture stored StorePicture;
    property Font: TRVMarkerFont read GetFont write SetFont stored StoreFont;
    property Options: TRVListLevelOptions read FOptions write FOptions default [rvloContinuous, rvloLevelReset];
  end;
  { ---------------------------------------------------------------------------
    TRVListLevelCollection: collection of levels of paragraph bullets/numbering.
    A type of TRVListInfo.Levels. Type of collection item is TRVListLevel
    Properties:
    Items[] - list levels
  }
  TRVListLevelCollection = class (TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TRVListLevel;
    procedure SetItem(Index: Integer; const Value: TRVListLevel);
  public
    constructor Create(Owner: TPersistent);
    {$IFDEF RICHVIEWCBDEF3}
    function GetOwner: TPersistent;  override;
    {$ENDIF}
    function Add: TRVListLevel;
    {$IFDEF RICHVIEWDEF4}
    function Insert(Index: Integer): TRVListLevel;
    {$ENDIF}
    function IsSimpleEqual(Value: TRVListLevelCollection): Boolean;
    property Items[Index: Integer]: TRVListLevel
       read GetItem write SetItem; default;
  end;
  {----------------------------------------------------------------------------
    TRVListInfo: style of paragraph bullets/numbering, item in the collection
    TRVStyle.ListStyles (collection type is TRVListInfos)
    Properties:
    - Levels[] - collection of list levels; must have at least one item in
      order to display bullet/numbering
    - OneLevelPreview - for using in user interface (if True, preview
      of this paragraph list should show only one level)
    - ListID (read-only) - a random number for distinguishing lists with the same
      properties when pasting RVF
  }
  TRVListInfo = class (TCustomRVInfo)
  private
    FLevels: TRVListLevelCollection;
    FOneLevelPreview: Boolean;
    FListID: Integer;
    procedure SetLevels(const Value: TRVListLevelCollection);
    function GetListID: Integer;
    procedure ReadListID(Reader: TReader);
    procedure WriteListID(Writer: TWriter);
  protected
    function SimilarityValue(Value: TCustomRVInfo): Integer; override;
    procedure DefineProperties(Filer: TFiler); override;
  public
    function IsSimpleEqual(Value: TCustomRVInfo; IgnoreReferences: Boolean;
      IgnoreID: Boolean{$IFDEF RICHVIEWDEF4}=True{$ENDIF}): Boolean; override;
    function IsSimpleEqualEx(Value: TCustomRVInfo; Mapping: TRVIntegerList): Boolean; override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    {$IFNDEF RVDONOTUSEINI}
    procedure SaveToINI(ini: TRVIniFile; const Section, fs: String); dynamic;
    procedure LoadFromINI(ini: TRVIniFile; const Section, fs: String); dynamic;
    {$ENDIF}
    function HasNumbering: Boolean;
    function AllNumbered: Boolean;
    function HasVariableWidth: Boolean;
    property ListID: Integer read GetListID;
  published
    property Levels: TRVListLevelCollection read FLevels write SetLevels;
    property OneLevelPreview: Boolean read FOneLevelPreview write FOneLevelPreview default False;
  end;
  { ---------------------------------------------------------------------------
    TRVListInfos: collection of styles of paragraph lists (of TRVListInfo),
    TRVStyle.ListStyles
    Properties:
    - Items[] - items
  }
  TRVListInfos = class (TCustomRVInfos)
  private
    function GetItem(Index: Integer): TRVListInfo;
    procedure SetItem(Index: Integer; const Value: TRVListInfo);
    procedure RemoveImageList(ImageList: TCustomImageList);
  public
    FRVData: TPersistent;
    function Add: TRVListInfo;
    {$IFDEF RICHVIEWDEF4}
    function Insert(Index: Integer): TRVListInfo;
    {$ENDIF}
    {$IFNDEF RVDONOTUSEINI}
    procedure LoadFromINI(ini: TRVIniFile; const Section: String);
    procedure SaveToINI(ini: TRVIniFile; const Section: String);
    {$ENDIF}
    function FindSuchStyle(Style: TRVListInfo; AddIfNotFound: Boolean): Integer;
    function FindStyleWithLevels(Levels: TRVListLevelCollection;
      const StyleNameForAdding: String; AddIfNotFound: Boolean): Integer;
    property Items[Index: Integer]: TRVListInfo
       read GetItem write SetItem; default;
  end;

  TRVFontInfoClass = class of TFontInfo;
  TRVParaInfoClass = class of TParaInfo;
  TRVListInfoClass = class of TRVListInfo;

  { ---------------------------------------------------------------------------
    TRVStyle: component. Contains properties affecting TCustomRichView.
    Assign TCustomRichView.Style to TRVStyle object.
    Properties:
    - TextStyles - collection of text styles, see TFontInfos, TFontInfo
    - ParaStyles - collection of paragraph styles, see TParaInfos, TParaInfo
    - ListStyles - collection of styles of paragraph lists, see TRVListInfos,
      TRVListInfo

    - SpacesInTab - a number of space characters used to replace TAB

    - JumpCursor - hypertext cursor for non-text items ("hot-pictures",
      "hotspots")
    - LineSelectCursor - cursor for line selection (when mouse pointer is
      above the left margin of RichView

    - Color - background color, if TCustomRichView.Color = clNone
    - HoverColor - color of hypertext under mouse (if TFontInfo.HoverColor =
      clNone), clNone for no effect.
    - CurrentItemColor - color of border around current image or control in
      editor. clNone for no effect.
    - SelColor - background color of selection, clNone for invisible selection
      (i.s.). Used if TCustomRichView has input focus.
    - SelTextColor -  color of selected text, clNone for i.s. Used if
      TCustomRichView has input focus.
    - InactiveSelColor - background color of selection, clNone for i.s. Used if
      TCustomRichView does not have input focus.
    - InactiveSelTextColor - color of selected text, clNone for i.s. Used if
      TCustomRichView does not have input focus.
    - CheckpointColor - color of "checkpoints"; used if rvoShowCheckpoints is
      in TCustomRichView.Options. For "checkpoints" with no "raise-event" flag
    - CheckpointEvColor - the same, but for "checkpoints" with "raise-event"
      flag
    - PageBreakColor - color of explicit page breaks. Used if rvoShowPageBreaks
      is in TCustomRichView.Options.
    - SoftPageBreakColor - the same for "soft" (automatic) page breaks

    - SelectionMode: mode of making selection, see TRVSelectionMode
    - SelectionStyle: visual appearance of selection, see TRVSelectionStyle

    - FullRedraw - (see the help file)
    - UseSound - allows beeping on incorrect operations (such as attempting
      deleting protected text)
    - DefUnicodeStyle - index (in TextStyles) of style that should be used
      for Unicode (if Unicode operation is performed in TCustomRichViewEdit
      but the current style is not Unicode). -1 for no special processing.
    - DefCodePage - code page for ANSI <-> Unicode conversion
    - InvalidPicture - picture to replace invalid/damaged pictures

    Events:
    - OnApplyStyle: TRVApplyStyleEvent - allows to set additional properties
      to Canvas then applying text style (by default font, spacing, bidi-mode
      are set)
    - OnApplyStyleColor: TRVApplyStyleColorEvent - allows to override color
      applied to Canvas's font and brush then applying text style
    - OnDrawStyleText: TRVDrawStyleTextEvent - event for text custom drawing
    - OnStyleHoverSensitive - asks, if the text should be redrawn when mouse
      enters/leaves it; used for custom drawing
    - OnDrawTextBack: TRVDrawTextBackEvent - event for text custom drawing
      (drawing text background)
    - OnDrawCheckpoint: TRVDrawCheckpointEvent - allows to override default
      drawing of "checkpoints"
    - OnDrawPageBreak: TRVDrawPageBreakEvent - allows to override default
      drawing of page breaks
    - OnDrawParaBack: TRVDrawParaRectEvent - custom drawing of paragraph
      background
  }
  TRVStyle = class(TComponent)
  private
    { Private declarations }
    FInvalidPicture: TPicture;
    FColor, FHoverColor, FCurrentItemColor, FSelColor, FSelTextColor,
    FInactiveSelColor, FInactiveSelTextColor,
    FCheckpointColor, FCheckpointEvColor: TColor;
    FJumpCursor: TCursor;
    FTextStyles: TFontInfos;
    FParaStyles: TParaInfos;
    FListStyles: TRVListInfos;
    FFullRedraw: Boolean;
    FSpacesInTab: SmallInt;
    FPageBreakColor, FSoftPageBreakColor: TColor;
    FOnApplyStyleColor: TRVApplyStyleColorEvent;
    FOnApplyStyle: TRVApplyStyleEvent;
    FOnDrawStyleText: TRVDrawStyleTextEvent;
    FOnStyleHoverSensitive: TRVStyleHoverSensitiveEvent;
    FOnDrawTextBack: TRVDrawTextBackEvent;
    FOnDrawCheckpoint: TRVDrawCheckpointEvent;
    FOnDrawPageBreak: TRVDrawPageBreakEvent;
    FOnDrawParaBack: TRVDrawParaRectEvent;
    {$IFNDEF RVDONOTUSEUNICODE}
    FDefUnicodeStyle: Integer;
    {$ENDIF}
    FDefCodePage:TRVCodePage;
    FUseSound: Boolean;
    FSelectionMode: TRVSelectionMode;
    FSelectionStyle: TRVSelectionStyle;
    FLineSelectCursor: TCursor;
    procedure SetTextStyles(Value: TFontInfos);
    procedure SetParaStyles(Value: TParaInfos);
    procedure SetListStyles(Value: TRVListInfos);
    function GetHoverColorByColor(Color: TColor): TColor;
    function GetInvalidPicture: TPicture;
    procedure SetInvalidPicture(const Value: TPicture);
  protected
    { Protected declarations }
    procedure ReadState(Reader: TReader);override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    ItemNo, OffsetInItem: Integer;
    RVData: TPersistent;
    procedure ResetTextStyles;
    procedure ResetParaStyles;
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetTextStyleClass: TRVFontInfoClass; virtual;
    function GetParaStyleClass: TRVParaInfoClass; virtual;
    function GetListStyleClass: TRVListInfoClass; virtual;
    function AddTextStyle: Integer; {$IFDEF RICHVIEWDEF6}deprecated;{$ENDIF}
    procedure DeleteTextStyle(Index: Integer); {$IFDEF RICHVIEWDEF6}deprecated;{$ENDIF}
    {$IFNDEF RVDONOTUSEINI}    
    procedure SaveINI(const FileName, Section: String); {WARNING: before saving all Section will be removed}
    procedure LoadINI(const FileName, Section: String);
    procedure SaveToINI(ini: TRVIniFile; Section: String);
    procedure LoadFromINI(ini: TRVIniFile; Section: String);
    {$IFDEF RICHVIEWDEF4}
    procedure SaveReg(const BaseKey: String); {WARNING: will be created 'RVStyle' subkey. If it
                                               already exists, all data and subkeys in this 'RVStyle'
                                               key will be erased}
    procedure LoadReg(const BaseKey: String);
    {$ENDIF}
    {$ENDIF}
    {$IFNDEF RVDONOTUSEHTML}
    procedure SaveCSSToStream(Stream: TStream; AOptions: TRVSaveCSSOptions);
    function SaveCSS(const FileName: String; AOptions: TRVSaveCSSOptions): Boolean;
    {$ENDIF}
    function GetHoverColor(StyleNo: Integer): TColor;

    procedure DrawTextBack(Canvas: TCanvas; ItemNo, StyleNo: Integer;
      RVData: TPersistent; Left, Top, Width, Height: Integer;
      DrawState: TRVTextDrawStates);
    procedure ApplyStyle(Canvas: TCanvas; StyleNo: Integer;
      DefBiDiMode: TRVBiDiMode);
    procedure ApplyStyleColor(Canvas: TCanvas; StyleNo: Integer;
      DrawState: TRVTextDrawStates; Printing: Boolean; ColorMode: TRVColorMode);
    procedure DrawStyleText(const s: String; Canvas: TCanvas;
      ItemNo, OffsetInItem, StyleNo: Integer; RVData: TPersistent;
      SpaceBefore, Left, Top, Width, Height: Integer;
      DrawState: TRVTextDrawStates; Printing, PreviewCorrection: Boolean;
      ColorMode: TRVColorMode; DefBiDiMode: TRVBidiMode);
    procedure DrawCheckpoint(Canvas: TCanvas; X,Y, ItemNo, XShift: Integer;
      RaiseEvent: Boolean; Control: TControl);
    procedure DrawPageBreak(Canvas: TCanvas; Y, XShift: Integer;
      PageBreakType: TRVPageBreakType; Control: TControl);
    procedure DrawParaBack(Canvas: TCanvas; ParaNo: Integer; const Rect: TRect;
      Printing: Boolean; ColorMode: TRVColorMode);
    function StyleHoverSensitive(StyleNo: Integer): Boolean;
  published
    { Published declarations }
    property TextStyles:  TFontInfos read FTextStyles  write SetTextStyles;
    property ParaStyles:  TParaInfos read FParaStyles  write SetParaStyles;
    property ListStyles:  TRVListInfos read FListStyles write SetListStyles;
    property SpacesInTab: SmallInt   read FSpacesInTab write FSpacesInTab   default 8;
    property JumpCursor:  TCursor    read FJumpCursor  write FJumpCursor    default crJump;
    property LineSelectCursor: TCursor read FLineSelectCursor write FLineSelectCursor default crRVFlipArrow;
    property FullRedraw:  Boolean    read FFullRedraw  write FFullRedraw    default False;
    property UseSound:    Boolean    read FUseSound    write FUseSound      default True;
    property Color:             TColor     read FColor             write FColor             default clWindow;
    property HoverColor:        TColor     read FHoverColor        write FHoverColor        default clNone;
    property CurrentItemColor:  TColor     read FCurrentItemColor  write FCurrentItemColor  default clNone;
    property SelColor:          TColor     read FSelColor          write FSelColor          default clHighlight;
    property SelTextColor:      TColor     read FSelTextColor      write FSelTextColor      default clHighlightText;
    property InactiveSelColor:     TColor  read FInactiveSelColor     write FInactiveSelColor      default clHighlight;
    property InactiveSelTextColor: TColor  read FInactiveSelTextColor write FInactiveSelTextColor  default clHighlightText;
    property CheckpointColor:   TColor     read FCheckpointColor   write FCheckpointColor   default clGreen;
    property CheckpointEvColor: TColor     read FCheckpointEvColor write FCheckpointEvColor default clLime;
    property PageBreakColor:    TColor     read FPageBreakColor    write FPageBreakColor    default clBtnShadow;
    property SoftPageBreakColor: TColor    read FSoftPageBreakColor  write FSoftPageBreakColor default clBtnFace;
    property SelectionMode: TRVSelectionMode read FSelectionMode write FSelectionMode default rvsmWord;
    property SelectionStyle: TRVSelectionStyle read FSelectionStyle write FSelectionStyle default rvssItems;    
    {$IFNDEF RVDONOTUSEUNICODE}
    property DefUnicodeStyle:   Integer    read FDefUnicodeStyle   write FDefUnicodeStyle   default -1;
    {$ENDIF}
    property DefCodePage:      TRVCodePage read FDefCodePage       write FDefCodePage       default CP_ACP;
    property InvalidPicture: TPicture      read GetInvalidPicture    write SetInvalidPicture;

    property OnApplyStyle: TRVApplyStyleEvent read FOnApplyStyle write FOnApplyStyle;
    property OnApplyStyleColor: TRVApplyStyleColorEvent read FOnApplyStyleColor write FOnApplyStyleColor;
    property OnDrawStyleText: TRVDrawStyleTextEvent read FOnDrawStyleText write FOnDrawStyleText;
    property OnStyleHoverSensitive: TRVStyleHoverSensitiveEvent read FOnStyleHoverSensitive write FOnStyleHoverSensitive;
    property OnDrawTextBack: TRVDrawTextBackEvent read FOnDrawTextBack write FOnDrawTextBack;
    property OnDrawCheckpoint: TRVDrawCheckpointEvent read FOnDrawCheckpoint write FOnDrawCheckpoint;
    property OnDrawPageBreak: TRVDrawPageBreakEvent read FOnDrawPageBreak write FOnDrawPageBreak;
    property OnDrawParaBack: TRVDrawParaRectEvent read FOnDrawParaBack write FOnDrawParaBack;
  end;

  procedure RVWrite(Stream: TStream; const s: String);
  procedure RVWriteLn(Stream: TStream; const s: String);
  procedure RVWriteX(Stream: TStream; const s: String; Multiline: Boolean);

const
  { default value for TCustomRichView.RTFOptions }
  rvrtfDefault: TRVRTFOptions =
    [rvrtfDuplicateUnicode, rvrtfSaveEMFAsWMF, rvrtfSaveJpegAsJpeg];
  { all properties of TFontInfo }
  RVAllFontInfoProperties: TRVFontInfoProperties =
    [Low(TRVFontInfoProperty)..High(TRVFontInfoProperty)];
  { all properties of TParaInfo }
  RVAllParaInfoProperties: TRVParaInfoProperties =
    [Low(TRVParaInfoProperty)..High(TRVParaInfoProperty)];
  { all properties of TRVBackgroundRect }
  RVAllParaBackgroundProperties: TRVParaInfoProperties =
    [rvpiBackground_Color..rvpiBackground_BO_Bottom];
  { all properties of TRVBorder }
  RVAllParaBorderProperties: TRVParaInfoProperties =
    [rvpiBorder_Color..rvpiBorder_Vis_Bottom];

  { If True, Standard properties of styles added from inserted RVF will
    be reset to False. }
  RichViewResetStandardFlag: Boolean = True;
  { If True, 'LstId' pseudo-property will not be saved when storing
    list styles in RVF. This pseudo-property allows smarter inserting RVF
    with lists, but does not allow aplications built with older version of
    TRichView to load new RVFs }
  RVNoLstIDProperty: Boolean = False;

implementation
uses RVUni, RVStr, CRVData, RVItem, RVFuncs, RVFMisc;
{==============================================================================}
{$IFNDEF RVDONOTUSEINI}
const arrNoYes: array [False..True] of String = (RVINIFILENO,RVINIFILEYES);
{ Write integer Value to ini only if it is not equal to DefValue               }
procedure WriteIntToIniIfNE(ini: TRVIniFile; const Section, Key: String;
  Value, DefValue: Integer);
begin
  if Value<>DefValue then
    ini.WriteInteger(Section, Key, Value);
end;
{------------------------------------------------------------------------------}
{ Write boolean Value to ini only if it is not equal to DefValue.
  Value is written as "Yes" or "No"                                            }
procedure WriteBoolToIniIfNE(ini: TRVIniFile; const Section, Key: String;
                               Value, DefValue: Boolean);
begin
  if Value<>DefValue then
    ini.WriteString(Section, Key, arrNoYes[Value]);
end;
{------------------------------------------------------------------------------}
{ Read boolean value ("Yes"/"No" from ini                                      }
function IniReadBool(ini: TRVIniFile; const Section, Key: String;
                        DefValue: Boolean): Boolean;
begin
  Result := UpperCase(ini.ReadString(Section, Key, arrNoYes[DefValue]))=RVINIFILEYESU;
end;
{------------------------------------------------------------------------------}
{ Writing long string to ini. String is splitted on parts by 500 characters.
  String is written in keys Key+'_'+number. Number is 0-based                  }
procedure WriteLongStringToINI(ini: TRVIniFile; const Section, Key, Value: String);
var l,i: Integer;
    s: String;
begin
  i := 0;
  l := 500;
  while l<Length(Value) do begin
    s := Copy(Value, l-500+1, 500);
    ini.WriteString(Section, Key+'_'+IntToStr(i), s);
    inc(i);
    inc(l,500);
  end;
  s := Copy(Value, l-500+1, Length(Value));
  if s<>'' then
    ini.WriteString(Section, Key+'_'+IntToStr(i), s);
end;
{------------------------------------------------------------------------------}
{ Reading strings saved with WriteLongStringToINI                              }
function ReadLongStringFromINI(ini: TRVIniFile; const Section, Key: String): String;
var i: Integer;
    s: String;
begin
  Result := '';
  i := 0;
  while True do begin
    s := ini.ReadString(Section, Key+'_'+IntToStr(i), '');
    if s='' then
      break;
    Result := Result+s;
    inc(i);
  end;
end;
{------------------------------------------------------------------------------}
{ Encoding font styles in string                                               }
function FontStylesToString(Styles: TFontStyles): String;
begin
  Result := '';
  if fsBold in Styles then
    Result := Result + 'B';
  if fsItalic in Styles then
    Result := Result + 'I';
  if fsUnderline in Styles then
    Result := Result + 'U';
  if fsStrikeOut in Styles then
    Result := Result + 'S';
end;
{------------------------------------------------------------------------------}
{ Decoding string in font styles                                               }
function StringToFontStyles(const Styles: string): TFontStyles;
var i: Integer;
begin
  Result := [];
  for i := 1 to Length(Styles) do
    case Styles[i] of
      'B','b':
        Include(Result, fsBold);
      'I','i':
        Include(Result, fsItalic);
      'U','u':
        Include(Result, fsUnderline);
      'S','s':
        Include(Result, fsStrikeOut);
    end;
end;
{------------------------------------------------------------------------------}
{ Encoding font in string like "Arial,8,BI,0,clWindowText,0"                   }
function FontToString(Font: TFont): String;
begin
  with Font do
    Result := Format('%s,%d,%s,%d,%s,%d', [Name, Height,
      FontStylesToString(Style), Ord(Pitch), ColorToString(Color),
      {$IFDEF RICHVIEWCBDEF3} Charset {$ELSE} 0 {$ENDIF}]);
end;
{------------------------------------------------------------------------------}
{ Decoding string created with FontToString                                    }
procedure StringToFont(const s: string; Font: TFont);
var
  i,j, State: Integer;
  s2: string;
begin
  i := 1;
  State := 1;
  while i<=Length(s) do begin
    j := i;
    while (j<=Length(s)) and (s[j]<>',') do
      inc(j);
    if (j<=Length(s)) and (s[j]=',') then begin
      s2 := Copy(s, i, j-i);
      i := j+1;
      end
    else begin
      s2 := Copy(s, i, j-i+1);
      i := j;
    end;
    case State of
      1: Font.Name := s2;
      2: Font.Height := StrToInt(s2);
      3: Font.Style := StringToFontStyles(s2);
      4: Font.Pitch := TFontPitch(StrToInt(s2));
      5: Font.Color := StringToColor(s2);
      {$IFDEF RICHVIEWCBDEF3}
      6: Font.Charset := TFontCharset(StrToInt(s2));
      {$ENDIF}
    end;
    inc(State);
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
{ Are rectangles r1 and r2 equal? }
function AreRectsEqual(const r1,r2: TRect): Boolean;
begin
  Result := (r1.Left=r2.Left) and (r1.Top=r2.Top) and
    (r1.Bottom=r2.Bottom) and (r1.Right=r2.Right);
end;
{------------------------------------------------------------------------------}
procedure ScaleRect(var R: TRect; sad: TRVScreenAndDevice);
begin
  exit;
  R.Left   := MulDiv(R.Left,   sad.ppixDevice, sad.ppixScreen);
  R.Right  := MulDiv(R.Right,  sad.ppixDevice, sad.ppixScreen);
  R.Top    := MulDiv(R.Top,    sad.ppiyDevice, sad.ppiyScreen);
  R.Bottom := MulDiv(R.Bottom, sad.ppiyDevice, sad.ppiyScreen);
end;
(*
{------------------------------------------------------------------------------}
procedure IniSavePen(ini: TRVIniFile; const Section,Key: String; Pen: TPen;
                     DefStyle: TPenStyle; DefColor: TColor);
begin
  WriteIntToIniIfNE(ini, Section, Key+'Style', ord(Pen.Style), ord(DefStyle));
  WriteIntToIniIfNE(ini, Section, Key+'Color', Pen.Color,      DefColor);
  WriteIntToIniIfNE(ini, Section, Key+'Width', Pen.Width,      1);
  WriteIntToIniIfNE(ini, Section, Key+'Mode',  ord(Pen.Mode),  ord(pmCopy));
end;
{------------------------------------------------------------------------------}
procedure IniLoadPen(ini: TRVIniFile; const Section,Key: String; Pen: TPen;
                     DefStyle: TPenStyle; DefColor: TColor);
begin
  Pen.Style := TPenStyle(ini.ReadInteger(Section, Key+'Style', ord(DefStyle)));
  Pen.Color := ini.ReadInteger(Section, Key+'Color', DefColor);
  Pen.Width := ini.ReadInteger(Section, Key+'Width', 1);
  Pen.Mode  := TPenMode(ini.ReadInteger(Section, Key+'Mode', ord(pmCopy)));
end;
*)
{=========================== TCustomRVInfo ====================================}
{ Constructor }
constructor TCustomRVInfo.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FBaseStyleNo  := -1;
  FStandard     := True;
end;
{------------------------------------------------------------------------------}
{ Assigns properties of Source to Self, if source is TCustomRVInfo }
procedure TCustomRVInfo.Assign(Source: TPersistent);
begin
  if Source is TCustomRVInfo then begin
    FName        := TFontInfo(Source).FName;
    FBaseStyleNo := TFontInfo(Source).FBaseStyleNo;
    FStandard    := TFontInfo(Source).FStandard;
    end
  else
    inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEINI}
{ Loads properties from the ini-file, from the section Section.
  fs is a format string for keys, it is like 'Font%s1', 'Font%s2', etc.
  DefName is a default style name.                                             }
procedure TCustomRVInfo.LoadFromINI(ini: TRVIniFile; const Section,
  fs, DefName: String);
begin
  StyleName   := ini.ReadString (Section, Format(fs,[RVINI_STYLENAME]), DefName);
  BaseStyleNo := ini.ReadInteger(Section, Format(fs,[RVINI_BASESTYLENO]), -1);
  Standard    := Boolean(ini.ReadInteger(Section, Format(fs,[RVINI_STANDARD]), Integer(True)));
end;
{------------------------------------------------------------------------------}
{ Saves properties to the ini-file, in the section Section, using the format
  string fs for keys. }
procedure TCustomRVInfo.SaveToINI(ini: TRVIniFile; const Section, fs: String);
begin
  ini.WriteString(Section,  Format(fs,[RVINI_STYLENAME]), StyleName);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_BASESTYLENO]),BaseStyleNo,-1);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_STANDARD]), Integer(Standard), Integer(True));
end;
{$ENDIF}
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
{ Returns a name of the collection item, for design-time collection editor. }
function TCustomRVInfo.GetDisplayName: String;
begin
  Result := FName;
end;
{$ENDIF}
{============================= TCustomRVInfos =================================}
{ Constructor }
constructor TCustomRVInfos.Create(ItemClass: TCollectionItemClass;
                                  Owner: TPersistent);
begin
  inherited Create(ItemClass);
  FOwner := Owner;
end;
{------------------------------------------------------------------------------}
{ Allows assigning properties to TStrings: style names are assigned. }
procedure TCustomRVInfos.AssignTo(Dest: TPersistent);
var i: Integer;
begin
  if Dest is TStrings then begin
    TStrings(Dest).Clear;
    for i:=0 to Count-1 do
      TStrings(Dest).Add(TCustomRVInfo(Items[i]).FName);
    end
  else
    inherited AssignTo(Dest);
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
{ For design-time collection editor. }
function TCustomRVInfos.GetOwner: TPersistent;
begin
  Result := FOwner;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
{ Adds items from Styles according to the method specified in the Mode.
  Mapping is filled: on exit, Mapping.Count = Styles.Count, and
  Mapping[i] is an index of the item of this collection which was created basing
  on Styles[i].
  Reference properties (BaseStyleNo, NextStyleNo, NextParaNo, DefStyleNo) are
  adjusted in the added items.
  If the global variable RichViewResetStandardFlag is True (default), Standard
  properties of all added styles are set to False.
  This method assumes that Styles have the same type as Self.
  Notes:
  * in rvs_merge_Map mode:
    - SimilarityValue method of items is used;
    - the method tries to keep Jump and Unicode properties if possible.
  * in rvs_merge_SmartMerge mode:
    - the method tries to map the style to the style with the same index,
      if possible;
    - IsSimpleEqualEx method of items is used;
    - several styles can be mapped in the same style, except for numbered lists:
      they are always mapped to the unique style.
}
procedure TCustomRVInfos.MergeWith(Styles: TCustomRVInfos;
  Mode: TRVStyleMergeMode; Mapping: TRVIntegerList;
  TextStyleMapping: TRVIntegerList);
var i,j,idx,oldcount: Integer;
    Style: TCustomRVInfo;
    wht, maxwht: Integer;
    ForbiddenStyles: TRVIntegerList;
    {.............................................}
    procedure AdjustReferences;
    var i: Integer;
        Style: TCustomRVInfo;
    begin
      for i := oldcount to Count-1 do begin
        Style := TCustomRVInfo(Items[i]);
        if RichViewResetStandardFlag then
          Style.Standard := False;
        if Style.BaseStyleNo>=0 then
          Style.BaseStyleNo := Mapping[Style.BaseStyleNo];
        if (Style is TFontInfo) and (TFontInfo(Style).NextStyleNo>=0) then
          TFontInfo(Style).NextStyleNo := Mapping[TFontInfo(Style).NextStyleNo];
        if (Style is TParaInfo) then begin
          if (TParaInfo(Style).NextParaNo>=0) then
            TParaInfo(Style).NextParaNo := Mapping[TParaInfo(Style).NextParaNo];
          if (TParaInfo(Style).DefStyleNo>=0) and (TextStyleMapping<>nil) then
            TParaInfo(Style).DefStyleNo :=
              TextStyleMapping[TParaInfo(Style).DefStyleNo];
        end;
      end;
    end;
    {.............................................}
begin
  Mapping.Clear;
  Mapping.Capacity := Styles.Count;
  oldcount := Count;
  case Mode of
    rvs_merge_Append: // Append one collection to another
      for i := 0 to Styles.Count-1 do begin
        Mapping.Add(Count);
        Add.Assign(Styles.Items[i]);
      end;
    rvs_merge_Map: // Use the most similar of existing styles. Do not add styles
      for i := 0 to Styles.Count-1 do begin
        Style := TCustomRVInfo(Styles.Items[i]);
        maxwht := 0;
        idx := -1;
        if (Style is TFontInfo) then begin
          {$IFNDEF RVDONOTUSEUNICODE}
          for j := 0 to Count-1 do
            if (TFontInfo(Items[j]).Jump=TFontInfo(Style).Jump) and
               (TFontInfo(Items[j]).Unicode=TFontInfo(Style).Unicode) then begin
              wht := Style.SimilarityValue(TCustomRVInfo(Styles.Items[j]));
              if (idx=-1) or (wht>maxwht) then begin
                maxwht := wht;
                idx := j;
              end;
            end;
          {$ENDIF}
          if idx=-1 then
            for j := 0 to Count-1 do
              if (TFontInfo(Items[j]).Jump=TFontInfo(Style).Jump) then begin
                wht := Style.SimilarityValue(TCustomRVInfo(Styles.Items[j]));
                if (idx=-1) or (wht>maxwht) then begin
                  maxwht := wht;
                  idx := j;
                end;
              end;
          {$IFNDEF RVDONOTUSEUNICODE}
          if idx=-1 then
            for j := 0 to Count-1 do
              if (TFontInfo(Items[j]).Unicode=TFontInfo(Style).Unicode) then begin
                wht := Style.SimilarityValue(TCustomRVInfo(Styles.Items[j]));
                if (idx=-1) or (wht>maxwht) then begin
                  maxwht := wht;
                  idx := j;
                end;
              end;
          {$ENDIF}
        end;
        if idx=-1 then
          for j := 0 to Count-1 do begin
            wht := Style.SimilarityValue(TCustomRVInfo(Styles.Items[j]));
            if (idx=-1) or (wht>maxwht) then begin
              maxwht := wht;
              idx := j;
            end;
          end;
        Mapping.Add(idx);
      end;
    rvs_merge_SmartMerge: // Reuse styles, add if necessary
      begin
        if Self is TRVListInfos then
          ForbiddenStyles := TRVIntegerList.Create
        else
          ForbiddenStyles := nil;
        for i := 0 to Styles.Count-1 do begin
          idx := -1;
          Style := TCustomRVInfo(Styles.Items[i]);
          if (i<Count) and Style.IsSimpleEqualEx(TCustomRVInfo(Items[i]), Mapping) and
             ((ForbiddenStyles=nil) or (ForbiddenStyles.IndexOf(Pointer(i))<0)) then
            idx := i;
          if idx<0 then
            for j := 0 to Count-1 do
              if Style.IsSimpleEqualEx(TCustomRVInfo(Items[j]), Mapping) and
                ((ForbiddenStyles=nil) or (ForbiddenStyles.IndexOf(Pointer(j))<0)) then begin
                idx := j;
                break;
              end;
          if idx<0 then begin
            idx := Count;
            Add.Assign(Styles.Items[i]);
          end;
          Mapping.Add(idx);
          if ForbiddenStyles<>nil then begin
            if TRVListInfo(Style).HasNumbering then
              ForbiddenStyles.Add(idx);
          end;
        end;
        ForbiddenStyles.Free;
      end;
  end;
  AdjustReferences;
end;
{================================ TFontInfo ===================================}
{ Constructor }
constructor TFontInfo.Create(Collection: TCollection);
begin
 inherited Create(Collection);
 FFontName  := RVDEFAULTSTYLEFONT;
 FSize      := 10;
 FColor     := clWindowText;
 FBackColor := clNone;
 FHoverBackColor := clNone;
 FHoverColor := clNone;
 FStyle     := [];
 FStyleEx   := [];
 {$IFDEF RICHVIEWCBDEF3}
 FCharset   := DEFAULT_CHARSET;
 {$ENDIF}
 Jump       := False;
 JumpCursor := crJump;
 FName      := RVDEFAULTTEXTSTYLENAME;
 FVShift    := 0;
 FNextStyleNo  := -1;
 FCharScale    := 100;
end;
{------------------------------------------------------------------------------}
{ Assigns properties of Source to Self, if it is TFontInfo or TFont.           }
procedure TFontInfo.Assign(Source: TPersistent);
begin
  if Source is TFontInfo then begin
      FFontName   := TFontInfo(Source).FFontName;
      FSize       := TFontInfo(Source).FSize;
      FColor      := TFontInfo(Source).FColor;
      FBackColor  := TFontInfo(Source).FBackColor;
      FHoverBackColor  := TFontInfo(Source).FHoverBackColor;
      FHoverColor  := TFontInfo(Source).FHoverColor;
      FStyle      := TFontInfo(Source).FStyle;
      FStyleEx    := TFontInfo(Source).FStyleEx;
      {$IFDEF RICHVIEWCBDEF3}
      FCharset    := TFontInfo(Source).FCharset;
      {$ENDIF}
      {$IFDEF RVLANGUAGEPROPERTY}
      FLanguage   := TFontInfo(Source).FLanguage;
      {$ENDIF}
      FJump       := TFontInfo(Source).FJump;
      FJumpCursor := TFontInfo(Source).FJumpCursor;
      FProtection := TFontInfo(Source).FProtection;
      FOptions    := TFontInfo(Source).FOptions;
      FVShift     := TFontInfo(Source).FVShift;
      FNextStyleNo:= TFontInfo(Source).FNextStyleNo;
      FCharScale  := TFontInfo(Source).FCharScale;
      FCharSpacing := TFontInfo(Source).FCharSpacing;
      FBiDiMode   := TFontInfo(Source).FBiDiMode;
      {$IFNDEF RVDONOTUSEUNICODE}
      FUnicode    := TFontInfo(Source).FUnicode;
      {$ENDIF}
      {$IFDEF RVTEXTFOOTNOTES}
      FFootNote := TFontInfo(Source).FFootNote;
      {$ENDIF}
      inherited Assign(Source);
    end
  else if Source is TFont then begin
      FFontName := TFont(Source).Name;
      FSize     := TFont(Source).Size;
      FColor    := TFont(Source).Color;
      FStyle    := TFont(Source).Style;
      {$IFDEF RICHVIEWCBDEF3}
      FCharset  := TFont(Source).Charset;
      {$ENDIF}
    end
  else
    inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
{ Allows assigning properties to TFont. }
procedure TFontInfo.AssignTo(Dest: TPersistent);
begin
  if Dest is TFont then begin
      TFont(Dest).Name    := FFontName;
      TFont(Dest).Size    := FSize;
      TFont(Dest).Color   := FColor;
      TFont(Dest).Style   := FStyle;
      {$IFDEF RICHVIEWCBDEF3}
      {$IFNDEF RVDONOTUSEUNICODE}
      if not Unicode then
      {$ENDIF}
        TFont(Dest).Charset := FCharset;
      {$ENDIF}
    end
  else
    inherited AssignTo(Dest);
end;
{------------------------------------------------------------------------------}
{ Assigns properties to TLogFont record. }
procedure TFontInfo.AssignToLogFont(var LogFont: TLogFont; Canvas: TCanvas);
var ppi: Integer;
begin
  FillChar(LogFont, sizeof(LogFont), 0);
  with LogFont do begin
    ppi := 0;
    if Collection<>nil then
      ppi := TFontInfos(Collection).PixelsPerInch;
    if ppi=0 then
      ppi := Canvas.Font.PixelsPerInch;
    lfHeight := -MulDiv(Size, ppi, 72);
    if fsBold in Style then
      lfWeight := FW_BOLD
    else
      lfWeight := FW_NORMAL;
    lfItalic := Byte(fsItalic in Style);
    lfUnderline := Byte(fsUnderline in Style);
    lfStrikeOut := Byte(fsStrikeOut in Style);
    {$IFDEF RICHVIEWCBDEF3}
    lfCharSet := Byte(Charset);
    {$ENDIF}
    StrPCopy(lfFaceName, FontName);
    lfQuality := DEFAULT_QUALITY;
    lfOutPrecision := OUT_DEFAULT_PRECIS;
    lfClipPrecision := CLIP_DEFAULT_PRECIS;
    lfPitchAndFamily := DEFAULT_PITCH;
  end;
end;
{------------------------------------------------------------------------------}
{ Is this item equal to Value (all properties are equal)?
  NextStyleNo property (adjusted using Mapping) is taken into account.
  Mapping is from the Value's collection to this collection, see
  TCustomRVInfos.MergeWith.
}
function TFontInfo.IsSimpleEqualEx(Value: TCustomRVInfo; Mapping: TRVIntegerList): Boolean;
begin
  Result := IsSimpleEqual(Value, True, False);
  if not Result then
    exit;
  Result := False;
  {
  if (Value.BaseStyleNo>=0) then begin
    if (Value.BaseStyleNo>=Mapping.Count) then
      Value.BaseStyleNo := -1 // fix up
    else if (Mapping[Value.BaseStyleNo]<>BaseStyleNo) then
      exit;
  end;
  }
  if (TFontInfo(Value).NextStyleNo>=0) then begin
    if (TFontInfo(Value).NextStyleNo>=Mapping.Count) then
      Value.BaseStyleNo := -1 // fix up
    else if (Mapping[TFontInfo(Value).NextStyleNo]<>NextStyleNo) then
      exit;
  end;
  Result := True;
end;
{------------------------------------------------------------------------------}
{ Is this item equal to Value (all properties are equal)?
  if IgnoreReferences=True, NextStyleNo property is ignored, otherwise they
  must be equal.
  IgnoreID is not used (used only in TRVListInfo). }
function TFontInfo.IsSimpleEqual(Value: TCustomRVInfo;
  IgnoreReferences, IgnoreID: Boolean): Boolean;
begin
   Result := (Size        = TFontInfo(Value).Size   ) and
             {$IFDEF RICHVIEWCBDEF3}
             (Charset     = TFontInfo(Value).Charset) and
             {$ENDIF}
             {$IFNDEF RVDONOTUSEUNICODE}
             (Unicode     = TFontInfo(Value).Unicode) and
             {$ENDIF}
             (Style       = TFontInfo(Value).Style  ) and
             (StyleEx     = TFontInfo(Value).StyleEx) and
             (AnsiCompareText(FontName, TFontInfo(Value).FontName)=0) and
             (VShift      = TFontInfo(Value).VShift ) and
             (Color       = TFontInfo(Value).Color  ) and
             (BackColor   = TFontInfo(Value).BackColor) and
             (Jump        = TFontInfo(Value).Jump   ) and
             {$IFDEF RVLANGUAGEPROPERTY}
             (Language     = TFontInfo(Value).Language) and
             {$ENDIF}
             (not Jump or
              ((HoverColor     = TFontInfo(Value).HoverColor    ) and
               (HoverBackColor = TFontInfo(Value).HoverBackColor) and
               (JumpCursor    = TFontInfo(Value).JumpCursor     ))
             ) and
             (IgnoreReferences or (NextStyleNo = TFontInfo(Value).NextStyleNo)) and
             (Protection  = TFontInfo(Value).Protection ) and
             (Options     = TFontInfo(Value).Options )    and
             (CharScale   = TFontInfo(Value).CharScale  ) and
             (CharSpacing = TFontInfo(Value).CharSpacing) and
             (BiDiMode    = TFontInfo(Value).BiDiMode  );
end;
{------------------------------------------------------------------------------}
{ Calculates a similarity value between Self and Value.
  The larger value means more similar. }
function TFontInfo.SimilarityValue(Value: TCustomRVInfo): Integer;
var fs: TFontStyle;
begin
   Result :=
     RV_CompareInts(TFontInfo(Value).Size, Size, RVSMW_FONTSIZE)+
     RV_CompareInts(TFontInfo(Value).VShift, VShift, RVSMW_VSHIFTRATIO)+
     RV_CompareInts(TFontInfo(Value).CharScale, CharScale, RVSMW_CHARSCALE)+
     RV_CompareInts(TFontInfo(Value).CharSpacing, CharSpacing, RVSMW_CHARSPACING)+
     RV_CompareColors(TFontInfo(Value).Color, Color, RVSMW_EACHRGBCOLOR, RVSMW_COLORSET)+
     RV_CompareColors(TFontInfo(Value).BackColor, BackColor, RVSMW_EACHRGBBCOLOR, RVSMW_BCOLORSET);
   if TFontInfo(Value).BiDiMode=BiDiMode then
     inc(Result, RVSMW_BIDIMODE);
   if AnsiCompareText(TFontInfo(Value).FontName, FontName)=0 then
     inc(Result, RVSMW_FONTNAME);
   for fs := Low(TFontStyle) to High(TFontStyle) do
     if (fs in TFontInfo(Value).Style) = (fs in Style) then
       inc(Result, RVSMW_FONTEACHSTYLE);
   if (rvfsOverline in TFontInfo(Value).StyleEx)=(rvfsOverline in StyleEx) then
     inc(Result, RVSMW_OVERLINE);
   if (rvfsAllCaps in TFontInfo(Value).StyleEx)=(rvfsAllCaps in StyleEx) then
     inc(Result, RVSMW_OVERLINE);
   if ((TFontInfo(Value).Style=[]) and (TFontInfo(Value).StyleEx=[]))
      =
      ((Style=[]) and (StyleEx=[])) then
     inc(Result, RVSMW_FONTSTYLESET);

   {$IFDEF RVLANGUAGEPROPERTY}
   if TFontInfo(Value).Language = Language then
     inc(Result, RVSMW_LANGUAGE);
   {$ENDIF}

   if Jump and TFontInfo(Value).Jump then begin
     if TFontInfo(Value).JumpCursor=JumpCursor then
       inc(Result, RVSMW_CURSOR);
     inc(Result,
         RV_CompareColors(TFontInfo(Value).HoverColor,HoverColor, RVSMW_EACHRGBCOLOR, RVSMW_COLORSET) div 2+
         RV_CompareColors(TFontInfo(Value).HoverBackColor,HoverBackColor, RVSMW_EACHRGBBCOLOR, RVSMW_BCOLORSET) div 2);
   end;
   if TFontInfo(Value).Protection<>Protection then
     dec(Result, RVSMW_PROTECTION);
   if (rvteoHTMLCode in TFontInfo(Value).Options)=(rvteoHTMLCode in Options) then
     inc(Result, RVSMW_SPECIALCODE);
   if (rvteoRTFCode in TFontInfo(Value).Options)=(rvteoRTFCode in Options) then
     inc(Result, RVSMW_SPECIALCODE);
  {$IFDEF RICHVIEWCBDEF3}
  if Charset=TFontInfo(Value).Charset then
    inc(Result, RVSMW_FONTCHARSET)
  else
    if (Charset=DEFAULT_CHARSET) or
       (TFontInfo(Value).Charset=DEFAULT_CHARSET) then
      inc(Result, RVSMW_FONTCHARSET div 4);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
{ Is this item equal to Value?
  Equality is determined by comparing all properties NOT included in IgnoreList. } 
function TFontInfo.IsEqual(Value: TFontInfo; IgnoreList: TRVFontInfoProperties): Boolean;
begin
   Result := ((rvfiSize        in IgnoreList) or (Size        = Value.Size       )) and
             {$IFDEF RICHVIEWCBDEF3}
             ((rvfiCharset     in IgnoreList) or (Charset     = Value.Charset    )) and
             {$ENDIF}
             {$IFNDEF RVDONOTUSEUNICODE}
             ((rvfiUnicode     in IgnoreList) or (Unicode     = Value.Unicode    )) and
             {$ENDIF}
             ((rvfiBold        in IgnoreList) or ((fsBold       in Style)   = (fsBold       in Value.Style  ))) and
             ((rvfiItalic      in IgnoreList) or ((fsItalic     in Style)   = (fsItalic     in Value.Style  ))) and
             ((rvfiUnderline   in IgnoreList) or ((fsUnderline  in Style)   = (fsUnderline  in Value.Style  ))) and
             ((rvfiStrikeout   in IgnoreList) or ((fsStrikeout  in Style)   = (fsStrikeout  in Value.Style  ))) and
             ((rvfiOverline    in IgnoreList) or ((rvfsOverline in StyleEx) = (rvfsOverline in Value.StyleEx))) and
             ((rvfiAllCaps     in IgnoreList) or ((rvfsAllCaps  in StyleEx) = (rvfsAllCaps  in Value.StyleEx))) and
             ((rvfiFontName    in IgnoreList) or (AnsiCompareText(FontName,Value.FontName)=0)) and
             ((rvfiVShift      in IgnoreList) or (VShift      = Value.VShift     )) and
             ((rvfiColor       in IgnoreList) or (Color       = Value.Color      )) and
             ((rvfiBackColor   in IgnoreList) or (BackColor   = Value.BackColor  )) and
             ((rvfiJump        in IgnoreList) or (Jump        = Value.Jump       )) and
             {$IFDEF RVLANGUAGEPROPERTY}
             ((rvfiLanguage    in IgnoreList) or (Language    = Value.Language   )) and
             {$ENDIF}
             (not Jump or
             ((rvfiHoverColor     in IgnoreList) or (HoverColor     = Value.HoverColor   )) and
             ((rvfiHoverBackColor in IgnoreList) or (HoverBackColor = Value.HoverBackColor)) and
             ((rvfiJumpCursor     in IgnoreList) or (JumpCursor     = Value.JumpCursor))
             ) and
             ((rvfiNextStyleNo in IgnoreList) or (NextStyleNo = Value.NextStyleNo)) and
             ((rvfiProtection  in IgnoreList) or (Protection  = Value.Protection)) and
             ((rvfiRTFCode     in IgnoreList) or ((rvteoRTFCode in Options)  = (rvteoRTFCode in Value.Options))) and
             ((rvfiHTMLCode    in IgnoreList) or ((rvteoHTMLCode in Options) = (rvteoHTMLCode in Value.Options))) and
             ((rvfiCharScale   in IgnoreList) or (CharScale  = Value.CharScale)) and
             ((rvfiCharSpacing in IgnoreList) or (CharSpacing  = Value.CharSpacing)) and
             ((rvfiBiDiMode    in IgnoreList) or (BiDiMode  = Value.BiDiMode)) and
             ((rvfiBaseStyleNo in IgnoreList) or (BaseStyleNo = Value.BaseStyleNo));
end;
{------------------------------------------------------------------------------}
{ Applies this text style to the Canvas. Colors are not applied, see ApplyColor.
  DefBiDiMode is a bi-di mode of paragraph containing text item of this style.
  Notes:
  - if FCharScale=100, this method assigns Canvas.Font properties,
     otherwise it assigns Canvas.Font.Handle.
  - if owning collection is defined and has nonzero PixelsPerInch property,
    font size is assigned according to this PixelsPerInch.
}
procedure TFontInfo.Apply(Canvas: TCanvas; DefBiDiMode: TRVBiDiMode);
var LogFont: TLogFont;
    ppi: Integer;
    tm: TTextMetric;
begin
  if FCharScale=100 then begin
    Canvas.Font.Style := Style;
    ppi := 0;
    if Collection<>nil then
      ppi := TFontInfos(Collection).PixelsPerInch;
    if ppi=0 then
      Canvas.Font.Size  := Size
    else
      Canvas.Font.Height := - MulDiv(Size, ppi, 72);
    Canvas.Font.Name  := FontName;
    {$IFDEF RICHVIEWCBDEF3}
    Canvas.Font.CharSet  := CharSet;
    {$ENDIF}
    end
  else begin
    AssignToLogFont(LogFont, Canvas);
    Canvas.Font.Handle := CreateFontIndirect(LogFont);
    if GetTextMetrics(Canvas.Handle, tm) then
      LogFont.lfWidth := tm.tmAveCharWidth*FCharScale div 100
    else
      LogFont.lfWidth := Canvas.TextWidth('x')*FCharScale div 100;
    Canvas.Font.Handle := CreateFontIndirect(LogFont);
  end;
  if BiDiMode<>rvbdUnspecified then
    DefBiDiMode := BiDiMode;
  case DefBiDiMode of
    rvbdLeftToRight:
      begin
        {$IFNDEF RVDONOTUSECHARSPACING}
        SetTextCharacterExtra(Canvas.Handle, 0);
        {$ENDIF}
        SetTextAlign(Canvas.Handle, TA_LEFT);
      end;
    rvbdRightToLeft:
      begin
        {$IFNDEF RVDONOTUSECHARSPACING}
        SetTextCharacterExtra(Canvas.Handle, 0);
        {$ENDIF}
        SetTextAlign(Canvas.Handle, TA_RTLREADING);
      end;
    else begin
      {$IFNDEF RVDONOTUSECHARSPACING}
      SetTextCharacterExtra(Canvas.Handle, FCharSpacing);
      {$ENDIF}
    end;
  end;
end;
{------------------------------------------------------------------------------}
{ Applies color properties of this style to the Canvas.
  Colors depend on values in DrawState (specifically: rvtsSelected, rvtsHover,
  rvtsControlFocused).
  ColorMode is used to adjust colors. }
procedure TFontInfo.ApplyColor(Canvas: TCanvas; RVStyle: TRVStyle;
  DrawState: TRVTextDrawStates; Printing: Boolean; ColorMode: TRVColorMode);
begin
  if rvtsSelected in DrawState then begin
    Canvas.Brush.Style := bsSolid;
    if rvtsControlFocused in DrawState then
      Canvas.Brush.Color := RVStyle.SelColor
    else
      Canvas.Brush.Color := RVStyle.InactiveSelColor;
    {$IFDEF RVUSETEXTHOVERCOLORWITHSELECTED}
    if rvtsHover in DrawState then begin
      Canvas.Font.Color := RVStyle.GetHoverColorByColor(HoverColor);
      if Canvas.Font.Color=clNone then
        Canvas.Font.Color := Color;
      end
    else
    {$ENDIF}
    if rvtsControlFocused in DrawState  then
      Canvas.Font.Color := RVStyle.SelTextColor
    else
      Canvas.Font.Color := RVStyle.InactiveSelTextColor;
    if Canvas.Font.Color=clNone then
      Canvas.Font.Color  := Color;
    end
  else begin
    if rvtsHover in DrawState then begin
       Canvas.Font.Color  := RVStyle.GetHoverColorByColor(HoverColor);
       if Canvas.Font.Color=clNone then
         Canvas.Font.Color := Color;
       Canvas.Brush.Color := HoverBackColor;
       end
     else if not Printing then begin
       Canvas.Font.Color  := Color;
       Canvas.Brush.Color := BackColor;
       end
     else
       case ColorMode of
         rvcmColor:
           begin
             Canvas.Font.Color  := Color;
             Canvas.Brush.Color := BackColor;
           end;
         rvcmPrinterColor:
           begin
             Canvas.Font.Color  := RV_GetPrnColor(Color);
             Canvas.Brush.Color := RV_GetPrnColor(BackColor);
           end;
         rvcmGrayScale:
           begin
             Canvas.Font.Color  := RV_GetGray(RV_GetPrnColor(Color));
             Canvas.Brush.Color := RV_GetGray(RV_GetPrnColor(BackColor));
           end;
         rvcmBlackAndWhite:
           begin
             if BackColor=clNone then begin
               Canvas.Brush.Color := clNone;
               if RV_GetPrnColor(Color)<>clWhite then
                 Canvas.Font.Color  := clBlack
               else
                 Canvas.Font.Color  := clWhite;
               end
             else if RV_GetLuminance(RV_GetPrnColor(BackColor))>RV_GetLuminance(RV_GetPrnColor(Color)) then begin
               Canvas.Brush.Color := clWhite;
               Canvas.Font.Color := clBlack;
               end
             else begin
               Canvas.Brush.Color := clBlack;
               Canvas.Font.Color := clWhite;
             end;
           end;
         rvcmBlackOnWhite:
           begin
             Canvas.Font.Color  := clBlack;
             Canvas.Brush.Color := clNone;
           end;
       end;
  end;
  if Canvas.Brush.Color=clNone then
    Canvas.Brush.Style := bsClear
  else
    Canvas.Brush.Style := bsSolid;
end;

{------------------------------------------------------------------------------}
{ Workaround for incorrect headers in D2-D6 }
type
{$IFDEF RICHVIEWDEF7}
  TGetCharacterPlacementVal = Integer;
{$ELSE}
  TGetCharacterPlacementVal = LongBool;
{$ENDIF}
const
  GETCHARACTERPLACEMENTFLAGS = GCP_DIACRITIC or GCP_GLYPHSHAPE or GCP_USEKERNING or GCP_REORDER;
{ Draws the string s onto the Canvas.
  For Unicode text, s contains "raw Unicode".
  Item occupies the rectangle Bounds(Left, Top, Width, Height), text is started
  at the position (Left+SpaceBefore, Top). SpaceBefore can be positive in
  justify-aligned paragraphs.
  This item is RVStyle.TextStyles[ThisStyleNo].
  DefBiDiMode is a bi-di mode of the paragraph containing this item.
  Printing is True if this is printing/print preview.
  PreviewCorrection is True if this is a print preview requiring correction.
  ColorMode is used to adjust colors.

  Notes:
  - if (BiDiMode is unspecified) and Printing and PreviewCorrection, a special
    procedure is used: it adjusts character positions to fit required text
    width (Width-SpaceBefore), see PrintText(..., True);
  - if (BiDiMode is unspecified) and Printing and not PreviewCorrection and
    (CharExtra<>0) a special procedure is used to apply CharExtra (because
    some printers ignore direct setting), see PrintText(..., False)
  - this procedure draws dots (#$B7) in place of spaces, if rvtsSpecialCharacters
    is in DrawState, see DrawDots. 
}
procedure TFontInfo.Draw(const s: String; Canvas: TCanvas; ThisStyleNo: Integer;
  SpaceBefore, Left, Top, Width, Height: Integer; RVStyle: TRVStyle;
  DrawState: TRVTextDrawStates; Printing, PreviewCorrection: Boolean;
  ColorMode: TRVColorMode; DefBiDiMode: TRVBiDiMode);

  {......................................................}
  function PrintText(Spacing: Integer; AutoCalcSpacing: Boolean): Boolean;
  var  PDx: PRVIntegerArray;
      Dummy: Integer;
      ItemOptions: TRVItemOptions;
      i, Len, w,w2,l: Integer;
  begin
    Result := True;
    {$IFNDEF RVDONOTUSEUNICODE}
    if Unicode then begin
      ItemOptions := [rvioUnicode];
      Len := Length(s) div 2;
      end
    else
    {$ENDIF}
    begin
      ItemOptions := [];
      Len := Length(s);
    end;
    if Len<2 then begin
      Result := False;
      exit;
    end;
    GetMem(PDx, (Len+1)*sizeof(Integer));
    try
      RVU_GetTextExtentExPoint(Canvas, s, Width*2, Dummy, PDx, ItemOptions);
      for i := Len-1 downto 1 do
        dec(PDx[i], PDx[i-1]);
      if not AutoCalcSpacing then begin
        for i := 0 to Len-1 do
          inc(PDx[i], Spacing);
        end
      else begin
        w := RVU_TextWidth(s, Canvas, ItemOptions);
        if w=Width-SpaceBefore then begin
          Result := False;
          exit;
        end;
        w := Width-SpaceBefore-w;
        l := Len;
        for i := 0 to Len-1 do begin
          if w=0 then
            break;
          if l=0 then
            w2 := w
          else
            w2 := w div l;
          inc(PDx[i], w2);
          dec(w,w2);
          dec(l);
        end;
      end;
      {$IFDEF RVDONOTUSEUNICODE}
      ExtTextOutA(Canvas.Handle, Left+SpaceBefore, Top, 0, nil, Pointer(s), Len, Pointer(PDx));
      {$ELSE}
        if not Unicode then
          ExtTextOutA(Canvas.Handle, Left+SpaceBefore, Top, 0, nil, Pointer(s), Len, Pointer(PDx))
        else
          ExtTextOutW(Canvas.Handle, Left+SpaceBefore, Top, 0, nil, Pointer(s), Len, Pointer(PDx));
      {$ENDIF}
    finally
      FreeMem(PDx);
    end;
  end;
  {......................................................}
  procedure DrawDots;
  var res: TGCPResults;
    i, Len, Spacing, X: Integer;
    POrder,POrderRev: PRVUnsignedArray;
    PDX: PRVIntegerArray;
    ok: Boolean;
    ItemOptions: TRVItemOptions;
  begin
    if Printing then
      exit;
    Len := Length(s);
    if Len=0 then
      exit;
    {$IFNDEF RVDONOTUSEUNICODE}
    if Unicode then
      Len := Len div 2;
    if Unicode and not RVNT then
      ok := False
    else {$ENDIF} begin
      Spacing := GetTextCharacterExtra(Canvas.Handle);
      FillChar(res, sizeof(TGCPResults), 0);
      res.lStructSize := sizeof(TGCPResults);
      GetMem(POrder,    Len*sizeof(Cardinal));
      GetMem(POrderRev, Len*sizeof(Cardinal));
      GetMem(PDX,       Len*sizeof(Integer));
      try
        FillChar(POrder^, Len*sizeof(Cardinal), 0);
        res.lpOrder := @(POrder[0]);
        res.lpDx    := @(PDX[0]);
        res.nGlyphs := Len;
        {$IFNDEF RVDONOTUSEUNICODE}
        if Unicode then
          ok := GetCharacterPlacementW(Canvas.Handle, Pointer(s),
            TGetCharacterPlacementVal(Len), TGetCharacterPlacementVal(0), res,
            GETCHARACTERPLACEMENTFLAGS)<>0
        else
        {$ENDIF}
          ok := GetCharacterPlacementA(Canvas.Handle, PChar(s),
            TGetCharacterPlacementVal(Len), TGetCharacterPlacementVal(0), res,
            GETCHARACTERPLACEMENTFLAGS)<>0;
        if ok then begin
          for i := 0 to Len-1 do
            POrderRev[POrder[i]] := i;
          inc(Left, SpaceBefore);
          {$IFNDEF RVDONOTUSEUNICODE}
          if not Unicode then
          {$ENDIF}
            for i := 0 to Len-1 do begin
              if s[POrderRev[i]+1]=' ' then
                Canvas.TextOut(Left, Top, chr($b7));
              inc(Left, PDX[i]+Spacing);
            end
          {$IFNDEF RVDONOTUSEUNICODE}
          else
            for i := 0 to Len-1 do begin
              if PRVWordArray(PChar(s))[POrderRev[i]]=ord(' ') then
                Canvas.TextOut(Left, Top, chr($b7));
              inc(Left, PDX[i]+Spacing);
            end;
          {$ENDIF}
        end;
      finally
        FreeMem(POrder);
        FreeMem(POrderRev);
        FreeMem(PDX);
      end;
    end;
    if ok then
      exit;
    GetMem(PDX, (Len+2)*sizeof(Integer));
    try
      {$IFNDEF RVDONOTUSEUNICODE}
      if Unicode then
        ItemOptions := [rvioUnicode]
      else
      {$ENDIF}
        ItemOptions := [];
      RVU_GetTextExtentExPoint(Canvas, s, Width*2, X, PDX, ItemOptions);
      inc(Left, SpaceBefore);
      {$IFNDEF RVDONOTUSEUNICODE}
      if not Unicode then
      {$ENDIF}
        for i := 0 to Len-1 do begin
          if s[i+1]=' ' then begin
            X := Left;
            if i>0 then
              inc(X, PDX[i-1]);
            Canvas.TextOut(X, Top, chr($b7));
          end;
        end
      {$IFNDEF RVDONOTUSEUNICODE}
      else
        for i := 0 to Len-1 do begin
          if PRVWordArray(PChar(s))[i]=ord(' ') then begin
            X := Left;
            if i>0 then
              inc(X, PDX[i-1]);
            Canvas.TextOut(X, Top, chr($b7));
          end;
        end;
      {$ENDIF}
    finally
      FreeMem(PDX);
    end;
  end;
  {......................................................}
var
    potm: POutlineTextMetric;
    sz: Integer;
    CharExtra: Integer;
    TextDone: Boolean;

begin
  TextDone := False;
  if BiDiMode<>rvbdUnspecified then
    DefBiDiMode := BiDiMode;
  if Printing and (DefBiDiMode=rvbdUnspecified) then begin
    if (Canvas.Brush.Style<>bsClear) then begin
      Canvas.Pen.Style := psClear;
      Canvas.FillRect(Bounds(Left,Top,Width,Height));
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psSolid;
    end;
    if not PreviewCorrection then begin
      CharExtra := GetTextCharacterExtra(Canvas.Handle);
      if CharExtra<>0 then begin
         SetTextCharacterExtra(Canvas.Handle, 0);
         TextDone := PrintText(CharExtra, False);
         SetTextCharacterExtra(Canvas.Handle, CharExtra);
      end;
      end
    else begin
      TextDone := PrintText(0, True);
    end;
  end;
  {$IFNDEF RVDONOTUSEJUSTIFY};
  if not TextDone then begin
    {$IFDEF RVDONOTUSEUNICODE}
    Canvas.TextOut(Left+SpaceBefore, Top, s);
    {$ELSE}
      if not Unicode then
        TextOutA(Canvas.Handle, Left+SpaceBefore, Top, PChar(s), Length(s))
      else
        TextOutW(Canvas.Handle, Left+SpaceBefore, Top, Pointer(s), Length(s) div 2);
    {$ENDIF}
    if rvtsSpecialCharacters in DrawState then
      DrawDots;
  end;
  if (SpaceBefore<>0) and not Printing then begin
    if (rvtsSelected in DrawState) and (Length(s)=0) then
      RVStyle.ApplyStyleColor(Canvas, ThisStyleNo, DrawState-[rvtsSelected], Printing, ColorMode);
    if Canvas.Brush.Style<>bsClear then
    Canvas.FillRect(Bounds(Left,Top,SpaceBefore,Height));
  end;
  {$ELSE}
  if not TextDone then begin
    {$IFDEF RVDONOTUSEUNICODE}
    Canvas.TextOut(Left, Top, s);
    {$ELSE}
      if not Unicode then
        TextOutA(Canvas.Handle, Left, Top, PChar(s), Length(s))
      else
        TextOutW(Canvas.Handle, Left, Top, Pointer(s), Length(s) div 2);
    {$ENDIF}
  end;
  {$ENDIF}
  Canvas.Brush.Style := bsClear;
  potm := nil;
  try
    {$IFNDEF RVDONOTUSEJUSTIFY}
    if (SpaceBefore<>0) and (fsUnderline in Canvas.Font.Style) then begin
      sz := GetOutlineTextMetrics(Canvas.Handle,0,nil);
      if sz>0 then begin
        GetMem(potm, sz);
        FillChar(potm^, sz, 0);
        sz := GetOutlineTextMetrics(Canvas.Handle,sz,potm);
        if sz>0 then begin
          Canvas.Pen.Color := Canvas.Font.Color;
          Canvas.Pen.Width := potm.otmsUnderscoreSize;
          Canvas.Pen.Style := psInsideFrame;
          Canvas.MoveTo(Left,Top-potm.otmsUnderscorePosition+potm.otmTextMetrics.tmAscent+potm.otmsUnderscoreSize div 2);
          Canvas.LineTo(Left+SpaceBefore+1,Top-potm.otmsUnderscorePosition+potm.otmTextMetrics.tmAscent+potm.otmsUnderscoreSize div 2);
          Canvas.Pen.Style := psSolid;
        end;
      end;
    end;
    {$ENDIF}
    if rvfsOverline in StyleEx then begin
      if potm=nil then begin
        sz := GetOutlineTextMetrics(Canvas.Handle,0,nil);
        if sz>0 then begin
          GetMem(potm, sz);
          FillChar(potm^, sz, 0);
          sz := GetOutlineTextMetrics(Canvas.Handle,sz,potm);
          if sz>0 then
            Canvas.Pen.Width := potm.otmsUnderscoreSize
          else
            Canvas.Pen.Width := 1;
          end
        else
          Canvas.Pen.Width := 1;
      end;
      Canvas.Pen.Color := Canvas.Font.Color;
      Canvas.MoveTo(Left, Top);
      Canvas.LineTo(Left+Width, Top);
    end;
  finally
    if potm<>nil then
      FreeMem(potm);
  end;
end;
{------------------------------------------------------------------------------}
{ You do not see this :) }
procedure TFontInfo.DrawVertical(const s: String; Canvas: TCanvas;
  ThisStyleNo, SpaceBefore, Left, Top, Width, Height: Integer;
  RVStyle: TRVStyle; DrawState: TRVTextDrawStates);
begin
  {$IFNDEF RVDONOTUSEJUSTIFY};
  {$IFDEF RVDONOTUSEUNICODE}
  Canvas.TextOut(Left, Top+SpaceBefore, s);
  {$ELSE}
    if not Unicode then
      TextOutA(Canvas.Handle, Left, Top+SpaceBefore, PChar(s), Length(s))
    else
      TextOutW(Canvas.Handle, Left, Top+SpaceBefore, Pointer(s), Length(s) div 2);
  {$ENDIF}
  if (SpaceBefore<>0) then begin
    if (rvtsSelected in DrawState) and (Length(s)=0) then
      RVStyle.ApplyStyleColor(Canvas, ThisStyleNo, DrawState-[rvtsSelected], False, rvcmColor);
    if Canvas.Brush.Style<>bsClear then
    Canvas.FillRect(Bounds(Left, Top, Height,SpaceBefore));
  end;
  {$ELSE}
  {$IFDEF RVDONOTUSEUNICODE}
  Canvas.TextOut(Left, Top, s);
  {$ELSE}
    if not Unicode then
      TextOutA(Canvas.Handle, Left, Top, PChar(s), Length(s))
    else
      TextOutW(Canvas.Handle, Left, Top, Pointer(s), Length(s) div 2);
  {$ENDIF}
  {$ENDIF}
  Canvas.Brush.Style := bsClear;
  if rvfsOverline in StyleEx then begin
    Canvas.Pen.Color := Canvas.Font.Color;
    Canvas.MoveTo(Left, Top);
    Canvas.LineTo(Left, Top+Width);
  end;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEINI}
const IniProtectMask = $3FF;
{ Loads properties from the ini-file, from the section Section.
  fs is a format string for keys, it is like 'Font%s1', 'Font%s2', etc.
  DefName is a default style name.
  JumpByDefault - for backward compatibility, defines if this style should be
  hypertext if this is not explicitly specified in the ini-file.
  DefJumpCursor - hypertext cursor assigned to this style if, if another
  cursor is not specified in the ini-file explicitly.
}
procedure TFontInfo.LoadFromINI(ini: TRVIniFile; const Section,
  fs: String; JumpByDefault: Boolean; DefJumpCursor: TCursor);
var s: String;
    pr: Word;
begin
  inherited LoadFromINI(ini, Section, fs, RVDEFAULTTEXTSTYLENAME);
  FontName   := ini.ReadString (Section, Format(fs,[RVINI_FONTNAME]),  RVDEFAULTSTYLEFONT);
  s := UpperCase(ini.ReadString(Section, Format(fs,[RVINI_JUMP]),      RVINIUNKNOWN));
  if (s=RVINIUNKNOWN) then begin // for compatibility with old saving format
    Jump := JumpByDefault;
    JumpCursor := DefJumpCursor;
    end
  else begin
    Jump       := (s=RVINIFILEYESU);
    JumpCursor := ini.ReadInteger(Section, Format(fs,[RVINI_JUMPCURSOR]), crJump);
  end;
  Size       := ini.ReadInteger(Section, Format(fs,[RVINI_SIZE]),        10);
  Color      := ini.ReadInteger(Section, Format(fs,[RVINI_COLOR]),       clWindowText);
  BackColor  := ini.ReadInteger(Section, Format(fs,[RVINI_BACKCOLOR]),   clNone);
  HoverBackColor  := ini.ReadInteger(Section, Format(fs,[RVINI_HOVERBACKCOLOR]), clNone);
  HoverColor := ini.ReadInteger(Section, Format(fs,[RVINI_HOVERCOLOR]), clNone);
  {$IFDEF RICHVIEWCBDEF3}
  Charset    := ini.ReadInteger(Section, Format(fs,[RVINI_CHARSET]),    DEFAULT_CHARSET);
  {$ENDIF}
  {$IFDEF RVLANGUAGEPROPERTY}
  Language := ini.ReadInteger(Section, Format(fs,[RVINI_LANGUAGE]), 0);
  {$ENDIF}
  CharScale  := ini.ReadInteger(Section, Format(fs,[RVINI_CHARSCALE]),  100);
  CharSpacing := ini.ReadInteger(Section, Format(fs,[RVINI_CHARSPACING]),  0);  
  BiDiMode   := TRVBiDiMode(ini.ReadInteger(Section, Format(fs,[RVINI_BIDIMODE]),  0));
  Style    := [];
  if IniReadBool(ini, Section, Format(fs,[RVINI_BOLD]), False) then
    Include(FStyle, fsBold);
  if IniReadBool(ini, Section, Format(fs,[RVINI_UNDERLINE]), False) then
    Include(FStyle, fsUnderline);
  if IniReadBool(ini, Section, Format(fs,[RVINI_STRIKEOUT]), False) then
    Include(FStyle, fsStrikeOut);
  if IniReadBool(ini, Section, Format(fs,[RVINI_ITALIC]), False) then
    Include(FStyle, fsItalic);
  StyleEx  := [];
  if IniReadBool(ini, Section, Format(fs,[RVINI_OVERLINE]), False) then
    Include(FStyleEx, rvfsOverline);
  if IniReadBool(ini, Section, Format(fs,[RVINI_ALLCAPS]), False) then
    Include(FStyleEx, rvfsAllCaps);
  FOptions  := [];
  if IniReadBool(ini, Section, Format(fs,[RVINI_RTFCODE]), False) then
    Include(FOptions, rvteoRTFCode);
  if IniReadBool(ini, Section, Format(fs,[RVINI_HTMLCODE]), False) then
    Include(FOptions, rvteoHTMLCode);
  pr := ini.ReadInteger(Section, Format(fs,[RVINI_PROTECTION]), 0) and IniProtectMask;
  Protection := TRVProtectOptions(pr);
  if iniReadBool(ini, Section, Format(fs,[RVINI_SINGLESYMBOLS]), False) then begin
    Include(FProtection, rvprStyleProtect);
    Include(FProtection, rvprDoNotAutoSwitch);
  end;
  VShift        := ini.ReadInteger(Section, Format(fs,[RVINI_VSHIFT]),      0);
  NextStyleNo   := ini.ReadInteger(Section, Format(fs,[RVINI_NEXTSTYLENO]), -1);
  {$IFNDEF RVDONOTUSEUNICODE}
  Unicode       := iniReadBool(ini, Section, Format(fs,[RVINI_UNICODE]), False);
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
{ Saves properties to the ini-file, to the section Section.
  fs is a format string for keys, it is like 'Font%s1', 'Font%s2', etc. }
procedure TFontInfo.SaveToINI(ini: TRVIniFile; const Section, fs: String);
begin
  inherited SaveToINI(ini, Section, fs);
  ini.WriteString(Section,  Format(fs,[RVINI_FONTNAME]),       FontName);
  ini.WriteString(Section,  Format(fs,[RVINI_JUMP]),       arrNoYes[Jump]);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_SIZE]),       Size,       10);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_COLOR]),      Color,      clWindowText);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_BACKCOLOR]),  BackColor,  clNone);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_HOVERBACKCOLOR]), HoverBackColor, clNone);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_HOVERCOLOR]), HoverColor, clNone);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_JUMPCURSOR]), JumpCursor, crJump);
  {$IFDEF RICHVIEWCBDEF3}
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_CHARSET]),    Charset,    DEFAULT_CHARSET);
  {$ENDIF}
  WriteIntToIniIfNE(ini, Section,  Format(fs,[RVINI_CHARSCALE]),  CharScale,  100);
  WriteIntToIniIfNE(ini, Section,  Format(fs,[RVINI_CHARSPACING]), CharSpacing,  0);  
  WriteIntToIniIfNE(ini, Section,  Format(fs,[RVINI_BiDiMode]),  ord(BiDiMode),  0);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_BOLD]),      fsBold      in Style, False);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_UNDERLINE]), fsUnderline in Style, False);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_STRIKEOUT]), fsStrikeOut in Style, False);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_ITALIC]),    fsItalic    in Style, False);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_OVERLINE]),  rvfsOverline in StyleEx, False);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_ALLCAPS]),   rvfsAllCaps in StyleEx, False);
  WriteIntToIniIfNE(ini, Section,  Format(fs,[RVINI_PROTECTION]), Word(Protection) and IniProtectMask, 0);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_RTFCODE]),    rvteoRTFCode  in Options, False);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_HTMLCODE]),   rvteoHTMLCode  in Options, False);
  WriteIntToIniIfNE(ini, Section,  Format(fs,[RVINI_VSHIFT]),     VShift,     0);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_NEXTSTYLENO]),NextStyleNo,-1);
  {$IFDEF RVLANGUAGEPROPERTY}
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_LANGUAGE]),Language,0);
  {$ENDIF}
  {$IFNDEF RVDONOTUSEUNICODE}
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_UNICODE]), Unicode, False);
  {$ENDIF}
end;
{$ENDIF}
{------------------------------------------------------------------------------}
{ Saves this text style as a part of CSS to the Stream.
  if BaseStyle<>nil, only a difference between this style and BaseStyle is
  saved.
  If Multiline=False, all text will be written on a single line. } 
procedure TFontInfo.SaveCSSToStream(Stream: TStream; BaseStyle: TFontInfo;
  Multiline: Boolean);
const
    cssFontStyle  : array[Boolean] of String = ('normal','italic');
    cssFontWeight : array[Boolean] of String = ('normal','bold');
    {..................................................}
    function GetTextDecoration(Style: TFontStyles; StyleEx: TRVFontStyles;
                               Jump: Boolean): String;
    var s1, s2: String;
    begin
      if fsUnderline in Style then
        s1 := 'underline'
      else if fsStrikeOut in Style then
        s1 := 'line-through'
      else
        s1 := '';
      if rvfsOverline in StyleEx then
        s2 := 'overline'
      else
        s2 := '';
      if (s1='') then begin
        if (s2='') then
          Result := 'none'
        else
          if Jump then
            Result := 'none '+s2
          else
            Result := s2
        end
      else begin
        if Jump and (s1<>'underline') then s1 := 'none '+s1;
        if (s2='') then
          Result := s1
        else
          Result := s1+' '+s2;
      end;
    end;
    {..................................................}
    function GetTextVAlign(VShift: Integer): String;
    begin
      if VShift>0 then
        Result := 'super'
      else if VShift<0 then
        Result := 'sub'
      else
        Result := '';
    end;
    {..................................................}
    function GetHoverColor(Color: TColor): TColor;
    begin
      if Color=clNone then
        Result := HoverColor
      else
        Result := Color;
    end;
    {..................................................}
var s: String;
begin
  if (BaseStyle=nil) or (BaseStyle.Size<>Size) then
    RVWriteX(Stream, Format(' font-size: %dpt;',[Size]), Multiline);
  if (BaseStyle=nil) or (AnsiUpperCase(BaseStyle.FontName)<>AnsiUpperCase(FontName)) then
    RVWriteX(Stream, Format(' font-family: ''%s'';',[FontName]), Multiline);
  if (BaseStyle=nil) or ((fsItalic in BaseStyle.Style)<>(fsItalic in Style)) then
    RVWriteX(Stream, Format(' font-style: %s;',[cssFontStyle[fsItalic in Style]]),
      Multiline);
  if (BaseStyle=nil) or ((fsBold in BaseStyle.Style)<>(fsBold in Style)) then
    RVWriteX(Stream, Format(' font-weight: %s;',[cssFontWeight[fsBold in Style]]),
      Multiline);
  if (BaseStyle=nil) or (BaseStyle.Color<>Color) then
    RVWriteX(Stream, Format(' color: %s;',[RV_GetHTMLRGBStr(Color, False)]), Multiline);
  if ((BaseStyle=nil) and (CharSpacing<>0)) or
     ((BaseStyle<>nil) and (BaseStyle.CharSpacing<>CharSpacing)) then
     RVWriteX(Stream, Format(' letter-spacing: %dpx;',[CharSpacing]), Multiline);
  if (rvfsAllCaps in StyleEx) then begin
    if (BaseStyle=nil) or not (rvfsAllCaps in BaseStyle.StyleEx) then
      RVWriteX(Stream, ' text-transform: uppercase;', Multiline);
    end
  else if (BaseStyle<>nil) and (rvfsAllCaps in BaseStyle.StyleEx) then
      RVWriteX(Stream, ' text-transform: none;', Multiline);
  if ((BaseStyle=nil) and ((BackColor<>clNone) or not Multiline)) or
     ((BaseStyle<>nil) and (BaseStyle.BackColor<>BackColor)) then
    RVWriteX(Stream, Format(' background-color: %s;',[RV_GetCSSBkColor(BackColor)]),
      Multiline);
  s := GetTextVAlign(VShift);
  if ((BaseStyle=nil) and (s<>'')) or
     ((BaseStyle<>nil) and (s<>GetTextVAlign(BaseStyle.VShift))) then
    RVWriteX(Stream, Format(' vertical-align: %s;',[s]), Multiline);
  s := GetTextDecoration(Style,StyleEx,Jump);
  if (BaseStyle=nil) or
     (s<>GetTextDecoration(BaseStyle.Style,BaseStyle.StyleEx,BaseStyle.Jump))
     or (Jump and (s='none')) then
    RVWriteX(Stream, Format(' text-decoration: %s;',[s]), Multiline);
end;
{================================== TFontInfos ================================}
destructor TFontInfos.Destroy;
begin
  FInvalidItem.Free;
  inherited;
end;
{-----------------------------------------------------------------------}
function TFontInfos.Add: TFontInfo;
begin
  Result := TFontInfo(inherited Add);
end;
{-----------------------------------------------------------------------}
function TFontInfos.AddFont(Name: TFontName; Size: Integer;
                   Color,BackColor: TColor; Style:TFontStyles): TFontInfo;
begin
   Result := Add;
   Result.FontName  := Name;
   Result.Size      := Size;
   Result.Color     := Color;
   Result.BackColor := BackColor;
   Result.Style     := Style;
end;
{-----------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
function TFontInfos.AddFontEx(Name: TFontName; Size: Integer;
                   Color, BackColor: TColor; Style:TFontStyles;
                   Charset: TFontCharset): TFontInfo;
begin
   Result := AddFont(Name, Size, Color, BackColor, Style);
   Result.Charset := Charset;
end;
{$ENDIF}
{-----------------------------------------------------------------------}
function TFontInfos.GetItem(Index: Integer): TFontInfo;
begin
  if (Index<0) or (Index>=Count) then
    Result := InvalidItem
  else
    Result := TFontInfo(inherited GetItem(Index));
end;
{-----------------------------------------------------------------------}
procedure TFontInfos.SetItem(Index: Integer; Value: TFontInfo);
begin
  inherited SetItem(Index, Value);
end;
{------------------------------------------------------------------------------}
function TFontInfos.GetInvalidItem: TFontInfo;
begin
  if FInvalidItem=nil then begin
    FInvalidItem := (FOwner as TRVStyle).GetTextStyleClass.Create(nil);
    if Count>0 then
      FInvalidItem.Assign(Items[0]);
    FInvalidItem.BackColor := clRed;
    FInvalidItem.Color := clWhite;
  end;
  Result := FInvalidItem;
end;
{------------------------------------------------------------------------------}
procedure TFontInfos.SetInvalidItem(const Value: TFontInfo);
begin
  if InvalidItem<>Value then
    InvalidItem.Assign(Value);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEINI}
procedure TFontInfos.LoadFromINI(ini: TRVIniFile; const Section: String;
                                 DefJumpCursor: TCursor);
var i, cnt: Integer;
begin
  // for compatibility with old versions, default count of styles is
  // LAST_DEFAULT_STYLE_NO+1
  cnt := ini.ReadInteger(Section, RVINI_TEXTSTYLECOUNT,   LAST_DEFAULT_STYLE_NO+1);
  Clear;
  for i := 0 to cnt-1 do begin
    Add;
    Items[i].LoadFromINI(ini, Section, RVINI_TEXTSTYLEPREFIX+IntToStr(i),
                         i in [rvsJump1, rvsJump2],
                         DefJumpCursor);
  end;
end;
{------------------------------------------------------------------------------}
procedure TFontInfos.SaveToINI(ini: TRVIniFile; const Section: String);
var i: Integer;
begin
  ini.WriteInteger(Section, RVINI_TEXTSTYLECOUNT, Count);
  for i:=0 to Count-1 do
    Items[i].SaveToINI(ini, Section, RVINI_TEXTSTYLEPREFIX+IntToStr(i));
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TFontInfo.DefineProperties(Filer: TFiler);
begin
  Filer.DefineProperty(RVINI_SINGLESYMBOLS, SingleSymbolsReader, nil, False);
end;
{------------------------------------------------------------------------------}
procedure TFontInfo.SingleSymbolsReader(reader: TReader);
var ss: Boolean;
begin
  ss := reader.ReadBoolean;
  if ss then begin
    Include(FProtection, rvprStyleProtect);
    Include(FProtection, rvprDoNotAutoSwitch);
  end;
end;
{------------------------------------------------------------------------------}
function TFontInfos.FindStyleWithFont(BaseStyle: Integer; Font: TFont): Integer;
var i: Integer;
    {........................................}
    function Matched(fi: TFontInfo): Boolean;
    begin
      Result := (fi.Size=Font.Size) and
                (fi.Style=Font.Style) and
                (fi.FontName=Font.Name) and
                {$IFDEF RICHVIEWCBDEF3}
                (fi.Charset=Font.Charset) and
                {$ENDIF}
                (fi.Color=Font.Color);
    end;
    {........................................}
begin
  if Matched(Items[BaseStyle]) then begin
    Result := BaseStyle;
    exit;
  end;
  for i := 0 to Count-1 do
    if (i<>BaseStyle) and Matched(Items[i]) and
       Items[BaseStyle].IsEqual(Items[i], [rvfiFontName, rvfiSize, rvfiCharset,
                                           rvfiBold, rvfiItalic, rvfiUnderline,
                                           rvfiStrikeout, rvfiColor]) then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;
{------------------------------------------------------------------------------}
function TFontInfos.FindStyleWithFontSize(BaseStyle, Size: Integer): Integer;
var i: Integer;
begin
  if Items[BaseStyle].Size = Size then begin
    Result := BaseStyle;
    exit;
  end;
  for i := 0 to Count-1 do
    if (i<>BaseStyle) and (Items[i].Size=Size) and
       Items[BaseStyle].IsEqual(Items[i], [rvfiSize]) then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;
{------------------------------------------------------------------------------}
function TFontInfos.FindStyleWithColor(BaseStyle: Integer; Color,
  BackColor: TColor): Integer;
var i: Integer;
begin
  if (Items[BaseStyle].Color     = Color) and
     (Items[BaseStyle].BackColor = BackColor) then begin
    Result := BaseStyle;
    exit;
  end;
  for i := 0 to Count-1 do
    if (i<>BaseStyle) and
       (Items[i].Color     = Color) and
       (Items[i].BackColor = BackColor) and
       Items[BaseStyle].IsEqual(Items[i], [rvfiColor, rvfiBackColor]) then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;
{------------------------------------------------------------------------------}
function TFontInfos.FindStyleWithFontName(BaseStyle: Integer;
  const FontName: TFontName): Integer;
var i: Integer;
begin
  if Items[BaseStyle].FontName = FontName then begin
    Result := BaseStyle;
    exit;
  end;
  for i := 0 to Count-1 do
    if (i<>BaseStyle) and (Items[i].FontName=FontName) and
       Items[BaseStyle].IsEqual(Items[i], [rvfiFontName]) then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;
{------------------------------------------------------------------------------}
function TFontInfos.FindSuchStyle(BaseStyle: Integer; Style: TFontInfo;
  Mask: TRVFontInfoProperties): Integer;
var i: Integer;
begin
  Mask := RVAllFontInfoProperties - Mask;
  if Style.IsEqual(Items[BaseStyle], Mask) then begin
    Result := BaseStyle;
    exit;
  end;
  for i := 0 to Count-1 do
    if (i<>BaseStyle) and Style.IsEqual(Items[i], Mask) then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
function TFontInfos.FindStyleWithCharset(BaseStyle: Integer; Charset: TFontCharset): Integer;
var i: Integer;
begin
  if (Items[BaseStyle].Charset=Charset)
     {$IFNDEF RVDONOTUSEUNICODE}
     and not Items[BaseStyle].Unicode
     {$ENDIF}
     then begin
    Result := BaseStyle;
    exit;
  end;
  for i := 0 to Count-1 do
    if (i<>BaseStyle) and (Items[i].Charset=Charset) and
       {$IFNDEF RVDONOTUSEUNICODE}
       not Items[i].Unicode and
       {$ENDIF}
       Items[BaseStyle].IsEqual(Items[i], [rvfiCharset, rvfiUnicode]) then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TFontInfos.FindStyleWithFontStyle(BaseStyle: Integer; Value,
  Mask: TFontStyles): Integer;
var i: Integer;
    IgnoreList: TRVFontInfoProperties;
    {........................................}
    function Matched(fi: TFontInfo): Boolean;
    var i: TFontStyle;
    begin
      for i := Low(TFontStyle) to High(TFontStyle) do
        if (i in Mask) and ((i in fi.Style)<>(i in Value)) then begin
          Result := False;
          exit;
        end;
      Result := True;
    end;
    {........................................}
begin
  if Matched(Items[BaseStyle]) then begin
    Result := BaseStyle;
    exit;
  end;
  IgnoreList := [];
  if fsBold in Mask then
    Include(IgnoreList, rvfiBold);
  if fsItalic in Mask then
    Include(IgnoreList, rvfiItalic);
  if fsUnderline in Mask then
    Include(IgnoreList, rvfiUnderline);
  if fsStrikeout in Mask then
    Include(IgnoreList, rvfiStrikeout);

  for i := 0 to Count-1 do
    if (i<>BaseStyle) and Matched(Items[i]) and
       Items[BaseStyle].IsEqual(Items[i], IgnoreList) then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;
{================================== TRVRect ===================================}
procedure TRVRect.Assign(Source: TPersistent);
begin
  if Source is TRVRect then begin
    Left   := TRVRect(Source).Left;
    Right  := TRVRect(Source).Right;
    Top    := TRVRect(Source).Top;
    Bottom := TRVRect(Source).Bottom;
    end
  else
    inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
procedure TRVRect.SetAll(Value: Integer);
begin
  Left   := Value;
  Top    := Value;
  Right  := Value;
  Bottom := Value;
end;
{------------------------------------------------------------------------------}
procedure TRVRect.AssignToRect(var Rect: TRect);
begin
  Rect.Left   := Left;
  Rect.Top    := Top;
  Rect.Right  := Right;
  Rect.Bottom := Bottom;
end;
{------------------------------------------------------------------------------}
procedure TRVRect.AssignToRectIfGreater(var Rect: TRect);
begin
  if Left>Rect.Left then
    Rect.Left   := Left;
  if Top>Rect.Top then
    Rect.Top    := Top;
  if Right>Rect.Right then
    Rect.Right  := Right;
  if Bottom>Rect.Bottom then
    Rect.Bottom := Bottom;
end;
{------------------------------------------------------------------------------}
procedure TRVRect.InflateRect(var Rect: TRect);
begin
  dec(Rect.Left,   Left);
  dec(Rect.Top,    Top);
  inc(Rect.Right,  Right);
  inc(Rect.Bottom, Bottom);
end;
{------------------------------------------------------------------------------}
procedure TRVRect.InflateRectSaD(var Rect: TRect;
  const sad: TRVScreenAndDevice);
begin
  dec(Rect.Left,   MulDiv(Left,   sad.ppixDevice, sad.ppixScreen));
  dec(Rect.Top,    MulDiv(Top,    sad.ppiyDevice, sad.ppiyScreen));
  inc(Rect.Right,  MulDiv(Right,  sad.ppixDevice, sad.ppixScreen));
  inc(Rect.Bottom, MulDiv(Bottom, sad.ppiyDevice, sad.ppiyScreen));
end;
{------------------------------------------------------------------------------}
function TRVRect.IsEqual(Value: TRVRect): Boolean;
begin
  Result := (Left=Value.Left) and (Right=Value.Right) and
            (Top =Value.Top)  and (Bottom=Value.Bottom);
end;
{------------------------------------------------------------------------------}
function TRVRect.IsEqualEx(Value: TRVRect; IgnL, IgnT, IgnR,
  IgnB: Boolean): Boolean;
begin
  Result := (IgnL or (Left=Value.Left)) and
            (IgnR or (Right=Value.Right)) and
            (IgnT or (Top =Value.Top)) and
            (ignB or (Bottom=Value.Bottom));
end;
{------------------------------------------------------------------------------}
function TRVRect.SimilarityValue(Value: TRVRect; Weight: Integer): Integer;
begin
  Result := RV_CompareInts(Left,   Value.Left,   Weight)+
            RV_CompareInts(Top,    Value.Top,    Weight)+
            RV_CompareInts(Right,  Value.Right,  Weight)+
            RV_CompareInts(Bottom, Value.Bottom, Weight);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEINI}
procedure TRVRect.LoadFromINI(ini: TRVIniFile; const Section, fs: String);
begin
  Left    := ini.ReadInteger(Section, Format(fs,[RVINI_LEFT]),   0);
  Right   := ini.ReadInteger(Section, Format(fs,[RVINI_RIGHT]),  0);
  Top     := ini.ReadInteger(Section, Format(fs,[RVINI_TOP]),    0);
  Bottom  := ini.ReadInteger(Section, Format(fs,[RVINI_BOTTOM]), 0);
end;
{------------------------------------------------------------------------------}
procedure TRVRect.SaveToINI(ini: TRVIniFile; const Section, fs: String);
begin
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_LEFT]),   Left,   0);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_RIGHT]),  Right,  0);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_TOP]),    Top,    0);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_BOTTOM]), Bottom, 0);
end;
{$ENDIF}
{================================ TRVBooleanRect ==============================}
constructor TRVBooleanRect.Create(DefValue: Boolean);
begin
  inherited Create;
  SetAll(DefValue);
end;
{------------------------------------------------------------------------------}
procedure TRVBooleanRect.SetAll(Value: Boolean);
begin
  Left   := Value;
  Top    := Value;
  Right  := Value;
  Bottom := Value;
end;
{------------------------------------------------------------------------------}
procedure TRVBooleanRect.SetValues(ALeft, ATop, ARight, ABottom: Boolean);
begin
  Left   := ALeft;
  Top    := ATop;
  Right  := ARight;
  Bottom := ABottom;
end;
{------------------------------------------------------------------------------}
procedure TRVBooleanRect.Assign(Source: TPersistent);
begin
  if Source is TRVBooleanRect then begin
    Left   := TRVBooleanRect(Source).Left;
    Right  := TRVBooleanRect(Source).Right;
    Top    := TRVBooleanRect(Source).Top;
    Bottom := TRVBooleanRect(Source).Bottom;
    end
  else
    inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
function TRVBooleanRect.IsEqual(Value: TRVBooleanRect): Boolean;
begin
  Result := (Left=Value.Left) and (Right=Value.Right) and
            (Top =Value.Top)  and (Bottom=Value.Bottom);
end;
{------------------------------------------------------------------------------}
function TRVBooleanRect.IsEqual2(ALeft, ATop, ARight, ABottom: Boolean): Boolean;
begin
  Result := (Left=ALeft) and (Right=ARight) and
            (Top =ATop)  and (Bottom=ABottom);
end;
{------------------------------------------------------------------------------}
function TRVBooleanRect.IsAllEqual(Value: Boolean): Boolean;
begin
  Result := (Left=Value) and (Right=Value) and
            (Top =Value) and (Bottom=Value);
end;
{------------------------------------------------------------------------------}
function TRVBooleanRect.IsEqualEx(Value: TRVBooleanRect; IgnL, IgnT, IgnR,
  IgnB: Boolean): Boolean;
begin
  Result := (IgnL or (Left=Value.Left)) and
            (IgnR or (Right=Value.Right)) and
            (IgnT or (Top =Value.Top)) and
            (ignB or (Bottom=Value.Bottom));
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEINI}
procedure TRVBooleanRect.LoadFromINI(ini: TRVIniFile; const Section,
  fs: String);
begin
  Left    := IniReadBool(ini, Section, Format(fs,[RVINI_LEFT]),   True);
  Right   := IniReadBool(ini, Section, Format(fs,[RVINI_RIGHT]),  True);
  Top     := IniReadBool(ini, Section, Format(fs,[RVINI_TOP]),    True);
  Bottom  := IniReadBool(ini, Section, Format(fs,[RVINI_BOTTOM]), True);
end;
{------------------------------------------------------------------------------}
procedure TRVBooleanRect.SaveToINI(ini: TRVIniFile; const Section,
  fs: String);
begin
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_LEFT]),   Left,   True);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_RIGHT]),  Right,  True);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_TOP]),    Top,    True);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_BOTTOM]), Bottom, True);
end;
{$ENDIF}
{============================= TRVBorder ==================================}
constructor TRVBorder.Create;
begin
  inherited Create;
  FBorderOffsets := TRVRect.Create;
  FVisibleBorders := TRVBooleanRect.Create(True);
  Style := rvbNone;
  Color := clWindowText;
  Width := 1;
  InternalWidth := 1;
end;
{------------------------------------------------------------------------------}
destructor TRVBorder.Destroy;
begin
  FBorderOffsets.Free;
  FVisibleBorders.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVBorder.Assign(Source: TPersistent);
begin
  if Source is TRVBorder then begin
    Width := TRVBorder(Source).Width;
    Style := TRVBorder(Source).Style;
    Color := TRVBorder(Source).Color;
    InternalWidth := TRVBorder(Source).InternalWidth;
    VisibleBorders.Assign(TRVBorder(Source).VisibleBorders);
    BorderOffsets.Assign(TRVBorder(Source).BorderOffsets);
    end
  else
    inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
procedure TRVBorder.SetBorderOffsets(const Value: TRVRect);
begin
  FBorderOffsets.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TRVBorder.SetVisibleBorders(const Value: TRVBooleanRect);
begin
  FVisibleBorders.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TRVBorder.DrawSaD(Rect: TRect; Canvas: TCanvas;
  const sad: TRVScreenAndDevice; ColorMode: TRVColorMode);
begin
  if Style = rvbNone then exit;
  ScaleRect(Rect, sad);
  BorderOffsets.InflateRectSaD(Rect,sad);
  DoDraw(Rect, Canvas,
         MulDiv(Width,         sad.ppiyDevice, sad.ppiyScreen),
         MulDiv(InternalWidth, sad.ppiyDevice, sad.ppiyScreen),
         MulDiv(1, sad.ppiyDevice, sad.ppiyScreen), ColorMode);
end;
{------------------------------------------------------------------------------}
procedure TRVBorder.Draw(Rect: TRect; Canvas: TCanvas);
begin
  if Style = rvbNone then exit;
  BorderOffsets.InflateRect(Rect);
  DoDraw(Rect, Canvas, Width, InternalWidth, 1, rvcmColor);
end;
{------------------------------------------------------------------------------}
procedure TRVBorder.DoDraw(Rect: TRect; Canvas: TCanvas;
  Width, InternalWidth, OnePixelWidth: Integer; ColorMode: TRVColorMode);
var Count: Integer;
begin
  with Canvas.Pen do begin
    Width := Self.Width;
    Style := psInsideFrame;
    case ColorMode of
      rvcmColor:
        Color := Self.Color;
      rvcmPrinterColor:
        Color := RV_GetPrnColor(Self.Color);
      rvcmGrayScale:
        Color := RV_GetGray(RV_GetPrnColor(Self.Color));
      rvcmBlackAndWhite,  rvcmBlackOnWhite:
        Color := clBlack;
    end;
  end;
  case Style of
    rvbSingle:
      Count := 1;
    rvbDouble, rvbThickInside, rvbThickOutside:
      Count := 2;
    rvbTriple:
      Count := 3;
    else
      Count := 1;
  end;
  while Count>0 do begin
    if ((Count=1) and (Style=rvbThickOutside)) or
       ((Count=2) and (Style=rvbThickInside)) then
      Canvas.Pen.Width := Width*2
    else
      Canvas.Pen.Width := Width;
    if VisibleBorders.Top then begin
      Canvas.MoveTo(Rect.Left,Rect.Top);
      Canvas.LineTo(Rect.Right,Rect.Top);
      Canvas.MoveTo(Rect.Right,Rect.Top);
      Canvas.LineTo(Rect.Left,Rect.Top);
    end;
    if VisibleBorders.Right then begin
      Canvas.MoveTo(Rect.Right,Rect.Top);
      Canvas.LineTo(Rect.Right,Rect.Bottom);
      Canvas.MoveTo(Rect.Right,Rect.Bottom);
      Canvas.LineTo(Rect.Right,Rect.Top);
    end;
    if VisibleBorders.Bottom then begin
      Canvas.MoveTo(Rect.Right,Rect.Bottom);
      Canvas.LineTo(Rect.Left,Rect.Bottom);
      Canvas.MoveTo(Rect.Left,Rect.Bottom);
      Canvas.LineTo(Rect.Right,Rect.Bottom);
    end;
    if VisibleBorders.Left then begin
      Canvas.MoveTo(Rect.Left,Rect.Bottom);
      Canvas.LineTo(Rect.Left,Rect.Top);
      Canvas.MoveTo(Rect.Left,Rect.Top);
      Canvas.LineTo(Rect.Left,Rect.Bottom);
    end;
    InflateRect(Rect, InternalWidth+OnePixelWidth, InternalWidth+OnePixelWidth);
    if (Width=1) and (Style=rvbThickOutside) then begin
      inc(Rect.Bottom,OnePixelWidth);
      inc(Rect.Right,OnePixelWidth);
    end;
    if (Width=1) and (Style = rvbThickInside) then begin
      dec(Rect.Top,OnePixelWidth);
      dec(Rect.Left,OnePixelWidth);
    end;
    dec(Count);
  end;
  Canvas.Pen.Width := 1;
end;
{------------------------------------------------------------------------------}
function TRVBorder.IsEqual(Value: TRVBorder): Boolean;
begin
  Result := (Style = Value.Style) and
            (Color = Value.Color) and
            (Width = Value.Width) and
            (InternalWidth = Value.InternalWidth) and
            BorderOffsets.IsEqual(Value.BorderOffsets) and
            VisibleBorders.IsEqual(Value.VisibleBorders);
end;
{------------------------------------------------------------------------------}
function TRVBorder.IsEqual_Para(Value: TRVBorder; IgnoreList: TRVParaInfoProperties): Boolean;
begin
  Result := ((rvpiBorder_Style in IgnoreList) or (Style = Value.Style)) and
            ((rvpiBorder_Color in IgnoreList) or (Color = Value.Color)) and
            ((rvpiBorder_Width in IgnoreList) or (Width = Value.Width)) and
            ((rvpiBorder_InternalWidth in IgnoreList) or (InternalWidth = Value.InternalWidth)) and
            BorderOffsets.IsEqualEx(Value.BorderOffsets,
              rvpiBorder_BO_Left in IgnoreList,
              rvpiBorder_BO_Top in IgnoreList,
              rvpiBorder_BO_Right in IgnoreList,
              rvpiBorder_BO_Bottom in IgnoreList) and
            VisibleBorders.IsEqualEx(Value.VisibleBorders,
              rvpiBorder_Vis_Left in IgnoreList,
              rvpiBorder_Vis_Top in IgnoreList,
              rvpiBorder_Vis_Right in IgnoreList,
              rvpiBorder_Vis_Bottom in IgnoreList);
end;
{------------------------------------------------------------------------------}
function TRVBorder.SimilarityValue(Value: TRVBorder): Integer;
var vis1,vis2: array[0..3] of Boolean;
    sum,i: Integer;
begin
  Result := 0;
  vis1[0] := ((Style<>rvbNone) and VisibleBorders.Left);
  vis2[0] := ((Value.Style<>rvbNone) and Value.VisibleBorders.Left);
  vis1[1] := ((Style<>rvbNone) and VisibleBorders.Top);
  vis2[1] := ((Value.Style<>rvbNone) and Value.VisibleBorders.Top);
  vis1[2] := ((Style<>rvbNone) and VisibleBorders.Right);
  vis2[2] := ((Value.Style<>rvbNone) and Value.VisibleBorders.Right);
  vis1[3] := ((Style<>rvbNone) and VisibleBorders.Bottom);
  vis2[3] := ((Value.Style<>rvbNone) and Value.VisibleBorders.Bottom);
  sum := 0;
  for i := 0 to 3 do begin
    inc(sum, ord(vis1[i] and vis2[i]));
  end;
  if sum>0 then begin
    Result := RV_CompareColors(Color, Value.Color, RVSMW_EACHRGBCOLOR, RVSMW_COLORSET)+
              RV_CompareInts(Width, Value.Width, RVSMW_WIDTH)+
              RV_CompareInts(InternalWidth, Value.InternalWidth, RVSMW_WIDTH);
    if Style = Value.Style then
      inc(Result, RVSMW_BORDERSTYLE);
    Result := Result * sum;
  end;
  for i := 0 to 3 do begin
    if not vis1[i] and not vis2[i] then
      inc(Result, RVSMW_BORDERNOSIDE);
    if vis1[i] <> vis2[i] then
      dec(Result, RVSMW_BORDERNOSIDE);
  end;
  if vis1[0] and vis2[0] then
    inc(Result, RV_CompareInts(BorderOffsets.Left, Value.BorderOffsets.Left, RVSMW_PADDING));
  if vis1[1] and vis2[1] then
    inc(Result, RV_CompareInts(BorderOffsets.Top, Value.BorderOffsets.Top, RVSMW_PADDING));
  if vis1[2] and vis2[2] then
    inc(Result, RV_CompareInts(BorderOffsets.Right, Value.BorderOffsets.Right, RVSMW_PADDING));
  if vis1[3] and vis2[3] then
    inc(Result, RV_CompareInts(BorderOffsets.Bottom, Value.BorderOffsets.Bottom, RVSMW_PADDING));
end;
{------------------------------------------------------------------------------}
function TRVBorder.GetTotalWidth: Integer;
begin
  case Style of
    rvbSingle:
      Result := Width;
    rvbDouble:
      Result := 2*Width+InternalWidth;
    rvbTriple:
      Result := 3*Width+2*InternalWidth;
    rvbThickInside, rvbThickOutside:
      Result := 3*Width+InternalWidth;
    else
      Result := 0;
  end;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEINI}
procedure TRVBorder.LoadFromINI(ini: TRVIniFile; const Section, fs: String);
begin
  Width := ini.ReadInteger(Section, Format(fs,[RVINI_WIDTH]), 1);
  Style := TRVBorderStyle(ini.ReadInteger(Section, Format(fs,[RVINI_STYLE]), ord(rvbNone)));
  Color := ini.ReadInteger(Section, Format(fs,[RVINI_COLOR]), clWindowText);
  InternalWidth := ini.ReadInteger(Section, Format(fs,[RVINI_INTERNALWIDTH]), 1);
  BorderOffsets.LoadFromINI(ini,  Section, Format(fs,[RVINI_BOFFSPREFIX]));
  VisibleBorders.LoadFromINI(ini, Section, Format(fs,[RVINI_VISBPREFIX]));
end;
{------------------------------------------------------------------------------}
procedure TRVBorder.SaveToINI(ini: TRVIniFile; const Section, fs: String);
begin
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_WIDTH]), Width, 1);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_STYLE]), ord(Style), ord(rvbNone));
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_COLOR]), Color, clWindowText);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_INTERNALWIDTH]), InternalWidth, 1);
  BorderOffsets.SaveToINI(ini,    Section, Format(fs,[RVINI_BOFFSPREFIX]));
  VisibleBorders.SaveToINI(ini, Section, Format(fs,[RVINI_VISBPREFIX]));
end;
{$ENDIF}
{============================== TRVBackgroundRect =============================}
constructor TRVBackgroundRect.Create;
begin
  inherited Create;
  FBorderOffsets := TRVRect.Create;
  Color := clNone
end;
{------------------------------------------------------------------------------}
destructor TRVBackgroundRect.Destroy;
begin
  FBorderOffsets.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVBackgroundRect.Assign(Source: TPersistent);
begin
  if Source is TRVBackgroundRect then begin
    Color := TRVBackgroundRect(Source).Color;
    BorderOffsets := TRVBackgroundRect(Source).BorderOffsets;
    end
  else
    inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
procedure TRVBackgroundRect.PrepareDraw(var Rect: TRect);
begin
  BorderOffsets.InflateRect(Rect);
end;
{------------------------------------------------------------------------------}
procedure TRVBackgroundRect.PrepareDrawSaD(var Rect: TRect; const sad: TRVScreenAndDevice);
begin
  BorderOffsets.InflateRectSaD(Rect,sad);
end;
{------------------------------------------------------------------------------}
procedure TRVBackgroundRect.Draw(Rect: TRect; Canvas: TCanvas;
  Printing: Boolean; ColorMode: TRVColorMode);
begin
  if (Color=clNone) or
    (Printing and (ColorMode in [rvcmBlackAndWhite, rvcmBlackOnWhite])) then
    exit;
  Canvas.Brush.Style := bsSolid;
  if ColorMode<>rvcmGrayScale then
    Canvas.Brush.Color := RV_GetPrnColor(Color)
  else
    Canvas.Brush.Color := RV_GetGray(RV_GetPrnColor(Color));
  Canvas.Pen.Style := psClear;
  inc(Rect.Right);
  inc(Rect.Bottom);
  Canvas.FillRect(Rect);
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsClear;
end;
{------------------------------------------------------------------------------}
procedure TRVBackgroundRect.SetBorderOffsets(const Value: TRVRect);
begin
  FBorderOffsets.Assign(Value);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEINI}
procedure TRVBackgroundRect.LoadFromINI(ini: TRVIniFile; const Section,
  fs: String);
begin
  Color := ini.ReadInteger(Section, Format(fs,[RVINI_COLOR]), clNone);
  BorderOffsets.LoadFromINI(ini,  Section, Format(fs,[RVINI_BOFFSPREFIX]));
end;
{------------------------------------------------------------------------------}
procedure TRVBackgroundRect.SaveToINI(ini: TRVIniFile; const Section,
  fs: String);
begin
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_COLOR]), Color, clNone);
  BorderOffsets.SaveToINI(ini,    Section, Format(fs,[RVINI_BOFFSPREFIX]));
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRVBackgroundRect.IsEqual(Value: TRVBackgroundRect): Boolean;
begin
  Result := (Color = Value.Color) and
            BorderOffsets.IsEqual(Value.BorderOffsets);
end;
{------------------------------------------------------------------------------}
function TRVBackgroundRect.SimilarityValue(
  Value: TRVBackgroundRect): Integer;
begin
  Result := RV_CompareColors(Color, Value.Color, RVSMW_EACHRGBBCOLOR, RVSMW_BCOLORSET)+
            BorderOffsets.SimilarityValue(Value.BorderOffsets, RVSMW_PADDING);
end;
{------------------------------------------------------------------------------}
function TRVBackgroundRect.IsEqual_Para(Value: TRVBackgroundRect;
  IgnoreList: TRVParaInfoProperties): Boolean;
begin
  Result := ((rvpiBackground_Color in IgnoreList) or (Color = Value.Color)) and
            BorderOffsets.IsEqualEx(Value.BorderOffsets,
            rvpiBackground_BO_Left in IgnoreList,
            rvpiBackground_BO_Top in IgnoreList,
            rvpiBackground_BO_Right in IgnoreList,
            rvpiBackground_BO_Bottom in IgnoreList);
end;
{================================== TParaInfo =================================}
constructor TParaInfo.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FirstIndent := 0;
  LeftIndent  := 0;
  RightIndent := 0;
  Alignment   := rvaLeft;
  FName       := RVDEFAULTPARASTYLENAME;
  FBorder     := TRVBorder.Create;
  FBackground := TRVBackgroundRect.Create;
  NextParaNo  := -1;
  DefStyleNo  := -1;
  LineSpacingType := rvlsPercent;
  LineSpacing := 100;
end;
{------------------------------------------------------------------------------}
destructor TParaInfo.Destroy;
begin
  FBorder.Free;
  FBackground.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TParaInfo.SetBorder(const Value: TRVBorder);
begin
  FBorder.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TParaInfo.SetBackground(const Value: TRVBackgroundRect);
begin
  FBackground.Assign(Value);
end;
{------------------------------------------------------------------------------}
function TParaInfo.ExtraLineSpacing: Boolean;
begin
  case LineSpacingType of
    rvlsPercent:
      Result := LineSpacing>100;
    rvlsSpaceBetween:
      Result := LineSpacing>0;
    else
      Result := False;
  end;
end;
{------------------------------------------------------------------------------}
procedure TParaInfo.Assign(Source: TPersistent);
begin
  if Source is TParaInfo then begin
    FirstIndent := TParaInfo(Source).FirstIndent;
    LeftIndent  := TParaInfo(Source).LeftIndent;
    RightIndent := TParaInfo(Source).RightIndent;
    Alignment   := TParaInfo(Source).Alignment;
    SpaceBefore := TParaInfo(Source).SpaceBefore;
    SpaceAfter  := TParaInfo(Source).SpaceAfter;
    LineSpacing := TParaInfo(Source).LineSpacing;
    LineSpacingType := TParaInfo(Source).LineSpacingType;
    Background  := TParaInfo(Source).Background;
    Border      := TParaInfo(Source).Border;
    Options     := TParaInfo(Source).Options;
    BiDiMode    := TParaInfo(Source).BiDiMode;
    NextParaNo  := TParaInfo(Source).NextParaNo;
    DefStyleNo  := TParaInfo(Source).DefStyleNo;
    inherited Assign(Source);
    end
  else
    inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
function TParaInfo.IsSimpleEqualEx(Value: TCustomRVInfo; Mapping: TRVIntegerList): Boolean;
begin
  Result := IsSimpleEqual(Value, True, False);
  if not Result then
    exit;
  Result := False;
  {
  if (Value.BaseStyleNo>=0) then begin
    if (Value.BaseStyleNo>=Mapping.Count) then
      Value.BaseStyleNo := -1 // fix up
    else if (Mapping[Value.BaseStyleNo]<>BaseStyleNo) then
      exit;
  end;
  }
  if (TParaInfo(Value).NextParaNo>=0) then begin
    if (TParaInfo(Value).NextParaNo>=Mapping.Count) then
      TParaInfo(Value).NextParaNo := -1 // fix up
    else if (Mapping[TParaInfo(Value).NextParaNo]<>NextParaNo) then
      exit;
  end;
  Result := True;
end;
{------------------------------------------------------------------------------}
function TParaInfo.IsSimpleEqual(Value: TCustomRVInfo;
  IgnoreReferences, IgnoreID: Boolean): Boolean;
begin
  Result :=
    (Alignment       = TParaInfo(Value).Alignment  ) and
    (FirstIndent     = TParaInfo(Value).FirstIndent) and
    (LeftIndent      = TParaInfo(Value).LeftIndent ) and
    (RightIndent     = TParaInfo(Value).RightIndent) and
    (SpaceBefore     = TParaInfo(Value).SpaceBefore) and
    (SpaceAfter      = TParaInfo(Value).SpaceAfter) and
    (LineSpacing     = TParaInfo(Value).LineSpacing) and
    (LineSpacingType = TParaInfo(Value).LineSpacingType) and
    (Options         = TParaInfo(Value).Options) and
    (BiDiMode        = TParaInfo(Value).BiDiMode) and
    (IgnoreReferences or (NextParaNo = TParaInfo(Value).NextParaNo)) and
    (IgnoreReferences or (DefStyleNo = TParaInfo(Value).DefStyleNo)) and    
    Background.IsEqual(TParaInfo(Value).Background) and
    Border.IsEqual(TParaInfo(Value).Border);
end;
{------------------------------------------------------------------------------}
function TParaInfo.SimilarityValue(Value: TCustomRVInfo): Integer;
begin
  Result :=
    RV_CompareInts(FirstIndent, TParaInfo(Value).FirstIndent, RVSMW_INDENT)+
    RV_CompareInts(LeftIndent,  TParaInfo(Value).LeftIndent,  RVSMW_INDENT)+
    RV_CompareInts(RightIndent, TParaInfo(Value).RightIndent, RVSMW_INDENT)+
    RV_CompareInts(SpaceBefore, TParaInfo(Value).SpaceBefore, RVSMW_INDENT)+
    RV_CompareInts(SpaceAfter, TParaInfo(Value).SpaceAfter, RVSMW_INDENT)+
    Background.SimilarityValue(TParaInfo(Value).Background)+
    Border.SimilarityValue(TParaInfo(Value).Border);
  if (Alignment = TParaInfo(Value).Alignment) then
    inc(Result, RVSMW_ALIGNMENT);
  if (BiDiMode = TParaInfo(Value).BiDiMode) then
    inc(Result, RVSMW_BIDIMODE);
  if ((rvpaoNoWrap in Options) = (rvpaoNoWrap in TParaInfo(Value).Options)) then
    inc(Result, RVSMW_NOWRAP);
  if ((rvpaoReadOnly in Options) = (rvpaoReadOnly in TParaInfo(Value).Options)) then
    inc(Result, RVSMW_READONLY);
  if ((rvpaoStyleProtect in Options) = (rvpaoStyleProtect in TParaInfo(Value).Options)) then
    inc(Result, RVSMW_STYLEPROTECT);
  if ((rvpaoDoNotWantReturns in Options) = (rvpaoDoNotWantReturns in TParaInfo(Value).Options)) then
    inc(Result, RVSMW_DONOTWANTRETURNS);
  if ((rvpaoKeepLinesTogether in Options) = (rvpaoKeepLinesTogether in TParaInfo(Value).Options)) then
    inc(Result, RVSMW_KEEPLINESTOGETHER);
  if ((rvpaoKeepWithNext in Options) = (rvpaoKeepWithNext in TParaInfo(Value).Options)) then
    inc(Result, RVSMW_KEEPWITHNEXT);
  if (LineSpacingType=TParaInfo(Value).LineSpacingType) then
    inc(Result, RV_CompareInts(LineSpacing, TParaInfo(Value).LineSpacing, RVSMW_LINESPACING))
  else if ExtraLineSpacing<>TParaInfo(Value).ExtraLineSpacing then
    dec(Result, RVSMW_LINESPACING*4);
end;
{------------------------------------------------------------------------------}
function TParaInfo.IsEqual(Value: TParaInfo;
  IgnoreList: TRVParaInfoProperties): Boolean;
begin
  Result :=
    ((rvpiAlignment       in IgnoreList) or (Alignment       = Value.Alignment)) and
    ((rvpiFirstIndent     in IgnoreList) or (FirstIndent     = Value.FirstIndent)) and
    ((rvpiLeftIndent      in IgnoreList) or (LeftIndent      = Value.LeftIndent)) and
    ((rvpiRightIndent     in IgnoreList) or (RightIndent     = Value.RightIndent)) and
    ((rvpiSpaceBefore     in IgnoreList) or (SpaceBefore     = Value.SpaceBefore)) and
    ((rvpiSpaceAfter      in IgnoreList) or (SpaceAfter     = Value.SpaceAfter)) and
    ((rvpiLineSpacing     in IgnoreList) or (LineSpacing     = Value.LineSpacing)) and
    ((rvpiLineSpacingType in IgnoreList) or (LineSpacingType = Value.LineSpacingType)) and
    ((rvpiNoWrap          in IgnoreList) or ((rvpaoNoWrap in Options) = (rvpaoNoWrap in TParaInfo(Value).Options))) and
    ((rvpiReadOnly        in IgnoreList) or ((rvpaoReadOnly in Options) = (rvpaoReadOnly in TParaInfo(Value).Options))) and
    ((rvpiStyleProtect    in IgnoreList) or ((rvpaoStyleProtect in Options) = (rvpaoStyleProtect in TParaInfo(Value).Options))) and
    ((rvpiDoNotWantReturns in IgnoreList) or ((rvpaoDoNotWantReturns in Options) = (rvpaoDoNotWantReturns in TParaInfo(Value).Options))) and
    ((rvpiKeepLinesTogether in IgnoreList) or ((rvpaoKeepLinesTogether in Options) = (rvpaoKeepLinesTogether in TParaInfo(Value).Options))) and
    ((rvpiKeepWithNext in IgnoreList) or ((rvpaoKeepWithNext in Options) = (rvpaoKeepWithNext in TParaInfo(Value).Options))) and    
    ((rvpiBiDiMode        in IgnoreList) or (BiDiMode        = Value.BiDiMode)) and
    ((rvpiNextParaNo      in IgnoreList) or (NextParaNo      = Value.NextParaNo)) and
    ((rvpiDefStyleNo      in IgnoreList) or (DefStyleNo      = Value.DefStyleNo)) and    
    Background.IsEqual_Para(Value.Background, IgnoreList) and
    Border.IsEqual_Para(Value.Border, IgnoreList);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEINI}
procedure TParaInfo.LoadFromINI(ini: TRVIniFile; const Section, fs: String);
begin
  inherited LoadFromINI(ini, Section, fs, RVDEFAULTPARASTYLENAME);
  SpaceBefore := ini.ReadInteger(Section,  Format(fs,[RVINI_SPACEBEFORE]),  0);
  SpaceAfter  := ini.ReadInteger(Section,  Format(fs,[RVINI_SPACEAFTER]),  0);
  LeftIndent  := ini.ReadInteger(Section,  Format(fs,[RVINI_LEFTINDENT]),  0);
  RightIndent := ini.ReadInteger(Section,  Format(fs,[RVINI_RIGHTIDENT]),  0);
  FirstIndent := ini.ReadInteger(Section,  Format(fs,[RVINI_FIRSTINDENT]), 0);
  LineSpacing := ini.ReadInteger(Section,  Format(fs,[RVINI_LINESPACING]), 100);
  LineSpacingType := TRVLineSpacingType(ini.ReadInteger(Section,  Format(fs,[RVINI_LINESPACINGTYPE]), ord(rvlsPercent)));
  NextParaNo  := ini.ReadInteger(Section,  Format(fs,[RVINI_NEXTPARANO]), -1);
  DefStyleNo  := ini.ReadInteger(Section,  Format(fs,[RVINI_DEFSTYLENO]), -1);  
  Alignment   := TRVAlignment(ini.ReadInteger(Section, Format(fs,[RVINI_ALIGNMENT]), ord(rvaLeft)));
  BiDiMode    := TRVBiDiMode(ini.ReadInteger(Section, Format(fs,[RVINI_BIDIMODE]), 0));
  Options := [];
  if IniReadBool(ini, Section, Format(fs,[RVINI_NOWRAP]), False) then
    Include(FOptions, rvpaoNoWrap);
  if IniReadBool(ini, Section, Format(fs,[RVINI_READONLY]), False) then
    Include(FOptions, rvpaoReadOnly);
  if IniReadBool(ini, Section, Format(fs,[RVINI_STYLEPROTECT]), False) then
    Include(FOptions, rvpaoStyleProtect);
  if IniReadBool(ini, Section, Format(fs,[RVINI_DONOTWANTRETURNS]), False) then
    Include(FOptions, rvpaoDoNotWantReturns);
  if IniReadBool(ini, Section, Format(fs,[RVINI_KEEPLINESTOGETHER]), False) then
    Include(FOptions, rvpaoKeepLinesTogether);
  if IniReadBool(ini, Section, Format(fs,[RVINI_KEEPWITHNEXT]), False) then
    Include(FOptions, rvpaoKeepWithNext);
  Border.LoadFromINI(ini,  Section, Format(fs,[RVINI_BORDERPREFIX]));
  Background.LoadFromINI(ini,  Section, Format(fs,[RVINI_BACKGROUNDPREFIX]));
end;
{------------------------------------------------------------------------------}
procedure TParaInfo.SaveToINI(ini: TRVIniFile; const Section, fs: String);
begin
  inherited SaveToINI(ini, Section, fs);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_SPACEBEFORE]), SpaceBefore, 0);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_SPACEAFTER]),  SpaceAfter,  0);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_LEFTINDENT]),  LeftIndent,  0);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_RIGHTIDENT]),  RightIndent, 0);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_FIRSTINDENT]), FirstIndent, 0);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_LINESPACING]), LineSpacing, 100);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_LINESPACINGTYPE]), ord(LineSpacingType), ord(rvlsPercent));
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_NEXTPARANO]), NextParaNo, -1);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_DEFSTYLENO]), DefStyleNo, -1);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_ALIGNMENT]),   ord(Alignment), ord(rvaLeft));
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_BIDIMODE]),   ord(BiDiMode), 0);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_NOWRAP]),  rvpaoNoWrap in Options, False);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_READONLY]),  rvpaoReadOnly in Options, False);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_STYLEPROTECT]),  rvpaoStyleProtect in Options, False);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_DONOTWANTRETURNS]), rvpaoDoNotWantReturns in Options, False);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_KEEPLINESTOGETHER]), rvpaoKeepLinesTogether in Options, False);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_KEEPWITHNEXT]), rvpaoKeepWithNext in Options, False);  
  Border.SaveToINI(ini,  Section, Format(fs,[RVINI_BORDERPREFIX]));
  Background.SaveToINI(ini,  Section, Format(fs,[RVINI_BACKGROUNDPREFIX]));
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TParaInfo.SaveCSSToStream(Stream: TStream; BaseStyle: TParaInfo;
  Multiline, IgnoreLeftAlignment, IgnoreLeftIndents: Boolean);
const cssTextAlign  : array[TRVAlignment] of String =
  ('left', 'right', 'center', 'justify');
    {..................................................}
    function GetBorderStyle(bs: TRVBorderStyle): String;
    begin
      Result := '';
      case bs of
        rvbNone:
          Result := 'none';
        rvbSingle:
          Result := 'solid';
        rvbDouble, rvbTriple, rvbThickInside, rvbThickOutside:
          Result := 'double';
      end;
    end;
    {..................................................}
    function GetBorderWidth(Border: TRVBorder): Integer;
    begin
      Result := 0;
      case Border.Style of
        rvbSingle:
          Result := Border.Width;
        rvbDouble:
          Result := Border.Width+Border.InternalWidth;
        rvbThickInside, rvbThickOutside:
          Result := Border.Width*3 div 2+Border.InternalWidth;
        rvbTriple:
          Result := Border.Width+Border.InternalWidth*2;
      end;
    end;
    {..................................................}
var r, baser: TRect;
begin
  if ((BaseStyle=nil) and (not IgnoreLeftAlignment or (Alignment<>rvaLeft))) or
     ((BaseStyle<>nil) and (BaseStyle.Alignment<>Alignment)) then
    RVWriteX(Stream, ' text-align: '+cssTextAlign[Alignment]+';', Multiline);
  if not IgnoreLeftIndents and ((BaseStyle=nil) or (BaseStyle.FirstIndent<>FirstIndent)) then
    RVWriteX(Stream, Format(' text-indent: %dpx;', [FirstIndent]), Multiline);
  if ((BaseStyle=nil) and (LineSpacingType=rvlsPercent) and (LineSpacing<>100)) or
     ((BaseStyle<>nil) and (LineSpacingType=rvlsPercent) and
      (BaseStyle.LineSpacingType=rvlsPercent) and
      (LineSpacing<>BaseStyle.LineSpacing)) then
    RVWriteX(Stream, Format(' line-height: %d.%d;',[LineSpacing div 100, LineSpacing mod 100]), Multiline)
  else if (BaseStyle<>nil) and (BaseStyle.LineSpacingType=rvlsPercent) and
     (BaseStyle.LineSpacing<>100) and (LineSpacingType<>rvlsPercent) then
    RVWriteX(Stream, ' line-height: normal;', Multiline);
  if rvpaoNoWrap in Options then
    RVWriteX(Stream, ' white-space: nowrap;', Multiline)
  else if (BaseStyle<>nil) and not (rvpaoNoWrap in BaseStyle.Options) then
    RVWriteX(Stream, ' white-space: normal;', Multiline);
  if rvpaoKeepLinesTogether in Options then
    RVWriteX(Stream, ' page-break-inside: avoid;', Multiline)
  else if (BaseStyle<>nil) and (rvpaoKeepLinesTogether in BaseStyle.Options) then
    RVWriteX(Stream, ' page-break-inside: auto;', Multiline);
  if rvpaoKeepWithNext in Options then
    RVWriteX(Stream, ' page-break-after: avoid;', Multiline)
  else if (BaseStyle<>nil) and (rvpaoKeepWithNext in BaseStyle.Options) then
    RVWriteX(Stream, ' page-break-after: auto;', Multiline);
  if (Border.Style <> rvbNone) and (Border.Color<>clNone) then begin
    RVWriteX(Stream, Format(' border-color: %s;', [RV_GetHTMLRGBStr(Border.Color, False)]),
      Multiline);
    RVWriteX(Stream, Format(' border-style: %s;', [GetBorderStyle(Border.Style)]),
      Multiline);
    RVWriteX(Stream, Format(' border-width: %dpx;', [GetBorderWidth(Border)]),
      Multiline);
    if not Border.VisibleBorders.Top then
      RVWriteX(Stream, ' border-top: none;', Multiline);
    if not Border.VisibleBorders.Right then
      RVWriteX(Stream, ' border-right: none;', Multiline);
    if not Border.VisibleBorders.Bottom then
      RVWriteX(Stream, ' border-bottom: none;', Multiline);
    if not Border.VisibleBorders.Left then
      RVWriteX(Stream, ' border-left: none;', Multiline);
    Border.BorderOffsets.AssignToRect(r);
    end
  else begin
    if (BaseStyle<>nil) and (BaseStyle.Border.Style <> rvbNone) and
       (BaseStyle.Border.Color<>clNone) then
      RVWriteX(Stream, ' border: none;', Multiline);
    r := Rect(0,0,0,0);
    //RVWriteX(Stream, ' border: none;', Multiline);
  end;
  if (BaseStyle<>nil) and (BaseStyle.Border.Style <> rvbNone) and
    (BaseStyle.Border.Color<>clNone) then
    BaseStyle.Border.BorderOffsets.AssignToRect(baser)
  else
    baser := Rect(0,0,0,0);
  if ((BaseStyle=nil) and (Background.Color<>clNone)) or
     ((BaseStyle<>nil) and (Background.Color<>BaseStyle.Background.Color)) then
    RVWriteX(Stream,
      Format(' background: %s;', [RV_GetCSSBkColor(Background.Color)]), Multiline);
  if Background.Color<>clNone then
    Background.BorderOffsets.AssignToRectIfGreater(r);
  if (BaseStyle=nil) or not AreRectsEqual(baser,r) then
    with r do
      RVWriteX(Stream, Format(' padding: %dpx %dpx %dpx %dpx;',
        [Top, Right, Bottom, Left]), Multiline);
  if (BaseStyle<>nil) then begin
    baser.Left   := BaseStyle.LeftIndent-baser.Left;
    baser.Right  := BaseStyle.RightIndent-baser.Right;
    baser.Top    := BaseStyle.SpaceBefore-baser.Top;
    baser.Bottom := BaseStyle.SpaceAfter-baser.Bottom;
  end;
  r.Left   := LeftIndent-r.Left;
  r.Right  := RightIndent-r.Right;
  r.Top    := SpaceBefore-r.Top;
  r.Bottom := SpaceAfter-r.Bottom;
  if (BaseStyle=nil) or not AreRectsEqual(baser,r) then
    with r do
      if not IgnoreLeftIndents then
        RVWriteX(Stream, Format(' margin: %dpx %dpx %dpx %dpx;',
          [Top, Right, Bottom, Left]), Multiline)
      else begin
        RVWriteX(Stream, Format(' margin-top: %dpx;', [Top]), Multiline);
        RVWriteX(Stream, Format(' margin-right: %dpx;', [Right]), Multiline);
        RVWriteX(Stream, Format(' margin-bottom: %dpx;', [Bottom]), Multiline);
      end;
end;
{============================== TParaInfos ====================================}
procedure TParaInfos.AssignTo(Dest: TPersistent);
var i: Integer;
begin
  if Dest is TStrings then begin
    TStrings(Dest).Clear;
    for i:=0 to Count-1 do
      TStrings(Dest).Add(Items[i].FName);
    end
  else
    inherited AssignTo(Dest);
end;
{------------------------------------------------------------------------------}
function TParaInfos.Add: TParaInfo;
begin
  Result := TParaInfo(inherited Add);
end;
{------------------------------------------------------------------------------}
function TParaInfos.GetItem(Index: Integer): TParaInfo;
begin
  if (Index<0) or (Index>=Count) then
    Result := InvalidItem
  else
    Result := TParaInfo(inherited GetItem(Index));
end;
{------------------------------------------------------------------------------}
procedure TParaInfos.SetItem(Index: Integer; Value: TParaInfo);
begin
  inherited SetItem(Index, Value);
end;
{------------------------------------------------------------------------------}
function TParaInfos.GetInvalidItem: TParaInfo;
begin
  if FInvalidItem=nil then begin
    FInvalidItem := (FOwner as TRVStyle).GetParaStyleClass.Create(nil);
    if Count>0 then
      FInvalidItem.Assign(Items[0]);
    FInvalidItem.SpaceBefore :=1;
    FInvalidItem.SpaceAfter :=1;
    FInvalidItem.LeftIndent :=1;
    FInvalidItem.RightIndent :=1;
    FInvalidItem.Border.Color := clRed;
    FInvalidItem.Border.Style := rvbSingle;
    FInvalidItem.Border.Width := 2;
    FInvalidItem.Border.BorderOffsets.SetAll(1);
  end;
  Result := FInvalidItem;
end;
{------------------------------------------------------------------------------}
procedure TParaInfos.SetInvalidItem(const Value: TParaInfo);
begin
  if InvalidItem<>Value then
    InvalidItem.Assign(Value);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEINI}
procedure TParaInfos.LoadFromINI(ini: TRVIniFile; const Section: String);
var i, cnt: Integer;
begin
  cnt := ini.ReadInteger(Section, RVINI_PARASTYLECOUNT, 2);
  Clear;
  for i:=0 to cnt-1 do begin
    Add;
    Items[i].LoadFromINI(ini, Section, RVINI_PARASTYLEPREFIX+IntToStr(i));
  end;
end;
{------------------------------------------------------------------------------}
procedure TParaInfos.SaveToINI(ini: TRVIniFile; const Section: String);
var i: Integer;
begin
  ini.WriteInteger(Section,RVINI_PARASTYLECOUNT, Count);
  for i:=0 to Count-1 do
    Items[i].SaveToINI(ini, Section, RVINI_PARASTYLEPREFIX+IntToStr(i));
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TParaInfos.FindStyleWithAlignment(BaseStyle: Integer;
  Alignment: TRVAlignment): Integer;
var i: Integer;
begin
  if Items[BaseStyle].Alignment = Alignment then begin
    Result := BaseStyle;
    exit;
  end;
  for i := 0 to Count-1 do
    if (i<>BaseStyle) and (Items[i].Alignment=Alignment) and
       Items[BaseStyle].IsEqual(Items[i], [rvpiAlignment]) then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;
{------------------------------------------------------------------------------}
function TParaInfos.FindSuchStyle(BaseStyle: Integer; Style: TParaInfo;
  Mask: TRVParaInfoProperties): Integer;
var i: Integer;
begin
  Mask := RVAllParaInfoProperties - Mask;
  if Style.IsEqual(Items[BaseStyle], Mask) then begin
    Result := BaseStyle;
    exit;
  end;
  for i := 0 to Count-1 do
    if (i<>BaseStyle) and Style.IsEqual(Items[i], Mask) then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;
{============================== TRVMarkerFont =================================}
constructor TRVMarkerFont.Create;
begin
  inherited Create;
  Name := RVDEFAULTSTYLEFONT;
  Size := 8;
end;
{------------------------------------------------------------------------------}
function TRVMarkerFont.IsEqual(Font: TFont): Boolean;
begin
  Result :=
    (Height=Font.Height) and
    (Style=Font.Style) and
    (Color=Font.Color) and
    {$IFDEF RICHVIEWCBDEF3}
    (Charset=Font.Charset) and
    {$ENDIF}
    (AnsiCompareText(Name, Font.Name)=0);
end;
{------------------------------------------------------------------------------}
function TRVMarkerFont.IsDefault: Boolean;
begin
  Result :=
    (Size=8) and
    (Color=clWindowText) and
    {$IFDEF RICHVIEWCBDEF3}
    (Charset=DEFAULT_CHARSET) and
    {$ENDIF}
    (Style=[]) and
    (AnsiCompareText(Name,RVDEFAULTSTYLEFONT)=0);
end;
{------------------------------------------------------------------------------}
function TRVMarkerFont.StoreName: Boolean;
begin
  Result := Name<>RVDEFAULTSTYLEFONT;
end;
{------------------------------------------------------------------------------}
function TRVMarkerFont.StoreHeight: Boolean;
begin
  Result := Size<>8;
end;
{============================== TRVListLevel ==================================}
constructor TRVListLevel.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFirstIndent := 10;
  FStartFrom   := 1;
  FFormatString  := #$B7;
  FOptions     := [rvloContinuous, rvloLevelReset];
end;
{------------------------------------------------------------------------------}
destructor TRVListLevel.Destroy;
begin
  FPicture.Free;
  FFont.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVListLevel.Assign(Source: TPersistent);
begin
  if Source is TRVListLevel then begin
    ListType        := TRVListLevel(Source).ListType;
    StartFrom       := TRVListLevel(Source).StartFrom;
    ImageList       := TRVListLevel(Source).ImageList;
    ImageIndex      := TRVListLevel(Source).ImageIndex;
    FormatString    := TRVListLevel(Source).FormatString;
    {$IFNDEF RVDONOTUSEUNICODE}
    {$IFDEF RICHVIEWCBDEF3}
    FormatStringW   := TRVListLevel(Source).FormatStringW;
    {$ENDIF}
    {$ENDIF}
    LeftIndent      := TRVListLevel(Source).LeftIndent;
    FirstIndent     := TRVListLevel(Source).FirstIndent;
    MarkerIndent    := TRVListLevel(Source).MarkerIndent;
    MarkerAlignment := TRVListLevel(Source).MarkerAlignment;
    Picture         := TRVListLevel(Source).FPicture;
    Font            := TRVListLevel(Source).FFont;
    Options         := TRVListLevel(Source).Options;
    end
  else
    inherited Assign(Source);
end;
{------------------------------------------------------------------------------}

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
        SHR     ECX,1
        SHR     ECX,1
        REPE    CMPSD
        JNE     @@2
        MOV     ECX,EDX
        REPE    CMPSB
        JNE     @@2
        INC     EAX
@@2:    POP     EDI
        POP     ESI
end;
{$ENDIF}

{------------------------------------------------------------------------------}
function ArePicturesEqual(FPicture1, FPicture2: TPicture): Boolean;
var Stream1, Stream2: TMemoryStream;
begin
  Result := ((FPicture1=nil) or (FPicture1.Graphic=nil)) =
            ((FPicture2=nil) or (FPicture2.Graphic=nil));

  if not Result then
    exit;
  if (FPicture1=nil) or (FPicture2.Graphic=nil) then
    exit;
  Result := FPicture1.ClassType=FPicture2.ClassType;
  if not Result then
    exit;
  Result := (FPicture1.Width=FPicture2.Width) and
            (FPicture1.Height=FPicture2.Height);
  if not Result then
    exit;
  Stream1 := TMemoryStream.Create;
  Stream2 := TMemoryStream.Create;
  try
    FPicture1.Graphic.SaveToStream(Stream1);
    FPicture2.Graphic.SaveToStream(Stream2);
    Result := (Stream1.Size=Stream2.Size) and
      CompareMem(Stream1.Memory,Stream2.Memory,Stream1.Size);
  finally
    Stream1.Free;
    Stream2.Free;
  end;
end;
{------------------------------------------------------------------------------}
function TRVListLevel.IsSimpleEqual(Value: TRVListLevel): Boolean;
begin
  Result :=
    (ListType = Value.ListType) and
    (StartFrom = Value.StartFrom) and
    (ImageList = Value.ImageList) and
    (ImageIndex = Value.ImageIndex) and
    (FormatString = Value.FormatString) and
    {$IFNDEF RVDONOTUSEUNICODE}
    {$IFDEF RICHVIEWCBDEF3}
    (FormatStringW = Value.FormatStringW) and
    {$ENDIF}
    {$ENDIF}
    (LeftIndent = Value.LeftIndent) and
    (FirstIndent = Value.FirstIndent) and
    (MarkerIndent = Value.MarkerIndent) and
    (MarkerAlignment = Value.MarkerAlignment) and
    (
      ((FFont=nil) or (FFont.IsDefault)) and ((Value.FFont=nil) or (Value.FFont.IsDefault)) or
      ((FFont<>nil) and (Value.FFont<>nil) and FFont.IsEqual(Value.FFont))
    ) and
    (Options = Value.Options) and
    ArePicturesEqual(FPicture, Value.FPicture);
end;
{------------------------------------------------------------------------------}
function TRVListLevel.SimilarityValue(Value: TRVListLevel): Integer;
begin
  Result := 0;
  if ListType=Value.ListType then
    inc(Result, RVMW_LISTTYPE);
  if StartFrom=Value.StartFrom then
    inc(Result, RVMW_LISTMISC);
  if ImageList=Value.ImageList then
    inc(Result, RVMW_LISTMISC);
  if ImageIndex=Value.ImageIndex then
    inc(Result, RVMW_LISTMISC);
  if FormatString=Value.FormatString then
    inc(Result, RVMW_LISTMISC);
  {$IFNDEF RVDONOTUSEUNICODE}
  {$IFDEF RICHVIEWCBDEF3}
  if FormatStringW=Value.FormatStringW then
    inc(Result, RVMW_LISTMISC);
  {$ENDIF}
  {$ENDIF}
  if LeftIndent=Value.LeftIndent then
    inc(Result, RVMW_LISTMISC);
  if FirstIndent=Value.FirstIndent then
    inc(Result, RVMW_LISTMISC);
  if FirstIndent=Value.FirstIndent then
    inc(Result, RVMW_LISTMISC);
  if MarkerIndent=Value.MarkerIndent then
    inc(Result, RVMW_LISTMISC);
  if MarkerAlignment=Value.MarkerAlignment then
    inc(Result, RVMW_LISTMISC);
  if Options=Value.Options then
    inc(Result, RVMW_LISTMISC);
  if ((FFont=nil) or (FFont.IsDefault)) and ((Value.FFont=nil) or (Value.FFont.IsDefault)) or
      ((FFont<>nil) and (Value.FFont<>nil) and FFont.IsEqual(Value.FFont)) then
    inc(Result, RVMW_LISTMISC);
  if ArePicturesEqual(FPicture, Value.FPicture) then
    inc(Result, RVMW_LISTMISC);
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
function TRVListLevel.GetDisplayName: String;
begin
  Result := Format(RVLISTLEVELDISPLAYNAME, [Index]);
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRVListLevel.GetPicture: TPicture;
begin
  if FPicture=nil then
    FPicture := TPicture.Create;
  Result := FPicture;
end;
{------------------------------------------------------------------------------}
procedure TRVListLevel.SetPicture(const Value: TPicture);
begin
  if Value<>FPicture then begin
    if (Value=nil) or (Value.Graphic=nil) then begin
      FPicture.Free;
      FPicture := nil;
      end
    else begin
      GetPicture.Assign(Value);
      {$IFDEF RICHVIEWDEF3}
      FPicture.Graphic.Transparent := True;
      {$ENDIF}
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVListLevel.StorePicture: Boolean;
begin
  Result := FPicture<>nil;
end;
{------------------------------------------------------------------------------}
function TRVListLevel.HasPicture: Boolean;
begin
  Result := (FPicture<>nil) and (FPicture.Graphic<>nil);
end;
{------------------------------------------------------------------------------}
function TRVListLevel.UsesFont: Boolean;
begin
  Result := ListType in [rvlstBullet,
                 rvlstDecimal, rvlstLowerAlpha, rvlstUpperAlpha,
                 rvlstLowerRoman, rvlstUpperRoman
                 {$IFNDEF RVDONOTUSEUNICODE}, rvlstUnicodeBullet{$ENDIF} ];
end;
{------------------------------------------------------------------------------}
function TRVListLevel.HasNumbering: Boolean;
begin
  Result := ListType in [rvlstDecimal, rvlstLowerAlpha, rvlstUpperAlpha,
                 rvlstLowerRoman, rvlstUpperRoman, rvlstImageListCounter];
end;
{------------------------------------------------------------------------------}
function TRVListLevel.HasVariableWidth: Boolean;
begin
  Result := ListType in [rvlstDecimal, rvlstLowerAlpha, rvlstUpperAlpha,
                 rvlstLowerRoman, rvlstUpperRoman];
end;
{------------------------------------------------------------------------------}
function TRVListLevel.GetHTMLOpenTagForCSS: String;
begin
  if HasNumbering then
    Result := 'OL'
  else
    Result := 'UL';
end;
{------------------------------------------------------------------------------}
function TRVListLevel.GetIndentCSSForTextVersion: String;
begin
  Result := Format('text-indent: %dpx; margin-left: %dpx;',
    [MarkerIndent-LeftIndent, LeftIndent]);
end;
{------------------------------------------------------------------------------}
procedure TRVListLevel.HTMLOpenTag(Stream: TStream; UseCSS: Boolean);
  {..............................................}
  function GetListType: String;
  begin
    case ListType of
      rvlstLowerAlpha:
        Result := 'a';
      rvlstUpperAlpha:
        Result := 'A';
      rvlstLowerRoman:
        Result := 'i';
      rvlstUpperRoman:
        Result := 'I';
      else
        Result := '';
    end;
    if Result<>'' then
      Result := ' type='+Result;
  end;
  {..............................................}
var CSS: String;
begin
  if UseCSS then begin
    if MarkerIndent>=LeftIndent then
      CSS := Format('text-indent: %dpx; margin-left: %dpx; list-style:inside;',
        [MarkerIndent-LeftIndent, LeftIndent])
    else
      CSS := Format('text-indent: %dpx; margin-left: %dpx; list-style:outside;',
        [FirstIndent, LeftIndent]);
    CSS := ' style="'+CSS+'"';
    end
  else
    CSS := '';
  if HasNumbering then
    RVWrite(Stream,Format('<OL%s%s>',[GetListType,CSS]))
  else
    RVWrite(Stream,Format('<UL%s>',[CSS]));
end;
{------------------------------------------------------------------------------}
procedure TRVListLevel.HTMLCloseTag(Stream: TStream; UseCSS: Boolean);
begin
  if HasNumbering then
    RVWrite(Stream,'</OL>')
  else
    RVWrite(Stream,'</UL>');
end;
{------------------------------------------------------------------------------}
function TRVListLevel.GetFont: TRVMarkerFont;
begin
  if FFont=nil then
    FFont := TRVMarkerFont.Create;
  Result := FFont;
end;
{------------------------------------------------------------------------------}
procedure TRVListLevel.SetFont(const Value: TRVMarkerFont);
begin
  if Value<>FFont then begin
    if (Value=nil) then begin
      FFont.Free;
      FFont := nil;
      end
    else
      GetFont.Assign(Value);
  end;
end;
{------------------------------------------------------------------------------}
function TRVListLevel.StoreFont: Boolean;
begin
  Result := FFont<>nil;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEINI}
procedure TRVListLevel.SaveToINI(ini: TRVIniFile; const Section, fs: String);
var Stream: TMemoryStream;
begin
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_LISTTYPE]), ord(ListType), ord(rvlstBullet));
  // ImageList ?
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_IMAGEINDEX]), ImageIndex, 0);
  ini.WriteString(Section,  Format(fs,[RVINI_FORMATSTRING]), FormatString);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_LEFTINDENT]), LeftIndent, 0);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_FIRSTINDENT]), FirstIndent, 10);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_MARKERINDENT]), MarkerIndent, 0);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_MARKERALIGNMENT]), ord(FMarkerAlignment), ord(rvmaLeft));
  if (FPicture<>nil) and (FPicture.Graphic<>nil) then begin
    ini.WriteString(Section,  Format(fs,[RVINI_GRAPHICCLASS]), FPicture.Graphic.ClassName);
    Stream :=  TMemoryStream.Create;
    FPicture.Graphic.SaveToStream(Stream);
    Stream.Position := 0;
    WriteLongStringToINI(ini, Section,  Format(fs,[RVINI_PICTURE]), RVFStream2TextString(Stream));
    Stream.Free;
  end;
  if FFont<>nil then
    ini.WriteString(Section,  Format(fs,[RVINI_FONT]), FontToString(FFont));
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_LOCONTINUOUS]), rvloContinuous in Options, True);
  WriteBoolToIniIfNE(ini, Section, Format(fs,[RVINI_LOLEVELRESET]), rvloLevelReset in Options, True);
  {$IFNDEF RVDONOTUSEUNICODE}
  {$IFDEF RICHVIEWCBDEF3}
  if FFormatStringW<>'' then begin
    Stream := TMemoryStream.Create;
    Stream.WriteBuffer(Pointer(FFormatStringW)^, Length(FFormatStringW)*2);
    Stream.Position := 0;
    ini.WriteString(Section,  Format(fs,[RVINI_FORMATSTRINGW]), RVFStream2TextString(Stream));
    Stream.Free;
  end;
  {$ENDIF}
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRVListLevel.LoadFromINI(ini: TRVIniFile; const Section, fs: String);
var s: String;
  Stream: TMemoryStream;
  GraphicClass: TGraphicClass;
  Graphic: TGraphic;
begin
  ListType := TRVListType(ini.ReadInteger(Section, Format(fs,[RVINI_LISTTYPE]), ord(rvlstBullet)));
  // ImageList ?
  ImageIndex := ini.ReadInteger(Section, Format(fs,[RVINI_IMAGEINDEX]), 0);
  FormatString := ini.ReadString(Section,  Format(fs,[RVINI_FORMATSTRING]), '');
  LeftIndent := ini.ReadInteger(Section, Format(fs,[RVINI_LEFTINDENT]), 0);
  FirstIndent := ini.ReadInteger(Section, Format(fs,[RVINI_FIRSTINDENT]), 10);
  MarkerIndent := ini.ReadInteger(Section, Format(fs,[RVINI_MARKERINDENT]), 0);
  FMarkerAlignment := TRVMarkerAlignment(ini.ReadInteger(Section, Format(fs,[RVINI_MARKERALIGNMENT]), ord(rvmaLeft)));
  s := ini.ReadString(Section,  Format(fs,[RVINI_GRAPHICCLASS]), '');
  GraphicClass := nil;
  if s<>'' then
    GraphicClass := TGraphicClass(GetClass(s));
  if GraphicClass=nil then
    Picture := nil
  else begin
    Graphic := RV_CreateGraphics(GraphicClass);
    Picture.Graphic := Graphic;
    Graphic.Free;
    Stream :=  TMemoryStream.Create;
    s := ReadLongStringFromINI(ini, Section,  Format(fs,[RVINI_PICTURE]));
    RVFTextString2Stream(s, Stream);
    Stream.Position := 0;
    try
      Picture.Graphic.LoadFromStream(Stream);
    except
      Picture := nil;
    end;
    Stream.Free;
  end;
  s := ini.ReadString(Section,  Format(fs,[RVINI_FONT]), '');
  if s='' then
    Font := nil
  else
    StringToFont(s, Font);
  FOptions := [];
  if IniReadBool(ini, Section, Format(fs,[RVINI_LOCONTINUOUS]), True) then
    Include(FOptions,rvloContinuous);
  if IniReadBool(ini, Section, Format(fs,[RVINI_LOLEVELRESET]), True) then
    Include(FOptions,rvloLevelReset);
  {$IFNDEF RVDONOTUSEUNICODE}
  {$IFDEF RICHVIEWCBDEF3}
  s := ini.ReadString(Section,  Format(fs,[RVINI_FORMATSTRINGW]), '');
  Stream := TMemoryStream.Create;
  RVFTextString2Stream(s, Stream);
  SetLength(FFormatStringW, Stream.Size div 2);
  Stream.Position := 0;
  Stream.ReadBuffer(Pointer(FFormatStringW)^, Stream.Size);
  Stream.Free;
  {$ENDIF}
  {$ENDIF}
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TRVListLevel.ImageListTagWriter(Writer: TWriter);
begin
  Writer.WriteInteger(FImageList.Tag);
end;
{------------------------------------------------------------------------------}
function TRVListLevel.GetRVFRVData: TPersistent;
begin
  if (Collection<>nil) and (TRVListLevelCollection(Collection).FOwner<>nil) and
     (TRVListInfo(TRVListLevelCollection(Collection).FOwner).Collection<>nil) then
    Result := TRVListInfos(TRVListInfo(TRVListLevelCollection(Collection).FOwner).Collection).FRVData
  else
    Result := nil;
end;
{------------------------------------------------------------------------------}
procedure TRVListLevel.ImageListTagReader(Reader: TReader);
var RVData: TCustomRVData;
    Tag: Integer;
begin
  RVData := TCustomRVData(GetRVFRVData);
  Tag := Reader.ReadInteger;
  if RVData<>nil then
    FImageList := RVData.RVFImageListNeeded(Tag)
  else
    FImageList := nil;
end;
{------------------------------------------------------------------------------}
function TRVListLevel.StoreImageList: Boolean;
begin
  Result := GetRVFRVData=nil;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEUNICODE}
{$IFDEF RICHVIEWCBDEF3}
procedure TRVListLevel.FormatStringWCodeReader(Reader: TReader);
begin
  FFormatStringW := RVDecodeWideString(Reader.ReadString);
end;
{------------------------------------------------------------------------------}
procedure TRVListLevel.FormatStringWCodeWriter(Writer: TWriter);
begin
  Writer.WriteString(RVEncodeWideString(FFormatStringW));
end;
{$ENDIF}
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TRVListLevel.FormatStringCodeReader(Reader: TReader);
begin
  FFormatString := RVDecodeString(Reader.ReadString);
end;
{------------------------------------------------------------------------------}
procedure TRVListLevel.FormatStringCodeWriter(Writer: TWriter);
begin
  Writer.WriteString(RVEncodeString(FFormatString));
end;
{------------------------------------------------------------------------------}
procedure TRVListLevel.DefineProperties(Filer: TFiler);
begin
  inherited;
  if GetRVFRVData<>nil then
    Filer.DefineProperty('ILTag', ImageListTagReader, ImageListTagWriter, FImageList<>nil);
  {$IFNDEF RVDONOTUSEUNICODE}
  {$IFDEF RICHVIEWCBDEF3}
  Filer.DefineProperty('FormatStringWCode', FormatStringWCodeReader, FormatStringWCodeWriter, FFormatStringW<>'');
  {$ENDIF}
  {$ENDIF}
  Filer.DefineProperty('FormatStringCode', FormatStringCodeReader, FormatStringCodeWriter, FFormatString<>#$B7);
end;
{========================= TRVListLevelCollection =============================}
constructor TRVListLevelCollection.Create(Owner: TPersistent);
begin
  inherited Create(TRVListLevel);
  FOwner := Owner;
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
function TRVListLevelCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRVListLevelCollection.GetItem(Index: Integer): TRVListLevel;
begin
  Result := TRVListLevel(inherited GetItem(Index));
end;
{------------------------------------------------------------------------------}
procedure TRVListLevelCollection.SetItem(Index: Integer;
  const Value: TRVListLevel);
begin
  inherited SetItem(Index, Value);
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWDEF4}
function TRVListLevelCollection.Insert(Index: Integer): TRVListLevel;
begin
  Result := TRVListLevel(inherited Insert(Index));
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRVListLevelCollection.IsSimpleEqual(Value: TRVListLevelCollection): Boolean;
var i: Integer;
begin
  Result := False;
  if Count<>Value.Count then
    exit;
  for i := 0 to Count-1 do
    if not Items[i].IsSimpleEqual(Value[i]) then
      exit;
  Result := True;      
end;
{------------------------------------------------------------------------------}
function TRVListLevelCollection.Add: TRVListLevel;
begin
  Result := TRVListLevel(inherited Add);
end;
{=========================== TRVListInfo ======================================}
constructor TRVListInfo.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FLevels := TRVListLevelCollection.Create(Self);
  StyleName := RVDEFAULTLISTSTYLENAME;
end;
{------------------------------------------------------------------------------}
destructor TRVListInfo.Destroy;
begin
  FLevels.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVListInfo.Assign(Source: TPersistent);
begin
  if Source is TRVListInfo then begin
    Levels := TRVListInfo(Source).Levels;
    OneLevelPreview := TRVListInfo(Source).OneLevelPreview;
  end;
  inherited Assign(Source);
end;
{------------------------------------------------------------------------------}
function TRVListInfo.IsSimpleEqualEx(Value: TCustomRVInfo;
  Mapping: TRVIntegerList): Boolean;
begin
  Result := IsSimpleEqual(Value, True, False);
  {
  if not Result then
    exit;
  Result := False;
  if (Value.BaseStyleNo>=0) then begin
    if (Value.BaseStyleNo>=Mapping.Count) then
      Value.BaseStyleNo := -1 // fix up
    else if (Mapping[Value.BaseStyleNo]<>BaseStyleNo) then
      exit;
  end;
  Result := True;
  }
end;
{------------------------------------------------------------------------------}
function TRVListInfo.IsSimpleEqual(Value: TCustomRVInfo;
  IgnoreReferences, IgnoreID: Boolean): Boolean;
begin
  Result := (OneLevelPreview=TRVListInfo(Value).OneLevelPreview) and
    (Levels.Count = TRVListInfo(Value).Levels.Count) and
    (IgnoreID or (ListID = TRVListInfo(Value).ListID));
  if not Result then
    exit;
  Result := Levels.IsSimpleEqual(TRVListInfo(Value).Levels);
end;
{------------------------------------------------------------------------------}
function TRVListInfo.SimilarityValue(Value: TCustomRVInfo): Integer;
var i,min,max: Integer;
begin
  Result := 0;
  if OneLevelPreview=TRVListInfo(Value).OneLevelPreview then
    inc(Result, RVMW_LISTMISC);
  if ListID=TRVListInfo(Value).ListID then
    inc(Result, RVMW_LISTMISC div 2);
  min := Levels.Count;
  max := min;
  if TRVListInfo(Value).Levels.Count<min then
    min := TRVListInfo(Value).Levels.Count;
  if TRVListInfo(Value).Levels.Count>max then
    max := TRVListInfo(Value).Levels.Count;
  for i := 0 to min-1 do
    inc(Result, Levels[i].SimilarityValue(TRVListInfo(Value).Levels[i]));
  dec(Result, RVMW_LISTMISC*(max-min));
end;
{------------------------------------------------------------------------------}
procedure TRVListInfo.ReadListID(Reader: TReader);
begin
  FListID := Reader.ReadInteger;
end;
{------------------------------------------------------------------------------}
procedure TRVListInfo.WriteListID(Writer: TWriter);
begin
  Writer.WriteInteger(ListID);
end;
{------------------------------------------------------------------------------}
procedure TRVListInfo.DefineProperties(Filer: TFiler);
begin
  inherited;
  if not RVNoLstIDProperty then
    Filer.DefineProperty('LstID', ReadListID, WriteListID, True);
end;
{------------------------------------------------------------------------------}
procedure TRVListInfo.SetLevels(const Value: TRVListLevelCollection);
begin
  if FLevels<>Value then
    FLevels.Assign(Value);
end;
{------------------------------------------------------------------------------}
function TRVListInfo.GetListID: Integer;
begin
  while FListID=0 do
    FListID := Random(MaxInt);
  Result := FListID;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEINI}
procedure TRVListInfo.SaveToINI(ini: TRVIniFile; const Section, fs: String);
var i: Integer;
begin
  inherited SaveToINI(ini, Section, fs);
  WriteIntToIniIfNE(ini, Section, Format(fs,[RVINI_LEVELSCOUNT]),Levels.Count,0);
  WriteBoolToIniIfNE(ini,Section, RVINI_ONELEVELPREVIEW, OneLevelPreview, False);
  for i := 0 to Levels.Count-1 do
    Levels[i].SaveToINI(ini, Section, Format(fs,[''])+RVINI_LEVELPREFIX+IntToStr(i));
end;
{------------------------------------------------------------------------------}
procedure TRVListInfo.LoadFromINI(ini: TRVIniFile; const Section, fs: String);
var cnt,i: Integer;
begin
  inherited LoadFromINI(ini, Section, fs, RVDEFAULTLISTSTYLENAME);
  OneLevelPreview := IniReadBool(ini, Section, RVINI_ONELEVELPREVIEW, False);
  cnt := ini.ReadInteger(Section, Format(fs,[RVINI_LEVELSCOUNT]), 0);
  for i := 0 to cnt-1 do
    Levels.Add.LoadFromINI(ini, Section, Format(fs,[''])+RVINI_LEVELPREFIX+IntToStr(i));
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRVListInfo.HasNumbering: Boolean;
var i: Integer;
begin
  Result := False;
  for i := 0 to Levels.Count-1 do
    if Levels[i].HasNumbering then begin
      Result := True;
      exit;
    end;
end;
{------------------------------------------------------------------------------}
function TRVListInfo.AllNumbered: Boolean;
var i: Integer;
begin
  Result := True;
  for i := 0 to Levels.Count-1 do
    if not Levels[i].HasNumbering then begin
      Result := False;
      exit;
    end;
end;
{------------------------------------------------------------------------------}
function TRVListInfo.HasVariableWidth: Boolean;
var i: Integer;
begin
  Result := False;
  for i := 0 to Levels.Count-1 do
    if Levels[i].HasVariableWidth then begin
      Result := True;
      exit;
    end;
end;
{============================== TRVListInfos ==================================}
function TRVListInfos.GetItem(Index: Integer): TRVListInfo;
begin
  Result := TRVListInfo(inherited GetItem(Index));
end;
{------------------------------------------------------------------------------}
procedure TRVListInfos.SetItem(Index: Integer; const Value: TRVListInfo);
begin
  inherited SetItem(Index, Value);
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWDEF4}
function TRVListInfos.Insert(Index: Integer): TRVListInfo;
begin
  Result := TRVListInfo(inherited Insert(Index));
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRVListInfos.Add: TRVListInfo;
begin
  Result := TRVListInfo(inherited Add);
end;
{------------------------------------------------------------------------------}
procedure TRVListInfos.RemoveImageList(ImageList: TCustomImageList);
var i, j: Integer;
begin
  for i := 0 to Count-1 do
    for j := 0 to Items[i].Levels.Count-1 do
      if Items[i].Levels[j].FImageList=ImageList then
        Items[i].Levels[j].FImageList := nil;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEINI}
procedure TRVListInfos.LoadFromINI(ini: TRVIniFile; const Section: String);
var i, cnt: Integer;
begin
  cnt := ini.ReadInteger(Section, RVINI_LISTSTYLECOUNT, 0);
  Clear;
  for i:=0 to cnt-1 do
    Add.LoadFromINI(ini, Section, RVINI_LISTSTYLEPREFIX+IntToStr(i));
end;
{------------------------------------------------------------------------------}
procedure TRVListInfos.SaveToINI(ini: TRVIniFile; const Section: String);
var i: Integer;
begin
  ini.WriteInteger(Section,RVINI_LISTSTYLECOUNT, Count);
  for i:=0 to Count-1 do
    Items[i].SaveToINI(ini, Section, RVINI_LISTSTYLEPREFIX+IntToStr(i));
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRVListInfos.FindSuchStyle(Style: TRVListInfo; AddIfNotFound: Boolean): Integer;
var i: Integer;
begin
  for i:=0 to Count-1 do
    if Items[i].IsSimpleEqual(Style, False, True) then begin
      Result := i;
      exit;
    end;
  if AddIfNotFound then begin
    Add.Assign(Style);
    Result := Count-1;
    if RichViewResetStandardFlag then
      Items[Result].Standard := False;
    end
  else
    Result := -1;
end;
{------------------------------------------------------------------------------}
function TRVListInfos.FindStyleWithLevels(Levels: TRVListLevelCollection;
  const StyleNameForAdding: String; AddIfNotFound: Boolean): Integer;
var i: Integer;
begin
  for i:=0 to Count-1 do
    if Items[i].Levels.IsSimpleEqual(Levels) then begin
      Result := i;
      exit;
    end;
  if AddIfNotFound then begin
    Add;
    Result := Count-1;
    if RichViewResetStandardFlag then
      Items[Result].Standard := False;
    Items[Result].StyleName := StyleNameForAdding;
    Items[Result].OneLevelPreview := True;
    Items[Result].Levels := Levels;
    end
  else
    Result := -1;
end;
{============================== TRVStyle ======================================}
constructor TRVStyle.Create(AOwner: TComponent);
var jumpcur : HCURSOR;
const IDC_HAND = MakeIntResource(32649);
begin
  inherited Create(AOwner);
  jumpcur := LoadCursor(0, IDC_HAND);
  if jumpcur=0 then
    jumpcur := LoadCursor(hInstance,RVRC_JUMP_CURSOR);
  Screen.Cursors[crJump] := jumpcur;
  Screen.Cursors[crRVFlipArrow] := LoadCursor(hInstance,RVRC_FLIPARROW_CURSOR);
  FSpacesInTab       := 8;
  FFullRedraw        := False;
  FJumpCursor        := crJump;
  FLineSelectCursor  := crRVFlipArrow;
  FColor             := clWindow;
  FHoverColor        := clNone;
  FCurrentItemColor  := clNone;
  FSelColor          := clHighlight;
  FSelTextColor      := clHighlightText;
  FInactiveSelColor     := clHighlight;
  FInactiveSelTextColor := clHighlightText;
  FCheckpointColor   := clGreen;
  FCheckpointEvColor := clLime;
  FPageBreakColor    := clBtnShadow;
  FSoftPageBreakColor := clBtnFace;
  FUseSound          := True;
  FSelectionMode     := rvsmWord;
  FSelectionStyle    := rvssItems;
  {$IFNDEF RVDONOTUSEUNICODE}
  FDefUnicodeStyle   := -1;
  FDefCodePage       := CP_ACP;
  {$ENDIF}
  FTextStyles := TFontInfos.Create(GetTextStyleClass, Self);
  FParaStyles := TParaInfos.Create(GetParaStyleClass, Self);
  FListStyles := TRVListInfos.Create(GetListStyleClass, Self);  
  ResetParaStyles;
  ResetTextStyles;
end;
{------------------------------------------------------------------------------}
function TRVStyle.GetParaStyleClass: TRVParaInfoClass;
begin
  Result := TParaInfo;
end;
{------------------------------------------------------------------------------}
function TRVStyle.GetTextStyleClass: TRVFontInfoClass;
begin
  Result := TFontInfo;
end;
{------------------------------------------------------------------------------}
function TRVStyle.GetListStyleClass: TRVListInfoClass;
begin
  Result := TRVListInfo;
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.ReadState(Reader: TReader);
begin
  {$IFNDEF RICHVIEWDEF3}
  ParaStyles.Clear;
  TextStyles.Clear;
  {$ENDIF}
  inherited ReadState(Reader);
end;
{------------------------------------------------------------------------------}
destructor TRVStyle.Destroy;
begin
  FTextStyles.Free;
  FParaStyles.Free;
  FListStyles.Free;
  FInvalidPicture.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.ApplyStyle(Canvas: TCanvas; StyleNo: Integer; DefBiDiMode: TRVBiDiMode);
var DoDefault: Boolean;
begin
  if Assigned(FOnApplyStyle) then begin
    DoDefault := True;
    FOnApplyStyle(Self, Canvas, StyleNo, DoDefault);
    if DoDefault then
      FTextStyles[StyleNo].Apply(Canvas, DefBiDiMode);
    end
  else
    FTextStyles[StyleNo].Apply(Canvas, DefBiDiMode);
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.ApplyStyleColor(Canvas: TCanvas; StyleNo: Integer;
  DrawState: TRVTextDrawStates; Printing: Boolean; ColorMode: TRVColorMode);
var DoDefault: Boolean;
begin
  if Assigned(FOnApplyStyleColor) then begin
    DoDefault := True;
    FOnApplyStyleColor(Self, Canvas, StyleNo, DrawState, DoDefault);
    if DoDefault then
      FTextStyles[StyleNo].ApplyColor(Canvas, Self, DrawState, Printing, ColorMode);
    end
  else
    FTextStyles[StyleNo].ApplyColor(Canvas, Self, DrawState, Printing, ColorMode);
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.DrawStyleText(const s: String; Canvas: TCanvas;
  ItemNo, OffsetInItem, StyleNo: Integer; RVData: TPersistent;
  SpaceBefore, Left, Top, Width, Height: Integer; DrawState: TRVTextDrawStates;
  Printing, PreviewCorrection: Boolean; ColorMode: TRVColorMode;
  DefBiDiMode: TRVBidiMode);
var DoDefault: Boolean;
    s2: String;
begin
  {$IFNDEF RVDONOTUSEALLCAPS}
  s2 := RV_ReturnCapitalized(s, TextStyles[StyleNo]);
  {$ELSE}
  s2 := s;
  {$ENDIF}
  if Assigned(FOnDrawStyleText) then begin
    DoDefault := True;
    Self.ItemNo := ItemNo;
    Self.RVData := RVData;
    Self.OffsetInItem := OffsetInItem;
    FOnDrawStyleText(Self, s2, Canvas, StyleNo,
                     SpaceBefore, Left, Top, Width, Height, DrawState, DoDefault);
    if DoDefault then
      FTextStyles[StyleNo].Draw(s2, Canvas, StyleNo, SpaceBefore, Left, Top, Width, Height,
        Self, DrawState, Printing, PreviewCorrection, ColorMode, DefBiDiMode);
    end
  else
    FTextStyles[StyleNo].Draw(s2, Canvas, StyleNo, SpaceBefore, Left, Top, Width, Height,
      Self, DrawState, Printing, PreviewCorrection, ColorMode, DefBiDiMode);
end;
{------------------------------------------------------------------------------}
function TRVStyle.StyleHoverSensitive(StyleNo: Integer): Boolean;
begin
  Result := (GetHoverColor(StyleNo)<>clNone) or
            (FTextStyles[StyleNo].HoverBackColor<>FTextStyles[StyleNo].BackColor);
  if Assigned(FOnStyleHoverSensitive) then
    FOnStyleHoverSensitive(Self, StyleNo, Result);
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.DrawTextBack(Canvas: TCanvas; ItemNo, StyleNo: Integer;
  RVData: TPersistent; Left, Top, Width, Height: Integer;
  DrawState: TRVTextDrawStates);
var DoDefault: Boolean;
begin
  if Assigned(FOnDrawTextBack) then begin
    DoDefault := True;
    Self.ItemNo := ItemNo;
    Self.RVData := RVData;
    FOnDrawTextBack(Self, Canvas, StyleNo, Left, Top, Width, Height, DrawState, DoDefault);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.DrawCheckpoint(Canvas: TCanvas; X,Y, ItemNo, XShift: Integer;
  RaiseEvent: Boolean; Control: TControl);
var DoDefault: Boolean;
begin
  DoDefault := True;
  if Assigned(FOnDrawCheckpoint) then
    FOnDrawCheckpoint(Self, Canvas, X, Y, ItemNo, XShift, RaiseEvent, Control,
      DoDefault);
  if DoDefault then begin
    Canvas.Pen.Width := 1;
    if RaiseEvent then
      Canvas.Pen.Color := CheckpointEvColor
    else
      Canvas.Pen.Color := CheckpointColor;
    Canvas.Brush.Style := bsClear;
    if ItemNo<>-1 then begin
      Canvas.Pen.Style := psSolid;
      Canvas.Ellipse(X-2,Y-2, X+2, Y+2);
    end;
    Canvas.Pen.Style := psDot;
    Canvas.MoveTo(-XShift, Y);
    Canvas.LineTo(Control.Width, Y);
  end;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsClear;
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.DrawPageBreak(Canvas: TCanvas; Y, XShift: Integer;
  PageBreakType: TRVPageBreakType; Control: TControl);
var DoDefault: Boolean;
    x: Integer;
const CORNERSIZE=8;
begin
  DoDefault := True;
  if Assigned(FOnDrawPageBreak) then
    FOnDrawPageBreak(Self, Canvas, Y, XShift, PageBreakType, Control, DoDefault);
  if DoDefault then begin
    if PageBreakType = rvpbPageBreak then
      Canvas.Pen.Color := PageBreakColor
    else
      Canvas.Pen.Color := SoftPageBreakColor;
    Canvas.Pen.Width := 1;
    Canvas.Pen.Style := psSolid;
    x := Control.ClientWidth-XShift-CORNERSIZE;
    Canvas.Brush.Color := clWindow;
    Canvas.Brush.Style := bsSolid;
    Canvas.MoveTo(-XShift,Y);
    Canvas.LineTo(X,Y);
    Canvas.Polygon([Point(X,Y), Point(X+CORNERSIZE,Y+CORNERSIZE),
                   Point(X,Y+CORNERSIZE)]);
  end;
  Canvas.Pen.Style := psSolid;
  Canvas.Brush.Style := bsClear;
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.DrawParaBack(Canvas: TCanvas; ParaNo: Integer; const Rect: TRect;
  Printing: Boolean; ColorMode: TRVColorMode);
var DoDefault: Boolean;
begin
  DoDefault := True;
  if Assigned(FOnDrawParaBack) then
    FOnDrawParaBack(Self, Canvas, ParaNo, Rect, DoDefault);
  if DoDefault then
    FParaStyles[ParaNo].Background.Draw(Rect, Canvas, Printing, ColorMode);
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.ResetTextStyles;
var fi: TFontInfo;
    i : Integer;
begin
  FTextStyles.Clear;
  for i := 0 to LAST_DEFAULT_STYLE_NO do begin
    fi := FTextStyles.Add;
    case i of
     rvsNormal:
        begin
           fi.StyleName := RVDEFSTYLENAME0;
        end;
     rvsHeading:
        begin
           fi.Style := fi.Style + [fsBold];
           fi.Color := clBlue;
           fi.StyleName := RVDEFSTYLENAME1;
        end;
     rvsSubheading:
        begin
           fi.Style := fi.Style + [fsBold];
           fi.Color := clNavy;
           fi.StyleName := RVDEFSTYLENAME2;
        end;
     rvsKeyword:
        begin
           fi.Style := fi.Style + [fsItalic];
           fi.Color := clMaroon;
           fi.StyleName := RVDEFSTYLENAME3;
        end;
     rvsJump1, rvsJump2:
        begin
           fi.Style := fi.Style + [fsUnderline];
           fi.Color := clGreen;
           fi.Jump  := True;
           fi.JumpCursor := JumpCursor;
           if i=rvsJump1 then
             fi.StyleName := RVDEFSTYLENAME4
           else
             fi.StyleName := RVDEFSTYLENAME5;
        end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.ResetParaStyles;
begin
  FParaStyles.Clear;
  FParaStyles.Add;
  with FParaStyles.Add as TParaInfo do begin
    StyleName := RVDEFPARASTYLENAME1;
    Alignment := rvaCenter;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.SetTextStyles(Value: TFontInfos);
begin
  if FTextStyles<>Value then
    FTextStyles.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.SetParaStyles(Value: TParaInfos);
begin
  if FParaStyles<>Value then
    FParaStyles.Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.SetListStyles(Value: TRVListInfos);
begin
  if FListStyles<>Value then
    FListStyles.Assign(Value);
end;
{------------------------------------------------------------------------------}
function TRVStyle.GetHoverColorByColor(Color: TColor): TColor;
begin
  if Color<>clNone then
    Result := Color
  else
    Result := HoverColor;
end;
{------------------------------------------------------------------------------}
function TRVStyle.GetHoverColor(StyleNo: Integer): TColor;
begin
  if FTextStyles[StyleNo].HoverColor<>clNone then
    Result := FTextStyles[StyleNo].HoverColor
  else
    Result := HoverColor;
end;
{------------------------------------------------------------------------------}
function TRVStyle.AddTextStyle: Integer;
begin
   FTextStyles.Add;
   AddTextStyle := FTextStyles.Count-1;
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.DeleteTextStyle(Index: Integer);
begin
   FTextStyles[Index].Free;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEINI}
procedure TRVStyle.LoadFromINI(ini: TRVIniFile; Section: String);
begin
  Color             := ini.ReadInteger(Section, RVINI_COLOR,             clWindow);
  HoverColor        := ini.ReadInteger(Section, RVINI_HOVERCOLOR,        clNone);
  CurrentItemColor  := ini.ReadInteger(Section, RVINI_CURRENTITEMCOLOR,  clNone);
  SelColor          := ini.ReadInteger(Section, RVINI_SELCOLOR,          clHighlight);
  SelTextColor      := ini.ReadInteger(Section, RVINI_SELTEXTCOLOR,      clHighlightText);
  InactiveSelColor     := ini.ReadInteger(Section, RVINI_ISELCOLOR,      clHighlight);
  InactiveSelTextColor := ini.ReadInteger(Section, RVINI_ISELTEXTCOLOR,  clHighlightText);
  CheckpointColor   := ini.ReadInteger(Section, RVINI_CPCOLOR,   clGreen);
  CheckpointEvColor := ini.ReadInteger(Section, RVINI_CPEVCOLOR, clLime);
  PageBreakColor    := ini.ReadInteger(Section, RVINI_PAGEBREAKCOLOR,  clBtnShadow);
  SoftPageBreakColor := ini.ReadInteger(Section, RVINI_SOFTPAGEBREAKCOLOR, clBtnFace);
  JumpCursor        := ini.ReadInteger(Section, RVINI_JUMPCURSOR,        crJump);
  UseSound          := Boolean(ini.ReadInteger(Section, RVINI_USESOUND,  Integer(True)));
  SelectionMode     := TRVSelectionMode(ini.ReadInteger(Section, RVINI_SELECTIONMODE, ord(rvsmWord)));
  SelectionStyle    := TRVSelectionStyle(ini.ReadInteger(Section, RVINI_SELECTIONSTYLE, ord(rvssItems)));
  {$IFNDEF RVDONOTUSEUNICODE}
  DefUnicodeStyle   := ini.ReadInteger(Section, RVINI_DEFUNICODESTYLE,   -1);
  DefCodePage       := ini.ReadInteger(Section, RVINI_DEFCODEPAGE,   CP_ACP);
  {$ENDIF}
  ParaStyles.LoadFromINI(ini, Section);
  TextStyles.LoadFromINI(ini, Section, JumpCursor);
  ListStyles.LoadFromINI(ini, Section);
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.SaveToINI(ini: TRVIniFile; Section: String);
begin
  ini.EraseSection(Section);
  WriteIntToIniIfNE(ini, Section, RVINI_COLOR,             Color,             clWindow);
  WriteIntToIniIfNE(ini, Section, RVINI_HOVERCOLOR,        HoverColor,        clNone);
  WriteIntToIniIfNE(ini, Section, RVINI_CURRENTITEMCOLOR,  CurrentItemColor,  clNone);  
  WriteIntToIniIfNE(ini, Section, RVINI_SELCOLOR,          SelColor,          clHighlight);
  WriteIntToIniIfNE(ini, Section, RVINI_SELTEXTCOLOR,      SelTextColor,      clHighlightText);
  WriteIntToIniIfNE(ini, Section, RVINI_ISELCOLOR,         InactiveSelColor,     clHighlight);
  WriteIntToIniIfNE(ini, Section, RVINI_ISELTEXTCOLOR,     InactiveSelTextColor, clHighlightText);
  WriteIntToIniIfNE(ini, Section, RVINI_CPCOLOR,   CheckpointColor,   clGreen);
  WriteIntToIniIfNE(ini, Section, RVINI_CPEVCOLOR, CheckpointEvColor, clLime);
  WriteIntToIniIfNE(ini, Section, RVINI_PAGEBREAKCOLOR,    PageBreakColor,      clBtnShadow);
  WriteIntToIniIfNE(ini, Section, RVINI_SOFTPAGEBREAKCOLOR, SoftPageBreakColor, clBtnFace);
  WriteIntToIniIfNE(ini, Section, RVINI_JUMPCURSOR,        JumpCursor,        crJump);
  WriteBoolToIniIfNE(ini, Section, RVINI_USESOUND,         UseSound,          True);
  WriteIntToIniIfNE(ini, Section, RVINI_SELECTIONMODE,     ord(SelectionMode), ord(rvsmWord));
  WriteIntToIniIfNE(ini, Section, RVINI_SELECTIONSTYLE,    ord(SelectionStyle), ord(rvssItems));
  {$IFNDEF RVDONOTUSEUNICODE}
  WriteIntToIniIfNE(ini, Section, RVINI_DEFUNICODESTYLE,   DefUnicodeStyle,   -1);
  WriteIntToIniIfNE(ini, Section, RVINI_DEFCODEPAGE,       DefCodePage,   CP_ACP);
  {$ENDIF}
  ParaStyles.SaveToINI(ini, Section);
  TextStyles.SaveToINI(ini, Section);
  ListStyles.SaveToINI(ini, Section);
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.SaveINI(const FileName, Section: String);
var ini: TIniFile;
begin
  ini := TIniFile.Create(FileName);
  try
    SaveToINI(ini, Section);
  finally
    ini.Free;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.LoadINI(const FileName, Section: String);
var ini: TIniFile;
begin
  ini := TIniFile.Create(filename);
  try
    LoadFromINI(ini, Section);
  finally
    ini.Free;
  end;
end;
{$IFDEF RICHVIEWDEF4}
{------------------------------------------------------------------------------}
procedure TRVStyle.LoadReg(const BaseKey: String);
var ini: TRegistryIniFile;
begin
  ini := TRegistryIniFile.Create(BaseKey);
  try
    LoadFromINI(ini, RVSTYLE_REG);
  finally
    ini.Free;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVStyle.SaveReg(const BaseKey: String);
var ini: TRegistryIniFile;
begin
  ini := TRegistryIniFile.Create(BaseKey);
  try
    SaveToINI(ini, RVSTYLE_REG);
  finally
    ini.Free;
  end;
end;
{$ENDIF}
{$ENDIF}
{-----------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEHTML}
{-----------------------------------------------------------------------}
function TRVStyle.SaveCSS(const FileName: String; AOptions: TRVSaveCSSOptions): Boolean;
var Stream: TFileStream;
begin
  Result := True;
  try
    Stream := TFileStream.Create(FileName, fmCreate);
    try
      SaveCSSToStream(Stream, AOptions);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;
{-----------------------------------------------------------------------}
procedure TRVStyle.SaveCSSToStream(Stream: TStream; AOptions: TRVSaveCSSOptions);
var i: Integer;
    comments: String;
    BaseStyle: TFontInfo;
    BaseParaStyle: TParaInfo;
    {..................................................}
    {$IFNDEF RVDONOTUSELISTS}
    (* Reserved for future - when browser will be CSS2 compatible    
    function GetListTagSequence(ListNo, Level: Integer): String;
    var i: Integer;
    begin
      Result := Format('%s.RVLS%d',
        [ListStyles[ListNo].Levels[0].GetHTMLOpenTagForCSS, ListNo]);
      with ListStyles[ListNo] do
        for i := 1 to Level do
          Result := Result+' '+Levels[i].GetHTMLOpenTagForCSS;
    end;
    {..................................................}
    function GetListTypeStr(ListType: TRVListType; Legal: Boolean): String;
    begin
      case ListType of
        rvlstBullet, rvlstPicture, rvlstUnicodeBullet, rvlstImageList:
           Result := '';
        rvlstLowerAlpha:
           Result := 'lower-alpha';
        rvlstUpperAlpha:
           Result := 'upper-alpha';
        rvlstLowerRoman:
           Result := 'lower-roman';
         rvlstUpperRoman:
           Result := 'upper-roman';
         else
           Result := 'decimal';
      end;
      if Legal and (Result<>'') then
         Result := 'decimal';
    end;
    {..................................................}
    function GetListContentStr(ListNo, Level: Integer): String;
    var CountersVal: array [0..255] of TVarRec;
        CountersStr: array [0..255] of String;
        s: String;
        i: Integer;
        Legal: Boolean;
    begin
      for i := 0 to 255 do begin
        CountersVal[i].VAnsiString := nil;
        CountersVal[i].VType := vtAnsiString;
      end;
      Legal := rvloLegalStyleNumbering in ListStyles[ListNo].Levels[Level].Options;
      for i := 0 to Level do begin
        s := GetListTypeStr(ListStyles[ListNo].Levels[i].ListType, Legal and (i<>Level));
        if s<>'' then begin
          CountersStr[i] := Format(#1' counter(c%dl%d,%s) '#1,[ListNo,i,s]);
          CountersVal[i].VAnsiString := PChar(CountersStr[i]);
        end
      end;
      s := Format(ListStyles[ListNo].Levels[Level].FormatString, CountersVal);
      repeat
        i := Pos(#1#1, s);
        if i>0 then
          Delete(s, i, 2);
      until i = 0;
      if Length(s)>0 then begin
        if s[1]=#1 then
          Delete(s,1,1)
        else
          s := '"'+s;
        if s[Length(s)]=#1 then
          Delete(s,Length(s),1)
        else
          s := s+'"';
      end;
      for i := 1 to Length(s) do
        if s[i]=#1 then
          s[i] := '"';
      Result := s;
    end;
    {..................................................}
    function GetListContent(ListNo, Level: Integer): String;
    var LevelInfo: TRVListLevel;
    begin
      LevelInfo := ListStyles[ListNo].Levels[Level];
      case LevelInfo.ListType of
        rvlstUnicodeBullet:
          {$IFDEF RICHVIEWCBDEF3}
          Result := RVU_GetHTMLEncodedUnicode(RVU_GetRawUnicode(LevelInfo.FFormatStringW), False,False);
          {$ELSE}
          Result := LevelInfo.FFormatStringW;
          {$ENDIF}
        rvlstBullet:
          Result := LevelInfo.FFormatString;
        else
          Result := GetListContentStr(ListNo,Level);
      end;
    end;
    *)
    {$ENDIF}
    {..................................................}    
begin
  RVWriteLn(Stream, '/*----------Text Styles----------*/');
  RVWriteLn(Stream, 'HR { color: '+RV_GetHTMLRGBStr(FTextStyles[0].Color, False)+'}');
  for i:=0 to FTextStyles.Count-1 do
    with FTextStyles[i] do begin
      if Standard then
        Comments := Format(' /* %s */', [StyleName])
      else
        Comments := '';
      if (i=0) and not Jump and (BackColor=clNone) and
         not (rvcssNoDefCSSStyle in AOptions) then
        RVWriteLn(Stream, Format('BODY, TABLE%s', [Comments]))
      else if Jump then
        RVWriteLn(Stream, Format('A.RVTS%d, SPAN.RVTS%d%s',[i,i, Comments]))
      else
        RVWriteLn(Stream, Format('SPAN.RVTS%d%s', [i, Comments]));
      if (rvcssOnlyDifference in AOptions) and
        (BaseStyleNo>=0) and (BaseStyleNo<TextStyles.Count) then
        BaseStyle := TextStyles[BaseStyleNo]
      else begin
        BaseStyle := nil;
        if (i>0) and not TextStyles[0].Jump and
           (TextStyles[0].BackColor=clNone) and
           not (rvcssNoDefCSSStyle in AOptions) then
          BaseStyle := TextStyles[0];
      end;
      RVWriteLn(Stream, '{');
      SaveCSSToStream(Stream, BaseStyle, True);
      RVWriteLn(Stream, '}');
      if Jump and ((GetHoverColorByColor(HoverColor)<>clNone) or (HoverBackColor<>clNone)) then begin
        RVWrite(Stream, Format('A.RVTS%d:hover {', [i]));
        if (((BaseStyle=nil) or not BaseStyle.Jump) and (GetHoverColorByColor(HoverColor)<>clNone)) or
           ((BaseStyle<>nil) and (GetHoverColorByColor(HoverColor)<>GetHoverColorByColor(BaseStyle.HoverColor))) then
          RVWrite(Stream, Format(' color: %s;', [RV_GetHTMLRGBStr(GetHoverColorByColor(HoverColor), False)]));
        if (((BaseStyle=nil) or not BaseStyle.Jump)  and (HoverBackColor<>clNone)) or
           ((BaseStyle<>nil) and (HoverBackColor<>BaseStyle.HoverBackColor)) then
          RVWrite(Stream, Format(' background-color: %s;', [RV_GetHTMLRGBStr(HoverBackColor, False)]));
        RVWriteLn(Stream, ' }');
      end;
    end;
  RVWriteLn(Stream, '/*----------Para Styles----------*/');
  for i:=0 to FParaStyles.Count-1 do
    with FParaStyles[i] do begin
      if Standard then
        Comments := Format(' /* %s */', [StyleName])
      else
        Comments := '';
      if (i=0) and not (rvcssNoDefCSSStyle in AOptions) then
        RVWriteLn(Stream, Format('P,UL,OL%s',[Comments]))
      else
        RVWriteLn(Stream, Format('.RVPS%d%s',[i,Comments]));
      if (rvcssOnlyDifference in AOptions) and
        (BaseStyleNo>=0) and (BaseStyleNo<ParaStyles.Count) then
        BaseParaStyle := ParaStyles[BaseStyleNo]
      else begin
        if (i>0) and not (rvcssNoDefCSSStyle in AOptions) then
          BaseParaStyle := ParaStyles[0]
        else
          BaseParaStyle := nil;
      end;
      RVWriteLn(Stream, '{');
      SaveCSSToStream(Stream, BaseParaStyle, True,
        rvcssIgnoreLeftAlignment in AOptions, False);
      RVWriteLn(Stream, '}');
    end;
  {$IFNDEF RVDONOTUSELISTS}
  (*
  RVWriteLn(Stream, '/*----------List Styles----------*/');
  for i:=0 to FListStyles.Count-1 do
    for j:=0 to FListStyles[i].Levels.Count-1 do
      with FListStyles[i].Levels[j] do begin
        s := GetListTagSequence(i,j);
        if j=0 then
          descr := Format('/* %s */ ',[FListStyles[i].StyleName])
        else
          descr := '';
        if MarkerIndent>=LeftIndent then
          s2 := Format('text-indent: %dpx !important; margin-left !important: %d; list-style:inside;',
            [MarkerIndent-LeftIndent, LeftIndent])
        else
          s2 := Format('text-indent: %dpx !important; margin-left: %d !important; list-style:outside;',
            [FirstIndent, LeftIndent]);
        RVWriteLn(Stream, Format('%s %s{ %s }', [s, descr, s2]));
      end;
  *)
  (*
  RVWriteLn(Stream, '/*----------List Styles----------*/');
  for i:=0 to FListStyles.Count-1 do
    for j:=0 to FListStyles[i].Levels.Count-1 do
      with FListStyles[i].Levels[j] do begin
        s := GetListTagSequence(i,j);
        if j=0 then
          descr := Format('/* %s */ ',[FListStyles[i].StyleName])
        else
          descr := '';
        if HasNumbering then begin
          if (rvloLevelReset in Options) then begin
            RVWriteLn(Stream, Format('%s %s{ counter-reset: c%dl%d; }', [s, descr, i,j]));
            descr := '';
          end;
          RVWriteLn(Stream, Format('%s > LI %s{ counter-increment: c%dl%d; }', [s, descr, i,j]));
          descr := '';
        end;
        RVWriteLn(Stream, Format('%s > LI:before %s{ content:%s }', [s, descr, GetListContent(i,j)]));
      end;
  *)
  {$ENDIF}
end;
{$ENDIF}
{-----------------------------------------------------------------------}
procedure TRVStyle.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation=opRemove) and (AComponent is TCustomImageList) then
    FListStyles.RemoveImageList(TCustomImageList(AComponent));
end;
{-----------------------------------------------------------------------}
function TRVStyle.GetInvalidPicture: TPicture;
begin
  if FInvalidPicture=nil then begin
    FInvalidPicture := TPicture.Create;
    FInvalidPicture.Bitmap.Handle := LoadBitmap(hInstance, 'RV_BAD_PICTURE');
  end;
  Result := FInvalidPicture;
end;
{-----------------------------------------------------------------------}
procedure TRVStyle.SetInvalidPicture(const Value: TPicture);
begin
  if Value=nil then begin
    FInvalidPicture.Free;
    FInvalidPicture := nil;
    exit;
  end;
  if FInvalidPicture=Value then
    exit;
  if FInvalidPicture=nil then
    FInvalidPicture := TPicture.Create;
  FInvalidPicture.Assign(Value);
end;
{-----------------------------------------------------------------------}
procedure RVWrite(Stream: TStream; const s: String);
begin
  Stream.WriteBuffer(PChar(s)^, Length(s));
end;
{-----------------------------------------------------------------------}
procedure RVWriteLn(Stream: TStream; const s: String);
var crlf: String;
begin
  Stream.WriteBuffer(PChar(s)^, Length(s));
  crlf := #13#10;
  Stream.WriteBuffer(PChar(crlf)^, 2);
end;
{-----------------------------------------------------------------------}
procedure RVWriteX(Stream: TStream; const s: String; Multiline: Boolean);
var crlf: String;
begin
  Stream.WriteBuffer(PChar(s)^, Length(s));
  if Multiline then begin
    crlf := #13#10;
    Stream.WriteBuffer(PChar(crlf)^, 2);
  end;
end;

end.
