RichView 1.8.14.1
Full source code (without help and demo projects)
-------------------------------------------------------
HELP
Help file:
http://www.trichview.com/rvfiles/rvhelpfile.zip

Demos for Delphi:
http://www.trichview.com/rvfiles/delphidemos.zip

Demos for C++Builder:
http://www.trichview.com/rvfiles/cbdemos.zip

All the files above in one zip:
http://www.trichview.com/rvfiles/rvhelp.zip
-------------------------------------------------------
COMPATIBILITY ISSUES
- TRVStyle.CurrentItemColor is used for drawing frame around
  the current item instead of TRVStyle.HoverColor
- ApplyTextStyle, ApplyParaStyle, ApplyStyleConversion,
  ApplyParaStyleConversion are applied to cells of all
  selected tables.
- OnSaveImage2 event: ImageSaveNo is declared as a 
  var-parameter (it does not make sense otherwise)
- functions in RVLinear.pas counts from 0 (see RVLinear.pas
  comments how to return counting from 1)
-------------------------------------------------------
1.8.14.1
chg: functions in RVLinear.pas counts from 0 (see RVLinear.pas
  comments how to return counting from 1)
small fix
-------------------------------------------------------
1.8.14
impr: when calling SelectAll or making line selection (with
  mouse on the left margin) no scrolling occurs.
fixes
-------------------------------------------------------
1.8.13
bug fixes (see also compatibility issues above)
-------------------------------------------------------
1.8.12
new: TRichView.VAlign property - defines vertical align
  of the document relative to the control window 
  (when the window is not completely filled).
  New method ClientToDocument converts client coordinates
  to the document coordinates. It should be used instead of
  adding HScrollPos to X and VScrollPos*VSmallStep to Y
  (the old method is correct only if the document is top-aligned).
fixes  
-------------------------------------------------------
1.8.11
new: rvpaoKeepWithNext - option for paragraph style's
  (TParaInfo) Options. Paragraphs with this options
  are printed on the same page as the next paragraph,
  if possible.
new: RichView.RTFReadProperties.ExtractMetafileBitmaps:
  Boolean 
  If True (default), TRichView attempts to extract
  bitmaps embedded in metafiles and to use them instead of
  metafiles. MS Word usually does not save bitmaps in RTF
  as they are: it wraps them in metafiles. Previous versions
  of TRichView loaded these metafiles in TMetafile, and
  TMetafile sometimes scales bitmap images inaccurately.
  This option does not affect RTF metafiles containing
  something besides bitmaps, no bitmaps, or multiple bitmaps
  - they will be loaded in TMetafile as before.
-------------------------------------------------------
1.8.10
impr: RVF reader forward compatibility improved. Addition of
  new properties to the style collection will not prevent
  applications compiled with older version of TRichView
  reading new RVF.
new: TRichView.OnProcess event. Occurs when loading large
  RTF files/streams (in future will be in other case)
  First, it is called with parameter Stage=rvpstgStarting.
  Next, it is called several times with parameter 
  Stage=rvpstgRunning, with PercentDone in range 0..100.
  Finally, it is called with parameter Stage=rvpstgEnding.
  new: TRichView.BeginOleDrag - starts dragging the selection.
  Useful if you want to implement drag&drop of inserted controls
  (all other contents can be dragged automatically)
  see EXAMPLES.TXT
new: LoadTextFromStream and LoadTextFromStreamW methods
  working like LoadText and LoadTextW, but with streams.
new: rvoNoCaretHighlightJumps option for EditorOptions.
  If set, hyperlinks will not be highlighted (with
  HoverColor and HoverBackColor) when caret is moved inside
  it.
fix: fixes (the most important: fix affecting "character
  case" action)
-------------------------------------------------------
1.8.9
impr: pressing ENTER key in the empty bulleted/numbered
  paragraph removes bullet/numbering
new: rvpaoKeepLinesTogether - option for paragraph style's
  (TParaInfo) Options. Paragraphs with this options
  are printed on the same page, if possible.
fix: old behavior is restored: when switching to the text
  style with rvprDoNotAutoSwitch protection
  and inserting text, a current text style is automatically
  restored back to the previous one after insertion.
new: TRichView.Reformat method. It works like Format,
  but keeps the selection/caret position. 
  Document must be formatted before calling this method. 
  It is useful, for example, if you want to reformat document
  after changing  margins.
fix: it was impossible to call SavePicture from inside
  OnSavePicture2 event;
impr: imagesprefix parameter of SaveHTML/SaveHTMLEx can contain 
  full path (determined by the presence of ':')
-------------------------------------------------------
1.8.8
impr: SaveHTMLEx was modified to conform 
  "HTML 4.01 Transitional" standard, see
   http://validator.w3.org/
  (two issues may cause the validator's warnings:
  - HTML encoding is written in HTML only if Charset of
    the 0-th text style <> DEFAULT_CHARSET;
  - non-standard <TABLE bordercolor> attribute is saved
    (for MS Internet Explorer).)
-------------------------------------------------------
1.8.7
impr: ApplyTextStyle, ApplyParaStyle, ApplyStyleConversion,
  ApplyParaStyleConversion are applied to cells of all
  selected tables.
-------------------------------------------------------
1.8.6
new: OnLoadDocument event for DBRichViewEdit.
  This is a complementary event for OnNewDocument.
  OnNewDocument is called before loading field from table,
  OnLoadDocument is called after.
new: DBRichView has OnNewDocument and OnLoadDocument as well.
new: TRVStyle.CurrentItemColor is used for drawing frame
  around the current item (all GetCurrent*** and SetCurrent***
  methods work with this item). Default value is clNone -
  no frame. Previously, TRVStyle.HoverColor was used for
  this effect.
new: extra item's integer property: rvepNoHTMLImageSize.
  if<>0, image size is not saved in HTML,
  even if rvsoImageSizes is included in Options for SaveHTML
  (this option is ignored if rvepImageWidth or Height are non-zero).
  Can be applied to: pictures, hot-pictures, bullets, hotspots,
  list markers.
fix: fixes and improvements in RTF import, table layout,
  Unicode bidirected text
-------------------------------------------------------
1.8.5
impr: table layout algorithm: handles wide merged cells smarter.
-------------------------------------------------------
1.8.4
new: extra items' string properties.
  All methods working with items' string properties
  are similar to the methods working with item's
  integer properties, but have 'Str' instead of 'Int'
  in names. 
  For example, SetItemExtraStrProperty is like
  SetItemExtraIntProperty.
  The following properties are available:
  - rvespAlt - text representation of image. Only for
    pictures and hot-pictures. Saved as <IMG alt>
    attribute in HTML files. Must not contain
    line breaks and double quotes.
  - rvespHint - item popup hint displayed if 
    rvoShowItemHints is in RichView.Options and
    RichView.ShowHint=True.
    Hints are displayed by assigning RichView.Hint.
    See also OnItemHint event.
    Note: this new feature adds 4 bytes in memory to each
    item. If you do not need it, enable RVDONOTUSEITEMHINTS
    compiler define in RV_Defs.inc. After enabling this
    define, you can still use OnItemHint.
    Hint is applicable to all item types (but not shown 
    for controls and tables).
    Hint must not contain line breaks and double quotes.
    When exporting to HTML, it is exported as "title" attribute
    For RTF, hints are exported and imported only for
    hyperlinks.
    In OnWriteHyperlink event, the proper value of
    Extras parameter is set by default.
  - rvespImageFileName - for pictures, hot-pictures,
    tables. Provides a place to store image file name
    (background image for table).
    For tables, this value is also accessible as
    table.BackgroundImageFileName.
new: new property for table cells: 
  BackgroundImageFileName: String;
new: rvsoUseItemImageFileNames option for SaveOptions parameter
  of SaveHTML and SaveHTMLEx. If included, images with
  defined (non-empty) file-names will not be saved, but
  their file-names will be written in HTML (image paths are 
  relative to the path of HTML file).
  If this option is included, OnHTMLSavePicture and 
  OnSavePicture2 events have the initial value of Location
  parameter equal to the image file name.
new: OnItemHint event. Occurs when mouse moves over the
  item, allows you to customize hint (default value for
  hint is a value of rvespHint property.
  See example in Examples.txt.
impr: all TRichViewEdit methods for inserting one item
  return boolean value now: True if the item was
  successfully inserted (insertion can fail because of
  protection). You can use SetCurrent*** method to
  set additional properties of this item after insertion.
impr: DocProperties strings now can contain #13 and #10 
  characters, but cannot contain #1 and #2 characters.    
impr: DBRichViewEdit stores a caret position when posting
  changes or switching to the editing mode (FieldFormat 
  must be rbdbRVF).
-------------------------------------------------------
1.8.3
new: drawing special characters (paragraph marks and
  dots in spaces)
  Include rvoShowSpecialCharacters in TRichView.Options.
-------------------------------------------------------
1.8.2
impr: faster redrawing on typing
fix: improvements and fixes in CSS output.
new: property TRVOfficeConverter.PreviewMode: Boolean.
  If set to True, converters will do their work silently,
  without displaying any dialogs. They can also produce
  lower quality results in this mode (that depends on
  converter).
