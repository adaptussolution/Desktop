
{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRVRTFReader: Rich Text Format (RTF) parser.    }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit  RVRTF;
interface

{$I RV_Defs.inc}
uses Windows, Classes, Graphics,  StdCtrls,  SysUtils,
     {$IFDEF RICHVIEW}
     RVFuncs,
     {$ENDIF}
     RVClasses, Forms,
     {$IFNDEF RVDONOTUSEJPEGIMAGE}
     Jpeg,
     {$ENDIF}
     RVRTFErr;

type

   TRVRTFHighlightConvert = (rtf_hl_Ignore, rtf_hl_FixedColors, rtf_hl_ColorTable);

   TRVRTFReader = class;

   TRVRTFPosition = (rtf_ts_ContinuePara, rtf_ts_NewLine, rtf_ts_NewPara);

   TRVRTFNewTextEvent = procedure (Sender: TRVRTFReader; const Text: String;
     Position: TRVRTFPosition) of object;

   TRVRTFHeaderFooterType = (rtf_hf_MainText, rtf_hf_Header, rtf_hf_Footer);

   TRVRTFHeaderFooterEvent = procedure (Sender: TRVRTFReader;
     HFType: TRVRTFHeaderFooterType; Starting: Boolean;
     var Supported: Boolean) of object;

   TRVRTFImportPictureEvent = procedure (Sender: TRVRTFReader;
     const Location: String; var Graphic: TGraphic) of object;

   {$IFDEF RICHVIEWCBDEF3}
   TRVRTFNewUnicodeTextEvent = procedure (Sender: TRVRTFReader;
     const Text: WideString; Position: TRVRTFPosition) of object;
   {$ENDIF}

   TRVRTFProgressStage = (
    rvprtfprStarting,
    rvprtfprRunning,
    rvprtfprEnding);

    TRVRTFProgressEvent = procedure (Sender: TRVRTFReader;
      Stage: TRVRTFProgressStage; PercentDone: Byte) of object;
   {------------------------------------------------------------------------------}
    TRVRTFParaListType = (
                 rtf_pn_Default,
                 // both for old and new style
                 rtf_pn_Decimal, rtf_pn_LowerLetter, rtf_pn_UpperLetter,
                 rtf_pn_LowerRoman, rtf_pn_UpperRoman,
                 // for new style
                 rtf_pn_Bullet
                 );

    TRVRTFPictureType = (rtf_pict_EMF, rtf_pict_PNG, rtf_pict_JPEG,
                         rtf_pict_MacPict, rtf_pict_PmMetafile,
                         rtf_pict_WMF, rtf_pict_DIB, rtf_pict_DDB);

    TRVRTFPicture = class
      private
        FType: TRVRTFPictureType;
        FMetafileMapMode: Integer; // MM_ constant
        FPicW, FPicH, FPicWGoalTw, FPicHGoalTw, FPicScaleX, FPicScaleY: Integer;
        FWBMWidthBytes, FWBMBitsPixel, FWBMPlanes: Integer;
        FMetafileWithBitmap: Boolean;
        FShpPict: Boolean;
      public
        FData: TMemoryStream;
        SuggestedWidth, SuggestedHeight: Integer;
        constructor Create;
        destructor Destroy; override;
        property PicW: Integer read FPicW;
        property PicH: Integer read FPicH;
        property PicWGoalTw: Integer read FPicWGoalTw;
        property PicHGoalTw: Integer read FPicHGoalTw;
        property PicScaleX: Integer read FPicScaleX;
        property PicScaleY: Integer read FPicScaleY;
        property PicType: TRVRTFPictureType read FType;
        property MetafileWithBitmap: Boolean read FMetafileWithBitmap;
        property ShpPict: Boolean read FShpPict;
    end;

    TRVRTFObjectType = (rtf_obj_Emb, rtf_obj_Link, rtf_obj_AutLink, rtf_obj_Sub,
      rtf_obj_Pub, rtf_obj_ICEmb, rtf_obj_HTML, rtf_obj_OCX);

    TRVRTFObject = class
      private
        FType: TRVRTFObjectType;
        FData: TMemoryStream;
        FWidthTw, FHeightTw: Integer;
      public
        constructor Create;
        destructor Destroy; override;
        property ObjType: TRVRTFObjectType read FType;
        property Data: TMemoryStream read FData;
        property WidthTw: Integer read FWidthTw;
        property HeightTw: Integer read FHeightTw;
    end;

   TRVRTFTableEventKind = (rvf_tbl_TableStart, rvf_tbl_TableEnd,
                           rvf_tbl_RowEnd, rvf_tbl_CellEnd);

   TRVRTFTableEvent = procedure (Sender: TRVRTFReader;
     WhatHappens: TRVRTFTableEventKind) of object;
   TRVRTFNewPictureEvent = procedure (Sender: TRVRTFReader; RTFPicture: TRVRTFPicture;
     Graphic: TGraphic; Position: TRVRTFPosition; var Inserted: Boolean) of object;
   TRVRTFNewObjectEvent = procedure (Sender: TRVRTFReader; RTFObject: TRVRTFObject;
     Position: TRVRTFPosition; var Inserted: Boolean) of object;

   {-------------------- character properties -----------------------}
   TRVRTFAlignment = (rtf_al_Left, rtf_al_Right, rtf_al_Center, rtf_al_Justify);
   TRVRTFVAlign = (rtf_val_Top, rtf_val_Bottom, rtf_val_Center);
   TRVRTFSScriptType = (rtf_ss_Normal, rtf_ss_Subscript, rtf_ss_Superscript);
   TRVRTFFontStyleEx = (rtf_fs_AllCaps {$IFDEF RVTEXTFOOTNOTES},rtf_fs_Footnote{$ENDIF});
   TRVRTFFontStylesEx = set of TRVRTFFontStyleEx;

   TRVRTFCharProperties = class
     private
       FHidden: Boolean;
       FSize: Integer;
       FColor, FBackColor: TColor;
       FFontIndex: Integer;
       FStyle: TFontStyles;
       FStyleEx: TRVRTFFontStylesEx;
       FCharScaleX: Integer;
       FSScriptType: TRVRTFSScriptType;
       FCharSpacingTw: Integer;
       FLanguage: Cardinal;
       FFontName: String;
       {$IFDEF RVTEXTFOOTNOTES}
       FFootNote: String;
       {$ENDIF}
     public
       constructor Create;
       procedure Reset (DefLanguage: Cardinal; DefFontIndex: Integer);
       procedure Assign(Source: TRVRTFCharProperties);
       property Size     : Integer     read FSize;
       property Color    : TColor      read FColor;
       property BackColor: TColor      read FBackColor;
       property FontIndex: Integer     read FFontIndex;
       property Style    : TFontStyles read FStyle;
       property StyleEx  : TRVRTFFontStylesEx read FStyleEx;
       property CharScaleX: Integer    read FCharScaleX;
       property SScriptType: TRVRTFSScriptType read FSScriptType;
       property CharSpacingTw: Integer read FCharSpacingTw;
       property Hidden: Boolean        read FHidden;
       property FontName: String       read FFontName;
       property Language: Cardinal     read FLanguage;
       {$IFDEF RVTEXTFOOTNOTES}
       property FootNote: String       read FFootnote;
       {$ENDIF}
   end;
   {-------------------- paragraph properties -----------------------}
   TRVRTFBorderType = (rtf_brdr_None,
                   rtf_brdr_SingleThickness, rtf_brdr_DoubleThickness,
                   rtf_brdr_Shadow, rtf_brdr_Double, rtf_brdr_Dot,
                   rtf_brdr_Dash, rtf_brdr_Hairline,
                   rtf_brdr_DashSmall, rtf_brdr_DotDash,
                   rtf_brdr_DotDotDash, rtf_brdr_Triple,
                   rtf_brdr_ThickThinSmall, rtf_brdr_ThinThickSmall,
                   rtf_brdr_ThinThickThinSmall,
                   rtf_brdr_ThickThinMed, rtf_brdr_ThinThickMed,
                   rtf_brdr_ThinThickThinMed,
                   rtf_brdr_ThickThinLarge, rtf_brdr_ThinThickLarge,
                   rtf_brdr_ThinThickThinLarge,
                   rtf_brdr_Wavy, rtf_brdr_DoubleWavy,
                   rtf_brdr_Striped, rtf_brdr_Emboss,
                   rtf_brdr_Engrave,
                   rtf_brdr_Inset, rtf_brdr_Outset);
   TRVRTFSide = (rtf_side_Left, rtf_side_Top, rtf_side_Right, rtf_side_Bottom);

   TRVRTFBorderSide = class
     private
        FBorderType: TRVRTFBorderType;
        FWidthTw: Integer;
        FColor: TColor;
        FSpaceTw: Integer; // Space between borders and the paragraph
     public
        procedure Assign(Source: TRVRTFBorderSide);
        property BorderType: TRVRTFBorderType read FBorderType;
        procedure Reset;
        property WidthTw   : Integer      read FWidthTw;
        property Color     : TColor       read FColor;
        property SpaceTw   : Integer      read FSpaceTw;
   end;

   TRVRTFParaBorder = class
     private
       FSides: array [TRVRTFSide] of TRVRTFBorderSide;
       function GetSides(Index: TRVRTFSide): TRVRTFBorderSide;
     public
       procedure Assign(Source: TRVRTFParaBorder);
       destructor Destroy; override;
       procedure Reset;
       property Sides[Index:TRVRTFSide]:TRVRTFBorderSide read GetSides;
   end;

   TRVRTFMarkerProp = (rtfmp_FontIndex, rtfmp_Size, rtfmp_Color,
     rtfmp_Bold, rtfmp_Italic, rtfmp_Underline, rtfmp_StrikeOut);
   TRVRTFMarkerProps = set of TRVRTFMarkerProp;

   TRVRTFCustomMarkerProperties = class
     private
       FListType: TRVRTFParaListType;
       FFontIndex, FIndentTw, FSpaceTw, FStart: Integer;
       FFontStyle: TFontStyles;
       FColor: TColor;
       FAlignment: TRVRTFAlignment;
       FFixedProperties: TRVRTFMarkerProps;
       procedure ChangeFontStyle(fs: TFontStyle; Val: Integer);
     public
       FFontSize: Integer;
       constructor Create;
       procedure Reset; dynamic;
       procedure Assign(Source: TRVRTFCustomMarkerProperties; FromDefaults: Boolean);
       procedure UpdateFrom(CharProps: TRVRTFCharProperties);
       property ListType: TRVRTFParaListType read FListType;
       property FontIndex: Integer           read FFontIndex;
       property FontSize: Integer            read FFontSize;
       property IndentTw: Integer            read FIndentTw;
       property SpaceTw: Integer             read FSpaceTw;
       property Start: Integer               read FStart;
       property FontStyle: TFontStyles       read FFontStyle;
       property Color    : TColor            read FColor;
       property Alignment: TRVRTFAlignment   read FAlignment;
   end;


   TRVRTFMarkerProperties = class (TRVRTFCustomMarkerProperties)
     private
       FLevel: Integer;
       FHanging: Boolean;
       FTextAfter, FTextBefore: String;
     public
       procedure Reset; override;
       procedure Assign(Source: TRVRTFMarkerProperties; FromDefaults: Boolean);
       property Level: Integer               read FLevel;
       property TextAfter: String            read FTextAfter;
       property TextBefore: String           read FTextBefore;
       property Hanging: Boolean             read FHanging;
   end;

   TRVRTFLevelFollow97 = (rtf_lf_Tab, rtf_lf_Space, rtf_lf_None);

   TRVRTFListLevel97 = class (TRVRTFCustomMarkerProperties)
     private
       FOldStyle, FLegal, FNoRestart: Boolean;
       FText,FNumbers: String;
       {$IFDEF RICHVIEWCBDEF3}
       FTextW: WideString;
       {$ELSE}
       FTextW: String;
       {$ENDIF}
       FFollow: TRVRTFLevelFollow97;
       FLeftIndentTw, FFirstIndentTw, FTabPosTw: Integer;
     public
       FFontSizeDefined: Boolean;
       procedure Reset; override;
       procedure Assign(Source: TRVRTFListLevel97);
       property OldStyle: Boolean           read FOldStyle;
       property Legal: Boolean              read FLegal;
       property NoRestart: Boolean          read FNoRestart;
       property Text: String                read FText;
       property TextW: {$IFDEF RICHVIEWCBDEF3}WideString{$ELSE}String{$ENDIF} read FTextW;
       property Numbers: String             read FNumbers;
       property Follow: TRVRTFLevelFollow97 read FFollow;
       property LeftIndentTw: Integer       read FLeftIndentTw;
       property FirstIndentTw: Integer      read FFirstIndentTw;
       property TabPosTw: Integer           read FTabPosTw;
   end;

   TRVRTFList97 = class (TRVList)
     private
       FId, FTemplateId: Integer;
       FSimple: Boolean;
       FName: String;
       function Get(Index: Integer): TRVRTFListLevel97;
       procedure Put(Index: Integer; const Value: TRVRTFListLevel97);
     protected
       function GetLastLevel: TRVRTFListLevel97;
       procedure AddNew;
     public
       property Items[Index: Integer]: TRVRTFListLevel97 read Get write Put; default;
       property Id: Integer         read FId;
       property TemplateId: Integer read FTemplateId;
       property Simple: Boolean     read FSimple;
       property Name: String        read FName;
   end;

   TRVRTFListTable97 = class (TRVList)
     private
       function Get(Index: Integer): TRVRTFList97;
       procedure Put(Index: Integer; const Value: TRVRTFList97);
     protected
       function GetLastList: TRVRTFList97;
       function FindList(ID: Integer): Integer;
       procedure AddNew;
     public
       property Items[Index: Integer]: TRVRTFList97 read Get write Put; default;
   end;

   TRVRTFListOverrideLevel97 = class
     private
       FUseStart: Boolean;
       FStart: Integer;
     public
       constructor Create;
       property Start: Integer    read FStart;
       property UseStart: Boolean read FUseStart;
   end;

   TRVRTFListOverride97 = class (TRVList)
     private
       FListIndex, FNumber, FOverrideCount: Integer;
       function Get(Index: Integer): TRVRTFListOverrideLevel97;
       procedure Put(Index: Integer; const Value: TRVRTFListOverrideLevel97);
     public
       function GetLastLevel: TRVRTFListOverrideLevel97;
       procedure AddNew;
       property ListIndex: Integer read FListIndex;
       property Number: Integer    read FNumber;
       property OverrideCount: Integer read FOverrideCount;
       property Items[Index: Integer]: TRVRTFListOverrideLevel97 read Get write Put; default;
   end;

   TRVRTFListOverrideTable97 = class (TRVList)
     private
       function Get(Index: Integer): TRVRTFListOverride97;
       procedure Put(Index: Integer; const Value: TRVRTFListOverride97);
     protected
       function GetLastListOverride: TRVRTFListOverride97;
       function FindListOverride(Number: Integer): Integer;
       procedure AddNew;
     public
       property Items[Index: Integer]: TRVRTFListOverride97 read Get write Put; default;
   end;


   TRVRTFParaProperties = class
     private
       FLeftIndentTw, FRightIndentTw, FFirstIndentTw:  Integer;
       FSpaceBeforeTw, FSpaceAfterTw: Integer;
       FAlignment:  TRVRTFAlignment;
       FBorder: TRVRTFParaBorder;
       FMarkerProps: TRVRTFMarkerProperties;
       FCurBorderSide: TRVRTFSide;
       FLineSpacing: Integer;
       FLineSpacingMulti: Boolean;
       FColor: TColor;
       FNestingLevel: Integer;
       FInTable: Boolean;
       Shading: Integer;
       ForeColor: TColor;
       FListOverrideIndex, FListLevel: Integer;
       FKeepLinesTogether, FKeepWithNext: Boolean;
       NoTableEv, NoResetLev: Boolean;
       function GetBorder: TRVRTFParaBorder;
       function GetMarkerProps: TRVRTFMarkerProperties;
       procedure Finalize;
     public
       constructor Create;
       destructor Destroy; override;
       procedure Reset;
       procedure Assign(Source: TRVRTFParaProperties);
       function HasBorder: Boolean;
       function HasMarker: Boolean;
       property LeftIndentTw : Integer         read FLeftIndentTw;
       property RightIndentTw: Integer         read FRightIndentTw;
       property FirstIndentTw: Integer         read FFirstIndentTw;
       property SpaceBeforeTw: Integer         read FSpaceBeforeTw;
       property SpaceAfterTw : Integer         read FSpaceAfterTw;
       property Alignment    : TRVRTFAlignment read FAlignment;
       property Border       : TRVRTFParaBorder read GetBorder;
       property MarkerProps  : TRVRTFMarkerProperties read GetMarkerProps;
       property Color        : TColor          read FColor;
       property LineSpacing  : Integer         read FLineSpacing;
       property LineSpacingMulti: Boolean      read FLineSpacingMulti;
       property NestingLevel : Integer         read FNestingLevel;
       property InTable      : Boolean         read FInTable;
       property ListOverrideIndex: Integer     read FListOverrideIndex;
       property ListLevel    : Integer         read FListLevel;
       property KeepLinesTogether: Boolean     read FKeepLinesTogether;
       property KeepWithNext: Boolean          read FKeepWithNext;
    end;
   {-------------------- table cell properties --------------------}
   TRVRTFCellMerge = (rtf_cm_None, rtf_cm_First, rtf_cm_Merged);
   TRVRTFCellProperties = class
     private
       FHMerge, FVMerge: TRVRTFCellMerge;
       InvertWidth: Boolean;
       FBestWidth: Integer;
       FColor: TColor;
       FVAlign: TRVRTFVAlign;
       FBorder: TRVRTFParaBorder;
       FCurBorderSide: TRVRTFSide;
       FRightBoundaryTw: Integer;
       ForeColor: TColor;
       Shading: Integer;
       procedure Finalize;
     public
       {$IFDEF RICHVIEW}
       BestHeight: Integer;
       {$ENDIF}
       constructor Create;
       destructor Destroy; override;
       procedure Reset;
       procedure Assign(Source: TRVRTFCellProperties);
       property HMerge         : TRVRTFCellMerge read FHMerge;
       property VMerge         : TRVRTFCellMerge read FVMerge;
       property BestWidth      : Integer         read FBestWidth; // positive: twips; negative: 50th of %; 0: undefined
       property Color          : TColor          read FColor;
       property VAlign         : TRVRTFVAlign    read FVAlign;
       property Border         : TRVRTFParaBorder read FBorder;
       property RightBoundaryTw: Integer         read FRightBoundaryTw;
   end;

   TRVRTFCellPropsList = class (TRVList)
      private
        function Get(Index: Integer): TRVRTFCellProperties;
        procedure Put(Index: Integer; const Value: TRVRTFCellProperties);
      public
        procedure AddNew;
        procedure AssignItems(Source: TRVRTFCellPropsList);
        property Items[Index: Integer]: TRVRTFCellProperties read Get write Put; default;
   end;

   {-------------------- table row properties ---------------------}
   TRVRTFRowProperties = class
     private
       InvertWidth, FHeading: Boolean;
       FGapHTw, FLeftTw, FHeightTw: Integer;
       FPaddingTw, FSpacingTw: array[TRVRTFSide] of Integer;
       FUsePadding, FUseSpacing: array[TRVRTFSide] of Boolean;
       FBestWidth: Integer;
       FBorder: TRVRTFParaBorder;
       FCurBorderSide: TRVRTFSide;
       FCellProps: TRVRTFCellPropsList;
       NewCellProps, AssumedLastCell: Boolean;
       function GetPaddingTw(Index: TRVRTFSide): Integer;
       function GetSpacingTw(Index: TRVRTFSide): Integer;
       function GetUsePadding(Index: TRVRTFSide): Boolean;
       function GetUseSpacing(Index: TRVRTFSide): Boolean;
       function GetLastCellProp: TRVRTFCellProperties;
       procedure Finalize;
     public
       {$IFDEF RICHVIEW}
       RichViewSpecial: Boolean;
       {$ENDIF}
       constructor Create;
       destructor Destroy; override;
       procedure Reset;
       procedure Assign(Source: TRVRTFRowProperties);
       property CellProps      : TRVRTFCellPropsList read FCellProps;
       property GapHTw         : Integer         read FGapHTw;
       property LeftTw         : Integer         read FLeftTw;
       property HeightTw       : Integer         read FHeightTw;
       property BestWidth      : Integer         read FBestWidth; // positive: twips; negative: 50-th of %; 0: undefined
       property PaddingTw  [Index:TRVRTFSide]: Integer read GetPaddingTw;
       property SpacingTw  [Index:TRVRTFSide]: Integer read GetSpacingTw;
       property UsePadding [Index:TRVRTFSide]: Boolean read GetUsePadding;
       property UseSpacing [Index:TRVRTFSide]: Boolean read GetUseSpacing;
       property Border         : TRVRTFParaBorder read FBorder;
       property Heading        : Boolean         read FHeading;
   end;
   {-------------------- section properties -----------------------}
   TRVRTFSectionBreakType = (rtf_sbk_None, rtf_sbk_Column, rtf_sbk_Even, rtf_sbk_Odd, rtf_sbk_Page);
   TRVRTFPageNumberFormat = (rtf_pg_Decimal,
                             rtf_pg_UpperRoman, rtf_pg_LowerRoman,
                             rtf_pg_UpperLetter, rtf_pg_LowerLetter);

   TRVRTFSectionProperties = class
     private
       FColumnCount: Integer;
       FSectionBreakType: TRVRTFSectionBreakType;
       FPageNumberXTw, FPageNumberYTw: Integer;
       FPageNumberFormat: TRVRTFPageNumberFormat;
       FHeaderYTw, FFooterYTw: Integer;
       FDefMarkerPropsList: TRVList;
       procedure InitListDefaults;
     public
       constructor Create;
       procedure Reset;
       destructor Destroy; override;
       procedure Assign(Source: TRVRTFSectionProperties);
       property ColumnCount     : Integer                read FColumnCount;
       property SectionBreakType: TRVRTFSectionBreakType read FSectionBreakType;
       property PageNumberXTw   : Integer                read FPageNumberXTw;
       property PageNumberYTw   : Integer                read FPageNumberYTw;
       property PageNumberFormat: TRVRTFPageNumberFormat read FPageNumberFormat;
       property HeaderYTw       : Integer                read FHeaderYTw;
       property FooterYTw       : Integer                read FFooterYTw;
   end;
   {-------------------- document properties -----------------------}
    TRVRTFDocProperties = class
      private
        FPaperWidthTw, FPaperHeightTw: Integer;
        FLeftMarginTw, FTopMarginTw, FRightMarginTw, FBottomMarginTw: Integer;
        FPageNumberStart: Integer;
        FFacingPages: Boolean;
        FLandscape: Boolean;
      public
        constructor Create;
        procedure Reset;
        procedure Assign(Source: TRVRTFDocProperties);
        property PaperWidthTw   : Integer read FPaperWidthTw;
        property PaperHeightTw  : Integer read FPaperHeightTw;
        property LeftMarginTw   : Integer read FLeftMarginTw;
        property TopMarginTw    : Integer read FTopMarginTw;
        property RightMarginTw  : Integer read FRightMarginTw;
        property BottomMarginTw : Integer read FBottomMarginTw;
        property PageNumberStart: Integer read FPageNumberStart;
        property FacingPages    : Boolean read FFacingPages;
        property Landscape      : Boolean read FLandscape;
    end;
//----------------------------------------------------------------------
    TRTFrds = ( rdsNorm, rdsSkip,  rdsFontTable, rdsStyleSheet, rdsColorTable,
                rdsStyleSheetEntry, rdsPict, rdsShpPict, rdsObject, rdsObjData,
                rdsField, rdsFldInst, rdsPN, rdsPNTextAfter, rdsPNTextBefore,
                rdsPNSecLvl, rdsListTable, rdsList, rdsListLevel,
                rdsListName, rdsListLevelText, rdsListLevelNumbers,
                rdsLOTable, rdsLO, rdsLOLevel);       // Rtf Destination State
//----------------------------------------------------------------------
    TRTFris = ( risNorm, risBin, risHex );// Rtf Internal State
//----------------------------------------------------------------------
// What types of properties are there?
    TRTFIPROP = (ipropBold, ipropItalic, ipropUnderline, ipropStrike,
              ipropCharScaleX, ipropCharSpacing, ipropSScript, ipropAllCaps,
              ipropFontSize, ipropTextColor, ipropTextBackColor, ipropHighlight,
              ipropLanguage, ipropDefLanguage,
              ipropSL, ipropSLMult,
              ipropLeftInd,
              ipropRightInd, ipropFirstInd, ipropCols, ipropPgnX,
              ipropPgnY, ipropHeaderY, ipropFooterY, ipropXaPage, ipropYaPage, ipropXaLeft,
              ipropXaRight, ipropYaTop, ipropYaBottom, ipropPgnStart,
              ipropSbk, ipropPgnFormat, ipropFacingp, ipropLandscape,
              ipropJust, ipropPard, ipropPlain, ipropSectd,
              ipropF, ipropDefF, ipropSpaceBefore, ipropSpaceAfter,
              {$IFDEF RVTEXTFOOTNOTES}
              ipropfootnote,
              {$ENDIF}
              ipropParaBorderType, ipropParaBorderSide, ipropParaBorderWidth,
              ipropParaBorderColor, ipropParaBorderSpace,
              ipropParaColor, ipropParaFColor, ipropParaShading,
              ipropAnsiCodePage, ipropU, ipropUC,
              ipropPage, ipropField, ipropFldInst, ipropFldRslt, ipropHidden,
              ipropKeepLinesTogether, ipropKeepWithNext,
              // color table:
              ipropRed, ipropGreen, ipropBlue,
              // font table:
              ipropFontFamily,
              ipropFCharset,
              // style sheet
              ipropS, ipropCS, ipropDS, ipropSBasedOn, irpropSNext, ipropAdditive, ipropSHidden,
              // picture
              ipropPicW, ipropPicH, ipropPicScaleX, ipropPicScaleY, ipropPicWGoal, ipropPicHGoal, 
              ipropWBMWidthBytes, ipropWBMBitsPixel, ipropWBMPlanes, ipropPictureType,
              ipropMetafile, ipropPicBmp,
              // objects
              ipropObjType, ipropObjWidth, ipropObjHeight,
              // lists
              ipropPNLevel, ipropPNHanging, ipropPNType,
              ipropPNBold, ipropPNItalic, ipropPNUnderline, ipropPNStrike,
              ipropPNColor, ipropPNF, ipropPNFontSize, ipropPNIndent, ipropPNSp,
              ipropPNAlign, ipropPNStart, ipropPNSecLvl,
              // new lists
              ipropListId, ipropListTemplateId, ipropListSimple, ipropListName,
              ipropLevelStartAt, ipropLevelNumberType, ipropLevelJustify, ipropLevelOld,
              ipropLevelIndent, ipropLevelSpace, ipropLevelFollow,
              ipropLevelLegal, ipropLevelNoRestart, ipropLOCount, ipropLONumber,
              ipropLOStart, ipropLevel, ipropTX,
              // tables
              ipropRowEnd, ipropCellEnd, ipropInTbl, ipropItap,
              ipropTRowD, ipropRowAlign, ipropTRGapH, ipropTRLeft, ipropTRRowHeight,
              ipropTRHeader, ipropTRPaddL, ipropTRPaddR, ipropTRPaddT, ipropTRPaddB,
              ipropTRPaddFL, ipropTRPaddFR, ipropTRPaddFT, ipropTRPaddFB,
              ipropTRSpdL, ipropTRSpdR, ipropTRSpdT, ipropTRSpdB,
              ipropTRSpdFL, ipropTRSpdFR, ipropTRSpdFT, ipropTRSpdFB,
              ipropTRwWidth, ipropTRftsWidth,
              ipropCLVMerge, ipropCLHMerge,
              ipropCLwWidth, ipropCLftsWidth, ipropCLColor, ipropCLFColor, ipropCLShading,ipropCLVertAl,
              ipropCellX, ipropTRBorderSide, ipropCLBorderSide, ipropNoTableEv,
              ipropNoResetLev,
              {$IFDEF RICHVIEW}
              ipropRVCellBestWidth,ipropRVCellBestHeight,ipropRVTableBestWidth,
              {$ENDIF}
              ipropMax);
//----------------------------------------------------------------------
    TRTFACTN = (actnSpec, actnByte, actnWord);
//----------------------------------------------------------------------
    TRTFPROPTYPE = (propChp, propPap, propSep, propDop);
//----------------------------------------------------------------------
    TRTFpropmod = record
      actn: TRTFACTN;       // size of value
      prop: TRTFPROPTYPE;   // structure containing value
      offset: Integer;      // offset of value from base of structure
    end; //PROP;
//----------------------------------------------------------------------
    TRTFIPFN = ( ipfnBin, ipfnHex, ipfnSkipDest );
//----------------------------------------------------------------------
    TRTFIDEST = ( idestShpPict, idestNonShpPict,
                  idestPict, idestSkip,  idestFontTable,  idestStyleSheet, idestColorTable,
                  idestStyleSheetParaStyle, idestStyleSheetCharStyle, idestStyleSheetSectStyle,
                  idestField, idestFldInst, idestFldRslt,
                  idestNestedTableProps, idestHeader, idestFooter,
                  idestPNTextAfter, idestPNTextBefore,
                  idestPN, idestPNSecLvl,
                  idestListTable, idestList, idestListName, idestListLevel,
                  idestLevelText, idestLevelNumbers, idestLOTable, idestLO, idestLOLevel,
                  idestObject, idestObjData, idestObjResult);
//----------------------------------------------------------------------
    TRVRTFKeywordType = ( rtf_kwd_Char, rtf_kwd_WideChar, rtf_kwd_Dest, rtf_kwd_Prop, rtf_kwd_Spec );
//----------------------------------------------------------------------
    TRVRTFKeywordAffect = (rtf_af_None, rtf_af_CharProp, rtf_af_ParaProp,
                           rtf_af_BothProp);
    TRVRTFsymbol = record
      Keyword: String;    // RTF keyword
      DefValue: Integer;  // default value to use
      UseDef: Boolean;    // true to use default value from this table
      Kwd: TRVRTFKeywordType; // base action to take
      Idx: Integer;       // index into property table    if Kwd == trf_kwd_Prop
                          // index into destination table if Kwd == trf_kwd_Dest
                          // character to print           if Kwd == trf_kwd_Char
      AffectTo: TRVRTFKeywordAffect;
    end;
    PRVRTFsymbol = ^TRVRTFsymbol;
    {----------------------------------------------------------------------}
    // RTF Style Sheet
    TRVRTFStyleSheetType = (rtf_sst_Char, rtf_sst_Par, rtf_sst_Sect);

    TRVRTFReaderState = class;

    TRVRTFStyleSheetEntry = class
      private
        FParaProps: TRVRTFParaProperties;
        FCharProps: TRVRTFCharProperties;
        FAdditive, FHidden: Boolean;
        FNumber:  Integer;
        FStyleType: TRVRTFStyleSheetType;
        FBasedOn, FNext:  TRVRTFStyleSheetEntry;
        FName: String;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Assign(Source:TRVRTFReaderState);
        property ParaProps: TRVRTFParaProperties  read FParaProps;
        property CharProps: TRVRTFCharProperties  read FCharProps;
        property Additive : Boolean               read FAdditive;
        property Hidden   : Boolean               read FHidden;
        property Number   : Integer               read FNumber;
        property StyleType: TRVRTFStyleSheetType  read FStyleType;
        property BasedOn  : TRVRTFStyleSheetEntry read FBasedOn;
        property Next     : TRVRTFStyleSheetEntry read FNext;
        property Name     : String                read FName;
    end;

    TRVRTFStyleSheet = class (TRVList)
      private
        function Get(Index: Integer): TRVRTFStyleSheetEntry;
        procedure Put(Index: Integer; const Value: TRVRTFStyleSheetEntry);
        procedure AddPara(Number: Integer);
      public
        function GetEntry(Number: Integer): TRVRTFStyleSheetEntry;
        property Items[Index: Integer]: TRVRTFStyleSheetEntry read Get write Put; default;
    end;
    {----------------------------------------------------------------------}
    // RTF font table

    TRVRTFFontFamily = (rtf_ff_Default, rtf_ff_Roman, rtf_ff_Swiss,
                        rtf_ff_Modern, rtf_ff_Script, rtf_ff_Decorative,
                        rtf_ff_Symbol, rtf_ff_BiDi);

    TRVRTFFont = class
      public
        Number: Integer;
        Name: String;
        Family: TRVRTFFontFamily;
        {$IFDEF RICHVIEWCBDEF3}
        Charset: TFontCharset;
        {$ENDIF}
        constructor Create;
    end;

    TRVRTFFontList = class (TRVList)
      private
        function Get(Index: Integer): TRVRTFFont;
        procedure Put(Index: Integer; const Value: TRVRTFFont);
        procedure RemoveChasetFromNames;
      public
        function GetFontIndex(Number, Default: Integer): Integer;
        procedure Add(Number: Integer);
        property Items[Index: Integer]: TRVRTFFont read Get write Put; default;
    end;
    {----------------------------------------------------------------------}
    // RTF color table
    TRVRTFColorList = class (TList)
      private
        function Get(Index: Integer): TColor;
        procedure Put(Index: Integer; const Value: TColor);
        procedure ResetLast;
        procedure SetLastRed(Value: Integer);
        procedure SetLastGreen(Value: Integer);
        procedure SetLastBlue(Value: Integer);
        procedure Finalize;
      public
        procedure Add;
        property Items[Index: Integer]: TColor read Get write Put; default;
    end;
{------------------------------------------------------------------------------}
    TRVRTFCurrentBorderType = (rtf_bt_Para, rtf_bt_Row, rtf_bt_Cell);
    TRVRTFReaderState =  class
      private
        FDefLanguage: Cardinal;
        FCharProps: TRVRTFCharProperties;
        FParaProps: TRVRTFParaProperties;
        FSectProps: TRVRTFSectionProperties;
        FDocProps:  TRVRTFDocProperties;
        FRowProps:  TRVRTFRowProperties;
        FCurrentBorderType: TRVRTFCurrentBorderType;
        rds: TRTFrds;
        ris: TRTFris;
        FFieldCode: String;
        FFieldPictureIncluded: Boolean;
        DefFontNumber, DefFontIndex: Integer;
        FHFType: TRVRTFHeaderFooterType;
        procedure ChangeFontStyle(fs: TFontStyle; Val: Integer);
        procedure ChangeFontStyleEx(fs: TRVRTFFontStyleEx; Val: Integer);
        function GetRowProps: TRVRTFRowProperties;
        function GetCurrentBorderSide: TRVRTFBorderSide;
      public
        constructor Create;
        destructor Destroy; override;
        procedure Assign(Source: TRVRTFReaderState);
        procedure Reset;
        property ParaProps: TRVRTFParaProperties    read FParaProps;
        property CharProps: TRVRTFCharProperties    read FCharProps;
        property SectProps: TRVRTFSectionProperties read FSectProps;
        property DocProps:  TRVRTFDocProperties     read FDocProps;
        property RowProps:  TRVRTFRowProperties     read GetRowProps;
        property FieldCode: String                  read FFieldCode;
        property DefLanguage: Cardinal              read FDefLanguage;
    end;
{------------------------------------------------------------------------------}
    TRVRTFReader  = class(TComponent)
      private
        Stream: TStream;
        StreamSize,InputStringIndex: Integer;
        Text,InputString: String;
        {$IFDEF RICHVIEWCBDEF3}
        TextW: WideString;
        {$ENDIF}
        LastChar: Char;
        UseLastChar: Boolean;
        FOnNewText: TRVRTFNewTextEvent;
        FOnHeaderFooter: TRVRTFHeaderFooterEvent;
        FOnImportPicture: TRVRTFImportPictureEvent;
        {$IFDEF RICHVIEWCBDEF3}
        FOnNewUnicodeText: TRVRTFNewUnicodeTextEvent;
        {$ENDIF}
        FOnNewPicture: TRVRTFNewPictureEvent;
        FOnNewObject: TRVRTFNewObjectEvent;
        FOnTable: TRVRTFTableEvent;
        FStyleSheet: TRVRTFStyleSheet;
        FFontTable:  TRVRTFFontList;
        FColorTable: TRVRTFColorList;
        FListTable: TRVRTFListTable97;
        FListOverrideTable: TRVRTFListOverrideTable97;
        FRTFState: TRVRTFReaderState;
        FPicture: TRVRTFPicture;
        FObject: TRVRTFObject;
        FMarkerProps: TRVRTFMarkerProperties;
        FDefCodePage, FCodePage: Cardinal;
        SkipAnsiCount, SkipNext: Integer;
        FOnRequiredPageBreak: TNotifyEvent;
        CurrentNestingLevel: Integer;
        FConvertHighlight: TRVRTFHighlightConvert;
        FOnUpdateMarker: TNotifyEvent;
        FCurPNSecLvl: Integer;
        FBasePath: String;
        FOnProgress: TRVRTFProgressEvent;
        FCallProgress: Boolean;
        ShpPictInserted, ObjectInserted: Boolean;
        FExtractMetafileBitmaps: Boolean;
        FOnEndParsing: TNotifyEvent;
        function FindKeyword(const Keyword: String): Integer;
      protected
        ForceEvenEmptyNewLine: Boolean;
        Position: TRVRTFPosition;
        cGroup: Integer;
        SaveList: TRVList;
        cbBin,
        lParam: LongInt;
        PicHexVal: Byte;
        PicHexStrt: Boolean;
        fSkipDestIfUnk: Boolean;
        fpIn: TextFile;
        function EndGroupAction(rds: TRTFrds): TRVRTFErrorCode;
        function TranslateKeyword(const Keyword: String; param: Integer; fParam:Boolean): TRVRTFErrorCode;
        function ParseSpecialProperty(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
        function ChangeDest(idest:  TRTFidest; Val: Integer): TRVRTFErrorCode;
        function ParseSpecialKeyword(ipfn:TRTFIPFN): TRVRTFErrorCode;
        function ApplyPropChange(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
        function ApplyPropChange_SSEntry(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
        function ApplyPropChange_Picture(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
        function ApplyPropChange_Object(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
        function ApplyPropChange_PN(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
        function ApplyPropChange_List(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
        function ApplyPropChange_ListLevel(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
        function ApplyPropChange_LO(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
        function ApplyPropChange_LOLevel(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
        function Parse: TRVRTFErrorCode;
        function PushRtfState: TRVRTFErrorCode;
        function PopRtfState: TRVRTFErrorCode;
        function ParseRtfKeyword: TRVRTFErrorCode;
        function ParseChar(ch: Char): TRVRTFErrorCode;
        procedure UpdateMarker;
        function FlushOutput(var NextPosition: TRVRTFPosition): TRVRTFErrorCode;
        function OutputChar(ch: Char; ACheckTableEnd, ACheckTable: Boolean): TRVRTFErrorCode;
        function InsertExternalPicture: TRVRTFErrorCode;
        function InsertSymbol: TRVRTFErrorCode;
        {$IFDEF RICHVIEWCBDEF3}
        function OutputWideChar(ch: WideChar): TRVRTFErrorCode;
        {$ENDIF}
        procedure UngetC;
        function GetC: Char;
        function IsEOF: Boolean;
        function DoNewText(Position: TRVRTFPosition; var NextPosition: TRVRTFPosition): TRVRTFErrorCode;
        function DoNewPicture(gr: TGraphic): TRVRTFErrorCode;
        function DoNewObject: TRVRTFErrorCode;
        procedure DoTable(WhatHappens: TRVRTFTableEventKind);
        procedure CheckTable(AllowEnd: Boolean);
      public
        FTableAlignment: TRVRTFAlignment;
        FTableAlignmentDefined: Boolean;      
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        function GetFieldCommandValue(const s: String): String;
        function GetFieldCommandValueEx(const s: String;
          var NextCharIndex: Integer): String;
        procedure Clear;
        function ReadFromStream(AStream: TStream): TRVRTFErrorCode;
        function ReadFromFile(const AFileName: String): TRVRTFErrorCode;
        {$IFDEF RICHVIEWCBDEF3}
        function AnsiToUnicode(const s: String; CodePage: Cardinal): WideString;
        function UnicodeToAnsi(const s: WideString): String;
        {$ENDIF}
        property StyleSheet: TRVRTFStyleSheet read FStyleSheet;
        property FontTable:  TRVRTFFontList read FFontTable;
        property ColorTable: TRVRTFColorList read FColorTable;
        property ListTable: TRVRTFListTable97 read FListTable;
        property ListOverrideTable: TRVRTFListOverrideTable97 read FListOverrideTable;
        property RTFState: TRVRTFReaderState read FRTFState;
        property BasePath: String read FBasePath write FBasePath;
      published
        property OnNewText: TRVRTFNewTextEvent read FOnNewText write FOnNewText;
        {$IFDEF RICHVIEWCBDEF3}
        property OnNewUnicodeText: TRVRTFNewUnicodeTextEvent
          read FOnNewUnicodeText write FOnNewUnicodeText;
        {$ENDIF}
        property OnNewPicture: TRVRTFNewPictureEvent read FOnNewPicture write FOnNewPicture;
        property OnNewObject: TRVRTFNewObjectEvent read FOnNewObject write FOnNewObject;
        property OnUpdateMarker: TNotifyEvent read FOnUpdateMarker write FOnUpdateMarker;
        property OnTable: TRVRTFTableEvent read FOnTable write FOnTable;
        property OnRequiredPageBreak: TNotifyEvent read FOnRequiredPageBreak write FOnRequiredPageBreak;
        property OnHeaderFooter: TRVRTFHeaderFooterEvent read FOnHeaderFooter write FOnHeaderFooter;
        property OnImportPicture: TRVRTFImportPictureEvent read FOnImportPicture write FOnImportPicture;
        property DefCodePage: Cardinal  read FDefCodePage write FDefCodePage default CP_ACP;
        property ConvertHighlight: TRVRTFHighlightConvert read FConvertHighlight write FConvertHighlight default rtf_hl_FixedColors;
        property OnProgress: TRVRTFProgressEvent read FOnProgress write FOnProgress;
        property ExtractMetafileBitmaps: Boolean read FExtractMetafileBitmaps write FExtractMetafileBitmaps default True;
        property OnEndParsing: TNotifyEvent read FOnEndParsing write FOnEndParsing;
    end;

{$IFNDEF RICHVIEW}
type
  TRV_CreateGraphicsFunction = function (GraphicClass: TGraphicClass): TGraphic;
  TRV_AfterImportGraphicsProc = procedure(Graphic: TGraphic);
var
  RV_CreateGraphics: TRV_CreateGraphicsFunction;
  RV_AfterImportGraphic: TRV_AfterImportGraphicsProc;
{$ENDIF}


implementation

// Keyword descriptions
const
 isymMax = 327{$IFDEF RICHVIEW}+3{$ENDIF};
 rgsymRtf:array[0..isymMax] of TRVRTFsymbol = (
    (Keyword:'object';   DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestObject);   AffectTo:rtf_af_None),
    (Keyword:'objemb';     DefValue:ord(rtf_obj_Emb);     UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropObjType); AffectTo:rtf_af_None),
    (Keyword:'objlink';    DefValue:ord(rtf_obj_Link);    UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropObjType); AffectTo:rtf_af_None),
    (Keyword:'objautlink'; DefValue:ord(rtf_obj_AutLink); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropObjType); AffectTo:rtf_af_None),
    (Keyword:'objsub';     DefValue:ord(rtf_obj_Sub);     UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropObjType); AffectTo:rtf_af_None),
    (Keyword:'objpub';     DefValue:ord(rtf_obj_Pub);     UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropObjType); AffectTo:rtf_af_None),
    (Keyword:'objicemb';   DefValue:ord(rtf_obj_ICEmb);   UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropObjType); AffectTo:rtf_af_None),
    (Keyword:'objhtml';    DefValue:ord(rtf_obj_HTML);    UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropObjType); AffectTo:rtf_af_None),
    (Keyword:'objocx';     DefValue:ord(rtf_obj_OCX);     UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropObjType); AffectTo:rtf_af_None),
    (Keyword:'objh';       DefValue:0;          UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropObjHeight);       AffectTo:rtf_af_None),
    (Keyword:'objw';       DefValue:0;          UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropObjWidth);        AffectTo:rtf_af_None),
    (Keyword:'objdata';    DefValue:0;          UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestObjData);         AffectTo:rtf_af_None),
    (Keyword:'result';     DefValue:0;          UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestObjResult);       AffectTo:rtf_af_None),
    (Keyword:'shppict';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestShpPict);   AffectTo:rtf_af_None),
    (Keyword:'nonshppict'; DefValue:0;          UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestNonShpPict); AffectTo:rtf_af_None),
    (Keyword:'b';        DefValue:1;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropBold);      AffectTo:rtf_af_CharProp),
    (Keyword:'ul';       DefValue:1;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropUnderline); AffectTo:rtf_af_CharProp),
    (Keyword:'ulnone';   DefValue:0;            UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropUnderline); AffectTo:rtf_af_CharProp),
    (Keyword:'i';        DefValue:1;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropItalic);    AffectTo:rtf_af_CharProp),
    (Keyword:'strike';   DefValue:1;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropStrike);    AffectTo:rtf_af_CharProp),
    (Keyword:'fs';       DefValue:24;           UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropFontSize);  AffectTo:rtf_af_CharProp),
    (Keyword:'lang';     DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropLanguage);  AffectTo:rtf_af_CharProp),
    (Keyword:'deflang';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropDefLanguage); AffectTo:rtf_af_None),
    (Keyword:'cf';       DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropTextColor); AffectTo:rtf_af_CharProp),
    (Keyword:'chcbpat';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropTextBackColor); AffectTo:rtf_af_CharProp),
    (Keyword:'cbpat';    DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropParaColor); AffectTo:rtf_af_ParaProp),
    (Keyword:'cfpat';    DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropParaFColor); AffectTo:rtf_af_ParaProp),
    (Keyword:'shading';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropParaShading); AffectTo:rtf_af_ParaProp),
    (Keyword:'li';       DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropLeftInd);   AffectTo:rtf_af_ParaProp),
    (Keyword:'ri';       DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropRightInd);  AffectTo:rtf_af_ParaProp),
    (Keyword:'fi';       DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropFirstInd);  AffectTo:rtf_af_ParaProp),
    (Keyword:'plain';    DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPlain);     AffectTo:rtf_af_CharProp),
    (Keyword:'u';        DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropU);         AffectTo:rtf_af_None),
    (Keyword:'uc';       DefValue:1;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropUC);        AffectTo:rtf_af_None),
    (Keyword:'cols';     DefValue:1;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropCols);      AffectTo:rtf_af_None),
    (Keyword:'sbknone';  DefValue:ord(rtf_sbk_None);   UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropSbk);  AffectTo:rtf_af_None),
    (Keyword:'sbkcol';   DefValue:ord(rtf_sbk_Column); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropSbk);  AffectTo:rtf_af_None),
    (Keyword:'sbkeven';  DefValue:ord(rtf_sbk_Even);   UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropSbk);  AffectTo:rtf_af_None),
    (Keyword:'sbkodd';   DefValue:ord(rtf_sbk_Odd);    UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropSbk);  AffectTo:rtf_af_None),
    (Keyword:'sbkpage';  DefValue:ord(rtf_sbk_Page);   UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropSbk);  AffectTo:rtf_af_None),
    (Keyword:'pgnx';     DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPgnX);      AffectTo:rtf_af_None),
    (Keyword:'pgny';     DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPgnY);      AffectTo:rtf_af_None),
    (Keyword:'pgndec';   DefValue:ord(rtf_pg_Decimal);     UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropPgnFormat); AffectTo:rtf_af_None),
    (Keyword:'pgnucrm';  DefValue:ord(rtf_pg_UpperRoman);  UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropPgnFormat); AffectTo:rtf_af_None),
    (Keyword:'pgnlcrm';  DefValue:ord(rtf_pg_LowerRoman);  UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropPgnFormat); AffectTo:rtf_af_None),
    (Keyword:'pgnucltr'; DefValue:ord(rtf_pg_UpperLetter); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropPgnFormat); AffectTo:rtf_af_None),
    (Keyword:'pgnlcltr'; DefValue:ord(rtf_pg_LowerLetter); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropPgnFormat); AffectTo:rtf_af_None),
    (Keyword:'qc';       DefValue:ord(rtf_al_Center);  UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropJust); AffectTo:rtf_af_ParaProp),
    (Keyword:'ql';       DefValue:ord(rtf_al_Left);    UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropJust); AffectTo:rtf_af_ParaProp),
    (Keyword:'qr';       DefValue:ord(rtf_al_Right);   UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropJust); AffectTo:rtf_af_ParaProp),
    (Keyword:'qj';       DefValue:ord(rtf_al_Justify); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropJust); AffectTo:rtf_af_ParaProp),
    (Keyword:'paperw';   DefValue:12240;        UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropXaPage);    AffectTo:rtf_af_None),
    (Keyword:'paperh';   DefValue:15480;        UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropYaPage);    AffectTo:rtf_af_None),
    (Keyword:'margl';    DefValue:1800;         UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropXaLeft);    AffectTo:rtf_af_None),
    (Keyword:'margr';    DefValue:1800;         UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropXaRight);   AffectTo:rtf_af_None),
    (Keyword:'margt';    DefValue:1440;         UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropYaTop);     AffectTo:rtf_af_None),
    (Keyword:'margb';    DefValue:1440;         UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropYaBottom);  AffectTo:rtf_af_None),
    (Keyword:'headery';  DefValue:720;          UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropHeaderY);   AffectTo:rtf_af_None),
    (Keyword:'footery';  DefValue:720;          UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropFooterY);   AffectTo:rtf_af_None),    
    (Keyword:'pgnstart'; DefValue:1;            UseDef:False;   kwd:rtf_kwd_Prop; idx:ord(ipropPgnStart);  AffectTo:rtf_af_None),
    (Keyword:'facingp';  DefValue:1;            UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropFacingp);   AffectTo:rtf_af_None),
    (Keyword:'landscape';DefValue:1;            UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropLandscape); AffectTo:rtf_af_None),
    (Keyword:'par';      DefValue:0;            UseDef:False;  kwd:rtf_kwd_Char; idx:$0a;                 AffectTo:rtf_af_None),
    (Keyword:'cell';     DefValue:0;            UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropCellEnd);   AffectTo:rtf_af_None),
    (Keyword:'row';      DefValue:0;            UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropRowEnd);    AffectTo:rtf_af_None),
    (Keyword:'nestcell'; DefValue:1;            UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropCellEnd);   AffectTo:rtf_af_None),
    (Keyword:'nestrow';  DefValue:1;            UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropRowEnd);    AffectTo:rtf_af_None),
    (Keyword:'nesttableprops'; DefValue:0;     UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestNestedTableProps); AffectTo:rtf_af_None),
    (Keyword:'intbl';    DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropInTbl);     AffectTo:rtf_af_None),
    (Keyword:'itap';     DefValue:1;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropItap);      AffectTo:rtf_af_None),
    (Keyword:'nonesttables'; DefValue:0;        UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'pard';     DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropParD);      AffectTo:rtf_af_ParaProp),
    (Keyword: #10;       DefValue:0;            UseDef:False;  kwd:rtf_kwd_Char; idx:$0A;                 AffectTo:rtf_af_None),
    (Keyword: #13;       DefValue:0;            UseDef:False;  kwd:rtf_kwd_Char; idx:$0A;                 AffectTo:rtf_af_None),
    (Keyword:'line';     DefValue:0;            UseDef:False;  kwd:rtf_kwd_Char; idx:$0D;                 AffectTo:rtf_af_None),
    (Keyword:'tab';      DefValue:0;            UseDef:False;  kwd:rtf_kwd_Char; idx:$09;                 AffectTo:rtf_af_None),
    (Keyword:'~';        DefValue:0;            UseDef:False;  kwd:rtf_kwd_Char; idx:$A0;                 AffectTo:rtf_af_None),
    (Keyword:'bullet';   DefValue:0;            UseDef:False;  kwd:rtf_kwd_Char; idx:$95;                 AffectTo:rtf_af_None),
    (Keyword:'ldblquote';DefValue:$201C;        UseDef:False;  kwd:rtf_kwd_WideChar; idx:ord('"');        AffectTo:rtf_af_None),
    (Keyword:'rdblquote';DefValue:$201D;        UseDef:False;  kwd:rtf_kwd_WideChar; idx:ord('"');        AffectTo:rtf_af_None),
    (Keyword:'lquote';   DefValue:$2018;        UseDef:False;  kwd:rtf_kwd_WideChar; idx:ord('''');       AffectTo:rtf_af_None),
    (Keyword:'rquote';   DefValue:$2019;        UseDef:False;  kwd:rtf_kwd_WideChar; idx:ord('''');       AffectTo:rtf_af_None),
    (Keyword:'endash';   DefValue:$2013;        UseDef:False;  kwd:rtf_kwd_WideChar; idx:ord('-');        AffectTo:rtf_af_None),
    (Keyword:'emdash';   DefValue:$2014;        UseDef:False;  kwd:rtf_kwd_WideChar; idx:ord('-');        AffectTo:rtf_af_None),
    (Keyword:'bin';      DefValue:0;            UseDef:False;  kwd:rtf_kwd_Spec; idx:ord(ipfnBin);        AffectTo:rtf_af_None),
    (Keyword:'*';        DefValue:0;            UseDef:False;  kwd:rtf_kwd_Spec; idx:ord(ipfnSkipDest);   AffectTo:rtf_af_None),
    (Keyword:'''';       DefValue:0;            UseDef:False;  kwd:rtf_kwd_Spec; idx:ord(ipfnHex);        AffectTo:rtf_af_None),
    (Keyword:'author';   DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'buptim';   DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'colortbl'; DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestColorTable);AffectTo:rtf_af_None),
    (Keyword:'comment';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'creatim';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'doccomm';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'fonttbl';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestFontTable); AffectTo:rtf_af_None),
    (Keyword:'footer';   DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestFooter);    AffectTo:rtf_af_None),
    (Keyword:'footerf';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'footerl';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'footerr';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    {$IFDEF RVTEXTFOOTNOTES}
    (Keyword:'footnote'; DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropFootNote);  AffectTo:rtf_af_CharProp),
    {$ELSE}
    (Keyword:'footnote'; DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    {$ENDIF}
    (Keyword:'ftncn';    DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'ftnsep';   DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'ftnsepc';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'header';   DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestHeader);    AffectTo:rtf_af_None),
    (Keyword:'headerf';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'headerl';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'headerr';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'info';     DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'keywords'; DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'operator'; DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'printim';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'private1'; DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'revtim';   DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'rxe';      DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'stylesheet';DefValue:0;           UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestStyleSheet);AffectTo:rtf_af_None),
    (Keyword:'subject';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'tc';       DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'title';    DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'txe';      DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'xe';       DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'pict';     DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestPict);      AffectTo:rtf_af_None),
    (Keyword:'{';        DefValue:0;            UseDef:False;  kwd:rtf_kwd_Char; idx:ord('{');            AffectTo:rtf_af_None),
    (Keyword:'}';        DefValue:0;            UseDef:False;  kwd:rtf_kwd_Char; idx:ord('}');            AffectTo:rtf_af_None),
    (Keyword:'\';        DefValue:0;            UseDef:False;  kwd:rtf_kwd_Char; idx:ord('\');            AffectTo:rtf_af_None),
    (Keyword:'sectd';    DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropSectd);     AffectTo:rtf_af_None),
    (Keyword:'charscalex';DefValue:100;         UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropCharScaleX);AffectTo:rtf_af_CharProp),
    (Keyword:'expndtw';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropCharSpacing);AffectTo:rtf_af_CharProp),
    (Keyword:'caps';     DefValue:1;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropAllCaps);   AffectTo:rtf_af_CharProp),
    (Keyword:'sub';        DefValue:ord(rtf_ss_Subscript);   UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropSScript); AffectTo:rtf_af_CharProp),
    (Keyword:'super';      DefValue:ord(rtf_ss_Superscript); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropSScript); AffectTo:rtf_af_CharProp),
    (Keyword:'nosupersub'; DefValue:ord(rtf_ss_Normal);      UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropSScript); AffectTo:rtf_af_CharProp),
    (Keyword:'f';        DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropF);         AffectTo:rtf_af_CharProp),
    (Keyword:'sb';       DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropSpaceBefore);AffectTo:rtf_af_ParaProp),
    (Keyword:'sa';       DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropSpaceAfter);AffectTo:rtf_af_ParaProp),
    (Keyword:'red';      DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropRed);       AffectTo:rtf_af_None),
    (Keyword:'green';    DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropGreen);     AffectTo:rtf_af_None),
    (Keyword:'blue';     DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropBlue);      AffectTo:rtf_af_None),
    (Keyword:'fnil';     DefValue:ord(rtf_ff_Default);    UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropFontFamily); AffectTo:rtf_af_None),
    (Keyword:'froman';   DefValue:ord(rtf_ff_Roman);      UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropFontFamily); AffectTo:rtf_af_None),
    (Keyword:'fswiss';   DefValue:ord(rtf_ff_Swiss);      UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropFontFamily); AffectTo:rtf_af_None),
    (Keyword:'fmodern';  DefValue:ord(rtf_ff_Modern);     UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropFontFamily); AffectTo:rtf_af_None),
    (Keyword:'fscript';  DefValue:ord(rtf_ff_Script);     UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropFontFamily); AffectTo:rtf_af_None),
    (Keyword:'fdecor';   DefValue:ord(rtf_ff_Decorative); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropFontFamily); AffectTo:rtf_af_None),
    (Keyword:'ftech';    DefValue:ord(rtf_ff_Symbol);     UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropFontFamily); AffectTo:rtf_af_None),
    (Keyword:'fbidi';    DefValue:ord(rtf_ff_BiDi);       UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropFontFamily); AffectTo:rtf_af_None),
    (Keyword:'fcharset'; DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropFCharset);  AffectTo:rtf_af_CharProp),
    (Keyword:'v';        DefValue:1;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropHidden);    AffectTo:rtf_af_CharProp),
    (Keyword:'s';        DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropS);         AffectTo:rtf_af_None),
    (Keyword:'cs';       DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropCS);        AffectTo:rtf_af_None),
    (Keyword:'ds';       DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropDS);        AffectTo:rtf_af_None),
    (Keyword:'sbasedon'; DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropSBasedOn);  AffectTo:rtf_af_None),
    (Keyword:'snext';    DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(irpropSNext);    AffectTo:rtf_af_None),
    (Keyword:'additive'; DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropAdditive);  AffectTo:rtf_af_None),
    (Keyword:'shidden';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropSHidden);   AffectTo:rtf_af_None),
    (Keyword:'deff';     DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropDefF);      AffectTo:rtf_af_None),
    (Keyword:'sl';       DefValue:240;          UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropSL);        AffectTo:rtf_af_ParaProp),
    (Keyword:'slmult';   DefValue:1;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropSLMult);    AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrs';         DefValue:ord(rtf_brdr_SingleThickness);    UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrth';        DefValue:ord(rtf_brdr_DoubleThickness);    UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrsh';        DefValue:ord(rtf_brdr_Shadow);             UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrdb';        DefValue:ord(rtf_brdr_Double);             UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrdot';       DefValue:ord(rtf_brdr_Dot);                UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrdash';      DefValue:ord(rtf_brdr_Dash);               UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrhair';      DefValue:ord(rtf_brdr_Hairline);           UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrdashsm';    DefValue:ord(rtf_brdr_DashSmall);          UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrdashd';     DefValue:ord(rtf_brdr_DotDash);            UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrdashdd';    DefValue:ord(rtf_brdr_DotDotDash);         UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrtriple';    DefValue:ord(rtf_brdr_Triple);             UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrtnthsg';    DefValue:ord(rtf_brdr_ThickThinSmall);     UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrthtnsg';    DefValue:ord(rtf_brdr_ThinThickSmall);     UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrtnthtnsg';  DefValue:ord(rtf_brdr_ThinThickThinSmall); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrtnthmg';    DefValue:ord(rtf_brdr_ThickThinMed);       UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrthtnmg';    DefValue:ord(rtf_brdr_ThinThickMed);       UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrtnthtnmg';  DefValue:ord(rtf_brdr_ThinThickThinMed);   UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrtnthlg';    DefValue:ord(rtf_brdr_ThickThinLarge);     UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrthtnlg';    DefValue:ord(rtf_brdr_ThinThickLarge);     UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrtnthtnlg';  DefValue:ord(rtf_brdr_ThinThickThinLarge); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrwavy';      DefValue:ord(rtf_brdr_Wavy);               UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrwavydb';    DefValue:ord(rtf_brdr_DoubleWavy);         UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrdashdotstr';DefValue:ord(rtf_brdr_Striped);            UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdremboss';    DefValue:ord(rtf_brdr_Emboss);             UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrengrave';   DefValue:ord(rtf_brdr_Emboss);             UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrinset';     DefValue:ord(rtf_brdr_Inset);              UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdroutset';    DefValue:ord(rtf_brdr_Outset);             UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderType); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrl';    DefValue:ord(rtf_side_Left);  UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderSide); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrt';    DefValue:ord(rtf_side_Top);   UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderSide); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrr';    DefValue:ord(rtf_side_Right); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderSide); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrb';    DefValue:ord(rtf_side_Bottom);UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderSide); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrw';    DefValue:1;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderWidth); AffectTo:rtf_af_ParaProp),
    (Keyword:'brdrcf';   DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderColor); AffectTo:rtf_af_ParaProp),
    (Keyword:'brsp';     DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropParaBorderSpace); AffectTo:rtf_af_ParaProp),
    (Keyword:'picw';     DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPicW);      AffectTo:rtf_af_None),
    (Keyword:'pich';     DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPicH);      AffectTo:rtf_af_None),
    (Keyword:'picwgoal'; DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPicWGoal);  AffectTo:rtf_af_None),
    (Keyword:'pichgoal'; DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPicHGoal);  AffectTo:rtf_af_None),
    (Keyword:'picscalex'; DefValue:100;         UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPicScaleX); AffectTo:rtf_af_None),
    (Keyword:'picscaley'; DefValue:100;         UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPicScaleY); AffectTo:rtf_af_None),
    (Keyword:'wbmbitspixel'; DefValue:1;        UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropWBMBitsPixel); AffectTo:rtf_af_None),
    (Keyword:'wbmplanes'; DefValue:1;           UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropWBMPlanes); AffectTo:rtf_af_None),
    (Keyword:'wbmwidthbytes'; DefValue:0;       UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropWBMWidthBytes); AffectTo:rtf_af_None),
    (Keyword:'emfblip';  DefValue:ord(rtf_pict_EMF);        UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropPictureType); AffectTo:rtf_af_None),
    (Keyword:'pngblip';  DefValue:ord(rtf_pict_PNG);        UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropPictureType); AffectTo:rtf_af_None),
    (Keyword:'jpegblip'; DefValue:ord(rtf_pict_JPEG);       UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropPictureType); AffectTo:rtf_af_None),
    (Keyword:'macpict';  DefValue:ord(rtf_pict_MacPict);    UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropPictureType); AffectTo:rtf_af_None),
    (Keyword:'pmmetafile'; DefValue:ord(rtf_pict_PmMetafile); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropPictureType); AffectTo:rtf_af_None),
      // ^- actually, we need to allow to specify parameter - metafile type here; but this picture type is not supported
    (Keyword:'dibitmap'; DefValue:ord(rtf_pict_DIB);      UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropPictureType); AffectTo:rtf_af_None),
    (Keyword:'wbitmap';  DefValue:ord(rtf_pict_DDB);      UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropPictureType); AffectTo:rtf_af_None),
    (Keyword:'wmetafile'; DefValue:1;                     UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropMetafile);    AffectTo:rtf_af_None),
    (Keyword:'picbmp';   DefValue:1;                      UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPicBmp);      AffectTo:rtf_af_None),
    (Keyword:'ansicpg';   DefValue:CP_ACP;                UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropAnsiCodePage);  AffectTo:rtf_af_None),
    (Keyword:'page';     DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPage);      AffectTo:rtf_af_None),
    (Keyword:'field';    DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestField);     AffectTo:rtf_af_None),
    (Keyword:'fldinst';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestFldInst);   AffectTo:rtf_af_None),
    (Keyword:'fldrslt';  DefValue:0;            UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestFldRslt);   AffectTo:rtf_af_None),
    (Keyword:'keep';     DefValue:1;            UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropKeepLinesTogether);  AffectTo:rtf_af_ParaProp),
    (Keyword:'keepn';    DefValue:1;            UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropKeepWithNext);  AffectTo:rtf_af_ParaProp),    
    (Keyword:'trowd';    DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropTRowD);     AffectTo:rtf_af_None),
    (Keyword:'trql';     DefValue:ord(rtf_al_Left);    UseDef:True;  kwd:rtf_kwd_Prop; idx:ord(ipropRowAlign);  AffectTo:rtf_af_None),
    (Keyword:'trqr';     DefValue:ord(rtf_al_Right);   UseDef:True;  kwd:rtf_kwd_Prop; idx:ord(ipropRowAlign);  AffectTo:rtf_af_None),
    (Keyword:'trqc';     DefValue:ord(rtf_al_Center);  UseDef:True;  kwd:rtf_kwd_Prop; idx:ord(ipropRowAlign);  AffectTo:rtf_af_None),
    (Keyword:'trhdr';    DefValue:1;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropTRHeader);  AffectTo:rtf_af_None),
    (Keyword:'trgaph';   DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropTRGapH);    AffectTo:rtf_af_None),
    (Keyword:'trleft';   DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropTRLeft);    AffectTo:rtf_af_None),
    (Keyword:'trrh';     DefValue:0;            UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropTRRowHeight); AffectTo:rtf_af_None),
    (Keyword:'trpaddl';  DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRPaddL); AffectTo:rtf_af_None),
    (Keyword:'trpaddr';  DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRPaddR); AffectTo:rtf_af_None),
    (Keyword:'trpaddt';  DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRPaddT); AffectTo:rtf_af_None),
    (Keyword:'trpaddb';  DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRPaddB); AffectTo:rtf_af_None),
    (Keyword:'trpaddfl'; DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRPaddFL); AffectTo:rtf_af_None),
    (Keyword:'trpaddfr'; DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRPaddFR); AffectTo:rtf_af_None),
    (Keyword:'trpaddft'; DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRPaddFT); AffectTo:rtf_af_None),
    (Keyword:'trpaddfb'; DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRPaddFB); AffectTo:rtf_af_None),
    (Keyword:'trspdl';   DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRSpdL);  AffectTo:rtf_af_None),
    (Keyword:'trspdr';   DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRSpdR);  AffectTo:rtf_af_None),
    (Keyword:'trspdt';   DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRSpdT);  AffectTo:rtf_af_None),
    (Keyword:'trspdb';   DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRSpdB);  AffectTo:rtf_af_None),
    (Keyword:'trspdfl';  DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRSpdFL); AffectTo:rtf_af_None),
    (Keyword:'trspdfr';  DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRSpdFR); AffectTo:rtf_af_None),
    (Keyword:'trspdft';  DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRSpdFT); AffectTo:rtf_af_None),
    (Keyword:'trspdfb';  DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRSpdFB); AffectTo:rtf_af_None),
    (Keyword:'trwWidth'; DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRwWidth); AffectTo:rtf_af_None),

    (Keyword:'trftsWidth'; DefValue:0;          UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropTRftsWidth); AffectTo:rtf_af_None),
    (Keyword:'clvmrg';   DefValue:ord(rtf_cm_Merged);  UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropCLVMerge); AffectTo:rtf_af_None),
    (Keyword:'clvmgf';   DefValue:ord(rtf_cm_First);   UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropCLVMerge); AffectTo:rtf_af_None),
    (Keyword:'clmrg';    DefValue:ord(rtf_cm_Merged);  UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropCLHMerge); AffectTo:rtf_af_None),
    (Keyword:'clmgf';    DefValue:ord(rtf_cm_First);   UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropCLHMerge); AffectTo:rtf_af_None),
    (Keyword:'clwWidth'; DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropCLwWidth); AffectTo:rtf_af_None),
    (Keyword:'clftsWidth'; DefValue:0;          UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropCLftsWidth); AffectTo:rtf_af_None),
    (Keyword:'clcbpat';  DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropCLColor);  AffectTo:rtf_af_None),
    (Keyword:'clcfpat';  DefValue:ord(clBlack); UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropCLFColor);  AffectTo:rtf_af_None),
    (Keyword:'clshdng';  DefValue:0;            UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropCLShading);  AffectTo:rtf_af_None),
    (Keyword:'clvertalt'; DefValue:ord(rtf_val_Top);   UseDef:True;  kwd:rtf_kwd_Prop; idx:ord(ipropCLVertAl);  AffectTo:rtf_af_None),
    (Keyword:'clvertalc'; DefValue:ord(rtf_val_Center); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropCLVertAl);  AffectTo:rtf_af_None),
    (Keyword:'clvertalb'; DefValue:ord(rtf_val_Bottom); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropCLVertAl);  AffectTo:rtf_af_None),
    (Keyword:'cellx';     DefValue:0;           UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropCellX);  AffectTo:rtf_af_None),
    (Keyword:'trbrdrl';    DefValue:ord(rtf_side_Left);  UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropTRBorderSide); AffectTo:rtf_af_ParaProp),
    (Keyword:'trbrdrt';    DefValue:ord(rtf_side_Top);   UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropTRBorderSide); AffectTo:rtf_af_ParaProp),
    (Keyword:'trbrdrr';    DefValue:ord(rtf_side_Right); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropTRBorderSide); AffectTo:rtf_af_ParaProp),
    (Keyword:'trbrdrb';    DefValue:ord(rtf_side_Bottom);UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropTRBorderSide); AffectTo:rtf_af_ParaProp),
    (Keyword:'clbrdrl';    DefValue:ord(rtf_side_Left);  UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropCLBorderSide); AffectTo:rtf_af_ParaProp),
    (Keyword:'clbrdrt';    DefValue:ord(rtf_side_Top);   UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropCLBorderSide); AffectTo:rtf_af_ParaProp),
    (Keyword:'clbrdrr';    DefValue:ord(rtf_side_Right); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropCLBorderSide); AffectTo:rtf_af_ParaProp),
    (Keyword:'clbrdrb';    DefValue:ord(rtf_side_Bottom);UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropCLBorderSide); AffectTo:rtf_af_ParaProp),
    (Keyword:'absnoovrlp'; DefValue:0; UseDef:False;                  kwd: rtf_kwd_Prop;idx:ord(ipropNoTableEv); AffectTo: rtf_af_ParaProp),
    //(Keyword:'pvpara';     DefValue:0; UseDef:False;                  kwd: rtf_kwd_Prop;idx:ord(ipropNoTableEv); AffectTo: rtf_af_ParaProp),
    //(Keyword:'pvmrg';      DefValue:0; UseDef:False;                  kwd: rtf_kwd_Prop;idx:ord(ipropNoTableEv); AffectTo: rtf_af_ParaProp),
    //(Keyword:'pvpg';       DefValue:0; UseDef:False;                  kwd: rtf_kwd_Prop;idx:ord(ipropNoTableEv); AffectTo: rtf_af_ParaProp),
    (Keyword:'highlight';  DefValue:0; UseDef:False;                  kwd: rtf_kwd_Prop;idx:ord(ipropHighlight); AffectTo: rtf_af_CharProp),
    // bullets & numbering, old style
    (Keyword:'pnseclvl';   DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNSecLvl);  AffectTo:rtf_af_None),
    (Keyword:'pntext';     DefValue:0; UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestSkip);      AffectTo:rtf_af_None),
    (Keyword:'pn';         DefValue:0; UseDef:False;  kwd:rtf_kwd_Dest; idx:ord(idestPN);        AffectTo:rtf_af_None),
    (Keyword:'pnlvl';      DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNLevel);   AffectTo:rtf_af_None),
    (Keyword:'pnlvlblt';   DefValue:11; UseDef:True;  kwd:rtf_kwd_Prop; idx:ord(ipropPNLevel);   AffectTo:rtf_af_None),
    (Keyword:'pnlvlbody';  DefValue:10; UseDef:True;  kwd:rtf_kwd_Prop; idx:ord(ipropPNLevel);   AffectTo:rtf_af_None),
    (Keyword:'pnhang';     DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNHanging);  AffectTo:rtf_af_None),
    (Keyword:'pndec';      DefValue:ord(rtf_pn_Decimal); UseDef:True;  kwd:rtf_kwd_Prop; idx:ord(ipropPNType);  AffectTo:rtf_af_None),
    (Keyword:'pnucltr';    DefValue:ord(rtf_pn_UpperLetter); UseDef:True;  kwd:rtf_kwd_Prop; idx:ord(ipropPNType);  AffectTo:rtf_af_None),
    (Keyword:'pnucrm';     DefValue:ord(rtf_pn_UpperRoman); UseDef:True;  kwd:rtf_kwd_Prop; idx:ord(ipropPNType);  AffectTo:rtf_af_None),
    (Keyword:'pnlcltr';    DefValue:ord(rtf_pn_LowerLetter); UseDef:True;  kwd:rtf_kwd_Prop; idx:ord(ipropPNType);  AffectTo:rtf_af_None),
    (Keyword:'pnlcrm';     DefValue:ord(rtf_pn_LowerRoman); UseDef:True;  kwd:rtf_kwd_Prop; idx:ord(ipropPNType);  AffectTo:rtf_af_None),
    (Keyword:'pnuldash';   DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNUnderline);  AffectTo:rtf_af_None),
    (Keyword:'pnuldashd';   DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNUnderline);  AffectTo:rtf_af_None),
    (Keyword:'pnuldashdd';   DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNUnderline);  AffectTo:rtf_af_None),
    (Keyword:'pnulhair';   DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNUnderline);  AffectTo:rtf_af_None),
    (Keyword:'pnulth';     DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNUnderline);  AffectTo:rtf_af_None),
    (Keyword:'pnulwave';   DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNUnderline);  AffectTo:rtf_af_None),
    (Keyword:'pnul';       DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNUnderline);  AffectTo:rtf_af_None),
    (Keyword:'pnuld';      DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNUnderline);  AffectTo:rtf_af_None),
    (Keyword:'pnuld';      DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNUnderline);  AffectTo:rtf_af_None),
    (Keyword:'pnuldb';     DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNUnderline);  AffectTo:rtf_af_None),
    (Keyword:'pnulw';      DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNUnderline);  AffectTo:rtf_af_None),
    (Keyword:'pnulnone';   DefValue:0; UseDef:True;   kwd:rtf_kwd_Prop; idx:ord(ipropPNUnderline);  AffectTo:rtf_af_None),
    (Keyword:'pnb';        DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNBold);       AffectTo:rtf_af_None),
    (Keyword:'pni';        DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNItalic);     AffectTo:rtf_af_None),
    (Keyword:'pnstrike';   DefValue:1; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNStrike);     AffectTo:rtf_af_None),
    (Keyword:'pncf';       DefValue:0; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNColor);      AffectTo:rtf_af_None),
    (Keyword:'pnf';        DefValue:0; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNF);          AffectTo:rtf_af_None),
    (Keyword:'pnfs';       DefValue:0; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNFontSize);   AffectTo:rtf_af_None),
    (Keyword:'pnindent';   DefValue:0; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNIndent);     AffectTo:rtf_af_None),
    (Keyword:'pnsp';       DefValue:0; UseDef:False;  kwd:rtf_kwd_Prop; idx:ord(ipropPNSp);         AffectTo:rtf_af_None),
    (Keyword:'pnqc';       DefValue:ord(rtf_al_Center); UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropPNAlign); AffectTo:rtf_af_None),
    (Keyword:'pnql';       DefValue:ord(rtf_al_Left);   UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropPNAlign); AffectTo:rtf_af_None),
    (Keyword:'pnqr';       DefValue:ord(rtf_al_Right);  UseDef:True; kwd:rtf_kwd_Prop; idx:ord(ipropPNAlign); AffectTo:rtf_af_None),
    (Keyword:'pnstart';    DefValue:1; UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropPNStart); AffectTo:rtf_af_None),
    (Keyword:'pntxta';     DefValue:0; UseDef:False; kwd:rtf_kwd_Dest; idx:ord(idestPNTextAfter);  AffectTo:rtf_af_None),
    (Keyword:'pntxtb';     DefValue:0; UseDef:False; kwd:rtf_kwd_Dest; idx:ord(idestPNTextBefore); AffectTo:rtf_af_None),
    // bullets & numbering, new style
    (Keyword:'listtext';         DefValue:0; UseDef:False; kwd: rtf_kwd_Dest;idx:ord(idestSkip);           AffectTo: rtf_af_None),
    (Keyword:'listtable';        DefValue:0; UseDef:False; kwd: rtf_kwd_Dest;idx:ord(idestListTable);      AffectTo: rtf_af_None),
    (Keyword:'list';             DefValue:0; UseDef:False; kwd: rtf_kwd_Dest;idx:ord(idestList);           AffectTo: rtf_af_None),
    (Keyword:'listid';           DefValue:0; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropListId);         AffectTo: rtf_af_None),
    (Keyword:'listsimple';       DefValue:1; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropListSimple);     AffectTo: rtf_af_None),
    (Keyword:'listname';         DefValue:1; UseDef:False; kwd: rtf_kwd_Dest;idx:ord(idestListName);       AffectTo: rtf_af_None),
    (Keyword:'listlevel';        DefValue:0; UseDef:False; kwd: rtf_kwd_Dest;idx:ord(idestListLevel);      AffectTo: rtf_af_None),
    (Keyword:'levelstartat';     DefValue:1; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropLevelStartAt);   AffectTo: rtf_af_None),
    (Keyword:'levelnfc';         DefValue:0; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropLevelNumberType); AffectTo: rtf_af_None),
    (Keyword:'leveljc';          DefValue:0; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropLevelJustify);   AffectTo: rtf_af_None),
    (Keyword:'levelold';         DefValue:1; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropLevelOld);       AffectTo: rtf_af_None),
    (Keyword:'levelindent';      DefValue:0; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropLevelIndent);    AffectTo: rtf_af_None),
    (Keyword:'levelspace';       DefValue:0; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropLevelSpace);     AffectTo: rtf_af_None),
    (Keyword:'leveltext';        DefValue:0; UseDef:False; kwd: rtf_kwd_Dest;idx:ord(idestLevelText);      AffectTo: rtf_af_None),
    (Keyword:'levelnumbers';     DefValue:0; UseDef:False; kwd: rtf_kwd_Dest;idx:ord(idestLevelNumbers);   AffectTo: rtf_af_None),
    (Keyword:'levelfollow';      DefValue:0; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropLevelFollow);    AffectTo: rtf_af_None),
    (Keyword:'levellegal';       DefValue:1; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropLevelLegal);     AffectTo: rtf_af_None),
    (Keyword:'levelnorestart';   DefValue:1; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropLevelNoRestart); AffectTo: rtf_af_None),
    (Keyword:'listoverridetable';DefValue:0; UseDef:False; kwd: rtf_kwd_Dest;idx:ord(idestLOTable);        AffectTo: rtf_af_None),
    (Keyword:'listoverride';     DefValue:0; UseDef:False; kwd: rtf_kwd_Dest;idx:ord(idestLO);             AffectTo: rtf_af_None),
    (Keyword:'listoverridecount';DefValue:0; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropLOCount);        AffectTo: rtf_af_None),
    (Keyword:'lfolevel';         DefValue:0; UseDef:False; kwd: rtf_kwd_Dest;idx:ord(idestLOLevel);        AffectTo: rtf_af_None),
    (Keyword:'listoverridestartat';DefValue:1; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropLOStart);      AffectTo: rtf_af_None),
    (Keyword:'ls';               DefValue:1; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropLONumber);       AffectTo: rtf_af_ParaProp),
    (Keyword:'ilvl';             DefValue:1; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropLevel);          AffectTo: rtf_af_ParaProp),
    (Keyword:'tx';               DefValue:0; UseDef:False; kwd: rtf_kwd_Prop;idx:ord(ipropTX);             AffectTo: rtf_af_None)
    {$IFDEF RICHVIEW}
   ,(Keyword:'richviewcbw'; DefValue:0; UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropRVCellBestWidth); AffectTo:rtf_af_None),
    (Keyword:'richviewcbh'; DefValue:0; UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropRVCellBestHeight); AffectTo:rtf_af_None),
    (Keyword:'richviewtbw'; DefValue:0; UseDef:False; kwd:rtf_kwd_Prop; idx:ord(ipropRVTableBestWidth); AffectTo:rtf_af_None)
    {$ENDIF}
    );

procedure SortKeywords; forward;

function GetMetafileMapMode(Val: Integer): Integer;
begin
  case val of
    2: Result := MM_LOMETRIC;
    3: Result := MM_HIMETRIC;
    4: Result := MM_LOENGLISH;
    5: Result := MM_HIENGLISH;
    6: Result := MM_TWIPS;
    7: Result := MM_ISOTROPIC;
    8: Result := MM_ANISOTROPIC;
    else
      Result := MM_TEXT
  end;
end;

function GetHighlightColor(Index: Integer): TColor;
begin
  case Index of
    1:  Result := clBlack;
    2:  Result := clBlue;
    3:  Result := clAqua;
    4:  Result := clLime;
    5:  Result := clFuchsia;
    6:  Result := clRed;
    7:  Result := clYellow;
    8:  Result := clWhite; // not used
    9:  Result := clNavy;
    10: Result := clTeal;
    11:	Result := clGreen;
    12:	Result := clPurple;
    13:	Result := clMaroon;
    14:	Result := clOlive;
    15:	Result := clGray;
    16:	Result := clSilver;
    else Result := clNone;
  end;
end;

function ShadeColor(Color1, Color2: TColor; Shading: Integer): TColor;
    {.................................................}
    function GetColorChannel(c1,c2: Integer): Integer;
    begin
      Result := ((c1*(10000-Shading)+c2*Shading) div 10000) and $FF;
    end;
    {.................................................}
var Red, Green, Blue: Integer;
begin
  Red   := GetColorChannel( Color1 and $0000FF,          Color2 and $0000FF);
  Green := GetColorChannel((Color1 and $00FF00) shr 8,  (Color2 and $00FF00) shr 8);
  Blue  := GetColorChannel((Color1 and $FF0000) shr 16, (Color2 and $FF0000) shr 16);
  Result := Red or (Green shl 8) or (Blue shl 16);
end;

{======================== TRVRTFReader ========================================}
constructor TRVRTFReader.Create(AOwner: TComponent);
begin
  inherited  Create(AOwner);
  FFontTable  := TRVRTFFontList.Create;
  FStyleSheet := TRVRTFStyleSheet.Create;
  FColorTable := TRVRTFColorList.Create;
  FListTable  := TRVRTFListTable97.Create;
  FListOverrideTable  := TRVRTFListOverrideTable97.Create;
  SaveList    := TRVList.Create;
  FRTFState   := TRVRTFReaderState.Create;
  FDefCodePage := CP_ACP;
  FConvertHighlight := rtf_hl_FixedColors;
  FExtractMetafileBitmaps := True;
end;
{------------------------------------------------------------------------------}
destructor TRVRTFReader.Destroy;
begin
  Clear;
  FFontTable.Free;
  FStyleSheet.Free;
  FColorTable.Free;
  SaveList.Free;
  FRTFState.Free;
  FListTable.Free;
  FListOverrideTable.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReader.Clear;
begin
  SortKeywords;
  FStyleSheet.Clear;
  FFontTable.Clear;
  FColorTable.Clear;
  FListTable.Clear;
  FListOverrideTable.Clear;
  SaveList.Clear;
  FRTFState.Reset;
  Text       := '';
  {$IFDEF RICHVIEWCBDEF3}
  TextW      := '';
  {$ENDIF}
  Position     := rtf_ts_NewPara;
  UseLastChar := False;
  FPicture.Free;
  FPicture := nil;
  FObject.Free;
  FObject := nil;
  FMarkerProps.Free;
  FMarkerProps := nil;
  FCodePage := FDefCodePage;
  SkipAnsiCount := 1;
  SkipNext      := 0;
  cGroup        := 0;
  CurrentNestingLevel := 0;
  ForceEvenEmptyNewLine := False;
  ShpPictInserted := False;
  ObjectInserted  := False;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.ApplyPropChange_SSEntry(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
begin
  Result := rtf_ec_Ok;
  case iprop of
    ipropS:
      Result := ChangeDest(idestStyleSheetParaStyle, Val);
    ipropCS:
      Result := ChangeDest(idestStyleSheetCharStyle, Val);
    ipropDS:
      Result := ChangeDest(idestStyleSheetSectStyle, Val);
    ipropSBasedOn:
      FStyleSheet[FStyleSheet.Count-1].FBasedOn := FStyleSheet.GetEntry(Val);
    irpropSNext:
      FStyleSheet[FStyleSheet.Count-1].FNext := FStyleSheet.GetEntry(Val);
    ipropAdditive:
      FStyleSheet[FStyleSheet.Count-1].FAdditive := True;
    ipropSHidden:
      FStyleSheet[FStyleSheet.Count-1].FHidden := True;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.ApplyPropChange_Picture(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
begin
  Result := rtf_ec_Ok;
  if FPicture<>nil then
    case iprop of
      ipropPictureType:
        begin
          FPicture.FType := TRVRTFPictureType(Val);
          case FPicture.FType of
            rtf_pict_DIB:
              begin
                FPicture.FData.Position := sizeof(TBitmapFileHeader);
              end;
          end;
        end;
      ipropMetafile:
        begin
          FPicture.FType := rtf_pict_WMF;
          FPicture.FMetafileMapMode := GetMetafileMapMode(Val);
        end;
      ipropPicW:
        FPicture.FPicW := Val;
      ipropPicH:
        FPicture.FPicH := Val;
      ipropPicWGoal:
        FPicture.FPicWGoalTw := Val;
      ipropPicHGoal:
        FPicture.FPicHGoalTw := Val;
      ipropPicScaleX:
        FPicture.FPicScaleX := Val;
      ipropPicScaleY:
        FPicture.FPicScaleY := Val;
      ipropWBMWidthBytes:
        FPicture.FWBMWidthBytes := Val;
      ipropWBMBitsPixel:
        FPicture.FWBMBitsPixel := Val;
      ipropWBMPlanes:
        FPicture.FWBMPlanes := Val;
      ipropPicBmp:
        FPicture.FMetafileWithBitmap := True;
    end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.ApplyPropChange_Object(iprop: TRTFiprop;
  val: Integer): TRVRTFErrorCode;
begin
  Result := rtf_ec_Ok;
  if FObject<>nil then
    case iprop of
      ipropObjType:
        FObject.FType := TRVRTFObjectType(val);
      ipropObjWidth:
        FObject.FWidthTw := val;
      ipropObjHeight:
        FObject.FHeightTw := val;
    end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.ApplyPropChange_PN(iprop: TRTFiprop;
  val: Integer): TRVRTFErrorCode;
var
  CurMarkerProps: TRVRTFMarkerProperties;
begin
  Result := rtf_ec_Ok;
  if FRTFState.rds=rdsPN then
    CurMarkerProps := FMarkerProps
  else begin
    FRTFState.SectProps.InitListDefaults;
    CurMarkerProps := TRVRTFMarkerProperties(FRTFState.SectProps.FDefMarkerPropsList[FCurPNSecLvl-1]);
  end;
  if CurMarkerProps<>nil then
    case iprop of
      ipropPNLevel:
        begin
          if RTFState.SectProps.FDefMarkerPropsList<>nil then
            CurMarkerProps.Assign(TRVRTFMarkerProperties(FRTFState.SectProps.FDefMarkerPropsList[val-1]), True);
          CurMarkerProps.FLevel := val;
        end;
      ipropPNHanging:
        CurMarkerProps.FHanging := True;
      ipropPNType:
        CurMarkerProps.FListType := TRVRTFParaListType(val);
      ipropPNUnderline:
        CurMarkerProps.ChangeFontStyle(fsUnderline,val);
      ipropPNBold:
        CurMarkerProps.ChangeFontStyle(fsBold,val);
      ipropPNItalic:
        CurMarkerProps.ChangeFontStyle(fsItalic,val);
      ipropPNStrike:
        CurMarkerProps.ChangeFontStyle(fsStrikeOut,val);
      ipropPNColor:
        begin
          if (Val>=0) and (Val<ColorTable.Count) then
             CurMarkerProps.FColor := ColorTable[Val];
          Include(CurMarkerProps.FFixedProperties, rtfmp_Color);
        end;
      ipropPNF:
        begin
          CurMarkerProps.FFontIndex := FFontTable.GetFontIndex(val,RTFState.DefFontNumber);
          Include(CurMarkerProps.FFixedProperties, rtfmp_FontIndex);
        end;
      ipropPNFontSize:
        begin
          CurMarkerProps.FFontSize := Round(val/2);
          Include(CurMarkerProps.FFixedProperties, rtfmp_Size);
        end;
      ipropPNIndent:
        CurMarkerProps.FIndentTw := val;
      ipropPNSp:
        CurMarkerProps.FSpaceTw := val;
      ipropPNAlign:
        CurMarkerProps.FAlignment := TRVRTFAlignment(val);
      ipropPNStart:
        CurMarkerProps.FStart := val;
    end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.ApplyPropChange_List(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
begin
  Result := rtf_ec_Ok;
  case iprop of
    ipropListId:
      FListTable.GetLastList.FId := Val;
    ipropListTemplateId:
      FListTable.GetLastList.FTemplateId := Val;
    ipropListSimple:
      FListTable.GetLastList.FSimple := Val<>0;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.ApplyPropChange_ListLevel(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
var Level: TRVRTFListLevel97;
begin
  Result := rtf_ec_Ok;
  Level := FListTable.GetLastList.GetLastLevel;
  case iprop of
    ipropLevelStartAt:
      Level.FStart := Val;
    ipropLevelNumberType:
      case Val of
        1:   Level.FListType := rtf_pn_UpperRoman;
        2:   Level.FListType := rtf_pn_LowerRoman;
        3:   Level.FListType := rtf_pn_UpperLetter;
        4:   Level.FListType := rtf_pn_LowerLetter;
        23:  Level.FListType := rtf_pn_Bullet;
        else Level.FListType := rtf_pn_Decimal;
      end;
    ipropLevelJustify:
      case Val of
        0: Level.FAlignment := rtf_al_Left;
        1: Level.FAlignment := rtf_al_Center;
        2: Level.FAlignment := rtf_al_Right;
      end;
    ipropLevelOld:
      Level.FOldStyle := Val<>0;
    ipropLevelIndent:
      Level.FIndentTw := Val;
    ipropLevelSpace:
      Level.FSpaceTw  := Val;
    ipropLevelFollow:
      Level.FFollow := TRVRTFLevelFollow97(Val);
    ipropLevelLegal:
      Level.FLegal := Val<>0;
    ipropLevelNoRestart:
      Level.FNoRestart := Val<>0;
    ipropBold:
      Level.ChangeFontStyle(fsBold, Val);
    ipropItalic:
      Level.ChangeFontStyle(fsItalic, Val);
    ipropUnderline:
      Level.ChangeFontStyle(fsUnderline, Val);
    ipropStrike:
      Level.ChangeFontStyle(fsStrikeOut, Val);
    ipropFontSize:
      begin
        Level.FFontSize := round(Val/2);
        Level.FFontSizeDefined := True;
      end;
    ipropTextColor:
      if (Val>=0) and (Val<ColorTable.Count) then
        Level.FColor := ColorTable[Val];
    ipropF:
      Level.FFontIndex := FFontTable.GetFontIndex(val,RTFState.DefFontNumber);
    ipropLeftInd:
      Level.FLeftIndentTw := val;
    ipropFirstInd:
      Level.FFirstIndentTw := val;
    ipropTX: // actually, we should check tab type here
      Level.FTabPosTw := val;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.ApplyPropChange_LO(iprop: TRTFiprop;
  val: Integer): TRVRTFErrorCode;
var LO: TRVRTFListOverride97;
begin
  Result := rtf_ec_Ok;
  LO := FListOverrideTable.GetLastListOverride;
  case iprop of
    ipropListId:
      LO.FListIndex := FListTable.FindList(val);
    ipropLONumber:
      LO.FNumber := Val;
    ipropLOCount:
      LO.FOverrideCount := Val;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.ApplyPropChange_LOLevel(iprop: TRTFiprop;
  val: Integer): TRVRTFErrorCode;
var LOLevel: TRVRTFListOverrideLevel97;
begin
  Result := rtf_ec_Ok;
  LOLevel := FListOverrideTable.GetLastListOverride.GetLastLevel;
  case iprop of
    ipropLOStart:
      LOLevel.FUseStart := True;
    ipropLevelStartAt:
      LOLevel.FStart := val;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.ApplyPropChange(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
{$IFDEF RICHVIEWCBDEF3}
var s: String;
{$ENDIF}
begin
  Result := rtf_ec_Ok;
  case FRTFState.rds of
    rdsSkip:
      exit;
    rdsStyleSheetEntry:
      Result := ApplyPropChange_SSEntry(iprop, val);    
    rdsPict:
      Result := ApplyPropChange_Picture(iprop, val);
    rdsObject:
      Result := ApplyPropChange_Object(iprop, val);
    rdsPN, rdsPNSecLvl:
      Result := ApplyPropChange_PN(iprop, val);
    rdsList:
      Result := ApplyPropChange_List(iprop, val);
    rdsListLevel:
      Result := ApplyPropChange_ListLevel(iprop, val);
    rdsLO:
      Result := ApplyPropChange_LO(iprop, val);
    rdsLOLevel:
      Result := ApplyPropChange_LOLevel(iprop, val);
    rdsColorTable:
      begin
        case iprop of
          ipropRed:
            FColorTable.SetLastRed(Val);
          ipropGreen:
            FColorTable.SetLastGreen(Val);
          ipropBlue:
            FColorTable.SetLastBlue(Val);
        end;
      end;
    else
      case iprop of
        ipropU:
          begin
            {$IFDEF RICHVIEWCBDEF3}
             case RTFState.rds of
               rdsNorm:
                 if Assigned(FOnNewUnicodeText) then begin
                   SkipNext := SkipAnsiCount;
                   OutputWideChar(WideChar(PWord(@Val)^));
                 end;
               rdsListLevelText:
                 begin
                   SkipNext := SkipAnsiCount;
                   OutputWideChar(WideChar(PWord(@Val)^));
                 end;
               rdsPNTextAfter, rdsPNTextBefore:
                 begin
                   if (FMarkerProps<>nil) and (FMarkerProps.FFontIndex>=0) and
                     (FMarkerProps.FFontIndex<FontTable.Count) and
                     (UpperCase(FontTable[FMarkerProps.FFontIndex].Name)='SYMBOL') and
                     (((PWord(@Val)^) and $FF00) = $F000) then begin
                     SkipNext := SkipAnsiCount;
                     ParseChar(Char(Val and $FF));
                     end
                   else if SkipAnsiCount>0 then begin
                     SkipNext := SkipAnsiCount;
                     s := UnicodeToAnsi(WideChar(PWord(@Val)^));
                     while s<>'' do begin
                       Result := ParseChar(s[1]);
                       s := Copy(s, 2, Length(s));
                     end;
                   end;
                 end;
               else
                 begin
                   if SkipAnsiCount>0 then begin
                     SkipNext := SkipAnsiCount;
                     s := UnicodeToAnsi(WideChar(PWord(@Val)^));
                     while s<>'' do begin
                       Result := ParseChar(s[1]);
                       s := Copy(s, 2, Length(s));
                     end;
                   end;
                 end;
             end;
            {$ENDIF}
          end;
        ipropUC:
          SkipAnsiCount := Val;
        {$IFDEF RVTEXTFOOTNOTES}
        iPropFootnote:
          begin
             FRTFState.ChangeFontStyleEx(rtf_fs_Footnote, 1);
             OutputChar('<', False, False);
             OutputChar('F', False, False);
             OutputChar('N', False, False);
             OutputChar('>', False, False);
          end;        
        {$ENDIF}          
        ipropBold:
          FRTFState.ChangeFontStyle(fsBold, Val);
        ipropItalic:
          FRTFState.ChangeFontStyle(fsItalic, Val);
        ipropUnderline:
          FRTFState.ChangeFontStyle(fsUnderline, Val);
        ipropStrike:
          FRTFState.ChangeFontStyle(fsStrikeOut, Val);
        ipropAllCaps:
          FRTFState.ChangeFontStyleEx(rtf_fs_AllCaps, Val);
        ipropLanguage:
          FRTFState.CharProps.FLanguage := Val;
        ipropDefLanguage:
          FRTFState.FDefLanguage := Val;
        ipropHidden:
          begin
            if FRTFState.CharProps.FHidden<>(Val<>0) then begin
              ForceEvenEmptyNewLine := not FRTFState.CharProps.FHidden;
              OutputChar(#0, False, False);
              ForceEvenEmptyNewLine := False;
            end;
           FRTFState.CharProps.FHidden := Val<>0;
          end;
        ipropSScript:
          FRTFState.CharProps.FSScriptType := TRVRTFSScriptType(Val);
        ipropFontSize:
          FRTFState.CharProps.FSize := round(Val/2);
        ipropTextColor:
          begin
            if (Val>=0) and (Val<ColorTable.Count) then
              FRTFState.CharProps.FColor := ColorTable[Val];
          end;
        ipropTextBackColor:
          begin
            if (Val>=0) and (Val<ColorTable.Count) then
              FRTFState.CharProps.FBackColor := ColorTable[Val];
            if FRTFState.CharProps.FBackColor=clWindowText then
              FRTFState.CharProps.FBackColor := clNone;
          end;
        ipropHighlight:
          case FConvertHighlight of
           rtf_hl_FixedColors:
             FRTFState.CharProps.FBackColor := GetHighlightColor(Val);
           rtf_hl_ColorTable:
             begin
               if (Val>=0) and (Val<ColorTable.Count) then
                 FRTFState.CharProps.FBackColor := ColorTable[Val];
               if FRTFState.CharProps.FBackColor=clWindowText then
                 FRTFState.CharProps.FBackColor := clNone;
             end;
          end;
        ipropSL:
          FRTFState.ParaProps.FLineSpacing := val;
        ipropSLMult:
          FRTFState.ParaProps.FLineSpacingMulti := Boolean(val);        
        ipropLeftInd:
          FRTFState.ParaProps.FLeftIndentTw := val;
        ipropRightInd:
          FRTFState.ParaProps.FRightIndentTw := val;
        ipropFirstInd:
          FRTFState.ParaProps.FFirstIndentTw := val;
        ipropSpaceBefore:
          FRTFState.ParaProps.FSpaceBeforeTw := val;
        ipropSpaceAfter:
          FRTFState.ParaProps.FSpaceAfterTw := val;
        ipropCols:
          FRTFState.SectProps.FColumnCount := val;
        ipropPgnX:
          FRTFState.SectProps.FPageNumberXTw := val;
        ipropPgnY:
          FRTFState.SectProps.FPageNumberYTw :=  val;
        ipropHeaderY:
          FRTFState.SectProps.FHeaderYTw :=  val;
        ipropFooterY:
          FRTFState.SectProps.FFooterYTw :=  val;
        ipropXaPage:
          FRTFState.DocProps.FPaperWidthTw := val;
        ipropYaPage:
          FRTFState.DocProps.FPaperHeightTw := val;
        ipropXaLeft:
          FRTFState.DocProps.FLeftMarginTw := val;
        ipropXaRight:
          FRTFState.DocProps.FRightMarginTw := val;
        ipropYaTop:
          FRTFState.DocProps.FTopMarginTw := val;
        ipropYaBottom:
          FRTFState.DocProps.FBottomMarginTw := val;
        ipropPgnStart:
          FRTFState.DocProps.FPageNumberStart := val;
        ipropSbk:
          FRTFState.SectProps.FSectionBreakType := TRVRTFSectionBreakType(val);
        ipropPgnFormat:
          FRTFState.SectProps.FPageNumberFormat := TRVRTFPageNumberFormat(val);
        ipropFacingp:
          FRTFState.DocProps.FFacingPages := Boolean(val);
        ipropLandscape:
          FRTFState.DocProps.FLandscape := Boolean(val);
        ipropJust:
          FRTFState.ParaProps.FAlignment := TRVRTFAlignment(val);
        ipropLONumber:
          FRTFState.ParaProps.FListOverrideIndex := FListOverrideTable.FindListOverride(val);
        ipropLevel:
          FRTFState.ParaProps.FListLevel := val;        
        ipropDefF:
          if FRTFState.rds=rdsNorm then begin
            FRTFState.DefFontNumber := Val;
          end;
        ipropF:
          case FRTFState.rds of
            rdsFontTable:
              FFontTable.Add(val);
            rdsStyleSheet:
              FStyleSheet[FStyleSheet.Count-1].CharProps.FFontIndex  := FFontTable.GetFontIndex(val,RTFState.DefFontNumber);
            rdsNorm:
              FRTFState.CharProps.FFontIndex := FFontTable.GetFontIndex(val,RTFState.DefFontNumber);
            else
              ;
            end;
        ipropCharScaleX:
          FRTFState.CharProps.FCharScaleX := val;
        ipropCharSpacing:
          FRTFState.CharProps.FCharSpacingTw := val;
        ipropPard,ipropPlain,ipropSectd,ipropTRowD:
          Result := ParseSpecialProperty(iprop, val);
        ipropKeepLinesTogether:
          FRTFState.ParaProps.FKeepLinesTogether := val<>0;
        ipropKeepWithNext:
          FRTFState.ParaProps.FKeepWithNext := val<>0;
        ipropParaBorderSide:
          begin
            FRTFState.FCurrentBorderType := rtf_bt_Para;
            FRTFState.ParaProps.FCurBorderSide := TRVRTFSide(Val);
          end;
        ipropTRBorderSide:
          begin
            FRTFState.FCurrentBorderType := rtf_bt_Row;
            FRTFState.RowProps.FCurBorderSide := TRVRTFSide(Val);
          end;
        ipropCLBorderSide:
          begin
            FRTFState.FCurrentBorderType := rtf_bt_Cell;
            FRTFState.RowProps.GetLastCellProp.FCurBorderSide := TRVRTFSide(Val);
          end;
        ipropParaBorderType:
          begin
            if FRTFState.GetCurrentBorderSide.FBorderType=rtf_brdr_None then
               FRTFState.GetCurrentBorderSide.FBorderType := TRVRTFBorderType(Val);
          end;
        ipropParaBorderWidth:
          begin
            FRTFState.GetCurrentBorderSide.FWidthTw := Val;
          end;
        ipropParaBorderColor:
          if (Val>=0) and (Val<ColorTable.Count) then
            FRTFState.GetCurrentBorderSide.FColor := ColorTable[Val];
        ipropParaColor:
          if (Val>=0) and (Val<ColorTable.Count) then
            FRTFState.ParaProps.FColor := ColorTable[Val];
        ipropParaFColor:
          if (Val>=0) and (Val<ColorTable.Count) then
            FRTFState.ParaProps.ForeColor := ColorTable[Val];
        ipropParaShading:
          FRTFState.ParaProps.Shading := Val;
        ipropParaBorderSpace:
          FRTFState.ParaProps.Border.Sides[FRTFState.ParaProps.FCurBorderSide].FSpaceTw := Val;
        ipropAnsiCodepage:
          FCodePage := Cardinal(Val);
        ipropPage:
          begin
            if Assigned(FOnRequiredPageBreak) then begin
              Result := OutputChar(#0, True, True);
              if Result<>rtf_ec_OK then
                exit;
              FOnRequiredPageBreak(Self);
            end;
          end;
        { font table }
        ipropFontFamily:
          if FRTFState.rds = rdsFontTable then
            FFontTable[FFontTable.Count-1].Family := TRVRTFFontFamily(Val);
        ipropFCharset:
          {$IFDEF RICHVIEWCBDEF3}
          if FRTFState.rds = rdsFontTable then
            if Val>255 then
              FFontTable[FFontTable.Count-1].Charset := MAC_CHARSET
            else
              FFontTable[FFontTable.Count-1].Charset := Val
          {$ENDIF}
          ;
        ipropIntbl:
          begin
            FRTFState.ParaProps.FInTable := True;
          end;
        ipropPNSecLvl:
          begin
            ChangeDest(idestPNSecLvl, val)
          end;
        ipropS, ipropCS, ipropDS, ipropTX: ;
        { table }
        ipropItap:
          begin                            
            FRTFState.ParaProps.FNestingLevel := Val;
            if Val>0 then
              FRTFState.ParaProps.FInTable := True;
          end;
        ipropRowEnd:
          begin
            FRTFState.RowProps.Finalize;
            CheckTable(True);
            DoTable(rvf_tbl_RowEnd);
          end;
        ipropCellEnd:
          begin
            CheckTable(True);
            Result := OutputChar(#0,True,True);
            Position := rtf_ts_NewPara;
            DoTable(rvf_tbl_CellEnd);
          end;
        ipropRowAlign:
          begin
            FTableAlignment := TRVRTFAlignment(Val);
            FTableAlignmentDefined := True;
          end;
        ipropTRGapH:
          FRTFState.RowProps.FGapHTw := Val;
        ipropTRLeft:
          FRTFState.RowProps.FLeftTw := Val;
        ipropTRRowHeight:
          FRTFState.RowProps.FHeightTw := Val;
        ipropTRHeader:
          FRTFState.RowProps.FHeading := True;
        ipropTRPaddL:
          FRTFState.RowProps.FPaddingTw[rtf_side_Left] := Val;
        ipropTRPaddR:
          FRTFState.RowProps.FPaddingTw[rtf_side_Right] := Val;
        ipropTRPaddT:
          FRTFState.RowProps.FPaddingTw[rtf_side_Top] := Val;
        ipropTRPaddB:
          FRTFState.RowProps.FPaddingTw[rtf_side_Bottom] := Val;
        ipropTRPaddFL:
          FRTFState.RowProps.FUsePadding[rtf_side_Left] := Val=3;
        ipropTRPaddFR:
          FRTFState.RowProps.FUsePadding[rtf_side_Right] := Val=3;
        ipropTRPaddFT:
          FRTFState.RowProps.FUsePadding[rtf_side_Top] := Val=3;
        ipropTRPaddFB:
          FRTFState.RowProps.FUsePadding[rtf_side_Bottom] := Val=3;
        ipropTRSpdL:
          FRTFState.RowProps.FSpacingTw[rtf_side_Left] := Val;
        ipropTRSpdR:
          FRTFState.RowProps.FSpacingTw[rtf_side_Right] := Val;
        ipropTRSpdT:
          FRTFState.RowProps.FSpacingTw[rtf_side_Top] := Val;
        ipropTRSpdB:
          FRTFState.RowProps.FSpacingTw[rtf_side_Bottom] := Val;
        ipropTRSpdFL:
          FRTFState.RowProps.FUseSpacing[rtf_side_Left] := Val=3;
        ipropTRSpdFR:
          FRTFState.RowProps.FUseSpacing[rtf_side_Right] := Val=3;
        ipropTRSpdFT:
          FRTFState.RowProps.FUseSpacing[rtf_side_Top] := Val=3;
        ipropTRSpdFB:
          FRTFState.RowProps.FUseSpacing[rtf_side_Bottom] := Val=3;
        ipropTRwWidth:
          if FRTFState.RowProps.InvertWidth then
            FRTFState.RowProps.FBestWidth := -Val
          else
            FRTFState.RowProps.FBestWidth := Val;
        ipropTRftsWidth:
          case Val of
            2:
              begin
                FRTFState.RowProps.InvertWidth := True;
                FRTFState.RowProps.FBestWidth := - FRTFState.RowProps.BestWidth;
              end;
            3: ;
            else FRTFState.RowProps.FBestWidth := 0;
          end;
        ipropCLVMerge:
          if FRTFState.RowProps.GetLastCellProp.FVMerge<>rtf_cm_First then
            FRTFState.RowProps.GetLastCellProp.FVMerge := TRVRTFCellMerge(Val);
        ipropCLHMerge:
          if FRTFState.RowProps.GetLastCellProp.FHMerge<>rtf_cm_First then
            FRTFState.RowProps.GetLastCellProp.FHMerge := TRVRTFCellMerge(Val);
        ipropCLwWidth:
          begin
            if FRTFState.RowProps.GetLastCellProp.InvertWidth then
              FRTFState.RowProps.GetLastCellProp.FBestWidth := -Val
            else
              FRTFState.RowProps.GetLastCellProp.FBestWidth := Val;
          end;
        ipropCLftsWidth:
          case Val of
            2:
             begin
                FRTFState.RowProps.GetLastCellProp.InvertWidth := True;
                FRTFState.RowProps.GetLastCellProp.FBestWidth := - FRTFState.RowProps.GetLastCellProp.BestWidth;
             end;
            3: ;
            else FRTFState.RowProps.GetLastCellProp.FBestWidth := 0;
          end;
        ipropCLColor:
          begin
            if (Val>=0) and (Val<ColorTable.Count) then
              FRTFState.RowProps.GetLastCellProp.FColor := ColorTable[Val];
            if FRTFState.RowProps.GetLastCellProp.FColor=clWindowText then
              FRTFState.RowProps.GetLastCellProp.FColor := clNone;
          end;
        ipropCLFColor:
          if (Val>=0) and (Val<ColorTable.Count) then
            FRTFState.RowProps.GetLastCellProp.ForeColor := ColorTable[Val];
        ipropCLShading:
          FRTFState.RowProps.GetLastCellProp.Shading := Val;
        ipropCLVertAl:
          FRTFState.RowProps.GetLastCellProp.FVAlign := TRVRTFVAlign(Val);
        ipropCellX:
          begin
            FRTFState.RowProps.GetLastCellProp.FRightBoundaryTw := Val;
            FRTFState.RowProps.CellProps.AddNew;
            FRTFState.RowProps.AssumedLastCell := True;
          end;
        ipropNoTableEv:
           FRTFState.ParaProps.NoTableEv := True;
        ipropNoResetLev:
          FRTFState.ParaProps.NoResetLev := True;
        {$IFDEF RICHVIEW}
        ipropRVCellBestWidth:
          begin
            FRTFState.RowProps.GetLastCellProp.FBestWidth := Val;
            FRTFState.RowProps.RichViewSpecial := True;
          end;
        ipropRVCellBestHeight:
          begin
            FRTFState.RowProps.GetLastCellProp.BestHeight := Val;
            FRTFState.RowProps.RichViewSpecial := True;
          end;
        ipropRVTableBestWidth:
          begin
            FRTFState.RowProps.FBestWidth := Val;
            FRTFState.RowProps.RichViewSpecial := True;
          end;
        {$ENDIF}
        else
          begin
            // Result := rtf_ec_BadTable;
          end;
      end;
  end;
end;
//----------------------------------------------------------------------
// %%Function: ecParseSpecialProperty
// Set a property that requires code to evaluate.
function TRVRTFReader.ParseSpecialProperty(iprop: TRTFiprop; val: Integer): TRVRTFErrorCode;
begin
   Result := rtf_ec_Ok;
   case (iprop) of
     ipropPard:
        FRTFState.ParaProps.Reset;
     ipropPlain:
       begin
         if RTFState.DefFontIndex<0 then
           RTFState.DefFontIndex := FFontTable.GetFontIndex(RTFState.DefFontNumber,
             RTFState.DefFontNumber);
         FRTFState.CharProps.Reset(FRTFState.FDefLanguage, RTFState.DefFontIndex);
       end;
     ipropSectd:
        FRTFState.SectProps.Reset;
     ipropTRowD:
        FRTFState.RowProps.Reset;
     else
        Result := rtf_ec_BadTable;
   end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.FindKeyword(const Keyword: String): Integer;
var min,max,cmp: Integer;
begin
  min := 0;
  max := isymMax;
  while max-min>1 do begin
    Result := (max+min) div 2;
    cmp := CompareStr(rgsymRtf[Result].Keyword,Keyword);
    if cmp=0 then
      exit;
    if cmp<0 then
      min := Result
    else
      max := Result;
  end;
  if Keyword = rgsymRtf[min].Keyword then
    Result := min
  else if (max<>min) and (Keyword = rgsymRtf[max].Keyword) then
    Result := max
  else
    Result := -1;
end;
{------------------------------------------------------------------------------}
// Search rgsymRtf for Keyword and evaluate it appropriately.
// Inputs:
// Keyword:   The RTF control to evaluate.
// param:       The parameter of the RTF control.
// fParam:      fTrue if the control had a parameter; (that is, if param is valid)
//              fFalse if it did not.
function TRVRTFReader.TranslateKeyword(const Keyword: String; param: Integer; fParam:Boolean): TRVRTFErrorCode;
var  isym: Integer;
begin
    Result := rtf_ec_Ok;
    // search for Keyword in rgsymRtf
    isym := FindKeyword(Keyword);
    if isym<0 then  begin            // control word not found
      if (fSkipDestIfUnk) then        // if this is a new destination
            FRTFState.rds := rdsSkip;          // skip the destination
                                    // else just discard it
        fSkipDestIfUnk := False;
        exit;
    end;

    // found it!  use kwd and idx to determine what to do with it.

    fSkipDestIfUnk := False;
    case rgsymRtf[isym].kwd of
      rtf_kwd_Prop:
        begin
          if (rgsymRtf[isym].UseDef or not fParam) then
            param := rgsymRtf[isym].DefValue;
          if FRTFState.rds=rdsNorm then begin
            if rgsymRtf[isym].AffectTo in [rtf_af_CharProp, rtf_af_BothProp] then begin

              Result := OutputChar(#0, {TRTFIPROP(rgsymRtf[isym].Idx)<>ipropPlain}False,True);
              if Result<>rtf_ec_OK then
                exit;
            end;
          end;
          Result := ApplyPropChange(TRTFiprop(rgsymRtf[isym].idx), param);
        end;
      rtf_kwd_Char:
        Result := ParseChar(Char(rgsymRtf[isym].idx));
      rtf_kwd_WideChar:
        {$IFDEF RICHVIEWCBDEF3}
        if (RTFState.rds=rdsNorm) and Assigned(FOnNewUnicodeText) then
          OutputWideChar(WideChar(PWord(@(rgsymRtf[isym].DefValue))^))
        else
        {$ENDIF}
          ParseChar(Char(rgsymRtf[isym].idx));
      rtf_kwd_Dest:
        Result := ChangeDest(TRTFidest(rgsymRtf[isym].idx), 0);
      rtf_kwd_Spec:
        Result := ParseSpecialKeyword(TRTFipfn(rgsymRtf[isym].idx));
      else
        Result := rtf_ec_BadTable;
    end;
end;
{------------------------------------------------------------------------------}
// Change to the destination specified by idest.
function TRVRTFReader.ChangeDest(idest:  TRTFidest; Val: Integer): TRVRTFErrorCode;
var b: Boolean;
begin
    Result := rtf_ec_Ok;
    if (FRTFState.rds = rdsSkip) then             // if we're skipping text,
      exit;                             // don't do anything
    case (idest) of
      idestObject:
        begin
          if Assigned(FOnNewObject) then begin
            FRTFState.rds := rdsObject;
            FObject := TRVRTFObject.Create;
            end
          else
            FRTFState.rds := rdsNorm;
        end;
      idestObjData:
        begin
          if FRTFState.rds = rdsObject then begin
            FRTFState.rds := rdsObjData;
            ObjectInserted := False;
            PicHexStrt := True;
            PicHexVal  := 0;
            end
          else
            FRTFState.rds := rdsSkip;
        end;
      idestObjResult:
        if ObjectInserted then
          FRTFState.rds := rdsSkip
        else
          FRTFState.rds := rdsNorm;
      idestShpPict:
        FRTFState.rds := rdsShpPict;
      idestNonShpPict:
        if ShpPictInserted then
          FRTFState.rds := rdsSkip
        else
          FRTFState.rds := rdsNorm;
      idestNestedTableProps:
        FRTFState.rds := rdsNorm;
      idestStyleSheetParaStyle:
         begin
           FStyleSheet[FStyleSheet.Count-1].FNumber    := Val;
           FStyleSheet[FStyleSheet.Count-1].FStyleType := rtf_sst_Par;
           FRTFState.rds := rdsStyleSheetEntry;
         end;
      idestStyleSheetCharStyle:
         begin
           FStyleSheet[FStyleSheet.Count-1].FNumber    := Val;
           FStyleSheet[FStyleSheet.Count-1].FStyleType := rtf_sst_Char;
           FRTFState.rds := rdsStyleSheetEntry;
         end;
      idestStyleSheetSectStyle:
         begin
           FStyleSheet[FStyleSheet.Count-1].FNumber    := Val;
           FStyleSheet[FStyleSheet.Count-1].FStyleType := rtf_sst_Sect;
           FRTFState.rds := rdsStyleSheetEntry;
         end;
      idestStyleSheet:
         begin
           FStyleSheet.Clear;
           FRTFState.rds := rdsStyleSheet;
           exit;
         end;
      idestFontTable:
         begin
           FFontTable.Clear;
           FRTFState.rds := rdsFontTable;
           exit;
         end;
      idestColorTable:
         begin
           FColorTable.Clear;
           FColorTable.Add;
           FRTFState.rds := rdsColorTable;
         end;
      idestPict:
         begin
           if FPicture<>nil then
             Result := rtf_ec_InvalidPicture
           else begin
             if Assigned(FOnNewPicture) then begin
               FPicture := TRVRTFPicture.Create;
               FPicture.FShpPict := FRTFState.rds=rdsShpPict;
             end;
             FRTFState.rds := rdsPict;
             PicHexStrt := True;
             PicHexVal  := 0;
           end;
         end;
      idestField:
         begin
           OutputChar(#0,True,True);
           FRTFState.rds := rdsField;
         end;
      idestFldInst:
         begin
           if FRTFState.rds=rdsField then begin
             RTFState.FFieldCode := '';
             RTFState.FFieldPictureIncluded := False;
             FRTFState.rds := rdsFldInst;
           end;
         end;
      idestFldRslt:
         begin
           FRTFState.rds := rdsNorm;
         end;
      idestHeader, idestFooter:
         begin
            case idest of
              idestHeader:
                FRTFState.FHFType := rtf_hf_Header;
              idestFooter:
                FRTFState.FHFType := rtf_hf_Footer;
            end;
            if Assigned(FOnHeaderFooter) then begin
              FOnHeaderFooter(Self, FRTFState.FHFType, True, b);
              if not b then
                FRTFState.rds := rdsSkip
              end
            else
              FRTFState.rds := rdsSkip
         end;
      idestPN:
         begin
           FRTFState.rds := rdsPN;
           FMarkerProps.Free;
           FMarkerProps := TRVRTFMarkerProperties.Create;
         end;
      idestPNTextAfter:
         begin
           if FRTFState.rds=rdsPN then
             FRTFState.rds := rdsPNTextAfter
           else
             FRTFState.rds := rdsSkip;
         end;
      idestPNTextBefore:
         begin
           if FRTFState.rds=rdsPN then
             FRTFState.rds := rdsPNTextBefore
           else
             FRTFState.rds := rdsSkip;
         end;
      idestPNSecLvl:
         begin
           FCurPNSecLvl := val;
           FRTFState.rds := rdsPNSecLvl;
         end;
      idestListTable:
         begin
           FListTable.Clear;
           FRTFState.rds := rdsListTable;
         end;
      idestList:
         begin
           if FRTFState.rds<>rdsListTable then
             FRTFState.rds := rdsSkip
           else begin
             FListTable.AddNew;
             FRTFState.rds := rdsList;
           end;
         end;
      idestListName:
         begin
           if FRTFState.rds=rdsList then
             FRTFState.rds := rdsListName;
         end;
      idestListLevel:
         begin
           if FRTFState.rds<>rdsList then
             FRTFState.rds := rdsSkip
           else begin
             FListTable.GetLastList.AddNew;
             FRTFState.rds := rdsListLevel;
           end;
         end;
      idestLevelText:
         begin
           if FRTFState.rds<>rdsListLevel then
             FRTFState.rds := rdsSkip
           else
             FRTFState.rds := rdsListLevelText;
         end;
      idestLevelNumbers:
         begin
           if FRTFState.rds<>rdsListLevel then
             FRTFState.rds := rdsSkip
           else
             FRTFState.rds := rdsListLevelNumbers;
         end;
      idestLOTable:
         begin
           FListOverrideTable.Clear;
           FRTFState.rds := rdsLOTable;
         end;
      idestLO:
         begin
           if FRTFState.rds<>rdsLOTable then
             FRTFState.rds := rdsSkip
           else begin
             FListOverrideTable.AddNew;
             FRTFState.rds := rdsLO;
           end;
         end;
      idestLOLevel:
         begin
           if FRTFState.rds<>rdsLO then
             RTFState.rds := rdsSkip
           else begin
             FListOverrideTable.GetLastListOverride.AddNew;
             FRTFState.rds := rdsLOLevel;
           end;
         end;
      else
        FRTFState.rds := rdsSkip;              // when in doubt, skip it...
    end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.GetFieldCommandValue(const s: String): String;
var NextCharIndex: Integer;
begin
  Result := GetFieldCommandValueEx(s, NextCharIndex);
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.GetFieldCommandValueEx(const s: String;
  var NextCharIndex: Integer): String;
var i,j, k: Integer;
begin
  NextCharIndex := Length(s)+1;
  Result := '';
  for i := 2 to Length(s) do
    if (s[i]<>' ') and (s[i]<>'\') and (s[i-1]=' ') then begin
      k := Length(s)+1;
      for j := i+1 to Length(s) do
        if (s[j]=' ') and ((s[i]<>'"') or ((j-1<>i) and (s[j-1]='"'))) then begin
          k := j;
          break;
        end;
      Result := Copy(s, i, k-i);
      if (Length(Result)>=2) and (Result[1]='"') then
        Result := Copy(Result,2, Length(Result)-2);
      NextCharIndex := k+1;
      break;
    end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.InsertExternalPicture: TRVRTFErrorCode;
const Code1 = 'INCLUDEPICTURE';
      Code2 = 'IMPORT';
var s: String;
    p: Integer;
    pic: TPicture;
    gr: TGraphic;
    Inserted: Boolean;
begin
  Result := rtf_ec_OK;
  if not Assigned(FOnNewPicture) then
    exit;
  s := Trim(FRTFState.FFieldCode);
  if s='' then
    exit;
  if s[1] = '\' then
    s := Copy(s, 2, Length(s));
  if (Copy(UpperCase(s),1, Length(Code1))<>Code1) and
     (Copy(UpperCase(s),1, Length(Code2))<>Code2) then
    exit;
  s := GetFieldCommandValue(s);
  if s='' then
    exit;
  while true do begin
    p := pos('\\', s);
    if p=0 then break;
    Delete(s,p, 1);
  end;
  if (BasePath<>'') and (BasePath[Length(BasePath)] in ['\', '/']) and
     (s<>'') and (s[1] in ['\', '/']) then
    s := Copy(s, 2, Length(s)-1);
  try
    gr := nil;
    if Assigned(FOnImportPicture) then
      FOnImportPicture(Self, BasePath+s, gr);
    if gr=nil then begin
      pic := TPicture.Create;
      try
        try
          pic.LoadFromFile(BasePath+s);
        except
          if BasePath<>'' then
            pic.LoadFromFile(s);
        end;
        if pic.Graphic<>nil then begin
          gr := RV_CreateGraphics(TGraphicClass(pic.Graphic.ClassType));
          gr.Assign(pic.Graphic);
        end;
      finally
        pic.Free;
      end;
    end;
    if gr<>nil then begin
      FRTFState.ParaProps.Finalize;
      RV_AfterImportGraphic(gr);
      FOnNewPicture(Self, nil, gr, Position, Inserted);
      Position := rtf_ts_ContinuePara;
      FRTFState.FFieldPictureIncluded := True;
    end;
  except
   ;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.InsertSymbol: TRVRTFErrorCode;
const Code = 'SYMBOL';
var s: String;
    p: Integer;
    ch: Char;
    FontName: String;
    OldFontSize, FontSize: Integer;

begin
  Result := rtf_ec_OK;
  OldFontSize := RTFState.CharProps.FSize;
  s := Trim(FRTFState.FFieldCode);
  if Copy(UpperCase(s),1, Length(Code))<>Code then
    exit;

  s := Copy(s,Length(Code)+2,Length(s)+1);

  p := Pos(' ', s);
  if p=0 then
    exit;
  ch := Chr(StrToInt(Copy(s, 1, p-1)));
  p := Pos('\f', s);
  if p=0 then
    exit;
  FontName := Copy(s, p+3, Length(s));
  if s='' then
    exit;
  if FontName[1]<>'"' then
    exit;
  FontName := Copy(FontName, 2,Length(FontName));
  p := Pos('"', FontName);
  if p=0 then
    exit;
  FontName := Copy(FontName, 1, p-1);

  p := Pos('\s', s);
  if p=0 then
    exit;
  s := Copy(s, p+3, Length(s));
  p := Pos(' ',s);
  if p>0 then
    s := Copy(s, 1, p-1);
  FontSize := StrToInt(s);

  RTFState.CharProps.FFontName := FontName;
  RTFState.CharProps.FSize := FontSize;

  OutputChar(ch, False, False);
  OutputChar(#0, False, False);
  RTFState.CharProps.FSize := OldFontSize;
  RTFState.CharProps.FFontName := '';
end;
{------------------------------------------------------------------------------}
type
  PBitmap = ^TBitmap;
function RVEnhMetaFileProc(DC: THandle; PHTable: PHandleTable;
  PEMFR: PENHMetaRecord; Obj: Integer; Data: Pointer): Integer; export; stdcall;
var PEMFRSDIB: PEMRStretchDIBits;
  bi: PBitmapInfo;
begin
  case PEMFR.iType of
    1, 70, 17, 11, 9, 10, 33, 37, 48, 75, 14:
      Result := 1;
    EMR_STRETCHDIBITS:
      begin
        if Assigned(PBitmap(Data)^) then begin
          (PBitmap(Data)^).Free;
          PBitmap(Data)^ := nil;
          Result := 0;
          exit;
        end;
        PBitmap(Data)^ := TBitmap.Create;
        PEMFRSDIB := PEMRStretchDIBits(PEMFR);
        bi := PBitmapInfo(PChar(PEMFRSDIB)+PEMFRSDIB.offBmiSrc);
        PBitmap(Data)^.Handle := CreateDIBitmap(DC, bi.bmiHeader,
        CBM_INIT, PChar(PEMFRSDIB)+ PEMFRSDIB.offBitsSrc,
          bi^, PEMFRSDIB.iUsageSrc);
        Result := 1;
      end;
    else begin
      (PBitmap(Data)^).Free;
      PBitmap(Data)^ := nil;
      Result := 0;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function ConvertMetafileToBitmap(wmf: TMetafile): TBitmap;
var bmp: TBitmap;
    DC: THandle;
begin
  bmp := nil;
  DC := GetDC(0);
  try
    EnumEnhMetaFile(DC, wmf.Handle, @RVEnhMetaFileProc, @bmp,
      Bounds(0, 0, wmf.Width, wmf.Height));
  finally
    ReleaseDC(0, DC);
  end;
  Result := bmp;
end;
{------------------------------------------------------------------------------}
// The destination specified by rds is coming to a close.
// If there's any cleanup that needs to be done, do it now.
function TRVRTFReader.EndGroupAction(rds: TRTFrds): TRVRTFErrorCode;
  {.........................................}
  procedure FinalizeDIB;
  var header: TBitmapFileHeader;
  begin
    header.bfType := 19778; // 'BM'
    header.bfSize := FPicture.FData.Size;
    header.bfReserved1 := 0;
    header.bfReserved2 := 0;
    header.bfOffBits   := 0; // ignored by TBitmap.LoadFromStream
    FPicture.FData.Position := 0;
    FPicture.FData.WriteBuffer(header, sizeof(Header));
  end;
  {.........................................}
  function CreateGraphic: TGraphic;
  begin
    case FPicture.FType of
      rtf_pict_EMF:
        begin
         Result := TMetafile.Create;
         TMetafile(Result).Enhanced := True;
        end;
      rtf_pict_WMF:
        begin
         Result := TMetafile.Create;
         TMetafile(Result).Enhanced := False;
        end;
      rtf_pict_DIB:
        begin
          FinalizeDIB;
          Result := TBitmap.Create;
        end;
      rtf_pict_DDB:
        begin
          Result := TBitmap.Create;
        end;
      {$IFNDEF RVDONOTUSEJPEGIMAGE}
      rtf_pict_JPEG:
        Result := TJpegImage.Create;
      {$ENDIF}
      else
        Result := nil;
    end;
  end;
  {.........................................}
  procedure LoadGraphic(var gr: TGraphic);
  var HM: HMetafile;
      MFP: TMetaFilePict;
      wmf: TMetafile;
      {$IFDEF RICHVIEWDEF4}
      //HM2: HMetafile;
      //gr2: TMetafile;
      {$ENDIF}
  begin
    case FPicture.FType of
      rtf_pict_WMF:
        begin
          with MFP do begin
            mm := FPicture.FMetafileMapMode;
            xExt := FPicture.FPicW;
            yExt := FPicture.FPicH;
            hMF  := 0;
          end;
          if (FPicture.FPicW>0) and (FPicture.FPicH>0) and 
             ((FPicture.FMetafileMapMode=MM_ISOTROPIC) or  (FPicture.FMetafileMapMode=MM_ANISOTROPIC))then begin
            FPicture.SuggestedWidth := Round(FPicture.FPicW/2540*Screen.PixelsPerInch);
            FPicture.SuggestedHeight := Round(FPicture.FPicH/2540*Screen.PixelsPerInch);
          end;
          HM := SetWinMetaFileBits(FPicture.FData.Size, FPicture.FData.Memory, 0, MFP);
          if (HM=0) then
            Exception.Create('Invalid metafile');
          TMetafile(gr).Handle := HM;
          {$IFDEF RICHVIEWDEF4}
          // black magic starts...
          {
          with MFP do begin
            mm := FPicture.FMetafileMapMode;
            xExt := 0;
            yExt := 0;
            hMF  := 0;
          end;
          HM2 := SetWinMetaFileBits(FPicture.FData.Size, FPicture.FData.Memory, 0, MFP);
          gr2 := TMetafile.Create;
          gr2.Handle := HM2;
          if (gr.Width<>gr2.Width) or
             (gr.Height<>gr2.Height) then
              TMetafile(gr).Inch := Screen.PixelsPerInch*100;
          gr2.Free;
          }
          // black magic ends...
          {$ENDIF}
          TMetafile(gr).Enhanced := True;
          if FExtractMetafileBitmaps then begin
            wmf := TMetafile(gr);
            gr := ConvertMetafileToBitmap(wmf);
            if gr=nil then
              gr := wmf
            else
              wmf.Free;
          end;
        end;
      rtf_pict_DDB:
        begin
          TBitmap(gr).Handle := CreateBitmap(FPicture.FPicW, FPicture.FPicH,
            FPicture.FWBMPlanes, FPicture.FWBMBitsPixel, FPicture.FData.Memory);
        end;
      else
        begin
          FPicture.FData.Position := 0;
          gr.LoadFromStream(FPicture.FData);
        end;
    end;
    if (FPicture.FPicWGoalTw<>0) and (FPicture.FPicHGoalTw<>0) then begin
      FPicture.SuggestedWidth := Round(FPicture.FPicWGoalTw * Screen.PixelsPerInch / (72*20));
      FPicture.SuggestedHeight := Round(FPicture.FPicHGoalTw * Screen.PixelsPerInch / (72*20));
    end;
  end;
  {.........................................}
var gr: TGraphic;
    s: String;
    {$IFDEF RICHVIEWCBDEF3}
    ws: WideString;
    {$ELSE}
    ws: String;
    {$ENDIF}
begin
  Result :=  rtf_ec_OK;
  case rds of
    rdsFontTable:
      FFontTable.RemoveChasetFromNames;
    rdsFldInst:
      begin
        InsertExternalPicture;
        InsertSymbol;
      end;
    rdsField:
      begin
        FRTFState.FFieldCode := '';
        FRTFState.FFieldPictureIncluded := False;
      end;
    rdsObjData:
      begin
        Result := OutputChar(#0,False,True);
        if Result<>rtf_ec_OK then
          exit;
        Result := DoNewObject;
      end;
    rdsObject:
      begin
        FObject.Free;
        FObject := nil;
      end;
    rdsPict:
      begin
        Result := OutputChar(#0,False,True);
        if Result<>rtf_ec_OK then
          exit;
        if not FRTFState.FFieldPictureIncluded then begin
          if (FPicture=nil) then begin
            if Assigned(FOnNewPicture) then
              Result := rtf_ec_InvalidPicture;
            exit;
          end;
          gr := CreateGraphic;
          if gr<>nil then begin
            try
              LoadGraphic(gr);
            except
              gr.Free;
              Result := rtf_ec_InvalidPicture;
            end;
          end;
          if gr<>nil then
            RV_AfterImportGraphic(gr);
          if Result = rtf_ec_OK then
            Result := DoNewPicture(gr);
        end;
        FPicture.Free;
        FPicture := nil;
      end;
    rdsListLevelText:
      begin
        s := FListTable.GetLastList.GetLastLevel.Text;
        if (Length(s)>0) then begin
          s := Copy(s,1, ord(s[1])+1);
          FListTable.GetLastList.GetLastLevel.FText := s;
        end;
        ws := FListTable.GetLastList.GetLastLevel.TextW;
        if (Length(ws)>0) then begin
          ws := Copy(ws,1, Word(ws[1])+1);
          FListTable.GetLastList.GetLastLevel.FTextW := ws;
        end;
      end;
    rdsListLevelNumbers:
      begin
        s := FListTable.GetLastList.GetLastLevel.Numbers;
        if (Length(s)>0) then begin
          s := Copy(s,1, ord(s[1])+1);
          FListTable.GetLastList.GetLastLevel.FNumbers := s;
        end;
      end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.DoNewPicture(gr: TGraphic): TRVRTFErrorCode;
var Inserted: Boolean;
begin
  try
    if Assigned(FOnNewPicture) then begin
      FRTFState.ParaProps.Finalize;
      FPicture.FData.Position := 0;
      FOnNewPicture(Self, FPicture, gr, Position, Inserted);
      if FPicture.ShpPict then begin
        ShpPictInserted := Inserted;
      end;
      Position := rtf_ts_ContinuePara;
    end;
    Result := rtf_ec_OK;
  except
    Result := rtf_ec_Aborted;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.DoNewObject: TRVRTFErrorCode;
begin
  try
    if Assigned(FOnNewObject) then begin
      FRTFState.ParaProps.Finalize;
      FObject.FData.Position := 0;
      FOnNewObject(Self, FObject, Position, ObjectInserted);
      Position := rtf_ts_ContinuePara;
    end;
    Result := rtf_ec_OK;
  except
    Result := rtf_ec_Aborted;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReader.DoTable(WhatHappens: TRVRTFTableEventKind);
begin
  UpdateMarker;
  if WhatHappens=rvf_tbl_TableStart then
    FTableAlignmentDefined := False;
  if Assigned(FOnTable) then
    FOnTable(Self, WhatHappens);
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReader.CheckTable(AllowEnd: Boolean);
var i, lev,newlev: Integer;
begin
  if FRTFState.ParaProps.NoTableEv then
    AllowEnd := False;
  newlev := CurrentNestingLevel;
  if FRTFState.ParaProps.InTable then
    lev := FRTFState.ParaProps.NestingLevel
  else
    lev := 0;
  if CurrentNestingLevel<lev then begin
    for i := CurrentNestingLevel to lev-1 do
      DoTable(rvf_tbl_TableStart);
    newlev := lev;
  end;
  if AllowEnd and (CurrentNestingLevel>lev) then begin
    for i := CurrentNestingLevel downto lev+1 do
      DoTable(rvf_tbl_TableEnd);
    newlev := lev;
  end;
  CurrentNestingLevel := newlev;
end;
{------------------------------------------------------------------------------}
// Evaluate an RTF control that needs special processing.
function TRVRTFReader.ParseSpecialKeyword(ipfn:TRTFIPFN): TRVRTFErrorCode;
begin
    Result  := rtf_ec_OK;
    if (FRTFState.rds = rdsSkip) and (ipfn <> ipfnBin) then // if we're skipping, and it's not
       exit;                                      // the \bin keyword, ignore it.
    case ipfn of
      ipfnBin:
        begin
           FRTFState.ris   := risBin;
           cbBin := lParam;
        end;
      ipfnSkipDest:
        {if rds<>rdsStyleSheet then } fSkipDestIfUnk := True;
      ipfnHex:
        FRTFState.ris := risHex;
      else
        Result := rtf_ec_BadTable;
    end;
end;
{------------------------------------------------------------------------------}
// Isolate RTF keywords and send them to ParseRtfKeyword;
// Push and pop state at the start and end of RTF groups;
// Send text to ParseChar for further processing.
function TRVRTFReader.Parse: TRVRTFErrorCode;
var
    cNibble,b,i: Integer;
    ch: Char;
begin
    cNibble := 2;
    b := 0;
    while not IsEOF do begin
      ch := GetC;
      if (cGroup < 0) then begin
        Result := rtf_ec_StackUnderflow;
        exit;
      end;
      if (FRTFState.ris = risBin) then begin // if we're parsing binary data, handle it directly
        Result := ParseChar(ch);
        if (Result <> rtf_ec_OK) then
          exit;
        end
      else begin
        case (ch) of
          '{':
             begin
               SkipNext := 0;
               Result := PushRtfState;
               if (Result <> rtf_ec_OK) then
                 exit;
             end;
          '}':
             begin
               SkipNext := 0;
               Result := PopRtfState;
               if (Result <> rtf_ec_OK) then
                 exit;
               if (cGroup=0) then
                 break;
             end;
          '\':
             begin
               Result := ParseRtfKeyword;
               if (Result <> rtf_ec_OK) then
                 exit;
             end;
          #$0d, #$0a:          // cr and lf are noise characters...
             begin
             end;
          else
             begin
               if (FRTFState.ris = risNorm) then begin
                 if SkipNext=0 then
                   Result := ParseChar(ch)
                 else begin
                   dec(SkipNext);
                   Result := rtf_ec_OK;
                 end;
                 if (Result <> rtf_ec_OK) then
                   exit;
                 end
               else begin // parsing hex data
                 if (FRTFState.ris <> risHex) then begin
                   Result := rtf_ec_Assertion;
                   exit;
                 end;
                 b := b shl 4;
                 if (ch in ['0'..'9']) then
                   b := b + (ord(ch) - ord('0'))
                 else begin
                   if (ch in ['a'..'z']) then  begin
                     if not (ch in ['a'..'f']) then begin
                       Result := rtf_ec_InvalidHex;
                       exit;
                     end;
                     b := b + 10+(ord(ch) - ord('a'));
                     end
                   else begin
                     if not (ch in ['A'..'F']) then begin
                       Result := rtf_ec_InvalidHex;
                       exit;
                     end;
                     b := b + 10+(ord(ch) - ord('A'));
                   end;
                 end;
                 dec(cNibble);
                 if (cNibble=0) then begin
                   if SkipNext=0 then
                     Result := ParseChar(Char(b))
                   else begin
                     dec(SkipNext);
                     Result := rtf_ec_OK;
                   end;
                   if (Result <> rtf_ec_OK) then
                     exit;
                   cNibble := 2;
                   b := 0;
                   FRTFState.ris := risNorm;
                 end;
               end                   // end else (ris != risNorm)
             end;
        end;       // case
      end;           // else (ris != risBin)
    end;               // while
    UpdateMarker;
    for i := CurrentNestingLevel downto 1 do
      DoTable(rvf_tbl_TableEnd);
    FColorTable.Finalize;
    if (cGroup < 0) then
      Result := rtf_ec_StackUnderflow
    else if (cGroup > 0) then
      Result := rtf_ec_UnmatchedBrace
    else begin
      if Text<>'' then
        Result := OutputChar(#0,True,True)
      else
        Result := rtf_ec_OK;
    end;
end;
{------------------------------------------------------------------------------}
// Save relevant info on a linked list of SAVE structures.
function TRVRTFReader.PushRtfState: TRVRTFErrorCode;
var SaveItem: TRVRTFReaderState;
begin
  try
    SaveItem := TRVRTFReaderState.Create;
  except
    SaveItem := nil;
  end;
  if (SaveItem=nil) then begin
    Result := rtf_ec_StackOverflow;
    exit;
  end;
  SaveItem.Assign(FRTFState);
  SaveList.Add(SaveItem);
  FRTFState.ris := risNorm;
  inc(cGroup);
  case FRTFState.rds of
    rdsStyleSheet:
      begin
        FStyleSheet.AddPara(0);
        FRTFState.rds := rdsStyleSheetEntry;
      end;
  end;
  Result := rtf_ec_OK;
end;
{------------------------------------------------------------------------------}
// If we're ending a destination (that is, the destination is changing),
// call ecEndGroupAction.
// Always restore relevant info from the top of the SAVE list.
function TRVRTFReader.PopRtfState: TRVRTFErrorCode;
var SaveItem: TRVRTFReaderState;
    CurRds: TRTFrds;
    b: Boolean;
begin
  if (SaveList.Count=0) then begin
    Result := rtf_ec_StackUnderflow;
    exit;
  end;
  if (SaveList.Count=1) then begin
    UpdateMarker;
    if Assigned(OnEndParsing) then
      OnEndParsing(Self);
  end;
  SaveItem := TRVRTFReaderState(SaveList.Items[SaveList.Count-1]);
  if (FRTFState.rds=rdsStyleSheetEntry) and (SaveItem.rds=rdsStyleSheet) then
    FStyleSheet[FStyleSheet.Count-1].Assign(FRTFState);
  if FRTFState.rds=rdsNorm then begin
    Result := OutputChar(#0,False,False);
    if Result<>rtf_ec_OK then
      exit;
  end;
  CurRds := FRTFState.rds;
  if CurRds <> SaveItem.rds then begin
    Result := EndGroupAction(FRTFState.rds);
    if (Result <> rtf_ec_OK) then
      exit;
  end;
  if (FRTFState.FHFType<>SaveItem.FHFType) and
     (FRTFState.FHFType in [rtf_hf_Header,rtf_hf_Footer]) and
     Assigned(FOnHeaderFooter) then
    FOnHeaderFooter(Self, FRTFState.FHFType, False, b);

  FRTFState.Assign(SaveItem);

  if (CurRds=rdsPN) and (FRTFState.rds<>rdsPN) and (FMarkerProps<>nil) then begin
    FRTFState.ParaProps.MarkerProps.Assign(FMarkerProps, False);
    FMarkerProps.Free;
    FMarkerProps := nil;
  end;

  SaveList.Delete(SaveList.Count-1);
  dec(cGroup);
  Result := rtf_ec_OK;
end;
{------------------------------------------------------------------------------}
// get a control word (and its associated value) and
// call TranslateKeyword to dispatch the control.
function TRVRTFReader.ParseRtfKeyword: TRVRTFErrorCode;
var ch: Char;
    fParam, fNeg: Boolean;
    param: Integer;
    Keyword, szParameter: String;
    {$IFDEF RVTEXTFOOTNOTES}
    footnotetext : String;
    footkeyword : String;
    isym : Integer;
    {$ENDIF}
begin
  fParam := False;
  fNeg   := False;
  param  := 0;
  //char *pch;

  Keyword  := '';
  szParameter := '';
  if IsEOF then begin
    Result := rtf_ec_EndOfFile;
    exit;
  end;
  ch := GetC;
  if (not (ch in ['a'..'z','A'..'Z'])) then begin
    // a control symbol; no delimiter.
    Keyword := ch;
    Result := TranslateKeyword(Keyword, 0, fParam);
    exit;
  end;
  repeat
    Keyword := Keyword + ch;
    ch := GetC;
  until (not (ch in ['a'..'z','A'..'Z'])) or IsEOF ;
  if (ch = '-') then begin
    fNeg  := True;
    ch := GetC;
  end;
  if (ch in ['0'..'9']) then begin
    fParam := True;         // a digit after the control means we have a parameter
    repeat
      szParameter := szParameter + ch;
      ch := GetC;
    until (not (ch in ['0'..'9'])) or IsEOF ;
    param := StrToInt(szParameter);
    if (fNeg) then
      param := -param;
    lParam := StrToInt(szParameter);
    if (fNeg) then
      lParam := -lParam;
  end;
  {$IFDEF RVTEXTFOOTNOTES}
  if keyword='footnote' then begin
    repeat
      ch := GetC;
    until (ch=' ') or (ch='}');
    footnotetext := '';
    if ch <> '}' then begin
      repeat
        //need to parse any special characters out.
        if ch = '\' then begin
          footKeyWord:='';
          ch :=' ';
          repeat
            footKeyword := footKeyword + ch;
            ch := GetC;
          until (not (ch in ['a'..'z','A'..'Z'])) or IsEOF ;
          isym := FindKeyword(trim(footKeyword));
          ch := chr(rgsymRtf[isym].idx);
         end;
         footnotetext := footnotetext + ch;
         ch := GetC;
      until ch = '}';
      trim(footnotetext);
    end;
    UngetC;
    FRTFState.FCharProps.FFootnote:=footnotetext;
    param:=1;
  end;
  {$ENDIF}
  if (ch <> ' ') then
    UngetC;
  if SkipNext=0 then
    Result := TranslateKeyword(Keyword, param, fParam)
  else begin
    dec(SkipNext);
    Result := rtf_ec_OK;
  end;
end;
{------------------------------------------------------------------------------}
// Route the character to the appropriate destination stream.
function TRVRTFReader.ParseChar(ch: Char): TRVRTFErrorCode;
  {..............................................}
  function AddChar(Stream: TMemoryStream; ch: Char): Boolean;
  begin
    Result := False;
    PicHexVal := PicHexVal shl 4;
    if (ch in ['0'..'9']) then
      PicHexVal := PicHexVal + (ord(ch) - ord('0'))
    else begin
      if (ch in ['a'..'z']) then  begin
        if not (ch in ['a'..'f']) then
          exit;
        PicHexVal := PicHexVal + 10+(ord(ch) - ord('a'));
      end
      else begin
        if not (ch in ['A'..'F']) then
          exit;
          PicHexVal := PicHexVal + 10+(ord(ch) - ord('A'));
      end;
    end;
    if not PicHexStrt then begin
      Stream.WriteBuffer(PicHexVal,1);
      PicHexVal := 0;
    end;
    PicHexStrt := not PicHexStrt;
    Result := True;
  end;
  {..............................................}
begin
  Result := rtf_ec_OK;
  case (FRTFState.rds) of
    rdsSkip:
      ;
    rdsStyleSheetEntry:
      begin
        if ch<>';' then
          FStyleSheet[FStyleSheet.Count-1].FName := FStyleSheet[FStyleSheet.Count-1].Name+ch;
      end;
    rdsNorm:
      begin
        if (ch in [#$0a, #$0d]) and Assigned(FOnUpdateMarker) then
          UpdateMarker;
        Result := OutputChar(ch,True,True);
        if (ch in [#$0a, #$0d]) and Assigned(FOnUpdateMarker) then
          UpdateMarker;
      end;
    rdsFontTable:
      begin
        if ch<>';' then
          FFontTable[FFontTable.Count-1].Name := FFontTable[FFontTable.Count-1].Name+ch;
      end;
    rdsColorTable:
      begin
        if ch=';' then
          FColorTable.Add;
      end;
    rdsObjData:
      begin
        if FObject<>nil then
          if FRTFState.ris = risBin then
            FObject.FData.WriteBuffer(ch,1)
          else if not AddChar(FObject.FData, ch) then
            Result := rtf_ec_InvalidPicture;
      end;
    rdsPict:
      begin
        if FPicture<>nil then
          if FRTFState.ris = risBin then
            FPicture.FData.WriteBuffer(ch,1)
          else if not AddChar(FPicture.FData, ch) then
            Result := rtf_ec_InvalidPicture;
      end;
    rdsFldInst:
      FRTFState.FFieldCode := FRTFState.FFieldCode+ch;
    rdsPNTextAfter:
      FMarkerProps.FTextAfter := FMarkerProps.FTextAfter+ch;
    rdsPNTextBefore:
      FMarkerProps.FTextBefore := FMarkerProps.FTextBefore+ch;
    rdsListName:
      FListTable.GetLastList.FName := FListTable.GetLastList.FName+ch;
    rdsListLevelText:
      with FListTable.GetLastList.GetLastLevel do begin
        FText := FText+ch;
        if FTextW<>'' then
          {$IFDEF RICHVIEWCBDEF3}
          FTextW := FTextW+AnsiToUnicode(ch, FCodePage);
          {$ELSE}
          FTextW := FTextW+ch;
          {$ENDIF}
      end;
    rdsListLevelNumbers:
      FListTable.GetLastList.GetLastLevel.FNumbers := FListTable.GetLastList.GetLastLevel.FNumbers+ch;
  end;
  if (FRTFState.ris = risBin) then begin
    dec(cbBin);
    if cbBin <= 0 then
        FRTFState.ris := risNorm;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReader.UpdateMarker;
begin
  if not Assigned(FOnUpdateMarker) then
    exit;
  if RTFState.ParaProps.HasMarker then
    RTFState.ParaProps.MarkerProps.UpdateFrom(RTFState.CharProps);
  FOnUpdateMarker(Self);
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.FlushOutput(var NextPosition: TRVRTFPosition): TRVRTFErrorCode;
begin
  {$IFDEF RICHVIEWCBDEF3}
  if Length(TextW)>0 then
    OutputWideChar(#0);
  {$ENDIF}
  Result := DoNewText(Position, NextPosition);
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
function TRVRTFReader.OutputWideChar(ch: WideChar): TRVRTFErrorCode;
begin
  Result := rtf_ec_OK;
  case RTFState.rds of
    rdsNorm:
      begin
        CheckTable(True);
        if Assigned(FOnNewUnicodeText) then begin
          if Text<>'' then
            OutputChar(#0, False, False);
          if ord(ch)>0 then begin
            TextW := TextW+WideString(ch);
          end;
        end;
      end;
    rdsListLevelText:
      begin
        if FListTable.GetLastList.GetLastLevel.TextW='' then
          FListTable.GetLastList.GetLastLevel.FTextW := AnsiToUnicode(FListTable.GetLastList.GetLastLevel.FText, FCodePage);
        FListTable.GetLastList.GetLastLevel.FTextW := FListTable.GetLastList.GetLastLevel.FTextW+WideString(ch);
        FListTable.GetLastList.GetLastLevel.FText := FListTable.GetLastList.GetLastLevel.FText+UnicodeToAnsi(ch);
      end;
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRVRTFReader.OutputChar(ch: Char; ACheckTableEnd, ACheckTable: Boolean): TRVRTFErrorCode;
var NextPosition: TRVRTFPosition;
begin
  if AcheckTable then
    CheckTable(ACheckTableEnd);

  if ch in [#0,#10,#13] then begin
    case ch of
      #10:
        NextPosition := rtf_ts_NewPara;
      #13:
        NextPosition := rtf_ts_NewLine;
      else
        NextPosition := rtf_ts_ContinuePara;
    end;
    Result := FlushOutput(NextPosition);
    if Result<>rtf_ec_OK then
      exit;
    if NextPosition<>rtf_ts_ContinuePara then
      Position := NextPosition
    else
      case ch of
        #10:
          Position := rtf_ts_NewPara;
        #13:
          Position := rtf_ts_NewLine;
        else
          Position := rtf_ts_ContinuePara;
      end;
    end
  else begin
    {$IFDEF RICHVIEWCBDEF3}
    if (TextW<>'') then begin
      OutputChar(#0, False, False);
    end;
    {$ENDIF}
    Text := Text+ch;
  end;
  Result := rtf_ec_OK;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.DoNewText(Position: TRVRTFPosition;
                                 var NextPosition: TRVRTFPosition): TRVRTFErrorCode;
begin
  Result := rtf_ec_OK;
  if not ForceEvenEmptyNewLine and (Text='')
    {$IFDEF RICHVIEWCBDEF3}
     and (TextW='')
    {$ENDIF}
  then begin
     if Position=rtf_ts_ContinuePara then
       exit;
     if NextPosition=rtf_ts_ContinuePara then begin
       NextPosition := Position;
       exit;
     end;
  end;
  FRTFState.ParaProps.Finalize;
  try
    {$IFDEF RICHVIEWCBDEF3}
    if Assigned(FOnNewUnicodeText) and (TextW<>'') then
      FOnNewUnicodeText(Self,TextW,Position)
    else
    {$ENDIF}
      if Assigned(FOnNewText) then
        FOnNewText(Self, Text, Position);
  except
    Result := rtf_ec_Aborted;
  end;
  Text := '';
  {$IFDEF RICHVIEWCBDEF3}
  TextW := '';
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.ReadFromFile(const AFileName: String): TRVRTFErrorCode;
var Stream: TFileStream;
begin
  Result := rtf_ec_FileOpenError;
  try
    Stream := TFileStream.Create(AFileName, fmOpenRead);
  except
    Stream := nil;
  end;
  if Stream<>nil then begin
    Result := ReadFromStream(Stream);
    Stream.Free;
  end;
end;
{------------------------------------------------------------------------------}
const BUFFERSIZE=4096;
function TRVRTFReader.ReadFromStream(AStream: TStream): TRVRTFErrorCode;
begin
  Stream := AStream;
  StreamSize := Stream.Size;
  Clear;
  SetLength(InputString, BUFFERSIZE);
  InputStringIndex := BUFFERSIZE+1;
  FCallProgress := (StreamSize>BUFFERSIZE*5) and Assigned(FOnProgress);
  if FCallProgress then
    FOnProgress(Self, rvprtfprStarting, 0);
  try
    Result := Parse;
  except
    Result := rtf_ec_Exception;
  end;
  InputString := '';
  if FCallProgress then
    FOnProgress(Self, rvprtfprEnding, 0);  
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.GetC: Char;
begin
  if UseLastChar then begin
    Result :=  LastChar;
    UseLastChar := False;
    exit;
  end;
  if InputStringIndex>Length(InputString) then begin
    if FCallProgress then
      FOnProgress(Self, rvprtfprRunning,
        MulDiv(Stream.Position, 100, StreamSize));
    if StreamSize-Stream.Position>=BUFFERSIZE then
      Stream.ReadBuffer(PChar(InputString)^, BUFFERSIZE)
    else begin
      SetLength(InputString,StreamSize-Stream.Position);
      Stream.ReadBuffer(PChar(InputString)^, Length(InputString))
    end;
    InputStringIndex := 1;
  end;
  Result := InputString[InputStringIndex];
  LastChar := Result;
  inc(InputStringIndex);
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReader.UngetC;
begin
  UseLastChar := True;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.IsEOF: Boolean;
begin
  Result := not UseLastChar and (InputStringIndex>Length(InputString)) and (Stream.Position>=StreamSize);
end;
{------------------------------------------------------------------------------}
{$IFDEF RICHVIEWCBDEF3}
function TRVRTFReader.AnsiToUnicode(const s: String; CodePage: Cardinal): WideString;
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
    SetLength(Result, l);
    MultiByteToWideChar(CodePage, MB_PRECOMPOSED or MB_USEGLYPHCHARS, PChar(s), Length(s),
                             Pointer(Result), l);
    end
  else begin
    SetLength(Result, Length(s));
    for l := 0 to Length(s)-1 do
      Result[l] := '?';
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFReader.UnicodeToAnsi(const s: WideString): String;
var l: Integer;
    DefChar: Char;
    Flags: Integer;
    Len: Integer;
    CodePage: Cardinal;
begin
  if Length(s)=0 then begin
    Result := '';
    exit;
  end;
  CodePage := FCodePage;
  DefChar := '?';
  Flags := WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR;
  Len := Length(s);
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
    FillChar(PChar(Result)^, Len, '?');
  end;
end;
{$ENDIF}
{============================ TRVRTFColorList =================================}
procedure TRVRTFColorList.Add;
begin
  inherited Add(Pointer(clWindowText));
end;
{------------------------------------------------------------------------------}
procedure TRVRTFColorList.Finalize;
begin
  if Count>0 then
   Delete(Count-1);
end;
{------------------------------------------------------------------------------}
function TRVRTFColorList.Get(Index: Integer): TColor;
begin
  Result := TColor(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
procedure TRVRTFColorList.Put(Index: Integer; const Value: TColor);
begin
  inherited Put(Index, Pointer(Value));
end;
{------------------------------------------------------------------------------}
procedure TRVRTFColorList.ResetLast;
begin
  if Items[Count-1]=clWindowText then
     Items[Count-1] := 0;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFColorList.SetLastBlue(Value: Integer);
begin
  ResetLast;
  Items[Count-1] := Items[Count-1] or (Value shl 16);
end;
{------------------------------------------------------------------------------}
procedure TRVRTFColorList.SetLastGreen(Value: Integer);
begin
  ResetLast;
  Items[Count-1] := Items[Count-1] or (Value shl 8);
end;
{------------------------------------------------------------------------------}
procedure TRVRTFColorList.SetLastRed(Value: Integer);
begin
  ResetLast;
  Items[Count-1] := Items[Count-1] or Value;
end;
{============================= TRVRTFFontList =================================}
function TRVRTFFontList.Get(Index: Integer): TRVRTFFont;
begin
  Result := TRVRTFFont(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
procedure TRVRTFFontList.Put(Index: Integer; const Value: TRVRTFFont);
begin
  inherited Put(Index, Value);
end;
{------------------------------------------------------------------------------}
procedure TRVRTFFontList.RemoveChasetFromNames;
var i: Integer;
    fontname,csname: String;
begin
  {$IFDEF RICHVIEWCBDEF3}
  csname := '';
  for i := 0 to Count-1 do begin
    case Items[i].Charset of
      RUSSIAN_CHARSET:
        csname := 'cyr';
      EASTEUROPE_CHARSET:
        csname := 'ce';
      GREEK_CHARSET:
        csname := 'greek';
      TURKISH_CHARSET:
        csname := 'tur';
      BALTIC_CHARSET:
        csname := 'baltic';
      else
        continue;
    end;
    fontname := Items[i].Name;
    if Length(fontname)<=Length(csname) then
      continue;
    if AnsiLowerCase(Copy(fontname, Length(fontname)-Length(csname), Length(csname)+1))=' '+csname then
      Items[i].Name := Copy(fontname, 1, Length(fontname)-Length(csname)-1);
  end;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
function TRVRTFFontList.GetFontIndex(Number, Default: Integer): Integer;
var i: Integer;
begin
  Result := -1;
  for i := 0 to Count-1 do
    if Items[i].Number=Number then begin
      Result := i;
      exit;
    end;
  if Number<>Default then
    Result := GetFontIndex(Default, Default);
  if Result<0 then
    Result := 0;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFFontList.Add(Number: Integer);
var Item:TRVRTFFont;
begin
  Item := TRVRTFFont.Create;
  Item.Number := Number;
  inherited Add(Item);
end;
{========================= TRVRTFTextProperties ================================}
constructor TRVRTFCharProperties.Create;
begin
  inherited Create;
  Reset(0,0);
end;
{------------------------------------------------------------------------------}
procedure TRVRTFCharProperties.Assign(Source: TRVRTFCharProperties);
begin
  FSize      := Source.Size;
  FColor     := Source.Color;
  FBackColor := Source.BackColor;
  FFontIndex := Source.FontIndex;
  FStyle     := Source.Style;
  FStyleEx   := Source.StyleEx;
  FCharScaleX := Source.CharScaleX;
  FSScriptType := Source.SScriptType;
  FCharSpacingTw := Source.CharSpacingTw;
  FHidden    := Source.Hidden;
  {$IFDEF RVTEXTFOOTNOTES}
  FFootNote  := Source.FootNote;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRVRTFCharProperties.Reset(DefLanguage: Cardinal; DefFontIndex: Integer);
begin
  FSize      := 12;
  FColor     := clWindowText;
  FBackColor := clNone;
  FFontIndex := DefFontIndex;
  FStyle     := [];
  FStyleEx   := [];
  FCharScaleX := 100;
  FSScriptType := rtf_ss_Normal;
  FCharSpacingTw := 0;
  FHidden    := False;
  FFontName := '';
  FLanguage  := DefLanguage;
  {$IFDEF RVTEXTFOOTNOTES}
  FFootNote  := '';
  {$ENDIF}
end;
{========================== TRVRTFParaProperties ==============================}
constructor TRVRTFParaProperties.Create;
begin
  inherited Create;
  Reset;
end;
{------------------------------------------------------------------------------}
destructor TRVRTFParaProperties.Destroy;
begin
  FBorder.Free;
  FMarkerProps.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFParaProperties.Assign(Source: TRVRTFParaProperties);
begin
  FLeftIndentTw  := Source.LeftIndentTw;
  FRightIndentTw := Source.RightIndentTw;
  FFirstIndentTw := Source.FirstIndentTw;
  FSpaceBeforeTw := Source.SpaceBeforeTw;
  FSpaceAfterTw  := Source.SpaceAfterTw;
  FAlignment     := Source.Alignment;
  FColor         := Source.Color;
  FLineSpacing   := Source.LineSpacing;
  FLineSpacingMulti := Source.LineSpacingMulti;
  if Source.FBorder<>nil then
    Border.Assign(Source.FBorder)
  else begin
    FBorder.Free;
    FBorder := nil;
  end;
  if Source.FMarkerProps<>nil then
    MarkerProps.Assign(Source.FMarkerProps, False)
  else begin
    FMarkerProps.Free;
    FMarkerProps := nil;
  end;
  FCurBorderSide := Source.FCurBorderSide;
  FNestingLevel  := Source.NestingLevel;
  FInTable       := Source.InTable;
  NoTableEv      := Source.NoTableEv;
  NoResetLev     := Source.NoResetLev;
  FListOverrideIndex := Source.ListOverrideIndex;
  FListLevel     := Source.ListLevel;
  Shading        := Source.Shading;
  ForeColor      := Source.ForeColor;
  FKeepLinesTogether := Source.KeepLinesTogether;
  FKeepWithNext  := Source.KeepWithNext;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFParaProperties.Reset;
begin
  FLeftIndentTw  := 0;
  FRightIndentTw := 0;
  FFirstIndentTw := 0;
  FSpaceBeforeTw := 0;
  FSpaceAfterTw  := 0;
  FAlignment     := rtf_al_Left;
  FBorder.Free;
  FBorder        := nil;
  FMarkerProps.Free;
  FMarkerProps   := nil;
  FCurBorderSide := rtf_side_Left;
  FColor         := clNone;
  FLineSpacing   := 240;     // \  single
  FLineSpacingMulti := True; // /  spacing
  if not NoResetLev then begin
    FNestingLevel  := 1;
    FInTable       := False;
  end;
  NoTableEv      := False;
  NoResetLev     := False;
  FListOverrideIndex := -1;
  FListLevel     := 0;
  Shading        := 0;
  ForeColor      := clBlack;
  FKeepLinesTogether := False;
  FKeepWithNext  := False;
end;
{------------------------------------------------------------------------------}
function TRVRTFParaProperties.GetBorder: TRVRTFParaBorder;
begin
  if FBorder=nil then
    FBorder := TRVRTFParaBorder.Create;
  Result := FBorder;
end;
{------------------------------------------------------------------------------}
function TRVRTFParaProperties.GetMarkerProps: TRVRTFMarkerProperties;
begin
  if FMarkerProps=nil then
    FMarkerProps := TRVRTFMarkerProperties.Create;
  Result := FMarkerProps;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFParaProperties.Finalize;
begin
  if Shading=0 then
    exit;
  if (FColor=clNone) then
    FColor := clWhite;
  FColor := ShadeColor(ColorToRGB(Color), ColorToRGB(ForeColor), Shading);
  Shading := 0;
end;
{------------------------------------------------------------------------------}
function TRVRTFParaProperties.HasBorder: Boolean;
var i: TRVRTFSide;
begin
  Result := (FBorder<>nil);
  if Result then begin
    Result := False;
    for i := Low(TRVRTFSide) to High(TRVRTFSide) do
      if (FBorder.FSides[i]<>nil) and (FBorder.FSides[i].FBorderType<>rtf_brdr_None) then begin
        Result := True;
        break;
      end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFParaProperties.HasMarker: Boolean;
begin
  Result := (FMarkerProps<>nil);
end;
{========================== TRVRTFSectionProperties ===========================}
constructor TRVRTFSectionProperties.Create;
begin
  inherited Create;
  Reset;
end;
{------------------------------------------------------------------------------}
destructor TRVRTFSectionProperties.Destroy;
begin
  FDefMarkerPropsList.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFSectionProperties.InitListDefaults;
var i: Integer;
begin
  if FDefMarkerPropsList= nil then begin
    FDefMarkerPropsList := TRVList.Create;
    for i := 1 to 11 do
      FDefMarkerPropsList.Add(TRVRTFMarkerProperties.Create);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFSectionProperties.Assign(Source: TRVRTFSectionProperties);
var i: Integer;
begin
  FColumnCount      := Source.FColumnCount;
  FPageNumberXTw    := Source.FPageNumberXTw;
  FPageNumberYTw    := Source.FPageNumberYTw;
  FPageNumberFormat := Source.FPageNumberFormat;
  FSectionBreakType := Source.FSectionBreakType;
  if Source.FDefMarkerPropsList=nil then begin
    FDefMarkerPropsList.Free;
    FDefMarkerPropsList := nil;
    end
  else begin
    InitListDefaults;
    for i := 1 to 11 do
      TRVRTFMarkerProperties(FDefMarkerPropsList[i-1]).Assign(TRVRTFMarkerProperties(Source.FDefMarkerPropsList[i-1]), False);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFSectionProperties.Reset;
begin
  FColumnCount      := 1;
  FPageNumberXTw    := 720;
  FPageNumberYTw    := 720;
  FPageNumberFormat := rtf_pg_Decimal;
  FSectionBreakType := rtf_sbk_Page;
  FFooterYTw        := 720;
  FHeaderYTw        := 720;
  FDefMarkerPropsList.Free;
  FDefMarkerPropsList := nil;
end;
{=========================== TRVRTFDocProperties ==============================}
constructor TRVRTFDocProperties.Create;
begin
  inherited Create;
  Reset;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFDocProperties.Assign(Source: TRVRTFDocProperties);
begin
  FPaperWidthTw   := Source.FPaperWidthTw;
  FPaperHeightTw  := Source.FPaperHeightTw;
  FLeftMarginTw   := Source.FLeftMarginTw;
  FRightMarginTw  := Source.FRightMarginTw;
  FTopMarginTw    := Source.FTopMarginTw;
  FBottomMarginTw := Source.FBottomMarginTw;
  FPageNumberStart := Source.FPageNumberStart;
  FFacingPages    := Source.FFacingPages;
  FLandscape      := Source.FLandscape;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFDocProperties.Reset;
begin
  FPaperWidthTw   := 12240;
  FPaperHeightTw  := 15480;
  FLeftMarginTw   := 1800;
  FRightMarginTw  := 1800;
  FTopMarginTw    := 1440;
  FBottomMarginTw := 1440;
  FPageNumberStart := 1;
  FFacingPages    := False;
  FLandscape      := False;
end;
{============================ TRVRTFStyleSheetEntry ============================}
constructor TRVRTFStyleSheetEntry.Create;
begin
  inherited Create;
  FParaProps := TRVRTFParaProperties.Create;
  FCharProps := TRVRTFCharProperties.Create;
  FAdditive := False;
  FHidden   := False;
  FStyleType := rtf_sst_Char;
  FNumber    := 0;
  FBasedOn   := nil;
  FNext      := Self;
end;
{------------------------------------------------------------------------------}
destructor TRVRTFStyleSheetEntry.Destroy;
begin
  FParaProps.Free;
  FCharProps.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFStyleSheetEntry.Assign(Source: TRVRTFReaderState);
begin
  FParaProps.Assign(Source.ParaProps);
  FCharProps.Assign(Source.CharProps);
end;
{============================ TRVRTFStyleSheet ================================}
procedure TRVRTFStyleSheet.AddPara(Number: Integer);
var item: TRVRTFStyleSheetEntry;
begin
  item := TRVRTFStyleSheetEntry.Create;
  item.FNumber := Number;
  item.FStyleType := rtf_sst_Par;
  Add(item);
end;
{------------------------------------------------------------------------------}
function TRVRTFStyleSheet.Get(Index: Integer): TRVRTFStyleSheetEntry;
begin
  Result := TRVRTFStyleSheetEntry(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
function TRVRTFStyleSheet.GetEntry(Number: Integer): TRVRTFStyleSheetEntry;
var i: Integer;
begin
  Result := nil;
  for i := 0 to Count-1 do
    if Items[i].Number = Number then begin
      Result := Items[i];
      break;
    end;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFStyleSheet.Put(Index: Integer;
  const Value: TRVRTFStyleSheetEntry);
begin
  inherited Put(Index, Value);
end;
{========================= TRVRTFReaderState ==================================}
constructor TRVRTFReaderState.Create;
begin
  inherited Create;
  FParaProps := TRVRTFParaProperties.Create;
  FCharProps := TRVRTFCharProperties.Create;
  FSectProps := TRVRTFSectionProperties.Create;
  FDocProps  := TRVRTFDocProperties.Create;
end;
{------------------------------------------------------------------------------}
destructor TRVRTFReaderState.Destroy;
begin
  FParaProps.Free;
  FCharProps.Free;
  FSectProps.Free;
  FDocProps.Free;
  FRowProps.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderState.Assign(Source: TRVRTFReaderState);
begin
  ParaProps.Assign(Source.ParaProps);
  CharProps.Assign(Source.CharProps);
  FSectProps.Assign(Source.SectProps);
  FDocProps.Assign(Source.DocProps);
  FDefLanguage := Source.FDefLanguage;
  if Source.FRowProps<>nil then begin
    if FRowProps=nil then
      FRowProps := TRVRTFRowProperties.Create;
    FRowProps.Assign(Source.FRowProps);
    end
  else begin
    FRowProps.Free;
    FRowProps := nil;
  end;
  rds        := Source.rds;
  ris        := Source.ris;
  FCurrentBorderType := Source.FCurrentBorderType;
  FHFType    := Source.FHFType;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderState.ChangeFontStyle(fs: TFontStyle; Val: Integer);
begin
  if Val=0 then
    Exclude(CharProps.FStyle, fs)
  else
    Include(CharProps.FStyle, fs);
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderState.ChangeFontStyleEx(fs: TRVRTFFontStyleEx;
  Val: Integer);
begin
  if Val=0 then
    Exclude(CharProps.FStyleEx, fs)
  else
    Include(CharProps.FStyleEx, fs);
end;
{------------------------------------------------------------------------------}
procedure TRVRTFReaderState.Reset;
begin
  FDefLanguage := 0;
  CharProps.Reset(DefLanguage, 0);
  ParaProps.Reset;
  SectProps.Reset;
  DocProps.Reset;
  FRowProps.Free;
  FRowProps := nil;
  FFieldCode := '';
  FFieldPictureIncluded := False;
  FCurrentBorderType := rtf_bt_Para;
  DefFontNumber := 0;
  DefFontIndex  := -1;
  FHFType := rtf_hf_MainText;
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderState.GetRowProps: TRVRTFRowProperties;
begin
  if FRowProps=nil then
    FRowProps := TRVRTFRowProperties.Create;
  Result := FRowProps;
end;
{------------------------------------------------------------------------------}
function TRVRTFReaderState.GetCurrentBorderSide: TRVRTFBorderSide;
begin
  case FCurrentBorderType of
    rtf_bt_Row:
      Result := RowProps.Border.Sides[RowProps.FCurBorderSide];
    rtf_bt_Cell:
      Result := RowProps.GetLastCellProp.Border.Sides[RowProps.GetLastCellProp.FCurBorderSide];
    else
      Result := ParaProps.Border.Sides[ParaProps.FCurBorderSide];
  end;
end;
{============================== TRVRTFBorderSide ==============================}
procedure TRVRTFBorderSide.Assign(Source: TRVRTFBorderSide);
begin
  FBorderType := Source.BorderType;
  FWidthTw    := Source.WidthTw;
  FColor      := Source.Color;
  FSpaceTw    := Source.SpaceTw;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFBorderSide.Reset;
begin
  FBorderType := rtf_brdr_None;
  FWidthTw    := 0;
  FColor      := clNone;
  FSpaceTw    := 0;
end;
{============================== TRVRTFParaBorder ==============================}
destructor TRVRTFParaBorder.Destroy;
var i: TRVRTFSide;
begin
  for i := Low(TRVRTFSide) to High(TRVRTFSide) do
    FSides[i].Free;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFParaBorder.Assign(Source: TRVRTFParaBorder);
var i: TRVRTFSide;
begin
  for i := Low(TRVRTFSide) to High(TRVRTFSide) do
    if Source.FSides[i]=nil then begin
      FSides[i].Free;
      FSides[i] := nil;
      end
    else
      Sides[i].Assign(Source.FSides[i]);
end;
{------------------------------------------------------------------------------}
procedure TRVRTFParaBorder.Reset;
var i: TRVRTFSide;
begin
  for i := Low(TRVRTFSide) to High(TRVRTFSide) do
    if FSides[i]<>nil then
      FSides[i].Reset;
end;
{------------------------------------------------------------------------------}
function TRVRTFParaBorder.GetSides(Index: TRVRTFSide): TRVRTFBorderSide;
begin
  if FSides[Index]=nil then
    FSides[Index] := TRVRTFBorderSide.Create;
  Result := FSides[Index];
end;
{============================= TRVRTFPicture ==================================}
constructor TRVRTFPicture.Create;
begin
  inherited Create;
  FData := TMemoryStream.Create;
  FPicScaleX := 100;
  FPicScaleY := 100;
  FWBMBitsPixel := 1;
  FWBMPlanes    := 1;
end;
{------------------------------------------------------------------------------}
destructor TRVRTFPicture.Destroy;
begin
  FData.Free;
  inherited Destroy;
end;
{============================== TRVRTFObject ==================================}
constructor TRVRTFObject.Create;
begin
  inherited;
  FData := TMemoryStream.Create;
end;
{------------------------------------------------------------------------------}
destructor TRVRTFObject.Destroy;
begin
  FData.Free;
  inherited;
end;
{============================ TRVRTFRowProperties =============================}
constructor TRVRTFRowProperties.Create;
begin
  inherited Create;
  FBorder := TRVRTFParaBorder.Create;
  FCellProps := TRVRTFCellPropsList.Create;
  Reset;
end;
{------------------------------------------------------------------------------}
destructor TRVRTFRowProperties.Destroy;
begin
  FBorder.Free;
  CellProps.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFRowProperties.Reset;
var side: TRVRTFSide;
begin
  FGapHTw    := 0;
  FLeftTw    := 0;
  FHeightTw  := 0;
  FBestWidth := 0;
  for side := Low(TRVRTFSide) to High(TRVRTFSide) do begin
    FPaddingTw[side] := 0;
    FSpacingTw[side] := 0;
    FUsePadding[side] := False;
    FUseSpacing[side] := False;
  end;
  FCurBorderSide := rtf_side_Left;
  FBorder.Reset;
  FCellProps.Clear;
  FCellProps.AddNew;
  NewCellProps := True;
  AssumedLastCell := True;
  FHeading := False;
  {$IFDEF RICHVIEW}
  RichViewSpecial := False;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRVRTFRowProperties.Assign(Source: TRVRTFRowProperties);
var side: TRVRTFSide;
begin
  FGapHTw    := Source.FGapHTw;
  FLeftTw    := Source.FLeftTw;
  FHeightTw  := Source.FHeightTw;
  FBestWidth := Source.FBestWidth;
  FCurBorderSide := Source.FCurBorderSide;
  for side := Low(TRVRTFSide) to High(TRVRTFSide) do begin
    FPaddingTw[side] := Source.FPaddingTw[side];
    FSpacingTw[side] := Source.FSpacingTw[side];
    FUsePadding[side] := Source.FUsePadding[side];
    FUseSpacing[side] := Source.FUseSpacing[side];
  end;
  NewCellProps    := Source.NewCellProps;
  AssumedLastCell := Source.AssumedLastCell;
  FBorder.Assign(Source.FBorder);
  FCellProps.AssignItems(Source.FCellProps);
  {$IFDEF RICHVIEW}
  RichViewSpecial := Source.RichViewSpecial;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRVRTFRowProperties.Finalize;
var i: Integer;
begin
  if AssumedLastCell then begin
    CellProps.Delete(CellProps.Count-1);
    AssumedLastCell := False;
  end;
  if NewCellProps then begin
    for i := 0 to CellProps.Count-1 do
      CellProps[i].Finalize;
    NewCellProps := False;      
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFRowProperties.GetPaddingTw(Index: TRVRTFSide): Integer;
begin
  Result := FPaddingTW[Index];
end;
{------------------------------------------------------------------------------}
function TRVRTFRowProperties.GetSpacingTw(Index: TRVRTFSide): Integer;
begin
  Result := FSpacingTW[Index];
end;
{------------------------------------------------------------------------------}
function TRVRTFRowProperties.GetUsePadding(Index: TRVRTFSide): Boolean;
begin
  Result := FUsePadding[Index];
end;
{------------------------------------------------------------------------------}
function TRVRTFRowProperties.GetUseSpacing(Index: TRVRTFSide): Boolean;
begin
  Result := FUseSpacing[Index];
end;
{------------------------------------------------------------------------------}
function TRVRTFRowProperties.GetLastCellProp: TRVRTFCellProperties;
begin
  //AssumedLastCell := False;
  Result := CellProps[CellProps.Count-1];
end;
{============================ TRVRTFCellProperties ============================}
constructor TRVRTFCellProperties.Create;
begin
  inherited Create;
  FBorder := TRVRTFParaBorder.Create;
  Reset;
end;
{------------------------------------------------------------------------------}
destructor TRVRTFCellProperties.Destroy;
begin
  FBorder.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFCellProperties.Reset;
begin
  FHMerge := rtf_cm_None;
  FVMerge := rtf_cm_None;
  FBestWidth := 0;
  FColor  := clNone;
  FVAlign := rtf_val_Top;
  FCurBorderSide := rtf_side_Left;
  FRightBoundaryTw := 0;
  ForeColor := clBlack;
  Shading   := 0;
  FBorder.Reset;
  {$IFDEF RICHVIEW}
  BestHeight := 0;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRVRTFCellProperties.Assign(Source: TRVRTFCellProperties);
begin
  FHMerge := Source.FHMerge;
  FVMerge := Source.FVMerge;
  FBestWidth := Source.FBestWidth;
  FColor  := Source.FColor;
  FVAlign := Source.FVAlign;
  FCurBorderSide := Source.FCurBorderSide;
  FRightBoundaryTw := Source.FRightBoundaryTw;
  ForeColor := Source.ForeColor;
  Shading   := Source.Shading;
  FBorder.Assign(Source.FBorder);
  {$IFDEF RICHVIEW}
  BestHeight := Source.BestHeight;
  {$ENDIF}
end;
{------------------------------------------------------------------------------}
procedure TRVRTFCellProperties.Finalize;
begin
  if (Shading>0) then begin
    if (FColor=clNone) then
      FColor := clWhite;
    FColor := ShadeColor(ColorToRGB(Color), ColorToRGB(ForeColor), Shading);
    Shading := 0;
  end;
end;
{=========================== TRVRTFCellPropsList ==============================}
procedure TRVRTFCellPropsList.AddNew;
var item: TRVRTFCellProperties;
begin
  item := TRVRTFCellProperties.Create;
  Add(item);
end;
{------------------------------------------------------------------------------}
procedure TRVRTFCellPropsList.AssignItems(Source: TRVRTFCellPropsList);
var i: Integer;
    item: TRVRTFCellProperties;
begin
  Clear;
  Capacity := Source.Count;
  for i := 0 to Source.Count-1 do begin
    item := TRVRTFCellProperties.Create;
    item.Assign(Source[i]);
    Add(item);
  end;
end;
{------------------------------------------------------------------------------}
function TRVRTFCellPropsList.Get(Index: Integer): TRVRTFCellProperties;
begin
  Result := TRVRTFCellProperties(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
procedure TRVRTFCellPropsList.Put(Index: Integer;
  const Value: TRVRTFCellProperties);
begin
  inherited Put(Index, Value);
end;
{============================ TRVRTFCustomMarkerProperties ====================}
constructor TRVRTFCustomMarkerProperties.Create;
begin
  inherited Create;
  Reset;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFCustomMarkerProperties.Assign(Source: TRVRTFCustomMarkerProperties; FromDefaults: Boolean);
  procedure UpdateStyle(Fixed: TRVRTFMarkerProp; Style: TFontStyle);
  begin
    if not (Fixed in Source.FFixedProperties) then
      if Style in Source.FontStyle then
        Include(FFontStyle, Style)
      else
        Exclude(FFontStyle, Style);
  end;
begin
  FListType    := Source.FListType;
  FAlignment   := Source.FAlignment;
  FIndentTw    := Source.FIndentTw;
  FSpaceTw     := Source.FSpaceTw;
  FStart       := Source.FStart;
  FFixedProperties := Source.FFixedProperties;
  if not FromDefaults then begin
    FFontIndex   := Source.FFontIndex;
    FFontStyle   := Source.FFontStyle;
    FColor       := Source.FColor;
    FFontSize    := Source.FFontSize;
    end
  else begin
    if not (rtfmp_Color in FFixedProperties) then
      FColor := Source.Color;
    if not (rtfmp_FontIndex in FFixedProperties) then
      FFontIndex := Source.FontIndex;
    if not (rtfmp_Size in FFixedProperties) then
      FFontSize := Source.FontSize;
    UpdateStyle(rtfmp_Bold, fsBold);
    UpdateStyle(rtfmp_Italic, fsItalic);
    UpdateStyle(rtfmp_Underline, fsUnderline);
    UpdateStyle(rtfmp_StrikeOut, fsStrikeOut);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFCustomMarkerProperties.Reset;
begin
  FListType   := rtf_pn_Default;
  FFontIndex  := -1;
  FFontStyle  := [];
  FColor      := clWindowText;
  FFontSize   := 12;
  FAlignment  := rtf_al_Left;
  FIndentTw   := 0;
  FSpaceTw    := 0;
  FStart      := 0;
  FFixedProperties := [];
end;
{------------------------------------------------------------------------------}
procedure TRVRTFCustomMarkerProperties.UpdateFrom(
  CharProps: TRVRTFCharProperties);
  procedure UpdateStyle(Fixed: TRVRTFMarkerProp; Style: TFontStyle);
  begin
    if not (Fixed in FFixedProperties) then
      if Style in CharProps.Style then
        Include(FFontStyle, Style)
      else
        Exclude(FFontStyle, Style);
  end;
begin
  if not (rtfmp_Color in FFixedProperties) then
    FColor := CharProps.Color;
  if not (rtfmp_FontIndex in FFixedProperties) then
    FFontIndex := CharProps.FontIndex;
  if not (rtfmp_Size in FFixedProperties) then
    FFontSize := CharProps.Size;
  UpdateStyle(rtfmp_Bold, fsBold);
  UpdateStyle(rtfmp_Italic, fsItalic);
  UpdateStyle(rtfmp_StrikeOut, fsStrikeOut);
end;
{------------------------------------------------------------------------------}
procedure TRVRTFCustomMarkerProperties.ChangeFontStyle(fs: TFontStyle;
  Val: Integer);
begin
  if Val=0 then
    Exclude(FFontStyle, fs)
  else
    Include(FFontStyle, fs);
end;
{=========================== TRVRTFMarkerProperties ===========================}
procedure TRVRTFMarkerProperties.Assign(Source: TRVRTFMarkerProperties; FromDefaults: Boolean);
begin
  inherited Assign(Source, FromDefaults);
  FTextAfter   := Source.FTextAfter;
  FTextBefore  := Source.FTextBefore;
  FHanging     := Source.FHanging;
  if not FromDefaults then
    FLevel  := Source.FLevel;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFMarkerProperties.Reset;
begin
  inherited Reset;
  FLevel      := 0;
  FTextAfter  := '';
  FTextBefore := '';
  FHanging    := False;
end;
{============================== TRVRTFListLevel97 =============================}
procedure TRVRTFListLevel97.Assign(Source: TRVRTFListLevel97);
begin
  inherited Assign(Source, False);
  FOldStyle := Source.FOldStyle;
  FLegal    := Source.FLegal;
  FNoRestart:= Source.FNoRestart;
  FText     := Source.FText;
  FNumbers  := Source.FNumbers;
  FFollow   := Source.FFollow;
end;
{------------------------------------------------------------------------------}
procedure TRVRTFListLevel97.Reset;
begin
  inherited;
  FOldStyle := False;
  FLegal    := False;
  FNoRestart:= False;
  FText     := '';
  FNumbers  := '';
  FFollow   := rtf_lf_Tab;
  FFontSizeDefined := False;
end;
{============================== TRVRTFList97 ==================================}
procedure TRVRTFList97.AddNew;
begin
  Add(TRVRTFListLevel97.Create);
end;
{------------------------------------------------------------------------------}
function TRVRTFList97.Get(Index: Integer): TRVRTFListLevel97;
begin
  Result := TRVRTFListLevel97(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
function TRVRTFList97.GetLastLevel: TRVRTFListLevel97;
begin
  Result := Items[Count-1];
end;
{------------------------------------------------------------------------------}
procedure TRVRTFList97.Put(Index: Integer; const Value: TRVRTFListLevel97);
begin
  inherited Put(Index, Value);
end;
{============================== TRVRTFListTable97 =============================}
procedure TRVRTFListTable97.AddNew;
begin
  Add(TRVRTFList97.Create);
end;
{------------------------------------------------------------------------------}
function TRVRTFListTable97.FindList(ID: Integer): Integer;
var i: Integer;
begin
  for i := 0 to Count-1 do
    if Items[i].Id = ID then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;
{------------------------------------------------------------------------------}
function TRVRTFListTable97.Get(Index: Integer): TRVRTFList97;
begin
  Result := TRVRTFList97(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
function TRVRTFListTable97.GetLastList: TRVRTFList97;
begin
  Result := Items[Count-1];
end;
{------------------------------------------------------------------------------}
procedure TRVRTFListTable97.Put(Index: Integer; const Value: TRVRTFList97);
begin
  inherited Put(Index, Value);
end;
{============================ TRVRTFListOverrideLevel =========================}
constructor TRVRTFListOverrideLevel97.Create;
begin
  inherited Create;
  FStart := 1;
end;
{============================= TRVRTFListOverride97 ===========================}
procedure TRVRTFListOverride97.AddNew;
begin
  Add(TRVRTFListOverrideLevel97.Create);
end;
{------------------------------------------------------------------------------}
function TRVRTFListOverride97.GetLastLevel: TRVRTFListOverrideLevel97;
begin
  Result := Items[Count-1];
end;
{------------------------------------------------------------------------------}
function TRVRTFListOverride97.Get(Index: Integer): TRVRTFListOverrideLevel97;
begin
  Result := TRVRTFListOverrideLevel97(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
procedure TRVRTFListOverride97.Put(Index: Integer;
  const Value: TRVRTFListOverrideLevel97);
begin
  inherited Put(Index, Value);
end;
{=========================== TRVRTFListOverrideTable97 ========================}
procedure TRVRTFListOverrideTable97.AddNew;
begin
  Add(TRVRTFListOverride97.Create);
end;
{------------------------------------------------------------------------------}
function TRVRTFListOverrideTable97.FindListOverride(Number: Integer): Integer;
var i: Integer;
begin
  for i := 0 to Count-1 do
    if Items[i].Number = Number then begin
      Result := i;
      exit;
    end;
  Result := -1;
end;
{------------------------------------------------------------------------------}
function TRVRTFListOverrideTable97.Get(
  Index: Integer): TRVRTFListOverride97;
begin
  Result := TRVRTFListOverride97(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
function TRVRTFListOverrideTable97.GetLastListOverride: TRVRTFListOverride97;
begin
  Result := Items[Count-1];
end;
{------------------------------------------------------------------------------}
procedure TRVRTFListOverrideTable97.Put(Index: Integer;
  const Value: TRVRTFListOverride97);
begin
  inherited Put(Index, Value);
end;
{================================== TRVRTFFont ================================}
constructor TRVRTFFont.Create;
begin
  inherited;
  {$IFDEF RICHVIEWCBDEF3}
  Charset := DEFAULT_CHARSET;
  {$ENDIF}
end;
{==============================================================================}
procedure QSort(L, R: Integer);
var
  I, J: Integer;
  P: TRVRTFsymbol;
  T: TRVRTFsymbol;
begin
  repeat
    I := L;
    J := R;
    P := rgsymRtf[(L + R) shr 1];
    repeat
      while rgsymRtf[I].Keyword < P.Keyword do
        inc(I);
      while rgsymRtf[J].Keyword > P.Keyword do
        dec(J);
      if I <= J then begin
        T := rgsymRtf[I];
        rgsymRtf[I] := rgsymRtf[J];
        rgsymRtf[J] := T;
        inc(I);
        dec(J);
      end;
    until I > J;
    if L < J then
      QSort(L, J);
    L := I;
  until I >= R;
end;

const KWSorted: Boolean = False;
procedure SortKeywords;
begin
  if KWSorted then
    exit;
  KWSorted := True;
  QSort(0,isymMax);
end;

{$IFNDEF RICHVIEW}
{------------------------------------------------------------------------------}
function RV_CreateGraphicsDefault(GraphicClass: TGraphicClass): TGraphic;
begin
  Result := GraphicClass.Create;
end;
{------------------------------------------------------------------------------}
procedure RV_AfterImportGraphicDefault(Graphic: TGraphic);
begin

end;

initialization
  RV_CreateGraphics := RV_CreateGraphicsDefault;
  RV_AfterImportGraphic := RV_AfterImportGraphicDefault;
{$ENDIF}



end.
