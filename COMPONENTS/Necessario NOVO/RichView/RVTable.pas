{*******************************************************}
{                                                       }
{       RichView                                        }
{       TRVTableItemInfo: RichView item type            }
{       representing tables. Related types.             }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}


unit RVTable;
interface

{$I RV_Defs.inc}

uses Windows, SysUtils, Classes, Graphics, Controls, Forms,
     RVClasses, RVItem, RVDataList, CRVData, CRVFData, RVStyle, RVFuncs,
     RVRVData, RVERVData,
     DLines, RVBack,
     {$IFNDEF RVDONOTUSELISTS}
     RVMarker,
     {$ENDIF}
     RVScroll,RichView, RVEdit, RVFMisc, PtblRV, PtRVData, TypInfo, RVUni, RVUndo,
     RVStr;

{.$DEFINE RVDEBUGTABLE}

{$IFNDEF RICHVIEWCBDEF3}
{$O-}
{$ENDIF}


const
  rvsTable = -60;

  {$R RVTable.res}
  crRVSelectRow = 104;
  crRVSelectCol = 105;

type
  TRVCellHAlign   = (rvcLeft, rvcCenter, rvcRight);
  TRVCellVAlign   = (rvcTop,  rvcMiddle, rvcBottom, rvcVDefault);

  TRVTableOption = (rvtoEditing, rvtoRowSizing, rvtoColSizing,
                    rvtoRowSelect, rvtoColSelect, rvtoRTFSaveCellPixelBestWidth,
                    rvtoRTFAllowAutofit,
                    rvtoHideGridLines, rvtoOverlappingCorners, rvtoCellBelowBorders);
  TRVTableOptions = set of TRVTableOption;

  TRVTablePrintOption = (rvtoHalftoneBorders, rvtoRowsSplit, rvtoWhiteBackground);
  TRVTablePrintOptions = set of TRVTablePrintOption;

const
  RVTABLEDEFAULTOPTIONS = [rvtoEditing, rvtoRowSizing, rvtoColSizing,
                           rvtoRowSelect, rvtoColSelect];
  RVTABLEDEFAULTPRINTOPTIONS = [rvtoHalftoneBorders,rvtoRowsSplit];
type

  TRVTableBorderStyle = (rvtbRaised, rvtbLowered, rvtbColor, rvtbRaisedColor, rvtbLoweredColor);


  TRVHTMLLength = type Integer; // > 0 - pixels
                                // = 0 - undefined
                                // < 0 - percents

  TRVTableRow = class;
  TRVTableRows = class;
  TRVTableItemInfo = class;

  TRVCellEditingEvent = procedure (Sender: TRVTableItemInfo;
    Row, Col : Integer; Automatic: Boolean; var AllowEdit: Boolean) of object;
  TRVCellEndEditEvent = procedure (Sender: TRVTableItemInfo; Row, Col: Integer;
    Clearing: Boolean)
    of object;

  TRVTableCellData = class (TRVItemFormattedData)
    private
      FColor: TColor;
      FVAlign: TRVCellVAlign;
      FBestWidth: TRVHTMLLength;
      FBestHeight: Integer;
      FRowSpan: Integer;
      FColSpan: Integer;
      FLeft, FTop, FWidth, FHeight: Integer;
      FVisibleBorders: TRVBooleanRect;
      //FReservedRVData: TCustomRVData;
      FChosenRVData: TCustomRVFormattedData;
      FChosenItem: TCustomRVItemInfo;
      ContainerUndoItem: TRVUndoInfo;
      FBorderColor: TColor;
      FBorderLightColor: TColor;
      FBackground: TRVBackground;
      FBackgroundImageFileName: String;
      procedure SetBestHeight(const Value: Integer);
      procedure SetBestWidth(const Value: TRVHTMLLength);
      procedure SetVisibleBorders(const Value: TRVBooleanRect);
      function CanClear: Boolean;
      procedure SetColor(const Value: TColor);
      function GetBackgroundImage: TGraphic;
      function GetBackgroundStyle: TRVItemBackgroundStyle;
      procedure SetBackgroundImage_(const Value: TGraphic; Copy: Boolean);
      procedure SetBackgroundImage(const Value: TGraphic);
      procedure SetBackgroundStyle(const Value: TRVItemBackgroundStyle);
      procedure BackgroundImageWriter(Stream: TStream);
      procedure BackgroundImageReader(Stream: TStream);
    protected
      procedure DefineProperties(Filer: TFiler); override;
      procedure AssignSizeFrom(Cell:TRVTableCellData);
      function GetRealVAlign: TRVCellVAlign;
      function SupportsPageBreaks: Boolean; override;
      function GetOptions: TRVOptions; override;
      procedure ResetSubCoords; override;
    public
      { methods for internal processing }
      function GetBackground: TRVBackground; override;
      function GetExtraVOffs: Integer;
      function IsTransparent: Boolean;
      procedure GetParentInfo(var ParentItemNo: Integer; var Location: TRVStoreSubRVData); override;
      function GetTable: TRVTableItemInfo;      
      procedure AssignAttributesFrom(Cell:TRVTableCellData; IncludeSize: Boolean;
                                     DivColSpan, DivRowSpan: Integer);      
      procedure Deselect(NewPartiallySelected: TCustomRVItemInfo; MakeEvent: Boolean); override;
      function GetEditor: TWinControl; override;
      procedure GetOrigin(var ALeft, ATop: Integer); override;
      procedure GetOriginEx(var ALeft, ATop: Integer); override;
      function GetWidth: Integer; override;
      function GetHeight: Integer; override;
      function GetColor: TColor; override;
      function GetHOffs: Integer; override;
      function GetVOffs: Integer; override;
      function GetAreaWidth: Integer; override;
      procedure AssignChosenRVData(RVData: TCustomRVFormattedData; Item: TCustomRVItemInfo); override;
      procedure UnassignChosenRVData(RVData: TCustomRVData); override;
      function GetChosenRVData: TCustomRVData; override;
      function GetChosenItem: TCustomRVItemInfo; override;
      procedure MovingToUndoList(AContainerUndoItem: TRVUndoInfo);
      procedure MovingFromUndoList;
      function GetCellHeight: Integer;
//      function GetWidthInFixedTable(TableWidth: Integer): Integer;
      function GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas): Integer;
      function HasData(CheckStyles: Boolean): Boolean;
      function GetRVData: TCustomRVData; override;
      procedure DoSelect; override;
      procedure ControlAction2(ControlAction: TRVControlAction; ItemNo: Integer; var Control: TControl); override;
      procedure ItemAction(ItemAction: TRVItemAction; Item: TCustomRVItemInfo;
                               var Text: String; RVData: TCustomRVData); override;
      procedure AdjustFocus(NewFocusedItemNo: Integer; TopLevelRVData: TPersistent; TopLevelItemNo: Integer); override;
      { public methods and properties }
      constructor Create(ARow: TRVTableRow);
      destructor Destroy; override;
      {$IFNDEF RVDONOTUSELISTS}
      function GetMarkers(AllowCreate: Boolean): TRVMarkerList; override;
      {$ENDIF}      

      function Edit: TCustomRVData; override;
      property Left:Integer read FLeft;
      property Top:Integer  read FTop;
      property Height:Integer read FHeight;
      property Width:Integer read FWidth;

      property ColSpan: Integer read FColSpan;
      property RowSpan: Integer read FRowSpan;

    published
      { published properties }
      property Color: TColor                  read FColor          write SetColor      default clNone;
      property BorderColor: TColor            read FBorderColor    write FBorderColor  default clNone;
      property BorderLightColor: TColor       read FBorderLightColor write FBorderLightColor default clNone;
      property BestWidth: TRVHTMLLength       read FBestWidth      write SetBestWidth  default 0;
      property BestHeight: Integer            read FBestHeight     write SetBestHeight default 0;
      property VisibleBorders: TRVBooleanRect read FVisibleBorders write SetVisibleBorders;
      property VAlign: TRVCellVAlign          read FVAlign         write FVAlign       default rvcVDefault;
      property BackgroundImage: TGraphic read GetBackgroundImage write SetBackgroundImage stored False;
      property BackgroundStyle: TRVItemBackgroundStyle read GetBackgroundStyle write SetBackgroundStyle default rvbsColor;
      property BackgroundImageFileName: String read FBackgroundImageFileName
        write FBackgroundImageFileName;
  end;

  TRVTableRow = class (TRVDataList)
    private
      FVAlign: TRVCellVAlign;
      function Get(Index: Integer): TRVTableCellData;
      procedure Put(Index: Integer; const Value: TRVTableCellData);
      procedure InsertEmpty(Index: Integer);
      procedure InsertPointer(Index: Integer; Item: TRVTableCellData);
    protected
      FRows: TRVTableRows;
      function GetParentRVData: TCustomRVData; override;
      function HasCellsInRange(Index, RangeStart, Count: Integer): Boolean;
    public
      constructor Create(nCols: Integer; ARows: TRVTableRows; MainRVData: TCustomRVData);
      function Add: TRVTableCellData;
      function Insert(Index: Integer): TRVTableCellData;
      function GetHeight: Integer;
      function GetBestHeight: Integer;
      property VAlign: TRVCellVAlign read FVAlign write FVAlign default rvcTop;
      property Items[Index: Integer]: TRVTableCellData read Get write Put; default;
  end;

  TRVTableRows = class (TRVList)
    private
      function Get(Index: Integer): TRVTableRow;
      procedure Put(Index: Integer; const Value: TRVTableRow);
      function GetMinColWidth(Col: Integer; sad: PRVScreenAndDevice;
                              Canvas: TCanvas): Integer;
      function GetPercentColWidth(Col, TableWidth: Integer): Integer;
      function IsPercentWidthColumn(Col: Integer): Boolean;
      function GetPixelColWidth(Col: Integer): Integer;
      function StartMergeCells(TopRow, LeftCol:Integer;
                               var ColSpan, RowSpan: Integer): Boolean;
    protected
      FMainRVData: TCustomRVData;
      FTable     : TRVTableItemInfo;
      function GetColCount: Integer;
      function IsEmptyRows(TopRow, LeftCol, ColSpan, RowSpan, TopRow2, RowSpan2: Integer): Boolean;
      function IsEmptyCols(TopRow, LeftCol, ColSpan, RowSpan, LeftCol2, ColSpan2: Integer): Boolean;
      function GetBestWidth(TopRow, LeftCol, ColSpan, RowSpan: Integer): Integer;
      procedure UnmergeCell(Row,Col: Integer; UnmergeRows, UnmergeCols: Boolean);
      procedure UnmergeCells(TopRow, LeftCol, ColSpan, RowSpan: Integer; UnmergeRows, UnmergeCols: Boolean);
      procedure InsertRows(Index, Count, CopyIndex: Integer; DivideHeights: Boolean);
      procedure InsertCols(Index, Count, CopyIndex: Integer; DivideWidths: Boolean);
      procedure DeleteRows(Index, Count: Integer; DecreaseHeight: Boolean);
      procedure DeleteCols(Index, Count: Integer; DecreaseWidth: Boolean);
      function SplitCellVertically(Row, Col, ColCount: Integer): Integer;
      function SplitCellsVertically(TopRow, LeftCol, ColSpan, RowSpan, ColCount: Integer):Integer;
      function SplitCellHorizontally(Row, Col, RowCount: Integer): Integer;
      function SplitCellsHorizontally(TopRow, LeftCol, ColSpan, RowSpan, RowCount: Integer):Integer;
      procedure MovingToUndoList(Row, Col, ColSpan, RowSpan: Integer; AContainerUndoItem: TRVUndoInfo);
      procedure MovingFromUndoList(Row, Col, ColSpan, RowSpan: Integer);
      procedure Do_BeforeInsertRows(ItemNo,Row,Count: Integer);
      procedure Do_InsertRows(Row,Count: Integer);
      procedure Do_UnInsertRows(Row,Count: Integer);
      procedure Do_BeforeInsertCell(ItemNo,Row,Col: Integer);
      procedure Do_BeforeSpreadOverEmptyCells(ItemNo, Row, Col, ColSpan: Integer);
      procedure Do_SpreadOverEmptyCells(Row, Col, ColSpan: Integer);
      procedure Do_UnSpreadOverEmptyCells(Row, Col, ColSpan: Integer);
      procedure Do_SetSpan(ItemNo,Row, Col, Span: Integer; IsColSpan: Boolean);
      procedure Do_BeforeFreeEmptyCells(ItemNo,Row, Col, ColSpan, RowSpan: Integer);
      procedure Do_FreeEmptyCells(Row, Col, ColSpan, RowSpan: Integer);
      procedure Do_UnFreeEmptyCells(Row, Col, ColSpan, RowSpan: Integer);
      procedure Do_BeforeInsertEmptyCells(ItemNo,Row, Col, ColCount, RowCount: Integer);
      procedure Do_InsertEmptyCells(Row, Col, ColCount, RowCount: Integer);
      procedure Do_UnInsertEmptyCells(Row, Col, ColCount, RowCount: Integer);
      procedure Do_BeforeSplitCellHorz(ItemNo,Row,Col,Row2: Integer; DecreaseHeight: Boolean);
      procedure Do_SplitCellHorz(Row,Col,Row2: Integer; DecreaseHeight: Boolean);
      procedure Do_UnSplitCellHorz(Row,Col,Row2: Integer; OldBestHeight: Integer);
      procedure Do_BeforeSplitCellVert(ItemNo,Row,Col,Col2: Integer; DecreaseWidth: Boolean);
      procedure Do_SplitCellVert(Row,Col,Col2: Integer; DecreaseWidth: Boolean);
      procedure Do_UnSplitCellVert(Row,Col,Col2: Integer; OldBestWidth: Integer);
      function Do_BeforeDeleteRows(ItemNo,Row,Count: Integer): TRVUndoInfo;
      procedure Do_DeleteRows(ItemNo,Row,Count: Integer; ui: TRVUndoInfo);
      procedure Do_UnDeleteRows(Row: Integer; RowList: TList);
      function Do_BeforeDeleteCols(ItemNo,Col,Count: Integer): TRVUndoInfo;
      procedure Do_DeleteCols(ItemNo,Col,Count: Integer;ui: TRVUndoInfo);
      procedure Do_UnDeleteCols(Col: Integer; CellList: TList);
      function Do_BeforeMergeCells(ItemNo, Row,Col, ColSpan, RowSpan: Integer): TRVUndoInfo;
      procedure Do_MergeCells(ItemNo, Row,Col, ColSpan, RowSpan: Integer;ui: TRVUndoInfo; ChangeBestWidth: Boolean);
      procedure Do_UndoMergeCells(ItemNo, Row,Col, OldColSpan, OldRowSpan: Integer;
                                  MergedItemsList: TRVList; OldBestWidth: TRVHTMLLength);
      procedure Do_BeforeUnmergeCell(ItemNo, Row,Col: Integer; UnmergeRows, UnmergeCols: Boolean);
      procedure Do_UnmergeCell(ItemNo, Row,Col: Integer; UnmergeRows, UnmergeCols: Boolean);
      procedure Do_UndoUnmergeCell(ItemNo, Row,Col: Integer;
                                   OldColSpan, OldRowSpan: Integer;
                                   OldBestWidth: TRVHTMLLength;
                                   OldBestHeight: Integer);
      function Do_BeforeClearCells(ItemNo: Integer; RowList, ColList: TRVIntegerList;
                                    var CellsList: TList): TRVUndoInfo;
      procedure Do_ClearCells(CellsList: TList; RowList, ColList: TRVIntegerList;
                              ui: TRVUndoInfo);
      procedure Do_UnClearCells(CellsList: TList; RowList, ColList: TRVIntegerList);
      procedure InsertPointer(Index: Integer; Item: TRVTableRow);
    public
      constructor Create(nRows, nCols: Integer; AMainRVData: TCustomRVData;
                         ATable: TRVTableItemInfo);
      destructor Destroy; override;
      procedure MergeCells(TopRow, LeftCol, ColSpan, RowSpan: Integer;
                           AllowMergeRC, ChangeBestWidth: Boolean);      
      function Empty: Boolean;
      function Add(nCols: Integer): TRVTableRow;
      function GetMainCell(ARow,ACol: Integer; var MRow, MCol: Integer): TRVTableCellData;
      function Insert(Index, nCols: Integer): TRVTableRow;
      procedure Reset(nRows, nCols: Integer);
      function CanMergeCells(TopRow, LeftCol, ColSpan, RowSpan: Integer;
                             AllowMergeRC: Boolean): Boolean;
      property Items[Index: Integer]: TRVTableRow read Get write Put; default;
  end;

  TRVTableInplaceParamStorage = record
    Stored: Boolean;
    StartNo, EndNo, StartOffs, EndOffs: Integer;
    Row, Col: Integer;
    PartialSelected: TCustomRVItemInfo;
  end;

  TRVTableItemFormattingInfo = class
    private
      ColWidths, RowHeights, ColStarts,RowStarts: TRVIntegerList;
      FWidth, FHeight: Integer;
      Rows : TRVList;
    public
      constructor Create(CreateRows:Boolean);
      destructor Destroy; override;
      procedure Clear;
      procedure QuickClear;
  end;

  TRVCellDirection = (rvcdLeft, rvcdUp, rvcdRight, rvcdDown,
                      rvcdDocTop, rvcdDocBottom,
                      rvcdNext, rvcdPrev);

  TRVTableDrawBorderEvent = procedure (Sender: TRVTableItemInfo;
    Canvas: TCanvas; Left,Top,Right,Bottom, Width: Integer;
    LightColor, Color, BackgroundColor: TColor;
    Style: TRVTableBorderStyle; Printing: Boolean;
    VisibleBorders: TRVBooleanRect; Row, Col: Integer;
    var DoDefault: Boolean) of object;

  TRVTableState = (rvtsInserted, rvtsEditMode, rvtsModified, rvtsFormatInplace,
     rvtsVerticalDraggedRule, rvtsDRChangeTableWidth,
     rvtsJustCreated, rvtsInplaceIsReformatting, rvtsSelExists);
  TRVTableStates = set of TRVTableState;


  TRVTableItemInfo = class (TRVFullLineItemInfo)
    private
      FPrintCell: TRVTableCellData;
      FPrintCellRect: TRect;
      FOnCellEditing: TRVCellEditingEvent;
      FOnCellEndEdit: TRVCellEndEditEvent;
      FState: TRVTableStates;
      CachedItemNo: Integer;
      MyTop, MyLeft: Integer;
      MyClientTop, MyClientLeft: Integer;
      MouseRow, MouseCol: Integer;
      DRMin, DRMax, DRNo, DRDelta: Integer;
      FRows: TRVTableRows;
      FCellVSpacing,FCellHSpacing: Integer;
      FCellPadding: Integer;
      FBestWidth: TRVHTMLLength;
      FBorderWidth: Integer;
      FBorderColor: TColor;
      FColor: TColor;
      FCellBorderWidth: Integer;
      FHRuleWidth: Integer;
      FVRuleWidth: Integer;
      FHRuleColor: TColor;
      FCellBorderColor: TColor;
      FVRuleColor: TColor;
      FBorderStyle: TRVTableBorderStyle;
      FCellBorderStyle: TRVTableBorderStyle;
      FBorderHSpacing: Integer;
      FBorderVSpacing: Integer;
      FHOutermostRule: Boolean;
      FVOutermostRule: Boolean;
      FCellBorderLightColor: TColor;
      FBorderLightColor: TColor;
      FSelStartCol, FSelStartRow, FSelColOffs, FSelRowOffs: Integer;
      BusyCount: Integer;
      FOptions: TRVTableOptions;
      FPrintOptions: TRVTablePrintOptions;
      FStoredInplace: TRVTableInplaceParamStorage;
      FMinWidthPlus, FInplaceMinWidthPlus: Integer;
      FTextRowSeparator: String;
      FTextColSeparator: String;
      FocusedCellRow,FocusedCellCol: Integer;
      ChosenCellRow,ChosenCellCol: Integer;
      StreamSaveStartRow, StreamSaveRowCount: Integer;
      StreamSaveHeadingRows: Boolean;
      FOnDrawBorder: TRVTableDrawBorderEvent;
      FHeadingRowCount: Integer;
      FBackground: TRVBackground;
      FBackgroundImageFileName: String;
      function GetItemNoInRootDocument: Integer;
      function GetCells(Row, Col: Integer): TRVTableCellData;
      procedure SetCells(Row, Col: Integer; const Value: TRVTableCellData);
      procedure SetBestWidth(const Value: TRVHTMLLength);
      function GetVerticalRuleNo(X: Integer; var MinX, ZeroChangeX: Integer): Integer;
      function GetHorizontalRuleNo(Y: Integer; var MinY, ZeroChangeY: Integer): Integer;
      function GetColNo(X: Integer): Integer;
      function GetRowNo(Y: Integer): Integer;
      function GetCrossed(Coord: Integer;List: TRVIntegerList): Integer;
      procedure UpdateCellXCoords(Fmt: TRVTableItemFormattingInfo; NoCaching: Boolean);
      procedure UpdateCellYCoords(Fmt: TRVTableItemFormattingInfo);
      function GetHorzExtra: Integer;
      procedure InplaceEditorChange(Sender: TCustomRichViewEdit; ClearRedo: Boolean);
      procedure InplaceEditorCaretGetout(Sender: TCustomRichViewEdit; Direction: TRVGetOutDirection);
      procedure InplaceEditorMouseDown(Sender: TCustomRichView; Button: TMouseButton; Shift: TShiftState; ItemNo, X, Y: Integer);
      procedure InplaceEditorMouseUp(Sender: TCustomRichView; Button: TMouseButton; Shift: TShiftState; ItemNo, X, Y: Integer);
      procedure InplaceEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure InplaceEditorDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
      procedure InplaceEditorDragDrop(Sender, Source: TObject; X, Y: Integer);
      procedure InplaceEditorControlAction(Sender: TCustomRichView; ControlAction: TRVControlAction; ItemNo: Integer;
                                     var ctrl: TControl);
      procedure DestroyInplace(ReformatCell:Boolean);
      procedure StoreRVSelection(RVData: TCustomRVFormattedData;
                             var storage: TRVTableInplaceParamStorage);
      procedure RestoreRVSelection(RVData: TCustomRVFormattedData;
                             const storage: TRVTableInplaceParamStorage);
      procedure Init(nRows, nCols: Integer; AMainRVData: TCustomRVData);
      procedure CellsWriter(Writer: TWriter);
      procedure CellsReader(Reader: TReader);
      procedure ClearTemporal;
      procedure SetBorderColor(const Value: TColor);
      procedure SetBorderHSpacing(const Value: Integer);
      procedure SetBorderLightColor(const Value: TColor);
      procedure SetBorderStyle(const Value: TRVTableBorderStyle);
      procedure SetBorderVSpacing(const Value: Integer);
      procedure SetBorderWidth(const Value: Integer);
      procedure SetCellBorderColorProp(const Value: TColor);
      procedure SetCellBorderLightColorProp(const Value: TColor);
      procedure SetCellBorderWidth(const Value: Integer);
      procedure SetCellHSpacing(const Value: Integer);
      procedure SetCellPadding(const Value: Integer);
      procedure SetCellVSpacing(const Value: Integer);
      procedure SetColor(const Value: TColor);
      procedure SetHOutermostRule(const Value: Boolean);
      procedure SetHRuleColor(const Value: TColor);
      procedure SetHRuleWidth(const Value: Integer);
      procedure SetVOutermostRule(const Value: Boolean);
      procedure SetVRuleColor(const Value: TColor);
      procedure SetVRuleWidth(const Value: Integer);
      procedure SetCellBorderStyle(const Value: TRVTableBorderStyle);
      procedure SetProperty(const PropertyName: String; Value: LongInt;
                            AffectSize, AffectWidth: Boolean);
      procedure SetCellProperty(ItemNo: Integer; const PropertyName: String; Value: LongInt;
                            Row,Col: Integer;
                            AffectSize, AffectWidth: Boolean);
      function IsFixedWidthTable: Boolean;
      function CompletelySelected: Boolean;
      procedure UnAssignActiveCell;
      function DoOnCellEditing(Row,Col: Integer; Automatic: Boolean): Boolean;
      function IsInEditor: Boolean;
      function CanUseHeadingRowCount: Boolean;
      procedure SetHeadingRowCount(const Value: Integer);
      function GetBackgroundImage: TGraphic;
      procedure SetBackgroundImage_(const Value: TGraphic; Copy: Boolean);
      procedure SetBackgroundImage(const Value: TGraphic);
      function GetBackgroundStyle: TRVItemBackgroundStyle;
      procedure SetBackgroundStyle(const Value: TRVItemBackgroundStyle);
      procedure BackgroundImageWriter(Stream: TStream);
      procedure BackgroundImageReader(Stream: TStream);
    protected
      Fmt: TRVTableItemFormattingInfo;
      cursad: PRVScreenAndDevice;
      procedure XorDrawing(Sender: TObject; Shift: TShiftState; X, Y: Integer);
      procedure DefineProperties(Filer: TFiler); override;
      function GetHeight: Integer; override;
      function GetWidth: Integer; override;

      procedure DrawBorder(Canvas: TCanvas;
        Left,Top,Right,Bottom, Width: Integer; LightColor, Color, BackgroundColor: TColor;
        Style: TRVTableBorderStyle; DrawEvenEmptyBorder, Printing: Boolean;
        const ClipRect: TRect; VisibleBorders: TRVBooleanRect;
        r,c: Integer; ColorMode: TRVColorMode);
      function GetCellAt_(X,Y: Integer; var Row,Col: Integer): Boolean;
      procedure UpdateCellSel;
      procedure PaintTo(Left, Right, Top, FromRow, RowCount: Integer; Canvas: TCanvas;
        State: TRVItemDrawStates; Style: TRVStyle; Fmt: TRVTableItemFormattingInfo;
        UseHeadingRowCount: Boolean; const ClipRect: TRect; ColorMode: TRVColorMode;
        RVData: TCustomPrintableRVData);
      function GetDevX(x: Integer): Integer;
      function GetDevY(y: Integer): Integer;
      procedure InternalOnDocWidthChange(DocWidth: Integer;
        Fmt: TRVTableItemFormattingInfo; Canvas: TCanvas; NoCaching: Boolean);
      procedure Change;
      procedure ChangeEx(ClearRedo: Boolean);
      function BeginModify(ItemNo: Integer): Integer;
      procedure EndModify(ItemNo, Data: Integer);
      function GetTableColor(UseParentBackground: Boolean): TColor;
      function GetCellColor(Cell: TRVTableCellData): TColor;
      function CanSeeBackgroundThroughCell(Cell: TRVTableCellData): Boolean;
      function GetSplitRowBelow(Row: Integer): Integer;
      function GetSplitRowAbove(Row: Integer): Integer;
      function BeforeChange: Boolean;
      function CanChange: Boolean;
      procedure InitUndo;
      procedure DoneUndo;
      procedure AssignCellAttributes(ItemNo,Row,Col: Integer; SourceCell: TRVTableCellData;
                                     IncludeSize: Boolean;
                                     DivColSpan, DivRowSpan: Integer);
      procedure SetCellBestWidth_(ItemNo: Integer; Value: TRVHTMLLength; Row,Col: Integer);
      procedure SetCellBestHeight_(ItemNo: Integer; Value: Integer; Row,Col: Integer);
      procedure SetCellColor_(ItemNo: Integer; Value: TColor; Row,Col: Integer);
      procedure SetCellBackgroundStyle_(ItemNo: Integer; Value: TRVItemBackgroundStyle; Row,Col: Integer);
      procedure SetCellVisibleBorders_(ItemNo: Integer; Left, Top, Right, Bottom: Boolean; Row,Col: Integer);
      procedure SetCellBorderColor_(ItemNo: Integer; Value: TColor; Row,Col: Integer);
      procedure SetCellBorderLightColor_(ItemNo: Integer; Value: TColor; Row,Col: Integer);      
      procedure SetCellVAlign_(ItemNo: Integer; Value: TRVCellVAlign; Row,Col: Integer);
      procedure SetRowVAlign_(ItemNo: Integer; Value: TRVCellVAlign; Row: Integer);
      function GetEditorItemNoForUndo: Integer;
      function CreateTemporalEditor: TCustomRichViewEdit;
      procedure ApplyToCells(Operation: Integer; UserData: Integer;
        SelectedOnly: Boolean);
      procedure ValidateFocused;
      procedure ValidateChosen;
      function CellIsChosen: Boolean;
      procedure AdjustFocus(Row,Col: Integer; TopLevelRVData: TPersistent; TopLevelItemNo: Integer);
      function UndoEnabled: Boolean;
      procedure ChooseSubRVData_(r,c: Integer);
      procedure EditCell_(Row,Col: Integer; Unquestioning: Boolean);
      function GetRVFExtraPropertyCount: Integer; override;
      procedure SaveRVFExtraProperties(Stream: TStream); override;
    public
      FMakingSelection: Boolean;
      FInplaceEditor: TCustomRichViewEdit;
      procedure DrawBackgroundUnderCell(Canvas: TCanvas; Cell: TRVTableCellData;
        const Rect: TRect);
      function IsSemiTransparentBackground: Boolean;
      function GetSoftPageBreakDY(Data: Integer): Integer; override;
      function GetMyItemNo: Integer;
      procedure SaveRowsToStream(Stream: TStream; Index, Count: Integer);
      procedure ResetSubCoords; override;
      function GetSubRVDataAt(X,Y: Integer): TPersistent; override;
      function GetCellWhichOwnsControl(AControl: TControl; var ARow,ACol, AItemNo: Integer): Boolean;
      function AdjustFocusToControl(Control: TControl; var TopLevelRVData: TPersistent; var TopLevelItemNo: Integer):Boolean;override;
      procedure Print(Canvas: TCanvas; x, y, x2: Integer; Preview, Correction: Boolean;
        const sad: TRVScreenAndDevice; RichView: TRVScroller; dli: TRVDrawLineInfo;
        Part: Integer; ColorMode: TRVColorMode; RVData: TPersistent); override;
      procedure CreateInplace(ItemNo, Row, Col: Integer; BuildJumps, CaretAtStart, CaretAtEnd, SetTime, Unquestioning: Boolean);
      procedure SetInplaceBounds(Left, Top, Width, Height: Integer);
      function StartSelecting(Row,Col: Integer): Boolean;
      procedure MovingToUndoList(ItemNo: Integer; RVData, AContainerUndoItem: TObject); override;
      procedure MovingFromUndoList(ItemNo: Integer; RVData: TObject); override;
      procedure FinalizeUndoGroup; override;
      procedure AdjustInserted(x,y: Integer; adjusty: Boolean); override;
      function OwnsControl(AControl: TControl): Boolean; override;
      function OwnsInplaceEditor(AEditor: TControl): Boolean; override;
      function GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas; RVData: TPersistent): Integer; override;
      procedure PaintFullWidth(Left, Right, Top: Integer; Canvas: TCanvas; State: TRVItemDrawStates;
        Style: TRVStyle; const ClipRect: TRect; dli: TRVDrawLineInfo); override;
      procedure OnDocWidthChange(DocWidth: Integer; dli: TRVDrawLineInfo;
        Printing: Boolean; Canvas: TCanvas; RVData: TPersistent; sad: PRVScreenAndDevice;
        var HShift, Desc: Integer; NoCaching: Boolean); override;
      function MouseMove(Shift: TShiftState; X,Y,ItemNo: Integer; RVData: TObject):Boolean; override;
      function MouseDown(Button: TMouseButton; Shift: TShiftState; X,Y,ItemNo: Integer; RVData: TObject):Boolean; override;
      function MouseUp(Button: TMouseButton; Shift: TShiftState; X,Y,ItemNo: Integer; RVData: TObject):Boolean; override;
      procedure DeselectPartial; override;
      procedure MergeInplaceUndo(DestroyLists: Boolean);
      procedure InplaceDeleted(Clearing: Boolean);
      function PartiallySelected: Boolean; override;
      function CanDeletePartiallySelected: Boolean; override;
      procedure DeletePartiallySelected; override;
      function GetBoolValue(Prop: TRVItemBoolProperty): Boolean; override;
      function GetBoolValueEx(Prop: TRVItemBoolPropertyEx; RVStyle: TRVStyle): Boolean; override;
      procedure SaveRVF(Stream: TStream; RVData: TPersistent;
        ItemNo, ParaNo: Integer; const Name: String; Part: TRVMultiDrawItemPart;
        ForceSameAsPrev: Boolean); override;
      function ReadRVFLine(const s: String; RVData: TPersistent;
        ReadType, LineNo, LineCount: Integer; var Name: String;
        var ReadMode: TRVFReadMode;
        var ReadState: TRVFReadState): Boolean; override;
      procedure BeforeLoading(FileFormat: TRVLoadFormat); override;
      {$IFNDEF RVDONOTUSELISTS}
      procedure AfterLoading(FileFormat: TRVLoadFormat); override;
      {$ENDIF}
      function CreatePrintingDrawItem(RVData: TObject;
        const sad: TRVScreenAndDevice): TRVDrawLineInfo; override;
      procedure DrawBackgroundForPrinting(Canvas: TCanvas;
        const Rect, FullRect: TRect; ColorMode: TRVColorMode;
        ItemBackgroundLayer: Integer); override;
      procedure SaveInplace;
      procedure RestoreInplace;
      procedure StartExport; override;
      procedure EndExport; override;
      procedure MarkStylesInUse(UsedTextStyles, UsedParaStyles, UsedListStyles: TRVIntegerList); override;
      procedure UpdateStyles(TextStylesShift, ParaStylesShift, ListStylesShift: TRVIntegerList); override;
      procedure Inserting(RVData: TObject; var Text: String; Safe: Boolean); override;
      {$IFNDEF RVDONOTUSELISTS}
      procedure Inserted(RVData: TObject; ItemNo: Integer); override;
      {$ENDIF}
      procedure BeforeUndoChangeProperty; override;
      procedure AfterUndoChangeProperty; override;
      procedure ApplyParaStyleToSubRVDatas(ParaNo: Integer; SelectedOnly: Boolean); override;
      procedure ApplyParaStyleConversionToSubRVDatas(UserData: Integer; SelectedOnly: Boolean); override;
      procedure ApplyStyleConversionToSubRVDatas(UserData: Integer; SelectedOnly: Boolean); override;
      function GetCellTo(Row,Col: Integer; Dir:TRVCellDirection;
        var NewRow,NewCol: Integer; Quiet: Boolean): Boolean;
      {$IFNDEF RVDONOTUSERTF}
      procedure SaveRTF(Stream: TStream; RVData: TPersistent; ItemNo: Integer;
        const Name: String; TwipsPerPixel: Double; Level: Integer; ColorList: TRVColorList;
        StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
        FontTable: TRVList); override;
      procedure FillRTFTables(ColorList: TRVColorList; ListOverrideCountList: TRVIntegerList;
        RVData: TPersistent); override;
      {$ENDIF}
      function AsText(LineWidth: Integer; RVData: TPersistent;
        const Text, Path: String; TextOnly,Unicode: Boolean): String; override;
      {$IFNDEF RVDONOTUSEHTML}
      procedure SaveToHTML(Stream: TStream; RVData: TPersistent;
        ItemNo: Integer; const Text, Path: String;
        const imgSavePrefix: String; var imgSaveNo: Integer;
        CurrentFileColor: TColor; SaveOptions: TRVSaveOptions;
        UseCSS: Boolean; Bullets: TRVList); override;
      {$ENDIF}
      function EnterItem(From: TRVEnterDirection; Coord: Integer): Boolean; override;
      procedure BuildJumps(Left,Top: Integer; var StartJumpNo: Integer; jumps: TList); override;
      function GetSubRVData(var StoreState: TRVStoreSubRVData; Position: TRVSubRVDataPos): TPersistent; override;
      procedure ChooseSubRVData(StoreState: TRVStoreSubRVData); override;
      procedure CleanUpChosen; override;
      { Public }
      constructor Create(RVData: TPersistent); override;
      constructor CreateEx(nRows, nCols: Integer; AMainRVData: TCustomRVData);
      destructor Destroy; override;
      procedure UnmergeCells(TopRow, LeftCol, ColSpan, RowSpan: Integer; UnmergeRows, UnmergeCols: Boolean);
      function CanMergeCells(TopRow, LeftCol, ColSpan, RowSpan: Integer;
                             AllowMergeRC: Boolean): Boolean;
      procedure MergeCells(TopRow, LeftCol, ColSpan, RowSpan: Integer; AllowMergeRC: Boolean);
      procedure MergeSelectedCells(AllowMergeRC: Boolean);
      function CanMergeSelectedCells(AllowMergeRC: Boolean): Boolean;
      procedure UnmergeSelectedCells(UnmergeRows, UnmergeCols: Boolean);
      procedure SplitSelectedCellsVertically(ColCount: Integer);
      procedure SplitSelectedCellsHorizontally(RowCount: Integer);
      function  IsCellSelected(Row, Col: Integer): Boolean;
      procedure InsertRows(Index, Count, CopyIndex: Integer
                           {$IFDEF RICHVIEWDEF4};Select:Boolean=True{$ENDIF});
      procedure InsertCols(Index, Count, CopyIndex: Integer
                           {$IFDEF RICHVIEWDEF4};Select:Boolean=True{$ENDIF});
      procedure InsertColsLeft(Count: Integer);
      procedure InsertColsRight(Count: Integer);
      procedure InsertRowsAbove(Count: Integer);
      procedure InsertRowsBelow(Count: Integer);
      procedure DeleteRows(Index, Count: Integer; DecreaseHeight: Boolean);
      procedure DeleteCols(Index, Count: Integer; DecreaseWidth: Boolean);
      procedure DeleteSelectedRows;
      procedure DeleteSelectedCols;
      procedure DeleteEmptyRows;
      procedure DeleteEmptyCols;
      function  GetCellAt(X,Y: Integer; var Row,Col: Integer): Boolean;
      procedure Select(StartRow, StartCol, RowOffs, ColOffs: Integer);
      procedure Deselect;
      procedure SelectRows(StartRow, Count: Integer);
      procedure SelectCols(StartCol, Count: Integer);
      function  GetSelectionBounds(var StartRow, StartCol, RowOffs, ColOffs: Integer): Boolean;
      function  GetNormalizedSelectionBounds(IncludeEditedCell: Boolean; var TopRow, LeftCol, ColSpan, RowSpan: Integer): Boolean;
      function GetEditedCell(var Row,Col: Integer): TCustomRichViewEdit;
      procedure SaveToStream(Stream: TStream);
      procedure LoadFromStream(Stream: TStream);
      procedure EditCell(Row,Col: Integer);
      procedure Changed;
      // Data
      property Rows: TRVTableRows read FRows;
      property Cells[Row, Col: Integer]: TRVTableCellData read GetCells write SetCells;

      procedure SetCellBestWidth(Value: TRVHTMLLength; Row,Col: Integer);
      procedure SetCellBestHeight(Value: Integer; Row,Col: Integer);
      procedure SetCellColor(Value: TColor; Row,Col: Integer);
      procedure SetCellBackgroundStyle(Value: TRVItemBackgroundStyle; Row,Col: Integer);      
      procedure SetCellBackgroundImage(Value: TGraphic; Row,Col: Integer);
      procedure SetCellVisibleBorders(Left, Top, Right, Bottom: Boolean; Row,Col: Integer);
      procedure SetCellBorderColor(Value: TColor; Row,Col: Integer);
      procedure SetCellBorderLightColor(Value: TColor; Row,Col: Integer);
      procedure SetCellVAlign(Value: TRVCellVAlign; Row,Col: Integer);
      procedure SetRowVAlign(Value: TRVCellVAlign; Row: Integer);
      function MoveFocus(GoForward: Boolean; var TopLevelRVData: TPersistent; var  TopLevelItemNo: Integer): Boolean; override;
      procedure ClearFocus; override;
      procedure GetCellPosition(Cell: TRVTableCellData; var Row, Col: Integer);
      function SetExtraStrProperty(Prop: TRVExtraItemStrProperty;
        const Value: String): Boolean; override;
      function GetExtraStrProperty(Prop: TRVExtraItemStrProperty;
        var Value: String): Boolean; override;
      property BackgroundImageFileName: String read FBackgroundImageFileName
        write FBackgroundImageFileName;
    published
      { Published properties }
      // Table:
      property Options: TRVTableOptions read FOptions write FOptions default RVTABLEDEFAULTOPTIONS;
      property PrintOptions: TRVTablePrintOptions read FPrintOptions write FPrintOptions default RVTABLEDEFAULTPRINTOPTIONS;
      property BestWidth: TRVHTMLLength read FBestWidth write SetBestWidth default 0;
      property Color: TColor read FColor write SetColor default clWindow;
      property BackgroundImage: TGraphic read GetBackgroundImage write SetBackgroundImage stored False;
      property BackgroundStyle: TRVItemBackgroundStyle read GetBackgroundStyle write SetBackgroundStyle default rvbsColor;
      property HeadingRowCount: Integer read FHeadingRowCount write SetHeadingRowCount default 0;
      property TextRowSeparator: String read FTextRowSeparator write FTextRowSeparator;
      property TextColSeparator: String read FTextColSeparator write FTextColSeparator;
      // Border around the table:
      property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 0;
      property BorderColor: TColor read FBorderColor write SetBorderColor default clWindowText;
      property BorderLightColor: TColor read FBorderLightColor write SetBorderLightColor default clBtnHighlight;
      property BorderStyle: TRVTableBorderStyle read FBorderStyle write SetBorderStyle default rvtbRaised;
      property BorderVSpacing: Integer read FBorderVSpacing write SetBorderVSpacing default 2;
      property BorderHSpacing: Integer read FBorderHSpacing write SetBorderHSpacing default 2;
      // Cells:
      property CellBorderWidth: Integer read FCellBorderWidth write SetCellBorderWidth default 0;
      property CellBorderColor: TColor read FCellBorderColor write SetCellBorderColorProp default clWindowText;
      property CellBorderLightColor: TColor read FCellBorderLightColor write SetCellBorderLightColorProp default clBtnHighlight;
      property CellPadding: Integer read FCellPadding write SetCellPadding default 1;
      property CellBorderStyle: TRVTableBorderStyle read FCellBorderStyle write SetCellBorderStyle default rvtbLowered;
      // Between cells:
      property VRuleWidth: Integer read FVRuleWidth write SetVRuleWidth default 0;
      property VRuleColor: TColor read FVRuleColor write SetVRuleColor default clWindowText;
      property HRuleWidth: Integer read FHRuleWidth write SetHRuleWidth default 0;
      property HRuleColor: TColor  read FHRuleColor write SetHRuleColor default clWindowText;
      property CellVSpacing: Integer read FCellVSpacing write SetCellVSpacing default 2;
      property CellHSpacing: Integer read FCellHSpacing write SetCellHSpacing default 2;
      property VOutermostRule: Boolean read FVOutermostRule write SetVOutermostRule default False;
      property HOutermostRule: Boolean read FHOutermostRule write SetHOutermostRule default False;
      // Events
      property OnCellEditing: TRVCellEditingEvent read FOnCellEditing write FOnCellEditing;
      property OnCellEndEdit: TRVCellEndEditEvent read FOnCellEndEdit write FOnCellEndEdit;
      property OnDrawBorder: TRVTableDrawBorderEvent read FOnDrawBorder write FOnDrawBorder;

  end;

 TRVTableStoreSubRVData = class (TRVStoreSubRVData)
   public
     Row, Col: Integer;
     constructor Create(ARow, ACol: Integer);
     function Duplicate: TRVStoreSubRVData; override;
 end;

  ERVTableInplaceError = class (Exception);

const RichViewTableGridStyle: TPenStyle = psDot;
      RichViewTableGridColor: TColor = clBtnFace;
      RichViewTableDefaultRTFAutofit: Boolean = False;

implementation
uses RVTInplace;
const errMerge = 'Parameters for cell merging are not correct';
      errIso   = 'Cannot perform operation for isolated cell';
      errReadCells  = 'Wrong end of cells list';
      errInplaceBusy = 'Cannot destroy cell inplace editor at this time';
      errInvalidIndex = 'Invalid row or column index';
      errInternalError = 'Internal error';
{==============================================================================}
procedure DrawFancyVLine(Canvas: TCanvas; X, Y1, Y2: Integer);
var
  i: Integer;
begin
  i := Y1;
  while i<Y2 do
    with Canvas do begin
      MoveTo(X, i);
      LineTo(X, i+1);
      inc(i, 2);
      MoveTo(X, i);
      LineTo(X, i+1);
      inc(i, 2);
      MoveTo(X, i);
      LineTo(X, i+1);
      Inc(i, 4);
    end;
end;
{------------------------------------------------------------------------------}
procedure DrawFancyHLine(Canvas: TCanvas; X1, X2, Y: Integer);
var
  i: Integer;
begin
  i := X1;
  while i<X2 do
    with Canvas do begin
      MoveTo(i, Y);
      LineTo(i+1,Y);
      inc(i, 2);
      MoveTo(i, Y);
      LineTo(i+1,Y);
      inc(i, 2);      
      MoveTo(i, Y);
      LineTo(i+1,Y);
      Inc(i, 4);
    end;
end;
{============================== TRVTablePrintInfo =============================}
type
  TRVTablePrintPart = class (TRVMultiDrawItemPart)
    public
      StartRow, RowCount: Integer;
      function GetSoftPageBreakInfo: Integer; override;
  end;

function TRVTablePrintPart.GetSoftPageBreakInfo: Integer;
begin
  Result := StartRow;
end;

type

  TRVTablePrintInfo = class (TRVMultiDrawItemInfo)
    private
      Fmt: TRVTableItemFormattingInfo;
      sad: TRVScreenAndDevice;
      FTable: TRVTableItemInfo;
      FUseHeadingRowCount: Boolean;
    public
      constructor Create(ATable: TRVTableItemInfo);
      procedure SetSize(AWidth, AHeight: Integer); override;
      destructor Destroy; override;
      function InitSplit: Boolean; override;
      function CanSplitFirst(Y: Integer): Boolean; override;
      function SplitAt(Y: Integer): Boolean; override;
  end;
{------------------------------------------------------------------------------}
constructor TRVTablePrintInfo.Create(ATable: TRVTableItemInfo);
begin
  inherited Create;
  FTable := ATable;
  Fmt := TRVTableItemFormattingInfo.Create(True);
  FUseHeadingRowCount := FTable.CanUseHeadingRowCount;
end;
{------------------------------------------------------------------------------}
destructor TRVTablePrintInfo.Destroy;
begin
  Fmt.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
function TRVTablePrintInfo.InitSplit: Boolean;
var part: TRVTablePrintPart;
begin
  if not (rvtoRowsSplit in FTable.PrintOptions) or
     (FUseHeadingRowCount and (FTable.HeadingRowCount>=FTable.Rows.Count)) then begin
    Result := False;
    exit;
  end;
  part := TRVTablePrintPart.Create;
  if FUseHeadingRowCount then begin
    part.StartRow := FTable.HeadingRowCount;
    if part.StartRow>Fmt.RowStarts.Count then
      part.StartRow := Fmt.RowStarts.Count;
    end
  else
    part.StartRow := 0;
  part.RowCount := Fmt.RowStarts.Count-part.StartRow;
  part.Height := Fmt.FHeight;
  PartsList.Add(part);
  Result := True;
end;
{------------------------------------------------------------------------------}
function TRVTablePrintInfo.CanSplitFirst(Y: Integer): Boolean;
var BorderHeight, CanSplitHere, hrc: Integer;
begin
  if FUseHeadingRowCount then begin
    hrc := FTable.HeadingRowCount;
    if hrc>FTable.Rows.Count then
      hrc := FTable.Rows.Count;
    if hrc=FTable.Rows.Count then begin
      Result := False;
      exit;
    end;
    end
  else
    hrc := 0;
  BorderHeight := Fmt.RowStarts[hrc]+
                  Fmt.FHeight-Fmt.RowStarts[Fmt.RowStarts.Count-1]-
                  Fmt.RowHeights[Fmt.RowStarts.Count-1];
  dec(y,BorderHeight);
  inc(y, Fmt.RowStarts[hrc]);
  CanSplitHere := FTable.GetSplitRowBelow(hrc);
  Result := y>Fmt.RowStarts[CanSplitHere]+Fmt.RowHeights[CanSplitHere];
end;
{------------------------------------------------------------------------------}
function TRVTablePrintInfo.SplitAt(Y: Integer): Boolean;
var BorderHeight, r,Count, CanSplitHere: Integer;
    part: TRVTablePrintPart;
begin
  if PartsList.Count=0 then
    raise ERichViewError.Create(errPrint);
  part := TRVTablePrintPart(PartsList[PartsList.Count-1]);
  if (part.RowCount=1) or (part.Height<=Y) then begin
    Result := False;
    exit;
  end;
  if FUseHeadingRowCount then begin
    r := FTable.HeadingRowCount;
    if r>Fmt.RowStarts.Count then
      r := Fmt.RowStarts.Count;
    if r=Fmt.RowStarts.Count then begin
      Result := False;
      exit;
    end;
    end
  else
    r := 0;
  BorderHeight := Fmt.RowStarts[r]+
                  Fmt.FHeight-Fmt.RowStarts[Fmt.RowStarts.Count-1]-
                  Fmt.RowHeights[Fmt.RowStarts.Count-1];
  dec(y,BorderHeight);
  inc(y, Fmt.RowStarts[part.StartRow]);
  r := part.StartRow+1;
  Count := 1;
  while y>Fmt.RowStarts[r]+Fmt.RowHeights[r] do begin
    inc(r);
    inc(Count);
  end;
  if Count>=part.RowCount then
    raise ERichViewError.Create(errPrint);
  CanSplitHere := FTable.GetSplitRowAbove(r);
  if CanSplitHere<=part.StartRow+1 then
    CanSplitHere := FTable.GetSplitRowBelow(part.StartRow)+1;
  inc(Count, CanSplitHere-r);
  r := CanSplitHere;
  if Count>part.RowCount then
    raise ERichViewError.Create(errPrint);
  if Count=part.RowCount then begin
    Result := False;
    exit;
  end;
  part.RowCount := Count;
  part.Height := BorderHeight+Fmt.RowStarts[r-1]+Fmt.RowHeights[r-1]-Fmt.RowStarts[part.StartRow];

  part := TRVTablePrintPart.Create;
  part.StartRow := r;
  part.RowCount := Fmt.RowStarts.Count-r;
  part.Height := BorderHeight+Fmt.RowStarts[Fmt.RowStarts.Count-1]+Fmt.RowHeights[Fmt.RowStarts.Count-1]-
                 Fmt.RowStarts[r];
  PartsList.Add(part);
  Result := True;
end;
{------------------------------------------------------------------------------}
procedure TRVTablePrintInfo.SetSize(AWidth, AHeight: Integer);
begin
  // do nothing
end;
{------------------------------------------------------------------------------}
type
  TCellPtblRVData = class (TRectPtblRVData)
    protected
      function GetRVStyle: TRVStyle; override;
  end;

  function TCellPtblRVData.GetRVStyle: TRVStyle;
  begin
    Result := FSourceDataForPrinting.GetRVStyle;
  end;

type

  TRVUndoModifyCellIntProperty = class(TRVUndoModifyItemIntProperty)
    public
      Row, Col: Integer;
      procedure Undo(RVData: TRichViewRVData); override;
      procedure SetOppositeUndoInfoProps(UndoInfo: TRVUndoModifyItemProps); override;
  end;

  TRVUndoModifyCellVisibleBorders = class(TRVUndoModifyItemProps)
    public
      Row, Col: Integer;
      Left, Right, Top, Bottom: Boolean;
      procedure Undo(RVData: TRichViewRVData); override;
  end;

  TRVUndoModifyBackgroundImage = class(TRVUndoModifyItemProps)
    public
      Row, Col: Integer;
      Image: TGraphic;
      procedure Undo(RVData: TRichViewRVData); override;
      destructor Destroy; override;
  end;

  TRVUndoModifyCellIntProperties = class(TRVUndoModifyItemIntProperties)
    public
       Row, Col: Integer;
      procedure Undo(RVData: TRichViewRVData); override;
      procedure SetOppositeUndoInfoProps(UndoInfo: TRVUndoModifyItemProps); override;
  end;

  TRVUndoRowVAlign = class(TRVUndoModifyItemProps)
    public
      Row: Integer;
      OldVAlign: TRVCellVAlign;
      procedure Undo(RVData: TRichViewRVData); override;
  end;

  TRVUndoInsertTableRows = class(TRVUndoModifyItemProps)
    public
      Row, Count: Integer;
      Flag: Boolean;
      procedure Undo(RVData: TRichViewRVData); override;
  end;

  TRVUndoInsertTableCell = class(TRVUndoModifyItemProps)
    public
      Row, Col: Integer;
      Flag: Boolean;
      procedure Undo(RVData: TRichViewRVData); override;
  end;

  TRVUndoSpreadOverEmptyCells = class(TRVUndoModifyItemProps)
    public
      Row, Col, ColSpan: Integer;
      Flag: Boolean;
      procedure Undo(RVData: TRichViewRVData); override;
  end;

  TRVUndoSpan = class(TRVUndoModifyItemProps)
    public
      IsColSpan: Boolean;
      Row, Col, OldSpan: Integer;
      procedure Undo(RVData: TRichViewRVData); override;
  end;

  TRVUndoFreeEmptyCell = class(TRVUndoModifyItemProps)
    public
      Flag: Boolean;
      Row, Col, ColSpan, RowSpan: Integer;
      procedure Undo(RVData: TRichViewRVData); override;
  end;

  TRVUndoInsertEmptyCell = class(TRVUndoModifyItemProps)
    public
      Flag: Boolean;
      Row, Col, ColCount, RowCount: Integer;
      procedure Undo(RVData: TRichViewRVData); override;
  end;

  TRVUndoCellSpitHorz = class(TRVUndoModifyItemProps)
    public
      Flag, DecreaseHeight: Boolean;
      Row, Col, Row2, OldBestHeight: Integer;
      procedure Undo(RVData: TRichViewRVData); override;
  end;

  TRVUndoCellSpitVert = class(TRVUndoModifyItemProps)
    public
      Flag, DecreaseWidth: Boolean;
      Row, Col, Col2: Integer;
      OldBestWidth: TRVHTMLLength;
      procedure Undo(RVData: TRichViewRVData); override;
  end;

  TRVUndoDeleteRows = class(TRVUndoModifyItemProps)
    public
      Flag: Boolean;
      Row, Count: Integer;
      Rows: TList;
      procedure Undo(RVData: TRichViewRVData); override;
      destructor Destroy; override;
  end;

  TCustomRVUndoWithCells  = class(TRVUndoModifyItemProps)
    public
      Flag: Boolean;
      CellsList: TList;
      destructor Destroy; override;
  end;

  TRVUndoDeleteCols = class(TCustomRVUndoWithCells)
    public
      Col, Count: Integer;
      procedure Undo(RVData: TRichViewRVData); override;
  end;

  TRVUndoCellsClear = class (TCustomRVUndoWithCells)
    public
      RowList, ColList: TRVIntegerList;
      procedure Undo(RVData: TRichViewRVData); override;
      destructor Destroy; override;
  end;

  TRVUndoMergeItem = class
    public
      Row,Col,ItemCount: Integer;
      Cell: TRVTableCellData;
      constructor Create(Table: TRVTableItemInfo; ARow,ACol,AVampRow,AVampCol: Integer);
  end;

  TRVUndoMerge = class(TRVUndoModifyItemProps)
    public
      Flag: Boolean;
      Row, Col, OldColSpan, OldRowSpan, NewColSpan,NewRowSpan: Integer;
      OldBestWidth: TRVHTMLLength;
      MergedItemsList: TRVList;
      procedure Undo(RVData: TRichViewRVData); override;
      destructor Destroy; override;
  end;

  TRVUndoUnmerge = class(TRVUndoModifyItemProps)
    public
      Flag, UnmergeCols, UnmergeRows: Boolean;
      Row, Col, OldColSpan, OldRowSpan, OldBestHeight: Integer;
      OldBestWidth: TRVHTMLLength;
      procedure Undo(RVData: TRichViewRVData); override;
  end;

  TRVUndoCellModify = class(TRVCompositeUndo)
    public
      Row,Col, CaretItemNo, CaretOffs: Integer;
      procedure Undo(RVData: TRichViewRVData); override;
  end;

  TRVUndoMultiCellsModify = class(TRVCompositeUndo)
    public
      RowList, ColList, CountList: TRVIntegerList;
      OldW: Integer;
      procedure Undo(RVData: TRichViewRVData); override;
      destructor Destroy; override;
      function RequiresFormat: Boolean; override;
      function RequiresFullReformat1(RVData: TRichViewRVData): Boolean; override;
      function RequiresFullReformat2(RVData: TRichViewRVData): Boolean; override;
  end;


function AddTableUndoInfo(RVData: TRichViewRVData; UndoInfoClass: TRVUndoInfoClass;
                             ItemNo: Integer;
                             AffectSize, AffectWidth: Boolean): TRVUndoModifyItemProps;
var List: TRVUndoList;
begin
  List := TRVEditRVData(RVData).GetUndoList;
  if List<>nil then begin
    Result := TRVUndoModifyItemProps(UndoInfoClass.Create);
    Result.Action := rvuModifyItem;
    Result.ItemNo := ItemNo;
    Result.AffectSize   := AffectSize;
    Result.AffectWidth  := AffectWidth;
    List.AddInfo(Result);
    end
  else
    Result := nil;
end;
{======================== TRVUndoModifyCellIntProperty ========================}
procedure TRVUndoModifyCellIntProperty.SetOppositeUndoInfoProps(UndoInfo: TRVUndoModifyItemProps);
begin
  if UndoInfo<>nil then begin
    (UndoInfo as TRVUndoModifyCellIntProperty).Row := Row;
    TRVUndoModifyCellIntProperty(UndoInfo).Col := Col;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVUndoModifyCellIntProperty.Undo(RVData: TRichViewRVData);
var table: TRVTableItemInfo;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  table.Changed;
  SubObject := table.Cells[Row,Col];
  inherited Undo(RVData);
end;
{====================== TRVUndoModifyCellVisibleBorders =======================}
procedure TRVUndoModifyCellVisibleBorders.Undo(RVData: TRichViewRVData);
var table: TRVTableItemInfo;
    cell: TRVTableCellData;
    ui: TRVUndoModifyCellVisibleBorders;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  SubObject := table.Cells[Row,Col];
  Cell      := table.Cells[Row,Col];
  ui := TRVUndoModifyCellVisibleBorders(AddTableUndoInfo(RVData, TRVUndoModifyCellVisibleBorders, ItemNo, False, False));
  if ui<>nil then begin
    ui.Row    := Row;
    ui.Col    := Col;
    ui.Left   := Cell.VisibleBorders.Left;
    ui.Top    := Cell.VisibleBorders.Top;
    ui.Right  := Cell.VisibleBorders.Right;
    ui.Bottom := Cell.VisibleBorders.Bottom;
  end;
  Cell.VisibleBorders.SetValues(Left, Top, Right, Bottom);
  table.Changed;
end;
{======================= TRVUndoModifyBackgroundImage =========================}
destructor TRVUndoModifyBackgroundImage.Destroy;
begin
  Image.Free;
  inherited;
end;
{------------------------------------------------------------------------------}
procedure TRVUndoModifyBackgroundImage.Undo(RVData: TRichViewRVData);
var table: TRVTableItemInfo;
    cell: TRVTableCellData;
    ui: TRVUndoModifyBackgroundImage;
    LImage: TGraphic;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  if (Row>=0) and (Col>=0) then begin
    SubObject := table.Cells[Row,Col];
    Cell      := table.Cells[Row,Col];
    LImage     := Cell.BackgroundImage;
    end
  else begin
    Cell := nil;
    SubObject := nil;
    LImage := table.BackgroundImage;
  end;
  ui := TRVUndoModifyBackgroundImage(AddTableUndoInfo(RVData, TRVUndoModifyBackgroundImage, ItemNo, False, False));
  if ui<>nil then begin
    ui.Row    := Row;
    ui.Col    := Col;
    ui.Image  := LImage;
  end;
  if Cell=nil then
    Table.SetBackgroundImage_(Image, False)
  else
    Cell.SetBackgroundImage_(Image, False);
  Image := nil;
  table.Changed;
end;
{========================= TRVUndoRowVAlign ===================================}
procedure TRVUndoRowVAlign.Undo(RVData: TRichViewRVData);
var table: TRVTableItemInfo;
    ui: TRVUndoRowVAlign;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  ui := TRVUndoRowVAlign(AddTableUndoInfo(RVData, TRVUndoRowVAlign, ItemNo, True, False));
  if ui<>nil then begin
    ui.OldVAlign := table.Rows[Row].VAlign;
    ui.Row       := Row;
  end;
  table.Rows[Row].VAlign := OldVAlign;
  table.Changed;
end;
{=========================== TRVUndoModifyCellIntProperties ===================}
procedure TRVUndoModifyCellIntProperties.SetOppositeUndoInfoProps(
  UndoInfo: TRVUndoModifyItemProps);
begin
  if UndoInfo<>nil then begin
    (UndoInfo as TRVUndoModifyCellIntProperties).Row := Row;
    TRVUndoModifyCellIntProperties(UndoInfo).Col := Col;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVUndoModifyCellIntProperties.Undo(RVData: TRichViewRVData);
var table: TRVTableItemInfo;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  table.Changed;
  SubObject := table.Cells[Row,Col];
  inherited Undo(RVData);
end;
{========================= TRVUndoInsertTableRows =============================}
procedure TRVUndoInsertTableRows.Undo(RVData: TRichViewRVData);
var ui: TRVUndoInsertTableRows;
    table: TRVTableItemInfo;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  ui := TRVUndoInsertTableRows(AddTableUndoInfo(RVData, TRVUndoInsertTableRows,
                             ItemNo, True, True));
  if ui<>nil then begin
    ui.Flag   := not Flag;
    ui.Row          := Row;
    ui.Count        := Count;
  end;
  if Flag then
    table.Rows.Do_InsertRows(Row,Count)
  else
    table.Rows.Do_UnInsertRows(Row,Count);
  table.Changed;
end;
{========================= TRVUndoInsertTableCell =============================}
procedure TRVUndoInsertTableCell.Undo(RVData: TRichViewRVData);
var ui: TRVUndoInsertTableCell;
    table: TRVTableItemInfo;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  ui := TRVUndoInsertTableCell(AddTableUndoInfo(RVData, TRVUndoInsertTableCell,
                             ItemNo, True, True));
  if ui<>nil then begin
    ui.Flag   := not Flag;
    ui.Row    := Row;
    ui.Col    := Col;
  end;
  if Flag then
    table.Rows[Row].Insert(Col)
  else
    table.Rows[Row].Delete(Col);
  table.Changed;
end;
{========================== TRVUndoSpreadOverEmptyCells =======================}
procedure TRVUndoSpreadOverEmptyCells.Undo(RVData: TRichViewRVData);
var ui: TRVUndoSpreadOverEmptyCells;
    table: TRVTableItemInfo;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  ui := TRVUndoSpreadOverEmptyCells(AddTableUndoInfo(RVData, TRVUndoSpreadOverEmptyCells,
                             ItemNo, True, True));
  if ui<>nil then begin
    ui.Flag   := not Flag;
    ui.Row          := Row;
    ui.Col          := Col;
    ui.ColSpan      := ColSpan;
  end;
  if Flag then
    table.Rows.Do_SpreadOverEmptyCells(Row,Col,ColSpan)
  else
    table.Rows.Do_UnSpreadOverEmptyCells(Row,Col,ColSpan);
  table.Changed;
end;
{=============================== TRVUndoSpan ==================================}
procedure TRVUndoSpan.Undo(RVData: TRichViewRVData);
var ui: TRVUndoSpan;
    table: TRVTableItemInfo;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  ui := TRVUndoSpan(AddTableUndoInfo(RVData, TRVUndoSpan,
                             ItemNo, True, True));
  if ui<>nil then begin
    ui.IsColSpan := IsColSpan;
    if IsColSpan then
      ui.OldSpan := table.Cells[Row,Col].ColSpan
    else
      ui.OldSpan := table.Cells[Row,Col].RowSpan;
    ui.Row          := Row;
    ui.Col          := Col;
  end;
  if IsColSpan then
    table.Cells[Row,Col].FColSpan := OldSpan
  else
    table.Cells[Row,Col].FRowSpan := OldSpan;
  table.Changed;
end;
{============================ TRVUndoFreeEmptyCell ============================}
procedure TRVUndoFreeEmptyCell.Undo(RVData: TRichViewRVData);
var ui: TRVUndoFreeEmptyCell;
    table: TRVTableItemInfo;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  ui := TRVUndoFreeEmptyCell(AddTableUndoInfo(RVData, TRVUndoFreeEmptyCell,
                             ItemNo, True, True));
  if ui<>nil then begin
    ui.Flag         := not Flag;
    ui.Row          := Row;
    ui.Col          := Col;
    ui.ColSpan      := ColSpan;
    ui.RowSpan      := RowSpan;
  end;
  if Flag then
    table.Rows.Do_FreeEmptyCells(Row,Col,ColSpan,RowSpan)
  else
    table.Rows.Do_UnFreeEmptyCells(Row,Col,ColSpan,RowSpan);
  table.Changed;
end;
{========================== TRVUndoInsertEmptyCell ============================}
procedure TRVUndoInsertEmptyCell.Undo(RVData: TRichViewRVData);
var ui: TRVUndoInsertEmptyCell;
    table: TRVTableItemInfo;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  ui := TRVUndoInsertEmptyCell(AddTableUndoInfo(RVData, TRVUndoInsertEmptyCell,
                             ItemNo, True, True));
  if ui<>nil then begin
    ui.Flag         := not Flag;
    ui.Row          := Row;
    ui.Col          := Col;
    ui.ColCount     := ColCount;
    ui.RowCount     := RowCount;
  end;
  if Flag then
    table.Rows.Do_InsertEmptyCells(Row,Col,ColCount,RowCount)
  else
    table.Rows.Do_UnInsertEmptyCells(Row,Col,ColCount,RowCount);
  table.Changed;
end;
{============================ TRVUndoCellSpitHorz =============================}
procedure TRVUndoCellSpitHorz.Undo(RVData: TRichViewRVData);
var ui: TRVUndoCellSpitHorz;
    table: TRVTableItemInfo;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  ui := TRVUndoCellSpitHorz(AddTableUndoInfo(RVData, TRVUndoCellSpitHorz,
                             ItemNo, True, False));
  if ui<>nil then begin
    ui.Flag         := not Flag;
    ui.Row          := Row;
    ui.Col          := Col;
    ui.Row2         := Row2;
    ui.OldBestHeight := OldBestHeight;
    ui.DecreaseHeight := DecreaseHeight;
  end;
  if Flag then
    table.Rows.Do_SplitCellHorz(Row,Col,Row2,DecreaseHeight)
  else
    table.Rows.Do_UnSplitCellHorz(Row,Col,Row2,OldBestHeight);
  table.Changed;
end;
{============================== TRVUndoCellSpitVert ===========================}
procedure TRVUndoCellSpitVert.Undo(RVData: TRichViewRVData);
var ui: TRVUndoCellSpitVert;
    table: TRVTableItemInfo;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  ui := TRVUndoCellSpitVert(AddTableUndoInfo(RVData, TRVUndoCellSpitVert,
                             ItemNo, True, True));
  if ui<>nil then begin
    ui.Flag         := not Flag;
    ui.Row          := Row;
    ui.Col          := Col;
    ui.Col2         := Col2;
    ui.OldBestWidth := OldBestWidth;
    ui.DecreaseWidth := DecreaseWidth;
  end;
  if Flag then
    table.Rows.Do_SplitCellVert(Row,Col,Col2,DecreaseWidth)
  else
    table.Rows.Do_UnSplitCellVert(Row,Col,Col2,OldBestWidth);
  table.Changed;
end;
{============================== TRVUndoDeleteRows =============================}
procedure TRVUndoDeleteRows.Undo(RVData: TRichViewRVData);
var ui: TRVUndoDeleteRows;
    table: TRVTableItemInfo;
    i: Integer;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  ui := TRVUndoDeleteRows(AddTableUndoInfo(RVData, TRVUndoDeleteRows,
                             ItemNo, True, True));
  if ui<>nil then begin
    ui.Flag         := not Flag;
    ui.Row          := Row;
    ui.Count        := Count;
    if Flag then begin
      ui.Rows := TList.Create;
      for i := 0 to Count-1 do
        ui.Rows.Add(table.Rows[Row+i]);
    end;
  end;
  if Flag then
    table.Rows.Do_DeleteRows(ItemNo, Row,Count,ui)
  else
    table.Rows.Do_UnDeleteRows(Row, Rows);
  table.Changed;
end;
{------------------------------------------------------------------------------}
destructor TRVUndoDeleteRows.Destroy;
var i: Integer;
begin
  if Rows<>nil then begin
    for i := 0 to Rows.Count-1 do
      TObject(Rows[i]).Free;
    Rows.Free;
  end;
  inherited Destroy;
end;
{================================= TCustomRVUndoWithCells =====================}
destructor TCustomRVUndoWithCells.Destroy;
var i: Integer;
begin
  if CellsList<>nil then begin
    for i := 0 to CellsList.Count-1 do
      TObject(CellsList[i]).Free;
    CellsList.Free;
  end;
  inherited Destroy;
end;
{=========================== TRVUndoDeleteCols ================================}
procedure TRVUndoDeleteCols.Undo(RVData: TRichViewRVData);
var ui: TRVUndoDeleteCols;
    table: TRVTableItemInfo;
    r,c: Integer;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  ui := TRVUndoDeleteCols(AddTableUndoInfo(RVData, TRVUndoDeleteCols,
                             ItemNo, True, True));
  if ui<>nil then begin
    ui.Flag         := not Flag;
    ui.Col          := Col;
    ui.Count        := Count;
    if Flag then begin
      ui.CellsList := TList.Create;
      for r := 0 to table.Rows.Count-1 do
        for c := Count-1 downto 0 do
          ui.CellsList.Add(table.Cells[r,Col+c]);
    end;
  end;
  if Flag then
    table.Rows.Do_DeleteCols(ItemNo, Col,Count, ui)
  else
    table.Rows.Do_UnDeleteCols(Col, CellsList);
  table.Changed;
end;
{================================= TRVUndoCellsClear ==========================}
destructor TRVUndoCellsClear.Destroy;
begin
  RowList.Free;
  ColList.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVUndoCellsClear.Undo(RVData: TRichViewRVData);
var ui: TRVUndoCellsClear;
    table: TRVTableItemInfo;
    NewCellsList: TList;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  NewCellsList := nil;
  ui := TRVUndoCellsClear(AddTableUndoInfo(RVData, TRVUndoCellsClear,
                             ItemNo, True, True));
  if ui<>nil then begin
    ui.Flag         := not Flag;
    ui.RowList      := TRVIntegerList.CreateCopy(RowList);
    ui.ColList      := TRVIntegerList.CreateCopy(ColList);
    if Flag then begin
      ui.CellsList := TList.Create;
      NewCellsList := ui.CellsList;
    end;
  end;
  if Flag then
    table.Rows.Do_ClearCells(NewCellsList, RowList, ColList, ui)
  else
    table.Rows.Do_UnClearCells(CellsList, RowList, ColList);
  table.Changed;
end;
{=============================== TRVUndoMergeItem =============================}
constructor TRVUndoMergeItem.Create(Table: TRVTableItemInfo; ARow, ACol,
  AVampRow, AVampCol: Integer);
begin
  inherited Create;
  Row := ARow;
  Col := ACol;
  Cell := Table.Cells[Row,Col];
  if Cell.HasData(False) then
    ItemCount := Cell.Items.Count
  else
    ItemCount := 0;
end;
{============================== TRVUndoMerge ==================================}
procedure TRVUndoMerge.Undo(RVData: TRichViewRVData);
var ui: TRVUndoMerge;
    table: TRVTableItemInfo;
    r,c: Integer;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  ui := TRVUndoMerge(AddTableUndoInfo(RVData, TRVUndoMerge,
                             ItemNo, True, True));
  if ui<>nil then begin
    ui.Flag         := not Flag;
    ui.Row          := Row;
    ui.Col          := Col;
    ui.OldColSpan   := OldColSpan;
    ui.OldRowSpan   := OldRowSpan;
    ui.NewColSpan   := NewColSpan;
    ui.NewRowSpan   := NewRowSpan;
    ui.OldBestWidth := OldBestWidth;
    if Flag then begin
      ui.MergedItemsList := TRVList.Create;
      for r := Row to Row+NewRowSpan-1 do
        for c := Col to Col+NewColSpan-1 do
          if (table.Cells[r,c]<>nil) and
             (table.Cells[r,c]<>table.Cells[Row,Col]) then
          ui.MergedItemsList.Add(TRVUndoMergeItem.Create(table,r,c,Row,Col));
    end;
  end;
  if Flag then
    table.Rows.Do_MergeCells(ItemNo, Row,Col, NewColSpan, NewRowSpan, ui, True)
  else
    table.Rows.Do_UndoMergeCells(ItemNo, Row,Col, OldColSpan, OldRowSpan, MergedItemsList,OldBestWidth);
  table.Changed;
end;
{------------------------------------------------------------------------------}
destructor TRVUndoMerge.Destroy;
var i: Integer;
begin
  if MergedItemsList<>nil then begin
    for i := 0 to MergedItemsList.Count-1 do
      TRVUndoMergeItem(MergedItemsList[i]).Cell.Free;
    MergedItemsList.Free;
  end;
  inherited Destroy;
end;
{================================ TRVUndoUnmerge ==============================}
procedure TRVUndoUnmerge.Undo(RVData: TRichViewRVData);
var ui: TRVUndoUnmerge;
    table: TRVTableItemInfo;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  ui := TRVUndoUnmerge(AddTableUndoInfo(RVData, TRVUndoUnmerge,
                             ItemNo, True, True));
  if ui<>nil then begin
    ui.Flag         := not Flag;
    ui.Row          := Row;
    ui.Col          := Col;
    ui.OldColSpan   := OldColSpan;
    ui.OldRowSpan   := OldRowSpan;
    ui.UnmergeCols  := UnmergeCols;
    ui.UnmergeRows  := UnmergeRows;
    ui.OldBestWidth  := OldBestWidth;
    ui.OldBestHeight := OldBestHeight;
  end;
  if Flag then
    table.Rows.Do_UnmergeCell(ItemNo, Row,Col, UnmergeRows, UnmergeCols)
  else
    table.Rows.Do_UndoUnmergeCell(ItemNo, Row,Col, OldColSpan, OldRowSpan,
                              OldBestWidth, OldBestHeight);
  table.Changed;
end;
{============================= TRVUndoCellModify ==============================}
procedure TRVUndoCellModify.Undo(RVData: TRichViewRVData);
var
  table: TRVTableItemInfo;
  ERow,ECol,i: Integer;
  List: TRVUndoList;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  table.GetEditedCell(ERow,ECol);
  if (ERow<>Row) or (ECol<>Col) then
    table.EditCell_(Row,Col,True);
  if IsRedo then
    List := TRVEditRVData(table.FInplaceEditor.RVData).RedoList
  else
    List := TRVEditRVData(table.FInplaceEditor.RVData).UndoList;
  for i := 0 to UndoList.Count-1 do begin
    List.AddInfos(TObject(UndoList.Items[i]) as TRVUndoInfos);
  end;
  for i := UndoList.Count-1 downto 0 do
    UndoList.DeleteAsPointer(i);
  table.FInplaceEditor.SetSelectionBounds(CaretItemNo, CaretOffs,CaretItemNo, CaretOffs);
  table.FInplaceEditor.SetFReadOnly(True);
  if IsRedo then
    PostMessage(table.FInplaceEditor.Handle, WM_RVINPLACEREDO,0,0)
  else
    PostMessage(table.FInplaceEditor.Handle, WM_RVINPLACEUNDO,0,0)
end;
{========================== TRVUndoMultiCellsModify ===========================}
destructor TRVUndoMultiCellsModify.Destroy;
begin
  RowList.Free;
  ColList.Free;
  CountList.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
function TRVUndoMultiCellsModify.RequiresFormat: Boolean;
begin
  Result := True;
end;
{------------------------------------------------------------------------------}
function TRVUndoMultiCellsModify.RequiresFullReformat1(
  RVData: TRichViewRVData): Boolean;
begin
  OldW := RVData.CalculateMinItemWidthPlusEx(ItemNo);
  Result := OldW >= RVData.DocumentWidth;
end;
{------------------------------------------------------------------------------}
function TRVUndoMultiCellsModify.RequiresFullReformat2(
  RVData: TRichViewRVData): Boolean;
var NewW: Integer;
begin
  NewW := RVData.CalculateMinItemWidthPlusEx(ItemNo);
  Result :=  (NewW<>OldW) and (NewW>RVData.DocumentWidth);
end;
{------------------------------------------------------------------------------}
procedure TRVUndoMultiCellsModify.Undo(RVData: TRichViewRVData);
var ui: TRVUndoMultiCellsModify;
    table: TRVTableItemInfo;
    Editor: TCustomRichViewEdit;
    i: Integer;
begin
  table := RVData.GetItem(ItemNo) as TRVTableItemInfo;
  ui := TRVUndoMultiCellsModify(AddTableUndoInfo(RVData, TRVUndoMultiCellsModify,
                             ItemNo, True, True));
  if ui<>nil then begin
    ui.IsRedo       := not IsRedo;
    ui.RowList      := TRVIntegerList.Create;
    ui.ColList      := TRVIntegerList.Create;
    ui.CountList    := TRVIntegerList.Create;
    ui.UndoList     := TRVUndoList.Create(GetUndoListOwnerRVData);
  end;
  Editor := table.CreateTemporalEditor;
  try
    while UndoList.Count>0 do begin
      Editor.RVData.Clear;
      Editor.RVData.DrainFrom(table.Cells[RowList[0],ColList[0]]);
      Editor.Format;
      Editor.SelectAll;
      for i := 0 to CountList[0]-1 do begin
        TRVEditRVData(Editor.RVData).UndoList.AddInfos(TObject(UndoList.Items[0]) as TRVUndoInfos);
        UndoList.DeleteAsPointer(0);
      end;
      Editor.RVData.State := Editor.RVData.State + [rvstSkipFormatting];
      Editor.Undo;
      Editor.RVData.State := Editor.RVData.State - [rvstSkipFormatting];
      if ui<>nil then begin
        ui.RowList.Add(RowList[0]);
        ui.ColList.Add(ColList[0]);
        ui.CountList.Add(TRVEditRVData(Editor.RVData).RedoList.Count);
        for i := 0 to TRVEditRVData(Editor.RVData).RedoList.Count-1 do
          ui.UndoList.AddInfos(TObject(TRVEditRVData(Editor.RVData).RedoList.Items[i]) as TRVUndoInfos);
        for i := TRVEditRVData(Editor.RVData).RedoList.Count-1 downto 0 do
          TRVEditRVData(Editor.RVData).RedoList.DeleteAsPointer(i);
      end;
      table.Cells[RowList[0],ColList[0]].DrainFrom(Editor.RVData);
      RowList.Delete(0);
      ColList.Delete(0);
      CountList.Delete(0);
    end;
  finally
    Editor.Free;
  end;
  table.Changed;
end;
{========================== TRVTableCellData ==================================}
constructor TRVTableCellData.Create(ARow: TRVTableRow);
begin
  inherited Create(ARow);
  FVisibleBorders := TRVBooleanRect.Create(True);
  FColSpan := 1;
  FRowSpan := 1;
  FColor            := clNone;
  FBorderColor      := clNone;
  FBorderLightColor := clNone;
  FValign  := rvcVDefault;
  State    := [];
  AddNL('',0,0);
end;
{------------------------------------------------------------------------------}
destructor TRVTableCellData.Destroy;
begin
  FVisibleBorders.Free;
  FBackground.Free;
  FBackground := nil;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.Edit: TCustomRVData;
var table: TRVTableItemInfo;
    Row,Col: Integer;
begin
  Result := GetRVData;
  if Result<>Self then
    exit;
  table := GetTable;
  table.GetCellPosition(Self,Row,Col);
  table.EditCell(Row,Col);
  Result := GetRVData;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetTable: TRVTableItemInfo;
begin
  if (ContainerUndoItem<>nil) or (FList=nil) or (TRVTableRow(FList).FRows=nil) then
    Result := nil
  else
    Result := TRVTableRow(FList).FRows.FTable;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.GetParentInfo(var ParentItemNo: Integer;
                                     var Location: TRVStoreSubRVData);
var r,c: Integer;
    table: TRVTableItemInfo;
begin
  table := GetTable;
  table.GetCellPosition(Self, r, c);
  Location := TRVTableStoreSubRVData.Create(r,c);
  ParentItemNo := table.GetMyItemNo;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.IsTransparent: Boolean;
begin
  Result := (Color=clNone);
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.SetBestHeight(const Value: Integer);
begin
  FBestHeight := Value;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.SetBestWidth(const Value: TRVHTMLLength);
begin
  FBestWidth := Value;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.SetColor(const Value: TColor);
var table: TRVTableItemInfo;
begin
  FColor := Value;
  table := GetTable;
  if table=nil then exit;
  if (table.FInplaceEditor<>nil) and (TRVTableInplaceEdit(table.FInplaceEditor).FCell=Self) then begin
    table.FInplaceEditor.Color := table.GetCellColor(Self);
    TRVTableInplaceEdit(table.FInplaceEditor).Transparent := table.CanSeeBackgroundThroughCell(Self);
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetBackgroundImage: TGraphic;
begin
  if FBackground<>nil then
    Result := FBackground.Image
  else
    Result := nil;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetBackgroundStyle: TRVItemBackgroundStyle;
begin
  if FBackground<>nil then
    Result := FBackground.ItemBackStyle
  else
    Result := rvbsColor;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.SetBackgroundImage_(const Value: TGraphic; Copy: Boolean);
var table: TRVTableItemInfo;
begin
  table := GetTable;
  if Value=BackgroundImage then
    exit;
  if (FBackground=nil) and (Value<>nil) and not Value.Empty then
    FBackground := TRVBackground.Create(False);
  if FBackground<>nil then begin
    FBackground.AssignImage(Value, table.Rows.FMainRVData, Copy);
    if FBackground.Empty then begin
      FBackground.Free;
      FBackground := nil;
    end
  end;
  if (table.FInplaceEditor<>nil) and (TRVTableInplaceEdit(table.FInplaceEditor).FCell=Self) then
    table.FInplaceEditor.Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.SetBackgroundImage(const Value: TGraphic);
begin
  SetBackgroundImage_(Value, True);
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.SetBackgroundStyle(
  const Value: TRVItemBackgroundStyle);
var table: TRVTableItemInfo;  
begin
  if Value=BackgroundStyle then
    exit;
  if (FBackground=nil) and (Value<>rvbsColor) then
    FBackground := TRVBackground.Create(False);
  if FBackground<>nil then begin
    FBackground.ItemBackStyle := Value;
    if FBackground.Empty then begin
      FBackground.Free;
      FBackground := nil;
    end
  end;
  table := GetTable;
  if (table.FInplaceEditor<>nil) and (TRVTableInplaceEdit(table.FInplaceEditor).FCell=Self) then
    table.FInplaceEditor.Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.BackgroundImageReader(Stream: TStream);
var s: String;
    v: Integer;
    gr: TGraphic;
begin
  Stream.ReadBuffer(v, sizeof(v));
  SetLength(s, v);
  Stream.ReadBuffer(PChar(s)^, v);
  gr := RV_CreateGraphics(TGraphicClass(GetClass(s)));
  RVFLoadPictureBinary2(Stream, gr);
  BackgroundImage := gr;
  gr.Free;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.BackgroundImageWriter(Stream: TStream);
var s: String;
    v: Integer;
begin
  s := FBackground.Image.ClassName;
  v := Length(s);
  Stream.WriteBuffer(v, sizeof(v));
  Stream.WriteBuffer(PChar(s)^, v);
  RVFSavePictureBinary(Stream, FBackground.Image);
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetBackground: TRVBackground;
begin
  Result := FBackground;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetCellHeight: Integer;
begin
  if (TRVTableRow(FList).FRows.FTable.FInplaceEditor<>nil) and
     (TRVTableInplaceEdit(TRVTableRow(FList).FRows.FTable.FInplaceEditor).FCell=Self) then begin
    Result := TRVTableRow(FList).FRows.FTable.FInplaceEditor.DocumentHeight-
              TRVTableRow(FList).FRows.FTable.CellPadding*2;
    {if Result>10000 then
      Result := 10000;}
    end
  else
    Result := DocumentHeight;
  if BestHeight>Result then
    Result := BestHeight;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas): Integer;
var ARVData: TCustomRVData;
begin
  ARVData := GetRVData;
  if (ARVData.Items.Count=0) then begin
    Result := 0;
    exit;
  end;
  if ((ARVData.Items.Count=1) and (ARVData.GetItemStyle(0)>=0) and (ARVData.Items[0]='')) then begin
    with GetRVStyle.ParaStyles[ARVData.GetItemPara(0)] do
      Result := LeftIndent+RightIndent+FirstIndent;
    if sad<>nil then
      Result := MulDiv(Result, sad.ppixDevice, sad.ppixScreen);
    exit;
  end;
  if (TRVTableRow(FList).FRows.FTable.FInplaceEditor<>nil) and
     (TRVTableInplaceEdit(TRVTableRow(FList).FRows.FTable.FInplaceEditor).FCell=Self) then
    Result := TRVTableRow(FList).FRows.FTable.FInplaceEditor.RVData.CalculateMinDocWidthPlus(sad, Canvas)
  else begin
    Result := CalculateMinDocWidthPlus(sad, Canvas);
    if Result=0 then
      Result := 10; // temporal;
  end;
end;
{------------------------------------------------------------------------------}
{
function TRVTableCellData.GetWidthInFixedTable(TableWidth: Integer): Integer;
begin
  if BestWidth>0 then
    Result := BestWidth
  else if BestWidth<0 then
    Result := -TableWidth*BestWidth div 100
  else
    Result := GetMinWidth;
end;
}
{------------------------------------------------------------------------------}
function TRVTableCellData.GetHeight: Integer;
begin
  Result := Height-TRVTableRow(FList).FRows.FTable.CellPadding*2;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetWidth: Integer;
begin
  Result := Width-TRVTableRow(FList).FRows.FTable.CellPadding*2;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetAreaWidth: Integer;
begin
  Result := GetWidth;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetColor: TColor;
begin
  Result := clNone;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.AssignChosenRVData(RVData: TCustomRVFormattedData;
                                              Item: TCustomRVItemInfo);
var r,c: Integer;
begin
  GetTable.GetCellPosition(Self,r,c);
  GetTable.ChooseSubRVData_(r,c);
  inherited;
  {
  with GetTable do
    if not FInserted or FEditMode then
      exit;
  }
  if FChosenRVData<>RVData then
    UnassignChosenRVData(FChosenRVData);
  FChosenRVData := RVData;
  FChosenItem   := Item;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.UnassignChosenRVData(RVData: TCustomRVData);
begin
  inherited;
  if rvstUnAssigningChosen in State then
    exit;
  State := State+[rvstUnAssigningChosen];
  if GetRVData is TCustomRVFormattedData then
    TCustomRVFormattedData(GetRVData).State := State + [rvstUnAssigningChosen];
  try
    {
    with GetTable do
      if not FInserted or FEditMode then
        exit;
    }
    if (RVData=FChosenRVData) or
       ((FChosenRVData<>nil) and (TCustomRVData(FChosenRVData).GetRVData=RVData)) then begin
      if FChosenRVData<>nil then
        TCustomRVFormattedData(FChosenRVData.GetRVData).Deselect(nil,False);
       FChosenRVData := nil;
       if FChosenItem<>nil then
         FChosenItem.CleanUpChosen;
       FChosenItem   := nil;
    end;
  finally
    if GetRVData is TCustomRVFormattedData then
      TCustomRVFormattedData(GetRVData).State := State-[rvstUnAssigningChosen];
    State := State-[rvstUnAssigningChosen];
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetChosenRVData: TCustomRVData;
begin
  Result := FChosenRVData;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetChosenItem: TCustomRVItemInfo;
begin
  Result := FChosenItem;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetHOffs: Integer;
begin
  Result := - (TRVTableRow(FList).FRows.FTable.MyClientLeft +
               TRVTableRow(FList).FRows.FTable.CellPadding +
               Left);
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetEditor: TWinControl;
begin
  Result := TRVTableRow(FList).FRows.FTable.FInplaceEditor;
  if (Result<>nil) and
     (TRVTableInplaceEdit(Result).FCell<>Self) then
    Result := nil;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.Deselect(
  NewPartiallySelected: TCustomRVItemInfo; MakeEvent: Boolean);
var table: TRVTableItemInfo;
begin
  inherited;
  if ContainerUndoItem<>nil then
    exit;
  if rvstDeselecting in State then
    exit;
  State := State + [rvstDeselecting];
  if GetRVData is TCustomRVFormattedData then
    TCustomRVFormattedData(GetRVData).State := State + [rvstDeselecting];
  try
    table := GetTable;
    if (NewPartiallySelected=nil) and (table<>nil) and (table.FRows.FMainRVData is TCustomRVFormattedData) then
      TCustomRVFormattedData(table.FRows.FMainRVData).UnassignChosenRVData(Self);
  finally
    if GetRVData is TCustomRVFormattedData then
      TCustomRVFormattedData(GetRVData).State := State - [rvstDeselecting];
    State := State - [rvstDeselecting];
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetVOffs: Integer;
var table: TRVTableItemInfo;
begin
  table := GetTable;
  Result := - (table.MyClientTop + table.CellPadding +
               GetExtraVOffs +
               Top);
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.ResetSubCoords;
begin
  inherited;
  GetTable.ResetSubCoords;
end;
{------------------------------------------------------------------------------}
{ Coordinates of the top left corner of this RVData relative to the closest
  parent RichView (root one or inplace editor)                                 }
procedure TRVTableCellData.GetOrigin(var ALeft, ATop: Integer);
var table: TRVTableItemInfo;
    x,y: Integer;
begin
  table := GetTable;
  TCustomRVFormattedData(table.FRows.FMainRVData).GetOrigin(x,y);
  TCustomRVFormattedData(table.FRows.FMainRVData).GetItemCoords(table.GetMyItemNo, ALeft, ATop);
  inc(ALeft, x+FLeft+table.CellPadding);
  inc(ATop, y+FTop+table.CellPadding);
  inc(ATop,GetExtraVOffs);
end;
{------------------------------------------------------------------------------}
{ Coordinates of the top left corner of this RVData relative to the root
  parent RichView                                                              }
procedure TRVTableCellData.GetOriginEx(var ALeft, ATop: Integer);
var table: TRVTableItemInfo;
    x,y: Integer;
begin
  table := GetTable;
  TCustomRVFormattedData(table.FRows.FMainRVData).GetOriginEx(x,y);
  TCustomRVFormattedData(table.FRows.FMainRVData).GetItemCoords(table.GetMyItemNo, ALeft, ATop);
  inc(ALeft, x+FLeft+table.CellPadding);
  inc(ATop, y+FTop+table.CellPadding);
  inc(ATop,GetExtraVOffs);
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.MovingToUndoList(AContainerUndoItem: TRVUndoInfo);
var i: Integer;
begin
  for i := 0 to Items.Count-1 do
    GetItem(i).MovingToUndoList(i,Self,AContainerUndoItem);
  ContainerUndoItem := AContainerUndoItem;
  FList := nil;    
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.MovingFromUndoList;
var i: Integer;
begin
  ContainerUndoItem := nil;
  for i := 0 to Items.Count-1 do
    GetItem(i).MovingFromUndoList(i,Self);
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.SetVisibleBorders(const Value: TRVBooleanRect);
begin
  FVisibleBorders.Assign(Value);
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.HasData(CheckStyles: Boolean): Boolean;
begin
  with GetRVData do
    Result :=
       (Items.Count>1) or
      (
        (Items.Count=1) and
        not
        (
         (CheckStyles and (GetItemStyle(0)=0) and (GetItemPara(0)=0) and (Items[0]='')) or
         (not CheckStyles and (GetItemStyle(0)>=0) and (Items[0]=''))
        )
      )
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty( 'Data', DataReader, DataWriter, HasData(True));
  Filer.DefineBinaryProperty('BackgroundImg', BackgroundImageReader, BackgroundImageWriter,
    (FBackground<>nil) and not FBackground.Empty);
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetRVData: TCustomRVData;
begin
  if (TRVTableRow(FList).FRows.FTable.FInplaceEditor<>nil) and
     (TRVTableInplaceEdit(TRVTableRow(FList).FRows.FTable.FInplaceEditor).FCell=Self) then
    Result := TRVTableInplaceEdit(TRVTableRow(FList).FRows.FTable.FInplaceEditor).RVData
  else
    Result := Self;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.AssignAttributesFrom(Cell: TRVTableCellData;
                                                IncludeSize: Boolean;
                                                DivColSpan, DivRowSpan: Integer);
begin
  FColor            := Cell.Color;
  FBorderColor      := Cell.BorderColor;
  FBorderLightColor := Cell.BorderLightColor;
  VisibleBorders.Assign(Cell.VisibleBorders);
  FValign := Cell.VAlign;
  BackgroundImage := Cell.BackgroundImage;
  BackgroundStyle := Cell.BackgroundStyle;
  BackgroundImageFileName := Cell.BackgroundImageFileName;  
  if IncludeSize then begin
    FBestWidth := Cell.FBestWidth div DivColSpan;
    FBestHeight := Cell.FBestHeight div DivRowSpan;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.AssignSizeFrom(Cell:TRVTableCellData);
begin
  FLeft := Cell.FLeft;
  FTop := Cell.FTop;
  FWidth := Cell.FWidth;
  FHeight := Cell.FHeight;  
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetRealVAlign: TRVCellVAlign;
begin
  if VAlign<>rvcVDefault then
    Result := VAlign
  else begin
    Result := TRVTableRow(FList).VAlign;
    if Result=rvcVDefault then
      Result := rvcMiddle;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetExtraVOffs: Integer;
var RVData: TCustomRVFormattedData;
begin
  case GetRealVAlign of
    rvcBottom:
      begin
        RVData := TCustomRVFormattedData(GetRVData);
        Result := Height-RVData.DocumentHeight;
        if RVData=Self then
          dec(Result,GetTable.CellPadding*2);
      end;
    rvcMiddle:
      begin
        RVData := TCustomRVFormattedData(GetRVData);
        Result := (Height-RVData.DocumentHeight) div 2;
        if RVData=Self then
          dec(Result,GetTable.CellPadding);
      end;
    else
      Result := 0;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.GetOptions: TRVOptions;
begin
  if ContainerUndoItem<>nil then
    Result := ContainerUndoItem.GetUndoListOwnerRVData.Options
  else
    Result := inherited GetOptions;
  Exclude(Result, rvoClientTextWidth);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSELISTS}
function TRVTableCellData.GetMarkers(AllowCreate: Boolean): TRVMarkerList;
begin
  if ContainerUndoItem<>nil then
    Result := nil // in undo list
  else begin
    if (GetTable.GetMyItemNo<0) then
      Result := nil // not inserted yet
    else
      Result := inherited GetMarkers(AllowCreate);
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRVTableCellData.SupportsPageBreaks: Boolean;
begin
  Result := False;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.DoSelect;
begin
  // do not perform inherited actions
end;
{------------------------------------------------------------------------------}
function TRVTableCellData.CanClear: Boolean;
var i: Integer;
    item: TCustomRVItemInfo;
    RVStyle: TRVStyle;
begin
  Result := True;
  RVStyle := GetRVStyle;
  if RVStyle=nil then
    exit;
  for i := 0 to Items.Count-1 do begin
    item := GetItem(i);
    if ((item.StyleNo>=0) and (rvprDeleteProtect in RVStyle.TextStyles[item.StyleNo].Protection)) or
       ((item.StyleNo<0) and TRVNonTextItemInfo(item).DeleteProtect) then begin
      Result := False;
      exit;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.ControlAction2(ControlAction: TRVControlAction;
  ItemNo: Integer; var Control: TControl);
begin
  if ContainerUndoItem<>nil then
    ContainerUndoItem.GetUndoListOwnerRVData.ControlAction2(ControlAction,-1,Control)
  else begin
    ItemNo :=  TRVTableRow(FList).FRows.FTable.GetMyItemNo;
    TRVTableRow(FList).FRows.FMainRVData.ControlAction2(ControlAction,ItemNo,Control);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.ItemAction(ItemAction: TRVItemAction; Item: TCustomRVItemInfo;
                                       var Text: String; RVData: TCustomRVData);
begin
  if ContainerUndoItem<>nil then
    ContainerUndoItem.GetUndoListOwnerRVData.ItemAction(ItemAction, Item, Text, RVData)
  else
    TRVTableRow(FList).FRows.FMainRVData.ItemAction(ItemAction, Item, Text, RVData);
end;
{------------------------------------------------------------------------------}
procedure TRVTableCellData.AdjustFocus(NewFocusedItemNo: Integer;
                           TopLevelRVData: TPersistent; TopLevelItemNo: Integer);
var r,c: Integer;
begin
  inherited AdjustFocus(NewFocusedItemNo, TopLevelRVData, TopLevelItemNo);
  with GetTable do begin
    GetCellPosition(Self,r,c);
    AdjustFocus(r,c,TopLevelRVData,TopLevelItemNo);
  end;
end;
{=============================== TRVTableRow ==================================}
constructor TRVTableRow.Create(nCols: Integer; ARows: TRVTableRows;
                               MainRVData: TCustomRVData);
var i: Integer;
begin
  inherited Create(MainRVData);
  FRows    := ARows;
  Capacity := nCols;
  Valign   := rvcTop;
  for i := 0 to nCols-1 do
    Add
end;
{------------------------------------------------------------------------------}
function TRVTableRow.GetParentRVData: TCustomRVData;
begin
  Result := FRows.FMainRVData;
end;
{------------------------------------------------------------------------------}
function TRVTableRow.HasCellsInRange(Index, RangeStart, Count: Integer): Boolean;
var c, RangeEnd, LastCellRow: Integer;
begin
  Result := False;
  if RangeStart>Index then
    exit;
  RangeEnd := RangeStart+Count-1;
  for c := 0 to Self.Count-1 do
    if Items[c]<>nil then begin
      LastCellRow := Index+Items[c].RowSpan-1;
      if LastCellRow<=RangeEnd then begin
        Result := True;
        exit;
      end;
    end;
end;
{------------------------------------------------------------------------------}
function TRVTableRow.Add: TRVTableCellData;
begin
  Result := TRVTableCellData.Create(Self);
  inherited Add(Result);
end;
{------------------------------------------------------------------------------}
function TRVTableRow.Insert(Index: Integer): TRVTableCellData;
begin
  Result := TRVTableCellData.Create(Self);
  inherited Insert(Index, Result);
end;
{------------------------------------------------------------------------------}
procedure TRVTableRow.InsertEmpty(Index: Integer);
begin
  inherited Insert(Index, nil);
end;
{------------------------------------------------------------------------------}
procedure TRVTableRow.InsertPointer(Index: Integer; Item: TRVTableCellData);
begin
  if Item<>nil then
    Item.FList := Self;
  inherited Insert(Index, Item);
end;
{------------------------------------------------------------------------------}
function TRVTableRow.Get(Index: Integer): TRVTableCellData;
begin
  Result := TRVTableCellData(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
procedure TRVTableRow.Put(Index: Integer; const Value: TRVTableCellData);
begin
  if Value<>nil then
    Value.FList := Self;
  inherited Put(Index, Value);
end;
{------------------------------------------------------------------------------}
function TRVTableRow.GetBestHeight: Integer;
var i,h: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    if (Items[i]<>nil) and (Items[i].RowSpan<=1) then begin
      h := Items[i].BestHeight;
      if h>Result then
        Result := h;
    end;
end;
{------------------------------------------------------------------------------}
function TRVTableRow.GetHeight: Integer;
var i,h: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    if (Items[i]<>nil) and (Items[i].RowSpan<=1) then begin
      h := Items[i].GetCellHeight;
      if h>Result then
        Result := h;
    end;
  if Result = 0 then
    Result := 10; // temporary
end;
{============================== TRVTableRows ==================================}
constructor TRVTableRows.Create(nRows, nCols: Integer; AMainRVData: TCustomRVData;
                                ATable: TRVTableItemInfo);
begin
  inherited Create;
  FMainRVData := AMainRVData;
  FTable      := ATable;
  Reset(nRows, nCols);
end;
{------------------------------------------------------------------------------}
destructor TRVTableRows.Destroy;
begin
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Reset(nRows, nCols: Integer);
var i: Integer;
begin
  Clear;
  Capacity := nRows;
  for i := 0 to nRows-1 do
    Add(nCols);
end;
{------------------------------------------------------------------------------}
function TRVTableRows.Empty: Boolean;
begin
  Result := (Count=0) or (Items[0].Count=0);
end;
{------------------------------------------------------------------------------}
function TRVTableRows.Add(nCols: Integer): TRVTableRow;
begin
  Result := TRVTableRow.Create(nCols, Self, FMainRVData);
  inherited Add(Result);
end;
{------------------------------------------------------------------------------}
function TRVTableRows.Insert(Index, nCols: Integer): TRVTableRow;
begin
  Result := TRVTableRow.Create(nCols, Self, FMainRVData);
  inherited Insert(Index, Result);
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.InsertPointer(Index: Integer; Item: TRVTableRow);
begin
  Item.FRows := Self;
  inherited Insert(Index, Item);
end;
{------------------------------------------------------------------------------}
function TRVTableRows.Get(Index: Integer): TRVTableRow;
begin
  Result := TRVTableRow(inherited Get(Index));
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Put(Index: Integer; const Value: TRVTableRow);
begin
  Value.FRows := Self;
  inherited Put(Index, Value);
end;
{------------------------------------------------------------------------------}
function TRVTableRows.GetBestWidth(TopRow, LeftCol, ColSpan, RowSpan: Integer): Integer;
var r,c,bw: Integer;
begin
  Result := 0;
  for r := TopRow to TopRow+RowSpan-1 do begin
    bw := 0;
    with Items[r] do
      for c := LeftCol to LeftCol+ColSpan-1 do
        if Items[c]<>nil then begin
          if Items[c].BestWidth>0 then begin
            if bw>=0 then
              inc(bw,Items[c].BestWidth);
            end
          else if Items[c].BestWidth<0 then begin
            if bw>0 then
              bw := 0;
            inc(bw,Items[c].BestWidth);
          end;
        end;
    if ((Result>=0) and ((bw<0) or (bw>Result))) or
       ((Result<0) and (bw<Result)) then
      Result := bw;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.IsEmptyCols(TopRow, LeftCol, ColSpan, RowSpan, LeftCol2, ColSpan2: Integer): Boolean;
var r,c: Integer;
    empty: Boolean;
begin
  Result := True;
  for r := TopRow to TopRow+RowSpan-1 do begin
    empty := True;
    for c := LeftCol to LeftCol+ColSpan-1 do
      if Items[r].Items[c]<>nil then begin
        empty := False;
        break;
      end;
    if empty then
      for c := LeftCol2 to LeftCol2+ColSpan2-1 do
        if Items[r].Items[c]<>nil then begin
          empty := False;
          break;
        end;
    if empty then
      exit;
  end;
  Result := False;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.GetColCount: Integer;
begin
  if Count>0 then
    Result := Items[0].Count
  else
    Result := 0;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.IsEmptyRows(TopRow, LeftCol, ColSpan, RowSpan, TopRow2, RowSpan2: Integer): Boolean;
var r,c: Integer;
    empty: Boolean;
begin
  Result := True;
  for c := LeftCol to LeftCol+ColSpan-1 do begin
    empty := True;
    for r := TopRow to TopRow+RowSpan-1 do
      if Items[r].Items[c]<>nil then begin
        empty := False;
        break;
      end;
    if empty then
      for r := TopRow2 to TopRow2+RowSpan2-1 do
        if Items[r].Items[c]<>nil then begin
          empty := False;
          break;
        end;
    if empty then
      exit;
  end;
  Result := False;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.UnmergeCell(Row, Col: Integer; UnmergeRows, UnmergeCols: Boolean);
var MainCell: TRVTableCellData;
    ItemNo: Integer;
begin
  MainCell := Items[Row].Items[Col];
  if (MainCell=nil) or
     (((MainCell.ColSpan=1) or not UnmergeCols) and
      ((MainCell.RowSpan=1) or not UnmergeRows)) then
    exit;
  ItemNo := FTable.GetEditorItemNoForUndo;
  Do_BeforeUnmergeCell(ItemNo, Row, Col, UnmergeRows, UnmergeCols);
  Do_UnmergeCell(ItemNo, Row, Col, UnmergeRows, UnmergeCols);  
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.UnmergeCells(TopRow, LeftCol, ColSpan, RowSpan: Integer;
                                    UnmergeRows, UnmergeCols: Boolean);
var r,c: Integer;
begin
  for r := TopRow+RowSpan-1 downto TopRow do
    for c := LeftCol+ColSpan-1 downto LeftCol do
      UnmergeCell(r, c, UnmergeRows, UnmergeCols);
end;
{------------------------------------------------------------------------------}
function TRVTableRows.StartMergeCells(TopRow, LeftCol: Integer;
                                      var ColSpan, RowSpan: Integer): Boolean;
var MainCell: TRVTableCellData;
begin
  Result := False;
  if TopRow+RowSpan>Count then
    RowSpan := Count - TopRow;
  if LeftCol+ColSpan>Items[TopRow].Count then
    ColSpan := Items[TopRow].Count - LeftCol;
  if (RowSpan<2) and (ColSpan<2) then exit;
  if (RowSpan<1) or (ColSpan<1) then
    raise ERichViewError.Create(errMerge);
  MainCell := Items[TopRow].Items[LeftCol];
  Result := (MainCell<>nil) and
            (
            (MainCell.RowSpan<RowSpan)
            or
            (MainCell.ColSpan<ColSpan)
            );
end;
{------------------------------------------------------------------------------}
function TRVTableRows.CanMergeCells(TopRow, LeftCol, ColSpan, RowSpan: Integer;
                                    AllowMergeRC: Boolean): Boolean;
var r,c,mr,mc: Integer;
    MainCell, Cell: TRVTableCellData;
    Flag: Boolean;
    {...............................................}
begin
  Result := StartMergeCells(TopRow, LeftCol, ColSpan, RowSpan);
  if not Result then
    exit;
  Result := False;
  MainCell := GetMainCell(TopRow, LeftCol, mr, mc);
  Flag := False;
  for r := TopRow to TopRow+RowSpan-1 do
    with Items[r] do
      for c := LeftCol to LeftCol+ColSpan-1 do begin
        if Items[c]<>nil then begin
          Cell := Items[c];
          mr := r;
          mc := c;
          end
        else
          Cell := GetMainCell(r,c,mr,mc);
        if (mr<TopRow) or (mc<LeftCol) or
           (mr+Cell.RowSpan>TopRow+RowSpan) or
           (mc+Cell.ColSpan>LeftCol+ColSpan) then
          exit;
        if Cell<>MainCell then
          Flag := True;
      end;
  if AllowMergeRC then begin
    Result := Flag;
    exit;
  end;
  // testing if merging removes row
  if (ColSpan>1) and
     IsEmptyRows(0, LeftCol+1, ColSpan-1, TopRow, TopRow+RowSpan, Count-(TopRow+RowSpan)) then
    exit;
  if (RowSpan>1) and
     IsEmptyCols(TopRow+1, 0, LeftCol, RowSpan-1, LeftCol+ColSpan, Items[0].Count-(LeftCol+ColSpan)) then
    exit;
  Result := True;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.MergeCells(TopRow, LeftCol, ColSpan, RowSpan: Integer;
                                  AllowMergeRC, ChangeBestWidth: Boolean);
var ItemNo: Integer;
    ui: TRVUndoInfo;
begin
  if not StartMergeCells(TopRow, LeftCol, ColSpan, RowSpan) then
    exit;
  if not CanMergeCells(TopRow, LeftCol, ColSpan, RowSpan, AllowMergeRC) then begin
    FMainRVData.Beep;
    exit;
  end;
  ItemNo := FTable.GetEditorItemNoForUndo;
  ui := Do_BeforeMergeCells(ItemNo, TopRow,LeftCol,ColSpan,RowSpan);
  Do_MergeCells(ItemNo, TopRow,LeftCol,ColSpan,RowSpan, ui, ChangeBestWidth);
end;
{------------------------------------------------------------------------------}
function TRVTableRows.GetMinColWidth(Col: Integer; sad: PRVScreenAndDevice;
                                     Canvas: TCanvas): Integer;
var i,w: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    if (Items[i].Items[Col]<>nil) and
       (Items[i].Items[Col].ColSpan<=1) then begin
      w := Items[i].Items[Col].GetMinWidth(sad, Canvas);
      if (w>Result) then
        Result := w;
    end;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.IsPercentWidthColumn(Col: Integer): Boolean;
var i,w,a,b: Integer;
    Cell:  TRVTableCellData;
begin
  for i := 0 to Count-1 do
    if (Items[i].Items[Col]<>nil) then begin
      if (Items[i].Items[Col].ColSpan<=1) then begin
        w := Items[i].Items[Col].BestWidth;
        if (w<0) then begin
          Result := True;
          exit;
        end;
      end
      end
    else begin
      Cell := GetMainCell(i,Col,a,b);
      if (Cell.BestWidth<0)  and
         (
         (Col=Items[i].Count-1) or
         (Items[i].Items[Col+1]<>nil) or
         (Cell<>GetMainCell(i,Col+1,a,b))
         ) then begin
        Result := True;
        exit;
      end;
    end;
  Result := False;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.GetPercentColWidth(Col, TableWidth: Integer): Integer;
var i,w: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    if (Items[i].Items[Col]<>nil) and
       (Items[i].Items[Col].ColSpan<=1) then begin
      w := Items[i].Items[Col].BestWidth;
      if (w<0) then begin
        w := -w*TableWidth div 100;
        if (w>Result) then
          Result := w;
      end;
    end;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.GetPixelColWidth(Col: Integer): Integer;
var i,w: Integer;
begin
  Result := 0;
  for i := 0 to Count-1 do
    if (Items[i].Items[Col]<>nil) and
       (Items[i].Items[Col].ColSpan<=1) then begin
      w := Items[i].Items[Col].BestWidth;
      if w<0 then begin
        Result := 0;
        exit;
      end;
      if (w>Result) then
        Result := w;
    end;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.GetMainCell(ARow, ACol: Integer; var MRow, MCol: Integer): TRVTableCellData;
begin
  MRow := ARow;
  MCol := ACol;
  if Items[MRow][MCol]<>nil then begin
    Result := Items[MRow][MCol];
    exit;
  end;
  while True do
    with Items[MRow] do begin
      while (Items[MCol]=nil) and (MCol>0) do
        dec(MCol);
      if (Items[MCol]<>nil) and
         (Items[MCol].ColSpan>ACol-MCol) and
         (Items[MCol].RowSpan>ARow-MRow) then begin
        Result := Items[MCol];
        exit;
      end;
      MCol := ACol;
      dec(MRow);
      //Assert(MRow>=0);
    end;
  Result := Items[MRow].Items[MCol];
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.MovingFromUndoList(Row, Col, ColSpan,
  RowSpan: Integer);
var r,c: Integer;
begin
  for r := Row to Row+RowSpan-1 do
    with Items[r] do
      for c := Col to Col+ColSpan-1 do
        if (Items[c]<>nil) then begin
          Items[c].FList := Self.Items[r];
          Items[c].MovingFromUndoList;
        end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.MovingToUndoList(Row, Col, ColSpan,
  RowSpan: Integer; AContainerUndoItem: TRVUndoInfo);
var r,c: Integer;
begin
  for r := Row to Row+RowSpan-1 do
    with Items[r] do
      for c := Col to Col+ColSpan-1 do
        if (Items[c]<>nil) then begin
          Items[c].MovingToUndoList(AContainerUndoItem);
          Items[c].FList := nil;
        end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_BeforeInsertRows(ItemNo,Row,Count: Integer);
var ui: TRVUndoInsertTableRows;
begin
  if (rvtsInserted in FTable.FState) and FTable.IsInEditor then begin
    if ItemNo=-1 then
      ItemNo := FTable.GetMyItemNo;
    ui := TRVUndoInsertTableRows(
             AddTableUndoInfo(TRVEditRVData(FMainRVData), TRVUndoInsertTableRows,
                             ItemNo, True, True));
    if ui<>nil then begin
      ui.Row          := Row;
      ui.Count        := Count;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_InsertRows(Row, Count: Integer);
var r: Integer;
begin
  for r := 0 to Count-1 do
    Insert(Row, GetColCount);
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_UnInsertRows(Row, Count: Integer);
var r: Integer;
begin
  for r := 0 to Count-1 do
    Delete(Row);
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_BeforeInsertCell(ItemNo, Row, Col: Integer);
var ui: TRVUndoInsertTableCell;
begin
  if (rvtsInserted in FTable.FState) and FTable.IsInEditor then begin
    if ItemNo=-1 then
      ItemNo := FTable.GetMyItemNo;
    ui := TRVUndoInsertTableCell(
             AddTableUndoInfo(TRVEditRVData(FMainRVData), TRVUndoInsertTableCell,
                             ItemNo, True, True));
    if ui<>nil then begin
      ui.Row        := Row;
      ui.Col        := Col;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_BeforeSpreadOverEmptyCells(ItemNo, Row, Col,
  ColSpan: Integer);
var ui: TRVUndoSpreadOverEmptyCells;
begin
  if (rvtsInserted in FTable.FState) and FTable.IsInEditor then begin
    ui := TRVUndoSpreadOverEmptyCells(
             AddTableUndoInfo(TRVEditRVData(FMainRVData), TRVUndoSpreadOverEmptyCells,
                             ItemNo, True, True));
    if ui<>nil then begin
      ui.Row := Row;
      ui.Col := Col;
      ui.ColSpan := ColSpan;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_SpreadOverEmptyCells(Row, Col, ColSpan: Integer);
var c: Integer;
begin
  Items[Row][Col].FColSpan := ColSpan;
  for c := Col+1 to Col+ColSpan-1 do begin
    Items[Row][c].Free;
    Items[Row][c] := nil;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_UnSpreadOverEmptyCells(Row, Col, ColSpan: Integer);
var c: Integer;
begin
  Items[Row][Col].FColSpan := 1;
  for c := Col+1 to Col+ColSpan-1 do begin
    //Assert(Items[Row][c]=nil);
    Items[Row][c] := TRVTableCellData.Create(Items[Row]);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_SetSpan(ItemNo, Row, Col, Span: Integer;
  IsColSpan: Boolean);
var ui: TRVUndoSpan;
begin
  if (IsColSpan     and (Items[Row][Col].FColSpan = Span)) or
     (not IsColSpan and (Items[Row][Col].FRowSpan = Span)) then
    exit;
  if (rvtsInserted in FTable.FState) and FTable.IsInEditor then begin
    if ItemNo=-1 then
      ItemNo := FTable.GetMyItemNo;
    ui := TRVUndoSpan(
             AddTableUndoInfo(TRVEditRVData(FMainRVData), TRVUndoSpan,
                             ItemNo, True, IsColSpan));
    if ui<>nil then begin
      ui.Row := Row;
      ui.Col := Col;
      ui.IsColSpan := IsColSpan;
      if IsColSpan then
        ui.OldSpan := Items[Row][Col].ColSpan
      else
        ui.OldSpan := Items[Row][Col].RowSpan;
    end;
  end;
  if IsColSpan then
    Items[Row][Col].FColSpan := Span
  else
    Items[Row][Col].FRowSpan := Span;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_BeforeFreeEmptyCells(ItemNo, Row, Col, ColSpan,
  RowSpan: Integer);
var ui: TRVUndoFreeEmptyCell;
begin
  if (rvtsInserted in FTable.FState) and FTable.IsInEditor then begin
    ui := TRVUndoFreeEmptyCell(
             AddTableUndoInfo(TRVEditRVData(FMainRVData), TRVUndoFreeEmptyCell,
                             ItemNo, True, True));
    if ui<>nil then begin
      ui.Row := Row;
      ui.Col := Col;
      ui.RowSpan := RowSpan;
      ui.ColSpan := ColSpan;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_FreeEmptyCells(Row, Col, ColSpan,
  RowSpan: Integer);
var r,c: Integer;
begin
  for r := Row to Row+RowSpan-1 do
    for c := Col to Col+ColSpan-1 do begin
      Items[r][c].Free;
      Items[r][c] := nil;
    end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_UnFreeEmptyCells(Row, Col, ColSpan,
  RowSpan: Integer);
var r,c: Integer;
begin
  for r := Row to Row+RowSpan-1 do
    for c := Col to Col+ColSpan-1 do begin
      //Assert(Items[r][c]=nil);
      Items[r][c] := TRVTableCellData.Create(Items[r]);
    end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_BeforeInsertEmptyCells(ItemNo, Row, Col, ColCount,
  RowCount: Integer);
var ui: TRVUndoInsertEmptyCell;
begin
  if (rvtsInserted in FTable.FState) and FTable.IsInEditor and (ColCount>0) and (RowCount>0) then begin
    ui := TRVUndoInsertEmptyCell(
             AddTableUndoInfo(TRVEditRVData(FMainRVData), TRVUndoInsertEmptyCell,
                             ItemNo, True, True));
    if ui<>nil then begin
      ui.Row := Row;
      ui.Col := Col;
      ui.RowCount := RowCount;
      ui.ColCount := ColCount;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_InsertEmptyCells(Row, Col, ColCount,
  RowCount: Integer);
var r,c: Integer;
begin
  for r := Row to Row+RowCount-1 do
    for c := Col to Col+ColCount-1 do begin
      Items[r].InsertEmpty(Col);
    end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_UnInsertEmptyCells(Row, Col, ColCount,
  RowCount: Integer);
var r,c: Integer;
begin
  for r := Row to Row+RowCount-1 do
    for c := Col to Col+ColCount-1 do begin
      Items[r].Delete(Col);
    end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_BeforeSplitCellHorz(ItemNo,Row,Col,Row2: Integer; DecreaseHeight: Boolean);
var ui: TRVUndoCellSpitHorz;
begin
  if (rvtsInserted in FTable.FState) and FTable.IsInEditor then begin
    ui := TRVUndoCellSpitHorz(
             AddTableUndoInfo(TRVEditRVData(FMainRVData), TRVUndoCellSpitHorz,
                             ItemNo, True, False));
    if ui<>nil then begin
      ui.Row := Row;
      ui.Col := Col;
      ui.Row2 := Row2;
      ui.DecreaseHeight := DecreaseHeight;
      ui.OldBestHeight := Items[Row][Col].BestHeight;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_SplitCellHorz(Row,Col,Row2: Integer; DecreaseHeight: Boolean);
var MainCell,NewCell: TRVTableCellData;
begin
  MainCell := Items[Row][Col];
  NewCell := TRVTableCellData.Create(Items[Row2]);
  NewCell.AssignAttributesFrom(MainCell,True,1,1);
  NewCell.FColSpan := MainCell.ColSpan;
  NewCell.FRowSpan := Row+MainCell.RowSpan-Row2;
  NewCell.FBestHeight := MainCell.BestHeight * NewCell.RowSpan div MainCell.RowSpan;
  Items[Row2][Col] := NewCell;
  if DecreaseHeight then
    MainCell.BestHeight := MainCell.BestHeight * (MainCell.RowSpan-NewCell.RowSpan) div MainCell.RowSpan;
  MainCell.FRowSpan := MainCell.RowSpan-NewCell.RowSpan;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_UnSplitCellHorz(Row,Col,Row2: Integer; OldBestHeight: Integer);
var MainCell,NewCell: TRVTableCellData;
begin
  MainCell := Items[Row][Col];
  NewCell  := Items[Row2][Col];
  MainCell.BestHeight := OldBestHeight;
  inc(MainCell.FRowSpan, NewCell.FRowSpan);
  NewCell.Free;
  Items[Row2][Col] := nil;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_BeforeSplitCellVert(ItemNo, Row, Col,
  Col2: Integer; DecreaseWidth: Boolean);
var ui: TRVUndoCellSpitVert;
begin
  if (rvtsInserted in FTable.FState) and FTable.IsInEditor then begin
    ui := TRVUndoCellSpitVert(
             AddTableUndoInfo(TRVEditRVData(FMainRVData), TRVUndoCellSpitVert,
                             ItemNo, True, True));
    if ui<>nil then begin
      ui.Row := Row;
      ui.Col := Col;
      ui.Col2 := Col2;
      ui.DecreaseWidth := DecreaseWidth;
      ui.OldBestWidth := Items[Row][Col].BestWidth;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_SplitCellVert(Row, Col, Col2: Integer;
  DecreaseWidth: Boolean);
var MainCell,NewCell: TRVTableCellData;  
begin
  MainCell := Items[Row][Col];
  NewCell := TRVTableCellData.Create(Items[Row]);
  NewCell.AssignAttributesFrom(MainCell,True,1,1);
  NewCell.FRowSpan := MainCell.RowSpan;
  NewCell.FColSpan := Col+MainCell.ColSpan-Col2;
  NewCell.FBestWidth := MainCell.BestWidth * NewCell.ColSpan div MainCell.ColSpan;
  Items[Row][Col2] := NewCell;
  if DecreaseWidth then
    MainCell.BestWidth := MainCell.BestWidth * (MainCell.ColSpan-NewCell.ColSpan) div MainCell.ColSpan;
  MainCell.FColSpan := MainCell.ColSpan-NewCell.ColSpan;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_UnSplitCellVert(Row, Col, Col2,
  OldBestWidth: Integer);
var MainCell,NewCell: TRVTableCellData;
begin
  MainCell := Items[Row][Col];
  NewCell  := Items[Row][Col2];
  MainCell.BestWidth := OldBestWidth;
  inc(MainCell.FColSpan, NewCell.FColSpan);
  NewCell.Free;
  Items[Row][Col2] := nil;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.Do_BeforeDeleteRows(ItemNo, Row, Count: Integer): TRVUndoInfo;
var ui: TRVUndoDeleteRows;
    i: Integer;
begin
  ui := nil;
  if (rvtsInserted in FTable.FState) and FTable.IsInEditor then begin
    ui := TRVUndoDeleteRows(AddTableUndoInfo(TRVEditRVData(FMainRVData), TRVUndoDeleteRows,
                            ItemNo, True, True));
    if ui<>nil then begin
      ui.Row          := Row;
      ui.Count        := Count;
      ui.Rows := TList.Create;
      for i := 0 to Count-1 do
        ui.Rows.Add(FTable.Rows[Row+i]);
    end;
  end;
  Result := ui;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_DeleteRows(ItemNo, Row, Count: Integer; ui: TRVUndoInfo);
begin
  if ItemNo=-1 then
    while Count>0 do begin
      Delete(Row);
      dec(Count);
    end
  else begin
    MovingToUndoList(Row,0,Items[Row].Count,Count,ui);
    while Count>0 do begin
      DeleteAsPointer(Row);
      dec(Count);
    end
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_UnDeleteRows(Row: Integer; RowList: TList);
var i,c: Integer;
begin
  for i := RowList.Count-1 downto 0 do begin
    InsertPointer(Row, RowList[i]);
    for c := 0 to Items[Row].Count-1 do
      if Items[Row][c]<>nil then
        Items[Row][c].FList := Items[Row];
  end;
  MovingFromUndoList(Row,0,Items[Row].Count,RowList.Count);
  RowList.Clear;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.Do_BeforeDeleteCols(ItemNo, Col, Count: Integer): TRVUndoInfo;
var ui: TRVUndoDeleteCols;
    r,c: Integer;
begin
  ui := nil;
  if (rvtsInserted in FTable.FState) and FTable.IsInEditor then begin
    ui := TRVUndoDeleteCols(AddTableUndoInfo(TRVEditRVData(FMainRVData), TRVUndoDeleteCols,
                            ItemNo, True, True));
    if ui<>nil then begin
      ui.Col          := Col;
      ui.Count        := Count;
      ui.CellsList := TList.Create;
      for r := 0 to Self.Count-1 do
        for c := Count-1 downto 0 do
          ui.CellsList.Add(Items[r][Col+c]);
    end;
  end;
  Result := ui;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_DeleteCols(ItemNo, Col, Count: Integer; ui: TRVUndoInfo);
var r,c: Integer;
begin
  if ItemNo=-1 then
    for r := 0 to Self.Count-1 do
      for c := 0 to Count-1 do
        Items[r].Delete(Col)
  else begin
    MovingToUndoList(0,Col,Count,Self.Count, ui);
    for r := 0 to Self.Count-1 do
      for c := 0 to Count-1 do
        Items[r].DeleteAsPointer(Col);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_UnDeleteCols(Col: Integer; CellList: TList);
var r,c,idx,DelCount: Integer;
begin
  idx := 0;
  DelCount := CellList.Count div Self.Count;
  for r := 0 to Self.Count-1 do
    for c := 0 to DelCount-1 do begin
      Items[r].InsertPointer(Col, CellList.Items[idx]);
      inc(idx);
    end;
  MovingFromUndoList(0,Col,DelCount,Self.Count);
  CellList.Clear;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.Do_BeforeClearCells(ItemNo: Integer; RowList, ColList: TRVIntegerList;
                                           var CellsList: TList): TRVUndoInfo;
var ui: TRVUndoCellsClear;
begin
  ui := nil;
  CellsList := nil;
  if (rvtsInserted in FTable.FState) and FTable.IsInEditor then begin
    ui := TRVUndoCellsClear(AddTableUndoInfo(TRVEditRVData(FMainRVData), TRVUndoCellsClear,
                            ItemNo, True, True));
    if ui<>nil then begin
      ui.RowList := TRVIntegerList.CreateCopy(RowList);
      ui.ColList := TRVIntegerList.CreateCopy(ColList);
      ui.CellsList := TList.Create;
      CellsList  := ui.CellsList;
    end;
  end;
  Result := ui;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_ClearCells(CellsList: TList; RowList, ColList: TRVIntegerList;
                                     ui: TRVUndoInfo);
var i: Integer;
    Cell,Cell2: TRVTableCellData;
    ChosenCell: TCustomRVData;
begin
  for i := 0 to RowList.Count-1 do begin
    Cell := Items[RowList[i]][ColList[i]];
    if CellsList<>nil then begin
       if (FMainRVData is TCustomRVFormattedData) and (TCustomRVFormattedData(FMainRVData).GetChosenRVData<>nil) then
         ChosenCell := TCustomRVFormattedData(FMainRVData).GetChosenRVData.GetSourceRVData
       else
         ChosenCell := nil;
       CellsList.Add(Cell);
       Cell.Deselect(nil, True);
       Cell.State := Cell.State - [rvstCompletelySelected];
       Cell.MovingToUndoList(ui);
       Cell2 := TRVTableCellData.Create(Items[RowList[i]]);
       Items[RowList[i]][ColList[i]] :=  Cell2;
       Cell2.AssignAttributesFrom(Cell,True,1,1);
       Cell2.AssignSizeFrom(Cell);
       Cell2.FColSpan := Cell.ColSpan;
       Cell2.FRowSpan := Cell.RowSpan;
       if ChosenCell=Cell then
         TCustomRVFormattedData(FMainRVData).SilentReplaceChosenRVData(Cell2);
       end
    else begin
      Cell.Clear;
      Cell.AddNL('',0,0);
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_UnClearCells(CellsList: TList;RowList, ColList: TRVIntegerList);
var i: Integer;
begin
  for i := 0 to RowList.Count-1 do begin
    Items[RowList[i]][ColList[i]].Free;
    Items[RowList[i]][ColList[i]] := TRVTableCellData(CellsList.Items[i]);
    TRVTableCellData(CellsList.Items[i]).MovingFromUndoList;    
  end;
  CellsList.Clear;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.Do_BeforeMergeCells(ItemNo, Row, Col, ColSpan, RowSpan: Integer):TRVUndoInfo;
var ui: TRVUndoMerge;
    r,c: Integer;
begin
  ui := nil;
  if (rvtsInserted in FTable.FState) and FTable.IsInEditor then begin
    ui := TRVUndoMerge(AddTableUndoInfo(TRVEditRVData(FMainRVData), TRVUndoMerge,
                               ItemNo, True, True));
    if ui<>nil then begin
      ui.Row          := Row;
      ui.Col          := Col;
      ui.OldColSpan   := Items[Row][Col].ColSpan;
      ui.OldRowSpan   := Items[Row][Col].RowSpan;
      ui.NewColSpan   := ColSpan;
      ui.NewRowSpan   := RowSpan;
      ui.OldBestWidth := Items[Row][Col].BestWidth;
      ui.MergedItemsList := TRVList.Create;
      for r := Row to Row+RowSpan-1 do
        for c := Col to Col+ColSpan-1 do
          if (Items[r][c]<>nil) and
             (Items[r][c]<>Items[Row][Col]) then
          ui.MergedItemsList.Add(TRVUndoMergeItem.Create(FTable,r,c,Row,Col));
    end;
  end;
  Result := ui;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_MergeCells(ItemNo, Row, Col, ColSpan, RowSpan: Integer;
                                     ui: TRVUndoInfo; ChangeBestWidth: Boolean);
var r,c: Integer;
    Vampire, Victim: TRVTableCellData;
begin
  Vampire := Items[Row][Col];
  if ChangeBestWidth then
    Vampire.FBestWidth := GetBestWidth(Row, Col, ColSpan, RowSpan);
  for r := Row to Row+RowSpan-1 do
    for c := Col to Col+ColSpan-1 do begin
      Victim := Items[r].Items[c];
      if (Victim<>Vampire) and (Victim<>nil) then begin
        if Victim.HasData(False) then
          Vampire.DrainFrom(Victim);
        if ItemNo=-1 then
          Victim.Free
        else begin
          Victim.MovingToUndoList(ui);
          Victim.State := Victim.State - [rvstCompletelySelected];
        end;
        Items[r][c] := nil;
      end;
    end;
  Vampire.FColSpan := ColSpan;
  Vampire.FRowSpan := RowSpan;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_UndoMergeCells(ItemNo, Row, Col, OldColSpan, OldRowSpan: Integer;
                                         MergedItemsList: TRVList;
                                         OldBestWidth: TRVHTMLLength);
var i,j,ItemIndex: Integer;
    Vampire: TRVTableCellData;
    UnmergeData: TRVUndoMergeItem;
begin
  Vampire := Items[Row][Col];
  Vampire.FBestWidth := OldBestWidth;
  ItemIndex := Vampire.Items.Count;
  for i := MergedItemsList.Count-1 downto 0 do begin
    UnmergeData := TRVUndoMergeItem(MergedItemsList.Items[i]);
    UnmergeData.Cell.MovingFromUndoList;
    Items[UnmergeData.Row][UnmergeData.Col] := UnmergeData.Cell;
    UnmergeData.Cell.FList := Items[UnmergeData.Row];
    dec(ItemIndex,UnmergeData.ItemCount);
    for j := ItemIndex to ItemIndex+UnmergeData.ItemCount-1 do
      UnmergeData.Cell.AddItem(Vampire.Items[j], TCustomRVItemInfo(Vampire.Items.Objects[j]));
  end;
  while Vampire.Items.Count>ItemIndex do
    Vampire.Items.Delete(Vampire.Items.Count-1);
  Vampire.FColSpan := OldColSpan;
  Vampire.FRowSpan := OldRowSpan;
  MergedItemsList.Clear;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_BeforeUnmergeCell(ItemNo, Row, Col: Integer;
  UnmergeRows, UnmergeCols: Boolean);
var ui: TRVUndoUnmerge;
begin
  if (rvtsInserted in FTable.FState) and FTable.IsInEditor then begin
    ui := TRVUndoUnmerge(AddTableUndoInfo(TRVEditRVData(FMainRVData), TRVUndoUnmerge,
                               ItemNo, True, True));
    if ui<>nil then begin
      ui.Row          := Row;
      ui.Col          := Col;
      ui.OldColSpan   := Items[Row][Col].ColSpan;
      ui.OldRowSpan   := Items[Row][Col].RowSpan;
      ui.UnmergeCols   := UnmergeCols;
      ui.UnmergeRows   := UnmergeRows;
      ui.OldBestWidth := Items[Row][Col].BestWidth;
      ui.OldBestHeight := Items[Row][Col].BestHeight;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_UnmergeCell(ItemNo, Row, Col: Integer;
  UnmergeRows, UnmergeCols: Boolean);
var r,w,h, RowSpan: Integer;
    MainCell: TRVTableCellData;
    {.................................................}
    procedure DoUnmergeCols(Row, RowSpan: Integer);
    var c: Integer;
        MainCell: TRVTableCellData;
    begin
      MainCell := Items[Row][Col];
      for c := Col+1 to Col+MainCell.ColSpan-1 do begin
        Items[Row][c] := TRVTableCellData.Create(Items[Row]);
        Items[Row][c].BestWidth := w;
        Items[Row][c].FRowSpan := RowSpan;
        Items[Row][c].AssignAttributesFrom(MainCell,False,0,0);
    end;
    MainCell.FColSpan := 1;
    MainCell.BestWidth := w;
    end;
    {.................................................}
begin
  MainCell := Items[Row][Col];
  if UnmergeCols then begin
    w := MainCell.BestWidth div MainCell.ColSpan;
    if w=0 then
      if MainCell.BestWidth>0 then
        w := 1
      else if MainCell.BestWidth<0 then
        w := -1;
    end
  else
    w := MainCell.BestWidth;

  if UnmergeRows then begin
    h := MainCell.BestHeight div MainCell.RowSpan;
    RowSpan := MainCell.RowSpan;
    for r := Row+1 to Row+RowSpan-1 do begin
      Items[r][Col] := TRVTableCellData.Create(Items[r]);
      Items[r][Col].AssignAttributesFrom( MainCell,True,1,MainCell.RowSpan);
      Items[r][Col].FColSpan := MainCell.ColSpan;
    end;
    MainCell.FRowSpan := 1;
    MainCell.BestHeight := h;
    if UnmergeCols then
      for r := Row to Row+RowSpan-1 do
        DoUnmergeCols(r, 1);
    end
  else if UnmergeCols then
    DoUnmergeCols(Row, MainCell.RowSpan);
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.Do_UndoUnmergeCell(ItemNo, Row, Col: Integer;
                                          OldColSpan, OldRowSpan: Integer;
                                          OldBestWidth: TRVHTMLLength;
                                          OldBestHeight: Integer);
var r,c: Integer;
    MainCell: TRVTableCellData;
begin
  MainCell := Items[Row][Col];
  if OldRowSpan<>MainCell.RowSpan then
    for r := Row+1 to Row+OldRowSpan-1 do
      for c := Col to Col+OldColSpan-1 do begin
        Items[r][c].Free;
        Items[r][c] := nil;
      end;
  if OldColSpan<>MainCell.ColSpan then
    for c := Col+1 to Col+OldColSpan-1 do begin
      Items[Row][c].Free;
      Items[Row][c] := nil;
    end;
  MainCell.FColSpan := OldColSpan;
  MainCell.FRowSpan := OldRowSpan;
  MainCell.FBestWidth  := OldBestWidth;
  MainCell.FBestHeight := OldBestHeight;  
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.InsertCols(Index, Count, CopyIndex: Integer;
                                  DivideWidths: Boolean);
var r,c,mr,mc: Integer;
    ItemNo: Integer;
    cell: TRVTableCellData;
    ForbiddenRows: TRVIntegerList;
begin
  if (Index<0) or (Index>Items[0].Count) or
     (CopyIndex<-1) or (CopyIndex>=Items[0].Count) then
    raise ERichViewError.Create(errInvalidIndex);
  ItemNo := FTable.GetEditorItemNoForUndo;
  if ItemNo<>-1 then
    TRVEditRVData(FMainRVData).Do_ItemModifyTerminator(ItemNo, True);
  ForbiddenRows := TRVIntegerList.CreateEx(Self.Count,0);
  try
    if Index<>Items[0].Count then begin
      r := Self.Count-1;
      while r>=0 do begin
        cell := GetMainCell(r,Index,mr,mc);
        if (mc<Index) and (mc+cell.ColSpan-1>=Index) then begin
          for c := mr to mr+cell.RowSpan-1 do
            ForbiddenRows[c] := 1;
          Do_SetSpan(ItemNo, mr, mc, cell.FColSpan+Count, True);
        end;
        r := mr-1;
      end;
    end;
    r := 0;
    while r<Self.Count do begin
      if ForbiddenRows[r]=0 then begin
        if CopyIndex<>-1 then
          cell := Items[r][CopyIndex]
        else
          cell := nil;
        if (cell<>nil) and DivideWidths then
          FTable.SetCellBestWidth_(ItemNo, cell.BestWidth * cell.ColSpan div (cell.ColSpan+Count),r,CopyIndex);
        for c := 0 to Count-1 do begin
          Do_BeforeInsertCell(ItemNo,r,Index);
          Items[r].Insert(Index);
          if cell<>nil then begin
            FTable.AssignCellAttributes(ItemNo, r, Index, cell, True, Cell.ColSpan,1);
            Do_SetSpan(ItemNo, r, Index, cell.RowSpan, False);
          end;
        end;
        if cell=nil then
          inc(r)
        else begin
          Do_BeforeInsertEmptyCells(ItemNo,r+1,Index,Count,Cell.RowSpan-1);
          Do_InsertEmptyCells(r+1,Index,Count,Cell.RowSpan-1);
          inc(r,cell.RowSpan);
        end;
        end
      else begin
        Do_BeforeInsertEmptyCells(ItemNo,r,Index,Count,1);
        Do_InsertEmptyCells(r,Index,Count,1);
        inc(r);
      end;
    end;
  finally
    ForbiddenRows.Free;
    if ItemNo<>-1 then
      TRVEditRVData(FMainRVData).Do_ItemModifyTerminator(ItemNo, False);    
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.InsertRows(Index, Count, CopyIndex: Integer;
                                  DivideHeights: Boolean);
var r,c,mr,mc: Integer;
    cell: TRVTableCellData;
    ForbiddenCols: TRVIntegerList;
    ItemNo: Integer;
    ColCount: Integer;
begin
  if (Index<0) or (Index>Self.Count) or
     (CopyIndex<-1) or (CopyIndex>=Self.Count) then
    raise ERichViewError.Create(errInvalidIndex);
  ItemNo := FTable.GetEditorItemNoForUndo;
  if ItemNo<>-1 then
    TRVEditRVData(FMainRVData).Do_ItemModifyTerminator(ItemNo, True);
  ColCount := GetColCount;
  ForbiddenCols := TRVIntegerList.CreateEx(ColCount,0);
  try
    if Index<>Self.Count then begin
      c := ColCount-1;
      while c>=0 do begin
        cell := GetMainCell(Index,c, mr,mc);
        if (mr<Index) and (mr+cell.RowSpan-1>=Index) then begin
          for r := mc to mc+cell.ColSpan-1 do
            ForbiddenCols[r] := 1;
          Do_SetSpan(ItemNo, mr, mc, cell.FRowSpan+Count, False);
        end;
        c := mc-1;
      end;
    end;
    if CopyIndex>=Index then
      inc(CopyIndex,Count);
    Do_BeforeInsertRows(ItemNo, Index,Count);
    Do_InsertRows(Index,Count);
    c := 0;
    while c<ColCount do
      if ForbiddenCols[c]=0 then begin
        if CopyIndex<>-1 then
          cell := Items[CopyIndex][c]
        else
          cell := nil;
        if cell<>nil then begin
          if DivideHeights then
            FTable.SetCellBestHeight_(ItemNo, cell.BestHeight * cell.RowSpan div (cell.RowSpan+Count), CopyIndex,c);
          for r := 0 to Count-1 do begin
            FTable.AssignCellAttributes(ItemNo, Index+r, c, cell, True, 1, cell.RowSpan);
            Do_BeforeSpreadOverEmptyCells(ItemNo, Index+r, c, cell.ColSpan);
            Do_SpreadOverEmptyCells(Index+r, c, cell.ColSpan);
          end;
          inc(c,cell.ColSpan);
          end
        else
          inc(c);
        end
      else begin
        Do_BeforeFreeEmptyCells(ItemNo, Index, c, 1, Count);
        Do_FreeEmptyCells(Index, c, 1, Count);
        inc(c);
      end;
  finally
    ForbiddenCols.Free;
    if ItemNo<>-1 then
      TRVEditRVData(FMainRVData).Do_ItemModifyTerminator(ItemNo, False);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.DeleteRows(Index, Count: Integer; DecreaseHeight: Boolean);
var c,mr,mc,span: Integer;
    cell: TRVTableCellData;
    ItemNo: Integer;
    ui: TRVUndoInfo;
begin
  if (Index<0) or (Index>=Self.Count) or (Count<=0) or
     ((Index=0) and (Count>Self.Count)) then
    raise ERichViewError.Create(errInvalidIndex);
  if Count+Index>Self.Count then
    Count := Self.Count-Index;

  ItemNo := FTable.GetEditorItemNoForUndo;
  if ItemNo<>-1 then
    TRVEditRVData(FMainRVData).Do_ItemModifyTerminator(ItemNo, True);
  try
    c := Items[0].Count-1;
    while c>=0 do begin
      cell := GetMainCell(Index+Count-1,c,mr,mc);
      if mr+cell.RowSpan-1>Index+Count-1 then
        if mr>=Index then begin
          Do_BeforeSplitCellHorz(ItemNo,mr,mc,Index+Count,DecreaseHeight);
          Do_SplitCellHorz(mr,mc,Index+Count,DecreaseHeight);
        end;
      c := mc-1;
    end;
    c := Items[0].Count-1;
    while c>=0 do begin
      cell := GetMainCell(Index,c,mr,mc);
      if mr<Index then begin
        if mr+cell.RowSpan-1<=Index+Count-1 then
          span := (Index-mr)
        else
          span := cell.RowSpan-Count;
        if DecreaseHeight then
          FTable.SetCellBestHeight_(ItemNo, cell.BestHeight * span div cell.RowSpan, mr, mc);
        Do_SetSpan(ItemNo,mr,mc,span,False);
      end;
      c := mc-1;
    end;
    ui := Do_BeforeDeleteRows(ItemNo,Index,Count);
    Do_DeleteRows(ItemNo,Index,Count, ui);
  finally
    if ItemNo<>-1 then
      TRVEditRVData(FMainRVData).Do_ItemModifyTerminator(ItemNo, False);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableRows.DeleteCols(Index, Count: Integer; DecreaseWidth: Boolean);
var r,mr,mc,span: Integer;
    cell: TRVTableCellData;
    ItemNo: Integer;
    ui: TRVUndoInfo;
begin
  if (Index<0) or (Index>=Items[0].Count) or (Count<=0) or
     ((Index=0) and (Count>Items[0].Count)) then
    raise ERichViewError.Create(errInvalidIndex);
  if Count+Index>Items[0].Count then
    Count := Items[0].Count-Index;
  ItemNo := FTable.GetEditorItemNoForUndo;
  if ItemNo<>-1 then
    TRVEditRVData(FMainRVData).Do_ItemModifyTerminator(ItemNo, True);
  try
    r := Self.Count-1;
    while r>=0 do begin
      cell := GetMainCell(r,Index+Count-1,mr,mc);
      if mc+cell.ColSpan-1>Index+Count-1 then
        if mc>=Index then begin
          Do_BeforeSplitCellVert(ItemNo,mr,mc,Index+Count,DecreaseWidth);
          Do_SplitCellVert(mr,mc,Index+Count,DecreaseWidth);
        end;
      r := mr-1;
    end;
    r := Self.Count-1;
    while r>=0 do begin
      cell := GetMainCell(r,Index,mr,mc);
      if mc<Index then begin
        if mc+cell.ColSpan-1<=Index+Count-1 then
          span := (Index-mc)
        else
          span := cell.ColSpan-Count;
        if DecreaseWidth then
          FTable.SetCellBestWidth_(ItemNo, cell.BestWidth * span div cell.ColSpan, mr, mc);
        Do_SetSpan(ItemNo,mr,mc,span,True);
      end;
      r := mr-1;
    end;
    ui := Do_BeforeDeleteCols(ItemNo,Index,Count);
    Do_DeleteCols(ItemNo,Index,Count, ui);
  finally
    if ItemNo<>-1 then
      TRVEditRVData(FMainRVData).Do_ItemModifyTerminator(ItemNo, False);
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.SplitCellVertically(Row, Col, ColCount: Integer): Integer;
var OldColSpan, r, c,mr,mc,mr2,mc2,
    NewColCount, NewColSpan: Integer;
    MainCell,Cell: TRVTableCellData;
begin
  Result := 0;
  if ColCount<=1 then
    exit;
  MainCell := Items[Row][Col];
  if MainCell=nil then
    exit;
  OldColSpan := MainCell.ColSpan;
  if OldColSpan>1 then
    UnmergeCell(Row,Col, False, True);
  if OldColSpan<ColCount then begin
    Result := ColCount-OldColSpan;
    for c := Col+OldColSpan-1 downto Col do begin
      if (ColCount=0) or (OldColSpan=0) then
        exit;
      NewColCount := ColCount div OldColSpan;
      InsertCols(c, NewColCount-1,c, True);
      r := Count-1;
      while r>=0 do begin
        GetMainCell(r,c,mr,mc);
        if (mr<>Row) then begin
          Cell := GetMainCell(mr,c+NewColCount-1,mr2,mc2);
          inc(mc2,Cell.ColSpan);
          MergeCells(mr,mc, mc2-mc,Cell.RowSpan, True, True);
        end;
        r := mr-1;
      end;
      dec(ColCount,NewColCount);
      dec(OldColSpan);
    end;
    end
  else if OldColSpan>ColCount then begin
    c := Col+OldColSpan-1;
    while c>=Col do begin
      if (ColCount=0) or (OldColSpan=0) then
        exit;
      NewColSpan := OldColSpan div ColCount;
      dec(c,NewColSpan-1);
      MergeCells(Row, c, NewColSpan, Items[Row][c].RowSpan, True, True);
      dec(OldColSpan, NewColSpan);
      dec(ColCount);
      dec(c);
    end;
    //Assert(ColCount=0);
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.SplitCellHorizontally(Row, Col, RowCount: Integer): Integer;
var OldRowSpan, r, c,mr,mc,mr2,mc2,
    NewRowCount, NewRowSpan: Integer;
    MainCell,Cell: TRVTableCellData;
begin
  Result := 0;
  if RowCount<=1 then
    exit;
  MainCell := Items[Row][Col];
  if MainCell=nil then
    exit;
  OldRowSpan := MainCell.RowSpan;
  if OldRowSpan>1 then
    UnmergeCell(Row,Col, True, False);
  if OldRowSpan<RowCount then begin
    Result := RowCount-OldRowSpan;
    for r := Row+OldRowSpan-1 downto Row do begin
      if (RowCount=0) or (OldRowSpan=0) then
        exit;
      NewRowCount := RowCount div OldRowSpan;
      InsertRows(r, NewRowCount-1,r, True);
      c := Items[0].Count-1;
      while c>=0 do begin
        GetMainCell(r,c,mr,mc);
        if (mc<>Col) then begin
          Cell := GetMainCell(r+NewRowCount-1,mc,mr2,mc2);
          inc(mr2,Cell.RowSpan);
          MergeCells(mr,mc, Cell.ColSpan,mr2-mr, True, True);
        end;
        c := mc-1;
      end;
      dec(RowCount,NewRowCount);
      dec(OldRowSpan);
    end;
    end
  else if OldRowSpan>RowCount then begin
    r := Row+OldRowSpan-1;
    while r>=Row do begin
      if (RowCount=0) or (OldRowSpan=0) then
        exit;
      NewRowSpan := OldRowSpan div RowCount;
      dec(r,NewRowSpan-1);
      MergeCells(r, Col, Items[r][Col].ColSpan, NewRowSpan,True, True);
      dec(OldRowSpan, NewRowSpan);
      dec(RowCount);
      dec(r);
    end;
    //Assert(RowCount=0);
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.SplitCellsHorizontally(TopRow, LeftCol, ColSpan,
  RowSpan, RowCount: Integer): Integer;
var r,c,r2,rowadded,Span: Integer;
begin
  Result := 0;
  for r := TopRow+RowSpan-1 downto TopRow do begin
    rowadded := 0;
    for c := LeftCol to LeftCol+ColSpan-1 do
      if Items[r][c]<>nil then begin
        if rowadded>0 then begin
          Span := 0;
          for r2 := r+1 to r+rowadded do
            if Items[r2][c]<>nil then
              inc(Span);
          if (Span>0) then begin
            MergeCells(r,c, Items[r][c].ColSpan,Items[r][c].RowSpan+Span,True,True);
          end;
        end;
        inc(rowadded, SplitCellHorizontally(r,c,RowCount));
      end;
    inc(Result,rowadded);
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableRows.SplitCellsVertically(TopRow, LeftCol, ColSpan,
  RowSpan, ColCount: Integer): Integer;
var r,c,c2,coladded,Span: Integer;
begin
  Result := 0;
  for c := LeftCol+ColSpan-1 downto LeftCol do begin
    coladded := 0;
    for r := TopRow to TopRow+RowSpan-1 do
      if Items[r][c]<>nil then begin
        if coladded>0 then begin
          Span := 0;
          for c2 := c+1 to c+coladded do
            if Items[r][c2]<>nil then
              inc(Span);
          if (Span>0) then begin
            MergeCells(r,c, Items[r][c].ColSpan+Span,Items[r][c].RowSpan,True,True);
          end;
        end;
        inc(coladded, SplitCellVertically(r,c,ColCount));
      end;
    inc(Result,coladded);
  end;
end;
{========================= TRVTableItemFormattingInfo =========================}
constructor TRVTableItemFormattingInfo.Create(CreateRows:Boolean);
begin
  inherited Create;
  ColWidths  := TRVIntegerList.Create;
  ColStarts  := TRVIntegerList.Create;
  RowHeights := TRVIntegerList.Create;
  RowStarts  := TRVIntegerList.Create;
  if CreateRows then
    Rows := TRVList.Create;
end;
{------------------------------------------------------------------------------}
destructor TRVTableItemFormattingInfo.Destroy;
begin
  ColWidths.Free;
  ColStarts.Free;
  RowHeights.Free;
  RowStarts.Free;
  Rows.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemFormattingInfo.Clear;
begin
  ColWidths.Clear;
  ColStarts.Clear;
  RowHeights.Clear;
  RowStarts.Clear;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemFormattingInfo.QuickClear;
begin
  ColWidths.Count := 0;
  ColStarts.Count := 0;
  RowHeights.Count := 0;
  RowStarts.Count := 0;
end;
{========================== TRVTableItemInfo ==================================}
constructor TRVTableItemInfo.Create(RVData: TPersistent);
begin
  inherited Create(RVData);
  Init(1,1, TCustomRVData(RVData));
  Include(FState, rvtsJustCreated);
end;
{------------------------------------------------------------------------------}
constructor TRVTableItemInfo.CreateEx(nRows, nCols: Integer; AMainRVData: TCustomRVData);
begin
  inherited Create(AMainRVData);
  Init(nRows,nCols, AMainRVData);
  Include(FState, rvtsJustCreated);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.Init(nRows, nCols: Integer; AMainRVData: TCustomRVData);
begin
  Fmt := TRVTableItemFormattingInfo.Create(False);
  StyleNo := rvsTable;
  FRows := TRVTableRows.Create(nRows, nCols, TCustomRVFormattedData(AMainRVData), Self);
  FBorderWidth     := 0;
  FCellBorderWidth := 0;
  FBorderStyle     := rvtbRaised;
  FCellBorderStyle := rvtbLowered;
  FHRuleColor      := clWindowText;
  FVRuleColor      := clWindowText;
  FBorderColor     := clWindowText;
  FCellBorderColor := clWindowText;
  FCellBorderLightColor := clBtnHighlight;
  FBorderLightColor := clBtnHighlight;
  FCellVSpacing    := 2;
  FCellHSpacing    := 2;
  FBorderVSpacing  := 2;
  FBorderHSpacing  := 2;
  FCellPadding     := 1;
  FHRuleWidth      := 0;
  FVRuleWidth      := 0;
  FColor       := clWindow;
  FSelStartCol := -1;
  FSelStartRow := -1;
  FSelColOffs := 0;
  FSelRowOffs := 0;
  FOptions    := RVTABLEDEFAULTOPTIONS;
  FPrintOptions := RVTABLEDEFAULTPRINTOPTIONS;
  Screen.Cursors[crRVSelectCol] := LoadCursor(hInstance, 'RV_SELECTCOL_CURSOR');
  Screen.Cursors[crRVSelectRow] := LoadCursor(hInstance, 'RV_SELECTROW_CURSOR');
  TextRowSeparator := #13#10;
  TextColSeparator := #13#10;
  FocusedCellRow := -1;
  FocusedCellCol := -1;
  ChosenCellRow := -1;
  ChosenCellCol := -1;
end;
{------------------------------------------------------------------------------}
destructor TRVTableItemInfo.Destroy;
begin
  ClearTemporal;
  FRows.Free;
  Fmt.Free;
  FBackground.Free;
  inherited Destroy;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.ClearTemporal;
begin
  if FRows.FMainRVData<>nil then begin
    DestroyInplace(False);
    if FRows.FMainRVData is TCustomRVFormattedData then
      with TCustomRVFormattedData(FRows.FMainRVData) do begin
        UnAssignXorDrawing(Self.XorDrawing);
        ReleaseMouseCapture(Self);
      end;
  end;
  Fmt.Clear;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetHeight: Integer;
begin
  Result := Fmt.FHeight;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetWidth: Integer;
begin
  Result := Fmt.FWidth;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetCells(Row, Col: Integer): TRVTableCellData;
begin
  Result := FRows.Items[Row].Items[Col];
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCells(Row, Col: Integer;
  const Value: TRVTableCellData);
begin
  if FRows.Items[Row].Items[Col]<>nil then
    FRows.Items[Row].Items[Col].Assign(Value);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.PaintFullWidth(Left, Right, Top: Integer;
  Canvas: TCanvas; State: TRVItemDrawStates; Style: TRVStyle; const ClipRect: TRect;
  dli: TRVDrawLineInfo);
begin
  PaintTo(Left, Right, Top, 0, Rows.Count, Canvas, State, Style, Fmt, False,
    ClipRect, rvcmColor, nil)
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.PaintTo(Left, Right, Top, FromRow, RowCount: Integer; Canvas: TCanvas;
  State: TRVItemDrawStates; Style: TRVStyle; Fmt: TRVTableItemFormattingInfo;
  UseHeadingRowCount: Boolean; const ClipRect: TRect; ColorMode: TRVColorMode;
  RVData: TCustomPrintableRVData);
var r,c,l,t,h: Integer;
    VRules, HRules: Boolean;
    DH, DV, DHHalf, DVHalf: Integer;
    CBOffs,ROffs: Integer;
    SelColor: TColor;
    {...............................................}
    procedure DrawVLine(X,Y1,Y2,Width: Integer);
    begin
      dec(X, Width div 2);
      while Width>0 do begin
        Canvas.MoveTo(X,Y1);
        Canvas.LineTo(X,Y2);
        inc(X);
        dec(Width);
      end;
    end;
    {...............................................}
    procedure DrawVLine2(X,Y1,Y2,Width: Integer);
    begin
      if Y1<ClipRect.Top-1 then
        Y1 := ClipRect.Top-1;
      if Y2>ClipRect.Bottom+1 then
        Y2 := ClipRect.Bottom+1;
      dec(X, Width div 2);
      while Width>0 do begin
        Canvas.MoveTo(X,Y1);
        Canvas.LineTo(X,Y2);
        inc(X);
        dec(Width);
      end;
    end;
    {...............................................}
    procedure DrawHLine(Y,X1,X2,Width: Integer);
    begin
      dec(Y, Width div 2);
      while Width>0 do begin
        Canvas.MoveTo(X1,Y);
        Canvas.LineTo(X2,Y);
        inc(Y);
        dec(Width);
      end;
    end;
    {...............................................}
    function GetExtraPrnVOffs(CellHeight,RowHeight: Integer; VAlign: TRVCellVAlign): Integer;
    begin
      case VAlign of
        rvcMiddle:
          Result := (RowHeight-CellHeight) div 2;
        rvcBottom:
          Result := (RowHeight-CellHeight);
        else
          Result := 0;
      end;
    end;
    {...............................................}
    procedure DrawCell(r,c,h, DV, DH, l,t: Integer;
                       Canvas: TCanvas;
                       const ClipRect, BorderRect: TRect);
    var cw,ch, i,idx: Integer;
        Cell: TRVTableCellData;
        CanDrawBitmappedBack,WasSelected: Boolean;
        CellColor: TColor;
        CPD: TCellPtblRVData;
        BColor, BLColor: TColor;
        CellRect, CellRect2: TRect;
        {$IFNDEF RICHVIEWCBDEF3}
        pt: TPoint;
        {$ENDIF}
    begin
      Cell := Cells[r,c];
      if Fmt.Rows=nil then begin
        idx := 0;
        ch  := Cell.FHeight;
        cw  := Cell.FWidth;
        end
      else begin
        idx := r*Rows[0].Count+c;
        if Fmt.Rows[idx]<>nil then begin
          ch := TCellPtblRVData(Fmt.Rows[idx]).Height+GetDevY(CellPadding*2);
          cw := TCellPtblRVData(Fmt.Rows[idx]).Width+GetDevX(CellPadding*2);
          end
        else begin
          ch := h + GetDevY(CellPadding*2);
          for i := 1 to Cell.RowSpan-1 do
            inc(ch, Fmt.RowHeights[r+i]+DV);
          cw := Fmt.ColWidths[c] + GetDevX(CellPadding*2);
          for i := 1 to Cell.ColSpan-1 do
            inc(cw, Fmt.ColWidths[c+i]+DH);
        end;
      end;
      CellColor := clNone;
      if rvidsSelected in State then begin
        WasSelected := rvstCompletelySelected in Cells[r,c].State;
        CellColor := SelColor;
        Include(Cells[r,c].State, rvstCompletelySelected);
        CanDrawBitmappedBack := False;
        end
      else begin
        CanDrawBitmappedBack := not ((Fmt.Rows=nil) and IsCellSelected(r,c));
        if not CanDrawBitmappedBack then
          CellColor := SelColor;
        WasSelected := True;
      end;
      if not CanDrawBitmappedBack then
        CanDrawBitmappedBack := SelColor=clNone;
      if CellColor=clNone then
        CellColor := Cells[r,c].Color;
      if (Fmt.Rows=nil) and (CellColor=clNone) and (FInplaceEditor<>nil) and
         (TRVTableInplaceEdit(FInplaceEditor).FCell=Cell) and
         TRVTableInplaceEdit(FInplaceEditor).NormalScrolling then
        CellColor := clWindow;
      if (CellColor<>clNone) or (Cell.FBackground<>nil) then begin
        if (Fmt.Rows<>nil) and (rvtoWhiteBackground in PrintOptions) then
          CellColor := clWhite;
        CellColor := RV_GetBackColor(CellColor, ColorMode);
        Canvas.Brush.Color := CellColor;
        Canvas.Pen.Color   := CellColor;
        CellRect := Bounds(l, t, cw, ch);
        if rvtoCellBelowBorders in Options then
          InflateRect(CellRect,GetDevX(CellBorderWidth),GetDevY(CellBorderWidth));
        if (Fmt.Rows=nil) and (FInplaceEditor<>nil) and
           (TRVTableInplaceEdit(FInplaceEditor).FCell=Cell) and
           TRVTableInplaceEdit(FInplaceEditor).NormalScrolling then begin
          SetBkColor(Canvas.Handle, ColorToRGB(CellColor));
          Canvas.Brush.Style := bsFDiagonal;
          Canvas.Brush.Color := CellBorderColor;
          SetBrushOrgEx(Canvas.Handle, l,t,
          {$IFNDEF RICHVIEWCBDEF3}pt{$ELSE}nil{$ENDIF});
          SetBkMode(Canvas.Handle, OPAQUE);
          SetBkColor(Canvas.Handle, ColorToRGB(CellColor));
          CanDrawBitmappedBack := False;
        end;
        if CanDrawBitmappedBack and (Cell.FBackground<>nil) then begin
          if Fmt.Rows=nil then
            Cell.FBackground.Draw(Canvas, CellRect, 0, 0, CellRect.Left, CellRect.Top,
              CellRect.Right-CellRect.Left, CellRect.Bottom-CellRect.Top, CellColor, True)
          else begin
            CellRect2 := CellRect;
            OffsetRect(CellRect2,-CellRect2.Left,-CellRect2.Top);
            OffsetRect(CellRect2,BorderRect.Left,BorderRect.Top);
            Cell.FBackground.Print(Canvas, CellRect, CellRect2, cursad^, CellColor,
              rvidsPreview in State, FRows.FMainRVData.GetRVLogPalette, RVData, 1);
          end;
          end
        else if CellColor<>clNone then
          Canvas.FillRect(CellRect);
        Canvas.Brush.Style := bsSolid;
      end;
      if Fmt.Rows=nil then begin
        Cell.PaintTo(Canvas, ClipRect);
        {$IFDEF RVDEBUGTABLE}
        if Cell.BestWidth>0 then begin
          Canvas.Pen.Color := clRed;
          Canvas.MoveTo(l+CellPadding,t);
          Canvas.LineTo(l+CellPadding+Cells[r,c].BestWidth,t);
        end;
        Canvas.Font.Color := clBlack;
        Canvas.Font.Name := 'Small Fonts';
        Canvas.Font.Size := 6;
        Canvas.TextOut(l,t, IntToStr(Cells[r,c].BestWidth)+','+IntToStr(Cells[r,c].BestHeight));
        {$ENDIF}
        end
      else begin
        if Fmt.Rows[idx]<>nil then begin
          CPD := TCellPtblRVData(Fmt.Rows[idx]);
          CPD.Left := Left;
          CPD.Top  := Top+GetExtraPrnVOffs(CPD.DocumentHeight, CPD.Height, Cells[r,c].GetRealVAlign);
          FPrintCell := Cell;
          FPrintCellRect := Bounds(l,t,cw,ch);
          OffsetRect(FPrintCellRect,-BorderRect.Left,-BorderRect.Top);
          RV_RectToScreen(FPrintCellRect, cursad^);
          if (FInplaceEditor<>nil) and (TRVTableInplaceEdit(FInplaceEditor).FCell=Cells[r,c]) then begin
            CPD.FSourceDataForPrinting := TRVTableInplaceEdit(FInplaceEditor).FCell;
            CPD.DrawPage(1, Canvas, rvidsPreview in State, rvidsPreviewCorrection in State);
            CPD.FSourceDataForPrinting := Cells[r,c];
            end
          else
            CPD.DrawPage(1, Canvas, rvidsPreview in State, rvidsPreviewCorrection in State);
          FPrintCell := nil;
        end;
      end;
      if Cell.BorderColor <> clNone then
        BColor := Cell.BorderColor
      else
        BColor := CellBorderColor;
      if Cell.BorderLightColor <> clNone then
        BLColor := Cell.BorderLightColor
      else
        BLColor := CellBorderLightColor;
      DrawBorder(Canvas, l-CBOffs,t-CBOffs,l+cw+CBOffs,t+ch+CBOffs,
                 FCellBorderWidth, BLColor, BColor, Color, CellBorderStyle,
                 (rvtsEditMode in FState) and not (rvtoHideGridLines in Options), rvidsPrinting in State,
                 ClipRect, Cell.VisibleBorders, r, c, ColorMode);
      if not WasSelected then
        Exclude(Cell.State, rvstCompletelySelected);
    end;
    {...............................................}
var StartRow, LastRow, StartCol,RuleWidth:Integer;
    mr,mc, hrc: Integer;
    Clr: TColor;
    BorderRect, BorderRect2: TRect;
begin
  if TRVScroller(Rows.FMainRVData.GetRootData.GetParentControl).FocusedEx then
    SelColor := Rows.FMainRVData.GetRVStyle.SelColor
  else
    SelColor := Rows.FMainRVData.GetRVStyle.InactiveSelColor;
  MyClientLeft := Left;
  MyClientTop  := Top;
  VRules := (FVRuleWidth<>0) and (FVRuleColor<>clNone);
  HRules := (FHRuleWidth<>0) and (FHRuleColor<>clNone);
  DH := GetDevX(CellPadding+CellHSpacing+CellPadding+FCellBorderWidth*2);
  DV := GetDevY(CellPadding+CellVSpacing+CellPadding+FCellBorderWidth*2);
  CBOffs  := GetDevY(FCellBorderWidth);
  if Fmt.Rows=nil then begin
    StartRow := GetCrossed(ClipRect.Top-Top-GetDevY(BorderWidth),Fmt.RowStarts);
    LastRow  := Rows.Count-1;
    BorderRect := Bounds(Left,Top,Fmt.FWidth,Fmt.FHeight);
    hrc := 0;
    end
  else begin
    if UseHeadingRowCount then begin
      hrc := HeadingRowCount;
      if hrc>FromRow then
        hrc := FromRow;
      end
    else
      hrc := 0;
    StartRow := FromRow;
    LastRow  := FromRow+RowCount-1;
    BorderRect := Bounds(Left,Top,Fmt.FWidth,
                  Fmt.RowStarts[0]+Fmt.FHeight-Fmt.RowStarts[Rows.Count-1]-Fmt.RowHeights[Rows.Count-1]);
    if LastRow>=StartRow then
      inc(BorderRect.Bottom, Fmt.RowStarts[LastRow]+Fmt.RowHeights[LastRow]-Fmt.RowStarts[StartRow]);
    if hrc>0 then begin
      inc(BorderRect.Bottom, Fmt.RowStarts[hrc-1]+Fmt.RowHeights[hrc-1]-Fmt.RowStarts[0]);
      if LastRow>=FromRow then
        inc(BorderRect.Bottom, Fmt.RowStarts[hrc]-(Fmt.RowStarts[hrc-1]+Fmt.RowHeights[hrc-1]));
    end;
    //dec(Top, Fmt.RowStarts[FromRow]-Fmt.RowStarts[0]);
  end;
  if FBackground<>nil then begin
    if FColor=clNone then
      Clr := clNone
    else if (Fmt.Rows<>nil) and (rvtoWhiteBackground in PrintOptions) then
      Clr := clWhite
    else
      Clr := RV_GetBackColor(FColor, ColorMode);
    IntersectRect(BorderRect2, BorderRect, ClipRect);
    if Fmt.Rows=nil then
      FBackground.Draw(Canvas, BorderRect2, 0, 0, Left, Top, Fmt.FWidth, Fmt.FHeight, Clr, True)
    else
      FBackground.Print(Canvas, BorderRect2, BorderRect, cursad^, Clr, rvidsPreview in State,
        FRows.FMainRVData.GetRVLogPalette, RVData, 0);
    end
  else if FColor<>clNone then begin
    if (Fmt.Rows<>nil) and (rvtoWhiteBackground in PrintOptions) then
      Canvas.Brush.Color := clWhite
    else
      Canvas.Brush.Color := RV_GetBackColor(FColor, ColorMode);
    IntersectRect(BorderRect2, BorderRect, ClipRect);
    Canvas.FillRect(BorderRect2);
  end;
  Canvas.Pen.Width := 1;
  Canvas.Pen.Style := psSolid;
  if VRules then begin
    RuleWidth := GetDevX(FVRuleWidth);
    DHHalf := (GetDevX(CellHSpacing+FCellBorderWidth*2)+1) div 2;
    StartCol := GetCrossed(ClipRect.Left-Left-GetDevX(BorderWidth+CellHSpacing+BorderHSpacing),Fmt.ColStarts);
    if HRules and FHOutermostRule then
      ROffs := GetDevX(BorderWidth+(BorderVSpacing{-FHRuleWidth}) div 2)
    else
      ROffs := 0;
    Canvas.Pen.Color := FVRuleColor;
    if FVOutermostRule and (StartCol=0) then begin
      l := Left+GetDevX(BorderWidth+BorderHSpacing div 2);
      DrawVLine2(l, BorderRect.Top+ROffs, BorderRect.Bottom-ROffs, RuleWidth);
    end;
    for c := StartCol to Rows[0].Count-2 do begin
      l := Left+Fmt.ColStarts[c+1]-DHHalf;
      if l-RuleWidth>ClipRect.Right then
        break;
      DrawVLine2(l, BorderRect.Top+ROffs, BorderRect.Bottom-ROffs, RuleWidth);
    end;
    if FVOutermostRule then begin
      l := Left+Fmt.FWidth-GetDevX(BorderWidth+BorderHSpacing div 2)-1;
      if GetDevX(FVRuleWidth) mod 2 = 0 then
        inc(l);
      DrawVLine2(l, BorderRect.Top+ROffs, BorderRect.Bottom-ROffs, RuleWidth);
    end;
  end;
  if HRules then begin
    RuleWidth := GetDevY(FHRuleWidth);
    DVHalf := (GetDevY(CellVSpacing+FCellBorderWidth*2)+1) div 2;
    if VRules and FVOutermostRule then
      ROffs := GetDevY(BorderWidth+(BorderHSpacing{-FVRuleWidth}) div 2)
    else
      ROffs := 0;
    Canvas.Pen.Color := FHRuleColor;
    if FHOutermostRule and ((StartRow<2) or (Fmt.Rows<>nil)) then begin
      t := BorderRect.Top+GetDevY(BorderWidth+BorderVSpacing div 2);
      DrawHLine(t, Left+ROffs, Left+Fmt.FWidth-ROffs, RuleWidth);
    end;
    if hrc>0 then
      for r := 0 to hrc-2 do begin
        t := Top+Fmt.RowStarts[r+1]-DVHalf;
        if t-RuleWidth>ClipRect.Bottom then
          break;
        DrawHLine(t, Left+ROffs, Left+Fmt.FWidth-ROffs, RuleWidth);
      end;
    if LastRow>=StartRow then begin
      if Fmt.Rows<>nil then
        dec(Top, Fmt.RowStarts[FromRow]-Fmt.RowStarts[hrc]);
      StartCol := StartRow-1;
      if StartCol<0 then
        StartCol := 0;
      for r := StartCol to LastRow-1 do begin
        t := Top+Fmt.RowStarts[r+1]-DVHalf;
        if t-RuleWidth>ClipRect.Bottom then
          break;
        DrawHLine(t, Left+ROffs, Left+Fmt.FWidth-ROffs, RuleWidth);
      end;
      if Fmt.Rows<>nil then
        inc(Top, Fmt.RowStarts[FromRow]-Fmt.RowStarts[hrc]);
    end;
    if FHOutermostRule then begin
      t := BorderRect.Bottom-GetDevY(BorderWidth+BorderVSpacing div 2)-1;
      if GetDevY(FHRuleWidth) mod 2 = 0 then
        inc(t);
      DrawHLine(t, Left+ROffs, Left+Fmt.FWidth-ROffs, RuleWidth);
    end;
  end;
  DrawBorder(Canvas, BorderRect.Left,BorderRect.Top,
    BorderRect.Right,BorderRect.Bottom, BorderWidth, BorderLightColor, BorderColor,
    TCustomRVFormattedData(Rows.FMainRVData).GetColor, BorderStyle,
    (rvtsEditMode in FState) and not (rvtoHideGridLines in Options),
    rvidsPrinting in State,  ClipRect, nil, -1, -1, ColorMode);
  if (StartRow>0) and (StartRow<Rows.Count) then begin
    c := Rows[StartRow].Count-1;
    while c>=0 do begin
      Rows.GetMainCell(StartRow,c,mr,mc);
      if mr<StartRow then
        DrawCell(mr, mc, Fmt.RowHeights[mr], DV, DH,
                 Fmt.ColStarts[mc]+Left,Fmt.RowStarts[mr]+Top,
                 Canvas, ClipRect, BorderRect);
      c := mc-1;
    end;
  end;
  if hrc>0 then
    for r := 0 to hrc-1 do begin
      t := Fmt.RowStarts[r]+Top;
      if t-CBOffs>ClipRect.Bottom then
        break;
      h := Fmt.RowHeights[r];
      for c := 0 to Rows[r].Count-1 do begin
        l := Fmt.ColStarts[c]+Left;
        if l-CBOffs>ClipRect.Right then break;
        if Cells[r,c]<>nil then
          DrawCell(r,c,h, DV, DH, l,t, Canvas, ClipRect, BorderRect);
      end;
    end;
  if LastRow>=StartRow then begin
    if Fmt.Rows<>nil then
      dec(Top, Fmt.RowStarts[FromRow]-Fmt.RowStarts[hrc]);
    for r := StartRow to LastRow do begin
      t := Fmt.RowStarts[r]+Top;
      if t-CBOffs>ClipRect.Bottom then
        break;
      h := Fmt.RowHeights[r];
      for c := 0 to Rows[r].Count-1 do begin
        l := Fmt.ColStarts[c]+Left;
        if l-CBOffs>ClipRect.Right then break;
        if Cells[r,c]<>nil then
          DrawCell(r,c,h, DV, DH, l,t, Canvas, ClipRect, BorderRect);
      end;
    end;
  end;
  Canvas.Pen.Width := 1;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.DrawBackgroundUnderCell(Canvas: TCanvas;
  Cell: TRVTableCellData; const Rect: TRect);
var r: TRect;
    Clr: TColor;
begin
  Clr := GetTableColor(False);
  if FBackground<>nil then
    FBackground.Draw(Canvas, Rect, 0, 0,
      -Cell.Left, -Cell.Top-Cell.GetExtraVOffs, Width, Height, Clr, False)
  else if Clr<>clNone then begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := Clr;
    r := Rect;
    OffsetRect(r, -r.Left, -r.Top);
    Canvas.FillRect(r);
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.IsSemiTransparentBackground: Boolean;
begin
  Result := (FColor=clNone) and (FBackground<>nil) and FBackground.IsSemitransparent; 
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetHorzExtra: Integer;
begin
  Result := GetDevX(((BorderWidth+BorderHSpacing)*2+(CellHSpacing*(Rows[0].Count-1)))+
            Rows[0].Count*CellBorderWidth*2);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetMinWidth(sad: PRVScreenAndDevice; Canvas: TCanvas; RVData: TPersistent): Integer;
var ColWidths: TRVIntegerList;
    MinColsWidth: Integer;
    {.................................................}
    procedure CalcMinWidths;
    var c,w,w2: Integer;
    begin
      MinColsWidth := 0;
      for c := 0 to Rows[0].Count-1 do begin
        w := Rows.GetMinColWidth(c,sad,Canvas);
        if BestWidth=0 then begin
          w2 := GetDevX(Rows.GetPixelColWidth(c));
          if w2>w then
            w := w2;
        end;
        inc(MinColsWidth, w);
        ColWidths.Add(w);
      end;
    end;
    {.................................................}
    procedure ExpandCols(Row,Col, Width: Integer);
    var c, oldw, pureoldw, DH, UnsizedCount: Integer;
        Cell: TRVTableCellData;
    begin
      inc(Width, GetDevX(CellPadding*2));
      Cell := Cells[Row,Col];
      DH := GetDevX(CellPadding+CellHSpacing+CellPadding+FCellBorderWidth*2);
      oldw := GetDevX(-CellHSpacing-FCellBorderWidth*2);
      pureoldw := 0;
      UnsizedCount := 0;
      for c := Col to Col+Cell.ColSpan-1 do begin
        inc(oldw, ColWidths[c]+DH);
        inc(pureoldw, ColWidths[c]+GetDevX(CellPadding*2));
        inc(UnsizedCount);
      end;
      if (oldw<Width) and (UnsizedCount<>0) then begin
        DH := Width-oldw;
        inc(DH, pureoldw);
        for c := Col to Col+Cell.ColSpan-1 do begin
            oldw := ColWidths[c]+GetDevX(CellPadding*2);
            Width := MulDiv(oldw,DH,pureoldw);
            ColWidths[c] := Width-GetDevX(CellPadding*2);
            dec(pureoldw, oldw);
            dec(DH,Width);
            inc(MinColsWidth, Width-oldw);
        end;
      end;
    end;
    {.................................................}
    procedure CalcMinWidthsSpan;
    var r,c,w,w2: Integer;
    begin
      for r := 0 to Rows.Count-1 do
        with Rows[r] do
          for c := 0 to Rows[r].Count-1 do
            if (Items[c]<>nil) and (Items[c].ColSpan>1) then begin
              w := Items[c].GetMinWidth(sad,Canvas);
              if BestWidth=0 then begin
                w2 := GetDevX(Items[c].BestWidth);
                if w2>w then
                  w := w2;
              end;
              ExpandCols(r,c, w);
            end;
    end;
    {.................................................}
var oldsad: PRVScreenAndDevice;
begin
  oldsad := cursad;
  cursad := sad;
  try
    if Rows.Empty then begin
      Result := GetDevX((BorderWidth+BorderHSpacing)*2);
      cursad := oldsad;
      exit;
    end;
    Result := GetHorzExtra+GetDevX(CellPadding*Rows[0].Count*2);
    ColWidths := TRVIntegerList.Create;
    try
      CalcMinWidths;
      CalcMinWidthsSpan;
    finally
      ColWidths.Free;
    end;
    inc(Result, MinColsWidth);
    if GetDevX(BestWidth)>Result then
      Result := GetDevX(BestWidth);
  finally
    cursad := oldsad;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.IsFixedWidthTable: Boolean;
var r,c,mr,mc: Integer;
    cell: TRVTableCellData;
    definedwidth: Boolean;
begin
  Result := False;
  if Rows.IsEmptyRows(0,0,Rows[0].Count,Rows.Count,0,0) then
    exit;
  for c := 0 to Rows[0].Count-1 do begin
    definedwidth := False;
    for r := 0 to Rows.Count-1 do begin
      cell := Rows.GetMainCell(r,c,mr,mc);
      if cell.BestWidth<0 then
        exit;
      if (mc+cell.ColSpan-1=c) and (cell.BestWidth>0) then
        definedwidth := True;
    end;
    if not definedwidth then
      exit;
  end;
  Result := True;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.OnDocWidthChange(DocWidth: Integer; dli: TRVDrawLineInfo;
  Printing: Boolean; Canvas: TCanvas; RVData: TPersistent; sad: PRVScreenAndDevice;
  var HShift, Desc: Integer; NoCaching: Boolean);
begin
  HShift := 0;
  Desc   := 0;
  if not Printing then begin
    if (rvtsModified in FState) then begin
      Exclude(FState, rvtsModified);
      Fmt.FWidth := 0;
    end;
    InternalOnDocWidthChange(DocWidth, Fmt, Canvas, NoCaching)
    end
  else
    try
      cursad := @(TRVTablePrintInfo(dli).sad);
      InternalOnDocWidthChange(DocWidth, (dli as TRVTablePrintInfo).Fmt, Canvas, False);
      dli.Width := TRVTablePrintInfo(dli).Fmt.FWidth;
      dli.Height := TRVTablePrintInfo(dli).Fmt.FHeight;
    finally
      cursad := nil;
    end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InternalOnDocWidthChange(DocWidth: Integer;
                                                    Fmt: TRVTableItemFormattingInfo;
                                                    Canvas: TCanvas;
                                                    NoCaching: Boolean);
var r,c,w,w2,w3, PureWidth, SumColWidth, MinColsWidth, ExtraWidth: Integer;
    ColsWithAssignedWidths: TRVIntegerList;
    FixedWidthCount,AssignedCount: Integer;
    FixedWidth: Boolean;
    {.................................................}
    procedure CalcMinWidths;
    var c,w: Integer;
    begin
      MinColsWidth := 0;
      for c := 0 to Rows[0].Count-1 do begin
        w := Rows.GetMinColWidth(c,cursad,Canvas);
        inc(MinColsWidth, w);
        Fmt.ColWidths.Add(w);
      end;
    end;
    {.................................................}
    procedure ExpandCols(Row,Col, Width, CWALim: Integer; ChangeMins: Boolean;
      Depth: Integer);
    var c, oldw, pureoldw, DH, UnsizedCount, neww, savedw: Integer;
        Cell: TRVTableCellData;
        Lim: Integer;
        AddProportionally: Boolean;
    const MAXDEPTH = 30;
    begin
      if Width=0 then
        exit;
      AddProportionally := not ChangeMins or (Depth>MAXDEPTH);
      savedw := Width;
      inc(Width, GetDevX(CellPadding*2));
      Cell := Cells[Row,Col];
      DH := GetDevX(CellPadding+CellHSpacing+CellPadding+FCellBorderWidth*2);
      Lim := 0;
      repeat
        oldw := GetDevX(-CellHSpacing-FCellBorderWidth*2);
        UnsizedCount := 0;
        pureoldw := 0;
        for c := Col to Col+Cell.ColSpan-1 do begin
          inc(oldw, Fmt.ColWidths[c]+DH);
          if ChangeMins or (ColsWithAssignedWidths[c]<=Lim) then begin
            if AddProportionally and (Fmt.ColWidths[c]+GetDevX(CellPadding*2)=0) then
              inc(pureoldw, 1)
            else
              inc(pureoldw, Fmt.ColWidths[c]+GetDevX(CellPadding*2));
            inc(UnsizedCount);
          end;
        end;
        inc(Lim);
      until ChangeMins or (Lim>CWALim) or (UnsizedCount>0);
      dec(Lim);
      if (oldw<Width) and (UnsizedCount<>0) then begin
        DH := Width-oldw;
        if not ChangeMins and not FixedWidth then begin
          if DH>ExtraWidth then
            DH := ExtraWidth;
          if AddProportionally then
            dec(ExtraWidth, DH);
        end;
        inc(DH, pureoldw);
        if AddProportionally then begin
          for c := Col to Col+Cell.ColSpan-1 do
            if ChangeMins or (ColsWithAssignedWidths[c]<=Lim) then begin
              oldw := Fmt.ColWidths[c]+GetDevX(CellPadding*2);
              if oldw=0 then
                inc(oldw);
              Width := MulDiv(oldw,DH,pureoldw);
              Fmt.ColWidths[c] := Width-GetDevX(CellPadding*2);
              dec(pureoldw, oldw);
              dec(DH,Width);
              if ChangeMins then begin
                inc(MinColsWidth, Width-oldw);
              end;
            end;
          end
        else begin
          for c := Col to Col+Cell.ColSpan-1 do begin
            if (UnsizedCount=0) or (DH<=0) then
              break;
            if ChangeMins or (ColsWithAssignedWidths[c]<=Lim) then begin
              oldw := Fmt.ColWidths[c]+GetDevX(CellPadding*2);
              neww := Round(DH/UnsizedCount);
              if oldw>=neww then begin
                dec(DH, oldw);
                dec(UnsizedCount);
              end;
            end;
          end;
          for c := Col to Col+Cell.ColSpan-1 do begin
            if (UnsizedCount=0) or (DH<=0) then
              break;
            if ChangeMins or (ColsWithAssignedWidths[c]<=Lim) then begin
              oldw := Fmt.ColWidths[c]+GetDevX(CellPadding*2);
              neww := Round(DH/UnsizedCount);
              if oldw<neww then begin
                Fmt.ColWidths[c] := neww-GetDevX(CellPadding*2);
                if ChangeMins then
                  inc(MinColsWidth, neww-oldw);
                dec(DH, neww);
                if not ChangeMins and not FixedWidth then
                  dec(ExtraWidth, neww-oldw);
                dec(UnsizedCount);
              end;
            end;
          end;
          if ChangeMins or FixedWidth or (ExtraWidth>0) then
            ExpandCols(Row,Col, savedw, CWALim, ChangeMins, Depth+1);
        end;
      end;
    end;
    {.................................................}
    procedure CalcMinWidthsSpan;
    var r,c: Integer;
    begin
      for r := 0 to Rows.Count-1 do
        with Rows[r] do
          for c := 0 to Rows[r].Count-1 do
            if (Items[c]<>nil) and (Items[c].ColSpan>1) then
              ExpandCols(r,c, Items[c].GetMinWidth(cursad,Canvas), 2,True,0);
    end;
    {.................................................}
    procedure CalcPercentWidths;
    var c,w,pw,NonDistribWidth: Integer;
        Widths: TRVIntegerList;
    begin
      ExtraWidth := PureWidth-MinColsWidth;
      NonDistribWidth := 0;
      Widths := TRVIntegerList.CreateEx(Rows[0].Count,0);
      try
        pw := 0;
        for c := 0 to Rows[0].Count-1 do begin
          w := Rows.GetPercentColWidth(c, SumColWidth)-GetDevX(CellPadding*2);
          if w<0 then
            w := 0;
          Widths[c] := w;
          if w>Fmt.ColWidths[c] then begin
            inc(pw,w);
            inc(NonDistribWidth, Fmt.ColWidths[c]);
            inc(ExtraWidth, Fmt.ColWidths[c]);
          end;
        end;
        if pw=0 then
          exit;
        if pw<ExtraWidth then
          pw := ExtraWidth;
        for c := 0 to Rows[0].Count-1 do begin
          if Widths[c]=0 then
            continue;
          if Widths[c]<=Fmt.ColWidths[c] then begin
            ColsWithAssignedWidths[c] := 2;
            inc(AssignedCount);
            continue;          
          end;
          dec(NonDistribWidth, Fmt.ColWidths[c]);
          w := MulDiv(Widths[c], ExtraWidth,pw);
          dec(pw, Widths[c]);
          if w>0 then begin
            ColsWithAssignedWidths[c] := 2;
            inc(AssignedCount);
          end;
          if w>ExtraWidth-NonDistribWidth then
            w := ExtraWidth-NonDistribWidth;
          if w>Fmt.ColWidths[c] then
            Fmt.ColWidths[c] := w;
          dec(ExtraWidth, Fmt.ColWidths[c]);
          if (ExtraWidth=0) and (NonDistribWidth=0) then
            break;
        end;
    finally
      Widths.Free;
    end;

      {
      ExtraWidth := PureWidth-MinColsWidth;
      for c := 0 to Rows[0].Count-1 do begin
        w := Rows.GetPercentColWidth(c, SumColWidth)-GetDevX(CellPadding*2);
        if w>0 then begin
          ColsWithAssignedWidths[c] := 2;
          inc(AssignedCount);
        end;
        if w>Fmt.ColWidths[c] then begin
          inc(ExtraWidth, Fmt.ColWidths[c]);
          if w>ExtraWidth then
            w := ExtraWidth;
          Fmt.ColWidths[c] := w;
          dec(ExtraWidth,w);
          if ExtraWidth=0 then break;
        end;
      end;
      }
    end;
    {.................................................}
    procedure CalcPercentWidthsSpan;
    var r,c,w,i: Integer;
    begin
      if ExtraWidth<=0 then exit;
      for r := 0 to Rows.Count-1 do
        with Rows[r] do
          for c := 0 to Rows[r].Count-1 do
            if (Items[c]<>nil) and (Items[c].ColSpan>1) and
               (Items[c].BestWidth<0) then begin
              w := MulDiv(-Items[c].BestWidth, SumColWidth, 100)-GetDevX(CellPadding*2);
              ExpandCols(r,c,w, 2, False, 0);
            end;
      for r := 0 to Rows.Count-1 do
        with Rows[r] do
          for c := 0 to Rows[r].Count-1 do
            if (Items[c]<>nil) and (Items[c].ColSpan>1) and
               (Items[c].BestWidth<0) then begin
              for i := 0 to Items[c].ColSpan-1 do
                if ColsWithAssignedWidths[c+i]=0 then begin
                  ColsWithAssignedWidths[c+i] := 2;
                  inc(AssignedCount);
                end;
            end;
    end;
    {.................................................}
    procedure CalcFixedWidths;
    var c,w: Integer;
    begin
      if not FixedWidth and (ExtraWidth<=0) then exit;
      for c := 0 to Rows[0].Count-1 do
        if ColsWithAssignedWidths[c]=0 then begin
          w := GetDevX(Rows.GetPixelColWidth(c));
          if w>0 then begin
            ColsWithAssignedWidths[c] := 1;
            inc(AssignedCount);
            inc(FixedWidthCount);
          end;
          if w>Fmt.ColWidths[c] then begin
            if not FixedWidth then begin
              inc(ExtraWidth, Fmt.ColWidths[c]);
              if w>ExtraWidth then
                w := ExtraWidth;
            end;
            Fmt.ColWidths[c] := w;
            if not FixedWidth then begin
              dec(ExtraWidth,w);
              if ExtraWidth=0 then break;
            end;
          end;
        end;
    end;
    {.................................................}
    procedure CalcFixedWidthsSpan;
    var r,c,w,i: Integer;
    begin
      if not FixedWidth and (ExtraWidth<=0) then exit;
      for r := 0 to Rows.Count-1 do
        with Rows[r] do
          for c := 0 to Rows[r].Count-1 do
            if //(ColsWithAssignedWidths[c]<>2) and
               (Items[c]<>nil) and (Items[c].ColSpan>1) and
               (Items[c].BestWidth>0) then begin
              w := GetDevX(Items[c].BestWidth);
              ExpandCols(r,c,w, 1, False, 0);
            end;
      for r := 0 to Rows.Count-1 do
        with Rows[r] do
          for c := 0 to Rows[r].Count-1 do
            if (Items[c]<>nil) and (Items[c].ColSpan>1) and
               (Items[c].BestWidth>0) then begin
              for i := 0 to Items[c].ColSpan-1 do
                if ColsWithAssignedWidths[c+i]=0 then begin
                  ColsWithAssignedWidths[c+i] := 1;
                  inc(AssignedCount);
                  inc(FixedWidthCount);
                end;
            end;
    end;
    {.................................................}
    procedure ExpandRows(Row,Col: Integer);
    var r,h,h2,oldh, pureoldh, pureoldh2, DV: Integer;
        EmptyRows: Boolean;
        Cell: TRVTableCellData;
    begin
      Cell := Cells[Row,Col];
      DV := GetDevY(CellPadding+CellVSpacing+CellPadding+FCellBorderWidth*2);
      if Fmt.Rows=nil then
        h := GetDevY(Cell.GetCellHeight+CellPadding*2)
      else begin
        if Cell.BestHeight>10 then
          h := GetDevY(Cell.BestHeight)
        else
          h := GetDevY(10);
        if Fmt.Rows[Row*Rows[0].Count+Col]<>nil then begin
          h2 := TCellPtblRVData(Fmt.Rows[Row*Rows[0].Count+Col]).DocumentHeight;
          if h2>h then
            h := h2;
        end;
        inc(h, GetDevY(CellPadding*2));
      end;
      oldh := GetDevY(-CellVSpacing-FCellBorderWidth*2);
      pureoldh := 0;
      pureoldh2 := 0;
      EmptyRows := False;
      for r := Row to Row+Cell.RowSpan-1 do begin
        inc(oldh, Fmt.RowHeights[r]+DV);
        inc(pureoldh, Fmt.RowHeights[r]);
        if not Rows[r].HasCellsInRange(r, Row, Cell.RowSpan) then begin
          inc(pureoldh2, Fmt.RowHeights[r]);
          EmptyRows := True;
        end;
      end;
      if EmptyRows then
        pureoldh := pureoldh2;
      if oldh<h then begin
        DV := h-oldh;
        inc(Fmt.FHeight, DV);
        for r := Row to Row+Cell.RowSpan-1 do
          if not (EmptyRows and Rows[r].HasCellsInRange(r, Row, Cell.RowSpan)) then begin
            oldh := Fmt.RowHeights[r];
            h := MulDiv(oldh,DV,pureoldh);
            Fmt.RowHeights[r] := Integer(Fmt.RowHeights.Items[r])+h;
            dec(pureoldh, oldh);
            dec(DV,h);
          end;
      end;
    end;
    {.................................................}
var NewWidth, HorExtra: Integer;
begin
  if Rows.Empty then begin
    Fmt.FWidth := GetDevX((BorderWidth+BorderHSpacing)*2);
    Fmt.FHeight := GetDevY((BorderWidth+BorderVSpacing)*2);
    exit;
  end;
  HorExtra := GetHorzExtra;
  FixedWidth := False;
  if BestWidth>0 then
    NewWidth := GetDevX(BestWidth)
  else if BestWidth<0 then
    NewWidth := -BestWidth*DocWidth div 100
  else begin
    FixedWidth := IsFixedWidthTable;
    if not FixedWidth then
      NewWidth := DocWidth //  temporary
    else
      NewWidth := 0; //CalcPixelWidth;
      //inc(NewWidth, HorExtra+GetDevX(CellPadding*Rows[0].Count*2));
  end;
  if (NewWidth<>0) and (NewWidth=Fmt.FWidth) and not NoCaching then
    exit;
  Fmt.FWidth := NewWidth;
  if Rows.Empty then
    exit;
  ColsWithAssignedWidths := nil;
  if not FixedWidth then begin
    SumColWidth := Fmt.FWidth - HorExtra;
    PureWidth := SumColWidth - GetDevX(CellPadding*Rows[0].Count*2);
    end
  else begin
    SumColWidth := 0;
    PureWidth   := 0;
    ExtraWidth  := 0;
  end;
  Fmt.QuickClear;
  // Calculating minimal widths
  CalcMinWidths;
  CalcMinWidthsSpan;
  if not FixedWidth and (MinColsWidth>=PureWidth) then begin
    // Table is too narrow. Setting all width to minimums
    Fmt.FWidth := Fmt.FWidth-PureWidth+MinColsWidth;
    end
  else begin
    ColsWithAssignedWidths := TRVIntegerList.Create;
    try
      AssignedCount := 0;
      FixedWidthCount := 0;
      ColsWithAssignedWidths.Capacity := Fmt.ColWidths.Count;
      for c := 0 to Fmt.ColWidths.Count-1 do
        ColsWithAssignedWidths.Add(0);
      // Setting widths for autosizing cols...
      if not FixedWidth then begin
        CalcPercentWidths;
        CalcPercentWidthsSpan;
      end;
      // Setting widths for fixed cols...
      CalcFixedWidths;
      CalcFixedWidthsSpan;
      if FixedWidth then begin
        Fmt.FWidth := HorExtra+GetDevX(CellPadding*Rows[0].Count*2);
        for c := 0 to Rows[0].Count-1 do
          inc(Fmt.FWidth, Fmt.ColWidths[c]);
        end
      else if ExtraWidth>0 then
        if AssignedCount<ColsWithAssignedWidths.Count then begin
          // Setting widths for other cols...
          for c := 0 to Rows[0].Count-1 do
            if ColsWithAssignedWidths[c]=0 then begin
              w := ExtraWidth div (ColsWithAssignedWidths.Count-AssignedCount);
              Fmt.ColWidths[c] := Fmt.ColWidths[c]+w;
              dec(ExtraWidth,w);
              inc(AssignedCount);
              if ExtraWidth<=0 then break;
            end;
          end
        else if FixedWidthCount>0 then begin
          // Expanding fixed width cols.
          w2 := 0;
          for c := 0 to Rows[0].Count-1 do
            if not Rows.IsPercentWidthColumn(c) then begin
              w := Rows.GetPixelColWidth(c);
              if w=0 then
                w := 1;
              ColsWithAssignedWidths[c] := w;
              inc(w2,w);
              end
            else
              ColsWithAssignedWidths[c] := 0;
          for c := 0 to Rows[0].Count-1 do begin
            w3 := ColsWithAssignedWidths[c];
            if w3<>0 then begin
              w := MulDiv(ExtraWidth, w3, w2);
              Fmt.ColWidths[c] := Fmt.ColWidths[c]+w;
              dec(ExtraWidth,w);
              dec(w2, w3);
              if ExtraWidth<=0 then break;
            end;
          end
          end
        else begin
          // Expanding all cols
         w2 := 0;
         for c := 0 to Rows[0].Count-1 do
          if Rows.IsPercentWidthColumn(c) then begin
            w := Rows.GetPercentColWidth(c, SumColWidth)-GetDevX(CellPadding*2);
            if w<=0 then
              w := 1;
            ColsWithAssignedWidths[c] := w;
            inc(w2,w);
          end;
          for c := 0 to Rows[0].Count-1 do begin
            if Rows.IsPercentWidthColumn(c) then begin
              w3 := ColsWithAssignedWidths[c];
              w := MulDiv(ExtraWidth, w3, w2);
              if w>ExtraWidth then
                w := ExtraWidth;
              Fmt.ColWidths[c] := Fmt.ColWidths[c]+w;
              dec(ExtraWidth,w);
              dec(w2, w3);
              if ExtraWidth<=0 then break;
            end;
          end;
        end;
    finally
      ColsWithAssignedWidths.Free;
    end;
  end;
  // Note: for table with undefined width algorithm must be completely different...
  UpdateCellXCoords(Fmt, NoCaching);
  Fmt.FHeight := GetDevY((CellPadding*Rows.Count+BorderWidth+BorderVSpacing)*2+(CellVSpacing*(Rows.Count-1))+
             Rows.Count*CellBorderWidth*2);
  Fmt.RowHeights.Capacity := Rows.Count;
  Fmt.RowStarts.Count := Rows.Count;
  // pass 1...
  if Fmt.Rows=nil then
    for r := 0 to Rows.Count-1 do begin
      w := GetDevY(Rows[r].GetHeight);
      Fmt.RowHeights.Add(w);
      inc(Fmt.FHeight, w);
    end
  else
    for r := 0 to Rows.Count-1 do begin
      with Rows[r] do begin
        w := GetDevY(GetBestHeight);
        for c := 0 to Count-1 do
          if (Items[c]<>nil) and (Items[c].RowSpan=1) then begin
            if Fmt.Rows[r*Count+c]<>nil then
              w2 := TCellPtblRVData(Fmt.Rows[r*Count+c]).DocumentHeight
            else
              w2 := GetDevY(Items[c].DocumentHeight);
            if w2>w then
              w := w2;
          end;
      end;
      //if w<GetDevY(10) then
      //  w := GetDevY(10);
      Fmt.RowHeights.Add(w);
      inc(Fmt.FHeight, w);
    end;
  // pass 2...
  for r := 0 to Rows.Count-1 do
    with Rows[r] do
      for c := 0 to Count-1 do
        if (Items[c]<>nil) and (Items[c].RowSpan>1) then
          ExpandRows(r,c);
  UpdateCellYCoords(Fmt);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.DrawBorder(Canvas: TCanvas;
  Left,Top,Right,Bottom, Width: Integer; LightColor, Color, BackgroundColor: TColor;
  Style: TRVTableBorderStyle; DrawEvenEmptyBorder, Printing: Boolean;
  const ClipRect: TRect; VisibleBorders: TRVBooleanRect;
  r,c: Integer; ColorMode: TRVColorMode);
var i,y1,y2,incr: Integer;
    par: array [0..2] of TPoint;
    DrawGridLines: Boolean;
    {..........................................}
    procedure ClipY(var y1,y2: Integer; const ClipRect: TRect);
    begin
      if y1<ClipRect.Top-1 then
        y1 := ClipRect.Top-1;
      if y2>ClipRect.Bottom+1 then
        y2 := ClipRect.Bottom+1;
    end;
    {..........................................}
    function HasTopLine: Boolean;
    var r2,c2: Integer;
    begin
      if r=0 then
        Result := (BorderVSpacing<0) and (BorderWidth>0)
      else
        Result := (CellVSpacing<0) and Rows.GetMainCell(r-1,c,r2,c2).VisibleBorders.Bottom;
    end;
    {..........................................}
    function HasBottomLine: Boolean;
    var r2,c2: Integer;
    begin
      r2 := r+Cells[r,c].RowSpan-1;
      if r2=Rows.Count-1 then
        Result := (BorderVSpacing<0) and (BorderWidth>0)
      else
        Result := (CellVSpacing<0) and Rows.GetMainCell(r2+1,c,r2,c2).VisibleBorders.Top;
    end;
    {..........................................}
    function HasLeftLine: Boolean;
    var r2,c2: Integer;
    begin
      if c=0 then
        Result := (BorderHSpacing<0) and (BorderWidth>0)
      else
        Result := (CellHSpacing<0) and Rows.GetMainCell(r,c-1,r2,c2).VisibleBorders.Right;
    end;
    {..........................................}
    function HasRightLine: Boolean;
    var r2,c2: Integer;
    begin
      c2 := c+Cells[r,c].ColSpan-1;
      if c2=Rows[r].Count-1 then
        Result := (BorderHSpacing<0) and (BorderWidth>0)
      else
        Result := (CellHSpacing<0) and Rows.GetMainCell(r,c2+1,r2,c2).VisibleBorders.Left;
    end;
    {..........................................}
var DoDefault: Boolean;
begin
  if (ClipRect.Left>Right) or
     (ClipRect.Right<Left) or
     (ClipRect.Top>Bottom) or
     (ClipRect.Bottom<Top) then
    exit;
  if Assigned(FOnDrawBorder) then begin
    DoDefault := True;
    FOnDrawBorder(Self, Canvas, Left, Top, Right, Bottom, Width, LightColor, Color,
    BackgroundColor, Style, Printing, VisibleBorders, r, c, DoDefault);
    if not DoDefault then
      exit;
  end;
  DrawGridLines := DrawEvenEmptyBorder and not Printing and (RichViewTableGridStyle<>psClear);
  if Width=0 then begin
    if not DrawGridLines then
      exit;
    Canvas.Pen.Width := 1;
    Canvas.Brush.Color := clNone;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := RichViewTableGridStyle;
    Canvas.Pen.Color := RichViewTableGridColor;
    Canvas.Rectangle(Left,Top,Right,Bottom);
    Canvas.Pen.Mode := pmCopy;
    Canvas.Pen.Style := psSolid;
    exit;
  end;
  if DrawGridLines and (VisibleBorders<>nil) and not VisibleBorders.IsAllEqual(True) then begin
    Canvas.Pen.Width := 1;
    Canvas.Brush.Color := clNone;
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := RichViewTableGridStyle;
    Canvas.Pen.Color := RichViewTableGridColor;
    if not VisibleBorders.Top and not HasTopLine then begin
      Canvas.MoveTo(Left,Top);
      Canvas.LineTo(Right,Top);
    end;
    if not VisibleBorders.Bottom and not HasBottomLine then begin
      Canvas.MoveTo(Left,Bottom);
      Canvas.LineTo(Right,Bottom);
    end;
    if not VisibleBorders.Left and not HasLeftLine then begin
      y1 := Top;
      y2 := Bottom;
      ClipY(y1,y2,ClipRect);
      Canvas.MoveTo(Left,y1);
      Canvas.LineTo(Left,y2);
    end;
    if not VisibleBorders.Right and not HasRightLine then begin
      y1 := Top;
      y2 := Bottom;
      ClipY(y1,y2,ClipRect);
      Canvas.MoveTo(Right,y1);
      Canvas.LineTo(Right,y2);
    end;
    Canvas.Pen.Mode := pmCopy;
    Canvas.Pen.Style := psSolid;
  end;

  Canvas.Brush.Style := bsClear;
  if Style in [rvtbRaised, rvtbLowered, rvtbRaisedColor, rvtbLoweredColor] then begin
    dec(Right);
    dec(Bottom);
    Width := GetDevY(Width);
    // 3d frame
    case Style of
      rvtbRaised:
        begin
          if ColorToRGB(BackgroundColor)=ColorToRGB(clBtnHighlight) then
            Canvas.Pen.Color := RV_GetColor(clBtnFace, ColorMode)
          else
            Canvas.Pen.Color := RV_GetColor(clBtnHighlight, ColorMode)
        end;
      rvtbRaisedColor:
        Canvas.Pen.Color := RV_GetColor(LightColor, ColorMode);
      rvtbLoweredColor:
        Canvas.Pen.Color := RV_GetColor(Color, ColorMode);
      else {rvtbLowered}
        Canvas.Pen.Color := RV_GetColor(clBtnShadow, ColorMode);
    end;
    if Printing and (rvtoHalftoneBorders in PrintOptions) then begin
      if (cursad<>nil) and (cursad.ppiyScreen<cursad.ppiyDevice) then begin
        Canvas.Pen.Width := Width+1;
        dec(Left);
        dec(Top);
      end;
      Canvas.Pen.Style := psInsideFrame;
      Width := Width div 2;
      inc(Left,Width);
      inc(Top,Width);
      dec(Right,Width);
      dec(Bottom,Width);
      Width := 1;
      incr := 0;
      end
    else begin
      Canvas.Pen.Width := 1;
      Canvas.Pen.Style := psSolid;
      incr := 1;
    end;
    with par[0] do begin x := Left;  y := Bottom; end;
    with par[1] do begin x := Left;  y := Top;    end;
    with par[2] do begin x := Right; y := Top;    end;
    for i := 0 to Width-1 do begin
      //Canvas.Polyline(par);  <- does not work for looooong tables
      if (VisibleBorders=nil) or (VisibleBorders.Top) then begin
        with par[1] do Canvas.MoveTo(x,y);
        with par[2] do Canvas.LineTo(x+incr,y);
      end;
      if (VisibleBorders=nil) or (VisibleBorders.Left) then begin
        y1 := par[1].y;
        y2 := par[0].y;
        ClipY(y1,y2,ClipRect);
        Canvas.MoveTo(par[1].x,y1);
        Canvas.LineTo(par[0].x,y2+incr);
      end;
      with par[0] do begin inc(x); dec(y); end;
      with par[1] do begin inc(x); inc(y); end;
      with par[2] do begin dec(x); inc(y); end;
    end;
    case Style of
      rvtbRaised:
        Canvas.Pen.Color := RV_GetColor(clBtnShadow, ColorMode);
      rvtbRaisedColor:
        Canvas.Pen.Color := RV_GetColor(Color, ColorMode);
      rvtbLoweredColor:
        Canvas.Pen.Color := RV_GetColor(LightColor, ColorMode);
      else {rvtbLowered}
        begin
          if ColorToRGB(BackgroundColor)=ColorToRGB(clBtnHighlight) then
            Canvas.Pen.Color := RV_GetColor(clBtnFace, ColorMode)
          else
            Canvas.Pen.Color := RV_GetColor(clBtnHighlight, ColorMode);
        end;
    end;
    with par[0] do begin x := Right; y := Top;    end;
    with par[1] do begin x := Right; y := Bottom; end;
    with par[2] do begin x := Left;  y := Bottom; end;
    for i := 0 to Width-1 do begin
      //Canvas.Polyline(par);  <- does not work for looooong tables
      if (VisibleBorders=nil) or (VisibleBorders.Bottom) then begin
        with par[2] do Canvas.MoveTo(x,y);
        with par[1] do Canvas.LineTo(x+incr,y);
      end;
      if (VisibleBorders=nil) or (VisibleBorders.Right) then begin
        y1 := par[0].y;
        y2 := par[1].y;
        ClipY(y1,y2,ClipRect);
        Canvas.MoveTo(par[0].x,y1);
        Canvas.LineTo(par[1].x,y2+incr);
      end;
      with par[0] do begin dec(x); inc(y); end;
      with par[1] do begin dec(x); dec(y); end;
      with par[2] do begin inc(x); dec(y); end;
    end;
    end
  else begin
    // Single frame
    if Color=clNone then exit;
    Canvas.Pen.Color := RV_GetColor(Color, ColorMode);
    if (RVNT or (Bottom-Top<32000)) and
       ((VisibleBorders=nil) or
       (VisibleBorders.Left and VisibleBorders.Right and
        VisibleBorders.Top and VisibleBorders.Bottom)) then begin
      Canvas.Pen.Width := GetDevY(Width);
      if Printing and (cursad<>nil) and (cursad.ppiyScreen<cursad.ppiyDevice) then begin
        Canvas.Pen.Width := Canvas.Pen.Width+1;
        dec(Left);
        dec(Top);
      end;
      Canvas.Pen.Style := psInsideFrame;
      Canvas.Rectangle(Left, Top, Right, Bottom);
      Canvas.Pen.Width := 1;
      Canvas.Pen.Style := psSolid;
      end
    else begin
      Width := GetDevY(Width);
      if Printing and (cursad<>nil) and (cursad.ppiyScreen<cursad.ppiyDevice) then begin
        inc(Width);
        dec(Top);
        dec(Left);
      end;
      if Printing and (rvtoHalftoneBorders in PrintOptions) then begin
        Canvas.Pen.Width := Width;
        Canvas.Pen.Style := psInsideFrame;
        Width := Width div 2;
        inc(Left,Width);
        inc(Top,Width);
        dec(Right,Width);
        dec(Bottom,Width);
        Width := 1;
        end
      else begin
        Canvas.Pen.Width := 1;
        Canvas.Pen.Style := psSolid;
      end;
      dec(Right);
      dec(Bottom);

      if rvtoOverlappingCorners in Options then begin
        for i := 0 to Width-1 do begin // idea of Harley Pebley
          y1 := Top;
          y2 := Bottom+1;
          ClipY(y1,y2,ClipRect);
          if (VisibleBorders=nil) or VisibleBorders.Left then begin
            Canvas.MoveTo(Left+i,y1);
            Canvas.LineTo(Left+i,y2);
          end;
          if (VisibleBorders=nil) or VisibleBorders.Right then begin
            Canvas.MoveTo(Right-i,y1);
            Canvas.LineTo(Right-i,y2);
          end;
          if (VisibleBorders=nil) or VisibleBorders.Top then begin
            Canvas.MoveTo(Left,Top+i);
            Canvas.LineTo(Right+1,Top+i);
          end;
          if (VisibleBorders=nil) or VisibleBorders.Bottom then begin
            Canvas.MoveTo(Left,Bottom-i);
            Canvas.LineTo(Right+1,Bottom-i);
          end;
        end
        end
      else
        for i := 0 to Width-1 do begin
          if (VisibleBorders=nil) or VisibleBorders.Left then begin
            y1 := Top;
            y2 := Bottom+1;
            ClipY(y1,y2,ClipRect);
            Canvas.MoveTo(Left,y1);
            Canvas.LineTo(Left,y2);
          end;
          if (VisibleBorders=nil) or VisibleBorders.Right then begin
            y1 := Top;
            y2 := Bottom+1;
            ClipY(y1,y2,ClipRect);
            Canvas.MoveTo(Right,y1);
            Canvas.LineTo(Right,y2);
          end;
          if (VisibleBorders=nil) or VisibleBorders.Top then begin
            Canvas.MoveTo(Left,Top);
            Canvas.LineTo(Right+1,Top);
          end;
          if (VisibleBorders=nil) or VisibleBorders.Bottom then begin
            Canvas.MoveTo(Left,Bottom);
            Canvas.LineTo(Right+1,Bottom);
          end;
          inc(Left);
          inc(Top);
          dec(Right);
          dec(Bottom);
        end;
    end;
  end;
  Canvas.Pen.Style := psSolid;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetVerticalRuleNo(X: Integer; var MinX, ZeroChangeX: Integer): Integer;
var i, DH,l: Integer;
 {..................................}
 function InEps(v, delta: Integer): Boolean;
 begin
   delta := delta div 2;
   if delta<2 then delta := 2;
   Result := abs(v-X)<=delta;
 end;
 {..................................}
begin
 Result := -1;
 if (X<0) or (X>Fmt.FWidth) then exit;
 DH := CellHSpacing+FCellBorderWidth*2;
 l := BorderWidth+BorderHSpacing+CellBorderWidth;
 if InEps(l div 2, l) then begin
   Result := 0;
   MinX := -1;
   ZeroChangeX := l div 2;
   exit;
 end;
 if InEps(Fmt.FWidth - l div 2, l) then begin
   Result := Rows[0].Count;
   MinX := Fmt.ColStarts[Result-1];
   ZeroChangeX := Fmt.FWidth - l div 2;
   exit;
 end;
 for i := 0 to Rows[0].Count-2 do begin
   l := Fmt.ColStarts[i+1];
   if InEps(l-DH div 2, DH) then begin
     Result := i+1;
     MinX := Fmt.ColStarts[Result-1];
     ZeroChangeX := l-(DH+1) div 2;
     exit;
   end;
 end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetHorizontalRuleNo(Y: Integer; var MinY, ZeroChangeY: Integer): Integer;
var i, DV,t: Integer;
 {..................................}
 function InEps(v, delta: Integer): Boolean;
 begin
   delta := delta div 2;
   if delta<2 then delta := 2;
   Result := abs(v-Y)<=delta;
 end;
 {..................................}
begin
 Result := -1;
 if (Y<0) or (Y>Fmt.FHeight) then exit;
 DV := CellVSpacing+FCellBorderWidth*2;
 t := BorderWidth+BorderVSpacing+CellBorderWidth;
 if InEps(t div 2, t) then begin
   Result := 0;
   MinY := -1;
   ZeroChangeY := t div 2;
   exit;
 end;
 if InEps(Fmt.FHeight - t div 2, t) then begin
   Result := Rows.Count;
   MinY := Fmt.RowStarts[Result-1];
   ZeroChangeY := Fmt.FHeight - t div 2;
   exit;
 end;
 for i := 0 to Rows.Count-2 do begin
   t := Fmt.RowStarts[i+1];
   if InEps(t-DV div 2, DV) then begin
     Result := i+1;
     MinY := Fmt.RowStarts[Result-1];
     ZeroChangeY := t-(DV+1) div 2;
     exit;
   end;
 end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetColNo(X: Integer): Integer;
var c:Integer;
begin
  Result := -1;
  for c := 0 to Rows[0].Count-1 do begin
    if X<Fmt.ColStarts[c] then
      exit;
    if X<=Fmt.ColStarts[c]+Fmt.ColWidths[c] then begin
      Result := c;
      exit;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetRowNo(Y: Integer): Integer;
var r:Integer;
begin
  Result := -1;
  for r := 0 to Rows.Count-1 do begin
    if Y<Fmt.RowStarts[r] then
      exit;
    if Y<=Fmt.RowStarts[r]+Fmt.RowHeights[r] then begin
      Result := r;
      exit;
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetCrossed(Coord: Integer;List: TRVIntegerList): Integer;
var a,b:Integer;
begin
  if List.Count<2 then begin
    Result := 0;
    exit;
  end;
  if Coord>=List[List.Count-1] then begin
    Result := List.Count-1;
    exit;
  end;
  a := 0;
  b := List.Count-1;
  while b-a>1 do begin
    Result := (b+a) div 2;
    if List[Result]<Coord then
      a := Result
    else
      b := Result;
  end;
  Result := a;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.MouseMove(Shift: TShiftState; X, Y,ItemNo: Integer; RVData: TObject): Boolean;
var Row,Col, RuleNo,a,b: Integer;
    NewSelColOffs, NewSelRowOffs: Integer;
    Redraw: Boolean;
begin
  if Rows.Empty or TCustomRVFormattedData(Rows.FMainRVData).UsingThisXorDrawing(XORDrawing) then begin
    Result := False;
    exit;
  end;
  MyClientTop := 0;
  MyClientLeft := 0;
  Result := True;
  if BusyCount>0 then exit;
  if GetCellAt(X, Y, Row, Col) then begin
    if FMakingSelection and (ssLeft in Shift) then begin
      NewSelColOffs := Col-FSelStartCol;
      NewSelRowOffs := Row-FSelStartRow;
      Redraw :=  (FSelColOffs<>NewSelColOffs) or
                 (FSelRowOffs<>NewSelRowOffs);
      FSelColOffs := NewSelColOffs;
      FSelRowOffs := NewSelRowOffs;
      if (FSelColOffs<>0) or (FSelRowOffs<>0) then
        Include(FState, rvtsSelExists)
      else
        Exclude(FState, rvtsSelExists);      
      if Redraw then begin
        UpdateCellSel;
        TCustomRVFormattedData(Rows.FMainRVData).Invalidate;
        if FInplaceEditor<>nil then
          TRVTableInplaceEdit(FInplaceEditor).SelectCompletely(rvtsSelExists in FState);
      end;
    end;
    if not FMakingSelection or not (rvtsSelExists in FState) then
      with Cells[Row,Col] do begin
        if RV_PointInRect(X, Y, Left+CellPadding, Top+CellPadding,
                          Width-CellPadding*2, Height-CellPadding*2) then begin
          MouseMove(Shift, X{-Left-CellPadding}, Y{-Top-CellPadding});
          Result := True;
          exit;
        end;
    end
    end
  else begin
    RuleNo := GetVerticalRuleNo(X,a,b);
    if (rvtoRowSelect in Options) and (RuleNo=0) and
       (rvoAllowSelection in TCustomRVFormattedData(RVData).Options) then begin
      TCustomRVFormattedData(RVData).SetCursor(crRVSelectRow);
      exit;
    end;
    if CanChange and (rvtoColSizing in Options) and (RuleNo>0) then begin
      TCustomRVFormattedData(RVData).SetCursor(crHSplit);
      exit;
    end;
    RuleNo := GetHorizontalRuleNo(Y,a,b);
    if (rvtoColSelect in Options) and (RuleNo=0) and
       (rvoAllowSelection in TCustomRVFormattedData(RVData).Options) then begin
      TCustomRVFormattedData(RVData).SetCursor(crRVSelectCol);
      exit;
    end;
    if CanChange and (rvtoRowSizing in Options) and (RuleNo>0) then begin
      TCustomRVFormattedData(RVData).SetCursor(crVSplit);
      exit;
    end;
  end;
  TCustomRVFormattedData(RVData).SetCursor(crArrow);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.XorDrawing(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
var Offs: Integer;
    Canvas: TCanvas;
begin
  Offs := 0;
  Canvas := TCustomRVFormattedData(Sender).GetCanvas;
  with Canvas do begin
    Pen.Mode := pmNot;
    Pen.Width := 1;
    Brush.Style := bsClear;
    Pen.Style := psSolid;
    if not (rvtsVerticalDraggedRule in FState) then begin
      if (DRMin<>-1) or (DRMax<>-1) then
        Offs := MyTop-TCustomRVFormattedData(Rows.FMainRVData).GetVOffs;
      if (DRMin<>-1) and (Y<DRMin+Offs) then
        Y := DRMin+Offs;
      if (DRMax<>-1) and (Y<DRMax+Offs) then
        Y := DRMax+Offs;
      DrawFancyHLine(Canvas, 0, TCustomRVFormattedData(Sender).GetWidth, Y);
      end
    else begin
      if (DRMin<>-1) or (DRMax<>-1) then
        Offs := MyLeft-TCustomRVFormattedData(Rows.FMainRVData).GetHOffs;
      if (DRMin<>-1) and (X<DRMin+Offs) then
        X := DRMin+Offs;
      if (DRMax<>-1) and (X<DRMax+Offs) then
        X := DRMax+Offs;
      DrawFancyVLine(Canvas, X, 0, TCustomRVFormattedData(Sender).GetHeight);
    end;
    Pen.Mode := pmCopy;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.DoOnCellEditing(Row,Col: Integer; Automatic: Boolean): Boolean;
begin
  Result := True;
  if Assigned(FOnCellEditing) then
    FOnCellEditing(Self, Row, Col, Automatic, Result);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.CreateInplace(ItemNo, Row, Col: Integer;
                                         BuildJumps, CaretAtStart, CaretAtEnd, SetTime,
                                         Unquestioning: Boolean);
var RVData: TCustomRVFormattedData;
    tie: TRVTableInplaceEdit;
    Idx, Offs: Integer;
begin
  if not (rvtsInserted in FState) or not IsInEditor or
  (not Unquestioning and not DoOnCellEditing(Row,Col, False)) then
    exit;
  if ItemNo=-1 then
    ItemNo := GetMyItemNo;
  DestroyInplace(True);
  DeselectPartial;
  TCustomRVFormattedData(FRows.FMainRVData).PartialSelectedItem := nil;
  TCustomRVFormattedData(FRows.FMainRVData).FActiveItem := nil;
  TCustomRVFormattedData(FRows.FMainRVData).RestoreSelBounds(ItemNo, ItemNo, 1, 1);
  Deselect;
  RVData := TCustomRVFormattedData(FRows.FMainRVData);
  if IsInEditor and TCustomRichViewEdit(TRichViewRVData(RVData).RichView).ReadOnly then
    BuildJumps := True;
  FMinWidthPlus := RVData.CalculateMinItemWidthPlusEx(ItemNo);
  tie := TRVTableInplaceEdit.Create(nil);
  FInplaceEditor := tie;
  tie.Options := TRichViewRVData(RVData).GetOptions;
  tie.OnChangeEx := InplaceEditorChange;
  tie.OnCaretGetOut := InplaceEditorCaretGetout;
  if RVData is TRichViewRVData then begin
    tie.BiDiMode := TCustomRichView(TRichViewRVData(RVData).RichView).BiDiMode;
    tie.AssignEvents(TCustomRichView(TRichViewRVData(RVData).RichView));
    tie.OnRVMouseDown := InplaceEditorMouseDown;
    tie.OnRVMouseUp   := InplaceEditorMouseUp;
    tie.OnControlAction   := InplaceEditorControlAction;
    tie.OnMouseMove   := InplaceEditorMouseMove;
    tie.OnDragOver    := InplaceEditorDragOver;
    tie.OnDragDrop    := InplaceEditorDragDrop;
    if RVData is TRVEditRVData then
      with TCustomRichViewEdit(TRVEditRVData(RVData).RichView) do begin
        tie.EditorOptions := EditorOptions + [rvoWantTabs];
        if UndoLimit<>0 then
          tie.UndoLimit := -1 // temporal solution
        else
          tie.UndoLimit := 0;
      end;
  end;
  tie.Color := GetCellColor(Cells[Row,Col]);
  tie.Transparent := CanSeeBackgroundThroughCell(Cells[Row,Col]);
  tie.Parent := TCustomRVData(RVData).GetParentControl;
  tie.VScrollVisible := False;
  tie.HScrollVisible := False;
  RVData.GetItemClientCoords(ItemNo, MyLeft, MyTop);
  with Cells[Row,Col] do
    SetInplaceBounds(MyLeft+Left, MyTop+Top+GetExtraVOffs, Width, Height-GetExtraVOffs);
  tie.SetParentRVData(RVData);
  tie.RVData.DrainFrom(Cells[Row,Col]);
  tie.SetCell(Row,Col,Self,CellPadding);
  tie.Format;
  if SetTime then
   tie.FClickTime := GetMessageTime;
  //tie.FirstJumpNo := Cells[Row,Col].FirstJumpNo;
  if BuildJumps then
    TRVTableInplaceRVData(tie.RVData).BuildJumpsCoords(True);
  tie.Visible := True;
  RVData.AssignChosenRVData(Cells[Row,Col], Self);
  ChosenCellRow := Row;
  ChosenCellCol := Col;
  FInplaceMinWidthPlus := tie.RVData.CalculateMinDocWidthPlus(nil,nil);
  if CaretAtEnd then begin
    Idx :=  tie.ItemCount-1;
    Offs := tie.GetOffsAfterItem(Idx);
    tie.SetSelectionBounds(Idx,Offs,Idx,Offs);
  end;
  if CaretAtStart then begin
    Idx :=  0;
    Offs := tie.GetOffsBeforeItem(Idx);
    tie.SetSelectionBounds(Idx,Offs,Idx,Offs);
  end;
  tie.CurTextStyleChange;
  tie.CurParaStyleChange;
  RVData.GetAbsoluteRootData.State := RVData.GetAbsoluteRootData.State+[rvstNoKillFocusEvents];
  try
    tie.SetFocusSilent;
  finally
    RVData.GetAbsoluteRootData.State := RVData.GetAbsoluteRootData.State-[rvstNoKillFocusEvents];
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y,ItemNo: Integer; RVData: TObject): Boolean;
var Row,Col, ERow, ECol: Integer;
    VDRNo,HDRNo:Integer;
begin
  Result  := False;
  if Rows.Empty then
    exit;
  if (BusyCount>0) or
     (rvstIgnoreNextMouseDown in TCustomRVFormattedData(Rows.FMainRVData).State) then exit;
  if (Button in [mbLeft, mbRight]) and
     CanChange and (rvtoEditing in Options) and
     GetCellAt(X, Y, Row, Col) then begin
    if (Button=mbRight) and (IsCellSelected(Row,Col) or CompletelySelected) then
      exit;
    CreateInplace(ItemNo, Row, Col, ssCtrl in Shift, False, False, Button=mbLeft, False);
    if (GetEditedCell(ERow,ECol)<>nil) and (ERow=Row) and (ECol=Col) then begin
      SetCaptureControl(FInplaceEditor);
      with Cells[Row,Col] do
        FInplaceEditor.MouseDown(Button, Shift, X-Left, Y-Top-GetExtraVOffs);
      exit;
    end;
  end;
  MyClientTop := 0;
  MyClientLeft := 0;
  if GetCellAt(X, Y, Row, Col) then begin
    DestroyInplace(True);
    if Rows.FMainRVData is TRVEditRVData then
      Rows.FMainRVData.GetParentControl.SetFocus;
    if Button=mbLeft then begin
      {
      TCustomRVFormattedData(Rows.FMainRVData).State := TCustomRVFormattedData(Rows.FMainRVData).State+[rvstNoScroll];
      try
        TCustomRVFormattedData(Rows.FMainRVData).SetSelectionBounds(ItemNo, 1, ItemNo, 1);
      finally
        TCustomRVFormattedData(Rows.FMainRVData).State := TCustomRVFormattedData(Rows.FMainRVData).State-[rvstNoScroll];
      end;
      }
      MyClientTop := 0;
      MyClientLeft := 0;
    end;
    if Button in [mbLeft, mbRight] then begin
      TCustomRVFormattedData(Rows.FMainRVData).AssignChosenRVData(Cells[Row,Col], Self);
      ChosenCellRow := Row;
      ChosenCellCol := Col;
    end;
    with Cells[Row,Col] do begin
      if RV_PointInRect(X, Y, Left+CellPadding, Top+CellPadding,
                        Width-CellPadding*2, Height-CellPadding*2) then begin
        MouseDown(Button, Shift, X, Y);
      end;
      if (Button=mbLeft) and not (rvstStartingDragDrop in Rows.FMainRVData.GetAbsoluteRootData.State) and
        StartSelecting(Row,Col) then begin
        TCustomRVFormattedData(RVData).SetMouseCapture(Self,MyLeft,MyTop);
        TCustomRVFormattedData(Rows.FMainRVData).Invalidate;
      end;
      exit;
    end;
  end;

  if (Button<>mbLeft) or
     not ((rvtoRowSizing in Options) or (rvtoColSizing in Options) or
          (rvtoColSelect in Options) or (rvtoRowSelect in Options)) then begin
    DestroyInplace(True);
    Rows.FMainRVData.GetParentControl.SetFocus;
    exit;
  end;
  if (CanChange and (rvtoColSizing in Options)) or (rvtoRowSelect in Options) then
    VDRNo := GetVerticalRuleNo(X, DRMin, DRDelta)
  else
    VDRNo := -1;
  if (VDRNo>0) and CanChange and (rvtoColSizing in Options) then begin
    DRNo := VDRNo;
    dec(DRDelta,X);
    TCustomRVFormattedData(RVData).AssignXorDrawing(XorDrawing);
    TCustomRVFormattedData(RVData).SetMouseCapture(Self,MyLeft,MyTop);
    DRMax := -1;
    if ssShift in Shift then
      Include(FState, rvtsDRChangeTableWidth)
    else
      Exclude(FState, rvtsDRChangeTableWidth);
    Include(FState, rvtsVerticalDraggedRule);
    SaveInplace;
    end
  else begin
    if (CanChange and (rvtoRowSizing in Options)) or (rvtoColSelect in Options) then
      HDRNo := GetHorizontalRuleNo(Y,DRMin,DRDelta)
    else
      HDRNo := -1;
    if (HDRNo>0) and CanChange and (rvtoRowSizing in Options) then begin
      DRNo := HDRNo;
      dec(DRDelta,Y);
      TCustomRVFormattedData(RVData).AssignXorDrawing(XorDrawing);
      TCustomRVFormattedData(RVData).SetMouseCapture(Self,MyLeft,MyTop);
      DRMax := -1;
      Exclude(FState, rvtsVerticalDraggedRule);
      SaveInplace;
      end
    else if (HDRNo=0) and (rvtoColSelect in Options) then begin
      Col := GetColNo(X);
      if Col>=0 then begin
        SelectCols(Col,1);
        TCustomRVFormattedData(RVData).SetMouseCapture(Self,MyLeft,MyTop);
      end;
      end
    else if (VDRNo=0) and (rvtoRowSelect in Options) then begin
      Row := GetRowNo(Y);
      if Row>=0 then begin
        SelectRows(Row,1);
        TCustomRVFormattedData(RVData).SetMouseCapture(Self,MyLeft,MyTop);
      end;
      end
//    else
//      DestroyInplace(True);
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y,ItemNo: Integer; RVData: TObject): Boolean;
  {...........................................}
  procedure SetHeight(r,h,y: Integer);
  var c,a,b: Integer;
      Changed: Boolean;
      Cell: TRVTableCellData;
  begin
    a := h;
    dec(h, CellPadding*2);
    if r<Rows.Count-1 then
      dec(h, CellVSpacing div 2 + CellBorderWidth)
    else
      dec(h, (BorderWidth+BorderVSpacing+CellBorderWidth+1) div 2);
    inc(y, h-a);
    if h<0 then h := 0;
    Changed := False;
    with Rows[r] do begin
      for c := 0 to Count-1 do
        if (Items[c]<>nil) and (Items[c].RowSpan=1) then begin
          SetCellBestHeight(h,r,c);
          Changed := True;
        end;
      for c := 0 to Count-1 do
        if (Items[c]=nil) then begin
          Cell := Rows.GetMainCell(r,c,a,b);
          if a=r then continue;
          if (r=Rows.Count-1) or
             (Cells[r+1,c]<>nil) or
             (Cell<>Rows.GetMainCell(r+1,c,a,b)) then begin
            if Changed then
              h := 0
            else begin
              h := y-Cell.Top;
              if h<0 then
                h := 0;
            end;
            SetCellBestHeight(h,a,b);
          end;
        end;
    end;
  end;
  {...........................................}
  procedure SetWidth(c,w,x: Integer; Decrement: Boolean);
  var r,a,b,oldw: Integer;
      Changed: Boolean;
      Cell: TRVTableCellData;
  begin
    oldw := w-CellPadding*2;
    if Decrement then begin
      if c<Rows[0].Count-1 then
        x := Fmt.ColStarts[c+1] - (CellBorderWidth+(CellHSpacing+1) div 2)
      else
        x := Fmt.FWidth-(BorderWidth+BorderHSpacing+CellBorderWidth{+1}) div 2;
      x  := x + (Fmt.ColStarts[c]-Fmt.ColStarts[c-1] - (CellHSpacing+1) div 2)-w-CellBorderWidth;
      w := x - Fmt.ColStarts[c];
    end;
    a := w;
    dec(w, CellPadding*2);
    if c<Rows[0].Count-1 then begin
      dec(w,CellBorderWidth);
      dec(w, CellHSpacing div 2);
      end
    else
      dec(w, (BorderWidth+BorderHSpacing+CellBorderWidth+1) div 2);
    //inc(x, w-a);
    if w<=0 then w := 1;
    Changed := False;
    for r := 0 to Rows.Count-1 do
      if (Cells[r,c]<>nil) and (Cells[r,c].ColSpan=1) then begin
        SetCellBestWidth(w,r,c);
        Changed := True;
      end;
    for r := 0 to Rows.Count-1 do
      if (Cells[r,c]=nil) then begin
        Cell := Rows.GetMainCell(r,c,a,b);
        if (b=c) or Decrement then continue;
        if (c=Rows[r].Count-1) or
           (Cells[r,c+1]<>nil) or
           (Cell<>Rows.GetMainCell(r,c+1,a,b)) then begin
          if Changed then
            w := 0
          else begin
           // w := x-Cell.Left;
            w := Cell.Width - Fmt.ColWidths[c]+oldw;
            if w<=0 then
              w := 1;
          end;
          SetCellBestWidth(w,a,b)
        end;
      end;
  end;
  {...........................................}
  procedure SetPercentWidth(c,w,x: Integer; Decrement: Boolean);
  var r,a,b, he: Integer;
      Changed: Boolean;
      Cell: TRVTableCellData;
  begin
    he := GetHorzExtra;
    a := w;
    if c<Rows[0].Count-1 then begin
      dec(w,CellBorderWidth);
      dec(w, CellHSpacing div 2);
      end
    else
      dec(w, (BorderWidth+BorderHSpacing+CellBorderWidth) div 2);
    inc(x, w-a);
    w := - w*100 div (Fmt.FWidth-he);
    if w>=0 then w := -1;
    Changed := False;
    for r := 0 to Rows.Count-1 do
      if (Cells[r,c]<>nil) and (Cells[r,c].ColSpan=1) then begin
        SetCellBestWidth(w,r,c);
        Changed := True;
      end;
    for r := 0 to Rows.Count-1 do
      if (Cells[r,c]=nil) then begin
        Cell := Rows.GetMainCell(r,c,a,b);
        if b=c then continue;
        if (c=Rows[r].Count-1) or
           (Cells[r,c+1]<>nil) or
           (Cell<>Rows.GetMainCell(r,c+1,a,b)) then begin
          if Changed then
            w := 0
          else begin
            w := -(x-Cell.Left)*100 div (Fmt.FWidth-he);
            if w>=0 then
              w := -1;
          end;
          SetCellBestWidth(w,a,b);
        end;
      end;
  end;
  {...........................................}
var pc: Boolean;
    data: Integer;
    Row,Col: Integer;
begin
  Result := False;
  if Rows.Empty then
    exit;
  if BusyCount>0 then exit;
  MyClientTop := 0;
  MyClientLeft := 0;
  if (Button=mbLeft) then begin
    if TCustomRVFormattedData(RVData).UsingThisXorDrawing(XORDrawing) then begin
      TCustomRVFormattedData(RVData).UnAssignXorDrawing(XorDrawing);
      if BeforeChange then
        if rvtsVerticalDraggedRule in FState then begin
          data := BeginModify(ItemNo);
          pc := Rows.IsPercentWidthColumn(DRNo-1);
          InitUndo;
          try
            if pc then begin
              SetPercentWidth(DRNo-1, X-DRMin+DRDelta, X+DRDelta,False);
              end
            else begin
              SetWidth(DRNo-1, X-DRMin+DRDelta, X+DRDelta,False);
              if not (rvtsDRChangeTableWidth in FState) and (DRNo<>Rows[0].Count) and
                 not Rows.IsPercentWidthColumn(DRNo) then
                SetWidth(DRNo, X-DRMin+DRDelta, X+DRDelta, True);
            end;
          finally
            DoneUndo;
          end;
          Fmt.FWidth := 0;
          EndModify(ItemNo, Data);
          Change;
          end
        else begin
          InitUndo;
          try
            SetHeight(DRNo-1, Y-DRMin+DRDelta, Y+DRDelta);
          finally
            DoneUndo;
          end;
          Fmt.FWidth := 0;
          if (Rows.FMainRVData is TRVEditRVData) then begin
            //VScroll := Rows.FMainRVData.GetVOffs;
            TRVEditRVData(Rows.FMainRVData).Reformat(False, False, False, Rows.FMainRVData.Items.IndexOfObject(Self), False);
            //Rows.FMainRVData.ScrollTo(VScroll,True);
            TCustomRVFormattedData(Rows.FMainRVData).Invalidate;
            Change;
          end;
        end;
      TCustomRVFormattedData(RVData).ReleaseMouseCapture(Self);
      RestoreInplace;
      exit;
    end;
    //TCustomRVFormattedData(RVData).GetItemClientCoords(ItemNo, MyClientTop,MyC
    MyClientTop := 0;
    MyClientLeft := 0;
    if FMakingSelection then begin
      if FInplaceEditor=nil then begin
        with Cells[FSelStartRow,FSelStartCol] do begin
          MouseUp(Button, Shift, X, Y);
        end;
        if (TCustomRVFormattedData(FRows.FMainRVData).GetChosenRVData<>nil) and
           (rvstCompletelySelected in TCustomRVFormattedData(FRows.FMainRVData).GetChosenRVData.State) then
          TCustomRVFormattedData(FRows.FMainRVData).UnassignChosenRVData(TCustomRVFormattedData(FRows.FMainRVData).GetChosenRVData);
      end
      end
    else
      if FInplaceEditor=nil then begin
        if GetCellAt(X, Y, Row, Col) then
          with Cells[Row,Col] do
            if RV_PointInRect(X, Y, Left+CellPadding, Top+CellPadding,
                              Width-CellPadding*2, Height-CellPadding*2) then
              MouseUp(Button, Shift, X, Y);
      end;
    TCustomRVFormattedData(RVData).ReleaseMouseCapture(Self);
    FMakingSelection := False;
    end
  else if FInplaceEditor=nil then begin
    if GetCellAt(X, Y, Row, Col) then begin
      with Cells[Row,Col] do
        if RV_PointInRect(X, Y, Left+CellPadding, Top+CellPadding,
                          Width-CellPadding*2, Height-CellPadding*2) then begin
          MouseUp(Button, Shift, X, Y);
          if (Button=mbRight) and (rvoRClickDeselects in Rows.FMainRVData.Options) and
             not IsCellSelected(Row,Col) and not CompletelySelected
             and not ((ChosenCellRow=Row) and (ChosenCellCol=Col) and CellIsChosen)
             then begin
            TCustomRVFormattedData(Rows.FMainRVData).SetSelectionBounds(ItemNo, 1, ItemNo, 1);
          end;
        end;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.UpdateCellXCoords(Fmt: TRVTableItemFormattingInfo;
  NoCaching: Boolean);
var r,c,l,w,i, cw: Integer;
    AllDH, StartAllDH, CurDH, CP, Cnt: Integer;
begin
  Cnt := Rows[0].Count;
  Fmt.ColStarts.Count := Cnt;
  AllDH := GetDevX((CellHSpacing+(CellPadding+FCellBorderWidth)*2)*Cnt);
  StartAllDH := AllDH;
  l := GetDevX(BorderWidth+BorderHSpacing+CellBorderWidth);
  CP := GetDevX(CellPadding);
  for c := 0 to Rows[0].Count-1 do begin
    Fmt.ColStarts[c] := l;
    CurDH := AllDH div Cnt;
    dec(AllDH,CurDH);
    inc(l, Fmt.ColWidths[c]+CurDH);
    dec(Cnt);
  end;
  for r := 0 to Rows.Count-1 do begin
    for c := 0 to Rows[r].Count-1 do begin
      l := Fmt.ColStarts[c];
      w := Fmt.ColWidths[c];
      if Cells[r,c]<>nil then begin
        cw := w + GetDevX(CellPadding*2);
        for i := 1 to Cells[r,c].ColSpan-1 do
          inc(cw, Fmt.ColWidths[c+i]);
        inc(cw, MulDiv(StartAllDH,Cells[r,c].ColSpan-1,Rows[r].Count));
        if Fmt.Rows=nil then
          with Cells[r,c] do begin
            FLeft := l;
            FWidth  := cw;
          end
        else
          if Fmt.Rows[r*Rows[0].Count+c]<>nil then
            with TCellPtblRVData(Fmt.Rows[r*Rows[0].Count+c]) do begin
              DX := l+CP;
              Width := cw-CP*2;
            end;
      end;
    end;
  end;
  if Fmt.Rows=nil then begin
    for r := 0 to Rows.Count-1 do
      with Rows[r] do
        for c := 0 to Count-1 do
          if (Items[c]<>nil) then begin
            Items[c].State := Items[c].State+[rvstDoNotMoveChildren];
            try
             Items[c].Format(NoCaching);
            finally
              Items[c].State := Items[c].State-[rvstDoNotMoveChildren];
            end;
          end;
    if (FInplaceEditor<>nil) and
       (TRVTableInplaceEdit(FInplaceEditor).FCell<>nil)  then
      FInplaceEditor.Width := TRVTableInplaceEdit(FInplaceEditor).FCell.Width;
    end
  else begin
    for r := 0 to Fmt.Rows.Count-1 do
      if Fmt.Rows[r]<>nil then
        TCellPtblRVData(Fmt.Rows[r]).Format(NoCaching);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.UpdateCellYCoords(Fmt: TRVTableItemFormattingInfo);
var r,c,t,h, i, ch: Integer;
    StartAllDV,AllDV,CurDV,CP,Cnt: Integer;
begin
  AllDV := GetDevY((CellVSpacing+(CellPadding+FCellBorderWidth)*2)*Rows.Count);
  StartAllDV := AllDV;
  CP := GetDevY(CellPadding);
  t := GetDevY(BorderWidth+BorderVSpacing+CellBorderWidth);
  Cnt := Rows.Count;
  for r := 0 to Rows.Count-1 do begin
    h := Fmt.RowHeights[r];
    Fmt.RowStarts[r] := t;
    for c := 0 to Rows[r].Count-1 do begin
      if Cells[r,c]<>nil then begin
        ch := h + GetDevY(CellPadding*2);
        for i := 1 to Cells[r,c].RowSpan-1 do
          inc(ch, Fmt.RowHeights[r+i]);
        inc(ch, MulDiv(StartAllDV,Cells[r,c].RowSpan-1,Rows.Count));
        if Fmt.Rows=nil then
          with Cells[r,c] do begin
            FTop  := t;
            FHeight := ch;
          end
        else
          if Fmt.Rows[r*Rows[0].Count+c]<>nil then
            with TCellPtblRVData(Fmt.Rows[r*Rows[0].Count+c]) do begin
              DY  := t+CP;
              Height := ch-CP*2;
            end;
      end;
    end;
    CurDV := AllDV div Cnt;
    dec(AllDV,CurDV);
    inc(t, h+CurDV);
    dec(Cnt);
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetCellAt_(X, Y: Integer; var Row,Col: Integer): Boolean;
var r,c,dx,dy: Integer;
begin
  Result := False;
  dx := Cells[0,0].Left;
  dy := Cells[0,0].Top;
  if not RV_PointInRect(X,Y,dx+1,dy+1,Fmt.FWidth-dx*2-2,Fmt.FHeight-dy*2-2) then exit;
  row := Rows.Count-1; // to change to bsearch !
  for r := 0 to Rows.Count-2 do
    if Fmt.RowStarts[r+1]>Y then begin
      row := r;
      break;
    end;
  with Rows[row] do begin
    col := Count-1;
    for c := 0 to Count-2 do // to change to bsearch !
      if (Fmt.ColStarts[c+1]>X) then begin
        col := c;
        break;
      end;
  end;
  Rows.GetMainCell(row, col, row, col);
  with Cells[row,col]  do
    Result := RV_PointInRect(X,Y, Left+1,Top+1,Width-2,Height-2);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetCellAt(X, Y: Integer; var Row, Col: Integer): Boolean;
begin
  if (MouseRow>=0) and (MouseRow<Rows.Count) and
     (MouseCol>=0) and (MouseCol<Rows[0].Count) and
     (Cells[MouseRow, MouseCol]<>nil) then
    with Cells[MouseRow, MouseCol] do
      if RV_PointInRect(X,Y, Left+1, Top+1, Width-2, Height-2) then begin
        Row := MouseRow;
        Col := MouseCol;
        Result := True;
        exit;
      end;
  Result := GetCellAt_(X,Y, Row, Col);
  if Result then begin
    MouseRow := Row;
    MouseCol := Col;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.IsCellSelected(Row, Col: Integer): Boolean;
var l,t,w,h: Integer;
    Cell: TRVTableCellData;
begin

  Result := False;
  if not (rvtsSelExists in FState) then
    exit;
  Cell := FRows.GetMainCell(Row,Col,Row,Col);
  GetNormalizedSelectionBounds(False, t,l,w,h);
  if not (rvtsSelExists in FState) then
    exit;  
  Result := (Col+Cell.ColSpan>l) and (Col<l+w) and
            (Row+Cell.RowSpan>t) and (Row<t+h);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InsertCols(Index, Count, CopyIndex: Integer
                                      {$IFDEF RICHVIEWDEF4};Select:Boolean=True{$ENDIF});
begin
  if (Index<0) or (Index>Rows[0].Count) or
     (CopyIndex<-1) or (CopyIndex>=Rows[0].Count) then
    raise ERichViewError.Create(errInvalidIndex);
  DestroyInplace(True);
  InitUndo;
  try
    Rows.InsertCols(Index, Count, CopyIndex, False);
  finally
    DoneUndo;
  end;
  {$IFDEF RICHVIEWDEF4}
  if Select then
    SelectCols(Index, Count);
  {$ENDIF}
  Changed;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InsertRows(Index, Count, CopyIndex: Integer
                                      {$IFDEF RICHVIEWDEF4};Select:Boolean=True{$ENDIF});
begin
  if (Index<0) or (Index>Rows.Count) or
     (CopyIndex<-1) or (CopyIndex>=Rows.Count) then
    raise ERichViewError.Create(errInvalidIndex);
  DestroyInplace(True);
  InitUndo;
  try
    Rows.InsertRows(Index, Count, CopyIndex, False);
  finally
    DoneUndo;
  end;
  {$IFDEF RICHVIEWDEF4}
  if Select then
    SelectRows(Index, Count);
  {$ENDIF}
  Changed;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InsertColsLeft(Count: Integer);
var r,c,cs,rs: Integer;
begin
  if not GetNormalizedSelectionBounds(True, r,c,cs,rs) then begin
    Rows.FMainRVData.Beep;
    exit;
  end;
  InsertCols(c, Count, c);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InsertColsRight(Count: Integer);
var r,c,cs,rs: Integer;
begin
  if not GetNormalizedSelectionBounds(True, r,c,cs,rs) then begin
    Rows.FMainRVData.Beep;
    exit;
  end;
  InsertCols(c+cs, Count, c+cs-1);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InsertRowsAbove(Count: Integer);
var r,c,cs,rs: Integer;
begin
  if not GetNormalizedSelectionBounds(True, r,c,cs,rs) then begin
    Rows.FMainRVData.Beep;
    exit;
  end;
  InsertRows(r, Count, r);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InsertRowsBelow(Count: Integer);
var r,c,cs,rs: Integer;
begin
  if not GetNormalizedSelectionBounds(True, r,c,cs,rs) then begin
    Rows.FMainRVData.Beep;
    exit;
  end;
  InsertRows(r+rs, Count, r+rs-1);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.DeleteRows(Index, Count: Integer;
                                      DecreaseHeight: Boolean);
var er,ec,mr,mc: Integer;
begin
  if Rows.Empty then
    exit;
  if (Index<0) or (Index>=Rows.Count) or (Count<=0) or
     ((Index=0) and (Count>Rows.Count)) then
    raise ERichViewError.Create(errInvalidIndex);
  if GetEditedCell(er,ec)<>nil then begin
    if er>=Index+Count then
      dec(er, Count)
  end;
  DestroyInplace(True);
  Deselect;
  InitUndo;
  try
    Rows.DeleteRows(Index,Count,DecreaseHeight);
  finally
    DoneUndo;
  end;
  if (Rows.Count>0)  and (rvtoEditing in Options) then begin
    if er<0 then begin
      ec := 0;
      er := Index;
    end;
    if er>=Rows.Count then
      er := Rows.Count-1;
    Rows.GetMainCell(er,ec,mr,mc);
    CreateInplace(-1, mr,mc, False, True, False, False, False);
  end;
  Changed;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.DeleteCols(Index, Count: Integer; DecreaseWidth: Boolean);
var er,ec,mr,mc: Integer;
begin
  if Rows.Empty then
    exit;
  if (Index<0) or (Index>=Rows[0].Count) or (Count<=0) or
     ((Index=0) and (Count>Rows[0].Count)) then
    raise ERichViewError.Create(errInvalidIndex);
  if GetEditedCell(er,ec)<>nil then begin
    if ec>=Index+Count then
      dec(ec, Count)
  end;
  DestroyInplace(True);
  Deselect;
    InitUndo;
  try
    Rows.DeleteCols(Index,Count,DecreaseWidth);
  finally
    DoneUndo;
  end;
  if (Rows.Count>0) and (Rows[0].Count>0) and CanChange and (rvtoEditing in Options) then begin
    if er<0 then begin
      er := 0;
      ec := Index;
    end;
    if ec>=Rows[er].Count then
      ec := Rows[er].Count-1;
    Rows.GetMainCell(er,ec,mr,mc);
    CreateInplace(-1, mr,mc, False, True, False, False, False);
  end;
  Changed;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.DeleteSelectedRows;
var r,c,cs,rs: Integer;
begin
  if not GetNormalizedSelectionBounds(True, r,c,cs,rs) then begin
    Rows.FMainRVData.Beep;
    exit;
  end;
  DeleteRows(r, rs, True);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.DeleteSelectedCols;
var r,c,cs,rs: Integer;
begin
  if not GetNormalizedSelectionBounds(True, r,c,cs,rs) then begin
    Rows.FMainRVData.Beep;
    exit;
  end;
  DeleteCols(c, cs, True);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.DeleteEmptyCols;
var r,c: Integer;
    empty: Boolean;
begin
  if Rows.Empty then
    exit;
  for c := Rows[0].Count-1 downto 0 do begin
    empty := True;
    for r := 0 to Rows.Count-1 do
      if Cells[r,c]<>nil then begin
        empty := False;
        break;
      end;
    if empty then
      DeleteCols(c,1,False);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.DeleteEmptyRows;
var r,c: Integer;
    empty: Boolean;
begin
  for r := Rows.Count-1 downto 0 do begin
    empty := True;
    for c := 0 to Rows[r].Count-1 do
      if Cells[r,c]<>nil then begin
        empty := False;
        break;
      end;
    if empty then
      DeleteRows(r,1,False);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.Deselect;
begin
  {
  if FInplaceEditor<>nil then begin
    FInplaceEditor.Deselect;
    FInplaceEditor.Invalidate;
    exit;
  end;
  }
  DestroyInplace(True);
  Exclude(FState, rvtsSelExists);
  FSelStartCol := -1;
  FSelStartRow := -1;
  FSelColOffs  := 0;
  FSelRowOffs  := 0;
  UpdateCellSel;
  UnAssignActiveCell;
  if Rows.FMainRVData is TCustomRVFormattedData then
    TCustomRVFormattedData(Rows.FMainRVData).Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.Select(StartRow, StartCol, RowOffs, ColOffs: Integer);
begin
  if not (rvtsInserted in FState) then
    exit;
  DestroyInplace(True);
  if not (Rows.FMainRVData is TCustomRVFormattedData) then
    exit;
  if not (rvoAllowSelection in Rows.FMainRVData.Options) then
    exit;
  TCustomRVFormattedData(Rows.FMainRVData).PartialSelectedItem := Self;
  Include(FState, rvtsSelExists);
  FSelStartCol := StartCol;
  FSelStartRow := StartRow;
  FSelColOffs  := ColOffs;
  FSelRowOffs  := RowOffs;
  UpdateCellSel;
  if Rows.FMainRVData is TCustomRVFormattedData then
    TCustomRVFormattedData(Rows.FMainRVData).Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SelectCols(StartCol, Count: Integer);
begin
  Select(0, StartCol, Rows.Count-1, Count-1);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SelectRows(StartRow, Count: Integer);
begin
  Select(StartRow, 0, Count-1, Rows[StartRow].Count-1);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetSelectionBounds(var StartRow, StartCol,
                                                 RowOffs, ColOffs: Integer): Boolean;
begin
  StartCol := FSelStartCol;
  StartRow := FSelStartRow;
  ColOffs  := FSelColOffs;
  RowOffs  := FSelRowOffs;
  Result := rvtsSelExists in FState;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetNormalizedSelectionBounds(IncludeEditedCell: Boolean;
     var TopRow, LeftCol, ColSpan, RowSpan: Integer): Boolean;
var r,c: Integer;
    Cell: TRVTableCellData;
begin

  if ((FSelStartRow<0) or (FSelStartCol<0)) and
     (rvtsSelExists in FState) then begin
    Exclude(FState, rvtsSelExists);
    TCustomRVFormattedData(FRows.FMainRVData).ReleaseMouseCapture(Self);
    if (TCustomRVFormattedData(FRows.FMainRVData).GetChosenRVData<>nil) then
      TCustomRVFormattedData(FRows.FMainRVData).SilentReplaceChosenRVData(nil);
    FMakingSelection := False;
  end;

  if not (rvtsSelExists in FState) then begin
    if IncludeEditedCell and (FInplaceEditor<>nil) then begin
      TopRow := TRVTableInplaceEdit(FInplaceEditor).FRow;
      LeftCol := TRVTableInplaceEdit(FInplaceEditor).FCol;
      Cell := Cells[TopRow,LeftCol];
      ColSpan := Cell.ColSpan;
      RowSpan := Cell.RowSpan;
      Result := True;
      exit;
      end
    else
      Result := False;
    exit;
  end;
  Cell := Rows.GetMainCell(FSelStartRow,FSelStartCol,TopRow,LeftCol);
  ColSpan := LeftCol+Cell.ColSpan-1;
  RowSpan := TopRow+Cell.RowSpan-1;
  Cell := Rows.GetMainCell(FSelStartRow+FSelRowOffs,
                           FSelStartCol+FSelColOffs,
                           r,c);
  if r<TopRow then
    TopRow := r;
  if c<LeftCol then
    LeftCol := c;
  if r+Cell.RowSpan-1>RowSpan then
    RowSpan := r+Cell.RowSpan-1;
  if c+Cell.ColSpan-1>ColSpan then
    ColSpan := c+Cell.ColSpan-1;
  dec(ColSpan, LeftCol-1);
  dec(RowSpan, TopRow-1);
  Result   := True;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.UpdateCellSel;
var r,c: Integer;
begin
  for r := 0 to Rows.Count-1 do
    with Rows[r] do
      for c := 0 to Count-1 do
        if Items[c]<>nil then begin
          if IsCellSelected(r,c) then
            Include(Items[c].State, rvstCompletelySelected)
          else begin
            if not ({not CanChange and} (r=FSelStartRow) and (c=FSelStartCol)) then
              Items[c].Deselect(nil, True);
            Exclude(Items[c].State, rvstCompletelySelected);
          end;
        end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.DeselectPartial;
begin
  Deselect;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.PartiallySelected: Boolean;
begin
  Result := rvtsSelExists in FState;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.AdjustInserted(x, y: Integer; adjusty: Boolean);
var r,c: Integer;
begin
  if (FInplaceEditor<>nil) and
     (TRVTableInplaceEdit(FInplaceEditor).FCell<>nil) then
    with TRVTableInplaceEdit(FInplaceEditor).FCell do begin
      FInplaceEditor.Tag := y+Top+GetExtraVOffs;
      SetInplaceBounds(x+Left, RV_GetYByTag(FInplaceEditor), Width, Height-GetExtraVOffs);
      FInplaceEditor.RVData.Format_(True,False,False,0,FInplaceEditor.Canvas,False,False);
    end;
  MyClientLeft := x;
  MyClientTop  := y;
  for r := 0 to Rows.Count-1 do begin
    with Rows[r] do
      for c := 0 to Count-1 do
        if Items[c]<>nil then
          Items[c].AdjustChildrenCoords;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.OwnsControl(AControl: TControl): Boolean;
var r,c,i: Integer;
begin
  Result := GetCellWhichOwnsControl(AControl, r,c,i);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.OwnsInplaceEditor(AEditor: TControl): Boolean;
begin
  Result := FInplaceEditor=AEditor;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetCellWhichOwnsControl(AControl: TControl; var ARow,ACol,AItemNo: Integer): Boolean;
var r,c: Integer;
begin
  for r := 0 to Rows.Count-1 do
    with Rows[r] do
      for c := 0 to Count-1 do
        if (Items[c]<>nil) then begin
          AItemNo := Items[c].GetRVData.FindControlItemNo(AControl);
          if (AItemNo<>-1) then begin
            ARow := r;
            ACol := c;
            Result := True;
            exit;
          end;
        end;
  Result := False;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.MergeInplaceUndo(DestroyLists: Boolean);
  {........................................................}
  procedure AddList(var List: TRVUndoList; DestList: TRVUndoList; IsRedo: Boolean);
  var ui: TRVUndoCellModify;
      UndoLimit: Integer;
  begin
//    DestList.PopIfEmpty;
    if List.Count>0 then begin
      UndoLimit := List.Limit;
      if IsRedo then
        TRVEditRVData(FRows.FMainRVData).BeginRedoSequence(rvutModifyItem,'')
      else
        TRVEditRVData(FRows.FMainRVData).BeginUndoSequence(rvutModifyItem, False);
      ui := TRVUndoCellModify.Create;
      ui.Row := TRVTableInplaceEdit(FInplaceEditor).FRow;
      ui.Col := TRVTableInplaceEdit(FInplaceEditor).FCol;
      ui.CaretItemNo := FInplaceEditor.CurItemNo;
      ui.CaretOffs   := FInplaceEditor.OffsetInCurItem;
      ui.UndoList := List;
      ui.Action := rvuModifyItem;
      ui.ItemNo := GetEditorItemNoForUndo;
      ui.IsRedo := IsRedo;
      DestList.AddInfo(ui);
      TRVUndoInfos(DestList.Items[DestList.Count-1]).CaretItemNo :=  TRVUndoInfos(List.Items[List.Count-1]).CaretItemNo;
      TRVUndoInfos(DestList.Items[DestList.Count-1]).CaretOffs :=  TRVUndoInfos(List.Items[List.Count-1]).CaretOffs;
      if DestroyLists then
        List := nil
      else begin
        List  := TRVUndoList.Create(DestList.FRVData);
        List.Limit := UndoLimit;
      end;
      end
    else
      DestList.PopIfEmpty;
  end;
  {........................................................}
begin
  if FInplaceEditor=nil then
    exit;
  AddList(TRVEditRVData(FInplaceEditor.RVData).UndoList,
          TRVEditRVData(FRows.FMainRVData).UndoList, False);
  AddList(TRVEditRVData(FInplaceEditor.RVData).RedoList,
          TRVEditRVData(FRows.FMainRVData).RedoList, True);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InplaceDeleted(Clearing: Boolean);
var Clr: Boolean;
    strg: TRVTableInplaceParamStorage;
    Cell: TCustomRVFormattedData;
    RVData: TRichViewRVData;
    Row, Col: Integer;
begin
  Clr := Clearing or (rvstClearing in TCustomRVFormattedData(Rows.FMainRVData).State) or
         not FRows.FMainRVData.GetParentControl.HandleAllocated;
  MergeInplaceUndo(True);
  Cell := TRVTableInplaceEdit(FInplaceEditor).FCell;
  Row  := TRVTableInplaceEdit(FInplaceEditor).FRow;
  Col  := TRVTableInplaceEdit(FInplaceEditor).FCol;
  StoreRVSelection(FInplaceEditor.RVData, strg);
  Cell.DrainFrom(FInplaceEditor.RVData);

  if not Clr then begin
    TCustomRVFormattedData(Rows.FMainRVData).GetItemClientCoords(GetMyItemNo,MyClientLeft,MyClientTop);
    Cell.Format(False);
    RestoreRVSelection(Cell, strg);
  end;
  FInplaceEditor := nil;
  if not Clr then begin
    TRVEditRVData(Rows.FMainRVData).Invalidate;
    TCustomRichViewEdit(TRichViewRVData(Rows.FMainRVData.GetRootData).RichView).AfterCaretMove;
    RVData := FRows.FMainRVData.GetRootData as TRichViewRVData;
    if IsInEditor and TCustomRichViewEdit(RVData.RichView).ReadOnly then
      TRVEditRVData(RVData).BuildJumpsCoords(True);
  end;
  if Assigned(FOnCellEndEdit) then
    FOnCellEndEdit(Self, Row, Col, Clr);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.MovingToUndoList(ItemNo: Integer;
  RVData, AContainerUndoItem: TObject);
begin
  DestroyInplace(False);
  if Rows.Count>0 then
    Rows.MovingToUndoList(0,0,Rows[0].Count,Rows.Count, TRVUndoInfo(AContainerUndoItem));
  Rows.FMainRVData := nil;
  inherited MovingToUndoList(ItemNo, RVData, AContainerUndoItem);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.MovingFromUndoList(ItemNo: Integer;
  RVData: TObject);
begin
  Rows.FMainRVData := TCustomRVData(RVData);
  if Rows.Count>0 then
    Rows.MovingFromUndoList(0,0,Rows[0].Count,Rows.Count);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.FinalizeUndoGroup;
begin
  MergeInplaceUndo(False);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InplaceEditorChange(Sender: TCustomRichViewEdit; ClearRedo: Boolean);
var FullReformat: Boolean;
    nwp: Integer;
    L,T,W,H: Integer;
begin
  if not (rvtsInplaceIsReformatting in FState) and
     (FInplaceEditor<>nil) and TRVTableInplaceEdit(FInplaceEditor).Resized then begin
    Fmt.FWidth := 0;
    Include(FState, rvtsInplaceIsReformatting);
    L := FInplaceEditor.Left;
    T := FInplaceEditor.Top;
    W := FInplaceEditor.Width;
    H := FInplaceEditor.Height;
    try
      nwp := FInplaceEditor.RVData.CalculateMinDocWidthPlus(nil,nil);
      FullReformat := FInplaceMinWidthPlus<>nwp;
      if FullReformat then begin
        FInplaceMinWidthPlus := nwp;
        nwp := TCustomRVFormattedData(Rows.FMainRVData).CalculateMinItemWidthPlusEx(GetMyItemNo);
        FullReformat := (nwp<>FMinWidthPlus) and
                        ((nwp>TCustomRVFormattedData(Rows.FMainRVData).DocumentWidth) or
                        (FMinWidthPlus>=TCustomRVFormattedData(Rows.FMainRVData).DocumentWidth));
        FMinWidthPlus := nwp;
      end;
      TRVEditRVData(Rows.FMainRVData).Reformat(FullReformat, False, True,
                          GetMyItemNo,
                          False);

    finally
      TRVTableInplaceEdit(FInplaceEditor).Resized;
      Exclude(FState, rvtsInplaceIsReformatting);
    end;
    if (L<>FInplaceEditor.Left) or (T<>FInplaceEditor.Top) or
       (W<>FInplaceEditor.Width) or (H<>FInplaceEditor.Height) then
      TCustomRVFormattedData(Rows.FMainRVData).Refresh;
  end;
  ChangeEx(ClearRedo);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.DestroyInplace(ReformatCell:Boolean);
begin
  if FInplaceEditor<>nil then begin
    if csDestroying in FInplaceEditor.ComponentState then
      exit;
    if False and TRVTableInplaceEdit(FInplaceEditor).Busy then
      raise ERVTableInplaceError.Create(errInplaceBusy);
    if not ReformatCell then
      TRVTableInplaceEdit(FInplaceEditor).SetClearingState
    else
      TCustomRVFormattedData(FRows.FMainRVData).UnassignChosenRVData(TRVTableInplaceEdit(FInplaceEditor).FCell);
    FInplaceEditor.Free;
    FInplaceEditor := nil;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.StartSelecting(Row,Col: Integer):Boolean;
begin
  Result := False;
  if not (rvoAllowSelection in Rows.FMainRVData.Options) then
    exit;
  FSelStartCol := Col;
  FSelStartRow := Row;
  FSelColOffs := 0;
  FSelRowOffs := 0;
  Exclude(FState, rvtsSelExists);
  TCustomRVFormattedData(FRows.FMainRVData).PartialSelectedItem := Self;
  TCustomRVFormattedData(FRows.FMainRVData).SetMouseCapture(Self,MyLeft,MyTop);
  UpdateCellSel;
  FMakingSelection := True;
  Result := True;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetBoolValue(Prop: TRVItemBoolProperty): Boolean;
begin
  case Prop of
    rvbpDrawingChangesFont, rvbpCanSaveUnicode,
    rvbpAlwaysInText, rvbpHasSubRVData, rvbpNoHTML_P:
      Result := True;
    else
      Result := inherited GetBoolValue(Prop);
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetBoolValueEx(Prop: TRVItemBoolPropertyEx; RVStyle: TRVStyle): Boolean;
begin
  case Prop of
    rvbpPrintToBMP:
      Result := False;
    else
      Result := inherited GetBoolValueEx(Prop, RVStyle);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.StoreRVSelection(RVData: TCustomRVFormattedData;
  var storage: TRVTableInplaceParamStorage);
begin
  with storage do begin
    RVData.GetSelectionBoundsEx(StartNo, StartOffs, EndNo, EndOffs, False);
    PartialSelected := RVData.PartialSelectedItem;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.RestoreRVSelection(RVData: TCustomRVFormattedData;
  const storage: TRVTableInplaceParamStorage);
begin
  with storage do begin
    RVData.SetSelectionBounds(StartNo, StartOffs, EndNo, EndOffs);
    if PartialSelected<>nil then
      RVData.PartialSelectedItem := PartialSelected;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SaveInplace;
begin
  if FInplaceEditor<>nil then begin
    if TRVTableInplaceEdit(FInplaceEditor).Busy then
      raise ERVTableInplaceError.Create(errInplaceBusy);
    StoreRVSelection(FInplaceEditor.RVData, FStoredInplace);
    FStoredInplace.Row := TRVTableInplaceEdit(FInplaceEditor).FRow;
    FStoredInplace.Col := TRVTableInplaceEdit(FInplaceEditor).FCol;
    FStoredInplace.Stored := True;
    DestroyInplace(True);
    end
  else begin
    // FStoredInplace.Stored := False;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.RestoreInplace;
begin
  if FStoredInplace.Stored then begin
    CreateInplace(-1, FStoredInplace.Row, FStoredInplace.Col, False, True, False, False, True);
    if FInplaceEditor=nil then
      exit;
    RestoreRVSelection(FInplaceEditor.RVData, FStoredInplace);
    FInplaceEditor.Invalidate;
    FStoredInplace.Stored := False;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InplaceEditorCaretGetout(Sender: TCustomRichViewEdit;
  Direction: TRVGetOutDirection);
var Dir: TRVCellDirection;
begin
  Dir := TRVCellDirection(ord(Direction));
  PostMessage(FInplaceEditor.Handle,  WM_RVMOVEEDITOR, ord(Dir),  0);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InplaceEditorControlAction(Sender: TCustomRichView;
  ControlAction: TRVControlAction; ItemNo: Integer; var ctrl: TControl);
begin
  Rows.FMainRVData.ControlAction2(ControlAction,
    TRVTableInplaceEdit(FInplaceEditor).FTableItemNo, ctrl);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InplaceEditorMouseDown(Sender: TCustomRichView;
  Button: TMouseButton; Shift: TShiftState; ItemNo, X, Y: Integer);
var p: TPoint;
    data: TRVMouseUpDownMessageData;
begin
  if (Rows.FMainRVData is TCustomRVFormattedData) then begin
    p := FInplaceEditor.ClientToScreen(Point(X,Y));
    p := TCustomRVFormattedData(Rows.FMainRVData).GetParentControl.ScreenToClient(p);
    data := TRVMouseUpDownMessageData.Create;
    data.Event  := rvetRVMouseDown;
    data.X      := p.X;
    data.Y      := p.Y;
    data.ItemNo := GetItemNoInRootDocument;
    data.Shift  := Shift;
    data.Button := Button;
    PostMessage(TCustomRVFormattedData(Rows.FMainRVData).GetParentControl.Handle,
      WM_RVEVENT, Integer(data), 0);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InplaceEditorMouseUp(Sender: TCustomRichView;
  Button: TMouseButton; Shift: TShiftState; ItemNo, X, Y: Integer);
var p: TPoint;
    data: TRVMouseUpDownMessageData;
begin
  if (Rows.FMainRVData is TCustomRVFormattedData) then begin
    p := FInplaceEditor.ClientToScreen(Point(X,Y));
    p := TCustomRVFormattedData(Rows.FMainRVData).GetParentControl.ScreenToClient(p);
    data := TRVMouseUpDownMessageData.Create;
    data.Event  := rvetRVMouseUp;
    data.X      := p.X;
    data.Y      := p.Y;
    data.ItemNo := GetItemNoInRootDocument;
    data.Shift  := Shift;
    data.Button := Button;
    PostMessage(TCustomRVFormattedData(Rows.FMainRVData).GetParentControl.Handle,
      WM_RVEVENT, Integer(data), 0);
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InplaceEditorMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var editor: TCustomRichViewEdit;
begin
  editor := TCustomRichViewEdit(TRVEditRVData(Rows.FMainRVData).RichView);
  if Assigned(editor.OnMouseMove) then
    editor.OnMouseMove(editor, Shift, X+TControl(Sender).Left, Y+TControl(Sender).Top);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InplaceEditorDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
var editor: TCustomRichViewEdit;
begin
  editor := TCustomRichViewEdit(TRVEditRVData(Rows.FMainRVData).RichView);
  if Assigned(editor.OnDragOver) then
    editor.OnDragOver(editor, Source, X+TControl(Sender).Left, Y+TControl(Sender).Top, State, Accept);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InplaceEditorDragDrop(Sender, Source: TObject; X, Y: Integer);
var editor: TCustomRichViewEdit;
begin
  editor := TCustomRichViewEdit(TRVEditRVData(Rows.FMainRVData).RichView);
  if Assigned(editor.OnDragDrop) then
    editor.OnDragDrop(editor, Source, X+TControl(Sender).Left, Y+TControl(Sender).Top);
end;
{------------------------------------------------------------------------------}
type

  TD = class (TComponent)
    private
      FCellCount: Integer;
      FRVTableSW: TComponent;
    protected
      procedure Loaded; override;
    public
      FCell: TRVTableCellData;
    published
      property C: TRVTableCellData read FCell write FCell;
  end;

  TRVTableSW = class (TComponent)
    public
      FTable: TRVTableItemInfo;
      FTD: TD;
    protected
       procedure DefineProperties(Filer: TFiler); override;
       procedure RowVAlignWriter(Writer: TWriter);
       procedure RowVAlignReader(Reader: TReader);
       function RowVAlignHasData: Boolean;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    published
      property T: TRVTableItemInfo read FTable write FTable;
  end;

procedure TD.Loaded;
begin
  inherited;
  dec(FCellCount);
  if FCellCount=0 then
    FRVTableSW.Free;
end;

constructor TRVTableSW.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTD := TD.Create(nil);
  FTD.FRVTableSW := Self;
end;

destructor TRVTableSW.Destroy;
begin
  FTD.Free;
  inherited Destroy;
end;

function TRVTableSW.RowVAlignHasData: Boolean;
var r, count: Integer;
begin
  Result := False;
  if T.StreamSaveHeadingRows then begin
    count := T.HeadingRowCount;
    if count>T.Rows.Count then
      count := T.Rows.Count;
    for r := 0 to count-1 do
      if T.Rows[r].VAlign<>rvcTop then begin
        Result := True;
        exit;
      end;
  end;
  for r := T.StreamSaveStartRow to T.StreamSaveStartRow+T.StreamSaveRowCount-1 do
    if T.Rows[r].VAlign<>rvcTop then begin
      Result := True;
      exit;
    end;
end;

procedure TRVTableSW.RowVAlignReader(Reader: TReader);
var r: Integer;
begin
  Reader.ReadListBegin;
  for r := 0 to T.Rows.Count-1 do
    T.Rows[r].VAlign := TRVCellVAlign(Reader.ReadInteger);
  Reader.ReadListEnd;
end;

procedure TRVTableSW.RowVAlignWriter(Writer: TWriter);
var r, count: Integer;
begin
  Writer.WriteListBegin;
  if T.StreamSaveHeadingRows then begin
    count := T.HeadingRowCount;
    if count>T.Rows.Count then
      count := T.Rows.Count;
    for r := 0 to count-1 do
      Writer.WriteInteger(Integer(T.Rows[r].VAlign));
  end;
  for r := T.StreamSaveStartRow to T.StreamSaveStartRow+T.StreamSaveRowCount-1 do
    Writer.WriteInteger(Integer(T.Rows[r].VAlign));
  Writer.WriteListEnd;
end;

procedure TRVTableSW.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('C', T.CellsReader, T.CellsWriter, True);
  Filer.DefineProperty('RowVAlign', RowVAlignReader, RowVAlignWriter, RowVAlignHasData);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SaveRVF(Stream: TStream; RVData: TPersistent;
  ItemNo, ParaNo: Integer; const Name: String; Part: TRVMultiDrawItemPart;
  ForceSameAsPrev: Boolean);
var SaveType, LineCount: Integer;
    StreamWrapper: TRVTableSW;
begin
  LineCount := 3+GetRVFExtraPropertyCount;
  if rvfoSaveBinary in TCustomRVData(RVData).RVFOptions then
    SaveType := 2 // save binary
  else
    SaveType := 0; // save hex dump
  RVFWriteLine(Stream,
   Format('%d %d %s %d %d %s %s',
          [StyleNo, LineCount,
           RVFSavePara(TCustomRVData(RVData).GetRVStyle,
                        rvfoUseStyleNames in TCustomRVData(RVData).RVFOptions,
                        ParaNo),
           Byte(ItemOptions) and RVItemOptionsMask,
           SaveType,
           RVFSaveTag(rvoTagsArePChars in TCustomRVData(RVData).Options,Tag),
           SaveRVFHeaderTail(RVData)]));
   RVFWriteLine(Stream, Name);
   SaveRVFExtraProperties(Stream);
   RVFWriteLine(Stream, '1');
   StreamWrapper := TRVTableSW.Create(nil);
   try
     if (Part=nil) or not (Part is TRVTablePrintPart) then begin
       StreamSaveHeadingRows := False;
       StreamSaveStartRow := 0;
       StreamSaveRowCount := Rows.Count;
       end
     else begin
       StreamSaveHeadingRows := True;
       StreamSaveStartRow := TRVTablePrintPart(Part).StartRow;
       StreamSaveRowCount := TRVTablePrintPart(Part).RowCount;
     end;
     StreamWrapper.T := Self;
     if rvfoSaveBinary in TCustomRVData(RVData).RVFOptions then
       RVFSaveControlBinary(Stream, StreamWrapper)
     else
       RVFWriteLine(Stream, RVFSaveControl(StreamWrapper));
   finally
     StreamWrapper.Free;
   end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.ReadRVFLine(const s: String; RVData: TPersistent;
  ReadType, LineNo, LineCount: Integer; var Name: String; var ReadMode: TRVFReadMode;
  var ReadState: TRVFReadState): Boolean;
var StreamWrapper: TRVTableSW;
begin
  Result := True;
  if LineNo=0 then begin
    Name := s;
    exit;
  end;
  case LineCount-LineNo of
    2:
      begin
        //  do nothing; this line contains version of table stream format; currently '1'
        if ReadType=2 then
          ReadMode := rmBeforeBinary;
      end;
    1:
      begin
        StreamWrapper := TRVTableSW.Create(nil);
        try
          StreamWrapper.T := Self;
          if ReadType=2 then
            RVFLoadControlBinary(s, TComponent(StreamWrapper), '', nil)
          else
            Result := RVFLoadControl(s, TComponent(StreamWrapper), '', nil);
          ReadState := rstSkip;
        finally
          // StreamWrapper.Free;
        end;
      end;
    else
      SetExtraPropertyFromRVFStr(s);
  end;
end;
{------------------------------------------------------------------------------}
// rvtsJustCreated is set in constructor. This method is called just after
// constructor when loading table from RTF or RVF.
procedure TRVTableItemInfo.BeforeLoading(FileFormat: TRVLoadFormat);
begin
  if FileFormat in [rvlfRVF, rvlfRTF, rvlfOther] then
    Exclude(FState, rvtsJustCreated);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSELISTS}
// This function is used to update list of marker when table is inserted from
// RVF or RTF
// When inserting nested tables, this method is called only for root table
// See also comments to Inserted
procedure TRVTableItemInfo.AfterLoading(FileFormat: TRVLoadFormat);
var r,c,i: Integer;
begin
  for r := 0 to Rows.Count-1 do
    for c := 0 to Rows[r].Count-1 do
      if Cells[r,c]<>nil then
        with Cells[r,c].GetRVData do
          for i := 0 to Items.Count-1 do begin
            AddMarkerInList(i);
            GetItem(i).AfterLoading(FileFormat);
          end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure ReadError;
begin
  raise EReadError.Create(errReadCells);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.CellsReader(Reader: TReader);
var r,c,colspan,rowspan: Integer;
    CellWrapper:TD;
begin
  ClearTemporal;
  r := Reader.ReadInteger;
  if r=0 then begin
    Rows.Reset(0,0);
    exit;
  end;
  c := Reader.ReadInteger;
  Rows.Reset(r,c);
  Reader.ReadListBegin;
  CellWrapper := (Reader.Root as TRVTableSW).FTD;
  for r := 0 to Rows.Count-1 do
    for c := 0 to Rows[r].Count-1 do
      if Cells[r,c]<>nil then begin
        if Reader.EndOfList then
          ReadError;
        colspan := Reader.ReadInteger;
        rowspan := Reader.ReadInteger;
        Rows.MergeCells(r,c,colspan,rowspan,True,True);
        CellWrapper.C := Cells[r,c];
        Reader.ReadComponent(CellWrapper);
        inc(CellWrapper.FCellCount);
      end;
  if not Reader.EndOfList then
    ReadError;
  Reader.ReadListEnd;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.CellsWriter(Writer: TWriter);
var CellWrapper:TD;
    r, count: Integer;
    {........................................................}
    procedure WriteRow(r: Integer);
    var c: Integer;
    begin
      for c := 0 to Rows[r].Count-1 do
        if Cells[r,c]<>nil then begin
          CellWrapper.C := Cells[r,c];
          Writer.WriteInteger(CellWrapper.C.ColSpan);
          Writer.WriteInteger(CellWrapper.C.RowSpan);
          Writer.WriteComponent(CellWrapper);
        end;
    end;
    {........................................................}
begin
  StartExport;
  try
    if StreamSaveHeadingRows then begin
      count := HeadingRowCount;
      if count>Rows.Count then
        count := Rows.Count;
      end
    else
      count := 0;
    Writer.WriteInteger(StreamSaveRowCount+count);      
    if StreamSaveRowCount+count>0 then begin
      Writer.WriteInteger(Rows[0].Count);
      Writer.WriteListBegin;
      CellWrapper := (Writer.Root as TRVTableSW).FTD;
      for r := 0 to count-1 do
        WriteRow(r);
      for r := StreamSaveStartRow to StreamSaveStartRow+StreamSaveRowCount-1 do
        WriteRow(r);
      Writer.WriteListEnd;
    end;
  finally
    EndExport;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.Print(Canvas: TCanvas; x, y, x2: Integer;
  Preview, Correction: Boolean; const sad: TRVScreenAndDevice;
  RichView: TRVScroller; dli: TRVDrawLineInfo;
  Part: Integer; ColorMode: TRVColorMode; RVData: TPersistent);
var State: TRVItemDrawStates;
    TablePart : TRVTablePrintPart;
    StartRow, RowCount: Integer;
    UHRC: Boolean;
begin
  if Part>=0 then begin
    TablePart := TRVTablePrintPart((dli as TRVTablePrintInfo).PartsList[Part]);
    StartRow := TablePart.StartRow;
    RowCount := TablePart.RowCount;
    UHRC := TRVTablePrintInfo(dli).FUseHeadingRowCount;
    end
  else begin
    StartRow := 0;
    RowCount := Rows.Count;
    UHRC := False;
  end;
  State := [rvidsPrinting];
  if Preview then
    Include(State, rvidsPreview);
  if Correction then
    Include(State, rvidsPreviewCorrection);
  cursad := @(TRVTablePrintInfo(dli).sad);
  try
    PaintTo(x,x2,y, StartRow, RowCount, Canvas, State, nil,
      TRVTablePrintInfo(dli).Fmt, UHRC,
      Rect(x,y,x+TRVTablePrintInfo(dli).Fmt.FWidth,y+TRVTablePrintInfo(dli).Fmt.FHeight),
      ColorMode, TCustomPrintableRVData(RVData));
  finally
    cursad := nil;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.CreatePrintingDrawItem(RVData: TObject; const sad: TRVScreenAndDevice): TRVDrawLineInfo;
var r,c: Integer;
    item : TCellPtblRVData;
    data: TCustomRVFormattedData;
    Cell: TRVTableCellData;
begin
  Result := TRVTablePrintInfo.Create(Self);
  TRVTablePrintInfo(Result).sad := sad;
  TRVTablePrintInfo(Result).Fmt.Rows.Capacity := Rows.Count*Rows[0].Count;
  for r := 0 to Rows.Count-1 do
    for c := 0 to Rows[r].Count-1 do begin
      Cell := Cells[r,c];
      if (Cell<>nil) then begin
        data := TCustomRVFormattedData(Cell.GetRVData);
        if (Cell.ColSpan=1) and (Cell.RowSpan=1) and not Cell.HasData(True) then
          data := nil;
        if data<>nil then begin
          item := TCellPtblRVData.Create(
                  (TCustomRVData(RVData).GetRootData as TCustomMainPtblRVData).RichView,
                  data, RVData as TCustomPrintableRVData);
          item.FColor := GetCellColor(Cell);
          item.ParentDrawsBack := True;
          item.Transparent := Cell.IsTransparent;
          end
        else
          item := nil;
        end
      else
        item := nil;
      TRVTablePrintInfo(Result).Fmt.Rows.Add(item);
    end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.DrawBackgroundForPrinting(Canvas: TCanvas;
  const Rect, FullRect: TRect; ColorMode: TRVColorMode; ItemBackgroundLayer: Integer);
var Clr: TColor;
    r: TRect;
begin
  if ItemBackgroundLayer=0 then
    exit;
  if FColor=clNone then
    Clr := clNone
  else if (rvtoWhiteBackground in PrintOptions) then
    Clr := clWhite
  else
    Clr := RV_GetBackColor(FColor, ColorMode);
  if FBackground<>nil then begin
    FBackground.Draw(Canvas, Rect, 0, 0, FullRect.Left, FullRect.Top,
      FullRect.Right-FullRect.Left, FullRect.Bottom-FullRect.Top, Clr, False);
    end
  else if Clr<>clNone then begin
    Canvas.Brush.Color := Clr;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rect);
  end;
  if (ItemBackgroundLayer=1) or (FPrintCell=nil) then
    exit;
  if FPrintCell.Color=clNone then
    Clr := clNone
  else if (rvtoWhiteBackground in PrintOptions) then
    Clr := clWhite
  else
    Clr := RV_GetBackColor(FPrintCell.Color, ColorMode);
  if FPrintCell.FBackground<>nil then begin
    r := FPrintCellRect;
    OffsetRect(r, -Rect.Left+FullRect.Left, -Rect.Top+FullRect.Top);
    FPrintCell.FBackground.Draw(Canvas, Rect, 0, 0, r.Left, r.Top,
      r.Right-r.Left, r.Bottom-r.Top, Clr, True);
    end
  else if Clr<>clNone then begin
    Canvas.Brush.Color := Clr;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(Rect);
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetDevX(x: Integer): Integer;
begin
  if cursad=nil then
    Result := x
  else
    Result := MulDiv(x, cursad.ppixDevice, cursad.ppixScreen);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetDevY(y: Integer): Integer;
begin
  if cursad=nil then
    Result := y
  else
    Result := MulDiv(y, cursad.ppiyDevice, cursad.ppiyScreen);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.LoadFromStream(Stream: TStream);
var StreamWrapper :TRVTableSW;
begin
   StreamWrapper := TRVTableSW.Create(nil);
   try
     StreamWrapper.T := Self;
    Stream.ReadComponent(StreamWrapper);
   finally
     // StreamWrapper.Free;
   end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SaveRowsToStream(Stream: TStream; Index,
  Count: Integer);
var StreamWrapper :TRVTableSW;
begin
  StreamSaveHeadingRows := False;
  StreamSaveStartRow := Index;
  StreamSaveRowCount := Count;
  StreamWrapper := TRVTableSW.Create(nil);
  try
    StreamWrapper.T := Self;
    Stream.WriteComponent(StreamWrapper);
  finally
    StreamWrapper.Free;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SaveToStream(Stream: TStream);
begin
  SaveRowsToStream(Stream, 0, Rows.Count);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.StartExport;
begin
  inc(BusyCount);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.EndExport;
begin
  dec(BusyCount);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.Inserting(RVData: TObject; var Text: String; Safe: Boolean);
var r,c: Integer;
begin
  Rows.FMainRVData := TCustomRVData(RVData);
  if RVData<>nil then
    Include(FState, rvtsInserted)
  else
    Exclude(FState, rvtsInserted);
  if (rvtsInserted in FState) and (TCustomRVData(RVData).GetRootData is TRVEditRVData) then
    Include(FState, rvtsEditMode)
  else
    Exclude(FState, rvtsEditMode);
  for r := 0 to Rows.Count-1 do begin
    for c := 0 to Rows[r].Count-1 do
      if (Cells[r,c]<>nil) then begin
        if Cells[r,c].ItemCount=0 then
          Cells[r,c].AddNL('',0,0);
        Cells[r,c].FList := Rows[r];
        Cells[r,c].Inserting(Cells[r,c], Safe);
      end;
  end;
  inherited Inserting(RVData, Text, Safe);
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSELISTS}
// This method is called after the table is inserted in RVData.
// We processing only the first direct inserting
// For RVF and RTF, we use AfterLoading
procedure TRVTableItemInfo.Inserted(RVData: TObject; ItemNo: Integer);
var r,c,i: Integer;
    CellRVData: TCustomRVData;
    Location: TRVStoreSubRVData;
begin
  inherited;
  if not (rvtsJustCreated in FState) then
    exit;
      
  // May be this table was inserted in table that was not inserted yet?
  // If yes, exiting (this procedure will be called later again recursively when
  // the parent table will be inserted).
  if not (rvflRoot in TCustomRVData(RVData).Flags) then begin
    TCustomRVData(RVData).GetParentInfo(i, Location);
    Location.Free;
    if i<0 then
      exit;
  end;

  try
    for r := 0 to Rows.Count-1 do
      for c := 0 to Rows[r].Count-1 do
        if Cells[r,c]<>nil then begin
          CellRVData := Cells[r,c].GetRVData;
          for i := 0 to CellRVData.Items.Count-1 do begin
            CellRVData.AddMarkerInList(i);
            CellRVData.GetItem(i).Inserted(CellRVData, i);
          end;
        end;
  finally
    Exclude(FState, rvtsJustCreated);
  end;
end;
{$ENDIF}
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.BackgroundImageReader(Stream: TStream);
var s: String;
    v: Integer;
    gr: TGraphic;
begin
  Stream.ReadBuffer(v, sizeof(v));
  SetLength(s, v);
  Stream.ReadBuffer(PChar(s)^, v);
  gr := RV_CreateGraphics(TGraphicClass(GetClass(s)));
  RVFLoadPictureBinary2(Stream, gr);
  BackgroundImage := gr;
  gr.Free;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.BackgroundImageWriter(Stream: TStream);
var s: String;
    v: Integer;
begin
  s := FBackground.Image.ClassName;
  v := Length(s);
  Stream.WriteBuffer(v, sizeof(v));
  Stream.WriteBuffer(PChar(s)^, v);
  RVFSavePictureBinary(Stream, FBackground.Image);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineBinaryProperty('BackgroundImg', BackgroundImageReader, BackgroundImageWriter,
    (FBackground<>nil) and not FBackground.Empty);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetBestWidth(const Value: TRVHTMLLength);
begin
  if rvtsInserted in FState then
    SetProperty('BestWidth', ord(Value), True, True)
  else
    FBestWidth := Value;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetBorderColor(const Value: TColor);
begin
  if rvtsInserted in FState then
    SetProperty('BorderColor', ord(Value), False, False)
  else
    FBorderColor := Value;
end;

procedure TRVTableItemInfo.SetBorderHSpacing(const Value: Integer);
begin
  if rvtsInserted in FState then
    SetProperty('BorderHSpacing', ord(Value), True, True)
  else
    FBorderHSpacing := Value;
end;

procedure TRVTableItemInfo.SetBorderLightColor(const Value: TColor);
begin
  if rvtsInserted in FState then
    SetProperty('BorderLightColor', ord(Value), False, False)
  else
    FBorderLightColor := Value;
end;

procedure TRVTableItemInfo.SetBorderStyle(const Value: TRVTableBorderStyle);
begin
  if rvtsInserted in FState then
    SetProperty('BorderStyle', ord(Value), False, False)
  else
    FBorderStyle := Value;
end;

procedure TRVTableItemInfo.SetBorderVSpacing(const Value: Integer);
begin
  if rvtsInserted in FState then
    SetProperty('BorderVSpacing', ord(Value), True, False)
  else
    FBorderVSpacing := Value;
end;

procedure TRVTableItemInfo.SetBorderWidth(const Value: Integer);
begin
  if rvtsInserted in FState then
    SetProperty('BorderWidth', ord(Value), True, True)
  else
    FBorderWidth := Value;
end;

procedure TRVTableItemInfo.SetCellBorderColorProp(const Value: TColor);
begin
  if rvtsInserted in FState then
    SetProperty('CellBorderColor', ord(Value), False, False)
  else
    FCellBorderColor := Value;
end;

procedure TRVTableItemInfo.SetCellBorderLightColorProp(const Value: TColor);
begin
  if rvtsInserted in FState then
    SetProperty('CellBorderLightColor', ord(Value), False, False)
  else
    FCellBorderLightColor := Value;
end;

procedure TRVTableItemInfo.SetCellBorderWidth(const Value: Integer);
begin
  if rvtsInserted in FState then
    SetProperty('CellBorderWidth', ord(Value), True, True)
  else
    FCellBorderWidth := Value;
end;

procedure TRVTableItemInfo.SetCellHSpacing(const Value: Integer);
begin
  if rvtsInserted in FState then
    SetProperty('CellHSpacing', ord(Value), True, True)
  else
    FCellHSpacing := Value;
end;

procedure TRVTableItemInfo.SetCellPadding(const Value: Integer);
begin
  if FInplaceEditor<>nil then begin
    FInplaceEditor.LeftMargin   := Value;
    FInplaceEditor.TopMargin    := Value;
    FInplaceEditor.RightMargin  := Value;
    FInplaceEditor.BottomMargin := Value;
    Include(FState, rvtsFormatInplace);
  end;
  if rvtsInserted in FState then
    SetProperty('CellPadding', ord(Value), True, True)
  else
    FCellPadding := Value;
end;

procedure TRVTableItemInfo.SetCellVSpacing(const Value: Integer);
begin
  if rvtsInserted in FState then
    SetProperty('CellVSpacing', ord(Value), True, False)
  else
    FCellVSpacing := Value;
end;

procedure TRVTableItemInfo.SetColor(const Value: TColor);
begin
  if rvtsInserted in FState then
    SetProperty('Color', ord(Value), False, False)
  else
    FColor := Value;
  if FInplaceEditor<>nil then begin
    FInplaceEditor.Color := GetCellColor(TRVTableInplaceEdit(FInplaceEditor).FCell);
    TRVTableInplaceEdit(FInplaceEditor).Transparent :=
      CanSeeBackgroundThroughCell(TRVTableInplaceEdit(FInplaceEditor).FCell);
  end;
end;

procedure TRVTableItemInfo.SetHeadingRowCount(const Value: Integer);
begin
  if rvtsInserted in FState then
    SetProperty('HeadingRowCount', ord(Value), False, False)
  else
    FHeadingRowCount := Value;
end;

procedure TRVTableItemInfo.SetHOutermostRule(const Value: Boolean);
begin
  if rvtsInserted in FState then
    SetProperty('HOutermostRule', ord(Value), False, False)
  else
    FHOutermostRule := Value;
end;

procedure TRVTableItemInfo.SetHRuleColor(const Value: TColor);
begin
  if rvtsInserted in FState then
    SetProperty('HRuleColor', ord(Value), False, False)
  else
    FHRuleColor := Value;
end;

procedure TRVTableItemInfo.SetHRuleWidth(const Value: Integer);
begin
  if rvtsInserted in FState then
    SetProperty('HRuleWidth', ord(Value), False, False)
  else
    FHRuleWidth := Value;
end;

procedure TRVTableItemInfo.SetVOutermostRule(const Value: Boolean);
begin
  if rvtsInserted in FState then
    SetProperty('VOutermostRule', ord(Value), False, False)
  else
    FVOutermostRule := Value;
end;

procedure TRVTableItemInfo.SetVRuleColor(const Value: TColor);
begin
  if rvtsInserted in FState then
    SetProperty('VRuleColor', ord(Value), False, False)
  else
    FVRuleColor := Value;
end;

procedure TRVTableItemInfo.SetVRuleWidth(const Value: Integer);
begin
  if rvtsInserted in FState then
    SetProperty('VRuleWidth', ord(Value), False, False)
  else
    FVRuleWidth := Value;
end;

procedure TRVTableItemInfo.SetCellBorderStyle(
  const Value: TRVTableBorderStyle);
begin
  if rvtsInserted in FState then
    SetProperty('CellBorderStyle', ord(Value), False, False)
  else
    FCellBorderStyle := Value;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellProperty(ItemNo: Integer; const PropertyName: String;
  Value: LongInt; Row, Col: Integer; AffectSize, AffectWidth: Boolean);
var fi:Boolean;
    ui: TRVUndoModifyCellIntProperty;
begin
  if (rvtsInserted in FState) and IsInEditor then
    try
      Exclude(FState, rvtsInserted);
      if ItemNo=-1 then
        ItemNo := GetMyItemNo;
      TRVEditRVData(FRows.FMainRVData).BeginUndoSequence(rvutModifyItem, True);
      ui := TRVEditRVData(FRows.FMainRVData).Do_ModifyItemIntProperty(
        ItemNo, Cells[Row,Col], PropertyName, Value,
        AffectSize, AffectWidth, TRVUndoModifyCellIntProperty) as  TRVUndoModifyCellIntProperty;
      if ui<>nil then begin
        ui.Row := Row;
        ui.Col := Col;
      end;
    finally
      Include(FState, rvtsInserted);
    end
  else begin
    fi := rvtsInserted in FState;
    try
      Exclude(FState, rvtsInserted);
      SetOrdProp(Cells[Row,Col], GetPropInfo(Cells[Row,Col].ClassInfo,  PropertyName), Value);
    finally
      if fi then
        Include(FState, rvtsInserted);
    end;
  end;
  Changed;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.AssignCellAttributes(ItemNo,Row, Col: Integer;
  SourceCell: TRVTableCellData;IncludeSize: Boolean;
  DivColSpan, DivRowSpan: Integer);
var ui: TRVUndoModifyCellIntProperties;
    Cell: TRVTableCellData;
    PropList: TStringList;
begin
  Cell := Cells[Row,Col];
  if (rvtsInserted in FState) and IsInEditor then
    try
      if ItemNo=-1 then
        ItemNo := GetMyItemNo;
      with SourceCell.VisibleBorders do
        SetCellVisibleBorders_(ItemNo, Left,Top,Right,Bottom,Row,Col);
      SetCellBackgroundImage(SourceCell.BackgroundImage, Row,Col);
      PropList := TStringList.Create;
      try
        PropList.AddObject('Color', TObject(SourceCell.Color));
        PropList.AddObject('BorderColor', TObject(SourceCell.BorderColor));
        PropList.AddObject('BorderLightColor', TObject(SourceCell.BorderLightColor));
        PropList.AddObject('VAlign', TObject(SourceCell.VAlign));
        PropList.AddObject('BackgroundStyle', TObject(SourceCell.BackgroundStyle));        
        if IncludeSize then begin
          PropList.AddObject('BestWidth', TObject(SourceCell.BestWidth div DivColSpan));
          PropList.AddObject('BestHeight', TObject(SourceCell.BestHeight div DivRowSpan));
        end;
        TRVEditRVData(FRows.FMainRVData).BeginUndoSequence(rvutModifyItem, True);
        ui := TRVEditRVData(FRows.FMainRVData).Do_ModifyItemIntProperties(
        ItemNo, Cell, PropList,
        IncludeSize, IncludeSize, TRVUndoModifyCellIntProperties) as TRVUndoModifyCellIntProperties;
        if ui<>nil then begin
          ui.Row := Row;
          ui.Col := Col;
        end;
      finally
        PropList.Free;
      end;
    finally
      Include(FState, rvtsInserted);
    end
  else begin
    Cells[Row,Col].AssignAttributesFrom(SourceCell, IncludeSize, DivColSpan, DivRowSpan);
  end;
  Changed;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetProperty(const PropertyName: String; Value: LongInt;
                                       AffectSize, AffectWidth: Boolean);
var fi:Boolean;
begin
  if (rvtsInserted in FState) and IsInEditor then
    try
      MergeInplaceUndo(False);
      Exclude(FState, rvtsInserted);
      TRVEditRVData(FRows.FMainRVData).BeginUndoSequence(rvutModifyItem, True);
      TRVEditRVData(FRows.FMainRVData).Do_ModifyItemIntProperty(
        GetMyItemNo, Self, PropertyName, Value,
        AffectSize, AffectWidth, TRVUndoModifyItemIntProperty);
      TCustomRVFormattedData(Rows.FMainRVData).Invalidate;
    finally
      Include(FState, rvtsInserted);
    end
  else begin
    fi := rvtsInserted in FState;
    try
      Exclude(FState, rvtsInserted);
      SetOrdProp(Self, GetPropInfo(Self.ClassInfo,  PropertyName), Value);
    finally
      if fi then
        Include(FState, rvtsInserted);
    end;
  end;
  Changed;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.BeforeUndoChangeProperty;
begin
  Exclude(FState, rvtsInserted)
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.AfterUndoChangeProperty;
begin
  Include(FState, rvtsInserted);
  Changed;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SplitSelectedCellsVertically(ColCount: Integer);
var r,c,cs,rs: Integer;
    ColsAdded: Integer;
begin
  if not GetNormalizedSelectionBounds(True, r,c,cs,rs) then
    exit;
  DestroyInplace(True);
  InitUndo;
  try
    ColsAdded := Rows.SplitCellsVertically(r,c,cs,rs, ColCount);
    Select(r,c,rs-1,cs+ColsAdded-1);    
  finally
    DoneUndo;
  end;
  Changed;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SplitSelectedCellsHorizontally(
  RowCount: Integer);
var r,c,cs,rs: Integer;
    RowsAdded: Integer;
begin
  if not GetNormalizedSelectionBounds(True, r,c,cs,rs) then
    exit;
  DestroyInplace(True);
  InitUndo;
  try
    RowsAdded := Rows.SplitCellsHorizontally(r,c,cs,rs, RowCount);
    Select(r,c,rs+RowsAdded-1,cs-1);
  finally
    DoneUndo;
  end;
  Changed;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.CanMergeSelectedCells(AllowMergeRC: Boolean): Boolean;
var r,c,cs,rs: Integer;
begin
  Result := False;
  if not (rvtsSelExists in FState) then
    exit;
  GetNormalizedSelectionBounds(False, r,c,cs,rs);
  Result := Rows.CanMergeCells(r,c,cs,rs, AllowMergeRC);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.CanMergeCells(TopRow, LeftCol, ColSpan,
  RowSpan: Integer; AllowMergeRC: Boolean): Boolean;
begin
  Result := Rows.CanMergeCells(TopRow, LeftCol, ColSpan, RowSpan, AllowMergeRC);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.MergeCells(TopRow, LeftCol, ColSpan,
  RowSpan: Integer; AllowMergeRC: Boolean);
begin
  DestroyInplace(True);
  InitUndo;
  try
    Rows.MergeCells(TopRow, LeftCol, ColSpan, RowSpan, AllowMergeRC, True);
  finally
    DoneUndo;
  end;
  Changed;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.MergeSelectedCells(AllowMergeRC: Boolean);
var r,c,cs,rs: Integer;
begin
  if not (rvtsSelExists in FState) then
    exit;
  GetNormalizedSelectionBounds(False, r,c,cs,rs);
  MergeCells(r,c,cs,rs,AllowMergeRC);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.UnmergeCells(TopRow, LeftCol, ColSpan,
  RowSpan: Integer; UnmergeRows, UnmergeCols: Boolean);
begin
  SaveInplace;
  InitUndo;
  try
    Rows.UnmergeCells(TopRow, LeftCol, ColSpan, RowSpan, UnmergeRows, UnmergeCols);
  finally
    DoneUndo;
    RestoreInplace;
    Changed;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.UnmergeSelectedCells(UnmergeRows, UnmergeCols: Boolean);
var r,c,cs,rs: Integer;
begin
  if not GetNormalizedSelectionBounds(True, r,c,cs,rs) then
    exit;
  UnmergeCells(r,c,cs,rs,UnmergeRows, UnmergeCols);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetEditedCell(var Row, Col: Integer): TCustomRichViewEdit;
begin
  if FInplaceEditor<>nil then begin
    Result := FInplaceEditor;
    Row := TRVTableInplaceEdit(FInplaceEditor).FRow;
    Col := TRVTableInplaceEdit(FInplaceEditor).FCol;
    end
  else begin
    Result := nil;
    Row := -1;
    Col := -1;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.Change;
begin
  ChangeEx(True);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.ChangeEx(ClearRedo: Boolean);
begin
  if (rvtsInserted in FState) and (Rows.FMainRVData is TRVEditRVData) then
    TRVEditRVData(Rows.FMainRVData).ChangeEx(ClearRedo);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.BeginModify(ItemNo: Integer): Integer;
begin
  if (rvtsInserted in FState) and (Rows.FMainRVData is TRVEditRVData) then
    TRVEditRVData(Rows.FMainRVData).BeginItemModify(ItemNo, Result)
  else
    Result := 0;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.EndModify(ItemNo, Data: Integer);
begin
  if (rvtsInserted in FState) and (Rows.FMainRVData is TRVEditRVData) then
    TRVEditRVData(Rows.FMainRVData).EndItemModify(ItemNo, data);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.CreateTemporalEditor: TCustomRichViewEdit;
var ParentEditor: TCustomRichViewEdit;
begin
  Result := TCustomRichViewEdit.Create(nil);
  ParentEditor := TRVEditRVData(Rows.FMainRVData).RichView as TCustomRichViewEdit;
  Result.Visible := False;
  Result.Parent := ParentEditor;
  Result.Options := ParentEditor.Options;
  Result.EditorOptions := ParentEditor.EditorOptions;
  Result.Style := ParentEditor.Style;
  if ParentEditor.UndoLimit=0 then
    Result.UndoLimit := 0
  else
    Result.UndoLimit := -1;
end;
{------------------------------------------------------------------------------}
// Operation = 0 => ApplyTextStyle
//             1 => ApplyParaStyle
//             2 => ApplyParaStyleConversion 
procedure TRVTableItemInfo.ApplyToCells(Operation, UserData: Integer;
  SelectedOnly: Boolean);
var tr,lc,rs,cs,r,c,i: Integer;
    ItemNo, Data: Integer;
    Editor: TCustomRichViewEdit;
    ParentEditor: TCustomRichViewEdit;
    ui: TRVUndoMultiCellsModify;
begin
  if not (rvtoEditing in Options) then
    exit;
  ItemNo := GetMyItemNo;
  Data := 0;
  if SelectedOnly then begin
    if not GetNormalizedSelectionBounds(False, tr, lc,cs,rs) or not BeforeChange then
      exit;
    Data := BeginModify(ItemNo);
    InitUndo;
  end;
  Editor := CreateTemporalEditor;
  try
    ParentEditor := TCustomRichViewEdit(TRVEditRVData(Rows.FMainRVData).RichView);
    ui := TRVUndoMultiCellsModify(
             AddTableUndoInfo(TRVEditRVData(Rows.FMainRVData), TRVUndoMultiCellsModify,
                             ItemNo, True, True));
    if ui<>nil then begin
      ui.ItemNo := ItemNo;
      ui.UndoList  := TRVUndoList.Create(TCustomRVFormattedData(ParentEditor.RVData.GetAbsoluteRootData));
      ui.RowList   := TRVIntegerList.Create;
      ui.ColList   := TRVIntegerList.Create;
      ui.CountList := TRVIntegerList.Create;
    end;
    case Operation of
      0:
        Editor.OnStyleConversion := ParentEditor.FCurStyleConversion;
      2:
        Editor.OnParaStyleConversion := ParentEditor.FCurStyleConversion;
    end;
    for r := 0 to Rows.Count-1 do
      with Rows[r] do
        for c := 0 to Count-1 do
          if (Items[c]<>nil) and (not SelectedOnly or IsCellSelected(r,c)) and
             DoOnCellEditing(r,c,True) then begin
            Editor.RVData.Clear;
            Editor.RVData.DrainFrom(Items[c]);
            Editor.Format;
            Editor.SelectAll;
            Editor.RVData.State := Editor.RVData.State + [rvstSkipFormatting];
            case Operation of
              0:
                Editor.ApplyStyleConversion(UserData);
              1:
                Editor.ApplyParaStyle(UserData);
              2:
                Editor.ApplyParaStyleConversion(UserData);
            end;
            Editor.RVData.State := Editor.RVData.State - [rvstSkipFormatting];
            if TRVEditRVData(Editor.RVData).UndoList.Count>0 then begin
              if ui<>nil then begin
                ui.RowList.Add(r);
                ui.ColList.Add(c);
                ui.CountList.Add(TRVEditRVData(Editor.RVData).UndoList.Count);
                for i := 0 to TRVEditRVData(Editor.RVData).UndoList.Count-1 do
                  ui.UndoList.AddInfos(TObject(TRVEditRVData(Editor.RVData).UndoList.Items[i]) as TRVUndoInfos);
                for i := TRVEditRVData(Editor.RVData).UndoList.Count-1 downto 0 do
                  TRVEditRVData(Editor.RVData).UndoList.DeleteAsPointer(i);
              end;
              Changed;
            end;
            Items[c].DrainFrom(Editor.RVData);
          end;
    //!!if ui<>nil then
    //!!  ui.UndoList.ChangeRVData(TRVEditRVData(Rows.FMainRVData),False);
  finally
    Editor.Free;
  end;
  if SelectedOnly then begin
    if rvtsModified in FState then
      EndModify(ItemNo, Data);
    DoneUndo;
    Change;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.ApplyParaStyleToSubRVDatas(ParaNo: Integer;
 SelectedOnly: Boolean);
begin
  ApplyToCells(1, ParaNo, SelectedOnly);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.ApplyParaStyleConversionToSubRVDatas(UserData: Integer;
  SelectedOnly: Boolean);
begin
  ApplyToCells(2, UserData, SelectedOnly);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.ApplyStyleConversionToSubRVDatas(UserData: Integer;
  SelectedOnly: Boolean);
begin
  ApplyToCells(0, UserData, SelectedOnly);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.CanDeletePartiallySelected: Boolean;
var tr,lc,rs,cs,r,c: Integer;
begin
  Result := rvtoEditing in Options;
  if not Result then
    exit;
  if not GetNormalizedSelectionBounds(False, tr, lc,cs,rs) then
    exit;
  if not (Rows.FMainRVData is TRVEditRVData) then
    exit;
  for r := 0 to Rows.Count-1 do
    with Rows[r] do
      for c := 0 to Count-1 do
        if (Items[c]<>nil) and IsCellSelected(r,c) then
          if not Items[c].CanClear then begin
            Result := False;
            exit;
          end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.DeletePartiallySelected;
var tr,lc,rs,cs,r,c: Integer;
    ItemNo, Data: Integer;
    RowList, ColList: TRVIntegerList;
    CellsList: TList;
    ui: TRVUndoInfo;
begin
  if not GetNormalizedSelectionBounds(False, tr, lc,cs,rs) or not BeforeChange then
    exit;
  ItemNo := GetEditorItemNoForUndo;
  RowList := TRVIntegerList.Create;
  ColList := TRVIntegerList.Create;
  try
    for r := 0 to Rows.Count-1 do
      with Rows[r] do
        for c := 0 to Count-1 do
          if (Items[c]<>nil) and IsCellSelected(r,c) and
             DoOnCellEditing(r,c, True) then begin
             RowList.Add(r);
             ColList.Add(c);
          end;
    if RowList.Count>0 then begin
      Data := BeginModify(ItemNo);
      InitUndo;
      try
        ui := Rows.Do_BeforeClearCells(ItemNo, RowList, ColList, CellsList);
        Rows.Do_ClearCells(CellsList, RowList, ColList, ui);
      finally
        DoneUndo;
        EndModify(ItemNo,Data);
        Change;
      end;
    end;
  finally
    RowList.Free;
    ColList.Free;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetCellTo(Row, Col: Integer;
  Dir: TRVCellDirection; var NewRow, NewCol: Integer;
  Quiet: Boolean): Boolean;
var r,c: Integer;
begin
  case Dir of
    rvcdUp:
      begin
        Result := Row>0;
        if Result then
          Rows.GetMainCell(Row-1,Col,NewRow,NewCol);
      end;
    rvcdLeft:
      begin
        Result := (Col>0) or (Row>0) ;
        if Result then
          if Col>0 then
            Rows.GetMainCell(Row,Col-1,NewRow,NewCol)
          else
            Result := GetCellTo(Row, Col, rvcdPrev, NewRow,NewCol, True);
      end;
    rvcdRight:
      begin
        Result := (Col+Cells[Row,Col].ColSpan<Rows[Row].Count) or
                  (Row+Cells[Row,Col].RowSpan<Rows.Count);
        if Result then
          if (Col+Cells[Row,Col].ColSpan<Rows[Row].Count) then
            Rows.GetMainCell(Row,Col+Cells[Row,Col].ColSpan,NewRow,NewCol)
          else
            Result := GetCellTo(Row, Col, rvcdNext, NewRow,NewCol, True);
      end;
    rvcdDown:
      begin
        Result := Row+Cells[Row,Col].RowSpan<Rows.Count;
        if Result then
          Rows.GetMainCell(Row+Cells[Row,Col].RowSpan,Col,NewRow,NewCol);
      end;
    rvcdNext:
      begin
        NewRow := Row;
        NewCol := Col+1;
        Result := True;
        while NewCol<Rows[NewRow].Count do begin
          if Cells[NewRow,NewCol]<>nil then
            exit;
          inc(NewCol);
        end;
        for r := Row+1 to Rows.Count-1 do
          for c := 0 to Rows[r].Count-1 do
            if (Cells[r,c]<>nil) then begin
              NewRow := r;
              NewCol := c;
              exit;
            end;
        Result := False;
        if not Quiet then Beep;
      end;
    rvcdPrev:
      begin
        NewRow := Row;
        NewCol := Col-1;
        Result := True;
        while NewCol>=0 do begin
          if Cells[NewRow,NewCol]<>nil then
            exit;
          dec(NewCol);
        end;
        for r := Row-1 downto 0 do
          for c := Rows[r].Count-1 downto 0 do
            if (Cells[r,c]<>nil) then begin
              NewRow := r;
              NewCol := c;
              exit;
            end;
        Result := False;
        if not Quiet then Beep;
      end;
    else
      Result := False;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.CanSeeBackgroundThroughCell(Cell: TRVTableCellData): Boolean;
begin
  Result := (Cell.Color=clNone) and (Color=clNone);
  if Result and (Rows.FMainRVData is TCustomRVFormattedData) then
    Result := Rows.FMainRVData.GetRVStyle.ParaStyles[ParaNo].Background.Color=clNone;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetTableColor(UseParentBackground: Boolean): TColor;
begin
  Result := Color;
  if Result=clNone then
    if Rows.FMainRVData is TCustomRVFormattedData then begin
      Result := Rows.FMainRVData.GetRVStyle.ParaStyles[ParaNo].Background.Color;
      if UseParentBackground and (Result=clNone) then
        Result := TCustomRVFormattedData(Rows.FMainRVData).GetColor;
    end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetCellColor(Cell: TRVTableCellData): TColor;
begin
  Result := Cell.Color;
  if Result=clNone then begin
    Result := Color;
    if Result=clNone then
      if Rows.FMainRVData is TCustomRVFormattedData then begin
        Result := Rows.FMainRVData.GetRVStyle.ParaStyles[ParaNo].Background.Color;
        if Result=clNone then
          Result := TCustomRVFormattedData(Rows.FMainRVData).GetColor;
      end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetSplitRowBelow(Row: Integer): Integer;
var r,c,mr,mc: Integer;
    Cell:TRVTableCellData;
    CanSplit:Boolean;
begin
  r := Row;
  while r<Rows.Count-1 do begin
    c := Rows[r].Count-1;
    CanSplit := True;
    while c>=0 do begin
      Cell := Rows.GetMainCell(r,c,mr,mc);
      mr := mr+Cell.RowSpan-1;
      if mr>r then begin
        r := mr;
        CanSplit := False;
        break;
      end;
      c := mc-1;
    end;
    if CanSplit then
      break;
  end;
  Result := r;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetSplitRowAbove(Row: Integer): Integer;
var r,c,mr,mc: Integer;
    CanSplit:Boolean;
begin
  r := Row;
  while r>0 do begin
    c := Rows[r].Count-1;
    CanSplit := True;
    while c>=0 do begin
      Rows.GetMainCell(r,c,mr,mc);
      if mr<r then begin
        r := mr;
        CanSplit := False;
        break;
      end;
      c := mc-1;
    end;
    if CanSplit then
      break;
  end;
  Result := r;
end;
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSERTF}
procedure TRVTableItemInfo.FillRTFTables(ColorList: TRVColorList;
  ListOverrideCountList: TRVIntegerList; RVData: TPersistent); 
var r,c: Integer;
begin
  ColorList.AddUnique(Color);
  case BorderStyle of
    rvtbRaised, rvtbLowered:
      begin
        ColorList.AddUnique(clBtnHighlight);
        ColorList.AddUnique(clBtnShadow);
        ColorList.AddUnique(clBtnFace);
      end;
    rvtbColor:
      ColorList.AddUnique(BorderColor);
    rvtbRaisedColor, rvtbLoweredColor:
      begin
        ColorList.AddUnique(BorderColor);
        ColorList.AddUnique(BorderLightColor);
      end;
  end;
  case CellBorderStyle of
    rvtbRaised, rvtbLowered:
      begin
        ColorList.AddUnique(clBtnHighlight);
        ColorList.AddUnique(clBtnShadow);
        ColorList.AddUnique(clBtnFace);
      end;
    rvtbColor:
      ColorList.AddUnique(CellBorderColor);
    rvtbRaisedColor, rvtbLoweredColor:
      begin
        ColorList.AddUnique(CellBorderColor);
        ColorList.AddUnique(CellBorderLightColor);
      end;
  end;
  if VRuleWidth>0 then
    ColorList.AddUnique(VRuleColor);
  if HRuleWidth>0 then
    ColorList.AddUnique(HRuleColor);
  for r := 0 to Rows.Count-1 do
    with Rows[r] do
      for c := 0 to Count-1 do
        if (Items[c]<>nil) then begin
          ColorList.AddUnique(Items[c].Color);
          ColorList.AddUnique(Items[c].BorderColor);
          ColorList.AddUnique(Items[c].BorderLightColor);
          Items[c].MakeRTFTables(ColorList, ListOverrideCountList, False);
        end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SaveRTF(Stream: TStream; RVData: TPersistent;
  ItemNo: Integer; const Name: String; TwipsPerPixel: Double;
  Level: Integer; ColorList: TRVColorList;
  StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2: TRVIntegerList;
  FontTable: TRVList);
  {......................................................}
  function GetBorderString(BorderStyle:TRVTableBorderStyle; Width: Integer;  Color: Integer): String;
  begin
    case BorderStyle of
      rvtbColor:
        Result := Format('\brdrs\brdrw%d\brdrcf%d',[Width,ColorList.IndexOf(Pointer(Color))]);
      rvtbLowered:
        Result := Format('\brdrinset\brdrw%d',[Width]);
      rvtbRaised:
        Result := Format('\brdroutset\brdrw%d',[Width]);
      rvtbLoweredColor:
        Result := Format('\brdrs\brdrw%d\brdrcf%d',[Width,ColorList.IndexOf(Pointer(Color))]);
      rvtbRaisedColor:
        Result := Format('\brdrs\brdrw%d\brdrcf%d',[Width,ColorList.IndexOf(Pointer(Color))]);
    end;
  end;
  {......................................................}
  procedure SaveBorder(BorderStyle:TRVTableBorderStyle; Width: Integer; Color, LightColor: TColor;
                       VisibleBorders: TRVBooleanRect;
                       const prefix: String);
  var LTColor,RBColor: TColor;
  begin
    if Width<=0 then
      exit;
    Width := Round(Width*TwipsPerPixel);
    case BorderStyle of
      rvtbRaisedColor:
        begin
          LTColor := LightColor;
          RBColor := Color;
        end;
      rvtbLoweredColor:
        begin
          LTColor := Color;
          RBColor := LightColor;
        end;
      else
        begin
          LTColor := Color;
          RBColor := Color;
        end;
    end;
    if (VisibleBorders=nil) or VisibleBorders.Left then
      RVFWrite(Stream, Format('\%sbrdrl%s', [prefix, GetBorderString(BorderStyle, Width, LTColor)]));
    if (VisibleBorders=nil) or VisibleBorders.Top then
      RVFWrite(Stream, Format('\%sbrdrt%s', [prefix, GetBorderString(BorderStyle, Width, LTColor)]));
    if (VisibleBorders=nil) or VisibleBorders.Right then
      RVFWrite(Stream, Format('\%sbrdrr%s', [prefix, GetBorderString(BorderStyle, Width, RBColor)]));
    if (VisibleBorders=nil) or VisibleBorders.Bottom then
      RVFWrite(Stream, Format('\%sbrdrb%s', [prefix, GetBorderString(BorderStyle, Width, RBColor)]));
  end;
  {......................................................}
var r,c,mr,mc,h: Integer;
  Nested: Boolean;
  NestS,s: String;
  Right: Integer;
  Cell: TRVTableCellData;
  val1,val2, gaph2: Integer;
  BColor, BLColor: TColor;
begin
  if (Fmt.FWidth=0) then
    OnDocWidthChange(600, nil, False,nil,nil,nil,r,r,False);
  Nested := Level>0;
  NestS := '';
  for r := 0 to Rows.Count-1 do begin
    if Nested then
      NestS := '\*\nesttableprops';
    RVFWrite(Stream, Format('{%s',[NestS]));
    case TCustomRVData(RVData).GetRVStyle.ParaStyles[TCustomRVData(RVData).GetItemPara(ItemNo)].Alignment of
      rvaRight:
        s := '\trqr';
      rvaCenter:
        s := '\trqc';
    end;
    gaph2 := (CellHSpacing+(CellPadding+CellBorderWidth)*2);
    RVFWrite(Stream, Format('\trowd%s\trgaph%d\trleft%d\itap%d',
      [s, Round(gaph2*TwipsPerPixel/2), 0, Level+1]));
    if r<HeadingRowCount then
      RVFWrite(Stream, '\trhdr');
    h := Rows[r].GetBestHeight;
    if h>0 then
      RVFWrite(Stream, Format('\trrh%d',[Round(h*TwipsPerPixel)]));
    val1 := Round(CellPadding*TwipsPerPixel);
    RVFWrite(Stream, Format('\trpaddl%d\trpaddt%d\trpaddr%d\trpaddb%d\trpaddfl3\trpaddft3\trpaddfr3\trpaddfb3',
                     [val1,val1,val1,val1]));
    val1 := Round(CellHSpacing*TwipsPerPixel/2);
    RVFWrite(Stream, Format('\trspdl%d\trspdr%d\trspdfl3\trspdfr3',[val1,val1]));
    // not sure about dividing by 2 below, but result looks much better:
    if r=0 then
      val1 := Round(BorderVSpacing*TwipsPerPixel/2)
    else
      val1 := Round(CellVSpacing*TwipsPerPixel/2);
    if r=Rows.Count-1 then
      val2 := Round(BorderVSpacing*TwipsPerPixel/2)
    else
      val2 := Round(CellVSpacing*TwipsPerPixel/2);
    RVFWrite(Stream, Format('\trspdt%d\trspdb%d\trspdft3\trspdfb3',[val1,val2]));
    { does not work...
    val1 := Round(BorderHSpacing*TwipsPerPixel);
    RVFWrite(Stream, Format('\trftsWidthB3\trftsWidthB3\trwWidthB%d\trwWidthA%d',[val1,val1]));
    }
    if BestWidth>0 then
      RVFWrite(Stream, Format('\trwWidth%d\trftsWidth3',[Round((BestWidth+BorderWidth*2)*TwipsPerPixel)]))
    else if BestWidth<0 then
      RVFWrite(Stream, Format('\trwWidth%d\trftsWidth2',[-BestWidth*50]))
    else
      RVFWrite(Stream, '\trftsWidth1');
    if RichViewTableDefaultRTFAutofit or (rvtoRTFAllowAutofit in Options) then
      RVFWrite(Stream, '\trautofit1');
    RVFWrite(Stream, Format('\richviewtbw%d',[BestWidth])); // for RichView only
    SaveBorder(BorderStyle, BorderWidth, BorderColor, BorderLightColor, nil, 'tr');
    RVFWrite(Stream, Rows.FMainRVData.GetExtraRTFCode(rv_rtfs_RowProps, Self, r, -1, False));
    with Rows[r] do begin
      for c := 0 to Count-1 do begin
        Cell := Rows.GetMainCell(r,c,mr,mc);
        if Items[c]=nil then begin
          if r>mr then begin
            RVFWrite(Stream, '\clvmrg');
            if (c=mc) and (Cell.ColSpan>1) then
              RVFWrite(Stream, '\clmgf');
          end;
          if c>mc then begin
            RVFWrite(Stream, '\clmrg');
            if (r=mr) and (Cell.RowSpan>1) then
              RVFWrite(Stream, '\clvmgf');
          end;
          end
        else begin
          if (Cell.RowSpan>1) then
            RVFWrite(Stream, '\clvmgf');
          if (Cell.ColSpan>1) then
            RVFWrite(Stream, '\clmgf');
        end;
        if Items[c]<>nil then begin
          if Cell.BestWidth>0 then begin
            if rvtoRTFSaveCellPixelBestWidth in Options then
              RVFWrite(Stream,Format('\clwWidth%d\clftsWidth3',[Round(Cell.BestWidth*TwipsPerPixel)]))
            else
              RVFWrite(Stream, '\clwWidth0')
            end
          else if Cell.BestWidth<0 then
            RVFWrite(Stream,Format('\clwWidth%d\clftsWidth2',[-Cell.BestWidth*50]))
          else
            RVFWrite(Stream,'\clftsWidth1');
         RVFWrite(Stream, Format('\richviewcbw%d\richviewcbh%d',[Cell.BestWidth,Cell.BestHeight])); // for RichView only
        end;
        if Cell.BorderColor<>clNone then
          BColor := Cell.BorderColor
        else
          BColor := CellBorderColor;
        if Cell.BorderLightColor<>clNone then
          BLColor := Cell.BorderLightColor
        else
          BLColor := CellBorderLightColor;
        SaveBorder(CellBorderStyle, CellBorderWidth, BColor, BLColor, Cell.VisibleBorders, 'cl');
        if Cell.Color<>clNone then
          RVFWrite(Stream, Format('\clcbpat%d',[ColorList.IndexOf(Pointer(Cell.Color))]));
        case Cell.GetRealVAlign of
          //rvcTop: {default}
          //  RVFWrite(Stream, '\clvertalt');
          rvcMiddle:
            RVFWrite(Stream, '\clvertalc');
          rvcBottom:
            RVFWrite(Stream, '\clvertalb');
        end;
        RVFWrite(Stream, Rows.FMainRVData.GetExtraRTFCode(rv_rtfs_CellProps, Self, r, c, False));
        Right := Fmt.ColStarts[c]+Fmt.ColWidths[c];
        RVFWrite(Stream,Format('\cellx%d',[Round(Right*TwipsPerPixel+CellHSpacing*TwipsPerPixel/2)]));
      end;
      RVFWrite(Stream, Format('\pard\intbl\itap%d{',[Level+1]));
      for c := 0 to Count-1 do begin
         RVFWrite(Stream, '{');
        if (Items[c]<>nil) then begin
          Items[c].GetRVData.SaveRTFToStream(Stream, False, Level+1,  clNone, nil,
          ColorList, StyleToFont, ListOverrideOffsetsList1, ListOverrideOffsetsList2,
          TRVRTFFontTable(FontTable), TwipsPerPixel);
        end;
        Rows.GetMainCell(r,c,mr,mc);
        if Nested then
          NestS := 'nest';
        RVFWrite(Stream,Format('\%scell}',[NestS]));
      end;
    end;
    RVFWrite(Stream, Format('}\pard\intbl\itap%d',[Level+1]));
    if Nested then
      NestS := 'nest';
    RVFWrite(Stream,Format('\%srow}',[NestS]));
  end;
  Include(TCustomRVData(RVData).State, rvstRTFSkipPar);
end;
{$ENDIF}
{------------------------------------------------------------------------------}
{$IFNDEF RVDONOTUSEHTML}
procedure TRVTableItemInfo.SaveToHTML(Stream: TStream; RVData: TPersistent;
  ItemNo: Integer;
  const Text, Path, imgSavePrefix: String; var imgSaveNo: Integer;
  CurrentFileColor: TColor; SaveOptions: TRVSaveOptions; UseCSS: Boolean;
  Bullets: TRVList);
var ThisUseCSS: Boolean;
const VAlignStr: array[TRVCellVAlign] of String =
    ('top', 'middle', 'bottom', '');
  {.....................................................}
  function GetHTMLLength(v: TRVHTMLLength): String;
  begin
    if v=0 then
      Result := ''
    else if v>0 then
      Result := IntToStr(v)
    else
      Result := IntToStr(-v)+'%';
  end;
  {.....................................................}
  function GetCellCSS(Cell: TRVTableCellData): String;
  var BColor, BLColor: TColor;
  begin
    Result := '';
    if not ThisUseCSS then
      exit;
    if (Cell<>nil) and (Cell.BorderColor<>clNone) then
      BColor := Cell.BorderColor
    else
      BColor := CellBorderColor;
    if (Cell<>nil) and (Cell.BorderLightColor<>clNone) then
      BLColor := Cell.BorderLightColor
    else
      BLColor := CellBorderLightColor;
    if ((CellBorderWidth<>BorderWidth) or (BorderWidth>1)) and
       not ((CellBorderWidth=1) and (BorderWidth>1)) then
      Result := Format('border-width : %dpx; ', [CellBorderWidth]);
    if CellBorderWidth>0 then
      case CellBorderStyle of
        rvtbColor:
          if (BorderStyle<>rvtbColor) or (BColor<>BorderColor) then
            Result := Format('%sborder-color : %s; border-style: solid',
              [Result, RV_GetHTMLRGBStr(BColor, False)]);
        rvtbRaisedColor:
          Result := Format('%sborder-color : %s %s %s %s; border-style: solid',
            [Result, RV_GetHTMLRGBStr(BLColor, False),RV_GetHTMLRGBStr(BColor, False),
                     RV_GetHTMLRGBStr(BColor, False),RV_GetHTMLRGBStr(BLColor, False)]);
        rvtbLoweredColor:
          if (BColor<>BorderColor) or (BLColor<>BorderLightColor) or
             not (BorderStyle in [rvtbLoweredColor,rvtbRaisedColor]) then
            Result := Format('%sborder-color : %s %s %s %s; border-style: solid',
              [Result, RV_GetHTMLRGBStr(BColor, False),RV_GetHTMLRGBStr(BLColor, False),
                       RV_GetHTMLRGBStr(BLColor, False), RV_GetHTMLRGBStr(BColor, False)]);
        rvtbRaised:
          Result := Format('%sborder-style: outset',[Result]);
        rvtbLowered:
          ;//Result := Format('%sborder-style: inset',[Result]);
      end;
  end;
  {.....................................................}
  function GetThisCellCSS(Cell : TRVTableCellData; const DefCss: String): String;
  var bss: String;
  begin
    if (Cell.BorderColor<>clNone) or (Cell.BorderLightColor<>clNone) then
      Result := GetCellCSS(Cell)
    else
      Result := DefCss;
    if not ThisUseCSS then
      exit;
    if ((Length(Result)>0) and (Result[Length(Result)]<>';')) and
       ((Length(Result)>1) and (Result[Length(Result)-1]<>';')) then
      Result := Result+'; ';
    if Cell.BackgroundStyle in [rvbsCentered, rvbsStretched] then
      bss := 'background-position: center center; background-repeat: no-repeat;'
    else if BackgroundStyle in [rvbsCentered, rvbsStretched] then
      bss := 'background-position: left top; background-repeat: repeat;'
    else
      bss := '';
    if (Result<>'') and (bss<>'') then
      Result := Result+' '+bss
    else
      Result := Result+bss;
    if CellBorderWidth>0 then begin
      if not Cell.VisibleBorders.Top then
        Result := Result+'border-top: none; ';
      if not Cell.VisibleBorders.Right then
        Result := Result+'border-right: none; ';
      if not Cell.VisibleBorders.Bottom then
        Result := Result+'border-bottom: none; ';
      if not Cell.VisibleBorders.Left then
        Result := Result+'border-left: none; ';
    end;

    if Result<>'' then
      Result := Format(' style=''{%s}''',[Result]);
  end;
  {.....................................................}
  function GetRules: String;
  begin
    {
    if HRuleWidth>0 then
     if VRuleWidth>0 then
       Result := 'all'
     else
       Result := 'cols'
    else
     if VRuleWidth>0 then
       Result := 'rows'
     else
    }
       Result := ''
   end;
  {.....................................................}
  function AddAttribute(const Name, Value, DefValue: String): String;
  begin
    if (Value<>DefValue) and (Value<>'') then
      Result := Format(' %s=%s',[Name,Value])
    else
      Result := '';
  end;
  {.....................................................}
  function AddBorderColorAttr(BorderStyle: TRVTableBorderStyle;
                              BorderColor, BorderLightColor: TColor): String;
  begin
    case BorderStyle of
      rvtbColor:
        Result := AddAttribute('bordercolor', RV_GetHTMLRGBStr(BorderColor, True), '');
      rvtbRaisedColor, rvtbLoweredColor:
        begin
          Result := AddAttribute('bordercolorlight', RV_GetHTMLRGBStr(BorderLightColor, True), '')+
                    AddAttribute('bordercolordark', RV_GetHTMLRGBStr(BorderColor, True), '');
        end;
      else
        Result := '';
    end;
  end;
  {.....................................................}
  function GetTableCSS: String;
  var bss: String;
  begin
    Result := '';
    if not ThisUseCSS then
      exit;
    //Result := Format('padding : %dpx %dpx; ',[BorderHSpacing,BorderVSpacing]);
    case BorderStyle of
      rvtbLoweredColor:
        Result := Format('%sborder-color : %s %s %s %s; border-style: solid;',
            [Result, RV_GetHTMLRGBStr(BorderColor, False),RV_GetHTMLRGBStr(BorderLightColor, False),
                     RV_GetHTMLRGBStr(BorderLightColor, False), RV_GetHTMLRGBStr(BorderColor, False)]);
      rvtbLowered:
        Result := Format('%sborder-style: inset;',[Result]);
    end;
    if BackgroundStyle in [rvbsCentered, rvbsStretched] then
      bss := 'background-position: center center; background-repeat: no-repeat;'
    else
      bss := '';
    if (Result<>'') and (bss<>'') then
      Result := Result+' '+bss
    else
      Result := Result+bss;
    if Result<>'' then
      Result := Format(' style=''{%s}''',[Result]);
  end;
  {.....................................................}
  function AddTableBorderAttribute: String;
  begin
    if not ThisUseCSS or (BorderWidth>0) or (CellBorderWidth=0) then
      Result := Format(' border=%d', [BorderWidth])
    else
      Result := Format(' border=1 style=''{border-width : 0px}''', [BorderWidth])
  end;
  {.....................................................}
  function AddBackgroundAttribute(Background: TRVBackground; Color: TColor;
    ItemNo: Integer; ARVData: TCustomRVData;
    const BackgroundImageFileName: String) : String;
  var Location: String;
      DoDefault: Boolean;
  begin
    Result := '';
    if (Background=nil) or not Background.Visible then
      exit;
    if Color=clNone then
      Color := CurrentFileColor;
    if (BackgroundImageFileName<>'') and (rvsoUseItemImageFileNames in SaveOptions) then
      Location := ExtractRelativePath(Path, BackgroundImageFileName)
    else
      Location := '';
    ARVData.HTMLSaveImage(ARVData, ItemNo, Path, Color, Location, DoDefault);
    if DoDefault then begin
      if (BackgroundImageFileName<>'') and (rvsoUseItemImageFileNames in SaveOptions) then
        Location := ExtractRelativePath(Path, BackgroundImageFileName)
      else
        Location := TCustomRVData(RVData).DoSavePicture(rvsfHTML, imgSavePrefix, Path,
          imgSaveNo, rvsoOverrideImages in SaveOptions, Color, Background.Image);
    end;
    if Location<>'' then
      Result := Format(' background="%s"', [RV_GetHTMLPath(Location)]);
  end;
  {.....................................................}
  {
  function GetTableAlign: String;
  begin
    case TCustomRVData(Rows.FMainRVData).GetRVStyle.ParaStyles[ParaNo].Alignment of
      rvaCenter:
        Result := 'center';
      rvaRight:
        Result := 'right';
      else
        Result := 'left';
    end;
  end;
  }
var
  Cell: TRVTableCellData;
  r,c: Integer;
  Options:TRVSaveOptions;
  CellCSS: String;
begin
  ThisUseCSS := UseCSS or (rvsoForceNonTextCSS in SaveOptions);
  CellCSS := GetCellCSS(nil);
  Options  := SaveOptions;
  Include(Options, rvsoMiddleOnly);
  Exclude(Options, rvsoFirstOnly);
  Exclude(Options, rvsoLastOnly);  
  RVFWriteLine(Stream, Format('<TABLE%s%s%s%s%s%s%s%s%s>',
    [
     //AddAttribute('align', GetTableAlign, ''),
     AddAttribute('width', GetHTMLLength(BestWidth), ''),
     AddAttribute('bgcolor', RV_GetHTMLRGBStr(Color, True), ''),
     AddTableBorderAttribute,
     AddAttribute('cellpadding', IntToStr(CellPadding), ''),
     AddBorderColorAttr(BorderStyle, BorderColor, BorderLightColor),
     AddAttribute('cellspacing', IntToStr((CellVSpacing+CellHSpacing) div 2), ''),
     AddAttribute('rules',GetRules,''),
     AddBackgroundAttribute(FBackground, Color, ItemNo, TCustomRVData(RVData),
       BackgroundImageFileName),
     GetTableCSS]));
  for r := 0 to Rows.Count-1 do begin
    RVFWriteLine(Stream, Format('<TR%s>',[AddAttribute('valign', VAlignStr[Rows[r].VAlign], '')]));
    for c := 0 to Rows[r].Count-1 do
      if Cells[r,c]<>nil then begin
        Cell := Cells[r,c];
        RVFWrite(Stream, Format('<TD%s%s%s%s%s%s%s%s>',
          [
          AddAttribute('colspan', IntToStr(Cell.ColSpan), '1'),
          AddAttribute('rowspan', IntToStr(Cell.RowSpan), '1'),
          AddAttribute('width', GetHTMLLength(Cell.BestWidth), ''),
          AddAttribute('height', GetHTMLLength(Cell.BestHeight), ''),
          AddAttribute('bgcolor', RV_GetHTMLRGBStr(Cell.Color, True), RV_GetHTMLRGBStr(Color, True)),
          AddAttribute('valign', VAlignStr[Cell.VAlign], ''),
          AddBackgroundAttribute(Cell.FBackground, GetCellColor(Cell), -1, Cell,
            Cell.BackgroundImageFileName),
          GetThisCellCSS(Cell, CellCSS)
          ]));
        if Cell.HasData(True) then
          if UseCSS then
            Cell.GetRVData.SaveHTMLToStreamEx(Stream, Path, '', imgSavePrefix, '','', '',Options,
                                    GetCellColor(Cell), CurrentFileColor, imgSaveNo,
                                    0,0,0,0,nil,Bullets)
          else
            Cell.GetRVData.SaveHTMLToStream(Stream,Path, '', imgSavePrefix,  Options,
                                  GetCellColor(Cell), imgSaveNo,
                                  0,0,0,0,nil,Bullets)
        else
          RVFWriteLine(Stream, '&nbsp;');
        RVFWriteLine(Stream, '</TD>');
      end;
    RVFWriteLine(Stream, '</TR>');
  end;
  RVFWriteLine(Stream, '</TABLE>');
end;
{$ENDIF}
{------------------------------------------------------------------------------}
function TRVTableItemInfo.AsText(LineWidth: Integer; RVData: TPersistent;
  const Text, Path: String; TextOnly,Unicode: Boolean): String;
var r,c: Integer;
    Stream: TMemoryStream;
    RSep,CSep:String;
    {$IFNDEF RVDONOTUSEUNICODE}
    CP: TRVCodePage;
    {$ENDIF}
begin
  Result := '';
  {$IFNDEF RVDONOTUSEUNICODE}
  if Unicode then begin
    CP := TCustomRVData(RVData).GetRVStyle.DefCodePage;
    RSep := RVU_AnsiToUnicode(cp, TextRowSeparator);
    CSep := RVU_AnsiToUnicode(cp, TextColSeparator);
    end
  else
  {$ENDIF}
  begin
    RSep := TextRowSeparator;
    CSep := TextColSeparator;
  end;
  Stream := TMemoryStream.Create;
  try
    for r := 0 to Rows.Count-1 do begin
      for c := 0 to Rows[r].Count-1 do begin
        if Cells[r,c]<>nil then
          Cells[r,c].GetRVData.SaveTextToStream(Path, Stream, LineWidth,  False,
            TextOnly, Unicode, False);
        if c<Rows[r].Count-1 then
          RVFWrite(Stream, CSep);
      end;
      RVFWrite(Stream, RSep);
    end;
    Stream.Position := 0;
    SetLength(Result,Stream.Size);
    Stream.ReadBuffer(PChar(Result)^,Stream.Size);
  finally
    Stream.Free;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.BeforeChange: Boolean;
begin
  Result := IsInEditor and
            TCustomRichViewEdit(TRVEditRVData(Rows.FMainRVData).RichView).BeforeChange(True);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.CanChange: Boolean;
begin
  Result := IsInEditor;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellBestHeight_(ItemNo, Value, Row,
  Col: Integer);
begin
  SetCellProperty(ItemNo, 'BestHeight', LongInt(Value), Row, Col, True, False);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellBestWidth_(ItemNo: Integer;
  Value: TRVHTMLLength; Row, Col: Integer);
begin
  SetCellProperty(ItemNo, 'BestWidth', LongInt(Value), Row, Col, True, True);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellColor_(ItemNo: Integer; Value: TColor;
  Row, Col: Integer);
begin
  SetCellProperty(ItemNo, 'Color', LongInt(Value), Row, Col, False, False);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellBackgroundStyle_(ItemNo: Integer;
  Value: TRVItemBackgroundStyle; Row,Col: Integer);
begin
  SetCellProperty(ItemNo, 'BackgroundStyle', LongInt(Value), Row, Col, False, False);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellVisibleBorders_(ItemNo: Integer; Left, Top, Right, Bottom: Boolean; Row,Col: Integer);
var ui: TRVUndoModifyCellVisibleBorders;
    Cell: TRVTableCellData;
begin
  Cell := Cells[Row, Col];
  if Cell.VisibleBorders.IsEqual2(Left,Top,Right,Bottom) then
    exit;
  if (rvtsInserted in FState) and IsInEditor then begin
    if ItemNo=-1 then
      ItemNo := GetMyItemNo;
    TRVEditRVData(FRows.FMainRVData).BeginUndoSequence(rvutModifyItem, True);
    ui := TRVUndoModifyCellVisibleBorders(AddTableUndoInfo(TRichViewRVData(FRows.FMainRVData), TRVUndoModifyCellVisibleBorders, ItemNo, False, False));
    if ui<>nil then begin
      ui.Row    := Row;
      ui.Col    := Col;
      ui.Left   := Cell.VisibleBorders.Left;
      ui.Top    := Cell.VisibleBorders.Top;
      ui.Right  := Cell.VisibleBorders.Right;
      ui.Bottom := Cell.VisibleBorders.Bottom;
    end;
  end;
  Cell.VisibleBorders.SetValues(Left,Top,Right,Bottom);
  Changed;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellBorderColor_(ItemNo: Integer; Value: TColor;
  Row, Col: Integer);
begin
  SetCellProperty(ItemNo, 'BorderColor', LongInt(Value), Row, Col, False, False);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellBorderLightColor_(ItemNo: Integer; Value: TColor;
  Row, Col: Integer);
begin
  SetCellProperty(ItemNo, 'BorderLightColor', LongInt(Value), Row, Col, False, False);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellVAlign_(ItemNo: Integer; Value: TRVCellVAlign; Row,Col: Integer);
begin
  SetCellProperty(ItemNo, 'VAlign', LongInt(Value), Row, Col, True, False);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetRowVAlign_(ItemNo: Integer;
  Value: TRVCellVAlign; Row: Integer);
var
    ui: TRVUndoRowVAlign;
begin
  if Rows[Row].VAlign<>Value then begin
    if (rvtsInserted in FState) and IsInEditor then begin
      MergeInplaceUndo(False);
      TRVEditRVData(FRows.FMainRVData).BeginUndoSequence(rvutModifyItem, True);
      if ItemNo=-1 then
        ItemNo := GetMyItemNo;
      ui := TRVUndoRowVAlign(AddTableUndoInfo(TRVEditRVData(FRows.FMainRVData), TRVUndoRowVAlign, ItemNo, True, False));
      if ui<>nil then begin
        ui.OldVAlign := Rows[Row].VAlign;
        ui.Row       := Row;
      end;
    end;
    Rows[Row].VAlign := Value;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellBestHeight(Value, Row, Col: Integer);
begin
  SetCellBestHeight_(-1, Value, Row, Col);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellBestWidth(Value: TRVHTMLLength; Row,
  Col: Integer);
begin
  SetCellBestWidth_(-1, Value, Row, Col);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellColor(Value: TColor; Row, Col: Integer);
begin
  SetCellColor_(-1, Value, Row, Col);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellBackgroundStyle(Value: TRVItemBackgroundStyle; Row,Col: Integer);
begin
  SetCellBackgroundStyle_(-1, Value, Row, Col);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellVisibleBorders(Left, Top, Right, Bottom: Boolean; Row,Col: Integer);
begin
  SetCellVisibleBorders_(-1, Left, Top, Right, Bottom, Row,Col);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellBorderColor(Value: TColor; Row, Col: Integer);
begin
  SetCellBorderColor_(-1, Value, Row, Col);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellBorderLightColor(Value: TColor; Row, Col: Integer);
begin
  SetCellBorderLightColor_(-1, Value, Row, Col);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellVAlign(Value: TRVCellVAlign; Row,
  Col: Integer);
begin
  SetCellVAlign_(-1, Value, Row, Col);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetRowVAlign(Value: TRVCellVAlign; Row: Integer);
begin
  SetRowVAlign_(-1, Value, Row);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.DoneUndo;
begin
  if (rvtsInserted in FState) and (FRows.FMainRVData is TRVEditRVData) then
    TRVEditRVData(FRows.FMainRVData).SetUndoGroupMode(False);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.InitUndo;
begin
  if (rvtsInserted in FState) and (FRows.FMainRVData is TRVEditRVData) then begin
    TRVEditRVData(FRows.FMainRVData).BeginUndoSequence(rvutModifyItem, True);
    TRVEditRVData(FRows.FMainRVData).SetUndoGroupMode(True);
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetEditorItemNoForUndo: Integer;
begin
  if (rvtsInserted in FState) and IsInEditor and UndoEnabled then
    Result := GetMyItemNo
  else
    Result := -1;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.IsInEditor: Boolean;
begin
  Result := FRows.FMainRVData is TRVEditRVData;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetMyItemNo: Integer;
begin
  if (CachedItemNo<0) or (CachedItemNo>=FRows.FMainRVData.Items.Count) or
     (FRows.FMainRVData.GetItem(CachedItemNo)<>Self) then
    CachedItemNo := FRows.FMainRVData.GetItemNo(Self);
  Result := CachedItemNo;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.EditCell_(Row,Col: Integer; Unquestioning: Boolean);
var PRow,PCol: Integer;
    Ptable : TRVTableItemInfo;
begin
  if not Unquestioning and not (rvtoEditing in Options) then
    exit;
  if FRows.FMainRVData is TRVTableCellData then begin
    Ptable := TRVTableCellData(FRows.FMainRVData).GetTable;
    Ptable.GetCellPosition(TRVTableCellData(FRows.FMainRVData), PRow, PCol);
    if PRow=-1 then
      ERichViewError.Create(errInternalError);
    Ptable.EditCell_(PRow, PCol, Unquestioning);
  end;
  if FRows.FMainRVData is TRVTableCellData then
    exit;
  CreateInplace(-1, Row, Col, False, True, False, False, Unquestioning);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.EditCell(Row, Col: Integer);
begin
  EditCell_(Row,Col,False);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.EnterItem(From: TRVEnterDirection; Coord: Integer): Boolean;
var r,c: Integer;
begin
  Result := False;
  if Rows.Empty or not (rvtoEditing in Options) then
    exit;
  case From of
    rvedLeft:
      begin
        CreateInplace(-1, 0, 0, False, True, False, False, False);
        Result := True;
      end;
    rvedRight:
      begin
        Rows.GetMainCell(Rows.Count-1, Rows[0].Count-1, r,c);
        CreateInplace(-1, r,c, False, False, True, False, False);
        Result := True;
      end;
    rvedTop:
      begin
        c := GetColNo(Coord);
        if c<0 then exit;
        Rows.GetMainCell(0,c, r,c);
        CreateInplace(-1, r,c, False, True, True, False, False);
        Result := True;
      end;
    rvedBottom:
      begin
        c := GetColNo(Coord);
        if c<0 then exit;
        Rows.GetMainCell(Rows.Count-1,c, r,c);
        CreateInplace(-1, r,c, False, False, True, False, False);
        Result := True;
      end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.BuildJumps(Left,Top: Integer; var StartJumpNo: Integer;
  jumps: TList);
var r,c, i, cnt: Integer;
begin
  for r := 0 to Rows.Count-1 do
    for c := 0 to Rows[r].Count-1 do
      if Cells[r,c]<>nil then begin
        cnt := jumps.Count;
        Cells[r,c].FirstJumpNo := FRows.FMainRVData.FirstJumpNo ;//StartJumpNo;
        if (FInplaceEditor<>nil) and (TRVTableInplaceEdit(FInplaceEditor).FCell=Cells[r,c]) then begin
          FInplaceEditor.FirstJumpNo := StartJumpNo+FRows.FMainRVData.FirstJumpNo;
          StartJumpNo := FInplaceEditor.FirstJumpNo+TRVTableInplaceRVData(TRVTableInplaceEdit(FInplaceEditor).RVData).ReallyBuildJumpsCoords-FRows.FMainRVData.FirstJumpNo;
          end
        else begin
          Cells[r,c].BuildJumpsCoords(StartJumpNo, jumps);
          for i := cnt to jumps.Count-1 do
            with TRVJumpInfo(jumps.Items[i]) do begin
              inc(l, Left+Cells[r,c].Left+CellPadding);
              inc(t, Top+Cells[r,c].Top+CellPadding+Cells[r,c].GetExtraVOffs);
            end;
        end;
      end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.Changed;
begin
  Include(FState, rvtsModified);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.ValidateFocused;
begin
  if (FocusedCellRow<0) or (FocusedCellRow>=Rows.Count) or
     (FocusedCellCol<0) or (FocusedCellCol>=Rows[0].Count) then begin
    FocusedCellRow := -1;
    FocusedCellCol := -1;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.ValidateChosen;
begin
  if (ChosenCellRow<0) or (ChosenCellRow>=Rows.Count) or
     (ChosenCellCol<0) or (ChosenCellCol>=Rows[0].Count) then begin
    ChosenCellRow := -1;
    ChosenCellCol := -1;
  end;
  if (ChosenCellRow<>-1) and (ChosenCellCol<>-1) then
    Rows.GetMainCell(ChosenCellRow, ChosenCellCol, ChosenCellRow, ChosenCellCol);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.MoveFocus(GoForward: Boolean;
                                    var TopLevelRVData: TPersistent;
                                    var TopLevelItemNo: Integer): Boolean;
var Cell: TCustomRVFormattedData;
    Dir: TRVCellDirection;
begin
  ValidateFocused;
  Result := False;
  if (FocusedCellRow=-1) then
  if GoForward then begin
    FocusedCellRow := 0;
    FocusedCellCol := 0;
    end
  else
    Rows.GetMainCell(Rows.Count-1, Rows[0].Count-1,FocusedCellRow,FocusedCellCol);
  if GoForward then
    Dir := rvcdNext
  else
    Dir := rvcdPrev;
  while True do begin
    Cell := Cells[FocusedCellRow,FocusedCellCol];
    Cell.FocusedItemNo := Cell.GetNextFocusedItem(Cell.FocusedItemNo, GoForward, TCustomRVFormattedData(TopLevelRVData), TopLevelItemNo);
    if Cell.FocusedItemNo<>-1 then begin
      Result := True;
      exit;
    end;
    if not GetCellTo(FocusedCellRow, FocusedCellCol, Dir, FocusedCellRow, FocusedCellCol, True) then begin
      FocusedCellRow := -1;
      FocusedCellCol := -1;
      exit;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.ClearFocus;
begin
  ValidateFocused;
  if (FocusedCellRow<>-1) then begin
    Cells[FocusedCellRow,FocusedCellCol].ClearFocus;
    FocusedCellRow := -1;
    FocusedCellCol := -1;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.CellIsChosen: Boolean;
begin
  Result := False;
  ValidateChosen;
  if ChosenCellRow=-1 then
    exit;
  Result := Cells[ChosenCellRow,ChosenCellCol].SelectionExists(False,True);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.GetCellPosition(Cell: TRVTableCellData; var Row,
  Col: Integer);
var r: Integer;
begin
  Row := -1;
  Col := -1;
  for r := 0 to Rows.Count-1 do begin
    Col := Rows[r].IndexOf(Cell);
    if Col<>-1 then begin
      Row := r;
      exit;
    end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.AdjustFocus(Row, Col: Integer;
  TopLevelRVData: TPersistent; TopLevelItemNo: Integer);
begin
  FocusedCellRow := Row;
  FocusedCellCol := Col;
  TCustomRVFormattedData(FRows.FMainRVData).AdjustFocus(GetMyItemNo,TopLevelRVData,TopLevelItemNo);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.AdjustFocusToControl(Control: TControl;
  var TopLevelRVData: TPersistent; var TopLevelItemNo: Integer): Boolean;
var r,c,ItemNo: Integer;
    item: TCustomRVItemInfo;
begin
  Result := GetCellWhichOwnsControl(Control, r, c, ItemNo);
  if Result then begin
    FocusedCellRow := r;
    FocusedCellCol := c;
    Cells[r,c].FocusedItemNo := ItemNo;
    item := TCustomRVItemInfo(Cells[r,c].Items.Objects[ItemNo]);
    item.AdjustFocusToControl(Control,TopLevelRVData,TopLevelItemNo);
    if item.GetBoolValue(rvbpImmediateControlOwner) then begin
      TopLevelItemNo := ItemNo;
      TopLevelRVData := Cells[r,c];
    end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.UndoEnabled: Boolean;
begin

  Result := (rvtsInserted in FState) and
            (Rows.FMainRVData.GetRootData is TRVEditRVData) and
            (TCustomRichViewEdit(TRVEditRVData(Rows.FMainRVData.GetRootData).RichView).UndoLimit<>0);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.MarkStylesInUse(UsedTextStyles, UsedParaStyles, UsedListStyles: TRVIntegerList);
var r,c: Integer;
begin
  inherited MarkStylesInUse(UsedTextStyles, UsedParaStyles, UsedListStyles);
  for r := 0 to Rows.Count-1 do
    for c := 0 to Rows[r].Count-1 do
      if Cells[r,c]<>nil then
        Cells[r,c].GetRVData.MarkStylesInUse(UsedTextStyles, UsedParaStyles, UsedListStyles);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.UpdateStyles(TextStylesShift,
  ParaStylesShift, ListStylesShift: TRVIntegerList);
var r,c: Integer;
begin
  inherited UpdateStyles(TextStylesShift, ParaStylesShift, ListStylesShift);
  for r := 0 to Rows.Count-1 do
    for c := 0 to Rows[r].Count-1 do
      if Cells[r,c]<>nil then
        Cells[r,c].GetRVData.UpdateStyles(TextStylesShift,ParaStylesShift,ListStylesShift);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.CompletelySelected: Boolean;
var SN,EN,SO,EO,No: Integer;
begin
  Result := Rows.FMainRVData is TCustomRVFormattedData;
  if not Result then
    exit;
  TCustomRVFormattedData(Rows.FMainRVData).GetSelectionBounds(SN,SO,EN,EO,True);
  Result := SN<>-1;
  if not Result then
    exit;
  No := GetMyItemNo;
  Result := ((No>SN) or ((No=SN) and (SO=0))) and
            ((No<EN) or ((No=EN) and (EO=1)));
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetSubRVDataAt(X, Y: Integer): TPersistent;
var Row, Col: Integer;
begin
  if GetCellAt(X, Y, Row, Col) then
    Result := Cells[Row,Col].GetRVData
  else
    Result := nil;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.UnAssignActiveCell;
begin
  ValidateChosen;
  if (ChosenCellRow<>-1) and (ChosenCellCol<>-1) and
     (Rows.FMainRVData is TCustomRVFormattedData) then
    TCustomRVFormattedData(Rows.FMainRVData).UnassignChosenRVData(Cells[ChosenCellRow,ChosenCellCol]);
  ChosenCellRow := -1;
  ChosenCellCol := -1;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.CleanUpChosen;
begin
  ValidateChosen;
  if ChosenCellRow<>-1 then
    TCustomRVFormattedData(Cells[ChosenCellRow,ChosenCellCol].GetRVData).Deselect(nil,False);
  ChosenCellRow := -1;
  ChosenCellCol := -1;
  DestroyInplace(True);
end;
{------------------------------------------------------------------------------}

 constructor TRVTableStoreSubRVData.Create(ARow, ACol: Integer);
 begin
   inherited Create;
   Row := ARow;
   Col := ACol;
 end;

 function TRVTableStoreSubRVData.Duplicate: TRVStoreSubRVData;
 begin
   Result := TRVTableStoreSubRVData.Create(Row,Col);
 end;

{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetSubRVData(var StoreState: TRVStoreSubRVData;
  Position: TRVSubRVDataPos): TPersistent;
var r,c,cs,rs: Integer;
    Dir: TRVCellDirection;
begin
  Result := nil;
  case Position of
    rvdFirst:
      begin
        StoreState := TRVTableStoreSubRVData.Create(0,0);
        Result := Cells[0,0];
      end;
    rvdLast:
      begin
        r := Rows.Count-1;
        c := Rows[r].Count-1;
        StoreState := TRVTableStoreSubRVData.Create(r,c);
        Result := FRows.GetMainCell(r,c,r,c);
      end;
    rvdChosenUp,rvdChosenDown:
      begin
        StoreState := nil;
        if GetNormalizedSelectionBounds(True,r,c,cs,rs) then begin
          if Position=rvdChosenUp then begin
            inc(r,rs-1);
            inc(c,cs-1);
          end;
          Result := FRows.GetMainCell(r,c,r,c);
          StoreState := TRVTableStoreSubRVData.Create(r,c);
          end
        else begin
          ValidateChosen;
          if ChosenCellRow<>-1 then begin
            StoreState := TRVTableStoreSubRVData.Create(ChosenCellRow,ChosenCellCol);
            Result := Cells[ChosenCellRow,ChosenCellCol];
          end;
        end;
      end;
    rvdNext, rvdPrev:
      begin
        if Position=rvdNext then
          Dir := rvcdNext
        else
          Dir := rvcdPrev;
        r := TRVTableStoreSubRVData(StoreState).Row;
        c := TRVTableStoreSubRVData(StoreState).Col;
        if GetCellTo(r,c, Dir, r, c, True) then begin
          TRVTableStoreSubRVData(StoreState).Row := r;
          TRVTableStoreSubRVData(StoreState).Col := c;
          Result := Cells[r,c];
        end;
      end;
  end;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.ChooseSubRVData_(r, c: Integer);
var no, no1,no2,off1,off2: Integer;
begin
  if (ChosenCellRow=r) and (ChosenCellCol=c) then
    exit;
  TCustomRVFormattedData(Cells[r,c].GetRVData).GetSelectionBounds(no1,off1,no2,off2,False);
  no := GetMyItemNo;
  TCustomRVFormattedData(FRows.FMainRVData).SetSelectionBounds(no,1,no,1);
  (FRows.FMainRVData as TCustomRVFormattedData).AssignChosenRVData(Cells[r,c], Self);
  ChosenCellRow := r;
  ChosenCellCol := c;
  if (rvtsEditMode in FState) and (rvtsInserted in FState) then
    EditCell(ChosenCellRow,ChosenCellCol);
  TCustomRVFormattedData(Cells[r,c].GetRVData).SetSelectionBounds(no1,off1,no2,off2);
  TCustomRVFormattedData(Cells[r,c].GetRVData).Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.ChooseSubRVData(StoreState: TRVStoreSubRVData);
begin
  ChooseSubRVData_(TRVTableStoreSubRVData(StoreState).Row,
                   TRVTableStoreSubRVData(StoreState).Col);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.ResetSubCoords;
begin
  if FRows.FMainRVData is TCustomRVFormattedData then
    TCustomRVFormattedData(FRows.FMainRVData).GetItemClientCoords(GetMyItemNo,
      MyClientLeft, MyClientTop);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetSoftPageBreakDY(Data: Integer): Integer;
begin
  if Data>=Fmt.RowStarts.Count then
    Result := Fmt.FHeight
  else
    Result := Fmt.RowStarts[Data];
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetInplaceBounds(Left, Top, Width,
  Height: Integer);
begin
  TRVTableInplaceEdit(FInplaceEditor).NormalScrolling := Height>10000;
  TRVTableInplaceEdit(FInplaceEditor).FullRedraw := TRVTableInplaceEdit(FInplaceEditor).NormalScrolling;
  if Height>10000 then
    Height := 10000;
  FInplaceEditor.SetBounds(Left, Top, Width, Height);
  if rvtsFormatInplace in FState then begin
    FInplaceEditor.RVData.TextWidth := -1;
    FInplaceEditor.RVData.DocumentWidth := -1;
    FInplaceEditor.RVData.Format_(True,False,True,0,FInplaceEditor.Canvas,False,False);
  end;
  Exclude(FState, rvtsFormatInplace);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.CanUseHeadingRowCount: Boolean;
var c,hrc, mr, mc: Integer;
    cell: TRVTableCellData;
begin
  Result := False;
  hrc := HeadingRowCount;
  if hrc>Rows.Count then
    hrc := Rows.Count;
  if hrc=0 then
    exit;
  for c := 0 to Rows[hrc-1].Count-1 do begin
    cell := Rows.GetMainCell(hrc-1,c,mr,mc);
    if mr+cell.RowSpan>hrc then
      exit;
  end;
  Result := True;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetBackgroundImage: TGraphic;
begin
  if FBackground<>nil then
    Result := FBackground.Image
  else
    Result := nil;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetBackgroundImage_(const Value: TGraphic; Copy: Boolean);
begin
  if Value=BackgroundImage then
    exit;
  if (FBackground=nil) and (Value<>nil) and not Value.Empty then
    FBackground := TRVBackground.Create(False);
  if FBackground<>nil then begin
    FBackground.AssignImage(Value, Rows.FMainRVData, Copy);
    if FBackground.Empty then begin
      FBackground.Free;
      FBackground := nil;
    end
  end;
  if FInplaceEditor<>nil then
    FInplaceEditor.Invalidate;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetBackgroundImage(const Value: TGraphic);
var ui: TRVUndoModifyBackgroundImage;
begin
  if Value=BackgroundImage then
    exit;
  if (rvtsInserted in FState) and IsInEditor then begin
    MergeInplaceUndo(False);
    TRVEditRVData(FRows.FMainRVData).BeginUndoSequence(rvutModifyItem, True);
    ui := TRVUndoModifyBackgroundImage(
             AddTableUndoInfo(TRVEditRVData(FRows.FMainRVData), TRVUndoModifyBackgroundImage,
             GetMyItemNo, False, False));
    if ui<>nil then begin
      ui.Row := -1;
      ui.Col := -1;
      ui.Image := GetBackgroundImage;
      SetBackgroundImage_(nil, False);
    end;
  end;
  SetBackgroundImage_(Value, True);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetCellBackgroundImage(Value: TGraphic; Row,Col: Integer);
var ui: TRVUndoModifyBackgroundImage;
    Cell: TRVTableCellData;
begin
  Cell := Cells[Row,Col];
  if Value=Cell.BackgroundImage then
    exit;
  if (rvtsInserted in FState) and IsInEditor then begin
    MergeInplaceUndo(False);
    TRVEditRVData(FRows.FMainRVData).BeginUndoSequence(rvutModifyItem, True);
    ui := TRVUndoModifyBackgroundImage(
             AddTableUndoInfo(TRVEditRVData(FRows.FMainRVData), TRVUndoModifyBackgroundImage,
             GetMyItemNo, False, False));
    if ui<>nil then begin
      ui.Row := Row;
      ui.Col := Col;
      ui.Image := Cell.GetBackgroundImage;
      Cell.SetBackgroundImage_(nil, False);
    end;
  end;
  Cell.SetBackgroundImage_(Value, True);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetBackgroundStyle: TRVItemBackgroundStyle;
begin
  if FBackground<>nil then
    Result := FBackground.ItemBackStyle
  else
    Result := rvbsColor;
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SetBackgroundStyle(
  const Value: TRVItemBackgroundStyle);
begin
  if Value=BackgroundStyle then
    exit;
  if rvtsInserted in FState then begin
    SetProperty('BackgroundStyle', ord(Value), False, False);
    exit;
  end;
  if (FBackground=nil) and (Value<>rvbsColor) then
    FBackground := TRVBackground.Create(False);
  if FBackground<>nil then begin
    FBackground.ItemBackStyle := Value;
    if FBackground.Empty then begin
      FBackground.Free;
      FBackground := nil;
    end
  end;
  if FInplaceEditor<>nil then
    FInplaceEditor.Invalidate;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetRVFExtraPropertyCount: Integer;
begin
  Result := inherited GetRVFExtraPropertyCount;
  if BackgroundImageFileName<>'' then
    inc(Result);
end;
{------------------------------------------------------------------------------}
procedure TRVTableItemInfo.SaveRVFExtraProperties(Stream: TStream);
begin
  inherited SaveRVFExtraProperties(Stream);
  if BackgroundImageFileName<>'' then
    WriteRVFExtraStrPropertyStr(Stream, rvespImageFileName,
      BackgroundImageFileName);
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetExtraStrProperty(
  Prop: TRVExtraItemStrProperty; var Value: String): Boolean;
begin
  case Prop of
    rvespImageFileName:
      begin
        Value := BackgroundImageFileName;
        Result := True;
      end;
    else
      Result := inherited GetExtraStrProperty(Prop, Value);
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.SetExtraStrProperty(
  Prop: TRVExtraItemStrProperty; const Value: String): Boolean;
begin
  case Prop of
    rvespImageFileName:
      begin
        BackgroundImageFileName := Value;
        Result := True;
      end;
    else
      Result := inherited SetExtraStrProperty(Prop, Value);
  end;
end;
{------------------------------------------------------------------------------}
function TRVTableItemInfo.GetItemNoInRootDocument: Integer;
var RVData: TCustomRVData;
    Location: TRVStoreSubRVData;
    ParentItemNo: Integer;
begin
  RVData := Rows.FMainRVData;
  ParentItemNo := GetMyItemNo;
  Result := ParentItemNo;
  repeat
    RVData.GetParentInfo(ParentItemNo, Location);
    Location.Free;
    if ParentItemNo>=0 then begin
      Result := ParentItemNo;
      RVData := RVData.GetAbsoluteParentData;
    end;
  until ParentItemNo<0;
end;

initialization
  RegisterRichViewItemClass(rvsTable, TRVTableItemInfo);

end.
