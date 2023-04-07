
{*******************************************************}
{                                                       }
{       RichView                                        }
{       Design-time support.                            }
{                                                       }
{       Copyright (c) Sergey Tkachenko                  }
{       svt@trichview.com                               }
{       http://www.trichview.com                        }
{                                                       }
{*******************************************************}

unit RVSEdit;

interface
{$I RV_Defs.inc}
uses
  Windows, Messages, SysUtils, Classes, Controls, Forms,
  {$IFDEF RICHVIEWDEF6}
  DesignIntf, DesignEditors,
  {$ELSE}
  DsgnIntf,
  {$ENDIF}
  TypInfo, ShellApi,
  RVStyle, RichView, RVEdit, RVCodePages, RVReport;
{$IFDEF RICHVIEWCBDEF3}
type
{$IFDEF RICHVIEWDEF6}
  TRVComponentEditor = TDefaultEditor;
{$ELSE}
  TRVComponentEditor = TComponentEditor;
{$ENDIF}
  {----------------------------------------------------------}
  TRVSEditor = class(TRVComponentEditor)
  protected
    {$IFDEF RICHVIEWDEF6}
    VerbIndex: Integer;
    procedure EditProperty(const PropertyEditor: IProperty;
      var Continue: Boolean); override;
    {$ELSE}
    FContinue : Boolean;
    procedure CheckEditF(PropertyEditor: TPropertyEditor);
    procedure CheckEditP(PropertyEditor: TPropertyEditor);
    procedure CheckEditL(PropertyEditor: TPropertyEditor);
    {$ENDIF}
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TRVEEditor = class(TRVComponentEditor)
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

  TRVCodePageProperty = class(TIntegerProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  procedure Register;
{$ENDIF}
implementation
uses RVDsgn;
{$IFDEF RICHVIEWCBDEF3}
{-----------------------------------------------------------------------}
function TRVSEditor.GetVerbCount: Integer;
begin
  Result := 5;
end;
{-----------------------------------------------------------------------}
function TRVSEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Edit Text Styles...';
    1:
      Result := 'Edit Paragraph Styles...';
    2:
      Result := 'Edit List Styles...';
    3:
      Result := '-';
    4:
      {$IFDEF RVDEBUG}
      Result := 'Register TRichView Online';
      {$ELSE}
      Result := 'TRichView Homepage';
      {$ENDIF}
    else
      Result := '';
  end;
end;
{-----------------------------------------------------------------------}
procedure TRVSEditor.ExecuteVerb(Index: Integer);
{$IFNDEF RICHVIEWDEF6}
var
{$IFDEF RICHVIEWDEF5}
  Components: TDesignerSelectionList;
{$ELSE}
  Components: TComponentList;
{$ENDIF}
{$ENDIF}
begin
  if Index>4 then exit;
  if Index=4 then begin
    {$IFDEF RVDEBUG}
    ShellExecute(0, 'open', 'http://www.trichview.com/rvregister.htm', nil, nil, SW_NORMAL);
    {$ELSE}
    ShellExecute(0, 'open', 'http://www.trichview.com', nil, nil, SW_NORMAL);
    {$ENDIF}
    exit;
  end;
  {$IFDEF RICHVIEWDEF6}
  VerbIndex := Index;
  Edit;
  {$ELSE}
  {$IFDEF RICHVIEWDEF5}
  Components := TDesignerSelectionList.Create;
  {$ELSE}
  Components := TComponentList.Create;
  {$ENDIF}
  try
    FContinue := True;
    Components.Add(Component);
    case Index of
     0:
      GetComponentProperties(Components, tkAny, Designer, CheckEditF);
     1:
      GetComponentProperties(Components, tkAny, Designer, CheckEditP);
     2:
      GetComponentProperties(Components, tkAny, Designer, CheckEditL);
    end;
  finally
    Components.Free;
  end;
  {$ENDIF}
end;
{-----------------------------------------------------------------------}
{$IFDEF RICHVIEWDEF6}
procedure TRVSEditor.EditProperty(const PropertyEditor: IProperty;
                      var Continue: Boolean);
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if ((VerbIndex=0) and (CompareText(PropertyEditor.GetName, 'TextStyles')=0)) or
     ((VerbIndex=1) and (CompareText(PropertyEditor.GetName, 'ParaStyles')=0)) or
     ((VerbIndex=2) and (CompareText(PropertyEditor.GetName, 'ListStyles')=0)) then
  begin
    PropertyEditor.Edit;
    Continue := False;
    VerbIndex := 0;
  end;
end;
{-----------------------------------------------------------------------}
{$ELSE}
procedure TRVSEditor.CheckEditF(PropertyEditor: TPropertyEditor);
begin
  try
    if FContinue and (CompareText(PropertyEditor.GetName, 'TextStyles') = 0) then
    begin
      PropertyEditor.Edit;
      FContinue := False;
    end;
  finally
    PropertyEditor.Free;
  end;
end;
{-----------------------------------------------------------------------}
procedure TRVSEditor.CheckEditP(PropertyEditor: TPropertyEditor);
begin
  try
    if FContinue and (CompareText(PropertyEditor.GetName, 'ParaStyles') = 0) then
    begin
      PropertyEditor.Edit;
      FContinue := False;
    end;
  finally
    PropertyEditor.Free;
  end;
end;
{-----------------------------------------------------------------------}
procedure TRVSEditor.CheckEditL(PropertyEditor: TPropertyEditor);
begin
  try
    if FContinue and (CompareText(PropertyEditor.GetName, 'ListStyles') = 0) then
    begin
      PropertyEditor.Edit;
      FContinue := False;
    end;
  finally
    PropertyEditor.Free;
  end;
end;
{$ENDIF}
{============================== TRVEEditor ====================================}
procedure TRVEEditor.ExecuteVerb(Index: Integer);
var frm:TfrmRVDesign;
begin
  case Index of
   0:
     begin
       frm := TfrmRVDesign.Create(Application);
       try
         frm.SetRichView(Component as TCustomRichView);
         if frm.ShowModal=mrOk then begin
           Designer.Modified;
         end;
       finally
         frm.Free;
       end;
     end;
   1:
     begin
       {$IFDEF RVDEBUG}
       ShellExecute(0, 'open', 'http://www.trichview.com/rvregister.htm', nil, nil, SW_NORMAL);
       {$ELSE}
       ShellExecute(0, 'open', 'http://www.trichview.com', nil, nil, SW_NORMAL);
       {$ENDIF}
     end;
  end;
end;
{------------------------------------------------------------------------------}
function TRVEEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0:
      Result := 'Settings...';
    1:
      {$IFDEF RVDEBUG}
      Result := 'Register TRichView Online';
      {$ELSE}
      Result := 'TRichView Homepage';
      {$ENDIF}
    else
      Result := '';
  end;
end;
{------------------------------------------------------------------------------}
function TRVEEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;
{=============================== TRVCodePageProperty ==========================}
function TRVCodePageProperty.GetAttributes: TPropertyAttributes;
begin
 Result := [paMultiSelect, paValueList, paRevertable];
end;
{------------------------------------------------------------------------------}
function TRVCodePageProperty.GetValue: string;
begin
  Result := CodePageToIdent(GetOrdValue);
end;
{------------------------------------------------------------------------------}
procedure TRVCodePageProperty.GetValues(Proc: TGetStrProc);
begin
  GetCodePageValues(Proc);
end;
{------------------------------------------------------------------------------}
procedure TRVCodePageProperty.SetValue(const Value: string);
var NewValue: TRVCodePage;
begin
  if IdentToCodePage(Value,NewValue) then
    SetOrdValue(NewValue)
  else
    inherited SetValue(Value);
end;
{==============================================================================}
{$IFDEF RICHVIEWDEF6}
const TRVIOCategory = 'Import/Export';
      TRVRVFCategory = 'RVF';
      TRVCPCategory ='Checkpoints';
      TRVHypertextCategory = 'Hypertext';
      TRVStyleNameCategory = 'Style Name';
{$ELSE}
{$IFDEF RICHVIEWDEF5}
type
 TRVStyleNameCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;
 TRVHypertextCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;
 TRVCPCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;
 TRVRVFCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;
 TRVIOCategory = class(TPropertyCategory)
  public
    class function Name: string; override;
    class function Description: string; override;
  end;

class function TRVStyleNameCategory.Description: string;
begin
  Result := 'Style Name';
end;
class function TRVStyleNameCategory.Name: string;
begin
  Result := 'Style Name';
end;

class function TRVHypertextCategory.Description: string;
begin
  Result := 'Hypertext related properties';
end;
class function TRVHypertextCategory.Name: string;
begin
  Result := 'Hypertext';
end;

class function TRVCPCategory.Description: string;
begin
  Result := 'Checkpoint related properties';
end;
class function TRVCPCategory.Name: string;
begin
  Result := 'Checkpoints';
end;

class function TRVRVFCategory.Description: string;
begin
  Result := 'RichView Format related properties'
end;
class function TRVRVFCategory.Name: string;
begin
  Result := 'RVF'
end;

class function TRVIOCategory.Description: string;
begin
  Result := 'Import/Export';
end;
class function TRVIOCategory.Name: string;
begin
  Result := 'Import/Export';
end;

{$ENDIF}
{$ENDIF}
{-----------------------------------------------------------------------}
procedure Register;
{$IFDEF RICHVIEWDEF6}
const
  TLocalizableCategory: String = sLocalizableCategoryName;
  TInputCategory:       String = sInputCategoryName;
  TVisualCategory:      String = sVisualCategoryName;
  TLegacyCategory:      String = sLegacyCategoryName;
{$ENDIF}
begin
  RegisterComponentEditor(TRVStyle, TRVSEditor);
  RegisterComponentEditor(TCustomRichView, TRVEEditor);
  RegisterPropertyEditor(TypeInfo(TRVCodePage), nil,'',  TRVCodePageProperty);
  {$IFDEF RICHVIEWDEF5}
  RegisterPropertiesInCategory(TLocalizableCategory, TFontInfo,
    ['StyleName','Charset','FontName','Size', 'Unicode']);
  RegisterPropertiesInCategory(TInputCategory, TFontInfo,
    ['NextStyleNo', 'Protection']);
  RegisterPropertiesInCategory(TVisualCategory, TFontInfo,
    ['Charset','FontName','Size','Style','StyleEx','VShift','Unicode',
     'CharScale', 'CharSpacing']);
  RegisterPropertiesInCategory(TRVStyleNameCategory, TFontInfo,
    ['StyleName']);
  RegisterPropertiesInCategory(TRVHypertextCategory, TFontInfo,
    ['Jump', 'JumpCursor', 'HoverColor', 'HoverBackColor']);
  RegisterPropertiesInCategory(TRVStyleNameCategory, TFontInfo,
    ['StyleName']);

  RegisterPropertiesInCategory(TLocalizableCategory, TParaInfo,
    ['StyleName','FirstIndent','LeftIndent','RightIndent', 'Alignment',
     'SpaceAfter', 'SpaceBefore']);
  RegisterPropertiesInCategory(TVisualCategory, TParaInfo,
    ['FirstIndent','LeftIndent','RightIndent', 'Alignment',
    'SpaceAfter', 'SpaceBefore', 'Border', 'Background',
    'LineSpacing', 'LineSpacingType']);
  RegisterPropertiesInCategory(TRVStyleNameCategory, TParaInfo,
    ['StyleName']);
  RegisterPropertiesInCategory(TInputCategory, TParaInfo,
    ['NextParaNo']);

  RegisterPropertiesInCategory(TRVStyleNameCategory, TRVListInfo,
    ['StyleName']);
  RegisterPropertiesInCategory(TVisualCategory, TRVListInfo,
    ['OneLevelPreview']);

  RegisterPropertiesInCategory(TVisualCategory, TRVListLevel,
    ['ImageIndex', 'ImageList', 'Picture', 'FirstIndent',
     'FormatString', 'FormatStringW', 'LeftIndent', 'ListType',
     'MarkerAlignment', 'MarkerIndent']);

  RegisterPropertiesInCategory(TLocalizableCategory, TRVStyle,
    ['DefCodePage']);
  RegisterPropertiesInCategory(TRVHypertextCategory, TRVStyle,
    ['JumpCursor', 'HoverColor']);
  RegisterPropertiesInCategory(TRVCPCategory, TRVStyle,
    ['CheckpointColor', 'CheckpointEvColor']);
  RegisterPropertiesInCategory(TVisualCategory, TRVStyle,
    ['SelectionStyle', 'SelectionMode']);

  RegisterPropertiesInCategory(TLocalizableCategory, TCustomRichView,
    ['Delimiters']);
  RegisterPropertiesInCategory(TInputCategory, TCustomRichView,
    ['OnRVDblClick','OnRVMouseDown', 'OnRVMouseUp','OnRVRightClick',
     'WheelStep']);
  RegisterPropertiesInCategory(TRVRVFCategory, TCustomRichView,
    ['RVFOptions', 'RVFTextStylesReadMode', 'RVFParaStylesReadMode',
     'OnRVFControlNeeded','OnRVFImageListNeeded', 'OnRVFPictureNeeded']);
  RegisterPropertiesInCategory(TVisualCategory, TCustomRichView,
    ['BackgroundStyle',
      'LeftMargin','RightMargin','TopMargin','BottomMargin',
      'MinTextWidth','MaxTextWidth',
      'Tracking', 'VScrollVisible', 'HScrollVisible',
      'DoInPaletteMode', 'UseXPThemes']);
  RegisterPropertiesInCategory(TRVCPCategory, TCustomRichView,
    ['CPEventKind','OnCheckpointVisible']);
  RegisterPropertiesInCategory(TRVHypertextCategory, TCustomRichView,
    ['FirstJumpNo','OnJump','OnRVMouseMove', 'OnURLNeeded', 'OnReadHyperlink',
     'OnWriteHyperlink']);
  RegisterPropertiesInCategory(TLegacyCategory, TCustomRichView,
    ['AllowSelection', 'SingleClick', 'OnURLNeeded']);
  RegisterPropertiesInCategory(TRVIOCategory, TCustomRichView,
    ['RTFOptions', 'RTFReadProperties', 'OnSaveComponentToFile', 'OnURLNeeded',
     'OnReadHyperlink', 'OnWriteHyperlink', 
     'OnHTMLSaveImage', 'OnSaveImage2', 'OnImportPicture',
     'OnSaveRTFExtra', 'OnSaveHTMLExtra']);

  RegisterPropertiesInCategory(TInputCategory, TCustomRichViewEdit,
    ['EditorOptions', 'UndoLimit', 'OnCaretGetOut', 'OnCaretMove',
     'OnChange', 'OnChanging', 'OnStyleConversion', 'OnParaStyleConversion']);
  {$ENDIF}
end;
{$ENDIF}





end.
