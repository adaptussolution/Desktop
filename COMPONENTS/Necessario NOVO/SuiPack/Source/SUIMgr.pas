////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   SUIMgr.pas
//  Creator     :   Shen Min
//  Date        :   2003-02-26
//  Comment     :
//
//  Copyright (c) 2002-2003 Sunisoft
//  http://www.sunisoft.com
//  Email: support@sunisoft.com
//
////////////////////////////////////////////////////////////////////////////////

unit SUIMgr;

interface

{$I SUIPack.inc}

uses Classes, Controls, SysUtils, Forms, TypInfo, Graphics, Dialogs,
     SUIThemes;

type
    // --------- TsuiFileTheme --------------------
    TsuiFileTheme = class(TComponent)
    private
        m_Mgr : TsuiFileThemeMgr;
        m_ThemeFile : String;
        m_CanUse : Boolean;

        procedure SetThemeFile(const Value: String);

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy(); override;

        function CanUse() : Boolean;
        
        procedure GetBitmap(const Index : Integer; const Buf : TBitmap); overload;
        procedure GetBitmap(const Index : Integer; const Buf : TBitmap; SpitCount, SpitIndex : Integer); overload;
        function GetInt(const Index : Integer) : Integer;
        function GetColor(const Index : Integer) : TColor;
        function GetBool(const Index : Integer) : Boolean;

    published
        property ThemeFile : String read m_ThemeFile write SetThemeFile;

    end;

    // --------- TsuiThemeManager -----------------
    TsuiThemeMgrCompList = class(TStringList)
    end;

    TsuiThemeManager = class(TComponent)
    private
        m_UIStyle : TsuiUIStyle;
        m_FileTheme : TsuiFileTheme;
        m_List : TsuiThemeMgrCompList;
        m_OnUIStyleChanged : TNotifyEvent;

        procedure SetUIStyle(const Value: TsuiUIStyle);
        procedure UpdateAll();

        procedure SetList(const Value: TsuiThemeMgrCompList);
        procedure SetFileTheme(const Value: TsuiFileTheme);

    protected
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    public
        constructor Create(AOwner: TComponent); override;
        destructor Destroy(); override;

        procedure UpdateTheme();
        
    published
        property UIStyle : TsuiUIStyle read m_UIStyle write SetUIStyle;
        property FileTheme : TsuiFileTheme read m_FileTheme write SetFileTheme;
        property CompList : TsuiThemeMgrCompList read m_List write SetList;

        property OnUIStyleChanged : TNotifyEvent read m_OnUIStyleChanged write m_OnUIStyleChanged;
    end;

    procedure ThemeManagerEdit(AComp : TsuiThemeManager);
    procedure FileThemeEdit(AComp : TsuiFileTheme);

    function UsingFileTheme(
        const FileTheme : TsuiFileTheme;
        const UIStyle : TsuiUIStyle;
        out SuggUIStyle : TsuiUIStyle
    ) : Boolean;


implementation

uses frmThemeMgr, SUIPublic;

procedure ThemeManagerEdit(AComp : TsuiThemeManager);
var
    frmMgr: TfrmMgr;
    Form : TForm;
    i : Integer;
    nIndex : Integer;
    Comp : TComponent;
begin
    if not (AComp.Owner is TForm) then
        Exit;
    Form := (AComp.Owner as TForm);
    frmMgr := TfrmMgr.Create(nil);

    for i := 0 to Form.ComponentCount - 1 do
    begin
        Comp := Form.Components[i];
        if (
            (Copy(Comp.ClassName, 1, 4) = 'Tsui') and
            (IsHasProperty(Comp, 'UIStyle')) and
            (IsHasProperty(Comp, 'FileTheme')) and
            not (Comp is TsuiThemeManager)
        ) then
        begin
            nIndex := frmMgr.List.Items.AddObject(Comp.Name, Comp);
            if AComp.m_List.IndexOf(Comp.Name) = -1 then
                frmMgr.List.Checked[nIndex] := false
            else
                frmMgr.List.Checked[nIndex] := true;
        end;
    end;

    if frmMgr.ShowModal() = mrOK then
    begin
        AComp.m_List.Clear();
        for i := 0 to frmMgr.List.Items.Count - 1 do
        begin
            if frmMgr.List.Checked[i] then
                AComp.m_List.Add(frmMgr.List.Items[i]);
        end;
    end;
    frmMgr.Free();

    AComp.UpdateAll();
end;

procedure FileThemeEdit(AComp : TsuiFileTheme);
var
    OpenDialog: TOpenDialog;
begin
    OpenDialog := TOpenDialog.Create(Application);
    OpenDialog.Filter := 'SUIPack Theme File(*.sui)|*.sui';
    if OpenDialog.Execute() then
        AComp.ThemeFile := OpenDialog.FileName;
    OpenDialog.Free();
end;

{ TsuiThemeManager }

constructor TsuiThemeManager.Create(AOwner: TComponent);
begin
    inherited;

    m_List := TsuiThemeMgrCompList.Create();
    UIStyle := SUI_THEME_DEFAULT;
end;

destructor TsuiThemeManager.Destroy;
begin
    m_List.Free();
    m_List := nil;

    inherited;
end;

procedure TsuiThemeManager.Notification(AComponent: TComponent;
  Operation: TOperation);
var
    nIndex : Integer;
begin
    inherited;

    if AComponent = nil then
        Exit;

    if (Operation = opRemove) and (AComponent <> self) then
    begin
        nIndex := m_List.IndexOf(AComponent.Name);
        if nIndex <> -1 then
            m_List.Delete(nIndex);
    end;

    if (
        (Operation = opRemove) and
        (AComponent = m_FileTheme)
    )then
    begin
        m_FileTheme := nil;
        m_UIStyle := SUI_THEME_DEFAULT;
    end;
end;

procedure TsuiThemeManager.SetFileTheme(const Value: TsuiFileTheme);
begin
    m_FileTheme := Value;
    SetUIStyle(m_UIStyle);    
end;

procedure TsuiThemeManager.SetList(const Value: TsuiThemeMgrCompList);
begin
    m_List.Assign(Value);
end;

procedure TsuiThemeManager.SetUIStyle(const Value: TsuiUIStyle);
var
    NeedUpdate : Boolean;
begin
    if m_UIStyle = Value then
        NeedUpdate := false
    else
        NeedUpdate := true;
    m_UIStyle := Value;
    UpdateAll();
    if NeedUpdate then
        if Assigned(m_OnUIStyleChanged) then
            m_OnUIStyleChanged(self);
end;

procedure TsuiThemeManager.UpdateAll;
var
    i : Integer;
    Comp : TComponent;
    Form : TCustomForm;
begin
    if not (Owner is TForm) then
        Exit;
    Form := Owner as TForm;

    for i := 0 to m_List.Count - 1 do
    begin
        Comp := Form.FindComponent(m_List[i]);
        if Comp = nil then
            continue;
        SetOrdProp(Comp, 'UIStyle', Ord(UIStyle));
        SetObjectProp(Comp, 'FileTheme', m_FileTheme);
    end;
end;

procedure TsuiThemeManager.UpdateTheme;
begin
    UpdateAll();
end;

{ TsuiFileTheme }

function TsuiFileTheme.CanUse: Boolean;
begin
    Result := m_CanUse;
end;

constructor TsuiFileTheme.Create(AOwner: TComponent);
begin
    inherited;

    m_Mgr := TsuiFileThemeMgr.Create();
    m_CanUse := false;
end;

destructor TsuiFileTheme.Destroy;
begin
    m_Mgr.Free();
    m_Mgr := nil;

    inherited;
end;

procedure TsuiFileTheme.GetBitmap(const Index: Integer;
  const Buf: TBitmap);
begin
    m_Mgr.GetBitmap(Index, Buf);
end;

procedure TsuiFileTheme.GetBitmap(const Index: Integer; const Buf: TBitmap;
  SpitCount, SpitIndex: Integer);
var
    TempBmp : TBitmap;
begin
    TempBmp := TBitmap.Create();
    m_Mgr.GetBitmap(Index, TempBmp);
    SpitBitmap(TempBmp, Buf, SpitCount, SpitIndex);
    TempBmp.Free();
end;

function TsuiFileTheme.GetBool(const Index: Integer): Boolean;
begin
    Result := m_Mgr.GetBool(Index);
end;

function TsuiFileTheme.GetColor(const Index: Integer): TColor;
begin
    Result := m_Mgr.GetColor(Index);
end;

function TsuiFileTheme.GetInt(const Index: Integer): Integer;
begin
    Result := m_Mgr.GetInt(Index);
end;

procedure TsuiFileTheme.SetThemeFile(const Value: String);
var
    FileName : String;
    PathName : String;
    i : Integer;
    Form : TForm;
    Comp : TComponent;
begin
    if (m_ThemeFile = Value) then
        Exit;
    FileName := Value;
    
    if not FileExists(FileName) then
    begin
        PathName := ExtractFilePath(Application.ExeName);
        if PathName[Length(PathName)] <> '\' then
            PathName := PathName + '\';
        FileName := PathName + ExtractFileName(FileName);
        if not FileExists(FileName) then
            Exit;
    end;
    
    m_ThemeFile := FileName;
    m_CanUse := m_Mgr.LoadFromFile(m_ThemeFile);

    if (not m_CanUse) or (not (Owner is TForm)) then
        Exit;
    Form := Owner as TForm;
    for i := 0 to Form.ComponentCount - 1 do
    begin
        Comp := Form.Components[i];
        if (
            (Copy(Comp.ClassName, 1, 4) = 'Tsui') and
            (IsHasProperty(Comp, 'FileTheme'))
        ) then
        begin
            if GetObjectProp(Comp, 'FileTheme') = self then
                SetObjectProp(Comp, 'FileTheme', self);
        end;
    end;
end;

function UsingFileTheme(
    const FileTheme : TsuiFileTheme;
    const UIStyle : TsuiUIStyle;
    out SuggUIStyle : TsuiUIStyle
) : Boolean;
begin
    Result := false;
    SuggUIStyle := UIStyle;
    
    if UIStyle = FromThemeFile then
    begin
        if FileTheme = nil then
            SuggUIStyle := SUI_THEME_DEFAULT
        else if FileTheme.CanUse() then
        begin
            Result := true;
            Exit;
        end
        else
            SuggUIStyle := SUI_THEME_DEFAULT;
    end;
end;
    
end.
