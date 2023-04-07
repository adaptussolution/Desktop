////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   SUIStatusBar.pas
//  Creator     :   Shen Min
//  Date        :   2003-08-04 V4
//  Comment     :
//
//  Copyright (c) 2002-2003 Sunisoft
//  http://www.sunisoft.com
//  Email: support@sunisoft.com
//
////////////////////////////////////////////////////////////////////////////////

unit SUIStatusBar;

interface

{$I SUIPack.inc}

uses Windows, ComCtrls, Graphics, Classes, Forms, Controls, Messages,
     SUIThemes, SUIMgr;

type
{$IFDEF SUIPACK_D6UP}
    TsuiStatusBar = class(TCustomStatusBar)
{$ENDIF}
{$IFDEF SUIPACK_D5}
    TsuiStatusBar = class(TStatusBar)
{$ENDIF}
    private
        m_UIStyle : TsuiUIStyle;
        m_FileTheme : TsuiFileTheme;
        m_HandleRect : TRect;
        m_MouseDown : Boolean;
        m_Handled : Boolean;

        procedure SetFileTheme(const Value: TsuiFileTheme);
        procedure SetUIStyle(const Value: TsuiUIStyle);
        function GetPanelColor(): TColor;
        procedure SetPanelColor(const Value: TColor);
        procedure WMPaint(var Msg : TMessage); message WM_PAINT;

    protected
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
        procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
        procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
        
    public
        constructor Create(AOwner : TComponent); override;

    published
        property FileTheme : TsuiFileTheme read m_FileTheme write SetFileTheme;
        property UIStyle : TsuiUIStyle read m_UIStyle write SetUIStyle;
        property PanelColor : TColor read GetPanelColor write SetPanelColor;

        property Action;
        property AutoHint default False;
        property Align default alBottom;
        property Anchors;
        property BiDiMode;
        property BorderWidth;
        property Color default clBtnFace;
        property DragCursor;
        property DragKind;
        property DragMode;
        property Enabled;
        property Font {$IFDEF SUIPACK_D6UP}stored IsFontStored{$ENDIF};
        property Constraints;
        property Panels;
        property ParentBiDiMode;
        property ParentColor default False;
        property ParentFont default False;
        property ParentShowHint;
        property PopupMenu;
        property ShowHint;
        property SimplePanel default False;
        property SimpleText;
        property SizeGrip default True;
        property UseSystemFont default True;
        property Visible;
        property OnClick;
        property OnContextPopup;
{$IFDEF SUIPACK_D6UP}
        property OnCreatePanelClass;
{$ENDIF}
        property OnDblClick;
        property OnDragDrop;
        property OnDragOver;
        property OnEndDock;
        property OnEndDrag;
        property OnHint;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property OnResize;
        property OnStartDock;
        property OnStartDrag;

    end;

implementation

uses SUIPublic, SUIForm;

{ TsuiStatusBar }

constructor TsuiStatusBar.Create(AOwner: TComponent);
begin
    inherited;
    m_HandleRect := Rect(0, 0, 0, 0);
    m_MouseDown := false;
    m_Handled := false;
    ControlStyle := ControlStyle + [csAcceptsControls];
    UIStyle := GetSUIFormStyle(AOwner);
end;

function TsuiStatusBar.GetPanelColor: TColor;
begin
    Result := Color;
end;

procedure TsuiStatusBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    inherited;

    m_MouseDown := true;
end;

procedure TsuiStatusBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
    Form : TForm;
    ParentForm : TForm;
    MousePoint : TPoint;
begin
    inherited;

    if m_MouseDown and m_Handled then
    begin
        MousePoint.X := X;
        MousePoint.Y := Y;
        MousePoint := ClientToScreen(MousePoint);
        Form := GetParentForm(self) as TForm;
        if Form = nil then
            Exit;
        if Form.FormStyle = fsMDIChild then
            if Application.MainForm.FormStyle = fsMDIForm then
            begin
                ParentForm := Application.MainForm;
                MousePoint := ParentForm.ScreenToClient(MousePoint);
                MousePoint.Y := MousePoint.Y - 40;
                MousePoint.X := MousePoint.X + 2;
            end;

        Form.Width := MousePoint.X - Form.Left + 5;
        Form.Height := MousePoint.Y - Form.Top + 5;
        Exit;
    end;

    if not InRect(X, Y, m_HandleRect) then
    begin
        Cursor := crDefault;
        m_Handled := false;
    end
    else
    begin
        Cursor := crSizeNWSE;
        m_Handled := true;
    end;
end;

procedure TsuiStatusBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
    inherited;

    m_MouseDown := false;
end;

procedure TsuiStatusBar.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
    inherited;

    if (
        (Operation = opRemove) and
        (AComponent = m_FileTheme)
    )then
    begin
        m_FileTheme := nil;
        SetUIStyle(SUI_THEME_DEFAULT);          
    end;
end;

procedure TsuiStatusBar.SetFileTheme(const Value: TsuiFileTheme);
begin
    m_FileTheme := Value;
    SetUIStyle(m_UIStyle);
end;

procedure TsuiStatusBar.SetPanelColor(const Value: TColor);
begin
    Color := Value;
end;

procedure TsuiStatusBar.SetUIStyle(const Value: TsuiUIStyle);
var
    OutUIStyle : TsuiUIStyle;
begin
    m_UIStyle := Value;
    if UsingFileTheme(m_FileTheme, m_UIStyle, OutUIStyle) then
        PanelColor := m_FileTheme.GetColor(SUI_THEME_FORM_BACKGROUND_COLOR)
    else
        PanelColor := GetInsideThemeColor(OutUIStyle, SUI_THEME_FORM_BACKGROUND_COLOR);
    Repaint();
end;

procedure TsuiStatusBar.WMPaint(var Msg: TMessage);
var
    Buf : TBitmap;
    Form : TsuiForm;
    ParentForm : TCustomForm;
begin
    inherited;

    if Parent = nil then
        Exit;
    if Parent is TsuiForm then
        Form := Parent as TsuiForm
    else
        Form := nil;
    if Form = nil then
        Exit;

    if (Form.GetFormBorderStyle() <> bsSizeable) and (Form.GetFormBorderStyle() <> bsSizeToolWin) then
        Exit;

    ParentForm := GetParentForm(self);
    if ParentForm = nil then
        Exit;
    if ParentForm.WindowState = wsMaximized then
        Exit;
    Buf := TBitmap.Create();
    Buf.LoadFromResourceName(hInstance, 'STATUSBAR_HANDLE');
    Buf.Transparent := true;
    m_HandleRect := Rect(Width - Buf.Width div 2 - 2, Height - Buf.Height div 2 - 2, Width, Height);
    Canvas.Draw(Width - Buf.Width - 1, Height - Buf.Height - 1, Buf);
    Buf.Free();
end;

end.
