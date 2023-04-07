////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   SUIForm.pas
//  Creator     :   Shen Min
//  Date        :   2002-05-21 V1-V3
//                  2003-06-19 V4
//  Comment     :
//
//  Copyright (c) 2002-2003 Sunisoft
//  http://www.sunisoft.com
//  Email: support@sunisoft.com
//
//  V3 Some code portions by Artyom Kamshilin (artyom@softadvance.com)
//
////////////////////////////////////////////////////////////////////////////////

unit SUIForm;

interface

{$I SUIPack.inc}

uses Windows, Messages, SysUtils, Classes, Controls, ExtCtrls, Graphics, Forms,
     ComCtrls, Menus, Dialogs, Buttons,
     SUITitleBar, SUIThemes, SUIMgr, SUIButton, SUIScrollBar, frmMSNPop;

type
    TsuiForm = class(TCustomPanel)
    private
        m_TitleBar : TsuiTitleBar;
        m_MenuBar : TToolBar;
        m_Menu : TMainMenu;
        m_PrevParentWndProc: Pointer;

        m_Form : TForm;
        m_BorderStyle : TFormBorderStyle;
        m_Color : TColor;
        m_Panel : TCustomPanel;
        m_Width : Integer;
        m_UIStyle : TsuiUIStyle;
        m_FileTheme : TsuiFileTheme;        
        m_MenuBarColor : TColor;
        m_MenuBarHeight : Integer;
        m_AppOnMsgAssigned : Boolean;
        m_UIStyleAutoUpdateSub : Boolean;

        m_FormInitRect : TRect;
        m_FormInitMax : Boolean;

        // AK - two new vars to calculate the client area difference
        // this difference will be later used to adjust for size changes
        // in design time - since in run time, m_Form.BorderStyle is bsNone,
        // and when adjusting the size in runtime, we have to count on this

        m_ClientAreaHeightDiff : integer;
        m_ClientAreaWidthDiff : integer;

        procedure NewParentWndProc(var Msg: TMessage);
        procedure ProcessKeyPress(var Msg : TMessage);
        procedure AlignSelf();
        procedure WMCREATE(var Msg : TMessage); message WM_CREATE;
        procedure WMERASEBKGND(var Msg : TMessage); message WM_ERASEBKGND;
        procedure SetButtons(const Value : TsuiTitleBarButtons);
        function GetButtons() : TsuiTitleBarButtons;

        function GetOnBtnClick() : TsuiTitleBarButtonClickEvent;
        procedure SetOnBtnClick(const Value : TsuiTitleBarButtonClickEvent);
        function GetOnHelpBtnClick() : TsuiTitleBarButtonClickEvent;
        procedure SetOnHelpBtnClick(const Value : TsuiTitleBarButtonClickEvent);

        function GetFont() : TFont;
        procedure SetFont(const Value : TFont);
        function GetCaption() : TCaption;
        procedure SetCaption(const Value : TCaption);

        procedure SetMenu(const Value : TMainMenu);
        procedure SetMenuBarColor(const Value : TColor);
        function GetSections: TsuiTitleBarSections;
        procedure SetSections(const Value: TsuiTitleBarSections);
        procedure SetColor(const Value: TColor);
        procedure SetHeight(const Value : Integer);
        function GetHeight() : Integer;
        procedure SetWidth(const Value : Integer);
        function GetWidth() : Integer;
        function GetDrawAppIcon: Boolean;
        procedure SetDrawAppIcon(const Value: Boolean);
        procedure SetMenuBarHeight(const Value: Integer);
        procedure SetTitleBarVisible(const Value: Boolean);
        function GetTitleBarVisible: Boolean;
        function GetMDIChild: Boolean;
        function GetRoundCorner: Integer;
        procedure SetRoundCorner(const Value: Integer);
        procedure SetFileTheme(const Value: TsuiFileTheme);

        procedure DrawButton(Sender: TToolBar; Button: TToolButton; State: TCustomDrawState; var DefaultDraw: Boolean);
        procedure DrawMenuBar(Sender: TToolBar; const ARect: TRect; var DefaultDraw: Boolean);

        procedure OnApplicationMessage(var Msg: TMsg; var Handled: Boolean);
        function GetTitleBarHeight: Integer;
        function GetTitleBarCustom: Boolean;
        procedure SetTitleBarCustom(const Value: Boolean);
        function GetVersion: String;
        procedure SetVersion(const Value: String);

    protected
        procedure SetPanel(const Value : TCustomPanel);
        procedure SetBorderWidth(const Value : Integer);
        procedure SetUIStyle(const Value : TsuiUIStyle);
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
        procedure PaintFormBorder();
        procedure Paint(); override;

        procedure CreateMenuBar();
        procedure DestroyMenuBar();

    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy(); override;

        procedure UpdateMenu();
        procedure UpdateTopMenu();
        procedure RepaintMenuBar();
        function GetFormBorderStyle() : TFormBorderStyle;        

        property MDIChild : Boolean read GetMDIChild;
        property TitleBarHeight : Integer read GetTitleBarHeight;

    published
        property TitleBarCustom : Boolean read GetTitleBarCustom write SetTitleBarCustom;
        property FileTheme : TsuiFileTheme read m_FileTheme write SetFileTheme;
        property UIStyle : TsuiUIStyle read m_UIStyle write SetUIStyle;
        property UIStyleAutoUpdateSub : Boolean read m_UIStyleAutoUpdateSub write m_UIStyleAutoUpdateSub;
        property BorderColor : TColor read m_Color write SetColor;
        property BorderWidth : Integer read m_Width write SetBorderWidth;
        property TitleBarVisible : Boolean read GetTitleBarVisible write SetTitleBarVisible default True;
        property BiDiMode;
        property Color;
        property Caption : TCaption read GetCaption write SetCaption;
        property FormPanel : TCustomPanel read m_Panel write SetPanel;
        property TitleBarButtons : TsuiTitleBarButtons read GetButtons write SetButtons;
        property TitleBarSections : TsuiTitleBarSections read GetSections write SetSections;
        property TitleBarDrawAppIcon : Boolean read GetDrawAppIcon write SetDrawAppIcon;
        property Font : TFont read GetFont write SetFont;
        property Menu : TMainMenu read m_Menu write SetMenu;
        property MenuBarColor : TColor read m_MenuBarColor write SetMenuBarColor;
        property MenuBarHeight : Integer read m_MenuBarHeight write SetMenuBarHeight;
        property Height read GetHeight write SetHeight;
        property Width read GetWidth write SetWidth;
        property RoundCorner : Integer read GetRoundCorner write SetRoundCorner;        
        property PopupMenu;
        property Version : String read GetVersion write SetVersion;

        property OnTitleBarCustomBtnsClick : TsuiTitleBarButtonClickEvent read GetOnBtnClick write SetOnBtnClick;
        property OnTitleBarHelpBtnClick : TsuiTitleBarButtonClickEvent read GetOnHelpBtnClick write SetOnHelpBtnClick;
        property OnMouseDown;
        property OnClick;
        property OnMouseUp;

    end;

    TsuiMDIForm = class(TComponent)
    private
        m_Form : TForm;
        m_PrevParentWndProc: Pointer;
        m_PrevClientWndProc: Pointer;
        m_TopPanel : TPanel;
        m_TitleBar : TsuiTitleBar;
        m_MenuBar : TToolBar;
        m_AppOnMsgAssigned : Boolean;
        m_ControlBtns : array [1..3] of TsuiToolBarSpeedButton;
        m_DrawChildCaptions : Boolean;
        m_DrawChildMenus : Boolean;

        m_Menu : TMainMenu;
        m_TopMenu : TMainMenu;
        m_UIStyle : TsuiUIStyle;
        m_FileTheme : TsuiFileTheme;
        m_MenuBarColor : TColor;
        m_BorderColor : TColor;

        m_ChildMaxed : Boolean;

        procedure NewParentWndProc(var Msg: TMessage);
        procedure NewClientWndProc(var Msg : TMessage);
        procedure ProcessKeyPress(var Msg : TMessage);
        procedure OnControlButtonClick(Sender : TObject);
        procedure OnApplicationMessage(var Msg: TMsg; var Handled: Boolean);
        procedure SetMenu(const Value: TMainMenu);
        procedure SetMenuBarColor(const Value : TColor);
        procedure SetUIStyle(const Value : TsuiUIStyle);
        procedure SetFileTheme(const Value: TsuiFileTheme);
        procedure SetBorderColor(const Value: TColor);
        function GetCaption: TCaption;
        procedure SetCaption(const Value: TCaption);

        procedure PaintBorder();
        procedure GetNextTopLeft(var nTop, nLeft : Integer);
        function GetActiveChildSUIForm() : TsuiForm;

        procedure CreateMenuBar();
        procedure DestroyMenuBar();
        procedure ShowMenuBar();
        procedure HideMenuBar();
        procedure ShowControlButtons();
        procedure HideControlButtons();
        function GetMainMenu() : TMainMenu;
        function GetTitleBarCaption(): String;

        procedure SetDrawAppIcon(const Value: Boolean);
        function GetDrawAppIcon: Boolean;
        function GetTitleBarCustom: Boolean;
        procedure SetTitleBarCustom(const Value: Boolean);
        procedure SetTitleBarVisible(const Value: Boolean);
        function GetTitleBarVisible: Boolean;
        function GetSections: TsuiTitleBarSections;
        procedure SetSections(const Value: TsuiTitleBarSections);
        procedure SetButtons(const Value : TsuiTitleBarButtons);
        function GetButtons() : TsuiTitleBarButtons;

    protected
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy(); override;

        procedure UpdateMenu();
        procedure Cascade();
        procedure Tile();

        function GetTitleBarHeight: integer;
    published
        property UIStyle : TsuiUIStyle read m_UIStyle write SetUIStyle;
        property FileTheme : TsuiFileTheme read m_FileTheme write SetFileTheme;
        property Menu : TMainMenu read m_Menu write SetMenu;
        property MenuBarColor : TColor read m_MenuBarColor write SetMenuBarColor;
        property BorderColor : TColor read m_BorderColor write SetBorderColor;
        property Caption : TCaption read GetCaption write SetCaption;
        property TitleBarDrawAppIcon : Boolean read GetDrawAppIcon write SetDrawAppIcon default False;
        property TitleBarCustom : Boolean read GetTitleBarCustom write SetTitleBarCustom default False;
        property TitleBarVisible : Boolean read GetTitleBarVisible write SetTitleBarVisible default True;
        property TitleBarButtons : TsuiTitleBarButtons read GetButtons write SetButtons;
        property TitleBarSections : TsuiTitleBarSections read GetSections write SetSections;
        property TitleBarDrawChildCaptions : Boolean read m_DrawChildCaptions write m_DrawChildCaptions default True;
        property TitleBarDrawChildMenus : Boolean read m_DrawChildMenus write m_DrawChildMenus default True;
    end;

//    TsuiScrollBox = class(TWinControl)
//    private
//        // scroll bar
//        m_VScrollBar : TsuiScrollBar;
//        m_HScrollBar : TsuiScrollBar;
//        m_MouseDown : Boolean;
//        m_SelfChanging : Boolean;
//        procedure SetVScrollBar(const Value: TsuiScrollBar);
//        procedure SetHScrollBar(const Value: TsuiScrollBar);
//        procedure OnVScrollBarChange(Sender : TObject);
//        procedure OnHScrollBarChange(Sender : TObject);
//        procedure UpdateScrollBars();
//        procedure UpdateScrollBarsPos();
//
//        procedure CMEnabledChanged(var Msg : TMessage); message CM_ENABLEDCHANGED;
//        procedure WMSIZE(var Msg : TMessage); message WM_SIZE;
//        procedure WMMOVE(var Msg : TMessage); message WM_MOVE;
//        procedure WMMOUSEWHEEL(var Message: TMessage); message WM_MOUSEWHEEL;
//        procedure WMLBUTTONDOWN(var Message: TMessage); message WM_LBUTTONDOWN;
//        procedure WMLButtonUp(var Message: TMessage); message WM_LBUTTONUP;
//        procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
//        procedure WMMOUSEMOVE(var Message: TMessage); message WM_MOUSEMOVE;
//        procedure WMVSCROLL(var Message: TWMVScroll); message WM_VSCROLL;
//        procedure WMHSCROLL(var Message: TWMHScroll); message WM_HSCROLL;
//
//    protected
//        procedure Notification(AComponent: TComponent; Operation: TOperation); override;
//
//    public
//        constructor Create(AOwner: TComponent); override;
//
//    published
//        // scroll bar
//        property VScrollBar : TsuiScrollBar read m_VScrollBar write SetVScrollBar;
//        property HScrollBar : TsuiScrollBar read m_HScrollBar write SetHScrollBar;
//    
//    end;

    TsuiMSNPopForm = class(TComponent)
    private
        m_Form : TfrmMSNPopForm;
        m_AnimateTime : Integer;
        m_StayTime : Integer;
        m_X : Integer;
        m_Y : Integer;
        m_AutoPosition : Boolean;
        m_ClickHide : Boolean;
        m_Title : TCaption;
        m_Text : String;
        m_TextFont : TFont;
        m_TitleFont : TFont;

        procedure SetTextFont(const Value: TFont);
        procedure SetTitleFont(const Value: TFont);

    public
        constructor Create(AOwner : TComponent); override;
        destructor Destroy(); override;

        procedure Popup();
        procedure Close();

    published
        property AnimateTime : Integer read m_AnimateTime write m_AnimateTime;
        property StayTime : Integer read m_StayTime write m_StayTime;
        property PositionX : Integer read m_X write m_X;
        property PositionY : Integer read m_Y write m_Y;
        property AutoPosition : Boolean read m_AutoPosition write m_AutoPosition;
        property ClickHide : Boolean read m_ClickHide write m_ClickHide;
        property Title : TCaption read m_Title write m_Title;
        property MessageText : String read m_Text write m_Text;

        property TitleFont : TFont read m_TitleFont write SetTitleFont;
        property MessageFont : TFont read m_TextFont write SetTextFont;
    end;


implementation

uses SUIResDef, SUIPublic, SUIImagePanel, SUIMainMenu, SUIMenu, SUIProgressBar;

const
    SUIM_UPDATEMDIMENU  = WM_USER + 3888;
    SUIM_MDICHILDMAX    = WM_USER + 3889;
    SUIM_MDICHILDNOMAX  = WM_USER + 3890;

{ TsuiForm }

procedure TsuiForm.AlignSelf();
var
    nTop : Integer;
begin
    if m_TitleBar.Visible then
        nTop := 0
    else
        nTop := m_Width;
    if m_Form.FormStyle = fsMDIChild then
        SetBounds(m_Width, nTop, m_Form.ClientWidth - (m_Width + 1) * 2 - 1, m_Form.ClientHeight - m_Width - nTop - 3)
    else
        SetBounds(m_Width, nTop, m_Form.ClientWidth - m_Width * 2, m_Form.ClientHeight - m_Width - nTop);
end;

constructor TsuiForm.Create(AOwner: TComponent);
var
{$WARNINGS OFF}
    P : Pointer;
{$WARNINGS ON}
    i : Integer;
begin
    inherited;

    if not (AOwner is TForm) then
        Exit;

    m_Form := AOwner as TForm;

    if not (csDesigning in ComponentState) then
    begin
        m_Form.Constraints.MinHeight := 50;
        m_Form.Constraints.MinWidth := 125;
    end;
    m_Form.BorderWidth := 0;
    m_FormInitRect := Classes.Rect(m_Form.Left, m_Form.Top, m_Form.Width + m_Form.Left, m_Form.Height + m_Form.Top);
    m_FormInitMax := (m_Form.WindowState = wsMaximized);

    m_TitleBar := TsuiTitleBar.Create(self);
    m_TitleBar.Parent := self;

    m_MenuBar := nil;
    m_MenuBarColor := Color;
    m_MenuBarHeight := 22;

    m_BorderStyle := m_Form.BorderStyle;
    UIStyle := SUI_THEME_DEFAULT;
    m_UIStyleAutoUpdateSub := false;

    // AK - calculate the client area difference in next two lines
    m_ClientAreaHeightDiff := m_Form.Height - m_Form.ClientHeight;
    m_ClientAreaWidthDiff := m_Form.Width - m_Form.ClientWidth;

    BevelOuter := bvNone;
    BevelInner := bvNone;
    BorderStyle := bsNone;
    Align := alNone;
    Caption := m_Form.Caption;
    TitleBarVisible := true;
    inherited Caption := ' ';
    m_AppOnMsgAssigned := false;

    for i := 0 to m_Form.ControlCount - 1 do
    begin
        if m_Form.Controls[i] is TsuiForm then
            raise Exception.Create('Sorry, You can create only one TsuiForm component in one form!');
    end;

    m_Form.Scaled := false;

    if not (csDesigning in ComponentState) then
    begin
        if (m_Form.FormStyle <> fsMDIChild) and (m_Form.FormStyle <> fsMDIForm) then
            m_Form.BorderStyle := bsNone;

        m_Form.AutoScroll := false;

        m_PrevParentWndProc := Pointer(GetWindowLong(TForm(Owner).Handle, GWL_WNDPROC));
        P := MakeObjectInstance(NewParentWndProc);
        SetWindowLong(TForm(AOwner).Handle, GWL_WNDPROC, LongInt(P));
        SetWindowLong(m_Form.Handle, GWL_STYLE, GETWINDOWLONG(m_Form.Handle, GWL_STYLE) and (not WS_CAPTION));

        if not Assigned(Application.OnMessage) then
        begin
            m_AppOnMsgAssigned := true;
            Application.OnMessage := OnApplicationMessage;
        end;
    end
//    else
//    begin
//        Height := Height + 27;
//        Width := Width + 6;
//    end;
end;

procedure TsuiForm.CreateMenuBar;
begin
    if m_MenuBar <> nil then
        Exit;
    if m_Form.FormStyle = fsMDIChild then
        Exit;
    m_MenuBar := TToolBar.Create(self);
    m_MenuBar.Parent := self;
    m_MenuBar.Flat := true;
    m_MenuBar.EdgeBorders := [];
    m_MenuBar.ShowCaptions := true;
    m_MenuBar.Height := m_MenuBarHeight;
    m_MenuBar.Color := m_MenuBarColor;
    if not (csDesigning in ComponentState) then
        m_MenuBar.OnCustomDrawButton := DrawButton;
    m_MenuBar.OnCustomDraw := DrawMenuBar;
    m_TitleBar.Top := 0;
end;

destructor TsuiForm.Destroy;
begin
    if m_AppOnMsgAssigned then
        Application.OnMessage := nil;

    m_MenuBar.Free();
    m_MenuBar := nil;

    m_TitleBar.Free();
    m_TitleBar := nil;

    inherited;
end;

procedure TsuiForm.DestroyMenuBar;
begin
    if m_Form.FormStyle <> fsMDIChild then
    begin
        m_MenuBar.Free();
        m_MenuBar := nil;
    end;
    m_Menu := nil;
end;

procedure TsuiForm.DrawButton(Sender: TToolBar; Button: TToolButton;
  State: TCustomDrawState; var DefaultDraw: Boolean);
var
    ACanvas : TCanvas;
    ARect : TRect;
    R : TRect;
    Bmp : TBitmap;
    Buf : TBitmap;
    Style : TsuiUIStyle;
    OutUIStyle : TsuiUIStyle;
    CanSetFont : Boolean;
    AFileTheme : TsuiFileTheme;
    bUseFileTheme : Boolean;
begin
    Style := m_UIStyle;
    AFileTheme := m_FileTheme;
    CanSetFont := false;

    ACanvas := Sender.Canvas;
    ARect := Button.BoundsRect;

    if Menu <> nil then
    begin
        if Menu is TsuiMainMenu then
        begin
            Style := (Menu as TsuiMainMenu).UIStyle;
            AFileTheme := (Menu as TsuiMainMenu).FileTheme;
            if (m_Menu as TsuiMainMenu).UseSystemFont then
                Menu_GetSystemFont(ACanvas.Font)
            else
            begin
                ACanvas.Font.Name := (m_Menu as TsuiMainMenu).FontName;
                ACanvas.Font.Size := (m_Menu as TsuiMainMenu).FontSize;
                ACanvas.Font.Charset := (m_Menu as TsuiMainMenu).FontCharset;
                ACanvas.Font.Color := (m_Menu as TsuiMainMenu).FontColor;
                CanSetFont := true;
            end;
        end;
    end;

    // MacOS
    if (
        ((cdsHot in State) or (cdsSelected in State)) and
        {$IFDEF RES_MACOS} (Style = MacOS) {$ELSE} false {$ENDIF}
    ) then
    begin
        DefaultDraw := false;

        Buf := TBitmap.Create();
        Bmp := TBitmap.Create();
        Buf.Width := Button.Width;
        Buf.Height := Button.Height;

        R := Rect(0, 0, Buf.Width, Buf.Height);
        Bmp.LoadFromResourceName(hInstance, 'MACOS_MENU_SELECT');
        Buf.Canvas.StretchDraw(R, Bmp);

        Buf.Canvas.Brush.Style := bsClear;
        if CanSetFont then
        begin
            Buf.Canvas.Font.Name := (m_Menu as TsuiMainMenu).FontName;
            Buf.Canvas.Font.Size := (m_Menu as TsuiMainMenu).FontSize;
            Buf.Canvas.Font.Charset := (m_Menu as TsuiMainMenu).FontCharset;
        end
        else
        begin
            Buf.Canvas.Font.Name := ACanvas.Font.Name;
            Buf.Canvas.Font.Size := ACanvas.Font.Size;
            Buf.Canvas.Font.Charset := ACanvas.Font.Charset;
        end;

        if UsingFileTheme(AFileTheme, Style, OutUIStyle) then
            Buf.Canvas.Font.Color := AFileTheme.GetColor(SUI_THEME_MENU_SELECTED_FONT_COLOR)
        else
            Buf.Canvas.Font.Color := GetInsideThemeColor(OutUIStyle, SUI_THEME_MENU_SELECTED_FONT_COLOR);
            
        R.Bottom := R.Bottom + 2;
        DrawText(Buf.Canvas.Handle, PChar(Button.Caption), -1, R, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
        ACanvas.Draw(ARect.Left, ARect.Top, Buf);

        Bmp.Free();
        Buf.Free();
    end

    else if (
        (cdsHot in State) or
        (cdsSelected in State)
    ) then
    begin // selected or hot top menu
        DefaultDraw := false;

        // draw client background
        if cdsSelected in State then
            ARect.Right := ARect.Right - 2;
        if UsingFileTheme(AFileTheme, Style, OutUIStyle) then
        begin
            ACanvas.Brush.Color := AFileTheme.GetColor(SUI_THEME_MENU_SELECTED_BACKGROUND_COLOR);
            bUseFileTheme := true
        end
        else
        begin
            ACanvas.Brush.Color := GetInsideThemeColor(OutUIStyle, SUI_THEME_MENU_SELECTED_BACKGROUND_COLOR);
            bUseFileTheme := false            
        end;
        ACanvas.FillRect(ARect);

        // draw border
        if bUseFileTheme then
            ACanvas.Brush.Color := AFileTheme.GetColor(SUI_THEME_MENU_SELECTED_BORDER_COLOR)
        else
            ACanvas.Brush.Color := GetInsideThemeColor(OutUIStyle, SUI_THEME_MENU_SELECTED_BORDER_COLOR);
        ACanvas.FrameRect(ARect);

        // draw text
        if cdsSelected in State then
            ARect.Left := ARect.Left + 2;
        ARect.Top := ARect.Top + 2;

        if bUseFileTheme then
            ACanvas.Font.Color := AFileTheme.GetColor(SUI_THEME_MENU_SELECTED_FONT_COLOR)
        else
            ACanvas.Font.Color := GetInsideThemeColor(OutUIStyle, SUI_THEME_MENU_SELECTED_FONT_COLOR);

        DrawText(ACanvas.Handle, PChar(Button.Caption), -1, ARect, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
        
//        // draw shadow (only selected, no hot)
//        if (cdsSelected in State) then
//        begin
//            ACanvas.Pen.Color := GetThemeColor(Style, SUI_MENU_SHADOW_COLOR);
//            ACanvas.Rectangle(ARect.Right, ARect.Top, ARect.Right + 2, ARect.Bottom);
//        end;
    end

    // not select and not hot top menu
    else
        DefaultDraw := true;
end;

procedure TsuiForm.DrawMenuBar(Sender: TToolBar; const ARect: TRect;
  var DefaultDraw: Boolean);
var
    ACanvas : TCanvas;
    Buf : TBitmap;
    Style : TsuiUIStyle;
begin
    Style := m_UIStyle;
    if Menu <> nil then
    begin
        if Menu is TsuiMainMenu then
            Style := (Menu as TsuiMainMenu).UIStyle;
    end;

{$IFDEF RES_MACOS}
    if (Style = MacOS) then
    begin
        ACanvas := Sender.Canvas;
        Buf := TBitmap.Create();
        Buf.LoadFromResourceName(hInstance, 'MACOS_MENU_BAR');
        ACanvas.StretchDraw(ARect, Buf);
        Buf.Free();
    end;
{$ENDIF}
end;

function TsuiForm.GetButtons: TsuiTitleBarButtons;
begin
    Result := m_TitleBar.Buttons;
end;

function TsuiForm.GetCaption: TCaption;
begin
    Result := m_TitleBar.Caption;
end;

function TsuiForm.GetDrawAppIcon: Boolean;
begin
    Result := m_TitleBar.DrawAppIcon;
end;

function TsuiForm.GetFont: TFont;
begin
    Result := m_TitleBar.Font;
end;

function TsuiForm.GetFormBorderStyle: TFormBorderStyle;
begin
    Result := m_BorderStyle;
end;

function TsuiForm.GetHeight: Integer;
begin
    Result := inherited Height;
end;

function TsuiForm.GetMDIChild: Boolean;
begin
    Result := (m_Form.FormStyle = fsMDIChild);
end;

function TsuiForm.GetOnBtnClick: TsuiTitleBarButtonClickEvent;
begin
    Result := m_TitleBar.OnCustomBtnsClick;
end;

function TsuiForm.GetOnHelpBtnClick: TsuiTitleBarButtonClickEvent;
begin
    Result := m_TitleBar.OnHelpBtnClick;
end;

function TsuiForm.GetRoundCorner: Integer;
begin
    Result := m_TitleBar.RoundCorner;
end;

function TsuiForm.GetSections: TsuiTitleBarSections;
begin
    Result := m_TitleBar.Sections;
end;

function TsuiForm.GetTitleBarCustom: Boolean;
begin
    Result := m_TitleBar.Custom;
end;

function TsuiForm.GetTitleBarHeight: Integer;
begin
    Result := m_TitleBar.Height;
end;

function TsuiForm.GetTitleBarVisible: Boolean;
begin
    Result := m_TitleBar.Visible;
end;

function TsuiForm.GetVersion: String;
begin
    Result := '4.20';
end;

function TsuiForm.GetWidth: Integer;
begin
    Result := inherited Width;
end;

var
    l_InFlag : Integer = 0;
    l_MaxFlag : Boolean = false;

procedure TsuiForm.NewParentWndProc(var Msg: TMessage);
var
    Pt : TPoint;
    Rect : TRect;
begin
    Msg.Result := CallWindowProc(m_PrevParentWndProc, TForm(Owner).Handle, Msg.Msg, Msg.WParam, Msg.LParam);

    if Msg.Msg = WM_MDIACTIVATE then
    begin
        if TWMMDIActivate(Msg).ActiveWnd = m_Form.Handle then
        begin
            SendMessage(Application.MainForm.ClientHandle, SUIM_UPDATEMDIMENU, 0, 0);
            m_TitleBar.FormActive := true;
        end
        else
            m_TitleBar.FormActive := false
    end;

    if Msg.Msg = WM_KEYDOWN then
        ProcessKeyPress(Msg);

    if Msg.Msg = WM_SHOWWINDOW then
    begin
        m_Form.Menu := nil;
    end;

    if Msg.Msg = WM_DESTROY then
    begin
        if m_Form.FormStyle = fsMDIChild then
        begin
            SendMessage(Application.MainForm.ClientHandle, SUIM_UPDATEMDIMENU, 0, 0);
            if m_Form.WindowState = wsMaximized then
                SendMessage(Application.MainForm.ClientHandle, SUIM_MDICHILDNOMAX, 0, 0);
        end;
    end;

   if Msg.Msg = WM_ACTIVATE then
    begin
        if Msg.WParamLo = WA_INACTIVE then
            m_TitleBar.FormActive := false
        else
            m_TitleBar.FormActive := true;
    end;

    if Msg.Msg = WM_SIZE then
    begin
        if m_FormInitMax then
        begin
            if m_Form.WindowState <> wsMaximized then
            begin
                PlaceControl(m_Form, m_FormInitRect);
                m_FormInitMax := false;
            end
            else if l_InFlag = 2 then
            begin
                m_Form.WindowState := wsMaximized;
            end
            else
                Inc(l_InFlag);
            l_MaxFlag := true;
        end;
        if (m_Form.WindowState = wsMaximized) and (m_Form.FormStyle <> fsMDIChild) then
        begin
            if m_BorderStyle <> bsNone then
            begin
                Rect := GetWorkAreaRect();
                PlaceControl(m_Form, Rect);
            end;
        end;
        AlignSelf();
        PaintFormBorder();
        m_TitleBar.ProcessMaxBtn();
        if m_Form.FormStyle = fsMDIChild then
        begin
            if m_Form.WindowState = wsMaximized then
            begin
                TitleBarVisible := false;
                if not l_MaxFlag then
                begin
                    Self.ScrollBy(0, m_TitleBar.Height * -1);
                    l_MaxFlag := true;
                    SendMessage(Application.MainForm.ClientHandle, SUIM_MDICHILDMAX, m_Form.Handle, 0);
                end;
            end
            else
            begin
                TitleBarVisible := true;
                if l_MaxFlag then
                begin
                    Self.ScrollBy(0, m_TitleBar.Height);
                    l_MaxFlag := false;
                    m_TitleBar.ProcessMaxBtn();
                    SendMessage(Application.MainForm.ClientHandle, SUIM_MDICHILDNOMAX, 0, 0);                    
                end;
            end;
        end;
        m_TitleBar.OnFormReSize();
    end;

    if Msg.Msg = WM_ERASEBKGND then
        PaintFormBorder();

    if Msg.Msg = WM_NCHITTEST then
    begin
        if m_Form.WindowState = wsMaximized then
            Exit;

        if m_Form.FormStyle = fsMDIForm then
        begin
            if Msg.Result = HTCLIENT then
                Msg.Result := HTTRANSPARENT;
        end;

        with m_Form do
        begin
            if m_BorderStyle <> bsSizeable then
                Exit;
            Pt := Point(Msg.LParamLo, Msg.LParamHi);
            Pt := ScreenToClient(Pt);
            if (Pt.X < 5) and (Pt.Y < 5) then
                Msg.Result := htTopLeft
            else if (Pt.X > ClientWidth - 5) and (Pt.y < 5) then
                Msg.Result := htTopRight
            else if (Pt.X > ClientWidth - 5) and (Pt.Y > ClientHeight - 5) then
                Msg.Result := htBottomRight
            else if (Pt.X < 5) and (Pt.Y > ClientHeight - 5) then
                Msg.Result := htBottomLeft
            else if (Pt.X < 5) then
                Msg.Result := htLeft
            else if (Pt.Y < 5) then
                Msg.Result := htTop
            else if (Pt.X > ClientWidth - 5) then
                Msg.Result := htRight
            else if (Pt.Y > ClientHeight - 5) then
                Msg.Result := htBottom;
        end; // with
    end;
end;

procedure TsuiForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
    inherited;

    if AComponent = nil then
        Exit;

    if (
        (Operation = opInsert) and
        (csDesigning in ComponentState) and
        (not (csLoading in ComponentState)) and
        (AComponent.ClassName <> 'TsuiMainMenu') and
        (AComponent is TMainMenu)
    ) then
    begin
        MessageDlg(
            'Strongly recommend you to use "TsuiMainMenu" instead of "TMainMenu".'
                + SUI_2ENTER +  'If you still want to use TMainForm, '
                + SUI_2ENTER + 'set ' + m_Form.Name + '''s MENU property to NIL please.'
                + SUI_2ENTER + 'And set ' + Name + '''s MENU property to this Menu when you finished designing the menu.',
            mtInformation,
            [mbOK],
            0
        );
    end;

    if (
        (Operation = opRemove) and
        (AComponent = m_Panel)
    )then
        m_Panel := nil;

    if (
        (Operation = opRemove) and
        (AComponent = m_Menu)
    ) then
        DestroyMenuBar();

    if (
        (Operation = opRemove) and
        (AComponent = self)
    )then
    begin
        m_Form.Repaint();

        if m_Menu <> nil then
        begin
            if m_Menu is TsuiMainMenu then
                (m_Menu as TsuiMainMenu).Form := nil;
        end;
    end;

    if (
        (Operation = opRemove) and
        (AComponent = m_FileTheme)
    )then
    begin
        m_FileTheme := nil;
        ContainerApplyUIStyle(self, SUI_THEME_DEFAULT, nil);
        SetUIStyle(SUI_THEME_DEFAULT);
    end;
end;

procedure TsuiForm.OnApplicationMessage(var Msg: TMsg;
  var Handled: Boolean);
var
    AMsg : TMessage;
begin
    if Msg.message = WM_KEYDOWN then
    begin
        AMsg.Msg := Msg.message;
        AMsg.WParam := Msg.wParam;
        AMsg.LParam := Msg.lParam;
        ProcessKeyPress(AMsg);
        Handled := Boolean(AMsg.Result);
    end;
end;

procedure TsuiForm.Paint;
var
    Buf : TBitmap;
    Bmp : TBitmap;
begin
    if {$IFDEF RES_MACOS} (m_UIStyle = MacOS) {$ELSE} false {$ENDIF} or
        {$IFDEF RES_PROTEIN} (m_UIStyle = Protein) {$ELSE} false {$ENDIF} then
    begin
        Buf := TBitmap.Create();
        Bmp := TBitmap.Create();
        try
            Buf.Width := Width;
            Buf.Height := Height;
{$IFDEF RES_MACOS}
            if m_UIStyle = MacOS then
                Bmp.LoadFromResourceName(hInstance, 'MACOS_FORM_BACKGROUND')
            else
{$ENDIF}            
                Bmp.LoadFromResourceName(hInstance, 'PROTEIN_FORM_BACKGROUND');
            Buf.Canvas.Brush.Bitmap := Bmp;
            Buf.Canvas.FillRect(ClientRect);
            BitBlt(Canvas.Handle, 0, 0, Width, Height, Buf.Canvas.Handle, 0, 0, SRCCOPY);
        finally
            Bmp.Free();
            Buf.Free();
        end;
    end
    else
    begin
        Canvas.Brush.Color := Color;
        Canvas.FillRect(ClientRect);
    end;

    if csDesigning in ComponentState then
    begin
        AlignSelf();
        PaintFormBorder();
    end;
end;

procedure TsuiForm.PaintFormBorder();
var
    R : TRect;
begin
    if m_Form.FormStyle = fsMDIChild then
    begin
        m_Form.Canvas.Pen.Color := m_Color;
        m_Form.Canvas.Pen.Width := m_Width;
        m_Form.Canvas.Pen.Style := psSolid;

        m_Form.Canvas.Brush.Color := m_Color;
        R := Rect(0, 0, m_Width, m_Form.ClientHeight - 2);
        m_Form.Canvas.FillRect(R);
        R := Rect(0, m_Form.ClientHeight - m_Width - 3, m_Form.ClientWidth, m_Form.ClientHeight);
        m_Form.Canvas.FillRect(R);
        R := Rect(m_Form.ClientWidth - m_Width - 3, m_Form.ClientHeight, m_Form.ClientWidth, 0);
        m_Form.Canvas.FillRect(R);
        m_Form.Canvas.Pen.Width := m_Width;
        m_Form.Canvas.MoveTo(0, 0);
        m_Form.Canvas.LineTo(m_Form.ClientWidth, 0);
    end
    else
    begin
        m_Form.Canvas.Pen.Color := m_Color;
        m_Form.Canvas.Pen.Width := m_Width;
        m_Form.Canvas.Pen.Style := psSolid;

        m_Form.Canvas.Brush.Color := m_Color;
        R := Rect(0, 0, m_Width, m_Form.ClientHeight);
        m_Form.Canvas.FillRect(R);
        R := Rect(0, m_Form.ClientHeight - m_Width, m_Form.ClientWidth, m_Form.ClientHeight);
        m_Form.Canvas.FillRect(R);
        R := Rect(m_Form.ClientWidth - m_Width, m_Form.ClientHeight, m_Form.ClientWidth, 0);
        m_Form.Canvas.FillRect(R);
        m_Form.Canvas.Pen.Width := m_Width;
        m_Form.Canvas.MoveTo(0, 0);
        m_Form.Canvas.LineTo(m_Form.ClientWidth, 0);
    end;
end;

procedure TsuiForm.ProcessKeyPress(var Msg: TMessage);
begin
    if (not FormHasFocus(m_Form)) or (not m_Form.Active) then
        Msg.Result := 0
    else if not Assigned(Menu) then
        Msg.Result := 0
    else if m_Menu.IsShortCut(TWMKEY(Msg)) then
        Msg.Result := 1
    else
        Msg.Result := 0;
end;

procedure TsuiForm.RepaintMenuBar;
begin
    if (m_MenuBar = nil) or (m_Menu = nil) then
        Exit;
    if m_Menu is TsuiMainMenu then
        if (m_Menu as TsuiMainMenu).UseSystemFont then
            Menu_GetSystemFont(m_MenuBar.Font)
        else
        begin
            m_MenuBar.Font.Name := (m_Menu as TsuiMainMenu).FontName;
            m_MenuBar.Font.Size := (m_Menu as TsuiMainMenu).FontSize;
            m_MenuBar.Font.Charset := (m_Menu as TsuiMainMenu).FontCharset;
        end;
    m_MenuBar.Repaint();
end;

procedure TsuiForm.SetBorderWidth(const Value: Integer);
begin
    m_Width := Value;

    SetWidth(Width);
    AlignSelf();
    SetHeight(Height);
    PaintFormBorder();
end;

procedure TsuiForm.SetButtons(const Value: TsuiTitleBarButtons);
begin
    m_TitleBar.Buttons.Assign(Value);
end;

procedure TsuiForm.SetCaption(const Value: TCaption);
begin
    m_TitleBar.Caption := Value;
    m_Form.Caption := Value;
end;

procedure TsuiForm.SetColor(const Value: TColor);
begin
    m_Color := Value;

    AlignSelf();
    PaintFormBorder();
end;

procedure TsuiForm.SetDrawAppIcon(const Value: Boolean);
begin
    m_TitleBar.DrawAppIcon := Value;
end;

procedure TsuiForm.SetFileTheme(const Value: TsuiFileTheme);
begin
    if m_FileTheme = Value then
        Exit;
    m_FileTheme := Value;
    SetUIStyle(m_UIStyle);
end;

procedure TsuiForm.SetFont(const Value: TFont);
begin
    m_TitleBar.Font := Value;
end;

procedure TsuiForm.SetHeight(const Value: Integer);
begin
    if csDesigning in ComponentState then
        m_Form.Height := Value + m_Width + m_Width div 2 + m_ClientAreaHeightDiff
    else
        m_Form.Height := Value + m_Width + m_Width div 2;

    AlignSelf();
end;

procedure TsuiForm.SetMenu(const Value: TMainMenu);
begin
    if m_Menu <> nil then
    begin
        if m_Menu is TsuiMainMenu then
            (m_Menu as TsuiMainMenu).Form := nil;
    end;

    m_Menu := Value;

    if m_Menu is TsuiMainMenu then
        (m_Menu as TsuiMainMenu).Form := self;

    if m_Menu <> nil then
    begin
        if m_MenuBar = nil then
            CreateMenuBar();
        UpdateMenu();
    end
    else
        DestroyMenuBar();
end;

procedure TsuiForm.SetMenuBarColor(const Value: TColor);
begin
    m_MenuBarColor := Value;

    if m_MenuBar <> nil then
        m_MenuBar.Color := Value;
end;

procedure TsuiForm.SetMenuBarHeight(const Value: Integer);
var
    i : Integer;
begin
    m_MenuBarHeight := Value;

    if m_MenuBar <> nil then
    begin
        m_MenuBar.Height := m_MenuBarHeight;
        for i := 0 to m_MenuBar.ButtonCount - 1 do
            m_MenuBar.Buttons[i].Height := m_MenuBarHeight;
    end;
end;

procedure TsuiForm.SetOnBtnClick(
  const Value: TsuiTitleBarButtonClickEvent);
begin
    m_TitleBar.OnCustomBtnsClick := Value;
end;

procedure TsuiForm.SetOnHelpBtnClick(
  const Value: TsuiTitleBarButtonClickEvent);
begin
    m_TitleBar.OnHelpBtnClick := Value;
end;

procedure TsuiForm.SetPanel(const Value: TCustomPanel);
begin
    if Value = self then
    begin
        MessageDlg('Sorry, you can''t select the Form assign to FormPanel property', mtError, [mbOK], 0);	 
        Exit;
    end;

    m_Panel := Value;

    if m_Panel = nil then
        Exit;

    m_Panel.Align := alClient;

    if m_Panel is TPanel then
    begin
        TPanel(m_Panel).BevelOuter := bvNone;
        TPanel(m_Panel).BevelInner := bvNone;
        TPanel(m_Panel).BorderStyle := bsNone;
        TPanel(m_Panel).Caption := '';
    end
    else if m_Panel is TsuiImagePanel then
        TsuiImagePanel(m_Panel).Caption := '';
end;

procedure TsuiForm.SetRoundCorner(const Value: Integer);
begin
    if m_Form.FormStyle <> fsMDIChild then
        m_TitleBar.RoundCorner := Value;
end;

procedure TsuiForm.SetSections(const Value: TsuiTitleBarSections);
begin
    m_TitleBar.Sections.Assign(Value);
end;

procedure TsuiForm.SetTitleBarCustom(const Value: Boolean);
begin
    m_TitleBar.Custom := Value;
end;

procedure TsuiForm.SetTitleBarVisible(const Value: Boolean);
begin
    if m_TitleBar.Visible = Value then
        Exit;
    m_TitleBar.Visible := Value;
    m_TitleBar.Top := 0;
    AlignSelf();
    PaintFormBorder();
end;

procedure TsuiForm.SetUIStyle(const Value: TsuiUIStyle);
var
    OutUIStyle : TsuiUIStyle;
    Rect: TRect;
begin
    Rect := m_Form.BoundsRect;

    m_UIStyle := Value;

    if UsingFileTheme(m_FileTheme, m_UIStyle, OutUIStyle) then
    begin
        BorderColor := m_FileTheme.GetColor(SUI_THEME_FORM_BORDER_COLOR);
        BorderWidth := m_FileTheme.GetInt(SUI_THEME_FORM_BORDERWIDTH_INT);
        Color := m_FileTheme.GetColor(SUI_THEME_FORM_BACKGROUND_COLOR);
    end
    else
    begin
        BorderColor := GetInsideThemeColor(OutUIStyle, SUI_THEME_FORM_BORDER_COLOR);
        BorderWidth := GetInsideThemeInt(OutUIStyle, SUI_THEME_FORM_BORDERWIDTH_INT);
        Color := GetInsideThemeColor(OutUIStyle, SUI_THEME_FORM_BACKGROUND_COLOR);
    end;
    MenuBarColor := Color;

    m_TitleBar.FileTheme := m_FileTheme;
    m_TitleBar.UIStyle := m_UIStyle;

    if m_Form.FormStyle = fsMDIChild then
        m_TitleBar.RoundCorner := 0;

    if m_Menu <> nil then
    begin
        if m_Menu is TsuiMainMenu then
        begin
            (m_Menu as TsuiMainMenu).UIStyle := m_UIStyle;
            (m_Menu as TsuiMainMenu).FileTheme := m_FileTheme;
        end;
    end;

    if m_UIStyleAutoUpdateSub then
        ContainerApplyUIStyle(self, m_UIStyle, FileTheme);

    if M_Form.FormStyle = fsMDIChild then
      M_Form.BoundsRect := Rect;

    AlignSelf();
    Repaint();
    PaintFormBorder();
end;


procedure TsuiForm.SetVersion(const Value: String);
begin
    // do nothing
end;

procedure TsuiForm.SetWidth(const Value: Integer);
begin
    if csDesigning in ComponentState then
        m_Form.Width := Value + m_Width * 2 + m_ClientAreaWidthDiff
    else
        m_Form.Width := Value + m_Width * 2;
    AlignSelf();
end;

procedure TsuiForm.UpdateMenu;
var
    i : Integer;
    Button : TToolButton;
begin
    if m_MenuBar = nil then
        Exit;

    if m_Menu = nil then
    begin
        DestroyMenuBar();
        Exit;
    end;

    for i := 0 to m_MenuBar.ButtonCount - 1 do
        m_MenuBar.Buttons[0].Free();

    for i := m_Menu.Items.Count - 1 downto 0 do
    begin
        if m_Menu.Items[i].Parent <> m_Menu.Items then
            continue;
        Button := TToolButton.Create(self);
        Button.Parent := m_MenuBar;
        Button.Grouped := true;
        Button.MenuItem := m_Menu.Items[i];
        Button.AutoSize := true;
    end;
end;

procedure TsuiForm.UpdateTopMenu;
var
    i : Integer;
begin
    if m_MenuBar = nil then
        Exit;
    for i := 0 to m_MenuBar.ButtonCount - 1 do
    begin
        if m_MenuBar.Buttons[i].MenuItem <> nil then
        begin
            m_MenuBar.Buttons[i].Caption := m_MenuBar.Buttons[i].MenuItem.Caption;
            m_MenuBar.Buttons[i].Enabled := m_MenuBar.Buttons[i].MenuItem.Enabled;
        end;
    end;
end;

procedure TsuiForm.WMCREATE(var Msg: TMessage);
begin
    AlignSelf();
    PaintFormBorder();
end;

procedure TsuiForm.WMERASEBKGND(var Msg: TMessage);
begin
    PaintFormBorder();
end;

{ TsuiMDIForm }

procedure TsuiMDIForm.Cascade;
var
    i : Integer;
    nLeft, nTop : Integer;
    nWidth, nHeight : Integer;
begin
    nLeft := 0;
    nTop := 0;
    nWidth := Trunc(m_Form.ClientWidth * 0.8);
    nHeight := Trunc(m_Form.ClientHeight * 0.6);
    for i := m_Form.MDIChildCount - 1 downto 0 do
    begin
        if m_Form.MDIChildren[i].WindowState = wsMinimized then
            continue;
        m_Form.MDIChildren[i].SetBounds(nLeft, nTop, nWidth, nHeight);
        GetNextTopLeft(nTop, nLeft);
    end;
end;

constructor TsuiMDIForm.Create(AOwner: TComponent);
var
    P : Pointer;
begin
    inherited;
    if not (AOwner is TForm) then
        Raise Exception.Create('Sorry, TsuiMDIForm must be placed on a form');
    m_Form := AOwner as TForm;
    m_ChildMaxed := false;
    m_AppOnMsgAssigned := false;
    m_TopMenu := nil;

    m_Form.Constraints.MinHeight := 50;
    m_Form.Constraints.MinWidth := 125;
    m_Form.BorderWidth := 1;
    m_Form.BorderStyle := bsNone;
    m_TitleBar := TsuiTitleBar.Create(self);
    m_TitleBar.Parent := m_Form;
    m_TitleBar.Top := 0;
    m_TitleBar.Left := 0;
    m_TitleBar.Caption := m_Form.Caption;


    m_DrawChildCaptions := true;
    m_DrawChildMenus := true;

    CreateMenuBar();
    UpdateMenu();
    UIStyle := SUI_THEME_DEFAULT;

    if not (csDesigning in ComponentState) then
    begin
       m_Form.AutoScroll := false;

        m_PrevParentWndProc := Pointer(GetWindowLong(TForm(Owner).Handle, GWL_WNDPROC));
        P := MakeObjectInstance(NewParentWndProc);
        SetWindowLong(TForm(AOwner).Handle, GWL_WNDPROC, LongInt(P));
        SetWindowLong(m_Form.Handle, GWL_STYLE, GETWINDOWLONG(m_Form.Handle, GWL_STYLE) and (not WS_CAPTION));

        m_PrevClientWndProc := Pointer(GetWindowLong(TForm(Owner).ClientHandle, GWL_WNDPROC));
        P := MakeObjectInstance(NewClientWndProc);
        SetWindowLong(TForm(AOwner).ClientHandle, GWL_WNDPROC, LongInt(P));

        if not Assigned(Application.OnMessage) then
        begin
            m_AppOnMsgAssigned := true;
            Application.OnMessage := OnApplicationMessage;
        end;
    end;


end;

procedure TsuiMDIForm.CreateMenuBar;
var
    i : Integer;
    TempBmp : TBitmap;
    nLeft : Integer;
begin
    if (m_MenuBar <> nil) then
        Exit;
    m_TopPanel := TPanel.Create(self);
    m_TopPanel.BevelOuter := bvNone;
    m_TopPanel.BevelInner := bvNone;
    m_TopPanel.Align := alTop;
    m_TopPanel.Parent := m_Form;
    m_TopPanel.AutoSize := true;
    m_TopPanel.Visible := false;

    m_MenuBar := TToolBar.Create(self);
    m_MenuBar.Parent := m_TopPanel;
    m_MenuBar.Flat := true;
    m_MenuBar.EdgeBorders := [];
    m_MenuBar.ShowCaptions := true;
    m_MenuBar.AutoSize := true;

    m_TitleBar.Top := 0;
    if (csDesigning in ComponentState) then
        exit;

    TempBmp := TBitmap.Create();
    TempBmp.Transparent := true;
    TempBmp.LoadFromResourceName(hInstance, 'MDI_CONTROL_BUTTON');

    nLeft := m_TopPanel.Width - 20;
    for i := 1 to 3 do
    begin
        m_ControlBtns[i] := TsuiToolBarSpeedButton.Create(self);
        m_ControlBtns[i].Parent := m_TopPanel;
        m_ControlBtns[i].Anchors := [akTop, akRight];
        SpitBitmap(TempBmp, m_ControlBtns[i].Glyph, 3, 4 - i);
        m_ControlBtns[i].Left := nLeft;
        m_ControlBtns[i].Top := 1;
        Dec(nLeft, 18);
        m_ControlBtns[i].Tag := -1380 - i;
        m_ControlBtns[i].OnClick := OnControlButtonClick;
        m_ControlBtns[i].Color := m_MenuBar.Color;
        m_ControlBtns[i].BringToFront();
        m_ControlBtns[i].Visible := false;
    end;

    TempBmp.Free();

end;

destructor TsuiMDIForm.Destroy;
begin
    DestroyMenuBar();
    m_TitleBar.Free();
    m_TitleBar := nil;
    if not (csDesigning in ComponentState) then
    begin
        if m_AppOnMsgAssigned then
            Application.OnMessage := nil;
    end;
    inherited;
end;

procedure TsuiMDIForm.DestroyMenuBar;
var
    i : Integer;
begin
    if (m_MenuBar = nil) then
        Exit;

    if not (csDesigning in ComponentState) then
         for i := 1 to 3 do
            begin
                m_ControlBtns[i].Free();
                m_ControlBtns[i] := nil;
            end;

    m_MenuBar.Free();
    m_MenuBar := nil;
    m_TopPanel.Free();
    m_TopPanel := nil;
    m_Menu := nil;
end;

function TsuiMDIForm.GetActiveChildSUIForm: TsuiForm;
var
    i : Integer;
    Form : TForm;
begin
    Result := nil;
    Form := m_Form.ActiveMDIChild;
    if Form = nil then
        Exit;
    for i := 0 to Form.ControlCount - 1 do
    begin
        if Form.Controls[i] is TsuiForm then
        begin
            Result := Form.Controls[i] as TsuiForm;
            Break;
        end;
    end;
end;

function TsuiMDIForm.GetCaption: TCaption;
begin
    Result := m_Form.Caption;
end;

function TsuiMDIForm.GetTitleBarCaption: String;
var
    Form : TForm;
begin
    if m_Form.ActiveMDIChild = nil then
    begin
        m_TitleBar.Caption := m_Form.Caption;
        Exit;
    end;

    Form := m_Form.ActiveMDIChild;

    if not m_DrawChildCaptions or (Pos(' - [', m_Form.Caption) <> 0) then
        m_TitleBar.Caption := m_Form.Caption
    else
        m_TitleBar.Caption := m_Form.Caption + ' - [' + Form.Caption + ']';
end;

function TsuiMDIForm.GetMainMenu: TMainMenu;
var
    i : Integer;
    Form : TForm;
begin
    Result := m_Menu;

    if not m_DrawChildMenus then
        Exit;
    if m_TitleBar = nil then
        Exit;
    if m_Form.ActiveMDIChild = nil then
        Exit;

    Form := m_Form.ActiveMDIChild;

    if Form.Menu <> nil then
    begin
        Result := Form.Menu;
        Exit;
    end;

    for i := 0 to Form.ControlCount - 1 do
    begin
        if Form.Controls[i] is TsuiForm then
        begin
            if (Form.Controls[i] as TsuiForm).Menu <> nil then
                Result := (Form.Controls[i] as TsuiForm).Menu;
            break;
        end;
    end;
end;

procedure TsuiMDIForm.GetNextTopLeft(var nTop, nLeft: Integer);
begin
    Inc(nTop, m_TitleBar.Height + 3);
    Inc(nLeft, m_TitleBar.Height + 3);
end;

procedure TsuiMDIForm.HideControlButtons;
var
    i : Integer;
begin
    for i := 1 to 3 do
        m_ControlBtns[i].Visible := false;
end;

procedure TsuiMDIForm.HideMenuBar;
begin
    if (m_MenuBar = nil) then
        Exit;
    m_TopPanel.Visible := false;
end;

procedure TsuiMDIForm.NewClientWndProc(var Msg: TMessage);
begin
    Msg.Result := CallWindowProc(m_PrevClientWndProc, TForm(Owner).ClientHandle, Msg.Msg, Msg.WParam, Msg.LParam);

    if Msg.Msg = SUIM_UPDATEMDIMENU then
    begin
        UpdateMenu();
        GetTitleBarCaption();
    end;

    if Msg.Msg = SUIM_MDICHILDMAX then
    begin
        ShowControlButtons();
    end;

    if Msg.Msg = SUIM_MDICHILDNOMAX then
    begin
        HideControlButtons();
    end;

    if Msg.Msg = WM_MDITILE then
    begin
        Tile();
    end;

    if Msg.Msg = WM_MDICASCADE then
    begin
        Cascade();
    end;
end;

procedure TsuiMDIForm.NewParentWndProc(var Msg: TMessage);
var
    Pt : TPoint;
    Rect : TRect;
begin
    Msg.Result := CallWindowProc(m_PrevParentWndProc, TForm(Owner).Handle, Msg.Msg, Msg.WParam, Msg.LParam);

    if Msg.Msg = WM_KEYDOWN then
        ProcessKeyPress(Msg);

    if (
        (Msg.Msg = WM_LBUTTONDOWN) or
        (Msg.Msg = WM_LBUTTONUP) or
        (Msg.Msg = WM_MOUSEMOVE) or
        (Msg.Msg = WM_RBUTTONDOWN) or
        (Msg.Msg = WM_RBUTTONUP) or
        (Msg.Msg = WM_LBUTTONDBLCLK) or
        (Msg.Msg = WM_RBUTTONDBLCLK)
    ) then
        SendMessage(m_TitleBar.Handle, Msg.Msg, Msg.WParam, Msg.LParam);

    if Msg.Msg = WM_NCPAINT then
    begin
        PaintBorder();
        m_TitleBar.OnFormReSize();
    end;

   if Msg.Msg = WM_ACTIVATE then
    begin
        if Msg.WParamLo = WA_INACTIVE then
            m_TitleBar.FormActive := false
        else
            m_TitleBar.FormActive := true;
    end;

    if Msg.Msg = WM_SIZE then
    begin
        if (m_Form.WindowState = wsMaximized) and (m_Form.FormStyle <> fsMDIChild) then
        begin
            Rect := GetWorkAreaRect();
            PlaceControl(m_Form, Rect);
        end;
        PaintBorder();
        m_TitleBar.OnFormReSize();        
    end;

    if Msg.Msg = WM_NCHITTEST then
    begin
        if m_Form.WindowState = wsMaximized then
            Exit;

        with m_Form do
        begin
            Pt := Point(Msg.LParamLo, Msg.LParamHi);
            Pt := ScreenToClient(Pt);
            if (Pt.X < 5) and (Pt.Y < 5) then
                Msg.Result := htTopLeft
            else if (Pt.X > ClientWidth - 5) and (Pt.y < 5) then
                Msg.Result := htTopRight
            else if (Pt.X > ClientWidth - 5) and (Pt.Y > ClientHeight - 5) then
                Msg.Result := htBottomRight
            else if (Pt.X < 5) and (Pt.Y > ClientHeight - 5) then
                Msg.Result := htBottomLeft
            else if (Pt.X < 5) then
                Msg.Result := htLeft
            else if (Pt.Y < 5) then
                Msg.Result := htTop
            else if (Pt.X > ClientWidth - 5) then
                Msg.Result := htRight
            else if (Pt.Y > ClientHeight - 5) then
                Msg.Result := htBottom;
        end; // with
    end;
end;

procedure TsuiMDIForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
    inherited;

    if AComponent = nil then
        Exit;

    if (
        (Operation = opRemove) and
        (AComponent = m_Menu)
    ) then
    begin
        m_Menu := nil;
        UpdateMenu();
    end;

    if (
        (Operation = opRemove) and
        (AComponent = m_MenuBar)
    ) then
    begin
        m_MenuBar := nil;
    end;

    if Operation = opInsert then
    begin
        if m_MenuBar <> nil then
            m_MenuBar.Top := 0;
        if m_TitleBar <> nil then
            m_TitleBar.Top := 0;
    end;

    if (
        (Operation = opRemove) and
        (AComponent = m_TitleBar)
    ) then
    begin
        m_TitleBar := nil;
    end;

    if (
        (Operation = opRemove) and
        (AComponent = m_FileTheme)
    )then
    begin
        m_FileTheme := nil;
        SetUIStyle(SUI_THEME_DEFAULT);
    end;
end;

procedure TsuiMDIForm.OnApplicationMessage(var Msg: TMsg;
  var Handled: Boolean);
var
    AMsg : TMessage;
begin
    if Msg.message = WM_KEYDOWN then
    begin
        AMsg.Msg := Msg.message;
        AMsg.WParam := Msg.wParam;
        AMsg.LParam := Msg.lParam;
        ProcessKeyPress(AMsg);
        Handled := Boolean(AMsg.Result);
    end;
end;

procedure TsuiMDIForm.OnControlButtonClick(Sender: TObject);
begin
    if not (Sender is TsuiToolBarSpeedButton) then
        Exit;
    case (Sender as TsuiToolBarSpeedButton).Tag of

    -1383 : // min
    begin
        ShowWindow(m_Form.ActiveMDIChild.Handle, SW_SHOWMINIMIZED);
    end;

    -1382 : // restore
    begin
        SendMessage(m_Form.ClientHandle, WM_MDIRESTORE, m_Form.ActiveMDIChild.Handle, 0);
    end;

    -1381 : // close
    begin
        m_Form.ActiveMDIChild.Close();
    end;

    end;
end;

procedure TsuiMDIForm.PaintBorder;
var
    Canvas : TCanvas;
begin
    if m_Form = nil then
        Exit;
    Canvas := TCanvas.Create();
    Canvas.Handle := GetWindowDC(m_Form.Handle);
    Canvas.Pen.Color := m_BorderColor;
    Canvas.MoveTo(0, 1);
    Canvas.LineTo(0, m_Form.Height - 1);
    Canvas.LineTo(m_Form.Width - 1, m_Form.Height -1);
    Canvas.LineTo(m_Form.Width - 1, 0);
    ReleaseDC(m_Form.Handle, Canvas.Handle);
    Canvas.Free();
end;

procedure TsuiMDIForm.ProcessKeyPress(var Msg: TMessage);
begin
    if not Assigned(m_TopMenu) then
        Msg.Result := 0
    else if m_TopMenu.IsShortCut(TWMKEY(Msg)) then
        Msg.Result := 1
    else
        Msg.Result := 0;
end;

procedure TsuiMDIForm.SetBorderColor(const Value: TColor);
begin
    m_BorderColor := Value;
    if csDesigning in ComponentState then
        Exit;
    PaintBorder();
end;

procedure TsuiMDIForm.SetCaption(const Value: TCaption);
begin
    m_Form.Caption := Value;
    m_TitleBar.Caption := Value;
    GetTitleBarCaption();
end;

procedure TsuiMDIForm.SetFileTheme(const Value: TsuiFileTheme);
begin
    if m_FileTheme = Value then
        Exit;
    m_FileTheme := Value;
    SetUIStyle(m_UIStyle);
end;

procedure TsuiMDIForm.SetMenu(const Value: TMainMenu);
begin
    if m_Menu <> nil then
    begin
        if m_Menu is TsuiMainMenu then
            (m_Menu as TsuiMainMenu).Form := nil;
    end;
    m_Menu := Value;
    UpdateMenu();
end;

procedure TsuiMDIForm.SetMenuBarColor(const Value: TColor);
begin
    m_MenuBarColor := Value;
    
    if (m_MenuBar = nil) then
        Exit;

    m_MenuBar.Color := Value;
end;

procedure TsuiMDIForm.SetUIStyle(const Value: TsuiUIStyle);
var
    OutUIStyle : TsuiUIStyle;
    i : Integer;
begin
    m_UIStyle := Value;

    if UsingFileTheme(m_FileTheme, m_UIStyle, OutUIStyle) then
    begin
        BorderColor := m_FileTheme.GetColor(SUI_THEME_FORM_BORDER_COLOR);
        MenuBarColor := m_FileTheme.GetColor(SUI_THEME_FORM_BACKGROUND_COLOR);
    end
    else
    begin
        BorderColor := GetInsideThemeColor(OutUIStyle, SUI_THEME_FORM_BORDER_COLOR);
        MenuBarColor := GetInsideThemeColor(OutUIStyle, SUI_THEME_FORM_BACKGROUND_COLOR);
    end;

    if (m_TitleBar = nil) then
        Exit;

    m_TitleBar.FileTheme := m_FileTheme;
    m_TitleBar.UIStyle := m_UIStyle;

    if not (csDesigning in ComponentState) then
    for i := 1 to 3 do
        m_ControlBtns[i].Color := MenuBarColor;

    m_TitleBar.RoundCorner := 0;
end;

procedure TsuiMDIForm.ShowControlButtons;
var
    i : Integer;
    Form : TsuiForm;
begin
    Form := GetActiveChildSUIForm();
    if Form = nil then
        Exit;

    for i := 0 to Form.TitleBarButtons.Count - 1 do
    begin
        if Form.TitleBarButtons.Items[i].ButtonType = suiMin then
            m_ControlBtns[3].Visible := true;
        if Form.TitleBarButtons.Items[i].ButtonType = suiMax then
            m_ControlBtns[2].Visible := true;
        if Form.TitleBarButtons.Items[i].ButtonType = suiClose then
            m_ControlBtns[1].Visible := true;
    end;
end;

procedure TsuiMDIForm.ShowMenuBar;
begin
    if (m_MenuBar = nil) then
        Exit;
    m_TopPanel.Visible := true;
end;

procedure TsuiMDIForm.Tile;
var
    i : Integer;
    nLeft, nTop, nWidth, nHeight : Integer;
    nCount : Integer;
begin
    nCount := 0;
    for i := m_Form.MDIChildCount - 1 downto 0 do
    begin
        if m_Form.MDIChildren[i].WindowState = wsMinimized then
            continue;
        Inc(nCount);
    end;

    if nCount = 0 then
        Exit;

    nHeight := m_Form.ClientHeight - m_MenuBar.Height - m_TitleBar.Height - 4;
    if nCount < m_Form.MDIChildCount then
        Dec(nHeight, 20);

    if m_Form.TileMode = tbVertical	then
    begin
        nLeft := 0;
        nWidth := m_Form.ClientWidth div nCount;
        for i := m_Form.MDIChildCount - 1 downto 0 do
        begin
            if m_Form.MDIChildren[i].WindowState = wsMinimized then
                continue;
            m_Form.MDIChildren[i].SetBounds(nLeft, 0, nWidth, nHeight);
            Inc(nLeft, nWidth - 6);
        end;
    end
    else
    begin
        nTop := 0;
        nHeight := nHeight div nCount;
        nWidth := m_Form.ClientWidth - 4;
        for i := m_Form.MDIChildCount - 1 downto 0 do
        begin
            if m_Form.MDIChildren[i].WindowState = wsMinimized then
                continue;
            m_Form.MDIChildren[i].SetBounds(0, nTop, nWidth, nHeight);
            Inc(nTop, nHeight - 6);
        end;
    end;
end;

procedure TsuiMDIForm.UpdateMenu();
var
    i : Integer;
    Button : TToolButton;
begin
    m_TopMenu := GetMainMenu();

    if m_TopMenu = nil then
    begin
        HideMenuBar();
        Exit;
    end;

    m_TopPanel.Constraints.MinHeight:=m_MenuBar.Height;

    if m_TopMenu.Items.Count<m_MenuBar.ButtonCount then
    for i := m_TopMenu.Items.Count to m_MenuBar.ButtonCount - 1 do
       m_MenuBar.Buttons[0].Free();

    if m_TopMenu.Items.Count>m_MenuBar.ButtonCount then
    for i := m_MenuBar.ButtonCount to  m_TopMenu.Items.Count - 1 do
    begin
        Button := TToolButton.Create(self);
        Button.Parent := m_MenuBar;
        Button.Grouped := true;
        Button.AutoSize := true;
    end;

    for i := m_TopMenu.Items.Count - 1 downto 0 do
        m_MenuBar.Buttons[i].MenuItem := m_TopMenu.Items[i];

    ShowMenuBar();
    m_TopPanel.Constraints.MinHeight:=m_MenuBar.Height;
    m_TopPanel.Height := m_MenuBar.Height;
end;


function TsuiMDIForm.GetDrawAppIcon: Boolean;
begin
    Result := m_TitleBar.DrawAppIcon;
end;

procedure TsuiMDIForm.SetDrawAppIcon(const Value: Boolean);
begin
    m_TitleBar.DrawAppIcon := Value;
end;

function TsuiMDIForm.GetTitleBarCustom: Boolean;
begin
    Result := m_TitleBar.Custom;
end;

procedure TsuiMDIForm.SetTitleBarCustom(const Value: Boolean);
begin
    m_TitleBar.Custom := Value;
end;

function TsuiMDIForm.GetTitleBarVisible: Boolean;
begin
    Result := m_TitleBar.Visible;
end;

procedure TsuiMDIForm.SetTitleBarVisible(const Value: Boolean);
begin
    if m_TitleBar.Visible = Value then
        Exit;
    m_TitleBar.Visible := Value;
    m_TitleBar.Top := 0;
end;

function TsuiMDIForm.GetSections: TsuiTitleBarSections;
begin
    Result := m_TitleBar.Sections;
end;

procedure TsuiMDIForm.SetSections(const Value: TsuiTitleBarSections);
begin
    m_TitleBar.Sections.Assign(Value);
end;

function TsuiMDIForm.GetButtons: TsuiTitleBarButtons;
begin
    Result := m_TitleBar.Buttons;
end;

procedure TsuiMDIForm.SetButtons(const Value: TsuiTitleBarButtons);
begin
    m_TitleBar.Buttons.Assign(Value);
end;

function TsuiMDIForm.GetTitleBarHeight: integer;
begin
  result:=0;
  if m_TitleBar.Visible then result:= result + m_TitleBar.Height;
  if m_TopPanel.Visible then result:= result + m_TopPanel.Height;
end;

{ TsuiScrollBox }

//procedure TsuiScrollBox.CMEnabledChanged(var Msg: TMessage);
//begin
//    inherited;
//    UpdateScrollBars();
//end;
//
//constructor TsuiScrollBox.Create(AOwner: TComponent);
//begin
//    inherited Create(AOwner);
//    ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csSetCaption, csDoubleClicks];
//    Width := 185;
//    Height := 185;
//
//    m_SelfChanging := false;
//    m_MouseDown := false;
//end;
//
//procedure TsuiScrollBox.Notification(AComponent: TComponent;
//  Operation: TOperation);
//begin
//    inherited;
//    if AComponent = nil then
//        Exit;
//
//    if (
//        (Operation = opRemove) and
//        (AComponent = m_VScrollBar)
//    )then
//        m_VScrollBar := nil;
//
//    if (
//        (Operation = opRemove) and
//        (AComponent = m_HScrollBar)
//    )then
//        m_HScrollBar := nil;
//end;
//
//procedure TsuiScrollBox.OnHScrollBarChange(Sender: TObject);
//begin
//    if m_SelfChanging then
//        Exit;
//    SendMessage(Handle, WM_HSCROLL, MakeWParam(SB_THUMBPOSITION, m_HScrollBar.Position), 0);
//    Invalidate;
//end;
//
//procedure TsuiScrollBox.OnVScrollBarChange(Sender: TObject);
//begin
//    if m_SelfChanging then
//        Exit;
//    SendMessage(Handle, WM_VSCROLL, MakeWParam(SB_THUMBPOSITION, m_VScrollBar.Position), 0);
//    Invalidate;
//end;
//
//procedure TsuiScrollBox.SetHScrollBar(const Value: TsuiScrollBar);
//begin
//    if m_HScrollBar = Value then
//        Exit;
//    if m_HScrollBar <> nil then
//    begin
//        m_HScrollBar.OnChange := nil;
//        m_HScrollBar.LineButton := 0;
//        m_HScrollBar.Max := 100;
//        m_HScrollBar.Enabled := true;
//    end;
//
//    m_HScrollBar := Value;
//    if m_HScrollBar = nil then
//        Exit;
//    m_HScrollBar.Orientation := suiHorizontal;
//    m_HScrollBar.OnChange := OnHScrollBArChange;
//    m_HScrollBar.BringToFront();
//
//    UpdateScrollBarsPos();
//end;
//
//procedure TsuiScrollBox.SetVScrollBar(const Value: TsuiScrollBar);
//begin
//    if m_VScrollBar = Value then
//        Exit;
//    if m_VScrollBar <> nil then
//    begin
//        m_VScrollBar.OnChange := nil;
//        m_VScrollBar.LineButton := 0;
//        m_VScrollBar.Max := 100;
//        m_VScrollBar.Enabled := true;
//    end;
//
//    m_VScrollBar := Value;
//    if m_VScrollBar = nil then
//        Exit;
//    m_VScrollBar.Orientation := suiVertical;
//    m_VScrollBar.OnChange := OnVScrollBArChange;
//    m_VScrollBar.BringToFront();
//
//    UpdateScrollBarsPos();
//end;
//
//procedure TsuiScrollBox.UpdateScrollBars;
//var
//    info : tagScrollInfo;
//    barinfo : tagScrollBarInfo;
//begin
//    m_SelfChanging := true;
//    if m_VScrollBar <> nil then
//    begin
//        barinfo.cbSize := SizeOf(barinfo);
//        GetScrollBarInfo(Handle, Integer(OBJID_VSCROLL), barinfo);
//        if (barinfo.rgstate[0] = STATE_SYSTEM_INVISIBLE) or
//           (barinfo.rgstate[0] = STATE_SYSTEM_UNAVAILABLE) then
//        begin
//            m_VScrollBar.LineButton := 0;
//            m_VScrollBar.Enabled := false;
//            m_VScrollBar.Visible := false;
//        end
//        else
//        begin
//            m_VScrollBar.LineButton := abs(barinfo.xyThumbBottom - barinfo.xyThumbTop);
//            m_VScrollBar.Enabled := true;
//            m_VScrollBar.Visible := true;
//        end;
//        info.cbSize := SizeOf(info);
//        info.fMask := SIF_ALL;
//        GetScrollInfo(Handle, SB_VERT, info);
//        m_VScrollBar.Max := info.nMax - Integer(info.nPage) + 1;
//        m_VScrollBar.Min := info.nMin;
//        m_VScrollBar.Position := info.nPos;
//    end;
//
//    if m_HScrollBar <> nil then
//    begin
//        barinfo.cbSize := SizeOf(barinfo);
//        GetScrollBarInfo(Handle, Integer(OBJID_HSCROLL), barinfo);
//        if (barinfo.rgstate[0] = STATE_SYSTEM_INVISIBLE) or
//           (barinfo.rgstate[0] = STATE_SYSTEM_UNAVAILABLE) then
//        begin
//            m_HScrollBar.LineButton := 0;
//            m_HScrollBar.Enabled := false;
//            m_HScrollBar.Visible := false;
//        end
//        else
//        begin
//            m_HScrollBar.LineButton := abs(barinfo.xyThumbBottom - barinfo.xyThumbTop);
//            m_HScrollBar.Enabled := true;
//            m_HScrollBar.Visible := true;
//        end;
//        info.cbSize := SizeOf(info);
//        info.fMask := SIF_ALL;
//        GetScrollInfo(Handle, SB_HORZ, info);
//        m_HScrollBar.Max := info.nMax - Integer(info.nPage) + 1;
//        m_HScrollBar.Min := info.nMin;
//        m_HScrollBar.Position := info.nPos;
//    end;
//
//    m_SelfChanging := false;
//end;
//
//procedure TsuiScrollBox.UpdateScrollBarsPos;
//begin
//    if m_HScrollBar <> nil then
//    begin
//        if m_HScrollBar.Height > Height then
//            m_HScrollBar.Top := Top
//        else
//        begin
//            m_HScrollBar.Left := Left + 1;
//            m_HScrollBar.Top := Top + Height - m_HScrollBar.Height - 10;
//            if m_VScrollBar <> nil then
//            begin
//                if m_VScrollBar.Visible then
//                    m_HScrollBar.Width := Width - 2 - m_VScrollBar.Width
//                else
//                    m_HScrollBar.Width := Width - 2
//            end
//            else
//                m_HScrollBar.Width := Width - 2
//        end;
//    end;
//
//    if m_VScrollBar <> nil then
//    begin
//        if m_VScrollBar.Width > Width then
//            m_VScrollBar.Left := Left
//        else
//        begin
//            m_VScrollBar.Left := Left + Width - m_VScrollBar.Width - 10;
//            m_VScrollBar.Top := Top + 1;
//            if m_HScrollBar <> nil then
//            begin
//                if m_HScrollBar.Visible then
//                    m_VScrollBar.Height := Height - 2 - m_HScrollBar.Height
//                else
//                    m_VScrollBar.Height := Height - 2;
//            end
//            else
//                m_VScrollBar.Height := Height - 2;
//        end;
//    end;
//
//    UpdateScrollBars();
//end;
//
//procedure TsuiScrollBox.WMHSCROLL(var Message: TWMHScroll);
//begin
//    inherited;
//    UpdateScrollBars();
//end;
//
//procedure TsuiScrollBox.WMKeyDown(var Message: TWMKeyDown);
//begin
//    inherited;
//    UpdateScrollBars();
//end;
//
//procedure TsuiScrollBox.WMLBUTTONDOWN(var Message: TMessage);
//begin
//    inherited;
//    m_MouseDown := true;
//    UpdateScrollBars();
//end;
//
//procedure TsuiScrollBox.WMLButtonUp(var Message: TMessage);
//begin
//    inherited;
//    m_MouseDown := false;
//end;
//
//procedure TsuiScrollBox.WMMOUSEMOVE(var Message: TMessage);
//begin
//    inherited;
//    if m_MouseDown then UpdateScrollBars();
//end;
//
//procedure TsuiScrollBox.WMMOUSEWHEEL(var Message: TMessage);
//begin
//    inherited;
//    UpdateScrollBars();
//end;
//
//procedure TsuiScrollBox.WMMOVE(var Msg: TMessage);
//begin
//    inherited;
//    UpdateScrollBarsPos();
//end;
//
//procedure TsuiScrollBox.WMSIZE(var Msg: TMessage);
//begin
//    inherited;
//    UpdateScrollBarsPos();
//end;
//
//procedure TsuiScrollBox.WMVSCROLL(var Message: TWMVScroll);
//begin
//    inherited;
//    UpdateScrollBars();
//end;


{ TsuiMSNPopForm }

procedure TsuiMSNPopForm.Close;
begin
    m_Form.Timer1.Enabled := false;
    m_Form.Hide();
end;

constructor TsuiMSNPopForm.Create(AOwner: TComponent);
begin
    inherited;

    m_AnimateTime := 400;
    m_StayTime := 2000;
    m_X := 0;
    m_Y := 0;
    m_AutoPosition := true;
    m_ClickHide := false;
    m_Text := '';
    m_Title := '';
    m_TitleFont := TFont.Create();
    m_TitleFont.Color := clNavy;
    m_TextFont := TFont.Create();
    m_TextFont.Color := clNavy;
    m_Form := TfrmMSNPopForm.Create(self);
    m_Form.AnimateTime := m_AnimateTime;
    m_Form.Hide();
end;

destructor TsuiMSNPopForm.Destroy;
begin
    m_Form.Free();
    m_Form := nil;
    
    inherited;
end;

procedure TsuiMSNPopForm.Popup;
begin
    m_Form.AnimateTime := m_AnimateTime;
    m_Form.StayTime := m_StayTime;
    m_Form.ClickHide := m_ClickHide;
    m_Form.lbl_title.Caption := m_Title;
    m_Form.lbl_title.Font.Assign(m_TitleFont);
    m_Form.lbl_text.Caption := m_Text;
    m_Form.lbl_text.Font.Assign(m_TextFont);

    if m_AutoPosition then
    begin
        m_Form.Left := GetWorkAreaRect().Right - m_Form.Width - 18;
        m_Form.Top := GetWorkAreaRect().Bottom - m_Form.Height;
    end
    else
    begin
        m_Form.Left := m_X;
        m_Form.Top := m_Y;
    end;

    m_Form.Show();
    m_Form.Timer1.Enabled := true;
end;

procedure TsuiMSNPopForm.SetTextFont(const Value: TFont);
begin
    m_TextFont.Assign(Value);
end;

procedure TsuiMSNPopForm.SetTitleFont(const Value: TFont);
begin
    m_TitleFont.Assign(Value);
end;

end.
