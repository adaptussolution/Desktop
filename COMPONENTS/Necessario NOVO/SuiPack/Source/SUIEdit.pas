////////////////////////////////////////////////////////////////////////////////
//
//
//  FileName    :   SUIEdit.pas
//  Creator     :   Shen Min
//  Date        :   2002-08-22 V1-V3
//                  2003-06-20 V4
//  Comment     :
//
//  Copyright (c) 2002-2003 Sunisoft
//  http://www.sunisoft.com
//  Email: support@sunisoft.com
//
////////////////////////////////////////////////////////////////////////////////

unit SUIEdit;

interface

{$I SUIPack.inc}

uses Windows, Classes, Controls, StdCtrls, Forms, Graphics, Messages, Mask,
     SysUtils,
     SUIThemes, SUIMgr;

type
    TsuiEdit = class(TCustomEdit)
    private
        m_BorderColor : TColor;
        m_UIStyle : TsuiUIStyle;
        m_FileTheme : TsuiFileTheme;

        procedure SetFileTheme(const Value: TsuiFileTheme);
        procedure SetUIStyle(const Value: TsuiUIStyle);
        procedure SetBorderColor(const Value: TColor);
        procedure WMPAINT(var Msg : TMessage); message WM_PAINT;
        procedure WMEARSEBKGND(var Msg : TMessage); message WM_ERASEBKGND;

    protected
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;    

    public
        constructor Create(AOwner : TComponent); override;

    published
        property FileTheme : TsuiFileTheme read m_FileTheme write SetFileTheme;
        property UIStyle : TsuiUIStyle read m_UIStyle write SetUIStyle;
        property BorderColor : TColor read m_BorderColor write SetBorderColor;

        property Anchors;
        property AutoSelect;
        property AutoSize;
        property BiDiMode;
        property CharCase;
        property Color;
        property Constraints;
        property Ctl3D;
        property DragCursor;
        property DragKind;
        property DragMode;
        property Enabled;
        property Font;
        property HideSelection;
        property ImeMode;
        property ImeName;
        property MaxLength;
        property OEMConvert;
        property ParentBiDiMode;
        property ParentColor;
        property ParentCtl3D;
        property ParentFont;
        property ParentShowHint;
        property PasswordChar;
        property PopupMenu;
        property ReadOnly;
        property ShowHint;
        property TabOrder;
        property TabStop;
        property Text;
        property Visible;
        property OnChange;
        property OnClick;
        property OnDblClick;
        property OnDragDrop;
        property OnDragOver;
        property OnEndDock;
        property OnEndDrag;
        property OnEnter;
        property OnExit;
        property OnKeyDown;
        property OnKeyPress;
        property OnKeyUp;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property OnStartDock;
        property OnStartDrag;
    end;

    TsuiMaskEdit = class(TCustomMaskEdit)
    private
        m_BorderColor : TColor;
        m_UIStyle : TsuiUIStyle;
        m_FileTheme : TsuiFileTheme;

        procedure SetFileTheme(const Value: TsuiFileTheme);
        procedure SetUIStyle(const Value: TsuiUIStyle);
        procedure SetBorderColor(const Value: TColor);
        procedure WMPAINT(var Msg : TMessage); message WM_PAINT;
        procedure WMEARSEBKGND(var Msg : TMessage); message WM_ERASEBKGND;

    protected
        procedure Notification(AComponent: TComponent; Operation: TOperation); override;    

    public
        constructor Create(AOwner : TComponent); override;

    published
        property FileTheme : TsuiFileTheme read m_FileTheme write SetFileTheme;
        property UIStyle : TsuiUIStyle read m_UIStyle write SetUIStyle;
        property BorderColor : TColor read m_BorderColor write SetBorderColor;

        property Anchors;
        property AutoSelect;
        property AutoSize;
        property BiDiMode;
        property BorderStyle;
        property CharCase;
        property Color;
        property Constraints;
        property DragCursor;
        property DragKind;
        property DragMode;
        property Enabled;
        property EditMask;
        property Font;
        property ImeMode;
        property ImeName;
        property MaxLength;
        property ParentBiDiMode;
        property ParentColor;
        property ParentCtl3D;
        property ParentFont;
        property ParentShowHint;
        property PasswordChar;
        property PopupMenu;
        property ReadOnly;
        property ShowHint;
        property TabOrder;
        property TabStop;
        property Text;
        property Visible;
        property OnChange;
        property OnClick;
        property OnDblClick;
        property OnDragDrop;
        property OnDragOver;
        property OnEndDock;
        property OnEndDrag;
        property OnEnter;
        property OnExit;
        property OnKeyDown;
        property OnKeyPress;
        property OnKeyUp;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property OnStartDock;
        property OnStartDrag;

    end;

    TsuiNumberEdit = class(TsuiEdit)
    private
        m_Mask: string;
        m_Value: Real;
        m_AutoSelectSigns: Integer;
        procedure SetValue(Value: Real);
        procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    protected
        procedure CreateParams(var Params: TCreateParams); override;
        procedure DoExit; override;
        procedure DoEnter; override;
        procedure Change; override;
        procedure KeyPress(var Key: Char); override;
        procedure Click; override;
    public
        constructor Create(AOwner: TComponent); override;

    published
        property Mask: string read m_Mask write m_Mask;
        property Value: Real read m_Value write SetValue;
        property AutoSelectSigns: Integer read m_AutoSelectSigns write m_AutoSelectSigns;
        property AutoSelect;
        property AutoSize;
        property BorderStyle;
        property CharCase;
        property Color;
        property Ctl3D;
        property DragCursor;
        property DragMode;
        property Enabled;
        property Font;
        property HideSelection;
        property ImeMode;
        property ImeName;
        property MaxLength;
        property OEMConvert;
        property ParentColor;
        property ParentCtl3D;
        property ParentFont;
        property ParentShowHint;
        property PasswordChar;
        property PopupMenu;
        property ReadOnly;
        property ShowHint;
        property TabOrder;
        property TabStop;
        property Visible;
        property OnChange;
        property OnClick;
        property OnDblClick;
        property OnDragDrop;
        property OnDragOver;
        property OnEndDrag;
        property OnEnter;
        property OnExit;
        property OnKeyDown;
        property OnKeyPress;
        property OnKeyUp;
        property OnMouseDown;
        property OnMouseMove;
        property OnMouseUp;
        property OnStartDrag;
    end;

implementation

uses SUIPublic;

{ TsuiEdit }

constructor TsuiEdit.Create(AOwner: TComponent);
begin
    inherited;

    ControlStyle := ControlStyle + [csOpaque];
    BorderStyle := bsNone;
    BorderWidth := 2;

    UIStyle := GetSUIFormStyle(AOwner);
end;


procedure TsuiEdit.Notification(AComponent: TComponent;
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

procedure TsuiEdit.SetBorderColor(const Value: TColor);
begin
    m_BorderColor := Value;
    Repaint();
end;

procedure TsuiEdit.SetFileTheme(const Value: TsuiFileTheme);
begin
    m_FileTheme := Value;
    SetUIStyle(m_UIStyle);
end;

procedure TsuiEdit.SetUIStyle(const Value: TsuiUIStyle);
var
    OutUIStyle : TsuiUIStyle;
begin
    m_UIStyle := Value;
    if UsingFileTheme(m_FileTheme, m_UIStyle, OutUIStyle) then
        m_BorderColor := m_FileTheme.GetColor(SUI_THEME_CONTROL_BORDER_COLOR)
    else
        m_BorderColor := GetInsideThemeColor(OutUIStyle, SUI_THEME_CONTROL_BORDER_COLOR);
    Repaint();
end;

procedure TsuiEdit.WMEARSEBKGND(var Msg: TMessage);
begin
    inherited;

    DrawControlBorder(self, m_BorderColor, Color);
end;

procedure TsuiEdit.WMPAINT(var Msg: TMessage);
begin
    inherited;

    DrawControlBorder(self, m_BorderColor, Color);
end;

{ TsuiMaskEdit }

constructor TsuiMaskEdit.Create(AOwner: TComponent);
begin
    inherited;

    ControlStyle := ControlStyle + [csOpaque];
    BorderStyle := bsNone;
    BorderWidth := 2;

    UIStyle := GetSUIFormStyle(AOwner);
end;

procedure TsuiMaskEdit.Notification(AComponent: TComponent;
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

procedure TsuiMaskEdit.SetBorderColor(const Value: TColor);
begin
    m_BorderColor := Value;
    Repaint();
end;

procedure TsuiMaskEdit.SetFileTheme(const Value: TsuiFileTheme);
begin
    m_FileTheme := Value;
    SetUIStyle(m_UIStyle);
end;

procedure TsuiMaskEdit.SetUIStyle(const Value: TsuiUIStyle);
var
    OutUIStyle : TsuiUIStyle;
begin
    m_UIStyle := Value;
    if UsingFileTheme(m_FileTheme, m_UIStyle, OutUIStyle) then
        m_BorderColor := m_FileTheme.GetColor(SUI_THEME_CONTROL_BORDER_COLOR)
    else
        m_BorderColor := GetInsideThemeColor(OutUIStyle, SUI_THEME_CONTROL_BORDER_COLOR);
    Repaint();
end;

procedure TsuiMaskEdit.WMEARSEBKGND(var Msg: TMessage);
begin
    inherited;

    DrawControlBorder(self, m_BorderColor, Color);
end;

procedure TsuiMaskEdit.WMPAINT(var Msg: TMessage);
begin
    inherited;
    DrawControlBorder(self, m_BorderColor, Color);
end;

{ TsuiNumberEdit }

procedure TsuiNumberEdit.Change;
begin
    if (Text <> '') and (Text <> '-') then
    begin
        try
            m_Value := StrToFloat(Text);
        except
        on E: EConvertError do
        begin
            SetValue(Value);
            raise;
        end;
        end;
    end;
    inherited;
end;

procedure TsuiNumberEdit.Click;
begin
    inherited;
    DoEnter;
end;

procedure TsuiNumberEdit.CMTextChanged(var Message: TMessage);
begin
    inherited;
    Change();
end;

constructor TsuiNumberEdit.Create(AOwner: TComponent);
begin
    inherited;
    Mask := '0.00';
    Value := 0;
    AutoSelectSigns := 2;
end;

procedure TsuiNumberEdit.CreateParams(var Params: TCreateParams);
begin
    inherited;
    Params.Style := Params.Style + ES_RIGHT;
end;

procedure TsuiNumberEdit.DoEnter;
begin
    inherited;
    if (AutoSelectSigns > 0) and AutoSelect then
    begin
        SelStart := Length(Text) - AutoSelectSigns;
        SelLength := AutoSelectSigns;
    end;
end;

procedure TsuiNumberEdit.DoExit;
begin
    inherited;
    if (Text = '') or (Text = '-') then
        Text := '0';
    SetValue(StrToFloat(Text));
end;

procedure TsuiNumberEdit.KeyPress(var Key: Char);
    function AnsiContainsText(const AText, ASubText: string): Boolean;
    begin
        Result := AnsiPos(AnsiUppercase(ASubText), AnsiUppercase(AText)) > 0;
    end;
var
    IsValidKey: Boolean;
begin
    inherited;
    IsValidKey := (Key in ['0'..'9'])
        or ((AnsiContainsText(Mask, '.')
        and ((Key = DecimalSeparator)
        and not (AnsiContainsText(Text, DecimalSeparator)))))
        or (Ord(Key) = VK_BACK)
        or (AnsiContainsText(Mask, '-')
        and ((GetSelStart = 0)
        and (Key = '-'))
        and not (AnsiContainsText(Text, '-')));
    if not IsValidKey then
    begin
        Beep();
        Abort();
    end;
end;

procedure TsuiNumberEdit.SetValue(Value: Real);
begin
    m_Value := Value;
    Text := FormatFloat(m_Mask, Value);
end;

end.
