unit Localiza;

interface

uses
  SysUtils, Classes, Graphics, Controls, ExtCtrls, StdCtrls, Buttons;

type
  TLocaliza = class(TCustomPanel)
  private
    { Private declarations }
    FEdit : TEdit;
    FButton : TSpeedButton;
    //FWidth : Integer;
    FEditColor : TColor;
    procedure RedrawComponents;
    procedure SetWidth(Value: Integer);
    function GetWidth : Integer;
    function GetEditColor : TColor;
    procedure SetEditColor(Value: TColor);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    destructor Destroy; override;
    procedure Repaint; override;
  published
    { Published declarations }

    //property Width :Integer read FWidth write SetWidth;

    property EditColor : TColor read GetEditColor write SetEditColor;

    property Align;
    property Alignment;
    property BevelInner;
    property BevelOuter;
    property BorderWidth;
    property BorderStyle;
    property DragCursor;
    property DragMode;
    property TabOrder;
    property OnClick;
    property OnDblClick;
    property OnExit;
    property OnResize;
    property Enabled;
    property Visible;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Embragec', [TLocaliza]);
end;

{ TLocaliza }

constructor TLocaliza.Create(Owner: TComponent);
begin
  inherited;
  with Self do
  begin
  	Caption := ' ';
  	Width := 305;
  	Height := 28;
    BevelInner := bvNone;
    BevelOuter := bvNone;
  end;

  {
  FPnlButton := TPanel.Create(Self);
  with FPnlButton do
  begin
  	Caption := ' ';
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Width := 28;
    Align := alRight;
  	Parent := Self;
  end;
  }
  FButton := TSpeedButton.Create(Self);
  with FButton do
  begin
  	Caption := '';
    //Width := FPnlButton.Width - 6;
    //Height := FPnlButton.Height - 6;
    Flat := True;
  	Parent := Self;
  end;


  {
  FPnlEdit := TPanel.Create(Self);
  with FPnlEdit do
  begin
  	Caption := ' ';
    BevelInner := bvNone;
    BevelOuter := bvNone;
    Align := alClient;
  	Parent := Self;
  end;
  }

  FEdit := TEdit.Create(Self);
  with FEdit do
  begin
  	Left := 2;
    Top := 2;
    //Width := FPnlEdit.Width - 4;
    //Height := FPnlEdit.Height - 4;
  	Parent := Self;
    FEditColor := FEdit.Color;
    Text := '';
  end;
  RedrawComponents;
end;

procedure TLocaliza.RedrawComponents;
begin
	//FPnlEdit.Width := Self.Width - FPnlButton.Width;
  //FPnlEdit.Height := Self.Height;


  FButton.Top := 2;
  FButton.Height := Self.Height - 6;
  FButton.Width := 28;
  FButton.Left := Self.Width - FButton.Width - 6;

  FEdit.Top 		:= 2;
  FEdit.Left 		:= 2;
  FEdit.Width 	:= Self.Width - FButton.Width - 10;
  FEdit.Height 	:= Self.Height - 4;
end;

destructor TLocaliza.Destroy;
begin
  if FEdit <> nil then FEdit.Free;
  if FButton <> nil then FButton.Free;
  inherited;
end;

procedure TLocaliza.SetWidth(Value: Integer);
begin
{
	if Value <> FWidth then
  begin
  	FWidth := Value;
    Self.Width := Value;
    RedrawComponents;
  end;
}
end;

function TLocaliza.GetWidth: Integer;
begin
//	Result := FWidth;
	Result := 0;
end;

function TLocaliza.GetEditColor: TColor;
begin
	Result := FEditColor;
end;

procedure TLocaliza.SetEditColor(Value: TColor);
begin
	if Value <> FEditColor then
  begin
  	FEditColor := Value;
  	FEdit.Color := Value;
  end;
end;

procedure TLocaliza.Repaint;
begin
	RedrawComponents;
  inherited;
end;

end.
