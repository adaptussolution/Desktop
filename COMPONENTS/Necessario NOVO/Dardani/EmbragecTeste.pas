unit EmbragecTeste;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, SapiensBusca, DBXpress, DB, SqlExpr,
  ExtCtrls, Grids, ComCtrls;

type
  TForm1 = class(TForm)
    Q: TEmbQuery;
    BitBtn1: TBitBtn;
    SqlCon: TSQLConnection;
    Grd: TDrawGrid;
    ScrollBox1: TScrollBox;
    Button1: TButton;
    Box01: TScrollBox;
    pnlBase01: TPanel;
    pnl01: TPanel;
    Label1: TLabel;
    lbl01: TLabel;
    Panel1: TPanel;
    Shape1: TShape;
    cbbBase: TComboBox;
    Edit1: TEdit;
    DateTimePicker1: TDateTimePicker;
    cbbEOU: TComboBox;
    ComboBox1: TComboBox;
    Panel2: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure BitBtn1Click(Sender: TObject);
    procedure lbl01MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.BitBtn1Click(Sender: TObject);
begin
	Q.Execute;
end;

procedure TForm1.lbl01MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
	p : TPanel;
  shp : TShape;
  cbb : TComboBox;
  edt : TEdit;
  i, h : Integer;
begin
	p := TPanel.Create(PnlBase01);
	with p do
  begin
		Height := 28;
  	Parent := PnlBase01;
    Align := alTop;
    Caption := '';
    Color := clWindow;
    BevelOuter := bvNone;
  end;
  shp := TShape.Create(p);
  with shp do
  begin
		Height := 1;
    Align := alBottom;
    Pen.Style := psDot;
    Parent := p;
  end;
  cbb := TComboBox.Create(p);
  with cbb do
  begin
    Parent := p;
    Left := 16;
    Top := 4;
    Style := csDropDownList;
    Items.Assign(cbbBase.Items);
    ItemIndex := 0;
    Width := 145;
    Font.Style := Font.Style + [fsBold];
  end;
  edt := TEdit.Create(p);
  with edt do
  begin
  	Left := cbb.Left + cbb.Width + 4;
    Parent := p;
    Text := '0'; // No caso de número
    Top := cbb.Top;
    Width := 168;
  end;
  cbb := TComboBox.Create(p);
  with cbb do
  begin
    Parent := p;
  	Left := edt.Left + edt.Width + 4;
    Top := 4;
    Style := csDropDownList;
    Items.Assign(cbbEOU.Items);
    ItemIndex := 0;
    Width := 50;
    Font.Style := Font.Style + [fsBold];
  end;


  h := 0;
	for i := 0 to PnlBase01.ComponentCount -1 do
  begin
  	if PnlBase01.Components[i] is TControl then
    begin
			h := h + TControl(PnlBase01.Components[i]).Height;
      //TControl(PnlBase01.Components[i]).Align := alTop;
    end;
  end;
  PnlBase01.Height := h;
  PnlBase01.Repaint;
end;

end.
