unit uFrm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, SapiensBusca, DBXpress, DB, SqlExpr, Buttons,
  SapiensCriterios, SapiensTypes;

type
  TForm1 = class(TForm)
    Q: TEmbQuery;
    Button1: TButton;
    SQLConnection1: TSQLConnection;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    EdtInternalName: TEdit;
    EdtDisplayText: TEdit;
    Label2: TLabel;
    Label3: TLabel;
    CG: TEmbCriteriaGroup;
    Mm: TMemo;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure BitBtn5Click(Sender: TObject);
    procedure prcDeletaPanel(Panel : TPanel);
    procedure BitBtn6Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
    procedure prcPopulaItemsComboEOU(Cbb: TComboBox);
    procedure prcPopulaItemsComboOperadores(Cbb: TComboBox);
		procedure prcAdicionaCriterio(Container : TWinControl);
    procedure VirtualIncluirCriterioClick(Sender: TObject);
    procedure VirtualExcluirCriterioClick(Sender: TObject);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
	if Q.Execute then
end;

procedure TForm1.prcPopulaItemsComboEOU(Cbb: TComboBox);
begin
	Cbb.Items.Add('OU');
	Cbb.Items.Add('E');
end;

procedure TForm1.prcPopulaItemsComboOperadores(Cbb: TComboBox);
begin
	Cbb.Items.Add('igual a');
	Cbb.Items.Add('diferente de');
	Cbb.Items.Add('menor');
	Cbb.Items.Add('menor ou igual a');
	Cbb.Items.Add('maior que');
	Cbb.Items.Add('maior ou igual a');
	Cbb.Items.Add('iniciando com');
	Cbb.Items.Add('contendo');
end;

procedure TForm1.BitBtn5Click(Sender: TObject);
var
	PnlCampo, PnlTituloCampo : TPanel;
  lblTituloCampo : TLabel;
  ScrBoxCriterios : TScrollBox;
  CbxEOUCriterio : TComboBox;
  BtInsCriterio : TBitBtn;
begin
	PnlCampo := TPanel.Create(Self);
	with PnlCampo do
  begin
    Top := 150;
    Left := 250;
  	Parent := Self;
    Width := 440;
    Height := 200;
    Caption := '';
    BevelOuter := bvNone;
    Color := clYellow;
  end;
  PnlTituloCampo := TPanel.Create(PnlCampo);
  with PnlTituloCampo do
  begin
  	Parent := PnlCampo;
    Height := 24;
    Align := alTop;
    BevelOuter := bvNone;
  end;
  lblTituloCampo := TLabel.Create(PnlTituloCampo);
  with lblTituloCampo do
  begin
		Parent := PnlTituloCampo;
    Align := alClient;
    Font.Color := clWhite;
    Color := clNavy;
    Caption := '   Nome do Servidor';
    LayOut := tlCenter;
  end;
  CbxEOUCriterio := TComboBox.Create(PnlTituloCampo);
  with CbxEOUCriterio do
  begin
  	Parent := PnlTituloCampo;
    Left := 288;
    Top := 1;
    Style := csDropDownList;
    Width := 41;
    PRCPopulaItemsComboEOU(CbxEOUCriterio);
    ItemIndex := 0;
  end;
  BtInsCriterio := TBitBtn.Create(PnlTituloCampo);
  with BtInsCriterio do
  begin
  	Parent := PnlTituloCampo;
    Top := 1;
    Left := 332;
    Height := 22;
    Width := 80;
    Caption := 'Incluir Critério';
    OnClick := VirtualIncluirCriterioClick;
  end;

  ScrBoxCriterios := TScrollBox.Create(PnlCampo);
  with ScrBoxCriterios do
  begin
  	Parent := PnlCampo;
  	Align := alClient;
    // BorderStyle := bsNone;
    // BevelOuter := bvNone;
  end;
end;

procedure TForm1.VirtualIncluirCriterioClick(Sender: TObject);
var
	Pnl : TPanel;
  ScBox : TScrollBox;
  i : Integer;
begin
	// Sender - TBitBtn
	if not (Sender is TBitBtn) then
  	Exit;
  // Pai - Título do campo
	if not ((Sender as TBitBtn).Parent is TPanel) then
  	Exit;
	// Avô - conteiner do campo
	if not ( ((Sender as TBitBtn).Parent as Tpanel).Parent is TPanel) then
  	Exit;

	Pnl := ( ((Sender as TBitBtn).Parent as TPanel).Parent as TPanel);

	// Em busca do scrollbox
  if (Pnl <> nil) then
  begin
    for i := 0 to Pnl.ComponentCount -1  do
    begin
      if Pnl.Components[i] is TScrollBox then
      begin
      	ScBox := (Pnl.Components[i] as TScrollBox);
        //(Pnl.Components[i] as TScrollBox).Color := RGB(Random(255),Random(255),Random(255));
        //ScBox.Color := RGB(Random(255),Random(255),Random(255));
        prcAdicionaCriterio(ScBox);
        Break;
      end;
    end;
  end;
end;

procedure TForm1.prcAdicionaCriterio(Container: TWinControl);
var
  Pnl : TPanel;
  Cbx : TComboBox;
  BtExcCriterio : TBitBtn;
  EdtValue : TEdit;
  Shp : TShape;
  i : Integer;
begin
	Pnl := TPanel.Create(Container);
  for i := 0 to Container.ComponentCount -1 do
  begin
    if Container.Components[i] is TWinControl then
    	(Container.Components[i] as TWinControl).Align := alNone;
  end;

	with Pnl do
  begin
  	Parent := Container;
    Height := 28;
    Align := alTop;
    Color := RGB(Random(255),Random(255),Random(255));
    BevelOuter := bvNone;
  end;

  Cbx := TComboBox.Create(Pnl);
  with Cbx do
  begin
  	Parent 		:= Pnl;
    Left 			:= 12;
    Top 			:= 3;
    Style 		:= csDropDownList;
    Width 		:= 145;
    prcPopulaItemsComboOperadores(Cbx);
    ItemIndex := 0;
  end;

  EdtValue := TEdit.Create(Pnl);
  with EdtValue do
  begin
  	Parent 	:= Pnl;
    Left 		:= 160;
    Top 		:= 3;
    Width 	:= 121;
    Text 		:= '';
  end;

  Cbx := TComboBox.Create(Pnl);
  with Cbx do
  begin
  	Parent 		:= Pnl;
    Left 			:= 288;
    Top 			:= 3;
    Style 		:= csDropDownList;
    Width 		:= 41;
    prcPopulaItemsComboEOU(Cbx);
    ItemIndex := 0;
  end;
  BtExcCriterio := TBitBtn.Create(Pnl);
  with BtExcCriterio do
  begin
  	Parent := Pnl;
    Top := 3;
    Left := 332;
    Height := 22;
    Width := 80;
    Caption := 'Excluir Critério';
    OnClick := VirtualExcluirCriterioClick;
  end;
  Shp := TShape.Create(Pnl);
  with Shp do
  begin
  	Parent := Pnl;
  	Height := 1;
    Pen.Style := psDot;
    Pen.Color := clBtnShadow;
    Align := alBottom;
  end;

	// Reposiciona para corrigir o fato do Windows colocar o último container incluído como top
  for i := Container.ComponentCount -1 downto 0 do
  begin
    if Container.Components[i] is TWinControl then
    	(Container.Components[i] as TWinControl).Align := alTop;
  end;

end;

procedure TForm1.VirtualExcluirCriterioClick(Sender: TObject);
var
	ScrollBox : TScrollBox;
  ScBox : TScrollBox;
  Pnl : TPanel;
  i : Integer;
begin
	// Sender - TBitBtn
	if not (Sender is TBitBtn) then
  	Exit;
  // Pai - Título do campo
	if not ((Sender as TBitBtn).Parent is TPanel) then
  	Exit;
	if not ( ((Sender as TBitBtn).Parent as Tpanel).Parent is TScrollBox) then
  	Exit;
	ScBox := ( ((Sender as TBitBtn).Parent as Tpanel).Parent as TScrollBox);

	(Sender as TBitBtn).OnClick := nil;
	Pnl := ((Sender as TBitBtn).Parent as TPanel);
  Pnl.Visible := False;
end;

procedure TForm1.prcDeletaPanel(Panel: TPanel);
var
	i : Integer;
begin
	try
		while Panel.ComponentCount > 0 do
    	Panel.Components[0].Free;
  	FreeAndNil(Panel);
  except
  end;
end;


procedure TForm1.BitBtn6Click(Sender: TObject);
var
	CF : TEmbCriteriaField;
begin
	try
		CF :=  TEmbCriteriaField.Create(EdtInternalName.Text,EdtDisplayText.Text, SapiensTypes.ctString, CG);
		CG.Add(CF);
    //CG.Left := CG.Left + 50;
  except
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
	Mm.Lines.Clear;
  Mm.Lines.Text := CG.GetExpression;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
	Q.PrepareCriteriaGroup;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
	Mm.Lines.Clear;
  Mm.Lines.Assign(Q.Expression);
end;

end.
