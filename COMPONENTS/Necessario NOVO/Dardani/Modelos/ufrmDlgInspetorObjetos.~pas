unit ufrmDlgInspetorObjetos;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FolhaModelos, Spin;

type
  TfrmDlgInspetorObjetos = class(TForm)
    CbbListaCampo: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    EdtLinha: TEdit;
    EdtColuna: TEdit;
    Label3: TLabel;
    EdtTamanho: TEdit;
    Label4: TLabel;
    EdtConteudo: TEdit;
    Label5: TLabel;
    cbbCategoriaCampo: TComboBox;
    cbbCampo: TComboBox;
    Label6: TLabel;
    cbbBanda: TComboBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CbbListaCampoChange(Sender: TObject);
    procedure EdtColunaChange(Sender: TObject);
    procedure EdtTamanhoChange(Sender: TObject);
    procedure cbbCategoriaCampoChange(Sender: TObject);
    procedure cbbCampoChange(Sender: TObject);
    procedure EdtConteudoChange(Sender: TObject);
    procedure EdtLinhaChange(Sender: TObject);
  private
    { Private declarations }
    Changing : Boolean;
    FModelo : TModelo;
    FCampoAtual : TCampoModelo;
  public
    { Public declarations }
    constructor Cria(Modelo: TModelo; AOwner: TComponent);
		procedure prcAtualizaCampo(TheCampoModelo: TCampoModelo);
		procedure prcSelecionaCampo(TheCampoModelo: TCampoModelo);
		procedure RefreshCamposModelo;
end;


var
  frmDlgInspetorObjetos: TfrmDlgInspetorObjetos;

implementation

{$R *.dfm}

constructor TfrmDlgInspetorObjetos.Cria(Modelo: TModelo;
  AOwner: TComponent);
begin
	inherited Create(AOwner);
  Changing := False;
	FModelo := Modelo;
  try
		cbbCampo.Items.Assign(FModelo.ListaCampos);
  except
  end;

  RefreshCamposModelo;
end;

procedure TfrmDlgInspetorObjetos.RefreshCamposModelo;
var
	i : Integer;
  C : TCampoModelo;
begin
  // Criação de campos
  CbbListaCampo.Items.Clear;
  try
		cbbCampo.Items.Assign(FModelo.ListaCampos);
  except
  end;

	for i := 0 to FModelo.Count -1 do
  begin
		C := FModelo.GetCampoModeloIndice(i);
    CbbListaCampo.Items.Add(C.Nome);
  end;
end;

procedure TfrmDlgInspetorObjetos.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
	frmDlgInspetorObjetos := nil;
end;

procedure TfrmDlgInspetorObjetos.CbbListaCampoChange(Sender: TObject);
var
  i : Integer;
begin
	if (CbbListaCampo.Items.Count = 0) or (FModelo.Count = 0) then Exit;

	try
    FCampoAtual := FModelo.GetCampoModeloIndice(CbbListaCampo.ItemIndex);
    if FCampoAtual <> nil then
    begin
			prcAtualizaCampo(FCampoAtual);
      if not FCampoAtual.Selecionado then
        FModelo.SelecionaCampoPorId(FCampoAtual.Id);
    end;
  finally
  end;
end;

procedure TfrmDlgInspetorObjetos.EdtColunaChange(Sender: TObject);
var
  iValue : Integer;
begin
	if EdtColuna.Text = '' then Exit;

  if FCampoAtual <> nil then
  begin
  	FCampoAtual.Coluna := StrToIntDef(EdtColuna.Text,0);
    EdtColuna.Text := IntToStr(FCampoAtual.Coluna);
  end;
end;

procedure TfrmDlgInspetorObjetos.EdtTamanhoChange(Sender: TObject);
var
  iValue : Integer;
begin
	if EdtTamanho.Text = '' then Exit;

  if FCampoAtual <> nil then
  begin
  	FCampoAtual.Tamanho := StrToIntDef(EdtTamanho.Text,0);
    EdtTamanho.Text := IntToStr(FCampoAtual.Tamanho);
  end;
end;

procedure TfrmDlgInspetorObjetos.cbbCategoriaCampoChange(Sender: TObject);
var
  iValue : Integer;
begin
  if FCampoAtual <> nil then
  begin
  	FCampoAtual.CategoriaCampo := TCategoriaCampo(cbbCategoriaCampo.ItemIndex);
    CbbCampo.Visible := FCampoAtual.CategoriaCampo = ccDado;
    try
    	CbbCampo.OnChange := nil;
    	CbbCampo.ItemIndex := CbbCampo.Items.IndexOf(FCampoAtual.Conteudo);
    finally
    	CbbCampo.OnChange := cbbCampoChange;
    end;
    EdtConteudo.Visible := not CbbCampo.Visible;
  end;
end;

procedure TfrmDlgInspetorObjetos.cbbCampoChange(Sender: TObject);
var
  iValue : Integer;
begin
  if FCampoAtual <> nil then
    FCampoAtual.Conteudo := cbbCampo.Items[cbbCampo.ItemIndex];
end;

procedure TfrmDlgInspetorObjetos.EdtConteudoChange(Sender: TObject);
var
  iValue : Integer;
begin
  if FCampoAtual <> nil then
  begin
  	FCampoAtual.Conteudo := EdtConteudo.Text;
    EdtConteudo.Text := FCampoAtual.Conteudo;
  end;
end;

procedure TfrmDlgInspetorObjetos.prcSelecionaCampo(TheCampoModelo: TCampoModelo);
var
	i : Integer;
begin
	if Changing then Exit;

  Changing := True;

  FCampoAtual := nil;
	for i := 0 to FModelo.Count -1 do
  begin
    FCampoAtual := TCampoModelo(FModelo.GetCampoModeloIndice(i));
    if (FCampoAtual = TheCampoModelo)  then
    begin
      if cbbListaCampo.ItemIndex <> i then
      begin
        cbbListaCampo.ItemIndex := i;
        prcAtualizaCampo(FCampoAtual);
      end;
      Break;
    end;
  end;
  Changing := False;
end;

procedure TfrmDlgInspetorObjetos.EdtLinhaChange(Sender: TObject);
var
  iValue : Integer;
begin
	if EdtLinha.Text = '' then Exit;

  if FCampoAtual <> nil then
  begin
  	FCampoAtual.Linha := StrToIntDef(EdtLinha.Text,0);
    EdtLinha.Text := IntToStr(FCampoAtual.Linha);
  end;
end;

procedure TfrmDlgInspetorObjetos.prcAtualizaCampo(
  TheCampoModelo: TCampoModelo);
begin
	try
    EdtLinha.OnChange := nil;
    EdtColuna.OnChange := nil;
    EdtTamanho.OnChange := nil;

    CbbListaCampo.OnChange := nil;
    CbbCategoriaCampo.OnChange := nil;

    EdtColuna.Text := IntToStr(FCampoAtual.Coluna);
    EdtLinha.Text 	:= IntToStr(FCampoAtual.Linha);
    EdtTamanho.Text 	:= IntToStr(FCampoAtual.Tamanho);
    EdtConteudo.Text := FCampoAtual.Conteudo;

    cbbBanda.ItemIndex := Ord(FCampoAtual.Banda);

    cbbCategoriaCampo.ItemIndex := Ord(FCampoAtual.CategoriaCampo);
    CbbCampo.ItemIndex := CbbCampo.Items.IndexOf(FCampoAtual.Conteudo);

    CbbCampo.Visible := FCampoAtual.CategoriaCampo = ccDado;
    EdtConteudo.Visible := not CbbCampo.Visible;
	finally
    CbbListaCampo.OnChange := CbbListaCampoChange;
    CbbCategoriaCampo.OnChange := cbbCategoriaCampoChange;

		EdtLinha.OnChange := EdtLinhaChange;
    EdtColuna.OnChange := EdtColunaChange;
    EdtTamanho.OnChange := EdtTamanhoChange;
	end;
end;

end.
