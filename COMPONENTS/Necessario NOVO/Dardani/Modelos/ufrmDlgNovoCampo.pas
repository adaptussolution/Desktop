unit ufrmDlgNovoCampo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, FolhaModelos, Spin, Buttons;

type
  TfrmDlgNovoCampo = class(TForm)
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
    Label7: TLabel;
    EdtDescricao: TEdit;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure EdtColunaChange(Sender: TObject);
    procedure EdtTamanhoChange(Sender: TObject);
    procedure EdtLinhaKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure cbbCategoriaCampoChange(Sender: TObject);
    procedure cbbCampoChange(Sender: TObject);
    procedure EdtConteudoChange(Sender: TObject);
    procedure cbbBandaChange(Sender: TObject);
  private
    { Private declarations }
    FModelo : TModelo;
  public
    { Public declarations }
    constructor Cria(Modelo: TModelo; ListaCampos: TStrings; AOwner: TComponent);
end;


var
  frmDlgNovoCampo: TfrmDlgNovoCampo;

implementation

{$R *.dfm}

constructor TfrmDlgNovoCampo.Cria(Modelo: TModelo; ListaCampos: TStrings;
  AOwner: TComponent);
begin
	inherited Create(AOwner);
  FModelo := Modelo;
  try
		cbbCampo.Items.Assign(ListaCampos);
    cbbCampo.ItemIndex := 0;
  except
  end;
end;


procedure TfrmDlgNovoCampo.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
	iLinha, iColuna, iTamanho : Integer;
  Controle : TWinControl;
  C : TCampoModelo;

begin
	if ModalResult = mrOk then
  begin
		try
      if (TrimLeft(TrimRight(EdtDescricao.Text)) = '') then
      begin
      	Controle := EdtDescricao;
        Raise Exception.Create('Campo Descrição precisa ser preenchido');
      end;
			iLinha := StrToInt( TrimLeft(TrimRight(EdtLinha.Text)));
      if iLinha < 0 then
      begin
      	Controle := EdtLinha;
        Raise Exception.Create('Campo Linha precisa ser inteiro maior ou igual a zero');
      end;

			iColuna := StrToInt( TrimLeft(TrimRight(EdtColuna.Text)));
      if iColuna < 0 then
      begin
      	Controle := EdtColuna;
        Raise Exception.Create('Campo Coluna precisa ser inteiro maior ou igual a zero');
      end;

			iTamanho := StrToInt( TrimLeft(TrimRight(EdtTamanho.Text)));
      if iColuna < 0 then
      begin
      	Controle := EdtTamanho;
        Raise Exception.Create('Campo Largura precisa ser inteiro maior que zero');
      end;

      C 					:= TCampoModelo.Create(FModelo);
      C.CategoriaCampo 	:= TCategoriaCampo(cbbCategoriaCampo.ItemIndex);
      C.Banda 					:= TBanda(cbbBanda.ItemIndex);
      C.Linha 		:= iLinha;
      C.Coluna 		:= iColuna;
      C.Tamanho 	:= iTamanho;
      C.Nome 			:= EdtDescricao.Text;
      C.Conteudo 	:= EdtConteudo.Text;
      FModelo.AdicionaCampoModelo(C);
      FModelo.SelecionaCampo(C);
    except
    	On E:Exception do
      begin
        MessageDlg(E.Message,mtError,[mbOk],0);
        Controle.SetFocus;
    		SysUtils.Abort;
      end;
    end;
  end;

  Action := caFree;
	frmDlgNovoCampo := nil;
end;

procedure TfrmDlgNovoCampo.EdtColunaChange(Sender: TObject);
var
	C : TCampoModelo;
  iValue : Integer;
begin
{
	if EdtColuna.Text = '' then Exit;

	C := FModelo.GetCampoModeloIndice(CbbListaCampo.ItemIndex);
  if C <> nil then
  begin
  	C.Coluna := StrToIntDef(EdtColuna.Text,0);
    EdtColuna.Text := IntToStr(C.Coluna);
  end;
}
end;

procedure TfrmDlgNovoCampo.EdtTamanhoChange(Sender: TObject);
var
	C : TCampoModelo;
  iValue : Integer;
begin
{
	if EdtTamanho.Text = '' then Exit;

	C := FModelo.GetCampoModeloIndice(CbbListaCampo.ItemIndex);
  if C <> nil then
  begin
  	C.Tamanho := StrToIntDef(EdtTamanho.Text,0);
    EdtTamanho.Text := IntToStr(C.Tamanho);
  end;
}
end;

procedure TfrmDlgNovoCampo.EdtLinhaKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
var
	C : TCampoModelo;
  iValue : Integer;
begin
{
	if (Key = VK_RETURN) then
  begin
    if EdtLinha.Text = '' then Exit;

    C := FModelo.GetCampoModeloIndice(CbbListaCampo.ItemIndex);
    if C <> nil then
    begin
      C.Linha := StrToIntDef(EdtLinha.Text,0);
			EdtLinha.Text := IntToStr(C.Linha);
    end;
  end else
	if (Key = VK_ESCAPE) then
  begin
    C := FModelo.GetCampoModeloIndice(CbbListaCampo.ItemIndex);
    if C <> nil then
    begin
			EdtLinha.Text := IntToStr(C.Linha);
    end;
  end;
}
end;

procedure TfrmDlgNovoCampo.cbbCategoriaCampoChange(Sender: TObject);
var
	C : TCampoModelo;
  iValue : Integer;
  CC : TCategoriaCampo;
begin
	CC := TCategoriaCampo(cbbCategoriaCampo.ItemIndex);
  CbbCampo.Visible := CC = ccDado;

	EdtConteudo.Visible := not CbbCampo.Visible;

{
	C := FModelo.GetCampoModeloIndice(CbbListaCampo.ItemIndex);
  if C <> nil then
  begin
  	C.CategoriaCampo := TCategoriaCampo(cbbCategoriaCampo.ItemIndex);
    CbbCampo.Visible := C.CategoriaCampo = ccDado;
    try
    	CbbCampo.OnChange := nil;
    	CbbCampo.ItemIndex := CbbCampo.Items.IndexOf(C.Conteudo);
    finally
    	CbbCampo.OnChange := cbbCampoChange;
    end;
    EdtConteudo.Visible := not CbbCampo.Visible;
  end;
}
end;

procedure TfrmDlgNovoCampo.cbbCampoChange(Sender: TObject);
var
	C : TCampoModelo;
  iValue : Integer;
begin
{
	C := FModelo.GetCampoModeloIndice(CbbListaCampo.ItemIndex);
  if C <> nil then
  begin
		C.Conteudo := cbbCampo.Items[cbbCampo.ItemIndex];
  end;
}
end;

procedure TfrmDlgNovoCampo.EdtConteudoChange(Sender: TObject);
var
	C : TCampoModelo;
  iValue : Integer;
begin
//	if EdtTamanho.Text = '' then Exit;

{
	C := FModelo.GetCampoModeloIndice(CbbListaCampo.ItemIndex);
  if C <> nil then
  begin
  	C.Conteudo := EdtConteudo.Text;
    EdtConteudo.Text := C.Conteudo;
  end;
}
end;

procedure TfrmDlgNovoCampo.cbbBandaChange(Sender: TObject);
var
	BD : TBanda;
begin
	BD := TBanda(cbbBanda.ItemIndex);
  EdtLinha.Enabled := BD = bdMestre;

  if BD = bdDetalhe then
  	EdtLinha.Text := '0';
end;

end.


MODALIDADE FOLHA = 8
