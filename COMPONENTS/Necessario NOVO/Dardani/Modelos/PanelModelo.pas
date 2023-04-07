unit PanelModelo;

interface

uses
  SysUtils, Classes, Controls, ExtCtrls, Graphics, Types, FolhaModelos,
  Dialogs, Windows, ufrmDlgInspetorObjetos, ufrmDlgNovoCampo;

type
  TPanelModelo = class(TCustomPanel)
  private
    { Private declarations }
    FLinhas					: Integer;
  	FColunas 				: Integer;
  	FPrimeiraLinhaDetalhe 	: Integer;
  	FLinhasDetalhe 	: Integer;

  	FReguaTopo 			: Integer;
  	FReguaEsquerda 	: Integer;

    FImgMestre : TImage;
    procedure DesenhaFundo;
    procedure SetColunas(Value : Integer);
    procedure SetLinhas(Value: Integer);
  	procedure SetPrimeiraLinhaDetalhe(Value : Integer);
  	procedure SetLinhasDetalhe(Value : Integer);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    destructor Destroy;
  published
    { Published declarations }
    property Colunas : Integer read FColunas write SetColunas;
    property Linhas : Integer read FLinhas write SetLinhas;

    property PrimeiraLinhaDetalhe : Integer read FPrimeiraLinhaDetalhe write SetPrimeiraLinhaDetalhe;
    property LinhasDetalhe : Integer read FLinhasDetalhe write SetLinhasDetalhe;
  end;

  TModeloControle = class
  private
  	FModelo : TModelo;
    FVisao : TPanelModelo;
    procedure RemoveCampo(TheCampoModelo: TCampoModelo);
    procedure CampoChange(TheCampoModelo: TCampoModelo);
    procedure CampoSelect(TheCampoModelo: TCampoModelo);
    procedure CampoCreate(TheCampoModelo: TCampoModelo);
    procedure CampoRemove(TheCampoModelo: TCampoModelo);
  public
		procedure processaKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
		procedure setModelo(Value: TModelo);
    function getModelo : TModelo;
    procedure setVisao(Value : TPanelModelo);
    function getVisao : TPanelModelo;

    procedure NovoCampoWizard;
    procedure ShowInspector;
    constructor Create(Modelo: TModelo; Visao: TPanelModelo);
  end;

  TImageCampo = class(TImage)
  private
  	FCampoModelo : TCampoModelo;
    procedure ImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  public
		constructor Create(AOwner: TComponent); overload;
    destructor Destroy; overload;
    procedure setCampoModelo(Value: TCampoModelo);
    function getCampoModelo : TCampoModelo;
    procedure Redesenha;
  end;

procedure Register;

implementation

const
	ALTURA_CARACTER = 14; //14
  LARGURA_CARACTER = 7; //7
  REGUA_TOPO 			= (2 * ALTURA_CARACTER) + 3;
  REGUA_ESQUERDA 	= (2 * LARGURA_CARACTER) + 3;

procedure Register;
begin
  RegisterComponents('Embragec', [TPanelModelo]);
end;

{ TPanelModelo }

constructor TPanelModelo.Create(Owner: TComponent);
begin
	inherited;
  FImgMestre := TImage.Create(Self);
  FImgMestre.Parent := Self;

  FPrimeiraLinhaDetalhe := 7;
  FLinhasDetalhe 	:= 10;

  FLinhas := 33;
  FColunas := 80;

  DesenhaFundo;

end;

procedure TPanelModelo.DesenhaFundo;
var
	lin, col : Integer;
  sTexto : String;
  Bmp : Graphics.TBitmap;
begin
  try
		Bmp := Graphics.TBitmap.Create;

    Bmp.Width 	:= (FColunas * LARGURA_CARACTER) + REGUA_ESQUERDA;
    Bmp.Height 	:= (FLinhas * ALTURA_CARACTER) + REGUA_TOPO;

		// Área de Detalhe
    Bmp.Canvas.Brush.Color := clGray; //$00E9D2FF; //clGray;
    Bmp.Canvas.Pen.Color := clGray; //$00E9D2FF; //clGray;
    Bmp.Canvas.Rectangle(REGUA_ESQUERDA,
                         REGUA_TOPO + (FPrimeiraLinhaDetalhe * ALTURA_CARACTER),
                         Bmp.Width,
                         REGUA_TOPO + (FPrimeiraLinhaDetalhe * ALTURA_CARACTER) + (FLinhasDetalhe * ALTURA_CARACTER));

    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.Pen.Style := psDot;
    Bmp.Canvas.Font.Name := 'courier new'; //
    Bmp.Canvas.Font.Color := clGray;
    Bmp.Canvas.Font.Size := 8;

		// Cor da linha inicial
    Bmp.Canvas.Pen.Color := clBlue; //$00FF8080; // Lilaz
    // Linha de delimitação da régua topo
    Bmp.Canvas.MoveTo( REGUA_ESQUERDA, 0 + REGUA_TOPO);
    Bmp.Canvas.LineTo( REGUA_ESQUERDA, Bmp.Height);

    // Linha de delimitação da régua esquerda
    Bmp.Canvas.MoveTo( REGUA_ESQUERDA, REGUA_TOPO);
    Bmp.Canvas.LineTo( Bmp.Width, REGUA_TOPO);

    Bmp.Canvas.Brush.Color := clWhite;
    Bmp.Canvas.Pen.Color := clBtnFace;
    //Bmp.Canvas.Pen.Style := psSolid;

    Bmp.Canvas.Brush.Style := bsClear;
    Bmp.Canvas.Font.Color := clRed;

		for col := 1 to FColunas do
    begin
			// Primeira linha da régua (dezenas)
      if (col-1) mod 10 = 0 then
      	sTexto := IntToStr((col-1) div 10)
      else
      	sTexto := '.';


      Bmp.Canvas.TextOut( ( (col-1) * LARGURA_CARACTER) + REGUA_ESQUERDA, 0, sTexto);

			// Segunda linha da régua (unidades)
      sTexto := IntToStr( (col-1) mod 10);
      Bmp.Canvas.TextOut( ( (col-1) * LARGURA_CARACTER) + REGUA_ESQUERDA, ALTURA_CARACTER, sTexto);

      // Colunas
      if col mod 10 = 0 then
      	Bmp.Canvas.Pen.Style := psSolid
      else
      	Bmp.Canvas.Pen.Style := psDot;

      Bmp.Canvas.MoveTo( (col*LARGURA_CARACTER) + REGUA_ESQUERDA, REGUA_TOPO);
      Bmp.Canvas.LineTo( (col*LARGURA_CARACTER) + REGUA_ESQUERDA, Bmp.Height);
    end;

		for lin := 1 to FLinhas do
    begin
			// Primeira linha da régua (dezenas)
      sTexto := IntToStr((lin-1) div 10);
      Bmp.Canvas.TextOut( 0, ( (lin-1) * ALTURA_CARACTER) + REGUA_TOPO, sTexto);

			// Segunda linha da régua (unidades)
      sTexto := IntToStr( (lin-1) mod 10);
      Bmp.Canvas.TextOut( LARGURA_CARACTER, ( (lin-1) * ALTURA_CARACTER) + REGUA_TOPO, sTexto);

      if lin mod 10 = 0 then
      	Bmp.Canvas.Pen.Style := psSolid
      else
      	Bmp.Canvas.Pen.Style := psDot;

      Bmp.Canvas.MoveTo( REGUA_ESQUERDA, (lin * ALTURA_CARACTER) + REGUA_TOPO);
      Bmp.Canvas.LineTo( Bmp.Width, (lin * ALTURA_CARACTER) + REGUA_TOPO);
    end;

    FImgMestre.Top := 0;
    FImgMestre.Left := 0;
    FImgMestre.Width := Bmp.Width;
    FImgMestre.Height := Bmp.Height;
    Self.Width := Bmp.Width;
    Self.Height := Bmp.Height;

    FImgMestre.Picture.Bitmap.Assign(Bmp);
	finally
  	Bmp.Free;
  end;
end;

destructor TPanelModelo.Destroy;
begin
	FreeAndNil(FImgMestre);
	inherited;
end;

procedure TPanelModelo.SetColunas(Value: Integer);
begin
	if Value <> FColunas then
  begin
  	FColunas := Value;
    DesenhaFundo;
  end;
end;

procedure TPanelModelo.SetPrimeiraLinhaDetalhe(Value: Integer);
begin
	if Value <> FPrimeiraLinhaDetalhe then
  begin
  	FPrimeiraLinhaDetalhe := Value;
    DesenhaFundo;
  end;
end;

procedure TPanelModelo.SetLinhas(Value: Integer);
begin
	if Value <> FLinhas then
  begin
  	FLinhas := Value;
    DesenhaFundo;
  end;
end;

procedure TPanelModelo.SetLinhasDetalhe(Value: Integer);
begin
	if Value <> FLinhasDetalhe then
  begin
  	FLinhasDetalhe := Value;
    DesenhaFundo;
  end;
end;

{ TModeloControle }

procedure TModeloControle.CampoChange(TheCampoModelo: TCampoModelo);
var
	i : Integer;
  Img : TImageCampo;
begin
	if Self.FVisao = nil then Exit;
	for i := 0 to Self.FVisao.ComponentCount -1 do
  begin
  	if (Self.FVisao.Components[i] is TImageCampo) and
    	 (TImageCampo(Self.FVisao.Components[i]).getCampoModelo = TheCampoModelo)
    then
    begin
    	Img := TImageCampo(Self.FVisao.Components[i]);
      Img.Redesenha;

      if TheCampoModelo.Selecionado and (frmDlgInspetorObjetos <> nil) then
        frmDlgInspetorObjetos.prcAtualizaCampo(TheCampoModelo);
    	Break;
    end;
  end;
end;

procedure TModeloControle.CampoSelect(TheCampoModelo: TCampoModelo);
var
	i : Integer;
  Img : TImageCampo;
begin
  if (frmDlgInspetorObjetos <> nil) then
  	frmDlgInspetorObjetos.prcSelecionaCampo(TheCampoModelo);

	// NÃO IMPLEMENTADO - ANALISAR

	// Mostra visualmente qual o campo selecionado
	if Self.FModelo = nil then Exit;
	for i := 0 to Self.FVisao.ComponentCount -1 {Self.FModelo.Count -1} do
  begin
  	if (Self.FVisao.Components[i] is TImageCampo) and
    	 (TImageCampo(Self.FVisao.Components[i]).getCampoModelo = TheCampoModelo)
    then
    begin
    	Img := TImageCampo(Self.FVisao.Components[i]);
      Img.Redesenha;
    	Break;
    end;
  end;
end;

procedure TModeloControle.RemoveCampo(TheCampoModelo: TCampoModelo);
begin
  // Remove campo do modelo. O modelo vai notificar que removeu campo
  FModelo.RemoveCampo(TheCampoModelo);
  if FModelo.Count > 0 then
  begin
    if (frmDlgInspetorObjetos <> nil) then
  		frmDlgInspetorObjetos.RefreshCamposModelo;

		FModelo.SelecionaCampoPorId(FModelo.GetCampoModeloIndice(0).Id);
  end;
end;

function TModeloControle.getModelo: TModelo;
begin
	Result := FModelo;
end;

function TModeloControle.getVisao: TPanelModelo;
begin
	Result := FVisao;
end;

procedure TModeloControle.processaKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
	C : TCampoModelo;
	iTag : Integer;
begin
  C := FModelo.GetCampoSelecionado;
  if (C <> nil) then
  begin
    if (Key = Windows.VK_DOWN) and (ssCtrl in Shift) then
    begin
      C.Linha := C.Linha +1;
    end else
    if (Key = VK_UP) and (ssCtrl in Shift) then
    begin
      C.Linha := C.Linha -1;
    end else
    if (Key = VK_LEFT) and (ssCtrl in Shift)  then
    begin
      C.Coluna := C.Coluna -1;
    end else
    if (Key = VK_RIGHT) and (ssCtrl in Shift) then
    begin
      C.Coluna := C.Coluna +1;
    end else
    if (Key = VK_RIGHT) and (ssShift in Shift) then
    begin
      C.Tamanho := C.Tamanho +1;
    end else
    if (Key = VK_LEFT) and (ssShift in Shift) then
    begin
      C.Tamanho := C.Tamanho -1;
    end else
    if (Key = VK_DELETE) then
    begin
      RemoveCampo(C);
    end;
  end;
end;

procedure TModeloControle.setModelo(Value: TModelo);
begin
	if Value <> FModelo then
  begin
  	FModelo := Value;
    FModelo.OnCampoCreate := CampoCreate;
    FModelo.OnCampoRemove := CampoRemove;
    FModelo.OnCampoSelect := CampoSelect;
    FModelo.OnCampoChange := CampoChange;
  end;
end;

procedure TModeloControle.setVisao(Value: TPanelModelo);
begin
	if Value <> FVisao then
  begin
  	FVisao := Value;
  	FVisao.Colunas 				:= FModelo.Colunas;
    FVisao.FLinhas 				:= FModelo.Linhas;   
  	FVisao.PrimeiraLinhaDetalhe 	:= FModelo.PrimeiraLinhaDetalhe;
  	FVisao.LinhasDetalhe 	:= FModelo.QuantidadeDetalhes;
  end;
end;

procedure TModeloControle.CampoCreate(TheCampoModelo: TCampoModelo);
var
	Img : TImageCampo;
begin
// Cria imagem do campo
	if Self.FVisao <> nil then
  begin
    TheCampoModelo.OnCampoChange := CampoChange;
    Img := TImageCampo.Create(FVisao);
    Img.Parent := FVisao;
    Img.Tag := TheCampoModelo.Id;
    Img.setCampoModelo(TheCampoModelo);
    Img.Redesenha;
	end;
  if (frmDlgInspetorObjetos <> nil) then
  	frmDlgInspetorObjetos.RefreshCamposModelo;
end;

procedure TModeloControle.CampoRemove(TheCampoModelo: TCampoModelo);
var
	i : Integer;
  Img : TImageCampo;
begin
	if Self.FVisao = nil then Exit;

	for i := 0 to Self.FVisao.ComponentCount -1 do
  begin
  	if (Self.FVisao.Components[i] is TImageCampo) and
    	 (TImageCampo(Self.FVisao.Components[i]).getCampoModelo = TheCampoModelo)
    then
    begin
    	Img := TImageCampo(Self.FVisao.Components[i]);
      FreeAndNil(Img);
    	Break;
    end;
  end;
end;

constructor TModeloControle.Create(Modelo: TModelo; Visao: TPanelModelo);
begin
	setModelo(Modelo);
  setVisao(Visao);

  {
  frmDlgInspetorObjetos := TfrmDlgInspetorObjetos.Cria(Modelo, nil);
  frmDlgInspetorObjetos.Show;
  }
end;

procedure TModeloControle.NovoCampoWizard;
begin
	if Self.FModelo <> nil then
  begin
    try
      frmDlgNovoCampo := TfrmDlgNovoCampo.Cria(FModelo, FModelo.ListaCampos, nil);
      frmDlgNovoCampo.ShowModal;
    finally
      FreeAndNil(frmDlgNovoCampo);
    end;
	end;
end;

procedure TModeloControle.ShowInspector;
begin
	if frmDlgInspetorObjetos = nil then
  begin
  	frmDlgInspetorObjetos := TfrmDlgInspetorObjetos.Cria(Self.FModelo, nil);
  	frmDlgInspetorObjetos.Show;
  end else
  begin
  	frmDlgInspetorObjetos.Show;
    frmDlgInspetorObjetos.BringToFront;
  end;
end;

{ TImageCampo }

constructor TImageCampo.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	Self.OnMouseDown := ImageMouseDown;
end;

destructor TImageCampo.Destroy;
begin
	Self.FCampoModelo := nil;
  if frmDlgInspetorObjetos <> nil then
  begin
  	frmDlgInspetorObjetos.Close;
    FreeAndNil(frmDlgInspetorObjetos);
  end;

	inherited;
end;

function TImageCampo.getCampoModelo: TCampoModelo;
begin
	Result := FCampoModelo;
end;

procedure TImageCampo.ImageMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
	//	Seleciona
	if Self.FCampoModelo <> nil then
    Self.FCampoModelo.Modelo.SelecionaCampoPorId( Self.FCampoModelo.Id );
end;

procedure TImageCampo.Redesenha;
var
	R : TRect;
  Bmp : Graphics.TBitmap;
begin
	if Self.FCampoModelo = nil then Exit;

  try
		Bmp := Graphics.TBitmap.Create;

    Height 	:= ALTURA_CARACTER;
		Top 		:= FCampoModelo.Modelo.ReguaTopo + (FCampoModelo.Linha * ALTURA_CARACTER);
    Left 		:= FCampoModelo.Modelo.ReguaEsquerda + (FCampoModelo.Coluna * LARGURA_CARACTER);
    Width 	:= FCampoModelo.Tamanho * LARGURA_CARACTER;

    Bmp.Width := Width;
    Bmp.Height := Height;

    if FCampoModelo.Selecionado then
    begin
      BringToFront;

      Bmp.Canvas.Brush.Color := clYellow;
      Bmp.Canvas.Font.Color := clBlack;
      Bmp.Canvas.Font.Style := [fsBold];
    end else
    begin
      Bmp.Canvas.Brush.Color := clBtnFace;
      Bmp.Canvas.Font.Color := clBlack;
      Bmp.Canvas.Font.Style := [];
    end;

    Bmp.Canvas.Font.Name := 'Courier New';
    Bmp.Canvas.Font.Size := 8;
    Bmp.Canvas.FillRect( Rect(0,0,Width,Height) );
    Bmp.Canvas.TextOut(1,1,FCampoModelo.Conteudo);
    Picture.Bitmap.Assign(Bmp);
	finally
  	Bmp.Free;
  end;
end;

procedure TImageCampo.setCampoModelo(Value: TCampoModelo);
begin
	if Value <> FCampoModelo then
		FCampoModelo := Value;
end;

end.



// Vídeo Pai e Filha
http://br.youtube.com/watch?v=RvmTsH4iHBo&NR=1
// Sombra com as mãos
http://br.youtube.com/watch?v=8cLcjgv_2d0&feature=dir
// Dança das sombras
http://br.youtube.com/watch?v=tn-sV65kS54&mode=related&search=
// Monalisa
http://br.youtube.com/watch?v=rH7OmG2aUL4



procedure TfrmDlgModeloMontagemVisual.AdicionaCampo(sDescricao,
  sConteudo: String; iLinha, iColuna, iTamanho: Integer; BD: TBanda;
  CC: TCategoriaCampo);
var
  Img : TImage;
	C : TCampoModelo;
begin
	C := TCampoModelo.Create(FModelo);
  C.Id := FModelo.Count;
  C.Linha := iLinha;
  C.Coluna := iColuna;
  C.Tamanho := iTamanho;
  C.Banda := BD;
	C.Conteudo := sConteudo;
  C.CategoriaCampo := CC;
  C.Nome := sDescricao;

  FModelo.AdicionaCampoModelo(C);;

  Img := TImage.Create(PnlMestre);
  Img.Parent := PnlMestre;

  Img.Tag := C.Id;
  Img.OnMouseDown := ShapeMouseDown;
  //C.Image := Img;

  if frmDlgInspetorObjetos <> nil then
  	frmDlgInspetorObjetos.RefreshCamposModelo;
end;

