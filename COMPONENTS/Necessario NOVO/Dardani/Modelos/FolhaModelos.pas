unit FolhaModelos;

interface

uses Classes, SysUtils, Dialogs, Math, DateUtils, ExtCtrls,
	Graphics, Types, Controls;

const
	LARGURA_SHAPE = 7;
  ALTURA_SHAPE = 14;

type
	TCategoriaCampo = (ccTexto, ccDado);
  TBanda = (bdMestre, bdDetalhe);

type
	TModelo = class;
  TCampoModelo = class;

	TCampoNotifyEvent = procedure(TheCampoModelo: TCampoModelo) of object;

	TCampoModelo = class
  private
  	FId : Integer;
    FLinha : Integer;
    FColuna : Integer;
    FTamanho : Integer;
    FNome : String;
    FConteudo : String;
    FCategoriaCampo : TCategoriaCampo;
    FModelo : TModelo;
    FBanda : TBanda;
    FSelecionado: Boolean;
    FOnCampoChange : TCampoNotifyEvent;
    FOnCampoSelect : TCampoNotifyEvent;
    procedure NotificaChange;
  	procedure setId(value : Integer);
  	procedure setLinha(value : Integer);
  	procedure setColuna(Value: Integer);
  	procedure setTamanho(Value: Integer);
  	procedure setNome(Value : String);
  	procedure setConteudo(Value : String);
    procedure setSelecionado(Value : Boolean);
    procedure setCategoriaCampo(Value: TCategoriaCampo);
    procedure setBanda(Value: TBanda);
  public
  	constructor Create(AOwner : TModelo);
    destructor Destroy;
  	property Id : Integer read FId write setId;
  	property Linha : Integer read FLinha write setLinha;
  	property Coluna : Integer read FColuna write setColuna;
  	property Tamanho : Integer read FTamanho write setTamanho;
  	property Nome : String read FNome write setNome;
    property CategoriaCampo: TCategoriaCampo read FCategoriaCampo write setCategoriaCampo;
  	property Conteudo : String read FConteudo write setConteudo;
    property Selecionado : Boolean read FSelecionado write setSelecionado;
    property Banda : TBanda read FBanda write setBanda;
    property Modelo : TModelo read FModelo;
    property OnCampoChange : TCampoNotifyEvent read FOnCampoChange write FOnCampoChange;
    property OnCampoSelect : TCampoNotifyEvent read FOnCampoSelect write FOnCampoSelect;
	end;

  TModelo = class
	private
  	Desmarcando : Boolean;
    FLista : TList;
    FListaCampos : TStrings;

    FNextId : Integer;
    FCampoSelecionadoX : TCampoModelo;
    FQuantidadeDetalhes : Integer;
    FPrimeiraLinhaDetalhe : Integer;
    FColunas : Integer;
    FLinhas : Integer;
    FReguaTopo : Integer;
    FReguaEsquerda : Integer;
    FOnCampoCreate : TCampoNotifyEvent;
    FOnCampoRemove : TCampoNotifyEvent;
    FOnCampoSelect : TCampoNotifyEvent;
    FOnCampoChange : TCampoNotifyEvent;

    procedure NotificaCampoSelect(TheCampoModelo: TCampoModelo);
    procedure NotificaCampoChange(TheCampoModelo: TCampoModelo);

    function GeraNextId : Integer;
    procedure setQuantidadeDetalhes(Value: Integer);
    procedure setPrimeiraLinhaDetalhe(Value: Integer);
    procedure setColunas(Value: Integer);
    procedure setLinhas(Value: Integer);
    function getListaCampos: TStrings;

	public
		constructor Create;
  	destructor Destroy;
    function Count : Integer;
    procedure AdicionaCampoDado(CampoDado: String);
    procedure LimpaListaCampos;
    procedure AdicionaCampoModelo(CampoModelo: TCampoModelo); overload;
    procedure LimpaLista;
    function GetCampoModeloIndice(Index: Integer): TCampoModelo;
    function GetCampoSelecionado: TCampoModelo;
    procedure SelecionaCampoPorId(IdCampo: Integer);
    procedure SelecionaCampo(TheCampo: TCampoModelo);
    procedure RemoveCampo(Campo: TCampoModelo);
    procedure RemoveCampoPorId(IdCampo: Integer);
    procedure DesmarcaTodos(CampoExcessao: TCampoModelo);

    property ReguaTopo : Integer read FReguaTopo;
    property ReguaEsquerda : Integer read FReguaEsquerda;
    property QuantidadeDetalhes : Integer read FQuantidadeDetalhes write setQuantidadeDetalhes;
    property PrimeiraLinhaDetalhe : Integer read FPrimeiraLinhaDetalhe write setPrimeiraLinhaDetalhe;
    property Colunas : Integer read FColunas write setColunas;
    property Linhas : Integer read FLinhas write setLinhas;
    property ListaCampos : TStrings read getListaCampos;

    property OnCampoCreate : TCampoNotifyEvent read FOnCampoCreate write FOnCampoCreate;
    property OnCampoRemove : TCampoNotifyEvent read FOnCampoRemove write FOnCampoRemove;
    property OnCampoSelect : TCampoNotifyEvent read FOnCampoSelect write FOnCampoSelect;
    property OnCampoChange : TCampoNotifyEvent read FOnCampoChange write FOnCampoChange;
  end;

implementation

{ TModelo }

procedure TModelo.AdicionaCampoDado(CampoDado: String);
begin
	if FListaCampos.IndexOf(CampoDado) < 0 then
		FListaCampos.Add(CampoDado);
end;

procedure TModelo.AdicionaCampoModelo(CampoModelo: TCampoModelo);
begin
	CampoModelo.FModelo := Self;
  CampoModelo.Id := GeraNextId;

  // Captura para o modelo quando selecionar campo
  if CampoModelo <> nil then
  begin
		CampoModelo.OnCampoSelect := NotificaCampoSelect;
  end;
	FLista.Add(CampoModelo);

  // Notifica que criou campo
	if Assigned(FOnCampoCreate) then
  	FOnCampoCreate(CampoModelo);
end;

function TModelo.Count: Integer;
begin
	if FLista <> nil then
  	Result := FLista.Count
	else
  	Result := -1;
end;

constructor TModelo.Create;
begin
	inherited;
	Desmarcando := False;
	FNextId := 1;
  FReguaTopo 			:= (2 * ALTURA_SHAPE) + 3;
  FReguaEsquerda 	:= (2 * LARGURA_SHAPE) + 3;

	FLista := TList.Create;
  FListaCampos := TStringList.Create;
  TStringList(FListaCampos).Sorted := True;
end;

procedure TModelo.DesmarcaTodos(CampoExcessao: TCampoModelo);
var
	i : Integer;
  tmpCampoNotifyEvent : TCampoNotifyEvent;
begin
	//Result := nil;
  if Desmarcando then Exit;

  try
  	Desmarcando := True;

    {
    if FCampoSelecionado <> nil then
    begin
    	FCampoSelecionado.Selecionado := False;
      /////////////////NotificaChange(FCampoSelecionado);
    end;
    }

  //    tmpCampoNotifyEvent := Self.OnCampoSelect;
//  	Self.OnCampoSelect := nil;
    {
    for i := 0 to FLista.Count -1 do
    begin
      if (TCampoModelo(FLista.Items[i]) <> CampoExcessao) and
         (TCampoModelo(FLista.Items[i]).Selecionado) then
      begin
        	TCampoModelo(FLista.Items[i]).Selecionado := False;
      end;
    end;
    }
  finally
//    Self.OnCampoSelect := tmpCampoNotifyEvent;
		Desmarcando := False;
  end;
end;

destructor TModelo.Destroy;
begin
	LimpaLista;
  FreeAndNil(FLista);

  FListaCampos.Clear;
  FreeAndNil(FListaCampos);
end;

function TModelo.GetCampoModeloIndice(Index: Integer): TCampoModelo;
begin
	if (Index > -1) and (FLista <> nil) and (FLista.Count > Index) then
  	Result := FLista.Items[Index]
  else
  	Result := nil;
end;

function TModelo.GetCampoSelecionado: TCampoModelo;
var
	i : Integer;
begin
	Result := nil;
	for i := 0 to FLista.Count -1 do
  begin
  	if (TCampoModelo(FLista.Items[i]) <> nil) and
    	 (TCampoModelo(FLista.Items[i]).Selecionado) then
		begin
    	Result := TCampoModelo(FLista.Items[i]);
      Break;
    end;
  end;
end;

function TModelo.getListaCampos: TStrings;
begin
	Result := FListaCampos;
end;

procedure TModelo.LimpaLista;
begin
	if FLista.Count > 0 then
    FLista.Clear;
end;

procedure TModelo.LimpaListaCampos;
begin
	FListaCampos.Clear;
end;

procedure TModelo.RemoveCampo(Campo: TCampoModelo);
var
	i : Integer;
begin
	if Assigned(FOnCampoRemove) then
  	FOnCampoRemove(Campo);

	for i := 0 to FLista.Count -1 do
  begin
  	if (TCampoModelo(FLista.Items[i]) = Campo) then
		begin
      FreeAndNil(Campo);
      FLista.Delete(i);
      Break;
    end;
  end;
end;

procedure TModelo.RemoveCampoPorId(IdCampo: Integer);
var
	i : Integer;
begin
	//Result := nil;
	for i := 0 to FLista.Count -1 do
  begin
  	if (TCampoModelo(FLista.Items[i]) <> nil) and
    	 (TCampoModelo(FLista.Items[i]).Id = IdCampo) then
		begin
    	TCampoModelo(FLista.Items[i]).Destroy;
    	FLista.Delete(i);
      Break;

    //TCampoModelo(FLista.Items[i]).Selecionado := (TCampoModelo(FLista.Items[i]).Id = IdCampo);

    	//TCampoModelo(FLista.Items[i]).Selecionado := (TCampoModelo(FLista.Items[i]).Id = IdCampo);
    end;
  end;
end;

procedure TModelo.SelecionaCampoPorId(IdCampo: Integer);
var
	i : Integer;
  Campo : TCampoModelo;
begin
  Campo := nil;

  if (FCampoSelecionadoX <> nil) and (FCampoSelecionadoX.Id = IdCampo) then
  	Exit;

  if (FCampoSelecionadoX <> nil) then
  begin
 		FCampoSelecionadoX.Selecionado := False;
    NotificaCampoChange(FCampoSelecionadoX);
  end;

	for i := 0 to FLista.Count -1 do
  begin
  	if (TCampoModelo(FLista.Items[i]) <> nil) then
		begin
    	Campo := TCampoModelo(FLista.Items[i]);
      if (Campo.Id = IdCampo) then
      begin
    		Campo.Selecionado := True;
        FCampoSelecionadoX := Campo;
      	NotificaCampoSelect(Campo);
      end;
    end;
  end;
end;

procedure TModelo.setPrimeiraLinhaDetalhe(Value: Integer);
begin
	if Value <> FPrimeiraLinhaDetalhe then
  	FPrimeiraLinhaDetalhe := Value;
end;

procedure TModelo.setLinhas(Value: Integer);
begin
	if Value <> FLinhas then
  	FLinhas := Value;
end;

procedure TModelo.setQuantidadeDetalhes(Value: Integer);
begin
	if Value <> FQuantidadeDetalhes then
  	FQuantidadeDetalhes := Value;
end;

procedure TModelo.setColunas(Value: Integer);
begin
	if Value <> FColunas then
  	FColunas := Value;
end;

function TModelo.GeraNextId: Integer;
begin
	inc(FNextId);
  Result := FNextId;
end;

procedure TModelo.NotificaCampoSelect(TheCampoModelo: TCampoModelo);
begin
	// Fica sabendo quando o campo foi selecionado
  // E avisa
  if Assigned(FOnCampoSelect) then
  	FOnCampoSelect(TheCampoModelo);
end;

procedure TModelo.NotificaCampoChange(TheCampoModelo: TCampoModelo);
begin
	// Fica sabendo quando o campo foi selecionado
  // E avisa
  if Assigned(FOnCampoChange) then
  	FOnCampoChange(TheCampoModelo);
end;

procedure TModelo.SelecionaCampo(TheCampo: TCampoModelo);
var
	i : Integer;
  C : TCampoModelo;
begin
  C := nil;

  if (FCampoSelecionadoX <> nil) and (FCampoSelecionadoX = TheCampo) then
  	Exit;

  if (FCampoSelecionadoX <> nil) then
  begin
 		FCampoSelecionadoX.Selecionado := False;
    NotificaCampoChange(FCampoSelecionadoX);
  end;

	for i := 0 to FLista.Count -1 do
  begin
  	if (TCampoModelo(FLista.Items[i]) <> nil) then
		begin
    	C := TCampoModelo(FLista.Items[i]);
      if (C = TheCampo) then
      begin
    		C.Selecionado := True;
        FCampoSelecionadoX := C;
      	NotificaCampoSelect(C);
      end;
    end;
  end;
end;

{ TCampoModelo }
constructor TCampoModelo.Create(AOwner : TModelo);
begin
	FModelo := AOwner;
  FCategoriaCampo := ccTexto;
	FSelecionado := False;
  FBanda := bdMestre;
end;

procedure TCampoModelo.setColuna(Value: Integer);
begin
	if Value < 0 then Exit;

  if (Value + Self.FTamanho) > FModelo.Colunas then
  	Exit;

	if Value <> FColuna then
  begin
  	FColuna := Value;
    NotificaChange;
	end;
end;

procedure TCampoModelo.setConteudo(Value: String);
begin
	if Value <> FConteudo then
  begin
  	FConteudo := Value;
    NotificaChange;
  end;
end;

procedure TCampoModelo.setId(value: Integer);
begin
	if Value <> FId then
  	FId := Value;
end;

procedure TCampoModelo.setLinha(value: Integer);
var
	bSubindo : Boolean;
  newValue : Integer;
begin
	newValue := Value;

	if newValue < 0 then Exit;

  bSubindo := newValue < FLinha;

	if FBanda = bdMestre then
  begin
    if (newValue >= FModelo.FPrimeiraLinhaDetalhe) and
       (newValue < (FModelo.PrimeiraLinhaDetalhe + FModelo.QuantidadeDetalhes) ) then
    begin
      if bSubindo then
        FLinha := FModelo.FPrimeiraLinhaDetalhe -1
      else
        FLinha := FModelo.PrimeiraLinhaDetalhe + FModelo.QuantidadeDetalhes;
    end else
      FLinha := newValue;
	end else
  	FLinha := FModelo.PrimeiraLinhaDetalhe;

  NotificaChange;
end;

procedure TCampoModelo.setNome(Value: String);
begin
	if Value <> FNome then
  	FNome := Value;
end;

procedure TCampoModelo.setSelecionado(Value: Boolean);
begin
	if (Value <> FSelecionado) then
  	FSelecionado := Value;
end;

procedure TCampoModelo.setTamanho(Value: Integer);
begin
	if Value <= 0 then Exit;

  if (Value + Self.FColuna) > FModelo.FColunas then
  	Exit;

	if Value <> FTamanho then
  begin
  	FTamanho := Value;
    NotificaChange;
	end;
end;

procedure TCampoModelo.setCategoriaCampo(Value: TCategoriaCampo);
begin
	if Value <> FCategoriaCampo then
  	FCategoriaCampo := Value;
end;

procedure TCampoModelo.setBanda(Value: TBanda);
begin
	if Value <> FBanda then
  begin
  	FBanda := Value;
  end;
end;

destructor TCampoModelo.Destroy;
begin
	//NotificaDestroy;
	inherited;
end;

procedure TCampoModelo.NotificaChange;
begin
	if Assigned(FOnCampoChange) then
  	FOnCampoChange(Self);
end;

end.

MODALIDADE FOLHA = 8
