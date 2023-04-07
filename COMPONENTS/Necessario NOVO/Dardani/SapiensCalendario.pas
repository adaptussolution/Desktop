unit SapiensCalendario;

interface

uses
  SysUtils, Classes, Graphics, Controls, ExtCtrls, StdCtrls, Buttons,
  Forms, DateUtils, Dialogs;

type
	TDayClickEvent = procedure(TheDate: TDateTime; Sender: TObject) of object;

type
	TTipoDia = (tdTOTAL, tdPARCIAL, tdDesabilitado);

type
	TSapiensCalendario = class;

  TPeriod = class
  	private
    	FStartTime : TTime;
    	FEndTime : TTime;
    public
    	property StartTime : TTime read FStartTime write FStartTime;
    	property EndTime : TTime read FEndTime write FEndTime;
      constructor Create(tStartTime, tEndTime: TTime);
  end;

  TPeriodList = class
  	private
    	FLista : TList;
    public
    	constructor Create;
      destructor Destroy;
      function isValidPeriod(Period: TPeriod): Boolean;
      function AddPeriod(NewPeriod: TPeriod): Boolean;
      procedure DeletePeriod(Period: TPeriod);
      procedure DeletePeriodIndex(Index: Integer);
      function Count : Integer;
      function GetPeriodByIndex(Index: Integer): TPeriod;
	end;

	TDateContent = class
  private
  	FTheDate : TDateTime;
    FQuantity : Integer;
    FTipoDia : TTipoDia;
    FPeriodList : TPeriodList;
  public
  	property TheDate : TDateTime read FTheDate;
    property Quantity : Integer read FQuantity write FQuantity;
    property TipoDia : TTipoDia read FTipoDia;
    property PeriodList : TPeriodList read FPeriodList write FPeriodList;
  	constructor Create(dDate: TDateTime); overload;
    constructor Create(dDate: TDateTime; NewQuantity: Integer); overload;
    constructor Create(dDate: TDateTime; NewQuantity: Integer; TipoDia: TTipoDia); overload;
    destructor Destroy;
  end;

	TListDate = class
  private
  	FListaDatas : TList;
    FSapiensCalendario : TSapiensCalendario;
    procedure RefreshCalendario;
  public
  	constructor Create(Owner: TSapiensCalendario);
    destructor Destroy;
    function Count : Integer;
    function GetDateByIndex(Index: Integer) : TDateContent;
    function GetByDate(TheDate: TDateTime) : TDateContent;
    function AddDate(TheDate: TDateTime; NewQuantity: Integer = 1): TDateContent; overload;
    function AddDate(TheDate: TDateTime; TipoDia: TTipoDia; NewQuantity: Integer = 1): TDateContent; overload;
    procedure AddDateContent(TheDateContent : TDateContent);
    procedure UpdateQuantity(TheDate: TDateTime; NewQuantity: Integer = 1);
    procedure IncQuantity(TheDate: TDateTime; NewQuantity: Integer = 1);
    procedure DecQuantity(TheDate: TDateTime; NewQuantity: Integer = 1);
    function GetQuantityByDate(TheDate: TDateTime): Integer;
    procedure RemoveDate(TheDate: TDateTime);
    procedure Clear;
  end;

  TSapiensCalendario = class(TCustomPanel)
  private
    { Private declarations }
    FColorDiaTotal: TColor;
    FColorDiaParcial: TColor;
    FColorDiaDesabilitado: TColor;

    FDiaAtual : Word;
    FMesAtual : Word;
    FAnoAtual : Word;
    //FDataIni : TDateTime;
    //FDataFim : TDateTime;

    FInitialDate : TDateTime;
    FFinalDate : TDateTime;

    FOnDayClick : TDayClickEvent;
    FListDate : TListDate;
    FpnlMes: TPanel;
		FPnlBtAnterior: TPanel;
		FPnlBtPosterior: TPanel;
    FSbtMesAnterior: TSpeedButton;
    FSbtMesPosterior: TSpeedButton;
		FLinhaCabecalhoDias : array[1..7] of TPanel;
		FLinhaDias : array[1..42] of TPanel;
    procedure MesAnteriorClick(Sender: TObject);
    procedure MesPosteriorClick(Sender: TObject);
    procedure PainelDiaClick(Sender: TObject);
    procedure DesenhaCabecalho;
    procedure DesenhaCabecalhoDias;
    procedure DesenhaDias;
    procedure prcMontaMes;
    function fncGetQuantidadePorDia(iDia: Word): Integer;

    function getColorDiaDesabilitado: TColor;
    function getColorDiaTotal: TColor;
    function getColorDiaParcial: TColor;

    procedure setColorDiaDesabilitado(value: TColor);
    procedure setColorDiaTotal(value: TColor);
    procedure setColorDiaParcial(value: TColor);
  protected
    { Protected declarations }
  public
    { Public declarations }
    constructor Create(Owner: TComponent); override;
    destructor Destroy;
    procedure StartDate(TheDate : TDateTime);
    procedure SetInitialDate(TheDate : TDateTime);
    procedure SetFinalDate(TheDate : TDateTime);
    property ListDate : TListDate read FListDate;
    procedure RefreshMes;
    function EditaHorariosData(TheDate : TDateTime) : Boolean;
  published
    { Published declarations }
    //property Width :Integer read FWidth write SetWidth;
    //property EditColor : TColor read GetEditColor write SetEditColor;
    property Align;
    property Alignment;
    //property BevelInner;
    //property BevelOuter;
    //property BorderWidth;
    //property BorderStyle;
    property DragCursor;
    property DragMode;
    property TabOrder;
    property OnClick;
    property OnDblClick;
    property OnExit;
    property OnResize;
    property Enabled;
    property Visible;
    property OnDayClick : TDayClickEvent read FOnDayClick write FOnDayClick;

		property ColorDiaTotal: TColor read getColorDiaTotal write SetColorDiaTotal;
		property ColorDiaParcial: TColor read getColorDiaParcial write SetColorDiaParcial;
		property ColorDiaDesabilitado: TColor read getColorDiaDesabilitado write SetColorDiaDesabilitado;
  end;

procedure Register;

implementation

uses ufrmEditorHorarios;

const
	aDias : array[1..7] of String[3] = ('Dom','Seg','Ter','Qua','Qui','Sex','Sab');
  aMeses : array[1..12] of String = ('Janeiro','Fevereiro','Março','Abril','Maio','Junho','Julho','Agosto','Setembro','Outubro','Novembro','Dezembro');

var
  aCoresDia : array[TTipoDia] of TColor;


procedure Register;
begin
  RegisterComponents('Embragec', [TSapiensCalendario]);
end;

{ TSapiensCalendario }

constructor TSapiensCalendario.Create(Owner: TComponent);
begin
  inherited;
  FColorDiaTotal := clRed;
  FColorDiaParcial := $0077BBFF;
  FColorDiaDesabilitado := clGray;

	FListDate := TListDate.Create(Self);
  BevelOuter := bvNone;
  Caption := ' ';
  Width := 280;
  Height := 200;
  Color := FColorDiaTotal;
  DesenhaCabecalho;
  DesenhaCabecalhoDias;
  DesenhaDias;

  StartDate(Now);
end;

procedure TSapiensCalendario.DesenhaCabecalho;
begin
	FpnlMes := TPanel.Create(Self);
  with FpnlMes do
  begin
  	Align := alTop;
    Height := Self.Height div 8; // 1/8 da altura
    BevelOuter := bvLowered;
    Caption := 'Janeiro/2006';
    Color := clNavy;
    Font.Color := clWhite;
    Font.Name := 'Arial';
    Font.Style := [fsBold];
    ParentFont := False;
		Parent := Self;
  end;

  FPnlBtAnterior := TPanel.Create(FPnlMes);
  with FPnlBtAnterior do
  begin
    Width := 38;
    Align := alLeft;
    ParentColor := False;
    Font.Color := clBlack;
    BevelOuter := bvNone;
    Caption := ' ';
    Parent := FPnlMes;
  end;

  FSbtMesAnterior := TSpeedButton.Create(FPnlBtAnterior);
  with FSbtMesAnterior do
  begin
		Left := 0;
    Top := 0;
    Width := FPnlBtAnterior.Width;
    Height := FPnlMes.Height -2;
    Font.Style := [fsBold];
    Caption := '<';
    OnClick := MesAnteriorClick;
    Flat := True;
    Parent := FPnlBtAnterior;
	end;

  FPnlBtPosterior := TPanel.Create(FPnlMes);
  with FPnlBtPosterior do
  begin
    Width := 38;
    Align := alRight;
    ParentColor := False;
    Font.Color := clBlack;
    Font.Style := [fsBold];
    BevelOuter := bvNone;
    Caption := ' ';
    Parent := FPnlMes;
  end;

  FSbtMesPosterior := TSpeedButton.Create(FPnlBtPosterior);
  with FSbtMesPosterior do
  begin
		Left := 0;
    Top := 0;
    Width := FPnlBtPosterior.Width;
    Height := FPnlMes.Height-2;
    Flat := True;
    Caption := '>';
    OnClick := MesPosteriorClick;
    Parent := FPnlBtPosterior;
	end;
end;

procedure TSapiensCalendario.DesenhaCabecalhoDias;
var
	i, w, t, h : Integer;
begin
	w := Self.Width div 7;
  t := (Self.Height div 8);
  h := (Self.Height div 8);
	for i := 1 to 7 do
  begin
  	FLinhaCabecalhoDias[i] := TPanel.Create(Self);
    with FLinhaCabecalhoDias[i] do
    begin
    	Left := (i - 1) * w;
      Width := w;
      Top := t;
      Height := h;
      BorderStyle := bsSingle;
      Font.Name := 'Arial';
      Font.Style := [fsBold];
      ParentFont := False;
    	Caption := aDias[i];
    	Parent := Self;
    end;
  end;
end;

procedure TSapiensCalendario.DesenhaDias;
var
	i, w, t, h : Integer;
begin
	w := Self.Width div 7;
  t := (Self.Height div 8) * 2;
  h := (Self.Height div 8);
	for i := 1 to 42 do
  begin
  	FLinhaDias[i] := TPanel.Create(Self);
    with FLinhaDias[i] do
    begin
    	Left := ( (i-1) mod 7) * w;
      Top := (( (i-1) div 7) * h) + t;
      Width := w;
      Height := h;
      //BorderStyle := bsSingle;
      Font.Name := 'Arial';
      //Font.Style := [fsBold];
      ParentFont := False;
    	Caption := IntToStr(i); //'0';
      Name := 'Pnl'+Format('%2.2d',[i]);
      OnClick := PainelDiaClick;
    	Parent := Self;
    end;
  end;
end;

procedure TSapiensCalendario.StartDate(TheDate: TDateTime);
begin
  DecodeDate(TheDate, FAnoAtual, FMesAtual, FDiaAtual);
  prcMontaMes;
end;

procedure TSapiensCalendario.prcMontaMes;
var
	FDataIni : TDateTime;
	FDataFim : TDateTime;

	i, Dia, PrimeiroDia, DiasNoMes : Integer;
  Pnl : TPanel;
begin
	for i := 1 to 42 do
  begin
  	Pnl := TPanel(FLinhaDias[i]);
    if Pnl <> nil then
    begin
    	Pnl.Caption := ' ';
      Pnl.Color := clBtnFace;
      Pnl.Tag := 0;
    end;
  end;

  FpnlMes.Caption := Format('%s/%d',[aMeses[FMesAtual],FAnoAtual]);

  FDataIni := EncodeDate(FAnoAtual, FMesAtual, 1);
  PrimeiroDia := DayOfWeek(FDataIni);
  DiasNoMes := DaysInAMonth(FAnoAtual, FMesAtual);
  FDataFim := EncodeDate(FAnoAtual, FMesAtual, DiasNoMes);

  Dia := 1;
  for i := PrimeiroDia to (PrimeiroDia + DiasNoMes -1) do
  begin
  	Pnl := TPanel(FLinhaDias[i]);
    if Pnl <> nil then
    begin
			Pnl.Font.Style := [];
      Pnl.BorderStyle := bsNone;
      //Pnl.OnMouseDown := VirtualPnlDiaMouseDown;
    	Pnl.Caption := IntToStr(Dia);
      Pnl.Color := clWhite;


      {if aDiasComFalta[dia] > 0 then
      	Pnl.Color := clRed
      else
      	Pnl.Color := clBtnFace;
      }
      Pnl.Tag := Dia;
    end;
    inc(Dia);
  end;
  // ClicaNoDiaAtual
  RefreshMes;
end;

procedure TSapiensCalendario.MesAnteriorClick(Sender: TObject);
begin
	IncAMonth(FAnoAtual, FMesAtual, FDiaAtual,-1);
  prcMontaMes;
end;

procedure TSapiensCalendario.MesPosteriorClick(Sender: TObject);
begin
	IncAMonth(FAnoAtual, FMesAtual, FDiaAtual);
  prcMontaMes;
end;

procedure TSapiensCalendario.PainelDiaClick(Sender: TObject);
var
	TheDate : TDateTime;
begin
	if (TPanel(Sender).Tag > 0) and Assigned(FOnDayClick) then
  begin
		TheDate := EncodeDate(FAnoAtual, FMesAtual, TPanel(Sender).Tag);
  	FOnDayClick(TheDate, Self);
	end;
end;

destructor TSapiensCalendario.Destroy;
begin
	FListDate.Destroy;
  FreeAndNil(FListDate);
end;

procedure TSapiensCalendario.RefreshMes;
var
	i : Integer;
  Pnl : TPanel;
  Dia : Word;
  Data : TDateTime;
  DC : TDateContent;
begin
//	if fncGetQuantidadePorDia(Dia) > 0 then
//  	Pnl.Color := clRed;
	for i := 1 to 42 do
  begin
  	Pnl := FLinhaDias[i];
    Dia := Pnl.Tag;
    if (Dia in [1..31]) then
    begin
    	Data := EncodeDate(FAnoAtual, FMesAtual, Dia);
      DC := Self.ListDate.GetByDate(Data);

      //if Self.FListDate.GetQuantityByDate(Data) > 0 then

      if (DC <> nil) then
      	Pnl.Color := aCoresDia[DC.TipoDia]
      else
    		Pnl.Color := clWhite;
		end;
  end;

end;

function TSapiensCalendario.fncGetQuantidadePorDia(iDia: Word): Integer;
begin
//
end;

function TSapiensCalendario.EditaHorariosData(TheDate : TDateTime): Boolean;
begin
  try
    frmEditorHorarios := TfrmEditorHorarios.Create(Self, TheDate, nil);
    Result := (frmEditorHorarios.ShowModal = mrOk);
  finally
    FreeAndNil(frmEditorHorarios);
  end;
end;

procedure TSapiensCalendario.SetFinalDate(TheDate: TDateTime);
begin
	FFinalDate := TheDate;
end;

procedure TSapiensCalendario.SetInitialDate(TheDate: TDateTime);
begin
	FInitialDate := TheDate;
end;

{
procedure TSapiensCalendario.setColorDesabilitado(value: TColor);
begin
	aCoresDia[tdDesabilitado]  := value;
end;
}

procedure TSapiensCalendario.setColorDiaDesabilitado(value: TColor);
begin
	aCoresDia[tdDesabilitado] := value;
end;

procedure TSapiensCalendario.setColorDiaParcial(value: TColor);
begin
	aCoresDia[tdPARCIAL]  := value;
end;

procedure TSapiensCalendario.setColorDiaTotal(value: TColor);
begin
	aCoresDia[tdTOTAL]  := value;
end;

function TSapiensCalendario.getColorDiaDesabilitado: TColor;
begin
	result := aCoresDia[tdDesabilitado];
end;

function TSapiensCalendario.getColorDiaParcial: TColor;
begin
	result := aCoresDia[tdParcial];
end;

function TSapiensCalendario.getColorDiaTotal: TColor;
begin
	result := aCoresDia[tdTotal];
end;

{ TDateContent }

constructor TDateContent.Create(dDate: TDateTime);
begin
	FTheDate := Trunc(dDate);
  FQuantity := 1;
  FTipoDia := tdTOTAL;
  FPeriodList := TPeriodList.Create;
end;

constructor TDateContent.Create(dDate: TDateTime; NewQuantity: Integer);
begin
	Create(dDate);
  FTipoDia := tdTOTAL;
	//FTheDate := Trunc(TheDate);
  if NewQuantity > 0 then
  	FQuantity := NewQuantity
  else
  	FQuantity := 1;
end;

constructor TDateContent.Create(dDate: TDateTime; NewQuantity: Integer; TipoDia: TTipoDia);
begin
	Create(dDate, NewQuantity);
{
	FTheDate := Trunc(TheDate);
  if Quantity > 0 then
  	FQuantity := Quantity
  else
  	FQuantity := 1;
}
  FTipoDia := TipoDia;
end;

destructor TDateContent.Destroy;
begin
	FPeriodList.FLista.Clear;
  FreeAndNil(FPeriodList);
end;

{ TListDate }

function TListDate.AddDate(TheDate: TDateTime; NewQuantity: Integer = 1): TDateContent;
var
	Encontrou : Boolean;
	i : Integer;
  DC, DCResult : TDateContent;
begin
  Result := nil; 
	if NewQuantity <= 0 then Exit;


	// Adiciona uma data com a quantidade. Se já houver data, muda a quantidade para a nova
  Encontrou := False;
  for i := 0 to FListaDatas.Count -1 do
  begin
		DC := TDateContent(FListaDatas.Items[i]);
    if DC.TheDate = TheDate then
    begin
  		DC.Quantity := NewQuantity;
      DCResult := DC;
    	Encontrou := True;
    	Break;
    end else
    if DC.TheDate > TheDate then
    begin
    	DCResult := TDateContent.Create(TheDate, NewQuantity);
    	FListaDatas.Insert(i,DCResult);
    	Encontrou := True;
      Break;
    end;
  end;

  if not Encontrou then
  begin
  	DCResult := TDateContent.Create(TheDate, NewQuantity);
  	FListaDatas.Add(DCResult);
  end;

  Result := DCResult;
	RefreshCalendario;
end;

function TListDate.AddDate(TheDate: TDateTime; TipoDia: TTipoDia; NewQuantity: Integer = 1): TDateContent;
var
	Encontrou : Boolean;
	i : Integer;
  DC, DCResult : TDateContent;
begin
	DC := nil;
  DCResult := nil;

	if NewQuantity <= 0 then Exit;

	// Adiciona uma data com a quantidade. Se já houver data, muda a quantidade para a nova
  Encontrou := False;
  for i := 0 to FListaDatas.Count -1 do
  begin
		DC := TDateContent(FListaDatas.Items[i]);
    if DC.TheDate = TheDate then
    begin
  		DC.Quantity := NewQuantity;
      DCResult := DC;
    	Encontrou := True;
    	Break;
    end else
    if DC.TheDate > TheDate then
    begin
    	DCResult := TDateContent.Create(TheDate, NewQuantity, TipoDia);
    	FListaDatas.Insert(i,DCResult);
    	Encontrou := True;
      Break;
    end;
  end;

  if not Encontrou then
  begin
  	DCResult := TDateContent.Create(TheDate, NewQuantity, TipoDia);
  	FListaDatas.Add(DCResult);
  end;
	Result := DCResult;
	RefreshCalendario;
end;


procedure TListDate.RemoveDate(TheDate: TDateTime);
var
	i : Integer;
  DC : TDateContent;
begin
	// Remove data da lista
  for i := 0 to FListaDatas.Count -1 do
  begin
		DC := TDateContent(FListaDatas.Items[i]);
    if DC.TheDate = TheDate then
    begin
    	DC.Destroy;
    	FListaDatas.Delete(i);
    	Break;
    end;
  end;
  RefreshCalendario;
end;

function TListDate.Count: Integer;
begin
	// Retorna a quantidade de datas
  Result := FListaDatas.Count;
end;

constructor TListDate.Create(Owner: TSapiensCalendario);
begin
	FListaDatas := TList.Create;
  Self.FSapiensCalendario := Owner;
end;

destructor TListDate.Destroy;
begin
	FListaDatas.Clear;
  FreeAndNil(FListaDatas);
end;

procedure TListDate.DecQuantity(TheDate: TDateTime; NewQuantity: Integer = 1);
var
	Encontrou : Boolean;
	i : Integer;
  DC : TDateContent;
begin
	// Decrementa Quantidade. Se nova quantidade for igual ou menor que zero, remove
	if NewQuantity <= 0 then Exit;

  Encontrou := False;
  for i := 0 to FListaDatas.Count -1 do
  begin
		DC := TDateContent(FListaDatas.Items[i]);
    if DC.TheDate = TheDate then
    begin
  		DC.Quantity := DC.Quantity - NewQuantity;
      if DC.Quantity <= 0 then
      begin
      	DC := nil;
				FListaDatas.Delete(i);
      end;

    	Encontrou := True;
    	Break;
    end;
  end;
  RefreshCalendario;
end;

procedure TListDate.IncQuantity(TheDate: TDateTime; NewQuantity: Integer = 1);
var
	Encontrou : Boolean;
	i : Integer;
  DC : TDateContent;
begin
	// Incrementa a quantidade. Se data não existir, cria
	if NewQuantity <= 0 then Exit;

  Encontrou := False;
  for i := 0 to FListaDatas.Count -1 do
  begin
		DC := TDateContent(FListaDatas.Items[i]);
    if DC.TheDate = TheDate then
    begin
  		DC.Quantity := DC.Quantity + NewQuantity;
    	Encontrou := True;
    	Break;
    end;
  end;

  if not Encontrou then
  	FListaDatas.Add(TDateContent.Create(TheDate, NewQuantity));

	RefreshCalendario;
end;

procedure TListDate.UpdateQuantity(TheDate: TDateTime;
  NewQuantity: Integer = 1);
var
	Encontrou : Boolean;
	i : Integer;
  DC : TDateContent;
begin
	// Atualiza nova quantidade. Só aceita se nova for maior que 0
	if NewQuantity <= 0 then Exit;

  Encontrou := False;
  for i := 0 to FListaDatas.Count -1 do
  begin
		DC := TDateContent(FListaDatas.Items[i]);
    if DC.TheDate = TheDate then
    begin
  		DC.Quantity := NewQuantity;
    	Encontrou := True;
    	Break;
    end;
  end;

  if not Encontrou then
  	FListaDatas.Add(TDateContent.Create(TheDate, NewQuantity));

	RefreshCalendario;
end;

function TListDate.GetQuantityByDate(TheDate: TDateTime): Integer;
var
	i : Integer;
  DC : TDateContent;
begin
	// Retorna quantos eventos possuem na data informada
  Result := 0;
  for i := 0 to FListaDatas.Count -1 do
  begin
		DC := TDateContent(FListaDatas.Items[i]);
    if DC.TheDate = TheDate then
    begin
  		Result := DC.Quantity;
    	Break;
    end;
  end;
end;

function TListDate.GetDateByIndex(Index: Integer): TDateContent;
begin
	// Retorna um TDateContent se existir o índice
  Result := nil;
	if (Index >= 0) and (Index < FListaDatas.Count) then
  	Result := FListaDatas.Items[Index];
end;

procedure TListDate.RefreshCalendario;
begin
	if Self.FSapiensCalendario <> nil then
  	Self.FSapiensCalendario.RefreshMes;
end;

function TListDate.GetByDate(TheDate: TDateTime): TDateContent;
var
	i : Integer;
  DC : TDateContent;
begin
	// Retorna quantos eventos possuem na data informada
  Result := nil;
  for i := 0 to FListaDatas.Count -1 do
  begin
		DC := TDateContent(FListaDatas.Items[i]);
    if DC.TheDate = TheDate then
    begin
  		Result := DC;
    	Break;
    end;
  end;
end;

procedure TListDate.AddDateContent(TheDateContent: TDateContent);
var
	Encontrou : Boolean;
	i : Integer;
  DC : TDateContent;
begin
	if TheDateContent.Quantity <= 0 then Exit;

	// Adiciona uma data com a quantidade. Se já houver data, muda a quantidade para a nova
  Encontrou := False;
  for i := 0 to Self.FListaDatas.Count -1 do
  begin
		DC := TDateContent(Self.FListaDatas.Items[i]);
    if DC.TheDate = TheDateContent.TheDate then
    begin
    	///////ShowMessage(DateToStr(DC.TheDate));
  		DC.Quantity := TheDateContent.Quantity;
      DC.PeriodList := TheDateContent.PeriodList;
    	Encontrou := True;
    	Break;
    end else
    if DC.TheDate > TheDateContent.TheDate then
    begin
    	FListaDatas.Insert(i, TheDateContent);
    	Encontrou := True;
      Break;
    end;
  end;

  if not Encontrou then
  	Self.FListaDatas.Add(TheDateContent);

	RefreshCalendario;
end;

procedure TListDate.Clear;
begin
	FListaDatas.Clear;
end;

{ TPeriodList }

function TPeriodList.AddPeriod(NewPeriod: TPeriod): Boolean;
var
	i : Integer;
  PAtual, PAnterior, PPosterior : TPeriod;
begin
	// Retorna True se não houver data compreendida
  Result := True;
  if (NewPeriod.EndTime < NewPeriod.StartTime) then
  begin
  	Result := False;
  	Exit;
  end;

  if FLista.Count = 0 then
  begin
		FLista.Add(NewPeriod);
    Result := True;
  end else
  begin
  	// Tem mais que um
    for i := 0 to FLista.Count-1 do
    begin
      PAtual := TPeriod(FLista.Items[i]);
      if i = 0 then
      	PAnterior := nil
      else
        PAnterior := TPeriod(FLista.Items[i-1]);
      if i = FLista.Count-1 then
        PPosterior := nil
      else
        PPosterior := TPeriod(FLista.Items[i+1]);

      if (NewPeriod.EndTime   < PAtual.StartTime) or
         (NewPeriod.StartTime > PAtual.EndTime) then
			begin
      	// Insere só antes desse se for menor
    		if NewPeriod.EndTime < PAtual.StartTime then
        begin
        	FLista.Insert(i, NewPeriod);
          Result := True;
          Break;
        end else
        begin
        	if (PPosterior = nil) then
          begin
          	FLista.Add(NewPeriod);
            Result := True;
            Break;
          end;
        end;
      end else
			begin
      	Result := False;
      	Break;
      end;
    end; // end for
  end; // Se primeiro
end;

function TPeriodList.Count: Integer;
begin
	Result := FLista.Count;
end;

constructor TPeriodList.Create;
begin
	FLista := TList.Create;
end;

procedure TPeriodList.DeletePeriod(Period: TPeriod);
var
	i : Integer;
  P : TPeriod;
begin
	// Retorna quantos eventos possuem na data informada
  //Result := nil;
  for i := 0 to FLista.Count -1 do
  begin
		P := TPeriod(FLista.Items[i]);
    if 	(P.StartTime = Period.StartTime) and
    		(P.EndTime = Period.EndTime) then
    begin
    	P.Destroy;
      FLista.Items[i] := nil;
      FLista.Delete(i);
    end;
  end;
end;

procedure TPeriodList.DeletePeriodIndex(Index: Integer);
begin
	if (Index >= 0) and (Index < FLista.Count) then
  begin
  	FLista.Items[Index] := nil;
  	FLista.Delete(Index);
  end;
end;

destructor TPeriodList.Destroy;
begin
	FLista.Clear;
  FreeAndNil(FLista);
end;

function TPeriodList.GetPeriodByIndex(Index: Integer): TPeriod;
begin
	// Retorna um TDateContent se existir o índice
  Result := nil;
	if (Index >= 0) and (Index < FLista.Count) then
  	Result := FLista.Items[Index];
end;

function TPeriodList.isValidPeriod(Period: TPeriod): Boolean;
var
	i : Integer;
  PAtual : TPeriod;
begin
{
	// Retorna True se não houver data compreendida
  Result := True;
  if (Period.EndTime < Period.StartTime) then
  begin
  	Result := False;
  	Exit;
  end;

  if FLista.Count = 0 then
  begin
  	Result := True;
  	Exit;
  end;


	for i := i to FLista.Count-1 do
  begin
  	PAtual := TPeriod(FLista.Items[i]);

      if not ( (PAtual.EndTime   < PPosterior.StartTime) or
               (PAtual.StartTime > PPosterior.EndTime)
             ) then



		//Period.EndTime < P.StartTime // Ok - antes


    //FLista.in
    //if
  end;
}
end;

{ TPeriod }

constructor TPeriod.Create(tStartTime, tEndTime: TTime);
begin
	if tEndTime < tStartTime then
  	Raise Exception.Create('Hora final não pode ser inferior a hora inicial');
	Self.FStartTime := tStartTime;
  Self.FEndTime := tEndTime;    
end;

end.

