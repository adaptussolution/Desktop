unit SapiensCriterios;

interface

uses Windows, Classes, SysUtils, Variants, SapiensTypes, Forms, Controls , ExtCtrls,
	StdCtrls, Buttons, Graphics, Dialogs;

type
	TEmbCriteriaField = class;
  TEmbCriteriaFields = class;
  TEmbCriterias = class;
  TEmbCriteria = class;

	TEmbCriteriaGroup = class(TScrollBox)
  private
  	FFields : TEmbCriteriaFields;
    procedure RefreshAll;
  public
		function Add(F: TEmbCriteriaField) : Integer;
    procedure Delete(Index: Integer);
    function Count : Integer;
    function GetExpression : String;
  	Constructor Create(AOwner: TComponent); override;
    Destructor Destroy;
  end;

  TEmbCriteriaFields = class(TList)
  private
  	// function Add(Item: Pointer): Integer; overload;
  public
  	function Add(Obj: TEmbCriteriaField): Integer; overload;
  end;

  TPanelCampo = class(TPanel)
  private
  	FTitulo : String;
    FPanelTitulo : TPanel;
    FLabelTitulo : TLabel;
    FCbxEOUCriterio : TComboBox;
    FBtInsCriterio : TBitBtn;
    FScrBoxCriterios : TSCrollBox;
    FField : TEmbCriteriaField;
    procedure SetTitulo(Value: String);
    procedure VirtualIncluirCriterioClick(Sender: TObject);
    procedure RefreshState;
    procedure RefreshHeight;
    procedure VirtualOperatorChange(Sender : TObject);
  public
    constructor Create(AOwner: TComponent; Field: TEmbCriteriaField);
    destructor Destroy; override;
  published
    property Titulo : String read FTitulo write SetTitulo;
	end;

  TPanelCriteria = class(TPanel)
  private
  	FCriteria : TEmbCriteria;
    FCbxCriterio : TComboBox;
    FCbxEOU : TComboBox;
    FBtExcluir : TBitBtn;
    FEdtValue : TEdit;
    FShpLinha : TShape;
    procedure VirtualCriteriaChange(Sender : TObject);
    procedure VirtualOperatorChange(Sender : TObject);
		procedure VirtualValueChange(Sender: TObject);
		procedure VirtualExcluirCriterioClick(Sender: TObject);
  public
  	constructor Create(AOwner: TComponent; TheCriteria: TEmbCriteria);
    destructor Destroy; override;
  published
  end;

	TEmbCriteriaField = class
  private
  	FInternalName : String;
    FDisplayText : String;
    FContentType : SapiensTypes.TContentType;
    FOperable : Boolean;
    FOperator : SapiensTypes.TOperator;
    FCriterias : TEmbCriterias;
    FEmbCriteriaGroup : TEmbCriteriaGroup;
    FPanelField : TPanelCampo;
    procedure SetOperable(Value : Boolean);
    procedure SetOperator(Value : TOperator);
    procedure SetCriterias(Value : TEmbCriterias);
    procedure RefreshStateCriterias;
    function GetOperatorText : String;
    function GetOperatorSql : String;
  public
  	constructor Create(InternalName, DisplayText: String; ContentType : TContentType;
    	TheParent: TEmbCriteriaGroup);
    destructor Destroy;
	published
  	property Criterias: TEmbCriterias read FCriterias write SetCriterias;
  	property InternalName : String read FInternalName;
  	property DisplayText : String read FDisplayText;
  	property ContentType : SapiensTypes.TContentType read FContentType;
  	property Operable : Boolean read FOperable write SetOperable;
  	property Operator : TOperator read FOperator write SetOperator;
    property OperatorText : String read GetOperatorText;
    property OperatorSql : String read GetOperatorSql;
  end;

  TEmbCriterias = class(TList)
  private
  	//function Add(Item: Pointer): Integer; overload;
    FParentField : TEmbCriteriaField;
    procedure RefreshStateAll;
  public
  	function Add(O: TEmbCriteria): Integer;
    procedure Delete(Index: Integer); 
    constructor Create(ParentField : TEmbCriteriaField);
  end;

  TEmbCriteria = class
  private
    FValue : Variant;
    FOperable : Boolean;
    FCriteriaType : TCriteriaType;
    FContentType : SapiensTypes.TContentType;
    FOperator : SapiensTypes.TOperator;
    FValidCriterias : Set of TCriteriaType;
    FField : TEmbCriteriaField;
    FPanelCriteria : TPanelCriteria;
    function GetOperatorText : String;
    function GetOperatorSql : String;
    function GetCriteriaText : String;
    function GetCriteriaSql : String;
    procedure SetOperable(NewValue : Boolean);
    procedure SetOperator(NewValue : TOperator);
    procedure SetValue(NewValue : Variant);
    procedure SetCriteriaType(NewValue : TCriteriaType);
  public
		constructor Create(TheContentType: TContentType; TheField: TEmbCriteriaField);
    destructor Destroy;
  published
  	property ContentType : SapiensTypes.TContentType read FContentType;
    property CriteriaType : TCriteriaType read FCriteriaType write SetCriteriaType;
  	property Value : Variant read FValue write SetValue;
  	property Operable : Boolean read FOperable write SetOperable;
  	property Operator : TOperator read FOperator write SetOperator;
    property OperatorText : String read GetOperatorText;
    property OperatorSql : String read GetOperatorSql;
    property CriteriaText : String read GetCriteriaText;
    property CriteriaSql : String read GetCriteriaSql;
  end;

	//function fncCriaPainelCampo(sTitulo : String; Controle :TWinControl) : TPanel;
	procedure prcPopulaItemsComboCriterios(Cbb: TComboBox);
	procedure prcPopulaItemsComboEOU(Cbb: TComboBox);
	//procedure VirtualIncluirCriterioClick(Sender: TObject);

procedure Register;

implementation

{ TEmbCriteriaGroup }

function TEmbCriteriaGroup.Add(F: TEmbCriteriaField): Integer;
begin
	Result := FFields.Add(F);
  RefreshAll;
end;

function TEmbCriteriaGroup.Count: Integer;
begin
	Result := FFields.Count;
end;

constructor TEmbCriteriaGroup.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
  Width := 470;
  Height := 200;
	FFields := TEmbCriteriaFields.Create;
end;

procedure TEmbCriteriaGroup.Delete(Index: Integer);
begin
	FFields.Delete(Index);;
  RefreshAll;
end;

destructor TEmbCriteriaGroup.Destroy;
begin
	FFields.Clear;
	FFields.Free;
end;

function TEmbCriteriaGroup.GetExpression: String;
var
	x, y : Integer;
  F : TEmbCriteriaField;
  C : TEmbCriteria;
  s, sValue, sLastOperator : String;
begin
	// To be implemented
	Result := '';
  sLastOperator := '';
  for x := 0 to Self.FFields.Count -1 do
  begin
  	F := Self.FFields[x];
    s := '';

    if F.FCriterias.Count > 0 then
    begin
    	if sLastOperator <> '' then
      	s := s + ' ' + sLastOperator + ' ';
    	s := s + '(';
    end;

    for y := 0 to F.FCriterias.Count -1 do
    begin
			C := F.FCriterias[y];

      if C.FValue = null then
      	sValue := ''
      else
      	sValue := C.FValue;

			if (F.FContentType in [ctInteger, ctReal]) then
      	sValue := sValue
      else
      	sValue := QuotedStr(sValue);

		 	if C.CriteriaType in [SapiensTypes.tcNull, SapiensTypes.tcNotNull] then
      	s := s + Format('(%s %s)',[F.FInternalName,
      														C.CriteriaSql
      														])
      else
      	s := s + Format('(%s %s %s)',[F.FInternalName,
      															C.CriteriaSql,
                                  	sValue
      															]);
			if C.Operable then
      	s := s + ' ' + C.OperatorSql + ' ';

    end;
    if F.FCriterias.Count > 0 then
    begin
      s := s + ') ';
      sLastOperator := F.OperatorSql;
    end;

		Result := Result + s;
  end;
end;

{ TEmbCriteriaFields }

{
function TEmbCriteriaFields.Add(Item: Pointer): Integer;
begin
	Result := inherited Add(Item);
end;
}

function TEmbCriteriaFields.Add(Obj: TEmbCriteriaField): Integer;
begin
	Result := inherited Add(Obj);
end;


procedure TEmbCriteriaGroup.RefreshAll;
var
	x, y : Integer;
	F, FTemp: TEmbCriteriaField;
  bOperable : Boolean;
begin
	// Varrerá todos os critérios setando-os para operable ou não
  for x := 0 to Self.FFields.Count -1 do
  begin
  	F := TEmbCriteriaField(Self.FFields[x]);
    bOperable := False;
    if F.Criterias.Count > 0 then
    begin
      for y := x + 1 to Self.FFields.Count -1 do
      begin
        FTemp := TEmbCriteriaField(Self.FFields[y]);
        if (FTemp <> nil) and (FTemp.FCriterias.Count > 0) then
        begin
          bOperable := True;
          Break;
        end;
      end;
		end;
    if F <> nil then
    begin
    	F.Operable := bOperable;
    end;
  end;
end;

{ TEmbCriteriaField }

constructor TEmbCriteriaField.Create(InternalName, DisplayText: STring;
  ContentType: TContentType; TheParent: TEmbCriteriaGroup);
begin
	FInternalName := InternalName;
  FDisplayText := DisplayText;
  FContentType := ContentType;
  FOperable := False;
  FOperator := opNull;
  FCriterias := TEmbCriterias.Create(Self);
	FPanelField := TPanelCampo.Create(TheParent, Self);
  FPanelField.Parent := TheParent;
  FPanelField.Text := DisplayText;
  FEmbCriteriaGroup := TheParent;
end;

destructor TEmbCriteriaField.Destroy;
begin
{
	FPanelField.Free;
	FCriterias.Clear;
  FCriterias.Free;
}
end;

function TEmbCriteriaField.GetOperatorSql: String;
begin
	case Self.FOperator of
  	opOr : Result := 'OR';
    opAnd: Result := 'AND';
    opNull : Result := '';
  end;
end;

function TEmbCriteriaField.GetOperatorText: String;
begin
	case Self.FOperator of
  	opOr : Result := 'OU';
    opAnd: Result := 'E';
    opNull : Result := '';
  end;
end;

procedure TEmbCriteriaField.RefreshStateCriterias;
begin
	// Varre todos os critérios e deixa apenas o último com opção de E/OU (SetOperable)
end;

procedure TEmbCriteriaField.SetCriterias(Value: TEmbCriterias);
begin
	if Value <> FCriterias then
  	FCriterias.Assign(Value);
end;

procedure TEmbCriteriaField.SetOperable(Value: Boolean);
begin
	if Value <> FOperable then
  begin
  	FOperable := Value;
  end;
  if Self.FPanelField <> nil then
  begin
		Self.FPanelField.FCbxEOUCriterio.Visible := FOperable;
    Self.FPanelField.FCbxEOUCriterio.Refresh;
  end;
end;

procedure TEmbCriteriaField.SetOperator(Value: TOperator);
begin
	if FOperator <> Value then
  	FOperator := Value;
end;


{ TCriteria }

constructor TEmbCriteria.Create(TheContentType: TContentType; TheField: TEmbCriteriaField);
var
	Pnl : TPanelCriteria;
begin
	FField := TheField;
	FContentType := TheContentType;
  FValue := null;
  FOperable := False;
  FOperator := opNull;
  FValidCriterias := SapiensTypes.CRITERIAS_FOR_ALL;
  if FContentType = ctString then
  	FValidCriterias := FValidCriterias + SapiensTypes.CRITERIAS_ONLY_TO_STRING;

	Pnl := TPanelCriteria.Create(FField.FPanelField.FScrBoxCriterios, Self);
  FPanelCriteria := Pnl;
end;

destructor TEmbCriteria.Destroy;
begin
//	ShowMessage(Format('IndexOf() = %d',[Self.FPanelCriteria.FCriteria.FField.FCriterias.IndexOf(Self)]));

	FreeAndNil(FPanelCriteria);

  //FPanelCriteria.Destroy;
	//FreeAndNil(FPanelCriteria);
//	FPanelCriteria := nil;
//	FPanelCriteria.FreeNotification(Self);
//  FPanelCriteria.Free;
end;

function TEmbCriteria.GetCriteriaSql: String;
begin
	Result := SapiensTypes.CRITERIAS_SYMBOLS[FCriteriaType];
end;

function TEmbCriteria.GetCriteriaText: String;
begin
	Result := SapiensTypes.CRITERIAS_NAMES[FCriteriaType];
end;

function TEmbCriteria.GetOperatorSql: String;
begin
	case FOperator of
  	opOr : Result := 'OR';
    opAnd: Result := 'AND';
    opNull : Result := '';
  end;
end;

function TEmbCriteria.GetOperatorText: String;
begin
	case FOperator of
  	opOr : Result := 'OU';
    opAnd: Result := 'E';
    opNull : Result := '';
  end;
end;

procedure TEmbCriteria.SetCriteriaType(NewValue: TCriteriaType);
begin
	if (NewValue <> FCriteriaType) and (NewValue in FValidCriterias) then
  	FCriteriaType := NewValue;
end;

procedure TEmbCriteria.SetOperable(NewValue: Boolean);
begin
	if NewValue <> FOperable then
  begin
  	FOperable := NewValue;
    if not FOperable then
    begin
    	FOperator := opNull;
    end;
  end;
  FPanelCriteria.FCbxEOU.Visible := FOperable;
  FPanelCriteria.FCbxEOU.OnChange(FPanelCriteria.FCbxEOU);
end;

procedure TEmbCriteria.SetOperator(NewValue: TOperator);
begin
	if FOperator <> NewValue then
  	FOperator := NewValue;
end;

procedure TEmbCriteria.SetValue(NewValue: Variant);
begin
	if NewValue <> Self.FValue then
  	FValue := NewValue;
end;

{ TEmbCriterias }

{
function TEmbCriterias.Add(Item: Pointer): Integer;
begin
	Result := inherited Add(Item);
end;
}

function TEmbCriterias.Add(O: TEmbCriteria): Integer;
begin
	Result := inherited Add(O);
  Self.FParentField.FPanelField.RefreshState;
  Self.RefreshStateAll;
  Self.FParentField.FEmbCriteriaGroup.RefreshAll;
end;


{
function fncCriaPainelCampo(sTitulo : String; Controle :TWinControl) : TPanel;
var
	PnlCampo, PnlTituloCampo : TPanel;
  lblTituloCampo : TLabel;
  ScrBoxCriterios : TScrollBox;
  CbxEOUCriterio : TComboBox;
  BtInsCriterio : TBitBtn;
begin
	PnlCampo := TPanel.Create(Controle);
	with PnlCampo do
  begin
    Top := 150;
    Left := 250;
  	Parent := Controle;
    Width := 440;
    Height := 200;
    Caption := '';
    BevelOuter := bvNone;
    //Color := clYellow;
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
    Color := clBtnShadow;
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
    prcPopulaItemsComboEOU(CbxEOUCriterio);
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
    //OnClick := VirtualIncluirCriterioClick;
  end;

  ScrBoxCriterios := TScrollBox.Create(PnlCampo);
  with ScrBoxCriterios do
  begin
  	Parent := PnlCampo;
  	Align := alClient;
    // BorderStyle := bsNone;
    // BevelOuter := bvNone;
  end;
  Result := PnlCampo;
end;
}

procedure prcPopulaItemsComboEOU(Cbb: TComboBox);
begin
	Cbb.Items.Add('OU');
	Cbb.Items.Add('E');
end;

procedure prcPopulaItemsComboCriterios(Cbb: TComboBox);
var
	i : TCriteriaType;
begin
	for i := Low(SapiensTypes.TCriteriaType) to High(SapiensTypes.TCriteriaType) do
  begin
		Cbb.Items.Add(CRITERIAS_NAMES[i]);
	end;
end;

procedure TPanelCampo.VirtualIncluirCriterioClick(Sender: TObject);
var
	Pnl : TPanelCampo;
  ScBox : TScrollBox;
  i : Integer;
  C : TEmbCriteria;
begin
	// Sender - TBitBtn
	if not (Sender is TBitBtn) then
  	Exit;
	if not ((Sender as TBitBtn).Parent is TPanel) then
  	Exit;
	if not ( ((Sender as TBitBtn).Parent as Tpanel).Parent is TPanelCampo) then
  	Exit;
	// ShowMessage('Clicou no botão e o pai é um TPanelCampo');

	Pnl := ( ((Sender as TBitBtn).Parent as TPanel).Parent as TPanelCampo);

  if (Pnl <> nil) and (Pnl.FField <> nil) and (Pnl.FField.Criterias <> nil) then
  begin
		//ShowMessage('Clicou no botão e o pai é um TPanelCampo e possui lista de critérios para o campo');

    C := TEmbCriteria.Create(Pnl.FField.ContentType, Pnl.FField);
		Pnl.FField.Criterias.Add(C);

		{

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
    }
	end;
end;

procedure Register;
begin
  RegisterComponents('Embragec', [TEmbCriteriaGroup]);
end;


{ TPanelCampo }

constructor TPanelCampo.Create(AOwner: TComponent; Field: TEmbCriteriaField);
begin
  inherited Create(AOwner);
  FField := Field;
	Top := 150;
  Left := 250;
  Self.Parent := TWinControl(AOwner);
	Align := alTop;
	Width := 440;
  //Height := 80; // Era 200. Deve ser dinâmico
  Height := 24; // Era 200. Deve ser dinâmico
  Caption := '';
  BevelOuter := bvNone;
  //Color := clYellow;
  FPanelTitulo := TPanel.Create(Self);
  with FPanelTitulo do
  begin
  	Parent := Self;
    Height := 24;
    Align := alTop;
    BevelOuter := bvNone;
  end;
  FLabelTitulo := TLabel.Create(FPanelTitulo);
  with FLabelTitulo do
  begin
		Parent := FPanelTitulo;
    Align := alClient;
    Font.Color := clWhite;
    Color := clBtnShadow;
    Caption := '   '+ Field.DisplayText;
    LayOut := tlCenter;
  end;

  FCbxEOUCriterio := TComboBox.Create(FPanelTitulo);
  with FCbxEOUCriterio do
  begin
  	Parent := Self;
    Left := 288;
    Top := 1;
    Style := csDropDownList;
    Width := 41;
    prcPopulaItemsComboEOU(FCbxEOUCriterio);
		OnChange := VirtualOperatorChange;
    ItemIndex := 0;
		FCbxEOUCriterio.OnChange(FCbxEOUCriterio);    
  end;
  FBtInsCriterio := TBitBtn.Create(FPanelTitulo);
  with FBtInsCriterio do
  begin
  	Parent := FPanelTitulo;
    Top := 1;
    Left := 332;
    Height := 22;
    Width := 80;
    Caption := 'Incluir Critério';
    OnClick := VirtualIncluirCriterioClick;
  end;

  FScrBoxCriterios := TScrollBox.Create(Self);
  with FScrBoxCriterios do
  begin
  	Parent := Self;
  	Align := alClient;
    BorderStyle := bsNone;
    // BorderStyle := bsNone;
    // BevelOuter := bvNone;
  end;
end;

destructor TPanelCampo.Destroy;
begin
	FScrBoxCriterios.Free;
	FCbxEOUCriterio.Free;
  FBtInsCriterio.Free;
  FLabelTitulo.Free;

	FPanelTitulo.Free;
 inherited Destroy;
end;

procedure TPanelCampo.SetTitulo(Value: String);
begin
	if Value <> FTitulo then
  begin
  	FTitulo := Value;
	if FLabelTitulo <> nil then
  	FLabelTitulo.Caption := '   ' + Value;
  end;
end;

procedure TPanelCampo.RefreshState;
begin
	if Self.FField.Criterias.Count > 0 then
  	Self.FLabelTitulo.Color := clNavy
  else
  	Self.FLabelTitulo.Color := clBtnShadow;
end;

procedure TPanelCampo.VirtualOperatorChange(Sender: TObject);
begin
	if (Sender is TComboBox) then
  	Self.FField.FOperator := TOperator(TComboBox(Sender).ItemIndex);
end;

procedure TPanelCampo.RefreshHeight;
var
	i : Integer;
	iAltura : Integer;
  ScBox : TScrollBox;
begin
	// Reposiciona para corrigir o fato do Windows colocar o último container incluído como top
  iAltura := 0;
{
  if (TheCriteria <> nil) and
  	 (TheCriteria.FField <> nil) and
     (TheCriteria.FField.FPanelField <> nil) and
     (TheCriteria.FField.FPanelField.FScrBoxCriterios <> nil) then
	begin
}
	ScBox := Self.FScrBoxCriterios;
    for i := ScBox.ComponentCount -1 downto 0 do
    begin
      if ScBox.Components[i] is TWinControl then
      begin
        (ScBox.Components[i] as TWinControl).Align := alTop;
        iAltura := iAltura + (ScBox.Components[i] as TWinControl).Height;
      end;
    end;
    Self.Height := FPanelTitulo.Height + iAltura;

//    Self.FCriteria.FField.FPanelField.Height :=  Self.FCriteria.FField.FPanelField.FPanelTitulo.Height + iAltura;
//  end;
end;

{ TPanelCriteria }

constructor TPanelCriteria.Create(AOwner: TComponent;
  TheCriteria: TEmbCriteria);
var
  Shp : TShape;
  i : Integer;
  ScBox : TScrollBox;
  iAltura : Integer;
begin
	inherited Create(AOwner);

  Self.FCriteria := TheCriteria;

  // Manda todo mundo pra baixo
  if (TheCriteria <> nil) and
  	 (TheCriteria.FField <> nil) and
     (TheCriteria.FField.FPanelField <> nil) and
     (TheCriteria.FField.FPanelField.FScrBoxCriterios <> nil) then
	begin
  	ScBox := TheCriteria.FField.FPanelField.FScrBoxCriterios;
    for i := 0 to ScBox.ComponentCount -1 do
    begin
      if ScBox.Components[i] is TWinControl then
        (ScBox.Components[i] as TWinControl).Align := alNone;
    end;
  end;

	Parent := TWinControl(AOwner);
  Height := 28;
  Align := alTop;
  Color := clWhite; //RGB(Random(255),Random(255),Random(255));
  BevelOuter := bvNone;

  FCbxCriterio := TComboBox.Create(Self);
  with FCbxCriterio do
  begin
  	Parent 		:= Self;
    Left 			:= 12;
    Top 			:= 3;
    Style 		:= csDropDownList;
    Width 		:= 145;
    prcPopulaItemsComboCriterios(FCbxCriterio);
    OnChange := VirtualCriteriaChange;
    ItemIndex := 0;
  end;

  FEdtValue := TEdit.Create(Self);
  with FEdtValue do
  begin
  	Parent 	:= Self;
    Left 		:= 160;
    Top 		:= 3;
    Width 	:= 121;
		OnChange := VirtualValueChange;
    Text 		:= '';
  end;

  FCbxEOU := TComboBox.Create(Self);
  with FCbxEOU do
  begin
  	Parent 		:= Self;
    Left 			:= 288;
    Top 			:= 3;
    Style 		:= csDropDownList;
    Width 		:= 41;
    prcPopulaItemsComboEOU(FCbxEOU);
    OnChange := VirtualOperatorChange;
    ItemIndex := 0;
  end;

  FBtExcluir := TBitBtn.Create(Self);
  with FBtExcluir do
  begin
  	Parent := Self;
    Top := 3;
    Left := 332;
    Height := 23;
    Width := 80;
    Caption := 'Excluir Critério';
    OnClick := VirtualExcluirCriterioClick;
  end;

  FShpLinha := TShape.Create(Self);
  with FShpLinha do
  begin
  	Parent := Self;
  	Height := 1;
    Pen.Style := psDot;
    Pen.Color := clBtnShadow;
    Align := alBottom;
  end;
	TheCriteria.FField.FPanelField.RefreshHeight;
{
	// Reposiciona para corrigir o fato do Windows colocar o último container incluído como top
  iAltura := 0;
  if (TheCriteria <> nil) and
  	 (TheCriteria.FField <> nil) and
     (TheCriteria.FField.FPanelField <> nil) and
     (TheCriteria.FField.FPanelField.FScrBoxCriterios <> nil) then
	begin
  	ScBox := TheCriteria.FField.FPanelField.FScrBoxCriterios;
    for i := ScBox.ComponentCount -1 downto 0 do
    begin
      if ScBox.Components[i] is TWinControl then
      begin
        (ScBox.Components[i] as TWinControl).Align := alTop;
        iAltura := iAltura + (ScBox.Components[i] as TWinControl).Height;
      end;
    end;
    TheCriteria.FField.FPanelField.Height :=  TheCriteria.FField.FPanelField.FPanelTitulo.Height + iAltura;
  end;
}

end;

destructor TPanelCriteria.Destroy;
begin
  FCbxCriterio.Free;
  FCbxEOU.Free;
  FBtExcluir.Free;
  FShpLinha.Free;
  FEdtValue.Free;
  inherited;
end;

constructor TEmbCriterias.Create(ParentField: TEmbCriteriaField);
begin
	inherited Create;
  FParentField := ParentField;
end;

procedure TEmbCriterias.Delete(Index: Integer);
var
	C : TEmbCriteria;
  PanelField : TPanelCampo;
begin
	C := TEmbCriteria(Self[Index]);
  PanelField := C.FField.FPanelField;
  inherited Delete(Index);
  C.Destroy;
  if C <> nil then
  begin
  	C := nil;
  end;

  PanelField.RefreshHeight;
  Self.FParentField.FPanelField.RefreshState;
  Self.FParentField.FEmbCriteriaGroup.RefreshAll;
end;

procedure TEmbCriterias.RefreshStateAll;
var
	i : Integer;
	C: TEmbCriteria;
begin
	// Varrerá todos os critérios setando-os para operable ou não
  for i := 0 to Self.Count -1 do
  begin
  	C := TEmbCriteria(Self[i]);
    if C <> nil then
    	C.Operable := (i < (Self.Count -1) ); // Só é operável se igual ao último
  end;
end;

procedure TPanelCriteria.VirtualCriteriaChange(Sender: TObject);
begin
	if (Sender is TComboBox) then
  	Self.FCriteria.FCriteriaType := TCriteriaType(TComboBox(Sender).ItemIndex);
end;

procedure TPanelCriteria.VirtualExcluirCriterioClick(Sender: TObject);
var
	index : Integer;
begin
	index := Self.FCriteria.FField.FCriterias.IndexOf(Self.FCriteria);
  if index <> -1 then
		Self.FCriteria.FField.FCriterias.Delete(index);

	//ShowMessage(Format('IndexOf() = %d',[Self.FCriteria.FField.FCriterias.IndexOf(Self.FCriteria)]));


/////////////	Self.FCriteria.Destroy;
end;

procedure TPanelCriteria.VirtualOperatorChange(Sender: TObject);
begin
	if (Sender is TComboBox) then
  	Self.FCriteria.FOperator := TOperator(TComboBox(Sender).ItemIndex);
end;

procedure TPanelCriteria.VirtualValueChange(Sender: TObject);
begin
	if (Sender is TEdit) then
  	Self.FCriteria.FValue := TEdit(Sender).Text;
end;

end.


