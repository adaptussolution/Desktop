unit UFrmFiltro;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, Grids, DBGrids, StdCtrls,
  ExtCtrls, DB, Provider, DBClient, IBCustomDataSet,
  IBQuery, ActnList, Buttons, DBCtrls;

type
  TFrmFiltro = class(TForm)
    Panel1: TPanel;
    DBGrid1: TDBGrid;
    QuyCNSObjetos: TIBQuery;
    dsCNSObjetos: TDataSource;
    ActionList1: TActionList;
    ActFechar: TAction;
    ActSelecionar: TAction;
    actcns: TAction;
    DBNavigator1: TDBNavigator;
    edFiltro: TEdit;
    Tbobjetos: TClientDataSet;
    dspObjetos: TDataSetProvider;
    BtnSelecionar: TBitBtn;
    BitBtn1: TBitBtn;
    Label2: TLabel;
    chxDecrescente: TCheckBox;
    Panel2: TPanel;
    BitBtn2: TBitBtn;

    procedure FormCreate(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);

    function DefineOrdemConsulta(var Grid: TDBGrid; var tabela: TClientDataSet; idxCampo: Integer; decrescente: Boolean = false): Integer;
    function ValidaData(value: string): Boolean;
    function ValidaInteiro(value: string): Boolean;
    function ValidaDouble(value: string): Boolean;
    function Validacurrency(value: string): Boolean;

    function ValidaString(value: string): Boolean;

    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure DBGrid1TitleClick(Column: TColumn);
    procedure edFiltroChange(Sender: TObject);

    /// //Rotina de realizar busca de qual ser� a consulta
    procedure Busca(TipoPesquisa: string);
    procedure TbobjetosFilterRecord(DataSet: TDataSet; var Accept: Boolean);
    function VerificaNReal(valor: string): Boolean;
    procedure TbobjetosAfterScroll(DataSet: TDataSet);
    /// ///////////////
    procedure ProcurarCampo(valor: string);
    procedure ProcurarCampoLOCATE(valor: string);
    procedure edFiltroExit(Sender: TObject);
    procedure edFiltroKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BtnSelecionarClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure AbreForm(aClasseForm: TComponentClass; var aForm);

  protected
    PegaPosicao: Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
    idxColunaProcura: Integer;
    NomeCampoTotalizar: string;
    VTabela: string;
    vfiltragem: string;

  end;

var
  FrmFiltro: TFrmFiltro;
  Vfiltro: string;
  VCODVENDA_COMPROVANTE: Integer;
  VarColuna: Integer;
  VarCampo: string; // nome do campo para filtragem
  VarTipoCampo: Integer; // tipo do campo para filtragem
  VarPesquisa: string;
  VarNomeForm: string;

implementation

{$R *.dfm}

uses UMenu;

procedure TFrmFiltro.AbreForm(aClasseForm: TComponentClass; var aForm);
begin
  Application.CreateForm(aClasseForm, aForm);
  try
    TForm(aForm).ShowModal;
  finally
    FreeAndNil(TForm(aForm));
  end;
end;

procedure TFrmFiltro.BitBtn2Click(Sender: TObject);
var
  TELA: TForm;
begin

  if VarNomeForm <> '' then
  begin

    if UpperCase(VarNomeForm) = UpperCase('FRMCIDADE') then
    begin

    end;
    if UpperCase(VarNomeForm) = UpperCase('TMATRICULA') then
    begin

    end;
    if UpperCase(VarNomeForm) = UpperCase('TCOTACAO') then
    begin

    end;
    if UpperCase(VarNomeForm) = UpperCase('CAR_TCARTORIO') then
    begin

    end;
    if UpperCase(VarNomeForm) = UpperCase('TGRUPO_USUARIOS') then
    begin

    end;
    if UpperCase(VarNomeForm) = UpperCase('TFORMASPGTO') then
    begin

    end;
    if UpperCase(VarNomeForm) = UpperCase('TMARCAS') then
    begin

    end;
    if UpperCase(VarNomeForm) = UpperCase('TMEDIDAS') then
    begin

    end;
    if UpperCase(VarNomeForm) = UpperCase('TREPRESENTANTE') then
    begin

    end;
    if UpperCase(VarNomeForm) = UpperCase('TATIVIDADE') then
    begin

    end;
    if UpperCase(VarNomeForm) = UpperCase('TVENDEDOR') then
    begin

    end;
    if UpperCase(VarNomeForm) = UpperCase('TPRODUTO') then
    begin

    end;
    if UpperCase(VarNomeForm) = UpperCase('CAR_TPESSOA') then
    begin

    end;
    if UpperCase(VarNomeForm) = UpperCase('TPESSOA') then
    begin

    end;
    if UpperCase(VarNomeForm) = UpperCase('FRMCAR_TPROFISSAO') then
    begin

    end;

  end;

  try
    TELA := TForm(VarNomeForm).Create(Self);
    TELA.FormStyle := fsStayOnTop;
    TELA.ShowModal;

  finally

    Tbobjetos.Close;
    Tbobjetos.Open;

  end;


  // VarNomeForm;

end;

procedure TFrmFiltro.BtnSelecionarClick(Sender: TObject);
begin
  BtnSelecionar.ModalResult := mrOk;
end;

procedure TFrmFiltro.Busca(TipoPesquisa: string);
begin
  VarNomeForm := '';
  if TipoPesquisa = 'TPRODUTO' then
  begin
    Tbobjetos.Close;

    DBGrid1.Columns.Add;
    DBGrid1.Columns[0].FieldName := 'CODIGO';
    DBGrid1.Columns[0].Title.Caption := 'C�digo';
    DBGrid1.Columns[0].Width := 80;

    DBGrid1.Columns.Add;
    DBGrid1.Columns[1].FieldName := 'NOME';
    DBGrid1.Columns[1].Title.Caption := 'Nome do Produto';
    DBGrid1.Columns[1].Width := 250;


    with QuyCNSObjetos do
    begin
      Close;
      sql.Clear;
      sql.Add('SELECT A.CODIGO, A.NOME  FROM TPRODUTO A ');
      Open;
      Last;
      First;
    end;
    Tbobjetos.Open;
    VARNOMEFORM := 'TPRODUTO';
  end;
  //////////////////////////////////////////////////////////////////////////////////
  if TipoPesquisa = 'TCARDAPIO_DIA' then
  begin
    Tbobjetos.Close;

    DBGrid1.Columns.Add;
    DBGrid1.Columns[0].FieldName := 'CODIGO';
    DBGrid1.Columns[0].Title.Caption := 'C�digo';
    DBGrid1.Columns[0].Width := 80;

    DBGrid1.Columns.Add;
    DBGrid1.Columns[1].FieldName := 'DESCRICAO';
    DBGrid1.Columns[1].Title.Caption := 'Descri��o do Card�pio';
    DBGrid1.Columns[1].Width := 250;


    with QuyCNSObjetos do
    begin
      Close;
      sql.Clear;
      sql.Add('SELECT A.CODIGO, A.DESCRICAO  FROM TCARDAPIO_DIA A ');
      Open;
      Last;
      First;
    end;
    Tbobjetos.Open;
    VARNOMEFORM := 'TCARDAPIO_DIA';
  end;
  //////////////////////////////////////////////////////////////////////////////////
  if TipoPesquisa = 'TFUNCIONARIO' then
  begin
    Tbobjetos.Close;

    DBGrid1.Columns.Add;
    DBGrid1.Columns[0].FieldName := 'CODIGO';
    DBGrid1.Columns[0].Title.Caption := 'C�digo';
    DBGrid1.Columns[0].Width := 80;

    DBGrid1.Columns.Add;
    DBGrid1.Columns[1].FieldName := 'NOME';
    DBGrid1.Columns[1].Title.Caption := 'Nome do Funcion�rio';
    DBGrid1.Columns[1].Width := 250;


    with QuyCNSObjetos do
    begin
      Close;
      sql.Clear;
      sql.Add('SELECT A.CODIGO, A.NOME  FROM TFUNCIONARIO A ');
      Open;
      Last;
      First;
    end;
    Tbobjetos.Open;
    VARNOMEFORM := 'TFUNCIONARIO';
  end;
  
  ////////////////////////////////////////////////////////////////////////////////////
  if TipoPesquisa = 'TMATRICULA' then
  begin
    Tbobjetos.Close;

    DBGrid1.Columns.Add;
    DBGrid1.Columns[0].FieldName := 'CODIGOMAT';
    DBGrid1.Columns[0].Title.Caption := 'C�digo';
    DBGrid1.Columns[0].Width := 80;

    DBGrid1.Columns.Add;
    DBGrid1.Columns[1].FieldName := 'MATRICULA';
    DBGrid1.Columns[1].Title.Caption := 'Matricula';
    DBGrid1.Columns[1].Width := 80;

    DBGrid1.Columns.Add;
    DBGrid1.Columns[2].FieldName := 'COMPRADOR';
    DBGrid1.Columns[2].Title.Caption := 'Comprador';
    DBGrid1.Columns[2].Width := 220;

    DBGrid1.Columns.Add;
    DBGrid1.Columns[3].FieldName := 'VENDEDOR';
    DBGrid1.Columns[3].Title.Caption := 'Vendedor';
    DBGrid1.Columns[3].Width := 220;

    with QuyCNSObjetos do
    begin
      Close;
      sql.Clear;
      sql.Add('SELECT A.CODIGOMAT,A.MATRICULA,B.NOME AS COMPRADOR, C.NOME  AS VENDEDOR  FROM TMATRICULA A');
      sql.Add('LEFT OUTER JOIN tpessoa B ON (B.codigo = A.codcomprador)');
      sql.Add('LEFT OUTER JOIN TPESSOA C ON (C.CODIGO = A.CODVENDEDOR)');
      Open;
      Last;
      First;
    end;
    Tbobjetos.Open;
    VarNomeForm := 'TMATRICULA';
  end;


end;

procedure TFrmFiltro.DBGrid1DblClick(Sender: TObject);
begin
  BtnSelecionar.Click;
end;

procedure TFrmFiltro.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin

  if Tbobjetos.RecNo mod 2 = 0 then
  begin
    // DBGrid1.Canvas.Brush.Color := $00CCFFFF;
    DBGrid1.Canvas.Brush.Color := clWhite;
  end
  else
  begin
    DBGrid1.Canvas.Brush.Color := clBtnFace;
  end;

  TDBGrid(Sender).Canvas.font.Color := clBlack;
  // aqui � definida a cor da fonte
  if gdSelected in State then
    with (Sender as TDBGrid).Canvas do
    begin

      TDBGrid(Sender).Canvas.font.Style := [fsbold];
      TDBGrid(Sender).Canvas.font.Size := 8;
      TDBGrid(Sender).Canvas.font.Color := clWhite;

      // Brush.Color := clGrayText; // aqui � definida a cor do fundo
      Brush.Color := $00996600; // aqui � definida a cor do fundo
      FillRect(Rect);
    end;
  DBGrid1.DefaultDrawColumnCell(Rect, DataCol, Column, State);
end;

procedure TFrmFiltro.DBGrid1TitleClick(Column: TColumn);
begin
  { try
    // quando clica no titulo da coluna temos que recuperar o nome do campo para pesquisa
    // e mostrar em um label  para fica facil de compriender qual coluna esta na pesquisa.
    VarColuna := Column.ID - 1; // pega ID da coluna
    // coloca mostra qual campo foi selecionado
    // StaticText1.Caption := 'Campo da filtragem: ' + DBGrid1.Columns.Items
    // [VarColuna].Field.DisplayLabel; //DBGrid1.Columns[0].FieldName
    lbFiltro.Caption := 'Campo da filtragem: ' + DBGrid1.Columns[VarColuna]
    .Title.Caption;
    // lbFiltro.Caption := ''; // limpa o campo do texto para nova pesquisa
    edFiltro.Text := '';
    // pega nome do campo
    VarCampo := Column.FieldName;

    // pega tipo do campo
    // 1 String ; 2 Inteiro ; 3 Boolem ; 4 data

    VarTipoCampo := 1;
    if (DBGrid1.Columns.Items[VarColuna].Field.DataType = ftString) then
    VarTipoCampo := 1;

    if DBGrid1.Columns.Items[VarColuna].Field.DataType = ftInteger then
    VarTipoCampo := 2;
    if DBGrid1.Columns.Items[VarColuna].Field.DataType = ftBoolean then
    VarTipoCampo := 3;
    if DBGrid1.Columns.Items[VarColuna].Field.DataType = ftDate then
    VarTipoCampo := 4;

    edFiltro.SetFocus;
    except
    on e: exception do
    begin
    if pos('list index out', e.Message) > 0 then
    begin
    Application.MessageBox('N�o ha dados para filtragem!', 'Aten��o',
    MB_OK + MB_ICONERROR);
    Abort;
    end
    else
    begin
    Application.MessageBox(pchar('Erro: ' + e.Message), 'Aten��o',
    MB_OK + MB_ICONERROR);
    Abort;
    end;
    end;
    end;

  }

  /// ///////////////////////////////////
  ///

  VarCampo := Column.FieldName;

  DefineOrdemConsulta(DBGrid1, Tbobjetos, Column.Index, false);
  edFiltro.Color := $0080FFFF;
  edFiltro.SetFocus;

end;

/// ////////////////////////////////

procedure TFrmFiltro.edFiltroChange(Sender: TObject);
begin

  if (DBGrid1.Columns[idxColunaProcura].Field) = nil then
    Exit;

  try
    with DBGrid1.Columns[idxColunaProcura].Field do
    begin
      if DataType in [ftDate, ftDateTime] then
      begin
        if ValidaData(edFiltro.Text) then
        begin
          ProcurarCampo(edFiltro.Text);
        end;
      end
      else if DataType in [ftInteger] then
      begin

        if ValidaInteiro(edFiltro.Text) then
          // ProcurarCampo(edFiltro.Text);
          ProcurarCampoLOCATE(edFiltro.Text);

      end
      else if DataType in [ftFloat] then
      begin
        if ValidaDouble(edFiltro.Text) then
          ProcurarCampo(edFiltro.Text);
      end
      else if DataType in [ftCurrency] then
      begin
        if Validacurrency(edFiltro.Text) then
          ProcurarCampo(edFiltro.Text);
      end

      else if DataType in [ftWideString] then
      begin
        if ValidaString(edFiltro.Text) then
          // ProcurarCampoLOCATE(edFiltro.Text);
          ProcurarCampo(edFiltro.Text);
      end
      else
      begin
        ProcurarCampoLOCATE(edFiltro.Text);
      end;
    end;

  finally
    if Trim(edFiltro.Text) = '' then
      ProcurarCampo(edFiltro.Text);

  end;

end;

procedure TFrmFiltro.edFiltroExit(Sender: TObject);
begin
  edFiltro.Color := clWindow;
end;

procedure TFrmFiltro.edFiltroKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin

  if Tbobjetos.Active then
  begin
    if (Key = VK_DOWN) or (Key = VK_RIGHT) then
    begin

      Tbobjetos.Next;
      abort;

    end;

    if (Key = VK_UP) or (Key = VK_LEFT) then
    begin
      Tbobjetos.Prior;
      abort;
    end;

  end;

end;

procedure TFrmFiltro.FormCreate(Sender: TObject);
var
  A: array of string;
  i: Integer;
begin

  // Busca(VTabela);

  { if DBGrid1.Columns.Count > 0 then
    begin
    for i := 0 to DBGrid1.Columns.Count - 1 do
    if (DBGrid1.Columns[i].Field = nil) then
    ComboBox2.Items.Add('N�o definido')
    else
    ComboBox2.Items.Add(DBGrid1.Columns[i].Field.DisplayName);

    end;

    ComboBox2.ItemIndex := 1;

    { setlength(a, DBGrid1.Columns.count);
    for i := 0 to DBGrid1.Columns.count - 1 do
    BEGIN
    a[i] := DBGrid1.Columns.Items[i].FieldName;
    // a[i] := TBObjetos.FieldDefs.Items[i].DisplayName;
    ComboBox2.Items.Add(a[i])
    END; }

end;

procedure TFrmFiltro.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
  begin
    Close;
  end;

end;

procedure TFrmFiltro.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    BtnSelecionar.Click;

end;

procedure TFrmFiltro.FormShow(Sender: TObject);
begin
  Busca(VTabela);

  if Tbobjetos.RecordCount > 0 then
    Tbobjetos.First;
  DBGrid1.SetFocus;



    DefineOrdemConsulta(DBGrid1, Tbobjetos, 1, false);
    edFiltro.SetFocus;


  VarCampo := DBGrid1.Columns[1].Field.DisplayName;

  // chxDecrescente.Checked := false;
  // DefineOrdemConsulta(DBGrid1, Tbobjetos, 1, chxDecrescente.Checked);

end;

procedure TFrmFiltro.ProcurarCampo(valor: string);

begin

  if Tbobjetos.State in [dsInsert, dsEdit] then
    Exit
  else
  begin
    if edFiltro.Text = '' then
    begin

      Tbobjetos.First;
    end
    else
    begin
      // Tbobjetos.FindNearest([valor]); //retirado para substituir por locate

      { � apena exemplo esse
        Tbobjetos.Filtered := false;
        Tbobjetos.Filter := UpperCase(VarCampo) + ' like ' + QuotedStr('%' + UpperCase(edFiltro.Text) + '%');
        Tbobjetos.Filtered := true;
      }

      // Tbobjetos.FindNearest([valor]);

      Tbobjetos.Filtered := false;
      Tbobjetos.Filter := 'UPPER(' + VarCampo + ')' + ' like ' + UpperCase(QuotedStr('%' + edFiltro.Text + '%'));
      Tbobjetos.Filtered := true;
      Tbobjetos.Locate(VarCampo, edFiltro.Text, [loPartialKey]);


      // Tbobjetos.Locate(VarCampo, edFiltro.Text, [loCaseInsensitive, loPartialKey]);

    end;
  end;

end;

procedure TFrmFiltro.ProcurarCampoLOCATE(valor: string);
begin

  if Tbobjetos.State in [dsInsert, dsEdit] then
    Exit
  else
  begin
    if edFiltro.Text = '' then
    begin
      Tbobjetos.Close; // novo
      Tbobjetos.Open; // novo
      Tbobjetos.First;
    end
    else
    begin
      // Tbobjetos.FindNearest([valor]); //retirado para substituir por locate

      { � apena exemplo esse
        Tbobjetos.Filtered := false;
        Tbobjetos.Filter := UpperCase(VarCampo) + ' like ' + QuotedStr('%' + UpperCase(edFiltro.Text) + '%');
        Tbobjetos.Filtered := true;
      }

      Tbobjetos.Locate(VarCampo, edFiltro.Text, [loCaseInsensitive, loPartialKey]);

    end;
  end;
end;

procedure TFrmFiltro.TbobjetosAfterScroll(DataSet: TDataSet);
begin
  //
end;

procedure TFrmFiltro.TbobjetosFilterRecord(DataSet: TDataSet; var Accept: Boolean);
begin
  { try
    if VarTipoCampo = 1 then
    begin
    Accept := (pos(UpperCase(edFiltro.Text),
    UpperCase(Tbobjetos.FieldByName(VarCampo).AsString)) > 0);
    end;

    if ((VarTipoCampo = 2) and (VerificaNReal(edFiltro.Text))) then
    begin

    Accept := (Tbobjetos.FieldByName(VarCampo).AsInteger = StrToInt
    (edFiltro.Text));

    end;
    Except
    //
    end;
  }

end;

function TFrmFiltro.Validacurrency(value: string): Boolean;
var
  numero: currency;
begin
  Result := true;
  try
    numero := StrToCurr(value);
  except
    Result := false;
  end;

end;

function TFrmFiltro.ValidaData(value: string): Boolean;
var
  Data: TDateTime;
begin
  Result := true;
  try
    Data := StrToDate(value);
  except
    Result := false;
  end;

end;

function TFrmFiltro.ValidaDouble(value: string): Boolean;
var
  numero: Double;
begin
  Result := true;
  try
    numero := StrToFloat(value);
  except
    Result := false;
  end;

end;

function TFrmFiltro.ValidaInteiro(value: string): Boolean;
var
  numero: Integer;
begin
  Result := true;
  try
    numero := StrToInt(value);
  except
    Result := false;
  end;

end;

function TFrmFiltro.ValidaString(value: string): Boolean;
var
  VString: string;
begin
  Result := true;
  try
    VString := value;
  except
    Result := false;
  end;

end;

function TFrmFiltro.VerificaNReal(valor: string): Boolean;

var
  resultado: Boolean;
  VarReal: Real;
begin
  try
    VarReal := StrToFloat(valor);
    resultado := true;
  except
    resultado := false;
  end;
  Result := resultado;
end;

function TFrmFiltro.DefineOrdemConsulta(var Grid: TDBGrid; var tabela: TClientDataSet; idxCampo: Integer; decrescente: Boolean): Integer;
var
  i: Integer;
  Origem: string;
  VaiAbrir: Boolean;
  Evento: TDataSetNotifyEvent;

begin

  if (idxCampo = -1) then
  begin
    tabela.IndexFieldNames := '';
    tabela.IndexName := '';
    tabela.IndexDefs.Clear;
    Exit;
  end;

  try
    Screen.Cursor := crHourGlass;
    if tabela.Active then
    begin

      tabela.IndexFieldNames := '';
      tabela.IndexName := '';
      tabela.IndexDefs.Clear;

      if decrescente = true then
      begin
        tabela.AddIndex('DESC' + Grid.Columns[idxCampo].FieldName, Grid.Columns[idxCampo].FieldName, [ixDescending, ixCaseInsensitive]);
        tabela.IndexDefs.Add('DESC' + Grid.Columns[idxCampo].FieldName, Grid.Columns[idxCampo].FieldName, [ixDescending, ixCaseInsensitive]);
        tabela.IndexName := 'DESC' + Grid.Columns[idxCampo].FieldName;

      end
      else
      begin
        tabela.AddIndex('ASC' + Grid.Columns[idxCampo].FieldName, Grid.Columns[idxCampo].FieldName, []);
        tabela.IndexDefs.Add('ASC' + Grid.Columns[idxCampo].FieldName, Grid.Columns[idxCampo].FieldName, []);
        tabela.IndexName := 'ASC' + Grid.Columns[idxCampo].FieldName;

      end;
      tabela.First;

    end;

    idxColunaProcura := idxCampo;

    Result := idxCampo;
    chxDecrescente.Checked := decrescente;

  finally

    Screen.Cursor := crDefault;
  end;

end;

initialization

  RegisterClass(TFrmFiltro);

finalization

  UnRegisterClass(TFrmFiltro);

end.

