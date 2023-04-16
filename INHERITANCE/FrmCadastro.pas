unit FrmCadastro;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DBCtrls, ExtCtrls, ImgList, ActnList, Menus, ComCtrls, ToolWin,
  StdCtrls, Mask, Buttons, Grids, DBGrids, DB, IBCustomDataSet, IBQuery,
  Provider, DBClient, ib, AppEvent, Placemnt;

type
  TFrmCadastros = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ActionList1: TActionList;
    ImageList1: TImageList;
    actNovo: TAction;
    actGravar: TAction;
    actExcluir: TAction;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    actSair: TAction;
    BitBtn4: TBitBtn;
    PageControl1: TPageControl;
    TabManutencao: TTabSheet;
    TabConsulta: TTabSheet;
    DBGrid1: TDBGrid;
    actPesquisar: TAction;
    ToolButton2: TToolButton;
    QuyObjetos: TIBQuery;
    TBObjetos: TClientDataSet;
    DSPObjetos: TDataSetProvider;
    dsObjetos: TDataSource;
    QuyComandos: TIBQuery;
    actCancelar: TAction;
    BitBtn6: TBitBtn;
    DBNavigator1: TDBNavigator;
    PANEL4: TPanel;
    rdgFiltragem: TGroupBox;
    label1: TLabel;
    Label8: TLabel;
    lbEmOrdem: TLabel;
    edPesquisar: TEdit;
    chxDecrescente: TCheckBox;
    ComboBox2: TComboBox;
    spdIncluir: TSpeedButton;
    spdAlterar: TSpeedButton;
    spdExcluir: TSpeedButton;
    spdGravar: TSpeedButton;
    spdCancelar: TSpeedButton;
    spdSair: TSpeedButton;
    SpeedButton1: TSpeedButton;
    SpeedButton3: TSpeedButton;
    SpeedButton4: TSpeedButton;
    SpeedButton5: TSpeedButton;
    SpeedButton6: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actSairExecute(Sender: TObject);
    procedure actNovoExecute(Sender: TObject);
    procedure actGravarExecute(Sender: TObject);
    procedure actExcluirExecute(Sender: TObject);
    procedure ConfiguraControles;
    procedure TBObjetosAfterScroll(DataSet: TDataSet);
    procedure dsObjetosStateChange(Sender: TObject);
    procedure BitBtn6Click(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);

    procedure EnterEx(Sender: TObject);
    procedure ExitEx(Sender: TObject);
    procedure MUDAFOCO(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure edPesquisarChange(Sender: TObject);
    function DefineOrdemConsulta(var Grid: TDBGrid; var tabela: TClientDataSet; idxCampo: Integer; decrescente: Boolean = false): Integer;
    function ValidaData(value: string): Boolean;
    function ValidaInteiro(value: string): Boolean;
    function ValidaDouble(value: string): Boolean;
    function ValidaString(value: string): Boolean;

    procedure ProcurarCampo(valor: string);
    procedure ComboBox2Change(Sender: TObject);
    procedure DBGrid1DblClick(Sender: TObject);

    procedure DBGrid1TitleClick(Column: TColumn);
    procedure Button1Click(Sender: TObject);
    procedure actPesquisarExecute(Sender: TObject);
    procedure actCancelarExecute(Sender: TObject);
    procedure PageControl1Change(Sender: TObject);
    procedure TabConsultaShow(Sender: TObject);
    procedure AppEvents1ActiveControlChange(Sender: TObject);

    procedure MUDANCAFOCOTELA(Sender: TObject);
    procedure edPesquisarKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);

    procedure SetCtrlFocado(Focar: Boolean);
    procedure ChangeControl(Sender: TObject);
    procedure PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
    procedure LimpezaMemoria;

  protected
    PegaPosicao: Boolean;
  private
    { Private declarations }
  public
    { Public declarations }
    idxColunaProcura: Integer;
    NomeCampoTotalizar: string;

  end;

var
  FrmCadastros: TFrmCadastros;
  FCorFocado: TColor = $00FCC294;
  _FAlterado: Boolean = false;
  _FCorAntiga: TColor;
  _FControleAtivo: TWinControl = nil;

implementation

uses
  UDM_PRINCIPAL, UFrmFiltro;




{$R *.dfm}

procedure TFrmCadastros.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  LimpezaMemoria;
end;

procedure TFrmCadastros.FormCreate(Sender: TObject);
var
  a: array of string;
  i, J: Integer;
begin

  if DBGrid1.Columns.Count > 0 then
  begin
    for i := 0 to DBGrid1.Columns.Count - 1 do
      if (DBGrid1.Columns[i].Field = nil) then
        ComboBox2.Items.Add('Não definido')
      else
        ComboBox2.Items.Add(DBGrid1.Columns[i].Field.DisplayName);

  end;

  Screen.OnActiveControlChange := ChangeControl;

  PageControl1.ActivePageIndex := 1;

end;

procedure TFrmCadastros.FormKeyPress(Sender: TObject; var Key: Char);
var
  i: Integer;
begin
  begin

    if Key = #13 then // Se o comando for igual a enter
    begin
      Key := #0;
      Perform(WM_NEXTDLGCTL, 0, 0); // Para pular de campo em campo
    end;

    if Key = chr(27) then
    begin
      if (TBObjetos.State in [DSEDIT, DSINSERT]) then
      begin
        if (Application.MessageBox('Você está nomeio de uma Operação, deseja cancelar e sair?', 'Atenção', MB_YESNO + MB_ICONWARNING) = id_yes) then
        begin
          TBObjetos.Cancel;
          actSair.Execute;
        end;
      end
      else
        actSair.Execute;
    end;
  end;

  // MUDAFOCO;
end;

procedure TFrmCadastros.FormShow(Sender: TObject);
begin
  // MUDAFOCO(Sender);
  LimpezaMemoria;
end;

procedure TFrmCadastros.LimpezaMemoria;
var
  MainHandle: THandle;
begin
  //
  try
    MainHandle := OpenProcess(PROCESS_ALL_ACCESS, false, GetCurrentProcessId);
    SetProcessWorkingSetSize(MainHandle, $FFFFFFFF, $FFFFFFFF);
    CloseHandle(MainHandle);
  except

  end;
  Application.ProcessMessages;

end;

procedure TFrmCadastros.MUDAFOCO(Sender: TObject);
var
  i: Integer;
  edt: TEdit;
  edbt: TDBEdit;
begin
  { : muda a cor dos componentes }
  for i := 0 to componentCount - 1 do
  begin
    { : se for um Edit }
    if (Components[i] is TEdit) then
    begin
      edt := (Components[i] as TEdit);
      if edt.Tag = 0 then
      begin
        if edt.focused then
          (Sender as TEdit).Color := clYellow
        else
          (Sender as TEdit).Color := clWindow;
      end;

    end;

    { : se for um DBEdit }
    if (Components[i] is TDBEdit) then
    begin
      edbt := (Components[i] as TDBEdit);
      if edbt.Tag = 0 then
      begin

        if edbt.focused then
          edbt.Color := clYellow
        else
          edbt.Color := clWindow;

      end;
    end;
  end;

end;

procedure TFrmCadastros.MUDANCAFOCOTELA(Sender: TObject);
var
  i: Integer;
  Ed: TDBEdit;
  EDIT: TEdit;
begin
  { Percorre a matriz de componentes do form }
  for i := 0 to componentCount - 1 do
    { Se o componente é do tipo TEdit... }
    if Components[i] is TDBEdit then
    begin
      { Faz um type-casting pata o tipo TEdit }
      Ed := Components[i] as TDBEdit;

      { Se o Edit está com o foco... }
      if Ed.focused then
        Ed.Color := clYellow { Amarelo }
      else
        Ed.Color := clWhite; { Branco }

    end;

  if Components[i] is TEdit then
  begin
    EDIT := Components[i] as TEdit;

    if (EDIT.focused) then
      EDIT.Color := clYellow
    else
      EDIT.Color := clWhite;

  end;

end;

procedure TFrmCadastros.PageControl1Change(Sender: TObject);
begin

  if PageControl1.ActivePage = TabConsulta then
  begin
    DefineOrdemConsulta(DBGrid1, TBObjetos, 1, chxDecrescente.Checked);
    edPesquisar.SetFocus;
  end;
end;

procedure TFrmCadastros.PageControl1Changing(Sender: TObject; var AllowChange: Boolean);
begin
  AllowChange := not (TBObjetos.State in [DSEDIT, DSINSERT]);
end;

procedure TFrmCadastros.ProcurarCampo(valor: string);
begin

  if TBObjetos.State in [DSINSERT, DSEDIT] then
    EXIT
  else
  begin

    if edPesquisar.Text = '' then
      TBObjetos.First
    else
      TBObjetos.FindNearest([valor]);
  end;
end;

procedure TFrmCadastros.SetCtrlFocado(Focar: Boolean);
begin
  if (_FControleAtivo <> nil) then
  try
    if (_FControleAtivo is TCustomEdit) or (_FControleAtivo is TCustomComboBox) then
    begin
      if Focar then
      begin
        _FCorAntiga := TEdit(_FControleAtivo).Color;
        _FAlterado := true;
        TEdit(_FControleAtivo).Color := FCorFocado;
      end
      else
      begin
        TEdit(_FControleAtivo).Color := _FCorAntiga;
        _FAlterado := false;
      end;
    end;
  except
      // vai q o individuo já foi destruido!!!
  end;
end;

procedure TFrmCadastros.actSairExecute(Sender: TObject);
begin
  if (TBObjetos.State in [DSEDIT, DSINSERT]) then
  begin
    if (Application.MessageBox('Você está no meio de uma operação, se optar por continuar a operação será cancelada, deseja continuar?', 'Atenção', MB_YESNO + MB_ICONWARNING) = id_yes) then
    begin
      TBObjetos.Cancel;
    end
    else
      abort;

  end;

  Close;
end;

procedure TFrmCadastros.AppEvents1ActiveControlChange(Sender: TObject);
begin
  // MUDANCAFOCOTELA(Sender);
end;

procedure TFrmCadastros.BitBtn6Click(Sender: TObject);
begin
  TBObjetos.Cancel;
end;

procedure TFrmCadastros.Button1Click(Sender: TObject);
begin
  idxColunaProcura := 1;
end;

procedure TFrmCadastros.actNovoExecute(Sender: TObject);
begin
  PageControl1.ActivePage := TabManutencao;
  /// ///////////////////////////////////////////////////////////////////////////
  /// CONTROLE DE PERMISSAO DO SISTEMA
  //

  /// //////////////////////////////////////////////////////////////////////////

  if not TBObjetos.Active then
    TBObjetos.Open;

  TBObjetos.Append;

end;

procedure TFrmCadastros.actPesquisarExecute(Sender: TObject);
begin
  // if not(TBObjetos.State in [DSEDIT, DSINSERT]) then
  // TBObjetos.EDIT;

end;

procedure TFrmCadastros.actGravarExecute(Sender: TObject);
begin
  // BitBtn1.Enabled := True;
  // BitBtn2.Enabled := false;
  // BitBtn3.Enabled := True;

end;

procedure TFrmCadastros.actCancelarExecute(Sender: TObject);
begin
  if (Application.MessageBox('Deseja Cancelar a operação?', 'Atenção', MB_YESNO + MB_ICONWARNING) = id_yes) then
    TBObjetos.Cancel;

end;

procedure TFrmCadastros.actExcluirExecute(Sender: TObject);
begin

  if TBObjetos.RecordCount = 0 then
  begin
    Application.MessageBox('Não há registros para excluir!', 'Atenção', MB_OK + MB_ICONWARNING);
    if (TBObjetos.State in [dsBrowse]) then
      abort;
  end;

end;

procedure TFrmCadastros.ChangeControl(Sender: TObject);
begin
  if Application.Terminated then
    EXIT;

  if Screen.ActiveControl <> _FControleAtivo then
  begin
    if _FAlterado then
      SetCtrlFocado(false);

    _FControleAtivo := Screen.ActiveControl;
    SetCtrlFocado(true);
  end;
end;

procedure TFrmCadastros.ComboBox2Change(Sender: TObject);
begin
  DefineOrdemConsulta(DBGrid1, TBObjetos, ComboBox2.ItemIndex, chxDecrescente.Checked);
end;

procedure TFrmCadastros.ConfiguraControles;
begin
  DBNavigator1.Enabled := (TBObjetos.State in [dsBrowse]) and (PageControl1.ActivePageIndex in [TabConsulta.TabIndex, TabManutencao.TabIndex]);
  actNovo.Enabled := (TBObjetos.State in [dsBrowse, dsInactive]) and (PageControl1.ActivePageIndex in [TabConsulta.TabIndex, TabManutencao.TabIndex]);
  actGravar.Enabled := (TBObjetos.State in [DSINSERT, DSEDIT]) and (PageControl1.ActivePageIndex in [TabConsulta.TabIndex, TabManutencao.TabIndex]);

  actCancelar.Enabled := (TBObjetos.State in [DSINSERT, DSEDIT]) and (PageControl1.ActivePageIndex in [TabConsulta.TabIndex, TabManutencao.TabIndex]);

  actExcluir.Enabled := (TBObjetos.State in [dsBrowse]) and (PageControl1.ActivePageIndex in [TabConsulta.TabIndex, TabManutencao.TabIndex]);

  actSair.Enabled := true;

end;

procedure TFrmCadastros.DBGrid1DblClick(Sender: TObject);
begin
  // idxColunaProcura := DBGrid1.Columns.

  // ShowMessage(IntToStr(DBGrid1.Columns.Items[TBObjetos.IndexFieldCount].Field));

  PageControl1.ActivePage := TabManutencao;

end;

procedure TFrmCadastros.DBGrid1DrawColumnCell(Sender: TObject; const Rect: TRect; DataCol: Integer; Column: TColumn; State: TGridDrawState);
begin
  { if idxColunaProcura = DataCol then
    begin
    DBGrid1.Columns[DataCol].Color := clBtnFace;
    DBGrid1.Columns[DataCol].Font.Color := clBlack;
    end
    else if (gdSelected in State) then
    begin
    DBGrid1.Canvas.Brush.Color := clNavy;
    DBGrid1.Canvas.Font.Color := clWhite;
    DBGrid1.Canvas.FillRect(Rect);
    DBGrid1.DefaultDrawDataCell(Rect, Column.Field, State);
    end
    else
    begin
    DBGrid1.Canvas.Brush.Color := clWhite;
    DBGrid1.Canvas.Font.Color := clBlack;
    DBGrid1.Canvas.FillRect(Rect);
    DBGrid1.DefaultDrawDataCell(Rect, Column.Field, State);
    end;
  }

  if TBObjetos.RecNo mod 2 = 0 then
  begin
    DBGrid1.Canvas.Brush.Color := clWhite;
  end
  else
  begin
    DBGrid1.Canvas.Brush.Color := clBtnFace;
  end;

  TDBGrid(Sender).Canvas.font.Color := clBlack;
  // aqui é definida a cor da fonte
  { if gdSelected in State then
    with (Sender as TDBGrid).Canvas do
    begin
    Brush.Color := clMoneyGreen; // aqui é definida a cor do fundo
    FillRect(Rect);
    end;
  }
  DBGrid1.DefaultDrawColumnCell(Rect, DataCol, Column, State);

  if PegaPosicao then
  begin
    if Column.FieldName = NomeCampoTotalizar then
    begin
      PegaPosicao := false;
    end;
  end;

  if gdSelected in State then
    with (Sender as TDBGrid).Canvas do
    begin
      Brush.Color := clNavy; // aqui é definida a cor do fundo
      DBGrid1.Canvas.font.Color := clWhite;
      DBGrid1.Canvas.font.Style := [];
      DBGrid1.Canvas.FillRect(Rect);
      DBGrid1.DefaultDrawColumnCell(Rect, DataCol, Column, State);
    end;

  { If odd(TBObjetos.RecNo) then
    begin
    DBGrid1.Canvas.Font.Color := clBlack;
    DBGrid1.Canvas.Brush.Color := clBtnFace;
    end
    else
    begin
    DBGrid1.Canvas.Font.Color := clBlack;
    DBGrid1.Canvas.Brush.Color := clWhite;
    end;
    DBGrid1.Canvas.FillRect(Rect);
    DBGrid1.Canvas.TextOut(Rect.Left + 2, Rect.Top, Column.Field.AsString);

    TDBGrid(Sender).Canvas.Font.Color := clBlack;
    // aqui é definida a cor da fonte
    if gdSelected in State then
    with (Sender as TDBGrid).Canvas do
    begin
    Brush.Color := clMoneyGreen; // aqui é definida a cor do fundo
    FillRect(Rect);
    end;

    TDBGrid(Sender).DefaultDrawDataCell(Rect, TDBGrid(Sender).columns[DataCol]
    .Field, State);

  }
end;

procedure TFrmCadastros.DBGrid1TitleClick(Column: TColumn);
begin
  // DefineOrdemConsulta(DBGrid1, TBObjetos, DBGrid1.Columns.Items[DBGrid1.SelectedField.FieldNo].Index,
  // chxDecrescente.Checked)

  DefineOrdemConsulta(DBGrid1, TBObjetos, Column.Index, chxDecrescente.Checked);
  edPesquisar.SetFocus;
end;

function TFrmCadastros.DefineOrdemConsulta(var Grid: TDBGrid; var tabela: TClientDataSet; idxCampo: Integer; decrescente: Boolean): Integer;
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
    EXIT;
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
    ComboBox2.ItemIndex := idxColunaProcura;

    Result := idxCampo;
    chxDecrescente.Checked := decrescente;

  finally

    Screen.Cursor := crDefault;
  end;

end;

procedure TFrmCadastros.TabConsultaShow(Sender: TObject);
begin

  if (TBObjetos.State in [DSEDIT, DSINSERT]) then
  begin
    TBObjetos.Cancel;
  end;

  // edPesquisar.SetFocus;

end;

procedure TFrmCadastros.TBObjetosAfterScroll(DataSet: TDataSet);
begin
  ConfiguraControles;
end;

function TFrmCadastros.ValidaData(value: string): Boolean;
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

function TFrmCadastros.ValidaDouble(value: string): Boolean;
var
  Numero: Double;
begin
  Result := true;
  try
    Numero := StrToFloat(value);
  except
    Result := false;
  end;

end;

function TFrmCadastros.ValidaInteiro(value: string): Boolean;
var
  Numero: Integer;
begin
  Result := true;
  try
    Numero := StrToInt(value);
  except
    Result := false;
  end;

end;

function TFrmCadastros.ValidaString(value: string): Boolean;
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

procedure TFrmCadastros.dsObjetosStateChange(Sender: TObject);
begin
  ConfiguraControles;
end;

procedure TFrmCadastros.edPesquisarChange(Sender: TObject);
begin
  // edPesquisar.Color := clWindow;

  if (DBGrid1.Columns[idxColunaProcura].Field) = nil then
    EXIT;

  try
    with DBGrid1.Columns[idxColunaProcura].Field do
    begin
      if DataType in [ftDate, ftDateTime] then
      begin
        if ValidaData(edPesquisar.Text) then
          ProcurarCampo(edPesquisar.Text);
      end
      else if DataType in [ftInteger] then
      begin

        if ValidaInteiro(edPesquisar.Text) then
          ProcurarCampo(edPesquisar.Text);
      end
      else if DataType in [ftFloat] then
      begin
        if ValidaDouble(edPesquisar.Text) then
          ProcurarCampo(edPesquisar.Text);
      end
      else if DataType in [ftWideString] then
      begin
        if ValidaString(edPesquisar.Text) then
          ProcurarCampo(edPesquisar.Text);
      end
      else if DataType in [ftString] then
      begin
        if ValidaString(edPesquisar.Text) then
          ProcurarCampo(edPesquisar.Text);
      end;

    end;

  finally
    if Trim(edPesquisar.Text) = '' then
      ProcurarCampo(edPesquisar.Text);

  end;

end;

procedure TFrmCadastros.edPesquisarKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if (TBObjetos.Active) and (TBObjetos.State in [dsBrowse]) then
  begin
    if (Key = VK_DOWN) or (Key = VK_RIGHT) then
    // if (Key = VK_DOWN) then
    begin

      TBObjetos.Next;
      abort;
    end;

    if (Key = VK_UP) or (Key = VK_LEFT) then
    // if (Key = VK_UP) then
    begin
      TBObjetos.Prior;
      abort;
    end;
  end;

end;

procedure TFrmCadastros.EnterEx(Sender: TObject);
begin

  begin
    { : altera a cor do componente quando receber o foco }
    if (Sender is TEdit) then
      (Sender as TEdit).Color := clYellow;
    if (Sender is TMemo) then
      (Sender as TMemo).Color := clYellow;
    if (Sender is TDBEdit) then
      (Sender as TDBEdit).Color := clYellow;
    if (Sender is TdbMemo) then
      (Sender as TdbMemo).Color := clYellow;

  end;

end;

procedure TFrmCadastros.ExitEx(Sender: TObject);
begin
  { : altera a cor do componente quando sair o foco }
  if (Sender is TEdit) then
    (Sender as TEdit).Color := clWindow;
  if (Sender is TMemo) then
    (Sender as TMemo).Color := clWindow;
  { : altera a cor do componente quando sair o foco }
  if (Sender is TDBEdit) then
    (Sender as TDBEdit).Color := clWindow;
  if (Sender is TdbMemo) then
    (Sender as TdbMemo).Color := clWindow;

end;

end.

