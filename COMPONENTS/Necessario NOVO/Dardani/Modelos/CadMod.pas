unit CadMod;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Wwdbigrd, Menus, StdCtrls, ComCtrls, wwSpeedButton, wwDBNavigator, Db,
  ExtCtrls, wwclearpanel, Buttons, Grids, Wwdbgrid, Mask, LibApl,
  dbgrids, wwdbedit, Wwdotdot, Wwdbcomb, IBQuery, Printers, FMTBcd, SqlExpr,
  FolhaModelos;

type
  TFrmCadModelo = class(TForm)
    StBrBarraStatus: TStatusBar;
    PMnModelo: TPopupMenu;
    MnItmSalvaGrid: TMenuItem;
    MnItmCarregaGrid: TMenuItem;
    MnItmRefresh: TMenuItem;
    MnItm1: TMenuItem;
    MnItmContinua: TMenuItem;
    MnItm2: TMenuItem;
    MnItmPesquisaAvancada: TMenuItem;
    MnItmFiltro: TMenuItem;
    PnlRight: TPanel;
    BtBtnIncluir: TBitBtn;
    BtBtnVisualizar: TBitBtn;
    wDBNvgtrSuperior: TwwDBNavigator;
    nabPaginaAnterior: TwwNavButton;
    nabProximaPagina: TwwNavButton;
    nabSalvaBookMark: TwwNavButton;
    nabRestauraBookMark: TwwNavButton;
    wDBNvgtrInferior: TwwDBNavigator;
    nabPrimeiro: TwwNavButton;
    nabAnterior: TwwNavButton;
    nabProximo: TwwNavButton;
    nabUltimo: TwwNavButton;
    BtBtnListar: TBitBtn;
    BtBtnAlterar: TBitBtn;
    BtBtnFiltrar: TBitBtn;
    BtBtnExcluir: TBitBtn;
    ChckBxFiltro: TCheckBox;
    BtBtnFechar: TBitBtn;
    PgCntrlModelo: TPageControl;
    TbShtModelo: TTabSheet;
    TbShtConteudo: TTabSheet;
    LblPesConteudo: TLabel;
    SpdBtnDownConteudo: TSpeedButton;
    SpdBtnFiltrarConteudo: TSpeedButton;
    EdtPesConteudo: TEdit;
    PnlTopModelo: TPanel;
    LblNome: TLabel;
    LblPesModelo: TLabel;
    SpdBtnDownModelo: TSpeedButton;
    SpdBtnFiltrarModelo: TSpeedButton;
    EdtPesModelo: TEdit;
    PnlModelo: TPanel;
    PnlConteudo: TPanel;
    wDBGrdConteudo: TwwDBGrid;
    wDBGrdModelo: TwwDBGrid;
    TbShtDetalhe: TTabSheet;
    PnlDetalhe: TPanel;
    LblPesDetalhe: TLabel;
    SpdBtnFiltrarDetalhe: TSpeedButton;
    SpdBtnDownDetalhe: TSpeedButton;
    EdtPesDetalhe: TEdit;
    wDBGrdDetalhe: TwwDBGrid;
    TbShtRelacao: TTabSheet;
    PnlRelacao: TPanel;
    LblPesRelacao: TLabel;
    SpdBtnFiltrarRelacao: TSpeedButton;
    SpdBtnDownRelacao: TSpeedButton;
    EdtPesRelacao: TEdit;
    wDBGrdRelacao: TwwDBGrid;
    TbShtOrdena: TTabSheet;
    wDBGrdOrdena: TwwDBGrid;
    PnlOrdena: TPanel;
    LblPesOrdena: TLabel;
    SpdBtnFiltrarOrdena: TSpeedButton;
    SpdBtnDownOrdena: TSpeedButton;
    EdtPesOrdena: TEdit;
    BtPreview: TBitBtn;
    PrintDialog: TPrintDialog;
    qryMain: TSQLQuery;
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure EdtPesModeloEnter(Sender: TObject);
    procedure EdtPesModeloKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MnItmPesquisaAvancadaClick(Sender: TObject);
    procedure MnItmFiltroClick(Sender: TObject);
    procedure BtBtnFiltrarClick(Sender: TObject);
    procedure BtBtnFecharClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure BtBtnExcluirClick(Sender: TObject);
    procedure ChckBxFiltroClick(Sender: TObject);
    procedure EdtPesModeloExit(Sender: TObject);
    procedure MnItmContinuaClick(Sender: TObject);
    procedure MnItmSalvaGridClick(Sender: TObject);
    procedure MnItmCarregaGridClick(Sender: TObject);
    procedure MnItmRefreshClick(Sender: TObject);
    procedure SpdBtnFiltrarModeloClick(Sender: TObject);
    procedure PgCntrlModeloChange(Sender: TObject);
    procedure wDBGrdModeloMouseDown(Sender: TObject;
      Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure wDBGrdModeloCalcCellColors(Sender: TObject; Field: TField;
      State: TGridDrawState; Highlight: Boolean; AFont: TFont;
      ABrush: TBrush);
    procedure wDBGrdModeloCellChanged(Sender: TObject);
    procedure wDBGrdModeloDblClick(Sender: TObject);
    procedure wDBGrdModeloKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure wDBGrdModeloTitleButtonClick(Sender: TObject;
      AFieldName: String);
    procedure wDBGrdModeloDrawTitleCell(Sender: TObject; Canvas: TCanvas;
      Field: TField; Rect: TRect; var DefaultDrawing: Boolean);
    procedure BtBtnIncluirClick(Sender: TObject);
    procedure BtPreviewClick(Sender: TObject);
  private
    { Private declarations }
    Descending: Boolean;
    Indice: Integer;
    procedure PrepareSql(Sender: TOBject);
    procedure Config(Tipo: boolean = True);
    function LocateAvancado(Dataset: TDataset; Field, Elemento: string; Continuado: Boolean = False): Boolean;

  public
    { Public declarations }
    UltimaProcura: string;
    procedure FormOpen(Campo: string = ''; Conteudo: string = '');
  end;

var
  FrmCadModelo: TFrmCadModelo;
  FiltroAtual: TStringList;

implementation

uses
  FilGer, Lib_Geral, DatMod, MenPri, ManMod, uFrmManCon, ManDet, ManRel, ManMor,
  uDmFolha, ufrmDlgModeloMontagemVisual;

{$R *.DFM}

procedure TFrmCadModelo.FormOpen(Campo: string = ''; Conteudo: string = '');
begin
  OpenTransaction;
  Aplicacao.Tag := 0;
  Descending    := False;

  if StartQuery(DM.qryModDoc) then
  begin
    if StartQueryItens(DM.qryConteudo, 'FOL_MODELO_CONTEUDO.MODELO = :P1', 'FOL_MODELO_CONTEUDO.LINHA, FOL_MODELO_CONTEUDO.COLUNA', DM.qryModDoc.FieldByName('MODELO').AsString) then
    begin
      if StartQueryItens(DM.qryDetalhe, 'FOL_MODELO_DETALHE.MODELO = :P1', 'FOL_MODELO_DETALHE.COLUNA', DM.qryModDoc.FieldByName('MODELO').AsString) then
      begin
        if StartQueryItens(DM.qryRelacao, 'FOL_MODELO_VINCULO.MODELO = :P1', 'FOL_MODELO_VINCULO.MODELO', DM.qryModDoc.FieldByName('MODELO').AsString) then
        begin

          if StartQueryItens(DM.qryModOrdena, 'FOL_MODELO_ORDENACAO.MODELO = :P1', 'FOL_MODELO_ORDENACAO.MODELO, FOL_MODELO_ORDENACAO.ID', DM.qryModDoc.FieldByName('MODELO').AsString) then
          begin

            PgCntrlModelo.ActivePage     := TbShtModelo;
            DM.qryModDoc.OnPrepareSql    := PrepareSql;
            DM.qryConteudo.OnPrepareSQL  := PrepareSql;
            DM.qryDetalhe.OnPrepareSQL   := PrepareSql;
            DM.qryRelacao.OnPrepareSQL   := PrepareSql;
            DM.qryModOrdena.OnPrepareSQL := PrepareSql;

            ShowModal;

            DM.qryModDoc.OnPrepareSql    := nil;
            DM.qryConteudo.OnPrepareSQL  := nil;
            DM.qryDetalhe.OnPrepareSQL   := nil;
            DM.qryRelacao.OnPrepareSQL   := nil;
            DM.qryModOrdena.OnPrepareSQL := nil;

            TerminateQuery(DM.qryModOrdena);
          end;
          TerminateQuery(DM.qryRelacao);
        end;
        TerminateQuery(DM.qryDetalhe);
      end;
      TerminateQuery(DM.qryConteudo);
    end;
    TerminateQuery(DM.qryModDoc);
  end;
  CloseTransaction;
end;

procedure TFrmCadModelo.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  Action := caFree;
end;

procedure TFrmCadModelo.FormShow(Sender: TObject);
begin
  ConfiguraGrid(wDBGrdModelo);
  PgCntrlModeloChange(Self);
  Config;
end;

procedure TFrmCadModelo.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) then
    Close
  else if (Shift = [ssCtrl]) and (Key = Ord('F')) then
    MnItmFiltroClick(Self)
  else if ((Key = VK_RETURN) or (Key = VK_DOWN)) and not (ActiveControl is TwwDBGrid) then
    Perform(WM_NEXTDLGCTL, 0, 0)
  else if (Key = VK_UP) and not (ActiveControl is TwwDBGrid) then
    Perform(WM_NEXTDLGCTL, 1, 0)
  else
  begin
    MenuPrincipal.FormKeyDown(Sender, Key, Shift);
    Exit;
  end;
  Key := 0;
end;

procedure TFrmCadModelo.FormKeyPress(Sender: TObject; var Key: Char);
begin
  if (Key = #13) then
    Key := #0;
end;

procedure TFrmCadModelo.BtBtnFecharClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmCadModelo.BtBtnFiltrarClick(Sender: TObject);
begin
  Application.CreateForm(TFiltroGeral, FiltroGeral);
  FiltroGeral.FormOpen(FiltroAtual, 'FOL_MODELOS');
  ChckBxFiltro.OnClick := nil;
  ChckBxFiltro.Checked := FiltroAtual.Count > 0;
  ChckBxFiltro.OnClick := ChckBxFiltroClick;
  ChckBxFiltroClick(Self);
end;

procedure TFrmCadModelo.EdtPesModeloKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) then
  begin
    if (ActiveControl = wDBGrdModelo) then
      wDBGrdModelo.SetFocus;
    if (ActiveControl = wDBGrdConteudo) then
      wDBGrdConteudo.SetFocus;
  end
end;

procedure TFrmCadModelo.wDBGrdModeloMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  wDBGrdModeloCellChanged(Self);
end;

procedure TFrmCadModelo.wDBGrdModeloCalcCellColors(Sender: TObject;
  Field: TField; State: TGridDrawState; Highlight: Boolean; AFont: TFont;
  ABrush: TBrush);
begin
  if (gdFocused in State) then
  begin
    AFont.Color  := clBtnText;
    ABrush.Color := clBtnFace;
  end;
end;

procedure TFrmCadModelo.wDBGrdModeloCellChanged(Sender: TObject);
begin
  Config(False);
end;

procedure TFrmCadModelo.wDBGrdModeloDblClick(Sender: TObject);
begin
  if (PgCntrlModelo.ActivePage = TbShtModelo) then
  begin
    if (DM.qryModDoc.Tag > 1) then
    begin
      Aplicacao.Tag := 1;
      Close;
    end;
  end
  else if (PgCntrlModelo.ActivePage = TbShtConteudo) then
  begin
    if (DM.qryConteudo.Tag > 1) then
    begin
      Aplicacao.Tag := 1;
      Close;
    end;
  end
  else if (PgCntrlModelo.ActivePage = TbShtDetalhe) then
  begin
    if (DM.qryDetalhe.Tag > 1) then
    begin
      Aplicacao.Tag := 1;
      Close;
    end;
  end
  else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
  begin
    if (DM.qryRelacao.Tag > 1) then
    begin
      Aplicacao.Tag := 1;
      Close;
    end;
  end
  else if (PgCntrlModelo.ActivePage = TbShtOrdena) then
  begin
    if (DM.qryModOrdena.Tag > 1) then
    begin
      Aplicacao.Tag := 1;
      Close;
    end;
  end;
end;

procedure TFrmCadModelo.wDBGrdModeloKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if (PgCntrlModelo.ActivePage = TbShtModelo) then
  begin
    if ((Key = VK_LEFT) or (Key = VK_RIGHT) or (Key = VK_UP) or (Key = VK_DOWN)) and (wDBGrdModelo.SelectedList.Count > 0) then
    begin
      wDBGrdModelo.SelectedList.Clear;
      wDBGrdModelo.Refresh;
    end;
    if (Key = VK_RIGHT) and (wDBGrdModelo.SelectedIndex = (wDBGrdModelo.FieldCount - 1)) then
      wDBGrdModelo.SelectedIndex := 0
    else if (Key = VK_LEFT) and (wDBGrdModelo.SelectedIndex = 0) then
      wDBGrdModelo.SelectedIndex := wDBGrdModelo.FieldCount - 1
    else if (Key = VK_F7) and (DM.qryModDoc.Tag > 1) then
    begin
      Aplicacao.Tag := 1;
      Close;
    end
    else if (Key = Ord('P')) then
      EdtPesModelo.SetFocus
    else
      Exit;
    Key := 0;
  end
  else if (PgCntrlModelo.ActivePage = TbShtConteudo) then
  begin
    if ((Key = VK_LEFT) or (Key = VK_RIGHT) or (Key = VK_UP) or (Key = VK_DOWN)) and (wDBGrdConteudo.SelectedList.Count > 0) then
    begin
      wDBGrdConteudo.SelectedList.Clear;
      wDBGrdConteudo.Refresh;
    end;
    if (Key = VK_RIGHT) and (wDBGrdConteudo.SelectedIndex = (wDBGrdConteudo.FieldCount - 1)) then
      wDBGrdConteudo.SelectedIndex := 0
    else if (Key = VK_LEFT) and (wDBGrdConteudo.SelectedIndex = 0) then
      wDBGrdConteudo.SelectedIndex := wDBGrdConteudo.FieldCount - 1
    else if (Key = VK_F7) and (DM.qryConteudo.Tag > 1) then
    begin
      Aplicacao.Tag := 1;
      Close;
    end
    else if (Key = Ord('P')) then
      EdtPesConteudo.SetFocus
    else
      Exit;
    Key := 0;
  end
  else if (PgCntrlModelo.ActivePage = TbShtDetalhe) then
  begin
    if ((Key = VK_LEFT) or (Key = VK_RIGHT) or (Key = VK_UP) or (Key = VK_DOWN)) and (wDBGrdDetalhe.SelectedList.Count > 0) then
    begin
      wDBGrdDetalhe.SelectedList.Clear;
      wDBGrdDetalhe.Refresh;
    end;
    if (Key = VK_RIGHT) and (wDBGrdDetalhe.SelectedIndex = (wDBGrdDetalhe.FieldCount - 1)) then
      wDBGrdDetalhe.SelectedIndex := 0
    else if (Key = VK_LEFT) and (wDBGrdDetalhe.SelectedIndex = 0) then
      wDBGrdDetalhe.SelectedIndex := wDBGrdDetalhe.FieldCount - 1
    else if (Key = VK_F7) and (DM.qryDetalhe.Tag > 1) then
    begin
      Aplicacao.Tag := 1;
      Close;
    end
    else if (Key = Ord('P')) then
      EdtPesDetalhe.SetFocus
    else
      Exit;
    Key := 0;
  end
  else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
  begin
    if ((Key = VK_LEFT) or (Key = VK_RIGHT) or (Key = VK_UP) or (Key = VK_DOWN)) and (wDBGrdRelacao.SelectedList.Count > 0) then
    begin
      wDBGrdRelacao.SelectedList.Clear;
      wDBGrdRelacao.Refresh;
    end;
    if (Key = VK_RIGHT) and (wDBGrdRelacao.SelectedIndex = (wDBGrdRelacao.FieldCount - 1)) then
      wDBGrdRelacao.SelectedIndex := 0
    else if (Key = VK_LEFT) and (wDBGrdRelacao.SelectedIndex = 0) then
      wDBGrdRelacao.SelectedIndex := wDBGrdRelacao.FieldCount - 1
    else if (Key = VK_F7) and (DM.qryRelacao.Tag > 1) then
    begin
      Aplicacao.Tag := 1;
      Close;
    end
    else if (Key = Ord('P')) then
      EdtPesRelacao.SetFocus
    else
      Exit;
    Key := 0;
  end
  else if (PgCntrlModelo.ActivePage = TbShtOrdena) then
  begin
    if ((Key = VK_LEFT) or (Key = VK_RIGHT) or (Key = VK_UP) or (Key = VK_DOWN)) and (wDBGrdOrdena.SelectedList.Count > 0) then
    begin
      wDBGrdOrdena.SelectedList.Clear;
      wDBGrdOrdena.Refresh;
    end;
    if (Key = VK_RIGHT) and (wDBGrdOrdena.SelectedIndex = (wDBGrdOrdena.FieldCount - 1)) then
      wDBGrdOrdena.SelectedIndex := 0
    else if (Key = VK_LEFT) and (wDBGrdOrdena.SelectedIndex = 0) then
      wDBGrdOrdena.SelectedIndex := wDBGrdOrdena.FieldCount - 1
    else if (Key = VK_F7) and (DM.qryModOrdena.Tag > 1) then
    begin
      Aplicacao.Tag := 1;
      Close;
    end
    else if (Key = Ord('P')) then
      EdtPesOrdena.SetFocus
    else
      Exit;
    Key := 0;
  end;
end;

procedure TFrmCadModelo.wDBGrdModeloTitleButtonClick(Sender: TObject;
  AFieldName: String);
begin
  if (AfieldName = 'MODELO') then
    Indice := 0
  else if (AfieldName = 'DESCRICAO') then
    Indice := 1
  else
    Exit;
  if (Indice = 0) then
  begin
    if (wDBGrdModelo.GetActiveField.FieldName = 'MODELO') and not Descending then
    begin
      ChangeOrder(DM.qryModDoc, 'FOL_MODELO.MODELO descending');
      Descending := True;
    end
    else
    begin
      ChangeOrder(DM.qryModDoc, 'FOL_MODELO.MODELO');
      Descending := False;
    end;
    wDBGrdModelo.SetActiveField('MODELO');
  end
  else if (Indice = 1) then
  begin
    if (wDBGrdModelo.GetActiveField.FieldName = 'DESCRICAO') and not Descending then
    begin
      ChangeOrder(DM.qryModDoc, 'FOL_MODELO.DESCRICAO_UPPER descending');
      Descending := True;
    end
    else
    begin
      ChangeOrder(DM.qryModDoc, 'FOL_MODELO.DESCRICAO_UPPER');
      Descending := False;
    end;
    wDBGrdModelo.SetActiveField('DESCRICAO');
  end;
  EdtPesModelo.Clear;
end;

procedure TFrmCadModelo.wDBGrdModeloDrawTitleCell(Sender: TObject;
  Canvas: TCanvas; Field: TField; Rect: TRect;
  var DefaultDrawing: Boolean);
begin
  if ((Field.FieldName = 'MODELO') and (Indice = 0)) then
    wDBGrdModelo.Canvas.Font.Style := [fsBold]
  else if ((Field.FieldName = 'DESCRICAO') and (Indice = 1)) then
    wDBGrdModelo.Canvas.Font.Style := [fsBold]
  else
    wDBGrdModelo.Canvas.Font.Style := [];
end;

procedure TFrmCadModelo.MnItmPesquisaAvancadaClick(Sender: TObject);
begin
  MnItmPesquisaAvancada.Checked := not MnItmPesquisaAvancada.Checked;
  EdtPesModelo.Clear;
  EdtPesConteudo.Clear;
  EdtPesDetalhe.Clear;
  EdtPesRelacao.Clear;
  EdtPesOrdena.Clear;
  Config;
end;

procedure TFrmCadModelo.EdtPesModeloEnter(Sender: TObject);
begin
  EdtPesModelo.Clear;
  EdtPesConteudo.Clear;
  EdtPesDetalhe.Clear;
  EdtPesRelacao.Clear;
  EdtPesOrdena.Clear;
  Config(False);
end;

procedure TFrmCadModelo.FormActivate(Sender: TObject);
begin
  AjustaPosicaoDoFormulario(Self);
end;

procedure TFrmCadModelo.MnItmFiltroClick(Sender: TObject);
begin
  SpdBtnFiltrarModeloClick(Sender);
end;

procedure TFrmCadModelo.Config(Tipo: boolean = True);
begin
  ChckBxFiltro.Visible := (FiltroAtual.Count > 0);
  if (PgCntrlModelo.ActivePage = TbShtModelo) then
  begin
    SpdBtnDownModelo.Visible := not (Trim(EdtPesModelo.Text)='');
    if (Trim(EdtPesModelo.Text)='') then
    begin
      if MnItmPesquisaAvancada.Checked then
        LblPesModelo.Caption := '&Pesquisa avançada: (' + StringReplace(wDBGrdModelo.Columns[wDBGrdModelo.SelectedIndex].DisplayLabel, '~', ' ', [rfReplaceAll, rfIgnoreCase]) + ')'
      else
        LblPesModelo.Caption := '&Pesquisa: (' + StringReplace(wDBGrdModelo.Columns[wDBGrdModelo.SelectedIndex].DisplayLabel, '~', ' ', [rfReplaceAll, rfIgnoreCase]) + ')';
    end;
    StBrBarraStatus.Panels[1].Text  := 'Selecionados: ' + IntToStr(wDBGrdModelo.SelectedList.Count);
    StBrBarraStatus.Panels[0].Text  := '';
    StBrBarraStatus.Panels[1].Width := Canvas.TextWidth(StBrBarraStatus.Panels[1].Text) + 14;
    StBrBarraStatus.Panels[0].Width := Width - StBrBarraStatus.Panels[1].Width - 6;
    EdtPesModelo.Left             := LblPesModelo.Left + LblPesModelo.Width + 10;
    EdtPesModelo.Width            := SpdBtnDownModelo.Left + SpdBtnDownModelo.Width - EdtPesModelo.Left - iif((Trim(EdtPesModelo.Text)=''), 0, SpdBtnDownModelo.Width + 10);
    MnItmContinua.Enabled           := not (Trim(EdtPesModelo.Text)='');
    BtBtnVisualizar.Enabled         := not DM.qryModDoc.IsEmpty;
    //BtBtnIncluir.Enabled            := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Incluir');
    //BtBtnAlterar.Enabled            := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Alterar') and (not DM.qryModDoc.IsEmpty);
    //BtBtnExcluir.Enabled            := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Excluir') and (not DM.qryModDoc.IsEmpty);
    //BtBtnListar.Enabled             := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Listar') and (not DM.qryModDoc.IsEmpty);
    if Tipo and (not DM.qryModDoc.IsEmpty) then
      wDBGrdModelo.SetFocus
    else if Tipo then
      BtBtnIncluir.SetFocus;
  end
  else if (PgCntrlModelo.ActivePage = TbShtConteudo) then
  begin
    SpdBtnDownConteudo.Visible := not (Trim(EdtPesConteudo.Text)='');
    if (Trim(EdtPesConteudo.Text)='') then
    begin
      if MnItmPesquisaAvancada.Checked then
        LblPesConteudo.Caption  := '&Pesquisa avançada: (' + wDBGrdConteudo.Columns[wDBGrdConteudo.SelectedIndex].DisplayLabel + ')'
      else
        LblPesConteudo.Caption  := '&Pesquisa: (' + wDBGrdConteudo.Columns[wDBGrdConteudo.SelectedIndex].DisplayLabel + ')';
    end;
    StBrBarraStatus.Panels[1].Text  := 'Selecionados: ' + IntToStr(wDBGrdConteudo.SelectedList.Count);
    StBrBarraStatus.Panels[0].Text  := '';
    StBrBarraStatus.Panels[1].Width := Canvas.TextWidth(StBrBarraStatus.Panels[1].Text) + 14;
    StBrBarraStatus.Panels[0].Width := Width - StBrBarraStatus.Panels[1].Width - 6;
    EdtPesConteudo.Left         := LblPesConteudo.Left + LblPesConteudo.Width + 10;
    EdtPesConteudo.Width        := SpdBtnDownConteudo.Left + SpdBtnDownConteudo.Width - EdtPesConteudo.Left - iif((Trim(EdtPesConteudo.Text)=''), 0, SpdBtnDownConteudo.Width + 10);
    MnItmContinua.Enabled           := not (Trim(EdtPesConteudo.Text)='');
    BtBtnVisualizar.Enabled         := not DM.qryConteudo.IsEmpty;
    //BtBtnIncluir.Enabled            := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Incluir');
    //BtBtnAlterar.Enabled            := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Alterar') and (not DM.qryConteudo.IsEmpty);
    //BtBtnExcluir.Enabled            := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Excluir') and (not DM.qryConteudo.IsEmpty);
    //BtBtnListar.Enabled             := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Listar') and (not DM.qryConteudo.IsEmpty);
    if Tipo and (not DM.qryConteudo.IsEmpty) then
      wDBGrdConteudo.SetFocus
    else if Tipo then
      BtBtnIncluir.SetFocus;
  end
  else if (PgCntrlModelo.ActivePage = TbShtDetalhe) then
  begin
    SpdBtnDownDetalhe.Visible := not (Trim(EdtPesDetalhe.Text)='');
    if (Trim(EdtPesDetalhe.Text)='') then
    begin
      if MnItmPesquisaAvancada.Checked then
        LblPesDetalhe.Caption  := '&Pesquisa avançada: (' + wDBGrdDetalhe.Columns[wDBGrdDetalhe.SelectedIndex].DisplayLabel + ')'
      else
        LblPesDetalhe.Caption  := '&Pesquisa: (' + wDBGrdDetalhe.Columns[wDBGrdDetalhe.SelectedIndex].DisplayLabel + ')';
    end;
    StBrBarraStatus.Panels[1].Text  := 'Selecionados: ' + IntToStr(wDBGrdDetalhe.SelectedList.Count);
    StBrBarraStatus.Panels[0].Text  := '';
    StBrBarraStatus.Panels[1].Width := Canvas.TextWidth(StBrBarraStatus.Panels[1].Text) + 14;
    StBrBarraStatus.Panels[0].Width := Width - StBrBarraStatus.Panels[1].Width - 6;
    EdtPesDetalhe.Left              := LblPesDetalhe.Left + LblPesDetalhe.Width + 10;
    EdtPesDetalhe.Width             := SpdBtnDownDetalhe.Left + SpdBtnDownDetalhe.Width - EdtPesDetalhe.Left - iif((Trim(EdtPesDetalhe.Text)=''), 0, SpdBtnDownDetalhe.Width + 10);
    MnItmContinua.Enabled           := not (Trim(EdtPesDetalhe.Text)='');
    BtBtnVisualizar.Enabled         := not DM.qryDetalhe.IsEmpty;
    //BtBtnIncluir.Enabled            := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Incluir');
    //BtBtnAlterar.Enabled            := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Alterar') and (not DM.qryDetalhe.IsEmpty);
    //BtBtnExcluir.Enabled            := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Excluir') and (not DM.qryDetalhe.IsEmpty);
    //BtBtnListar.Enabled             := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Listar') and (not DM.qryDetalhe.IsEmpty);
    if Tipo and (not DM.qryDetalhe.IsEmpty) then
      wDBGrdDetalhe.SetFocus
    else if Tipo then
      BtBtnIncluir.SetFocus;
  end
  else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
  begin
    SpdBtnDownRelacao.Visible := not (Trim(EdtPesRelacao.Text)='');
    if (Trim(EdtPesRelacao.Text)='') then
    begin
      if MnItmPesquisaAvancada.Checked then
        LblPesRelacao.Caption  := '&Pesquisa avançada: (' + wDBGrdRelacao.Columns[wDBGrdRelacao.SelectedIndex].DisplayLabel + ')'
      else
        LblPesRelacao.Caption  := '&Pesquisa: (' + wDBGrdRelacao.Columns[wDBGrdRelacao.SelectedIndex].DisplayLabel + ')';
    end;
    StBrBarraStatus.Panels[1].Text  := 'Selecionados: ' + IntToStr(wDBGrdRelacao.SelectedList.Count);
    StBrBarraStatus.Panels[0].Text  := '';
    StBrBarraStatus.Panels[1].Width := Canvas.TextWidth(StBrBarraStatus.Panels[1].Text) + 14;
    StBrBarraStatus.Panels[0].Width := Width - StBrBarraStatus.Panels[1].Width - 6;
    EdtPesRelacao.Left              := LblPesRelacao.Left + LblPesRelacao.Width + 10;
    EdtPesRelacao.Width             := SpdBtnDownRelacao.Left + SpdBtnDownRelacao.Width - EdtPesRelacao.Left - iif((Trim(EdtPesRelacao.Text)=''), 0, SpdBtnDownRelacao.Width + 10);
    MnItmContinua.Enabled           := not (Trim(EdtPesRelacao.Text)='');
    ////////BtBtnVisualizar.Enabled         := False;
    ////////BtBtnAlterar.Enabled            := False;
    //BtBtnIncluir.Enabled            := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Incluir');
    //BtBtnExcluir.Enabled            := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Excluir') and (not DM.qryRelacao.IsEmpty);
    //BtBtnListar.Enabled             := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Listar') and (not DM.qryRelacao.IsEmpty);
    if Tipo and (not DM.qryRelacao.IsEmpty) then
      wDBGrdRelacao.SetFocus
    else if Tipo then
      BtBtnIncluir.SetFocus;
  end
  else if (PgCntrlModelo.ActivePage = TbShtOrdena) then
  begin
    SpdBtnDownOrdena.Visible := not (Trim(EdtPesOrdena.Text)='');
    if (Trim(EdtPesOrdena.Text)='') then
    begin
      if MnItmPesquisaAvancada.Checked then
        LblPesOrdena.Caption  := '&Pesquisa avançada: (' + wDBGrdOrdena.Columns[wDBGrdOrdena.SelectedIndex].DisplayLabel + ')'
      else
        LblPesOrdena.Caption  := '&Pesquisa: (' + wDBGrdOrdena.Columns[wDBGrdOrdena.SelectedIndex].DisplayLabel + ')';
    end;
    StBrBarraStatus.Panels[1].Text  := 'Selecionados: ' + IntToStr(wDBGrdOrdena.SelectedList.Count);
    StBrBarraStatus.Panels[0].Text  := '';
    StBrBarraStatus.Panels[1].Width := Canvas.TextWidth(StBrBarraStatus.Panels[1].Text) + 14;
    StBrBarraStatus.Panels[0].Width := Width - StBrBarraStatus.Panels[1].Width - 6;
    EdtPesOrdena.Left               := LblPesOrdena.Left + LblPesOrdena.Width + 10;
    EdtPesOrdena.Width              := SpdBtnDownOrdena.Left + SpdBtnDownOrdena.Width - EdtPesOrdena.Left - iif((Trim(EdtPesOrdena.Text)=''), 0, SpdBtnDownOrdena.Width + 10);
    MnItmContinua.Enabled           := not (Trim(EdtPesOrdena.Text)='');
    BtBtnVisualizar.Enabled         := not DM.qryModDoc.IsEmpty;
    //BtBtnIncluir.Enabled            := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Incluir');
    //BtBtnAlterar.Enabled            := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Alterar') and (not DM.qryModOrdena.IsEmpty);
    //BtBtnExcluir.Enabled            := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Excluir') and (not DM.qryModOrdena.IsEmpty);
    //BtBtnListar.Enabled             := ControleDeAcesso(MenuPrincipal.MnItmModelos, 'Listar') and (not DM.qryModOrdena.IsEmpty);
    if Tipo and (not DM.qryModOrdena.IsEmpty) then
      wDBGrdOrdena.SetFocus
    else if Tipo then
      BtBtnIncluir.SetFocus;
  end;
end;

procedure TFrmCadModelo.BtBtnExcluirClick(Sender: TObject);
var
  Contador: Integer;
begin
  if (PgCntrlModelo.ActivePage = TbShtModelo) then
  begin
    if (WaitMessage('Confirma a exclusão ' + iif(wDBGrdModelo.SelectedList.Count = 0, 'do registro corrente', iif(wDBGrdModelo.SelectedList.Count = 1, 'do registro selecionado', 'dos registros selecionados')) + '?', 'Exclusão', MB_YESNO + MB_DEFBUTTON1 + MB_ICONQUESTION) = IDYES) then
    begin
      SayMessage('Aguarde um momento. Realizando a exclusão.', StBrBarraStatus);
      CurrentTransaction.StartTransaction;
      if (wDBGrdModelo.SelectedList.Count = 0) then
      begin
        GravaLogFile(DM.qryModDoc, Caption, 'FOL_MODELO', DM.qryModDoc.FieldByName('MODELO').AsString, Exclusao);
        try
          DM.qryModDoc.Delete;
        except
          on E: Exception do
          begin
            SayMessage('', StBrBarraStatus);
            CurrentTransaction.Rollback;
            ShowSqlError(E, Caption);
            Exit;
          end;
        end;
      end
      else
      begin
        for Contador := 0 to (wDBGrdModelo.SelectedList.Count - 1) do
        begin
          GravaLogFile(DM.qryModDoc, Caption, 'FOL_MODELO', DM.qryModDoc.FieldByName('MODELO').AsString, Exclusao);
          try
            DM.qryModDoc.Delete;
          except
            on E: Exception do
            begin
              SayMessage('', StBrBarraStatus);
              CurrentTransaction.Rollback;
              ShowSqlError(E, Caption);
              Exit;
            end;
          end;
        end;
        wDBGrdModelo.SelectedList.Clear;
      end;
      CurrentTransaction.Commit;
      SayMessage('', StBrBarraStatus);
      Config;
    end;
  end
  else if (PgCntrlModelo.ActivePage = TbShtConteudo) then
  begin
    if (WaitMessage('Confirma a exclusão ' + iif(wDBGrdConteudo.SelectedList.Count = 0, 'do registro corrente', iif(wDBGrdConteudo.SelectedList.Count = 1, 'do registro selecionado', 'dos registros selecionados')) + '?', 'Exclusão', MB_YESNO + MB_DEFBUTTON1 + MB_ICONQUESTION) = IDYES) then
    begin
      SayMessage('Aguarde um momento. Realizando a exclusão.', StBrBarraStatus);
      CurrentTransaction.StartTransaction;
      if (wDBGrdConteudo.SelectedList.Count = 0) then
      begin
        GravaLogFile(DM.qryConteudo, Caption, 'FOL_MODELO_CONTEUDO', DM.qryConteudoMODELO.AsString + DM.qryConteudoORDEM.AsString + DM.qryConteudoLINHA.AsString + DM.qryConteudoCOLUNA.AsString, Exclusao);
        try
          DM.qryConteudo.Delete;
        except
          on E: Exception do
          begin
            SayMessage('', StBrBarraStatus);
            CurrentTransaction.Rollback;
            ShowSqlError(E, Caption);
            Exit;
          end;
        end;
      end
      else
      begin
        for Contador := 0 to (wDBGrdConteudo.SelectedList.Count - 1) do
        begin
          GravaLogFile(DM.qryConteudo, Caption, 'FOL_MODELO_CONTEUDO', DM.qryConteudoMODELO.AsString + DM.qryConteudoORDEM.AsString + DM.qryConteudoLINHA.AsString + DM.qryConteudoCOLUNA.AsString, Exclusao);
          try
            DM.qryConteudo.Delete;
          except
            on E: Exception do
            begin
              SayMessage('', StBrBarraStatus);
              CurrentTransaction.Rollback;
              ShowSqlError(E, Caption);
              Exit;
            end;
          end;
        end;
        wDBGrdConteudo.SelectedList.Clear;
      end;
      CurrentTransaction.Commit;
      SayMessage('', StBrBarraStatus);
      Config;
    end;
  end
  else if (PgCntrlModelo.ActivePage = TbShtDetalhe) then
  begin
    if (WaitMessage('Confirma a exclusão ' + iif(wDBGrdDetalhe.SelectedList.Count = 0, 'do registro corrente', iif(wDBGrdDetalhe.SelectedList.Count = 1, 'do registro selecionado', 'dos registros selecionados')) + '?', 'Exclusão', MB_YESNO + MB_DEFBUTTON1 + MB_ICONQUESTION) = IDYES) then
    begin
      SayMessage('Aguarde um momento. Realizando a exclusão.', StBrBarraStatus);
      CurrentTransaction.StartTransaction;
      if (wDBGrdDetalhe.SelectedList.Count = 0) then
      begin
        GravaLogFile(DM.qryDetalhe, Caption, 'FOL_MODELO_DETALHE', DM.qryDetalheMODELO.AsString + DM.qryDetalheORDEM.AsString + DM.qryDetalheCOLUNA.AsString, Exclusao);
        try
          DM.qryDetalhe.Delete;
        except
          on E: Exception do
          begin
            SayMessage('', StBrBarraStatus);
            CurrentTransaction.Rollback;
            ShowSqlError(E, Caption);
            Exit;
          end;
        end;
      end
      else
      begin
        for Contador := 0 to (wDBGrdDetalhe.SelectedList.Count - 1) do
        begin
          GravaLogFile(DM.qryDetalhe, Caption, 'FOL_MODELO_DETALHE', DM.qryDetalheMODELO.AsString + DM.qryDetalheORDEM.AsString + DM.qryDetalheCOLUNA.AsString, Exclusao);
          try
            DM.qryDetalhe.Delete;
          except
            on E: Exception do
            begin
              SayMessage('', StBrBarraStatus);
              CurrentTransaction.Rollback;
              ShowSqlError(E, Caption);
              Exit;
            end;
          end;
        end;
        wDBGrdDetalhe.SelectedList.Clear;
      end;
      CurrentTransaction.Commit;
      SayMessage('', StBrBarraStatus);
      Config;
    end;
  end
  else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
  begin
    if (WaitMessage('Confirma a exclusão ' + iif(wDBGrdRelacao.SelectedList.Count = 0, 'do registro corrente', iif(wDBGrdRelacao.SelectedList.Count = 1, 'do registro selecionado', 'dos registros selecionados')) + '?', 'Exclusão', MB_YESNO + MB_DEFBUTTON1 + MB_ICONQUESTION) = IDYES) then
    begin
      SayMessage('Aguarde um momento. Realizando a exclusão.', StBrBarraStatus);
      CurrentTransaction.StartTransaction;
      if (wDBGrdRelacao.SelectedList.Count = 0) then
      begin
        GravaLogFile(DM.qryDetalhe, Caption, 'FOL_MODELO_VINCULO', DM.qryRelacaoMODELO.AsString + DM.qryRelacaoCAMPOCONTEUDO.AsString + DM.qryRelacaoCAMPODETALHE.AsString, Exclusao);
        try
          DM.qryRelacao.Delete;
        except
          on E: Exception do
          begin
            SayMessage('', StBrBarraStatus);
            CurrentTransaction.Rollback;
            ShowSqlError(E, Caption);
            Exit;
          end;
        end;
      end
      else
      begin
        for Contador := 0 to (wDBGrdRelacao.SelectedList.Count - 1) do
        begin
          GravaLogFile(DM.qryDetalhe, Caption, 'FOL_MODELO_VINCULO', DM.qryRelacaoMODELO.AsString + DM.qryRelacaoCAMPOCONTEUDO.AsString + DM.qryRelacaoCAMPODETALHE.AsString, Exclusao);
          try
            DM.qryRelacao.Delete;
          except
            on E: Exception do
            begin
              SayMessage('', StBrBarraStatus);
              CurrentTransaction.Rollback;
              ShowSqlError(E, Caption);
              Exit;
            end;
          end;
        end;
        wDBGrdRelacao.SelectedList.Clear;
      end;
      CurrentTransaction.Commit;
      SayMessage('', StBrBarraStatus);
      Config;
    end;
  end
  else if (PgCntrlModelo.ActivePage = TbShtOrdena) then
  begin
    if (WaitMessage('Confirma a exclusão ' + iif(wDBGrdOrdena.SelectedList.Count = 0, 'do registro corrente', iif(wDBGrdOrdena.SelectedList.Count = 1, 'do registro selecionado', 'dos registros selecionados')) + '?', 'Exclusão', MB_YESNO + MB_DEFBUTTON1 + MB_ICONQUESTION) = IDYES) then
    begin
      SayMessage('Aguarde um momento. Realizando a exclusão.', StBrBarraStatus);
      CurrentTransaction.StartTransaction;
      if (wDBGrdOrdena.SelectedList.Count = 0) then
      begin
        GravaLogFile(DM.qryModOrdena, Caption, 'FOL_MODELO_ORDENACAO', DM.qryModOrdenaMODELO.AsString + DM.qryModOrdenaID.AsString, Exclusao);
        try
          DM.qryModOrdena.Delete;
        except
          on E: Exception do
          begin
            SayMessage('', StBrBarraStatus);
            CurrentTransaction.Rollback;
            ShowSqlError(E, Caption);
            Exit;
          end;
        end;
      end
      else
      begin
        for Contador := 0 to (wDBGrdOrdena.SelectedList.Count - 1) do
        begin
          GravaLogFile(DM.qryModOrdena, Caption, 'FOL_MODELO_ORDENACAO', DM.qryModOrdenaMODELO.AsString + DM.qryModOrdenaID.AsString, Exclusao);
          try
            DM.qryModOrdena.Delete;
          except
            on E: Exception do
            begin
              SayMessage('', StBrBarraStatus);
              CurrentTransaction.Rollback;
              ShowSqlError(E, Caption);
              Exit;
            end;
          end;
        end;
        wDBGrdOrdena.SelectedList.Clear;
      end;
      CurrentTransaction.Commit;
      SayMessage('', StBrBarraStatus);
      Config;
    end;
  end;
end;

procedure TFrmCadModelo.ChckBxFiltroClick(Sender: TObject);
begin
  try
    if (PgCntrlModelo.ActivePage = TbShtModelo) then
    begin
      wDBGrdModelo.SelectedList.Clear;
      DM.qryModDoc.Close;
      DM.qryModDoc.Unprepare;
      DM.qryModDoc.Prepare;
      DM.qryModDoc.Open;
    end
    else if (PgCntrlModelo.ActivePage = TbShtConteudo) then
    begin
      wDBGrdConteudo.SelectedList.Clear;
      DM.qryConteudo.Close;
      DM.qryConteudo.Unprepare;
      DM.qryConteudo.Prepare;
      DM.qryConteudo.Open;
    end
    else if (PgCntrlModelo.ActivePage = TbShtDetalhe) then
    begin
      wDBGrdDetalhe.SelectedList.Clear;
      DM.qryDetalhe.Close;
      DM.qryDetalhe.Unprepare;
      DM.qryDetalhe.Prepare;
      DM.qryDetalhe.Open;
    end
    else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
    begin
      wDBGrdRelacao.SelectedList.Clear;
      DM.qryRelacao.Close;
      DM.qryRelacao.Unprepare;
      DM.qryRelacao.Prepare;
      DM.qryRelacao.Open;
    end
    else if (PgCntrlModelo.ActivePage = TbShtOrdena) then
    begin
      wDBGrdOrdena.SelectedList.Clear;
      DM.qryModOrdena.Close;
      DM.qryModOrdena.Unprepare;
      DM.qryModOrdena.Prepare;
      DM.qryModOrdena.Open;
    end;
  except
    on E: Exception do ShowSqlError(E, Caption);
  end;
  Config;
end;

procedure TFrmCadModelo.EdtPesModeloExit(Sender: TObject);
var
  Registro: Integer;
begin
  if (PgCntrlModelo.ActivePage = TbShtModelo) then
  begin
    if (Trim(EdtPesModelo.Text)='') then
      Exit;
    SayMessage('Aguarde um momento. Localizando o registro...', StBrBarraStatus);
    DM.qryModDoc.DisableControls;
    Registro      := DM.qryModDoc.Recno;
    UltimaProcura := wDBGrdModelo.SelectedField.FieldName;
    if (MnItmPesquisaAvancada.Checked and LocateAvancado(DM.qryModDoc, wDBGrdModelo.SelectedField.FieldName, EdtPesModelo.Text)) or (not MnItmPesquisaAvancada.Checked and DM.qryModDoc.Locate(wDBGrdModelo.SelectedField.FieldName, EdtPesModelo.Text, [loCaseInsensitive, loPartialKey])) then
      SayMessage('', StBrBarraStatus)
    else
    begin
      SayMessage('', StBrBarraStatus);
      DM.qryModDoc.Recno := Registro;
      WaitMessage('Registro não encontrado.', Caption);
    end;
    DM.qryModDoc.EnableControls;
    wDBGrdModelo.SetFocus;
  end
  else if (PgCntrlModelo.ActivePage = TbShtConteudo) then
  begin
    if (Trim(EdtPesConteudo.Text)='') then
      Exit;
    SayMessage('Aguarde um momento. Localizando o registro...', StBrBarraStatus);
    DM.qryConteudo.DisableControls;
    Registro      := DM.qryConteudo.Recno;
    UltimaProcura := wDBGrdConteudo.SelectedField.FieldName;
    if (MnItmPesquisaAvancada.Checked and LocateAvancado(DM.qryConteudo, wDBGrdConteudo.SelectedField.FieldName, EdtPesConteudo.Text)) or (not MnItmPesquisaAvancada.Checked and DM.qryConteudo.Locate(wDBGrdConteudo.SelectedField.FieldName, EdtPesConteudo.Text, [loCaseInsensitive, loPartialKey])) then
      SayMessage('', StBrBarraStatus)
    else
    begin
      SayMessage('', StBrBarraStatus);
      DM.qryConteudo.Recno := Registro;
      WaitMessage('Registro não encontrado.', Caption);
    end;
    DM.qryConteudo.EnableControls;
    wDBGrdConteudo.SetFocus;
  end
  else if (PgCntrlModelo.ActivePage = TbShtDetalhe) then
  begin
    if (Trim(EdtPesDetalhe.Text)='') then
      Exit;
    SayMessage('Aguarde um momento. Localizando o registro...', StBrBarraStatus);
    DM.qryDetalhe.DisableControls;
    Registro      := DM.qryDetalhe.Recno;
    UltimaProcura := wDBGrdDetalhe.SelectedField.FieldName;
    if (MnItmPesquisaAvancada.Checked and LocateAvancado(DM.qryDetalhe, wDBGrdDetalhe.SelectedField.FieldName, EdtPesDetalhe.Text)) or (not MnItmPesquisaAvancada.Checked and DM.qryDetalhe.Locate(wDBGrdDetalhe.SelectedField.FieldName, EdtPesDetalhe.Text, [loCaseInsensitive, loPartialKey])) then
      SayMessage('', StBrBarraStatus)
    else
    begin
      SayMessage('', StBrBarraStatus);
      DM.qryDetalhe.Recno := Registro;
      WaitMessage('Registro não encontrado.', Caption);
    end;
    DM.qryDetalhe.EnableControls;
    wDBGrdDetalhe.SetFocus;
  end
  else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
  begin
    if (Trim(EdtPesRelacao.Text)='') then
      Exit;
    SayMessage('Aguarde um momento. Localizando o registro...', StBrBarraStatus);
    DM.qryRelacao.DisableControls;
    Registro      := DM.qryRelacao.Recno;
    UltimaProcura := wDBGrdRelacao.SelectedField.FieldName;
    if (MnItmPesquisaAvancada.Checked and LocateAvancado(DM.qryRelacao, wDBGrdRelacao.SelectedField.FieldName, EdtPesRelacao.Text)) or (not MnItmPesquisaAvancada.Checked and DM.qryRelacao.Locate(wDBGrdRelacao.SelectedField.FieldName, EdtPesRelacao.Text, [loCaseInsensitive, loPartialKey])) then
      SayMessage('', StBrBarraStatus)
    else
    begin
      SayMessage('', StBrBarraStatus);
      DM.qryRelacao.Recno := Registro;
      WaitMessage('Registro não encontrado.', Caption);
    end;
    DM.qryRelacao.EnableControls;
    wDBGrdRelacao.SetFocus;
  end
  else if (PgCntrlModelo.ActivePage = TbShtOrdena) then
  begin
    if (Trim(EdtPesOrdena.Text)='') then
      Exit;
    SayMessage('Aguarde um momento. Localizando o registro...', StBrBarraStatus);
    DM.qryModOrdena.DisableControls;
    Registro      := DM.qryModOrdena.Recno;
    UltimaProcura := wDBGrdOrdena.SelectedField.FieldName;
    if (MnItmPesquisaAvancada.Checked and LocateAvancado(DM.qryModOrdena, wDBGrdOrdena.SelectedField.FieldName, EdtPesOrdena.Text)) or (not MnItmPesquisaAvancada.Checked and DM.qryModOrdena.Locate(wDBGrdOrdena.SelectedField.FieldName, EdtPesOrdena.Text, [loCaseInsensitive, loPartialKey])) then
      SayMessage('', StBrBarraStatus)
    else
    begin
      SayMessage('', StBrBarraStatus);
      DM.qryModOrdena.Recno := Registro;
      WaitMessage('Registro não encontrado.', Caption);
    end;
    DM.qryModOrdena.EnableControls;
    wDBGrdOrdena.SetFocus;
  end;
  SayMessage('', StBrBarraStatus);
  Config;
end;

function TFrmCadModelo.LocateAvancado(Dataset: TDataset; Field, Elemento: string; Continuado: Boolean = False): Boolean;
var
  Registro: Integer;
begin
  Dataset.DisableControls;
  Registro := Dataset.Recno;
  if not Continuado and not DataSet.Bof then
    Dataset.First
  else
    Dataset.Next;
  Result := False;
  while not Dataset.Eof do
  begin
    if (At(UpperCase(Elemento), UpperCase(Dataset.FieldByName(Field).AsString)) > 0) then
    begin
      Result := True;
      Break;
    end;
    Dataset.Next;
  end;
  if not Result then
    Dataset.Recno := Registro;
  Dataset.EnableControls;
end;

procedure TFrmCadModelo.MnItmContinuaClick(Sender: TObject);
var
  Registro: Integer;
begin
  if (Trim(UltimaProcura)='') then
    Exit;
  SayMessage('Aguarde um momento. Localizando o registro...', StBrBarraStatus);
  if (PgCntrlModelo.ActivePage = TbShtModelo) then
  begin
    DM.qryModDoc.DisableControls;
    Registro := DM.qryModDoc.Recno;
    if (MnItmPesquisaAvancada.Checked and not LocateAvancado(DM.qryModDoc, UltimaProcura, EdtPesModelo.Text, True)) or (not MnItmPesquisaAvancada.Checked and not TIBQuery(DM.qryModDoc).LocateNext(UltimaProcura, EdtPesModelo.Text, [loCaseInsensitive, loPartialKey])) then
    begin
      DM.qryModDoc.Recno := Registro;
      SayMessage('', StBrBarraStatus);
      WaitMessage('Registro não encontrado.', Caption);
    end;
    DM.qryModDoc.EnableControls;
  end
  else if (PgCntrlModelo.ActivePage = TbShtConteudo) then
  begin
    DM.qryConteudo.DisableControls;
    Registro := DM.qryConteudo.Recno;
    if (MnItmPesquisaAvancada.Checked and not LocateAvancado(DM.qryConteudo, UltimaProcura, EdtPesConteudo.Text, True)) or (not MnItmPesquisaAvancada.Checked and not TIBQuery(DM.qryConteudo).LocateNext(UltimaProcura, EdtPesConteudo.Text, [loCaseInsensitive, loPartialKey])) then
    begin
      DM.qryConteudo.Recno := Registro;
      SayMessage('', StBrBarraStatus);
      WaitMessage('Registro não encontrado.', Caption);
    end;
    DM.qryConteudo.EnableControls;
  end
  else if (PgCntrlModelo.ActivePage = TbShtDetalhe) then
  begin
    DM.qryDetalhe.DisableControls;
    Registro := DM.qryDetalhe.Recno;
    if (MnItmPesquisaAvancada.Checked and not LocateAvancado(DM.qryDetalhe, UltimaProcura, EdtPesDetalhe.Text, True)) or (not MnItmPesquisaAvancada.Checked and not TIBQuery(DM.qryDetalhe).LocateNext(UltimaProcura, EdtPesDetalhe.Text, [loCaseInsensitive, loPartialKey])) then
    begin
      DM.qryDetalhe.Recno := Registro;
      SayMessage('', StBrBarraStatus);
      WaitMessage('Registro não encontrado.', Caption);
    end;
    DM.qryDetalhe.EnableControls;
  end
  else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
  begin
    DM.qryRelacao.DisableControls;
    Registro := DM.qryRelacao.Recno;
    if (MnItmPesquisaAvancada.Checked and not LocateAvancado(DM.qryRelacao, UltimaProcura, EdtPesRelacao.Text, True)) or (not MnItmPesquisaAvancada.Checked and not TIBQuery(DM.qryRelacao).LocateNext(UltimaProcura, EdtPesRelacao.Text, [loCaseInsensitive, loPartialKey])) then
    begin
      DM.qryRelacao.Recno := Registro;
      SayMessage('', StBrBarraStatus);
      WaitMessage('Registro não encontrado.', Caption);
    end;
    DM.qryRelacao.EnableControls;
  end
  else if (PgCntrlModelo.ActivePage = TbShtOrdena) then
  begin
    DM.qryModOrdena.DisableControls;
    Registro := DM.qryModOrdena.Recno;
    if (MnItmPesquisaAvancada.Checked and not LocateAvancado(DM.qryModOrdena, UltimaProcura, EdtPesOrdena.Text, True)) or (not MnItmPesquisaAvancada.Checked and not TIBQuery(DM.qryModOrdena).LocateNext(UltimaProcura, EdtPesOrdena.Text, [loCaseInsensitive, loPartialKey])) then
    begin
      DM.qryModOrdena.Recno := Registro;
      SayMessage('', StBrBarraStatus);
      WaitMessage('Registro não encontrado.', Caption);
    end;
    DM.qryModOrdena.EnableControls;
  end;
  SayMessage('', StBrBarraStatus)
end;

procedure TFrmCadModelo.MnItmSalvaGridClick(Sender: TObject);
begin
  if (PgCntrlModelo.ActivePage = TbShtModelo) then
    wDBGrdModelo.SaveToIniFile
  else if (PgCntrlModelo.ActivePage = TbShtConteudo) then
    wDBGrdConteudo.SaveToIniFile
  else if (PgCntrlModelo.ActivePage = TbShtDetalhe) then
    wDBGrdDetalhe.SaveToIniFile
  else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
    wDBGrdRelacao.SaveToIniFile
  else if (PgCntrlModelo.ActivePage = TbShtOrdena) then
    wDBGrdOrdena.SaveToIniFile;
end;

procedure TFrmCadModelo.MnItmCarregaGridClick(Sender: TObject);
begin
  if (PgCntrlModelo.ActivePage = TbShtModelo) then
    wDBGrdModelo.LoadFromIniFile
  else if (PgCntrlModelo.ActivePage = TbShtConteudo) then
    wDBGrdConteudo.LoadFromIniFile
  else if (PgCntrlModelo.ActivePage = TbShtDetalhe) then
    wDBGrdDetalhe.LoadFromIniFile
  else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
    wDBGrdRelacao.LoadFromIniFile
  else if (PgCntrlModelo.ActivePage = TbShtOrdena) then
    wDBGrdOrdena.LoadFromIniFile;
end;

procedure TFrmCadModelo.MnItmRefreshClick(Sender: TObject);
begin
  if (PgCntrlModelo.ActivePage = TbShtModelo) then
    DM.qryModDoc.Refresh
  else if (PgCntrlModelo.ActivePage = TbShtConteudo) then
    DM.qryConteudo.Refresh
  else if (PgCntrlModelo.ActivePage = TbShtDetalhe) then
    DM.qryDetalhe.Refresh
  else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
    DM.qryRelacao.Refresh
  else if (PgCntrlModelo.ActivePage = TbShtOrdena) then
    DM.qryModOrdena.Refresh;
end;

procedure TFrmCadModelo.PrepareSql(Sender: TOBject);
begin
  if (PgCntrlModelo.ActivePage = TbShtModelo) then
  begin
    DM.qryModDoc.SQLWhereItems.Clear;
    if ChckBxFiltro.Checked and (FiltroAtual.Count > 0) then
      DM.qryModDoc.SQLWhereItems.Text := StrTran(FiltroAtual.Text, #13 + #10, ' ');
  end
  else if (PgCntrlModelo.ActivePage = TbShtConteudo) then
  begin
    DM.qryConteudo.SQLWhereItems.Clear;
    if ChckBxFiltro.Checked and (FiltroAtual.Count > 0) then
      DM.qryConteudo.SQLWhereItems.Text := StrTran(FiltroAtual.Text, #13 + #10, ' ');
  end
  else if (PgCntrlModelo.ActivePage = TbShtDetalhe) then
  begin
    DM.qryDetalhe.SQLWhereItems.Clear;
    if ChckBxFiltro.Checked and (FiltroAtual.Count > 0) then
      DM.qryDetalhe.SQLWhereItems.Text := StrTran(FiltroAtual.Text, #13 + #10, ' ');
  end
  else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
  begin
    DM.qryRelacao.SQLWhereItems.Clear;
    if ChckBxFiltro.Checked and (FiltroAtual.Count > 0) then
      DM.qryRelacao.SQLWhereItems.Text := StrTran(FiltroAtual.Text, #13 + #10, ' ');
  end
  else if (PgCntrlModelo.ActivePage = TbShtOrdena) then
  begin
    DM.qryModOrdena.SQLWhereItems.Clear;
    if ChckBxFiltro.Checked and (FiltroAtual.Count > 0) then
      DM.qryModOrdena.SQLWhereItems.Text := StrTran(FiltroAtual.Text, #13 + #10, ' ');
  end;
end;

procedure TFrmCadModelo.SpdBtnFiltrarModeloClick(Sender: TObject);
var
  Elemento, Campo, Filtro, Ordem: string;
begin
  if (PgCntrlModelo.ActivePage = TbShtModelo) then
  begin
    Ordem := StrTran(DM.qryModDoc.SQLOrder.Text, 'order by ');
    if (Trim(EdtPesModelo.Text)='') then
      ExecuteQueryItens(DM.qryModDoc, '', Ordem)
    else
    begin
      if (wDBGrdModelo.SelectedField.FieldName = 'DESCRICAO') then
      begin
        Campo    := 'FOL_MODELO.DESCRICAO_UPPER';
        Elemento := Raw(EdtPesModelo.Text);
      end
      else
      begin
        Campo    := wDBGrdModelo.SelectedField.FieldName;
        Elemento := EdtPesModelo.Text;
      end;
      if MnItmPesquisaAvancada.Checked then
        Filtro := Campo + ' containing ' + #39 + Elemento + #39
      else
        Filtro := Campo + ' starting with ' + #39 + Elemento + #39;
      ExecuteQueryItens(DM.qryModDoc, Filtro, Ordem);
      EdtPesModelo.Text := '';
    end;
  end
  else if (PgCntrlModelo.ActivePage = TbShtConteudo) then
  begin
    Ordem := StrTran(DM.qryConteudo.SQLOrder.Text, 'order by ');
    if (Trim(EdtPesConteudo.Text)='') then
      ExecuteQueryItens(DM.qryConteudo, '', Ordem)
    else
    begin
      Campo    := wDBGrdConteudo.SelectedField.FieldName;
      Elemento := EdtPesConteudo.Text;
      if MnItmPesquisaAvancada.Checked then
        Filtro := Campo + ' containing ' + #39 + Elemento + #39
      else
        Filtro := Campo + ' starting with ' + #39 + Elemento + #39;
      ExecuteQueryItens(DM.qryConteudo, Filtro, Ordem);
      EdtPesConteudo.Text := '';
    end;
  end
  else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
  begin
    Ordem := StrTran(DM.qryDetalhe.SQLOrder.Text, 'order by ');
    if (Trim(EdtPesDetalhe.Text)='') then
      ExecuteQueryItens(DM.qryDetalhe, '', Ordem)
    else
    begin
      Campo    := wDBGrdDetalhe.SelectedField.FieldName;
      Elemento := EdtPesDetalhe.Text;
      if MnItmPesquisaAvancada.Checked then
        Filtro := Campo + ' containing ' + #39 + Elemento + #39
      else
        Filtro := Campo + ' starting with ' + #39 + Elemento + #39;
      ExecuteQueryItens(DM.qryDetalhe, Filtro, Ordem);
      EdtPesDetalhe.Text := '';
    end;
  end
  else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
  begin
    Ordem := StrTran(DM.qryRelacao.SQLOrder.Text, 'order by ');
    if (Trim(EdtPesRelacao.Text)='') then
      ExecuteQueryItens(DM.qryRelacao, '', Ordem)
    else
    begin
      Campo    := wDBGrdRelacao.SelectedField.FieldName;
      Elemento := EdtPesRelacao.Text;
      if MnItmPesquisaAvancada.Checked then
        Filtro := Campo + ' containing ' + #39 + Elemento + #39
      else
        Filtro := Campo + ' starting with ' + #39 + Elemento + #39;
      ExecuteQueryItens(DM.qryRelacao, Filtro, Ordem);
      EdtPesRelacao.Text := '';
    end;
  end
  else if (PgCntrlModelo.ActivePage = TbShtOrdena) then
  begin
    Ordem := StrTran(DM.qryModOrdena.SQLOrder.Text, 'order by ');
    if (Trim(EdtPesOrdena.Text)='') then
      ExecuteQueryItens(DM.qryModOrdena, '', Ordem)
    else
    begin
      Campo    := wDBGrdOrdena.SelectedField.FieldName;
      Elemento := EdtPesOrdena.Text;
      if MnItmPesquisaAvancada.Checked then
        Filtro := Campo + ' containing ' + #39 + Elemento + #39
      else
        Filtro := Campo + ' starting with ' + #39 + Elemento + #39;
      ExecuteQueryItens(DM.qryModOrdena, Filtro, Ordem);
      EdtPesOrdena.Text := '';
    end;
  end;
  Config;
end;

procedure TFrmCadModelo.PgCntrlModeloChange(Sender: TObject);
begin
  LblNome.Caption := DM.qryModDocMODELO.AsString + '  ' + DM.qryModDocDESCRICAO.AsString;
  if (PgCntrlModelo.ActivePage = TbShtModelo) then
  begin
    LblNome.Caption := '';
    wDBNvgtrSuperior.DataSource := DM.dasModelo;
    wDBNvgtrInferior.DataSource := DM.dasModelo;
  end
  else if (PgCntrlModelo.ActivePage = TbShtConteudo) then
  begin
    FiltroAtual.Clear;
    wDBGrdConteudo.OnCellChanged := nil;
    wDBGrdConteudo.OnCellChanged := nil;
    ExecuteQueryItens(DM.qryConteudo, 'FOL_MODELO_CONTEUDO.MODELO = :P1', 'FOL_MODELO_CONTEUDO.LINHA, FOL_MODELO_CONTEUDO.COLUNA', DM.qryModDocMODELO.AsString);
    wDBGrdConteudo.OnCellChanged := wDBGrdModeloCellChanged;
    wDBNvgtrSuperior.DataSource  := DM.dasConteudo;
    wDBNvgtrInferior.DataSource  := DM.dasConteudo;
  end
  else if (PgCntrlModelo.ActivePage = TbShtDetalhe) then
  begin
    FiltroAtual.Clear;
    wDBGrdDetalhe.OnCellChanged := nil;
    wDBGrdDetalhe.OnCellChanged := nil;
    ExecuteQueryItens(DM.qryDetalhe, 'FOL_MODELO_DETALHE.MODELO = :P1', 'FOL_MODELO_DETALHE.COLUNA', DM.qryModDocMODELO.AsString);
    wDBGrdDetalhe.OnCellChanged := wDBGrdModeloCellChanged;
    wDBNvgtrSuperior.DataSource  := DM.dasDetalhe;
    wDBNvgtrInferior.DataSource  := DM.dasDetalhe;
  end
  else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
  begin
    FiltroAtual.Clear;
    wDBGrdRelacao.OnCellChanged := nil;
    wDBGrdRelacao.OnCellChanged := nil;
    ExecuteQueryItens(DM.qryRelacao, 'FOL_MODELO_VINCULO.MODELO = :P1', 'FOL_MODELO_VINCULO.MODELO', DM.qryModDocMODELO.AsString);
    wDBGrdRelacao.OnCellChanged := wDBGrdModeloCellChanged;
    wDBNvgtrSuperior.DataSource  := DM.dasRelacao;
    wDBNvgtrInferior.DataSource  := DM.dasRelacao;
  end
  else if (PgCntrlModelo.ActivePage = TbShtOrdena) then
  begin
    FiltroAtual.Clear;
    wDBGrdOrdena.OnCellChanged := nil;
    wDBGrdOrdena.OnCellChanged := nil;
    ExecuteQueryItens(DM.qryModOrdena, 'FOL_MODELO_ORDENACAO.MODELO = :P1', 'FOL_MODELO_ORDENACAO.MODELO, FOL_MODELO_ORDENACAO.ID', DM.qryModDocMODELO.AsString);
    wDBGrdOrdena.OnCellChanged := wDBGrdModeloCellChanged;
    wDBNvgtrSuperior.DataSource  := DM.dasModOrdena;
    wDBNvgtrInferior.DataSource  := DM.dasModOrdena;
  end;
  Descending    := False;
  UltimaProcura := '';
  Indice        := 0;
  FiltroAtual.Clear;
  Config;
end;

procedure TFrmCadModelo.BtBtnIncluirClick(Sender: TObject);
begin
  if (PgCntrlModelo.ActivePage = TbShtModelo) then
  begin
    Application.CreateForm(TFrmManModelo, FrmManModelo);
    if (Sender = BtBtnIncluir) then
    begin
      FrmManModelo.Modo := Inclusao;
      FrmManModelo.FormOpen;
    end
    else
    begin
      if (Sender = BtBtnVisualizar) then
        FrmManModelo.Modo := Visualizacao
      else
        FrmManModelo.Modo := Alteracao;
      FrmManModelo.FormOpen;
    end;
    if (Sender <> BtBtnVisualizar) then
      DM.qryModDoc.Refresh;
    wDBGrdModelo.SetFocus;
  end
  else if (PgCntrlModelo.ActivePage = TbShtConteudo) then
  begin
    Application.CreateForm(TFrmManConteudo, FrmManConteudo);
    if (Sender = BtBtnIncluir) then
    begin
      FrmManConteudo.Modo := Inclusao;
      FrmManConteudo.FormOpen;
    end
    else
    begin
      if (Sender = BtBtnVisualizar) then
        FrmManConteudo.Modo := Visualizacao
      else
        FrmManConteudo.Modo := Alteracao;
      FrmManConteudo.FormOpen;
    end;
    if (Sender <> BtBtnVisualizar) then
      DM.qryConteudo.Refresh;
    wDBGrdConteudo.SetFocus;
  end
  else if (PgCntrlModelo.ActivePage = TbShtDetalhe) then
  begin
    Application.CreateForm(TFrmManDetalhe, FrmManDetalhe);
    if (Sender = BtBtnIncluir) then
    begin
      FrmManDetalhe.Modo := Inclusao;
      FrmManDetalhe.FormOpen;
    end
    else
    begin
      if (Sender = BtBtnVisualizar) then
        FrmManDetalhe.Modo := Visualizacao
      else
        FrmManDetalhe.Modo := Alteracao;
      FrmManDetalhe.FormOpen;
    end;
    if (Sender <> BtBtnVisualizar) then
      DM.qryDetalhe.Refresh;
    wDBGrdDetalhe.SetFocus;
  end
  else if (PgCntrlModelo.ActivePage = TbShtRelacao) then
  begin
    Application.CreateForm(TFrmManRelacao, FrmManRelacao);
    FrmManRelacao.Modo := Inclusao;
    FrmManRelacao.FormOpen;

    DM.qryRelacao.Refresh;
    wDBGrdRelacao.SetFocus;
  end
  else if (PgCntrlModelo.ActivePage = TbShtOrdena) then
  begin
    Application.CreateForm(TFrmManOrdena, FrmManOrdena);
    if (Sender = BtBtnIncluir) then
    begin
      FrmManOrdena.Modo := Inclusao;
      FrmManOrdena.FormOpen;
    end
    else
    begin
      if (Sender = BtBtnVisualizar) then
        FrmManOrdena.Modo := Visualizacao
      else
        FrmManOrdena.Modo := Alteracao;
      FrmManOrdena.FormOpen;
    end;
    if (Sender <> BtBtnVisualizar) then
      DM.qryModOrdena.Refresh;
    wDBGrdOrdena.SetFocus;
  end;
end;

procedure TFrmCadModelo.BtPreviewClick(Sender: TObject);
var
	iQtdItemDetalhe,
  iInicioDetalhe,
  iTotalColunas : Integer;
  sLinha : String;
  x, i, iCol, iLin : Integer;
  sCampo : String;
  iNumCopias : Integer;
  Impressora : TextFile;
  bImpressora : Boolean;

  L : TModelo;
  C: TCampoModelo;
begin
//	if not PrintDialog.Execute then Exit;
//  bImpressora := False;
  {
  if bImpressora then
		iNumCopias := PrintDialog.Copies
  else
  	iNumCopias := 1;

  }


  iNumCopias := 2;//PrintDialog.Copies;

	try
		{
    if bImpressora then
    	AssignPrn(Impressora)
    else
    	AssignFile(Impressora,'c:\impressao.txt');

  	Rewrite(Impressora);
    }
    
    with DmFolha.qryMain do
    begin
      Close;
      Sql.Clear;
      Sql.Add('SELECT     M.MODELO            ');
      Sql.Add('         , M.ITEMDETALHE       ');
      Sql.Add('         , M.INICIODETALHE     ');
      Sql.Add('         , M.TOTALCOLUNAS      ');
      Sql.Add('FROM       FOL_MODELO M        ');
      Sql.Add('WHERE      M.MODELO = :MODELO  ');
      ParamByName('MODELO').AsInteger := DM.dasModelo.DataSet.FieldByName('MODELO').AsInteger;
      Open;
      iQtdItemDetalhe := FieldByName('ITEMDETALHE').AsInteger;
      iInicioDetalhe 	:= FieldByName('INICIODETALHE').AsInteger;
      iTotalColunas 	:= FieldByName('TOTALCOLUNAS').AsInteger;
    end;
    L := TModelo.Create;
    L.QuantidadeItens := iQtdItemDetalhe;
    L.InicioDetalhe 	:= iInicioDetalhe;
    L.TotalColunas 		:= iTotalColunas;

    with DmFolha.qryMain do
    begin
      Close;
      Sql.Clear;
      // CAMPOS
      Sql.Add('SELECT     C.LINHA                         ');
      Sql.Add('         , C.COLUNA                        ');
      Sql.Add('         , C.TAMANHO                       ');
      Sql.Add('         , CAST(''C'' AS VARCHAR(1)) TIPO  ');
      Sql.Add('         , CAST(C.CAMPO AS VARCHAR(200)) CAMPO ');
      Sql.Add('         , C.DESCRICAO                     ');
      Sql.Add('FROM       FOL_MODELO_CONTEUDO C           ');
      Sql.Add('WHERE      C.MODELO = :MODELO              ');
      Sql.Add('                                           ');
      {
      Sql.Add('UNION ALL                                  ');
      // DETALHES
      Sql.Add('                                           ');
      Sql.Add('SELECT     M.INICIODETALHE LINHA           ');
      Sql.Add('         , D.COLUNA                        ');
      Sql.Add('         , D.TAMANHO                       ');
      Sql.Add('         , CAST(''D'' AS VARCHAR(1)) TIPO  ');
      Sql.Add('         , CAST(D.CAMPO AS VARCHAR(200)) CAMPO ');
      Sql.Add('         , C.DESCRICAO                     ');
      Sql.Add('FROM       FOL_MODELO M                    ');
      Sql.Add('INNER JOIN FOL_MODELO_DETALHE D            ');
      Sql.Add('ON         D.MODELO = M.MODELO             ');
      Sql.Add('WHERE      M.MODELO = :MODELO              ');
      Sql.Add('                                           ');
      Sql.Add('ORDER BY   1, 2                            ');
      }
      ParamByName('MODELO').AsInteger := DM.dasModelo.DataSet.FieldByName('MODELO').AsInteger;
      Open;

			//for x := 1 to iNumCopias do
      //begin
      	iLin := 0;
        First;
        sLinha := 'FLAG';
        i := 1;

        while not EOF do
        begin
        	if FieldByName('TIPO').AsString = 'C' then
          begin
            C 					:= TCampoModelo.Create;
            C.Id 				:= i;
            C.Linha 		:= FieldByName('LINHA').AsInteger;
            C.Coluna 		:= FieldByName('COLUNA').AsInteger;
            C.Tamanho 	:= FieldByName('TAMANHO').AsInteger;
            C.Nome 			:= FieldByName('DESCRICAO').AsString;
            C.Conteudo 	:= FieldByName('CAMPO').AsString;
            L.AdicionaCampoModelo(C);
          	inc(i);
          end;
        	Next;
        end;

        try
        	frmDlgModeloMontagemVisual := TfrmDlgModeloMontagemVisual.Cria(L, Self);
          frmDlgModeloMontagemVisual.Show;
        finally
        	//FreeAndNil(frmDlgModeloMontagemVisual);
        end;




        {
      	iLin := 0;
        First;
        sLinha := 'FLAG';
        while not EOF do
        begin
          // alcança a linha do registro
					while (iLin >= DmFolha.qryMain.FieldByName('LINHA').AsInteger) do
            WriteLn(Impressora, sLinha);
            sLinha := '';
            iCol := 0;
            inc(iLin);
          end;


          //while (DmFolha.qryMain.FieldByName('LINHA').AsInteger > iLin) do
          //begin
          //  WriteLn(Impressora, sLinha);
          //  sLinha := '';
          //  iCol := 0;
          //  inc(iLin);
          //end;


          while (DmFolha.qryMain.FieldByName('COLUNA').AsInteger > iCol) do
          begin
            sLinha := sLinha + ' ';
            inc(iCol);
          end;

          sCampo := '';

          // Traduzir o campo
          for i := 1 to DmFolha.qryMain.FieldByName('TAMANHO').AsInteger do
          begin
            sCampo := sCampo + 'X';
            inc(iCol);
          end;
          sLinha := sLinha + sCampo;
          Next;
        }
        end;

        //WriteLn(Impressora, ' ');
      //end;

    //end;
  finally
    ////////System.CloseFile(Impressora);
  end;
end;



initialization

  FiltroAtual := TStringList.Create;

end.


---------------------------
Serviço Mensageiro 
---------------------------
Mensagem de DESENV-05 a DESENV-02 em 30/6/2004 16:24:42

http://www.lagoinha.com/bibliaonline/Pag_search.asp?livroantigo=1&capituloantigo=6&versiculoantigo=&tipo=1&antigo=1
---------------------------
OK
---------------------------




SELECT DISTINCT FC.MATRICULA
         , FC.ANOMES
         , ADM.ADMISSAO
         , SER.NOME
         , SER.PIS
         , CAST(SER.CPF AS VARCHAR(11)) AS NUMEROCPF
         , LCL.DESCRICAO AS LOCAL_TRABALHO
         , RTRIM(CASE SUBSTRING(FC.ANOMES FROM 5 FOR 2)
                     WHEN 1 THEN 'Janeiro'
                     WHEN 2 THEN 'Fevereiro'
                     WHEN 3 THEN 'Março'
                     WHEN 4 THEN 'Abril'
                     WHEN 5 THEN 'Maio'
                     WHEN 6 THEN 'Junho'
                     WHEN 7 THEN 'Julho'
                     WHEN 8 THEN 'Agosto'
                     WHEN 9 THEN 'Setembro'
                     WHEN 10 THEN 'Outubro'
                     WHEN 11 THEN 'Novembro'
                     WHEN 12 THEN 'Dezembro'
                END) || '/' || SUBSTRING(FC.ANOMES FROM 1 FOR 4) AS REFERENCIA
         , FC.CBO
         , FC.SALARIO
         , N.DESCRICAO AS NIVEL
         , G.DESCRICAO AS GRAU
         , CRG.DESCRICAO AS CARGO
         , FIC.DOTACAO
         , UND.DESCRICAO AS DESCRICAOUNIDADE
         , SUBUND.DESCRICAO AS DESCRICAOSUBUNIDADE
         , RAW(PRJ.DESCRICAO) AS DESCRICAOPROJETO
         , BAN.DESCRICAO BANCO
         , BAN.BANCO CODIGOBANCO
         , AGE.AGENCIA
         , CAST(LTRIM(RTRIM(COALESCE(FC.CONTA_CORRENTE,''))) || '-' ||
                LTRIM(RTRIM(COALESCE(FC.DIGITO_CONTA_CORRENTE,'')))
           AS VARCHAR(20)
           ) AS CONTACORRENTE
         , FC.BASEPREVIDENCIA
         , FC.VALORPREVIDENCIA
         , FC.BASEFGTS
         , FC.VALORFGTS
         , FC.VALORIRRF IRRF
         , FC.FAIXAIRRF
         , SUBSTRING(EMP.EMPRESA FROM 1 FOR 2) || '.' ||
           SUBSTRING(EMP.EMPRESA FROM 3 FOR 3) || '.' ||
           SUBSTRING(EMP.EMPRESA FROM 6 FOR 3) || '/' ||
           SUBSTRING(EMP.EMPRESA FROM 9 FOR 4) || '-' ||
           SUBSTRING(EMP.EMPRESA FROM 13 FOR 2
           ) AS CGC
         , EMP.NOME AS NOMEEMPRESA
         , CAST( LTRIM(RTRIM(COALESCE(EMP.ENDERECO, ''))) || ', ' ||
                 LTRIM(RTRIM(COALESCE(EMP.NUMERO, ''))) || ' - ' ||
                 LTRIM(RTRIM(COALESCE(EMP.CIDADE, ''))) || '-' ||
                 COALESCE(EMP.ESTADO,'')
                 AS VARCHAR(80)
               ) AS ENDERECO
         , CAST(RTRIM(LTRIM(EMP.CIDADE)) || '-' || RTRIM(LTRIM(EMP.ESTADO)) AS VARCHAR(60) ) AS CIDADEESTADO
FROM       FOL_FICHAFINANCEIRA_CABEC FC
INNER JOIN FOL_FICHAFINANCEIRA FF
ON         FF.ANOMES = FC.ANOMES
AND        FF.MATRICULA = FC.MATRICULA
AND        FF.PAGTO = FC.PAGTO
AND        FF.SEQUENCIA = FC.SEQUENCIA
INNER JOIN FOL_ADMISSAO ADM
ON         ADM.MATRICULA = FC.MATRICULA
INNER JOIN FOL_SERVIDOR SER
ON         SER.SERVIDOR = ADM.SERVIDOR
INNER JOIN FOL_ADMISSAO_CARGO ADMCRG
ON         ADMCRG.MATRICULA = ADM.MATRICULA
AND        ADMCRG.DATA_INICIO = ( SELECT MIN(TB.DATA_INICIO)
                                  FROM   FOL_ADMISSAO_CARGO TB
                                  WHERE  TB.MATRICULA = ADM.MATRICULA
                                )
INNER JOIN FOL_GRAU G
ON         G.ID_GRAU = FC.ID_GRAU
INNER JOIN FOL_NIVEL N
ON         N.ID_NIVEL = FC.ID_NIVEL
INNER JOIN FOL_FUNCAO CRG
ON         CRG.FUNCAO = FC.FUNCAO
LEFT JOIN  FOL_AGENCIA AGE
ON         AGE.ID_AGENCIA = FC.ID_AGENCIA
LEFT JOIN  FOL_BANCO BAN
ON         BAN.BANCO = AGE.BANCO
INNER JOIN CTB_FICHA FIC
ON         FIC.ANUENIO = FC.ANUENIOCLS
AND        FIC.FICHA   = FC.FICHACLS
LEFT JOIN  PAD_UNIDADE UND
ON         UND.ID_PAD_UNIDADE       = FIC.ID_PAD_UNIDADE
LEFT JOIN  PAD_ORGAO ORG
ON         ORG.ORGAO                = UND.ORGAO
LEFT JOIN  PAD_ENTIDADE ENT
ON         ENT.ORGAO                = UND.ORGAO
AND        ENT.ENTIDADE             = UND.ENTIDADE
LEFT JOIN  PAD_SUBUNIDADE SUBUND
ON         SUBUND.ID_PAD_SUBUNIDADE = FIC.ID_PAD_SUBUNIDADE
LEFT JOIN  CTB_FUNCAO FUN
ON         FUN.FUNCAO               = FIC.FUNCAO
LEFT JOIN  CTB_SUBFUNCAO SUBFUN
ON         SUBFUN.SUBFUNCAO         = FIC.SUBFUNCAO
LEFT JOIN  CTB_PROGRAMA PRG
ON         PRG.QUADRIENIO           = FIC.QUADRIENIO
AND        PRG.PROGRAMA             = FIC.PROGRAMA
LEFT JOIN  CTB_ACAO ACA
ON         ACA.QUADRIENIO           = FIC.QUADRIENIO
AND        ACA.PROGRAMA             = FIC.PROGRAMA
AND        ACA.ACAO                 = FIC.ACAO
LEFT JOIN  CTB_PROJETO PRJ
ON         PRJ.ANUENIO              = FIC.ANUENIO
AND        PRJ.PROJETO              = FIC.PROJETO
LEFT JOIN  CTB_DESPESA DSP
ON         DSP.CONTA                = FIC.ELEMENTO
LEFT JOIN  FOL_LOCAL_TRABALHO LCL
ON         LCL.LOCALTRABALHO =  ADMCRG.LOCALTRABALHO
LEFT JOIN  SYS_EMPRESA EMP
ON         1 = 1
WHERE      FC.ANOMES = :P1
AND        FC.PAGTO = :P2
ORDER BY   SER.NOME
