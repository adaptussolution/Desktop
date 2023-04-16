unit URegisterAccess;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrmCadastro, DB, Provider, DBClient, IBCustomDataSet, IBQuery,
  ImgList, ActnList, StdCtrls, Grids, DBGrids, ComCtrls, DBCtrls, Buttons,
  ToolWin, ExtCtrls, Mask, UAccess, UAdaptusGrid, UMenuAccess;

type
  TfrmRegisterAccess = class(TFrmCadastros, IAccess, IMenuAccess)
    intgrfldQuyObjetosID_ACESSO: TIntegerField;
    QuyObjetosNOME: TIBStringField;
    TBObjetosID_ACESSO: TIntegerField;
    TBObjetosNOME: TStringField;
    pnlAcess: TPanel;
    lbl: TLabel;
    dbedtID_ACESSO: TDBEdit;
    dbedtNOME: TDBEdit;
    lbl1: TLabel;
    pnlAccessItem: TPanel;
    Label2: TLabel;
    pnlButtonsAction: TPanel;
    sbRigthArrow: TSpeedButton;
    sbRigthArrowAll: TSpeedButton;
    sbLeftArrowAll: TSpeedButton;
    sbLeftArrow: TSpeedButton;
    Panel3: TPanel;
    Label3: TLabel;
    DBGridAccessItem: TAdaptusGrid;
    DBGridNOAccessItem: TAdaptusGrid;
    tbAccessItem: TClientDataSet;
    dspAccessItem: TDataSetProvider;
    dsAccessItem: TDataSource;
    quyAccessItem: TIBQuery;
    quyAccessItemID_ACESSO_ITEM: TIntegerField;
    quyAccessItemID_ACESSO: TIntegerField;
    quyAccessItemNOME: TIBStringField;
    quyAccessItemFLAG: TIBStringField;
    tbAccessItemID_ACESSO_ITEM: TIntegerField;
    tbAccessItemID_ACESSO: TIntegerField;
    tbAccessItemNOME: TStringField;
    tbAccessItemFLAG: TStringField;
    quyNOAccessItem: TIBQuery;
    dspNOAccessItem: TDataSetProvider;
    tbNOAccessItem: TClientDataSet;
    dsNOAccessItem: TDataSource;
    quyNOAccessItemID_ACESSO_ITEM: TIntegerField;
    quyNOAccessItemID_ACESSO: TIntegerField;
    quyNOAccessItemNOME: TIBStringField;
    quyNOAccessItemFLAG: TIBStringField;
    tbNOAccessItemID_ACESSO_ITEM: TIntegerField;
    tbNOAccessItemID_ACESSO: TIntegerField;
    tbNOAccessItemNOME: TStringField;
    tbNOAccessItemFLAG: TStringField;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure TBObjetosAfterScroll(DataSet: TDataSet);
    procedure FormCreate(Sender: TObject);
    procedure actExcluirExecute(Sender: TObject);
    procedure actGravarExecute(Sender: TObject);
    procedure sbRigthArrowClick(Sender: TObject);
    procedure sbLeftArrowClick(Sender: TObject);
    procedure sbLeftArrowAllClick(Sender: TObject);
    procedure sbRigthArrowAllClick(Sender: TObject);
    procedure actNovoExecute(Sender: TObject);
    procedure actCancelarExecute(Sender: TObject);
    procedure dsObjetosStateChange(Sender: TObject);
  private
    { Private declarations }
    function validateFileds: Boolean;
    procedure OnGetMenu;
    procedure OnSucess(AStatus: string; AId: Integer);
    procedure OnFaild(AMsg: string);
    procedure AccessModification(ACDSAlt: TClientDataSet; ACDSDelete: TClientDataSet; ADelete: boolean = true; AFLAG: string = 'Y');
  public
    { Public declarations }
  end;

var
  frmRegisterAccess: TfrmRegisterAccess;

implementation

uses
  UDM_PRINCIPAL;

{$R *.dfm}

procedure TfrmRegisterAccess.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  TAccess.GetInstance.RemListener(self);
  DM_PRINCIPAL.FMenuAccess.RemListener(Self);
  Action := caFree;
  frmRegisterAccess := NIL;
end;

procedure TfrmRegisterAccess.TBObjetosAfterScroll(DataSet: TDataSet);
begin
  inherited;
  try
    if not tbAccessItem.IsEmpty then
      tbAccessItem.EmptyDataSet;

    tbAccessItem.Close;
    TAccess.GetInstance.getItens(quyAccessItem, TBObjetosID_ACESSO.AsInteger, 'Y');
    tbAccessItem.Open;

    if not tbNOAccessItem.IsEmpty then
      tbNOAccessItem.EmptyDataSet;

    tbNOAccessItem.Close;
    TAccess.GetInstance.getItens(quyNOAccessItem, TBObjetosID_ACESSO.AsInteger, 'N');
    tbNOAccessItem.Open;
  except
    on e: Exception do
    begin
      Application.MessageBox(PCHAR('Ocorreu um erro!' + e.Message), 'Atenção', mb_ok + MB_ICONERROR);
      abort;
    end;
  end;

end;

procedure TfrmRegisterAccess.FormCreate(Sender: TObject);
begin
  inherited;
  TAccess.GetInstance.AddListener(self);
  DM_PRINCIPAL.FMenuAccess.AddListener(Self);
end;

procedure TfrmRegisterAccess.OnFaild(AMsg: string);
begin
  Application.MessageBox(PCHAR(AMsg), 'Atenção', mb_ok + MB_ICONERROR);
  abort;
end;

procedure TfrmRegisterAccess.OnSucess(AStatus: string; AId: Integer);
begin
  if AStatus = 'D' then
    TBObjetos.Delete
  else
  begin
    TBObjetosID_ACESSO.AsInteger := AId;

    if TBObjetos.State in [DSINSERT] then
      Application.MessageBox('Cadastro realizado com sucesso!', 'Atenção', mb_ok + MB_ICONWARNING)
    else
      Application.MessageBox('Alteração realizada com sucesso!', 'Atenção', mb_ok + MB_ICONWARNING);

    TBObjetos.Post;
    DM_PRINCIPAL.FMenuAccess.fUserLogged := False;
    try
      with QuyComandos do
      begin
        close;
        sql.Clear;
        sql.add('select LOGIN from TB_USUARIO where ID_ACESSO = :ID_ACESSO');
        ParamByName('ID_ACESSO').AsInteger := AId;
        Open;
        First;
        while not Eof do
        begin
          if FieldByName('LOGIN').AsString = DM_PRINCIPAL.FParameters.Login then
          begin
            DM_PRINCIPAL.FMenuAccess.fUserLogged := True;
            Exit;
          end;
          Next;
        end;
      end;
    finally
      DM_PRINCIPAL.FMenuAccess.getId(QuyComandos, AId);
      PageControl1.TabIndex := 0;
    end;
  end;
end;

procedure TfrmRegisterAccess.actExcluirExecute(Sender: TObject);
begin
  inherited;
  if (Application.MessageBox('Deseja Realmente Excluir?', 'Atenção', MB_YESNO + MB_ICONWARNING) = id_yes) then
  begin
    TAccess.GetInstance.deleteItens(TBObjetosID_ACESSO.AsInteger);
    TAccess.GetInstance.delete(TBObjetosID_ACESSO.AsInteger);
  end;
end;

function TfrmRegisterAccess.validateFileds: Boolean;
begin
  Result := False;
  dbedtID_ACESSO.SetFocus;
  if TBObjetosNOME.AsString = '' then
  begin
    Application.MessageBox('POR FAVOR INFORME O NOME!', 'ATENÇÃO', MB_OK + MB_ICONWARNING);
    dbedtNOME.SetFocus;
    Result := True;
    Exit;
  end;
end;

procedure TfrmRegisterAccess.actGravarExecute(Sender: TObject);
var
  xAccessOBJECT: TAccessOBJECT;
  xAccessItemOBJECT: TAccessItemOBJECT;
  xStatus: string;
begin
  inherited;
  if validateFileds then
    Exit;

  if TBObjetos.State in [DSINSERT, DSEDIT] then
  begin
    if TBObjetos.State in [DSINSERT] then
      xStatus := 'I'
    else
      xStatus := 'U';

    xAccessOBJECT := TAccessOBJECT.Create;
    xAccessOBJECT.ID_Access := TBObjetosID_ACESSO.AsInteger;
    xAccessOBJECT.NOME := TBObjetosNOME.AsString;
    TAccess.GetInstance.save(xAccessOBJECT, xStatus);

    tbNOAccessItem.First;
    while not tbNOAccessItem.Eof do
    begin
      xAccessItemOBJECT := TAccessItemOBJECT.Create;
      xAccessItemOBJECT.ID_Access_item := tbNOAccessItemID_ACESSO_ITEM.AsInteger;
      xAccessItemOBJECT.ID_Access := xAccessOBJECT.ID_Access;
      xAccessItemOBJECT.NOME := tbNOAccessItemNOME.AsString;
      xAccessItemOBJECT.FLAG := tbNOAccessItemFLAG.AsString;
      TAccess.GetInstance.saveItem(xAccessItemOBJECT, xStatus);
      //TBNOAcessItemID_ACESSO_ITEM.AsInteger := xAccessItemOBJECT.ID_Access;
      tbNOAccessItem.Next;
    end;

    tbAccessItem.First;
    while not tbAccessItem.Eof do
    begin
      xAccessItemOBJECT := TAccessItemOBJECT.Create;
      xAccessItemOBJECT.ID_Access_item := tbAccessItemID_ACESSO_ITEM.AsInteger;
      xAccessItemOBJECT.ID_Access := xAccessOBJECT.ID_Access;
      xAccessItemOBJECT.NOME := tbAccessItemNOME.AsString;
      xAccessItemOBJECT.FLAG := tbAccessItemFLAG.AsString;
      TAccess.GetInstance.saveItem(xAccessItemOBJECT, xStatus);
      //TBAcessItemID_ACESSO_ITEM.AsInteger := xAccessItemOBJECT.ID_Access;
      tbAccessItem.Next;
    end;

    TAccess.GetInstance.NotifyAccessSucess(xStatus, xAccessOBJECT.ID_Access);
  end;
end;

procedure TfrmRegisterAccess.sbRigthArrowClick(Sender: TObject);
begin
  inherited;
  AccessModification(tbNOAccessItem, tbAccessItem, true, 'N');
end;

procedure TfrmRegisterAccess.AccessModification(ACDSAlt, ACDSDelete: TClientDataSet; ADelete: boolean = true; AFLAG: string = 'Y');
begin
  if ACDSDelete.IsEmpty then
    Exit;

  if not (ACDSAlt.State in [DSINSERT]) then
    ACDSAlt.Append;

  ACDSAlt.FieldByName('ID_ACESSO').AsInteger := ACDSDelete.FieldByName('ID_ACESSO').AsInteger;
  ACDSAlt.FieldByName('ID_ACESSO_ITEM').AsInteger := ACDSDelete.FieldByName('ID_ACESSO_ITEM').AsInteger;
  ACDSAlt.FieldByName('NOME').AsString := ACDSDelete.FieldByName('NOME').AsString;
  ACDSAlt.FieldByName('FLAG').AsString := AFLAG;
  ACDSAlt.Post;

  if ADelete then
    ACDSDelete.Delete;

  if (tbobjetos.State in [dsBrowse]) then
    tbobjetos.Edit;
end;

procedure TfrmRegisterAccess.sbLeftArrowClick(Sender: TObject);
begin
  inherited;
  AccessModification(tbAccessItem, tbNOAccessItem);
end;

procedure TfrmRegisterAccess.sbLeftArrowAllClick(Sender: TObject);
begin
  inherited;
  tbNOAccessItem.DisableControls;
  tbNOAccessItem.First;
  while not tbNOAccessItem.Eof do
  begin
    if not tbAccessItem.Locate('nome', tbNOAccessItemNOME.AsString, [lopartialkey, loCaseinsensitive]) then
      AccessModification(tbAccessItem, tbNOAccessItem, false);
    tbNOAccessItem.Next;
  end;

  tbNOAccessItem.EmptyDataSet;
  tbAccessItem.First;
  tbNOAccessItem.EnableControls;
end;

procedure TfrmRegisterAccess.sbRigthArrowAllClick(Sender: TObject);
begin
  inherited;
  tbAccessItem.DisableControls;
  tbAccessItem.First;
  while not tbAccessItem.Eof do
  begin
    if not tbNOAccessItem.Locate('nome', tbAccessItemNOME.AsString, [lopartialkey, loCaseinsensitive]) then
      AccessModification(tbNOAccessItem, tbAccessItem, false, 'N');

    tbAccessItem.Next;
  end;

  tbAccessItem.EmptyDataSet;
  tbNOAccessItem.First;
  tbAccessItem.EnableControls;
end;

procedure TfrmRegisterAccess.actNovoExecute(Sender: TObject);
begin
  inherited;
  with QuyComandos do
  begin
    Close;
    SQL.Clear;
    SQL.add('select NOME from TB_ACESSO_ITEM where ID_ACESSO =1');
    Open;
    Last;
    First;
  end;
  tbNOAccessItem.EmptyDataSet;
  tbAccessItem.EmptyDataSet;

  if not tbNOAccessItem.Active then
    tbNOAccessItem.Open;

  if not tbAccessItem.Active then
    tbAccessItem.Open;

  QuyComandos.First;
  while not QuyComandos.Eof do
  begin
    tbAccessItem.Append;
    tbAccessItemID_ACESSO.AsInteger := 0;
    tbAccessItemID_ACESSO_ITEM.AsInteger := 0;
    tbAccessItemNOME.AsString := QuyComandos.FIELDBYNAME('NOME').ASSTRING;
    tbAccessItemFLAG.AsString := 'Y';
    QuyComandos.Next;
  end;
  tbAccessItem.First;
  dbedtNOME.SetFocus;
end;

procedure TfrmRegisterAccess.actCancelarExecute(Sender: TObject);
begin
  inherited;
  TBObjetosAfterScroll(TBObjetos);
end;

procedure TfrmRegisterAccess.OnGetMenu;
begin

end;

procedure TfrmRegisterAccess.dsObjetosStateChange(Sender: TObject);
begin
  inherited;
  dbedtNOME.ReadOnly := not (TBObjetos.State in [DSINSERT]);
end;

initialization
  RegisterClass(TfrmRegisterAccess);


finalization
  UnRegisterClass(TfrmRegisterAccess);

end.

