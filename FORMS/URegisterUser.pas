unit URegisterUser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrmCadastro, DB, Provider, DBClient, IBCustomDataSet, IBQuery,
  ImgList, ActnList, StdCtrls, Grids, DBGrids, ComCtrls, DBCtrls, Buttons,
  ToolWin, ExtCtrls, UUser, Mask;

type
  TPassword = class
    PasswordDecrypt: string;
  end;

  TfrmRegisterUser = class(TFrmCadastros, IUser)
    intgrfldQuyObjetosID_USUARIO: TIntegerField;
    intgrfldQuyObjetosID_PESSOA: TIntegerField;
    QuyObjetosLOGIN: TIBStringField;
    QuyObjetosSENHA: TIBStringField;
    QuyObjetosNOME_PESSOA: TIBStringField;
    TBObjetosID_USUARIO: TIntegerField;
    TBObjetosID_PESSOA: TIntegerField;
    TBObjetosLOGIN: TStringField;
    TBObjetosSENHA: TStringField;
    TBObjetosNOME_PESSOA: TStringField;
    lbl: TLabel;
    dbedtID_USUARIO: TDBEdit;
    lbl1: TLabel;
    dbedtLOGIN: TDBEdit;
    lbl2: TLabel;
    dbedtSENHA: TDBEdit;
    lbl3: TLabel;
    dbedtID_PESSOA: TDBEdit;
    lbl4: TLabel;
    dbedtNOME_PESSOA: TDBEdit;
    btnSeachPerson: TSpeedButton;
    chkViewPassword: TCheckBox;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actExcluirExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure chkViewPasswordClick(Sender: TObject);
    procedure actGravarExecute(Sender: TObject);
    procedure actNovoExecute(Sender: TObject);
    procedure dbedtID_PESSOAExit(Sender: TObject);
    procedure btnSeachPersonClick(Sender: TObject);
    procedure TBObjetosAfterScroll(DataSet: TDataSet);
    procedure TBObjetosBeforeCancel(DataSet: TDataSet);
    procedure TBObjetosBeforePost(DataSet: TDataSet);
  private
    { Private declarations }
    FPassword: TPassword;
    function validateFileds: Boolean;
    procedure OnSucess(AStatus: string; AId: Integer);
    procedure OnFaild(AMsg: string);
  public
    { Public declarations }
  end;

var
  frmRegisterUser: TfrmRegisterUser;

implementation

uses
  UDM_PRINCIPAL, UFrmFiltro, UGlobal;

{$R *.dfm}

procedure TfrmRegisterUser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  TUser.GetInstance.RemListener(Self);
  Action := caFree;
  frmRegisterUser := NIL;
end;

procedure TfrmRegisterUser.actExcluirExecute(Sender: TObject);
begin
  inherited;
  if (Application.MessageBox('Deseja Realmente Excluir?', 'Aten��o', MB_YESNO + MB_ICONWARNING) = id_yes) then
    TUser.GetInstance.delete(TBObjetosID_USUARIO.AsInteger);
end;

procedure TfrmRegisterUser.FormCreate(Sender: TObject);
begin
  inherited;
  TUser.GetInstance.AddListener(Self);
end;

procedure TfrmRegisterUser.OnFaild(AMsg: string);
begin
  Application.MessageBox(PCHAR(AMsg), 'Aten��o', mb_ok + MB_ICONERROR);
  abort;
end;

procedure TfrmRegisterUser.OnSucess(AStatus: string; AId: Integer);
begin
  if AStatus = 'D' then
    TBObjetos.Delete
  else
  begin
    TBObjetosID_USUARIO.AsInteger := AId;

    if TBObjetos.State in [DSINSERT] then
      Application.MessageBox('Cadastro realizado com sucesso!', 'Aten��o', mb_ok + MB_ICONWARNING)
    else
      Application.MessageBox('Altera��o realizada com sucesso!', 'Aten��o', mb_ok + MB_ICONWARNING);

    TBObjetos.Post;

    dbedtSENHA.PasswordChar := '*';
    PageControl1.TabIndex := 0;
  end;
end;

function TfrmRegisterUser.validateFileds: Boolean;
  function VerifyLogin: Boolean;
  begin
    with QuyComandos do
    begin
      Close;
      SQL.Clear;
      SQL.Add('select u.login  from TB_USUARIO u where u.login =:login');
      ParamByName('login').AsString := TBObjetosLOGIN.AsString;
      Open;
      Last;
      First;
      Result := IsEmpty
    end;
  end;
begin
  Result := False;
  dbedtID_USUARIO.SetFocus;
  if TBObjetosLOGIN.AsString = '' then
  begin
    Application.MessageBox('POR FAVOR INFORME O LOGIN!', 'ATEN��O', MB_OK + MB_ICONWARNING);
    dbedtLOGIN.SetFocus;
    Result := True;
    Exit;
  end;

  if TBObjetosSENHA.AsString = '' then
  begin
    Application.MessageBox('POR FAVOR INFORME A SENHA!', 'ATEN��O', MB_OK + MB_ICONWARNING);
    dbedtSENHA.SetFocus;
    Result := True;
    Exit;
  end;

  IF not VerifyLogin THEN
  BEGIN
    Application.MessageBox('POR FAVOR INFORME OUTRO LOGIN, POIS ESSE, J� EST� SENDO USADO!', 'ATEN��O', MB_OK + MB_ICONWARNING);
    dbedtLOGIN.SetFocus;
    Result := True;
    Exit;
  end;

  if TBObjetosID_PESSOA.AsInteger = 0 then
  begin
    Application.MessageBox('POR FAVOR INFORME UMA PESSOA RELACIONADA AO USU�RIO!', 'ATEN��O', MB_OK + MB_ICONWARNING);
    dbedtID_PESSOA.SetFocus;
    Result := True;
    Exit;
  end;
end;

procedure TfrmRegisterUser.chkViewPasswordClick(Sender: TObject);
begin
  inherited;
  if chkViewPassword.Checked then
  begin
    if (tbobjetos.State in [dsBrowse]) then
      tbobjetos.Edit;

    if (tbobjetos.State in [dsedit]) then
      TBObjetosSENHA.AsString := FPassword.PasswordDecrypt;

    dbedtSENHA.PasswordChar := #0;
  end
  else
  begin
    dbedtSENHA.PasswordChar := '*';
    if (tbobjetos.State in [dsedit]) then
      TBObjetosSENHA.AsString := DM_PRINCIPAL.FGlobal.Encrypt(TBObjetosSENHA.AsString, 1010);
  end;
end;

procedure TfrmRegisterUser.actGravarExecute(Sender: TObject);
var
  xUserOBJECT: TUserOBJECT;
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

    dbedtSENHA.PasswordChar := '*';
    chkViewPassword.Checked := False;

    if TBObjetos.State in [DSINSERT] then
      TBObjetosSENHA.AsString := DM_PRINCIPAL.FGlobal.Encrypt(TBObjetosSENHA.AsString, 1010)
    else
    if dbedtSENHA.PasswordChar = #0 then
    begin
      if TBObjetos.State in [DSEDIT] then
        TBObjetosSENHA.AsString := DM_PRINCIPAL.FGlobal.Encrypt(TBObjetosSENHA.AsString, 1010);
    end;

    xUserOBJECT := TUserOBJECT.Create;
    xUserOBJECT.ID_USUARIO := TBObjetosID_USUARIO.AsInteger;
    xUserOBJECT.ID_PESSOA := TBObjetosID_PESSOA.AsInteger;
    xUserOBJECT.LOGIN := TBObjetosLOGIN.AsString;

    xUserOBJECT.SENHA := TBObjetosSENHA.AsString;
    TUser.GetInstance.save(xUserOBJECT, xStatus);
  end;
end;

procedure TfrmRegisterUser.actNovoExecute(Sender: TObject);
begin
  inherited;
  dbedtLOGIN.SetFocus;
end;

procedure TfrmRegisterUser.dbedtID_PESSOAExit(Sender: TObject);
begin
  inherited;
  if not (TBObjetos.State in [dsedit, dsinsert]) then
    exit;
  if TBObjetosID_PESSOA.AsInteger = 0 then
  begin
    TBObjetosID_PESSOA.Clear;
    TBObjetosNOME_PESSOA.Clear;
    Abort;
  end;

  with QuyComandos do
  begin
    Close;
    SQL.Clear;
    SQL.add('select * from tb_pessoa where id_pessoa =:id_pessoa');
    parambyname('id_pessoa').AsInteger := TBObjetosID_PESSOA.AsInteger;
    Open;
    Last;
    First;
    if recordcount = 0 then
    begin
      Application.MessageBox('ID inv�lido!', 'Informa��o', MB_OK + MB_ICONINFORMATION);
      TBObjetosID_PESSOA.Clear;
      TBObjetosNOME_PESSOA.Clear;
      Abort;
    end
    else
    begin
      TBObjetosNOME_PESSOA.ASSTRING := FIELDBYNAME('NOME').ASSTRING;
    end;
  end;
end;

procedure TfrmRegisterUser.btnSeachPersonClick(Sender: TObject);
begin
  inherited;
  if not (tbobjetos.State in [dsedit, dsinsert]) then
    TBObjetos.Edit;

  try
    FrmFiltro := TFrmFiltro.Create(self);
    FrmFiltro.VTabela := 'tb_pessoa';
    FrmFiltro.ShowModal;
    if FrmFiltro.BtnSelecionar.ModalResult = mrOk then
    begin
      TBObjetosID_PESSOA.AsInteger := FrmFiltro.Tbobjetos.fieldbyname('ID_PESSOA').AsInteger;
      TBObjetosNOME_PESSOA.AsString := FrmFiltro.Tbobjetos.fieldbyname('NOME').AsString;
    end;
  finally
    FreeAndNil(FrmFiltro);
  end;
end;

procedure TfrmRegisterUser.TBObjetosAfterScroll(DataSet: TDataSet);
begin
  inherited;
  if not Assigned(FPassword) then
    FPassword := TPassword.Create;

  FPassword.PasswordDecrypt := DM_PRINCIPAL.FGlobal.Decrypt(TBObjetosSENHA.AsString, 1010);
end;

procedure TfrmRegisterUser.TBObjetosBeforeCancel(DataSet: TDataSet);
begin
  inherited;
  dbedtSENHA.PasswordChar := '*';
  chkViewPassword.Checked := False;
end;

procedure TfrmRegisterUser.TBObjetosBeforePost(DataSet: TDataSet);
begin
  inherited;
  FPassword.PasswordDecrypt := DM_PRINCIPAL.FGlobal.Decrypt(TBObjetosSENHA.AsString, 1010);
end;

initialization
  RegisterClass(TfrmRegisterUser);


finalization
  UnRegisterClass(TfrmRegisterUser);

end.

