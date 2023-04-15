unit URegisterCompany;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrmCadastro, DB, Provider, DBClient, IBCustomDataSet, IBQuery,
  ImgList, ActnList, StdCtrls, Grids, DBGrids, ComCtrls, DBCtrls, Buttons,
  ToolWin, ExtCtrls, UCompany, Mask;

type
  TfrmRegisterCompany = class(TFrmCadastros, ICompany)
    QuyObjetosID_EMPRESA: TIntegerField;
    QuyObjetosID_PESSOA: TIntegerField;
    QuyObjetosNOME_FANTASIA: TIBStringField;
    QuyObjetosRAZAO_SOCIAL: TIBStringField;
    QuyObjetosCNPJ: TIBStringField;
    QuyObjetosNOME_EMPRESA: TIBStringField;
    TBObjetosID_EMPRESA: TIntegerField;
    TBObjetosID_PESSOA: TIntegerField;
    TBObjetosNOME_FANTASIA: TStringField;
    TBObjetosRAZAO_SOCIAL: TStringField;
    TBObjetosCNPJ: TStringField;
    TBObjetosNOME_EMPRESA: TStringField;
    lbl: TLabel;
    dbedtID_EMPRESA: TDBEdit;
    lbl1: TLabel;
    dbedtID_PESSOA: TDBEdit;
    lbl2: TLabel;
    dbedtNOME_FANTASIA: TDBEdit;
    lbl3: TLabel;
    dbedtRAZAO_SOCIAL: TDBEdit;
    dbedtCNPJ: TDBEdit;
    dbedtNOME_EMPRESA: TDBEdit;
    btnSeachPerson: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure actExcluirExecute(Sender: TObject);
    procedure actGravarExecute(Sender: TObject);
    procedure actNovoExecute(Sender: TObject);
    procedure btnSeachPersonClick(Sender: TObject);
    procedure dbedtID_PESSOAExit(Sender: TObject);
    procedure dbedtCNPJExit(Sender: TObject);
    procedure dsObjetosStateChange(Sender: TObject);
  private
    { Private declarations }
    function validateFileds: Boolean;
    procedure OnSucess(AStatus: string; AId: Integer);
    procedure OnFaild(AMsg: string);
  public
    { Public declarations }
  end;

var
  frmRegisterCompany: TfrmRegisterCompany;

implementation

uses
  UDM_PRINCIPAL, UFrmFiltro;

{$R *.dfm}

procedure TfrmRegisterCompany.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  TCompany.GetInstance.RemListener(Self);
  Action := caFree;
  frmRegisterCompany := NIL;
end;

procedure TfrmRegisterCompany.OnFaild(AMsg: string);
begin
  Application.MessageBox(PCHAR(AMsg), 'Aten��o', mb_ok + MB_ICONERROR);
  abort;
end;

procedure TfrmRegisterCompany.OnSucess(AStatus: string; AId: Integer);
begin
  if AStatus = 'D' then
    TBObjetos.Delete
  else
  begin
    TBObjetosid_empresa.AsInteger := AId;

    if TBObjetos.State in [DSINSERT] then
      Application.MessageBox('Cadastro realizado com sucesso!', 'Aten��o', mb_ok + MB_ICONWARNING)
    else
      Application.MessageBox('Altera��o realizada com sucesso!', 'Aten��o', mb_ok + MB_ICONWARNING);

    TBObjetos.Post;
    PageControl1.TabIndex := 0;
  end;
end;

procedure TfrmRegisterCompany.FormCreate(Sender: TObject);
begin
  inherited;
  TCompany.GetInstance.AddListener(Self);
end;

procedure TfrmRegisterCompany.actExcluirExecute(Sender: TObject);
begin
  inherited;
  if (Application.MessageBox('Deseja Realmente Excluir?', 'Aten��o', MB_YESNO + MB_ICONWARNING) = id_yes) then
    TCompany.GetInstance.delete(TBObjetosid_empresa.AsInteger);
end;

function TfrmRegisterCompany.validateFileds: Boolean;
  function VerifyFantasyName: Boolean;
  begin
    with QuyComandos do
    begin
      Close;
      SQL.Clear;
      SQL.Add('select e.nome_fantasia  from tb_empresa e where e.nome_fantasia = :nome_fantasia');
      ParamByName('nome_fantasia').AsString := TBObjetosNome_fantasia.AsString;
      Open;
      Last;
      First;
      Result := IsEmpty
    end;
  end;

  function VerifyCorporateName: Boolean;
  begin
    with QuyComandos do
    begin
      Close;
      SQL.Clear;
      SQL.Add('select e.razao_social  from tb_empresa e where e.razao_social = :razao_social');
      ParamByName('razao_social').AsString := TBObjetosRazao_social.AsString;
      Open;
      Last;
      First;
      Result := IsEmpty
    end;
  end;

begin
  Result := False;
  dbedtid_empresa.SetFocus;
  if TBObjetosNome_fantasia.AsString = '' then
  begin
    Application.MessageBox('POR FAVOR INFORME O NOME FANTASIA!', 'ATEN��O', MB_OK + MB_ICONWARNING);
    dbedtNome_fantasia.SetFocus;
    Result := True;
    Exit;
  end;

  if TBObjetosRazao_social.AsString = '' then
  begin
    Application.MessageBox('POR FAVOR INFORME O RAZ�O SOCIAL!', 'ATEN��O', MB_OK + MB_ICONWARNING);
    dbedtRazao_social.SetFocus;
    Result := True;
    Exit;
  end;

  if TBObjetos.State in [DSINSERT] then
  begin
    if not VerifyFantasyName then
    begin
      Application.MessageBox('POR FAVOR INFORME OUTRO NOME FANTASIA, POIS ESSE, J� EST� SENDO USADO!', 'ATEN��O', MB_OK + MB_ICONWARNING);
      dbedtNOME_FANTASIA.SetFocus;
      Result := True;
      Exit;
    end;

    if not VerifyCorporateName then
    begin
      Application.MessageBox('POR FAVOR INFORME OUTRA RAZ�O SOCIAL, POIS ESSE, J� EST� SENDO USADO!', 'ATEN��O', MB_OK + MB_ICONWARNING);
      dbedtRAZAO_SOCIAL.SetFocus;
      Result := True;
      Exit;
    end;
  end;

end;

procedure TfrmRegisterCompany.actGravarExecute(Sender: TObject);
var
  xCompanyOBJECT: TCompanyOBJECT;
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

    xCompanyOBJECT := TCompanyOBJECT.Create;
    xCompanyOBJECT.ID_EMPRESA := TBObjetosID_EMPRESA.AsInteger;
    xCompanyOBJECT.ID_PESSOA := TBObjetosID_PESSOA.AsInteger;
    xCompanyOBJECT.NOME_FANTASIA := TBObjetosNOME_FANTASIA.AsString;
    xCompanyOBJECT.RAZAO_SOCIAL := TBObjetosRAZAO_SOCIAL.AsString;

    TCompany.GetInstance.save(xCompanyOBJECT, xStatus);
  end;
end;

procedure TfrmRegisterCompany.actNovoExecute(Sender: TObject);
begin
  inherited;
  dbedtNOME_FANTASIA.SetFocus;
end;

procedure TfrmRegisterCompany.btnSeachPersonClick(Sender: TObject);
begin
  inherited;
  if not (tbobjetos.State in [dsedit, dsinsert]) then
    TBObjetos.Edit;

  try
    FrmFiltro := TFrmFiltro.Create(self);
    FrmFiltro.VTabela := 'TB_EMPRESA';
    FrmFiltro.ShowModal;
    if FrmFiltro.BtnSelecionar.ModalResult = mrOk then
    begin
      TBObjetosID_PESSOA.AsInteger := FrmFiltro.Tbobjetos.fieldbyname('ID_PESSOA').AsInteger;
      TBObjetosNOME_EMPRESA.AsString := FrmFiltro.Tbobjetos.fieldbyname('nome_empresa').AsString;
      TBObjetosCNPJ.AsString := FrmFiltro.Tbobjetos.fieldbyname('cnpj').AsString;
    end;
  finally
    FreeAndNil(FrmFiltro);
  end;
end;

procedure TfrmRegisterCompany.dbedtID_PESSOAExit(Sender: TObject);
begin
  inherited;
  if not (TBObjetos.State in [dsedit, dsinsert]) then
    exit;
  if TBObjetosID_PESSOA.AsInteger = 0 then
  begin
    TBObjetosID_PESSOA.Clear;
    TBObjetosNOME_EMPRESA.Clear;
    TBObjetosCNPJ.Clear;
    Abort;
  end;

  with QuyComandos do
  begin
    Close;
    SQL.Clear;
    SQL.add('select id_pessoa, cpf_cnpj as cnpj, nome as nome_empresa from tb_pessoa where tipo_pessoa = ''J'' and char_length(cpf_cnpj) = 14 and id_pessoa =:id_pessoa');
    parambyname('id_pessoa').AsInteger := TBObjetosID_PESSOA.AsInteger;
    Open;
    Last;
    First;

    if recordcount = 0 then
    begin
      Application.MessageBox('ID inv�lido!', 'Informa��o', MB_OK + MB_ICONINFORMATION);
      TBObjetosID_PESSOA.Clear;
      TBObjetosNOME_EMPRESA.Clear;
      TBObjetosCNPJ.Clear;
      Abort;
    end
    else
    begin
      TBObjetosNOME_EMPRESA.ASSTRING := FIELDBYNAME('nome_empresa').ASSTRING;
      TBObjetosCNPJ.AsString := FIELDBYNAME('cnpj').AsString;
    end;
  end;
end;

procedure TfrmRegisterCompany.dbedtCNPJExit(Sender: TObject);
begin
  inherited;
  if not (TBObjetos.State in [dsedit, dsinsert]) then
    exit;
  if TBObjetosCNPJ.AsString = '' then
  begin
    TBObjetosID_PESSOA.Clear;
    TBObjetosNOME_EMPRESA.Clear;
    TBObjetosCNPJ.Clear;
    Abort;
  end;

  with QuyComandos do
  begin
    Close;
    SQL.Clear;
    SQL.add('select id_pessoa, cpf_cnpj as cnpj, nome as nome_empresa from tb_pessoa where tipo_pessoa = ''J'' and char_length(cpf_cnpj) = 14 and cpf_cnpj =:cpf_cnpj');
    parambyname('cpf_cnpj').AsString := TBObjetosCNPJ.AsString;
    Open;
    Last;
    First;
    
    if recordcount = 0 then
    begin
      Application.MessageBox('CNPJ inv�lido!', 'Informa��o', MB_OK + MB_ICONINFORMATION);
      TBObjetosID_PESSOA.Clear;
      TBObjetosNOME_EMPRESA.Clear;
      TBObjetosCNPJ.Clear;
      Abort;
    end
    else
    begin
      TBObjetosNOME_EMPRESA.ASSTRING := FIELDBYNAME('nome_empresa').ASSTRING;
      TBObjetosID_PESSOA.AsInteger := FIELDBYNAME('id_pessoa').AsInteger;
    end;
  end;
end;

procedure TfrmRegisterCompany.dsObjetosStateChange(Sender: TObject);
begin
  inherited;
  dbedtNOME_FANTASIA.ReadOnly := not (TBObjetos.State in [DSINSERT]);
  dbedtRAZAO_SOCIAL.ReadOnly := not (TBObjetos.State in [DSINSERT]);
end;

initialization
  RegisterClass(TfrmRegisterCompany);


finalization
  UnRegisterClass(TfrmRegisterCompany);

end.

