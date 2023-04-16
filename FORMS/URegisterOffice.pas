unit URegisterOffice;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrmCadastro, DB, Provider, DBClient, IBCustomDataSet, IBQuery,
  ImgList, ActnList, StdCtrls, Grids, DBGrids, ComCtrls, DBCtrls, Buttons,
  ToolWin, ExtCtrls, UOffice, Mask;

type
  TfrmRegisterOffice = class(TFrmCadastros, IOffice)
    QuyObjetosID_CARGO: TIntegerField;
    QuyObjetosID_SETOR: TIntegerField;
    QuyObjetosNOME: TIBStringField;
    QuyObjetosSALARIO: TFloatField;
    QuyObjetosSETOR: TIBStringField;
    TBObjetosID_CARGO: TIntegerField;
    TBObjetosID_SETOR: TIntegerField;
    TBObjetosNOME: TStringField;
    TBObjetosSALARIO: TFloatField;
    TBObjetosSETOR: TStringField;
    dbedtID_CARGO: TDBEdit;
    dbedtID_SETOR: TDBEdit;
    lbl2: TLabel;
    dbedtNOME: TDBEdit;
    lbl3: TLabel;
    dbedtSALARIO: TDBEdit;
    lbl4: TLabel;
    dbedtSETOR: TDBEdit;
    btnSeachSector: TSpeedButton;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure actExcluirExecute(Sender: TObject);
    procedure dsObjetosStateChange(Sender: TObject);
    procedure actNovoExecute(Sender: TObject);
    procedure actGravarExecute(Sender: TObject);
    procedure dbedtID_SETORExit(Sender: TObject);
    procedure btnSeachSectorClick(Sender: TObject);
  private
    { Private declarations }
    function validateFileds: Boolean;
    procedure OnSucess(AStatus: string; AId: Integer);
    procedure OnFaild(AMsg: string);
  public
    { Public declarations }
  end;

var
  frmRegisterOffice: TfrmRegisterOffice;

implementation

uses
  UDM_PRINCIPAL, UFrmFiltro;

{$R *.dfm}

procedure TfrmRegisterOffice.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  TOffice.GetInstance.RemListener(Self);
  Action := caFree;
  frmRegisterOffice := NIL;
end;

procedure TfrmRegisterOffice.OnFaild(AMsg: string);
begin
  Application.MessageBox(PCHAR(AMsg), 'Aten��o', mb_ok + MB_ICONERROR);
  abort;
end;

procedure TfrmRegisterOffice.OnSucess(AStatus: string; AId: Integer);
begin
  if AStatus = 'D' then
    TBObjetos.Delete
  else
  begin
    TBObjetosID_CARGO.AsInteger := AId;

    if TBObjetos.State in [DSINSERT] then
      Application.MessageBox('Cadastro realizado com sucesso!', 'Aten��o', mb_ok + MB_ICONWARNING)
    else
      Application.MessageBox('Altera��o realizada com sucesso!', 'Aten��o', mb_ok + MB_ICONWARNING);

    TBObjetos.Post;
    PageControl1.TabIndex := 0;
  end;
end;

function TfrmRegisterOffice.validateFileds: Boolean;

  function VerifyOffice: Boolean;
  begin
    with QuyComandos do
    begin
      Close;
      SQL.Clear;
      SQL.Add('select c.nome from tb_cargo c where c.nome = :nome');
      ParamByName('nome').AsString := TBObjetosNOME.AsString;
      Open;
      Last;
      First;
      Result := IsEmpty
    end;
  end;

begin
  Result := False;
  dbedtID_SETOR.SetFocus;
  if TBObjetosNOME.AsString = '' then
  begin
    Application.MessageBox('POR FAVOR INFORME O CARGO!', 'ATEN��O', MB_OK + MB_ICONWARNING);
    dbedtNOME.SetFocus;
    Result := True;
    Exit;
  end;

  if TBObjetosID_SETOR.AsInteger = 0 then
  begin
    Application.MessageBox('POR FAVOR INFORME O SETOR!', 'ATEN��O', MB_OK + MB_ICONWARNING);
    dbedtID_SETOR.SetFocus;
    Result := True;
    Exit;
  end;

  if TBObjetos.State in [DSINSERT] then
  begin
    if not VerifyOffice then
    begin
      Application.MessageBox('POR FAVOR INFORME OUTRO CARGO, POIS ESSE, J� EST� FOI REGISTRADO!', 'ATEN��O', MB_OK + MB_ICONWARNING);
      dbedtNOME.SetFocus;
      Result := True;
      Exit;
    end;
  end;

end;

procedure TfrmRegisterOffice.FormCreate(Sender: TObject);
begin
  inherited;
  TOffice.GetInstance.AddListener(Self);
end;

procedure TfrmRegisterOffice.actExcluirExecute(Sender: TObject);
begin
  inherited;
  if (Application.MessageBox('Deseja Realmente Excluir?', 'Aten��o', MB_YESNO + MB_ICONWARNING) = id_yes) then
    TOffice.GetInstance.delete(TBObjetosid_CARGO.AsInteger);
end;

procedure TfrmRegisterOffice.dsObjetosStateChange(Sender: TObject);
begin
  inherited;
  dbedtNOME.ReadOnly := not (TBObjetos.State in [DSINSERT]);
end;

procedure TfrmRegisterOffice.actNovoExecute(Sender: TObject);
begin
  inherited;
  TBObjetosSALARIO.AsFloat := 0;
  dbedtNOME.SetFocus;
end;

procedure TfrmRegisterOffice.actGravarExecute(Sender: TObject);
var
  xOfficeOBJECT: TOfficeOBJECT;
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

    xOfficeOBJECT := TOfficeOBJECT.Create;
    xOfficeOBJECT.ID_CARGO := TBObjetosID_CARGO.AsInteger;
    xOfficeOBJECT.ID_SETOR := TBObjetosID_SETOR.AsInteger;
    xOfficeOBJECT.NOME := TBObjetosNOME.AsString;
    xOfficeOBJECT.SALARIO := TBObjetosSALARIO.AsFloat;
    TOffice.GetInstance.save(xOfficeOBJECT, xStatus);
  end;
end;

procedure TfrmRegisterOffice.dbedtID_SETORExit(Sender: TObject);
begin
  inherited;
  if not (TBObjetos.State in [dsedit, dsinsert]) then
    exit;
  if TBObjetosID_SETOR.AsInteger = 0 then
  begin
    TBObjetosID_SETOR.Clear;
    TBObjetosSETOR.Clear;
    Abort;
  end;

  with QuyComandos do
  begin
    Close;
    SQL.Clear;
    SQL.add('SELECT S.id_setor, S.nome AS SETOR FROM tb_setor S where S.id_setor =:id_setor');
    parambyname('id_setor').AsInteger := TBObjetosID_SETOR.AsInteger;
    Open;
    Last;
    First;

    if recordcount = 0 then
    begin
      Application.MessageBox('ID inv�lido!', 'Informa��o', MB_OK + MB_ICONINFORMATION);
      TBObjetosID_SETOR.Clear;
      TBObjetosSETOR.Clear;
      Abort;
    end
    else
    begin
      TBObjetosSETOR.ASSTRING := FIELDBYNAME('SETOR').ASSTRING;
    end;
  end;
end;

procedure TfrmRegisterOffice.btnSeachSectorClick(Sender: TObject);
begin
  inherited;
  if not (tbobjetos.State in [dsedit, dsinsert]) then
    TBObjetos.Edit;

  try
    FrmFiltro := TFrmFiltro.Create(self);
    FrmFiltro.VTabela := 'TB_CARGO';
    FrmFiltro.ShowModal;
    if FrmFiltro.BtnSelecionar.ModalResult = mrOk then
    begin
      TBObjetosID_SETOR.AsInteger := FrmFiltro.Tbobjetos.fieldbyname('ID_SETOR').AsInteger;
      TBObjetosSETOR.AsString := FrmFiltro.Tbobjetos.fieldbyname('SETOR').AsString;
    end;
  finally
    FreeAndNil(FrmFiltro);
  end;
end;

initialization
  RegisterClass(TfrmRegisterOffice);


finalization
  UnRegisterClass(TfrmRegisterOffice);

end.

