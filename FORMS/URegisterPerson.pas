unit URegisterPerson;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrmCadastro, DB, Provider, DBClient, IBCustomDataSet, IBQuery,
  ImgList, ActnList, StdCtrls, Grids, DBGrids, ComCtrls, DBCtrls, Buttons,
  ToolWin, ExtCtrls, UPerson, Mask, xmldom, XMLIntf, msxmldom, XMLDoc;

type
  TfrmRegisterPerson = class(TFrmCadastros, IPerson)
    QuyObjetosID_PESSOA: TIntegerField;
    QuyObjetosCPF_CNPJ: TIBStringField;
    QuyObjetosNOME: TIBStringField;
    QuyObjetosDATA_NASCIMENTO: TDateField;
    QuyObjetosSEXO: TIBStringField;
    QuyObjetosEMAIL: TIBStringField;
    QuyObjetosTELEFONE: TIBStringField;
    QuyObjetosCEP: TIBStringField;
    QuyObjetosBAIRRO: TIBStringField;
    QuyObjetosNUMERO: TIntegerField;
    QuyObjetosLOGRADOURO: TIBStringField;
    QuyObjetosCOMPLEMENTO: TIBStringField;
    QuyObjetosCIDADE: TIBStringField;
    QuyObjetosUF: TIBStringField;
    QuyObjetosPAIS: TIBStringField;
    QuyObjetosTIPO_PESSOA: TIBStringField;
    QuyObjetosESTADO_CIVIL: TIBStringField;
    TBObjetosID_PESSOA: TIntegerField;
    TBObjetosCPF_CNPJ: TStringField;
    TBObjetosNOME: TStringField;
    TBObjetosDATA_NASCIMENTO: TDateField;
    TBObjetosSEXO: TStringField;
    TBObjetosEMAIL: TStringField;
    TBObjetosTELEFONE: TStringField;
    TBObjetosCEP: TStringField;
    TBObjetosBAIRRO: TStringField;
    TBObjetosNUMERO: TIntegerField;
    TBObjetosLOGRADOURO: TStringField;
    TBObjetosCOMPLEMENTO: TStringField;
    TBObjetosCIDADE: TStringField;
    TBObjetosUF: TStringField;
    TBObjetosPAIS: TStringField;
    TBObjetosTIPO_PESSOA: TStringField;
    TBObjetosESTADO_CIVIL: TStringField;
    Label2: TLabel;
    DBEdit1: TDBEdit;
    Label3: TLabel;
    DBEdit2: TDBEdit;
    Label4: TLabel;
    DBEdit3: TDBEdit;
    lbl: TLabel;
    dbedtEMAIL: TDBEdit;
    lbl1: TLabel;
    dbedtTELEFONE: TDBEdit;
    lbl2: TLabel;
    dbedtCEP: TDBEdit;
    lbl3: TLabel;
    dbedtBAIRRO: TDBEdit;
    lbl4: TLabel;
    dbedtNUMERO: TDBEdit;
    lbl5: TLabel;
    dbedtLOGRADOURO: TDBEdit;
    lbl6: TLabel;
    dbedtCOMPLEMENTO: TDBEdit;
    lbl7: TLabel;
    dbedtCIDADE: TDBEdit;
    lbl8: TLabel;
    dbedtUF: TDBEdit;
    lbl9: TLabel;
    dbedtPAIS: TDBEdit;
    dbrgrpSEXO: TDBRadioGroup;
    dbrgrpTIPO_PESSOA: TDBRadioGroup;
    Label5: TLabel;
    DBEdit4: TDBEdit;
    XMLDocument1: TXMLDocument;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure actExcluirExecute(Sender: TObject);
    procedure dbrgrpTIPO_PESSOAChange(Sender: TObject);
    procedure dbedtCEPExit(Sender: TObject);
  private
    { Private declarations }
    procedure OnSucess(AStatus: string; AId: Integer);
    procedure OnFaild(AMsg: string);
  public
    { Public declarations }
  end;

var
  frmRegisterPerson: TfrmRegisterPerson;

implementation

uses UDM_PRINCIPAL, UGlobal;

{$R *.dfm}

{ TfrmRegisterPerson }

procedure TfrmRegisterPerson.OnFaild(AMsg: string);
begin
  Application.MessageBox(PCHAR(AMsg), 'Aten��o', mb_ok + MB_ICONERROR);
  abort;
end;

procedure TfrmRegisterPerson.OnSucess(AStatus: string; AId: Integer);
begin
  if AStatus = 'D' then
    TBObjetos.Delete
  else
  begin
    TBObjetosID_PESSOA.AsInteger := AId;

    if TBObjetos.State in [DSINSERT] then
      Application.MessageBox('Cadastro realizado com sucesso!', 'Aten��o', mb_ok + MB_ICONWARNING)
    else
      Application.MessageBox('Altera��o realizada com sucesso!', 'Aten��o', mb_ok + MB_ICONWARNING);

    TBObjetos.Post;
  end;

end;

procedure TfrmRegisterPerson.FormCreate(Sender: TObject);
begin
  inherited;
  TPerson.GetInstance.AddListener(Self);
end;

procedure TfrmRegisterPerson.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  TPerson.GetInstance.RemListener(Self);
  Action := caFree;
  frmRegisterPerson := NIL;
end;

procedure TfrmRegisterPerson.actExcluirExecute(Sender: TObject);
begin
  inherited;
  if (Application.MessageBox('Deseja Realmente Excluir?', 'Aten��o', MB_YESNO + MB_ICONWARNING) = id_yes) then
  begin
    TPerson.GetInstance.delete(TBObjetosID_PESSOA.AsInteger);
  end;
end;

procedure TfrmRegisterPerson.dbrgrpTIPO_PESSOAChange(Sender: TObject);
begin
  inherited;
  TBObjetosCPF_CNPJ.EditMask := '999.999.999-99;0;_';
  if TBObjetosTIPO_PESSOA.AsString = 'J' then
    TBObjetosCPF_CNPJ.EditMask := '999.999.999/9999-99;0;_';
end;

procedure TfrmRegisterPerson.dbedtCEPExit(Sender: TObject);
var
  xCep: TCep;
begin
  inherited;
  if Length(Trim(dbedtCEP.Text)) <> 8 then
  begin
    ShowMessage('CEP inv�lido');
    Exit;
  end;
  
  xCep := DM_PRINCIPAL.FGlobal.ConsultaCEP(TBObjetosCEP.AsString, XMLDocument1);
  TBObjetosLOGRADOURO.AsString := xCep.logradouro;
  TBObjetosBAIRRO.AsString := xCep.bairro;
  TBObjetosCIDADE.AsString := xCep.cidade;
  TBObjetosUF.AsString := xCep.uf;
end;

initialization
  RegisterClass(TfrmRegisterPerson);


finalization
  UnRegisterClass(TfrmRegisterPerson);

end.

