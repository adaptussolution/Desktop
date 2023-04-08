unit UPerson;

interface

uses
  Classes;

type
  IPerson = interface
    ['{7EA4099E-8AA6-4350-9748-82C4698664B1}']
    procedure OnSucess(AStatus: string; AId: Integer);
    procedure OnFaild(AMsg: string);
  end;

  TPersonOBJECT = class
    ID_PESSOA: Integer;
    CPF_CNPJ: string;
    NOME: string;
    DATA_NASCIMENTO: TDateTime;
    SEXO: string;
    EMAIL: string;
    TELEFONE: string;
    CEP: string;
    BAIRRO: string;
    NUMERO: Integer;
    LOGRADOURO: string;
    COMPLEMENTO: string;
    CIDADE: string;
    UF: string;
    PAIS: string;
    TIPO_PESSOA: string;
  end;

  TPerson = class
  private
    FInterfaceList: TInterfaceList;
    procedure NotifyPersonSucess(AStatus: string; AId: Integer);
    procedure NotifyPersonFaild(AMsg: string);
  protected
  public
    procedure AddListener(AIPerson: IPerson);
    procedure RemListener(AIPerson: IPerson);
    procedure save(APersonOBJECT: TPersonOBJECT; AStatus: string = 'I');
    procedure delete(AId: Integer);
    constructor Create;
    class function GetInstance(): TPerson;
  end;

implementation

uses
  UDM_PRINCIPAL, SysUtils;

{ TPerson }

procedure TPerson.save(APersonOBJECT: TPersonOBJECT; AStatus: string = 'I');
var
  xCodigo: string;
begin
  try
    DM_PRINCIPAL.StartTransaction;

    if AStatus = 'U' then
      xCodigo := IntToStr(APersonOBJECT.ID_PESSOA)
    else
      xCodigo := DM_PRINCIPAL.GetSequence('GEN_ID_PESSOA');

    with DM_PRINCIPAL.IBQuery1 do
    begin
      Close;
      SQL.Clear;
      SQL.Add('update or insert into tb_pessoa (id_pessoa, cpf_cnpj, nome, data_nascimento, sexo, email, telefone, cep, bairro, numero, logradouro, complemento, cidade, uf, pais, tipo_pessoa) ');
      SQL.Add('values (:id_pessoa, :cpf_cnpj, :nome, :data_nascimento, :sexo, :email, :telefone, :cep, :bairro, :numero, :logradouro, :complemento, :cidade, :uf, :pais, :tipo_pessoa) ');
      SQL.Add('matching (id_pessoa)');

      ParamByName('id_pessoa').AsString := xCodigo;
      ParamByName('cpf_cnpj').AsString := APersonOBJECT.CPF_CNPJ;
      ParamByName('nome').AsString := APersonOBJECT.NOME;
      ParamByName('data_nascimento').AsDateTime := APersonOBJECT.DATA_NASCIMENTO;
      ParamByName('sexo').AsString := APersonOBJECT.SEXO;
      ParamByName('email').AsString := APersonOBJECT.EMAIL;
      ParamByName('telefone').AsString := APersonOBJECT.TELEFONE;
      ParamByName('cep').AsString := APersonOBJECT.CEP;
      ParamByName('bairro').AsString := APersonOBJECT.BAIRRO;
      ParamByName('numero').AsInteger := APersonOBJECT.NUMERO;
      ParamByName('logradouro').AsString := APersonOBJECT.LOGRADOURO;
      ParamByName('complemento').AsString := APersonOBJECT.COMPLEMENTO;
      ParamByName('cidade').AsString := APersonOBJECT.CIDADE;
      ParamByName('uf').AsString := APersonOBJECT.UF;
      ParamByName('pais').AsString := APersonOBJECT.PAIS;
      ParamByName('tipo_pessoa').AsString := APersonOBJECT.TIPO_PESSOA;

      ExecSQL;
      DM_PRINCIPAL.ConfirmTransaction;
      APersonOBJECT.ID_PESSOA := StrToInt(xCodigo);
      NotifyPersonSucess(AStatus, APersonOBJECT.ID_PESSOA);
    end;
  except
    on e: Exception do
    begin
      DM_PRINCIPAL.CancelTransaction;
      NotifyPersonFaild(PCHAR('Ocorreu um erro!' + e.Message));
    end;
  end;
end;

procedure TPerson.delete(AId: Integer);
begin
  try
    DM_PRINCIPAL.StartTransaction;
    with DM_PRINCIPAL.IBQuery1 do
    begin
      CLOSE;
      SQL.Clear;
      SQL.Add('delete from tb_pessoa where (id_pessoa = :id_pessoa)');
      ParamByName('id_pessoa').AsInteger := AId;
      ExecSQL;
    end;
    DM_PRINCIPAL.ConfirmTransaction;
    NotifyPersonSucess('D', AId);
  except
    on e: Exception do
    begin
      DM_PRINCIPAL.CancelTransaction;
      NotifyPersonFaild('Ocorreu um erro!' + e.Message);
    end;
  end;
end;

procedure TPerson.AddListener(AIPerson: IPerson);
begin
  if not Assigned(FInterfaceList) then
    Exit;

  if FInterfaceList.IndexOf(AIPerson) = -1 then
    FInterfaceList.Add(AIPerson);
end;

procedure TPerson.RemListener(AIPerson: IPerson);
begin
  if not Assigned(FInterfaceList) then
    Exit;

  if FInterfaceList.IndexOf(AIPerson) > -1 then
    FInterfaceList.Remove(AIPerson);
end;

procedure TPerson.NotifyPersonFaild(AMsg: string);
var
  xIPerson: IPerson;
  i: Integer;
begin
  try
    if not Assigned(FInterfaceList) then
      Exit;

    for i := 0 to FInterfaceList.Count - 1 do
    begin
      if Supports(FInterfaceList[i], IPerson, xIPerson) then
        xIPerson.OnFaild(AMsg);
    end;
  except
    
  end;
end;

procedure TPerson.NotifyPersonSucess(AStatus: string; AId: Integer);
var
  xIPerson: IPerson;
  i: Integer;
begin
  try
    if not Assigned(FInterfaceList) then
      Exit;

    for i := 0 to FInterfaceList.Count - 1 do
    begin
      if Supports(FInterfaceList[i], IPerson, xIPerson) then
        xIPerson.OnSucess(AStatus, AId);
    end;
  except
    
  end;
end;

constructor TPerson.Create;
begin
  if not Assigned(FInterfaceList) then
    FInterfaceList := TInterfaceList.Create;
end;

class function TPerson.GetInstance: TPerson;
begin
  Result := TPerson.Create;
end;

end.

