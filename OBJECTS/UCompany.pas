unit UCompany;

interface

uses
  Classes;

type
  ICompany = interface
    ['{7EA4099E-8AA6-4350-9748-82C4698664B1}']
    procedure OnSucess(AStatus: string; AId: Integer);
    procedure OnFaild(AMsg: string);
  end;

  TCompanyOBJECT = class
    ID_EMPRESA: Integer;
    ID_PESSOA: Integer;
    NOME_FANTASIA: string;
    RAZAO_SOCIAL: string;
  end;

  TCompany = class
  private
    FInterfaceList: TInterfaceList;
    procedure NotifyCompanySucess(AStatus: string; AId: Integer);
    procedure NotifyCompanyFaild(AMsg: string);
  public
    procedure AddListener(AICompany: ICompany);
    procedure RemListener(AICompany: ICompany);
    procedure save(ACompanyOBJECT: TCompanyOBJECT; AStatus: string = 'I');
    procedure delete(AId: Integer);
    constructor Create;
    class function GetInstance(): TCompany;
  end;

var
  Company: TCompany;

implementation

uses
  UDM_PRINCIPAL, SysUtils;

{ TCompany}

procedure TCompany.save(ACompanyOBJECT: TCompanyOBJECT; AStatus: string = 'I');
var
  xCodigo: string;
begin
  try
    DM_PRINCIPAL.StartTransaction;

    if AStatus = 'U' then
      xCodigo := IntToStr(ACompanyOBJECT.id_empresa)
    else
      xCodigo := DM_PRINCIPAL.GetSequence('GEN_ID_EMPRESA');

    with DM_PRINCIPAL.IBQuery1 do
    begin
      Close;
      SQL.Clear;
      SQL.Add('update or insert into tb_empresa (id_empresa, id_pessoa, nome_fantasia, razao_social) ');
      SQL.Add('values (:id_empresa, :id_pessoa, :nome_fantasia, :razao_social) ');
      SQL.Add('matching (id_empresa) ');

      ParamByName('id_empresa').AsString := xCodigo;
      ParamByName('id_pessoa').AsInteger := ACompanyOBJECT.id_pessoa;
      ParamByName('nome_fantasia').AsString := ACompanyOBJECT.nome_fantasia;
      ParamByName('razao_social').AsString := ACompanyOBJECT.razao_social;
      ExecSQL;
      DM_PRINCIPAL.ConfirmTransaction;
      ACompanyOBJECT.id_empresa := StrToInt(xCodigo);
      NotifyCompanySucess(AStatus, ACompanyOBJECT.id_empresa);
    end;
  except
    on e: Exception do
    begin
      DM_PRINCIPAL.CancelTransaction;
      NotifyCompanyFaild(PCHAR('Ocorreu um erro!' + e.Message));
    end;
  end;
end;

procedure TCompany.delete(AId: Integer);
begin
  try
    DM_PRINCIPAL.StartTransaction;
    with DM_PRINCIPAL.IBQuery1 do
    begin
      CLOSE;
      SQL.Clear;
      SQL.Add('delete from tb_empresa where (id_empresa = :id_empresa)');
      ParamByName('id_empresa').AsInteger := AId;
      ExecSQL;
    end;
    DM_PRINCIPAL.ConfirmTransaction;
    NotifyCompanySucess('D', AId);
  except
    on e: Exception do
    begin
      DM_PRINCIPAL.CancelTransaction;
      NotifyCompanyFaild('Ocorreu um erro!' + e.Message);
    end;
  end;
end;

procedure TCompany.AddListener(AICompany: ICompany);
begin
  if not Assigned(FInterfaceList) then
    Exit;

  if FInterfaceList.IndexOf(AICompany) = -1 then
    FInterfaceList.Add(AICompany);
end;

procedure TCompany.RemListener(AICompany: ICompany);
begin
  if not Assigned(FInterfaceList) then
    Exit;

  if FInterfaceList.IndexOf(AICompany) > -1 then
    FInterfaceList.Remove(AICompany);
end;

procedure TCompany.NotifyCompanyFaild(AMsg: string);
var
  xICompany: ICompany;
  i: Integer;
begin
  try
    if not Assigned(FInterfaceList) then
      Exit;

    for i := 0 to FInterfaceList.Count - 1 do
    begin
      if Supports(FInterfaceList[i], ICompany, xICompany) then
        xICompany.OnFaild(AMsg);
    end;
  except

  end;
end;

procedure TCompany.NotifyCompanysucess(AStatus: string; AId: Integer);
var
  xICompany: ICompany;
  i: Integer;
begin
  try
    if not Assigned(FInterfaceList) then
      Exit;

    for i := 0 to FInterfaceList.Count - 1 do
    begin
      if Supports(FInterfaceList[i], ICompany, xICompany) then
        xICompany.OnSucess(AStatus, AId);
    end;
  except

  end;
end;

constructor TCompany.Create;
begin
  if not Assigned(FInterfaceList) then
    FInterfaceList := TInterfaceList.Create;
end;

class function TCompany.GetInstance: TCompany;
begin
  if not Assigned(Company) then
    Company := TCompany.Create;

  Result := Company;
end;

end.

