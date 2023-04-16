unit UOffice;

interface

uses
  Classes;

type
  IOffice= interface
    ['{7EA4099E-8AA6-4350-9748-82C4698664B1}']
    procedure OnSucess(AStatus: string; AId: Integer);
    procedure OnFaild(AMsg: string);
  end;

  TOfficeOBJECT = class
    ID_SETOR: Integer;
    ID_CARGO: Integer;
    NOME: string;
    SALARIO: Double;
  end;

  TOffice = class
  private
    FInterfaceList: TInterfaceList;
    procedure NotifyOfficeSucess(AStatus: string; AId: Integer);
    procedure NotifyOfficeFaild(AMsg: string);
  public
    procedure AddListener(AIOffice: IOffice);
    procedure RemListener(AIOffice: IOffice);
    procedure save(AOfficeOBJECT: TOfficeOBJECT; AStatus: string = 'I');
    procedure delete(AId: Integer);
    constructor Create;
    class function GetInstance(): TOffice;
  end;

var
  Office: TOffice;

implementation

uses
  UDM_PRINCIPAL, SysUtils;

{ TSector }

procedure TOffice.save(AOfficeOBJECT: TOfficeOBJECT; AStatus: string = 'I');
var
  xCodigo: string;
begin
  try
    DM_PRINCIPAL.StartTransaction;

    if AStatus = 'U' then
      xCodigo := IntToStr(AOfficeOBJECT.ID_CARGO)
    else
      xCodigo := DM_PRINCIPAL.GetSequence('GEN_ID_CARGO');

    with DM_PRINCIPAL.IBQuery1 do
    begin
      Close;
      SQL.Clear;
      SQL.Add('update or insert into tb_cargo (id_cargo, id_setor, nome, salario) ');
      SQL.Add('values (:id_cargo, :id_setor, :nome, :salario) ');
      SQL.Add('matching (id_cargo) ');

      ParamByName('id_cargo').AsString := xCodigo;
      ParamByName('id_setor').AsInteger := AOfficeOBJECT.id_setor;
      ParamByName('nome').AsString := AOfficeOBJECT.nome;
      ParamByName('salario').AsFloat := AOfficeOBJECT.salario;
      ExecSQL;
      DM_PRINCIPAL.ConfirmTransaction;
      AOfficeOBJECT.id_cargo := StrToInt(xCodigo);
      NotifyOfficeSucess(AStatus, AOfficeOBJECT.id_cargo);
    end;
  except
    on e: Exception do
    begin
      DM_PRINCIPAL.CancelTransaction;
      NotifyOfficeFaild(PCHAR('Ocorreu um erro!' + e.Message));
    end;
  end;
end;

procedure TOffice.delete(AId: Integer);
begin
  try
    DM_PRINCIPAL.StartTransaction;
    with DM_PRINCIPAL.IBQuery1 do
    begin
      CLOSE;
      SQL.Clear;
      SQL.Add('delete from tb_cargo where (id_cargo = :id_cargo)');
      ParamByName('id_cargo').AsInteger := AId;
      ExecSQL;
    end;
    DM_PRINCIPAL.ConfirmTransaction;
    NotifyOfficeSucess('D', AId);
  except
    on e: Exception do
    begin
      DM_PRINCIPAL.CancelTransaction;
      NotifyOfficeFaild('Ocorreu um erro!' + e.Message);
    end;
  end;
end;

procedure TOffice.AddListener(AIOffice: IOffice);
begin
  if not Assigned(FInterfaceList) then
    Exit;

  if FInterfaceList.IndexOf(AIOffice) = -1 then
    FInterfaceList.Add(AIOffice);
end;

procedure TOffice.RemListener(AIOffice: IOffice);
begin
  if not Assigned(FInterfaceList) then
    Exit;

  if FInterfaceList.IndexOf(AIOffice) > -1 then
    FInterfaceList.Remove(AIOffice);
end;

procedure TOffice.NotifyOfficeFaild(AMsg: string);
var
  xIOffice: IOffice;
  i: Integer;
begin
  try
    if not Assigned(FInterfaceList) then
      Exit;

    for i := 0 to FInterfaceList.Count - 1 do
    begin
      if Supports(FInterfaceList[i], IOffice, xIOffice) then
        xIOffice.OnFaild(AMsg);
    end;
  except

  end;
end;

procedure TOffice.NotifyOfficesucess(AStatus: string; AId: Integer);
var
  xIOffice: IOffice;
  i: Integer;
begin
  try
    if not Assigned(FInterfaceList) then
      Exit;

    for i := 0 to FInterfaceList.Count - 1 do
    begin
      if Supports(FInterfaceList[i], IOffice, xIOffice) then
        xIOffice.OnSucess(AStatus, AId);
    end;
  except

  end;
end;

constructor TOffice.Create;
begin
  if not Assigned(FInterfaceList) then
    FInterfaceList := TInterfaceList.Create;
end;

class function TOffice.GetInstance: TOffice;
begin
  if not Assigned(Office) then
    Office := TOffice.Create;

  Result := Office;
end;

end.

