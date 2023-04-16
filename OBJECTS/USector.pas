unit USector;

interface

uses
  Classes;

type
  ISector= interface
    ['{7EA4099E-8AA6-4350-9748-82C4698664B1}']
    procedure OnSucess(AStatus: string; AId: Integer);
    procedure OnFaild(AMsg: string);
  end;

  TSectorOBJECT = class
    ID_EMPRESA: Integer;
    ID_SETOR: Integer;
    NOME: string;
  end;

  TSector = class
  private
    FInterfaceList: TInterfaceList;
    procedure NotifySectorSucess(AStatus: string; AId: Integer);
    procedure NotifySectorFaild(AMsg: string);
  public
    procedure AddListener(AISector: ISector);
    procedure RemListener(AISector: ISector);
    procedure save(ASectorOBJECT: TSectorOBJECT; AStatus: string = 'I');
    procedure delete(AId: Integer);
    constructor Create;
    class function GetInstance(): TSector;
  end;

var
  Sector: TSector;

implementation

uses
  UDM_PRINCIPAL, SysUtils;

{ TSector }

procedure TSector.save(ASectorOBJECT: TSectorOBJECT; AStatus: string = 'I');
var
  xCodigo: string;
begin
  try
    DM_PRINCIPAL.StartTransaction;

    if AStatus = 'U' then
      xCodigo := IntToStr(ASectorOBJECT.id_empresa)
    else
      xCodigo := DM_PRINCIPAL.GetSequence('GEN_ID_SETOR');

    with DM_PRINCIPAL.IBQuery1 do
    begin
      Close;
      SQL.Clear;
      SQL.Add('update or insert into tb_setor (id_setor, id_empresa, nome) ');
      SQL.Add('values (:id_setor, :id_empresa, :nome) ');
      SQL.Add('matching (id_setor) ');

      ParamByName('id_setor').AsString := xCodigo;
      ParamByName('id_empresa').AsInteger := ASectorOBJECT.id_empresa;
      ParamByName('nome').AsString := ASectorOBJECT.nome;
      ExecSQL;
      DM_PRINCIPAL.ConfirmTransaction;
      ASectorOBJECT.id_setor := StrToInt(xCodigo);
      NotifySectorSucess(AStatus, ASectorOBJECT.id_setor);
    end;
  except
    on e: Exception do
    begin
      DM_PRINCIPAL.CancelTransaction;
      NotifySectorFaild(PCHAR('Ocorreu um erro!' + e.Message));
    end;
  end;
end;

procedure TSector.delete(AId: Integer);
begin
  try
    DM_PRINCIPAL.StartTransaction;
    with DM_PRINCIPAL.IBQuery1 do
    begin
      CLOSE;
      SQL.Clear;
      SQL.Add('delete from tb_setor where (id_setor = :id_setor)');
      ParamByName('id_setor').AsInteger := AId;
      ExecSQL;
    end;
    DM_PRINCIPAL.ConfirmTransaction;
    NotifySectorSucess('D', AId);
  except
    on e: Exception do
    begin
      DM_PRINCIPAL.CancelTransaction;
      NotifySectorFaild('Ocorreu um erro!' + e.Message);
    end;
  end;
end;

procedure TSector.AddListener(AISector: ISector);
begin
  if not Assigned(FInterfaceList) then
    Exit;

  if FInterfaceList.IndexOf(AISector) = -1 then
    FInterfaceList.Add(AISector);
end;

procedure TSector.RemListener(AISector: ISector);
begin
  if not Assigned(FInterfaceList) then
    Exit;

  if FInterfaceList.IndexOf(AISector) > -1 then
    FInterfaceList.Remove(AISector);
end;

procedure TSector.NotifySectorFaild(AMsg: string);
var
  xISector: ISector;
  i: Integer;
begin
  try
    if not Assigned(FInterfaceList) then
      Exit;

    for i := 0 to FInterfaceList.Count - 1 do
    begin
      if Supports(FInterfaceList[i], ISector, xISector) then
        xISector.OnFaild(AMsg);
    end;
  except

  end;
end;

procedure TSector.NotifySectorSucess(AStatus: string; AId: Integer);
var
  xISector: ISector;
  i: Integer;
begin
  try
    if not Assigned(FInterfaceList) then
      Exit;

    for i := 0 to FInterfaceList.Count - 1 do
    begin
      if Supports(FInterfaceList[i], ISector, xISector) then
        xISector.OnSucess(AStatus, AId);
    end;
  except

  end;
end;

constructor TSector.Create;
begin
  if not Assigned(FInterfaceList) then
    FInterfaceList := TInterfaceList.Create;
end;

class function TSector.GetInstance: TSector;
begin
  if not Assigned(Sector) then
    Sector := TSector.Create;

  Result := Sector;
end;

end.

