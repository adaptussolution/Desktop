unit UAccess;

interface

uses
  Classes, IBQuery;

type
  IAccess = interface
    ['{EEADCC31-AB51-4A8E-BF7E-F33B56381F63}']
    procedure OnSucess(AStatus: string; AId: Integer);
    procedure OnFaild(AMsg: string);
  end;

  TAccessOBJECT = class
    ID_Access: Integer;
    NOME: string;
  end;

  TAccessItemOBJECT = class
    ID_Access_item: Integer;
    ID_Access: Integer;
    NOME: string;
    flag: string;
  end;

  TAccess = class
  private
    FInterfaceList: TInterfaceList;

    procedure NotifyAccessFaild(AMsg: string);
  public
    procedure AddListener(AIAccess: IAccess);
    procedure RemListener(AIAccess: IAccess);
    procedure save(AAccessOBJECT: TAccessOBJECT; AStatus: string = 'I');
    procedure saveItem(AAccessItemOBJECT: TAccessItemOBJECT; AStatus: string = 'I');
    procedure delete(AId: Integer);
    procedure deleteItens(AId: Integer);
    procedure getItens(AQuery: TIBQuery; AId: Integer; AFlag: string);
    procedure NotifyAccessSucess(AStatus: string; AId: Integer);
    constructor Create;
    class function GetInstance(): TAccess;
  end;

var
  Access: TAccess;

implementation

uses
  UDM_PRINCIPAL, SysUtils;

{ TAccess }

procedure TAccess.AddListener(AIAccess: IAccess);
begin
  if not Assigned(FInterfaceList) then
    Exit;

  if FInterfaceList.IndexOf(AIAccess) = -1 then
    FInterfaceList.Add(AIAccess);
end;

constructor TAccess.Create;
begin
  if not Assigned(FInterfaceList) then
    FInterfaceList := TInterfaceList.Create;
end;

procedure TAccess.delete(AId: Integer);
begin
  try
    DM_PRINCIPAL.StartTransaction;
    with DM_PRINCIPAL.IBQuery1 do
    begin
      CLOSE;
      SQL.Clear;
      SQL.Add('delete from TB_ACESSO where (ID_ACESSO = :ID_ACESSO)');
      ParamByName('ID_ACESSO').AsInteger := AId;
      ExecSQL;
    end;
    DM_PRINCIPAL.ConfirmTransaction;
    NotifyAccessSucess('D', AId);
  except
    on e: Exception do
    begin
      DM_PRINCIPAL.CancelTransaction;
      NotifyAccessFaild('Ocorreu um erro!' + e.Message);
    end;
  end;
end;

procedure TAccess.deleteItens(AId: Integer);
begin
  try
    DM_PRINCIPAL.StartTransaction;
    with DM_PRINCIPAL.IBQuery1 do
    begin
      CLOSE;
      SQL.Clear;
      SQL.Add('delete from TB_ACESSO_ITEM where (ID_ACESSO = :ID_ACESSO)');
      ParamByName('ID_ACESSO').AsInteger := AId;
      ExecSQL;
    end;
    DM_PRINCIPAL.ConfirmTransaction;
  except
    on e: Exception do
    begin
      DM_PRINCIPAL.CancelTransaction;
      NotifyAccessFaild('Ocorreu um erro!' + e.Message);
    end;
  end;
end;

class function TAccess.GetInstance: TAccess;
begin
  if not Assigned(Access) then
    Access := TAccess.Create;

  Result := Access;
end;

procedure TAccess.getItens(AQuery: TIBQuery; AId: Integer; AFlag: string);
begin
  try
    with AQuery do
    begin
      CLOSE;
      SQL.Clear;
      //SQL.add('select * from TB_ACESSO_ITEM where ID_ACESSO = :ID_ACESSO AND FLAG = :FLAG');
      SQL.Text := 'select * from TB_ACESSO_ITEM where (ID_ACESSO = :ID_ACESSO) AND (FLAG = '''+AFlag+''')';
      ParamByName('ID_ACESSO').AsInteger := AId;
      //ParamByName('FLAG').AsString := AFlag;
      Open;
      Last;
      First;
    end;
  except
    on e: Exception do
    begin
      NotifyAccessFaild('Ocorreu um erro!' + e.Message);
    end;
  end;
end;

procedure TAccess.NotifyAccessFaild(AMsg: string);
var
  xIAccess: IAccess;
  i: Integer;
begin
  try
    if not Assigned(FInterfaceList) then
      Exit;

    for i := 0 to FInterfaceList.Count - 1 do
    begin
      if Supports(FInterfaceList[i], IAccess, xIAccess) then
        xIAccess.OnFaild(AMsg);
    end;
  except

  end;
end;

procedure TAccess.NotifyAccessSucess(AStatus: string; AId: Integer);
var
  xIAccess: IAccess;
  i: Integer;
begin
  try
    if not Assigned(FInterfaceList) then
      Exit;

    for i := 0 to FInterfaceList.Count - 1 do
    begin
      if Supports(FInterfaceList[i], IAccess, xIAccess) then
        xIAccess.OnSucess(AStatus, AId);
    end;
  except

  end;
end;

procedure TAccess.RemListener(AIAccess: IAccess);
begin
  if not Assigned(FInterfaceList) then
    Exit;

  if FInterfaceList.IndexOf(AIAccess) > -1 then
    FInterfaceList.Remove(AIAccess);
end;

procedure TAccess.save(AAccessOBJECT: TAccessOBJECT; AStatus: string);
var
  xCodigo: string;
begin
  try
    DM_PRINCIPAL.StartTransaction;

    if AStatus = 'U' then
      xCodigo := IntToStr(AAccessOBJECT.ID_Access)
    else
      xCodigo := DM_PRINCIPAL.GetSequence('GEB_ID_ACESSO');

    with DM_PRINCIPAL.IBQuery1 do
    begin
      Close;
      SQL.Clear;
      SQL.Add('update or insert into TB_ACESSO (ID_ACESSO, NOME) ');
      SQL.Add('values (:ID_ACESSO, :NOME) ');
      SQL.Add('matching (ID_ACESSO)');

      ParamByName('ID_ACESSO').AsString := xCodigo;
      ParamByName('NOME').AsString := AAccessOBJECT.NOME;
      ExecSQL;
      DM_PRINCIPAL.ConfirmTransaction;
      AAccessOBJECT.ID_Access := StrToInt(xCodigo);
    end;
  except
    on e: Exception do
    begin
      DM_PRINCIPAL.CancelTransaction;
      NotifyAccessFaild(PCHAR('Ocorreu um erro!' + e.Message));
    end;
  end;
end;

procedure TAccess.saveItem(AAccessItemOBJECT: TAccessItemOBJECT; AStatus: string);
var
  xCodigo: string;
begin
  try
    DM_PRINCIPAL.StartTransaction;

    if AStatus = 'U' then
      xCodigo := IntToStr(AAccessItemOBJECT.ID_Access_item)
    else
      xCodigo := DM_PRINCIPAL.GetSequence('GEB_ID_ACESSO_ITEM');

    with DM_PRINCIPAL.IBQuery1 do
    begin
      Close;
      SQL.Clear;
      SQL.Add('update or insert into TB_ACESSO_ITEM (ID_ACESSO_ITEM, ID_ACESSO, NOME, FLAG) ');
      SQL.Add('values (:ID_ACESSO_ITEM, :ID_ACESSO, :nome, :FLAG) ');
      SQL.Add('matching (ID_ACESSO_ITEM)');

      ParamByName('ID_ACESSO_ITEM').AsString := xCodigo;
      ParamByName('ID_ACESSO').AsInteger := AAccessItemOBJECT.ID_Access;
      ParamByName('NOME').AsString := AAccessItemOBJECT.NOME;
      ParamByName('FLAG').AsString := AAccessItemOBJECT.FLAG;
      ExecSQL;
      DM_PRINCIPAL.ConfirmTransaction;
      AAccessItemOBJECT.ID_Access_item := StrToInt(xCodigo);
    end;
  except
    on e: Exception do
    begin
      DM_PRINCIPAL.CancelTransaction;
      NotifyAccessFaild(PCHAR('Ocorreu um erro!' + e.Message));
    end;
  end;
end;

end.

