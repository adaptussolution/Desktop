unit UMenuAccess;

interface

uses
  IBQuery, Classes;

type
  IMenuAccess = interface
    ['{E5BB876D-BD57-465A-B8B9-615273C46C09}']
    procedure OnGetMenu;
  end;

  TMenuAccess = class
  private
    FInterfaceList: TInterfaceList;
    fPerson: Boolean;
    fUser: Boolean;
    fcompany: Boolean;
    fEmployee: Boolean;
    fSECTOR: Boolean;
    fOFFICE: Boolean;
    fAccess: Boolean;
    procedure NotifyGetMenu;
  protected
  public
    fUserLogged: Boolean;
    property Person: Boolean read fPerson write fPerson;
    property User: Boolean read fUser write fUser;
    property Company: Boolean read fcompany write fcompany;
    property Employee: Boolean read fEmployee write fEmployee;
    property SECTOR: Boolean read fSECTOR write fSECTOR;
    property OFFICE: Boolean read fOFFICE write fOFFICE;
    property Access: Boolean read fAccess write fAccess;
    procedure getId(AQuery: TIBQuery; AIdAcess: Integer);
    procedure AddListener(AIMenuAccess: IMenuAccess);
    procedure RemListener(AIMenuAccess: IMenuAccess);
    constructor Create(AQuery: TIBQuery; AIdAcess: Integer);
  end;

implementation

uses SysUtils;

{ TMenuAccess }

procedure TMenuAccess.AddListener(AIMenuAccess: IMenuAccess);
begin
  if not Assigned(FInterfaceList) then
    Exit;

  if FInterfaceList.IndexOf(AIMenuAccess) = -1 then
    FInterfaceList.Add(AIMenuAccess);
end;

constructor TMenuAccess.Create(AQuery: TIBQuery; AIdAcess: Integer);
begin
  if not Assigned(FInterfaceList) then
    FInterfaceList := TInterfaceList.Create;

  fUserLogged:= False;
    
  getId(AQuery, AIdAcess);
end;

procedure TMenuAccess.getId(AQuery: TIBQuery; AIdAcess: Integer);
begin
  with AQuery do
  begin
    Close;
    sql.Clear;
    SQL.Add('select nome as telas, flag from tb_acesso_item where id_acesso = :id_acesso');
    ParamByName('id_acesso').AsInteger := AIdAcess;
    Open;
  end;

  AQuery.First;
  while not AQuery.Eof do
  begin
    if AQuery.FieldByName('telas').AsString = 'PESSOA' then
      FPerson := AQuery.FieldByName('flag').AsString = 'Y';

    if AQuery.FieldByName('telas').AsString = 'USUÁRIO' then
      FUser := AQuery.FieldByName('flag').AsString = 'Y';

    if AQuery.FieldByName('telas').AsString = 'ACESSO' then
      FAccess := AQuery.FieldByName('flag').AsString = 'Y';

    if AQuery.FieldByName('telas').AsString = 'ACESSO' then
      FAccess := AQuery.FieldByName('flag').AsString = 'Y';

    if AQuery.FieldByName('telas').AsString = 'EMPRESA' then
      Fcompany := AQuery.FieldByName('flag').AsString = 'Y';

    if AQuery.FieldByName('telas').AsString = 'SETOR' then
      FSECTOR := AQuery.FieldByName('flag').AsString = 'Y';

    if AQuery.FieldByName('telas').AsString = 'CARGO' then
      FOFFICE := AQuery.FieldByName('flag').AsString = 'Y';

    if AQuery.FieldByName('telas').AsString = 'FUNCIONÁRIO' then
      FEmployee := AQuery.FieldByName('flag').AsString = 'Y';

    AQuery.Next;
  end;

  NotifyGetMenu;
end;

procedure TMenuAccess.NotifyGetMenu;
var
  xIMenuAccess: IMenuAccess;
  i: Integer;
begin
  try
    if not Assigned(FInterfaceList) then
      Exit;

    for i := 0 to FInterfaceList.Count - 1 do
    begin
      if Supports(FInterfaceList[i], IMenuAccess, xIMenuAccess) then
        xIMenuAccess.OnGetMenu;
    end;
  except

  end;
end;

procedure TMenuAccess.RemListener(AIMenuAccess: IMenuAccess);
begin
  if not Assigned(FInterfaceList) then
    Exit;

  if FInterfaceList.IndexOf(AIMenuAccess) > -1 then
    FInterfaceList.Remove(AIMenuAccess);
end;

end.

