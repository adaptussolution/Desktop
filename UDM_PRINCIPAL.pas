unit UDM_PRINCIPAL;

interface

uses
  SysUtils, Classes, DB, IBDatabase, IBCustomDataSet, IBQuery, UParameters, UGlobal,
  Dialogs;

type
  TDM_PRINCIPAL = class(TDataModule)
    IBTransaction1: TIBTransaction;
    IBQuery1: TIBQuery;
    IBDatabase1: TIBDatabase;
    OpenDialog1: TOpenDialog;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    function ConnectBank(APath: string): Boolean;
    procedure ValidatePathBank;
    procedure StartTransaction;
    procedure ConfirmTransaction;
    procedure CancelTransaction;
    function GetSequence(AGENERATOR: string): string;
  public
    { Public declarations }
    FParameters: TParameters;
    FGlobal: TGlobal;
  end;

var
  DM_PRINCIPAL: TDM_PRINCIPAL;

implementation

uses
  FORMS, WINDOWS;

{$R *.dfm}

procedure TDM_PRINCIPAL.CancelTransaction;
begin
  if IBTransaction1.InTransaction then
    IBTransaction1.Rollback;
end;

procedure TDM_PRINCIPAL.ConfirmTransaction;
begin
  if IBTransaction1.InTransaction then
    IBTransaction1.Commit;
end;

function TDM_PRINCIPAL.ConnectBank(APath: string): Boolean;
begin
  try
    with IBDatabase1 do
    begin
      Connected := false;
      DatabaseName := APath;
      Connected := True;
      IBTransaction1.Active := True;

      if Connected then
        Result := True;
    end;
  except
    on e: Exception do
    begin
      Application.MessageBox('Não foi possível conectar no banco de dados, Entre em contato com o suporte', 'Atenção', MB_OK + MB_ICONWARNING);
      Abort;
      Result := false;
    end;
  end;
end;

procedure TDM_PRINCIPAL.DataModuleCreate(Sender: TObject);
begin
  FGlobal := TGlobal.Create;
  FParameters := TParameters.Create;
  ValidatePathBank;
  ConnectBank(FParameters.PathBank);
end;

procedure TDM_PRINCIPAL.DataModuleDestroy(Sender: TObject);
begin
  FreeAndNil(FParameters);
end;

function TDM_PRINCIPAL.GetSequence(AGENERATOR: string): string;
var
  query: TIBQuery;
begin
  query := TIBQuery.Create(self);
  with query do
  begin
    Database := IBDatabase1;
    Transaction := IBTransaction1;
    SQL.Clear;
    SQL.Add('Select gen_id(' + AGENERATOR + ',1) from  RDB$DATABASE');
    Open;
    Result := Fields[0].AsString;
    CLOSE;
  end;
  FreeAndNil(query);
end;

procedure TDM_PRINCIPAL.StartTransaction;
begin
  if IBTransaction1.InTransaction then
  begin
    IBTransaction1.Commit;
  end;
  IBTransaction1.StartTransaction;
end;

procedure TDM_PRINCIPAL.ValidatePathBank;
begin
  if FParameters.PathBank = '' then
  begin
    if OpenDialog1.Execute then
      FParameters.PathBank := OpenDialog1.FileName;
  end;
  FParameters.FArqIni.WriteString('Geral', 'Caminho do banco', FParameters.PathBank);
end;

end.

