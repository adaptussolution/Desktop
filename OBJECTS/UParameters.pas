unit UParameters;

interface

uses
  INIFILES, SysUtils, Forms;

type
  TParameters = class
  private
    fPathBank: string;
    fSaveUserPassword: Boolean;
    fLogin: string;
    fPassword: string;
    procedure getPathBank(const Value: string);
    function setPathBank: string;
  protected
    procedure getPassword(const Value: string);
    function setPassword: string;
  public
    FArqIni: TIniFile;
    property PathBank: string read setPathBank write getPathBank;
    property SaveUserPassword: Boolean read fSaveUserPassword write fSaveUserPassword;
    property Login: string read fLogin write fLogin;
    property Password: string read setPassword write getPassword;
    constructor Create;
  end;

implementation

{ TParameters }

constructor TParameters.Create;
begin
  FArqIni := TIniFile.create(ExtractFilePath(Application.ExeName) + 'Config.ini');

  fPathBank := FArqIni.readstring('Geral', 'Caminho do banco', fPathBank);
  fSaveUserPassword := StrToBool(FArqIni.readstring('Geral', 'SaveUserPassword', BoolToStr(fSaveUserPassword)));
  fLogin := FArqIni.readstring('Geral', 'Login', fLogin);
  fPassword := FArqIni.readstring('Geral', 'Password', fPassword);
end;

procedure TParameters.getPassword(const Value: string);
begin
  fPassword := Value;
end;

procedure TParameters.getPathBank(const Value: string);
begin
  fPathBank := Value;
end;

function TParameters.setPassword: string;
begin
  Result := fPassword;
end;

function TParameters.setPathBank: string;
begin
  Result := fPathBank;
end;

end.

