unit ULogin;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Buttons, StdCtrls, DB, IBCustomDataSet, IBQuery;

type
  TfrmLogin = class(TForm)
    Panel1: TPanel;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    edtLogin: TEdit;
    edtPassword: TEdit;
    Image1: TImage;
    btnLogin: TSpeedButton;
    chkViewPassword: TCheckBox;
    Image2: TImage;
    chkSaveUserPassword: TCheckBox;
    imgTools: TImage;
    QuyComandos: TIBQuery;
    procedure Timer1Timer(Sender: TObject);
    procedure chkViewPasswordClick(Sender: TObject);
    procedure btnLoginClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure imgToolsClick(Sender: TObject);
    procedure edtLoginKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    { Private declarations }
    FID_ACESSO: Integer;
    procedure SaveUserPassword(ASave: Boolean);
    function ValidateFields: Boolean;
  public
    { Public declarations }
  end;

var
  frmLogin: TfrmLogin;

implementation

uses
  UDM_PRINCIPAL, StrUtils, UParameters, UGlobal, UParametersScreen, UMenu, UMenuAccess;

{$R *.dfm}

procedure TfrmLogin.Timer1Timer(Sender: TObject);
begin
  StatusBar1.Panels.Items[1].Text := DateTimeToStr(Now);
end;

procedure TfrmLogin.chkViewPasswordClick(Sender: TObject);
begin
  if chkViewPassword.Checked then
    edtPassword.PasswordChar := #0
  else
    edtPassword.PasswordChar := '*';
end;

procedure TfrmLogin.SaveUserPassword(ASave: Boolean);
begin
  DM_PRINCIPAL.FParameters.FArqIni.WriteString('Geral', 'SaveUserPassword', BoolToStr(ASave));
  DM_PRINCIPAL.FParameters.FArqIni.WriteString('Geral', 'Login', ifthen(ASave, edtLogin.text, ''));
  DM_PRINCIPAL.FParameters.FArqIni.WriteString('Geral', 'Password', ifthen(ASave, DM_PRINCIPAL.FGlobal.Encrypt(edtPassword.text, 1010), ''));
  DM_PRINCIPAL.FParameters.FArqIni.WriteString('Geral', 'Access', IntToStr(FID_ACESSO));
end;

procedure TfrmLogin.btnLoginClick(Sender: TObject);
begin

  if ValidateFields then
    Exit;

  SaveUserPassword(chkSaveUserPassword.Checked);

  if not Assigned(frmMenu) then
    frmMenu := TfrmMenu.Create(self);
    
  frmMenu.Show;
end;

procedure TfrmLogin.FormCreate(Sender: TObject);
begin
  chkSaveUserPassword.Checked := DM_PRINCIPAL.FParameters.SaveUserPassword;
  edtLogin.Text := DM_PRINCIPAL.FParameters.Login;

  if trim(DM_PRINCIPAL.FParameters.Password) <> '' then
    edtPassword.Text := DM_PRINCIPAL.FGlobal.Decrypt(DM_PRINCIPAL.FParameters.Password, 1010);
end;

function TfrmLogin.ValidateFields: Boolean;

  function VerifyLogin: Boolean;
  begin
    with QuyComandos do
    begin
      Close;
      SQL.Clear;
      SQL.Add('select u.login, ID_ACESSO from TB_USUARIO u where u.login =:login');
      ParamByName('login').AsString := edtLogin.Text;
      Open;
      Last;
      First;
      FID_ACESSO := FieldByName('ID_ACESSO').AsInteger;
      Result := IsEmpty
    end;
  end;

  function VerifyPassword: Boolean;
  begin
    with QuyComandos do
    begin
      Close;
      SQL.Clear;
      SQL.Add('select u.SENHA from TB_USUARIO u where u.login =:login');
      ParamByName('login').AsString := edtLogin.Text;
      Open;
      Last;
      First;
      Result := edtPassword.Text = DM_PRINCIPAL.FGlobal.Decrypt(FieldByName('SENHA').AsString, 1010)
    end;
  end;

begin
  Result := False;
  if Trim(edtLogin.Text) = '' then
  begin
    Application.MessageBox('Por favor insira o Login!', 'Aten��o', MB_OK + MB_ICONWARNING);
    edtLogin.SetFocus;
    Result := True;
    Exit;
  end;

  if Trim(edtPassword.Text) = '' then
  begin
    Application.MessageBox('Por favor insira a Senha!', 'Aten��o', MB_OK + MB_ICONWARNING);
    edtPassword.SetFocus;
    Result := True;
    Exit;
  end;

  if VerifyLogin then
  begin
    Application.MessageBox('Esse Login n�o existe!', 'Aten��o', MB_OK + MB_ICONWARNING);
    edtLogin.SetFocus;
    Result := True;
    Exit;
  end;

  if not VerifyPassword then
  begin
    Application.MessageBox('Senha Incorreta!', 'Aten��o', MB_OK + MB_ICONWARNING);

    edtPassword.SetFocus;
    Result := True;
    Exit;
  end;

end;

procedure TfrmLogin.imgToolsClick(Sender: TObject);
begin
  if not Assigned(frmParameters) then
    frmParameters := TfrmParameters.Create(Self);

  frmParameters.ShowModal;
end;

procedure TfrmLogin.edtLoginKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_RETURN then
    btnLogin.Click;
end;

end.

