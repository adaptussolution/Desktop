program ADAPTUS;

uses
  Forms,
  ULogin in 'ULogin.pas' {frmLogin},
  UDM_PRINCIPAL in 'UDM_PRINCIPAL.pas' {DM_PRINCIPAL: TDataModule},
  UParameters in 'OBJECTS\UParameters.pas',
  UGlobal in 'OBJECTS\UGlobal.pas',
  UParametersScreen in 'UParametersScreen.pas' {frmParameters},
  UMenu in 'UMenu.pas' {frmMenu};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDM_PRINCIPAL, DM_PRINCIPAL);
  Application.CreateForm(TfrmLogin, frmLogin);
  Application.Run;
end.
