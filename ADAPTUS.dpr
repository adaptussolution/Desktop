program ADAPTUS;

uses
  Forms,
  ULogin in 'ULogin.pas' {frmLogin},
  UDM_PRINCIPAL in 'UDM_PRINCIPAL.pas' {DM_PRINCIPAL: TDataModule},
  UParameters in 'OBJECTS\UParameters.pas',
  UGlobal in 'OBJECTS\UGlobal.pas',
  UParametersScreen in 'UParametersScreen.pas' {frmParameters},
  UMenu in 'UMenu.pas' {frmMenu},
  UFrmFiltro in 'INHERITANCE\UFrmFiltro.pas' {FrmFiltro},
  FrmCadastro in 'INHERITANCE\FrmCadastro.pas' {FrmCadastros},
  UPerson in 'OBJECTS\UPerson.pas',
  URegisterPerson in 'FORMS\URegisterPerson.pas' {frmRegisterPerson},
  URegisterUser in 'FORMS\URegisterUser.pas' {frmRegisterUser},
  UUser in 'OBJECTS\UUser.pas',
  URegisterCompany in 'FORMS\URegisterCompany.pas' {frmRegisterCompany},
  URegisterSector in 'FORMS\URegisterSector.pas' {frmRegisterSector},
  URegisterOffice in 'FORMS\URegisterOffice.pas' {frmRegisterOffice},
  URegisterEmployee in 'FORMS\URegisterEmployee.pas' {frmRegisterEmployee},
  URegisterAccess in 'FORMS\URegisterAccess.pas' {frmRegisterAccess},
  UAccess in 'OBJECTS\UAccess.pas',
  UMenuAccess in 'OBJECTS\UMenuAccess.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDM_PRINCIPAL, DM_PRINCIPAL);
  Application.CreateForm(TfrmLogin, frmLogin);
  Application.Run;
end.
