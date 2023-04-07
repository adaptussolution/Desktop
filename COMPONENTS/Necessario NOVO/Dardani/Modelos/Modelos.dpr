program Modelos;

uses
  Forms,
  ufrmPrincipalModelos in 'ufrmPrincipalModelos.pas' {frmPrincipalModelos},
  FolhaModelos in 'FolhaModelos.pas',
  ufrmDlgInspetorObjetos in 'ufrmDlgInspetorObjetos.pas' {frmDlgInspetorObjetos},
  ufrmDlgNovoCampo in 'ufrmDlgNovoCampo.pas' {frmDlgNovoCampo},
  FundoModelo in 'FundoModelo.pas',
  PanelModelo in 'PanelModelo.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmPrincipalModelos, frmPrincipalModelos);
  Application.CreateForm(TfrmDlgInspetorObjetos, frmDlgInspetorObjetos);
  Application.Run;
end.
