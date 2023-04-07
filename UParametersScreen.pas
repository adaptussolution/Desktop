unit UParametersScreen;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, Buttons, StdCtrls;

type
  TfrmParameters = class(TForm)
    Panel1: TPanel;
    lblPathBank: TLabel;
    edtPathBank: TEdit;
    btnPathBank: TSpeedButton;
    OpenDialog1: TOpenDialog;
    btnSave: TSpeedButton;
    procedure btnSaveClick(Sender: TObject);
    procedure btnPathBankClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure ValidatePathBank;
  public
    { Public declarations }
  end;

var
  frmParameters: TfrmParameters;

implementation

uses
  UDM_PRINCIPAL, UParameters;

{$R *.dfm}

{ TfrmParameters }

procedure TfrmParameters.ValidatePathBank;
begin
  if OpenDialog1.Execute then
    edtPathBank.Text := OpenDialog1.FileName;
    
  DM_PRINCIPAL.FParameters.FArqIni.WriteString('Geral', 'Caminho do banco', edtPathBank.Text);
end;

procedure TfrmParameters.btnSaveClick(Sender: TObject);
begin
  DM_PRINCIPAL.FParameters.PathBank :=  edtPathBank.Text;
  DM_PRINCIPAL.FParameters.FArqIni.WriteString('Geral', 'Caminho do banco', DM_PRINCIPAL.FParameters.PathBank);
  Application.MessageBox('Para aplicar as alterações o sistema têm que ser reiniciado!', 'Atenção', MB_OK + MB_ICONWARNING);
  Application.Terminate;
end;

procedure TfrmParameters.btnPathBankClick(Sender: TObject);
begin
  ValidatePathBank;
end;

procedure TfrmParameters.FormCreate(Sender: TObject);
begin
  edtPathBank.Text := DM_PRINCIPAL.FParameters.PathBank;
end;

end.

