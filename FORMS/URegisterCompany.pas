unit URegisterCompany;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrmCadastro, DB, Provider, DBClient, IBCustomDataSet, IBQuery,
  ImgList, ActnList, StdCtrls, Grids, DBGrids, ComCtrls, DBCtrls, Buttons,
  ToolWin, ExtCtrls;

type
  TfrmRegisterCompany = class(TFrmCadastros)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRegisterCompany: TfrmRegisterCompany;

implementation

{$R *.dfm}

procedure TfrmRegisterCompany.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
  frmRegisterCompany := NIL;
end;

initialization
  RegisterClass(TfrmRegisterCompany);


finalization
  UnRegisterClass(TfrmRegisterCompany);

end.
