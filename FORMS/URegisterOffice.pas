unit URegisterOffice;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrmCadastro, DB, Provider, DBClient, IBCustomDataSet, IBQuery,
  ImgList, ActnList, StdCtrls, Grids, DBGrids, ComCtrls, DBCtrls, Buttons,
  ToolWin, ExtCtrls;

type
  TfrmRegisterOffice = class(TFrmCadastros)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRegisterOffice: TfrmRegisterOffice;

implementation

{$R *.dfm}

procedure TfrmRegisterOffice.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
  frmRegisterOffice := NIL;
end;

initialization
  RegisterClass(TfrmRegisterOffice);


finalization
  UnRegisterClass(TfrmRegisterOffice);

end.
