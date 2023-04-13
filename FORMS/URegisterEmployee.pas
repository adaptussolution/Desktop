unit URegisterEmployee;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrmCadastro, DB, Provider, DBClient, IBCustomDataSet, IBQuery,
  ImgList, ActnList, StdCtrls, Grids, DBGrids, ComCtrls, DBCtrls, Buttons,
  ToolWin, ExtCtrls;

type
  TfrmRegisterEmployee = class(TFrmCadastros)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRegisterEmployee: TfrmRegisterEmployee;

implementation

{$R *.dfm}

procedure TfrmRegisterEmployee.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
  frmRegisterEmployee := NIL;
end;

initialization
  RegisterClass(TfrmRegisterEmployee);


finalization
  UnRegisterClass(TfrmRegisterEmployee);

end.
