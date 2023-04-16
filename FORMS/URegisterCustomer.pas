unit URegisterCustomer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrmCadastro, DB, Provider, DBClient, IBCustomDataSet, IBQuery,
  ImgList, ActnList, StdCtrls, Grids, DBGrids, ComCtrls, DBCtrls, Buttons,
  ToolWin, ExtCtrls;

type
  TfrmRegisterCustomer = class(TFrmCadastros)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRegisterCustomer: TfrmRegisterCustomer;

implementation

{$R *.dfm}

procedure TfrmRegisterCustomer.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
  frmRegisterCustomer := NIL;
end;

initialization
  RegisterClass(TfrmRegisterCustomer);


finalization
  UnRegisterClass(TfrmRegisterCustomer);

end.
