unit URegisterSupplier;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrmCadastro, DB, Provider, DBClient, IBCustomDataSet, IBQuery,
  ImgList, ActnList, StdCtrls, Grids, DBGrids, ComCtrls, DBCtrls, Buttons,
  ToolWin, ExtCtrls;

type
  TfrmRegisterSupplier = class(TFrmCadastros)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRegisterSupplier: TfrmRegisterSupplier;

implementation

{$R *.dfm}

procedure TfrmRegisterSupplier.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
  frmRegisterSupplier := NIL;
end;

initialization
  RegisterClass(TfrmRegisterSupplier);


finalization
  UnRegisterClass(TfrmRegisterSupplier);

end.
