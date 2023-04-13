unit URegisterSector;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrmCadastro, DB, Provider, DBClient, IBCustomDataSet, IBQuery,
  ImgList, ActnList, StdCtrls, Grids, DBGrids, ComCtrls, DBCtrls, Buttons,
  ToolWin, ExtCtrls;

type
  TfrmRegisterSector = class(TFrmCadastros)
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmRegisterSector: TfrmRegisterSector;

implementation

{$R *.dfm}

procedure TfrmRegisterSector.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Action := caFree;
  frmRegisterSector := NIL;
end;

initialization
  RegisterClass(TfrmRegisterSector);


finalization
  UnRegisterClass(TfrmRegisterSector);

end.
