unit UFrmRelatorio;

interface

uses
Windows, Messages, SysUtils, Variants,
  Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, Buttons, ExtCtrls,
  RLXLSFilter, RLFilters, RLPDFFilter, RLDraftFilter, RLRichFilter, RLConsts,
  RLReport, ActnList,   Placemnt, DB, IBCustomDataSet, IBQuery;

type
  TFrmRelatorio = class(TForm)
    Panel1: TPanel;
    Panel2: TPanel;
    ActionList1: TActionList;
    Panel3: TPanel;
    ActSair: TAction;
    FormStorage1: TFormStorage;
    DsSource: TDataSource;
    btnvisualizar: TButton;
    btnimprimir: TButton;
    btnsair: TButton;
    Label1: TLabel;
    procedure btnSairClick(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure PngBitBtn3Click(Sender: TObject);
    procedure ActSairExecute(Sender: TObject);
    procedure btnImprimirClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FrmRelatorio: TFrmRelatorio;

implementation

{$R *.dfm}

uses UDM,RLPrintDialog;

procedure TFrmRelatorio.ActSairExecute(Sender: TObject);
begin
close;
end;

procedure TFrmRelatorio.btnImprimirClick(Sender: TObject);
begin

//if cbxtipoimp.ItemIndex = 1 then







end;

procedure TFrmRelatorio.btnSairClick(Sender: TObject);
begin
  Close;
end;

procedure TFrmRelatorio.FormKeyPress(Sender: TObject; var Key: Char);
begin
  If Key = #13 then // Se o comando for igual a enter
  Begin
    Key := #0;
    Perform(wm_nextdlgctl, 0, 0); // Para pular de campo em campo
  End;
end;

procedure TFrmRelatorio.PngBitBtn3Click(Sender: TObject);
begin
close;
end;

initialization

RLConsts.SetVersion(3, 72, 'B');

FINALIZATION

RLConsts.SetVersion(3, 72, 'B');

end.
