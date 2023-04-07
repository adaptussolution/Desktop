unit ufrmEditorHorarios;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons, Mask, SapiensCalendario, Grids, Math;

type
  TfrmEditorHorarios = class(TForm)
    EdtHoraIni: TMaskEdit;
    EdtHoraFim: TMaskEdit;
    BtAdiciona: TBitBtn;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    lblData: TLabel;
    BtRemove: TBitBtn;
    grdHorarios: TStringGrid;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure BtAdicionaClick(Sender: TObject);
    constructor Create(SapiensCalendario: TSapiensCalendario; dData: TDateTime; AOwner: TComponent);
    procedure FormKeyPress(Sender: TObject; var Key: Char);
    procedure BtRemoveClick(Sender: TObject);
  private
    { Private declarations }
    FDC : TDateContent;
    FSC : TSapiensCalendario;
    FData : TDateTime;
    procedure prcListaHoras;
  public
    { Public declarations }
  end;

var
  frmEditorHorarios: TfrmEditorHorarios;

implementation

{$R *.dfm}

procedure TfrmEditorHorarios.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
	//DC.Destroy;
  if Self.ModalResult = mrOk then
  begin
		//FSC.ListDate.RemoveDate(FData);
    if FDC.PeriodList.Count > 0 then
    	FSC.ListDate.AddDateContent(FDC);
  end;
	frmEditorHorarios := nil;
  Action := caFree;
end;

procedure TfrmEditorHorarios.BtAdicionaClick(Sender: TObject);
var
	hHoraIni, hHoraFim : TTime;
  PL : TPeriodList;
  P : TPeriod;
  i : Integer;
begin
	try
  	hHoraIni := StrToTime(EdtHoraIni.Text);
  except
  	On E: Exception do
    begin
    	ShowMessage('Hora Inicial inválida');
      EdtHoraIni.SetFocus;
      Abort;
    end;
  end;

	try
  	hHoraFim := StrToTime(EdtHoraFim.Text);
  except
  	On E: Exception do
    begin
    	ShowMessage('Hora Final inválida');
      EdtHoraFim.SetFocus;
      Abort;
    end;
  end;

  if hHoraFim < hHoraIni then
  begin
  	ShowMessage('Hora Final não pode ser inferior a Hora Inicial');
    EdtHoraIni.SetFocus;
    Abort;
  end;

  if FDC.PeriodList.AddPeriod(TPeriod.Create(hHoraIni, hHoraFim)) then
  begin
  	EdtHoraIni.Clear;
  	EdtHoraFim.Clear;
  end else
  	MessageDlg('Período inválido', mtError, [mbOk], 0);

{
  PL := FDC.PeriodList;
  for i := 0 to PL.Count -1 do
  begin
  	P := PL.GetPeriodByIndex(i);


    Lista.Items.Add(Format('%s a %s',[FormatDateTime('hh:nn',P.StartTime),
    																	FormatDateTime('hh:nn',P.EndTime)	]) );

  end;
}
	prcListaHoras;
  EdtHoraIni.SetFocus;
end;

constructor TfrmEditorHorarios.Create(
  SapiensCalendario: TSapiensCalendario; dData: TDateTime;
  AOwner: TComponent);
begin
	inherited Create(AOwner);

	FData := dData;

  FSC := SapiensCalendario;
  FDC := FSC.ListDate.GetByDate(dData);
  if (FDC = nil) then
		FDC := TDateContent.Create(dData, 1, tdPARCIAL);

	lblData.Caption := DateToStr(FData);
	prcListaHoras;
end;

procedure TfrmEditorHorarios.prcListaHoras;
var
	i : Integer;
  PL : TPeriodList;
  P : TPeriod;
begin
  // Lista.Items.Clear;
  PL := FDC.PeriodList;
	//grdHorarios.RowCount := 0;
  grdHorarios.RowCount := Math.Max(PL.Count + 1,2);
  grdHorarios.FixedRows := 1;
  grdHorarios.Cells[1,0] := 'Início';
  grdHorarios.Cells[2,0] := 'Término';

  grdHorarios.Cells[1,1] := '';
  grdHorarios.Cells[2,1] := '';


  for i := 0 to PL.Count -1 do
  begin
  	P := PL.GetPeriodByIndex(i);
    grdHorarios.Cells[1,i+1] := FormatDateTime('hh:nn',P.StartTime);
    grdHorarios.Cells[2,i+1] := FormatDateTime('hh:nn',P.EndTime);
    {
    Lista.Items.Add(Format('%s a %s',[FormatDateTime('hh:nn',P.StartTime),
    FormatDateTime('hh:nn',P.EndTime)	]) );
		}
  end;
  BtRemove.Enabled := (PL.Count > 0);
end;

procedure TfrmEditorHorarios.FormKeyPress(Sender: TObject; var Key: Char);
begin
	if (key = #13) then
   begin
			key:=#0;
			perform(WM_NEXTDLGCTL,0,0)
   end;
end;

procedure TfrmEditorHorarios.BtRemoveClick(Sender: TObject);
var
	iLinha : Integer;

  PL : TPeriodList;
begin
  iLinha := grdHorarios.Row -1;

  //ShowMessage('Linha selecionada: '+IntToStr(iLinha));

  PL := FDC.PeriodList;


  if (iLinha >= 0) and (iLinha <= (PL.Count -1)) then
  begin
  	PL.DeletePeriodIndex(iLinha);
    prcListaHoras;
  end;
  {
  for i := 0 to PL.Count -1 do
  begin
  	P := PL.GetPeriodByIndex(i);
    grdHorarios.Cells[1,i+1] := FormatDateTime('hh:nn',P.StartTime);
    grdHorarios.Cells[2,i+1] := FormatDateTime('hh:nn',P.EndTime);
  end;
  }
end;

end.
