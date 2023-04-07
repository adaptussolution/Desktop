unit uDBCtrlGridCns;

interface

uses
  SysUtils, Classes, Controls, Grids, DBGrids, StdCtrls, Math, Windows, Messages,
  dbcgrids, Dialogs, DB;


type
  TDBCtrlGridCns = class(TDBCtrlGrid)
  private
    FDataSetCns: TDataSet;

    FEditKey: TEdit;
    FEditCns: TEdit;
    FEdit1: TEdit;

    FWidthCns: Integer;

    FFieldKey: String;
    FFieldCns: String;
    FField1: String;

    function GetDataSetCns: TDataSet;
    procedure SetDataSetCns(value: TDataSet);

    function GetEditKey: TEdit;
    procedure SetEditKey(value: TEdit);
    function GetEditCns: TEdit;
    procedure SetEditCns(value: TEdit);
    function GetEdit1: TEdit;
    procedure SetEdit1(value: TEdit);

    function GetFieldKey: String;
    procedure SetFieldKey(value: String);
    function GetFieldCns: String;
    procedure SetFieldCns(value: String);
    function GetField1: String;
    procedure SetField1(value: String);
    function GetWidthCns: Integer;
    procedure SetWidthCns(value: Integer);

    procedure ConfigurarDBGrid;
    function VerificarCampos: Boolean;
    procedure WMWindowPosChanged(var message: TWMWindowPosChanged);
      message WM_WINDOWPOSCHANGED;
    { Private declarations }
  protected
    procedure DblClick; override;
    { Protected declarations }
  public
    constructor Create(Sender: TComponent); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    { Public declarations }
  published
    property DataSetCns: TDataSet read GetDataSetCns write SetDataSetCns;

    property EditKey: TEdit read GetEditKey write SetEditKey;
    property EditCns: TEdit read GetEditCns write SetEditCns;
    property Edit1: TEdit read GetEdit1 write SetEdit1;

    property FieldKey: String read GetFieldKey write SetFieldKey;
    property FieldCns: String read GetFieldCns write SetFieldCns;
    property Field1: String read GetField1 write SetField1;
    property WidthCns: Integer read GetWidthCns write SetWidthCns;

    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DardaniSistemas', [TDBCtrlGridCns]);
end;

constructor TDBCtrlGridCns.Create(Sender: TComponent);
begin
  inherited Create(Sender);

  ConfigurarDBGrid;
end;

procedure TDBCtrlGridCns.ConfigurarDBGrid;
begin
  Self.Visible := False;
  FWidthCns := 0;
end;

function TDBCtrlGridCns.GetDataSetCns: TDataSet;
begin
  Result := FDataSetCns;
end;

procedure TDBCtrlGridCns.SetDataSetCns(value: TDataSet);
begin
	if Value <> FDataSetCns then
  	FDataSetCns := value;
end;

function TDBCtrlGridCns.GetEditCns: TEdit;
begin
  Result := FEditCns;
end;

procedure TDBCtrlGridCns.SetEditCns(value: TEdit);
begin
	if Value <> FEditCns then
  	FEditCns := value;

  if FEditCns <> nil then
  begin
    Self.Left := FEditCns.Left;
    Self.Top := FEditCns.Top + FEditCns.Height;
    Self.Width := IfThen(FWidthCns > 0, FWidthCns, FEditCns.Width);
  end;
end;

function TDBCtrlGridCns.GetEdit1: TEdit;
begin
  Result := FEdit1;
end;

procedure TDBCtrlGridCns.SetEdit1(value: TEdit);
begin
	if Value <> FEdit1 then
  	FEdit1 := value;
end;

function TDBCtrlGridCns.GetEditKey: TEdit;
begin
  Result := FEditKey;
end;

procedure TDBCtrlGridCns.SetEditKey(value: TEdit);
begin
	if Value <> FEditKey then
  	FEditKey := value;
end;

function TDBCtrlGridCns.GetFieldCns: String;
begin
  Result := FFieldCns;
end;

procedure TDBCtrlGridCns.SetFieldCns(value: String);
begin
	FFieldCns := value;
end;

function TDBCtrlGridCns.GetField1: String;
begin
  Result := FField1;
end;

procedure TDBCtrlGridCns.SetField1(value: String);
begin
	FField1 := value;
end;

function TDBCtrlGridCns.GetFieldKey: String;
begin
  Result := FFieldKey;
end;

procedure TDBCtrlGridCns.SetFieldKey(value: String);
begin
	FFieldKey := value;
end;

procedure TDBCtrlGridCns.DblClick;
begin
  inherited DblClick;

  if not VerificarCampos then
    Exit;

  if not Self.DataSource.DataSet.IsEmpty then
  begin
    Self.EditCns.Text := Self.DataSource.DataSet.FieldByName(Self.FieldCns).AsString;
    Self.EditCns.SelStart := Length(Self.EditCns.Text) + 1;

    if (Self.FField1 <> '') and (Self.Edit1 <> nil) then
      Self.Edit1.Text := Self.DataSource.DataSet.FieldByName(Self.FField1).AsString;

    if (Self.FFieldKey <> '') and (Self.EditKey <> nil) then
      Self.EditKey.Text := Self.DataSource.DataSet.FieldByName(Self.FFieldKey).AsString;

    Self.Refresh;
    Self.Repaint;
    Self.Visible := False;
  end;
end;

procedure TDBCtrlGridCns.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(key, Shift);

  if not VerificarCampos then
    Exit;

  if Key = 38 then
  begin
    Self.DataSource.DataSet.Prior;
    Self.EditCns.SelStart := Length(Self.EditCns.Text) + 1;
  end
  else if Key = 40 then
  begin
    Self.DataSource.DataSet.Next;
    Self.EditCns.SelStart := Length(Self.EditCns.Text) + 1;
  end
  else if (Key = 13) and (not Self.DataSource.DataSet.IsEmpty) then
  begin
    Self.EditCns.Text := Self.DataSource.DataSet.FieldByName(Self.FFieldCns).AsString;

    if (Self.FFieldKey <> '') and (Self.EditKey <> nil) then
      Self.EditKey.Text := Self.DataSource.DataSet.FieldByName(Self.FFieldKey).AsString;

    if (Self.FField1 <> '') and (Self.Edit1 <> nil) then
      Self.Edit1.Text := Self.DataSource.DataSet.FieldByName(Self.FField1).AsString;

    Self.EditCns.SelStart := Length(Self.EditCns.Text) + 1;
    Self.Refresh;
    Self.Repaint;
    Self.Visible := False;
  end;
end;

function TDBCtrlGridCns.VerificarCampos: Boolean;
begin
  Result := False;

  if (Self.DataSource = nil) then
  begin
    //ShowMessage('DataSource n�o atribu�do.');
    Exit;
  end;

  if (Self.DataSource.DataSet = nil) then
  begin
    //ShowMessage('DataSet n�o atribu�do ao DataSource.');
    Exit;
  end;

  if (EditCns = nil) then
  begin
    //ShowMessage('Campo de consulta n�o definido');
    Exit;
  end;

  if (Trim(FieldCns) = '') then
  begin
    //ShowMessage('Campo de consulta n�o definido');
    Exit;
  end;

  Result := True;
end;

procedure TDBCtrlGridCns.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  Windows.ShowScrollBar(Handle, SB_VERT, True);
end;

function TDBCtrlGridCns.GetWidthCns: Integer;
begin
  Result := FWidthCns;
end;

procedure TDBCtrlGridCns.SetWidthCns(value: Integer);
begin
  FWidthCns := value;
end;

end.
