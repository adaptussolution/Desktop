unit uDBGridCns;

interface

uses
  SysUtils, Classes, Controls, Grids, DBGrids, StdCtrls, Math, Windows, Messages;


type
  TDBGridCns = class(TDBGrid)
  private
    FEdit: TCustomEdit;
    FEdit2: TCustomEdit;
    FColumnsAutoCreate: Boolean;
    FColumnsAutoCount: Integer;
    function GetEditCns: TCustomEdit;
    procedure SetEditCns(value: TCustomEdit);
    function GetEditCns2: TCustomEdit;
    procedure SetEditCns2(value: TCustomEdit);
    function GetColumnsAutoCreate: Boolean;
    procedure SetColumnsAutoCreate(value: Boolean);
    function GetColumnsAutoCount: Integer;
    procedure SetColumnsAutoCount(value: Integer);
    procedure ConfigurarDBGrid;
    function VerificarCampos: Boolean;
    procedure WMWindowPosChanged(var message: TWMWindowPosChanged);
      message WM_WINDOWPOSCHANGED;
    { Private declarations }
  protected
    procedure DblClick; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    { Protected declarations }
  public
    constructor Create(Sender: TComponent); override;
    { Public declarations }
  published
    property EditCns: TCustomEdit read GetEditCns write SetEditCns;
    property EditCns2: TCustomEdit read GetEditCns2 write SetEditCns2;
    property ColumnsAutoCreate: Boolean read GetColumnsAutoCreate write SetColumnsAutoCreate;
    property ColumnsAutoCount: Integer read GetColumnsAutoCount write SetColumnsAutoCount;
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('DardaniSistemas', [TDBGridCns]);
end;

constructor TDBGridCns.Create(Sender: TComponent);
begin
  inherited Create(Sender);

  SetColumnsAutoCreate(True);
  SetColumnsAutoCount(1);

  ConfigurarDBGrid;
end;

procedure TDBGridCns.ConfigurarDBGrid;
var
  qtde, i: Integer;
begin
  Self.Options := [dgColumnResize, dgAlwaysShowSelection];
  Self.Visible := False;
  if not FColumnsAutoCreate then
    Exit;

  qtde := Self.Columns.Count;

  for i := 1 to qtde do
    Self.Columns.Delete(i - 1);

  for i := 1 to FColumnsAutoCount do
    Self.Columns.Add;
end;

function TDBGridCns.GetEditCns: TCustomEdit;
begin
  Result := FEdit;
end;

procedure TDBGridCns.SetEditCns(value: TCustomEdit);
begin
	if Value <> FEdit then
  	FEdit := value;

  if FEdit <> nil then
  begin
    Self.Left := FEdit.Left;
    Self.Top := FEdit.Top + FEdit.Height;
    Self.Width := FEdit.Width;

    if Self.Columns.Count > 0 then
      Self.Columns[0].Width := Self.Width - 22;
  end;
end;

procedure TDBGridCns.DblClick;
begin
  inherited DblClick;

  if not VerificarCampos then
    Exit;

  if not Self.DataSource.DataSet.IsEmpty then
  begin
    Self.EditCns.Text := Self.Fields[0].AsString;
    Self.EditCns.SelStart := Length(Self.EditCns.Text) + 1;

    if (Self.EditCns2 <> nil) and (Self.Fields[1] <> nil) then
      Self.EditCns2.Text := Self.Fields[1].AsString;

    Self.Refresh;
    Self.Repaint;
    Self.Visible := False;
  end;
end;

procedure TDBGridCns.KeyDown(var Key: Word; Shift: TShiftState);
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
    Self.EditCns.Text := Self.Fields[0].AsString;

    if (Self.EditCns2 <> nil) and (Self.Fields[1] <> nil) then
      Self.EditCns2.Text := Self.Fields[1].AsString;

    Self.EditCns.SelStart := Length(Self.EditCns.Text) + 1;
    Self.Refresh;
    Self.Repaint;
    Self.Visible := False;
  end;
end;

function TDBGridCns.VerificarCampos: Boolean;
begin
  Result := False;

  if (Self.DataSource = nil) then
  begin
    //ShowMessage('DataSource não atribuído.');
    Exit;
  end;

  if (Self.DataSource.DataSet = nil) then
  begin
    //ShowMessage('DataSet não atribuído ao DataSource.');
    Exit;
  end;

  if (Self.FieldCount < 1) then
  begin
    //ShowMessage('Coluna não atribuída.');
    Exit;
  end;

  if (Self.Fields[0] = nil) then
  begin
    //ShowMessage('Coluna não atribuída.');
    Exit;
  end;

  if (EditCns = nil) then
  begin
    //ShowMessage('Campo de consulta não definido');
    Exit;
  end;

  Result := True;
end;

function TDBGridCns.GetColumnsAutoCount: Integer;
begin
  Result := FColumnsAutoCount;
end;

function TDBGridCns.GetColumnsAutoCreate: Boolean;
begin
  Result := FColumnsAutoCreate;  
end;

procedure TDBGridCns.SetColumnsAutoCount(value: Integer);
begin
	if Value <> FColumnsAutoCount then
    FColumnsAutoCount := IfThen(value < 1, 1, value);
end;

procedure TDBGridCns.SetColumnsAutoCreate(value: Boolean);
begin
	if Value <> FColumnsAutoCreate then
  	FColumnsAutoCreate := value;
end;

procedure TDBGridCns.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  Windows.ShowScrollBar(Handle, SB_VERT, True);
end;

function TDBGridCns.GetEditCns2: TCustomEdit;
begin
  Result := FEdit2;
end;

procedure TDBGridCns.SetEditCns2(value: TCustomEdit);
begin
	if Value <> FEdit2 then
  	FEdit2 := value;
end;

end.
