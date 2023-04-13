unit UAdaptusGrid;

interface

uses
  WinTypes, WinProcs, Classes, DBGrids;

type
  TAdaptusGrid = class(TDBGrid)
  protected
    procedure Paint; override;
  end;

procedure Register;

implementation

procedure TAdaptusGrid.Paint;
begin
  SetScrollRange(Self.Handle, SB_VERT, 0, 0, False);
  inherited Paint;
end;

procedure Register;
begin
  RegisterComponents('Data Controls', [TAdaptusGrid]);
end;

end.

