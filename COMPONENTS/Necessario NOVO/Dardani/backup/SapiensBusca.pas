unit SapiensBusca;

interface

uses Classes, SqlExpr, Controls, SysUtils, SapiensTypes, SapiensCriterios;

type
  TEmbQuery = class;

  TEmbQueryField = class(TCollectionItem)
  private
    FDisplayText: string;
    FInternalName: string;
    FDisplayFormat: String;
    FExpression : TStrings;
    FPrefix: String;
    FContentType : TContentType;
    FVisible : Boolean;
    FIndexed: Boolean;
    FDisplayWidth : Integer;
    function GetDisplayName: string; override;
    procedure SetDisplayText(const Value: string);
    procedure SetPrefix(const Value: String);
    procedure SetInternalName(const Value: string);
    procedure SetDisplayFormat(const Value: string);
    procedure SetContentType(const Value: TContentType);
    procedure SetVisible(const Value: Boolean);
    procedure SetIndexed(const Value: Boolean);
    function GetExpression : TStrings;
    procedure SetExpression(Value: TStrings);
    procedure SetDisplayWidth(Value: Integer);
  public
  	constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property InternalName: string read FInternalName write SetInternalName;
    property Prefix: string read FPrefix write SetPrefix;
    property DisplayFormat: string read FDisplayFormat write SetDisplayFormat;
    property DisplayText: string read FDisplayText write SetDisplayText;
    property DisplayWidth: Integer read FDisplayWidth write SetDisplayWidth;
    property ContentType: TContentType read FContentType write SetContentType;
    property Expression: TStrings read GetExpression write SetExpression;
    property Visible: Boolean read FVisible write SetVisible;
    property Indexed: Boolean read FIndexed write SetIndexed;
  end;

  TEmbQueryFields = class(TCollection)
  private
    FEQuery: TEmbQuery;
    function GetItem(Index: Integer): TEmbQueryField;
    procedure SetItem(Index: Integer; Value: TEmbQueryField);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(EQuery: TEmbQuery);
    function Add: TEmbQueryField;
    property Items[Index: Integer]: TEmbQueryField
      read GetItem write SetItem; default;
  end;

  TEmbQuery = class(TComponent)
  private
    FSqlFrom : TStrings;
    FSqlOrderBy : TStrings;
    FSqlWhere : TStrings;
    FExpression : TStrings;
    FFields: TEmbQueryFields;
    FSqlConnection : TSqlConnection;
    FCriteriaGroup : TEmbCriteriaGroup;
    FMultiRows : Boolean;
    FSelectDistinct : Boolean;
    procedure SetFields(Value: TEmbQueryFields);
    function GetSqlFrom : TStrings;
    function GetSqlWhere : TStrings;
    function GetSqlOrderBy : TStrings;
    function GetExpression : TStrings;
    procedure SetSqlFrom(Value: TStrings);
    procedure SetSqlWhere(Value: TStrings);
    procedure SetSqlOrderBy(Value: TStrings);
    procedure SetSqlConnection(Value: TSqlConnection);
    procedure SetCriteriaGroup(Value: TEmbCriteriaGroup);
    procedure SetMultiRows(Value: Boolean);
    procedure SetSelectDistinct(Value: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    procedure PrepareCriteriaGroup;
    destructor Destroy; override;
    function Execute : Boolean;
  published
    property Fields: TEmbQueryFields read FFields write SetFields;
    property SqlFrom: TStrings read GetSqlFrom write SetSqlFrom;
    property SqlWhere: TStrings read GetSqlWhere write SetSqlWhere;
    property SqlOrderBy: TStrings read GetSqlOrderBy write SetSqlOrderBy;
    property SqlConnection: TSqlConnection read FSqlConnection write SetSqlConnection;
    property Expression: TStrings read GetExpression;
    property CriteriaGroup: TEmbCriteriaGroup read FCriteriaGroup write SetCriteriaGroup;
    property MultiRows : Boolean read FMultiRows write SetMultiRows default False;
    property SelectDistinct : Boolean read FSelectDistinct write SetSelectDistinct default False;
  end;

procedure Register;

implementation

uses ufrmBusca;

procedure Register;
begin
  RegisterComponents('Embragec', [TEmbQuery]);
end;


{ TEmbQueryField }


// Note: Inherited default behavior of GetDisplayName is to
// return the classname.

constructor TEmbQueryField.Create(Collection: TCollection);
begin
  inherited Create(Collection);
	FExpression := TStringList.Create;
  FDisplayWidth := 0;
	FVisible := True;
end;

destructor TEmbQueryField.Destroy;
begin
	FExpression.Free;
  inherited;
end;

function TEmbQueryField.GetDisplayName: string;
begin
  Result := DisplayText;
  if Result = '' then Result := inherited GetDisplayName;
end;

function TEmbQueryField.GetExpression: TStrings;
begin
	Result := FExpression;
end;

procedure TEmbQueryField.SetContentType(const Value: TContentType);
begin
  if FContentType <> Value then
    FContentType := Value;
end;

procedure TEmbQueryField.SetDisplayFormat(const Value: string);
begin
  if FDisplayFormat <> Value then
    FDisplayFormat := Value;
end;

procedure TEmbQueryField.SetDisplayText(const Value: string);
begin
  if FDisplayText <> Value then
    FDisplayText := Value;
end;

procedure TEmbQueryField.SetDisplayWidth(Value: Integer);
begin
	if Value <> FDisplayWidth then
  	FDisplayWidth := Value;
end;

procedure TEmbQueryField.SetExpression(Value: TStrings);
begin
	if Value <> FExpression then
  	FExpression.Assign(Value);
end;

procedure TEmbQueryField.SetIndexed(const Value: Boolean);
begin
	if Value <> FIndexed then
  	FIndexed := Value;
end;

procedure TEmbQueryField.SetInternalName(const Value: string);
begin
  if FInternalName <> Value then
  begin
    FInternalName := Value;
    FDisplayText := Value;
  end;
end;

procedure TEmbQueryField.SetPrefix(const Value: String);
begin
  if FPrefix <> Value then
    FPrefix := Value;
end;

procedure TEmbQueryField.SetVisible(const Value: Boolean);
begin
	if Value <> FVisible then
  	FVisible := Value;
end;

{ TEmbQueryFields }

constructor TEmbQueryFields.Create(EQuery: TEmbQuery);
begin
  inherited Create(TEmbQueryField);
  FEQuery := EQuery;
end;

function TEmbQueryFields.Add: TEmbQueryField;
begin
  Result := TEmbQueryField(inherited Add);
end;

function TEmbQueryFields.GetItem(Index: Integer): TEmbQueryField;
begin
  Result := TEmbQueryField(inherited GetItem(Index));
end;

procedure TEmbQueryFields.SetItem(Index: Integer;
        Value: TEmbQueryField);
begin
  inherited SetItem(Index, Value);
end;

// Note: You must override GetOwner in Delphi 3.x to get
// correct streaming behavior.
function TEmbQueryFields.GetOwner: TPersistent;
begin
  Result := FEQuery;
end;


{ TEmbQuery }

constructor TEmbQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFields := TEmbQueryFields.Create(Self);
  FSqlFrom := TStringList.Create;
  FSqlWhere := TStringList.Create;
  FSqlOrderBy := TStringList.Create;
  FExpression := TStringList.Create;
  FSelectDistinct := False;
  FMultiRows := False;
end;

destructor TEmbQuery.Destroy;
begin
  FFields.Free;
  FSqlFrom.Free;
  FSqlWhere.Free;
  FSqlOrderBy.Free;
  FExpression.Free;
  inherited Destroy;
end;


function TEmbQuery.Execute : Boolean;
var
	sValor: String;
begin
	sValor := '';
  Result := False;
	try
		frmBusca := TfrmBusca.Cria(Self, nil);
    if (frmBusca.ShowModal = mrOk) then
    begin
      {
      if bMultiplos then
        sValor := frmBusca.buscaLista(sCampo)
      else
        sValor := frmBusca.buscaValor(sCampo);
      }
      Result := True;
    end;
   finally
      FreeAndNil(frmBusca);
   end;
end;

function TEmbQuery.GetSqlFrom: TStrings;
begin
	Result := FSqlFrom;
end;

function TEmbQuery.GetSqlOrderBy: TStrings;
begin
	Result := FSqlOrderBy;
end;

function TEmbQuery.GetSqlWhere: TStrings;
begin
	Result := FSqlWhere;
end;

procedure TEmbQuery.SetFields(Value: TEmbQueryFields);
begin
  FFields.Assign(Value);
end;


procedure TEmbQuery.SetSqlFrom(Value: TStrings);
begin
	if Value <> FSqlFrom then
  	FSqlFrom.Assign(Value);
end;

procedure TEmbQuery.SetSqlWhere(Value: TStrings);
begin
	if Value <> FSqlWhere then
  	FSqlWhere.Assign(Value);
end;

procedure TEmbQuery.SetSqlOrderBy(Value: TStrings);
begin
	if Value <> FSqlOrderBy then
  	FSqlOrderBy.Assign(Value);
end;

procedure TEmbQuery.SetSqlConnection(Value: TSqlConnection);
begin
{  if Value <> FSQLConnection then
  begin
    IsActive := Active;
    CheckInactive;
    if Assigned(FSQLConnection) and not FKeepConnection then
      SQLConnection.UnregisterTraceMonitor(Self);
    FSQLConnection := Value;
    if Assigned(FSQLConnection) then
    begin
      FSQLConnection.RegisterTraceMonitor(Self);
      Active := IsActive;
    end;


}
	if Value <> FSqlConnection then
  	FSqlConnection := Value;
end;

procedure TEmbQuery.SetCriteriaGroup(Value: TEmbCriteriaGroup);
begin
	if Value <> FCriteriaGroup then
  	FCriteriaGroup := Value;
end;

procedure TEmbQuery.PrepareCriteriaGroup;
var
	sFieldName : String;
	F : TEmbQueryField;
	CF : TEmbCriteriaField;
  i : Integer;
begin
	if FCriteriaGroup = nil then Exit;

	for i:= 0 to Self.FFields.Count-1 do
  begin
		F := TEmbQueryField(Self.FFields[i]);
    if F.Indexed then
    begin
      try
      	sFieldName := F.InternalName;
        if F.Prefix <> '' then
      		sFieldName := F.Prefix + '.' +sFieldName;
        CF :=  TEmbCriteriaField.Create(sFieldName, F.DisplayName, F.ContentType, FCriteriaGroup);
        FCriteriaGroup.Add(CF);
      except
      end;
    end;
  end;
end;

function TEmbQuery.GetExpression: TStrings;
var
	i : Integer;
  QF : TEmbQueryField;
  sCampo : String;
  sWhere, sFilter, sSqlWhere: String;
begin
	FExpression.Clear;
  if FSelectDistinct then
  	FExpression.Add('SELECT DISTINCT')
  else
		FExpression.Add('SELECT');

  for i := 0 to FFields.Count -1 do
  begin
    QF := FFields[i];
    if QF.Expression.Text <> '' then
    begin
      if (i < (FFields.Count -1)) then
        FExpression.Add(QF.Expression.Text + ', ')
      else
        FExpression.Add(QF.Expression.Text);
    end;

    if (QF.Expression.Text = '') and (QF.InternalName <> '') then
    begin
      sCampo := QF.InternalName;
      if QF.Prefix <> '' then
        sCampo := QF.Prefix + '.' + sCampo;
      if (i < (FFields.Count -1)) then
        sCampo := sCampo + ',';
      FExpression.Add(sCampo);
    end;
  end;
  FExpression.Add('FROM ' + FSqlFrom.Text);

  sSqlWhere := '';
  sWhere := Trim(FSqlWhere.Text);
  if FCriteriaGroup <> nil then
  	sFilter := Trim(FCriteriaGroup.GetExpression)
  else
  	sFilter := '';

  if (sFilter <> '') or (sWhere <> '') then
  begin
    sSqlWhere := 'WHERE ';
    if sWhere <> '' then
      sSqlWhere := sSqlWhere + sWhere;

    if (sFilter <> '') and (sWhere <> '') then
      sSqlWhere := sSqlWhere + ' AND ';

    if sFilter <> '' then
      sSqlWhere := sSqlWhere + sFilter;
  end;

  if Trim(sSqlWhere) <> '' then
  begin
    FExpression.Add(sSqlWhere);
  end;

  if FSqlOrderBy.Text <> '' then
  begin
    FExpression.Add('ORDER BY '+ FSqlOrderBy.Text);
  end;
  Result := FExpression;
end;

procedure TEmbQuery.SetMultiRows(Value: Boolean);
begin
	if Value <> FMultiRows then
		FMultiRows := Value;
end;

procedure TEmbQuery.SetSelectDistinct(Value: Boolean);
begin
	if Value <> FSelectDistinct then
  	FSelectDistinct := Value;
end;

end.


