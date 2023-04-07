unit Collec1;

interface

//  Note: TCollection and TCollectionItem are defined in Classes.Pas.

uses Classes;

type

  TEQuery = class;
  TContentType = (ctString, ctInteger, ctReal, ctDate);

  TEField = class(TCollectionItem)
  private
    FDisplayText: string;
    FInternalName: string;
    FContentType : TContentType;
    FVisible : Boolean;
    function GetDisplayName: string; override;
    procedure SetDisplayText(const Value: string);
    procedure SetInternalName(const Value: string);
    procedure SetContentType(const Value: TContentType);
    procedure SetVisible(const Value: Boolean);
  public
  	constructor Create(Collection: TCollection); override;
  published
    property InternanName: string read FInternalName write SetInternalName;
    property DisplayText: string read FDisplayText write SetDisplayText;
    property ContentType: TContentType read FContentType write SetContentType;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  TEFields = class(TCollection)
  private
    FEQuery: TEQuery;
    function GetItem(Index: Integer): TEField;
    procedure SetItem(Index: Integer; Value: TEField);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(EQuery: TEQuery);
    function Add: TEField;
    property Items[Index: Integer]: TEField
      read GetItem write SetItem; default;
  end;

  TEQuery = class(TComponent)
  private
    FSqlFrom : TStrings;
    FSqlOrderBy : TStrings;
    FFields: TEFields;
    procedure SetFields(Value: TEFields);
    function GetSqlFrom : TStrings;
    procedure SetSqlFrom(Value: TStrings);
    function GetSqlOrderBy : TStrings;
    procedure SetSqlOrderBy(Value: TStrings);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Fields: TEFields read FFields write SetFields;
    property SqlFrom: TStrings read GetSqlFrom write SetSqlFrom;
    property SqlOrderBy: TStrings read GetSqlOrderBy write SetSqlOrderBy;
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('Sample', [TEQuery]);
end;


{ TEField }


// Note: Inherited default behavior of GetDisplayName is to
// return the classname.

constructor TEField.Create(Collection: TCollection);
begin
  inherited Create(Collection);
	FVisible := True;
end;

function TEField.GetDisplayName: string;
begin
  Result := DisplayText;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TEField.SetContentType(const Value: TContentType);
begin
  if FContentType <> Value then
    FContentType := Value;
end;

procedure TEField.SetDisplayText(const Value: string);
begin
  if FDisplayText <> Value then
    FDisplayText := Value;
end;

procedure TEField.SetInternalName(const Value: string);
begin
  if FInternalName <> Value then
  begin
    FInternalName := Value;
    FDisplayText := Value;
  end;
end;

{ TEFields }

constructor TEFields.Create(EQuery: TEQuery);
begin
  inherited Create(TEField);
  FEQuery := EQuery;
end;

function TEFields.Add: TEField;
begin
  Result := TEField(inherited Add);
end;

function TEFields.GetItem(Index: Integer): TEField;
begin
  Result := TEField(inherited GetItem(Index));
end;

procedure TEFields.SetItem(Index: Integer;
        Value: TEField);
begin
  inherited SetItem(Index, Value);
end;

// Note: You must override GetOwner in Delphi 3.x to get
// correct streaming behavior.
function TEFields.GetOwner: TPersistent;
begin
  Result := FEQuery;
end;




{ TEQuery }

constructor TEQuery.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFields := TEFields.Create(Self);
  FSqlFrom := TStringList.Create;
end;

destructor TEQuery.Destroy;
begin
  FFields.Free;
  FSqlFrom.Free;
  inherited Destroy;
end;

function TEQuery.GetSqlFrom: TStrings;
begin
	Result := FSqlFrom;
end;

function TEQuery.GetSqlOrderBy: TStrings;
begin
	Result := FSqlOrderBy;
end;

procedure TEQuery.SetFields(Value: TEFields);
begin
  FFields.Assign(Value);
end;


procedure TEQuery.SetSqlFrom(Value: TStrings);
begin
	if Value <> FSqlFrom then
  	FSqlFrom.Assign(Value);
end;

procedure TEQuery.SetSqlOrderBy(Value: TStrings);
begin
	if Value <> FSqlOrderBy then
  	FSqlOrderBy.Assign(Value);
end;

procedure TEField.SetVisible(const Value: Boolean);
begin
	if Value <> FVisible then
  	FVisible := Value;
end;

end.
{--------------------------------------------------------------------}

