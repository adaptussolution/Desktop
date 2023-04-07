unit EmbResultTable;

interface

uses Classes, Controls, SysUtils, Variants;

type
  TEmbResultField = class
  private
		FName : String;
    FValue : Variant;
  	constructor Create(sName : String; oValue : Variant);
	public
  	function AsInteger : Integer;
    function AsString : String;
    function AsDateTime : TDateTime;
    function AsFloat : Double;
	published
  	property Name : String read FName;
    property Value : Variant read FValue;
  end;

  TEmbResultRow = class
  private
  	FFields : TList;
  public
  	constructor Create;
		function Fields : TList;
    function Count : Integer;
    procedure Add(sName: String; vValue : Variant); overload;
    procedure Add(F: TEmbResultField); overload;
    procedure Delete(Index : Integer);
    function FieldByName(sName: String) : TEmbResultField; overload;
    function FieldByIndex(Index: Integer) : TEmbResultField; overload;
    procedure Clear;
    destructor Destroy;
  published
  end;

  TEmbResultTable = class
  private
  	FLines : TList;
    FPosition : Integer;
  public
    constructor Create;
    function Lines : TList;
    function Count: Integer;
		procedure Add(Row: TEmbResultRow);
    procedure Delete(Index : Integer);
    procedure First;
    procedure Next;
    procedure Last;
    procedure Prior;
    function FieldByName(sName: String) : TEmbResultField; overload;
    function FieldByIndex(Index: Integer) : TEmbResultField; overload;
    procedure Clear;
    destructor Destroy;
  published
    property Position : Integer read FPosition;
  end;

implementation


{ TEmbResultField }

function TEmbResultField.AsDateTime: TDateTime;
begin
//
end;

function TEmbResultField.AsFloat: Double;
begin
//
end;

function TEmbResultField.AsInteger: Integer;
begin
//
end;

function TEmbResultField.AsString: String;
begin
//
end;

constructor TEmbResultField.Create(sName: String; oValue: Variant);
begin
	FName  := sName;
  FValue := oValue;
end;

{ TEmbResultRow }

procedure TEmbResultRow.Add(sName: String; vValue: Variant);
begin
//
end;

procedure TEmbResultRow.Add(F: TEmbResultField);
begin
//
end;

procedure TEmbResultRow.Clear;
begin
//
end;

function TEmbResultRow.Count: Integer;
begin
//
end;

constructor TEmbResultRow.Create;
begin
	FFields := TList.Create;
end;

procedure TEmbResultRow.Delete(Index: Integer);
begin
//
end;

destructor TEmbResultRow.Destroy;
begin
	if FFields <> nil then
  begin
  	// Primeiro deve-se remover todos os campos
		FFields.Free;
  end;
end;

function TEmbResultRow.FieldByIndex(Index: Integer): TEmbResultField;
begin
//
end;

function TEmbResultRow.FieldByName(sName: String): TEmbResultField;
begin
//
end;

function TEmbResultRow.Fields: TList;
begin
//
end;

{ TEmbResultTable }

procedure TEmbResultTable.Add(Row: TEmbResultRow);
begin
//
end;

procedure TEmbResultTable.Clear;
begin
//
end;

function TEmbResultTable.Count: Integer;
begin
//
end;

constructor TEmbResultTable.Create;
begin
	FLines := TList.Create;
end;

procedure TEmbResultTable.Delete(Index: Integer);
begin
	if FLines <> nil then
  begin
  	FLines.Free;
  end;
end;

destructor TEmbResultTable.Destroy;
begin
//
end;

function TEmbResultTable.FieldByIndex(Index: Integer): TEmbResultField;
begin
//
end;

function TEmbResultTable.FieldByName(sName: String): TEmbResultField;
begin
//
end;

procedure TEmbResultTable.First;
begin
//
end;

procedure TEmbResultTable.Last;
begin
//
end;

function TEmbResultTable.Lines: TList;
begin
//
end;

procedure TEmbResultTable.Next;
begin
//
end;

procedure TEmbResultTable.Prior;
begin
//
end;

end.
