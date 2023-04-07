unit UGlobal;

interface

uses SysUtils;


type
  TGlobal = class
  private
  protected
  public
    function Encrypt(Atext: string; Akey: integer): string;
    function Decrypt(Atext: string; Akey: integer): string;
    function AsciiToInt(ACaracter: Char): Integer;
    constructor Create;
  end;

implementation

{ TGlobal }

constructor TGlobal.Create;
begin

end;

function TGlobal.AsciiToInt(ACaracter: Char): Integer;
var
  i: Integer;
begin
  i := 32;
  while i < 255 do
  begin
    if Chr(i) = ACaracter then
      Break;
    i := i + 1;
  end;
  Result := i;
end;

function TGlobal.Decrypt(Atext: string; Akey: integer): string;
var
  xcont: integer;
  xreturn: string;
begin
  if (trim(Atext) = EmptyStr) or (Akey = 0) then
  begin
    result := Atext;
  end
  else
  begin
    xreturn := '';
    for xcont := 1 to length(Atext) do
    begin
      xreturn := xreturn + chr(asciitoint(Atext[xcont]) - Akey);
    end;
    result := xreturn;
  end;
end;

function TGlobal.Encrypt(Atext: string; Akey: integer): string;
var
  xcont: integer;
  xreturn: string;
begin
  if (trim(Atext) = EmptyStr) or (Akey = 0) then
  begin
    result := Atext;
  end
  else
  begin
    xreturn := '';
    for xcont := 1 to length(Atext) do
    begin
      xreturn := xreturn + chr(asciitoint(Atext[xcont]) + Akey);
    end;
    result := xreturn;
  end;
end;

end.

