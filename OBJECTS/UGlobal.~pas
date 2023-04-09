unit UGlobal;

interface

uses
  SysUtils, XMLDoc, XMLIntf;

type

  TCep = class
    logradouro: string;
    complemento: string;
    bairro: string;
    cidade: string;
    uf: string;
  end;

  TGlobal = class
  private
  protected
  public
    function ConsultaCEP(Cep: string; AXMLDocument: TXMLDocument): TCep;
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

function TGlobal.ConsultaCEP(Cep: string; AXMLDocument: TXMLDocument): TCep;
var
  xtempXML: IXMLNode;
  xtempNodePAI: IXMLNode;
  xtempNodeFilho: IXMLNode;
  xI: Integer;
  xCep: TCep;
begin
  try
    xCep := TCep.Create;
    AXMLDocument.FileName := 'https://viacep.com.br/ws/' + Trim(Cep) + '/xml/';
    AXMLDocument.Active := true;

    xtempXML := AXMLDocument.DocumentElement;

    if xtempXML.ChildNodes.FindNode('logradouro') = nil then
      Exit; 

    xtempNodePAI := xtempXML.ChildNodes.FindNode('logradouro');
    for xI := 0 to xtempNodePAI.ChildNodes.Count - 1 do
    begin
      xtempNodeFilho := xtempNodePAI.ChildNodes[xI];
      xCep.logradouro :=  xtempNodeFilho.Text;
    end;

    xtempNodePAI := xtempXML.ChildNodes.FindNode('bairro');
    for xI := 0 to xtempNodePAI.ChildNodes.Count - 1 do
    begin
      xtempNodeFilho := xtempNodePAI.ChildNodes[xI];
      xCep.bairro :=  xtempNodeFilho.Text;
    end;

    xtempNodePAI := xtempXML.ChildNodes.FindNode('localidade');
    for xI := 0 to xtempNodePAI.ChildNodes.Count - 1 do
    begin
      xtempNodeFilho := xtempNodePAI.ChildNodes[xI];
      xCep.cidade :=  xtempNodeFilho.Text;
    end;

    xtempNodePAI := xtempXML.ChildNodes.FindNode('uf');
    for xI := 0 to xtempNodePAI.ChildNodes.Count - 1 do
    begin
      xtempNodeFilho := xtempNodePAI.ChildNodes[xI];
      xCep.uf :=  xtempNodeFilho.Text;
    end;

//    tempNodePAI := xtempXML.ChildNodes.FindNode('ibge');
//    for xI := 0 to xtempNodePAI.ChildNodes.Count - 1 do
//    begin
//      xtempNodeFilho := xtempNodePAI.ChildNodes[xI];
//    end;
//
//    tempNodePAI := xtempXML.ChildNodes.FindNode('gia');
//    for xI := 0 to xtempNodePAI.ChildNodes.Count - 1 do
//    begin
//      xtempNodeFilho := xtempNodePAI.ChildNodes[xI];
//    end;
  finally
    Result := xCep;
  end;
end;

end.

