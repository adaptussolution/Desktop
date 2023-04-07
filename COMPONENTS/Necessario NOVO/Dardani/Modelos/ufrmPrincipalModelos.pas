unit ufrmPrincipalModelos;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DBXpress, DB, SqlExpr, FolhaModelos, FMTBcd, Buttons,
  ExtCtrls, QuickRpt, Grids, DBGrids, PanelModelo, SapiensBusca;

type
  TfrmPrincipalModelos = class(TForm)
    Button1: TSpeedButton;
    SqlCon: TSQLConnection;
    qryMain: TSQLQuery;
    Img: TImage;
    SpeedButton1: TSpeedButton;
    pnlHolerith: TPanelModelo;
    EQModeloConteudo: TEmbQuery;
    EQModeloDetalhe: TEmbQuery;
    EQModeloTotais: TEmbQuery;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
    Modelo : TModelo;
    Controle : TModeloControle;

  public
    { Public declarations }
  end;

var
  frmPrincipalModelos: TfrmPrincipalModelos;

implementation

{$R *.dfm}

procedure TfrmPrincipalModelos.Button1Click(Sender: TObject);
var
	iQtdItemDetalhe,
  iInicioDetalhe,
  iTotalColunas : Integer;
  sLinha : String;
  x, i, iCol, iLin : Integer;
  sCampo : String;
  Impressora : TextFile;
  bImpressora : Boolean;

  C: TCampoModelo;
begin
	try
		{
    if bImpressora then
    	AssignPrn(Impressora)
    else
    	AssignFile(Impressora,'c:\impressao.txt');

  	Rewrite(Impressora);
    }

    with qryMain do
    begin
      Close;
      Sql.Clear;
      Sql.Add('SELECT     M.MODELO            ');
      Sql.Add('         , M.ITEMDETALHE       ');
      Sql.Add('         , M.INICIODETALHE     ');
      Sql.Add('         , M.TOTALCOLUNAS      ');
      Sql.Add('FROM       FOL_MODELO M        ');
      Sql.Add('WHERE      M.MODELO = :MODELO  ');
      ParamByName('MODELO').AsInteger := 1;
      Open;
      iQtdItemDetalhe := FieldByName('ITEMDETALHE').AsInteger;
      iInicioDetalhe 	:= FieldByName('INICIODETALHE').AsInteger;
      iTotalColunas 	:= FieldByName('TOTALCOLUNAS').AsInteger;
    end;
    Modelo := TModelo.Create;
    Modelo.QuantidadeDetalhes := iQtdItemDetalhe;
    Modelo.PrimeiraLinhaDetalhe 	:= iInicioDetalhe;
    Modelo.Colunas 		:= iTotalColunas;
    Modelo.Linhas 					:= 33;

    Controle := TModeloControle.Create(Modelo, PnlHolerith);

    with qryMain do
    begin
      Close;
      Sql.Clear;
      // CAMPOS
      Sql.Add('SELECT     C.LINHA ');
      Sql.Add('         , C.COLUNA ');
      Sql.Add('         , C.TAMANHO ');
      Sql.Add('         , CAST(''C'' AS VARCHAR(1)) TIPO ');
      Sql.Add('         , CAST(C.CAMPO AS VARCHAR(200)) CAMPO ');
      Sql.Add('         , C.DESCRICAO ');
      Sql.Add('FROM       FOL_MODELO_CONTEUDO C ');
      Sql.Add('WHERE      C.MODELO = :MODELO ');
      Sql.Add('UNION ALL ');
      Sql.Add('SELECT     M.INICIODETALHE LINHA ');
      Sql.Add('         , D.COLUNA ');
      Sql.Add('         , D.TAMANHO ');
      Sql.Add('         , CAST(''D'' AS VARCHAR(1)) TIPO ');
      Sql.Add('         , CAST(D.CAMPO AS VARCHAR(200)) CAMPO ');
      Sql.Add('         , D.DESCRICAO ');
      Sql.Add('FROM       FOL_MODELO M ');
      Sql.Add('INNER JOIN FOL_MODELO_DETALHE D ');
      Sql.Add('ON         D.MODELO = M.MODELO ');
      Sql.Add('WHERE      M.MODELO = :MODELO ');
      Sql.Add('ORDER BY   1, 2 ');

      ParamByName('MODELO').AsInteger := 1;
      Open;

			//for x := 1 to iNumCopias do
      //begin
      	iLin := 0;
        First;
        sLinha := 'FLAG';
        i := 1;

        for i := 0 to EQModeloConteudo.Fields.Count -1 do
        	Modelo.AdicionaCampoDado(EQModeloConteudo.Fields[i].DisplayText);

        for i := 0 to EQModeloDetalhe.Fields.Count -1 do
        	Modelo.AdicionaCampoDado(EQModeloDetalhe.Fields[i].DisplayText);

        for i := 0 to EQModeloTotais.Fields.Count -1 do
        	Modelo.AdicionaCampoDado(EQModeloTotais.Fields[i].DisplayText);

				Modelo.AdicionaCampoDado('Sistema.Mensagem1');
				Modelo.AdicionaCampoDado('Sistema.Mensagem2');
				Modelo.AdicionaCampoDado('Sistema.Mensagem3');
				Modelo.AdicionaCampoDado('Sistema.Mensagem4');
				Modelo.AdicionaCampoDado('Sistema.Pagina');


        while not EOF do
        begin
            C 					:= TCampoModelo.Create(Modelo);
            //C.Id 				:= i;
            C.CategoriaCampo := ccTexto;
        		if FieldByName('TIPO').AsString = 'C' then
            	C.Banda 		:= bdMestre
            else
            	C.Banda 		:= bdDetalhe;

            C.Linha 		:= FieldByName('LINHA').AsInteger;
            C.Coluna 		:= FieldByName('COLUNA').AsInteger;
            C.Tamanho 	:= FieldByName('TAMANHO').AsInteger;
            C.Nome 			:= FieldByName('DESCRICAO').AsString;
            C.Conteudo 	:= TrimLeft(TrimRight(FieldByName('CAMPO').AsString));


            Modelo.AdicionaCampoModelo(C);
          	inc(i);

        	Next;
        end;


        //Controle := TModeloControle.Create;
        //Controle.setModelo(Modelo);
        //Controle.setVisao(PnlHolerith);
        //Controle.DisplayModelo;


        { // POR ENQUANTO N�O
        try
        	frmDlgModeloMontagemVisual := TfrmDlgModeloMontagemVisual.Cria(Modelo, Self);
          frmDlgModeloMontagemVisual.Show;
        finally
        	//FreeAndNil(frmDlgModeloMontagemVisual);
        end;
        }



        {
      	iLin := 0;
        First;
        sLinha := 'FLAG';
        while not EOF do
        begin
          // alcan�a a linha do registro
					while (iLin >= DmFolha.qryMain.FieldByName('LINHA').AsInteger) do
            WriteLn(Impressora, sLinha);
            sLinha := '';
            iCol := 0;
            inc(iLin);
          end;


          //while (DmFolha.qryMain.FieldByName('LINHA').AsInteger > iLin) do
          //begin
          //  WriteLn(Impressora, sLinha);
          //  sLinha := '';
          //  iCol := 0;
          //  inc(iLin);
          //end;


          while (DmFolha.qryMain.FieldByName('COLUNA').AsInteger > iCol) do
          begin
            sLinha := sLinha + ' ';
            inc(iCol);
          end;

          sCampo := '';

          // Traduzir o campo
          for i := 1 to DmFolha.qryMain.FieldByName('TAMANHO').AsInteger do
          begin
            sCampo := sCampo + 'X';
            inc(iCol);
          end;
          sLinha := sLinha + sCampo;
          Next;
        }
        end;

        //WriteLn(Impressora, ' ');
      //end;

    //end;

    Controle.getModelo.SelecionaCampoPorId(Controle.getModelo.GetCampoModeloIndice(0).Id);
  finally
    ////////System.CloseFile(Impressora);
  end;
end;

procedure TfrmPrincipalModelos.FormCreate(Sender: TObject);
begin
	SqlCon.Connected := True;
end;

procedure TfrmPrincipalModelos.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
	Controle.processaKeyDown(Sender,Key,Shift);
end;

procedure TfrmPrincipalModelos.SpeedButton1Click(Sender: TObject);
begin
	if Controle <> nil then
		Controle.NovoCampoWizard;
end;

end.
