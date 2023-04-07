object frmPrincipalModelos: TfrmPrincipalModelos
  Left = 275
  Top = 119
  Width = 709
  Height = 473
  Caption = 'Gerador de Modelos - HOLLERITH'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Scaled = False
  WindowState = wsMaximized
  OnCreate = FormCreate
  OnKeyDown = FormKeyDown
  PixelsPerInch = 96
  TextHeight = 14
  object Img: TImage
    Left = 464
    Top = 0
    Width = 73
    Height = 57
  end
  object Button1: TSpeedButton
    Left = 8
    Top = 8
    Width = 75
    Height = 25
    Caption = 'Vai'
    OnClick = Button1Click
  end
  object SpeedButton1: TSpeedButton
    Left = 280
    Top = 16
    Width = 113
    Height = 22
    Caption = 'NovoCampoWizard'
    OnClick = SpeedButton1Click
  end
  object pnlHolerith: TPanelModelo
    Left = 8
    Top = 96
    Width = 577
    Height = 493
    Colunas = 80
    Linhas = 33
    PrimeiraLinhaDetalhe = 7
    LinhasDetalhe = 10
  end
  object SqlCon: TSQLConnection
    ConnectionName = 'AGEDOC'
    DriverName = 'Interbase'
    GetDriverFunc = 'getSQLDriverINTERBASE'
    LibraryName = 'dbexpint.dll'
    LoginPrompt = False
    Params.Strings = (
      'DriverName=Interbase'
      'Database=c:\sapiens\clientes\bancos\pmmcarmelo.fdb'
      'RoleName=RoleName'
      'User_Name=SYSDBA'
      'Password=masterkey'
      'ServerCharSet=ISO8859_1'
      'SQLDialect=3'
      'BlobSize=-1'
      'CommitRetain=False'
      'WaitOnLocks=True'
      'ErrorResourceFile='
      'LocaleCode=0000'
      'Interbase TransIsolation=ReadCommited'
      'Trim Char=False')
    VendorLib = 'fbclient.dll'
    Left = 104
    Top = 8
  end
  object qryMain: TSQLQuery
    MaxBlobSize = -1
    Params = <>
    SQLConnection = SqlCon
    Left = 144
    Top = 8
  end
  object EQModeloConteudo: TEmbQuery
    Fields = <
      item
        InternalName = 'MATRICULA'
        Prefix = 'FC'
        DisplayText = 'Servidor.Matricula'
        DisplayWidth = 0
        ContentType = ctInteger
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'ANOMES'
        Prefix = 'FC'
        DisplayText = 'Folha.AnoMes'
        DisplayWidth = 0
        ContentType = ctInteger
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'ADMISSAO'
        Prefix = 'ADM'
        DisplayText = 'Servidor.DataAdmissao'
        DisplayWidth = 0
        ContentType = ctDate
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'NOMESERVIDOR'
        Prefix = 'SER'
        DisplayText = 'Servidor.Nome'
        DisplayWidth = 0
        ContentType = ctString
        Values.Strings = (
          'SER.NOME AS NOMESERVIDOR')
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'PIS'
        Prefix = 'SER'
        DisplayText = 'Servidor.NumeroPis'
        DisplayWidth = 0
        ContentType = ctString
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'NUMEROCPF'
        DisplayText = 'Servidor.NumeroCPF'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'CAST(SER.CPF AS VARCHAR(11)) AS NUMEROCPF')
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'DESCRICAOLOCAL'
        DisplayText = 'Servidor.LocalTrabalho.Descricao'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'LCL.DESCRICAO AS DESCRICAOLOCAL')
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'REFERENCIA'
        DisplayText = 'Folha.Referencia'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'RTRIM(CASE SUBSTRING(FC.ANOMES FROM 5 FOR 2)'
          '                   WHEN 1 THEN '#39'Janeiro'#39
          '                   WHEN 2 THEN '#39'Fevereiro'#39
          '                   WHEN 3 THEN '#39'Mar'#231'o'#39
          '                   WHEN 4 THEN '#39'Abril'#39
          '                   WHEN 5 THEN '#39'Maio'#39
          '                   WHEN 6 THEN '#39'Junho'#39
          '                   WHEN 7 THEN '#39'Julho'#39
          '                   WHEN 8 THEN '#39'Agosto'#39
          '                   WHEN 9 THEN '#39'Setembro'#39
          '                   WHEN 10 THEN '#39'Outubro'#39
          '                   WHEN 11 THEN '#39'Novembro'#39
          '                   WHEN 12 THEN '#39'Dezembro'#39
          
            '           END) || '#39'/'#39' || SUBSTRING(FC.ANOMES FROM 1 FOR 4) AS R' +
            'EFERENCIA')
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'CBO'
        Prefix = 'FC'
        DisplayText = 'Servidor.CBO'
        DisplayWidth = 0
        ContentType = ctString
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'SALARIO'
        Prefix = 'FC'
        DisplayText = 'Servidor.Salario'
        DisplayWidth = 0
        ContentType = ctReal
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'DESCRICAONIVEL'
        DisplayText = 'Servidor.Nivel.Descricao'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'N.DESCRICAO AS DESCRICAONIVEL')
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'DESCRICAOGRAU'
        DisplayText = 'Servidor.Grau.Descricao'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'G.DESCRICAO AS DESCRICAOGRAU')
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'DESCRICAOCARGO'
        DisplayText = 'Servidor.Cargo.Descricao'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'CRG.DESCRICAO AS DESCRICAOCARGO')
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'DOTACAO'
        Prefix = 'FIC'
        DisplayText = 'Ficha.Dotacao'
        DisplayWidth = 0
        ContentType = ctString
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'DESCRICAOUNIDADE'
        DisplayText = 'Ficha.Unidade.Descricao'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'UND.DESCRICAO AS DESCRICAOUNIDADE')
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'DESCRICAOSUBUNIDADE'
        DisplayText = 'Ficha.SubUnidade.Descricao'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'SUBUND.DESCRICAO AS DESCRICAOSUBUNIDADE')
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'DESCRICAOPROJETO'
        DisplayText = 'Ficha.Projeto.Descricao'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'RAW(PRJ.DESCRICAO) AS DESCRICAOPROJETO')
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'DESCRICAOBANCO'
        DisplayText = 'Servidor.Banco.Nome'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'BAN.DESCRICAO AS BANCODESCRICAO')
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'CODIGOBANCO'
        DisplayText = 'Servidor.Banco.Codigo'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'BAN.BANCO AS CODIGOBANCO')
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'AGENCIA'
        Prefix = 'AGE'
        DisplayText = 'Servidor.Agencia.Numero'
        DisplayWidth = 0
        ContentType = ctString
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'CONTACORRENTE'
        DisplayText = 'Servidor.ContaCorrente'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'CAST( LTRIM(RTRIM(COALESCE(FC.CONTA_CORRENTE,'#39#39'))) || '#39'-'#39' ||'
          
            '                 LTRIM(RTRIM(COALESCE(FC.DIGITO_CONTA_CORRENTE,'#39 +
            #39')))'
          '                 AS VARCHAR(20)'
          '           ) AS CONTACORRENTE')
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'BASEPREVIDENCIA'
        Prefix = 'FC'
        DisplayText = 'Folha.ValorBasePrevidencia'
        DisplayWidth = 0
        ContentType = ctReal
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'VALORPREVIDENCIA'
        Prefix = 'FC'
        DisplayText = 'Folha.ValorPrevidencia'
        DisplayWidth = 0
        ContentType = ctReal
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'BASEFGTS'
        Prefix = 'FC'
        DisplayText = 'Folha.ValorBaseFGTS'
        DisplayWidth = 0
        ContentType = ctReal
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'VALORFGTS'
        Prefix = 'FC'
        DisplayText = 'Folha.ValorFGTS'
        DisplayWidth = 0
        ContentType = ctReal
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'VALORIRRF'
        Prefix = 'FC'
        DisplayText = 'Folha.ValorIRRF'
        DisplayWidth = 0
        ContentType = ctReal
        Visible = False
        Indexed = False
      end
      item
        InternalName = 'FAIXAIRRF'
        Prefix = 'FC'
        DisplayText = 'Folha.FaixaIRRF'
        DisplayWidth = 0
        ContentType = ctReal
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'NUMEROCGC'
        DisplayText = 'Empresa.NumeroCGC'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'SUBSTRING(EMP.EMPRESA FROM 1 FOR 2) || '#39'.'#39' ||'
          '           SUBSTRING(EMP.EMPRESA FROM 3 FOR 3) || '#39'.'#39' ||'
          '           SUBSTRING(EMP.EMPRESA FROM 6 FOR 3) || '#39'/'#39' ||'
          '           SUBSTRING(EMP.EMPRESA FROM 9 FOR 4) || '#39'-'#39' ||'
          '           SUBSTRING(EMP.EMPRESA FROM 13 FOR 2) AS NUMEROCGC')
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'NOMEEMPRESA'
        DisplayText = 'Empresa.Nome'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'EMP.NOME AS NOMEEMPRESA')
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'EMPRESAENDERECO'
        DisplayText = 'Empresa.Endereco'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          'CAST( LTRIM(RTRIM(COALESCE(EMP.ENDERECO, '#39#39'))) || '#39', '#39' ||'
          
            '                 LTRIM(RTRIM(COALESCE(EMP.NUMERO, '#39#39'))) || '#39' - '#39 +
            ' ||'
          
            '                 LTRIM(RTRIM(COALESCE(EMP.CIDADE, '#39#39'))) || '#39'-'#39' |' +
            '|'
          '                 COALESCE(EMP.ESTADO,'#39#39')'
          '                 AS VARCHAR(80)'
          '               ) AS EMPRESAENDERECO')
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'EMPRESACIDADEESTADO'
        DisplayText = 'Empresa.CidadeEstado'
        DisplayWidth = 0
        ContentType = ctString
        Expression.Strings = (
          
            'CAST(RTRIM(LTRIM(EMP.CIDADE)) || '#39'-'#39' || RTRIM(LTRIM(EMP.ESTADO))' +
            ' AS VARCHAR(60) ) AS EMPRESACIDADEESTADO')
        Visible = True
        Indexed = False
      end>
    SqlFrom.Strings = (
      'FOL_FICHAFINANCEIRA_CABEC FC'
      'INNER JOIN FOL_FICHAFINANCEIRA FF'
      'ON         FF.ANOMES = FC.ANOMES'
      'AND        FF.MATRICULA = FC.MATRICULA'
      'AND        FF.PAGTO = FC.PAGTO'
      'AND        FF.SEQUENCIA = FC.SEQUENCIA'
      'INNER JOIN FOL_ADMISSAO ADM'
      'ON         ADM.MATRICULA = FC.MATRICULA'
      'INNER JOIN FOL_SERVIDOR SER'
      'ON         SER.SERVIDOR = ADM.SERVIDOR'
      'INNER JOIN FOL_ADMISSAO_CARGO ADMCRG'
      'ON         ADMCRG.MATRICULA = ADM.MATRICULA'
      'AND        ADMCRG.DATA_INICIO = ( SELECT MIN(TB.DATA_INICIO)'
      '                                  FROM   FOL_ADMISSAO_CARGO TB'
      
        '                                  WHERE  TB.MATRICULA = ADM.MATR' +
        'ICULA'
      '                                )'
      'INNER JOIN FOL_GRAU G'
      'ON         G.ID_GRAU = FC.ID_GRAU'
      'INNER JOIN FOL_NIVEL N'
      'ON         N.ID_NIVEL = FC.ID_NIVEL'
      'INNER JOIN FOL_FUNCAO CRG'
      'ON         CRG.FUNCAO = FC.FUNCAO'
      'LEFT JOIN  FOL_AGENCIA AGE'
      'ON         AGE.ID_AGENCIA = FC.ID_AGENCIA'
      'LEFT JOIN  FOL_BANCO BAN'
      'ON         BAN.BANCO = AGE.BANCO'
      'INNER JOIN CTB_FICHA FIC'
      'ON         FIC.ID_FICHA = FC.ID_FICHA'
      'LEFT JOIN  PAD_UNIDADE UND'
      'ON         UND.ID_PAD_UNIDADE       = FIC.ID_PAD_UNIDADE'
      'LEFT JOIN  PAD_ORGAO ORG'
      'ON         ORG.ORGAO                = UND.ORGAO'
      'LEFT JOIN  PAD_ENTIDADE ENT'
      'ON         ENT.ORGAO                = UND.ORGAO'
      'AND        ENT.ENTIDADE             = UND.ENTIDADE'
      'LEFT JOIN  PAD_SUBUNIDADE SUBUND'
      'ON         SUBUND.ID_PAD_SUBUNIDADE = FIC.ID_PAD_SUBUNIDADE'
      'LEFT JOIN  CTB_FUNCAO FUN'
      'ON         FUN.FUNCAO               = FIC.FUNCAO'
      'LEFT JOIN  CTB_SUBFUNCAO SUBFUN'
      'ON         SUBFUN.SUBFUNCAO         = FIC.SUBFUNCAO'
      'LEFT JOIN  CTB_PROGRAMA PRG'
      'ON         PRG.QUADRIENIO           = FIC.QUADRIENIO'
      'AND        PRG.PROGRAMA             = FIC.PROGRAMA'
      'LEFT JOIN  CTB_ACAO ACA'
      'ON         ACA.QUADRIENIO           = FIC.QUADRIENIO'
      'AND        ACA.PROGRAMA             = FIC.PROGRAMA'
      'AND        ACA.ACAO                 = FIC.ACAO'
      'LEFT JOIN  CTB_PROJETO PRJ'
      'ON         PRJ.ANUENIO              = FIC.ANUENIO'
      'AND        PRJ.PROJETO              = FIC.PROJETO'
      'LEFT JOIN  CTB_DESPESA DSP'
      'ON         DSP.CONTA                = FIC.ELEMENTO'
      'LEFT JOIN  FOL_LOCAL_TRABALHO LCL'
      'ON         LCL.LOCALTRABALHO =  ADMCRG.LOCALTRABALHO'
      'LEFT JOIN  SYS_EMPRESA EMP'
      'ON         1 = 1')
    SqlWhere.Strings = (
      'FC.ANOMES = :ANOMES'
      'AND        FC.PAGTO = :PAGTO')
    SqlOrderBy.Strings = (
      'SER.NOME')
    SqlConnection = SqlCon
    MultiRows = True
    SelectDistinct = True
    Left = 176
    Top = 8
  end
  object EQModeloDetalhe: TEmbQuery
    Fields = <
      item
        InternalName = 'ANOMES'
        Prefix = 'FF'
        DisplayText = 'Folha.AnoMes'
        DisplayWidth = 0
        ContentType = ctInteger
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'EVENTO'
        Prefix = 'EVE'
        DisplayText = 'Folha.Evento.Codigo'
        DisplayWidth = 0
        ContentType = ctInteger
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'MATRICULA'
        Prefix = 'FF'
        DisplayText = 'Folha.Matricula'
        DisplayWidth = 0
        ContentType = ctString
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'TIPOEVENTO'
        Prefix = 'EVE'
        DisplayText = 'Folha.Evento.Tipo'
        DisplayWidth = 0
        ContentType = ctString
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'DESCRICAO'
        Prefix = 'EVE'
        DisplayText = 'Folha.Evento.Descricao'
        DisplayWidth = 0
        ContentType = ctString
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'BASE'
        Prefix = 'FF'
        DisplayText = 'Folha.Evento.ValorBase'
        DisplayWidth = 0
        ContentType = ctReal
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'QTDE'
        Prefix = 'FF'
        DisplayText = 'Folha.Evento.Quantidade'
        DisplayWidth = 0
        ContentType = ctReal
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'PROVENTO'
        DisplayText = 'Folha.Evento.ValorProvento'
        DisplayWidth = 0
        ContentType = ctReal
        Expression.Strings = (
          'CASE'
          'WHEN EVE.TIPOEVENTO = '#39'P'#39' THEN FF.VALOREVENTO'
          'END AS PROVENTO')
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'DESCONTO'
        DisplayText = 'Folha.Evento.ValorDesconto'
        DisplayWidth = 0
        ContentType = ctReal
        Expression.Strings = (
          'CASE'
          'WHEN EVE.TIPOEVENTO = '#39'D'#39' THEN FF.VALOREVENTO'
          'END AS DESCONTO')
        Visible = True
        Indexed = False
      end>
    SqlFrom.Strings = (
      'FOL_FICHAFINANCEIRA FF'
      'INNER JOIN FOL_EVENTO EVE'
      'ON EVE.EVENTO = FF.EVENTO')
    SqlWhere.Strings = (
      'FF.ANOMES = :ANOMES'
      'AND FF.PAGTO = :PAGTO'
      'AND FF.MATRICULA = :MATRICULA'
      'AND EVE.TIPOEVENTO IN ('#39'P'#39', '#39'D'#39')')
    SqlOrderBy.Strings = (
      'FF.MATRICULA'
      ', EVE.TIPOEVENTO DESCENDING'
      ', EVE.EVENTO')
    SqlConnection = SqlCon
    MultiRows = True
    Left = 208
    Top = 8
  end
  object EQModeloTotais: TEmbQuery
    Fields = <
      item
        InternalName = 'TOTA_PROVENTOS'
        DisplayText = 'Folha.ValorTotalProventos'
        DisplayWidth = 0
        ContentType = ctReal
        Expression.Strings = (
          'COALESCE(SUM(CASE'
          
            '                        WHEN E.TIPOEVENTO = '#39'P'#39' THEN FF.VALOREVE' +
            'NTO'
          '                        ELSE 0'
          '                   END),0) TOTAL_PROVENTOS')
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'TOTAL_DESCONTOS'
        DisplayText = 'Folha.ValorTotalDescontos'
        DisplayWidth = 0
        ContentType = ctReal
        Expression.Strings = (
          'COALESCE(SUM(CASE'
          
            '                        WHEN E.TIPOEVENTO = '#39'D'#39' THEN FF.VALOREVE' +
            'NTO'
          '                        ELSE 0'
          '                   END),0) TOTAL_DESCONTOS')
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'TOTAL_LIQUIDO'
        DisplayText = 'Folha.ValorTotalLiquido'
        DisplayWidth = 0
        ContentType = ctReal
        Expression.Strings = (
          'COALESCE(SUM(CASE'
          
            '                        WHEN E.TIPOEVENTO = '#39'D'#39' THEN FF.VALOREVE' +
            'NTO * -1'
          
            '                        WHEN E.TIPOEVENTO = '#39'P'#39' THEN FF.VALOREVE' +
            'NTO'
          '                        ELSE 0'
          '                   END),0) TOTAL_LIQUIDO')
        Visible = True
        Indexed = False
      end>
    SqlFrom.Strings = (
      'FOL_FICHAFINANCEIRA FF'
      'INNER JOIN FOL_EVENTO E'
      'ON         E.EVENTO = FF.EVENTO')
    SqlWhere.Strings = (
      'FF.ANOMES = :ANOMES'
      'AND    FF.PAGTO = :PAGTO'
      'AND    FF.MATRICULA = :MATRICULA')
    SqlOrderBy.Strings = (
      'FF.MATRICULA'
      ', EVE.TIPOEVENTO DESCENDING'
      ', EVE.EVENTO')
    SqlConnection = SqlCon
    MultiRows = True
    Left = 240
    Top = 8
  end
end
