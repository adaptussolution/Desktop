inherited frmRegisterPerson: TfrmRegisterPerson
  Left = 253
  Top = 195
  Caption = 'Pessoa'
  ClientHeight = 357
  ClientWidth = 1086
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited Panel1: TPanel
    Width = 1086
    Height = 357
    inherited Panel2: TPanel
      Top = 303
      Width = 1084
      inherited DBNavigator1: TDBNavigator
        Hints.Strings = ()
      end
    end
    inherited PageControl1: TPageControl
      Width = 1084
      Height = 302
      inherited TabManutencao: TTabSheet
        object Label2: TLabel
          Left = 16
          Top = 8
          Width = 13
          Height = 13
          Caption = 'ID'
          FocusControl = DBEdit1
        end
        object Label3: TLabel
          Left = 72
          Top = 8
          Width = 32
          Height = 13
          Caption = 'Nome'
          FocusControl = DBEdit2
        end
        object Label4: TLabel
          Left = 273
          Top = 46
          Width = 53
          Height = 13
          Caption = 'CPF/CNPJ'
          FocusControl = DBEdit3
        end
        object lbl: TLabel
          Left = 16
          Top = 88
          Width = 35
          Height = 13
          Caption = 'E-mail'
          FocusControl = dbedtEMAIL
        end
        object lbl1: TLabel
          Left = 480
          Top = 88
          Width = 49
          Height = 13
          Caption = 'Telefone'
          FocusControl = dbedtTELEFONE
        end
        object lbl2: TLabel
          Left = 16
          Top = 131
          Width = 20
          Height = 13
          Caption = 'CEP'
          FocusControl = dbedtCEP
        end
        object lbl3: TLabel
          Left = 16
          Top = 213
          Width = 34
          Height = 13
          Caption = 'Bairro'
          FocusControl = dbedtBAIRRO
        end
        object lbl4: TLabel
          Left = 833
          Top = 131
          Width = 44
          Height = 13
          Caption = 'N'#250'mero'
          FocusControl = dbedtNUMERO
        end
        object lbl5: TLabel
          Left = 184
          Top = 132
          Width = 65
          Height = 13
          Caption = 'Logradouro'
          FocusControl = dbedtLOGRADOURO
        end
        object lbl6: TLabel
          Left = 16
          Top = 171
          Width = 79
          Height = 13
          Caption = 'Complemento'
          FocusControl = dbedtCOMPLEMENTO
        end
        object lbl7: TLabel
          Left = 256
          Top = 213
          Width = 38
          Height = 13
          Caption = 'Cidade'
          FocusControl = dbedtCIDADE
        end
        object lbl8: TLabel
          Left = 512
          Top = 213
          Width = 38
          Height = 13
          Caption = 'Estado'
        end
        object Label5: TLabel
          Left = 688
          Top = 48
          Width = 113
          Height = 13
          Caption = 'Data de Nascimento'
        end
        object lbl10: TLabel
          Left = 817
          Top = 48
          Width = 64
          Height = 13
          Caption = 'Estado Civil'
        end
        object DBEdit1: TDBEdit
          Left = 16
          Top = 24
          Width = 49
          Height = 21
          DataField = 'ID_PESSOA'
          DataSource = dsObjetos
          ReadOnly = True
          TabOrder = 0
        end
        object DBEdit2: TDBEdit
          Left = 72
          Top = 24
          Width = 889
          Height = 21
          DataField = 'NOME'
          DataSource = dsObjetos
          TabOrder = 1
        end
        object DBEdit3: TDBEdit
          Left = 273
          Top = 62
          Width = 223
          Height = 21
          DataField = 'CPF_CNPJ'
          DataSource = dsObjetos
          MaxLength = 14
          TabOrder = 3
        end
        object dbedtEMAIL: TDBEdit
          Left = 16
          Top = 104
          Width = 459
          Height = 21
          DataField = 'EMAIL'
          DataSource = dsObjetos
          TabOrder = 7
        end
        object dbedtTELEFONE: TDBEdit
          Left = 480
          Top = 104
          Width = 195
          Height = 21
          DataField = 'TELEFONE'
          DataSource = dsObjetos
          MaxLength = 14
          TabOrder = 8
        end
        object dbedtCEP: TDBEdit
          Left = 16
          Top = 147
          Width = 158
          Height = 21
          DataField = 'CEP'
          DataSource = dsObjetos
          MaxLength = 9
          TabOrder = 9
          OnExit = dbedtCEPExit
        end
        object dbedtBAIRRO: TDBEdit
          Left = 16
          Top = 229
          Width = 233
          Height = 21
          DataField = 'BAIRRO'
          DataSource = dsObjetos
          ReadOnly = True
          TabOrder = 13
        end
        object dbedtNUMERO: TDBEdit
          Left = 833
          Top = 147
          Width = 132
          Height = 21
          DataField = 'NUMERO'
          DataSource = dsObjetos
          TabOrder = 11
        end
        object dbedtLOGRADOURO: TDBEdit
          Left = 184
          Top = 148
          Width = 636
          Height = 21
          DataField = 'LOGRADOURO'
          DataSource = dsObjetos
          ReadOnly = True
          TabOrder = 10
        end
        object dbedtCOMPLEMENTO: TDBEdit
          Left = 16
          Top = 187
          Width = 949
          Height = 21
          DataField = 'COMPLEMENTO'
          DataSource = dsObjetos
          TabOrder = 12
        end
        object dbedtCIDADE: TDBEdit
          Left = 256
          Top = 229
          Width = 249
          Height = 21
          DataField = 'CIDADE'
          DataSource = dsObjetos
          ReadOnly = True
          TabOrder = 14
        end
        object dbrgrpSEXO: TDBRadioGroup
          Left = 505
          Top = 52
          Width = 172
          Height = 37
          Caption = 'Sexo'
          Columns = 2
          DataField = 'SEXO'
          DataSource = dsObjetos
          Items.Strings = (
            'Feminino'
            'Masculino')
          TabOrder = 4
          TabStop = True
          Values.Strings = (
            'F'
            'M')
        end
        object dbrgrpTIPO_PESSOA: TDBRadioGroup
          Left = 15
          Top = 51
          Width = 250
          Height = 34
          Caption = 'Tipo'
          Columns = 2
          DataField = 'TIPO_PESSOA'
          DataSource = dsObjetos
          Items.Strings = (
            'Pessoa Jur'#237'dica'
            'Pessoa F'#237'sica')
          TabOrder = 2
          TabStop = True
          Values.Strings = (
            'J'
            'F')
          OnExit = dbrgrpTIPO_PESSOAExit
        end
        object DBDateEdit1: TDBDateEdit
          Left = 688
          Top = 64
          Width = 119
          Height = 21
          DataField = 'DATA_NASCIMENTO'
          DataSource = dsObjetos
          NumGlyphs = 2
          TabOrder = 5
        end
        object RxDBComboBox1: TRxDBComboBox
          Left = 817
          Top = 64
          Width = 145
          Height = 21
          DataField = 'ESTADO_CIVIL'
          DataSource = dsObjetos
          EnableValues = False
          ItemHeight = 13
          Items.Strings = (
            'solteiro'
            'casado'
            'separado'
            'divorciado'
            'vi'#250'vo')
          TabOrder = 6
          Values.Strings = (
            'solteiro'
            'casado'
            'separado'
            'divorciado'
            'vi'#250'vo')
        end
        object RxDBComboBox2: TRxDBComboBox
          Left = 512
          Top = 228
          Width = 222
          Height = 21
          DataField = 'UF'
          DataSource = dsObjetos
          EnableValues = False
          ItemHeight = 13
          Items.Strings = (
            'Acre'#9'            '
            'Alagoas'#9'            '
            'Amap'#225#9'            '
            'Amazonas'#9'        '
            'Bahia'#9'            '
            'Cear'#225#9'            '
            'Distrito Federal'#9
            'Esp'#237'rito Santo'#9'    '
            'Goi'#225's'#9'            '
            'Maranh'#227'o'#9'        '
            'Mato Grosso'#9'        '
            'Mato Grosso do Sul'#9
            'Minas Gerais'#9'    '
            'Par'#225#9'            '
            'Para'#237'ba'#9'            '
            'Paran'#225#9'            '
            'Pernambuco'#9'        '
            'Piau'#237#9'            '
            'Rio de Janeiro'#9'    '
            'Rio Grande do Norte'#9
            'Rio Grande do Sul'#9
            'Rond'#244'nia'#9'        '
            'Roraima'#9'            '
            'Santa Catarina'#9'    '
            'S'#227'o Paulo'#9'        '
            'Sergipe'#9'            '
            'Tocantins'#9'        ')
          TabOrder = 15
          Values.Strings = (
            'AC'
            'AL'
            'AP'
            'AM'
            'BA'
            'CE'
            'DF'
            'ES'
            'GO'
            'MA'
            'MT'
            'MS'
            'MG'
            'PA'
            'PB'
            'PR'
            'PE'
            'PI'
            'RJ'
            'RN'
            'RS'
            'RO'
            'RR'
            'SC'
            'SP'
            'SE'
            'TO')
        end
      end
      inherited TabConsulta: TTabSheet
        inherited DBGrid1: TDBGrid
          Width = 1076
          Height = 206
        end
        inherited PANEL4: TPanel
          Top = 206
          Width = 1076
          inherited rdgFiltragem: TGroupBox
            Width = 1076
          end
        end
      end
    end
  end
  inherited ActionList1: TActionList
    Left = 829
    Top = 197
  end
  inherited ImageList1: TImageList
    Left = 940
    Top = 139
  end
  inherited QuyObjetos: TIBQuery
    SQL.Strings = (
      
        'select id_pessoa, cpf_cnpj, nome, data_nascimento, sexo, email, ' +
        'telefone, cep, bairro, numero, logradouro, complemento, cidade, ' +
        'uf, pais, tipo_pessoa, estado_civil from tb_pessoa')
    Left = 633
    Top = 191
    object QuyObjetosID_PESSOA: TIntegerField
      FieldName = 'ID_PESSOA'
      Origin = 'TB_PESSOA.ID_PESSOA'
      Required = True
    end
    object QuyObjetosCPF_CNPJ: TIBStringField
      FieldName = 'CPF_CNPJ'
      Origin = 'TB_PESSOA.CPF_CNPJ'
      Required = True
      Size = 14
    end
    object QuyObjetosNOME: TIBStringField
      FieldName = 'NOME'
      Origin = 'TB_PESSOA.NOME'
      Required = True
      Size = 100
    end
    object QuyObjetosDATA_NASCIMENTO: TDateField
      FieldName = 'DATA_NASCIMENTO'
      Origin = 'TB_PESSOA.DATA_NASCIMENTO'
      Required = True
    end
    object QuyObjetosSEXO: TIBStringField
      FieldName = 'SEXO'
      Origin = 'TB_PESSOA.SEXO'
      FixedChar = True
      Size = 1
    end
    object QuyObjetosEMAIL: TIBStringField
      FieldName = 'EMAIL'
      Origin = 'TB_PESSOA.EMAIL'
      Required = True
      Size = 100
    end
    object QuyObjetosTELEFONE: TIBStringField
      FieldName = 'TELEFONE'
      Origin = 'TB_PESSOA.TELEFONE'
      Required = True
      Size = 12
    end
    object QuyObjetosCEP: TIBStringField
      FieldName = 'CEP'
      Origin = 'TB_PESSOA.CEP'
      Required = True
      Size = 10
    end
    object QuyObjetosBAIRRO: TIBStringField
      FieldName = 'BAIRRO'
      Origin = 'TB_PESSOA.BAIRRO'
      Required = True
      Size = 100
    end
    object QuyObjetosNUMERO: TIntegerField
      FieldName = 'NUMERO'
      Origin = 'TB_PESSOA.NUMERO'
      Required = True
    end
    object QuyObjetosLOGRADOURO: TIBStringField
      FieldName = 'LOGRADOURO'
      Origin = 'TB_PESSOA.LOGRADOURO'
      Required = True
      Size = 200
    end
    object QuyObjetosCOMPLEMENTO: TIBStringField
      FieldName = 'COMPLEMENTO'
      Origin = 'TB_PESSOA.COMPLEMENTO'
      Size = 200
    end
    object QuyObjetosCIDADE: TIBStringField
      FieldName = 'CIDADE'
      Origin = 'TB_PESSOA.CIDADE'
      Required = True
      Size = 150
    end
    object QuyObjetosUF: TIBStringField
      FieldName = 'UF'
      Origin = 'TB_PESSOA.UF'
      Required = True
      FixedChar = True
      Size = 2
    end
    object QuyObjetosPAIS: TIBStringField
      FieldName = 'PAIS'
      Origin = 'TB_PESSOA.PAIS'
      Required = True
      Size = 150
    end
    object QuyObjetosTIPO_PESSOA: TIBStringField
      FieldName = 'TIPO_PESSOA'
      Origin = 'TB_PESSOA.TIPO_PESSOA'
      Required = True
      FixedChar = True
      Size = 1
    end
    object QuyObjetosESTADO_CIVIL: TIBStringField
      FieldName = 'ESTADO_CIVIL'
      Origin = 'TB_PESSOA.ESTADO_CIVIL'
      Required = True
      Size = 50
    end
  end
  inherited TBObjetos: TClientDataSet
    Active = True
    Left = 721
    Top = 207
    object TBObjetosID_PESSOA: TIntegerField
      DisplayLabel = 'ID'
      DisplayWidth = 10
      FieldName = 'ID_PESSOA'
      Origin = 'TB_PESSOA.ID_PESSOA'
      Required = True
    end
    object TBObjetosNOME: TStringField
      DisplayLabel = 'Nome'
      DisplayWidth = 55
      FieldName = 'NOME'
      Origin = 'TB_PESSOA.NOME'
      Required = True
      Size = 100
    end
    object TBObjetosCPF_CNPJ: TStringField
      DisplayLabel = 'CPF/CNPJ'
      DisplayWidth = 21
      FieldName = 'CPF_CNPJ'
      Origin = 'TB_PESSOA.CPF_CNPJ'
      Required = True
      EditMask = '999.999.999-99;0;_'
      Size = 14
    end
    object TBObjetosDATA_NASCIMENTO: TDateField
      DisplayLabel = 'Data de Nascimento'
      DisplayWidth = 28
      FieldName = 'DATA_NASCIMENTO'
      Origin = 'TB_PESSOA.DATA_NASCIMENTO'
      Required = True
      DisplayFormat = 'dd/mm/yyyy'
    end
    object TBObjetosSEXO: TStringField
      DisplayLabel = 'Sexo'
      DisplayWidth = 6
      FieldName = 'SEXO'
      Origin = 'TB_PESSOA.SEXO'
      Visible = False
      FixedChar = True
      Size = 1
    end
    object TBObjetosEMAIL: TStringField
      DisplayLabel = 'E-mail'
      DisplayWidth = 35
      FieldName = 'EMAIL'
      Origin = 'TB_PESSOA.EMAIL'
      Required = True
      Size = 100
    end
    object TBObjetosTELEFONE: TStringField
      DisplayLabel = 'Telefone'
      DisplayWidth = 18
      FieldName = 'TELEFONE'
      Origin = 'TB_PESSOA.TELEFONE'
      Required = True
      EditMask = '(99)99999-9999;0;_'
      Size = 12
    end
    object TBObjetosCEP: TStringField
      DisplayWidth = 12
      FieldName = 'CEP'
      Origin = 'TB_PESSOA.CEP'
      Required = True
      Visible = False
      EditMask = '99999-999;0;_'
      Size = 10
    end
    object TBObjetosBAIRRO: TStringField
      DisplayLabel = 'Bairro'
      DisplayWidth = 120
      FieldName = 'BAIRRO'
      Origin = 'TB_PESSOA.BAIRRO'
      Required = True
      Visible = False
      Size = 100
    end
    object TBObjetosNUMERO: TIntegerField
      DisplayLabel = 'N'#250'mero'
      DisplayWidth = 12
      FieldName = 'NUMERO'
      Origin = 'TB_PESSOA.NUMERO'
      Required = True
      Visible = False
    end
    object TBObjetosLOGRADOURO: TStringField
      DisplayLabel = 'Logradouro'
      DisplayWidth = 240
      FieldName = 'LOGRADOURO'
      Origin = 'TB_PESSOA.LOGRADOURO'
      Required = True
      Visible = False
      Size = 200
    end
    object TBObjetosCOMPLEMENTO: TStringField
      DisplayLabel = 'Complemento'
      DisplayWidth = 240
      FieldName = 'COMPLEMENTO'
      Origin = 'TB_PESSOA.COMPLEMENTO'
      Visible = False
      Size = 200
    end
    object TBObjetosCIDADE: TStringField
      DisplayLabel = 'Cidade'
      DisplayWidth = 180
      FieldName = 'CIDADE'
      Origin = 'TB_PESSOA.CIDADE'
      Required = True
      Visible = False
      Size = 150
    end
    object TBObjetosUF: TStringField
      DisplayLabel = 'Estado'
      DisplayWidth = 8
      FieldName = 'UF'
      Origin = 'TB_PESSOA.UF'
      Required = True
      Visible = False
      FixedChar = True
      Size = 2
    end
    object TBObjetosPAIS: TStringField
      DisplayLabel = 'Pa'#237's'
      DisplayWidth = 180
      FieldName = 'PAIS'
      Origin = 'TB_PESSOA.PAIS'
      Required = True
      Visible = False
      Size = 150
    end
    object TBObjetosTIPO_PESSOA: TStringField
      FieldName = 'TIPO_PESSOA'
      Origin = 'TB_PESSOA.TIPO_PESSOA'
      Required = True
      Visible = False
      FixedChar = True
      Size = 1
    end
    object TBObjetosESTADO_CIVIL: TStringField
      DisplayLabel = 'Estado Civil'
      DisplayWidth = 24
      FieldName = 'ESTADO_CIVIL'
      Origin = 'TB_PESSOA.ESTADO_CIVIL'
      Required = True
      Visible = False
      Size = 50
    end
  end
  inherited DSPObjetos: TDataSetProvider
    Left = 698
    Top = 183
  end
  inherited dsObjetos: TDataSource
    Left = 758
  end
  inherited QuyComandos: TIBQuery
    Left = 541
    Top = 185
  end
  object XMLDocument1: TXMLDocument
    Left = 901
    Top = 196
    DOMVendorDesc = 'MSXML'
  end
end
