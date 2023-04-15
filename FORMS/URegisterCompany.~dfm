inherited frmRegisterCompany: TfrmRegisterCompany
  Left = 315
  Top = 245
  Caption = 'Empresa'
  ClientHeight = 264
  ClientWidth = 988
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited Panel1: TPanel
    Width = 988
    Height = 264
    inherited Panel2: TPanel
      Top = 210
      Width = 986
      inherited DBNavigator1: TDBNavigator
        Hints.Strings = ()
      end
    end
    inherited PageControl1: TPageControl
      Width = 986
      Height = 209
      inherited TabManutencao: TTabSheet
        object lbl: TLabel
          Left = 24
          Top = 5
          Width = 13
          Height = 13
          Caption = 'ID'
          FocusControl = dbedtID_EMPRESA
        end
        object lbl1: TLabel
          Left = 24
          Top = 48
          Width = 87
          Height = 13
          Caption = 'Pessoa Jur'#237'dica'
          FocusControl = dbedtID_PESSOA
        end
        object lbl2: TLabel
          Left = 112
          Top = 5
          Width = 83
          Height = 13
          Caption = 'Nome Fantasia'
          FocusControl = dbedtNOME_FANTASIA
        end
        object lbl3: TLabel
          Left = 512
          Top = 5
          Width = 71
          Height = 13
          Caption = 'Raz'#227'o Social'
          FocusControl = dbedtRAZAO_SOCIAL
        end
        object btnSeachPerson: TSpeedButton
          Left = 485
          Top = 63
          Width = 23
          Height = 22
          Glyph.Data = {
            26040000424D2604000000000000360000002800000012000000120000000100
            180000000000F0030000D80E0000D80E00000000000000000000FFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFAFAFAF0000007878780000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA0A0A000
            00000000000000000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFA0A0A0000000000000000000B0B0B0
            0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD1D1D1CACACAF4F4F4FFFFFF
            FFFFFFFFFFFFA0A0A0000000000000000000A1A1A1FFFFFF0000FFFFFFFFFFFF
            FFFFFFD1D1D1252525000000000000000000000000040404A8A8A8A9A9A90000
            00000000000000A1A1A1FFFFFFFFFFFF0000FFFFFFFFFFFF7070700000000000
            00000000000000000000000000000000000000000000000000000000A1A1A1FF
            FFFFFFFFFFFFFFFF0000FFFFFF8080800000000000000000008A8A8ADBDBDBE4
            E4E4A9A9A9131313000000000000000000ABABABFFFFFFFFFFFFFFFFFFFFFFFF
            0000F8F8F80000000000000F0F0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
            545454000000000000A9A9A9FFFFFFFFFFFFFFFFFFFFFFFF0000727272000000
            000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1313130000
            00040404FFFFFFFFFFFFFFFFFFFFFFFF00002424240000004F4F4FFFFFFFFFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFAAAAAA000000000000F5F5F5FF
            FFFFFFFFFFFFFFFF00000101010000008F8F8FFFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFF8F8F8F000000FFFFFF000000000000CACACAFFFFFFFFFFFFFFFFFF
            0000070707000000858585FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF4C4C4C
            000000F7F7F7000000000000D1D1D1FFFFFFFFFFFFFFFFFF0000343434000000
            2E2E2EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD1D1D10000008383839A9A9A0000
            00000000FFFFFFFFFFFFFFFFFFFFFFFF0000919191000000000000D2D2D2FFFF
            FFFFFFFF4D4D4D141414AFAFAF4B4B4BFFFFFF000000000000262626FFFFFFFF
            FFFFFFFFFFFFFFFF0000FFFFFF000000000000000000D3D3D3FFFFFF2C2C2C2E
            2E2EFFFFFFFFFFFF101010000000000000D2D2D2FFFFFFFFFFFFFFFFFFFFFFFF
            0000FFFFFFCBCBCB0000000000000000002F2F2F9C9C9CA6A6A6535353000000
            000000000000717171FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
            CDCDCD000000000000000000000000000000000000000000000000828282FFFF
            FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFF9090
            90343434070707010101242424717171F8F8F8FFFFFFFFFFFFFFFFFFFFFFFFFF
            FFFFFFFFFFFFFFFF0000}
          OnClick = btnSeachPersonClick
        end
        object dbedtID_EMPRESA: TDBEdit
          Left = 24
          Top = 21
          Width = 82
          Height = 21
          DataField = 'ID_EMPRESA'
          DataSource = dsObjetos
          ReadOnly = True
          TabOrder = 0
        end
        object dbedtID_PESSOA: TDBEdit
          Left = 24
          Top = 64
          Width = 82
          Height = 21
          DataField = 'ID_PESSOA'
          DataSource = dsObjetos
          TabOrder = 3
          OnExit = dbedtID_PESSOAExit
        end
        object dbedtNOME_FANTASIA: TDBEdit
          Left = 112
          Top = 21
          Width = 369
          Height = 21
          DataField = 'NOME_FANTASIA'
          DataSource = dsObjetos
          TabOrder = 1
        end
        object dbedtRAZAO_SOCIAL: TDBEdit
          Left = 512
          Top = 21
          Width = 409
          Height = 21
          DataField = 'RAZAO_SOCIAL'
          DataSource = dsObjetos
          TabOrder = 2
        end
        object dbedtCNPJ: TDBEdit
          Left = 112
          Top = 64
          Width = 369
          Height = 21
          DataField = 'CNPJ'
          DataSource = dsObjetos
          MaxLength = 19
          TabOrder = 4
          OnExit = dbedtCNPJExit
        end
        object dbedtNOME_EMPRESA: TDBEdit
          Left = 512
          Top = 64
          Width = 409
          Height = 21
          DataField = 'NOME_EMPRESA'
          DataSource = dsObjetos
          ReadOnly = True
          TabOrder = 5
        end
      end
      inherited TabConsulta: TTabSheet
        inherited DBGrid1: TDBGrid
          Width = 978
          Height = 113
        end
        inherited PANEL4: TPanel
          Top = 113
          Width = 978
          inherited rdgFiltragem: TGroupBox
            Width = 978
          end
        end
      end
    end
  end
  inherited ActionList1: TActionList
    Left = 565
    Top = 109
  end
  inherited ImageList1: TImageList
    Left = 644
    Top = 83
  end
  inherited QuyObjetos: TIBQuery
    SQL.Strings = (
      
        'select e.*, p.cpf_cnpj as cnpj, p.nome as nome_empresa from tb_e' +
        'mpresa e'
      'inner join tb_pessoa p on p.id_pessoa = e.id_pessoa')
    Left = 265
    Top = 63
    object QuyObjetosID_EMPRESA: TIntegerField
      FieldName = 'ID_EMPRESA'
      Origin = 'TB_EMPRESA.ID_EMPRESA'
      Required = True
    end
    object QuyObjetosID_PESSOA: TIntegerField
      FieldName = 'ID_PESSOA'
      Origin = 'TB_EMPRESA.ID_PESSOA'
      Required = True
    end
    object QuyObjetosNOME_FANTASIA: TIBStringField
      FieldName = 'NOME_FANTASIA'
      Origin = 'TB_EMPRESA.NOME_FANTASIA'
      Size = 100
    end
    object QuyObjetosRAZAO_SOCIAL: TIBStringField
      FieldName = 'RAZAO_SOCIAL'
      Origin = 'TB_EMPRESA.RAZAO_SOCIAL'
      Required = True
      Size = 100
    end
    object QuyObjetosCNPJ: TIBStringField
      FieldName = 'CNPJ'
      Origin = 'TB_PESSOA.CPF_CNPJ'
      Required = True
      Size = 14
    end
    object QuyObjetosNOME_EMPRESA: TIBStringField
      FieldName = 'NOME_EMPRESA'
      Origin = 'TB_PESSOA.NOME'
      Required = True
      Size = 100
    end
  end
  inherited TBObjetos: TClientDataSet
    Active = True
    Left = 297
    Top = 63
    object TBObjetosID_EMPRESA: TIntegerField
      DisplayLabel = 'ID'
      DisplayWidth = 6
      FieldName = 'ID_EMPRESA'
      Required = True
    end
    object TBObjetosID_PESSOA: TIntegerField
      FieldName = 'ID_PESSOA'
      Required = True
      Visible = False
    end
    object TBObjetosNOME_FANTASIA: TStringField
      DisplayLabel = 'Nome Fantasia'
      DisplayWidth = 61
      FieldName = 'NOME_FANTASIA'
      Size = 100
    end
    object TBObjetosRAZAO_SOCIAL: TStringField
      DisplayLabel = 'Raz'#227'o Social'
      DisplayWidth = 65
      FieldName = 'RAZAO_SOCIAL'
      Required = True
      Size = 100
    end
    object TBObjetosCNPJ: TStringField
      DisplayWidth = 46
      FieldName = 'CNPJ'
      Required = True
      EditMask = '999.999.999/9999-99;0;_'
      Size = 14
    end
    object TBObjetosNOME_EMPRESA: TStringField
      DisplayLabel = 'Raz'#227'o Social'
      DisplayWidth = 54
      FieldName = 'NOME_EMPRESA'
      Required = True
      Visible = False
      Size = 100
    end
  end
  inherited DSPObjetos: TDataSetProvider
    Left = 322
    Top = 63
  end
  inherited dsObjetos: TDataSource
    Left = 350
    Top = 63
  end
  inherited QuyComandos: TIBQuery
    Left = 229
    Top = 65
  end
end
