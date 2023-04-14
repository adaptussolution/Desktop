inherited frmRegisterUser: TfrmRegisterUser
  Left = 431
  Top = 202
  Caption = 'Usu'#225'rio'
  ClientHeight = 291
  ClientWidth = 765
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited Panel1: TPanel
    Width = 765
    Height = 291
    inherited Panel2: TPanel
      Top = 237
      Width = 763
      inherited DBNavigator1: TDBNavigator
        Hints.Strings = ()
      end
    end
    inherited PageControl1: TPageControl
      Width = 763
      Height = 236
      inherited TabManutencao: TTabSheet
        object lbl: TLabel
          Left = 16
          Top = 16
          Width = 13
          Height = 13
          Caption = 'ID'
          FocusControl = dbedtID_USUARIO
        end
        object lbl1: TLabel
          Left = 80
          Top = 16
          Width = 30
          Height = 13
          Caption = 'Login'
          FocusControl = dbedtLOGIN
        end
        object lbl2: TLabel
          Left = 336
          Top = 16
          Width = 35
          Height = 13
          Caption = 'Senha'
          FocusControl = dbedtSENHA
        end
        object lbl3: TLabel
          Left = 16
          Top = 64
          Width = 56
          Height = 13
          Caption = 'ID Pessoa'
          FocusControl = dbedtID_PESSOA
        end
        object lbl4: TLabel
          Left = 119
          Top = 64
          Width = 92
          Height = 13
          Caption = 'Nome da Pessoa'
          FocusControl = dbedtNOME_PESSOA
        end
        object btnSeachPerson: TSpeedButton
          Left = 86
          Top = 79
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
        object lbl5: TLabel
          Left = 16
          Top = 112
          Width = 56
          Height = 13
          Caption = 'ID Acesso'
          FocusControl = dbedtID_ACESSO
        end
        object btnbtnSeachAccess: TSpeedButton
          Left = 86
          Top = 126
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
          OnClick = btnbtnSeachAccessClick
        end
        object dbedtID_USUARIO: TDBEdit
          Left = 16
          Top = 30
          Width = 57
          Height = 21
          DataField = 'ID_USUARIO'
          DataSource = dsObjetos
          ReadOnly = True
          TabOrder = 0
        end
        object dbedtLOGIN: TDBEdit
          Left = 80
          Top = 32
          Width = 249
          Height = 21
          DataField = 'LOGIN'
          DataSource = dsObjetos
          ReadOnly = True
          TabOrder = 1
        end
        object dbedtSENHA: TDBEdit
          Left = 336
          Top = 32
          Width = 329
          Height = 21
          DataField = 'SENHA'
          DataSource = dsObjetos
          PasswordChar = '*'
          TabOrder = 2
        end
        object dbedtID_PESSOA: TDBEdit
          Left = 16
          Top = 80
          Width = 65
          Height = 21
          DataField = 'ID_PESSOA'
          DataSource = dsObjetos
          TabOrder = 3
          OnExit = dbedtID_PESSOAExit
        end
        object dbedtNOME_PESSOA: TDBEdit
          Left = 119
          Top = 80
          Width = 546
          Height = 21
          DataField = 'NOME_PESSOA'
          DataSource = dsObjetos
          ReadOnly = True
          TabOrder = 4
        end
        object chkViewPassword: TCheckBox
          Left = 674
          Top = 34
          Width = 65
          Height = 17
          Caption = 'Mostrar'
          TabOrder = 5
          OnClick = chkViewPasswordClick
        end
        object dbedtID_ACESSO: TDBEdit
          Left = 16
          Top = 128
          Width = 65
          Height = 21
          DataField = 'ID_ACESSO'
          DataSource = dsObjetos
          TabOrder = 6
          OnExit = dbedtID_ACESSOExit
        end
        object dbedtACESSO: TDBEdit
          Left = 119
          Top = 128
          Width = 546
          Height = 21
          DataField = 'ACESSO'
          DataSource = dsObjetos
          ReadOnly = True
          TabOrder = 7
        end
      end
      inherited TabConsulta: TTabSheet
        inherited DBGrid1: TDBGrid
          Width = 755
          Height = 140
        end
        inherited PANEL4: TPanel
          Top = 140
          Width = 755
          inherited rdgFiltragem: TGroupBox
            Width = 755
          end
        end
      end
    end
  end
  inherited ActionList1: TActionList
    Left = 613
    Top = 149
  end
  inherited ImageList1: TImageList
    Left = 404
    Top = 155
  end
  inherited QuyObjetos: TIBQuery
    SQL.Strings = (
      
        'select u.*, p.nome as nome_pessoa, a.nome as acesso from tb_usua' +
        'rio u'
      'inner join tb_pessoa p on p.id_pessoa = u.id_pessoa'
      'inner join tb_acesso a on a.id_acesso = u.id_acesso')
    Left = 497
    Top = 159
    object intgrfldQuyObjetosID_USUARIO: TIntegerField
      FieldName = 'ID_USUARIO'
      Origin = 'TB_USUARIO.ID_USUARIO'
      Required = True
    end
    object intgrfldQuyObjetosID_PESSOA: TIntegerField
      FieldName = 'ID_PESSOA'
      Origin = 'TB_USUARIO.ID_PESSOA'
      Required = True
    end
    object QuyObjetosLOGIN: TIBStringField
      FieldName = 'LOGIN'
      Origin = 'TB_USUARIO.LOGIN'
      Required = True
      Size = 150
    end
    object QuyObjetosSENHA: TIBStringField
      FieldName = 'SENHA'
      Origin = 'TB_USUARIO.SENHA'
      Required = True
      Size = 150
    end
    object QuyObjetosNOME_PESSOA: TIBStringField
      FieldName = 'NOME_PESSOA'
      Origin = 'TB_PESSOA.NOME'
      Required = True
      Size = 100
    end
    object QuyObjetosID_ACESSO: TIntegerField
      FieldName = 'ID_ACESSO'
      Origin = 'TB_USUARIO.ID_ACESSO'
      Required = True
    end
    object QuyObjetosACESSO: TIBStringField
      FieldName = 'ACESSO'
      Origin = 'TB_ACESSO.NOME'
      Required = True
      Size = 100
    end
  end
  inherited TBObjetos: TClientDataSet
    Active = True
    BeforePost = TBObjetosBeforePost
    BeforeCancel = TBObjetosBeforeCancel
    Left = 529
    Top = 159
    object TBObjetosID_USUARIO: TIntegerField
      DisplayLabel = 'ID'
      DisplayWidth = 12
      FieldName = 'ID_USUARIO'
      Origin = 'TB_USUARIO.ID_USUARIO'
      Required = True
    end
    object TBObjetosLOGIN: TStringField
      DisplayLabel = 'Login'
      DisplayWidth = 43
      FieldName = 'LOGIN'
      Origin = 'TB_USUARIO.LOGIN'
      Required = True
      Size = 150
    end
    object TBObjetosSENHA: TStringField
      DisplayLabel = 'Senha'
      DisplayWidth = 25
      FieldName = 'SENHA'
      Origin = 'TB_USUARIO.SENHA'
      Required = True
      Visible = False
      Size = 150
    end
    object TBObjetosID_PESSOA: TIntegerField
      DisplayLabel = 'ID Pessoa'
      DisplayWidth = 12
      FieldName = 'ID_PESSOA'
      Origin = 'TB_USUARIO.ID_PESSOA'
      Required = True
    end
    object TBObjetosNOME_PESSOA: TStringField
      DisplayLabel = 'Nome da Pessoa'
      DisplayWidth = 66
      FieldName = 'NOME_PESSOA'
      Origin = 'TB_PESSOA.NOME'
      Required = True
      Size = 100
    end
    object TBObjetosID_ACESSO: TIntegerField
      FieldName = 'ID_ACESSO'
      Origin = 'TB_USUARIO.ID_ACESSO'
      Required = True
      Visible = False
    end
    object TBObjetosACESSO: TStringField
      FieldName = 'ACESSO'
      Origin = 'TB_ACESSO.NOME'
      Required = True
      Visible = False
      Size = 100
    end
  end
  inherited DSPObjetos: TDataSetProvider
    Left = 554
    Top = 159
  end
  inherited dsObjetos: TDataSource
    Left = 582
    Top = 159
  end
  inherited QuyComandos: TIBQuery
    Left = 461
    Top = 161
  end
end
