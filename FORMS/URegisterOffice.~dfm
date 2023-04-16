inherited frmRegisterOffice: TfrmRegisterOffice
  Left = 487
  Top = 228
  Caption = 'Cargos'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited Panel1: TPanel
    inherited Panel2: TPanel
      inherited DBNavigator1: TDBNavigator
        Hints.Strings = ()
      end
    end
    inherited PageControl1: TPageControl
      ActivePage = TabManutencao
      inherited TabManutencao: TTabSheet
        object lbl2: TLabel
          Left = 16
          Top = 8
          Width = 33
          Height = 13
          Caption = 'Cargo'
          FocusControl = dbedtNOME
        end
        object lbl3: TLabel
          Left = 16
          Top = 96
          Width = 39
          Height = 13
          Caption = 'Sal'#225'rio'
          FocusControl = dbedtSALARIO
        end
        object lbl4: TLabel
          Left = 16
          Top = 48
          Width = 36
          Height = 13
          Caption = 'SETOR'
          FocusControl = dbedtSETOR
        end
        object btnSeachSector: TSpeedButton
          Left = 128
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
          OnClick = btnSeachSectorClick
        end
        object dbedtID_CARGO: TDBEdit
          Left = 16
          Top = 24
          Width = 105
          Height = 21
          DataField = 'ID_CARGO'
          DataSource = dsObjetos
          ReadOnly = True
          TabOrder = 0
        end
        object dbedtID_SETOR: TDBEdit
          Left = 16
          Top = 64
          Width = 105
          Height = 21
          DataField = 'ID_SETOR'
          DataSource = dsObjetos
          TabOrder = 2
          OnExit = dbedtID_SETORExit
        end
        object dbedtNOME: TDBEdit
          Left = 161
          Top = 24
          Width = 550
          Height = 21
          DataField = 'NOME'
          DataSource = dsObjetos
          TabOrder = 1
        end
        object dbedtSALARIO: TDBEdit
          Left = 16
          Top = 112
          Width = 134
          Height = 21
          DataField = 'SALARIO'
          DataSource = dsObjetos
          TabOrder = 4
        end
        object dbedtSETOR: TDBEdit
          Left = 160
          Top = 64
          Width = 550
          Height = 21
          DataField = 'SETOR'
          DataSource = dsObjetos
          ReadOnly = True
          TabOrder = 3
        end
      end
    end
  end
  inherited QuyObjetos: TIBQuery
    SQL.Strings = (
      'SELECT C.*, S.nome AS SETOR FROM TB_CARGO C'
      'INNER join tb_setor S ON S.id_setor = C.id_setor')
    object QuyObjetosID_CARGO: TIntegerField
      FieldName = 'ID_CARGO'
      Origin = 'TB_CARGO.ID_CARGO'
      Required = True
    end
    object QuyObjetosID_SETOR: TIntegerField
      FieldName = 'ID_SETOR'
      Origin = 'TB_CARGO.ID_SETOR'
      Required = True
    end
    object QuyObjetosNOME: TIBStringField
      FieldName = 'NOME'
      Origin = 'TB_CARGO.NOME'
      Required = True
      Size = 100
    end
    object QuyObjetosSALARIO: TFloatField
      FieldName = 'SALARIO'
      Origin = 'TB_CARGO.SALARIO'
      Required = True
    end
    object QuyObjetosSETOR: TIBStringField
      FieldName = 'SETOR'
      Origin = 'TB_SETOR.NOME'
      Required = True
      Size = 100
    end
  end
  inherited TBObjetos: TClientDataSet
    Active = True
    object TBObjetosID_CARGO: TIntegerField
      DisplayLabel = 'ID'
      FieldName = 'ID_CARGO'
      Origin = 'TB_CARGO.ID_CARGO'
      Required = True
    end
    object TBObjetosID_SETOR: TIntegerField
      FieldName = 'ID_SETOR'
      Origin = 'TB_CARGO.ID_SETOR'
      Required = True
      Visible = False
    end
    object TBObjetosNOME: TStringField
      DisplayLabel = 'Cargo'
      FieldName = 'NOME'
      Origin = 'TB_CARGO.NOME'
      Required = True
      Size = 100
    end
    object TBObjetosSALARIO: TFloatField
      FieldName = 'SALARIO'
      Origin = 'TB_CARGO.SALARIO'
      Required = True
      Visible = False
      DisplayFormat = 'R$,0.00;-R$,0.00'
    end
    object TBObjetosSETOR: TStringField
      FieldName = 'SETOR'
      Origin = 'TB_SETOR.NOME'
      Required = True
      Visible = False
      Size = 100
    end
  end
end
