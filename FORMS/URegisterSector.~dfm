inherited frmRegisterSector: TfrmRegisterSector
  Left = 525
  Top = 228
  Caption = 'Setor'
  ClientHeight = 390
  ClientWidth = 520
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  inherited Panel1: TPanel
    Width = 520
    Height = 390
    inherited Panel2: TPanel
      Top = 336
      Width = 518
      inherited DBNavigator1: TDBNavigator
        Hints.Strings = ()
      end
    end
    inherited PageControl1: TPageControl
      Width = 518
      Height = 335
      ActivePage = TabManutencao
      inherited TabManutencao: TTabSheet
        object lbl1: TLabel
          Left = 25
          Top = 48
          Width = 49
          Height = 13
          Caption = 'Empresa'
          FocusControl = dbedtID_EMPRESA
        end
        object lbl2: TLabel
          Left = 25
          Top = 0
          Width = 31
          Height = 13
          Caption = 'Setor'
          FocusControl = dbedtNOME
        end
        object btnSeachCompany: TSpeedButton
          Left = 122
          Top = 61
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
          OnClick = btnSeachCompanyClick
        end
        object dbedtID_SETOR: TDBEdit
          Left = 24
          Top = 16
          Width = 95
          Height = 21
          DataField = 'ID_SETOR'
          DataSource = dsObjetos
          ReadOnly = True
          TabOrder = 0
        end
        object dbedtID_EMPRESA: TDBEdit
          Left = 24
          Top = 64
          Width = 95
          Height = 21
          DataField = 'ID_EMPRESA'
          DataSource = dsObjetos
          TabOrder = 2
          OnExit = dbedtID_EMPRESAExit
        end
        object dbedtNOME: TDBEdit
          Left = 149
          Top = 16
          Width = 337
          Height = 21
          DataField = 'NOME'
          DataSource = dsObjetos
          TabOrder = 1
        end
        object dbedtEMPRESA: TDBEdit
          Left = 149
          Top = 64
          Width = 337
          Height = 21
          DataField = 'EMPRESA'
          DataSource = dsObjetos
          ReadOnly = True
          TabOrder = 3
        end
      end
      inherited TabConsulta: TTabSheet
        inherited DBGrid1: TDBGrid
          Width = 510
          Height = 239
        end
        inherited PANEL4: TPanel
          Top = 239
          Width = 510
          inherited rdgFiltragem: TGroupBox
            Width = 510
          end
        end
      end
    end
  end
  inherited ActionList1: TActionList
    Left = 205
    Top = 261
  end
  inherited ImageList1: TImageList
    Left = 284
    Top = 235
  end
  inherited QuyObjetos: TIBQuery
    SQL.Strings = (
      'select s.*, e.razao_social as empresa  from TB_SETOR s'
      'inner join tb_empresa e on e.id_empresa = s.id_empresa')
    object QuyObjetosID_SETOR: TIntegerField
      DisplayLabel = 'ID'
      FieldName = 'ID_SETOR'
      Origin = 'TB_SETOR.ID_SETOR'
      Required = True
    end
    object QuyObjetosID_EMPRESA: TIntegerField
      FieldName = 'ID_EMPRESA'
      Origin = 'TB_SETOR.ID_EMPRESA'
      Required = True
    end
    object QuyObjetosNOME: TIBStringField
      FieldName = 'NOME'
      Origin = 'TB_SETOR.NOME'
      Required = True
      Size = 100
    end
    object QuyObjetosEMPRESA: TIBStringField
      FieldName = 'EMPRESA'
      Origin = 'TB_EMPRESA.RAZAO_SOCIAL'
      Required = True
      Size = 100
    end
  end
  inherited TBObjetos: TClientDataSet
    Active = True
    object TBObjetosID_SETOR: TIntegerField
      DisplayLabel = 'ID'
      DisplayWidth = 7
      FieldName = 'ID_SETOR'
      Required = True
    end
    object TBObjetosID_EMPRESA: TIntegerField
      FieldName = 'ID_EMPRESA'
      Required = True
      Visible = False
    end
    object TBObjetosNOME: TStringField
      DisplayLabel = 'Setor'
      DisplayWidth = 67
      FieldName = 'NOME'
      Required = True
      Size = 100
    end
    object TBObjetosEMPRESA: TStringField
      FieldName = 'EMPRESA'
      Required = True
      Visible = False
      Size = 100
    end
  end
end
