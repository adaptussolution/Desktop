inherited frmRegisterAccess: TfrmRegisterAccess
  Left = 465
  Top = 184
  Caption = 'Acessos'
  ClientHeight = 406
  ClientWidth = 520
  OldCreateOrder = True
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  inherited Panel1: TPanel
    Width = 520
    Height = 406
    inherited Panel2: TPanel
      Top = 352
      Width = 518
      inherited DBNavigator1: TDBNavigator
        Hints.Strings = ()
      end
    end
    inherited PageControl1: TPageControl
      Width = 518
      Height = 351
      inherited TabManutencao: TTabSheet
        object pnlAcess: TPanel
          Left = 0
          Top = 0
          Width = 510
          Height = 52
          Align = alTop
          BevelOuter = bvNone
          Color = 16771304
          TabOrder = 0
          object lbl: TLabel
            Left = 16
            Top = 5
            Width = 12
            Height = 13
            Caption = 'Id'
            FocusControl = dbedtID_ACESSO
          end
          object lbl1: TLabel
            Left = 80
            Top = 5
            Width = 59
            Height = 13
            Caption = 'Permiss'#227'o'
            FocusControl = dbedtNOME
          end
          object dbedtID_ACESSO: TDBEdit
            Left = 16
            Top = 21
            Width = 57
            Height = 21
            DataField = 'ID_ACESSO'
            DataSource = dsObjetos
            ReadOnly = True
            TabOrder = 0
          end
          object dbedtNOME: TDBEdit
            Left = 80
            Top = 21
            Width = 409
            Height = 21
            DataField = 'NOME'
            DataSource = dsObjetos
            TabOrder = 1
          end
        end
        object pnlAccessItem: TPanel
          Left = 306
          Top = 52
          Width = 204
          Height = 268
          Align = alRight
          BevelOuter = bvNone
          Color = 16771304
          TabOrder = 1
          object Label2: TLabel
            Left = 0
            Top = 0
            Width = 204
            Height = 13
            Align = alTop
            Alignment = taCenter
            Caption = 'Sem Permiss'#227'o'
          end
          object DBGridNOAccessItem: TAdaptusGrid
            Left = 0
            Top = 13
            Width = 204
            Height = 255
            Align = alClient
            DataSource = dsNOAccessItem
            ReadOnly = True
            TabOrder = 0
            TitleFont.Charset = ANSI_CHARSET
            TitleFont.Color = clBlack
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = [fsBold]
          end
        end
        object pnlButtonsAction: TPanel
          Left = 204
          Top = 52
          Width = 102
          Height = 268
          Align = alClient
          BevelOuter = bvNone
          Color = 16771304
          TabOrder = 2
          object sbRigthArrow: TSpeedButton
            Left = 1
            Top = 135
            Width = 23
            Height = 22
            Glyph.Data = {
              26040000424D2604000000000000360000002800000012000000120000000100
              180000000000F0030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
              FFFFFFFFFFFF2D2D2D020202EFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFF0000
              00000000000000E9E9E9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFF9F9F9F00000000000000
              0000E9E9E9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9E9E9E000000000000000000E9E9E9
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF9E9E9E000000000000000000E9E9E9FFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFF9E9E9E000000000000000000E9E9E9FFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFF9E9E9E000000000000000000E9E9E9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9E9E9E
              000000000000000000E9E9E9FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFACACAC0000000000
              00000000FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFACACAC000000000000000000FFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFF9E9E9E000000000000000000E9E9E9FFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9E9E9E000000
              000000000000E9E9E9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9E9E9E000000000000000000E9E9E9FFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFF9E9E9E000000000000000000E9E9E9FFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF9E9E9E00000000
              0000000000E9E9E9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFF9F9F9F000000000000000000E9E9E9FFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFF000000000000000000E9E9E9FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFF2D2D
              2D020202EFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000}
            OnClick = sbRigthArrowClick
          end
          object sbRigthArrowAll: TSpeedButton
            Left = 26
            Top = 135
            Width = 23
            Height = 22
            Glyph.Data = {
              26040000424D2604000000000000360000002800000012000000120000000100
              180000000000F0030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFDEDEDE676767FFFFFFFFFFFFFFFFFF4B4B4BFFFFFFFFFFFFFFFFFF
              616161F1F1F1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFAFAFA
              0000004F4F4FFFFFFFFFFFFF131313090909FFFFFFFFFFFF626262000000FFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFF0000005050
              50FFFFFFFFFFFF1B1B1B080808FFFFFFFFFFFF686868000000FFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFF000000505050FFFFFFFF
              FFFF1B1B1B080808FFFFFFFFFFFF686868000000FFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000505050FFFFFFFFFFFF1B1B1B
              080808FFFFFFFFFFFF686868000000FFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF000000505050FFFFFFFFFFFF313131090909FFFF
              FFFFFFFF797979000000F2F2F2FFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFF000000505050FFFFFFFFFFFF313131090909FFFFFFFFFFFF79797900
              0000F2F2F2FFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000505050FF
              FFFFFFFFFF1B1B1B080808FFFFFFFFFFFF686868000000FFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFF000000505050FFFFFFFFFFFF1B1B1B080808
              FFFFFFFFFFFF686868000000FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFF000000505050FFFFFFFFFFFF1B1B1B080808FFFFFFFFFFFF6868680000
              00FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFAFAFA0000004F4F4FFFFF
              FFFFFFFF131313090909FFFFFFFFFFFF626262000000FFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFDEDEDE676767FFFFFFFFFFFFFFFFFF4A4A4AFF
              FFFFFFFFFFFFFFFF616161F1F1F1FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000}
            OnClick = sbRigthArrowAllClick
          end
          object sbLeftArrowAll: TSpeedButton
            Left = 52
            Top = 135
            Width = 23
            Height = 22
            Glyph.Data = {
              26040000424D2604000000000000360000002800000012000000120000000100
              180000000000F0030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1F1F1616161FFFFFFFFFFFF
              FFFFFF4A4A4AFFFFFFFFFFFFFFFFFF676767DEDEDEFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFF000000626262FFFFFFFFFFFF090909131313FFFF
              FFFFFFFF4F4F4F000000FAFAFAFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FF000000686868FFFFFFFFFFFF0808081B1B1BFFFFFFFFFFFF505050000000FF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFF000000686868FFFFFFFF
              FFFF0808081B1B1BFFFFFFFFFFFF505050000000FFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFF000000686868FFFFFFFFFFFF0808081B1B1BFFFFFF
              FFFFFF505050000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFF2F2F2
              000000797979FFFFFFFFFFFF090909313131FFFFFFFFFFFF505050000000FFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFF2F2F2000000797979FFFF
              FFFFFFFF090909313131FFFFFFFFFFFF505050000000FFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFF000000686868FFFFFFFFFFFF08
              08081B1B1BFFFFFFFFFFFF505050000000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFF000000686868FFFFFFFFFFFF0808081B1B1B
              FFFFFFFFFFFF505050000000FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFF000000686868FFFFFFFFFFFF0808081B1B1BFFFFFFFFFF
              FF505050000000FFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFF000000626262FFFFFFFFFFFF090909131313FFFFFFFFFFFF4F4F4F00
              0000FAFAFAFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF1F1F161
              6161FFFFFFFFFFFFFFFFFF4B4B4BFFFFFFFFFFFFFFFFFF676767DEDEDEFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000}
            OnClick = sbLeftArrowAllClick
          end
          object sbLeftArrow: TSpeedButton
            Left = 78
            Top = 135
            Width = 23
            Height = 22
            Glyph.Data = {
              26040000424D2604000000000000360000002800000012000000120000000100
              180000000000F0030000C40E0000C40E00000000000000000000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEF0202
              022D2D2DFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E9E9000000000000000000FFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFE9E9E90000000000000000009F9F9FFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E9E9000000
              0000000000009E9E9EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E9E90000000000000000009E9E9EFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFE9E9E90000000000000000009E9E9EFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E9E900000000
              00000000009E9E9EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFE9E9E90000000000000000009E9E9EFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFF000000000000000000ACACACFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFF0000
              00000000000000ACACACFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFE9E9E900000000000000
              00009E9E9EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E9E90000000000000000009E9E9E
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFE9E9E90000000000000000009E9E9EFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFE9E9E90000000000000000009E9E9EFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFE9E9E90000000000000000009E9E9EFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
              0000FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E9E9
              0000000000000000009F9F9FFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE9E9E90000000000
              00000000FFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
              FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEFEFEF0202022D2D2DFFFFFFFF
              FFFFFFFFFFFFFFFF0000}
            OnClick = sbLeftArrowClick
          end
        end
        object Panel3: TPanel
          Left = 0
          Top = 52
          Width = 204
          Height = 268
          Align = alLeft
          BevelOuter = bvNone
          Color = 16771304
          TabOrder = 3
          object Label3: TLabel
            Left = 0
            Top = 0
            Width = 204
            Height = 13
            Align = alTop
            Alignment = taCenter
            Caption = 'Permiss'#227'o'
          end
          object DBGridAccessItem: TAdaptusGrid
            Left = 0
            Top = 13
            Width = 204
            Height = 255
            Align = alClient
            DataSource = dsAccessItem
            ReadOnly = True
            TabOrder = 0
            TitleFont.Charset = ANSI_CHARSET
            TitleFont.Color = clBlack
            TitleFont.Height = -11
            TitleFont.Name = 'Tahoma'
            TitleFont.Style = [fsBold]
          end
        end
      end
      inherited TabConsulta: TTabSheet
        inherited DBGrid1: TDBGrid
          Width = 510
          Height = 255
        end
        inherited PANEL4: TPanel
          Top = 255
          Width = 510
          inherited rdgFiltragem: TGroupBox
            Width = 510
          end
        end
      end
    end
  end
  inherited ActionList1: TActionList
    Left = 517
    Top = 365
  end
  inherited ImageList1: TImageList
    Left = 476
    Top = 355
  end
  inherited QuyObjetos: TIBQuery
    SQL.Strings = (
      'select id_acesso, nome from tb_acesso')
    Left = 337
    Top = 367
    object intgrfldQuyObjetosID_ACESSO: TIntegerField
      DisplayLabel = 'Id'
      FieldName = 'ID_ACESSO'
      Origin = 'TB_ACESSO.ID_ACESSO'
      Required = True
    end
    object QuyObjetosNOME: TIBStringField
      DisplayLabel = 'Acesso'
      FieldName = 'NOME'
      Origin = 'TB_ACESSO.NOME'
      Required = True
      Size = 100
    end
  end
  inherited TBObjetos: TClientDataSet
    Active = True
    Left = 393
    Top = 367
    object TBObjetosID_ACESSO: TIntegerField
      DisplayLabel = 'Id'
      DisplayWidth = 12
      FieldName = 'ID_ACESSO'
      Origin = 'TB_ACESSO.ID_ACESSO'
      Required = True
    end
    object TBObjetosNOME: TStringField
      DisplayLabel = 'Permiss'#227'o'
      DisplayWidth = 60
      FieldName = 'NOME'
      Origin = 'TB_ACESSO.NOME'
      Required = True
      Size = 100
    end
  end
  inherited DSPObjetos: TDataSetProvider
    Left = 370
    Top = 367
  end
  inherited dsObjetos: TDataSource
    Left = 422
    Top = 367
  end
  inherited QuyComandos: TIBQuery
    Left = 301
    Top = 369
  end
  object tbAccessItem: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    ProviderName = 'dspAccessItem'
    Left = 383
    Top = 296
    object tbAccessItemID_ACESSO_ITEM: TIntegerField
      FieldName = 'ID_ACESSO_ITEM'
      Required = True
      Visible = False
    end
    object tbAccessItemID_ACESSO: TIntegerField
      FieldName = 'ID_ACESSO'
      Required = True
      Visible = False
    end
    object tbAccessItemNOME: TStringField
      FieldName = 'NOME'
      Required = True
      Size = 100
    end
    object tbAccessItemFLAG: TStringField
      FieldName = 'FLAG'
      Required = True
      Visible = False
      FixedChar = True
      Size = 1
    end
  end
  object dspAccessItem: TDataSetProvider
    DataSet = quyAccessItem
    Left = 351
    Top = 296
  end
  object dsAccessItem: TDataSource
    DataSet = tbAccessItem
    Left = 415
    Top = 296
  end
  object quyAccessItem: TIBQuery
    Database = DM_PRINCIPAL.IBDatabase1
    Transaction = DM_PRINCIPAL.IBTransaction1
    BufferChunks = 1000
    CachedUpdates = False
    SQL.Strings = (
      'select * from TB_ACESSO_ITEM')
    Left = 319
    Top = 296
    object quyAccessItemID_ACESSO_ITEM: TIntegerField
      FieldName = 'ID_ACESSO_ITEM'
      Origin = 'TB_ACESSO_ITEM.ID_ACESSO_ITEM'
      Required = True
    end
    object quyAccessItemID_ACESSO: TIntegerField
      FieldName = 'ID_ACESSO'
      Origin = 'TB_ACESSO_ITEM.ID_ACESSO'
      Required = True
    end
    object quyAccessItemNOME: TIBStringField
      FieldName = 'NOME'
      Origin = 'TB_ACESSO_ITEM.NOME'
      Required = True
      Size = 100
    end
    object quyAccessItemFLAG: TIBStringField
      FieldName = 'FLAG'
      Origin = 'TB_ACESSO_ITEM.FLAG'
      Required = True
      FixedChar = True
      Size = 1
    end
  end
  object quyNOAccessItem: TIBQuery
    Database = DM_PRINCIPAL.IBDatabase1
    Transaction = DM_PRINCIPAL.IBTransaction1
    BufferChunks = 1000
    CachedUpdates = False
    SQL.Strings = (
      'select * from TB_ACESSO_ITEM')
    Left = 319
    Top = 336
    object quyNOAccessItemID_ACESSO_ITEM: TIntegerField
      FieldName = 'ID_ACESSO_ITEM'
      Origin = 'TB_ACESSO_ITEM.ID_ACESSO_ITEM'
      Required = True
    end
    object quyNOAccessItemID_ACESSO: TIntegerField
      FieldName = 'ID_ACESSO'
      Origin = 'TB_ACESSO_ITEM.ID_ACESSO'
      Required = True
    end
    object quyNOAccessItemNOME: TIBStringField
      FieldName = 'NOME'
      Origin = 'TB_ACESSO_ITEM.NOME'
      Required = True
      Size = 100
    end
    object quyNOAccessItemFLAG: TIBStringField
      FieldName = 'FLAG'
      Origin = 'TB_ACESSO_ITEM.FLAG'
      Required = True
      FixedChar = True
      Size = 1
    end
  end
  object dspNOAccessItem: TDataSetProvider
    DataSet = quyNOAccessItem
    Left = 343
    Top = 336
  end
  object tbNOAccessItem: TClientDataSet
    Active = True
    Aggregates = <>
    Params = <>
    ProviderName = 'dspNOAccessItem'
    Left = 375
    Top = 336
    object tbNOAccessItemID_ACESSO_ITEM: TIntegerField
      FieldName = 'ID_ACESSO_ITEM'
      Required = True
      Visible = False
    end
    object tbNOAccessItemID_ACESSO: TIntegerField
      FieldName = 'ID_ACESSO'
      Required = True
      Visible = False
    end
    object tbNOAccessItemNOME: TStringField
      FieldName = 'NOME'
      Required = True
      Size = 100
    end
    object tbNOAccessItemFLAG: TStringField
      FieldName = 'FLAG'
      Required = True
      Visible = False
      FixedChar = True
      Size = 1
    end
  end
  object dsNOAccessItem: TDataSource
    DataSet = tbNOAccessItem
    Left = 423
    Top = 336
  end
end
