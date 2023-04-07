object FrmBusca: TFrmBusca
  Left = 380
  Top = 26
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Pesquisa'
  ClientHeight = 620
  ClientWidth = 552
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object Panel1: TPanel
    Left = 8
    Top = 8
    Width = 537
    Height = 225
    BorderStyle = bsSingle
    TabOrder = 0
    object Grd: TDBGrid
      Left = 1
      Top = 88
      Width = 531
      Height = 132
      Align = alClient
      DataSource = Ds
      Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgCancelOnExit]
      ReadOnly = True
      TabOrder = 1
      TitleFont.Charset = ANSI_CHARSET
      TitleFont.Color = clWindowText
      TitleFont.Height = -11
      TitleFont.Name = 'Arial'
      TitleFont.Style = []
      OnDrawDataCell = GrdDrawDataCell
      OnDblClick = GrdDblClick
      OnKeyDown = GrdKeyDown
      OnTitleClick = GrdTitleClick
    end
    object Panel2: TPanel
      Left = 1
      Top = 1
      Width = 531
      Height = 50
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 92
        Height = 14
        Caption = 'Texto de Pesquisa:'
      end
      object Label2: TLabel
        Left = 312
        Top = 8
        Width = 98
        Height = 14
        Caption = 'Campo de Pesquisa:'
      end
      object EdtPesquisa: TEdit
        Left = 8
        Top = 24
        Width = 289
        Height = 22
        TabOrder = 0
        OnChange = EdtPesquisaChange
      end
      object cbxCampos: TComboBox
        Left = 312
        Top = 24
        Width = 209
        Height = 22
        Style = csDropDownList
        ItemHeight = 14
        TabOrder = 1
      end
    end
    object PnlMultiSel: TPanel
      Left = 1
      Top = 51
      Width = 531
      Height = 37
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
      object BtTodos: TBitBtn
        Left = 196
        Top = 6
        Width = 137
        Height = 25
        Caption = 'Selecionar Todos - F5'
        TabOrder = 0
        OnClick = BtTodosClick
      end
      object BtNenhum: TBitBtn
        Left = 384
        Top = 6
        Width = 137
        Height = 25
        Caption = 'Desmarcar Todos - F6'
        TabOrder = 1
        OnClick = BtNenhumClick
      end
      object BtMarcaDesmarca: TBitBtn
        Left = 8
        Top = 6
        Width = 137
        Height = 25
        Caption = 'Marca/Desmarca - F4'
        TabOrder = 2
        OnClick = BtMarcaDesmarcaClick
      end
    end
  end
  object Btn_OK: TBitBtn
    Left = 200
    Top = 432
    Width = 75
    Height = 25
    Caption = '&Ok'
    ModalResult = 1
    TabOrder = 1
    Glyph.Data = {
      DE010000424DDE01000000000000760000002800000024000000120000000100
      0400000000006801000000000000000000001000000000000000000000000000
      80000080000000808000800000008000800080800000C0C0C000808080000000
      FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
      3333333333333333333333330000333333333333333333333333F33333333333
      00003333344333333333333333388F3333333333000033334224333333333333
      338338F3333333330000333422224333333333333833338F3333333300003342
      222224333333333383333338F3333333000034222A22224333333338F338F333
      8F33333300003222A3A2224333333338F3838F338F33333300003A2A333A2224
      33333338F83338F338F33333000033A33333A222433333338333338F338F3333
      0000333333333A222433333333333338F338F33300003333333333A222433333
      333333338F338F33000033333333333A222433333333333338F338F300003333
      33333333A222433333333333338F338F00003333333333333A22433333333333
      3338F38F000033333333333333A223333333333333338F830000333333333333
      333A333333333333333338330000333333333333333333333333333333333333
      0000}
    NumGlyphs = 2
  end
  object BitBtn2: TBitBtn
    Left = 296
    Top = 432
    Width = 75
    Height = 25
    Caption = '&Cancelar'
    TabOrder = 2
    Kind = bkCancel
  end
  object Button1: TButton
    Left = 88
    Top = 432
    Width = 75
    Height = 25
    Caption = 'Abre'
    TabOrder = 3
    OnClick = Button1Click
  end
  object Mm: TMemo
    Left = 0
    Top = 464
    Width = 537
    Height = 97
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object CG: TEmbCriteriaGroup
    Left = 8
    Top = 236
    Width = 529
    Height = 181
    TabOrder = 5
  end
  object Ds: TDataSource
    DataSet = cds
    Left = 432
    Top = 112
  end
  object qry: TSQLQuery
    MaxBlobSize = -1
    Params = <>
    Left = 264
    Top = 112
  end
  object dsp: TDataSetProvider
    DataSet = qry
    Left = 320
    Top = 112
  end
  object cds: TClientDataSet
    Aggregates = <>
    Params = <>
    ProviderName = 'dsp'
    Left = 376
    Top = 112
  end
end
