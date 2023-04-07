object FrmFeriados: TFrmFeriados
  Left = 399
  Top = 226
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Feriados'
  ClientHeight = 205
  ClientWidth = 183
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PanFundo: TPanel
    Left = 0
    Top = 30
    Width = 183
    Height = 175
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 6
      Top = 9
      Width = 19
      Height = 13
      Caption = '&Dia:'
      FocusControl = EdtDia
    end
    object Label2: TLabel
      Left = 70
      Top = 9
      Width = 23
      Height = 13
      Caption = '&M'#234's:'
      FocusControl = CBoxMes
    end
    object LstFeriados: TListBox
      Left = 6
      Top = 34
      Width = 171
      Height = 134
      ItemHeight = 13
      TabOrder = 2
      OnClick = LstFeriadosClick
    end
    object EdtDia: TEdit
      Left = 28
      Top = 6
      Width = 25
      Height = 21
      MaxLength = 2
      TabOrder = 0
      OnChange = DiaMesChange
      OnKeyPress = EdtDiaKeyPress
    end
    object CBoxMes: TComboBox
      Left = 96
      Top = 6
      Width = 81
      Height = 21
      Style = csDropDownList
      DropDownCount = 12
      ItemHeight = 13
      TabOrder = 1
      OnChange = DiaMesChange
      Items.Strings = (
        'Janeiro'
        'Fevereiro'
        'Mar'#231'o'
        'Abril'
        'Maio'
        'Junho'
        'Julho'
        'Agosto'
        'Setembro'
        'Outubro'
        'Novembro'
        'Dezembro')
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 183
    Height = 30
    Caption = 'ToolBar1'
    EdgeBorders = [ebTop, ebBottom]
    TabOrder = 1
    object ToolButton2: TToolButton
      Left = 0
      Top = 2
      Width = 6
      Caption = 'ToolButton2'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object BtnAdicionar: TSpeedButton
      Left = 6
      Top = 2
      Width = 23
      Height = 22
      Enabled = False
      Flat = True
      Glyph.Data = {
        06010000424D06010000000000007600000028000000180000000C0000000100
        0400000000009000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        77777777777777772222777777778888777777772AA277777777877877777777
        2AA2777777778778777772222AA2222778888778888772AAAAAAAA2778777777
        778772AAAAAAAA2778777777778772222AA2222778888778888777772AA27777
        77778778777777772AA277777777877877777777222277777777888877777777
        77777777777777777777}
      NumGlyphs = 2
      OnClick = BtnAdicionarClick
    end
    object BtnRemover: TSpeedButton
      Left = 29
      Top = 2
      Width = 23
      Height = 22
      Enabled = False
      Flat = True
      Glyph.Data = {
        06010000424D06010000000000007600000028000000180000000C0000000100
        0400000000009000000000000000000000001000000000000000000000000000
        80000080000000808000800000008000800080800000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
        7777777777777777777777777777777777777777777777777777777777777777
        7777777777777777777771111111111778888888888771999999991778777777
        7787719999999917787777777787711111111117788888888887777777777777
        7777777777777777777777777777777777777777777777777777777777777777
        77777777777777777777}
      NumGlyphs = 2
      OnClick = BtnRemoverClick
    end
  end
end
