object frmDlgInspetorObjetos: TfrmDlgInspetorObjetos
  Left = 597
  Top = 130
  BorderIcons = []
  BorderStyle = bsToolWindow
  Caption = 'Inspetor de Objetos'
  ClientHeight = 180
  ClientWidth = 281
  Color = 16776176
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 14
  object Label1: TLabel
    Left = 8
    Top = 56
    Width = 33
    Height = 14
    Caption = 'Coluna'
  end
  object Label2: TLabel
    Left = 8
    Top = 32
    Width = 26
    Height = 14
    Caption = 'Linha'
  end
  object Label3: TLabel
    Left = 8
    Top = 80
    Width = 38
    Height = 14
    Caption = 'Largura'
  end
  object Label4: TLabel
    Left = 8
    Top = 128
    Width = 46
    Height = 14
    Caption = 'Conte'#250'do'
  end
  object Label5: TLabel
    Left = 8
    Top = 103
    Width = 46
    Height = 14
    Caption = 'Categoria'
  end
  object Label6: TLabel
    Left = 8
    Top = 152
    Width = 34
    Height = 14
    Caption = 'Banda:'
  end
  object CbbListaCampo: TComboBox
    Left = 4
    Top = 1
    Width = 277
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    TabOrder = 0
    OnChange = CbbListaCampoChange
  end
  object EdtLinha: TEdit
    Left = 64
    Top = 32
    Width = 217
    Height = 22
    TabOrder = 1
    OnChange = EdtLinhaChange
  end
  object EdtColuna: TEdit
    Left = 64
    Top = 56
    Width = 217
    Height = 22
    TabOrder = 2
    OnChange = EdtColunaChange
  end
  object EdtTamanho: TEdit
    Left = 64
    Top = 80
    Width = 217
    Height = 22
    TabOrder = 3
    OnChange = EdtTamanhoChange
  end
  object EdtConteudo: TEdit
    Left = 64
    Top = 128
    Width = 217
    Height = 22
    TabOrder = 4
    OnChange = EdtConteudoChange
  end
  object cbbCategoriaCampo: TComboBox
    Left = 64
    Top = 104
    Width = 217
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    TabOrder = 5
    OnChange = cbbCategoriaCampoChange
    Items.Strings = (
      'Texto'
      'Dado')
  end
  object cbbCampo: TComboBox
    Left = 64
    Top = 128
    Width = 217
    Height = 22
    Style = csDropDownList
    ItemHeight = 14
    TabOrder = 6
    OnChange = cbbCampoChange
    Items.Strings = (
      'Texto'
      'Dado')
  end
  object cbbBanda: TComboBox
    Left = 64
    Top = 152
    Width = 217
    Height = 22
    Style = csDropDownList
    Enabled = False
    ItemHeight = 14
    TabOrder = 7
    Items.Strings = (
      'Mestre'
      'Detalhe')
  end
end
