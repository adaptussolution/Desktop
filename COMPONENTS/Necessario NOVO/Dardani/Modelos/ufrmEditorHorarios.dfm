object frmEditorHorarios: TfrmEditorHorarios
  Left = 425
  Top = 122
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'Editor de Hor'#225'rios'
  ClientHeight = 346
  ClientWidth = 279
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  OnClose = FormClose
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object lblData: TLabel
    Left = 8
    Top = 8
    Width = 201
    Height = 13
    AutoSize = False
    Caption = 'lblData'
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object EdtHoraIni: TMaskEdit
    Left = 8
    Top = 24
    Width = 81
    Height = 21
    EditMask = '!90:00;1;_'
    MaxLength = 5
    TabOrder = 0
    Text = '  :  '
  end
  object EdtHoraFim: TMaskEdit
    Left = 104
    Top = 24
    Width = 81
    Height = 21
    EditMask = '!90:00;1;_'
    MaxLength = 5
    TabOrder = 1
    Text = '  :  '
  end
  object BtAdiciona: TBitBtn
    Left = 192
    Top = 24
    Width = 81
    Height = 25
    Caption = 'Adiciona'
    TabOrder = 2
    OnClick = BtAdicionaClick
  end
  object BitBtn1: TBitBtn
    Left = 8
    Top = 312
    Width = 81
    Height = 25
    Caption = '&OK'
    ModalResult = 1
    TabOrder = 3
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
    Left = 104
    Top = 312
    Width = 81
    Height = 25
    Caption = '&Cancelar'
    TabOrder = 4
    Kind = bkCancel
  end
  object BtRemove: TBitBtn
    Left = 192
    Top = 56
    Width = 81
    Height = 25
    Caption = 'Remove'
    TabOrder = 5
    OnClick = BtRemoveClick
  end
  object grdHorarios: TStringGrid
    Left = 8
    Top = 56
    Width = 177
    Height = 249
    ColCount = 3
    DefaultColWidth = 15
    DefaultRowHeight = 20
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 6
    ColWidths = (
      15
      62
      70)
  end
end
