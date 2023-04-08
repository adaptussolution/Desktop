object frmMenu: TfrmMenu
  Left = 190
  Top = 141
  Width = 1195
  Height = 588
  BorderIcons = [biSystemMenu]
  Caption = 'Menu'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poMainFormCenter
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1187
    Height = 518
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
  end
  object StatusBar1: TStatusBar
    Left = 0
    Top = 518
    Width = 1187
    Height = 19
    Panels = <
      item
        Width = 790
      end
      item
        Width = 50
      end>
  end
  object MainMenu1: TMainMenu
    Left = 136
    Top = 104
    object Register: TMenuItem
      Caption = 'Cadastro'
      object Person: TMenuItem
        Caption = 'Pessoa'
        OnClick = PersonClick
      end
      object Operational: TMenuItem
        Caption = 'Operacional'
        object Usurio1: TMenuItem
          Caption = 'Usu'#225'rio'
          Hint = 'User'
        end
      end
      object company: TMenuItem
        Caption = 'Empresa'
        object Employee: TMenuItem
          Caption = 'Funcion'#225'rio'
        end
      end
    end
    object Report: TMenuItem
      Caption = 'Relat'#243'rio'
    end
    object Exit: TMenuItem
      Caption = '&Sair'
      OnClick = ExitClick
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 168
    Top = 104
  end
end
