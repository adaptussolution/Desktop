object frmMenu: TfrmMenu
  Left = -22
  Top = 144
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
  OnCreate = FormCreate
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
        Width = 1000
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
        object User: TMenuItem
          Caption = 'Usu'#225'rio'
          Hint = 'User'
          OnClick = UserClick
        end
        object RegisterAccess: TMenuItem
          Caption = 'Acesso'
          OnClick = RegisterAccessClick
        end
      end
      object company: TMenuItem
        Caption = 'Empresa'
        object REGISTERCOMPANY: TMenuItem
          Caption = 'Empresa'
          OnClick = REGISTERCOMPANYClick
        end
        object SECTOR: TMenuItem
          Caption = 'Setor'
          OnClick = SECTORClick
        end
        object OFFICE: TMenuItem
          Caption = 'Cargo'
          OnClick = OFFICEClick
        end
        object Employee: TMenuItem
          Caption = 'Funcion'#225'rio'
          OnClick = EmployeeClick
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
