object FrmRelatorio: TFrmRelatorio
  Left = 510
  Top = 132
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  ClientHeight = 429
  ClientWidth = 451
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  OnKeyPress = FormKeyPress
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 380
    Width = 451
    Height = 49
    Align = alBottom
    BevelOuter = bvNone
    Color = 2621440
    ParentBackground = False
    TabOrder = 0
    object btnvisualizar: TButton
      Left = 128
      Top = 2
      Width = 100
      Height = 40
      Caption = 'Visualizar'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 1
    end
    object btnimprimir: TButton
      Left = 24
      Top = 2
      Width = 100
      Height = 40
      Caption = 'Imprimir'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 0
    end
    object btnsair: TButton
      Left = 231
      Top = 2
      Width = 100
      Height = 40
      Caption = 'Sair'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 451
    Height = 380
    Align = alClient
    Color = clWhite
    ParentBackground = False
    TabOrder = 1
    object Panel3: TPanel
      Left = 1
      Top = 323
      Width = 449
      Height = 56
      Align = alBottom
      BevelOuter = bvNone
      Color = clWhite
      ParentBackground = False
      TabOrder = 0
      object Label1: TLabel
        Left = 8
        Top = 8
        Width = 31
        Height = 13
        Caption = 'Label1'
      end
      object BitBtn1: TBitBtn
        Left = 295
        Top = 134
        Width = 75
        Height = 25
        Caption = 'BitBtn1'
        TabOrder = 0
        Visible = False
      end
    end
  end
  object ActionList1: TActionList
    Left = 400
    Top = 8
    object ActSair: TAction
      Caption = 'ActSair'
      ShortCut = 27
      OnExecute = ActSairExecute
    end
  end
  object FormStorage1: TFormStorage
    UseRegistry = True
    StoredValues = <>
    Left = 288
    Top = 56
  end
  object DsSource: TDataSource
    Left = 184
    Top = 56
  end
end
