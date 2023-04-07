object Form1: TForm1
  Left = 423
  Top = 110
  Width = 582
  Height = 480
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 14
  object BitBtn1: TBitBtn
    Left = 16
    Top = 224
    Width = 75
    Height = 25
    Caption = 'Executa'
    TabOrder = 0
    OnClick = BitBtn1Click
  end
  object Grd: TDrawGrid
    Left = 120
    Top = 240
    Width = 505
    Height = 153
    FixedCols = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing]
    TabOrder = 1
  end
  object ScrollBox1: TScrollBox
    Left = 8
    Top = 8
    Width = 425
    Height = 209
    BevelInner = bvNone
    BevelOuter = bvNone
    TabOrder = 2
    object Box01: TScrollBox
      Left = 0
      Top = 44
      Width = 405
      Height = 169
      Align = alTop
      TabOrder = 0
      object pnlBase01: TPanel
        Left = 0
        Top = 0
        Width = 401
        Height = 57
        Align = alTop
        BevelOuter = bvNone
        Caption = 'pnlBase01'
        TabOrder = 0
      end
    end
    object pnl01: TPanel
      Left = 0
      Top = 21
      Width = 405
      Height = 23
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
      object Label1: TLabel
        Left = 18
        Top = 5
        Width = 91
        Height = 14
        Caption = 'Nome do Cliente'
        Color = clBtnFace
        Font.Charset = ANSI_CHARSET
        Font.Color = clRed
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentColor = False
        ParentFont = False
      end
      object lbl01: TLabel
        Left = 0
        Top = 0
        Width = 18
        Height = 23
        Align = alLeft
        Caption = '  +  '
        OnMouseDown = lbl01MouseDown
      end
      object ComboBox1: TComboBox
        Left = 340
        Top = 2
        Width = 65
        Height = 21
        Style = csDropDownList
        Ctl3D = True
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = [fsBold]
        ItemHeight = 13
        ItemIndex = 0
        ParentCtl3D = False
        ParentFont = False
        TabOrder = 0
        Text = 'E'
        Items.Strings = (
          'E'
          'OU')
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 0
      Width = 405
      Height = 21
      Align = alTop
      TabOrder = 2
      object Label2: TLabel
        Left = 16
        Top = 5
        Width = 85
        Height = 14
        Caption = 'Campo ou Crit'#233'rio'
      end
      object Label3: TLabel
        Left = 144
        Top = 5
        Width = 26
        Height = 14
        Caption = 'Valor'
      end
      object Label4: TLabel
        Left = 344
        Top = 5
        Width = 24
        Height = 14
        Caption = 'E/OU'
      end
    end
  end
  object Button1: TButton
    Left = 24
    Top = 312
    Width = 75
    Height = 25
    Caption = 'Button1'
    TabOrder = 3
  end
  object Panel1: TPanel
    Left = 456
    Top = 8
    Width = 561
    Height = 28
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 4
    object Shape1: TShape
      Left = 0
      Top = 27
      Width = 561
      Height = 1
      Align = alBottom
      Pen.Style = psDot
    end
    object cbbBase: TComboBox
      Left = 16
      Top = 4
      Width = 145
      Height = 21
      Style = csDropDownList
      Ctl3D = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ItemHeight = 13
      ItemIndex = 0
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 0
      Text = 'igual a'
      Items.Strings = (
        'igual a'
        'diferente de'
        'maior que'
        'maior ou igual a'
        'menor que'
        'menor ou igual a')
    end
    object Edit1: TEdit
      Left = 168
      Top = 3
      Width = 201
      Height = 22
      TabOrder = 1
      Text = '0'
    end
    object cbbEOU: TComboBox
      Left = 374
      Top = 2
      Width = 65
      Height = 21
      Style = csDropDownList
      Ctl3D = True
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ItemHeight = 13
      ItemIndex = 0
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 2
      Text = 'E'
      Items.Strings = (
        'E'
        'OU')
    end
  end
  object DateTimePicker1: TDateTimePicker
    Left = 624
    Top = 72
    Width = 186
    Height = 22
    Date = 38627.504592546300000000
    Time = 38627.504592546300000000
    TabOrder = 5
  end
  object Q: TEmbQuery
    Fields = <
      item
        InternalName = 'CODCLI'
        Prefix = 'C'
        DisplayText = 'C'#243'digo'
        DisplayWidth = 0
        ContentType = ctInteger
        Visible = False
        Indexed = True
      end
      item
        InternalName = 'NOMCLI'
        Prefix = 'C'
        DisplayText = 'Nome'
        DisplayWidth = 0
        ContentType = ctString
        Visible = True
        Indexed = True
      end
      item
        InternalName = 'DATNSC'
        Prefix = 'C'
        DisplayText = 'Nascimento'
        DisplayWidth = 0
        ContentType = ctDate
        Visible = True
        Indexed = False
      end
      item
        InternalName = 'SEXO'
        DisplayText = 'Sexo'
        DisplayWidth = 20
        ContentType = ctString
        Expression.Strings = (
          'CASE'
          '   WHEN C.IDTSEX = '#39'M'#39' THEN '#39'MASCULINO'#39
          '   ELSE '#39'FEMININO'#39
          'END AS SEXO')
        Visible = True
        Indexed = False
      end>
    SqlFrom.Strings = (
      'CLI C'
      'LEFT JOIN CLIDET CD'
      'ON        CD.CODCLI = C.CODCLI')
    SqlOrderBy.Strings = (
      'C.NOMCLI')
    SqlConnection = SqlCon
    Left = 48
    Top = 256
  end
  object SqlCon: TSQLConnection
    ConnectionName = 'SWSEGURANCA'
    DriverName = 'Interbase'
    GetDriverFunc = 'getSQLDriverINTERBASE'
    LibraryName = 'dbexpint.dll'
    LoginPrompt = False
    Params.Strings = (
      'DriverName=Interbase'
      'Database=localhost:D:\Aplicativos\SWPLUS\BD\SWMT.DAT'
      'RoleName=RoleName'
      'User_Name=sysdba'
      'Password=masterkey'
      'ServerCharSet=WIN1252'
      'SQLDialect=3'
      'BlobSize=-1'
      'CommitRetain=False'
      'WaitOnLocks=True'
      'ErrorResourceFile='
      'LocaleCode=0000'
      'Interbase TransIsolation=ReadCommited'
      'Trim Char=False')
    VendorLib = 'gds32.dll'
    Left = 16
    Top = 256
  end
end
