object frmParameters: TfrmParameters
  Left = 555
  Top = 267
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Par'#226'metros'
  ClientHeight = 91
  ClientWidth = 439
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 439
    Height = 91
    Align = alClient
    BevelOuter = bvNone
    Color = clBtnHighlight
    TabOrder = 0
    object lblPathBank: TLabel
      Left = 8
      Top = 24
      Width = 93
      Height = 13
      Caption = 'Caminho do Banco:'
    end
    object btnPathBank: TSpeedButton
      Left = 406
      Top = 20
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
      OnClick = btnPathBankClick
    end
    object btnSave: TSpeedButton
      Left = 341
      Top = 56
      Width = 89
      Height = 26
      Caption = 'Salvar'
      Glyph.Data = {
        26040000424D2604000000000000360000002800000012000000120000000100
        180000000000F003000000000000000000000000000000000000FFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFF2F7F6F2F3
        F2EFEFECF1F0EFF2F4F4F1F2EFF0F0EDF1EFECF0EEEBF1F0EEF3F4F2F2F3F3F2
        F4F2FFFFFFFFFFFF0000FFFFFFFFFFFFD7BCA4D5651ADA6C1BD5B7A5C2B4AAC2
        936EBEAA9EC6B9B0CCC0B8D6CDC7CBB5A4D36514DB7429C75A08F3F4F1FFFFFF
        0000FFFFFFF1F6F7D56C1FF1914BEE863AF7E7DADBCAC3E9771EDCBFAFD6CFCA
        E1D8D4EDE7E9E0D0C4E67F33F1914CDB7429F1F2F1FFFFFF0000FFFFFFF0F2F2
        E27B2FF0904BED8539F7ECE0E2D3CBE2751CE0C8B6D3CBC6D7CDC6E2DCD8D6C5
        B8E48135F1924CDC752AF1F2F1FFFFFF0000FFFFFFF0F3F3E27C32F29050EF87
        3BFBEFE4EAEBE9E8A675E7DBD1DDD7D6D0C9C4D8D4D2CEC0B1E6823BF3944EDB
        762BF2F2F1FFFFFF0000FFFFFFF0F2F2E07D33F39352F28B45F2CCB1FAF0E6F9
        EFE6EFE0D1EAD1C3DFC7B6D6C0B2CFA482EE8D49F59451DD792DF2F2F2FFFFFF
        0000FFFFFFF2F1F2DF7F34F39655F48F4BF4873CF28536F18436F18439F38738
        F4883BF6883CF68B42F2904BF69656DE7930F2F2F0FFFFFF0000FFFFFFF1F1F2
        E28137F5944FF2CEADF8DBC6F8D9C4F8D9C4F8D9C4F8D9C4F8D9C4F8D9C4F7DB
        C6F1CEADF49550DE7A36F2F2F1FFFFFF0000FFFFFFF1F1F2E58338F7954DF9EB
        E2F9EFE8FAEEE6FAEEE6FAEEE6FAEEE6FAEEE6FAEEE6FBEFE7F6EDE2F9954EDF
        7E38F0F1F0FFFFFF0000FFFFFFF0F1F0E4843BF89850FAEDE4FAF2EBFCF0E9FC
        F0E9FCF0E9FCF0E9FCF0E9FCF0E9FCF2EAF7EDE4FA9851E0813BF1F1F0FFFFFF
        0000FFFFFFF0F1F0E5853EF99A52F9F0E9FBF6EEFDF4ECFDF4ECFDF4ECFDF4EC
        FDF4ECFDF4ECFDF6EDF8F1EAFB9A52E1833DF1F1F0FFFFFF0000FFFFFFF0F1F1
        E78542FD9B5AF9F5EFFEF8F7FEF7F5FEF7F5FEF7F5FEF7F5FEF7F5FEF6F5FFF8
        F6F9F5F0FD9A58E28440F1F1F0FFFFFF0000FFFFFFF0F0F0EA8A45FF9F5CFCFC
        FCFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFCFEFDFF9E5AE7
        8944F1F0EFFFFFFF0000FFFFFFF1F2F1CF7A3CE8965DF2D2B9EFD4B9EED1B8EC
        D0B7EAD0B7EACDB5E9CBB4E7CBB3E5CBB3E5C9ADEA995ECB793BF3F2F0FFFFFF
        0000FFFFFFF1F3F2BC580ED17A3AD06615C85C0CC5590BC4580ABF5507BC5305
        B75102B54F00B04A00B24C04D67C3EB6590FF3F3F0FFFFFF0000FFFFFFFFFFFF
        F1F3F1F1F2F1F1F4F4F2F5F5F3F5F5F2F5F5F3F6F5F3F5F5F4F5F5F5F6F6F5F6
        F6F5F6F6F0F2F1F3F3F0FFFFFFFFFFFF0000FFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
        FFFFFFFFFFFFFFFF0000}
      OnClick = btnSaveClick
    end
    object edtPathBank: TEdit
      Left = 104
      Top = 22
      Width = 297
      Height = 21
      TabOrder = 0
    end
  end
  object OpenDialog1: TOpenDialog
    Left = 240
    Top = 40
  end
end
