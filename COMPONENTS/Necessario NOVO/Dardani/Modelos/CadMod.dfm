object FrmCadModelo: TFrmCadModelo
  Left = 228
  Top = 107
  HorzScrollBar.Smooth = True
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Cadastro de Modelos de Documentos'
  ClientHeight = 403
  ClientWidth = 762
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnClose = FormClose
  OnKeyDown = FormKeyDown
  OnKeyPress = FormKeyPress
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object StBrBarraStatus: TStatusBar
    Left = 0
    Top = 384
    Width = 762
    Height = 19
    Panels = <
      item
        Text = ' Mensagem: Registro n'#227'o encontrado.'
        Width = 460
      end
      item
        Alignment = taCenter
        Text = 'Selecionados: 1'
        Width = 50
      end>
    SizeGrip = False
  end
  object PnlRight: TPanel
    Left = 656
    Top = 20
    Width = 106
    Height = 364
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
    object BtBtnIncluir: TBitBtn
      Left = 11
      Top = 19
      Width = 84
      Height = 25
      Caption = '&Incluir'
      TabOrder = 1
      OnClick = BtBtnIncluirClick
      NumGlyphs = 3
      Spacing = 11
    end
    object BtBtnVisualizar: TBitBtn
      Left = 11
      Top = 49
      Width = 84
      Height = 25
      Caption = '&Visualizar'
      TabOrder = 2
      OnClick = BtBtnIncluirClick
      NumGlyphs = 2
      Spacing = 8
    end
    object wDBNvgtrSuperior: TwwDBNavigator
      Left = 8
      Top = 219
      Width = 90
      Height = 25
      AutosizeStyle = asSizeNavButtons
      DataSource = DM.dasModelo
      Flat = False
      MoveBy = 17
      ShowHint = True
      RepeatInterval.InitialDelay = 500
      RepeatInterval.Interval = 100
      ParentShowHint = False
      object nabPaginaAnterior: TwwNavButton
        Left = 0
        Top = 0
        Width = 23
        Height = 25
        Hint = 'Move para a p'#225'gina anterior'
        ImageIndex = -1
        NumGlyphs = 2
        Spacing = 4
        Transparent = False
        Caption = 'nabPaginaAnterior'
        Enabled = False
        DisabledTextColors.ShadeColor = clGray
        DisabledTextColors.HighlightColor = clBtnHighlight
        Index = 0
        Style = nbsPriorPage
      end
      object nabProximaPagina: TwwNavButton
        Left = 23
        Top = 0
        Width = 23
        Height = 25
        Hint = 'Move para a pr'#243'xima p'#225'gina'
        ImageIndex = -1
        NumGlyphs = 2
        Spacing = 4
        Transparent = False
        Caption = 'nabProximaPagina'
        Enabled = False
        DisabledTextColors.ShadeColor = clGray
        DisabledTextColors.HighlightColor = clBtnHighlight
        Index = 1
        Style = nbsNextPage
      end
      object nabSalvaBookMark: TwwNavButton
        Left = 46
        Top = 0
        Width = 22
        Height = 25
        Hint = 'Marca a posi'#231#227'o do registro corrente'
        ImageIndex = -1
        NumGlyphs = 2
        Spacing = 4
        Transparent = False
        Caption = 'nabSalvaBookMark'
        Enabled = False
        DisabledTextColors.ShadeColor = clGray
        DisabledTextColors.HighlightColor = clBtnHighlight
        Index = 2
        Style = nbsSaveBookmark
      end
      object nabRestauraBookMark: TwwNavButton
        Left = 68
        Top = 0
        Width = 22
        Height = 25
        Hint = 'Restaura a posi'#231#227'o do registro marcado'
        ImageIndex = -1
        NumGlyphs = 2
        Spacing = 4
        Transparent = False
        Caption = 'nabRestauraBookMark'
        Enabled = False
        DisabledTextColors.ShadeColor = clGray
        DisabledTextColors.HighlightColor = clBtnHighlight
        Index = 3
        Style = nbsRestoreBookmark
      end
    end
    object wDBNvgtrInferior: TwwDBNavigator
      Left = 8
      Top = 243
      Width = 90
      Height = 25
      AutosizeStyle = asSizeNavButtons
      DataSource = DM.dasModelo
      Flat = False
      ShowHint = True
      RepeatInterval.InitialDelay = 500
      RepeatInterval.Interval = 100
      ParentShowHint = False
      object nabPrimeiro: TwwNavButton
        Left = 0
        Top = 0
        Width = 23
        Height = 25
        Hint = 'Move para o primeiro registro'
        ImageIndex = -1
        NumGlyphs = 2
        Spacing = 4
        Transparent = False
        Caption = 'wwDBNavigator1First'
        Enabled = False
        DisabledTextColors.ShadeColor = clGray
        DisabledTextColors.HighlightColor = clBtnHighlight
        Index = 0
        Style = nbsFirst
      end
      object nabAnterior: TwwNavButton
        Left = 23
        Top = 0
        Width = 23
        Height = 25
        Hint = 'Move para o registro anterior'
        ImageIndex = -1
        NumGlyphs = 2
        Spacing = 4
        Transparent = False
        Caption = 'wwDBNavigator1Prior'
        Enabled = False
        DisabledTextColors.ShadeColor = clGray
        DisabledTextColors.HighlightColor = clBtnHighlight
        Index = 1
        Style = nbsPrior
      end
      object nabProximo: TwwNavButton
        Left = 46
        Top = 0
        Width = 22
        Height = 25
        Hint = 'Move para o pr'#243'ximo registro'
        ImageIndex = -1
        NumGlyphs = 2
        Spacing = 4
        Transparent = False
        Caption = 'wwDBNavigator1Next'
        Enabled = False
        DisabledTextColors.ShadeColor = clGray
        DisabledTextColors.HighlightColor = clBtnHighlight
        Index = 2
        Style = nbsNext
      end
      object nabUltimo: TwwNavButton
        Left = 68
        Top = 0
        Width = 22
        Height = 25
        Hint = 'Move para o '#250'ltimo registro'
        ImageIndex = -1
        NumGlyphs = 2
        Spacing = 4
        Transparent = False
        Caption = 'wwDBNavigator1Last'
        Enabled = False
        DisabledTextColors.ShadeColor = clGray
        DisabledTextColors.HighlightColor = clBtnHighlight
        Index = 3
        Style = nbsLast
      end
    end
    object BtBtnListar: TBitBtn
      Left = 11
      Top = 168
      Width = 84
      Height = 25
      Caption = '&Listar'
      TabOrder = 7
      NumGlyphs = 2
      Spacing = 12
    end
    object BtBtnAlterar: TBitBtn
      Left = 11
      Top = 79
      Width = 84
      Height = 25
      Caption = '&Alterar'
      TabOrder = 3
      OnClick = BtBtnIncluirClick
      NumGlyphs = 2
      Spacing = 12
    end
    object BtBtnFiltrar: TBitBtn
      Left = 11
      Top = 138
      Width = 84
      Height = 25
      Caption = 'Fil&trar'
      TabOrder = 5
      OnClick = BtBtnFiltrarClick
      NumGlyphs = 2
      Spacing = 12
    end
    object BtBtnExcluir: TBitBtn
      Left = 11
      Top = 108
      Width = 84
      Height = 25
      Caption = '&Excluir'
      TabOrder = 4
      OnClick = BtBtnExcluirClick
      NumGlyphs = 3
      Spacing = 15
    end
    object ChckBxFiltro: TCheckBox
      Left = 80
      Top = 141
      Width = 10
      Height = 13
      Hint = 'Abilita/Desabilita'
      TabOrder = 6
      Visible = False
      OnClick = ChckBxFiltroClick
    end
    object BtBtnFechar: TBitBtn
      Left = 11
      Top = 282
      Width = 84
      Height = 25
      Caption = '&Fechar'
      TabOrder = 0
      OnClick = BtBtnFecharClick
      NumGlyphs = 2
      Spacing = 8
    end
    object BtPreview: TBitBtn
      Left = 11
      Top = 312
      Width = 84
      Height = 25
      Caption = 'Preview'
      TabOrder = 10
      OnClick = BtPreviewClick
    end
  end
  object PgCntrlModelo: TPageControl
    Left = 0
    Top = 20
    Width = 656
    Height = 364
    ActivePage = TbShtModelo
    Align = alClient
    TabOrder = 1
    TabStop = False
    OnChange = PgCntrlModeloChange
    object TbShtModelo: TTabSheet
      Caption = '&Modelos'
      object PnlModelo: TPanel
        Left = 0
        Top = 0
        Width = 648
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object SpdBtnFiltrarModelo: TSpeedButton
          Left = 610
          Top = 10
          Width = 25
          Height = 21
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000130B0000130B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333FF3333333333333003333333333333377F33333333333307
            733333FFF333337773333C003333307733333777FF333777FFFFC0CC03330770
            000077777FF377777777C033C03077FFFFF077FF77F777FFFFF7CC00000F7777
            777077777777777777773CCCCC00000000003777777777777777333330030FFF
            FFF03333F77F7F3FF3F7333C0C030F00F0F03337777F7F77373733C03C030FFF
            FFF03377F77F7F3F333733C03C030F0FFFF03377F7737F733FF733C000330FFF
            0000337777F37F3F7777333CCC330F0F0FF0333777337F737F37333333330FFF
            0F03333333337FFF7F7333333333000000333333333377777733}
          NumGlyphs = 2
          OnClick = SpdBtnFiltrarModeloClick
        end
        object SpdBtnDownModelo: TSpeedButton
          Left = 581
          Top = 10
          Width = 25
          Height = 21
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            0400000000000001000000000000000000001000000010000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
            333333333337F33333333333333033333333333333373F333333333333090333
            33333333337F7F33333333333309033333333333337373F33333333330999033
            3333333337F337F33333333330999033333333333733373F3333333309999903
            333333337F33337F33333333099999033333333373333373F333333099999990
            33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
            33333333337F7F33333333333309033333333333337F7F333333333333090333
            33333333337F7F33333333333309033333333333337F7F333333333333090333
            33333333337F7F33333333333300033333333333337773333333}
          NumGlyphs = 2
          OnClick = MnItmContinuaClick
        end
        object LblPesModelo: TLabel
          Left = 10
          Top = 13
          Width = 46
          Height = 13
          Caption = '&Pesquisa:'
        end
        object EdtPesModelo: TEdit
          Left = 61
          Top = 10
          Width = 518
          Height = 21
          TabOrder = 0
          OnEnter = EdtPesModeloEnter
          OnExit = EdtPesModeloExit
          OnKeyDown = EdtPesModeloKeyDown
        end
      end
      object wDBGrdModelo: TwwDBGrid
        Left = 0
        Top = 41
        Width = 648
        Height = 295
        DisableThemes = False
        Selected.Strings = (
          'MODELO'#9'8'#9'Modelo'#9'F'
          'DESCRICAO'#9'55'#9'Descri'#231#227'o'#9'F'
          'ITEMDETALHE'#9'12'#9'Qtde de Itens~no Detalhe'#9'F'
          'INICIODETALHE'#9'13'#9'In'#237'cio da linha~no detalhe'#9'F'
          'TOTALCOLUNAS'#9'10'#9'Qtde de~Colunas'#9'F')
        IniAttributes.Delimiter = ';;'
        TitleColor = clBtnFace
        FixedCols = 0
        ShowHorzScrollBar = True
        Align = alClient
        Color = clInfoBk
        DataSource = DM.dasModelo
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        KeyOptions = [dgEnterToTab]
        MultiSelectOptions = [msoAutoUnselect, msoShiftSelect]
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgWordWrap, dgMultiSelect, dgTrailingEllipsis, dgShowCellHint]
        ParentFont = False
        ParentShowHint = False
        PopupMenu = PMnModelo
        ShowHint = True
        TabOrder = 1
        TitleAlignment = taCenter
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        TitleLines = 3
        TitleButtons = True
        UseTFields = False
        OnCalcCellColors = wDBGrdModeloCalcCellColors
        OnTitleButtonClick = wDBGrdModeloTitleButtonClick
        OnDblClick = wDBGrdModeloDblClick
        OnKeyDown = wDBGrdModeloKeyDown
        OnMouseDown = wDBGrdModeloMouseDown
        OnDrawTitleCell = wDBGrdModeloDrawTitleCell
      end
    end
    object TbShtConteudo: TTabSheet
      Caption = '&Conte'#250'do'
      ImageIndex = 1
      object PnlConteudo: TPanel
        Left = 0
        Top = 0
        Width = 648
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object LblPesConteudo: TLabel
          Left = 10
          Top = 14
          Width = 46
          Height = 13
          Caption = '&Pesquisa:'
        end
        object SpdBtnFiltrarConteudo: TSpeedButton
          Left = 616
          Top = 9
          Width = 25
          Height = 21
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000130B0000130B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333FF3333333333333003333333333333377F33333333333307
            733333FFF333337773333C003333307733333777FF333777FFFFC0CC03330770
            000077777FF377777777C033C03077FFFFF077FF77F777FFFFF7CC00000F7777
            777077777777777777773CCCCC00000000003777777777777777333330030FFF
            FFF03333F77F7F3FF3F7333C0C030F00F0F03337777F7F77373733C03C030FFF
            FFF03377F77F7F3F333733C03C030F0FFFF03377F7737F733FF733C000330FFF
            0000337777F37F3F7777333CCC330F0F0FF0333777337F737F37333333330FFF
            0F03333333337FFF7F7333333333000000333333333377777733}
          NumGlyphs = 2
          OnClick = SpdBtnFiltrarModeloClick
        end
        object SpdBtnDownConteudo: TSpeedButton
          Left = 582
          Top = 10
          Width = 25
          Height = 21
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            0400000000000001000000000000000000001000000010000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
            333333333337F33333333333333033333333333333373F333333333333090333
            33333333337F7F33333333333309033333333333337373F33333333330999033
            3333333337F337F33333333330999033333333333733373F3333333309999903
            333333337F33337F33333333099999033333333373333373F333333099999990
            33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
            33333333337F7F33333333333309033333333333337F7F333333333333090333
            33333333337F7F33333333333309033333333333337F7F333333333333090333
            33333333337F7F33333333333300033333333333337773333333}
          NumGlyphs = 2
          OnClick = MnItmContinuaClick
        end
        object EdtPesConteudo: TEdit
          Left = 61
          Top = 10
          Width = 546
          Height = 21
          TabOrder = 0
          OnEnter = EdtPesModeloEnter
          OnExit = EdtPesModeloExit
          OnKeyDown = EdtPesModeloKeyDown
        end
      end
      object wDBGrdConteudo: TwwDBGrid
        Left = 0
        Top = 41
        Width = 648
        Height = 295
        DisableThemes = False
        Selected.Strings = (
          'LINHA'#9'10'#9'Linha'#9'F'
          'COLUNA'#9'10'#9'Coluna'#9'F'
          'DESCRICAO'#9'69'#9'Descri'#231#227'o'#9'F'
          'TAMANHO'#9'10'#9'Tamanho'#9'F')
        IniAttributes.Delimiter = ';;'
        TitleColor = clBtnFace
        OnCellChanged = wDBGrdModeloCellChanged
        FixedCols = 0
        ShowHorzScrollBar = True
        Align = alClient
        Color = clInfoBk
        DataSource = DM.dasConteudo
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        KeyOptions = [dgEnterToTab]
        MultiSelectOptions = [msoAutoUnselect, msoShiftSelect]
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgWordWrap, dgMultiSelect, dgShowCellHint]
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        TabOrder = 1
        TitleAlignment = taCenter
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        TitleLines = 1
        TitleButtons = True
        UseTFields = False
        OnCalcCellColors = wDBGrdModeloCalcCellColors
        OnDblClick = wDBGrdModeloDblClick
        OnKeyDown = wDBGrdModeloKeyDown
        OnMouseDown = wDBGrdModeloMouseDown
        FooterCellColor = clWindow
      end
    end
    object TbShtDetalhe: TTabSheet
      Caption = '&Detalhe'
      ImageIndex = 2
      object PnlDetalhe: TPanel
        Left = 0
        Top = 0
        Width = 648
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object LblPesDetalhe: TLabel
          Left = 10
          Top = 14
          Width = 46
          Height = 13
          Caption = '&Pesquisa:'
        end
        object SpdBtnFiltrarDetalhe: TSpeedButton
          Left = 616
          Top = 9
          Width = 25
          Height = 21
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000130B0000130B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333FF3333333333333003333333333333377F33333333333307
            733333FFF333337773333C003333307733333777FF333777FFFFC0CC03330770
            000077777FF377777777C033C03077FFFFF077FF77F777FFFFF7CC00000F7777
            777077777777777777773CCCCC00000000003777777777777777333330030FFF
            FFF03333F77F7F3FF3F7333C0C030F00F0F03337777F7F77373733C03C030FFF
            FFF03377F77F7F3F333733C03C030F0FFFF03377F7737F733FF733C000330FFF
            0000337777F37F3F7777333CCC330F0F0FF0333777337F737F37333333330FFF
            0F03333333337FFF7F7333333333000000333333333377777733}
          NumGlyphs = 2
          OnClick = SpdBtnFiltrarModeloClick
        end
        object SpdBtnDownDetalhe: TSpeedButton
          Left = 582
          Top = 10
          Width = 25
          Height = 21
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            0400000000000001000000000000000000001000000010000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
            333333333337F33333333333333033333333333333373F333333333333090333
            33333333337F7F33333333333309033333333333337373F33333333330999033
            3333333337F337F33333333330999033333333333733373F3333333309999903
            333333337F33337F33333333099999033333333373333373F333333099999990
            33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
            33333333337F7F33333333333309033333333333337F7F333333333333090333
            33333333337F7F33333333333309033333333333337F7F333333333333090333
            33333333337F7F33333333333300033333333333337773333333}
          NumGlyphs = 2
          OnClick = MnItmContinuaClick
        end
        object EdtPesDetalhe: TEdit
          Left = 61
          Top = 10
          Width = 546
          Height = 21
          TabOrder = 0
          OnEnter = EdtPesModeloEnter
          OnExit = EdtPesModeloExit
          OnKeyDown = EdtPesModeloKeyDown
        end
      end
      object wDBGrdDetalhe: TwwDBGrid
        Left = 0
        Top = 41
        Width = 648
        Height = 295
        DisableThemes = False
        Selected.Strings = (
          'COLUNA'#9'10'#9'Coluna'#9'F'
          'DESCRICAO'#9'80'#9'Descri'#231#227'o'#9'F'
          'TAMANHO'#9'10'#9'Tamanho'#9'F')
        IniAttributes.Delimiter = ';;'
        TitleColor = clBtnFace
        OnCellChanged = wDBGrdModeloCellChanged
        FixedCols = 0
        ShowHorzScrollBar = True
        Align = alClient
        Color = clInfoBk
        DataSource = DM.dasDetalhe
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        KeyOptions = [dgEnterToTab]
        MultiSelectOptions = [msoAutoUnselect, msoShiftSelect]
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgWordWrap, dgMultiSelect, dgShowCellHint]
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        TabOrder = 1
        TitleAlignment = taCenter
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        TitleLines = 1
        TitleButtons = True
        UseTFields = False
        OnCalcCellColors = wDBGrdModeloCalcCellColors
        OnDblClick = wDBGrdModeloDblClick
        OnKeyDown = wDBGrdModeloKeyDown
        OnMouseDown = wDBGrdModeloMouseDown
        FooterCellColor = clWindow
      end
    end
    object TbShtRelacao: TTabSheet
      Caption = 'V'#237'&nculo'
      ImageIndex = 3
      object PnlRelacao: TPanel
        Left = 0
        Top = 0
        Width = 648
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 0
        object LblPesRelacao: TLabel
          Left = 10
          Top = 14
          Width = 46
          Height = 13
          Caption = '&Pesquisa:'
        end
        object SpdBtnFiltrarRelacao: TSpeedButton
          Left = 616
          Top = 9
          Width = 25
          Height = 21
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000130B0000130B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333FF3333333333333003333333333333377F33333333333307
            733333FFF333337773333C003333307733333777FF333777FFFFC0CC03330770
            000077777FF377777777C033C03077FFFFF077FF77F777FFFFF7CC00000F7777
            777077777777777777773CCCCC00000000003777777777777777333330030FFF
            FFF03333F77F7F3FF3F7333C0C030F00F0F03337777F7F77373733C03C030FFF
            FFF03377F77F7F3F333733C03C030F0FFFF03377F7737F733FF733C000330FFF
            0000337777F37F3F7777333CCC330F0F0FF0333777337F737F37333333330FFF
            0F03333333337FFF7F7333333333000000333333333377777733}
          NumGlyphs = 2
          OnClick = SpdBtnFiltrarModeloClick
        end
        object SpdBtnDownRelacao: TSpeedButton
          Left = 582
          Top = 10
          Width = 25
          Height = 21
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            0400000000000001000000000000000000001000000010000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
            333333333337F33333333333333033333333333333373F333333333333090333
            33333333337F7F33333333333309033333333333337373F33333333330999033
            3333333337F337F33333333330999033333333333733373F3333333309999903
            333333337F33337F33333333099999033333333373333373F333333099999990
            33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
            33333333337F7F33333333333309033333333333337F7F333333333333090333
            33333333337F7F33333333333309033333333333337F7F333333333333090333
            33333333337F7F33333333333300033333333333337773333333}
          NumGlyphs = 2
          OnClick = MnItmContinuaClick
        end
        object EdtPesRelacao: TEdit
          Left = 61
          Top = 10
          Width = 546
          Height = 21
          TabOrder = 0
          OnEnter = EdtPesModeloEnter
          OnExit = EdtPesModeloExit
          OnKeyDown = EdtPesModeloKeyDown
        end
      end
      object wDBGrdRelacao: TwwDBGrid
        Left = 0
        Top = 41
        Width = 648
        Height = 295
        DisableThemes = False
        Selected.Strings = (
          'CAMPOCONTEUDO'#9'37'#9'Conteudo'#9'F'#9'Campo'
          'CAMPODETALHE'#9'37'#9'Detalhe'#9'F'#9'Campo'
          'POSICAOCONTEUDO'#9'12'#9'Conteudo'#9'F'#9'Posi'#231#227'o'
          'POSICAODETALHE'#9'13'#9'Detalhe'#9'F'#9'Posi'#231#227'o')
        IniAttributes.Delimiter = ';;'
        TitleColor = clBtnFace
        OnCellChanged = wDBGrdModeloCellChanged
        FixedCols = 0
        ShowHorzScrollBar = True
        Align = alClient
        Color = clInfoBk
        DataSource = DM.dasRelacao
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        KeyOptions = [dgEnterToTab]
        MultiSelectOptions = [msoAutoUnselect, msoShiftSelect]
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgWordWrap, dgMultiSelect, dgShowCellHint]
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        TabOrder = 1
        TitleAlignment = taCenter
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        TitleLines = 3
        TitleButtons = True
        UseTFields = False
        OnCalcCellColors = wDBGrdModeloCalcCellColors
        OnDblClick = wDBGrdModeloDblClick
        OnKeyDown = wDBGrdModeloKeyDown
        OnMouseDown = wDBGrdModeloMouseDown
        FooterCellColor = clWindow
      end
    end
    object TbShtOrdena: TTabSheet
      Caption = '&Ordena'#231#227'o'
      ImageIndex = 4
      object wDBGrdOrdena: TwwDBGrid
        Left = 0
        Top = 41
        Width = 648
        Height = 295
        DisableThemes = False
        Selected.Strings = (
          'RECNO'#9'10'#9'Ordem'#9'F'
          'DESCRICAO'#9'91'#9'Descri'#231#227'o'#9'F')
        IniAttributes.Delimiter = ';;'
        TitleColor = clBtnFace
        OnCellChanged = wDBGrdModeloCellChanged
        FixedCols = 0
        ShowHorzScrollBar = True
        Align = alClient
        Color = clInfoBk
        DataSource = DM.dasModOrdena
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'MS Sans Serif'
        Font.Style = []
        KeyOptions = [dgEnterToTab]
        MultiSelectOptions = [msoAutoUnselect, msoShiftSelect]
        Options = [dgTitles, dgIndicator, dgColumnResize, dgColLines, dgRowLines, dgTabs, dgConfirmDelete, dgCancelOnExit, dgWordWrap, dgMultiSelect, dgShowCellHint]
        ParentFont = False
        ParentShowHint = False
        ShowHint = False
        TabOrder = 0
        TitleAlignment = taCenter
        TitleFont.Charset = DEFAULT_CHARSET
        TitleFont.Color = clWindowText
        TitleFont.Height = -11
        TitleFont.Name = 'MS Sans Serif'
        TitleFont.Style = []
        TitleLines = 3
        TitleButtons = True
        UseTFields = False
        OnCalcCellColors = wDBGrdModeloCalcCellColors
        OnDblClick = wDBGrdModeloDblClick
        OnKeyDown = wDBGrdModeloKeyDown
        OnMouseDown = wDBGrdModeloMouseDown
        FooterCellColor = clWindow
      end
      object PnlOrdena: TPanel
        Left = 0
        Top = 0
        Width = 648
        Height = 41
        Align = alTop
        BevelOuter = bvNone
        TabOrder = 1
        object LblPesOrdena: TLabel
          Left = 10
          Top = 14
          Width = 46
          Height = 13
          Caption = '&Pesquisa:'
        end
        object SpdBtnFiltrarOrdena: TSpeedButton
          Left = 616
          Top = 9
          Width = 25
          Height = 21
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            04000000000000010000130B0000130B00001000000000000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
            3333333333333333FF3333333333333003333333333333377F33333333333307
            733333FFF333337773333C003333307733333777FF333777FFFFC0CC03330770
            000077777FF377777777C033C03077FFFFF077FF77F777FFFFF7CC00000F7777
            777077777777777777773CCCCC00000000003777777777777777333330030FFF
            FFF03333F77F7F3FF3F7333C0C030F00F0F03337777F7F77373733C03C030FFF
            FFF03377F77F7F3F333733C03C030F0FFFF03377F7737F733FF733C000330FFF
            0000337777F37F3F7777333CCC330F0F0FF0333777337F737F37333333330FFF
            0F03333333337FFF7F7333333333000000333333333377777733}
          NumGlyphs = 2
          OnClick = SpdBtnFiltrarModeloClick
        end
        object SpdBtnDownOrdena: TSpeedButton
          Left = 582
          Top = 10
          Width = 25
          Height = 21
          Glyph.Data = {
            76010000424D7601000000000000760000002800000020000000100000000100
            0400000000000001000000000000000000001000000010000000000000000000
            800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
            FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333303333
            333333333337F33333333333333033333333333333373F333333333333090333
            33333333337F7F33333333333309033333333333337373F33333333330999033
            3333333337F337F33333333330999033333333333733373F3333333309999903
            333333337F33337F33333333099999033333333373333373F333333099999990
            33333337FFFF3FF7F33333300009000033333337777F77773333333333090333
            33333333337F7F33333333333309033333333333337F7F333333333333090333
            33333333337F7F33333333333309033333333333337F7F333333333333090333
            33333333337F7F33333333333300033333333333337773333333}
          NumGlyphs = 2
          OnClick = MnItmContinuaClick
        end
        object EdtPesOrdena: TEdit
          Left = 61
          Top = 10
          Width = 546
          Height = 21
          TabOrder = 0
          OnEnter = EdtPesModeloEnter
          OnExit = EdtPesModeloExit
          OnKeyDown = EdtPesModeloKeyDown
        end
      end
    end
  end
  object PnlTopModelo: TPanel
    Left = 0
    Top = 0
    Width = 762
    Height = 20
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object LblNome: TLabel
      Left = 0
      Top = 0
      Width = 762
      Height = 20
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
  end
  object PMnModelo: TPopupMenu
    Left = 271
    Top = 241
    object MnItmSalvaGrid: TMenuItem
      Caption = '&Salva grid'
      ShortCut = 16467
      OnClick = MnItmSalvaGridClick
    end
    object MnItmCarregaGrid: TMenuItem
      Caption = '&Carrega grid'
      ShortCut = 16451
      OnClick = MnItmCarregaGridClick
    end
    object MnItmRefresh: TMenuItem
      Caption = '&Refresh'
      ShortCut = 16466
      OnClick = MnItmRefreshClick
    end
    object MnItm1: TMenuItem
      Caption = '-'
    end
    object MnItmContinua: TMenuItem
      Caption = 'Continua &pesquisa'
      ShortCut = 16464
      OnClick = MnItmContinuaClick
    end
    object MnItm2: TMenuItem
      Caption = '-'
    end
    object MnItmPesquisaAvancada: TMenuItem
      Caption = 'Pesquisa &avan'#231'ada'
      ShortCut = 16449
      OnClick = MnItmPesquisaAvancadaClick
    end
    object MnItmFiltro: TMenuItem
      Caption = '&Filtro'
      ShortCut = 16454
      OnClick = MnItmFiltroClick
    end
  end
  object PrintDialog: TPrintDialog
    Left = 196
    Top = 244
  end
  object qryMain: TSQLQuery
    MaxBlobSize = -1
    Params = <
      item
        DataType = ftUnknown
        Name = 'FORNECEDOR'
        ParamType = ptUnknown
      end>
    SQL.Strings = (
      'select   FORNECEDOR, NOME'
      'from     ALC_FORNECEDOR'
      'where FORNECEDOR = :FORNECEDOR')
    SQLConnection = DmConexao.SqlCon
    Left = 224
    Top = 152
  end
end
