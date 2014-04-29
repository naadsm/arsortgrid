object Form1: TForm1
  Left = 333
  Top = 70
  Width = 696
  Height = 751
  Caption = 'Form1'
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
  object SortGrid1: TARSortGrid
    Left = 0
    Top = 89
    Width = 688
    Height = 635
    Align = alClient
    ColCount = 8
    DefaultColWidth = 75
    DefaultRowHeight = 16
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goDrawFocusSelected, goColSizing]
    TabOrder = 0
    IsReadOnly = True
    AlignmentHorz = taLeftJustify
    AlignmentVert = taMiddle
    ProportionalScrollBars = False
    ExtendedKeys = False
    SortSymbol = sgGlyph
    SortSpacingHor = 2
    SortColumn = 1
    SortOnClick = True
    FooterFont.Charset = DEFAULT_CHARSET
    FooterFont.Color = clWindowText
    FooterFont.Height = -11
    FooterFont.Name = 'MS Sans Serif'
    FooterFont.Style = []
    PrintOptions.Orientation = poPortrait
    PrintOptions.PageTitleMargin = 0
    PrintOptions.PageFooter = 'date|time|page'
    PrintOptions.HeaderSize = 10
    PrintOptions.FooterSize = 7
    PrintOptions.DateFormat = 'd-mmm-yyyy'
    PrintOptions.TimeFormat = 'h:nn'
    PrintOptions.Copies = 0
    PrintOptions.FromRow = 0
    PrintOptions.ToRow = 0
    PrintOptions.PreviewPage = 0
    PrintOptions.BorderStyle = bsNone
    PrintOptions.MarginBottom = 0
    PrintOptions.MarginLeft = 0
    PrintOptions.MarginTop = 0
    PrintOptions.MarginRight = 0
    WordWrap = False
    OnGetCellFormat = SortGrid1GetCellFormat
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 89
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 192
      Top = 16
      Width = 44
      Height = 13
      Caption = 'Find Text'
    end
    object Label2: TLabel
      Left = 192
      Top = 48
      Width = 82
      Height = 13
      Caption = 'Search in column'
    end
    object Button1: TButton
      Left = 8
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Load'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 8
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Save'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 104
      Top = 8
      Width = 75
      Height = 25
      Caption = 'Add something'
      TabOrder = 2
      OnClick = Button3Click
    end
    object chkfilter: TCheckBox
      Left = 379
      Top = 16
      Width = 47
      Height = 17
      Caption = 'Filter'
      TabOrder = 3
      OnClick = chkfilterClick
    end
    object ChkCase: TCheckBox
      Left = 380
      Top = 40
      Width = 89
      Height = 17
      Caption = 'CaseSensitive'
      TabOrder = 4
      OnClick = ChkCaseClick
    end
    object FindButton: TButton
      Left = 480
      Top = 16
      Width = 75
      Height = 21
      Caption = 'Find'
      TabOrder = 5
      OnClick = FindButtonClick
    end
    object FindNext: TButton
      Left = 480
      Top = 44
      Width = 75
      Height = 25
      Caption = 'Find Next'
      TabOrder = 6
      OnClick = FindNextClick
    end
    object findedit: TEdit
      Left = 248
      Top = 12
      Width = 121
      Height = 21
      TabOrder = 7
      OnChange = findeditChange
    end
    object ComColumn: TComboBox
      Left = 283
      Top = 44
      Width = 87
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 8
      OnChange = ComColumnChange
      Items.Strings = (
        'All'
        '1'
        '2'
        '3'
        '4'
        '5'
        '6'
        '7'
        '8')
    end
    object btnPrint: TButton
      Left = 104
      Top = 40
      Width = 75
      Height = 25
      Caption = 'Print'
      TabOrder = 9
      OnClick = btnPrintClick
    end
  end
end
