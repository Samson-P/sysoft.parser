object Lab2Form: TLab2Form
  Left = 251
  Top = 108
  Width = 546
  Height = 440
  Caption = 'Лабораторная работа №2'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  Icon.Data = {
    0000010001002020100000000000E80200001600000028000000200000004000
    0000010004000000000080020000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF009999
    99999999999BBBBBBBBBB9999999999999999999999BBBBBBBBBB99999999999
    99999999999BB9BBB99999999999999009990000099BB0BBB009900990099990
    09900990090BB90BBB0990099009999999900990099BB000BBB9900990099999
    9999009009900009BBBB90099009999999999000090009999BBB900990099999
    999099900900990090BBB000900999999999000099900009900BB00900999999
    9999999999999999999BBBB99999999999999999999999999999BBB99999EEEE
    EEEEEEEEEBBBBBBBBBBBBBBBEEEEEEEEEBBBBEEEEEBBBBBBBBBBBBBBBEEEEEEE
    EBBBBEEEEEBBBBBBBBBBBBBBBEEEEEE0000BBBEE00EBBB000EEEEE00EEE0EE00
    EE0BBBEE00EBBBBE00EEEE00EEE0EEBBBBBBBBBBBBBBBBBBBBBBBE00EEEEEEBB
    BBBBBBBBBBBBBBBBBBBBBE00EEEEEEBBBBBBBBBBBBBBBBBBBBBBBE00EEEEEE00
    EE00EEBBB0EE00BBB0EEEE00EEEEEE00EE00EE0BB0EE00EBBBEE0E00EEEEEE00
    EE00EE0BBBEE00EBBBBE0000EEEEEE00EE00EEE0BBBE00EEBBBEE000EEEEEEE0
    BBBBBBBBBBBBBBBBBBBBBBBBBBEEEEEEBBBBBBBBBBBBBBBBBBBBBBBBBBEEEE00
    BBBBBBBBBBBBBBBBBBBBBBBBBB00EE00EE00EE00E0BBB00E00EBBBEEEE00EEEE
    EEEEEEEEE00BBB0E00EBBBBEEE00EEEEEEEEEEEEE00BBBBE00EEBBBBEE00EEEE
    EEEEEEEEE00EBBBE00EE0BBBEE00EEEEEEEEEEEEE00EBBBE00EE0BBBEE000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000}
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 537
    Height = 369
    ActivePage = SheetFile
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object SheetFile: TTabSheet
      Caption = 'Исходный файл'
      object GroupText: TGroupBox
        Left = 8
        Top = 8
        Width = 516
        Height = 329
        Anchors = [akLeft, akTop, akRight, akBottom]
        Caption = 'Исходные данные'
        TabOrder = 0
        object ListIdents: TMemo
          Left = 4
          Top = 72
          Width = 506
          Height = 252
          Hint = 'Перечень всех идентификаторов, загруженных из файла'
          Anchors = [akLeft, akTop, akRight, akBottom]
          ParentShowHint = False
          ReadOnly = True
          ScrollBars = ssBoth
          ShowHint = True
          TabOrder = 3
          WordWrap = False
        end
        object EditFile: TEdit
          Left = 10
          Top = 30
          Width = 386
          Height = 21
          Hint = 'Строка для ввода имени загружаемого файла'
          Anchors = [akLeft, akTop, akRight]
          ParentShowHint = False
          ShowHint = True
          TabOrder = 0
          Text = 'Lab2.txt'
          OnChange = EditFileChange
        end
        object BtnFile: TButton
          Left = 403
          Top = 16
          Width = 105
          Height = 25
          Hint = 'Выбор загружаемого файла'
          Anchors = [akTop, akRight]
          Caption = '&Выбрать файл'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 1
          OnClick = BtnFileClick
        end
        object BtnLoad: TButton
          Left = 403
          Top = 44
          Width = 105
          Height = 25
          Hint = 'Загрузка выбранного файла в хэш-таблицы'
          Anchors = [akTop, akRight]
          Caption = '&Загрузить файл'
          ParentShowHint = False
          ShowHint = True
          TabOrder = 2
          OnClick = BtnLoadClick
        end
      end
    end
    object SheetLexems: TTabSheet
      Caption = 'Таблица лексем'
      ImageIndex = 1
      object GridLex: TStringGrid
        Left = 0
        Top = 0
        Width = 529
        Height = 341
        Align = alClient
        ColCount = 3
        FixedCols = 0
        RowCount = 2
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing]
        TabOrder = 0
        ColWidths = (
          54
          194
          263)
      end
    end
  end
  object BtnExit: TButton
    Left = 8
    Top = 377
    Width = 265
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Завершить работу с программой'
    TabOrder = 1
    OnClick = BtnExitClick
  end
  object FileOpenDlg: TOpenDialog
    DefaultExt = 'txt'
    Filter = 'Text files (*.txt)|*.txt|All files (*.*)|*.*'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Left = 368
    Top = 40
  end
end
