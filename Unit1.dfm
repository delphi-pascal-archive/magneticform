object Form1: TForm1
  Left = 387
  Top = 162
  Caption = 'Form1 - '#1043#1083#1072#1074#1085#1072#1103' '#1092#1086#1088#1084#1072
  ClientHeight = 98
  ClientWidth = 273
  Color = clActiveCaption
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesigned
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 34
    Top = 8
    Width = 22
    Height = 25
    AutoSize = False
    Caption = '15'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Label2: TLabel
    Left = 62
    Top = 47
    Width = 193
    Height = 25
    AutoSize = False
    Caption = #1054#1090#1089#1090#1091#1087' '#1087#1088#1080#1083#1080#1087#1072#1085#1080#1103
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object TrackBar1: TTrackBar
    Left = 62
    Top = 8
    Width = 150
    Height = 33
    Max = 50
    Min = 3
    Position = 15
    TabOrder = 0
    OnChange = TrackBar1Change
  end
end
