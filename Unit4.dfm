object Form4: TForm4
  Left = 321
  Top = 361
  BorderStyle = bsNone
  Caption = 'Form4'
  ClientHeight = 159
  ClientWidth = 331
  Color = clGradientInactiveCaption
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 92
    Top = 25
    Width = 168
    Height = 25
    Caption = #1060#1086#1088#1084#1072' '#1073#1077#1079' '#1088#1072#1084#1082#1080
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 331
    Height = 25
    Cursor = crSizeAll
    Align = alTop
    BevelOuter = bvNone
    Caption = #1058#1103#1085#1080' '#1079#1072' '#1084#1077#1085#1103
    Color = clAppWorkSpace
    TabOrder = 0
    OnMouseDown = Panel1MouseDown
  end
  object Panel2: TPanel
    Left = 0
    Top = 129
    Width = 331
    Height = 30
    Align = alBottom
    BevelOuter = bvNone
    ParentColor = True
    TabOrder = 1
    object Panel3: TPanel
      Left = 291
      Top = 0
      Width = 40
      Height = 30
      Cursor = crSizeNWSE
      Align = alRight
      BevelOuter = bvNone
      Caption = #1058#1103#1085#1080
      Color = clAppWorkSpace
      TabOrder = 0
      OnMouseDown = Panel3MouseDown
    end
  end
end
