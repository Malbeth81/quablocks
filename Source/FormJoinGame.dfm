object frmJoinGame: TfrmJoinGame
  Left = 221
  Top = 110
  AutoScroll = False
  Caption = 'Join a game'
  ClientHeight = 233
  ClientWidth = 281
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object lblGames: TLabel
    Left = 8
    Top = 8
    Width = 71
    Height = 13
    Caption = 'Hosted games:'
  end
  object lblName: TLabel
    Left = 8
    Top = 148
    Width = 54
    Height = 13
    Caption = 'Your name:'
  end
  object stgGames: TStringGrid
    Left = 8
    Top = 24
    Width = 265
    Height = 113
    ColCount = 2
    DefaultColWidth = 105
    DefaultRowHeight = 18
    FixedCols = 0
    RowCount = 2
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    ScrollBars = ssVertical
    TabOrder = 0
    ColWidths = (
      135
      104)
  end
  object btnOk: TButton
    Left = 136
    Top = 200
    Width = 65
    Height = 25
    Caption = 'Ok'
    Default = True
    TabOrder = 4
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 208
    Top = 200
    Width = 65
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 5
    OnClick = btnCancelClick
  end
  object chkSpecifyIP: TCheckBox
    Left = 120
    Top = 144
    Width = 153
    Height = 17
    Caption = 'Join at this address:'
    TabOrder = 2
    OnClick = chkSpecifyIPClick
  end
  object edtName: TEdit
    Left = 8
    Top = 164
    Width = 105
    Height = 21
    TabOrder = 1
  end
  object edtIPaddress: TEdit
    Left = 120
    Top = 164
    Width = 153
    Height = 21
    Enabled = False
    TabOrder = 3
    Text = '0.0.0.0'
  end
end
