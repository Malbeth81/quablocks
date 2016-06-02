object frmChat: TfrmChat
  Left = 485
  Top = 157
  Width = 336
  Height = 115
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSizeToolWin
  BorderWidth = 4
  Caption = 'Chat'
  Color = clBtnFace
  Constraints.MinHeight = 115
  Constraints.MinWidth = 336
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  OnResize = FormResize
  OnShortCut = FormShortCut
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object memChat: TMemo
    Left = 0
    Top = 0
    Width = 320
    Height = 51
    TabStop = False
    Align = alClient
    BevelKind = bkFlat
    BorderStyle = bsNone
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 51
    Width = 320
    Height = 29
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object edtMessage: TEdit
      Left = 0
      Top = 4
      Width = 240
      Height = 25
      BevelKind = bkFlat
      BorderStyle = bsNone
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 100
      ParentFont = False
      TabOrder = 0
    end
    object btnSend: TButton
      Left = 246
      Top = 4
      Width = 74
      Height = 25
      Caption = 'Send'
      Default = True
      Enabled = False
      TabOrder = 1
      OnClick = btnSendClick
    end
  end
end
