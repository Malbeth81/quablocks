object frmMain: TfrmMain
  Left = 221
  Top = 108
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  BorderWidth = 2
  Caption = 'QuaBlocks'
  ClientHeight = 243
  ClientWidth = 420
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  KeyPreview = True
  Menu = Menu
  OldCreateOrder = False
  OnClose = FormClose
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseWheelDown = FormMouseWheelDown
  OnMouseWheelUp = FormMouseWheelUp
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object pnlBoard: TPanel
    Left = 0
    Top = 0
    Width = 420
    Height = 243
    Align = alClient
    BevelOuter = bvNone
    BorderStyle = bsSingle
    Color = clWindow
    TabOrder = 1
    object PaintBox: TPaintBox
      Left = 0
      Top = 0
      Width = 416
      Height = 239
      Align = alClient
      ParentColor = False
      OnMouseMove = PaintBoxMouseMove
      OnMouseUp = PaintBoxMouseUp
    end
    object lblBlock: TLabel
      Left = 8
      Top = 8
      Width = 66
      Height = 13
      Caption = 'Current block:'
      Visible = False
    end
    object lblTime: TLabel
      Left = 8
      Top = 24
      Width = 26
      Height = 13
      Caption = 'Time:'
      Visible = False
    end
    object lblScore: TLabel
      Left = 8
      Top = 40
      Width = 31
      Height = 13
      Caption = 'Score:'
      Visible = False
    end
    object lblReady: TLabel
      Left = 8
      Top = 56
      Width = 31
      Height = 13
      Caption = 'Ready'
      Visible = False
    end
    object lblNotReady: TLabel
      Left = 8
      Top = 72
      Width = 46
      Height = 13
      Caption = 'Not ready'
      Visible = False
    end
  end
  object pnlHostWaiting: TPanel
    Left = 128
    Top = 80
    Width = 257
    Height = 105
    BevelInner = bvRaised
    BevelOuter = bvLowered
    ParentColor = True
    TabOrder = 0
    Visible = False
    object lblWaiting: TLabel
      Left = 8
      Top = 8
      Width = 241
      Height = 15
      Alignment = taCenter
      AutoSize = False
      Caption = 'Waiting for players...'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object lblIPAddress: TLabel
      Left = 8
      Top = 28
      Width = 241
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Your IP addresses are:'
    end
    object lstIpAddresses: TListBox
      Left = 60
      Top = 48
      Width = 137
      Height = 49
      ItemHeight = 13
      TabOrder = 0
    end
  end
  object pnlReady: TPanel
    Left = 128
    Top = 120
    Width = 257
    Height = 65
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 2
    Visible = False
    object lblImReady: TLabel
      Left = 8
      Top = 8
      Width = 241
      Height = 13
      Alignment = taCenter
      AutoSize = False
      Caption = 'Click the button when ready...'
    end
    object btnReady: TButton
      Left = 76
      Top = 32
      Width = 105
      Height = 25
      Caption = 'I'#39'm ready!'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
      TabOrder = 0
      OnClick = btnReadyClick
    end
  end
  object Menu: TMainMenu
    Left = 8
    Top = 88
    object Menu_Game: TMenuItem
      Caption = '&Game'
      object Menu_Game_New: TMenuItem
        Caption = '&Start a new game...'
        ShortCut = 16455
        OnClick = Menu_Game_NewClick
      end
      object Menu_Game_Join: TMenuItem
        Caption = '&Join a game...'
        ShortCut = 16458
        OnClick = Menu_Game_JoinClick
      end
      object Menu_Game_N1: TMenuItem
        Caption = '-'
      end
      object Menu_Game_Pause: TMenuItem
        Caption = '&Pause game'
        ShortCut = 16464
        OnClick = Menu_Game_PauseClick
      end
      object Menu_Game_Resume: TMenuItem
        Caption = '&Resume game'
        ShortCut = 16464
        Visible = False
        OnClick = Menu_Game_ResumeClick
      end
      object Menu_Game_N2: TMenuItem
        Caption = '-'
      end
      object Menu_Game_Quit: TMenuItem
        Caption = '&Quit'
        ShortCut = 16465
        OnClick = Menu_Game_QuitClick
      end
    end
    object Menu_View: TMenuItem
      Caption = '&View'
      object Menu_View_Chat: TMenuItem
        Caption = '&Chat window...'
        ShortCut = 117
        OnClick = Menu_View_ChatClick
      end
      object Menu_View_Options: TMenuItem
        Caption = '&Options...'
        ShortCut = 119
        OnClick = Menu_View_OptionsClick
      end
    end
    object Menu_Help: TMenuItem
      Caption = '&Help'
      object Menu_Help_Update: TMenuItem
        Caption = '&Check for new version'
        OnClick = Menu_Help_UpdateClick
      end
      object Menu_Help_N1: TMenuItem
        Caption = '-'
      end
      object Menu_Help_About: TMenuItem
        Caption = '&About...'
        OnClick = Menu_Help_AboutClick
      end
    end
  end
  object Timer: TTimer
    Interval = 30
    OnTimer = TimerTimer
    Left = 40
    Top = 88
  end
end
