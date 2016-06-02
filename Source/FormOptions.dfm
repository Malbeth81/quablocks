object frmOptions: TfrmOptions
  Left = 228
  Top = 119
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Options'
  ClientHeight = 225
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object btnCancel: TButton
    Left = 328
    Top = 192
    Width = 65
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 2
    OnClick = btnCancelClick
  end
  object btnOk: TButton
    Left = 256
    Top = 192
    Width = 65
    Height = 25
    Caption = 'Ok'
    Default = True
    TabOrder = 1
    OnClick = btnOkClick
  end
  object pgcOptions: TPageControl
    Left = 8
    Top = 8
    Width = 385
    Height = 177
    ActivePage = tbsGeneral
    TabIndex = 0
    TabOrder = 0
    object tbsGeneral: TTabSheet
      Caption = 'General'
      object lblUpdateInfo: TLabel
        Left = 8
        Top = 24
        Width = 254
        Height = 13
        Caption = '(This option may slow down the opening of the game).'
      end
      object lblServer: TLabel
        Left = 8
        Top = 104
        Width = 63
        Height = 13
        Caption = 'Game server:'
      end
      object lblLanguages: TLabel
        Left = 8
        Top = 56
        Width = 51
        Height = 13
        Caption = 'Language:'
      end
      object lblPlayerColor: TLabel
        Left = 286
        Top = 77
        Width = 58
        Height = 13
        Alignment = taRightJustify
        Caption = 'Player color:'
      end
      object lblBorderColor: TLabel
        Left = 284
        Top = 101
        Width = 60
        Height = 13
        Alignment = taRightJustify
        Caption = 'Border color:'
      end
      object lblTextColor: TLabel
        Left = 294
        Top = 125
        Width = 50
        Height = 13
        Alignment = taRightJustify
        Caption = 'Text color:'
      end
      object lblBackColor: TLabel
        Left = 257
        Top = 53
        Width = 87
        Height = 13
        Alignment = taRightJustify
        Caption = 'Background color:'
      end
      object chkAutoUpdate: TCheckBox
        Left = 8
        Top = 4
        Width = 257
        Height = 17
        Caption = 'Check for new version on startup.'
        TabOrder = 0
      end
      object cmbGameServer: TComboBox
        Left = 8
        Top = 120
        Width = 185
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 2
        Text = 'http://www.marcandre.info'
        Items.Strings = (
          'http://www.marcandre.info')
      end
      object cmbLanguages: TComboBox
        Left = 8
        Top = 72
        Width = 121
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        TabOrder = 1
      end
      object pnlTextColor: TPanel
        Left = 348
        Top = 120
        Width = 21
        Height = 21
        BevelOuter = bvNone
        BorderStyle = bsSingle
        Color = clWhite
        TabOrder = 6
        OnClick = pnlColorClick
      end
      object pnlBorderColor: TPanel
        Left = 348
        Top = 96
        Width = 21
        Height = 21
        BevelOuter = bvNone
        BorderStyle = bsSingle
        Color = clWhite
        TabOrder = 5
        OnClick = pnlColorClick
      end
      object pnlPlayerColor: TPanel
        Left = 348
        Top = 72
        Width = 21
        Height = 21
        BevelOuter = bvNone
        BorderStyle = bsSingle
        Color = clWhite
        TabOrder = 4
        OnClick = pnlColorClick
      end
      object pnlBackColor: TPanel
        Left = 348
        Top = 48
        Width = 21
        Height = 21
        BevelOuter = bvNone
        BorderStyle = bsSingle
        Color = clWhite
        TabOrder = 3
        OnClick = pnlColorClick
      end
    end
    object tbsControls: TTabSheet
      Caption = 'Controls'
      ImageIndex = 2
      object lblControls: TLabel
        Left = 8
        Top = 4
        Width = 82
        Height = 13
        Caption = 'Enabled controls:'
      end
      object lblLeft: TLabel
        Left = 237
        Top = 12
        Width = 47
        Height = 13
        Alignment = taRightJustify
        Caption = 'Move left:'
      end
      object lblRight: TLabel
        Left = 231
        Top = 40
        Width = 53
        Height = 13
        Alignment = taRightJustify
        Caption = 'Move right:'
      end
      object lblUp: TLabel
        Left = 234
        Top = 68
        Width = 50
        Height = 13
        Alignment = taRightJustify
        Caption = 'Rotate up:'
      end
      object lblDown: TLabel
        Left = 220
        Top = 96
        Width = 64
        Height = 13
        Alignment = taRightJustify
        Caption = 'Rotate down:'
      end
      object lblDrop: TLabel
        Left = 258
        Top = 124
        Width = 26
        Height = 13
        Alignment = taRightJustify
        Caption = 'Drop:'
      end
      object cmbControls: TComboBox
        Left = 8
        Top = 20
        Width = 161
        Height = 21
        Style = csDropDownList
        ItemHeight = 13
        ItemIndex = 0
        TabOrder = 0
        Text = 'Keyboard and mouse'
        Items.Strings = (
          'Keyboard and mouse'
          'Mouse only'
          'Keyboard only')
      end
      object Panel1: TPanel
        Left = 8
        Top = 48
        Width = 161
        Height = 93
        BevelOuter = bvLowered
        BorderWidth = 2
        TabOrder = 6
        object lblMouseInfo: TLabel
          Left = 3
          Top = 3
          Width = 155
          Height = 87
          Align = alClient
          AutoSize = False
          Caption = 
            'When enabling mouse controls, move the mouse to change column, c' +
            'lick with left button to drop, right button or wheel up/down to ' +
            'rotate colors.'
          WordWrap = True
        end
      end
      object edtLeft: TEdit
        Left = 288
        Top = 8
        Width = 81
        Height = 21
        ReadOnly = True
        TabOrder = 1
        OnDblClick = edtControlDblClick
        OnEnter = edtControlEnter
        OnExit = edtControlExit
        OnKeyUp = edtControlKeyUp
      end
      object edtRight: TEdit
        Left = 288
        Top = 36
        Width = 81
        Height = 21
        ReadOnly = True
        TabOrder = 2
        OnDblClick = edtControlDblClick
        OnEnter = edtControlEnter
        OnExit = edtControlExit
        OnKeyUp = edtControlKeyUp
      end
      object edtUp: TEdit
        Left = 288
        Top = 64
        Width = 81
        Height = 21
        ReadOnly = True
        TabOrder = 3
        OnDblClick = edtControlDblClick
        OnEnter = edtControlEnter
        OnExit = edtControlExit
        OnKeyUp = edtControlKeyUp
      end
      object edtDown: TEdit
        Left = 288
        Top = 92
        Width = 81
        Height = 21
        ReadOnly = True
        TabOrder = 4
        OnDblClick = edtControlDblClick
        OnEnter = edtControlEnter
        OnExit = edtControlExit
        OnKeyUp = edtControlKeyUp
      end
      object edtDrop: TEdit
        Left = 288
        Top = 120
        Width = 81
        Height = 21
        ReadOnly = True
        TabOrder = 5
        OnDblClick = edtControlDblClick
        OnEnter = edtControlEnter
        OnExit = edtControlExit
        OnKeyUp = edtControlKeyUp
      end
    end
  end
  object ColorDialog: TColorDialog
    Ctl3D = True
    Options = [cdFullOpen]
    Left = 8
    Top = 192
  end
end
