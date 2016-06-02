object frmNewGame: TfrmNewGame
  Left = 258
  Top = 109
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Create a new game'
  ClientHeight = 233
  ClientWidth = 241
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
  object lblGridSize: TLabel
    Left = 8
    Top = 49
    Width = 73
    Height = 13
    Caption = 'Size of the grid:'
  end
  object lblColors: TLabel
    Left = 128
    Top = 49
    Width = 83
    Height = 13
    Caption = 'Number of colors:'
  end
  object lblPlayers: TLabel
    Left = 128
    Top = 9
    Width = 88
    Height = 13
    Caption = 'Number of players:'
  end
  object lblGameType: TLabel
    Left = 8
    Top = 9
    Width = 68
    Height = 13
    Caption = 'Type of game:'
  end
  object lblPlayer1: TLabel
    Left = 8
    Top = 109
    Width = 70
    Height = 13
    Caption = 'Player 1 name:'
  end
  object lblPlayer2: TLabel
    Left = 128
    Top = 109
    Width = 70
    Height = 13
    Caption = 'Player 2 name:'
    Enabled = False
  end
  object lblPlayer3: TLabel
    Left = 8
    Top = 149
    Width = 70
    Height = 13
    Caption = 'Player 3 name:'
    Enabled = False
  end
  object lblPlayer4: TLabel
    Left = 128
    Top = 149
    Width = 70
    Height = 13
    Caption = 'Player 4 name:'
    Enabled = False
  end
  object Bevel1: TBevel
    Left = 8
    Top = 88
    Width = 225
    Height = 10
    Shape = bsBottomLine
  end
  object cmbGridSize: TComboBox
    Left = 8
    Top = 64
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 5
    TabOrder = 2
    Text = '6x6'
    OnChange = cmbGridSizeChange
    Items.Strings = (
      '21x21'
      '18x18'
      '15x15'
      '12x12'
      '9x9'
      '6x6')
  end
  object cmbColors: TComboBox
    Left = 128
    Top = 64
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 3
    Text = '4'
    Items.Strings = (
      '4'
      '5'
      '6'
      '7'
      '8')
  end
  object btnOk: TButton
    Left = 96
    Top = 200
    Width = 65
    Height = 25
    Caption = 'Ok'
    Default = True
    TabOrder = 8
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 168
    Top = 200
    Width = 65
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    TabOrder = 9
    OnClick = btnCancelClick
  end
  object cmbPlayers: TComboBox
    Left = 128
    Top = 24
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 1
    Text = '1 player'
    OnChange = cmbPlayersChange
    Items.Strings = (
      '1 player'
      '2 players'
      '3 players'
      '4 players')
  end
  object edtPlayer1: TEdit
    Left = 8
    Top = 124
    Width = 105
    Height = 21
    TabOrder = 4
  end
  object edtPlayer2: TEdit
    Left = 128
    Top = 124
    Width = 105
    Height = 21
    Enabled = False
    TabOrder = 5
  end
  object edtPlayer4: TEdit
    Left = 128
    Top = 164
    Width = 105
    Height = 21
    Enabled = False
    TabOrder = 7
  end
  object edtPlayer3: TEdit
    Left = 8
    Top = 164
    Width = 105
    Height = 21
    Enabled = False
    TabOrder = 6
  end
  object cmbGameType: TComboBox
    Left = 8
    Top = 24
    Width = 105
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = 'Local game'
    OnChange = cmbGameTypeChange
    Items.Strings = (
      'Local game'
      'Online game')
  end
end
