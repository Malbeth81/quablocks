(* QuaBlocks *******************************************************)
(* Copyright (c)2005-2006 Marc-Andre Lamothe ***********************)
(* All rights reserved *********************************************)

unit FormNewGame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, UnitMessages;

type
  TfrmNewGame = class(TForm)
    lblGameType: TLabel;
    cmbGameType: TComboBox;
    lblPlayers: TLabel;
    cmbPlayers: TComboBox;
    cmbGridSize: TComboBox;
    lblGridSize: TLabel;
    cmbColors: TComboBox;
    lblColors: TLabel;
    Bevel1: TBevel;
    lblPlayer1: TLabel;
    lblPlayer2: TLabel;
    lblPlayer3: TLabel;
    lblPlayer4: TLabel;
    edtPlayer1: TEdit;
    edtPlayer2: TEdit;
    edtPlayer4: TEdit;
    edtPlayer3: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure cmbPlayersChange(Sender: TObject);
    procedure cmbGridSizeChange(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure cmbGameTypeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
  public               
    Texts : TStringList;
    HostGame : Boolean;
    Players,
    GridSize,
    Colors : Word;
    Names : TStrings;
  end;

var
  frmNewGame: TfrmNewGame;

implementation

{$R *.dfm}
                   
procedure TfrmNewGame.FormCreate(Sender: TObject);
begin
  Names := TStringList.Create;
  Texts := TStringList.Create;
  Texts.Assign(cmbPlayers.Items);
end;

procedure TfrmNewGame.FormDestroy(Sender: TObject);
begin
  Names.Free;   
  Texts.Free;
end;
        
procedure TfrmNewGame.FormShow(Sender: TObject);
begin
  if Names.Count > 0 then
    edtPlayer1.Text := Names[0];   
  if Names.Count > 1 then
    edtPlayer2.Text := Names[1];  
  if Names.Count > 2 then
    edtPlayer3.Text := Names[2]; 
  if Names.Count > 3 then
    edtPlayer4.Text := Names[3];
end;

procedure TfrmNewGame.cmbGameTypeChange(Sender: TObject);
begin
  if cmbGameType.ItemIndex > 0 then
  begin
    cmbPlayers.Clear;
    cmbPlayers.Items.Add(Texts[1]);
    cmbPlayers.Items.Add(Texts[2]);
    cmbPlayers.Items.Add(Texts[3]);
    cmbPlayers.ItemIndex := 0;
    edtPlayer1.Enabled := True;
    edtPlayer2.Enabled := False;
    edtPlayer3.Enabled := False;
    edtPlayer4.Enabled := False;
    lblPlayer1.Enabled := edtPlayer1.Enabled;
    lblPlayer2.Enabled := edtPlayer2.Enabled;
    lblPlayer3.Enabled := edtPlayer3.Enabled;
    lblPlayer4.Enabled := edtPlayer4.Enabled;
  end
  else
  begin
    cmbPlayers.Clear;   
    cmbPlayers.Items.Add(Texts[0]);
    cmbPlayers.Items.Add(Texts[1]);
    cmbPlayers.Items.Add(Texts[2]);
    cmbPlayers.Items.Add(Texts[3]);
    cmbPlayers.ItemIndex := 0;
  end;
  cmbPlayers.OnChange(Self);
end;

procedure TfrmNewGame.cmbPlayersChange(Sender: TObject);
begin
  cmbGridSize.Clear;
  cmbGridSize.Items.Add('21x21');
  cmbGridSize.Items.Add('18x18');
  cmbGridSize.Items.Add('15x15');
  cmbGridSize.Items.Add('12x12');
  if cmbPlayers.ItemIndex < 3-cmbGameType.ItemIndex then
    cmbGridSize.Items.Add('9x9');
  if cmbPlayers.ItemIndex < 1-cmbGameType.ItemIndex then
    cmbGridSize.Items.Add('6x6');
  cmbGridSize.ItemIndex := cmbGridSize.Items.Count-1;
  cmbGridSize.OnChange(Self);
  if cmbGameType.ItemIndex = 0 then
  begin
    if cmbPlayers.ItemIndex > 0 then edtPlayer2.Enabled := True else edtPlayer2.Enabled := False;
    if cmbPlayers.ItemIndex > 1 then edtPlayer3.Enabled := True else edtPlayer3.Enabled := False;
    if cmbPlayers.ItemIndex > 2 then edtPlayer4.Enabled := True else edtPlayer4.Enabled := False;
    lblPlayer1.Enabled := edtPlayer1.Enabled;
    lblPlayer2.Enabled := edtPlayer2.Enabled;
    lblPlayer3.Enabled := edtPlayer3.Enabled;
    lblPlayer4.Enabled := edtPlayer4.Enabled;
  end;
end;
    
procedure TfrmNewGame.cmbGridSizeChange(Sender: TObject);
begin
  cmbColors.Clear;
  cmbColors.Items.Add('4');
  cmbColors.Items.Add('5');
  if cmbGridSize.ItemIndex < 5 then
    cmbColors.Items.Add('6');
  if cmbGridSize.ItemIndex < 4 then
  begin
    cmbColors.Items.Add('7');
    cmbColors.Items.Add('8');
  end;
  cmbColors.ItemIndex := cmbColors.Items.Count-1;
end;

procedure TfrmNewGame.btnOkClick(Sender: TObject);
begin
  if (edtPlayer1.Text = '') or (edtPlayer2.Enabled and (edtPlayer2.Text = '')) or (edtPlayer3.Enabled and (edtPlayer3.Text = '')) or (edtPlayer4.Enabled and (edtPlayer4.Text = '')) then
    ShowError(4)
  else
  begin
    Players := cmbGameType.ItemIndex+cmbPlayers.ItemIndex+1;
    case cmbGridSize.ItemIndex of
      0: GridSize := 21;
      1: GridSize := 18;
      2: GridSize := 15;
      3: GridSize := 12;
      4: GridSize := 9;
      5: GridSize := 6;
    end;
    Colors := cmbColors.ItemIndex+4;
    HostGame := (cmbGameType.ItemIndex > 0);
    Names.Clear;
    Names.Add(edtPlayer1.Text);
    if edtPlayer2.Enabled then
      Names.Add(edtPlayer2.Text); 
    if edtPlayer3.Enabled then
      Names.Add(edtPlayer3.Text);
    if edtPlayer4.Enabled then
      Names.Add(edtPlayer4.Text);
    ModalResult := mrOk;
  end;
end;

procedure TfrmNewGame.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

end.
