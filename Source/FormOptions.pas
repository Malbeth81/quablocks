(* QuaBlocks *******************************************************)
(* Copyright (c)2005-2006 Marc-Andre Lamothe ***********************)
(* All rights reserved *********************************************)

unit FormOptions;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, StrUtils2, LanguagePack, ComCtrls;

type
  TfrmOptions = class(TForm)
    btnCancel: TButton;
    btnOk: TButton;
    ColorDialog: TColorDialog;
    pgcOptions: TPageControl;
    tbsGeneral: TTabSheet;
    tbsControls: TTabSheet;
    chkAutoUpdate: TCheckBox;
    lblUpdateInfo: TLabel;
    lblServer: TLabel;
    cmbGameServer: TComboBox;
    lblControls: TLabel;
    cmbControls: TComboBox;
    Panel1: TPanel;
    lblMouseInfo: TLabel;
    lblLeft: TLabel;
    edtLeft: TEdit;
    lblRight: TLabel;
    edtRight: TEdit;
    lblUp: TLabel;
    edtUp: TEdit;
    lblDown: TLabel;
    edtDown: TEdit;
    lblDrop: TLabel;
    edtDrop: TEdit;
    lblLanguages: TLabel;
    cmbLanguages: TComboBox;
    lblPlayerColor: TLabel;
    lblBorderColor: TLabel;
    lblTextColor: TLabel;
    pnlTextColor: TPanel;
    pnlBorderColor: TPanel;
    pnlPlayerColor: TPanel;
    lblBackColor: TLabel;
    pnlBackColor: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure pnlColorClick(Sender: TObject);
    procedure edtControlEnter(Sender: TObject);
    procedure edtControlExit(Sender: TObject);
    procedure edtControlDblClick(Sender: TObject);
    procedure edtControlKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    function GetKeyName(Key : Word) : string;
    procedure ListLanguagePacks;
  private
    LanguageFiles : TStrings;
  public
    Texts : TStringList;
    GameServer,
    LanguageFile : string;
    AutoUpdate : Boolean;
    BackColor,
    BorderColor,
    PlayerColor,
    TextColor : TColor;
    UseMouse,
    UseKeyboard : Boolean;
    ControlLeft,
    ControlRight,
    ControlUp,
    ControlDown,
    ControlDrop : Word;
  end;

var
  frmOptions: TfrmOptions;

implementation

{$R *.dfm}

procedure TfrmOptions.FormCreate(Sender: TObject);
begin
  LanguageFiles := TStringList.Create; 
  Texts := TStringList.Create;
  Texts.Add('Key00=Press a key...');
  Texts.Add('Key01=Up');
  Texts.Add('Key02=Down');
  Texts.Add('Key03=Left');
  Texts.Add('Key04=Right');
  Texts.Add('Key05=Space');
  Texts.Add('Key06=Numpad 0');
  Texts.Add('Key07=Numpad 1');
  Texts.Add('Key08=Numpad 2');
  Texts.Add('Key09=Numpad 3');
  Texts.Add('Key10=Numpad 4');
  Texts.Add('Key11=Numpad 5');
  Texts.Add('Key12=Numpad 6');
  Texts.Add('Key13=Numpad 7');
  Texts.Add('Key14=Numpad 8');
  Texts.Add('Key15=Numpad 9');
  Texts.Add('Key16=Insert');
  Texts.Add('Key17=Delete');
  Texts.Add('Key18=Home');
  Texts.Add('Key19=End');
  Texts.Add('Key20=Page Up');
  Texts.Add('Key21=Page Down');
  Texts.Add('Key22=Plus');
  Texts.Add('Key23=Minus');
  Texts.Add('Key24=Multiply');
  Texts.Add('Key25=Divide');
  pgcOptions.ActivePageIndex := 0; 
  pgcOptions.SetFocus;
end;
     
procedure TfrmOptions.FormDestroy(Sender: TObject);
begin
  LanguageFiles.Free;
  Texts.Free;
end;

procedure TfrmOptions.FormShow(Sender: TObject);
begin
  ListLanguagePacks;
  cmbGameServer.ItemIndex := cmbGameServer.Items.IndexOf(GameServer);
  cmbLanguages.ItemIndex := LanguageFiles.IndexOf(LanguageFile);
  chkAutoUpdate.Checked := AutoUpdate;  
  pnlBackColor.Color := BackColor;
  pnlBorderColor.Color := BorderColor;
  pnlPlayerColor.Color := PlayerColor;
  pnlTextColor.Color := TextColor;
  if UseMouse and UseKeyboard then
    cmbControls.ItemIndex := 0
  else if UseMouse then
    cmbControls.ItemIndex := 1  
  else
    cmbControls.ItemIndex := 2;
  edtUp.Text := GetKeyName(ControlUp);
  edtDown.Text := GetKeyName(ControlDown);
  edtLeft.Text := GetKeyName(ControlLeft);
  edtRight.Text := GetKeyName(ControlRight);
  edtDrop.Text := GetKeyName(ControlDrop);
  edtUp.Tag := ControlUp;
  edtDown.Tag := ControlDown;
  edtLeft.Tag := ControlLeft;
  edtRight.Tag := ControlRight;
  edtDrop.Tag := ControlDrop;
end;
     
procedure TfrmOptions.pnlColorClick(Sender: TObject);
begin
  if ColorDialog.Execute then
    TPanel(Sender).Color := ColorDialog.Color;
end;

procedure TfrmOptions.edtControlEnter(Sender: TObject);
begin            
  TLabeledEdit(Sender).Tag := 0;
  TLabeledEdit(Sender).Text := Texts.Values['Key00'];
end;

procedure TfrmOptions.edtControlExit(Sender: TObject);
begin
  if TLabeledEdit(Sender).Tag = 0 then
    TLabeledEdit(Sender).SetFocus;
end;
   
procedure TfrmOptions.edtControlDblClick(Sender: TObject);
begin
  TLabeledEdit(Sender).OnEnter(Sender);
end;

procedure TfrmOptions.edtControlKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  Name : string;
begin
  if TLabeledEdit(Sender).Tag = 0 then
  begin
    Name := GetKeyName(Key);
    if Name <> '' then
    begin
      TLabeledEdit(Sender).Text := Name;
      TLabeledEdit(Sender).Tag := Key;
    end;
  end;
end;

procedure TfrmOptions.btnOkClick(Sender: TObject);
begin
  if cmbLanguages.ItemIndex >= 0 then
    LanguageFile := LanguageFiles[cmbLanguages.ItemIndex];
  GameServer := cmbGameServer.Text;
  AutoUpdate := chkAutoUpdate.Checked; 
  BackColor := pnlBackColor.Color;
  BorderColor := pnlBorderColor.Color;
  PlayerColor := pnlPlayerColor.Color;
  TextColor := pnlTextColor.Color;
  UseMouse := (cmbControls.ItemIndex < 2);
  UseKeyboard := (cmbControls.ItemIndex in [0,2]);
  ControlLeft := edtLeft.Tag;
  ControlRight := edtRight.Tag;
  ControlUp := edtUp.Tag;
  ControlDown := edtDown.Tag;
  ControlDrop := edtDrop.Tag;
  ModalResult := mrOk;
end;

procedure TfrmOptions.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

function TfrmOptions.GetKeyName(Key : Word) : string;
begin
  case Key of
    65..90: Result := Char(Key);
    VK_UP: Result := Texts.Values['Key01'];
    VK_DOWN: Result := Texts.Values['Key02'];
    VK_LEFT: Result := Texts.Values['Key03'];
    VK_RIGHT: Result := Texts.Values['Key04'];
    VK_SPACE: Result := Texts.Values['Key05'];
    VK_NUMPAD0: Result := Texts.Values['Key06'];
    VK_NUMPAD1: Result := Texts.Values['Key07'];
    VK_NUMPAD2: Result := Texts.Values['Key08'];
    VK_NUMPAD3: Result := Texts.Values['Key09'];
    VK_NUMPAD4: Result := Texts.Values['Key10'];
    VK_NUMPAD5: Result := Texts.Values['Key11'];
    VK_NUMPAD6: Result := Texts.Values['Key12'];
    VK_NUMPAD7: Result := Texts.Values['Key13'];
    VK_NUMPAD8: Result := Texts.Values['Key14'];
    VK_NUMPAD9: Result := Texts.Values['Key15'];
    VK_INSERT: Result := Texts.Values['Key16'];  
    VK_DELETE: Result := Texts.Values['Key17'];
    VK_HOME: Result := Texts.Values['Key18'];
    VK_END: Result := Texts.Values['Key19'];
    VK_NEXT: Result := Texts.Values['Key20'];
    VK_PRIOR: Result := Texts.Values['Key21'];   
    VK_ADD: Result := Texts.Values['Key22'];
    VK_SUBTRACT: Result := Texts.Values['Key23'];
    VK_MULTIPLY: Result := Texts.Values['Key24'];
    VK_DIVIDE: Result := Texts.Values['Key25'];
    else Result := '';
  end;
end;
  
procedure TfrmOptions.ListLanguagePacks;
var
  FileInfo : TSearchRec;
  LangPack : TLangPack;
begin
  cmbLanguages.Items.Clear;
  LanguageFiles.Clear;
  LangPack := TLangPack.Create;
  if FindFirst(ExtractFilePath(Application.ExeName) + '*.lang', faAnyFile, FileInfo) = 0 then
  begin
    repeat
    begin
      if (FileInfo.Attr and faDirectory) = 0 then
        if LangPack.LoadHeaderFromFile(ExtractFilePath(Application.ExeName) + FileInfo.Name) then
        begin
          cmbLanguages.Items.Add(LangPack.Name);
          LanguageFiles.Add(FileInfo.Name);
        end;
    end;
    until FindNext(FileInfo) <> 0;
    SysUtils.FindClose(FileInfo);
  end;
  LangPack.Free;
end;

end.
