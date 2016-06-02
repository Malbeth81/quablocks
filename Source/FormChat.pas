(* QuaBlocks *******************************************************)
(* Copyright (c)2005-2006 Marc-Andre Lamothe ***********************)
(* All rights reserved *********************************************)

unit FormChat;

interface

uses
  Windows, SysUtils, Messages, Classes, Controls, Forms, StdCtrls, ExtCtrls;

type   
  TTextEvent = procedure(Sender : TObject; Text : string) of object;
  TfrmChat = class(TForm)
    memChat: TMemo;
    pnlBottom: TPanel;
    edtMessage: TEdit;
    btnSend: TButton;
    procedure FormShow(Sender: TObject);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    procedure FormResize(Sender: TObject);
    procedure btnSendClick(Sender: TObject);
  private
  public
    procedure Enable;
    procedure Disable;
    procedure DisplayText(Text : String);
    procedure DisplayMessage(From, Text : String);
    procedure Clear;
  end;

var
  frmChat: TfrmChat;

implementation

uses FormMain;

{$R *.dfm}

procedure TfrmChat.Enable;
begin
  edtMessage.Enabled := True;
  btnSend.Enabled := True;
end;

procedure TfrmChat.Disable;
begin
  edtMessage.Text := '';
  edtMessage.Enabled := False;
  btnSend.Enabled := False;
end;
                         
procedure TfrmChat.DisplayText(Text : String);
begin
  if memChat.Lines[memChat.Lines.Count-1] = '' then
    memChat.Lines[memChat.Lines.Count-1] := Text
  else
    memChat.Lines.Add(Text);
end;

procedure TfrmChat.DisplayMessage(From, Text : String);
begin
  DisplayText(From + ' : ' + Text);
end;

procedure TfrmChat.Clear;
begin
  memChat.Lines.Clear;
end;

procedure TfrmChat.FormShow(Sender: TObject);
begin
  if edtMessage.Enabled then
    edtMessage.SetFocus;
end;
   
procedure TfrmChat.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
begin
  if Msg.CharCode = VK_ESCAPE  then
  begin
    frmChat.Close;
    Handled := True;
  end;
end;
      
procedure TfrmChat.FormResize(Sender: TObject);
begin
  btnSend.Left := pnlBottom.Width-74;
  edtMessage.Width := pnlBottom.Width-80;
end;

procedure TfrmChat.btnSendClick(Sender: TObject);
begin
  if edtMessage.Text = '' then
    Exit;
  frmMain.Game.SendText(edtMessage.Text);
  DisplayMessage(frmMain.Game.GetPlayer(frmMain.Game.PlayerNumber).Name, edtMessage.Text);
  edtMessage.Text := '';
end;

end.
