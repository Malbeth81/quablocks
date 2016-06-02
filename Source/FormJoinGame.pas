(* QuaBlocks *******************************************************)
(* Copyright (c)2005-2006 Marc-Andre Lamothe ***********************)
(* All rights reserved *********************************************)

unit FormJoinGame;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  ExtCtrls, Grids, WinUtils, StrUtils2, UnitMessages;

type
  TfrmJoinGame = class(TForm)
    stgGames: TStringGrid;
    lblGames: TLabel;
    lblName: TLabel;
    edtName: TEdit;
    chkSpecifyIP: TCheckBox;
    edtIPaddress: TEdit;
    btnOk: TButton;
    btnCancel: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);      
    procedure chkSpecifyIPClick(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure DisplayServers;
  private
  public
    GameServer,
    Name,
    HostAddress : string;
  end;

var
  frmJoinGame: TfrmJoinGame;

implementation

{$R *.dfm}
           
procedure TfrmJoinGame.FormCreate(Sender: TObject);
begin
  stgGames.Cells[0,0] := 'Host name';
  stgGames.Cells[1,0] := 'IP address';
end;

procedure TfrmJoinGame.FormShow(Sender: TObject);
begin
  DisplayServers;
  edtName.Text := Name;
  edtIPAddress.Text := HostAddress;
end;

procedure TfrmJoinGame.chkSpecifyIPClick(Sender: TObject);
begin
  edtIPAddress.Enabled := chkSpecifyIP.Checked;
end;

procedure TfrmJoinGame.btnOkClick(Sender: TObject);
begin
  if edtName.Text = '' then
    ShowError(4)
  else
  begin
    Name := edtName.Text;
    if chkSpecifyIP.Checked then
    begin
      if edtIPAddress.Text = '' then
        ShowError(5)
      else
      begin
        HostAddress := edtIPAddress.Text;  
        ModalResult := mrOk;
      end;
    end
    else
    begin
      HostAddress := stgGames.Cells[1, stgGames.Row];
      ModalResult := mrOk;
    end;
  end;
end;

procedure TfrmJoinGame.btnCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;
   
procedure TfrmJoinGame.DisplayServers;
var
  Content : string;
  List : TStringList;
  i, j : Integer;
begin
  if GetHTTPFile(Application.Title, GameServer + '/quablocks.inf', Content) then
  begin
    List := TStringList.Create;
    StringToList(Content, List);
    if List.Count = 0 then
    begin
      stgGames.RowCount := 2;
      stgGames.Rows[1].Clear;
    end
    else
      stgGames.RowCount := List.Count+1;
    for i := 0 to List.Count-1 do
    begin
      j := System.Pos('|', List[i]);
      stgGames.Cells[0, i+1] := Copy(List[i], 1, j-1);
      stgGames.Cells[1, i+1] := Copy(List[i], j+1, Length(List[i])-j);
    end;
    List.Free;
  end
  else
    ShowError(6);
end;

end.
