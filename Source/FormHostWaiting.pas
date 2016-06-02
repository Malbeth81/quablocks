unit FormHostWaiting;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, UnitWinUtils, UnitLanguagePack;

type
  TfrmHostWaiting = class(TForm)
    procedure FormShow(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
  private
    Section : TLanguageSection;
  public
    GameServer : string;
  end;

var
  frmHostWaiting: TfrmHostWaiting;

implementation

uses FormMain;

{$R *.dfm}

procedure TfrmHostWaiting.FormShow(Sender: TObject);
var
  Address : string;
begin
  lblWaiting.Caption := Section.GetValue(0);
  lblIPAddress.Caption := Section.GetValue(1);
  lstIpAddresses.Clear;
  lstIpAddresses.Items.Add(GetLocalIPAddress);
  Address := GetRemoteIPAddress(GameServer);
  if Address <> lstIpAddresses.Items[0] then
    lstIpAddresses.Items.Add(Address);
end;

procedure TfrmHostWaiting.btnCancelClick(Sender: TObject);
begin
  frmMain.Enabled := True;
  frmHostWaiting.Hide;         
end;

end.
