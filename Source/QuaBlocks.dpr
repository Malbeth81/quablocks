program QuaBlocks;

uses
  Forms,
  FormMain in 'FormMain.pas' {frmMain},
  FormJoinGame in 'FormJoinGame.pas' {frmJoinGame},
  FormNewGame in 'FormNewGame.pas' {frmNewGame},
  FormOptions in 'FormOptions.pas' {frmOptions},
  UnitMessages in 'UnitMessages.pas',
  UnitQuaBlocks in 'UnitQuaBlocks.pas',
  UnitDissolves in 'UnitDissolves.pas',
  UnitScores in 'UnitScores.pas',
  FormChat in 'FormChat.pas' {frmChat};

{$R *.res}
{$R Ressources\WinXP.res}

begin
  Application.Initialize;
  Application.Title := 'QuaBlocks';
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmNewGame, frmNewGame);
  Application.CreateForm(TfrmOptions, frmOptions);
  Application.CreateForm(TfrmJoinGame, frmJoinGame);
  Application.CreateForm(TfrmChat, frmChat);
  Application.Run;
end.
