(* QuaBlocks *******************************************************)
(* Copyright (c)2005-2006 Marc-Andre Lamothe ***********************)
(* All rights reserved *********************************************)

unit FormMain;

interface

uses
  Windows, ShellAPI, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, Math, StdCtrls, Menus, IniFiles, WinUtils, DateUtils, Update, LanguagePack,
  UnitMessages, UnitQuaBlocks, UnitScores, UnitDissolves;

const
  sName = 'QuaBlocks';
  iVersion = 200;
  sVersion = '2.0.0';
  sCopyright = 'Copyright (c)2005 Marc-André Lamothe.';
  sWebsite = 'http://www.marcandre.info';

type
  TfrmMain = class (TForm)
    Menu: TMainMenu;
    Menu_Game: TMenuItem;
    Menu_Game_New: TMenuItem;
    Menu_Game_N1: TMenuItem;
    Menu_Game_Quit: TMenuItem;
    Menu_View: TMenuItem;
    Menu_Help: TMenuItem;
    Menu_Help_About: TMenuItem;
    Menu_View_Options: TMenuItem;
    Menu_Help_Update: TMenuItem;
    Menu_Help_N1: TMenuItem;
    Menu_Game_Join: TMenuItem;
    pnlHostWaiting: TPanel;
    lblWaiting: TLabel;
    lblIPAddress: TLabel;
    lstIpAddresses: TListBox;
    pnlBoard: TPanel;
    PaintBox: TPaintBox;
    Menu_Game_Pause: TMenuItem;
    Menu_Game_Resume: TMenuItem;
    Menu_Game_N2: TMenuItem;
    pnlReady: TPanel;
    lblImReady: TLabel;
    btnReady: TButton;
    Menu_View_Chat: TMenuItem;
    Timer: TTimer;
    lblBlock: TLabel;
    lblTime: TLabel;
    lblScore: TLabel;
    lblReady: TLabel;
    lblNotReady: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure Menu_Game_NewClick(Sender: TObject);
    procedure Menu_Game_QuitClick(Sender: TObject);
    procedure Menu_View_OptionsClick(Sender: TObject);
    procedure Menu_Help_UpdateClick(Sender: TObject);
    procedure Menu_Help_AboutClick(Sender: TObject);
    procedure Menu_Game_JoinClick(Sender: TObject);
    procedure Menu_Game_PauseClick(Sender: TObject);
    procedure Menu_Game_ResumeClick(Sender: TObject);
    procedure btnReadyClick(Sender: TObject);
    procedure Menu_View_ChatClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  public
    Game : TQuaBlocks;
  private
    AutoUpdate : TUpdate;
    Digits,
    Tiles,
    Dissolve,
    Buffer : TBitmap;
                                        
    procedure ShowHostWaiting;
    procedure HideHostWaiting;
    procedure AddToServer(Host : string);
    procedure RemoveFromServer;
    function NeedUpdate : Boolean;
    procedure SocketError(Sender : TObject);
    procedure PlayerConnected(Sender : TObject);
    procedure PlayerDisconnected(Sender : TObject);
    procedure PlayerChat(Sender : TObject; From, Text : string);
    procedure GameStarted(Sender : TObject);
    procedure GameEnded(Sender : TObject);
    procedure DrawScore(Info : PScoreInfo);
    procedure DrawDissolve(Info : PDissolveInfo);
    procedure DrawTile(ToRect : TRect; Cube : TCubeColor; Col, Row : Integer);
    procedure DrawBoard;
    procedure TranslateTo(FileName : string);
    procedure LoadSettings;
    procedure SaveSettings;
    procedure LoadOptions;
    procedure SaveOptions;
  end;

var
  frmMain: TfrmMain;

implementation

uses FormNewGame, FormOptions, FormJoinGame, FormChat;

{$R *.dfm} 
{$R Ressources\Bitmaps.res}

procedure TfrmMain.FormCreate(Sender: TObject);
begin               {
  if not SingleInstance(Application.Title) then
    Application.Terminate;   }
  AutoUpdate := TUpdate.Create;
  Game := TQuaBlocks.Create;
  Game.OnSocketError := SocketError;
  Game.OnPlayerConnected := PlayerConnected;
  Game.OnPlayerDisconnected := PlayerDisconnected;
  Game.OnPlayerChat := PlayerChat;
  Game.OnGameStarted := GameStarted;
  Game.OnGameStopped := GameEnded;  
  Game.OnGamePaused := nil;
  Game.OnGameResumed := nil;
  Tiles := TBitmap.Create;
  Tiles.LoadFromResourceName(HInstance, 'Tiles');
  Digits := TBitmap.Create;
  Digits.LoadFromResourceName(HInstance, 'Digits');
  Dissolve := TBitmap.Create;
  Dissolve.LoadFromResourceName(HInstance, 'Dissolve');
  Buffer := TBitmap.Create;                  
end;

procedure TfrmMain.FormDestroy(Sender: TObject);
begin
  AutoUpdate.Free;
  Game.Free;
  Tiles.Free;
  Digits.Free;  
  Dissolve.Free;
  Buffer.Free;
end;
  
procedure TfrmMain.FormShow(Sender: TObject); 
begin
  LoadSettings;
  LoadOptions;         
  TranslateTo(frmOptions.LanguageFile);
  if frmOptions.AutoUpdate and NeedUpdate then
     if AskQuestion(1) = idYes then
       ShellExecute(Application.Handle, 'open', PChar(AutoUpdate.FileURL), nil, nil, SW_SHOW);
end;
              
procedure TfrmMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if Game.State <> gEnded then
    CanClose := (AskQuestion(2) = idYes);
end;

procedure TfrmMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Game.State <> gEnded then
    Game.Stop;      
  if Game.Mode = gHost then
    RemoveFromServer;
  SaveSettings;
end;

procedure TfrmMain.FormResize(Sender: TObject);
begin
  pnlHostWaiting.Left := (PaintBox.Width-pnlHostWaiting.Width+116) div 2;
  pnlHostWaiting.Top := (PaintBox.Height-pnlHostWaiting.Height) div 2; 
  pnlReady.Left := (PaintBox.Width-pnlReady.Width+116) div 2;
  pnlReady.Top := (PaintBox.Height-pnlReady.Height) div 2;
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if frmOptions.UseKeyboard and ((Game.Mode = gLocal) or (Game.CurPlayer = Game.PlayerNumber)) then
  begin
    if Key =	frmOptions.ControlUp then
      Game.RotateUp
    else if Key =	frmOptions.ControlDown then
      Game.RotateDown
    else if Key =	frmOptions.ControlLeft then
      Game.MoveLeft
    else if Key =	frmOptions.ControlRight then
      Game.MoveRight
    else if Key = frmOptions.ControlDrop then
      Game.Drop;
  end;
end;

procedure TfrmMain.FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if frmOptions.UseMouse and ((Game.Mode = gLocal) or (Game.CurPlayer = Game.PlayerNumber)) then
    Game.RotateDown;
end;

procedure TfrmMain.FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if frmOptions.UseMouse and ((Game.Mode = gLocal) or (Game.CurPlayer = Game.PlayerNumber)) then
    Game.RotateUp;
end;

procedure TfrmMain.PaintBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  CurPos : Integer;
begin
  if frmMain.Focused and frmOptions.UseMouse and
    ((Game.Mode = gLocal) or (Game.CurPlayer = Game.PlayerNumber)) then
    if (X > 120) and (X < 120+Game.GridSize*30) then
    begin
      CurPos := ((X-120)div 30);
      if CurPos < Game.CurCol then
        Game.MoveLeft
      else if CurPos > Game.CurCol then
        Game.MoveRight;
    end;
end;
   
procedure TfrmMain.PaintBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if frmOptions.UseMouse and ((Game.Mode = gLocal) or (Game.CurPlayer = Game.PlayerNumber)) then
    if Button = mbLeft then
      Game.Drop
    else if Button = mbRight then
      Game.RotateDown;
end;

procedure TfrmMain.TimerTimer(Sender: TObject);
begin
  DrawBoard;
end;

procedure TfrmMain.btnReadyClick(Sender: TObject);
begin
  Game.Ready;
  pnlReady.Visible := False;
end;

procedure TfrmMain.Menu_Game_NewClick(Sender: TObject);
var
  GameType : TGameMode;
begin
  if frmNewGame.ShowModal = mrOk then
  begin
    if frmNewGame.HostGame then
      GameType := gHost
    else
      GameType := gLocal;
    if Game.Mode = gHost then
    begin
      RemoveFromServer;
      HideHostWaiting;
    end;
    pnlReady.Visible := False;
    if Game.New(GameType, frmNewGame.Players, frmNewGame.Colors, frmNewGame.GridSize, frmNewGame.Names) then
      if Game.Mode = gLocal then
        Game.Start
      else
      begin
        AddToServer(Game.GetPlayer(0).Name);
        ShowHostWaiting;
      end;
  end;
end;

procedure TfrmMain.Menu_Game_JoinClick(Sender: TObject);
begin
  frmJoinGame.GameServer := frmOptions.GameServer;
  if frmJoinGame.ShowModal = mrOk then
  begin
    if Game.Join(frmJoinGame.Name, frmJoinGame.HostAddress) then
    begin
      pnlReady.Visible := True;
      frmChat.Enable;
    end
    else
      ShowError(1);   
  end;
end;

procedure TfrmMain.Menu_Game_PauseClick(Sender: TObject);
begin
  Game.Pause;   
  Menu_Game_Pause.Visible := False;
  Menu_Game_Resume.Visible := True;
end;

procedure TfrmMain.Menu_Game_ResumeClick(Sender: TObject);
begin
  Game.Resume;
  Menu_Game_Pause.Visible := True;
  Menu_Game_Resume.Visible := False;
end;

procedure TfrmMain.Menu_Game_QuitClick(Sender: TObject);
begin
  Close;
end;
    
procedure TfrmMain.Menu_View_ChatClick(Sender: TObject);
begin
  frmChat.Show;
end;

procedure TfrmMain.Menu_View_OptionsClick(Sender: TObject);
begin
  if frmOptions.ShowModal = mrOk then
  begin
    TranslateTo(frmOptions.LanguageFile);
    SaveOptions;
  end;
end;

procedure TfrmMain.Menu_Help_UpdateClick(Sender: TObject);
begin
  if not NeedUpdate then
    ShowMessage(1)
  else if AskQuestion(1) = idYes then
    ShellExecute(Application.Handle, 'open', PChar(AutoUpdate.FileURL), nil, nil, SW_SHOW);
end;

procedure TfrmMain.Menu_Help_AboutClick(Sender: TObject);
begin
  Application.MessageBox(sName + #13#10 + sVersion + #13#10#13#10 + sCopyright + #13#10 + sWebsite, 'About', mb_Ok + mb_IconInformation);
end;

procedure TfrmMain.ShowHostWaiting;
var
  Address : string;
begin                             
  lstIpAddresses.Clear;
  lstIpAddresses.Items.Add(GetLocalIPAddress);
  Address := GetRemoteIPAddress(frmOptions.GameServer);
  if Address <> lstIpAddresses.Items[0] then
    lstIpAddresses.Items.Add(Address);
  pnlHostWaiting.Visible := True;
end;

procedure TfrmMain.HideHostWaiting;
begin
  pnlHostWaiting.Visible := False;
end;

procedure TfrmMain.AddToServer(Host : string);
begin
  if not ExecuteHTTPFile(Application.Title, frmOptions.GameServer + '/quablocks.php?action=add&name=' + Host) then
    ShowError(3);
end;

procedure TfrmMain.RemoveFromServer;
begin
  if not ExecuteHTTPFile(Application.Title, frmOptions.GameServer + '/quablocks.php?action=remove') then
    ShowError(3);
end;

function TfrmMain.NeedUpdate() : Boolean;
begin
  Result := False;
  if AutoUpdate.Execute(sName, frmOptions.GameServer + '/versions.inf') then
    if AutoUpdate.FileVersion > iVersion then
      Result := True;
end;
  
procedure TfrmMain.PlayerConnected(Sender : TObject);
begin
  if (Game.Mode = gHost) and (Game.ClientCount = Game.PlayerCount-1) then
  begin
    HideHostWaiting;
    RemoveFromServer;  
    pnlReady.Visible := True;  
    frmChat.Enable;
  end;
end;

procedure TfrmMain.PlayerDisconnected(Sender : TObject);
begin
  pnlReady.Visible := False;
  if Game.Mode = gHost then
  begin
    ShowMessage(2);
    AddToServer(Game.GetPlayer(0).Name);
    ShowHostWaiting;
  end
  else
    ShowMessage(3);
end;
     
procedure TfrmMain.PlayerChat(Sender : TObject; From, Text : string);
begin
  if not frmChat.Visible then
    frmChat.Show;
  frmChat.DisplayMessage(From, Text);
end;

procedure TfrmMain.SocketError(Sender : TObject);
begin
  pnlReady.Visible := False;
  if Game.Mode = gHost then
  begin
    AddToServer(Game.GetPlayer(0).Name);
    ShowHostWaiting;
  end;
  ShowError(2);
end;

procedure TfrmMain.GameStarted(Sender : TObject);
begin
  if Game.Mode = gHost then
    HideHostWaiting;
end;

procedure TfrmMain.GameEnded(Sender : TObject);
begin         {
  ShowMessage(4); }
end;

procedure TfrmMain.DrawScore(Info : PScoreInfo);
var
  i, x, y : Integer;
  Str : string;
begin
  with Buffer.Canvas do
  begin
    x := Info.X*30+120;
    y := Info.Y*30+7-Info.Life;
    Brush.Style := bsClear;
    BrushCopy(Rect(x, y, x+8, y+9), Digits, Rect(0,0,8,9), clFuchsia);
    Str := IntToStr(Info.Value);
    for i := 1 to Length(Str) do
      case Str[i] of
        '1': BrushCopy(Rect(x+(i*8), y, x+(i*8)+8, y+9), Digits, Rect(8,0,16,9), clFuchsia);
        '2': BrushCopy(Rect(x+(i*8), y, x+(i*8)+8, y+9), Digits, Rect(16,0,24,9), clFuchsia);
        '3': BrushCopy(Rect(x+(i*8), y, x+(i*8)+8, y+9), Digits, Rect(24,0,32,9), clFuchsia);
        '4': BrushCopy(Rect(x+(i*8), y, x+(i*8)+8, y+9), Digits, Rect(32,0,40,9), clFuchsia);
        '5': BrushCopy(Rect(x+(i*8), y, x+(i*8)+8, y+9), Digits, Rect(40,0,48,9), clFuchsia);
        '6': BrushCopy(Rect(x+(i*8), y, x+(i*8)+8, y+9), Digits, Rect(48,0,56,9), clFuchsia);
        '7': BrushCopy(Rect(x+(i*8), y, x+(i*8)+8, y+9), Digits, Rect(56,0,64,9), clFuchsia);
        '8': BrushCopy(Rect(x+(i*8), y, x+(i*8)+8, y+9), Digits, Rect(64,0,72,9), clFuchsia);
        '9': BrushCopy(Rect(x+(i*8), y, x+(i*8)+8, y+9), Digits, Rect(72,0,80,9), clFuchsia);
        '0': BrushCopy(Rect(x+(i*8), y, x+(i*8)+8, y+9), Digits, Rect(80,0,88,9), clFuchsia);
      end;
  end;
end;

procedure TfrmMain.DrawDissolve(Info : PDissolveInfo);   
var
  x, y : Integer;
begin
  with Buffer.Canvas do
  begin
    x := Info.X*30+120;
    y := Info.Y*30+7;
    Brush.Style := bsClear;
    BrushCopy(Rect(x, y, x+30, y+30), Dissolve, Rect((Info.Life div 3)*30,0,((Info.Life div 3)*30)+30,30), clFuchsia);
  end;
end;

procedure TfrmMain.DrawTile(ToRect : TRect; Cube : TCubeColor; Col, Row : Integer);
begin
  with Buffer.Canvas do
  begin
    case Cube of
      cNone :
        if (Col = Game.CurCol) and (Row = 0) then
          CopyRect(ToRect, Tiles.Canvas, Rect(270,0,300,30))
        else if Col = Game.CurCol then
          CopyRect(ToRect, Tiles.Canvas, Rect(300,0,330,30))
        else
          CopyRect(ToRect, Tiles.Canvas, Rect(0,0,30,30));
      cBlue : CopyRect(ToRect, Tiles.Canvas, Rect(30,0,60,30));
      cRed : CopyRect(ToRect, Tiles.Canvas, Rect(60,0,90,30));
      cGreen : CopyRect(ToRect, Tiles.Canvas, Rect(90,0,120,30));
      cYellow : CopyRect(ToRect, Tiles.Canvas, Rect(120,0,150,30));
      cTeal : CopyRect(ToRect, Tiles.Canvas, Rect(150,0,180,30));
      cPurple : CopyRect(ToRect, Tiles.Canvas, Rect(180,0,210,30));
      cOrange : CopyRect(ToRect, Tiles.Canvas, Rect(210,0,240,30));
      cGrey : CopyRect(ToRect, Tiles.Canvas, Rect(240,0,270,30));
    end;
  end;
end;

procedure TfrmMain.DrawBoard;
var
  Size, i, j, Min, Sec : Integer;
  Cube : PCube;
  Score : PScoreInfo;
  Dissolve : PDissolveInfo;
begin
  with Buffer.Canvas do
  begin
    { Calculates size }
    if Game.GridSize > 0 then
      Size := Game.GridSize
    else
      Size := 9;
    frmMain.ClientWidth := Size*30+131;
    frmMain.ClientHeight := Size*30+18;
    Buffer.Width := PaintBox.Width;
    Buffer.Height := PaintBox.Height;
    { Clears canvas }
    Pen.Style := psSolid;
    Pen.Width := 1;             
    Pen.Color := frmOptions.BackColor;
    Brush.Color := frmOptions.BackColor;
    Rectangle(0, 0, PaintBox.Width, PaintBox.Height);
    { Grid }
    Pen.Width := 2;
    Pen.Color := frmOptions.BorderColor;
    Rectangle(119, 6, Size*30+122, Size*30+9);
    for i := 0 to Size-1 do
      for j := 0 to Size-1 do
      begin
        Cube := Game.GetCube(i, j);
        if Cube <> nil then
          DrawTile(Rect(i*30+120, j*30+7, i*30+150, j*30+37), Cube.Color, i, j)
        else
          DrawTile(Rect(i*30+120, j*30+7, i*30+150, j*30+37), cNone, i, j);
      end;
    { Block }
    Font.Color := frmOptions.TextColor;    
    TextOut(74-TextWidth(lblBlock.Caption), 4, lblBlock.Caption);
    Rectangle(79, 6, 112, 99);
    DrawTile(Rect(80, 7, 110, 37), Game.Cube3, -1, -1);
    DrawTile(Rect(80, 37, 110, 67), Game.Cube2, -1, -1);
    DrawTile(Rect(80, 67, 110, 97), Game.Cube1, -1, -1);
    if Game.State <> gEnded then
    begin
      { Dissolves }
      if Game.Dissolves.Size > 0 then
      begin
        Dissolve := Game.Dissolves.First;
        while Dissolve <> nil do
        begin
          DrawDissolve(Dissolve);
          Inc(Dissolve.Life);
          if Dissolve.Life = 24 then
            Dissolve := Game.Dissolves.Remove(Dissolve)
          else
            Dissolve := Dissolve.Next;
        end;
        if Game.Dissolves.Size = 0 then
          Game.ClearMatches;
      end;
      { Floating scores }
      Score := Game.Scores.First;
      while Score <> nil do
      begin
        DrawScore(Score);
        Inc(Score.Life);
        if Score.Life = 60 then
          Score := Game.Scores.Remove(Score)
        else
          Score := Score.Next;
      end;
    end;
    { Player infos }
    Font.Color := frmOptions.TextColor;
    j := 110;               
    for i := 0 to Game.PlayerCount-1 do
    begin
      if i = Game.CurPlayer then
      begin
        Pen.Style := psClear;
        Brush.Color := frmOptions.PlayerColor;
        RoundRect(4, j-4, 115, j+50, 9, 9);
        Font.Style := [fsBold];
      end
      else
        Brush.Color := frmOptions.BackColor;
      TextOut(10, j, Game.GetPlayer(i).Name);
      Font.Style := []; 
      { Time }
      Min := Floor(Game.GetPlayer(i).Time/60);
      Sec := Game.GetPlayer(i).Time - (Min*60);
      if (Game.State = gEnded) and (Game.Mode <> gLocal) then
        if Game.GetPlayer(i).Ready then
          TextOut(20, j+16, lblReady.Caption)
        else
          TextOut(20, j+16, lblNotReady.Caption)
      else
      begin      
        TextOut(20, j+16, lblTime.Caption);
        if Sec < 10 then
          TextOut(110-TextWidth(IntToStr(Min) + ':0' + IntToStr(Sec)), j+16, IntToStr(Min) + ':0' + IntToStr(Sec))
        else
          TextOut(110-TextWidth(IntToStr(Min) + ':' + IntToStr(Sec)), j+16, IntToStr(Min) + ':' + IntToStr(Sec));
      end;
      { Score }  
      TextOut(20, j+32, lblScore.Caption);
      TextOut(110-TextWidth(IntToStr(Game.GetPlayer(i).Score)), j+32, IntToStr(Game.GetPlayer(i).Score));
      j := j + 60;
    end;           
  end;   
  PaintBox.Canvas.CopyRect(PaintBox.ClientRect, Buffer.Canvas, PaintBox.ClientRect);
end;

procedure TfrmMain.TranslateTo(FileName : string);
var
  LangPack : TLangPack;
begin
  LangPack := TLangPack.Create;
  if LangPack.LoadFromFile(FileName) then
  begin                                
    LangPack.TranslateForm(frmChat);
    LangPack.TranslateForm(frmJoinGame);
    LangPack.TranslateForm(frmMain);
    LangPack.TranslateForm(frmNewGame);
    LangPack.TranslateForm(frmOptions);
    LangPack.TranslateStrings('frmOptions', frmOptions.Texts);
    LangPack.TranslateStrings('messages', MsgStr);
  end;
  LangPack.Free;
end;

procedure TfrmMain.LoadSettings;
var
  IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFileDir(Application.ExeName) + '\QuaBlocks.ini');

  frmMain.Left := IniFile.ReadInteger('Appearance', 'FormLeft', 25);   
  frmMain.Top := IniFile.ReadInteger('Appearance', 'FormTop', 25);
  frmNewGame.Names.Clear;
  frmNewGame.Names.Add(IniFile.ReadString('NewGame', 'Player1', ''));
  frmNewGame.Names.Add(IniFile.ReadString('NewGame', 'Player2', ''));
  frmNewGame.Names.Add(IniFile.ReadString('NewGame', 'Player3', ''));
  frmNewGame.Names.Add(IniFile.ReadString('NewGame', 'Player4', ''));
  frmJoinGame.Name := IniFile.ReadString('JoinGame', 'Player', ''); 
  frmJoinGame.HostAddress := IniFile.ReadString('JoinGame', 'LastAddress', '');

  IniFile.Free;
end;

procedure TfrmMain.SaveSettings;
var
  IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFileDir(Application.ExeName) + '\QuaBlocks.ini');
                    
  IniFile.WriteInteger('Appearance', 'FormLeft', frmMain.Left);
  IniFile.WriteInteger('Appearance', 'FormTop', frmMain.Top);
  if frmNewGame.Names.Count > 0 then
    IniFile.WriteString('NewGame', 'Player1', frmNewGame.Names[0]);
  if frmNewGame.Names.Count > 1 then
    IniFile.WriteString('NewGame', 'Player2', frmNewGame.Names[1]);
  if frmNewGame.Names.Count > 2 then
    IniFile.WriteString('NewGame', 'Player3', frmNewGame.Names[2]);
  if frmNewGame.Names.Count > 3 then
    IniFile.WriteString('NewGame', 'Player4', frmNewGame.Names[3]); 
  IniFile.WriteString('JoinGame', 'Player', frmJoinGame.Name);
  IniFile.WriteString('JoinGame', 'LastAddress', frmJoinGame.HostAddress);

  IniFile.UpdateFile;
  IniFile.Free;
end;

procedure TfrmMain.LoadOptions;
var
  IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFileDir(Application.ExeName) + '\QuaBlocks.ini');
                                                                
  frmOptions.GameServer := IniFile.ReadString('Options', 'GameServer', 'http://www.marcandre.info'); 
  frmOptions.LanguageFile := IniFile.ReadString('Options', 'LanguageFile', 'GameEn.lang');
  frmOptions.AutoUpdate := IniFile.ReadBool('Options', 'AutoUpdate', False);
  frmOptions.BackColor := IniFile.ReadInteger('Options', 'BackgroundColor', clWindow);
  frmOptions.BorderColor := IniFile.ReadInteger('Options', 'BorderColor', cl3DDkShadow);
  frmOptions.PlayerColor := IniFile.ReadInteger('Options', 'PlayerColor', clBtnFace);
  frmOptions.TextColor := IniFile.ReadInteger('Options', 'TextColor', cl3DDkShadow);
  frmOptions.UseMouse := IniFile.ReadBool('Options', 'UseTheMouse', True);
  frmOptions.UseKeyboard := IniFile.ReadBool('Options', 'UseTheKeyboard', True);
  frmOptions.ControlUp := IniFile.ReadInteger('Options', 'ControlUp', 38);
  frmOptions.ControlDown := IniFile.ReadInteger('Options', 'ControlDown', 40);
  frmOptions.ControlLeft := IniFile.ReadInteger('Options', 'ControlLeft', 37);
  frmOptions.ControlRight := IniFile.ReadInteger('Options', 'ControlRight', 39);
  frmOptions.ControlDrop := IniFile.ReadInteger('Options', 'ControlDrop', 32);
  IniFile.Free;
end;

procedure TfrmMain.SaveOptions;
var
  IniFile : TIniFile;
begin
  IniFile := TIniFile.Create(ExtractFileDir(Application.ExeName) + '\QuaBlocks.ini');

  IniFile.WriteString('Options', 'GameServer', frmOptions.GameServer);       
  IniFile.WriteString('Options', 'LanguageFile', frmOptions.LanguageFile);
  IniFile.WriteBool('Options', 'AutoUpdate', frmOptions.AutoUpdate);
  IniFile.WriteInteger('Options', 'BackgroundColor', Integer(frmOptions.BackColor));
  IniFile.WriteInteger('Options', 'BorderColor', Integer(frmOptions.BorderColor));
  IniFile.WriteInteger('Options', 'PlayerColor', Integer(frmOptions.PlayerColor));
  IniFile.WriteInteger('Options', 'TextColor', Integer(frmOptions.TextColor));
  IniFile.WriteBool('Options', 'UseTheMouse', frmOptions.UseMouse);
  IniFile.WriteBool('Options', 'UseTheKeyboard', frmOptions.UseKeyboard);
  IniFile.WriteInteger('Options', 'ControlUp', frmOptions.ControlUp);
  IniFile.WriteInteger('Options', 'ControlDown', frmOptions.ControlDown);
  IniFile.WriteInteger('Options', 'ControlLeft', frmOptions.ControlLeft);
  IniFile.WriteInteger('Options', 'ControlRight', frmOptions.ControlRight);
  IniFile.WriteInteger('Options', 'ControlDrop', frmOptions.ControlDrop);

  IniFile.UpdateFile;
  IniFile.Free;
end;

end.
