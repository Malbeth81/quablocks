(* QuaBlocks *******************************************************)
(* Copyright (c)2005-2006 Marc-Andre Lamothe ***********************)
(* All rights reserved *********************************************)

unit UnitQuaBlocks;

interface
uses ExtCtrls, SysUtils, ScktComp, Classes, UnitScores, UnitDissolves;

type
TGameMode = (gLocal, gHost, gClient);
TGameState = (gEnded, gStarted, gPaused);
TNetState = (sNone, sConnected, sDisconnected, sStarted, sPaused, sEnded);
TAction = (aMoveLeft, aMoveRight, aRotateUp, aRotateDown, aDrop);
TDataType = (dNone, dState, dVersion, dGridSize, dTime, dPlayerCount, dPlayerNumber,
             dPlayerName, dPlayerReady, dBlock, dAction, dText);
TCubeColor = (cNone, cBlue, cRed, cGreen, cYellow, cTeal, cPurple, cOrange, cGrey);
PCube = ^TCube;
TCube = record
  Color : TCubeColor;
  Matched : Boolean;
  Passed : Boolean;
end;
PPlayer = ^TPlayer;
TPlayer = record
  Name : ShortString;
  Time,
  Score : Integer;
  Ready : Boolean;
end;
TBlock = record
  Cube1 : TCubeColor;
  Cube2 : TCubeColor;
  Cube3 : TCubeColor;
end;
TText = record
  Player : Integer;
  Text : ShortString;
end;               
TChatEvent = procedure(Sender : TObject; From, Text : string) of object;

TQuaBlocks = class (TObject)
private        
  gVersion : Integer;
  gTimer : TTimer;    
  gMode : TGameMode;
  gState : TGameState;

  gColors : Integer;
  gGridSize : Integer;
  gGrid : array of array of TCube;
  gPlayers : array of PPlayer;
  gPlayerCount : Integer;
  gPlayerNumber,                  { Identifies the player in client/host games }
  gCurPlayer,                     { Identifies the played making a move }
  gCurCol : Integer;
  gCube1 : TCubeColor;
  gCube2 : TCubeColor;
  gCube3 : TCubeColor;
  gScores : TScoreList;
  gDissolves : TDissolveList;

  gServerSocket : TServerSocket;
  gClientSocket : TClientSocket;
  gDataType : TDataType;

  gOnSocketError : TNotifyEvent;
  gOnPlayerConnected : TNotifyEvent;
  gOnPlayerDisconnected : TNotifyEvent;
  gOnPlayerChat : TChatEvent;
  gOnGameStarted : TNotifyEvent;
  gOnGameStopped : TNotifyEvent;
  gOnGamePaused : TNotifyEvent;
  gOnGameResumed : TNotifyEvent;

  function BlockSize(Col, Line : Integer) : Integer;
  function FindMatches : Boolean;
  function FindBlocks : Boolean;
  function GetScore(Color : TCubeColor) : Integer;
  procedure BlockFall(Col, Line : Integer);
  procedure ChangePlayer;
  procedure ClearGrid;
  procedure TimerEvent(Sender: TObject);
  function GetClientCount : Integer;
  procedure SendDataType(DataType : TDataType; Socket: TCustomWinSocket);
  procedure SendState(State : TNetState; Socket: TCustomWinSocket);
  procedure ServerSocketConnect(Sender: TObject; Socket: TCustomWinSocket);
  procedure ServerSocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
  procedure ServerSocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  procedure ServerSocketRead(Sender: TObject; Socket: TCustomWinSocket);
  procedure ClientSocketConnect(Sender: TObject; Socket: TCustomWinSocket);
  procedure ClientSocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
  procedure ClientSocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  procedure ClientSocketRead(Sender: TObject; Socket: TCustomWinSocket);
public
  constructor Create;
  destructor Destroy; override;

  function GetCube(Col, Line : Integer) : PCube;
  function GetPlayer(Index : Integer) : PPlayer;
  function New(Mode : TGameMode; Players, Colors, GridSize : Integer; Names : TStrings) : Boolean;
  function Join(Name, HostAddress : string) : Boolean;
  procedure Ready;
  procedure Start;
  procedure Stop;
  procedure Pause;
  procedure Resume;
  procedure MoveLeft;
  procedure MoveRight;
  procedure RotateDown;
  procedure RotateUp;
  procedure Drop;
  procedure ClearMatches;
  procedure SendText(Str : string);

  property Mode : TGameMode read gMode;
  property State : TGameState read gState;
  property GridSize : Integer read gGridSize;
  property PlayerCount : Integer read gPlayerCount;
  property PlayerNumber : Integer read gPlayerNumber;
  property CurPlayer : Integer read gCurPlayer;
  property CurCol : Integer read gCurCol;
  property Cube1 : TCubeColor read gCube1;
  property Cube2 : TCubeColor read gCube2;
  property Cube3 : TCubeColor read gCube3;
  property ClientCount : Integer read GetClientCount;
  property Scores : TScoreList read gScores;
  property Dissolves : TDissolveList read gDissolves;
  property OnSocketError : TNotifyEvent read gOnSocketError write gOnSocketError;
  property OnPlayerConnected : TNotifyEvent read gOnPlayerConnected write gOnPlayerConnected;
  property OnPlayerDisconnected : TNotifyEvent read gOnPlayerDisconnected write gOnPlayerDisconnected;
  property OnPlayerChat : TChatEvent read gOnPlayerChat write gOnPlayerChat;
  property OnGameStarted : TNotifyEvent read gOnGameStarted write gOnGameStarted;
  property OnGameStopped : TNotifyEvent read gOnGameStopped write gOnGameStopped;
  property OnGamePaused : TNotifyEvent read gOnGamePaused write gOnGamePaused;
  property OnGameResumed : TNotifyEvent read gOnGameResumed write gOnGameResumed;
end;

implementation

constructor TQuaBlocks.Create;
begin
  gVersion := 200;
  gTimer := TTimer.Create(nil);
  gTimer.Enabled := False;
  gTimer.Interval := 1000;
  gTimer.OnTimer := TimerEvent;
  gScores := TScoreList.Create;
  gDissolves := TDissolveList.Create;
  gServerSocket := TServerSocket.Create(nil);
  gServerSocket.OnClientConnect := ServerSocketConnect;
  gServerSocket.OnClientDisconnect := ServerSocketDisconnect;
  gServerSocket.OnClientError := ServerSocketError;
  gServerSocket.OnClientRead := ServerSocketRead;
  gClientSocket := TClientSocket.Create(nil);
  gClientSocket.OnConnect := ClientSocketConnect;
  gClientSocket.OnDisconnect := ClientSocketDisconnect;
  gClientSocket.OnError := ClientSocketError;
  gClientSocket.OnRead := ClientSocketRead;
  inherited Create; 
end;

destructor TQuaBlocks.Destroy;
begin
  gTimer.Free;   
  gScores.Free;
  gDissolves.Free;
  if gServerSocket.Active then
    gServerSocket.Close;
  gServerSocket.Free;  
  if gClientSocket.Active then
    gClientSocket.Close;
  gClientSocket.Free;
  inherited Destroy;
end;

function TQuaBlocks.GetCube(Col, Line : Integer) : PCube;
begin
  Result := nil;
  if (gGridSize > 0) and (Col in [0..gGridSize-1]) and (Line in [0..gGridSize-1]) then
    Result := @gGrid[Col, Line];
end;

function TQuaBlocks.GetPlayer(Index : Integer) : PPlayer;
begin
  Result := nil;
  if (gPlayerCount > 0) and (Index in [0..gPlayerCount-1]) then
    Result := gPlayers[Index];
end;

function TQuaBlocks.New(Mode : TGameMode; Players, Colors, GridSize : Integer; Names : TStrings) : Boolean;
var
  i : Integer;
begin
  Result := True;
  Stop;     
  ClearGrid;
  Randomize;
  { Deletes player info }
  if gPlayerCount > 0 then
    for i := 0 to gPlayerCount-1 do
      Dispose(gPlayers[i]);
  { Initializing global values }
  gMode := Mode;
  gGridSize := GridSize;
  gColors := Colors;
  gPlayerNumber := 0;
  gPlayerCount := Players;
  gCurPlayer := 0;
  gCurCol := 0;
  gCube1 := TCubeColor(Random(gColors)+1);
  gCube2 := TCubeColor(Random(gColors)+1);
 	gCube3 := TCubeColor(Random(gColors)+1);
  gScores.Clear;
  gDissolves.Clear;
  { Created player info }
  SetLength(gPlayers, gPlayerCount);
  for i := 0 to gPlayerCount-1 do
  begin
    System.New(gPlayers[i]);
    if Names.Count > i then
      gPlayers[i].Name := Names[i]
    else
      gPlayers[i].Name := '';
    gPlayers[i].Score := 0;
    gPlayers[i].Time := 0;
    gPlayers[i].Ready := False;
  end;
  { Creates grid }
  SetLength(gGrid, gGridSize);
  for i := 0 to gGridSize-1 do
    SetLength(gGrid[i], gGridSize);
  ClearGrid;
  { Creates connection }
  if gServerSocket.Active then
    gServerSocket.Close;
  if gMode = gHost then
    try
      gServerSocket.Port := 2580;
      gServerSocket.ServerType := stNonBlocking;
      gServerSocket.Open;
    except            
      Result := False;
      if Assigned(gOnSocketError) then
        gOnSocketError(Self);
    end;
end;

function TQuaBlocks.Join(Name, HostAddress : string) : Boolean;
begin   
  Result := True;
  Stop;
  ClearGrid;
  gMode := gCLient;
  { Create player info }
  if gPlayerCount = 0 then
  begin
    gPlayerCount := 1;
    SetLength(gPlayers, gPlayerCount);
    System.New(gPlayers[0]);
  end;
  gPlayers[0].Name := Name;
  gPlayers[0].Score := 0;
  gPlayers[0].Time := 0;
  gPlayers[0].Ready := False;
  { Creates connection }
  if gClientSocket.Active then
    gClientSocket.Close;
  try
    gClientSocket.Host := HostAddress;
    gClientSocket.Port := 2580;
    gClientSocket.ClientType := ctNonBlocking;
    gClientSocket.Open;
  except
    Result := False;
    if Assigned(gOnSocketError) then
      gOnSocketError(Self);
  end;
end;

procedure TQuaBlocks.Ready;
var
  i : Integer;
begin
  case gMode of
    gHost : begin
      gPlayers[gPlayerNumber].Ready := True;
      for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
      begin
        SendDataType(dPlayerReady, gServerSocket.Socket.Connections[i]);
        gServerSocket.Socket.Connections[i].SendBuf(gPlayerNumber, SizeOf(Integer));
      end;
      for i := 0 to gPlayerCount-1 do
        if not gPlayers[i].Ready then
          Exit;
      Start;
    end;
    gClient : begin
      gPlayers[gPlayerNumber].Ready := True;
      SendDataType(dPlayerReady, gClientSocket.Socket);
      gClientSocket.Socket.SendBuf(gPlayerNumber, SizeOf(Integer));
    end;
  end;
end;

procedure TQuaBlocks.Start;
var
  i : Integer;
begin
  if gState = gEnded then
  begin
    if gMode <> gClient then
      gTimer.Enabled := True;
    gState := gStarted;
    if (gMode = gHost) and (gCurPlayer = gPlayerNumber) then
      for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
        SendState(sStarted, gServerSocket.Socket.Connections[i]);
    if Assigned(gOnGameStarted) then
      gOnGameStarted(Self);
  end;
end;
      
procedure TQuaBlocks.Pause;
var
  i : Integer;
begin                  
  if gState = gStarted then
  begin
    gState := gPaused;
    gTimer.Enabled := False;
    if (gMode = gHost) and (gCurPlayer = gPlayerNumber) then
      for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
        SendState(sPaused, gServerSocket.Socket.Connections[i])
    else if (gMode = gClient) and (gCurPlayer = gPlayerNumber) then
      SendState(sPaused, gClientSocket.Socket);
    if Assigned(gOnGamePaused) then
      gOnGamePaused(Self);
  end;
end;

procedure TQuaBlocks.Resume;
var
  i : Integer;
begin
  if gState = gPaused then
  begin
    gState := gStarted;     
    if gMode <> gClient then
      gTimer.Enabled := True;
    if (gMode = gHost) and (gCurPlayer = gPlayerNumber) then
      for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
        SendState(sStarted, gServerSocket.Socket.Connections[i])
    else if (gMode = gClient) and (gCurPlayer = gPlayerNumber) then
      SendState(sStarted, gClientSocket.Socket);
    if Assigned(gOnGameResumed) then
      gOnGameResumed(Self);
  end;
end;

procedure TQuaBlocks.Stop;
var
  i : Integer;
begin        
  if gState <> gEnded then
  begin
    gState := gEnded;
    gTimer.Enabled := False;
    if (gMode = gHost) and (gCurPlayer = gPlayerNumber) then
      for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
      begin
        SendState(sEnded, gServerSocket.Socket.Connections[i]);
        SendState(sDisconnected, gServerSocket.Socket.Connections[i]);
      end
    else if (gMode = gClient) and (gCurPlayer = gPlayerNumber) then
    begin
      SendState(sEnded, gClientSocket.Socket);  
      SendState(sDisconnected, gClientSocket.Socket);
    end;
    if Assigned(gOnGameStopped) then
      gOnGameStopped(Self);           
  end;
end;

procedure TQuaBlocks.MoveLeft;
var               
  i : Integer;  
  Action : TAction;
begin
  if (gState = gStarted) and (gCurCol in [1..gGridSize-1]) then
  begin
    Action := aMoveLeft;
    gCurCol := gCurCol-1;
    if (gMode = gHost) and (gCurPlayer = gPlayerNumber) then
      for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
      begin
        SendDataType(dAction, gServerSocket.Socket.Connections[i]);
        gServerSocket.Socket.Connections[i].SendBuf(Action, SizeOf(TAction));
      end
    else if (gMode = gClient) and (gCurPlayer = gPlayerNumber) then
    begin
      SendDataType(dAction, gClientSocket.Socket);
      gClientSocket.Socket.SendBuf(Action, SizeOf(TAction));
    end;
  end;
end;

procedure TQuaBlocks.MoveRight;  
var                   
  i : Integer;
  Action : TAction;
begin      
  if (gState = gStarted) and (gCurCol in [0..gGridSize-2]) then
  begin
    Action := aMoveRight;
    gCurCol := gCurCol+1;
    if (gMode = gHost) and (gCurPlayer = gPlayerNumber) then
      for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
      begin
        SendDataType(dAction, gServerSocket.Socket.Connections[i]);
        gServerSocket.Socket.Connections[i].SendBuf(Action, SizeOf(TAction));
      end
    else if (gMode = gClient) and (gCurPlayer = gPlayerNumber) then
    begin
      SendDataType(dAction, gClientSocket.Socket);
      gClientSocket.Socket.SendBuf(Action, SizeOf(TAction));
    end;
  end;
end;

procedure TQuaBlocks.RotateDown;
var
  i : Integer;
  TmpInt : TCubeColor;
  Action : TAction;
begin
  if (gState = gStarted) then
  begin        
    Action := aRotateDown;
  	TmpInt := gCube1;
    gCube1 := gCube2;
    gCube2 := gCube3;
    gCube3 := TmpInt;
    if (gMode = gHost) and (gCurPlayer = gPlayerNumber) then
      for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
      begin
        SendDataType(dAction, gServerSocket.Socket.Connections[i]);
        gServerSocket.Socket.Connections[i].SendBuf(Action, SizeOf(TAction));
      end
    else if (gMode = gClient) and (gCurPlayer = gPlayerNumber) then
    begin
      SendDataType(dAction, gClientSocket.Socket);
      gClientSocket.Socket.SendBuf(Action, SizeOf(TAction));
    end;
  end;
end;

procedure TQuaBlocks.RotateUp;
var                    
  i : Integer;
  TmpInt : TCubeColor; 
  Action : TAction;
begin
  if (gState = gStarted) then
  begin      
    Action := aRotateUp;
    TmpInt := gCube3;
    gCube3 := gCube2;
    gCube2 := gCube1;
	  gCube1 := TmpInt;
    if (gMode = gHost) and (gCurPlayer = gPlayerNumber) then
      for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
      begin
        SendDataType(dAction, gServerSocket.Socket.Connections[i]);
        gServerSocket.Socket.Connections[i].SendBuf(Action, SizeOf(TAction));
      end
    else if (gMode = gClient) and (gCurPlayer = gPlayerNumber) then
    begin
      SendDataType(dAction, gClientSocket.Socket);
      gClientSocket.Socket.SendBuf(Action, SizeOf(TAction));
    end;
  end;
end;

procedure TQuaBlocks.Drop;
var
  i : Integer;
  GridIsFull : Boolean;
  Action : TAction;
  Block : TBlock;
begin
  if (gState = gStarted) and (gDissolves.Size = 0) then
  begin            
    Action := aDrop;
    { Drops block }
    if gGrid[gCurCol, 2].Color = cNone then
    begin
      gGrid[gCurCol, 2].Color := gCube1;
      gGrid[gCurCol, 1].Color := gCube2;
      gGrid[gCurCol, 0].Color := gCube3;
    end
    else if gGrid[gCurCol, 1].Color = cNone then
    begin
      gGrid[gCurCol, 1].Color := gCube1;
      gGrid[gCurCol, 0].Color := gCube2;
    end
    else if gGrid[gCurCol, 0].Color = cNone then
    begin
      gGrid[gCurCol, 0].Color := gCube1;
    end
    else
      Exit;
    { Sends action on network }
    if (gMode = gHost) and (gCurPlayer = gPlayerNumber) then
      for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
      begin
        SendDataType(dAction, gServerSocket.Socket.Connections[i]);
        gServerSocket.Socket.Connections[i].SendBuf(Action, SizeOf(TAction));
      end
    else if (gMode = gClient) and (gCurPlayer = gPlayerNumber) then
    begin
      Action := aDrop;
      SendDataType(dAction, gClientSocket.Socket);
      gClientSocket.Socket.SendBuf(Action, SizeOf(Action));
    end;
    { Drops block }
    BlockFall(gCurCol, 2);
    if not FindBlocks then
    begin
      GridIsFull := True;
      for i := 0 to GridSize-1 do
        if gGrid[i, 0].Color = cNone then
          GridIsFull := False;
      if GridIsFull then
      begin
        Stop;
        Exit;
      end;
      ChangePlayer;
    end;
    { Generates new block }
    if gMode <> gClient then
    begin
	    gCube1 := TCubeColor(Random(gColors)+1);
      gCube2 := TCubeColor(Random(gColors)+1);
 	    gCube3 := TCubeColor(Random(gColors)+1);
    end;
    { Sends new block on network }
    if gMode = gHost then
    begin
      Block.Cube1 := gCube1;
      Block.Cube2 := gCube2;
      Block.Cube3 := gCube3;
      for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
      begin
        SendDataType(dBlock, gServerSocket.Socket.Connections[i]);
        gServerSocket.Socket.Connections[i].SendBuf(Block, SizeOf(TBlock));
      end;
    end;
  end;
end;

function TQuaBlocks.BlockSize(Col, Line : Integer) : Integer;
begin
  gGrid[Col, Line].Passed := True;
  Result := 1;
  if (Line < gGridSize-1) and not gGrid[Col, Line+1].Passed and (gGrid[Col, Line].Color = gGrid[Col, Line+1].Color) then
    Result := Result + BlockSize(Col, Line+1);
  if (Col < gGridSize-1) and not gGrid[Col+1, Line].Passed and (gGrid[Col, Line].Color = gGrid[Col+1, Line].Color) then
    Result := Result + BlockSize(Col+1, Line);
  if (Col > 0) and not gGrid[Col-1, Line].Passed and (gGrid[Col, Line].Color = gGrid[Col-1, Line].Color) then
    Result := Result + BlockSize(Col-1, Line);
  if (Line > 0) and not gGrid[Col, Line-1].Passed and (gGrid[Col, Line].Color = gGrid[Col, Line-1].Color) then
    Result := Result + BlockSize(Col, Line-1);
end;

function TQuaBlocks.FindMatches : Boolean;
var
  i, j, k, l : Integer;
  Size : Integer;
begin
  Result := False;
  for i := 0 to gGridSize-1 do
	  for j := 0 to gGridSize-1 do
      if (gGrid[i, j].Color <> cNone) and not gGrid[i, j].Matched then
      begin
        Size := BlockSize(i, j);
        Result := Result or (Size >= 4);
        for k := 0 to gGridSize-1 do
	        for l := 0 to gGridSize-1 do
            if gGrid[k, l].Passed then
            begin
              gGrid[k, l].Matched := (Size >= 4);
              gGrid[k, l].Passed := False;
            end;
      end;
end;

function TQuaBlocks.FindBlocks : Boolean;
var
  i, j, Val : Integer;
  Color : TCubeColor;
  Breaking : Boolean;
begin
  Result := False;
  if FindMatches then
  begin
    { Dissolves }
    for i := 0 to GridSize-1 do
      for j := 0 to GridSize-1 do
        if GetCube(i, j).Matched then
          Dissolves.Add(i, j);
    { Scores }
    Color := cGrey;
    while Color <> cNone do
    begin
      Breaking := False;
      for j := 0 to GridSize-1 do
      begin
        for i := 0 to GridSize-1 do
          if (GetCube(i, j).Color = Color) and GetCube(i, j).Matched then
          begin
            Val := GetScore(Color);
            Inc(GetPlayer(CurPlayer).Score, Val);
            Scores.Add(Val, i, j);
            Breaking := True;
            Break;
          end;
        if Breaking then Break;
      end;
      Color := TCubeColor(Integer(Color)-1);
    end;
    Result := True;
  end;
end;

function TQuaBlocks.GetScore(Color : TCubeColor)  : Integer;
var
  i, j, Count : Integer;
begin
  Result := 0;
  Count := 0;
  for i := 0 to gGridSize-1 do
	  for j := 0 to gGridSize-1 do
      if (gGrid[i, j].Color = Color) and gGrid[i, j].Matched then
      begin
        if Count < 4 then
          Result := Result + 50
        else
          Result := Result + 10;
        Inc(Count);
      end;
end;

procedure TQuaBlocks.BlockFall(Col, Line : Integer);
var
  i : Integer;
begin
  i := Line;
  while (i < gGridSize-1) and (gGrid[Col][i+1].Color = cNone) do
    i := i+1;
  if i > Line then
    while Line >= 0 do
    begin
	    gGrid[Col, i].Color := gGrid[Col, Line].Color;
    	gGrid[Col, Line].Color := cNone;
      Line := Line-1;
      i := i-1;
    end;
end;

procedure TQuaBlocks.ClearMatches;
var
  i, j : Integer;
begin
  for i := 0 to gGridSize-1 do
    for j := 0 to gGridSize-1 do
  	  if gGrid[i, j].Matched then
      begin
  			gGrid[i, j].Color := cNone;
        gGrid[i, j].Matched := False;
      end;
  for i := 0 to gGridSize-1 do
    for j := gGridSize-2 downto 0 do
      if (gGrid[i, j].Color <> cNone) and (gGrid[i, j+1].Color = cNone) then
        BlockFall(i, j);
  if not FindBlocks then
    ChangePlayer;
end;

procedure TQuaBlocks.ChangePlayer;
begin
  if gCurPlayer = gPlayerCount-1 then
    gCurPlayer := 0
  else
    gCurPlayer := gCurPlayer+1;
end;

procedure TQuaBlocks.ClearGrid;
var
  i, j : Integer;
begin
  for i := 0 to gGridSize-1 do
    for j := 0 to gGridSize-1 do
    begin
      gGrid[i, j].Color := cNone;
      gGrid[i, j].Matched := False;
      gGrid[i, j].Passed := False;
    end;
end;

procedure TQuaBlocks.TimerEvent(Sender: TObject);
var
  i : Integer;
begin
  Inc(gPlayers[gCurPlayer].Time);
  if gMode = gHost then
    for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
      try
        SendDataType(dTime, gServerSocket.Socket.Connections[i]);
        gServerSocket.Socket.Connections[i].SendBuf(gPlayers[gCurPlayer].Time, SizeOf(Integer));
      except
        Exit;
      end;
end;
          
procedure TQuaBlocks.SendText(Str : string);
var
  Text : TText;
begin
  Text.Player := gPlayerNumber;
  Text.Text := Str;
  if gMode = gHost then
  begin
    SendDataType(dText, gServerSocket.Socket.Connections[0]);
    gServerSocket.Socket.Connections[0].SendBuf(Text, SizeOf(TText));
  end
  else if gMode = gClient then
  begin
    SendDataType(dText, gClientSocket.Socket);
    gClientSocket.Socket.SendBuf(Text, SizeOf(TText));
  end;
end;

function TQuaBlocks.GetClientCount : Integer;
begin
  Result := gServerSocket.Socket.ActiveConnections;
end;

procedure TQuaBlocks.SendDataType(DataType : TDataType; Socket: TCustomWinSocket);
var
  SentDataType : TDataType;
begin
  if Socket.Connected then
    try
      SentDataType := DataType;
      Socket.SendBuf(SentDataType, SizeOf(TDataType));
    except
      if Assigned(gOnSocketError) then
        gOnSocketError(Self);
    end;
end;

procedure TQuaBlocks.SendState(State : TNetState; Socket: TCustomWinSocket);
var
  SentState : TNetState;
begin
  if Socket.Connected then
    try
      SentState := State;
      SendDataType(dState, Socket);
      Socket.SendBuf(SentState, SizeOf(TNetState));
    except      
      if Assigned(gOnSocketError) then
        gOnSocketError(Self);
    end;
end;

procedure TQuaBlocks.ServerSocketConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  if gServerSocket.Socket.ActiveConnections > gPlayerCount-1 then
  begin
    SendState(sDisconnected, Socket);
    Socket.Close;
  end;
  { OnPlayerConnect event is called only after veryfing the version and username }
end;

procedure TQuaBlocks.ServerSocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin    
  Stop;       
  if Assigned(gOnPlayerDisconnected) then
    gOnPlayerDisconnected(Self);
end;

procedure TQuaBlocks.ServerSocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  Stop;
  if Assigned(gOnSocketError) then
    gOnSocketError(Self);
  ErrorCode := 0;
end;

procedure TQuaBlocks.ServerSocketRead(Sender: TObject; Socket: TCustomWinSocket);
var
  i : Integer;
  State : TNetState;
  Version : Integer;
  PlayerNumber : Integer;
  Action : TAction;
  Block : TBlock;
  Text : TText;
begin
  try
    case gDataType of
      dNone : Socket.ReceiveBuf(gDataType, SizeOf(TDataType));
      dState : begin
        Socket.ReceiveBuf(State, SizeOf(TNetState));
        gDataType := dNone;
        { Redirects data to other clients }
        for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
          if gServerSocket.Socket.Connections[i] <> Socket then
            SendState(State, gServerSocket.Socket.Connections[i]);
        case State of
          sStarted : if gState = gPaused then Resume;
          sPaused : Pause;
          sEnded : Stop;
          sDisconnected : Socket.Close;
        end;
      end;
      dVersion : begin
        Socket.ReceiveBuf(Version, SizeOf(Integer));
        gDataType := dNone;
        if Version = gVersion then
        begin
          SendState(sConnected, Socket);
          SendDataType(dGridSize, Socket);
          Socket.SendBuf(gGridSize, SizeOf(Integer));
          SendDataType(dPlayerCount, Socket);
          Socket.SendBuf(gPlayerCount, SizeOf(Integer));
          PlayerNumber := gServerSocket.Socket.ActiveConnections;
          SendDataType(dPlayerNumber, Socket);
          Socket.SendBuf(PlayerNumber, SizeOf(Integer));
          for i := 0 to gPlayerCount-1 do
            if i <> PlayerNumber then
            begin
              Text.Player := i;
              Text.Text := gPlayers[i].Name;
              SendDataType(dPlayerName, Socket);
              Socket.SendBuf(Text, SizeOf(TText));
            end;
          Block.Cube1 := gCube1;
          Block.Cube2 := gCube2;
          Block.Cube3 := gCube3;
          SendDataType(dBlock, Socket);
          Socket.SendBuf(Block, SizeOf(TBlock));
          if Assigned(gOnPlayerConnected) then
            gOnPlayerConnected(Self);
        end
        else
        begin
          SendState(sDisconnected, Socket);
          Socket.Close;
        end;
      end;
      dPlayerReady : begin
        Socket.ReceiveBuf(PlayerNumber, SizeOf(Integer));
        gDataType := dNone;        
        { Redirects data to other clients }
        for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
          if gServerSocket.Socket.Connections[i] <> Socket then
          begin
            SendDataType(dPlayerReady, gServerSocket.Socket.Connections[i]);
            gServerSocket.Socket.Connections[i].SendBuf(PlayerNumber, SizeOf(Integer));
          end;
        gPlayers[PlayerNumber].Ready := True;
        { Starts game }
        for i := 0 to gPlayerCount-1 do
          if not gPlayers[i].Ready then
            Exit;
        Start;
      end;
      dPlayerName : begin
        Socket.ReceiveBuf(Text, SizeOf(TText));
        gDataType := dNone;        
        { Redirects data to other clients }
        for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
          if gServerSocket.Socket.Connections[i] <> Socket then
          begin
            SendDataType(dPlayerName, gServerSocket.Socket.Connections[i]);
            gServerSocket.Socket.Connections[i].SendBuf(Text, SizeOf(Text));
          end;
        gPlayers[Text.Player].Name := Text.Text;
      end;
      dAction : begin
        Socket.ReceiveBuf(Action, SizeOf(TAction));
        gDataType := dNone;                 
        { Redirects data to other clients }
        for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
          if gServerSocket.Socket.Connections[i] <> Socket then
          begin
            SendDataType(dAction, gServerSocket.Socket.Connections[i]);
            gServerSocket.Socket.Connections[i].SendBuf(Action, SizeOf(TAction));
          end;
        case Action of
          aMoveLeft: MoveLeft;
          aMoveRight: MoveRight;
          aRotateUp: RotateUp;
          aRotateDown: RotateDown;
          aDrop: Drop;
        end;
      end;
      dText : begin
        Socket.ReceiveBuf(Text, SizeOf(TText));
        gDataType := dNone;        
        { Redirects data to other clients }
        for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
          if gServerSocket.Socket.Connections[i] <> Socket then
          begin
            SendDataType(dText, gServerSocket.Socket.Connections[i]);
            gServerSocket.Socket.Connections[i].SendBuf(Text, SizeOf(TText));
          end;
        if Assigned(gOnPlayerChat) then
          gOnPlayerChat(Self, gPlayers[Text.Player].Name, Text.Text);
      end;
    end;
  except
    Stop;
    for i := 0 to gServerSocket.Socket.ActiveConnections-1 do
      if gServerSocket.Socket.Connections[i] <> Socket then
        SendState(sDisconnected, gServerSocket.Socket.Connections[i]);
    gServerSocket.Close;
    if Assigned(gOnSocketError) then
      gOnSocketError(Self);
  end;
end;

procedure TQuaBlocks.ClientSocketConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  try
    SendDataType(dVersion, Socket);
    Socket.SendBuf(gVersion, SizeOf(Integer));
  except
    Stop;
    gClientSocket.Close;  
    if Assigned(gOnSocketError) then
      gOnSocketError(Self);
  end;             
  { OnPlayerConnect event is called only after veryfing the version and username }
end;
      
procedure TQuaBlocks.ClientSocketDisconnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  Stop;
  if Assigned(gOnPlayerDisconnected) then
    gOnPlayerDisconnected(Self);
end;

procedure TQuaBlocks.ClientSocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  Stop;
  if Assigned(gOnSocketError) then
    gOnSocketError(Self);
  ErrorCode := 0;
end;

procedure TQuaBlocks.ClientSocketRead(Sender: TObject; Socket: TCustomWinSocket);
var                     
  i : Integer;
  State : TNetState;
  Action : TAction;
  PlayerNumber : Integer;
  Block : TBlock;
  Text : TText;
begin
  try
    case gDataType of
      dNone : Socket.ReceiveBuf(gDataType, SizeOf(TDataType));
      dState : begin
        Socket.ReceiveBuf(State, SizeOf(TNetState));
        gDataType := dNone;
        case State of
          sConnected : begin  
            if Assigned(gOnPlayerConnected) then
              gOnPlayerConnected(Self);
          end;
          sDisconnected : Socket.Close;
          sStarted : if gState = gPaused then Resume else Start;
          sPaused : Pause;
          sEnded : Stop;
        end;
      end;
      dTime : begin
        Socket.ReceiveBuf(gPlayers[gCurPlayer].Time, SizeOf(Integer));
        gDataType := dNone;
      end;
      dGridSize : begin
        Socket.ReceiveBuf(gGridSize, SizeOf(Integer));
        gDataType := dNone;
        SetLength(gGrid, gGridSize);
        for i := 0 to gGridSize-1 do
          SetLength(gGrid[i], gGridSize);
        ClearGrid;
      end;        
      dPlayerCount : begin
        Socket.ReceiveBuf(gPlayerCount, SizeOf(Integer));
        gDataType := dNone;
        SetLength(gPlayers, gPlayerCount);
        for i := 0 to gPlayerCount-1 do
          if gPlayers[i] = nil then
          begin
            System.New(gPlayers[i]);
            gPlayers[i].Name := '';
            gPlayers[i].Score := 0;
            gPlayers[i].Time := 0;
            gPlayers[i].Ready := False;
          end;
      end;
      dPlayerNumber : begin
        Socket.ReceiveBuf(gPlayerNumber, SizeOf(Integer));
        gDataType := dNone;
        gPlayers[gPlayerNumber].Name := gPlayers[0].Name;
        { Sends name }
        Text.Player := gPlayerNumber;
        Text.Text := gPlayers[gPlayerNumber].Name;
        SendDataType(dPlayerName, Socket);
        Socket.SendBuf(Text, SizeOf(TText));
      end;  
      dPlayerName : begin
        Socket.ReceiveBuf(Text, SizeOf(TText));
        gDataType := dNone;
        gPlayers[Text.Player].Name := Text.Text;
      end;
      dPlayerReady : begin
        Socket.ReceiveBuf(PlayerNumber, SizeOf(Integer));
        gDataType := dNone;
        gPlayers[PlayerNumber].Ready := True;
      end;
      dAction : begin
        Socket.ReceiveBuf(Action, SizeOf(TAction));
        gDataType := dNone;
        case Action of
          aMoveLeft: MoveLeft;
          aMoveRight: MoveRight;
          aRotateUp: RotateUp;
          aRotateDown: RotateDown;
          aDrop: Drop;
        end;
      end;  
      dBlock : begin
        Socket.ReceiveBuf(Block, SizeOf(TBlock));
        gDataType := dNone;
        gCube1 := Block.Cube1; 
        gCube2 := Block.Cube2;
        gCube3 := Block.Cube3;
      end;
      dText : begin
        Socket.ReceiveBuf(Text, SizeOf(TText));
        gDataType := dNone;
        if Assigned(gOnPlayerChat) then
          gOnPlayerChat(Self, gPlayers[Text.Player].Name, Text.Text);
      end;
    end;
  except
    Stop;
    SendState(sDisconnected, Socket);
    gClientSocket.Close;
    if Assigned(gOnSocketError) then
      gOnSocketError(Self);
  end;
end;

end.
