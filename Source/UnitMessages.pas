(* QuaBlocks *******************************************************)
(* Copyright (c)2005-2006 Marc-Andre Lamothe ***********************)
(* All rights reserved *********************************************)

unit UnitMessages;

interface
  uses Forms, Windows, Classes, SysUtils, StrUtils2;
                  
  procedure ShowError (Code : Integer; Param : String = '');   
  procedure ShowMessage(Code : Integer; Param : String = ''); 
  function AskQuestion (Code : Integer; Param : String = ''; Cancel : Boolean = False) : Integer;

var
  MsgStr : TStringList;

implementation

procedure ShowError(Code : Integer; Param : String = '');
begin
  if Code in [1..6] then
    Application.MessageBox(PChar(ReplaceAll2(MsgStr.Values['E'+IntToStr(Code)], ';', Param)), PChar(MsgStr.Values['E0']), mb_Ok);
end;
                  
procedure ShowMessage(Code : Integer; Param : String = '');
begin
  if Code in [1..4] then
    Application.MessageBox(PChar(ReplaceAll2(MsgStr.Values['M'+IntToStr(Code)], ';', Param)), PChar(MsgStr.Values['M0']), mb_Ok);
end;

function AskQuestion(Code : Integer; Param : String = ''; Cancel : Boolean = False) : Integer;
begin
  Result := 0;
  if Code in [1..2] then
    if Cancel then
      Result := Application.MessageBox(PChar(ReplaceAll2(MsgStr.Values['Q'+IntToStr(Code)], ';', Param)), PChar(MsgStr.Values['Q0']), mb_YesNoCancel)
    else
      Result := Application.MessageBox(PChar(ReplaceAll2(MsgStr.Values['Q'+IntToStr(Code)], ';', Param)), PChar(MsgStr.Values['Q0']), mb_YesNo);
end;

initialization
  MsgStr := TStringList.Create;
  MsgStr.Add('E0=Error');
  MsgStr.Add('E1=Cannot connect to the remote game!');
  MsgStr.Add('E2=An error occured on the network! The connection was terminated.');
  MsgStr.Add('E3=Could not add/remove the game in the list on the server!');
  MsgStr.Add('E4=You must enter your name(s)!');
  MsgStr.Add('E5=You must enter the IP address of the host computer to connect to!');
  MsgStr.Add('E6=Cannot retreive the games list from the server!');
  MsgStr.Add('M0=Message');
  MsgStr.Add('M1=You already have the latest version of AlphaChess, no update is necessary.');
  MsgStr.Add('M2=Your opponent disconnected from the game.');
  MsgStr.Add('M3=The connection to the game has been lost.');
  MsgStr.Add('M4=; has won the game!');
  MsgStr.Add('Q0=Question');
  MsgStr.Add('Q1=A new version of AlphaChess is availlable! Do you want to download it now?');
  MsgStr.Add('Q2=A game is in progress, do you really want to quit?');

finalization
  MsgStr.Free;

end.

