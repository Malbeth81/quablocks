unit UnitScores;

interface
type
PScoreInfo = ^TScoreInfo;
TScoreInfo = record
  Value,
  Life,
  X, Y : Integer;
  Prev, Next : PScoreInfo;
end;

type TScoreList = class(TObject)
private
  lFirst,
  lLast : PScoreInfo;
  lSize : Word;
public
  constructor Create;
  destructor Destroy; override;

  function Get(Index : Word) : PScoreInfo;
  procedure Add(Value, X, Y : Integer);
  procedure Remove(Index : Word); overload;
  function Remove(Ptr : PScoreInfo) : PScoreInfo; overload;  
  procedure Clear;

  property First : PScoreInfo read lFirst;
  property Last : PScoreInfo read lLast;
  property Size : Word read lSize;
end;

implementation
       
constructor TScoreList.Create;
begin
  lFirst := nil;
  lLast := nil;
  lSize := 0;
end;

destructor TScoreList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TScoreList.Get(Index : Word) : PScoreInfo;
var
  i : Word;
begin              
  if Index in [0..lSize-1] then
  begin
    Result := lFirst;
    if Index > 0 then
      for i := 0 to Index-1 do
        Result := Result.Next;
  end
  else
    Result := nil;
end;
   
procedure TScoreList.Add(Value, X, Y : Integer);
var
  ScoreInfo : PScoreInfo;
begin
  try
    New(ScoreInfo);
    ScoreInfo.Life := 0;
    ScoreInfo.Value := Value;
    ScoreInfo.X := X;
    ScoreInfo.Y := Y;
    ScoreInfo.Prev := lLast;
    ScoreInfo.Next := nil;
    if lLast <> nil then
      lLast.Next := ScoreInfo
    else
      lFirst := ScoreInfo;
    lLast := ScoreInfo;
    lSize := lSize +1;
  except
  end;
end;

procedure TScoreList.Remove(Index : Word);
begin
  Remove(Get(Index));
end;
        
function TScoreList.Remove(Ptr : PScoreInfo) : PScoreInfo;
begin
  Result := nil;
  if Ptr <> nil then
  begin
    if Ptr.Prev = nil then
      lFirst := Ptr.Next
    else
      Ptr.Prev.Next := Ptr.Next;
    if Ptr.Next = nil then
      lLast := Ptr.Prev
    else
      Ptr.Next.Prev := Ptr.Prev;
    Result := Ptr.Next;
    Dispose(Ptr);
    lSize := lSize -1;
  end;
end;

procedure TScoreList.Clear;
var
  Ptr : PScoreInfo;
begin
  while lFirst <> nil do
  begin
    Ptr := lFirst;
    lFirst := Ptr.Next;
    Dispose(Ptr);
  end;
  lLast := nil;
  lSize := 0;
end;

end.
