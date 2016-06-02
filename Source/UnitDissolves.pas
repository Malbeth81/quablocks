unit UnitDissolves;

interface
type
PDissolveInfo = ^TDissolveInfo;
TDissolveInfo = record
  Life,
  X, Y : Integer;
  Prev, Next : PDissolveInfo;
end;

type TDissolveList = class(TObject)
private
  lFirst,
  lLast : PDissolveInfo;
  lSize : Word;
public
  constructor Create;
  destructor Destroy; override;

  function Get(Index : Word) : PDissolveInfo;
  procedure Add(X, Y : Integer);
  procedure Remove(Index : Word); overload;
  function Remove(Ptr : PDissolveInfo) : PDissolveInfo; overload;  
  procedure Clear;

  property First : PDissolveInfo read lFirst;
  property Last : PDissolveInfo read lLast;
  property Size : Word read lSize;
end;

implementation
       
constructor TDissolveList.Create;
begin
  lFirst := nil;
  lLast := nil;
  lSize := 0;
end;

destructor TDissolveList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TDissolveList.Get(Index : Word) : PDissolveInfo;
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
   
procedure TDissolveList.Add(X, Y : Integer);
var
  DissolveInfo : PDissolveInfo;
begin
  try
    New(DissolveInfo);
    DissolveInfo.Life := 0;
    DissolveInfo.X := X;
    DissolveInfo.Y := Y;
    DissolveInfo.Prev := lLast;
    DissolveInfo.Next := nil;
    if lLast <> nil then
      lLast.Next := DissolveInfo
    else
      lFirst := DissolveInfo;
    lLast := DissolveInfo;
    lSize := lSize +1;
  except
  end;
end;

procedure TDissolveList.Remove(Index : Word);
begin
  Remove(Get(Index));
end;
        
function TDissolveList.Remove(Ptr : PDissolveInfo) : PDissolveInfo;
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

procedure TDissolveList.Clear;
var
  Ptr : PDissolveInfo;
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
