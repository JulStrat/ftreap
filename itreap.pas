unit itreap;
{$ifdef FPC}
{$mode delphi}
{$endif}

interface
uses
  Classes, SysUtils, rheap;

type

TImplicitTreap<T> = class(TRandomHeap)
  type
    PImplicitTreapNode = ^TImplicitTreapNode;
    TImplicitTreapNode = object(TRandomHeapNode)
      FValue: T;
    end;
  var
    FRoot: PImplicitTreapNode;

    constructor Create;
    destructor Destroy; override;

    class function CreateNode(v: T): PImplicitTreapNode;
    class procedure DestroyNode(node: PImplicitTreapNode);
    class procedure DestroyTreap(node: PImplicitTreapNode);
    class function CheckStucture(node: PImplicitTreapNode): boolean;

    procedure InsertAt(pos: SizeInt; v: T);
    function GetAt(pos: SizeInt): T;
    function RemoveAt(pos: SizeInt): T;

end;

implementation

constructor TImplicitTreap<T>.Create();
begin
  inherited Create;
  FRoot := nil;
end;

class function TImplicitTreap<T>.CreateNode(v: T): PImplicitTreapNode;
var
  node: PImplicitTreapNode;
begin
  node := New(PImplicitTreapNode);
  node.FPriority := Random;
  node.FLeft := nil;
  node.FRight := nil;
  node.FSize := 1;
  node.FValue := v;
  Result := node;
end;

class procedure TImplicitTreap<T>.DestroyNode(node: PImplicitTreapNode);
begin
  node.FLeft := nil;
  node.FRight := nil;
  node.FSize := 0;
  node.FValue := Default(T);
  Dispose(node);
end;

class procedure TImplicitTreap<T>.DestroyTreap(node: PImplicitTreapNode);
begin
  if node <> nil then
  begin
    DestroyTreap(PImplicitTreapNode(node.FLeft));
    DestroyTreap(PImplicitTreapNode(node.FRight));
    DestroyNode(node);
  end;
end;

destructor TImplicitTreap<T>.Destroy;
begin
  DestroyTreap(FRoot);
  FRoot := nil;
  inherited;
  //inherited;
end;

procedure TImplicitTreap<T>.InsertAt(pos: SizeInt; v: T);
var
  l, r: PImplicitTreapNode;
begin
  DivideAt(FRoot, pos, PImplicitTreapNode(l), PImplicitTreapNode(r));
  FRoot := PImplicitTreapNode(Meld(l, Meld(CreateNode(v), r)))
end;

// PASSED
function TImplicitTreap<T>.GetAt(pos: SizeInt): T;
var
  lsize: SizeInt = 0;
  node: PImplicitTreapNode;
begin
  node := FRoot;
  if (node = nil) or (pos > GetSize(node) - 1) then
    raise EArgumentException.Create('Set is empty or position is out of range.');
  node := FRoot;
  while node <> nil do
  begin
    lsize := GetSize(node.FLeft);
    if pos = lsize then
      Exit(node.FValue);
    if pos > lsize then
    begin
      node := PImplicitTreapNode(node.FRight);
      pos := pos - lsize - 1
    end
    else
      node := PImplicitTreapNode(node.FLeft)
  end;
  raise Exception.Create('Unreachable point.')
end;

// RWRT
function TImplicitTreap<T>.RemoveAt(pos: SizeInt): T;
var
  l: PImplicitTreapNode = nil;
  r: PImplicitTreapNode = nil;
  m: PImplicitTreapNode = nil;

begin
  DivideAt(FRoot, pos, l, r);
  DivideAt(r, 1, m, r);
  Result := m.FValue;
  FRoot := PImplicitTreapNode(Meld(l, r));
end;

class function TImplicitTreap<T>.CheckStucture(node: PImplicitTreapNode): boolean;
begin
  Result := True;
  if node = nil then
    Exit(Result);


    Result := Result and CheckStucture(PImplicitTreapNode(node.FLeft));
    Result := Result and CheckStucture(PImplicitTreapNode(node.FRight));
    Result := Result and (GetSize(node) = GetSize(node.FLeft) +
      GetSize(node.FRight) + 1);
    if node.FLeft <> nil then
      Result := Result and (node.FPriority >= node.FLeft.FPriority);
    if node.FRight <> nil then
      Result := Result and (node.FPriority >= node.FRight.FPriority);
end;

end.
