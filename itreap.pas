unit itreap;
{$ifdef FPC}
{$mode delphi}
{$endif}

interface
uses
  // Classes, SysUtils,
  rheap;

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
    function Select(pos: SizeInt): T;
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
end;

procedure TImplicitTreap<T>.InsertAt(pos: SizeInt; v: T);
var
  l, r: PImplicitTreapNode;
begin
  DivideAt(FRoot, pos, l, r);
  FRoot := PImplicitTreapNode(Meld(l, Meld(CreateNode(v), r)))
end;

function TImplicitTreap<T>.Select(pos: SizeInt): T;
var
  node: PImplicitTreapNode;
begin
  node := PImplicitTreapNode(KthNode(FRoot, pos));
  if (node <> nil) then
    Result := node.FValue
  else
    Result := Default(T);
end;

// RWRT ?
function TImplicitTreap<T>.RemoveAt(pos: SizeInt): T;
var
  l, m, r: PImplicitTreapNode;
begin
  DivideAt(FRoot, pos, l, r);
  DivideAt(r, 1, m, r);
  if (m <> nil) then
    Result := m.FValue
  else
    Result := Default(T);
  DestroyTreap(PImplicitTreapNode(m));
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
