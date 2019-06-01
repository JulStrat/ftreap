{$mode delphi}
unit treap;

interface

uses Classes, SysUtils, rheap;

type
  TTreap<T> = class(TRandomHeap)
  public

  type
    PTreapNode = ^TreapNode;
    TreapNode = object(TRandomHeapNode)
      FKey: T;
    end;
  var
    FRoot: PTreapNode;

    constructor Create;
    destructor Destroy; override;

    class function CreateNode(k: T): PTreapNode;
    class procedure DivideRight(node: PTreapNode; k: T; var l, r: PTreapNode);
    class procedure DivideLeft(node: PTreapNode; k: T; var l, r: PTreapNode);

    class procedure DestroyNode(node: PTreapNode);
    class procedure DestroyTreap(node: PTreapNode);

    class function CheckStucture(node: PTreapNode): boolean;

    procedure Insert(k: T);
    function Contains(k: T): boolean;

    function BisectLeft(k: T): SizeInt;
    function BisectRight(k: T): SizeInt;

    function GetPosition(k: T): SizeInt;
    function GetAt(pos: SizeInt): T;

    function Min: T;
    function Max: T;

    function Remove(k: T): boolean;
    function RemoveAt(pos: SizeInt): T;
    //class procedure PostUpdate(const node: PTreapNode); override;

  end;

implementation
//
// TTreapNode Class methods
//

constructor TTreap<T>.Create();
begin
  inherited Create;
  FRoot := nil;
end;

class function TTreap<T>.CreateNode(k: T): PTreapNode;
var
  node: PTreapNode;
begin
  node := New(PTreapNode);
  node.FPriority := Random;
  node.FLeft := nil;
  node.FRight := nil;
  node.FSize := 1;
  node.FKey := k;
  Result := node;
end;

class procedure TTreap<T>.DestroyNode(node: PTreapNode);
begin
  node.FLeft := nil;
  node.FRight := nil;
  node.FSize := 0;
  Dispose(node);
end;

class procedure TTreap<T>.DestroyTreap(node: PTreapNode);
begin
  if node <> nil then
  begin
    DestroyTreap(PTreapNode(node.FLeft));
    DestroyTreap(PTreapNode(node.FRight));
    //WriteLn('Destroying Node - ', node.FKey);
    DestroyNode(node);
  end;
end;

destructor TTreap<T>.Destroy;
begin
  DestroyTreap(FRoot);
  FRoot := nil;
  inherited;
end;

class procedure TTreap<T>.DivideRight(node: PTreapNode; k: T; var l, r: PTreapNode);
begin
  if node = nil then
  begin
    l := nil;
    r := nil;
    Exit;
  end;
  if k < node.FKey then
  begin
    DivideRight(PTreapNode(node.FLeft), k, l, PTreapNode(node.FLeft));
    r := node;
  end
  else
  begin
    DivideRight(PTreapNode(node.FRight), k, PTreapNode(node.FRight), r);
    l := node;
  end;
  UpdateSize(node);
end;

class procedure TTreap<T>.DivideLeft(node: PTreapNode; k: T; var l, r: PTreapNode);
begin
  if node = nil then
  begin
    l := nil;
    r := nil;
    Exit;
  end;
  if k > node.FKey then
  begin
    DivideLeft(PTreapNode(node.FRight), k, PTreapNode(node.FRight), r);
    l := node;
  end
  else
  begin
    DivideLeft(PTreapNode(node.FLeft), k, l, PTreapNode(node.FLeft));
    r := node;
  end;
  UpdateSize(node);
end;

procedure TTreap<T>.Insert(k: T);
var
  l, r: PTreapNode;
begin
  DivideRight(FRoot, k, l, r);
  l := PTreapNode(Meld(l, CreateNode(k)));
  FRoot := PTreapNode(Meld(l, r));
end;

// PASSED
function TTreap<T>.Contains(k: T): boolean;
var
  node: PTreapNode;
begin
  node := FRoot;
  while node <> nil do
  begin
    if k = node.FKey then
      Exit(True);
    if k > node.FKey then
      node := PTreapNode(node.FRight)
    else
      node := PTreapNode(node.FLeft);
  end;
  Exit(False);
end;

function TTreap<T>.BisectLeft(k: T): SizeInt;
var
  node: PTreapNode;
  pos: SizeInt = 0;
begin
  node := FRoot;
  while node <> nil do
  begin
    if k > node.FKey then
    begin
      pos := pos + GetSize(node.FLeft) + 1;
      node := PTreapNode(node.FRight);
    end
    else
      node := PTreapNode(node.FLeft);
  end;
  Exit(pos);
end;

function TTreap<T>.BisectRight(k: T): SizeInt;
var
  node: PTreapNode;
  pos: SizeInt = 0;
begin
  node := FRoot;
  while node <> nil do
  begin
    if k < node.FKey then
      node := PTreapNode(node.FLeft)
    else
    begin
      pos := pos + GetSize(node.FLeft) + 1;
      node := PTreapNode(node.FRight);
    end;
  end;
  Exit(pos);
end;

// PASSED
function TTreap<T>.GetPosition(k: T): SizeInt;
var
  node: PTreapNode;
  pos: SizeUInt = 0;
begin
  node := FRoot;
  while node <> nil do
  begin
    if k = node.FKey then
      Exit(pos + GetSize(node.FLeft));
    if k > node.FKey then
    begin
      pos := pos + GetSize(node.FLeft) + 1;
      node := PTreapNode(node.FRight);
    end
    else
      node := PTreapNode(node.FLeft);
  end;
  Exit(-1);
end;

// PASSED
function TTreap<T>.GetAt(pos: SizeInt): T;
var
  node: PTreapNode;
begin
  node := PTreapNode(KthLeaf(FRoot, pos));
  if (node = nil) then
    Result := Default(T)
  else
    Result := node.FKey;
end;

function TTreap<T>.Min: T;
var
  node: PTreapNode;
begin
  node := PTreapNode(FirstLeaf(FRoot));
  if (node = nil) then
    Result := Default(T)
  else
    Result := node.FKey;
end;

function TTreap<T>.Max: T;
var
  node: PTreapNode;
begin
  node := PTreapNode(LastLeaf(FRoot));
  if (node = nil) then
    Result := Default(T)
  else
    Result := node.FKey;
end;

function TTreap<T>.Remove(k: T): boolean;
var
  l, m, r: PTreapNode;
begin
  DivideLeft(FRoot, k, l, r);
  DivideRight(r, k, m, r);
  DestroyTreap(m);
  FRoot := PTreapNode(Meld(l, r));
  Result := True;
end;

(*
class procedure TTreapNode.PostUpdate(const node: TRandomHeapNode);
begin
  //WriteLn('Call from treap - ', TTreapNode(node).FKey);
end;
*)

// RWRT
function TTreap<T>.RemoveAt(pos: SizeInt): T;
var
  l, m, r: PTreapNode;
begin
  DivideAt(FRoot, pos, l, r);
  DivideAt(r, 1, m, r);
  Result := m.FKey;
  FRoot := PTreapNode(Meld(l, r));
end;

(*
class procedure TTreap<T>.DestroyTreap(var node: TTreapNode);
begin
  if node <> nil then
  begin
    DestroyTreap(TTreapNode(node.FLeft));
    DestroyTreap(TTreapNode(node.FRight));
    FreeAndNil(node);
  end;
end;
*)

class function TTreap<T>.CheckStucture(node: PTreapNode): boolean;
begin
  Result := True;
  if node = nil then
    Exit(Result);

  Result := Result and CheckStucture(PTreapNode(node.FLeft));
  Result := Result and CheckStucture(PTreapNode(node.FRight));
  Result := Result and (GetSize(node) = GetSize(node.FLeft) +
    GetSize(node.FRight) + 1);
  if node.FLeft <> nil then
  begin
    Result := Result and (node.FPriority >= node.FLeft.FPriority);
    Result := Result and ((node.FKey >= PTreapNode(node.FLeft).FKey));
  end;
  if node.FRight <> nil then
  begin
    Result := Result and (node.FPriority >= node.FRight.FPriority);
    Result := Result and (node.FKey < PTreapNode(node.FRight).FKey);
  end;

end;

initialization
  Randomize;
end.
