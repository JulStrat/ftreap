//{$mode objfpc}{$H+}{$J-}
{$mode delphi}
{$ASSERTIONS ON}
{$warnings on}
{$hints on}
{$R+}{$Q+}

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

    //constructor Create(const k: T);
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

    //class function Min(node: PTreapNode): T;
    //class function Max(node: PTreapNode): T;

    function Remove(k: T): boolean;
    function RemoveAt(pos: SizeInt): T;
    //class procedure PostUpdate(const node: PTreapNode); override;
    destructor Destroy; override;

  end;

implementation
//
// TTreapNode Class methods
//
(*
constructor TTreap<T>.Create(const k: T);
begin
  inherited Create;
  FKey := k;
end;
*)

class function TTreap<T>.CreateNode(k: T): PTreapNode;
var
  node: PTreapNode;
begin
  node := New(PTreapNode);
  node.FPriority := Random;
  node.FSize := 1;
  node.FKey := k;
  node.FLeft := nil;
  node.FRight := nil;
  Result := node;
end;

class procedure TTreap<T>.DestroyNode(node: PTreapNode);
begin
  node.FLeft := nil;
  node.FRight := nil;
  Dispose(node);
end;

class procedure TTreap<T>.DestroyTreap(node: PTreapNode);
begin
  if node <> nil then
  begin
    DestroyTreap(PTreapNode(node.FLeft));
    DestroyTreap(PTreapNode(node.FRight));
    DestroyNode(node);
  end;
end;

destructor TTreap<T>.Destroy;
begin
  DestroyTreap(FRoot);
  //inherited;
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
  l: PTreapNode = nil;
  r: PTreapNode = nil;
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
  raise Exception.Create('No such key');
end;

// PASSED
function TTreap<T>.GetAt(pos: SizeInt): T;
var
  node: PTreapNode;
  lsize: SizeInt = 0;
begin
  node := FRoot;
  if (node = nil) or (pos > GetSize(node) - 1) then
    raise EArgumentException.Create('Set is empty or position is out of range.');
  while node <> nil do
  begin
    lsize := GetSize(node.FLeft);
    if pos = lsize then
      Exit(node.FKey);
    if pos > lsize then
    begin
      node := PTreapNode(node.FRight);
      pos := pos - lsize - 1;
    end
    else
      node := PTreapNode(node.FLeft);
  end;
  raise Exception.Create('Unreachable point.');
end;

(*
class function TTreap<T>.Min(node: TTreapNode): T;
begin
  if node = nil then
    raise EArgumentException.Create('Set is empty.');
  Exit(TTreapNode(FirstNode(node)).FKey);
end;

class function TTreap<T>.Max(node: TTreapNode): T;
begin
  if node = nil then
    raise EArgumentException.Create('Set is empty.');
  Exit(TTreapNode(LastNode(node)).FKey);
end;
*)

function TTreap<T>.Remove(k: T): boolean;
var
  l: PTreapNode = nil;
  r: PTreapNode = nil;
  m: PTreapNode = nil;

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
  l: PTreapNode = nil;
  r: PTreapNode = nil;
  m: PTreapNode = nil;

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
