{$mode objfpc}{$H+}{$J-}
{$warnings on}
{$hints on}

unit ftreap;

interface

type
  generic TTreapNode<T> = class
  private
    key: T;
    rank: extended;
    size: SizeUInt;
    left: TTreapNode;
    right: TTreapNode;
  public
    constructor Create(const k: T);
    class function GetSize(const node: TTreapNode): SizeUInt; static; inline;
    class procedure UpdateSize(const node: TTreapNode); static; inline;

    class function Meld(l: TTreapNode; r: TTreapNode): TTreapNode; static;
    class procedure Divide(treap: TTreapNode; k: T; var l: TTreapNode;
      var r: TTreapNode); static;
    class procedure DivideAt(treap: TTreapNode; const pos: SizeUInt;
      var l: TTreapNode; var r: TTreapNode); static;

    class function Append(root: TTreapNode; const k: T): TTreapNode; static;
    class function Insert(root: TTreapNode; const k: T): TTreapNode; static;
    class function Find(root: TTreapNode; const k: T): boolean; static;
    class function Min(root: TTreapNode): T; static;
    class function Max(root: TTreapNode): T; static;
    class function DeleteAt(var root: TTreapNode; const pos: SizeUInt): T; static;

    class function CheckStucture(root: TTreapNode): boolean; static;
  end;

implementation

constructor TTreapNode.Create(const k: T);
begin
  key := k;
  rank := Random;
  size := 1;
  left := nil;
  right := nil;
end;

class function TTreapNode.GetSize(const node: TTreapNode): SizeUInt; static; inline;
begin
  if node <> nil then
    Exit(node.size);
  Exit(0);
end;

class procedure TTreapNode.UpdateSize(const node: TTreapNode); static; inline;
begin
  if node <> nil then
    node.size := GetSize(node.left) + GetSize(node.right) + 1;
end;

class function TTreapNode.Meld(l: TTreapNode; r: TTreapNode): TTreapNode; static;
begin
  if l = nil then
    Exit(r)
  else if r = nil then
    Exit(l)
  else if l.rank > r.rank then
  begin
    l.right := Meld(l.right, r);
    UpdateSize(l);
    Result := l;
  end
  else
  begin
    r.left := Meld(l, r.left);
    UpdateSize(r);
    Result := r;
  end;
end;

class procedure TTreapNode.DivideAt(treap: TTreapNode; const pos: SizeUInt;
  var l: TTreapNode; var r: TTreapNode); static;
begin
  if treap = nil then
  begin
    l := nil;
    r := nil;
    Exit;
  end;

  if pos <= GetSize(treap.left) then
  begin
    DivideAt(treap.left, pos, l, treap.left);
    r := treap;
  end
  else
  begin
    DivideAt(treap.right, pos - GetSize(treap.left) - 1, treap.right, r);
    l := treap;
  end;
  UpdateSize(treap);
end;

class procedure TTreapNode.Divide(treap: TTreapNode; k: T; var l: TTreapNode;
  var r: TTreapNode); static;
begin
  if treap = nil then
  begin
    l := nil;
    r := nil;
    Exit;
  end;
  if k < treap.key then
  begin
    Divide(treap.left, k, l, treap.left);
    r := treap;
  end
  else
  begin
    Divide(treap.right, k, treap.right, r);
    l := treap;
  end;
  UpdateSize(treap);
end;

class function TTreapNode.Append(root: TTreapNode; const k: T): TTreapNode;
  static; inline;
begin
  Result := Meld(root, TTreapNode.Create(k));
end;

class function TTreapNode.Insert(root: TTreapNode; const k: T): TTreapNode;
  static; inline;
var
  l, r: TTreapNode;
begin
  Divide(root, k, l, r);
  Result := Meld(l, Meld(TTreapNode.Create(k), r));
end;

class function TTreapNode.Find(root: TTreapNode; const k: T): boolean; static;
begin
  // Result := False;
  while root <> nil do
  begin
    if root.key < k then
      root := root.right
    else if root.key > k then
      root := root.left
    else
      Exit(True);
  end;
  Exit(False);
end;

class function TTreapNode.Min(root: TTreapNode): T; static;
begin
  Assert(root <> nil);
  while root.left <> nil do
    root := root.left;
  Exit(root.key);
end;

class function TTreapNode.Max(root: TTreapNode): T; static;
begin
  Assert(root <> nil);
  while root.right <> nil do
    root := root.right;
  Exit(root.key);
end;


class function TTreapNode.DeleteAt(var root: TTreapNode; const pos: SizeUInt): T; static;
begin
  Assert(root <> nil);
  Assert(pos < GetSize(root));

  with root do
  begin
    if pos < GetSize(left) then
    begin
      Result := DeleteAt(left, pos);
      UpdateSize(root);
    end
    else if pos > GetSize(left) then
    begin
      Result := DeleteAt(right, pos - GetSize(left) - 1);
      UpdateSize(root);
    end
    else
    begin
      Result := key;
      root := Meld(left, right);
    end;
  end;
end;

class function TTreapNode.CheckStucture(root: TTreapNode): boolean; static;
begin
  Result := True;
  if root = nil then
    Exit(Result);
  with root do
  begin
    Result := Result and CheckStucture(left);
    Result := Result and CheckStucture(right);
    Result := Result and (GetSize(root) = GetSize(left) + GetSize(right) + 1);
    if left <> nil then
    begin
      Result := Result and (rank >= left.rank);
      Result := Result and (key >= left.key);
    end;
    if right <> nil then
    begin
      Result := Result and (rank >= right.rank);
      Result := Result and (key < right.key);
    end;
  end;
end;

end.
