unit ftreap;

{$mode objfpc}{$H+}{$J-}

interface

// uses Classes, SysUtils;

type
  generic TTreapNode<T> = class
  public
    val: T;
    rank: extended;
    size: SizeUInt;
    left: TTreapNode;
    right: TTreapNode;
    constructor Create(v: T);
    class function GetSize(node: TTreapNode): SizeUInt; inline;
    class procedure UpdateSize(node: TTreapNode); inline;
    class function Meld(l: TTreapNode; r: TTreapNode): TTreapNode;
    class procedure DivideAt(treap: TTreapNode; pos: SizeUInt;
      var l: TTreapNode; var r: TTreapNode);
    class procedure Divide(treap: TTreapNode; v: T; var l: TTreapNode;
      var r: TTreapNode);

    class function Append(root: TTreapNode; v: T): TTreapNode; inline;
    class function Insert(root: TTreapNode; v: T): TTreapNode;
    class function DeleteAt(var root: TTreapNode; pos: SizeUInt): T;
  end;

implementation

constructor TTreapNode.Create(v: T);
begin
  val := v;
  rank := random;
  size := 1;
  left := nil;
  right := nil;
end;

class function TTreapNode.GetSize(node: TTreapNode): SizeUInt; inline;
begin
  if node <> nil then
    Result := node.size
  else
    Result := 0;
end;

class procedure TTreapNode.UpdateSize(node: TTreapNode); inline;
begin
  if node <> nil then
    node.size := GetSize(node.left) + GetSize(node.right) + 1;
end;

class function TTreapNode.Meld(l: TTreapNode; r: TTreapNode): TTreapNode;
begin
  if l = nil then
    exit(r)
  else if r = nil then
    exit(l)
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

class procedure TTreapNode.DivideAt(treap: TTreapNode; pos: SizeUInt;
  var l: TTreapNode; var r: TTreapNode);
begin
  if treap = nil then
  begin
    l := nil;
    r := nil;
    exit;
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

class procedure TTreapNode.Divide(treap: TTreapNode; v: T; var l: TTreapNode;
  var r: TTreapNode);
begin
  if treap = nil then
  begin
    l := nil;
    r := nil;
    exit;
  end;
  if v < treap.val then
  begin
    Divide(treap.left, v, l, treap.left);
    r := treap;
  end
  else
  begin
    Divide(treap.right, v, treap.right, r);
    l := treap;
  end;
  UpdateSize(treap);
end;

class function TTreapNode.Append(root: TTreapNode; v: T): TTreapNode; inline;
begin
  Result := Meld(root, TTreapNode.Create(v));
end;

class function TTreapNode.Insert(root: TTreapNode; v: T): TTreapNode; inline;
var
  l, r: TTreapNode;
begin
  Divide(root, v, l, r);
  Result := Meld(l, Meld(TTreapNode.Create(v), r));
end;

class function TTreapNode.DeleteAt(var root: TTreapNode; pos: SizeUInt): T;
begin
  if pos < GetSize(root.left) then
  begin
    Result := DeleteAt(root.left, pos);
    UpdateSize(root);
  end
  else if pos > GetSize(root.left) then
  begin
    pos := pos - GetSize(root.left) - 1;
    Result := DeleteAt(root.right, pos);
    UpdateSize(root);
  end
  else
  begin
    Result := root.val;
    root := Meld(root.left, root.right);
  end;
end;


end.
