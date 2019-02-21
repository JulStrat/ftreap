unit ftreap;

{$mode objfpc}{$H+}{$J-}

// uses Classes, SysUtils;

type
  generic TTreapNode<T> = class
  public
    left: TTreapNode;
    right: TTreapNode;
    size: SizeUInt;
    rank: extended;
    val: T;
    constructor Create(v: T);
    class function GetSize(node: TTreapNode): SizeUInt; inline; static;
    class procedure UpdateSize(node: TTreapNode); inline; static;
    class function Meld(l: TTreapNode; r: TTreapNode): TTreapNode; static;
    class procedure DivideAt(treap: TTreapNode; pos: SizeUInt;
      var l: TTreapNode; var r: TTreapNode); static;
    class procedure Divide(treap: TTreapNode; v: T; var l: TTreapNode;
      var r: TTreapNode); static;

    class function Append(root: TTreapNode; v: T): TTreapNode; static;
    class function Insert(root: TTreapNode; v: T): TTreapNode; static;
    class function DeleteAt(var root: TTreapNode; pos: SizeUInt): T; static;
  end;

implementation

constructor TTreapNode.Create(v: T);
begin
  left := nil;
  right := nil;
  size := 1;
  val := v;
  rank := random;
end;

class function TTreapNode.GetSize(node: TTreapNode): SizeUInt; inline; static;
begin
  if node <> nil then
    Result := node.size
  else
    Result := 0;
end;

class procedure TTreapNode.UpdateSize(node: TTreapNode); inline; static;
begin
  if node <> nil then
    node.size := GetSize(node.left) + GetSize(node.right) + 1;
end;

class function TTreapNode.Meld(l: TTreapNode; r: TTreapNode): TTreapNode; static;
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
  var l: TTreapNode; var r: TTreapNode); static;
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
  var r: TTreapNode); static;
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

class function TTreapNode.Append(root: TTreapNode; v: T): TTreapNode; static;
begin
  Result := Meld(root, TTreapNode.Create(v));
end;

class function TTreapNode.Insert(root: TTreapNode; v: T): TTreapNode; static;
var
  l, r: TTreapNode;
begin
  Divide(root, v, l, r);
  Result := Meld(l, Meld(TTreapNode.Create(v), r));
end;

class function TTreapNode.DeleteAt(var root: TTreapNode; pos: SizeUInt): T; static;
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
