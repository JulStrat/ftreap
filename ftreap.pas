{$mode objfpc}{$H+}{$J-}
{$warnings on}
{$hints on}

unit ftreap;

interface

type
  generic TTreapNode<T> = class
  private
    // Key
    key: T;
    // Random heap prority
    priority: extended;
    // Number of nodes in subtree
    size: SizeUInt;
    // Left subtree reference
    left: TTreapNode;
    // Right subtree reference
    right: TTreapNode;
  public
    (* Tree node constructor. *)
    constructor Create(const k: T);
    (* Tree node destructor. *)
    destructor Destroy; override;

    (* Returns number of keys in the tree rooted at @code(node). *)
    class function GetSize(const node: TTreapNode): SizeUInt; inline;
    (* Recalculates number of keys in the tree rooted at @code(node) after insert, delete operations. *)
    class procedure UpdateSize(const node: TTreapNode); inline;

    class procedure RemoveNode(var node: TTreapNode); inline;

    (* Creates new tree from two trees, where @code(Min(r) >= Max(l)). *)
    class function Meld(l, r: TTreapNode): TTreapNode;
    (* Divides tree into two trees. Where @code(Max(l) <= k). *)
    class procedure Divide(node: TTreapNode; k: T; var l, r: TTreapNode);
    (* Divides tree into two trees. Where @code(Size(l) = pos + 1). *)
    class procedure DivideAt(node: TTreapNode; const pos: SizeUInt; var l, r: TTreapNode);

    (* Returns @true if tree rooted at @code(node) is empty, @false otherwise *)
    class function IsEmpty(const node: TTreapNode): boolean; inline;

    class function Append(node: TTreapNode; const k: T): TTreapNode;

    (* Insert key @code(k) in tree rooted at @code(node). *)
    class function Insert(var node: TTreapNode; const k: T): boolean; inline;
    (* Check if tree rooted at @code(root) node contains key @code(k). *)
    class function Contains(node: TTreapNode; const k: T): boolean;

    class function GetPosition(node: TTreapNode; const k: T): SizeUInt;

    (* @deprecated *)
    class function Min(node: TTreapNode): T;
    (* @deprecated *)
    class function Max(node: TTreapNode): T;

    (* Removes key from the tree.
       @returns(@true if successful, @false otherwise) *)
    class function Remove(var node: TTreapNode; const k: T): boolean;
    (* Removes key from the given position.
       @returns(@true if successful, @false otherwise) *)
    class function RemoveAt(var node: TTreapNode; const pos: SizeUInt): T;

    (* Destroy tree. *)
    class procedure DestroyTreap(var node: TTreapNode);

    class function CheckStucture(node: TTreapNode): boolean;
    class procedure ClassInfo;
    class procedure StuctureInfo(node: TTreapNode);
  end;

implementation

uses SysUtils, TypInfo;

constructor TTreapNode.Create(const k: T);
begin
  key := k;
  priority := Random;
  size := 1;
  left := nil;
  right := nil;
end;

destructor TTreapNode.Destroy;
begin
  left := nil;
  right := nil;
  inherited;
end;

class function TTreapNode.GetSize(const node: TTreapNode): SizeUInt; inline;
begin
  if node <> nil then
    Exit(node.size);
  Exit(0);
end;

class procedure TTreapNode.UpdateSize(const node: TTreapNode); inline;
begin
  if node <> nil then
    node.size := GetSize(node.left) + GetSize(node.right) + 1;
end;

class procedure TTreapNode.RemoveNode(var node: TTreapNode); inline;
var
  n: TTreapNode;
begin
  if node <> nil then
  begin
     n := Meld(node.left, node.right);
     FreeAndNil(node);
     node := n
  end;
end;

class function TTreapNode.IsEmpty(const node: TTreapNode): boolean; inline;
begin
  Exit(GetSize(node) = 0);
end;

class function TTreapNode.Meld(l, r: TTreapNode): TTreapNode;
begin
  if l = nil then Exit(r);
  if r = nil then Exit(l);
  if l.priority > r.priority then
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

class procedure TTreapNode.DivideAt(node: TTreapNode; const pos: SizeUInt;
  var l, r: TTreapNode);
begin
  if node = nil then
  begin
    l := nil;
    r := nil;
    Exit;
  end;

  if pos <= GetSize(node.left) then
  begin
    DivideAt(node.left, pos, l, node.left);
    r := node;
  end
  else
  begin
    DivideAt(node.right, pos - GetSize(node.left) - 1, node.right, r);
    l := node;
  end;
  UpdateSize(node);
end;

class procedure TTreapNode.Divide(node: TTreapNode; k: T; var l, r: TTreapNode);
begin
  if node = nil then
  begin
    l := nil;
    r := nil;
    Exit;
  end;
  if k < node.key then
  begin
    Divide(node.left, k, l, node.left);
    r := node;
  end
  else
  begin
    Divide(node.right, k, node.right, r);
    l := node;
  end;
  UpdateSize(node);
end;

class function TTreapNode.Append(node: TTreapNode; const k: T): TTreapNode;

begin
  Result := Meld(node, TTreapNode.Create(k));
end;

class function TTreapNode.Insert(var node: TTreapNode; const k: T): boolean;
  inline;
var
  l, r: TTreapNode;
begin
  Divide(node, k, l, r);
  node := Meld(l, Meld(TTreapNode.Create(k), r));
  Exit(True);
end;

class function TTreapNode.Contains(node: TTreapNode; const k: T): boolean;
begin
  while node <> nil do
  begin
    if node.key < k then
      node := node.right
    else if node.key > k then
      node := node.left
    else
      Exit(True);
  end;
  Exit(False);
end;

class function TTreapNode.GetPosition(node: TTreapNode; const k: T): SizeUInt;
var
  pos: SizeUInt;
begin
  pos := 0;
  while node <> nil do
  begin
    if node.key < k then
    begin
      pos := pos + GetSize(node.left) + 1;
      node := node.right;
    end
    else if node.key > k then
      node := node.left
    else
      Exit(pos + GetSize(node.left));
  end;
  raise Exception.Create('No such key');
end;

class function TTreapNode.Min(node: TTreapNode): T;
begin
  Assert(node <> nil);
  while node.left <> nil do
    node := node.left;
  Exit(node.key);
end;

class function TTreapNode.Max(node: TTreapNode): T;
begin
  Assert(node <> nil);
  while node.right <> nil do
    node := node.right;
  Exit(node.key);
end;

class function TTreapNode.Remove(var node: TTreapNode; const k: T): boolean;
var
  n: TTreapNode;

begin
  if node <> nil then
  begin
    if k < node.key then
    begin
      Result := Remove(node.left, k);
      if Result then
        UpdateSize(node);
    end
    else if k > node.key then
    begin
      Result := Remove(node.right, k);
      if Result then
        UpdateSize(node);
    end
    else
    begin
      n := node;
      node := Meld(node.left, node.right);
      FreeAndNil(n);
      Exit(True);
    end;
  end;
  Exit(False);
end;

class function TTreapNode.RemoveAt(var node: TTreapNode; const pos: SizeUInt): T;
var
  n: TTreapNode;
begin
  Assert(node <> nil);
  Assert(pos < GetSize(node));

  if pos < GetSize(node.left) then
  begin
    Result := RemoveAt(node.left, pos);
    UpdateSize(node);
  end
  else if pos > GetSize(node.left) then
  begin
    Result := RemoveAt(node.right, pos - GetSize(node.left) - 1);
    UpdateSize(node);
  end
  else
  begin
    Result := node.key;
    n := node;
    node := Meld(node.left, node.right);
    FreeAndNil(n);
  end;

end;

class procedure TTreapNode.DestroyTreap(var node: TTreapNode);
begin
  if node <> nil then
  begin
    DestroyTreap(node.left);
    DestroyTreap(node.right);
    FreeAndNil(node);
  end;
end;

class function TTreapNode.CheckStucture(node: TTreapNode): boolean;
begin
  Result := True;
  if node = nil then
    Exit(Result);
  with node do
  begin
    Result := Result and CheckStucture(node.left);
    Result := Result and CheckStucture(node.right);
    Result := Result and (GetSize(node) = GetSize(node.left) + GetSize(node.right) + 1);
    if node.left <> nil then
    begin
      Result := Result and (node.priority >= node.left.priority);
      Result := Result and ((node.key > node.left.key) or (node.key = node.left.key));
    end;
    if node.right <> nil then
    begin
      Result := Result and (node.priority >= node.right.priority);
      Result := Result and (node.key < node.right.key);
    end;
  end;
end;

class procedure TTreapNode.ClassInfo;
begin
  WriteLn('*** Class Info ***');
  //WriteLn('Default       - ', Default(T));
  WriteLn('TypeInfo      - ', TTypeInfo(TypeInfo(T)^).Name);
  WriteLn('SizeOf        - ', SizeOf(T));
  //WriteLn('IsManagedType - ', IsManagedType(T));
  //WriteLn('HasWeakRef    - ', HasWeakRef(T));
  //WriteLn('GetTypeKind   - ', GetTypeKind(T));
  WriteLn('*** Class Info ***');
end;

class procedure TTreapNode.StuctureInfo(node: TTreapNode);
begin
  if GetSize(node) <> 0 then
    WriteLn('Size - ', GetSize(node), '. Left subtree size - ', GetSize(node.left),
      '. Right subtree size - ', GetSize(node.right), '. Ratio - ',
      GetSize(node.left) / GetSize(node.right));
end;

end.
