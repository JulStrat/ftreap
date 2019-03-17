{$mode objfpc}{$H+}{$J-}
{$ASSERTIONS ON}
{$warnings on}
{$hints on}
{$R+}{$Q+}

unit ftreap;

interface

uses SysUtils;

type EArgumentNilException = class(EArgumentException);

type
  generic TTreapNode<T> = class
  private
    // Key
    FKey: T;
    // Random heap priority
    FPriority: extended;
    // Number of nodes in subtree
    FSize: SizeUInt;
    // Left subtree reference
    FLeft: TTreapNode;
    // Right subtree reference
    FRight: TTreapNode;
  public
    (* Tree node constructor. *)
    constructor Create(const k: T);

    (* Tree node destructor. *)
    destructor Destroy; override;

    (* Returns number of keys in the tree rooted at @code(node). *)
    class function GetSize(const node: TTreapNode): SizeUInt; inline;

    (* Recalculates number of keys in the tree rooted at @code(node) after insert, delete operations. *)
    class procedure UpdateSize(const node: TTreapNode); inline;

    (* Creates new tree from two trees, where @code(Min(r) >= Max(l)). *)
    class function Meld(l, r: TTreapNode): TTreapNode;

    (* Divides tree into two trees. Where @code(Max(l) <= k). *)
    class procedure Divide(node: TTreapNode; k: T; var l, r: TTreapNode);

    (* Divides tree into two trees. Where @code(Size(l) = pos + 1). *)
    class procedure DivideAt(node: TTreapNode; const pos: SizeUInt;
      var l, r: TTreapNode);

    (* Returns @true if tree rooted at @code(node) is empty, @false otherwise *)
    class function IsEmpty(const node: TTreapNode): boolean; inline;

    (* Insert key @code(k) in tree rooted at @code(node). *)
    class procedure Insert(var node: TTreapNode; const k: T); inline;

    (* Check if tree rooted at @code(root) node contains key @code(k). *)
    class function Contains(node: TTreapNode; const k: T): boolean; inline;

    class function GetPosition(node: TTreapNode; const k: T): SizeUInt;

    (* @raises(EArgumentNilException) @raises(EArgumentOutOfRangeException) *)
    class function GetAt(node: TTreapNode; pos: SizeUInt): T;

    (* Removes key from the tree.
       @returns(@true if successful, @false otherwise) *)
    class function Remove(var node: TTreapNode; const k: T): boolean;

    (* Removes key from the given position.
       @returns(@true if successful, @false otherwise) *)
    class function RemoveAt(var node: TTreapNode; const pos: SizeUInt): T;

    //class function ToArray(node: TTreapNode): TKeyArray;

    (* Destroy tree. *)
    class procedure DestroyTreap(var node: TTreapNode);

    class function CheckStucture(node: TTreapNode): boolean;
  end;

implementation

constructor TTreapNode.Create(const k: T);
begin
  FKey := k;
  FPriority := Random;
  FSize := 1;
  FLeft := nil;
  FRight := nil;
end;

destructor TTreapNode.Destroy;
begin
  FLeft := nil;
  FRight := nil;
  inherited;
end;

// PASSED
class function TTreapNode.GetSize(const node: TTreapNode): SizeUInt; inline;
begin
  if node <> nil then
    Exit(node.FSize);
  Exit(0);
end;

// PASSED
class procedure TTreapNode.UpdateSize(const node: TTreapNode); inline;
begin
  if node <> nil then
    node.FSize := GetSize(node.FLeft) + GetSize(node.FRight) + 1;
end;

class function TTreapNode.IsEmpty(const node: TTreapNode): boolean; inline;
begin
  Exit(node = nil);
end;

class function TTreapNode.Meld(l, r: TTreapNode): TTreapNode;
begin
  if l = nil then
    Exit(r);
  if r = nil then
    Exit(l);
  if l.FPriority > r.FPriority then
  begin
    l.FRight := Meld(l.FRight, r);
    Result := l;
  end
  else
  begin
    r.FLeft := Meld(l, r.FLeft);
    Result := r;
  end;
  UpdateSize(Result);
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
  if pos <= GetSize(node.FLeft) then
  begin
    DivideAt(node.FLeft, pos, l, node.FLeft);
    r := node;
  end
  else
  begin
    DivideAt(node.FRight, pos - GetSize(node.FLeft) - 1, node.FRight, r);
    l := node;
  end;
  UpdateSize(node);
end;

// DivideRight
class procedure TTreapNode.Divide(node: TTreapNode; k: T; var l, r: TTreapNode);
begin
  if node = nil then
  begin
    l := nil;
    r := nil;
    Exit;
  end;
  if k < node.FKey then
  begin
    Divide(node.FLeft, k, l, node.FLeft);
    r := node;
  end
  else
  begin
    Divide(node.FRight, k, node.FRight, r);
    l := node;
  end;
  UpdateSize(node);
end;

class procedure TTreapNode.Insert(var node: TTreapNode; const k: T); inline;
var
  l: TTreapNode = nil;
  r: TTreapNode = nil;
begin
  Divide(node, k, l, r);
  node := Meld(l, Meld(TTreapNode.Create(k), r));
end;

// PASSED
class function TTreapNode.Contains(node: TTreapNode; const k: T): boolean; inline;
begin
  while node <> nil do
  begin
    if k = node.FKey then
      Exit(True);
    if k > node.FKey then
      node := node.FRight
    else
      node := node.FLeft
  end;
  Exit(False);
end;

// PASSED
class function TTreapNode.GetPosition(node: TTreapNode; const k: T): SizeUInt;
var
  pos: SizeUInt;
begin
  pos := 0;
  while node <> nil do
  begin
    if k = node.FKey then
      Exit(pos + GetSize(node.FLeft));
    if k > node.FKey then
    begin
      pos := pos + GetSize(node.FLeft) + 1;
      node := node.FRight;
    end
    else
      node := node.FLeft;
  end;
  raise Exception.Create('No such key');
end;

// PASSED
class function TTreapNode.GetAt(node: TTreapNode; pos: SizeUInt): T;
var
  lsize: SizeUInt = 0;
begin
  if node = nil then
    raise EArgumentNilException.Create('Set is empty.');
  if pos > GetSize(node) - 1 then
    raise EArgumentOutOfRangeException.Create('Position is out of range.');
  while node <> nil do
  begin
    lsize := GetSize(node.FLeft);
    if pos = lsize then
      Exit(node.FKey);
    if pos > lsize then
    begin
      node := node.FRight;
      pos := pos - lsize - 1;
    end
    else
      node := node.FLeft;
  end;
  raise Exception.Create('Unreachable point.');
end;

class function TTreapNode.Remove(var node: TTreapNode; const k: T): boolean;
var
  n: TTreapNode;
begin
  Result := False;
  if node <> nil then
  begin
    if k = node.FKey then
    begin
      n := node;
      node := Meld(node.FLeft, node.FRight);
      FreeAndNil(n);
      Exit(True);
    end;
    if k > node.FKey then
      Result := Remove(node.FRight, k)
    else
      Result := Remove(node.FLeft, k);
    if Result then
      UpdateSize(node);
  end;
end;

// RWRT
class function TTreapNode.RemoveAt(var node: TTreapNode; const pos: SizeUInt): T;
var
  n: TTreapNode;
begin
  if node = nil then
    raise EArgumentNilException.Create('Set is empty.');
  if pos > GetSize(node) - 1 then
    raise EArgumentOutOfRangeException.Create('Position is out of range.');
  if pos = GetSize(node.FLeft) then
  begin
    Result := node.FKey;
    n := node;
    node := Meld(node.FLeft, node.FRight);
    FreeAndNil(n);
    Exit;
  end;
  if pos > GetSize(node.FLeft) then
    Result := RemoveAt(node.FRight, pos - GetSize(node.FLeft) - 1)
  else
    Result := RemoveAt(node.FLeft, pos);
  UpdateSize(node);
end;

class procedure TTreapNode.DestroyTreap(var node: TTreapNode);
begin
  if node <> nil then
  begin
    DestroyTreap(node.FLeft);
    DestroyTreap(node.FRight);
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
    Result := Result and CheckStucture(node.FLeft);
    Result := Result and CheckStucture(node.FRight);
    Result := Result and (GetSize(node) = GetSize(node.FLeft) +
      GetSize(node.FRight) + 1);
    if node.FLeft <> nil then
    begin
      Result := Result and (node.FPriority >= node.FLeft.FPriority);
      Result := Result and ((node.FKey > node.FLeft.FKey) or
        (node.FKey = node.FLeft.FKey));
    end;
    if node.FRight <> nil then
    begin
      Result := Result and (node.FPriority >= node.FRight.FPriority);
      Result := Result and (node.FKey < node.FRight.FKey);
    end;
  end;
end;

initialization
  Randomize;
end.
