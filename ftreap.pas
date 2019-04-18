{$mode objfpc}{$H+}{$J-}
{$ASSERTIONS ON}
{$warnings on}
{$hints on}
{$R+}{$Q+}

unit ftreap;

interface

uses Classes, SysUtils, rtreap;

type
  generic TTreapNode<T> = class(TTreapNodeBase)
  private
    // Key
    FKey: T;
  public
    (* Tree node constructor. *)
    constructor Create(const k: T);

    (* Tree node destructor. *)
    destructor Destroy;

    (* Divides tree into two trees. Where @code(Max(l) <= k). *)
    class procedure DivideRight(node: TTreapNode; k: T; var l, r: TTreapNode);

    (* Divides tree into two trees. Where @code(Max(l) < k). *)
    class procedure DivideLeft(node: TTreapNode; k: T; var l, r: TTreapNode);

    (* Insert key @code(k) in tree rooted at @code(node). *)
    class procedure Insert(var node: TTreapNode; const k: T); inline;

    (* Check if tree rooted at @code(root) node contains key @code(k). *)
    class function Contains(node: TTreapNode; const k: T): boolean; inline;

    (* Number of keys less than @code(k) *)
    class function BisectLeft(node: TTreapNode; const k: T): SizeUInt;
    (* Number of keys less or equal @code(k) *)
    class function BisectRight(node: TTreapNode; const k: T): SizeUInt;

    class function GetPosition(node: TTreapNode; const k: T): SizeUInt;

    (* @raises(EArgumentException) *)
    class function GetAt(node: TTreapNode; pos: SizeUInt): T;
    class function Min(node: TTreapNode): T;
    class function Max(node: TTreapNode): T;

    (* Removes key from the tree.
       @returns(@true if successful, @false otherwise) *)
    class function Remove(var node: TTreapNode; const k: T): boolean;

    (* Removes key from the given position.
       @returns(key) *)
    class function RemoveAt(var node: TTreapNode; const pos: SizeUInt): T;

    (* Destroy tree. *)
    class procedure DestroyTreap(var node: TTreapNode);

    class function CheckStucture(node: TTreapNode): boolean;
  end;

  generic TImplicitTreapNode<T> = class(TTreapNodeBase)
  private
    // Value
    FValue: T;
  public
    (* Tree node constructor. *)
    constructor Create(const v: T);

    (* Tree node destructor. *)
    destructor Destroy;

    (* Insert value @code(k) at position in tree rooted at @code(node). *)
    class procedure InsertAt(var node: TImplicitTreapNode; const pos: SizeUInt; const v: T); inline;

    (* @raises(EArgumentException) *)
    class function GetAt(node: TImplicitTreapNode; pos: SizeUInt): T;

    //class function UpdateAt(node: TImplicitTreapNode; pos: SizeUInt; const v: T): T;    
  
    (* Removes value from the given position.
       @returns(key) *)
    class function RemoveAt(var node: TImplicitTreapNode; const pos: SizeUInt): T;

    (* Destroy tree. *)
    class procedure DestroyTreap(var node: TImplicitTreapNode);

    class function CheckStucture(node: TImplicitTreapNode): boolean;
  end;
  
  
implementation
//
// TTreapNode Class methods
//
constructor TTreapNode.Create(const k: T);
begin
  inherited Create;
  FKey := k;
end;

destructor TTreapNode.Destroy;
begin
  inherited;
end;

class procedure TTreapNode.DivideRight(node: TTreapNode; k: T; var l, r: TTreapNode);
begin
  if node = nil then
  begin
    l := nil;
    r := nil;
    Exit;
  end;
  if k < node.FKey then
  begin
    DivideRight(TTreapNode(node.FLeft), k, l, TTreapNode(node.FLeft));
    r := node;
  end
  else
  begin
    DivideRight(TTreapNode(node.FRight), k, TTreapNode(node.FRight), r);
    l := node;
  end;
  UpdateSize(node);
end;

class procedure TTreapNode.DivideLeft(node: TTreapNode; k: T; var l, r: TTreapNode);
begin
  if node = nil then
  begin
    l := nil;
    r := nil;
    Exit;
  end;
  if k > node.FKey then
  begin
    DivideLeft(TTreapNode(node.FRight), k, TTreapNode(node.FRight), r);
    l := node;
  end
  else
  begin
    DivideLeft(TTreapNode(node.FLeft), k, l, TTreapNode(node.FLeft));
    r := node;
  end;
  UpdateSize(node);
end;

class procedure TTreapNode.Insert(var node: TTreapNode; const k: T); inline;
var
  l: TTreapNode = nil;
  r: TTreapNode = nil;
begin
  DivideRight(node, k, l, r);
  node := Meld(TTreapNode.Create(k), r) as TTreapNode;
  node := Meld(l, node) as TTreapNode;
end;

// PASSED
class function TTreapNode.Contains(node: TTreapNode; const k: T): boolean; inline;
begin
  while node <> nil do
  begin
    if k = node.FKey then
      Exit(True);
    if k > node.FKey then
      node := TTreapNode(node.FRight)
    else
      node := TTreapNode(node.FLeft);
  end;
  Exit(False);
end;

class function TTreapNode.BisectLeft(node: TTreapNode; const k: T): SizeUInt;
var
  pos: SizeUInt = 0;
begin
  while node <> nil do
  begin
    if k > node.FKey then
    begin
      pos := pos + GetSize(node.FLeft) + 1;
      node := TTreapNode(node.FRight);
    end
    else
      node := TTreapNode(node.FLeft);
  end;
  Exit(pos);
end;

class function TTreapNode.BisectRight(node: TTreapNode; const k: T): SizeUInt;
var
  pos: SizeUInt = 0;
begin
  while node <> nil do
  begin
    if k < node.FKey then
      node := TTreapNode(node.FLeft)
    else
    begin
      pos := pos + GetSize(node.FLeft) + 1;
      node := TTreapNode(node.FRight);
    end;
  end;
  Exit(pos);
end;

// PASSED
class function TTreapNode.GetPosition(node: TTreapNode; const k: T): SizeUInt;
var
  pos: SizeUInt = 0;
begin
  while node <> nil do
  begin
    if k = node.FKey then
      Exit(pos + GetSize(node.FLeft));
    if k > node.FKey then
    begin
      pos := pos + GetSize(node.FLeft) + 1;
      node := TTreapNode(node.FRight);
    end
    else
      node := TTreapNode(node.FLeft);
  end;
  raise Exception.Create('No such key');
end;

// PASSED
class function TTreapNode.GetAt(node: TTreapNode; pos: SizeUInt): T;
var
  lsize: SizeUInt = 0;
begin
  if (node = nil) or (pos > GetSize(node) - 1) then
    raise EArgumentException.Create('Set is empty or position is out of range.');
  while node <> nil do
  begin
    lsize := GetSize(node.FLeft);
    if pos = lsize then
      Exit(node.FKey);
    if pos > lsize then
    begin
      node := TTreapNode(node.FRight);
      pos := pos - lsize - 1;
    end
    else
      node := TTreapNode(node.FLeft);
  end;
  raise Exception.Create('Unreachable point.');
end;

// Min, Max
class function TTreapNode.Min(node: TTreapNode): T;
begin
  if node = nil then
    raise EArgumentException.Create('Set is empty.');
  while node.FLeft <> nil do
    node := TTreapNode(node.FLeft);
  Exit(node.FKey);
end;

class function TTreapNode.Max(node: TTreapNode): T;
begin
  if node = nil then
    raise EArgumentException.Create('Set is empty.');
  while node.FRight <> nil do
    node := TTreapNode(node.FRight);
  Exit(node.FKey);
end;
// Min, Max

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
      node := TTreapNode(Meld(node.FLeft, node.FRight));
      FreeAndNil(n);
      Exit(True);
    end;
    if k > node.FKey then
    begin
      Result := Remove(TTreapNode(node.FRight), k)
    end
    else
    begin
      Result := Remove(TTreapNode(node.FLeft), k);
    end;
    if Result then
      UpdateSize(node);
  end;
end;

// RWRT
class function TTreapNode.RemoveAt(var node: TTreapNode; const pos: SizeUInt): T;
var
  n: TTreapNode;
begin
  if (node = nil) or (pos > GetSize(node) - 1) then
    raise EArgumentException.Create('Set is empty or position is out of range.');
  if pos = GetSize(node.FLeft) then
  begin
    Result := node.FKey;
    n := node;
    node := TTreapNode(Meld(node.FLeft, node.FRight));
    FreeAndNil(n);
    Exit;
  end;
  if pos > GetSize(node.FLeft) then
  begin
    Result := RemoveAt(TTreapNode(node.FRight), pos - GetSize(node.FLeft) - 1)
  end
  else
  begin
    Result := RemoveAt(TTreapNode(node.FLeft), pos);
  end;
  UpdateSize(node);
end;

class procedure TTreapNode.DestroyTreap(var node: TTreapNode);
begin
  if node <> nil then
  begin
    DestroyTreap(TTreapNode(node.FLeft));
    DestroyTreap(TTreapNode(node.FRight));
    FreeAndNil(node);
  end;
end;

class function TTreapNode.CheckStucture(node: TTreapNode): boolean;
var
  l, r: TTreapNode;
begin
  Result := True;
  if node = nil then
    Exit(Result);
  with node do
  begin
    //l := node.FLeft as TTreapNode;
    //r := node.FRight as TTreapNode;

    Result := Result and CheckStucture(TTreapNode(node.FLeft));
    Result := Result and CheckStucture(TTreapNode(node.FRight));
    Result := Result and (GetSize(node) = GetSize(node.FLeft) +
      GetSize(node.FRight) + 1);
    if node.FLeft <> nil then
    begin
      Result := Result and (node.FPriority >= node.FLeft.FPriority);
      Result := Result and ((node.FKey >= TTreapNode(node.FLeft).FKey));
    end;
    if node.FRight <> nil then
    begin
      Result := Result and (node.FPriority >= node.FRight.FPriority);
      Result := Result and (node.FKey < TTreapNode(node.FRight).FKey);
    end;
  end;
end;

//
// TImplicitTreapNode Class methods
//
constructor TImplicitTreapNode.Create(const v: T);
begin
  inherited Create;
  FValue := v;
end;

destructor TImplicitTreapNode.Destroy;
begin
  inherited
end;

class procedure TImplicitTreapNode.InsertAt(var node: TImplicitTreapNode; const pos: SizeUInt; const v: T); inline;
var
  l, r: TImplicitTreapNode;
begin
  DivideAt(node, pos, TTreapNodeBase(l), TTreapNodeBase(r));
  node := TImplicitTreapNode(Meld(l, Meld(TImplicitTreapNode.Create(v), r)))
end;

// PASSED
class function TImplicitTreapNode.GetAt(node: TImplicitTreapNode; pos: SizeUInt): T;
var
  lsize: SizeUInt = 0;
begin
  if (node = nil) or (pos > GetSize(node) - 1) then
    raise EArgumentException.Create('Set is empty or position is out of range.');
  while node <> nil do
  begin
    lsize := GetSize(node.FLeft);
    if pos = lsize then
      Exit(node.FValue);
    if pos > lsize then
    begin
      node := TImplicitTreapNode(node.FRight);
      pos := pos - lsize - 1
    end
    else
      node := TImplicitTreapNode(node.FLeft)
  end;
  raise Exception.Create('Unreachable point.')
end;

// RWRT
class function TImplicitTreapNode.RemoveAt(var node: TImplicitTreapNode; const pos: SizeUInt): T;
var
  n: TImplicitTreapNode;
begin
  if (node = nil) or (pos > GetSize(node) - 1) then
    raise EArgumentException.Create('Set is empty or position is out of range.');
  if pos = GetSize(node.FLeft) then
  begin
    Result := node.FValue;
    n := node;
    node := TImplicitTreapNode(Meld(node.FLeft, node.FRight));
    FreeAndNil(n);
    Exit;
  end;
  if pos > GetSize(node.FLeft) then
    Result := RemoveAt(TImplicitTreapNode(node.FRight), pos - GetSize(node.FLeft) - 1)
  else
    Result := RemoveAt(TImplicitTreapNode(node.FLeft), pos);
  UpdateSize(node);
end;

class procedure TImplicitTreapNode.DestroyTreap(var node: TImplicitTreapNode);
begin
  if node <> nil then
  begin
    DestroyTreap(TImplicitTreapNode(node.FLeft));
    DestroyTreap(TImplicitTreapNode(node.FRight));
    FreeAndNil(node)
  end;
end;

class function TImplicitTreapNode.CheckStucture(node: TImplicitTreapNode): boolean;
begin
  Result := True;
  if node = nil then
    Exit(Result);
  with node do
  begin
    Result := Result and CheckStucture(TImplicitTreapNode(node.FLeft));
    Result := Result and CheckStucture(TImplicitTreapNode(node.FRight));
    Result := Result and (GetSize(node) = GetSize(node.FLeft) +
      GetSize(node.FRight) + 1);
    if node.FLeft <> nil then
      Result := Result and (node.FPriority >= node.FLeft.FPriority);
    if node.FRight <> nil then
      Result := Result and (node.FPriority >= node.FRight.FPriority);
  end;
end;

initialization
  Randomize;
end.
