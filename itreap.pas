unit itreap;
{$mode objfpc}{$H+}{$J-}
interface
uses
  Classes, SysUtils, rheap;

type

generic TImplicitTreapNode<T> = class(TRandomHeapNode)
private
  // Value
  FValue: T;
public
  (* Tree node constructor. *)
  constructor Create(const v: T);

  (* Tree node destructor. *)
  destructor Destroy; override;

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
  DivideAt(node, pos, TRandomHeapNode(l), TRandomHeapNode(r));
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

end.

