unit rheap;

{$mode objfpc}{$H+}{$J-}
interface

uses
  Classes, SysUtils;

type
  TTreapNodeBase = class(TObject)
  public
    // Random heap priority
    FPriority: extended;
    // Number of nodes in treap
    FSize: SizeUInt;
    // Left subtree reference
    FLeft: TTreapNodeBase;
    // Right subtree reference
    FRight: TTreapNodeBase;

    constructor Create;
    destructor Destroy;

    class function IsEmpty(const node: TTreapNodeBase): boolean; static; inline;

    (* Returns number of keys in the tree rooted at @code(node). *)
    class function GetSize(const node: TTreapNodeBase): SizeUInt; static; inline;

    (* Recalculates number of keys in the tree rooted at @code(node) after insert, delete operations. *)
    class procedure UpdateSize(const node: TTreapNodeBase); static; inline;

    (* Creates new tree from two trees. *)
    class function Meld(l, r: TTreapNodeBase): TTreapNodeBase;

    (* Divides tree into two trees. Where @code(Size(l) = pos). *)
    class procedure DivideAt(node: TTreapNodeBase; const pos: SizeUInt;
      var l, r: TTreapNodeBase);

  end;


implementation

constructor TTreapNodeBase.Create();
begin
  FPriority := Random;
  FSize := 1;
  FLeft := nil;
  FRight := nil;
end;

destructor TTreapNodeBase.Destroy;
begin
  FLeft := nil;
  FRight := nil;
  inherited;
end;

class function TTreapNodeBase.GetSize(const node: TTreapNodeBase): SizeUInt; inline;
begin
  if node <> nil then
    Exit(node.FSize);
  Exit(0);
end;

class procedure TTreapNodeBase.UpdateSize(const node: TTreapNodeBase); inline;
begin
  if node <> nil then
    node.FSize := GetSize(node.FLeft) + GetSize(node.FRight) + 1;
end;

class function TTreapNodeBase.IsEmpty(const node: TTreapNodeBase): boolean; inline;
begin
  Exit(node = nil);
end;

class function TTreapNodeBase.Meld(l, r: TTreapNodeBase): TTreapNodeBase;
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

class procedure TTreapNodeBase.DivideAt(node: TTreapNodeBase;
  const pos: SizeUInt; var l, r: TTreapNodeBase);
begin
  if node = nil then
  begin
    l := nil;
    r := nil;
    Exit;
  end;
  if pos > GetSize(node.FLeft) then
  begin
    DivideAt(node.FRight, pos - GetSize(node.FLeft) - 1, node.FRight, r);
    l := node;
  end
  else
  begin
    DivideAt(node.FLeft, pos, l, node.FLeft);
    r := node;
  end;
  UpdateSize(node);
end;

initialization
  Randomize;
end.
