unit rheap;

{$mode objfpc}{$H+}{$J-}
interface

uses
  Classes, SysUtils;

type
  TRandomHeapNode = class(TObject)
  public
    // Random heap priority
    FPriority: extended;
    // Number of nodes in heap
    FSize: SizeUInt;
    // Left subtree reference
    FLeft: TRandomHeapNode;
    // Right subtree reference
    FRight: TRandomHeapNode;

    constructor Create;
    destructor Destroy; override;

    class function IsEmpty(const node: TRandomHeapNode): boolean; static; inline;

    (* Returns number of keys in the tree rooted at @code(node). *)
    class function GetSize(const node: TRandomHeapNode): SizeUInt; static; inline;

    (* Recalculates number of keys in the tree rooted at @code(node) after insert, delete operations. *)
    class procedure UpdateSize(const node: TRandomHeapNode); static; inline;

    (* Creates new tree from two trees. *)
    class function Meld(l, r: TRandomHeapNode): TRandomHeapNode;

    (* Divides tree into two trees. Where @code(Size(l) = pos). *)
    class procedure DivideAt(node: TRandomHeapNode; const pos: SizeUInt;
      var l, r: TRandomHeapNode);

    class procedure PostUpdate(const node: TRandomHeapNode); virtual;

  end;

implementation

constructor TRandomHeapNode.Create();
begin
  FPriority := Random;
  FSize := 1;
  FLeft := nil;
  FRight := nil
end;

destructor TRandomHeapNode.Destroy;
begin
  FLeft := nil;
  FRight := nil;
  inherited
end;

class function TRandomHeapNode.GetSize(const node: TRandomHeapNode): SizeUInt;
begin
  if node <> nil then
    Exit(node.FSize);
  Exit(0)
end;

class procedure TRandomHeapNode.UpdateSize(const node: TRandomHeapNode);
begin
  if node <> nil then
    node.FSize := GetSize(node.FLeft) + GetSize(node.FRight) + 1
end;

class function TRandomHeapNode.IsEmpty(const node: TRandomHeapNode): boolean;
begin
  Exit(node = nil)
end;

class function TRandomHeapNode.Meld(l, r: TRandomHeapNode): TRandomHeapNode;
begin
  if l = nil then
    Exit(r);
  if r = nil then
    Exit(l);
  if l.FPriority > r.FPriority then
  begin
    l.FRight := Meld(l.FRight, r);
    Result := l
  end
  else
  begin
    r.FLeft := Meld(l, r.FLeft);
    Result := r
  end;
  UpdateSize(Result);
  PostUpdate(Result)
end;

class procedure TRandomHeapNode.DivideAt(node: TRandomHeapNode;
  const pos: SizeUInt; var l, r: TRandomHeapNode);
begin
  if node = nil then
  begin
    l := nil;
    r := nil;
    Exit
  end;
  if pos > GetSize(node.FLeft) then
  begin
    DivideAt(node.FRight, pos - GetSize(node.FLeft) - 1, node.FRight, r);
    l := node
  end
  else
  begin
    DivideAt(node.FLeft, pos, l, node.FLeft);
    r := node
  end;
  UpdateSize(node);
  PostUpdate(node)
end;

class procedure TRandomHeapNode.PostUpdate(const node: TRandomHeapNode);
begin
end;

initialization
  Randomize;
end.
