unit rheap;
{$ifdef FPC}
{$mode delphi}
{$endif}

interface

uses
  Classes, SysUtils;

type
  TRandomHeap = class
  public

  type
    PRandomHeapNode = ^TRandomHeapNode;
    TRandomHeapNode = object
      FPriority: Single;
      FLeft: PRandomHeapNode;
      FRight: PRandomHeapNode;
      FSize: SizeInt;
    end;

    class function GetSize(const node: PRandomHeapNode): SizeInt; static; inline;
    class procedure UpdateSize(const node: PRandomHeapNode); static; inline;

    class function Meld(l, r: PRandomHeapNode): PRandomHeapNode;
    class procedure DivideAt(node: PRandomHeapNode; pos: SizeInt;
      var l, r: PRandomHeapNode);

    class function FirstNode(node: PRandomHeapNode): PRandomHeapNode; static; inline;
    class function LastNode(node: PRandomHeapNode): PRandomHeapNode; static; inline;
    class function KthNode(node: PRandomHeapNode; k: SizeInt): PRandomHeapNode; static;
    (*
    class procedure PostUpdate(const node: TRandomHeapNode); virtual;
    *)
  end;

implementation

class function TRandomHeap.GetSize(const node: PRandomHeapNode): SizeInt;
begin
  if node <> nil then
    Result := node.FSize
  else
    Result := 0;
end;

class procedure TRandomHeap.UpdateSize(const node: PRandomHeapNode);
begin
  if node <> nil then
    node.FSize := GetSize(node.FLeft) + GetSize(node.FRight) + 1
end;

class function TRandomHeap.Meld(l, r: PRandomHeapNode): PRandomHeapNode;
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
  //PostUpdate(Result)
end;

class procedure TRandomHeap.DivideAt(node: PRandomHeapNode;
  pos: SizeInt; var l, r: PRandomHeapNode);
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
  //PostUpdate(node)
end;

class function TRandomHeap.FirstNode(node: PRandomHeapNode): PRandomHeapNode;
begin
  while node.FLeft <> nil do
    node := node.FLeft;
  Result := node;
end;

class function TRandomHeap.LastNode(node: PRandomHeapNode): PRandomHeapNode;
begin
  while node.FRight <> nil do
    node := node.FRight;
  Result := node;
end;

class function TRandomHeap.KthNode(node: PRandomHeapNode; k: SizeInt): PRandomHeapNode;
var
  lsize: SizeInt = 0;
begin
  while node <> nil do
  begin
    lsize := GetSize(node.FLeft);
    if k = lsize then
      break;
    if k > lsize then
    begin
      node := node.FRight;
      k := k - lsize - 1;
    end
    else
      node := node.FLeft;
  end;
  Result := node;
end;

(*
class procedure TRandomHeapNode.PostUpdate(const node: TRandomHeapNode);
begin
end;
*)
initialization
  Randomize;
end.
