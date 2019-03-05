{$mode objfpc}{$H+}{$J-}
{$warnings on}
{$hints on}

unit ftreap;

interface

type
  generic TTreapNode<T> = class
  private
    // Key
    FKey: T;
    // Random heap prority
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
    class function GetAt(node: TTreapNode; pos: SizeUInt): T;

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

class function TTreapNode.GetSize(const node: TTreapNode): SizeUInt; inline;
begin
  if node <> nil then
    Exit(node.FSize);
  Exit(0);
end;

class procedure TTreapNode.UpdateSize(const node: TTreapNode); inline;
begin
  if node <> nil then
    node.FSize := GetSize(node.FLeft) + GetSize(node.FRight) + 1;
end;

class procedure TTreapNode.RemoveNode(var node: TTreapNode); inline;
var
  n: TTreapNode;
begin
  if node <> nil then
  begin
     n := Meld(node.FLeft, node.FRight);
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
  if l.FPriority > r.FPriority then
  begin
    l.FRight := Meld(l.FRight, r);
    UpdateSize(l);
    Result := l;
  end
  else
  begin
    r.FLeft := Meld(l, r.FLeft);
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
    if node.FKey < k then
      node := node.FRight
    else if node.FKey > k then
      node := node.FLeft
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
    if node.FKey < k then
    begin
      pos := pos + GetSize(node.FLeft) + 1;
      node := node.FRight;
    end
    else if node.FKey > k then
      node := node.FLeft
    else
      Exit(pos + GetSize(node.FLeft));
  end;
  raise Exception.Create('No such key');
end;

class function TTreapNode.GetAt(node: TTreapNode; pos: SizeUInt): T;
var 
    lsize: SizeUInt;
begin
    if (node = nil) or (pos > GetSize(node) - 1) then
      raise Exception.Create('Invalid position.');
      
    while node <> nil do
    begin
        lsize := GetSize(node.FLeft);	
        if pos = lsize then 
        	exit(node.FKey);
        if pos > lsize then
        begin
            node := node.FRight;
            pos := pos - lsize - 1
        end    
        else 
        	node := node.FLeft
    end;        
end;    

class function TTreapNode.Min(node: TTreapNode): T;
begin
  Assert(node <> nil);
  while node.FLeft <> nil do
    node := node.FLeft;
  Exit(node.FKey);
end;

class function TTreapNode.Max(node: TTreapNode): T;
begin
  Assert(node <> nil);
  while node.FRight <> nil do
    node := node.FRight;
  Exit(node.FKey);
end;

class function TTreapNode.Remove(var node: TTreapNode; const k: T): boolean;
var
  n: TTreapNode;

begin
  if node <> nil then
  begin
    if k < node.FKey then
    begin
      Result := Remove(node.FLeft, k);
      if Result then
        UpdateSize(node);
    end
    else if k > node.FKey then
    begin
      Result := Remove(node.FRight, k);
      if Result then
        UpdateSize(node);
    end
    else
    begin
      n := node;
      node := Meld(node.FLeft, node.FRight);
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

  if pos < GetSize(node.FLeft) then
  begin
    Result := RemoveAt(node.FLeft, pos);
    UpdateSize(node);
  end
  else if pos > GetSize(node.FLeft) then
  begin
    Result := RemoveAt(node.FRight, pos - GetSize(node.FLeft) - 1);
    UpdateSize(node);
  end
  else
  begin
    Result := node.FKey;
    n := node;
    node := Meld(node.FLeft, node.FRight);
    FreeAndNil(n);
  end;

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
    Result := Result and (GetSize(node) = GetSize(node.FLeft) + GetSize(node.FRight) + 1);
    if node.FLeft <> nil then
    begin
      Result := Result and (node.FPriority >= node.FLeft.FPriority);
      Result := Result and ((node.FKey > node.FLeft.FKey) or (node.FKey = node.FLeft.FKey));
    end;
    if node.FRight <> nil then
    begin
      Result := Result and (node.FPriority >= node.FRight.FPriority);
      Result := Result and (node.FKey < node.FRight.FKey);
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
    WriteLn('Size - ', GetSize(node), '. Left subtree size - ', GetSize(node.FLeft),
      '. Right subtree size - ', GetSize(node.FRight), '. Ratio - ',
      GetSize(node.FLeft) / GetSize(node.FRight));
end;

end.
