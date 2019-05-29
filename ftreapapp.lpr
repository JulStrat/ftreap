{$mode delphi}

program ftreapapp;

uses
  SysUtils, treap, itreap, rheap;

(*
type
  TIntTreapNode = TTreapNode<LongInt>;
  TImpIntTreapNode = specialize TImplicitTreapNode<LongInt>;
*)
const
  NODES_NUM = 100;
  MAX_KEY = 1000;

var
  ra, ln, rn: TTreap<LongInt>;
  //ria: TImplicitTreapNode<LongInt>;
  ta: array of longint;
  i, j: longint;

begin
  //TIntTreapNode.ClassInfo;
  WriteLn('TTreap<LongInt> instanceSize - ', TTreap<LongInt>.InstanceSize);
  WriteLn('TRandomHeap instanceSize - ', TRandomHeap.InstanceSize);

  SetLength(ta, NODES_NUM);
  for i := 0 to NODES_NUM - 1 do
  begin
    ta[i] := 2*i; //Random(MAX_KEY);
  end;

  ra := TTreap<LongInt>.Create;

  for i := 0 to NODES_NUM - 1 do
  begin
    ra.Insert(ta[i]);
  end;

  WriteLn('Check treap structure - ', TTreap<LongInt>.CheckStucture(ra.FRoot));
  WriteLn('Size - ', TTreap<LongInt>.GetSize(ra.FRoot));
  if ra <> nil then
    WriteLn(ra.FRoot.FKey);
  WriteLn('PASSED...');
  //ReadLn();

  WriteLn('Check treap Contains method.');
  for i := 0 to NODES_NUM - 1 do
  begin
    Assert(ra.Contains(ta[i]));
    Assert(not ra.Contains(1));
  end;
  WriteLn('PASSED...');

  WriteLn('Check treap GetPosition method.');
  for i := 0 to NODES_NUM - 1 do
  begin
    writeLn('Key - ', ta[i], ' Pos - ', ra.GetPosition(ta[i]));
    Assert(i = ra.GetPosition(ta[i]));
  end;
  WriteLn('PASSED...');

  WriteLn('Check treap GetAt method.');
  for i := 0 to NODES_NUM - 1 do
  begin
    Assert(2*i = ra.GetAt(i));
  end;
  WriteLn('PASSED...');

  WriteLn('Check treap BisectLeft method.');
  for i := 0 to NODES_NUM - 1 do
  begin
    writeLn('Key - ', ta[i], ' BL - ', ra.BisectLeft(ta[i]));
    Assert(i = ra.BisectLeft(ta[i]));
  end;
  WriteLn('PASSED...');

  WriteLn('Check treap BisectRight method.');
  for i := 0 to NODES_NUM - 1 do
  begin
    writeLn('Key - ', ta[i], ' BR - ', ra.BisectRight(ta[i]));
    Assert(i+1 = ra.BisectRight(ta[i]));
  end;
  WriteLn('PASSED...');

  WriteLn('Check treap DeleteAt method.');
  for i := 0 to NODES_NUM - 1 do
  begin
    j := ra.RemoveAt(0);
    WriteLn(j);
  end;
  WriteLn('Check treap structure - ', TTreap<LongInt>.CheckStucture(ra.FRoot));
  WriteLn('Size - ', TTreap<LongInt>.GetSize(ra.FRoot));
  WriteLn('PASSED...');

  TTreap<LongInt>.DestroyTreap(ra.FRoot);
  WriteLn('Check treap structure - ', TTreap<LongInt>.CheckStucture(ra.FRoot));
  WriteLn('Size - ', TTreap<LongInt>.GetSize(ra.FRoot));

  for i := 0 to NODES_NUM - 1 do
  begin
    ra.Insert(ta[i]);
  end;

  WriteLn('Check treap structure - ', TTreap<LongInt>.CheckStucture(ra.FRoot));
  WriteLn('Size - ', TTreap<LongInt>.GetSize(ra.FRoot));

  (*
  WriteLn('DivideRight test ...');
  for i := 0 to NODES_NUM - 1 do
  begin
    TIntTreapNode.DivideRight(ra, ta[i], ln, rn);
    Assert(TIntTreapNode.Contains(ln, ta[i]));
    Assert(not TIntTreapNode.Contains(rn, ta[i]));
    ra := TIntTreapNode.Meld(ln, rn) as TIntTreapNode;
  end;
  WriteLn('PASSED...');

  WriteLn('Check treap structure - ', TIntTreapNode.CheckStucture(ra));
  WriteLn('Size - ', TIntTreapNode.GetSize(ra));
  WriteLn('DivideLeft test ...');
  for i := 0 to NODES_NUM - 1 do
  begin
    TIntTreapNode.DivideLeft(ra, ta[i], ln, rn);
    Assert(TIntTreapNode.Contains(rn, ta[i]));
    Assert(not TIntTreapNode.Contains(ln, ta[i]));
    ra := TIntTreapNode.Meld(ln, rn)  as TIntTreapNode;
  end;
  WriteLn('Check treap structure - ', TIntTreapNode.CheckStucture(ra));
  WriteLn('Size - ', TIntTreapNode.GetSize(ra));
  WriteLn('PASSED...');
  *)

  (*
  WriteLn('MIN KEY - ', TIntTreapNode.Min(ra));
  WriteLn('MAX KEY - ', TIntTreapNode.Max(ra));
  *)
  WriteLn('PASSED...');
  //TTreap<LongInt>.DestroyTreap(ra.FRoot);
  //WriteLn('Size - ', TTreap<LongInt>.GetSize(ra.FRoot));
  ra.Free;

  //Assert(ra = nil);

  (*
  WriteLn('-----------------------');
  WriteLn('Implicit Treap test ...');
  WriteLn('-----------------------');
  WriteLn('TImpIntTreapNode instanceSize - ', TImpIntTreapNode.InstanceSize);

  for i := 0 to NODES_NUM - 1 do
  begin
    TImpIntTreapNode.InsertAt(ria, 0, ta[i]);
  end;

  Assert(True = TImpIntTreapNode.CheckStucture(ria));
  WriteLn('Check implicit treap structure PASSED...');

  for i := 0 to NODES_NUM - 1 do
  begin
    j := TImpIntTreapNode.GetAt(ria, i);
    WriteLn(j);
    Assert(ta[NODES_NUM-1-i] = j);
  end;
  WriteLn('InsertAt PASSED...');

  for i := 0 to NODES_NUM - 1 do
  begin
    j := TImpIntTreapNode.RemoveAt(ria, 0);
    WriteLn(j);
    Assert(ta[NODES_NUM-1-i] = j);
  end;
  WriteLn('RemoveAt PASSED...');

  Assert(ria = nil);
  Assert(TImpIntTreapNode.GetSize(ria) = 0);
  TImpIntTreapNode.DestroyTreap(ria);
  *)

  //ReadLn();

end.
