{$mode objfpc}{$H+}{$J-}
{$ASSERTIONS ON}
{$warnings on}
{$hints on}
{$R+}

program ftreapapp;

uses
  treap, itreap;

type
  TIntTreapNode = specialize TTreapNode<LongInt>;
  TImpIntTreapNode = specialize TImplicitTreapNode<LongInt>;

const
  NODES_NUM = 100;
  MAX_KEY = 1000;

var
  ra, ln, rn: TIntTreapNode;
  ria: TImpIntTreapNode = nil;
  ta: array of longint;
  i, j: longint;

begin
  //TIntTreapNode.ClassInfo;
  WriteLn('TIntTreapNode instanceSize - ', TIntTreapNode.InstanceSize);

  SetLength(ta, NODES_NUM);
  for i := 0 to NODES_NUM - 1 do
  begin
    ta[i] := 2*i; //Random(MAX_KEY);
  end;

  for i := 0 to NODES_NUM - 1 do
  begin
    TIntTreapNode.Insert(ra, ta[i]);
  end;

  WriteLn('Check treap structure - ', TIntTreapNode.CheckStucture(ra));
  WriteLn('Size - ', TIntTreapNode.GetSize(ra));
  if ra <> nil then
    WriteLn(ra.FKey);
  WriteLn('PASSED...');

  WriteLn('Check treap Contains method.');
  for i := 0 to NODES_NUM - 1 do
  begin
    Assert(TIntTreapNode.Contains(ra, ta[i]));
    Assert(not TIntTreapNode.Contains(ra, 1));
  end;
  WriteLn('PASSED...');

  WriteLn('Check treap GetPosition method.');
  for i := 0 to NODES_NUM - 1 do
  begin
    writeLn('Key - ', ta[i], ' Pos - ', TIntTreapNode.GetPosition(ra, ta[i]));
    Assert(i = TIntTreapNode.GetPosition(ra, ta[i]));
  end;
  WriteLn('PASSED...');

  WriteLn('Check treap GetAt method.');
  for i := 0 to NODES_NUM - 1 do
  begin
    Assert(2*i = TIntTreapNode.GetAt(ra, i));
  end;
  WriteLn('PASSED...');

  WriteLn('Check treap BisectLeft method.');
  for i := 0 to NODES_NUM - 1 do
  begin
    writeLn('Key - ', ta[i], ' BL - ', TIntTreapNode.BisectLeft(ra, ta[i]));
    Assert(i = TIntTreapNode.BisectLeft(ra, ta[i]));
  end;
  WriteLn('PASSED...');

  WriteLn('Check treap BisectRight method.');
  for i := 0 to NODES_NUM - 1 do
  begin
    writeLn('Key - ', ta[i], ' BR - ', TIntTreapNode.BisectRight(ra, ta[i]));
    Assert(i+1 = TIntTreapNode.BisectRight(ra, ta[i]));
  end;
  WriteLn('PASSED...');

  WriteLn('Check treap DeleteAt method.');
  for i := 0 to NODES_NUM - 1 do
  begin
    j := TIntTreapNode.RemoveAt(ra, 0);
    WriteLn(j);
  end;
  WriteLn('Check treap structure - ', TIntTreapNode.CheckStucture(ra));
  WriteLn('Size - ', TIntTreapNode.GetSize(ra));
  WriteLn('PASSED...');
  TIntTreapNode.DestroyTreap(ra);

  for i := 0 to NODES_NUM - 1 do
  begin
    TIntTreapNode.Insert(ra, ta[i]);
  end;

  WriteLn('Check treap structure - ', TIntTreapNode.CheckStucture(ra));
  WriteLn('Size - ', TIntTreapNode.GetSize(ra));
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

  TIntTreapNode.DestroyTreap(ra);
  Assert(ra = nil);
  Assert(TIntTreapNode.GetSize(ra) = 0);

  WriteLn('-----------------------');
  WriteLn('Implicit Treap test ...');
  WriteLn('-----------------------');

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

  //ReadLn();

end.
