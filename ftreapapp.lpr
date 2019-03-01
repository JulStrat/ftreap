{$mode objfpc}{$H+}{$J-}
{$MODESWITCH ADVANCEDRECORDS}
{$warnings on}
{$hints on}

program ftreapapp;

uses
  ftreap;

(*
type
  TStudent = record
    sname: String;
    sage: LongInt;
    class operator < (const A, B: TStudent): Boolean;
    class operator > (const A, B: TStudent): Boolean;
    class operator = (const A, B: TStudent): Boolean;
  end;

  class operator TStudent.< (const A, B: TStudent): Boolean;
  begin
    Result := A.sage < B.sage;
  end;

  class operator TStudent.> (const A, B: TStudent): Boolean;
  begin
    Result := A.sage > B.sage;
  end;

  class operator TStudent.= (const A, B: TStudent): Boolean;
  begin
    Result := (A.sage = B.sage) and (A.sname = B.sname);
  end;
*)
type
  TIntTreapNode = specialize TTreapNode<LongInt>;
  //TStTreapNode = specialize TTreapNode<TStudent>;

const
  NODES_NUM = 1000;
  MAX_KEY = 1000;

var
  ra: TIntTreapNode = nil;
  ta: array of longint;
  i, j: longint;

begin
  WriteLn('SizeOf(LongInt): ', SizeOf(LongInt));
  WriteLn('SizeOf(SizeUInt): ', SizeOf(SizeUInt));
  WriteLn('SizeOf(Pointer): ', SizeOf(Pointer));

  TIntTreapNode.ClassInfo;

  WriteLn('Heap MaxHeapSize - ', GetFPCHeapStatus().MaxHeapSize);
  WriteLn('Heap MaxHeapUsed - ', GetFPCHeapStatus().MaxHeapUsed);
  WriteLn('Heap CurrHeapSize - ', GetFPCHeapStatus().CurrHeapSize);
  WriteLn('Heap CurrHeapUsed - ', GetFPCHeapStatus().CurrHeapUsed);
  WriteLn('Heap CurrHeapFree - ', GetFPCHeapStatus().CurrHeapFree);

  SetLength(ta, NODES_NUM);
  for i := 0 to NODES_NUM - 1 do
  begin
    ta[i] := Random(MAX_KEY);
  end;

  for i := 0 to NODES_NUM - 1 do
  begin
    ra := TIntTreapNode.Insert(ra, ta[i]);
  end;

  WriteLn('Check treap structure - ', TIntTreapNode.CheckStucture(ra));
  WriteLn('Size - ', TIntTreapNode.GetSize(ra));
  if ra <> nil then
    WriteLn(ra.key);
  WriteLn('PASSED...');

  WriteLn('Check treap Find method.');
  for i := 0 to NODES_NUM - 1 do
  begin
    Assert(TIntTreapNode.Find(ra, ta[i]));
  end;
  Assert(not TIntTreapNode.Find(ra, -1));
  Assert(not TIntTreapNode.Find(ra, MAX_KEY + 1));
  Assert(not TIntTreapNode.Find(ra, MAX_KEY + 2));
  WriteLn('PASSED...');

  WriteLn('Check treap DeleteAt method.');
  for i := 0 to NODES_NUM - 1 do
  begin
    j := TIntTreapNode.DeleteAt(ra, 0);
    WriteLn(j);
  end;

  WriteLn('Check treap structure - ', TIntTreapNode.CheckStucture(ra));
  WriteLn('Size - ', TIntTreapNode.GetSize(ra));
  if ra <> nil then
    WriteLn(ra.key);
  WriteLn('PASSED...');

  ReadLn();
  (*
  i := 5;
  while i < 1000000 do
  begin
    ra := TIntTreapNode.Append(ra, i);
    ra := TIntTreapNode.Append(ra, i+2);
    i := i + 6;
  end;
  WriteLn('Size - ', TIntTreapNode.GetSize(ra));
  WriteLn('Check treap structure - ', TIntTreapNode.CheckStucture(ra));

  WriteLn('Heap MaxHeapSize - ', GetFPCHeapStatus().MaxHeapSize);
  WriteLn('Heap MaxHeapUsed - ', GetFPCHeapStatus().MaxHeapUsed);
  WriteLn('Heap CurrHeapSize - ', GetFPCHeapStatus().CurrHeapSize);
  WriteLn('Heap CurrHeapUsed - ', GetFPCHeapStatus().CurrHeapUsed);
  WriteLn('Heap CurrHeapFree - ', GetFPCHeapStatus().CurrHeapFree);

  ReadLn();
  *)
  TIntTreapNode.DestroyTreap(ra);

  // Assert error
  (*
  TIntTreapNode.DeleteAt(ra, 0);
  WriteLn(TIntTreapNode.GetSize(ra));
  if ra <> nil then
    WriteLn(ra.key);
  ReadLn();
  *)
  for i := 0 to NODES_NUM - 1 do
  begin
    ra := TIntTreapNode.Insert(ra, ta[i]);
  end;

  WriteLn('Check treap structure - ', TIntTreapNode.CheckStucture(ra));
  WriteLn('Size - ', TIntTreapNode.GetSize(ra));
  TIntTreapNode.StuctureInfo(ra);
  if ra <> nil then
    WriteLn(ra.key);
  WriteLn('PASSED...');
  ReadLn();
  TIntTreapNode.DestroyTreap(ra);
  Assert(ra = nil);
  Assert(TIntTreapNode.GetSize(ra) = 0);

end.
