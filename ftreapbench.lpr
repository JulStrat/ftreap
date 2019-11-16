program ftreapbench;
{$ifdef FPC}
{$mode delphi}
{$endif}

{$ASSERTIONS ON}

uses
  SysUtils, treap, gset, gutil;

type
  TInt64Treap = TTreap<Int64>;
  iLess = TLess<Int64>;
  iSet = TSet<Int64, iLess>;

const
  keyNum = 1000000;

var
  ra: TInt64Treap;
  i: LongInt;
  tck: QWORD;
  S: iSet;
  key: Int64;

begin
  ra := TInt64Treap.Create;

  WriteLn('Random');
  WriteLn('----------');

  tck := GetTickCount64;
  for i := 1 to keyNum do
  begin
    ra.Insert(Random(9223372036854775807));
  end;
  WriteLn('Ticks <treap> create - ', GetTickCount64 - tck);

  tck := GetTickCount64;
  for i := 1 to keyNum do
    ra.Contains(Random(9223372036854775807));
  WriteLn('Ticks <treap> search - ', GetTickCount64 - tck);

  tck := GetTickCount64;
  for i := 1 to keyNum do
    ra.Remove(Random(9223372036854775807));
  WriteLn('Ticks <treap> delete - ', GetTickCount64 - tck);
  WriteLn('Treap nodes - ', ra.GetSize(ra.FRoot));
  ra.Free;

  WriteLn('----------');

  S := iSet.Create;
  tck := GetTickCount64;
  for i := 1  to keyNum do
    S.Insert(Random(9223372036854775807));
  Writeln('Ticks <gset> create - ', GetTickCount64 - tck);

  tck := GetTickCount64;
  for i := 1  to keyNum do
    S.NFind(Random(9223372036854775807));
  Writeln('Ticks <gset> search - ', GetTickCount64 - tck);

  tck := GetTickCount64;
  for i := 1 to keyNum do
    S.Delete(Random(9223372036854775807));
  WriteLn('Ticks <gset> delete - ', GetTickCount64 - tck);
  WriteLn('<gset> nodes - ', S.Size);
  S.Free;

  WriteLn;
  WriteLn;
  WriteLn;

  ra := TInt64Treap.Create;
  WriteLn('Key = i*i');
  WriteLn('----------');

  tck := GetTickCount64;
  for i := 1 to keyNum do
  begin
    ra.Insert(i*i);
  end;
  WriteLn('Ticks <treap> create - ', GetTickCount64 - tck);

  tck := GetTickCount64;
  for i := 1 to keyNum do
    Assert(ra.Contains(i*i));
  WriteLn('Ticks <treap> search - ', GetTickCount64 - tck);

  tck := GetTickCount64;
  for i := 1 to keyNum do
    ra.Remove(i*i);
  WriteLn('Ticks <treap> delete - ', GetTickCount64 - tck);
  WriteLn('Treap nodes - ', ra.GetSize(ra.FRoot));
  ra.Free;

  WriteLn('----------');

  S := iSet.Create;
  tck := GetTickCount64;
  for i := 1  to keyNum do
    S.Insert(i*i);
  Writeln('Ticks <gset> create - ', GetTickCount64 - tck);

  tck := GetTickCount64;
  for i := 1  to keyNum do
    Assert(S.Find(i*i) <> nil);
  Writeln('Ticks <gset> search - ', GetTickCount64 - tck);

  tck := GetTickCount64;
  for i := 1 to keyNum do
    S.Delete(i*i);
  WriteLn('Ticks <gset> delete - ', GetTickCount64 - tck);
  WriteLn('<gset> nodes - ', S.Size);
  S.Free;

  { ReadLn; }

end.

