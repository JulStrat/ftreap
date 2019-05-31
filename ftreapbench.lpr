{$mode delphi}
program ftreapbench;

uses
  SysUtils, treap, gset, gutil;

type
  TInt64Treap = TTreap<Int64>;
  iLess = TLess<Int64>;
  iSet = TSet<Int64, iLess>;

var
  ra: TInt64Treap;
  i: LongInt;
  tck: QWORD;
  S: iSet;

begin
  ra := TInt64Treap.Create;
  tck := GetTickCount64();
  for i := 1 to 2000000 do
  begin
    ra.Insert(Random(9223372036854775807));
  end;
  WriteLn('Tick count treap - ', GetTickCount64() - tck);
  ra.Free;

  tck := GetTickCount64;
  S := iSet.Create();
  for i := 1  to 2000000 do
    S.Insert(Random(9223372036854775807));
  Writeln('Tick count gset - ', GetTickCount64 - tck);
  //ReadLn;

end.

