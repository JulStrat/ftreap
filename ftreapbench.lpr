{$mode delphi}
program ftreapbench;

uses
  SysUtils, treap;

type
  TInt64Treap = TTreap<Int64>;

var
  ra: TInt64Treap;
  i: LongInt;
  tck: QWORD;

begin
  ra := TInt64Treap.Create;
  tck := GetTickCount64();
  for i := 1 to 1000000 do
  begin
    ra.Insert(Random(9223372036854775807));
  end;
  WriteLn('Tick count - ', GetTickCount64() - tck);
  ra.Free;
end.

