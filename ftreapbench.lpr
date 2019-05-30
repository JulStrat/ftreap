{$mode delphi}
program ftreapbench;

uses
  SysUtils, treap;

type
  TInt64TreapNode = TTreap<Int64>;

var
  ra: TInt64TreapNode;
  i: longint;
  tck: DWORD;

begin
  ra := TInt64TreapNode.Create;
  tck := GetTickCount64();
  for i := 0 to 1000000 - 1 do
  begin
    ra.Insert(Random(9223372036854775807));
  end;
  WriteLn('Tick count - ', GetTickCount64() - tck);
  ra.Free;
end.

