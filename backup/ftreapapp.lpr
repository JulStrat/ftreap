program ftreapapp;

uses
  ftreap, dos;
  gset, gutil;

type
  TIntTreapNode = specialize TTreapNode<longint>;
  lesslli = specialize TLess<longint>;
  setlli = specialize TSet<longint, lesslli>;

var
  ra: TIntTreapNode = nil;
  i: longint;
  data: setlli;
  start, endt: Int64;

begin
  start := GetMsCount;
  for i := 0 to 1000356 do
  begin
    ra := TIntTreapNode.Insert(ra, i);
  end;
  endt := GetMsCount;
  WriteLn(ra.size);
  WriteLn(ra.val);
  WriteLn(endt - start);
  ReadLn();

  start := GetMsCount;
  data := setlli.Create;
  for i := 0 to 1000356 do
  begin
    data.insert(i);
  end;
  endt := GetMsCount;
  WriteLn(endt - start);
  ReadLn();

end.
