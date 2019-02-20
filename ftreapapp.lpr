program ftreapapp;

uses
  ftreap, gset, gutil;

type
  TIntTreapNode = specialize TTreapNode<longint>;
  lesslli = specialize TLess<longint>;
  setlli = specialize TSet<longint, lesslli>;

var
  ra: TIntTreapNode = nil;
  i: longint;
  data: setlli;

begin
  for i := 0 to 1000356 do
  begin
    ra := TIntTreapNode.Insert(ra, i);
  end;
  WriteLn(ra.size);
  WriteLn(ra.val);
  data := setlli.Create;
  for i := 0 to 1000356 do
  begin
    data.insert(i);
  end;

end.
