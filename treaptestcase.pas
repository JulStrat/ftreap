unit treaptestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ftreap;

const
  KEYS_NUMBER = 10000;

type
  TIntTreapNode = specialize TTreapNode<LongInt>;

  TTreapTestCase= class(TTestCase)
  protected
    root: TIntTreapNode;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure TestStructure;
    procedure TestFindExisting;
    procedure TestFindNotExisting;
    procedure TestMin;
    procedure TestMax;
  end;

implementation

procedure TTreapTestCase.TestHookUp;
begin
  //Fail('Write your own test');
end;

procedure TTreapTestCase.TestStructure;
begin
  try
  begin
    AssertEquals(True, TIntTreapNode.CheckStucture(root));
  end;
  except
    Fail('Invalid Treap structure!');
  end;
end;

procedure TTreapTestCase.TestFindExisting;
var
  i: LongInt;
begin
  try
  begin
    for i := 0 to KEYS_NUMBER-1 do
      AssertEquals(True, TIntTreapNode.Find(root, 2*i));
  end;
  except
    Fail('Find existing key failed!');
  end;
end;

procedure TTreapTestCase.TestFindNotExisting;
var
  i: LongInt;
begin
  try
  begin
    for i := 0 to KEYS_NUMBER-1 do
      AssertEquals(False, TIntTreapNode.Find(root, 2*i+1));
  end;
  except
    Fail('Find not existing key failed!');
  end;
end;

procedure TTreapTestCase.TestMin;
begin
  try
  begin
    AssertEquals(0, TIntTreapNode.Min(root));
  end;
  except
    Fail('Invalid min key!');
  end;
end;

procedure TTreapTestCase.TestMax;
begin
  try
  begin
    AssertEquals(2*(KEYS_NUMBER-1), TIntTreapNode.Max(root));
  end;
  except
    Fail('Invalid max key!');
  end;
end;

procedure TTreapTestCase.SetUp;
var
  i: LongInt;
begin
  for i := 0 to KEYS_NUMBER-1 do
    TIntTreapNode.Insert(root, 2*i);
end;

procedure TTreapTestCase.TearDown;
begin
  TIntTreapNode.DestroyTreap(root);
end;

initialization
  RegisterTest(TTreapTestCase);
end.

