unit treaptestcase;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, ftreap;

const
  KEYS_NUMBER = 10000;

type
  TIntTreapNode = specialize TTreapNode<longint>;

  TTreapTestCase = class(TTestCase)
  protected
    root: TIntTreapNode;
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestHookUp;
    procedure TestStructure;
    procedure TestFindExisting;
    procedure TestFindNotExisting;
    procedure TestGetPosition;
    procedure TestGetAt;
    procedure TestRemove;
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
  i: longint;
begin
  try
    begin
      for i := 0 to KEYS_NUMBER - 1 do
        AssertEquals(True, TIntTreapNode.Contains(root, 2 * i));
    end;
  except
    Fail('Find existing key failed!');
  end;
end;

procedure TTreapTestCase.TestFindNotExisting;
var
  i: longint;
begin
  try
    begin
      for i := 0 to KEYS_NUMBER - 1 do
        AssertEquals(False, TIntTreapNode.Contains(root, 2 * i + 1));
    end;
  except
    Fail('Find not existing key failed!');
  end;
end;

procedure TTreapTestCase.TestGetPosition;
var
  i: longint;
begin
  try
    begin
      for i := 0 to KEYS_NUMBER - 1 do
        AssertEquals(i, TIntTreapNode.GetPosition(root, 2 * i));
    end;
  except
    Fail('GetPosition failed!');
  end;
end;

procedure TTreapTestCase.TestGetAt;
var
  i: longint;
begin
  try
    begin
      for i := 0 to KEYS_NUMBER - 1 do
        AssertEquals(2 * i, TIntTreapNode.GetAt(root, i));
    end;
  except
    Fail('GetAt failed!');
  end;
end;

procedure TTreapTestCase.TestRemove;
var
  i: longint;
begin
  try
    begin
      for i := 0 to KEYS_NUMBER - 1 do
      begin
        TIntTreapNode.Remove(root, 2 * i);
        AssertEquals(False, TIntTreapNode.Contains(root, 2 * i));
      end;
      AssertEquals(0, TIntTreapNode.GetSize(root));
    end;
  except
    Fail('GetPosition failed!');
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
      AssertEquals(2 * (KEYS_NUMBER - 1), TIntTreapNode.Max(root));
    end;
  except
    Fail('Invalid max key!');
  end;
end;

procedure TTreapTestCase.SetUp;
var
  i: longint;
begin
  for i := 0 to KEYS_NUMBER - 1 do
    TIntTreapNode.Insert(root, 2 * i);
end;

procedure TTreapTestCase.TearDown;
begin
  TIntTreapNode.DestroyTreap(root);
end;

initialization
  RegisterTest(TTreapTestCase);
end.

