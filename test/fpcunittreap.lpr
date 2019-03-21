program fpcunittreap;

{$mode objfpc}{$H+}

uses
  // {$ifopt D-}CMem,{$endif}
  Interfaces, Forms, GuiTestRunner, treaptestcase;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

