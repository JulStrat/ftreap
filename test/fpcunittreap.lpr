program fpcunittreap;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, treaptestcase;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

