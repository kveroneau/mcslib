program demoapp;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF USE_THREADS}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, webform
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TSyncForm, SyncForm);
  Application.Run;
end.

