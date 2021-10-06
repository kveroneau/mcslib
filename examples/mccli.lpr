program mccli;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, netcard;

type

  { TMCApp }

  TMCApp = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMCApp }

procedure TMCApp.DoRun;
var
  ErrorMsg: String;
  port: integer;
  mc: TNetCard;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', ['help', 'host:', 'port:', 'token:', 'stop']);
  if ErrorMsg<>'' then
  begin
    WriteLn(ErrorMsg);
    WriteHelp;
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if not (HasOption('host') and HasOption('port') and HasOption('token')) then
  begin
    WriteLn('Missing options.  Please see --help.');
    Terminate;
    Exit;
  end;

  port:=StrToInt(GetOptionValue('port'));
  WriteLn('Attempting connection to server...');
  mc:=TNetCard.Create(GetOptionValue('host'), port);
  try
    WriteLn('Attempting to authenticate using token...');
    mc.Authenticate(GetOptionValue('token'));
    WriteLn('Looks like everything is in order.');
    if HasOption('stop') then
      mc.StopServer;
  finally
    mc.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TMCApp.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMCApp.Destroy;
begin
  inherited Destroy;
end;

procedure TMCApp.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h --host=server --port=3845 --token=TestKey123');
end;

var
  Application: TMCApp;
begin
  Application:=TMCApp.Create(nil);
  Application.Title:='MC CLI App';
  Application.Run;
  Application.Free;
end.

