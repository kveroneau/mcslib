program mccli;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp, netcard, ssockets, cmdparser;

type

  { TMCCLI }

  TMCCLI = class(TCustomApplication)
  protected
    procedure DoRun; override;
  private
    mc: TNetCard;
    blk: TMemoryStream;
    info: PBlockInfo;
    procedure InitNetCard;
    procedure MCShell;
    procedure ReadBlock(blid: word);
    procedure WriteBlock(blid: word);
    procedure NewBlock;
    procedure ShowInfo;
    procedure GetInfo(blid: word);
    procedure FindType(typno: word);
    procedure FindApp(appno: word);
    procedure BlockList;
    procedure AddAuth(perm, token: string);
    procedure ProcessSync(card, blkid: integer);
    procedure SetInfo(appno, typno: word; blktitle: string);
    procedure ExportBlock(Filename: string);
    procedure ImportBlock(Filename: string);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TMCCLI }

procedure TMCCLI.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h', 'help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  blk:=Nil;
  New(info);
  try
    try
      InitNetCard;
      MCShell;
    except
      on ESocketError do WriteLn('Failed to connect to server!');
      on EAuthenticationFailed do WriteLn('Failed to auth!');
    end;
  finally
    mc.Free;
  end;

  if Assigned(blk) then
    blk.Free;
  Dispose(info);

  // stop program loop
  Terminate;
end;

procedure TMCCLI.InitNetCard;
var
  host, port, token: string;
begin
  Write('Host: ');
  ReadLn(host);
  Write('Port: ');
  ReadLn(port);
  Write('Root Token: ');
  ReadLn(token);
  Write('Attempting to connect to ',host,':',port,'...');
  mc:=TNetCard.Create(host, StrToInt(port));
  Write('Connected.'#10'Authenticating...');
  mc.Authenticate(token);
  WriteLn('Done.');
  mc.OnSync:=@ProcessSync;
end;

procedure TMCCLI.MCShell;
var
  line, cmd: string;
  done: Boolean;
begin
  repeat
    Write('MC>');
    ReadLn(line);
    cmd:=getToken(line);
    try
      case cmd of
        'stop': mc.StopServer;
        'card': mc.SelectCard(StrToInt(getToken(line)));
        'read': ReadBlock(StrToInt(getToken(line)));
        'write': WriteBlock(StrToInt(getToken(line)));
        'new': NewBlock;
        'delete': mc.DeleteBlock(StrToInt(getToken(line)));
        'info': GetInfo(StrToInt(getToken(line)));
        'free': WriteLn(' Next Free Block: ',mc.FindFree);
        'findtype': FindType(StrToInt(getToken(line)));
        'findapp': FindApp(StrToInt(getToken(line)));
        'list': BlockList;
        'addauth': AddAuth(getToken(line), getToken(line));
        'sub': mc.Subscribe(StrToInt(getToken(line)));
        'setinfo': SetInfo(StrToInt(getToken(line)), StrToInt(getToken(line)), getToken(line));
        'export': ExportBlock(getToken(line));
        'import': ImportBlock(getToken(line));
      else
        WriteLn('?SYNTAX ERROR');
      end;
      mc.CheckEvents(Self, done);
    except
      On EConvertError do WriteLn(' * Invalid Integer!');
      On EInvalidCard do WriteLn(' * Invalid Card Selected!');
      On EAuthError do WriteLn(' * Access Denied!');
      On ERangeError do WriteLn(' * Invalid arguments for command!');
      On EFOpenError do WriteLn(' * Could not the file specified!');
    end;
  until (cmd = 'exit') or (cmd = 'stop');
end;

procedure TMCCLI.ReadBlock(blid: word);
begin
  if Assigned(blk) then
    blk.Free;
  mc.GetInfo(blid, info);
  blk:=mc.ReadBlock(blid);
end;

procedure TMCCLI.WriteBlock(blid: word);
begin
  if not Assigned(blk) then
  begin
    WriteLn(' * No data to write.');
    Exit;
  end;
  mc.WriteBlock(blid, blk, info);
end;

procedure TMCCLI.NewBlock;
begin
  if mc.BlockSize = 0 then
  begin
    WriteLn(' * Select a card first!');
    Exit;
  end;
  if Assigned(blk) then
    blk.Free;
  blk:=TMemoryStream.Create;
  blk.SetSize(mc.BlockSize);
end;

procedure TMCCLI.ShowInfo;
begin
  WriteLn(' Title: ',info^.title);
  WriteLn(' Type:  ',IntToHex(info^.typno, 2));
  WriteLn(' App:   ',IntToHex(info^.appno, 2));
  WriteLn(' NextID:',info^.nextid);
  WriteLn(' Total: ',info^.total);
end;

procedure TMCCLI.GetInfo(blid: word);
begin
  mc.GetInfo(blid, info);
  ShowInfo;
end;

procedure TMCCLI.FindType(typno: word);
var
  b: integer;
begin
  b:=mc.FindType(typno, info);
  if b = 0 then
  begin
    WriteLn(' * Not found!');
    Exit;
  end;
  WriteLn('BlockID: ',b);
  ShowInfo;
end;

procedure TMCCLI.FindApp(appno: word);
var
  b: integer;
begin
  b:=mc.FindApp(appno, info);
  if b = 0 then
  begin
    WriteLn(' * Not found!');
    Exit;
  end;
  WriteLn('BlockID: ',b);
  ShowInfo;
end;

procedure TMCCLI.BlockList;
var
  lst: TStringList;
  i: integer;
begin
  lst:=mc.BlockList;
  for i:=1 to lst.Count do
    WriteLn(i,' - ',lst.Strings[i-1]);
  lst.Free;
end;

procedure TMCCLI.AddAuth(perm, token: string);
var
  rd, wd: Boolean;
begin
  rd:=False;
  wd:=False;
  if perm = 'r' then
    rd:=True
  else if perm = 'w' then
    wd:=True
  else if perm = 'rw' then
  begin
    rd:=True;
    wd:=True;
  end;
  mc.AddAuth(token, rd, wd);
end;

procedure TMCCLI.ProcessSync(card, blkid: integer);
begin
  WriteLn(' * Subscription Sync event!');
  WriteLn(' - Card: ',card,' on Block: ',blkid);
end;

procedure TMCCLI.SetInfo(appno, typno: word; blktitle: string);
begin
  info^.title:=blktitle;
  info^.typno:=typno;
  info^.appno:=appno;
end;

procedure TMCCLI.ExportBlock(Filename: string);
begin
  if not Assigned(blk) then
  begin
    WriteLn(' * Read a Block first!');
    Exit;
  end;
  blk.SaveToFile(Filename);
end;

procedure TMCCLI.ImportBlock(Filename: string);
begin
  NewBlock;
  if mc.BlockSize = 0 then
    Exit;
  blk.LoadFromFile(Filename);
  blk.SetSize(mc.BlockSize);
end;

constructor TMCCLI.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TMCCLI.Destroy;
begin
  inherited Destroy;
end;

procedure TMCCLI.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TMCCLI;
begin
  Application:=TMCCLI.Create(nil);
  Application.Title:='MCServer CLI';
  Application.Run;
  Application.Free;
end.

