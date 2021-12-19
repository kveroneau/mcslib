unit netcard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ssockets;

type

  TMCData = Array[0..30] of Byte;
  PMCProtocol = ^TMCProtocol;
  TMCProtocol = packed record
    op: Byte;
    data: TMCData;
  end;
  PBlockInfo = ^TBlockInfo;
  TBlockInfo = packed record
    title: String[20];
    typno: Byte;
    appno: Byte;
    nextid: Byte;
    total: Word;
  end;
  PBlockList = ^TBlockList;
  TBlockList = array[1..15] of string[20];
  TCardAuth = record
    key: string[20];
    read, write: boolean;
  end;

  TSyncEvent = Procedure(card, blkid: integer) of Object;

  EAuthenticationFailed = Class(Exception);
  EInvalidCard = Class(Exception);
  EAuthError = Class(Exception);

  { TNetCard }

  TNetCard = class(TObject)
  private
    FHost: string;
    FPort: integer;
    FSock: TInetSocket;
    FBlockSize: integer;
    FOnSync: TSyncEvent;
    procedure ProcessEvent(buf: PMCProtocol);
    procedure SyncNotify(data: TMCData);
  public
    constructor Create(AHost: string; APort: Integer);
    destructor Destroy; override;
    procedure Authenticate(key: string);
    procedure StopServer;
    procedure SelectCard(card: word);
    function ReadBlock(blkid: word): TMemoryStream;
    procedure WriteBlock(blkid: word; blk: TMemoryStream);
    procedure WriteBlock(blkid: word; blk: TMemoryStream; info: PBlockInfo);
    procedure DeleteBlock(blkid: word);
    procedure GetInfo(blkid: word; info: PBlockInfo);
    function FindFree: integer;
    function FindType(typno: word; info: PBlockInfo): integer;
    function FindApp(appno: word; info: PBlockInfo): integer;
    function BlockList: TStringList;
    procedure Subscribe(blkid: word);
    procedure AddAuth(const key: string; read, write: boolean);
    procedure CheckEvents(Sender: TObject; var Done: Boolean);
    Property OnSync: TSyncEvent read FOnSync write FOnSync;
    Property BlockSize: integer read FBlockSize;
  end;

implementation

{ TNetCard }

procedure TNetCard.ProcessEvent(buf: PMCProtocol);
begin
  case buf^.op of
    120: SyncNotify(buf^.data);
  end;
end;

procedure TNetCard.SyncNotify(data: TMCData);
var
  card, blkid: ^Word;
begin
  card:=@data[0];
  blkid:=@data[2];
  if Assigned(FOnSync) then
    FOnSync(card^, blkid^);
end;

constructor TNetCard.Create(AHost: string; APort: Integer);
begin
  FHost:=AHost;
  FPort:=APort;
  FSock:=TInetSocket.Create(FHost, FPort);
end;

destructor TNetCard.Destroy;
begin
  FSock.Free;
  inherited Destroy;
end;

procedure TNetCard.Authenticate(key: string);
var
  buf: TMCProtocol;
  s: string[15];
begin
  buf.op:=20;
  s:=key;
  Move(s[0], buf.data, 15);
  FSock.Write(buf, SizeOf(buf));
  FSock.Read(buf, SizeOf(buf));
  if buf.op <> 200 then
    Raise EAuthenticationFailed.Create('Authentication failure.');
end;

procedure TNetCard.StopServer;
var
  buf: TMCProtocol;
begin
  buf.op:=30;
  FSock.Write(buf, SizeOf(buf));
end;

procedure TNetCard.SelectCard(card: word);
var
  buf: TMCProtocol;
begin
  buf.op:=10;
  Move(card, buf.data, 2);
  FSock.Write(buf, SizeOf(buf));
  FBlockSize:=FSock.ReadWord;
  if FBlockSize = 0 then
    Raise EInvalidCard.Create('Invalid card selected or no access.');
end;

function TNetCard.ReadBlock(blkid: word): TMemoryStream;
var
  buf: TMCProtocol;
begin
  Result:=Nil;
  buf.op:=40;
  Move(blkid, buf.data, 2);
  FSock.Write(buf, SizeOf(buf));
  FSock.Read(buf, SizeOf(buf));
  if buf.op <> 200 then
    Raise EAuthError.Create('Access Denied.');
  Result:=TMemoryStream.Create;
  Result.SetSize(FBlockSize);
  Result.CopyFrom(FSock, FBlockSize);
  Result.Position:=0;
end;

procedure TNetCard.WriteBlock(blkid: word; blk: TMemoryStream);
var
  buf: TMCProtocol;
begin
  buf.op:=50;
  Move(blkid, buf.data, 2);
  buf.data[2]:=0;
  FSock.Write(buf, SizeOf(buf));
  blk.Position:=0;
  FSock.CopyFrom(blk, FBlockSize);
  FSock.Read(buf, SizeOf(buf));
  if buf.op <> 200 then
    Raise EAuthError.Create('Access Denied.');
end;

procedure TNetCard.WriteBlock(blkid: word; blk: TMemoryStream; info: PBlockInfo
  );
var
  buf: TMCProtocol;
begin
  buf.op:=50;
  Move(blkid, buf.data, 2);
  buf.data[2]:=128;
  Move(info^, buf.data[3], SizeOf(info^));
  FSock.Write(buf, SizeOf(buf));
  FSock.Read(buf, SizeOf(buf));
  if buf.op <> 200 then
    Raise EAuthError.Create('Access Denied.');
  blk.Position:=0;
  FSock.CopyFrom(blk, FBlockSize);
end;

procedure TNetCard.DeleteBlock(blkid: word);
var
  buf: TMCProtocol;
begin
  buf.op:=60;
  Move(blkid, buf.data, 2);
  FSock.Write(buf, SizeOf(buf));
  FSock.Read(buf, SizeOf(buf));
  if buf.op <> 200 then
    Raise EAuthError.Create('Access Denied.');
end;

procedure TNetCard.GetInfo(blkid: word; info: PBlockInfo);
var
  buf: TMCProtocol;
begin
  buf.op:=70;
  Move(blkid, buf.data, 2);
  FSock.Write(buf, SizeOf(buf));
  FSock.Read(buf, SizeOf(buf));
  if buf.op <> 200 then
    Raise EAuthError.Create('Access Denied.');
  FSock.Read(info^, SizeOf(info^));
end;

function TNetCard.FindFree: integer;
var
  buf: TMCProtocol;
begin
  buf.op:=80;
  FSock.Write(buf, SizeOf(buf));
  Result:=FSock.ReadWord;
  if Result = 0 then
    Raise EAuthError.Create('Access Denied.');
end;

function TNetCard.FindType(typno: word; info: PBlockInfo): integer;
var
  buf: TMCProtocol;
  blkid: ^word;
begin
  buf.op:=90;
  Move(typno, buf.data, 2);
  FSock.Write(buf, SizeOf(buf));
  FSock.Read(buf, SizeOf(buf));
  if buf.op <> 90 then
    Raise EAuthError.Create('Access Denied.');
  blkid:=@buf.data[0];
  Result:=blkid^;
  if blkid^ > 0 then
    Move(buf.data[2], info^, SizeOf(info^));
end;

function TNetCard.FindApp(appno: word; info: PBlockInfo): integer;
var
  buf: TMCProtocol;
  blkid: ^word;
begin
  buf.op:=100;
  Move(appno, buf.data, 2);
  FSock.Write(buf, SizeOf(buf));
  FSock.Read(buf, SizeOf(buf));
  if buf.op <> 100 then
    Raise EAuthError.Create('Access Denied.');
  blkid:=@buf.data[0];
  Result:=blkid^;
  if blkid^ > 0 then
    Move(buf.data[2], info^, SizeOf(info^));
end;

function TNetCard.BlockList: TStringList;
var
  buf: TMCProtocol;
  list: TBlockList;
  i: integer;
begin
  buf.op:=110;
  FSock.Write(buf, SizeOf(buf));
  FSock.Read(buf, SizeOf(buf));
  if buf.op <> 200 then
    Raise EAuthError.Create('Access Denied.');
  FSock.Read(list, SizeOf(list));
  Result:=TStringList.Create;
  for i := 1 to 15 do
    Result.Add(list[i]);
end;

procedure TNetCard.Subscribe(blkid: word);
var
  buf: TMCProtocol;
begin
  buf.op:=120;
  Move(blkid, buf.data, 2);
  FSock.Write(buf, SizeOf(buf));
  FSock.Read(buf, SizeOf(buf));
  if buf.op <> 200 then
    Raise EAuthError.Create('Access Denied.');
end;

procedure TNetCard.AddAuth(const key: string; read, write: boolean);
var
  buf: TMCProtocol;
  auth: TCardAuth;
begin
  buf.op:=140;
  auth.key:=key;
  auth.read:=read;
  auth.write:=write;
  Move(auth, buf.data, SizeOf(auth));
  FSock.Write(buf, SizeOf(buf));
  FSock.Read(buf, SizeOf(buf));
  if buf.op <> 200 then
    Raise EAuthError.Create('Access Denied.');
end;

procedure TNetCard.CheckEvents(Sender: TObject; var Done: Boolean);
var
  size: integer;
  buf: TMCProtocol;
begin
  FSock.IOTimeout:=1;
  size:=FSock.Read(buf, SizeOf(buf));
  FSock.IOTimeout:=0;
  if size > 0 then
    ProcessEvent(@buf);
  Done:=True;
end;

end.

