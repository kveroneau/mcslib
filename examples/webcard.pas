unit webcard;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, netcard, webdata;

type

  { TWebCard }

  TWebCard = class(TObject)
  private
    mc: TNetCard;
    blkid: word;
    blk: TMemoryStream;
    mydata: PWebData;
    mycontent: string;
    procedure CreateData;
    function GetVisits: integer;
  public
    constructor Create;
    destructor Destroy; override;
    Procedure TemplateParam(Sender : TObject; Const ParamName : String; Out AValue : String);
    Procedure LoadBlock;
    Procedure SaveBlock;
  end;

var
  wc: TWebCard;

implementation

const
  MCS_HOST = 'dev-02.home.lan';
  MCS_PORT = 3846;
  MCS_KEY  = 'WebTest123';
  MCS_CARD = 0;

{ TWebCard }

procedure TWebCard.CreateData;
var
  info: TBlockInfo;
begin
  blkid:=mc.FindFree;
  if blkid = 0 then
  begin
    WriteLn(' * Card full!');
    Exit;
  end;
  info.title:='HTTP Server App Demo';
  info.appno:=WD_APP;
  info.typno:=WD_TYP;
  info.nextid:=0;
  info.total:=SizeOf(mydata^);
  mydata^.title:=info.title;
  mydata^.myflag:=False;
  mydata^.visits:=0;
  blk:=mc.ReadBlock(blkid);
  blk.Write(mydata^, SizeOf(mydata^));
  blk.WriteAnsiString('Some example content stored in MCS.');
  mc.WriteBlock(blkid, blk, @info);
  blk.Free;
  blk:=Nil;
end;

function TWebCard.GetVisits: integer;
begin
  Inc(mydata^.visits);
  Result:=mydata^.visits;
end;

constructor TWebCard.Create;
var
  info: TBlockInfo;
begin
  blk:=Nil;
  New(mydata);
  mc:=TNetCard.Create(MCS_HOST, MCS_PORT);
  mc.Authenticate(MCS_KEY);
  mc.SelectCard(MCS_CARD);
  blkid:=mc.FindType(WD_TYP, @info);
  if blkid = 0 then
    CreateData;
end;

destructor TWebCard.Destroy;
begin
  mc.Free;
  if Assigned(blk) then
    blk.Free;
  Dispose(mydata);
  inherited Destroy;
end;

procedure TWebCard.TemplateParam(Sender: TObject; const ParamName: String; out
  AValue: String);
begin
  case ParamName of
    'title': AValue:=mydata^.title;
    'visits': AValue:=IntToStr(GetVisits);
    'content': AValue:=mycontent;
  else
    AValue:='?UNKNOWN VARIABLE';
  end;
end;

procedure TWebCard.LoadBlock;
begin
  if Assigned(blk) then
    blk.Free;
  blk:=mc.ReadBlock(blkid);
  blk.Read(mydata^, SizeOf(mydata^));
  mycontent:=blk.ReadAnsiString;
end;

procedure TWebCard.SaveBlock;
begin
  blk.Position:=0;
  blk.Write(mydata^, SizeOf(mydata^));
  mc.WriteBlock(blkid, blk);
  blk.Free;
  blk:=Nil;
end;

end.

