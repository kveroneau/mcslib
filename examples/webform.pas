unit webform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, netcard,
  webdata;

type

  { TSyncThread }

  TSyncThread = class(TThread)
  private
    mc: TNetCard;
    blkid: integer;
    lock: TRTLCriticalSection;
    running: Boolean;
    procedure PerformSync(card, bid: integer);
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Stop;
    procedure GetWebData(data: PWebData; content: TStrings);
  end;

  { TSyncForm }

  TSyncForm = class(TForm)
    SaveBtn: TButton;
    SiteContent: TMemo;
    SiteVisits: TEdit;
    Label2: TLabel;
    MyFlag: TCheckBox;
    PageTitle: TEdit;
    Label1: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
  private
    FWebData: PWebData;
    mc: TNetCard;
    blkid: integer;
    procedure PerformSync(card, bid: integer);
  public
    procedure UpdateData;
  end;

var
  SyncForm: TSyncForm;
  SyncThread: TSyncThread;

implementation

const
  MCS_HOST = 'dev-02.home.lan';
  MCS_PORT = 3846;
  MCS_KEY = 'WebTest123';
  MCS_CARD = 0;

{$R *.lfm}

{ TSyncForm }

procedure TSyncForm.FormCreate(Sender: TObject);
var
  info: TBlockInfo;
begin
  {SyncThread:=TSyncThread.Create;
  SyncThread.Start;
  UpdateData;}
  New(FWebData);
  mc:=TNetCard.Create(MCS_HOST, MCS_PORT);
  mc.Authenticate(MCS_KEY);
  mc.SelectCard(MCS_CARD);
  blkid:=mc.FindType(WD_TYP, @info);
  mc.OnSync:=@PerformSync;
  mc.Subscribe(blkid);
  PerformSync(MCS_CARD, blkid);
  Application.OnIdle:=@mc.CheckEvents;
end;

procedure TSyncForm.FormDestroy(Sender: TObject);
begin
  {SyncThread.Stop;
  repeat
    Sleep(10);
  until SyncThread.Finished;
  SyncThread.Free;}
  mc.Free;
  Dispose(FWebData);
end;

procedure TSyncForm.SaveBtnClick(Sender: TObject);
var
  blk: TMemoryStream;
begin
  blk:=TMemoryStream.Create;
  blk.SetSize(mc.BlockSize);
  FWebData^.title:=PageTitle.Text;
  FWebData^.myflag:=MyFlag.Checked;
  FWebData^.visits:=StrToInt(SiteVisits.Text);
  blk.Write(FWebData^, SizeOf(FWebData^));
  blk.WriteAnsiString(SiteContent.Text);
  try
    mc.WriteBlock(blkid, blk);
  except
    On EAuthError do ShowMessage('Access Denied, no write access.');
  end;
  blk.Free;
end;

procedure TSyncForm.PerformSync(card, bid: integer);
var
  blk: TMemoryStream;
begin
  blk:=mc.ReadBlock(bid);
  try
    blk.Read(FWebData^, SizeOf(FWebData^));
    PageTitle.Text:=FWebData^.title;
    MyFlag.Checked:=FWebData^.myflag;
    SiteVisits.Text:=IntToStr(FWebData^.visits);
    SiteContent.Text:=blk.ReadAnsiString;
  finally
    blk.Free;
  end;
end;

procedure TSyncForm.UpdateData;
begin
  try
    SyncThread.GetWebData(FWebData, SiteContent.Lines);
  except
    On EAuthError do ShowMessage('Access Denied!');
  end;
end;

{ TSyncThread }

procedure TSyncThread.PerformSync(card, bid: integer);
begin
  Synchronize(@SyncForm.UpdateData);
end;

procedure TSyncThread.Execute;
var
  done: boolean;
begin
  running:=True;
  repeat
    Sleep(1000);
    mc.CheckEvents(Self, done);
  until not running;
end;

constructor TSyncThread.Create;
var
  info: TBlockInfo;
begin
  inherited Create(True);
  FreeOnTerminate:=False;
  mc:=TNetCard.Create(MCS_HOST, MCS_PORT);
  mc.Authenticate(MCS_KEY);
  mc.SelectCard(MCS_CARD);
  blkid:=mc.FindType(WD_TYP, @info);
  mc.OnSync:=@PerformSync;
  mc.Subscribe(blkid);
  InitCriticalSection(lock);
end;

destructor TSyncThread.Destroy;
begin
  mc.Free;
  DoneCriticalSection(lock);
  inherited Destroy;
end;

procedure TSyncThread.Stop;
begin
  EnterCriticalSection(lock);
  try
    running:=False;
  finally
    LeaveCriticalSection(lock);
  end;
end;

procedure TSyncThread.GetWebData(data: PWebData; content: TStrings);
var
  blk: TMemoryStream;
begin
  EnterCriticalSection(lock);
  try
    blk:=mc.ReadBlock(blkid);
    blk.Read(data^, SizeOf(data^));
    content.Text:=blk.ReadAnsiString;
    blk.Free;
  finally
    LeaveCriticalSection(lock);
  end;
end;

end.

