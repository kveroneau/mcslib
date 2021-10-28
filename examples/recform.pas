unit recform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  myrec, netcard;

type

  { TRecordForm }

  TRecordForm = class(TForm)
    AddBtn: TButton;
    NextBtn: TButton;
    PrevBtn: TButton;
    NewBtn: TButton;
    FirstBtn: TButton;
    EGender: TComboBox;
    EAge: TEdit;
    EName: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    MemSize: TLabel;
    RecSize: TLabel;
    StatusBar: TStatusBar;
    procedure AddBtnClick(Sender: TObject);
    procedure FirstBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure NewBtnClick(Sender: TObject);
    procedure NextBtnClick(Sender: TObject);
    procedure PrevBtnClick(Sender: TObject);
  private
    FPerson: TPerson;
    FDB: TMemoryStream;
    FCurRec: integer;
    FCard: TNetCard;
    FBlock: TMemoryStream;
    FBlockId: integer;
    FBlockInfo: PBlockInfo;
    procedure ToRecord;
    procedure FromRecord;
    procedure ClearForm;
    procedure LoadRecord(const rec: integer);
    function RecordCount: integer;
    procedure UpdateStatus;
    procedure HandleSync(card, blkid: integer);
    procedure SaveBlock;
  public

  end;

var
  RecordForm: TRecordForm;

implementation

const
  APP_ID = 34;
  APP_TITLE = 'RecDBTest';

{$R *.lfm}

{ TRecordForm }

procedure TRecordForm.FormCreate(Sender: TObject);
begin
  RecSize.Caption:=IntToStr(SizeOf(FPerson));
  EName.MaxLength:=SizeOf(FPerson.name);
  FDB:=TMemoryStream.Create;
  MemSize.Caption:=IntToStr(FDB.Size);
  FCurRec:=-1;
  FCard:=TNetCard.Create('192.168.100.152', 3846);
  FCard.Authenticate('TestKey123');
  FCard.SelectCard(0);
  FBlock:=Nil;
  New(FBlockInfo);
  FBlockInfo^.typno:=0;
  FBlockId:=FCard.FindApp(APP_ID, FBlockInfo);
  if FBlockId = 0 then
    FBlockId:=FCard.FindType(MYREC_TYP, FBlockInfo);
  if FBlockId = 0 then
    FBlockId:=FCard.FindFree
  else
  begin
    FBlock:=FCard.ReadBlock(FBlockId);
    FDB.LoadFromStream(FBlock);
    FDB.SetSize(FBlockInfo^.total);
    MemSize.Caption:=IntToStr(FDB.Size);
    LoadRecord(0);
  end;
  if FBlockInfo^.typno <> MYREC_TYP then
    with FBlockInfo^ do
    begin
      title:=APP_TITLE;
      appno:=APP_ID;
      typno:=MYREC_TYP;
      total:=0;
      nextid:=0;
    end;
  FCard.OnSync:=@HandleSync;
  FCard.Subscribe(FBlockId);
  Application.OnIdle:=@FCard.CheckEvents;
end;

procedure TRecordForm.AddBtnClick(Sender: TObject);
begin
  ToRecord;
  if FCurRec > -1 then
    FDB.Position:=FCurRec*SizeOf(FPerson)
  else
    FDB.Seek(0, soEnd);
  FDB.Write(FPerson, SizeOf(FPerson));
  SaveBlock;
  if FCurRec = -1 then
    ClearForm;
end;

procedure TRecordForm.FirstBtnClick(Sender: TObject);
begin
  LoadRecord(0);
end;

procedure TRecordForm.FormDestroy(Sender: TObject);
begin
  SaveBlock;
  FDB.Free;
  Dispose(FBlockInfo);
  FBlock.Free;
  FCard.Free;
end;

procedure TRecordForm.NewBtnClick(Sender: TObject);
begin
  ClearForm;
  FCurRec:=-1;
end;

procedure TRecordForm.NextBtnClick(Sender: TObject);
begin
  if FCurRec = RecordCount-1 then
    Exit;
  LoadRecord(FCurRec+1);
end;

procedure TRecordForm.PrevBtnClick(Sender: TObject);
begin
  if FCurRec < 1 then
    Exit;
  LoadRecord(FCurRec-1);
end;

procedure TRecordForm.ToRecord;
begin
  with FPerson do
  begin
    name:=EName.Text;
    age:=StrToInt(EAge.Text);
    gender:=EGender.ItemIndex;
  end;
end;

procedure TRecordForm.FromRecord;
begin
  with FPerson do
  begin
    EName.Text:=name;
    EAge.Text:=IntToStr(age);
    EGender.ItemIndex:=gender;
  end;
end;

procedure TRecordForm.ClearForm;
begin
  MemSize.Caption:=IntToStr(FDB.Size);
  EName.Text:='';
  EAge.Text:='';
  EGender.ItemIndex:=0;
  StatusBar.SimpleText:='Record count: '+IntToStr(RecordCount);
end;

procedure TRecordForm.LoadRecord(const rec: integer);
begin
  FCurRec:=rec;
  FDB.Position:=FCurRec*SizeOf(FPerson);
  FDB.Read(FPerson, SizeOf(FPerson));
  FromRecord;
  UpdateStatus;
end;

function TRecordForm.RecordCount: integer;
begin
  Result:=FDB.Size div SizeOf(FPerson);
end;

procedure TRecordForm.UpdateStatus;
begin
  StatusBar.SimpleText:='Current Record: '+IntToStr(FCurRec);
end;

procedure TRecordForm.HandleSync(card, blkid: integer);
begin
  WriteLn('Got Sync Event!  Card: ',card,', Block: ',blkid);
  if blkid <> FBlockId then
    Exit;
  FBlock.Free;
  FBlock:=FCard.ReadBlock(FBlockId);
  FCard.GetInfo(FBlockId, FBlockInfo);
  FDB.LoadFromStream(FBlock);
  FDB.SetSize(FBlockInfo^.total);
  LoadRecord(FCurRec);
  StatusBar.SimpleText:='Sync Complete!';
  MemSize.Caption:=IntToStr(FDB.Size);
end;

procedure TRecordForm.SaveBlock;
begin
  if FBlock = Nil then
    FBlock:=FCard.ReadBlock(FBlockId);
  FBlock.Position:=0;
  FDB.SaveToStream(FBlock);
  FBlockInfo^.total:=FDB.Size;
  FCard.WriteBlock(FBlockId, FBlock, FBlockInfo);
end;

end.

