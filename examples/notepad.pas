unit notepad;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Menus, StdCtrls,
  ExtCtrls, ComCtrls, netfile, netcard;

type

  { TNotepadForm }

  TNotepadForm = class(TForm)
    MainMenu: TMainMenu;
    FileMenu: TMenuItem;
    EditMenu: TMenuItem;
    MemoPad: TMemo;
    MenuItem1: TMenuItem;
    CloseConnMenu: TMenuItem;
    NewMenu: TMenuItem;
    OpenMenu: TMenuItem;
    SaveMenu: TMenuItem;
    SaveAsMenu: TMenuItem;
    ExitMenu: TMenuItem;
    StatusBar: TStatusBar;
    Timer: TTimer;
    procedure CloseConnMenuClick(Sender: TObject);
    procedure ExitMenuClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure MemoPadChange(Sender: TObject);
    procedure NewMenuClick(Sender: TObject);
    procedure OpenMenuClick(Sender: TObject);
    procedure SaveAsMenuClick(Sender: TObject);
    procedure SaveMenuClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FBlockID: integer;
    FBlock: TMemoryStream;
    procedure SaveBlock(const Title: string);
    procedure ProcessSync(cid, blkid: integer);
    procedure UpdateSize;
  public

  end;

var
  NotepadForm: TNotepadForm;

implementation

{$R *.lfm}

{ TNotepadForm }

procedure TNotepadForm.FormResize(Sender: TObject);
begin
  MemoPad.Width:=ClientWidth;
  MemoPad.Height:=ClientHeight-StatusBar.Height;
end;

procedure TNotepadForm.MemoPadChange(Sender: TObject);
begin
  UpdateSize;
end;

procedure TNotepadForm.NewMenuClick(Sender: TObject);
begin
  MemoPad.Clear;
  FBlockID:=-1;
end;

procedure TNotepadForm.OpenMenuClick(Sender: TObject);
var
  f: TNetFiler;
begin
  f:=TNetFiler.Create(Self);
  try
    {f.FilterType:=8;}
    f.OkayBtn.Caption:='Open';
    if Assigned(Card) then
    begin
      f.ListBtn.Enabled:=False;
      f.OkayBtn.Enabled:=True;
      f.GetList;
    end;
    if f.ShowModal <> mrOK then
      Exit;
    CloseConnMenu.Enabled:=True;
    MemoPad.Clear;
    Caption:=f.BlockTitle+' : Network Notepad';
    if Assigned(FBlock) then
      FBlock.Free;
    FBlock:=Card.ReadBlock(f.BlockID);
    MemoPad.Text:=FBlock.ReadAnsiString;
    FBlockID:=f.BlockID;
    Card.OnSync:=@ProcessSync;
    Card.Subscribe(FBlockID);
  finally
    f.Free;
  end;
end;

procedure TNotepadForm.SaveAsMenuClick(Sender: TObject);
var
  f: TNetFiler;
begin
  f:=TNetFiler.Create(Self);
  try
    f.OkayBtn.Caption:='Save';
    if Assigned(Card) then
    begin
      f.ListBtn.Enabled:=False;
      f.OkayBtn.Enabled:=True;
      f.GetList;
    end;
    if f.ShowModal <> mrOK then
      Exit;
    CloseConnMenu.Enabled:=True;
    FBlockID:=f.BlockID;
    SaveBlock(f.BlockTitle);
  finally
    f.Free;
  end;
end;

procedure TNotepadForm.SaveMenuClick(Sender: TObject);
begin
  if FBlockID = -1 then
  begin
    SaveAsMenuClick(Sender);
    Exit;
  end;
  SaveBlock('KEEP');
end;

procedure TNotepadForm.TimerTimer(Sender: TObject);
var
  done: Boolean;
begin
  if Assigned(Card) then
    Card.CheckEvents(Sender, done);
end;

procedure TNotepadForm.SaveBlock(const Title: string);
var
  info: TBlockInfo;
begin
  if not Assigned(FBlock) then
    FBlock:=Card.ReadBlock(FBlockID);
  FBlock.Position:=0;
  FBlock.WriteAnsiString(MemoPad.Text);
  if Title <> 'KEEP' then
  begin
    info.title:=InputBox('Save As...', 'Enter Block Title:', Title);
    info.appno:=8;
    info.typno:=8;
    info.total:=Length(MemoPad.Text);
    info.nextid:=0;
    Card.WriteBlock(FBlockID, FBlock, @info);
    Caption:=info.title+' : Network Notepad';
    Card.OnSync:=@ProcessSync;
    Card.Subscribe(FBlockID);
  end
  else
    Card.WriteBlock(FBlockID, FBlock);
end;

procedure TNotepadForm.ProcessSync(cid, blkid: integer);
begin
  if Assigned(FBlock) then
    FBlock.Free;
  FBlock:=Card.ReadBlock(blkid);
  MemoPad.Text:=FBlock.ReadAnsiString;
  FBlockID:=blkid;
end;

procedure TNotepadForm.UpdateSize;
begin
  if Assigned(Card) then
    StatusBar.SimpleText:='Characters: '+IntToStr(Memopad.GetTextLen)+'/'+IntToStr(Card.BlockSize);
end;

procedure TNotepadForm.FormCreate(Sender: TObject);
begin
  FBlockID:=-1;
end;

procedure TNotepadForm.ExitMenuClick(Sender: TObject);
begin
  Close;
end;

procedure TNotepadForm.CloseConnMenuClick(Sender: TObject);
begin
  if Assigned(Card) then
    Card.Free;
  Card:=Nil;
  if Assigned(FBlock) then
    FBlock.Free;
  FBlock:=Nil;
  FBlockID:=-1;
  CloseConnMenu.Enabled:=False;
end;

procedure TNotepadForm.FormDestroy(Sender: TObject);
begin
  if Assigned(Card) then
    Card.Free;
  if Assigned(FBlock) then
    FBlock.Free;
end;

end.

