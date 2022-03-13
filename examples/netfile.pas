unit netfile;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  netcard, ssockets;

type

  { TNetFiler }

  TNetFiler = class(TForm)
    AuthToken: TEdit;
    CancelBtn: TButton;
    OkayBtn: TButton;
    ImageList: TImageList;
    ListBtn: TButton;
    CardNumber: TEdit;
    Label4: TLabel;
    FileList: TListView;
    ServerHost: TEdit;
    ServerPort: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FileListClick(Sender: TObject);
    procedure FileListDblClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ListBtnClick(Sender: TObject);
  private
    FFilterType, FFilterApp: Byte;
    FBlockID: Byte;
    procedure ConnectError(const msg: string);
    function CheckFilter(info: PBlockInfo): Boolean;
    function GetBlockTitle: string;
    function GetBlockSize: integer;
  public
    procedure GetList;
    property FilterType: Byte Read FFilterType Write FFilterType;
    property FilterApp: Byte Read FFilterApp Write FFilterApp;
    property BlockID: Byte Read FBlockID;
    property BlockTitle: string Read GetBlockTitle;
    property BlockSize: integer read GetBlockSize;
  end;

var
  Card: TNetCard;

implementation

{$R *.lfm}

{ TNetFiler }

procedure TNetFiler.ListBtnClick(Sender: TObject);
begin
  if (ServerHost.Text = '') or (ServerPort.Text = '') or (AuthToken.Text = '') or (CardNumber.Text = '') then
  begin
    ShowMessage('Please fill in all fields!');
    Exit;
  end;
  ListBtn.Enabled:=False;
  OkayBtn.Enabled:=True;
  try
    Card:=TNetCard.Create(ServerHost.Text, StrToInt(ServerPort.Text));
    Card.Authenticate(AuthToken.Text);
    Card.SelectCard(StrToInt(CardNumber.Text));
    GetList;
  except
    on ESocketError do ConnectError('Failed to connect to server!');
    on EAuthenticationFailed do ConnectError('Invalid Auth Token!');
    on EInvalidCard do ConnectError('Invalid card selected or no access.');
  end;
end;

procedure TNetFiler.FileListClick(Sender: TObject);
begin
  FBlockID:=FileList.ItemIndex+1;
end;

procedure TNetFiler.FileListDblClick(Sender: TObject);
begin
  FBlockID:=FileList.ItemIndex+1;
  OkayBtn.Click;
end;

procedure TNetFiler.FormCreate(Sender: TObject);
begin
  FFilterApp:=0;
  FFilterType:=0;
  FBlockID:=0;
end;

procedure TNetFiler.ConnectError(const msg: string);
begin
  ShowMessage(msg);
  if Assigned(Card) then
    Card.Free;
  Card:=Nil;
  ListBtn.Enabled:=True;
  OkayBtn.Enabled:=False;
end;

function TNetFiler.CheckFilter(info: PBlockInfo): Boolean;
begin
  Result:=True;
  if (FFilterType = 0) and (FFilterApp = 0) then
    Exit;
  Result:=False;
  if (FFilterType <> 0) and (FFilterType = info^.typno) then
    Result:=True;
  if (FFilterApp <> 0) and (FFilterApp = info^.appno) then
    Result:=True;
end;

function TNetFiler.GetBlockTitle: string;
begin
  Result:=FileList.Items.Item[FBlockID-1].Caption;
end;

function TNetFiler.GetBlockSize: integer;
begin
  Result:=StrToInt(FileList.Items.Item[FBlockID-1].SubItems.Strings[2]);
end;

procedure TNetFiler.GetList;
var
  lst: TStringList;
  i: integer;
  itm: TListItem;
  info: TBlockInfo;
begin
  FileList.Items.Clear;
  lst:=Card.BlockList;
  for i:=0 to 14 do
  begin
    Card.GetInfo(i+1, @info);
    if CheckFilter(@info) then
    begin
      itm:=FileList.Items.Add;
      itm.Caption:=lst.Strings[i];
      if info.typno = 8 then
        itm.ImageIndex:=0;
      itm.SubItems.Add(IntToStr(info.appno));
      itm.SubItems.Add(IntToStr(info.typno));
      itm.SubItems.Add(IntToStr(info.total));
    end;
  end;
  lst.Free;
end;

initialization
  Card:=Nil;
end.

