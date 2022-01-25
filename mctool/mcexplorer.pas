unit mcexplorer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, khexeditor, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls, netcard, ssockets, connbox;

type

  { TExplorerForm }

  TExplorerForm = class(TForm)
    MemoPad: TMemo;
    ImportDialog: TOpenDialog;
    ExportDialog: TSaveDialog;
    SubBtn: TButton;
    Timer: TTimer;
    WordMode: TCheckBox;
    KHexEditor: TKHexEditor;
    SaveBtn: TButton;
    FTitle: TEdit;
    FAppID: TEdit;
    FTypID: TEdit;
    FSize: TEdit;
    IconList: TImageList;
    BlockList: TListView;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    TabBar: TPageControl;
    PropsTab: TTabSheet;
    HexTab: TTabSheet;
    NativeTab: TTabSheet;
    ToolBar: TToolBar;
    ConnBtn: TToolButton;
    ToolButton1: TToolButton;
    NewDocBtn: TToolButton;
    ImportBtn: TToolButton;
    ExportBtn: TToolButton;
    procedure BlockListClick(Sender: TObject);
    procedure ConnBtnClick(Sender: TObject);
    procedure ExportBtnClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ImportBtnClick(Sender: TObject);
    procedure KHexEditorExit(Sender: TObject);
    procedure MemoPadEditingDone(Sender: TObject);
    procedure NewDocBtnClick(Sender: TObject);
    procedure SaveBtnClick(Sender: TObject);
    procedure SubBtnClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure WordModeClick(Sender: TObject);
  private
    FCard: TNetCard;
    FInfo: PBlockInfo;
    FBlock: TMemoryStream;
    FBlockID: integer;
    procedure ConnectError(const msg: string);
    procedure GetList;
    function GetIcon(const typ: integer): integer;
    procedure EnableWidgets(value: Boolean);
    procedure EnableToolbar(value: Boolean);
    procedure PopulateProps;
    procedure PopulateHex;
    procedure ProcessSync(card, blkid: integer);
  public

  end;

var
  ExplorerForm: TExplorerForm;

implementation

{$R *.lfm}

{ TExplorerForm }

procedure TExplorerForm.FormResize(Sender: TObject);
begin
  BlockList.Height:=ClientHeight-ToolBar.Height;
  TabBar.Width:=ClientWidth-BlockList.Width;
  TabBar.Height:=ClientHeight-ToolBar.Height;
  HexTab.Width:=TabBar.ClientWidth;
  HexTab.Height:=TabBar.ClientHeight;
  KHexEditor.Width:=HexTab.ClientWidth;
  KHexEditor.Height:=HexTab.ClientHeight-WordMode.Height-3;
  WordMode.Top:=HexTab.ClientHeight-WordMode.Height-3;
end;

procedure TExplorerForm.ImportBtnClick(Sender: TObject);
var
  ObjFile: TMemoryStream;
  done: Boolean;
begin
  if not ImportDialog.Execute then
    Exit;
  FBlock.Clear;
  ObjFile:=TMemoryStream.Create;
  ObjFile.LoadFromFile(ImportDialog.FileName);
  ObjFile.Read(FInfo^, SizeOf(FInfo^));
  FBlock.SetSize(ObjFile.Size-SizeOf(FInfo^));
  ObjFile.Read(FBlock.Memory^, FBlock.Size);
  ObjFile.Free;
  if FBlock.Size > FCard.BlockSize then
    ShowMessage('Data truncated to '+IntToStr(FCard.BlockSize));
  FBlock.SetSize(FCard.BlockSize);
  if FBlockID = -1 then
    FBlockID:=FCard.FindFree;
  Timer.Enabled:=False;
  FCard.WriteBlock(FBlockID, FBlock, FInfo);
  FCard.CheckEvents(Sender, done);
  GetList;
  PopulateProps;
  PopulateHex;
  Timer.Enabled:=True;
end;

procedure TExplorerForm.KHexEditorExit(Sender: TObject);
begin
  FBlock.Position:=0;
  KHexEditor.SaveToStream(FBlock);
  FBlock.Position:=0;
  if FInfo^.typno = 8 then
  begin
    MemoPad.Text:=FBlock.ReadAnsiString;
    NativeTab.Enabled:=True;
  end
  else
    NativeTab.Enabled:=False;
end;

procedure TExplorerForm.MemoPadEditingDone(Sender: TObject);
begin
  FBlock.Position:=0;
  FBlock.WriteAnsiString(MemoPad.Text);
  FSize.Text:=IntToStr(Length(MemoPad.Text));
  FBlock.Position:=0;
  KHexEditor.LoadFromStream(FBlock);
end;

procedure TExplorerForm.NewDocBtnClick(Sender: TObject);
begin
  FBlockID:=FCard.FindFree;
  FTitle.Text:='';
  FAppID.Text:='';
  FTypID.Text:='';
  FSize.Text:='';
  KHexEditor.Clear;
  NativeTab.Enabled:=False;
end;

procedure TExplorerForm.SaveBtnClick(Sender: TObject);
begin
  FInfo^.title:=FTitle.Text;
  FInfo^.appno:=StrToInt(FAppID.Text);
  FInfo^.typno:=StrToInt(FTypID.Text);
  FInfo^.total:=StrToInt(FSize.Text);
  FBlock.Position:=0;
  KHexEditor.SaveToStream(FBlock);
  FCard.WriteBlock(FBlockID, FBlock, FInfo);
end;

procedure TExplorerForm.SubBtnClick(Sender: TObject);
begin
  FCard.Subscribe(FBlockID);
end;

procedure TExplorerForm.TimerTimer(Sender: TObject);
var
  done: boolean;
begin
  if Assigned(FCard) then
    FCard.CheckEvents(Sender, done);
end;

procedure TExplorerForm.ConnBtnClick(Sender: TObject);
begin
  if ConnectForm.ShowModal <> mrOK then
    Exit;
  if Assigned(FCard) then
    FCard.Free;
  try
    FCard:=TNetCard.Create(ConnectForm.HostName.Text, StrToInt(ConnectForm.HostPort.Text));
    FCard.Authenticate(ConnectForm.AuthToken.Text);
    ConnectForm.AuthToken.Text:='';
    FCard.SelectCard(StrToInt(ConnectForm.CardNumber.Text));
    GetList;
    FCard.OnSync:=@ProcessSync;
    EnableToolbar(True);
  except
    on ESocketError do ConnectError('Failed to connect to server!');
    on EAuthenticationFailed do ConnectError('Invalid Auth Token!');
    on EInvalidCard do ConnectError('Invalid card selected or no access!');
  end;
end;

procedure TExplorerForm.ExportBtnClick(Sender: TObject);
var
  ObjFile: TMemoryStream;
begin
  if not ExportDialog.Execute then
    Exit;
  ObjFile:=TMemoryStream.Create;
  ObjFile.Write(FInfo^, SizeOf(FInfo^));
  FBlock.SaveToStream(ObjFile);
  ObjFile.SaveToFile(ExportDialog.FileName);
  ObjFile.Free;
end;

procedure TExplorerForm.BlockListClick(Sender: TObject);
begin
  if not Assigned(FCard) then
    Exit;
  FBlockID:=BlockList.ItemIndex+1;
  PopulateProps;
  PopulateHex;
  EnableWidgets(True);
end;

procedure TExplorerForm.FormCreate(Sender: TObject);
begin
  FCard:=Nil;
  FInfo:=Nil;
  FBlock:=Nil;
  FBlockID:=-1;
end;

procedure TExplorerForm.FormDestroy(Sender: TObject);
begin
  if Assigned(FCard) then
    FCard.Free;
  if Assigned(FInfo) then
    Dispose(FInfo);
  if Assigned(FBlock) then
    FBlock.Free;
end;

procedure TExplorerForm.WordModeClick(Sender: TObject);
begin
  if WordMode.Checked then
    KHexEditor.DigitGrouping:=2
  else
    KHexEditor.DigitGrouping:=1;
end;

procedure TExplorerForm.ConnectError(const msg: string);
begin
  ShowMessage(msg);
  if Assigned(FCard) then
    FCard.Free;
  FCard:=Nil;
  BlockList.Clear;
  EnableToolbar(False);
  EnableWidgets(False);
end;

procedure TExplorerForm.GetList;
var
  lst: TStringList;
  i: integer;
  itm: TListItem;
  info: TBlockInfo;
begin
  BlockList.Clear;
  lst:=FCard.BlockList;
  try
    for i:=0 to 14 do
    begin
      FCard.GetInfo(i+1, @info);
      itm:=BlockList.Items.Add;
      itm.Caption:=lst.Strings[i];
      itm.ImageIndex:=GetIcon(info.typno);
      itm.SubItems.Add(IntToStr(info.appno));
      itm.SubItems.Add(IntToStr(info.typno));
      itm.SubItems.Add(IntToStr(info.total));
    end;
  finally
    lst.Free;
  end;
end;

function TExplorerForm.GetIcon(const typ: integer): integer;
begin
  Result:=6;
  case typ of
    8: Result:=7;
  end;
end;

procedure TExplorerForm.EnableWidgets(value: Boolean);
begin
  FTitle.Enabled:=value;
  FAppID.Enabled:=value;
  FTypID.Enabled:=value;
  FSize.Enabled:=value;
  SaveBtn.Enabled:=value;
  SubBtn.Enabled:=value;
  KHexEditor.Enabled:=value;
end;

procedure TExplorerForm.EnableToolbar(value: Boolean);
begin
  NewDocBtn.Enabled:=value;
  ImportBtn.Enabled:=value;
  ExportBtn.Enabled:=value;
end;

procedure TExplorerForm.PopulateProps;
begin
  if not Assigned(FInfo) then
    New(FInfo);
  FCard.GetInfo(FBlockID, FInfo);
  FTitle.Caption:=FInfo^.title;
  FAppID.Caption:=IntToStr(FInfo^.appno);
  FTypID.Caption:=IntToStr(FInfo^.typno);
  FSize.Caption:=IntToStr(FInfo^.total);
end;

procedure TExplorerForm.PopulateHex;
var
  tmp: longint;
begin
  if Assigned(FBlock) then
    FBlock.Free;
  FBlock:=FCard.ReadBlock(FBlockID);
  tmp:=KHexEditor.AddressOffset;
  KHexEditor.LoadFromStream(FBlock);
  KHexEditor.AddressOffset:=tmp;
  if FInfo^.typno = 8 then
  begin
    FBlock.Position:=0;
    try
      MemoPad.Text:=FBlock.ReadAnsiString;
      NativeTab.Enabled:=True;
    except
      on EReadError do ShowMessage('Could not parse native format.');
    end;
  end;
end;

procedure TExplorerForm.ProcessSync(card, blkid: integer);
begin
  if blkid = FBlockID then
  begin
    PopulateProps;
    PopulateHex;
  end;
  { TODO : Enable the switching of tabs on sync from server. }
end;

end.

