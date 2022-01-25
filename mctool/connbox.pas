unit connbox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls;

type

  { TConnectForm }

  TConnectForm = class(TForm)
    AuthToken: TEdit;
    CloseBtn: TButton;
    OkayBtn: TButton;
    CardNumber: TEdit;
    HostName: TEdit;
    HostPort: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    procedure OkayBtnClick(Sender: TObject);
  private

  public

  end;

var
  ConnectForm: TConnectForm;

implementation

{$R *.lfm}

{ TConnectForm }

procedure TConnectForm.OkayBtnClick(Sender: TObject);
begin
  if (HostName.Text = '') or (HostPort.Text = '') or (AuthToken.Text = '') or (CardNumber.Text = '') then
  begin
    ShowMessage('All fields are required!');
    Exit;
  end;
  ModalResult:=mrOK;
end;

end.

