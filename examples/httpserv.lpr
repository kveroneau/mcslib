program httpserv;

{$mode objfpc}{$H+}

uses fphttpapp, httpdefs, httproute, netcard, fpTemplate, webcard, webdata,
  ssockets;

var
  tmpl: TFPTemplate;

procedure index(req: TRequest; resp: TResponse);
begin
  tmpl.FileName:='templates/test.html';
  wc.LoadBlock;
  resp.Content:=tmpl.GetContent;
  wc.SaveBlock;
end;

procedure stopServer(req: TRequest; resp: TResponse);
begin
  resp.Content:='OK';
  Application.Terminate;
end;

procedure main;
begin
  wc:=TWebCard.Create;
  tmpl:=TFPTemplate.Create;
  tmpl.OnGetParam:=@wc.TemplateParam;
  HTTPRouter.RegisterRoute('/', @index);
  HTTPRouter.RegisterRoute('/stop', @stopServer);
  Application.Title:='MCS HTTP Example Server';
  Application.Port:=8080;
  Application.Initialize;
  Application.Run;
  tmpl.Free;
  wc.Free;
end;

begin
  try
    main;
  except
    On ESocketError do WriteLn(' * Unable to contact MCS.');
    On EAuthenticationFailed do WriteLn(' * Authentication to MCS Failed.');
    On EInvalidCard do WriteLn(' * Access denied or invalid card.');
  end;
end.

