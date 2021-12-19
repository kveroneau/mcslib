unit cmdparser;

{$mode objfpc}{$H+}

interface

uses
  StrUtils;

function getToken(var src: string): string;

implementation

function getString(var src: string): string;
begin
  src:=RightStr(src, Length(src)-1);
  Result:=Copy2SymbDel(src, '"');
end;

function getToken(var src: string): string;
begin
  if src[1] = '"' then
    Result:=getString(src)
  else
    Result:=Copy2SpaceDel(src);
end;

end.

