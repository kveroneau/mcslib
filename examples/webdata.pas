unit webdata;

{$mode objfpc}{$H+}

interface

const
  WD_APP = $10;
  WD_TYP = $07;

type
  PWebData = ^TWebData;
  TWebData = packed record
    title: string[40];
    myflag: Boolean;
    visits: UInt32;
  end;

implementation

end.

