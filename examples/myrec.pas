unit myrec;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

const
  MYREC_TYP = 29;

type
  TGender = (Male, Female, Other);
  PPerson = ^TPerson;
  TPerson = record
    name: string[30];
    age: byte;
    gender: byte;
  end;
  TPersonArray = Array of TPerson;
  PPersonArray = ^TPersonArray;

implementation

end.

