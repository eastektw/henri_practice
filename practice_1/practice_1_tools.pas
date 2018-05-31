unit practice_1_tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure SplitString(StringList: TStringList; const Source, Delimiter: String ; DelimitNum: integer = 0 );

implementation

procedure SplitString(StringList: TStringList; const Source, Delimiter: String; DelimitNum: integer = 0);
var
  Temp: String;
  Index, DelimitCount: Integer;
begin
  if Source='' then
  begin
    StringList.Clear;
    Exit;
  end;

  Temp:=Source;
  StringList.Clear;
  Index:=pos(Delimiter, Source);
  if Index=0 then
  begin
    StringList.Add(Source);
    Exit;
  end
  else
  begin
    DelimitCount:=0;
    while Index<>0 do
    begin
      StringList.Add(copy(Temp, 0, Index-1));
      Delete(Temp, 1, Index);
      Index:=pos(Delimiter, Temp);
      DelimitCount:=DelimitCount + 1;
      if DelimitNum<>0 then
        if DelimitCount=DelimitNum then break;
    end;
    StringList.Add(temp);
  end;
end;

end.

