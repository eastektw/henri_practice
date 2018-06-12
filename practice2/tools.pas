unit tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

procedure SplitString(AStringList:TStringList; const Source, Delimiter:String; DelimitNum:Integer=0);

implementation

procedure SplitString(AStringList: TStringList; const Source,
  Delimiter: String; DelimitNum: Integer);
var
  TempString:String;
  DelimiterIndex, DelimiterCount:Integer;
begin
  AStringList.Clear;
  if Length(Source)=0 then
  begin
    ShowMessage('You can''t specify empty string to be delimited, procedure exited.');
    Exit;
  end;
  TempString:=Source;
  DelimiterIndex:=Pos(Delimiter,Source);
  if DelimiterIndex=0 then
  begin
    AStringList.Add(Source)
  end else
  begin
    DelimiterCount:=0;
    while DelimiterIndex<>0 do
    begin
      AStringList.Add(copy(TempString, 0, DelimiterIndex-1));
      Delete(TempString, 1, DelimiterIndex);
      DelimiterCount:=DelimiterCount+1;
      DelimiterIndex:=Pos(Delimiter, TempString);
      if (DelimitNum<>0) and (DelimiterCount=DelimitNum) then break;
    end;
    AStringList.Add(TempString);
  end;
end;

end.

