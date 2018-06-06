unit tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs;

procedure SplitString(AStringList:TStringList; const Source, Delimiter:String; DelimitNum:Integer=0);
procedure FreeList(ATList:TList);

implementation

procedure SplitString(AStringList: TStringList; const Source,
  Delimiter: String; DelimitNum: Integer);
var
  TempString:String;
  DelimiterIndex, DelimiterCount:Integer;
begin
  if Length(Source)=0 then
  begin
    AStringList.Clear;
    ShowMessage('You can''t specify empty string to be delimited, procedure exited.');
    Exit;
  end;
  TempString:=Source;
  AStringList.Clear;
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

procedure FreeList(ATList: TList);
var
  TListIndex:Integer;
begin
  if ATList=nil then exit;
  for TListIndex:=0 to (ATList.Count-1) do
  begin
    if Assigned(ATList[TListIndex]) then
      Freememory(ATList[TListIndex]);
  end;
  ATList.Clear;
end;

end.

