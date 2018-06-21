unit tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Grids, contnrs;

procedure SplitString(AStringList:TStringList; const Source, Delimiter:String; DelimitNum:Integer=0);
procedure ValueCopyStringListTo(ATargetStringList, ASourceStringList: TStrings);
procedure DeleteStringGridRowAt(ARow: Integer; AStringGrid: TStringGrid);
procedure CleanStringGrid(AStringGrid: TStringGrid);
function CompareTwoStringList(AFirstStringList, ASecondStringList: TStrings): Boolean;

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

procedure ValueCopyStringListTo(ATargetStringList, ASourceStringList: TStrings);
var
  Index: Integer;
begin
  ATargetStringList.Clear;
  for Index:=0 to (ASourceStringList.Count-1) do
  begin
    ATargetStringList.Add(ASourceStringList[Index]);
  end;
end;

procedure DeleteStringGridRowAt(ARow: Integer; AStringGrid: TStringGrid);
var
  Index: Integer;
begin
  for Index:=ARow to (AStringGrid.RowCount-2) do
    AStringGrid.Rows[Index].Assign(AStringGrid.Rows[Index+1]);
  AStringGrid.RowCount:=AStringGrid.RowCount-1;
end;

procedure CleanStringGrid(AStringGrid: TStringGrid);
var
  ColIndex: Integer;
begin
  for ColIndex:=0 to (AStringGrid.ColCount-1) do
    AStringGrid.Cols[ColIndex].Clear;
  AStringGrid.RowCount:=0;
end;

function CompareTwoStringList(AFirstStringList, ASecondStringList: TStrings
  ): Boolean;
var
  Index: Integer;
begin
  Result:=True;

  if AFirstStringList.Count<>ASecondStringList.Count then
  begin
    Result:=False;
  end
  else
  begin
    for Index:=0 to AFirstStringList.Count-1 do
    if AFirstStringList[Index]<>ASecondStringList[Index] then Result:=False
  end;
end;

end.

