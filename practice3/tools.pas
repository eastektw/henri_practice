unit tools;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Dialogs, Grids, Forms;

procedure SplitString(AStringList:TStringList; const Source, Delimiter:String; DelimitNum:Integer=-1);
procedure RemoveStringFrom(AStringList:TStringList; const AString:String);
procedure DeleteStringGridRowAt(ARow: Integer; AStringGrid: TStringGrid);
procedure EqualizeColWidth(AForm: TForm; AStringGrid: TStringGrid);
procedure ValueCopyStringListTo(ATargetStringList, ASourceStringList: TStrings);
function CompareTwoStringList(AFirstStringList, ASecondStringList: TStrings): Boolean;
function DecimalPlacesOf(ANumberString: String): Integer;

implementation

procedure SplitString(AStringList: TStringList; const Source,
  Delimiter: String; DelimitNum: Integer);
var
  TempString:String;
  DelimiterIndex, DelimiterCount:Integer;
begin
  if Length(Source)=0 then
  begin
    ShowMessage('You can''t specify empty string to be delimited, procedure exited.');
    Exit;
  end;
  AStringList.Clear;
  TempString:=Source;
  DelimiterIndex:=Pos(Delimiter,Source);
  DelimiterCount:=0;

  while DelimiterIndex<>0 do
  begin
    if (DelimiterCount=DelimitNum) then break;
    AStringList.Add(copy(TempString, 0, DelimiterIndex-1));
    Delete(TempString, 1, DelimiterIndex);
    DelimiterCount:=DelimiterCount+1;
    DelimiterIndex:=Pos(Delimiter, TempString);
  end;
  AStringList.Add(TempString);
end;

procedure RemoveStringFrom(AStringList: TStringList; const AString: String);
var
  Index:Integer;
begin
  for Index:=(AStringList.Count-1) downto 0 do
  begin
    if AStringList[Index]=AString then
       AStringList.Delete(Index);
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

procedure EqualizeColWidth(AForm: TForm; AStringGrid: TStringGrid);
var
  EqualWidth: Integer;
begin
  EqualWidth:=AForm.Width div AStringGrid.ColCount;
  AStringGrid.DefaultColWidth:=EqualWidth;
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
    if AFirstStringList[Index]<>ASecondStringList[Index] then
    begin
      Result:=False;
      exit;
    end;
  end;
end;

function DecimalPlacesOf(ANumberString: String): Integer;
var
  TestNumber: Extended;
  NumberStringList: TStringList;
begin
  NumberStringList:=TStringList.Create;
  try
    if not TryStrToFloat(ANumberString, TestNumber) then
      Result:=-1
    else
    begin
      SplitString(NumberStringList, ANumberString, '.', 1);
      if NumberStringList.Count=1 then
        Result:=0
      else
        Result:=Length(NumberStringList[1]);
    end;
  finally
    NumberStringList.Free;
  end;
end;

end.

