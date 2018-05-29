unit practice_1_array;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, practice_1_tools, Dialogs;
type
  FileArray = array of array of string;
  TFileArrayRecord = Record
    FileArray : FileArray;
    FileLineCount : Integer;
    MaxLineItemsCount: Integer;
  end;

  procedure ConvertLineIntoArrayOfArray(var DoubleArray: FileArray; const Line: String; var MaxLineItemsCount: Integer; InsertIndex: Integer);
  procedure ReadFileIntoArray(var FileRecord: TFileArrayRecord; FileName: String);
implementation

procedure ConvertLineIntoArrayOfArray(var DoubleArray: FileArray; const Line: String; var MaxLineItemsCount: Integer; InsertIndex: Integer);
var
  TempStringList, KeyValueStringList : TStringList;
  LineItemsCount, Index: Integer;
begin
  LineItemsCount:=0;
  TempStringList:=TStringList.Create;
  SplitString(TempStringList, Line, ' ', 1);
  LineItemsCount:=LineItemsCount+1;
  SetLength(DoubleArray[InsertIndex], LineItemsCount);
  DoubleArray[InsertIndex][LineItemsCount-1]:=TempStringList[0];

  SplitString(TempStringList, TempStringList[1], ',');
  LineItemsCount:=LineItemsCount+1;
  SetLength(DoubleArray[InsertIndex], LineItemsCount);
  DoubleArray[InsertIndex][LineItemsCount-1]:=TempStringList[0];

  if TempStringList.Count>1 then
  begin
    for Index:=1 to (TempStringList.Count - 1) do
    begin
      KeyValueStringList:=TStringList.Create;
      SplitString(KeyValueStringList, TempStringList.Strings[Index], '=', 1);
      LineItemsCount:=LineItemsCount+1;
      SetLength(DoubleArray[InsertIndex], LineItemsCount);
      DoubleArray[InsertIndex][LineItemsCount-1]:=KeyValueStringList[0];

      if KeyValueStringList.Count=2 then
      begin
        LineItemsCount:=LineItemsCount+1;
        SetLength(DoubleArray[InsertIndex], LineItemsCount);
        DoubleArray[InsertIndex][LineItemsCount-1]:=KeyValueStringList[1];
      end;
    end;
    KeyValueStringList.Free;
  end;

  if LineItemsCount>MaxLineItemsCount then MaxLineItemsCount:=LineItemsCount;
  TempStringList.Free;
end;


procedure ReadFileIntoArray(var FileRecord: TFileArrayRecord; FileName: String);
var
  MyFile: TextFile;
  TempText, PreviousText: String;
  DoubleArray : Array of Array of String;
  SlashIndex, LineCount, MaxLineItemsCount: Integer;
  LineFinished: Boolean;
begin
  try
    AssignFile(MyFile, FileName);
    Reset(MyFile);
  except
    ShowMessage('Unknown error encountered while reading the file');
  end;

  LineCount:=0;
  MaxLineItemsCount:=0;
  LineFinished:=True;

  while not EOF(myFile) do
  begin
    ReadLn(myFile, TempText);
    if Length(TempText)=0 then continue;

    if not LineFinished then TempText:=PreviousText+TempText;

    SlashIndex:=pos('\', TempText);
    if SlashIndex <> 0 then
    begin
      LineFinished:=False;
      Delete(TempText, SlashIndex, 1);
      PreviousText:=TempText;
      continue;
    end
    else
    begin
      LineFinished:=True;
      LineCount:=LineCount+1;
      SetLength(DoubleArray, LineCount);
      ConvertLineIntoArrayOfArray(DoubleArray, TempText, MaxLineItemsCount, LineCount-1);
    end;
  end;

  FileRecord.FileArray:=DoubleArray;
  FileRecord.FileLineCount:=Length(DoubleArray);
  FileRecord.MaxLineItemsCount:=MaxLineItemsCount;

  CloseFile(MyFile);
end;

end.

