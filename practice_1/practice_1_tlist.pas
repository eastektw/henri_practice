unit practice_1_TList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, practice_1_tools, Dialogs;

type
  KeyValuePairRecord=Record
    Key: String;
    Value: String;
  end;
  PLineRecord=^TLineRecord;
  TLineRecord=Record
    Prefix: String;
    Name: String;
    KeyValuePairs: Array of KeyValuePairRecord;
    ItemCount: Integer;
  end;

procedure ConvertLineIntoLineRecord(Line: String; LineRecordPointer: PLineRecord);
procedure ReadFileIntoTList(InTList: TList; FileName: String);

implementation

procedure ConvertLineIntoLineRecord(Line: String; LineRecordPointer: PLineRecord
  );
var
  TempKeyValueRecord: KeyValuePairRecord;
  TempKeyValueRecordArray: Array of KeyValuePairRecord;
  LinePrefix, LineName: String;
  TempStringList, KeyValueStringList: TStringList;
  Index, KeyValueCount, LineItemsCount: Integer;
begin
  TempStringList:=TStringList.Create;
  LineItemsCount:=0;
  SplitString(TempStringList, Line, ' ', 1);
  LinePrefix:=TempStringList[0];
  LineItemsCount:=LineItemsCount+1;

  SplitString(TempStringList, TempStringList[1], ',');
  LineName := TempStringList[0];
  LineItemsCount:=LineItemsCount+1;

  TempKeyValueRecordArray:=Nil;
  KeyValueCount:=TempStringList.Count-1;
  SetLength(TempKeyValueRecordArray, KeyValueCount);
  for Index := 1 to (TempStringList.Count-1) do
  begin
    KeyValueStringList:=TStringList.Create;
    SplitString(KeyValueStringList, TempStringList[Index], '=', 1);
    if KeyValueStringList.Count = 2 then
    begin
      TempKeyValueRecord.Key:=KeyValueStringList[0];
      TempKeyValueRecord.Value:=KeyValueStringList[1];
    end
    else
    begin
      TempKeyValueRecord.Key:=KeyValueStringList[0];
      TempKeyValueRecord.Value:='';
    end;

    LineItemsCount:=LineItemsCount+2;
    TempKeyValueRecordArray[Index - 1]:=TempKeyValueRecord;

    KeyValueStringList.Clear;
  end;

  LineRecordPointer^.Prefix:=LinePrefix;
  LineRecordPointer^.Name:=LineName;
  LineRecordPointer^.KeyValuePairs:=TempKeyValueRecordArray;
  LineRecordPointer^.ItemCount := LineItemsCount;

  TempKeyValueRecordArray:=Nil;
  TempStringList.Free;
end;

procedure ReadFileIntoTList(InTList: TList; FileName: String);
var
  MyFile: TextFile;
  LineRecordPt: PLineRecord;
  TempText, PreviousText: String;
  TempInt: Integer;
  LineFinished: Boolean;
begin
  try
    AssignFile(MyFile, FileName);
    Reset(MyFile);
  except
    ShowMessage('Unknown error encountered while reading the file');
  end;

  LineFinished:=True;

  while not EOF(MyFile) do
  begin
    ReadLn(MyFile, TempText);
    if Length(TempText)=0 then continue;

    if not LineFinished then TempText:=PreviousText+TempText;

    TempInt:=pos('\', TempText);
    if TempInt <> 0 then
    begin
      LineFinished:=False;
      Delete(TempText, TempInt, TempInt);
      PreviousText:=TempText;
      continue;
    end
    else
    begin
      LineFinished:=True;
      new(LineRecordPt);
      ConvertLineIntoLineRecord(TempText, LineRecordPt);
      InTList.Add(LineRecordPt);
    end;
  end;
CloseFile(MyFile);
end;

end.

