unit practice_1_linklist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, practice_1_tools, Dialogs;

type
  KeyValueRecord = record
    Key: String;
    Value: String;
  end;

  TLineRecord = record
    Command: String;
    Name: String;
    KeyValuePairs: Array of KeyValueRecord;
    ItemsCount: Integer;
  end;

  TNodePtr = ^TNode;
  TNode = record
    LineRecord: TLineRecord;
    NodePrt: TNodePtr
  end;

  TLineRecordLinkedList = record
    Head: TNodePtr;
    Size: Integer;
  end;

procedure InitLineRecordLinkedList(var LinkedList: TLineRecordLinkedList);
procedure AddAfterNode(var Node: TNode; LineRecord: TLineRecord);
procedure AppendLinkedList(var LinkedList: TLineRecordLinkedList; LineRecord: TLineRecord);
procedure ConvertLineIntoLineRecord(var LineRecord: TLineRecord; Line: String);
procedure ReadFileIntoLinkedList(var LinkedList: TLineRecordLinkedList; FileName: String);

implementation

procedure InitLineRecordLinkedList(var LinkedList: TLineRecordLinkedList);
begin
  LinkedList.Head:=nil;
  LinkedList.Size:=0;
end;

procedure AddAfterNode(var Node: TNode; LineRecord: TLineRecord);
var
  TempNodePtr: TNodePtr;
  TempNode: TNode;
begin
  TempNodePtr:=Node.NodePrt;
  TempNode.LineRecord:=LineRecord;
  TempNode.NodePrt:=TempNodePtr;
  Node.NodePrt^:=TempNode;
end;

procedure AppendLinkedList(var LinkedList: TLineRecordLinkedList; LineRecord: TLineRecord);
var
  NewNode: TNode;
  NewNodePtr: TNodePtr;
  TempNode: TNode;
begin
  NewNode.LineRecord:=LineRecord;
  NewNode.NodePrt:=Nil;
  TempNode:=LinkedList.Head^;
  if LinkedList.Head=Nil then LinkedList.Head^:=TempNode
  else
  begin
    while TempNode.NodePrt<>Nil do
    begin
      TempNode:=TempNode.NodePrt^;
    end;
    TempNode.NodePrt^:=NewNode;
  end;
  LinkedList.Size:=LinkedList.Size+1;
end;

procedure ConvertLineIntoLineRecord(var LineRecord: TLineRecord; Line: String);
var
  TempStringList, KeyValueStringList: TStringList;
  ItemsCount, KeyValuePairsCount, Index: Integer;
  Command: String;
  Name: String;
  KeyValuePair: KeyValueRecord;
  KeyValueArray: Array of KeyValueRecord;
begin
  ItemsCount:=0;
  KeyValuePairsCount:=0;
  TempStringList:=TStringList.Create;

  SplitString(TempStringList, Line, ' ', 1);
  if TempStringList.count=2 then
  begin
    ItemsCount:=ItemsCount+1;
    Command:=TempStringList[0];
  end
  else
  begin
    ShowMessage('The Line format is incorrect, please check the file: ' + Line);
    exit;
  end;

  SplitString(TempStringList, TempStringList[1], ',');
  Name:=TempStringList[0];
  ItemsCount:=ItemsCount+1;
  KeyValuePairsCount:=TempStringList.Count-1;
  SetLength(KeyValueArray, KeyValuePairsCount);
  if TempStringList.Count>1 then
  begin
    for Index:=1 to TempStringList.Count-1 do
    begin
      KeyValueStringList:=TStringList.Create;
      SplitString(KeyValueStringList, TempStringList[Index], '=', 1);
      if KeyValueStringList.Count=2 then
      begin
        KeyValuePair.Key:=KeyValueStringList[0];
        KeyValuePair.Value:=KeyValueStringList[1];
        ItemsCount:=ItemsCount+2;
      end else
      begin
        KeyValuePair.Key:=KeyValueStringList[0];
        KeyValuePair.Value:='no =';
        ItemsCount:=ItemsCount+1;
      end;
      KeyValueArray[Index-1]:=KeyValuePair;
      KeyValueStringList.Free;
    end;
  end;

  LineRecord.Command:=Command;
  LineRecord.Name:=Name;
  LineRecord.KeyValuePairs:=KeyValueArray;
  LineRecord.ItemsCount:=ItemsCount;

  TempStringList.Free;
  KeyValueArray:=Nil;
end;

procedure ReadFileIntoLinkedList(var LinkedList: TLineRecordLinkedList;
  FileName: String);
var
  MyFile: TextFile;
  TempText, PreviousText: String;
  LineFinished: Boolean;
  SlashIndex: integer;
  LineRecord: TLineRecord;
begin
  InitLineRecordLinkedList(LinkedList);

  try
    AssignFile(MyFile, FileName);
    Reset(MyFile);
  except
    ShowMessage('Unknown error encountered while reading the file');
  end;

  while not EOF(MyFile) do
  begin
    ReadLn(MyFile, TempText);
    if Length(TempText)=0 then continue;

    if not LineFinished then TempText:=PreviousText+TempText;

    SlashIndex:=Pos('\', TempText);
    if SlashIndex=0 then
    begin
      LineFinished:=True;
      ConvertLineIntoLineRecord(LineRecord, TempText);
      AppendLinkedList(LinkedList, LineRecord);
    end
    else
    begin
      LineFinished:=False;
      Delete(TempText, SlashIndex, 1);
      PreviousText:=TempText;
      Continue;
    end;
  end;
end;


end.

