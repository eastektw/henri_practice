unit practice_1_linklist;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, practice_1_tools, Dialogs;

type
  TLineRecordPtr = ^TLineRecord;
  TLineRecord = record
    Command: String;
    Name: String;
    KeyValuePairs: TStringList;
    ItemsCount: Integer;
  end;

  TNodePtr = ^TNode;
  TNode = record
    LineRecordPtr: TLineRecordPtr;
    NodePtr: TNodePtr
  end;

  TLineRecordLinkedList = record
    Head: TNodePtr;
    Size: Integer;
  end;

procedure ConvertLineIntoLineRecordPtr(var LineRecordPtr: TLineRecordPtr; Line: String);
procedure ReadFileIntoLinkedList(var LinkedList: TLineRecordLinkedList; FileName: String);
procedure AppendLinkedList(var LinkedList: TLineRecordLinkedList; LineRecordPtr: TLineRecordPtr);

implementation

procedure ConvertLineIntoLineRecordPtr(var LineRecordPtr: TLineRecordPtr; Line: String);
var
  TempStringList, KeyValuePair: TStringList;
  ItemsCount, Index: Integer;
  Command: String;
  Name: String;
begin
  ItemsCount:=0;
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
  LineRecordPtr^.KeyValuePairs:=TStringList.Create;

  if TempStringList.Count>1 then
  begin
    for Index:=1 to (TempStringList.Count-1) do
    begin
      KeyValuePair:=TStringList.Create;
      SplitString(KeyValuePair, TempStringList[Index], '=', 1);
      LineRecordPtr^.KeyValuePairs.Add(KeyValuePair[0]);
      if KeyValuePair.Count=2 then LineRecordPtr^.KeyValuePairs.Add(KeyValuePair[1]);
      KeyValuePair.Free;
    end;
    ItemsCount:=ItemsCount+LineRecordPtr^.KeyValuePairs.Count;
  end;


  LineRecordPtr^.Command:=Command;
  LineRecordPtr^.Name:=Name;
  LineRecordPtr^.ItemsCount:=ItemsCount;

  TempStringList.Free;
end;

procedure ReadFileIntoLinkedList(var LinkedList: TLineRecordLinkedList;
  FileName: String);
var
  MyFile: TextFile;
  TempText, PreviousText: String;
  LineFinished: Boolean;
  SlashIndex: integer;
  LineRecordPtr: TLineRecordPtr;
begin
  LineFinished:=True;
  PreviousText:='';

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
      new(LineRecordPtr);
      ConvertLineIntoLineRecordPtr(LineRecordPtr, TempText);
      AppendLinkedList(LinkedList, LineRecordPtr);
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

procedure AppendLinkedList(var LinkedList: TLineRecordLinkedList;
  LineRecordPtr: TLineRecordPtr);
var
  NewNode: TNodePtr;
  TempNode: TNodePtr;
begin
  New(NewNode);
  NewNode^.LineRecordPtr:=LineRecordPtr;
  NewNode^.NodePtr:=Nil;
  if LinkedList.Head=Nil then
  begin
    LinkedList.Head:=NewNode;
    LinkedList.Size:=LinkedList.Size+1;
  end else
  begin
    TempNode:=LinkedList.Head;
    while TempNode^.NodePtr<>Nil do
    begin
      TempNode:=TempNode^.NodePtr;
    end;
    TempNode^.NodePtr:=NewNode;
    LinkedList.Size:=LinkedList.Size+1;
  end;
end;


end.
