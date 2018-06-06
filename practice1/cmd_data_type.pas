unit cmd_data_type;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tools, Dialogs,LCLProc;

type
  TCOMMAND_Param=packed record
    Param_name:String;
    Param_value:String;
  end;

  PCOMMAND_Ptr=^TCOMMAND_;

  { TCOMMAND_ }

  TCOMMAND_=packed record
    COMMAND_type:String;
    COMMAND_name:String;
    COMMAND_Params:array of TCOMMAND_Param;
    Case inLinkedList:Boolean of
      true:
        (Next:PCOMMAND_Ptr);
  end;

  TCOMMANDS_array=array of TCOMMAND_;

  TCommandLineArray=array of String;

  function ReadFile_to_ArrayOfCommandLine(AFile:String):TCommandLineArray;
  procedure ReadFile_by_array(AFile:String; var AFileData:TCOMMANDS_array);
  procedure ReadFile_by_Tlist(Afile:String; AFileData:TList);
  procedure ReadFile_by_LinkedList(AFile:String; AFileData:PCOMMAND_Ptr);
  procedure FreeLinkedList(ALinkedListNode:PCOMMAND_Ptr);
  procedure CommandLineIntoCommandRecord(ACommand_Line:String; var ACommand_Line_Record:TCOMMAND_);

implementation

function ReadFile_to_ArrayOfCommandLine(AFile: String): TCommandLineArray;
var
  Command_File_Object:TextFile;
  Command_Line, Incomplete_Command_Line:String;
  Is_Command_Line_Finished:Boolean;
  Line_Feed_Index:Integer;
  CommandLine_Count:Integer;
  ResultCommandLineArray:TCommandLineArray;
begin
  AssignFile(Command_File_Object, AFile);
  Reset(Command_File_Object);
  SetLength(Incomplete_Command_Line, 0);
  Is_Command_Line_Finished:=True;
  CommandLine_Count:=0;

  while not EOF(Command_File_Object) do
  begin
    ReadLn(Command_File_Object, Command_Line);
    if Length(Command_Line)=0 then continue
    else
      begin
        if not Is_Command_Line_Finished then Command_Line:=Incomplete_Command_Line+Command_Line;

        Line_Feed_Index:=Pos('\', Command_Line);
        if Line_Feed_Index<>0 then
        begin
          Is_Command_Line_Finished:=False;
          Delete(Command_Line, Line_Feed_Index, 1);
          Incomplete_Command_Line:=Command_Line;
          continue
        end else
        begin
          Is_Command_Line_Finished:=True;
          CommandLine_Count:=CommandLine_Count+1;
          SetLength(ResultCommandLineArray, CommandLine_Count);
          ResultCommandLineArray[CommandLine_Count-1]:=Command_Line;
        end;
      end;
  end;
  CloseFile(Command_File_Object);
  Result:=ResultCommandLineArray;
end;

procedure ReadFile_by_array(AFile: String; var AFileData: TCOMMANDS_array);
var
  CommandLineArray:TCommandLineArray;
  CommandLineArray_index:Integer;
  CommandLineArray_Count:Integer;
begin
  CommandLineArray:=ReadFile_to_ArrayOfCommandLine(AFile);
  CommandLineArray_Count:=Length(CommandLineArray);
  SetLength(AFileData, CommandLineArray_Count);
  for CommandLineArray_index:=0 to (Length(CommandLineArray)-1) do
  begin
    AFileData[CommandLineArray_index].inLinkedList:=False;
    CommandLineIntoCommandRecord(CommandLineArray[CommandLineArray_index], AFileData[CommandLineArray_index]);
  end;
  SetLength(CommandLineArray, 0);
end;

procedure ReadFile_by_Tlist(Afile: string; AFileData: TList);
var
  CommandLineArray:TCommandLineArray;
  CommandLineArray_index:Integer;
  CommandLineRecord_Ptr:PCOMMAND_Ptr;
begin
  CommandLineArray:=ReadFile_to_ArrayOfCommandLine(AFile);
  for CommandLineArray_index:=0 to (Length(CommandLineArray)-1) do
  begin
    New(CommandLineRecord_Ptr);
    CommandLineRecord_Ptr^.inLinkedList:=False;
    CommandLineIntoCommandRecord(CommandLineArray[CommandLineArray_index], CommandLineRecord_Ptr^);
    AFileData.Add(CommandLineRecord_Ptr);
  end;
  SetLength(CommandLineArray, 0);
end;

procedure ReadFile_by_LinkedList(AFile: String; AFileData: PCOMMAND_Ptr);
var
  CommandLineArray:TCommandLineArray;
  CommandLineArray_index:Integer;
  CommandLineRecord_Ptr:PCOMMAND_Ptr;
  TempCommandLineRecord_Ptr:PCOMMAND_Ptr;
begin
  CommandLineArray:=ReadFile_to_ArrayOfCommandLine(AFile);
  for CommandLineArray_index:=0 to (Length(CommandLineArray)-1) do
  begin
    if CommandLineArray_index=0 then CommandLineRecord_Ptr:=AFileData;
    CommandLineRecord_Ptr^.inLinkedList:=True;
    CommandLineIntoCommandRecord(CommandLineArray[CommandLineArray_index], CommandLineRecord_Ptr^);

    if CommandLineArray_index<>(Length(CommandLineArray)-1) then
    begin
      New(TempCommandLineRecord_Ptr);
      CommandLineRecord_Ptr^.Next:=TempCommandLineRecord_Ptr;
      CommandLineRecord_Ptr:=TempCommandLineRecord_Ptr;
    end else
      CommandLineRecord_Ptr^.Next:=Nil;
  end;
  SetLength(CommandLineArray, 0);
end;

procedure FreeLinkedList(ALinkedListNode:PCOMMAND_Ptr);
var
  Node_Ptr, TempNode_Ptr:PCOMMAND_Ptr;
begin
  Node_Ptr:=ALinkedListNode;
  while Node_Ptr<>Nil do
  begin
    TempNode_Ptr:=Node_Ptr^.Next;
    Dispose(Node_Ptr);
    Node_Ptr:=TempNode_Ptr;
  end;
end;

procedure CommandLineIntoCommandRecord(ACommand_Line: String;
  var ACommand_Line_Record: TCOMMAND_);
var
  TempStringList, ParamStringList:TStringList;
  Index:Integer;
begin
  TempStringList:=TStringList.Create;
  SplitString(TempStringList, ACommand_Line, ' ', 1);
  ACommand_Line_Record.COMMAND_type:=TempStringList[0];

  SplitString(TempStringList,TempStringList[1], ',');
  ACommand_Line_Record.COMMAND_name:=TempStringList[0];
  if TempStringList.count>1 then
  begin
    SetLength(ACommand_Line_Record.COMMAND_Params, (TempStringList.count-1));
    for Index:=1 to (TempStringList.count-1) do
    begin
      ParamStringList:=TStringList.Create;
      SplitString(ParamStringList, TempStringList[Index], '=', 1);
      ACommand_Line_Record.COMMAND_Params[Index-1].Param_name:=ParamStringList[0];

      if ParamStringList.count=2 then
        ACommand_Line_Record.COMMAND_Params[Index-1].Param_value:=ParamStringList[1]
      else
        ACommand_Line_Record.COMMAND_Params[Index-1].Param_value:='';
      ParamStringList.Free;
    end;
  end
  else
    SetLength(ACommand_Line_Record.COMMAND_Params, 0);
  TempStringList.Free;
end;

end.

