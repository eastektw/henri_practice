unit cmd_data_type;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, tools, Dialogs;

type
  TCOMMAND_Param=packed record
    Param_name:String;
    Param_value:String;
  end;

  { TCOMMAND_ }
  PCOMMAND_Ptr=^TCOMMAND_;
  TCOMMAND_=packed record
    COMMAND_type:String;
    COMMAND_name:String;
    COMMAND_Params:array of TCOMMAND_Param;
    Next:PCOMMAND_Ptr;
  end;

  TCOMMANDS_array=array of TCOMMAND_;

  { TCOMMAND_list }

  TCOMMANDS_list=class(Tlist)
  private
    function GetItems(Index: Integer): PCOMMAND_Ptr;
    procedure SetItems(Index: Integer; AValue: PCOMMAND_Ptr);
  public
    property Items[Index: Integer]: PCOMMAND_Ptr read GetItems write SetItems;
    procedure ReadFile(AFile:string);
  end;

  function ReadFile_to_CompleteCommandLine(var AFileHandle:TextFile):String;
  procedure ReadFile_by_array(AFile:String; var AFileData:TCOMMANDS_array);
  procedure ReadFile_by_Tlist(Afile:String; AFileData:TCOMMANDS_list);
  procedure ReadFile_by_LinkedList(AFile:String; const AFileData:PCOMMAND_Ptr);
  procedure WriteFile_by_LinkedList(AFile:String; AFileData:PCOMMAND_Ptr);
  procedure FreeLinkedList(ALinkedListNode:PCOMMAND_Ptr);
  procedure CommandLineIntoCommandRecord(ACommand_Line:String; var ACommand_Line_Record:TCOMMAND_);

implementation

function ReadFile_to_CompleteCommandLine(var AFileHandle: TextFile): String;
var
  Command_Line, Incomplete_Command_Line:String;
  Is_Command_Line_Finished:Boolean;
  Line_Feed_Index:Integer;
begin
  Command_Line:='';
  Incomplete_Command_Line:='';
  Result:='';
  Is_Command_Line_Finished:=False;
  while not Is_Command_Line_Finished do
  begin
    ReadLn(AFileHandle, Command_Line);
    if Length(Command_Line)<>0 then
    begin
      if not Is_Command_Line_Finished then Command_Line:=Incomplete_Command_Line+Command_Line;

      Line_Feed_Index:=Pos('\', Command_Line);
      if Line_Feed_Index<>0 then
      begin
        Is_Command_Line_Finished:=False;
        Delete(Command_Line, Line_Feed_Index, 1);
        Incomplete_Command_Line:=Command_Line;
      end
      else
        Is_Command_Line_Finished:=True;
    end;
  end;
  Result:=Command_Line;
end;

procedure ReadFile_by_array(AFile: String; var AFileData: TCOMMANDS_array);
var
  Command_File_Object:TextFile;
  CommandLine:String;
  CommandLine_Count:Integer;
begin
  try
    AssignFile(Command_File_Object, AFile);

    try
      Reset(Command_File_Object);
      CommandLine:='';
      CommandLine_Count:=0;
      while not EOF(Command_File_Object) do
      begin
        CommandLine:=ReadFile_to_CompleteCommandLine(Command_File_Object);
        CommandLine_Count:=CommandLine_Count+1;
        SetLength(AFileData, CommandLine_Count);
        CommandLineIntoCommandRecord(CommandLine, AFileData[CommandLine_Count-1]);
      end;
    finally
      CloseFile(Command_File_Object);
    end;

  except
    ShowMessage('Cannot open the file.');
  end;

end;

procedure ReadFile_by_Tlist(Afile: string; AFileData: TCOMMANDS_list);
var
  Command_File_Object:TextFile;
  CommandLine:String;
  CommandLineRecord_Ptr:PCOMMAND_Ptr;
begin
  try
    AssignFile(Command_File_Object, AFile);

    try
      Reset(Command_File_Object);
      CommandLine:='';
      while not EOF(Command_File_Object) do
      begin
        CommandLine:=ReadFile_to_CompleteCommandLine(Command_File_Object);
        New(CommandLineRecord_Ptr);
        CommandLineRecord_Ptr^.Next:=Nil;
        CommandLineIntoCommandRecord(CommandLine, CommandLineRecord_Ptr^);
        AFileData.Add(CommandLineRecord_Ptr);
      end;
    finally
      CloseFile(Command_File_Object);
    end;

  except
    ShowMessage('Cannot open the file.');
  end;

end;

procedure ReadFile_by_LinkedList(AFile: String; const AFileData: PCOMMAND_Ptr);
var
  Command_File_Object:TextFile;
  CommandLine:String;
  CommandLineRecord_Ptr:PCOMMAND_Ptr;
  TempCommandLineRecord_Ptr:PCOMMAND_Ptr;
begin
  try
    AssignFile(Command_File_Object, AFile);

    try
      Reset(Command_File_Object);
      CommandLine:='';
      CommandLineRecord_Ptr:=AFileData;
      while not EOF(Command_File_Object) do
      begin
        CommandLine:=ReadFile_to_CompleteCommandLine(Command_File_Object);
        CommandLineIntoCommandRecord(CommandLine, CommandLineRecord_Ptr^);
        if not EOF(Command_File_Object) then
        begin
          New(TempCommandLineRecord_Ptr);
          CommandLineRecord_Ptr^.Next:=TempCommandLineRecord_Ptr;
          CommandLineRecord_Ptr:=TempCommandLineRecord_Ptr;
        end
        else
          CommandLineRecord_Ptr^.Next:=Nil;
      end;
    finally
      CloseFile(Command_File_Object);
    end;

  except
    ShowMessage('Cannot open the file.');
  end;
end;

procedure WriteFile_by_LinkedList(AFile: String; AFileData: PCOMMAND_Ptr);
var
  Command_File_Object:TextFile;
  Command_Ptr:PCOMMAND_Ptr;
  Command_Params_Index:Integer;
begin
  try
    AssignFile(Command_File_Object, AFile);
  except
    Exit;
  end;

  try
    Rewrite(Command_File_Object);
    Command_Ptr:=AFileData;
    while Command_Ptr<>Nil do
    begin
      Write(Command_File_Object, Command_Ptr^.COMMAND_type+' ');
      Write(Command_File_Object, Command_Ptr^.COMMAND_name);
      if (Length(Command_Ptr^.COMMAND_Params))<>0 then
      begin
        Write(Command_File_Object, ',');
        for Command_Params_Index:=0 to (Length(Command_Ptr^.COMMAND_Params)-1) do
        begin
          Write(Command_File_Object, Command_Ptr^.COMMAND_Params[Command_Params_Index].Param_name+'=');
          Write(Command_File_Object, Command_Ptr^.COMMAND_Params[Command_Params_Index].Param_value);
          if Command_Params_Index=(Length(Command_Ptr^.COMMAND_Params)-1) then
            WriteLn(Command_File_Object)
          else
            Write(Command_File_Object, ',');
        end;
      end
      else
        WriteLn(Command_File_Object);

      Command_Ptr:=Command_Ptr^.Next;
    end;
  finally
    CloseFile(Command_File_Object);
  end;
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
    ParamStringList:=TStringList.Create;

    for Index:=1 to (TempStringList.count-1) do
    begin
      SplitString(ParamStringList, TempStringList[Index], '=', 1);
      ACommand_Line_Record.COMMAND_Params[Index-1].Param_name:=ParamStringList[0];

      if ParamStringList.count=2 then
        ACommand_Line_Record.COMMAND_Params[Index-1].Param_value:=ParamStringList[1]
      else
        ACommand_Line_Record.COMMAND_Params[Index-1].Param_value:='';
    end;

    ParamStringList.Free;
  end
  else
    SetLength(ACommand_Line_Record.COMMAND_Params, 0);

  TempStringList.Free;
end;

{ TCOMMANDS_list }

function TCOMMANDS_list.GetItems(Index: Integer): PCOMMAND_Ptr;
begin
  Result:=Self.Items[Index];
end;

procedure TCOMMANDS_list.SetItems(Index: Integer; AValue: PCOMMAND_Ptr);
begin
  Self.Items[Index]:=AValue;
end;

procedure TCOMMANDS_list.ReadFile(AFile: string);
begin
  ReadFile_by_Tlist(AFile, self);
end;

end.

