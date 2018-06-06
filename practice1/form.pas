unit form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, cmd_data_type, tools;

type

  { TForm1 }

  TForm1 = class(TForm)
    ArrayButton: TButton;
    OpenDialog1: TOpenDialog;
    StringGrid1: TStringGrid;
    TListButton: TButton;
    LinkListButton: TButton;
    ClearButton: TButton;
    StringGridPanel: TPanel;
    MidButtonPanel: TPanel;
    BottomButtonPanel: TPanel;
    procedure ArrayButtonClick(Sender: TObject);
    procedure ClearButtonClick(Sender: TObject);
    procedure LinkListButtonClick(Sender: TObject);
    procedure MidButtonPanelResize(Sender: TObject);
    procedure TListButtonClick(Sender: TObject);
  private

  public

  end;
  procedure ReadArrayIntoStringGrid(AFileData:TCOMMANDS_array; AStringGrid:TStringGrid);
  procedure ReadTListIntoStringGrid(AFileData:TList; AStringGrid:TStringGrid);
  procedure ReadLinkedListIntoStringGrid(AFileData:PCOMMAND_Ptr; AStringGrid:TStringGrid);
  Procedure ResizeStringGridColWidth(AStringGrid:TStringGrid);
var
  Form1: TForm1;

implementation

procedure ReadArrayIntoStringGrid(AFileData: TCOMMANDS_array;
  AStringGrid: TStringGrid);
var
  Rows, Cols, NowColIndex, ColsForCommandLine:Integer;
  IndexOfAFileData, IndexOfParam:Integer;
begin
  Rows:=Length(AFileData);
  Cols:=0;
  AStringGrid.RowCount:=Rows+1;

  for IndexOfAFileData:=0 to (Length(AFileData)-1) do
  begin
    ColsForCommandLine:=2+Length(AFileData[IndexOfAFileData].COMMAND_Params)*2;
    if ColsForCommandLine>Cols then
    begin
      Cols:=ColsForCommandLine;
      AStringGrid.ColCount:=Cols+1
    end;

    NowColIndex:=0;
    AStringGrid.Cells[0, IndexOfAFileData+1]:=IntToStr(IndexOfAFileData+1);
    AStringGrid.Cells[1, IndexOfAFileData+1]:=AFileData[IndexOfAFileData].COMMAND_type;
    AStringGrid.Cells[2, IndexOfAFileData+1]:=AFileData[IndexOfAFileData].COMMAND_name;
    NowColIndex:=3;

    if Length(AFileData[IndexOfAFileData].COMMAND_Params)<>0 then
    begin
      for IndexOfParam:=0 to (Length(AFileData[IndexOfAFileData].COMMAND_Params)-1) do
      begin
        AStringGrid.Cells[IndexOfParam+NowColIndex, IndexOfAFileData+1]:=AFileData[IndexOfAFileData].COMMAND_Params[IndexOfParam].Param_name;
        AStringGrid.Cells[IndexOfParam+NowColIndex+1, IndexOfAFileData+1]:=AFileData[IndexOfAFileData].COMMAND_Params[IndexOfParam].Param_value;
        NowColIndex:=NowColIndex+1;
      end;
    end;
  end;
end;

procedure ReadTListIntoStringGrid(AFileData: TList; AStringGrid: TStringGrid);
var
  Rows, Cols, NowColIndex, ColsForCommandLine:Integer;
  IndexOfAFileData, IndexOfParam:Integer;
  Command_Ptr:PCOMMAND_Ptr;
begin
  Rows:=AFileData.Count;
  AStringGrid.RowCount:=Rows+1;
  Cols:=0;

  for IndexOfAFileData:=0 to (AFileData.Count-1) do
  begin
    Command_Ptr:=AFileData[IndexOfAFileData];
    ColsForCommandLine:=2+Length(Command_Ptr^.COMMAND_Params)*2;
    if ColsForCommandLine>Cols then
    begin
      Cols:=ColsForCommandLine;
      AStringGrid.ColCount:=Cols+1
    end;

    NowColIndex:=0;
    AStringGrid.Cells[0, IndexOfAFileData+1]:=IntToStr(IndexOfAFileData+1);
    AStringGrid.Cells[1, IndexOfAFileData+1]:=Command_Ptr^.COMMAND_type;
    AStringGrid.Cells[2, IndexOfAFileData+1]:=Command_Ptr^.COMMAND_name;
    NowColIndex:=3;

    if Length(Command_Ptr^.COMMAND_Params)<>0 then
    begin
      for IndexOfParam:=0 to (Length(Command_Ptr^.COMMAND_Params)-1) do
      begin
        AStringGrid.Cells[IndexOfParam+NowColIndex, IndexOfAFileData+1]:=Command_Ptr^.COMMAND_Params[IndexOfParam].Param_name;
        AStringGrid.Cells[IndexOfParam+NowColIndex+1, IndexOfAFileData+1]:=Command_Ptr^.COMMAND_Params[IndexOfParam].Param_value;
        NowColIndex:=NowColIndex+1;
      end;
    end;
  end;
end;

procedure ReadLinkedListIntoStringGrid(AFileData: PCOMMAND_Ptr;
  AStringGrid: TStringGrid);
var
  Rows, Cols, NowColIndex, NowRowIndex, ColsForCommandLine:Integer;
  IndexOfParam:Integer;
  Command_Ptr:PCOMMAND_Ptr;
begin
  Rows:=0;
  Cols:=0;
  Command_Ptr:=AFileData;

  while Command_Ptr<>Nil do
  begin
    Rows:=Rows+1;
    AStringGrid.RowCount:=Rows+1;
    NowRowIndex:=AStringGrid.RowCount-1;
    ColsForCommandLine:=2+Length(Command_Ptr^.COMMAND_Params)*2;

    if ColsForCommandLine > Cols then
    begin
      Cols:=ColsForCommandLine;
      AStringGrid.ColCount:=Cols+1;
    end;

    NowColIndex:=0;
    AStringGrid.Cells[0, NowRowIndex]:=IntToStr(Rows);
    AStringGrid.Cells[1, NowRowIndex]:=Command_Ptr^.COMMAND_type;
    AStringGrid.Cells[2, NowRowIndex]:=Command_Ptr^.COMMAND_name;
    NowColIndex:=3;

    if Length(Command_Ptr^.COMMAND_Params)<>0 then
    begin
      for IndexOfParam:=0 to (Length(Command_Ptr^.COMMAND_Params)-1) do
      begin
        AStringGrid.Cells[IndexOfParam+NowColIndex, NowRowIndex]:=Command_Ptr^.COMMAND_Params[IndexOfParam].Param_name;
        AStringGrid.Cells[IndexOfParam+NowColIndex+1, NowRowIndex]:=Command_Ptr^.COMMAND_Params[IndexOfParam].Param_value;
        NowColIndex:=NowColIndex+1;
      end;
    end;
    Command_Ptr:=Command_Ptr^.Next;
  end;
end;

procedure ResizeStringGridColWidth(AStringGrid: TStringGrid);
const
  MinColWidth=10;
var
  RowIndex, ColIndex, Width, MaxColWidth:Integer;
begin
  for ColIndex:=0 to (AStringGrid.ColCount-1) do
  begin
    MaxColWidth:=MinColWidth;
    for RowIndex:=0 to (AStringGrid.RowCount-1) do
    begin
      Width:=AStringGrid.Canvas.TextWidth(AStringGrid.Cells[ColIndex, RowIndex]);
      if Width>MaxColWidth then MaxColWidth:=Width;
    end;
    AStringGrid.ColWidths[ColIndex]:=MaxColWidth+AStringGrid.Canvas.TextWidth('A');
  end;
end;

{$R *.lfm}

{ TForm1 }

procedure TForm1.MidButtonPanelResize(Sender: TObject);
var
  ControlIndex:Integer;
  ControlWidth:Integer;
begin
  ControlWidth:=MidButtonPanel.Width div MidButtonPanel.ControlCount;
  for ControlIndex:=0 to (MidButtonPanel.ControlCount-1) do
  begin
    MidButtonPanel.Controls[ControlIndex].Left:=ControlIndex*ControlWidth;
    MidButtonPanel.Controls[ControlIndex].Width:=ControlWidth;
  end;
end;

procedure TForm1.TListButtonClick(Sender: TObject);
var
  FileName:String;
  CommandLineTList:TList;
begin
  if OpenDialog1.Execute then
  begin
    try
      FileName:=OpenDialog1.FileName;
    except
      exit;
    end;
    CommandLineTList:=TList.Create;
    try
      ReadFile_by_Tlist(FileName, CommandLineTList);
      ReadTListIntoStringGrid(CommandLineTList, StringGrid1);
      ResizeStringGridColWidth(StringGrid1);
      StringGrid1.FixedCols := 2;

      FreeList(CommandLineTList);
    finally
      CommandLineTList.Free;
    end;
  end;
end;

procedure TForm1.ArrayButtonClick(Sender: TObject);
var
  FileName:String;
  CommandLineArray:TCOMMANDS_array;
begin
  if OpenDialog1.Execute then
  begin
    try
      FileName:=OpenDialog1.FileName;
    except
      exit;
    end;
    SetLength(CommandLineArray, 0);
    try
      ReadFile_by_array(FileName, CommandLineArray);
      ReadArrayIntoStringGrid(CommandLineArray, StringGrid1);
      ResizeStringGridColWidth(StringGrid1);
      StringGrid1.FixedCols := 2;
    finally
      SetLength(CommandLineArray, 0);
    end;
  end;
end;

procedure TForm1.LinkListButtonClick(Sender: TObject);
var
  FileName:String;
  CommandLineHeadPtr:PCOMMAND_Ptr;
begin
  if OpenDialog1.Execute then
  begin
    try
      FileName:=OpenDialog1.FileName;
    except
      exit;
    end;
    New(CommandLineHeadPtr);
    try
      ReadFile_by_LinkedList(FileName, CommandLineHeadPtr);
      ReadLinkedListIntoStringGrid(CommandLineHeadPtr, StringGrid1);
      ResizeStringGridColWidth(StringGrid1);
      StringGrid1.FixedCols := 2;

    finally
      FreeLinkedList(CommandLineHeadPtr);
    end;
  end;
end;

procedure TForm1.ClearButtonClick(Sender: TObject);
var
  RowIndex, ColIndex:Integer;
begin
  for ColIndex:=0 to (StringGrid1.ColCount-1) do
  begin
    for RowIndex:=0 to (StringGrid1.RowCount-1) do
      StringGrid1.Cells[ColIndex, RowIndex]:='';
  end;
end;

end.

