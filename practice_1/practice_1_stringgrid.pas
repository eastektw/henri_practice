unit practice_1_stringgrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, Grids,
  StdCtrls, ExtCtrls, practice_1_array, practice_1_TList, practice_1_linklist;

type

  { TPractice1 }

  TPractice1 = class(TForm)
    Button1: TButton;
    Button_TList_Run: TButton;
    Button_LinkList_Run: TButton;
    Button_Array_Run: TButton;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    StringGridPanel: TPanel;
    ButtonPanel: TPanel;
    StringGrid1: TStringGrid;
    procedure Button1Click(Sender: TObject);
    procedure ButtonPanelResize(Sender: TObject);
    procedure Button_Array_RunClick(Sender: TObject);
    procedure Button_LinkList_RunClick(Sender: TObject);
    procedure Button_TList_RunClick(Sender: TObject);
  private

  public

  end;

  procedure LoadArrayIntoStringGrid(FileArrayRecord: TFileArrayRecord; StringGrid: TStringGrid);
  procedure LoadTListIntoStringGrid(FileTList: TList; StringGrid: TStringGrid);
  procedure LoadLinkedListIntoStringGrid(FileLinkedList: TLineRecordLinkedList; StringGrid: TStringGrid);
  procedure AutoResizeColsWidth(StringGrid : TStringGrid);
  procedure SetFixedColsAndRows(Cols, Rows : Integer ; StringGrid : TStringGrid);
var
  Practice1: TPractice1;

implementation

procedure LoadArrayIntoStringGrid(FileArrayRecord: TFileArrayRecord; StringGrid: TStringGrid);
var
  LineIndex, LineItemIndex: Integer;
begin
  StringGrid.RowCount:=FileArrayRecord.FileLineCount+1;
  StringGrid.ColCount:=FileArrayRecord.MaxLineItemsCount+1;
  for LineIndex:=0 to (FileArrayRecord.FileLineCount-1) do
  begin
    StringGrid.Cells[0, LineIndex+1]:=IntToStr(LineIndex+1);
    for LineItemIndex:=0 to (Length(FileArrayRecord.FileArray[LineIndex])-1) do
      StringGrid.Cells[LineItemIndex+1, LineIndex+1] := FileArrayRecord.FileArray[LineIndex, LineItemIndex];
  end;
end;

procedure LoadTListIntoStringGrid(FileTList: TList; StringGrid: TStringGrid);
var
  MaxRows, MaxCols, TListIndex, KeyValueRecordArrayIndex, Position: Integer;
  LineRecordPt : PLineRecord;
begin
  //FileTList := ReadFileIntoTList(FileName);
  MaxRows := FileTList.Count;
  StringGrid.RowCount := MaxRows + 1;
  MaxCols := 0;

  for TListIndex := 0 to (FileTList.count - 1) do
  begin
    LineRecordPt := FileTList[TListIndex];

    if LineRecordPt^.ItemCount > MaxCols then
    begin
      MaxCols := LineRecordPt^.ItemCount;
      StringGrid.ColCount := MaxCols + 1;
    end;

    StringGrid.Cells[0, TListIndex + 1]:=IntToStr(TListIndex + 1);
    StringGrid.Cells[1, TListIndex + 1]:=LineRecordPt^.Prefix;
    StringGrid.Cells[2, TListIndex + 1]:=LineRecordPt^.Name;

    Position := 3;
    for KeyValueRecordArrayIndex:=0 to (Length(LineRecordPt^.KeyValuePairs)-1) do
    begin
      StringGrid.Cells[Position, TListIndex+1]:= LineRecordPt^.KeyValuePairs[KeyValueRecordArrayIndex].Key;
      Position:=Position+1;
      StringGrid.Cells[Position, TListIndex+1]:= LineRecordPt^.KeyValuePairs[KeyValueRecordArrayIndex].Value;
      Position:=Position+1;
    end;
  end;
end;

procedure LoadLinkedListIntoStringGrid(FileLinkedList: TLineRecordLinkedList;
  StringGrid: TStringGrid);
var
  MaxRow, MaxCol, NowRow, NowCol, Index, Count: Integer;
  CurrentNodePtr: TNodePtr;
  CurrentLineRecord: TLineRecord;
begin
  MaxRow:=FileLinkedList.Size;
  StringGrid.RowCount:=MaxRow+1;

  MaxCol:=0;
  NowRow:=1;
  Count:=0;
  CurrentNodePtr:=FileLinkedList.Head;

  repeat
    NowCol:=0;
    CurrentLineRecord:=CurrentNodePtr^.LineRecordPtr^;
    if CurrentLineRecord.ItemsCount>MaxCol then
    begin
      MaxCol:=CurrentLineRecord.ItemsCount;
      StringGrid.ColCount:=MaxCol+1;
    end;

    StringGrid.Cells[NowCol, NowRow]:=IntToStr(NowRow);
    NowCol:=NowCol+1;
    StringGrid.Cells[NowCol, NowRow]:=CurrentLineRecord.Command;
    NowCol:=NowCol+1;
    StringGrid.Cells[NowCol, NowRow]:=CurrentLineRecord.Name;
    if CurrentLineRecord.KeyValuePairs.Count<>0 then
    begin
      for Index:=0 to (CurrentLineRecord.KeyValuePairs.Count-1) do
      begin
        NowCol:=NowCol+1;
        StringGrid.Cells[NowCol, NowRow]:=CurrentLineRecord.KeyValuePairs[Index];
      end;
    end;

    CurrentNodePtr:=CurrentNodePtr^.NodePtr;
    NowRow:=NowRow+1;
    Count:=Count+1;
  until Count=FileLinkedList.Size;
end;

procedure AutoResizeColsWidth(StringGrid: TStringGrid);
const
  MinColWidth = 10;
var
  Col, Row, Width, MaxColWidth : Integer;
begin
  for Col := 0 to (StringGrid.ColCount - 1) do
  begin
    MaxColWidth := MinColWidth;
    for Row := 0 to (StringGrid.RowCount - 1) do
    begin
      Width := StringGrid.Canvas.TextWidth(StringGrid.Cells[Col, Row]);
      if Width > MaxColWidth then MaxColWidth := Width;
    end;
    StringGrid.ColWidths[Col] := MaxColWidth + StringGrid.Canvas.TextWidth('A');
  end;
end;

procedure SetFixedColsAndRows(Cols, Rows: Integer; StringGrid: TStringGrid);
begin
  StringGrid.FixedCols := Cols;
  StringGrid.FixedRows := Rows;
end;

{$R *.lfm}

{ TPractice1 }

procedure TPractice1.ButtonPanelResize(Sender: TObject);
var
  Index : Integer;
  ButtonWidth : Integer;
begin
  ButtonWidth := ButtonPanel.Width div ButtonPanel.ControlCount;
  for Index := 0 to (ButtonPanel.ControlCount - 1) do
  begin
    ButtonPanel.Controls[Index].Left := Index * ButtonWidth;
    ButtonPanel.Controls[Index].Width := ButtonWidth;
  end;
end;

procedure TPractice1.Button1Click(Sender: TObject);
var
  Col, Row: Integer;
begin
  for Col:=0 to (StringGrid1.ColCount-1) do
  begin
    for Row:=0 to (StringGrid1.RowCount-1) do
    StringGrid1.Cells[Col, Row]:='';
  end;
end;

procedure TPractice1.Button_Array_RunClick(Sender: TObject);
var
  FileArrayRecord: TFileArrayRecord;
  FileName: String;
begin
  if OpenDialog1.Execute then
  begin
    FileName:=OpenDialog1.FileName;

    ReadFileIntoArray(FileArrayRecord, FileName);
    LoadArrayIntoStringGrid(FileArrayRecord, StringGrid1);
    AutoResizeColsWidth(StringGrid1);
    SetFixedColsAndRows(2, 1, StringGrid1);

    FileArrayRecord.FileArray:=Nil;
  end;
end;

procedure TPractice1.Button_LinkList_RunClick(Sender: TObject);
var
  LinkedList: TLineRecordLinkedList;
  FileName: String;

begin
  if OpenDialog1.Execute then
  begin
    FileName:=OpenDialog1.FileName;

    LinkedList.Head:=Nil;
    LinkedList.Size:=0;

    ReadFileIntoLinkedList(LinkedList, FileName);
    LoadLinkedListIntoStringGrid(LinkedList, StringGrid1);
    AutoResizeColsWidth(StringGrid1);
    SetFixedColsAndRows(2, 1, StringGrid1);
  end;
end;

procedure TPractice1.Button_TList_RunClick(Sender: TObject);
var
  FileTList: TList;
  FileName: String;
begin
  if OpenDialog1.Execute then
  begin
    FileName:=OpenDialog1.FileName;
    FileTList:=TList.Create;
    ReadFileIntoTList(FileTList, FileName);
    LoadTListIntoStringGrid(FileTList, StringGrid1);
    AutoResizeColsWidth(StringGrid1);
    SetFixedColsAndRows(2, 1, StringGrid1);
    FileTList.Free;
  end;
end;

end.

