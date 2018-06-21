unit check_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, Types, tools, LCLProc;

type

  { TForm2 }

  TForm2 = class(TForm)
    CancelButton: TButton;
    StringGridPanel: TPanel;
    ButtonPanel: TPanel;
    CheckStringGrid: TStringGrid;
    procedure CancelButtonClick(Sender: TObject);
    procedure CheckStringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormResize(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private

  public
    GridCells: Array of Array of TColor;
    procedure SortCheckStringGridByFirstCol;
    procedure DefaultCheckStringGrid;
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormResize(Sender: TObject);
var
  ColIndex, EqualWidth:Integer;
begin
  EqualWidth:=Width div CheckStringGrid.ColCount;
  for ColIndex:=0 to (CheckStringGrid.ColCount-1) do
    CheckStringGrid.ColWidths[ColIndex]:=EqualWidth;
end;

procedure TForm2.SortCheckStringGridByFirstCol;
var
  MainRowIndex, SubRowIndex: Integer;
  TempStringList: TStringList;
  PreHeadIndex, NowHeadIndex, GridCellsColIndex: Integer;
  TempColor: TColor;
begin
  if CheckStringGrid.RowCount>2 then
  begin
    TempStringList:=TStringList.Create;
    for MainRowIndex:=(CheckStringGrid.RowCount-1) downto 1 do
    begin
      for SubRowIndex:=2 to MainRowIndex do
      begin
        PreHeadIndex:=StrToInt(CheckStringGrid.Cells[0,SubRowIndex-1]);
        NowHeadIndex:=StrToInt(CheckStringGrid.Cells[0,SubRowIndex]);
        if PreHeadIndex>NowHeadIndex then
        begin
          ValueCopyStringListTo(TempStringList, CheckStringGrid.Rows[SubRowIndex-1]);
          ValueCopyStringListTo(CheckStringGrid.Rows[SubRowIndex-1], CheckStringGrid.Rows[SubRowIndex]);
          ValueCopyStringListTo(CheckStringGrid.Rows[SubRowIndex], TempStringList);
          for GridCellsColIndex:=0 to (Length(GridCells)-1) do
          begin
            TempColor:=GridCells[GridCellsColIndex][SubRowIndex-1];
            GridCells[GridCellsColIndex][SubRowIndex-1]:=GridCells[GridCellsColIndex][SubRowIndex];
            GridCells[GridCellsColIndex][SubRowIndex]:=TempColor;
          end;
        end;
      end;
    end;
    TempStringList.Free;
  end;
end;

procedure TForm2.DefaultCheckStringGrid;
var
  ColIndex: Integer;
begin
  CleanStringGrid(CheckStringGrid);
  CheckStringGrid.RowCount:=1;
  SetLength(GridCells, CheckStringGrid.ColCount);
  for ColIndex:=0 to (Length(GridCells)-1) do
    SetLength(GridCells[ColIndex], 1);
end;

procedure TForm2.CheckStringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  LeftGap, TopGap: Integer;
begin
  DebuglnThreadLog('CheckStringGridDrawCell');
  if GridCells[aCol, aRow]<>clYellow then GridCells[aCol, aRow]:=CheckStringGrid.Canvas.Brush.Color;
  CheckStringGrid.Canvas.Brush.Color:=GridCells[aCol, aRow];
  CheckStringGrid.Canvas.FillRect(aRect);
  LeftGap:=5;
  TopGap:=((aRect.Bottom-aRect.Top)-CheckStringGrid.Canvas.TextHeight(CheckStringGrid.Cells[ACol,ARow])) div 2;
  CheckStringGrid.Canvas.TextOut(aRect.Left+LeftGap, aRect.Top+TopGap, CheckStringGrid.Cells[ACol,ARow]);
end;

procedure TForm2.CancelButtonClick(Sender: TObject);
begin
  Self.Hide;
end;

procedure TForm2.FormShow(Sender: TObject);
begin
  DefaultCheckStringGrid;
  FormResize(Self);
end;

end.

