unit check_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, PrintersDlgs, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, Grids, StdCtrls, Types, tools;

type

  { TForm2 }

  TForm2 = class(TForm)
    CancelButton: TButton;
    StringGridPanel: TPanel;
    ButtonPanel: TPanel;
    CheckStringGrid: TStringGrid;
    //procedure CancelButtonClick(Sender: TObject);
    //procedure CheckStringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      //aRect: TRect; aState: TGridDrawState);
    procedure FormResize(Sender: TObject);
    //procedure FormShow(Sender: TObject);
  private

  public
    ColorGrid: Array of Array of TColor;
    //procedure SortCheckStringGridByFirstCol;
    //procedure DefaultCheckStringGrid;
    //procedure ExchangeRowInColorGridBetween(ARow1, ARow2:Integer);
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
  ColIndex:=0;
  EqualWidth:=0;

  EqualWidth:=Width div CheckStringGrid.ColCount;
  for ColIndex:=0 to (CheckStringGrid.ColCount-1) do
    CheckStringGrid.ColWidths[ColIndex]:=EqualWidth;
end;

//procedure TForm2.SortCheckStringGridByFirstCol;
//var
//  TempStringList: TStringList;
//  DataNum, MainRowIndex, SubRowIndex: Integer;
//  PreHeadIndex, NowHeadIndex, ColorGridColIndex: Integer;
//begin
//
//  DataNum:=0;
//  PreHeadIndex:=0;
//  NowHeadIndex:=0;
//  ColorGridColIndex:=0;
//
//  DataNum:=CheckStringGrid.RowCount-CheckStringGrid.FixedRows;
//  if not (DataNum>1) then Exit;
//
//  TempStringList:=TStringList.Create;
//  try
//    for MainRowIndex:=(CheckStringGrid.RowCount-1) downto 1 do
//    begin
//      for SubRowIndex:=2 to MainRowIndex do
//      begin
//        PreHeadIndex:=StrToInt(CheckStringGrid.Cells[0,SubRowIndex-1]);
//        NowHeadIndex:=StrToInt(CheckStringGrid.Cells[0,SubRowIndex]);
//        if PreHeadIndex>NowHeadIndex then
//        begin
//          ValueCopyStringListTo(TempStringList, CheckStringGrid.Rows[SubRowIndex-1]);
//          ValueCopyStringListTo(CheckStringGrid.Rows[SubRowIndex-1], CheckStringGrid.Rows[SubRowIndex]);
//          ValueCopyStringListTo(CheckStringGrid.Rows[SubRowIndex], TempStringList);
//          ExchangeRowInColorGridBetween(SubRowIndex-1, SubRowIndex);
//        end;
//      end;
//    end;
//  finally
//    TempStringList.Free;
//  end;
//end;

//procedure TForm2.DefaultCheckStringGrid;
//var
//  ColIndex: Integer;
//begin
//  CleanStringGrid(CheckStringGrid);
//  CheckStringGrid.RowCount:=1;
//  SetLength(GridCells, CheckStringGrid.ColCount);
//  for ColIndex:=0 to (Length(GridCells)-1) do
//    SetLength(GridCells[ColIndex], 1);
//end;

//procedure TForm2.ExchangeRowInColorGridBetween(ARow1, ARow2: Integer);
//var
//  GridColIndex, GridRowIndex:Integer;
//  TempColor:TColor;
//begin
//  if (ARow1 > (CheckStringGrid.RowCount-1)) or (ARow2 > (CheckStringGrid.RowCount-1)) or ARow1=ARow2 then
//    Exit;
//
//  GridColIndex:=0;
//  GridRowIndex:=0;
//  TempColor:=CheckStringGrid.Canvas.Brush.Color;
//
//  for GridColIndex:=0 to (Length(ColorGrid)-1) do
//  begin
//    TempColor:=ColorGrid[GridColIndex][ARow1];
//    ColorGrid[GridColIndex][ARow1]:=ColorGrid[GridColIndex][ARow2];
//    ColorGrid[GridColIndex][ARow2]:=TempColor;
//  end;
//end;

//procedure TForm2.CheckStringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
//  aRect: TRect; aState: TGridDrawState);
//var
//  LeftGap, TopGap: Integer;
//begin
//  if GridCells[aCol, aRow]<>clYellow then GridCells[aCol, aRow]:=CheckStringGrid.Canvas.Brush.Color;
//  CheckStringGrid.Canvas.Brush.Color:=GridCells[aCol, aRow];
//  CheckStringGrid.Canvas.FillRect(aRect);
//  LeftGap:=5;
//  TopGap:=((aRect.Bottom-aRect.Top)-CheckStringGrid.Canvas.TextHeight(CheckStringGrid.Cells[ACol,ARow])) div 2;
//  CheckStringGrid.Canvas.TextOut(aRect.Left+LeftGap, aRect.Top+TopGap, CheckStringGrid.Cells[ACol,ARow]);
//end;

//procedure TForm2.CancelButtonClick(Sender: TObject);
//begin
//  Self.Hide;
//end;

//procedure TForm2.FormShow(Sender: TObject);
//begin
//  DefaultCheckStringGrid;
//  FormResize(Self);
//end;

end.

