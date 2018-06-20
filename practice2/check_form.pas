unit check_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, Types, tools;

type

  { TForm2 }

  TForm2 = class(TForm)
    CheckButton: TButton;
    StringGridPanel: TPanel;
    ButtonPanel: TPanel;
    CheckStringGrid: TStringGrid;
    procedure CheckStringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormResize(Sender: TObject);
  private

  public
    GridCells: Array of Array of TColor;
    procedure SortCheckStringGridByFirstCol;
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
  PreHeadIndex, NowHeadIndex: Integer;
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
        end;
      end;
    end;
    TempStringList.Free;
  end;
end;

procedure TForm2.CheckStringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  SetLength(GridCells, aCol+1);
  SetLength(GridCells[aCol], aRow+1);
  GridCells[aCol, aRow]:=CheckStringGrid.Canvas.Brush.Color;
end;

end.

