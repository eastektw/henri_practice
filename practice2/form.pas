unit form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, Types, Contnrs, shape_data_type;

type

  { TForm1 }

  TForm1 = class(TForm)
    EditButton: TButton;
    OpenDialog1: TOpenDialog;
    SaveButton: TButton;
    LoadFileButton: TButton;
    DataPresentPanel: TPanel;
    OperatedButtonPanel: TPanel;
    LoadFileButtonPanel: TPanel;
    StringGrid1: TStringGrid;
    procedure FormResize(Sender: TObject);
    procedure LoadFileButtonClick(Sender: TObject);
  private

  public
    function ShapeObjectListToStringGrid(AObjectList:TObjectList; AStringGrid:TStringGrid):Boolean;
  end;

const
  ExpandValue=10000000;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormResize(Sender: TObject);
var
  ColIndex, EqualWidth:Integer;
begin
  EqualWidth:=Width div StringGrid1.ColCount;
  for ColIndex:=0 to (StringGrid1.ColCount-1) do
    StringGrid1.ColWidths[ColIndex]:=EqualWidth;
end;

procedure TForm1.LoadFileButtonClick(Sender: TObject);
var
  FileName:String;
  ShapeObjectList:TObjectList;
  State:Boolean;
begin
  if OpenDialog1.Execute then
  begin
    try
      FileName:=OpenDialog1.FileName;
      ShapeObjectList:=TObjectList.Create;
      State:=True;

      if State then State:=ReadFileIntoObjecList(FileName, ShapeObjectList, ExpandValue);
      if State then State:=ShapeObjectListToStringGrid(ShapeObjectList, StringGrid1);
      //if State then State
      //ReadTListIntoStringGrid(CommandLineTList, StringGrid1);
      //ResizeStringGridColWidth(StringGrid1);
      //StringGrid1.FixedCols := 2;

    except
      exit;
    end;
  end;
end;

function TForm1.ShapeObjectListToStringGrid(AObjectList: TObjectList;
  AStringGrid: TStringGrid): Boolean;
var
  ObjectListIndex, RowStartIndex:Integer;
begin
  AStringGrid.RowCount:=AObjectList.Count+1;
  AStringGrid.ColCount:=7;//因為整筆資料中，線的屬性最多，屬性的資料總共有6筆，而又要在這筆資料前面加序號，所以最多會有7筆資料要填入
  RowStartIndex:=1;
  for ObjectListIndex:=0 to (AObjectList.Count-1) do;
  begin
    AStringGrid.Cells[0, RowStartIndex+ObjectListIndex]:=IntToStr(ObjectListIndex);
    AStringGrid.Cells[1, RowStartIndex+ObjectListIndex]:=TShape(AObjectList[ObjectListIndex]).Name;

    if AObjectList[ObjectListIndex] is TPoint then
    begin
      AStringGrid.Cells[2, RowStartIndex+ObjectListIndex]:=FloatToStr(TPoint(AObjectList[ObjectListIndex]).Point.X / ExpandValue);
      AStringGrid.Cells[3, RowStartIndex+ObjectListIndex]:=FloatToStr(TPoint(AObjectList[ObjectListIndex]).Point.Y / ExpandValue);
      AStringGrid.Cells[4, RowStartIndex+ObjectListIndex]:=FloatToStr(TPoint(AObjectList[ObjectListIndex]).Radius / ExpandValue);
    end
    else if AObjectList[ObjectListIndex] is TLine then
    begin
      AStringGrid.Cells[2, RowStartIndex+ObjectListIndex]:=FloatToStr(TLine(AObjectList[ObjectListIndex]).Point.X / ExpandValue);
      AStringGrid.Cells[3, RowStartIndex+ObjectListIndex]:=FloatToStr(TLine(AObjectList[ObjectListIndex]).Point.Y / ExpandValue);
      AStringGrid.Cells[4, RowStartIndex+ObjectListIndex]:=FloatToStr(TLine(AObjectList[ObjectListIndex]).Point2.X / ExpandValue);
      AStringGrid.Cells[5, RowStartIndex+ObjectListIndex]:=FloatToStr(TLine(AObjectList[ObjectListIndex]).Point2.Y / ExpandValue);
      AStringGrid.Cells[6, RowStartIndex+ObjectListIndex]:=FloatToStr(TLine(AObjectList[ObjectListIndex]).Radius / ExpandValue);
    end;
  end;



end;

end.

