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
    procedure ShapeObjectListToStringGrid(AObjectList:TObjectList; AStringGrid:TStringGrid);
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
      ShapeObjectListToStringGrid(ShapeObjectList, StringGrid1);
      //if State then State
      //ReadTListIntoStringGrid(CommandLineTList, StringGrid1);
      //ResizeStringGridColWidth(StringGrid1);
      //StringGrid1.FixedCols := 2;
    except
      exit;
    end;
  end;
end;

procedure TForm1.ShapeObjectListToStringGrid(AObjectList: TObjectList;
  AStringGrid: TStringGrid);
const
  PointName='#P';
  LineName='#L';
var
  ObjectListIndex, RowStartIndex:Integer;
  ShapeName:String;
begin
  AStringGrid.RowCount:=AObjectList.Count+1;
  AStringGrid.ColCount:=7;//因為整筆資料中，線的屬性最多，其屬性的資料總共有6筆，而又要在這筆資料前面加序號，所以最多會有7筆資料要填入
  RowStartIndex:=1;

  for ObjectListIndex:=0 to (AObjectList.Count-1) do
  begin
    AStringGrid.Cells[0, RowStartIndex+ObjectListIndex]:=IntToStr(ObjectListIndex+1);
    if TShape(AObjectList[ObjectListIndex]).Name=PointType then ShapeName:=PointName
    else if TShape(AObjectList[ObjectListIndex]).Name=LineType then ShapeName:=LineName
    else continue;
    AStringGrid.Cells[1, RowStartIndex+ObjectListIndex]:=ShapeName;

    if AObjectList[ObjectListIndex] is TPointShape then
    begin
      AStringGrid.Cells[2, RowStartIndex+ObjectListIndex]:=FloatToStr(TPointShape(AObjectList[ObjectListIndex]).Point.X / ExpandValue);
      AStringGrid.Cells[3, RowStartIndex+ObjectListIndex]:=FloatToStr(TPointShape(AObjectList[ObjectListIndex]).Point.Y / ExpandValue);
      AStringGrid.Cells[4, RowStartIndex+ObjectListIndex]:=FloatToStr(TPointShape(AObjectList[ObjectListIndex]).Radius / ExpandValue);
    end
    else if AObjectList[ObjectListIndex] is TLineShape then
    begin
      AStringGrid.Cells[2, RowStartIndex+ObjectListIndex]:=FloatToStr(TLineShape(AObjectList[ObjectListIndex]).StartPoint.X / ExpandValue);
      AStringGrid.Cells[3, RowStartIndex+ObjectListIndex]:=FloatToStr(TLineShape(AObjectList[ObjectListIndex]).StartPoint.Y / ExpandValue);
      AStringGrid.Cells[4, RowStartIndex+ObjectListIndex]:=FloatToStr(TLineShape(AObjectList[ObjectListIndex]).EndPoint.X / ExpandValue);
      AStringGrid.Cells[5, RowStartIndex+ObjectListIndex]:=FloatToStr(TLineShape(AObjectList[ObjectListIndex]).EndPoint.Y / ExpandValue);
      AStringGrid.Cells[6, RowStartIndex+ObjectListIndex]:=FloatToStr(TLineShape(AObjectList[ObjectListIndex]).Radius / ExpandValue);
    end;
  end;
end;

end.

