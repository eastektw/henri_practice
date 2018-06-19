unit form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, Types, Contnrs, shape_data_type, check_form;

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
    Timer1: TTimer;
    procedure EditButtonClick(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LoadFileButtonClick(Sender: TObject);
    procedure StringGrid1EditingDone(Sender: TObject);
    procedure StringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer;
      var Value: string);
    procedure StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure Timer1Timer(Sender: TObject);
  private

  public
    procedure ShapeObjectListToStringGrid(AObjectList:TObjectList; AStringGrid:TStringGrid);
    procedure GotoColRow(ACol, ARow: Integer);
  end;

const
  ExpandValue=10000000;

var
  Form1: TForm1;
  ValueChanged: Boolean;
  GotoCol, GotoRow: Integer;
  OriginValue:String;

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

procedure TForm1.EditButtonClick(Sender: TObject);
const
  InEditButtonName='End Edit';
  OutEditButtonName='Edit';
begin
  if EditButton.Caption=OutEditButtonName then
  begin
    StringGrid1.Options:=StringGrid1.Options+[GoEditing];
    Form2.Show;
    Form2.Left:=Form1.Left+Form1.Width;
    Form2.Top:=Form1.Top;
    EditButton.Caption:=InEditButtonName;
  end
  else
  begin
    StringGrid1.Options:=StringGrid1.Options-[GoEditing];
    Form2.Hide;
    EditButton.Caption:=OutEditButtonName;
  end;
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
    except
      exit;
    end;
  end;
end;

procedure TForm1.StringGrid1EditingDone(Sender: TObject);
begin

end;

procedure TForm1.StringGrid1GetEditText(Sender: TObject; ACol, ARow: Integer;
  var Value: string);
begin
  OriginValue:=Value;
end;

procedure TForm1.StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
var
  ColIndex, RowCount: Integer;
  RowInForm2: Boolean;
begin
  if Form2.CheckStringGrid.Cols[0].IndexOf(StringGrid1.Cells[0, ARow])=-1 then RowInForm2:=False
  else RowInForm2:=True;

  if (Value<>OriginValue) and  (not RowInForm2) then
  begin
    Form2.CheckStringGrid.RowCount:=Form2.CheckStringGrid.RowCount+1;
    for ColIndex:=0 to (StringGrid1.ColCount-1) do
    begin
      if ColIndex<>ACol then
        Form2.CheckStringGrid.Cells[ColIndex, Form2.CheckStringGrid.RowCount-1]:=StringGrid1.Cells[ColIndex, ARow]
      else
      begin
        Form2.CheckStringGrid.Cells[ColIndex, Form2.CheckStringGrid.RowCount-1]:=OriginValue;
      end;
    end;
  end;
end;

procedure TForm1.StringGrid1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  ColIndex, RowIndex: Integer;
begin
  StringGrid1.MouseToCell(X, Y, ColIndex, RowIndex);
end;

procedure TForm1.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin

end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Timer1.Enabled:=False;
  StringGrid1.Col:=GotoCol;
  StringGrid1.Row:=GotoRow;
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

procedure TForm1.GotoColRow(ACol, ARow: Integer);
begin
  GotoCol:=ACol;
  GotoRow:=ARow;
  Timer1.Enabled:=True;
end;

end.

