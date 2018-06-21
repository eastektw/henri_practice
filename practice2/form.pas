unit form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, Types, Contnrs, shape_data_type, check_form, tools, LCLProc;

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

    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure LoadFileButtonClick(Sender: TObject);
    procedure EditButtonClick(Sender: TObject);
    procedure SaveButtonClick(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure Timer1Timer(Sender: TObject);
  private

  public
    procedure ShapeObjectListToStringGrid(AObjectList:TObjectList; AStringGrid:TStringGrid);
    procedure WriteValueToObjectList(AStringValue: String; AObjectList: TObjectList; const ACol, ARow: Integer);
    function ValueInObjectList(AObjectList: TObjectList; const ACol, ARow: Integer): String;
  end;

const
  ExpandValue=10000000;
  PointName='P';
  LineName='L';
  InEditButtonName='End Edit';
  OutEditButtonName='Edit';

var
  Form1: TForm1;
  ShapeObjectList:TObjectList;

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

procedure TForm1.FormDestroy(Sender: TObject);
begin
  ShapeObjectList.Free;
end;

procedure TForm1.LoadFileButtonClick(Sender: TObject);
var
  FileName:String;
begin
  if OpenDialog1.Execute then
  begin
    try
      ShapeObjectList.Free;
      FileName:=OpenDialog1.FileName;
      ShapeObjectList:=TObjectList.Create;
      ReadFileIntoObjecList(FileName, ShapeObjectList, ExpandValue);
      ShapeObjectListToStringGrid(ShapeObjectList, StringGrid1);
      EditButton.Enabled:=True;
      SaveButton.Enabled:=True;
    except
      exit;
      ShapeObjectList.Free;
    end;
  end;
end;

procedure TForm1.EditButtonClick(Sender: TObject);
var
  RowIndexInForm2, ColIndexInForm2: Integer;
  RowIndexInForm1, ColIndexInForm1: Integer;
begin
  if EditButton.Caption=OutEditButtonName then
  begin
    StringGrid1.Options:=StringGrid1.Options+[GoEditing];
    Form2.Show;
    Form2.Left:=Form1.Left+Form1.Width;
    Form2.Top:=Form1.Top;
    EditButton.Caption:=InEditButtonName;
    LoadFileButton.Enabled:=False;
    SaveButton.Enabled:=False;
    Timer1.Enabled:=True;
  end
  else
  begin
    StringGrid1.Options:=StringGrid1.Options-[GoEditing];
    Form2.Hide;
    EditButton.Caption:=OutEditButtonName;
    LoadFileButton.Enabled:=True;
    SaveButton.Enabled:=True;
    for ColIndexInForm2:=0 to (Form2.CheckStringGrid.ColCount-1) do
    begin
      for RowIndexInForm2:=0 to (Form2.CheckStringGrid.RowCount-1) do
      begin
        if Form2.GridCells[ColIndexInForm2][RowIndexInForm2]=clYellow then
        begin
          ColIndexInForm1:=ColIndexInForm2;
          RowIndexInForm1:=StrToInt(Form2.CheckStringGrid.Cells[0, RowIndexInForm2]);
          WriteValueToObjectList(StringGrid1.Cells[ColIndexInForm1, RowIndexInForm1], ShapeObjectList, ColIndexInForm1, RowIndexInForm1);
        end;
      end;
    end;
    Form2.DefaultCheckStringGrid;
    StringGrid1.Invalidate;
  end;
end;

procedure TForm1.SaveButtonClick(Sender: TObject);
var
  FileName:String;
begin
  if OpenDialog1.Execute then
  begin
    try
      FileName:=OpenDialog1.FileName;
      WriteObjectListIntoFile(FileName, ShapeObjectList, ExpandValue);
    except
      exit;
    end;
  end;
end;

procedure TForm1.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  OriginValue, CurrentValue: String;
  LeftGap, TopGap: Integer;
begin
  if EditButton.Caption=InEditButtonName then
  begin
    if (aCol>0) and (aRow>0) then
    begin
      OriginValue:=ValueInObjectList(ShapeObjectList, aCol, aRow);
      CurrentValue:=StringGrid1.Cells[aCol, aRow];
      if OriginValue<>CurrentValue then
        StringGrid1.Canvas.Brush.Color:=clYellow;
    end;
  end
  else
  if EditButton.Caption=OutEditButtonName then
  begin
    if (aCol>0) and (aRow>0) then
      StringGrid1.Canvas.Brush.Color:=clWhite;
  end;

  StringGrid1.Canvas.FillRect(aRect);
  LeftGap:=5;
  TopGap:=((aRect.Bottom-aRect.Top)-StringGrid1.Canvas.TextHeight(StringGrid1.Cells[ACol,ARow])) div 2;
  StringGrid1.Canvas.TextOut(aRect.Left+LeftGap, aRect.Top+TopGap, StringGrid1.Cells[ACol,ARow]);
end;

procedure TForm1.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var
  ShapeName: String;
begin
  if EditButton.Caption=InEditButtonName then
  begin
    if (aCol>0) and (aRow>0) then
    begin
      ShapeName:=StringGrid1.Cells[1, aRow];

      if ShapeName=PointName then
        if (aCol=1) or (aCol>4) then StringGrid1.Options:=StringGrid1.Options-[goEditing]
        else StringGrid1.Options:=StringGrid1.Options+[goEditing]
      else
      if ShapeName=LineName then
        if (aCol=1) or (aCol>6) then StringGrid1.Options:=StringGrid1.Options-[goEditing]
        else StringGrid1.Options:=StringGrid1.Options+[goEditing];
    end;
  end;
end;

procedure TForm1.StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
var
  CellInForm2, StrIsNum: Boolean;
  OriginValue, NumString: String;
  TestNum: Extended;
  RowIndexInForm2, ColIndexInForm2: Integer;
begin
  RowIndexInForm2:=Form2.CheckStringGrid.Cols[0].IndexOf(StringGrid1.Rows[ARow][0]);

  if RowIndexInForm2=-1 then
    CellInForm2:=False
  else
    CellInForm2:=True;

  StrIsNum:=TryStrToFloat(Value, TestNum);
  if StrIsNum then
  begin
    OriginValue:=ValueInObjectList(ShapeObjectList, ACol, ARow);

    if (Value<>OriginValue) then
    begin
      if not CellInForm2 then
      begin
        for ColIndexInForm2:=0 to (Form2.CheckStringGrid.ColCount-1) do
          SetLength(Form2.GridCells[ColIndexInForm2], Form2.CheckStringGrid.RowCount+1);
        Form2.GridCells[aCol, Form2.CheckStringGrid.RowCount]:=clYellow;
        Form2.CheckStringGrid.RowCount:=Form2.CheckStringGrid.RowCount+1;
        ValueCopyStringListTo(Form2.CheckStringGrid.Rows[Form2.CheckStringGrid.RowCount-1], StringGrid1.Rows[ARow]);
        Form2.CheckStringGrid.Cells[ACol, Form2.CheckStringGrid.RowCount-1]:=OriginValue;
        if Form2.CheckStringGrid.RowCount>2 then Form2.SortCheckStringGridByFirstCol;
      end
      else
        Form2.GridCells[aCol, RowIndexInForm2]:=clYellow;
    end
    else
    if (Value=OriginValue) and CellInForm2 then
    begin
      if CompareTwoStringList(StringGrid1.Rows[ARow], Form2.CheckStringGrid.Rows[RowIndexInForm2]) then
        DeleteStringGridRowAt(RowIndexInForm2, Form2.CheckStringGrid)
      else
        Form2.GridCells[aCol, RowIndexInForm2]:=clWhite;
    end;
    Form2.CheckStringGrid.Invalidate;
  end
  else
  begin
    NumString:=Copy(Value, 0, Length(Value)-1);
    StringGrid1.Cells[ACol, ARow]:=NumString;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if (EditButton.Caption=InEditButtonName) and (not Form2.Showing) then
  begin
    EditButton.Caption:=OutEditButtonName;
    SaveButton.Enabled:=True;
    LoadFileButton.Enabled:=True;
    ShapeObjectListToStringGrid(ShapeObjectList, StringGrid1);
    StringGrid1.Invalidate;
    Timer1.Enabled:=False;
  end;
end;

procedure TForm1.ShapeObjectListToStringGrid(AObjectList: TObjectList;
  AStringGrid: TStringGrid);
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

procedure TForm1.WriteValueToObjectList(AStringValue: String;
  AObjectList: TObjectList; const ACol, ARow: Integer);
var
  ShapeName: String;
  NumValue: Integer;
  TempPoint1, TempPoint2: TPoint;
  TempPointObject: TPointShape;
  TempLineObject: TLineShape;
begin
  ShapeName:=StringGrid1.Cells[1, ARow];
  NumValue:=trunc(StrToFloat(AStringValue)*ExpandValue);

  if ShapeName=PointName then
  begin
    TempPointObject:=TPointShape(AObjectList[ARow-1]);
    TempPoint1:=TPointShape(AObjectList[ARow-1]).Point;
    if ACol=2 then TempPoint1.x:=NumValue
    else if ACol=3 then TempPoint1.y:=NumValue
    else if ACol=4 then TempPointObject.Radius:=NumValue;
    TempPointObject.Point:=TempPoint1;
  end
  else
  if ShapeName=LineName then
  begin
    TempLineObject:=TLineShape(AObjectList[ARow-1]);
    TempPoint1:=TLineShape(AObjectList[ARow-1]).StartPoint;
    TempPoint2:=TLineShape(AObjectList[ARow-1]).EndPoint;
    if ACol=2 then TempPoint1.x:=NumValue
    else if ACol=3 then TempPoint1.y:=NumValue
    else if ACol=4 then TempPoint2.x:=NumValue
    else if ACol=5 then TempPoint2.y:=NumValue
    else if ACol=6 then TempLineObject.Radius:=NumValue;
    TempLineObject.StartPoint:=TempPoint1;
    TempLineObject.EndPoint:=TempPoint2;
  end;
end;

function TForm1.ValueInObjectList(AObjectList: TObjectList; const ACol, ARow: Integer): String;
var
  ObjectName: String;
  TempObject: TShape;
begin
  if (ACol<StringGrid1.ColCount) and (ARow<StringGrid1.RowCount) then
  begin
    ObjectName:=StringGrid1.Cells[1, ARow];
    TempObject:=TShape(AObjectList[ARow-1]);
    if Length(ObjectName)<>0 then
    begin
      if ObjectName=PointName then
      begin
        if ACol=1 then Result:=ObjectName
        else if ACol=2 then Result:=FloatToStr(TPointShape(TempObject).Point.x / ExpandValue)
        else if ACol=3 then Result:=FloatToStr(TPointShape(TempObject).Point.y / ExpandValue)
        else if ACol=4 then Result:=FloatToStr(TPointShape(TempObject).Radius / ExpandValue);
      end
      else if ObjectName=LineName then
      begin
        if ACol=1 then Result:=ObjectName
        else if ACol=2 then Result:=FloatToStr(TLineShape(TempObject).StartPoint.x / ExpandValue)
        else if ACol=3 then Result:=FloatToStr(TLineShape(TempObject).StartPoint.y / ExpandValue)
        else if ACol=4 then Result:=FloatToStr(TLineShape(TempObject).EndPoint.x / ExpandValue)
        else if ACol=5 then Result:=FloatToStr(TLineShape(TempObject).EndPoint.y / ExpandValue)
        else if ACol=6 then Result:=FloatToStr(TLineShape(TempObject).Radius / ExpandValue);
      end;
    end;
  end;
end;

end.

