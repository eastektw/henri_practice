unit form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, Types, Contnrs, shape_data_type, check_form, tools, math;


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

    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
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
    OriginShapeObjectList:TOriginShapeObjectList;
    ChangedShapeObjectList:TChangedShapeObjectList;
  public
    procedure OriginShapeObjectListToStringGrid(AOriginShapeObjectList:TOriginShapeObjectList; AStringGrid:TStringGrid);
    procedure LoadToChangedObjectList(const AValue: String; ACol, ARow: Integer);
    procedure ModifyChangedObjectList(const AValue: String; ACol, ARow: Integer);
    procedure CheckChangedObjectListAt(const ARow: Integer);
    procedure PrintChangedObjectList;
    procedure UpdateOriginShapeObjectList;
    function ValueInOriginObjectList(const ACol, ARow: Integer): String;
  end;

const
  ExpandValue=10000000;
  InEditButtonName='End Edit';
  OutEditButtonName='Edit';

var
  Form1: TForm1;
  Form2: TForm2;

implementation
{$R *.lfm}

{ TForm1 }

procedure TForm1.FormResize(Sender: TObject);
begin
  EqualizeColWidth(Self, StringGrid1);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  OriginShapeObjectList:=TOriginShapeObjectList.Create;
  ChangedShapeObjectList:=TChangedShapeObjectList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  OriginShapeObjectList.Free;
  ChangedShapeObjectList.Free;
end;

procedure TForm1.LoadFileButtonClick(Sender: TObject);
var
  FileName:String;
begin
  if OpenDialog1.Execute then
  begin
    FileName:=OpenDialog1.FileName;
    try
      ReadFileIntoOriginShapeObjecList(FileName, OriginShapeObjectList, ExpandValue);
      OriginShapeObjectListToStringGrid(OriginShapeObjectList, StringGrid1);
      EditButton.Enabled:=True;
      SaveButton.Enabled:=True;
    except
      OriginShapeObjectList.Clear;
      ChangedShapeObjectList.Clear;
    end;
  end;
end;

procedure TForm1.EditButtonClick(Sender: TObject);
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
    Form2.DefaultCheckStringGrid;
    EditButton.Caption:=OutEditButtonName;
    LoadFileButton.Enabled:=True;
    SaveButton.Enabled:=True;
    UpdateOriginShapeObjectList;
    ChangedShapeObjectList.Clear;
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
      WriteOriginShapeObjectListIntoFile(FileName, OriginShapeObjectList, ExpandValue);
    except
    end;
  end;
end;

procedure TForm1.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  OriginValue, CurrentValue: String;
  LeftGap, TopGap: Integer;
  InDataRegion: Boolean;
begin
  OriginValue:='';
  CurrentValue:='';
  LeftGap:=5;
  TopGap:=((aRect.Bottom-aRect.Top)-StringGrid1.Canvas.TextHeight(StringGrid1.Cells[ACol,ARow])) div 2;
  if (aCol>0) and (aRow>0) then
    InDataRegion:=True
  else
    InDataRegion:=False;

  if InDataRegion then
  begin
    OriginValue:=ValueInOriginObjectList(aCol, aRow);
    CurrentValue:=StringGrid1.Cells[aCol, aRow];

    if OriginValue<>CurrentValue then
      StringGrid1.Canvas.Brush.Color:=clYellow
    else
      StringGrid1.Canvas.Brush.Color:=clWhite;
  end;

  StringGrid1.Canvas.FillRect(aRect);
  StringGrid1.Canvas.TextOut(aRect.Left+LeftGap, aRect.Top+TopGap, StringGrid1.Cells[ACol,ARow]);
end;

procedure TForm1.StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
const
  UpperBound=100.0;
  lowerBound=(-100.0);
var
  TempOriginIndex:Integer;
  InChangedList:Boolean;
  StrIsNum: Boolean;
  DataNum: Double;
  OriginValue, NumString: String;
begin
  OriginValue:='';
  NumString:='';
  TempOriginIndex:=ARow-1;
  InChangedList:=ChangedShapeObjectList.HasOriginIndex(TempOriginIndex);
  StrIsNum:=TryStrToFloat(Value, DataNum);
  OriginValue:=ValueInOriginObjectList(ACol, ARow);


  if not StrIsNum then
  begin
    NumString:=Copy(Value, 0, Length(Value)-1);
    if Length(Value)=0 then
      NumString:='0';
    StringGrid1.Cells[ACol, ARow]:=NumString;
  end
  else
  begin
    if (CompareValue(DataNum, UpperBound)=1) then
    begin
      ShowMessage('the upper bound is 100');
      StringGrid1.Cells[ACol, ARow]:='100';
    end
    else
    if (CompareValue(DataNum, lowerBound)=-1) then
    begin
      ShowMessage('the lower bound is -100');
      StringGrid1.Cells[ACol, ARow]:='-100';
    end;

    if (Value<>OriginValue) then
    begin
      if not InChangedList then
        LoadToChangedObjectList(Value, ACol, ARow)
      else
        ModifyChangedObjectList(Value, ACol, ARow);
      PrintChangedObjectList;
    end
    else
    begin
      if InChangedList then
      begin
        ModifyChangedObjectList(Value, ACol, ARow);
        CheckChangedObjectListAt(ARow);
        PrintChangedObjectList;
      end;
    end;
  end;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if (EditButton.Caption=InEditButtonName) and (not Form2.Showing) then
  begin
    EditButton.Caption:=OutEditButtonName;
    SaveButton.Enabled:=True;
    LoadFileButton.Enabled:=True;
    OriginShapeObjectListToStringGrid(OriginShapeObjectList, StringGrid1);
    StringGrid1.Invalidate;
    Timer1.Enabled:=False;
  end;
end;

procedure TForm1.OriginShapeObjectListToStringGrid(
  AOriginShapeObjectList: TOriginShapeObjectList; AStringGrid: TStringGrid);
var
  TempShapeObject: TShape;
  TempPointObject: TPointShape;
  TempLineObject: TLineShape;
  ObjectListIndex, RowStartIndex:Integer;
  ShapeName:String;
begin
  ShapeName:='';
  AStringGrid.RowCount:=AOriginShapeObjectList.Count+1;
  AStringGrid.ColCount:=7;//因為整筆資料中，線的屬性最多，其屬性的資料總共有6筆，而又要在這筆資料前面加序號，所以最多會有7筆資料要填入
  RowStartIndex:=1;

  for ObjectListIndex:=0 to (AOriginShapeObjectList.Count-1) do
  begin
    AStringGrid.Cells[0, RowStartIndex+ObjectListIndex]:=IntToStr(ObjectListIndex+1);

    TempShapeObject:=AOriginShapeObjectList[ObjectListIndex] as TShape;
    if TempShapeObject.Name=PointType then ShapeName:='P'
    else if TempShapeObject.Name=LineType then ShapeName:='L'
    else continue;
    AStringGrid.Cells[1, RowStartIndex+ObjectListIndex]:=ShapeName;

    if AOriginShapeObjectList[ObjectListIndex] is TPointShape then
    begin
      TempPointObject:=AOriginShapeObjectList[ObjectListIndex] as TPointShape;
      AStringGrid.Cells[2, RowStartIndex+ObjectListIndex]:=FloatToStr(TempPointObject.Point.X / ExpandValue);
      AStringGrid.Cells[3, RowStartIndex+ObjectListIndex]:=FloatToStr(TempPointObject.Point.Y / ExpandValue);
      AStringGrid.Cells[4, RowStartIndex+ObjectListIndex]:=FloatToStr(TempPointObject.Radius / ExpandValue);
    end
    else if AOriginShapeObjectList[ObjectListIndex] is TLineShape then
    begin
      TempLineObject:=AOriginShapeObjectList[ObjectListIndex] as TLineShape;
      AStringGrid.Cells[2, RowStartIndex+ObjectListIndex]:=FloatToStr(TempLineObject.StartPoint.X / ExpandValue);
      AStringGrid.Cells[3, RowStartIndex+ObjectListIndex]:=FloatToStr(TempLineObject.StartPoint.Y / ExpandValue);
      AStringGrid.Cells[4, RowStartIndex+ObjectListIndex]:=FloatToStr(TempLineObject.EndPoint.X / ExpandValue);
      AStringGrid.Cells[5, RowStartIndex+ObjectListIndex]:=FloatToStr(TempLineObject.EndPoint.Y / ExpandValue);
      AStringGrid.Cells[6, RowStartIndex+ObjectListIndex]:=FloatToStr(TempLineObject.Radius / ExpandValue);
    end;
  end;
end;

function TForm1.ValueInOriginObjectList(const ACol, ARow: Integer): String;
var
  ShapeName: String;
  TempPointObject: TPointShape;
  TempLineObject: TLineShape;
begin
  ShapeName:='';

  if (ACol<StringGrid1.ColCount) and (ARow<StringGrid1.RowCount) then
  begin
    ShapeName:=StringGrid1.Cells[1, ARow];
    if ShapeName='P' then
    begin
      TempPointObject:=OriginShapeObjectList[ARow-1] as TPointShape;
      if ACol=1 then Result:=ShapeName
      else if ACol=2 then Result:=FloatToStr(TempPointObject.Point.x / ExpandValue)
      else if ACol=3 then Result:=FloatToStr(TempPointObject.Point.y / ExpandValue)
      else if ACol=4 then Result:=FloatToStr(TempPointObject.Radius / ExpandValue);
    end
    else if ShapeName='L' then
    begin
      TempLineObject:=OriginShapeObjectList[ARow-1] as TLineShape;
      if ACol=1 then Result:=ShapeName
      else if ACol=2 then Result:=FloatToStr(TempLineObject.StartPoint.x / ExpandValue)
      else if ACol=3 then Result:=FloatToStr(TempLineObject.StartPoint.y / ExpandValue)
      else if ACol=4 then Result:=FloatToStr(TempLineObject.EndPoint.x / ExpandValue)
      else if ACol=5 then Result:=FloatToStr(TempLineObject.EndPoint.y / ExpandValue)
      else if ACol=6 then Result:=FloatToStr(TempLineObject.Radius / ExpandValue);
    end;
  end;
end;

procedure TForm1.LoadToChangedObjectList(const AValue: String; ACol,
  ARow: Integer);
var
  InPointObject, OutPointObject: TPointShape;
  InLineObject, OutLineObject: TLineShape;
  ChangedPointObject, ChangedLineObject: TChangedShapeObject;
  TempShapeType: TShapeType;
  TempPoint1, TempPoint2: TPoint;
  TempRadius: Integer;
  TempOriginIndex: Integer;
  NumData:Double;
begin
  if (ACol<2) or (ARow<1) then Exit;
  if not TryStrToFloat(AValue, NumData) then Exit;

  TempOriginIndex:=ARow-1;

  if OriginShapeObjectList[TempOriginIndex].Name=PointType then
  begin
    InPointObject:=OriginShapeObjectList[TempOriginIndex] as TPointShape;
    TempShapeType:=InPointObject.Name;
    TempPoint1:=InPointObject.Point;
    TempRadius:=InPointObject.Radius;
    if ACol=2 then TempPoint1.x:=trunc(NumData*ExpandValue)
    else if ACol=3 then TempPoint1.y:=trunc(NumData*ExpandValue)
    else if ACol=4 then TempRadius:=trunc(NumData*ExpandValue);
    OutPointObject:=TPointShape.create(TempShapeType, TempPoint1, TempRadius);
    ChangedPointObject:=TChangedShapeObject.create(OutPointObject, TempOriginIndex);
    ChangedShapeObjectList.AddChangedShapeObject(ChangedPointObject);
    ChangedShapeObjectList.SortByOriginIndex;
  end
  else if OriginShapeObjectList[TempOriginIndex].Name=LineType then
  begin
    InLineObject:=OriginShapeObjectList[TempOriginIndex] as TLineShape;
    TempShapeType:=InLineObject.Name;
    TempPoint1:=InLineObject.StartPoint;
    TempPoint2:=InLineObject.EndPoint;
    TempRadius:=InLineObject.Radius;
    if ACol=2 then TempPoint1.x:=trunc(NumData*ExpandValue)
    else if ACol=3 then TempPoint1.y:=trunc(NumData*ExpandValue)
    else if ACol=4 then TempPoint2.x:=trunc(NumData*ExpandValue)
    else if ACol=5 then TempPoint2.y:=trunc(NumData*ExpandValue)
    else if ACol=6 then TempRadius:=trunc(NumData*ExpandValue);
    OutLineObject:=TLineShape.create(TempShapeType, TempPoint1, TempPoint2, TempRadius);
    ChangedLineObject:=TChangedShapeObject.create(OutLineObject, TempOriginIndex);
    ChangedShapeObjectList.AddChangedShapeObject(ChangedLineObject);
    ChangedShapeObjectList.SortByOriginIndex;
  end;
end;

procedure TForm1.ModifyChangedObjectList(const AValue: String; ACol, ARow: Integer);
var
  TempPointObject: TPointShape;
  TempLineObject: TLineShape;
  TempPoint1, TempPoint2: TPoint;
  Index: Integer;
  TempOriginIndex: Integer;
  TempChangedIndex: Integer;
  TestNum: Double;
  NumData: Integer;
begin
  TempOriginIndex:=ARow-1;
  TempChangedIndex:=0;
  NumData:=trunc(StrToFloat(AValue)*ExpandValue);

  if (ACol<2) or (ARow<1) then Exit;
  if not TryStrToFloat(AValue, TestNum) then Exit;
  if not ChangedShapeObjectList.HasOriginIndex(TempOriginIndex) then Exit;

  for Index:=0 to (ChangedShapeObjectList.Count-1) do
    if ChangedShapeObjectList[Index].OriginIndex=TempOriginIndex then
    begin
      TempChangedIndex:=Index;
      break;
    end;

  if ChangedShapeObjectList[TempChangedIndex].ChangedShapeObject.Name=PointType then
  begin
    TempPointObject:=ChangedShapeObjectList[TempChangedIndex].ChangedShapeObject as TPointShape;
    TempPoint1:=Point(TempPointObject.Point.x, TempPointObject.Point.y);
    if ACol=2 then TempPoint1.x:=NumData
    else if ACol=3 then TempPoint1.y:=NumData
    else if ACol=4 then TempPointObject.Radius:=NumData;
    TempPointObject.Point:=TempPoint1;
  end
  else if ChangedShapeObjectList[TempChangedIndex].ChangedShapeObject.Name=LineType then
  begin
    TempLineObject:=ChangedShapeObjectList[TempChangedIndex].ChangedShapeObject as TLineShape;
    TempPoint1:=Point(TempLineObject.StartPoint.x, TempLineObject.StartPoint.y);
    TempPoint2:=Point(TempLineObject.EndPoint.x, TempLineObject.EndPoint.y);
    if ACol=2 then TempPoint1.x:=NumData
    else if ACol=3 then TempPoint1.y:=NumData
    else if ACol=4 then TempPoint2.x:=NumData
    else if ACol=5 then TempPoint2.y:=NumData
    else if ACol=6 then TempLineObject.Radius:=NumData;
    TempLineObject.StartPoint:=TempPoint1;
    TempLineObject.EndPoint:=TempPoint2;
  end;
end;

procedure TForm1.CheckChangedObjectListAt(const ARow: Integer);
var
  Index: Integer;
  TempOriginIndex: Integer;
  TempChangedIndex:Integer;
  OriginShapeObject, ChangedShapeObject: TShape;
begin
  TempOriginIndex:=ARow-1;
  TempChangedIndex:=0;
  for Index:=0 to (ChangedShapeObjectList.Count-1) do
      if ChangedShapeObjectList[Index].OriginIndex=TempOriginIndex then
      begin
        TempChangedIndex:=Index;
        break;
      end;
  OriginShapeObject:=OriginShapeObjectList[TempOriginIndex] as TShape;
  ChangedShapeObject:=ChangedShapeObjectList[TempChangedIndex].ChangedShapeObject as TShape;
  if TwoShapeObjectSame(OriginShapeObject, ChangedShapeObject) then
  begin
    ChangedShapeObjectList.DeleteChangedShapeObject(TempChangedIndex);
  end;
end;

procedure TForm1.PrintChangedObjectList;
var
  TempChangedIndex, TempOriginIndex: Integer;
  OriginPointObject: TPointShape;
  OriginLineObject: TLineShape;
begin
  TempChangedIndex:=0;
  TempOriginIndex:=0;
  Form2.DefaultCheckStringGrid;
  Form2.CheckStringGrid.RowCount:=ChangedShapeObjectList.Count+1;
  for TempChangedIndex:=0 to (ChangedShapeObjectList.Count-1) do
  begin
    TempOriginIndex:=ChangedShapeObjectList[TempChangedIndex].OriginIndex;
    if OriginShapeObjectList[TempOriginIndex] is TPointShape then
    begin
      OriginPointObject:=OriginShapeObjectList[TempOriginIndex] as TPointShape;
      Form2.CheckStringGrid.Cells[0, TempChangedIndex+1]:=IntToStr(TempOriginIndex+1);
      Form2.CheckStringGrid.Cells[1, TempChangedIndex+1]:='P';
      Form2.CheckStringGrid.Cells[2, TempChangedIndex+1]:=FloatToStr(OriginPointObject.Point.x / ExpandValue);
      Form2.CheckStringGrid.Cells[3, TempChangedIndex+1]:=FloatToStr(OriginPointObject.Point.y / ExpandValue);
      Form2.CheckStringGrid.Cells[4, TempChangedIndex+1]:=FloatToStr(OriginPointObject.Radius / ExpandValue);
    end
    else if OriginShapeObjectList[TempOriginIndex] is TLineShape then
    begin
      OriginLineObject:=OriginShapeObjectList[TempOriginIndex] as TLineShape;
      Form2.CheckStringGrid.Cells[0, TempChangedIndex+1]:=IntToStr(TempOriginIndex+1);
      Form2.CheckStringGrid.Cells[1, TempChangedIndex+1]:='L';
      Form2.CheckStringGrid.Cells[2, TempChangedIndex+1]:=FloatToStr(OriginLineObject.StartPoint.x / ExpandValue);
      Form2.CheckStringGrid.Cells[3, TempChangedIndex+1]:=FloatToStr(OriginLineObject.StartPoint.y / ExpandValue);
      Form2.CheckStringGrid.Cells[4, TempChangedIndex+1]:=FloatToStr(OriginLineObject.EndPoint.x / ExpandValue);
      Form2.CheckStringGrid.Cells[5, TempChangedIndex+1]:=FloatToStr(OriginLineObject.EndPoint.y / ExpandValue);
      Form2.CheckStringGrid.Cells[6, TempChangedIndex+1]:=FloatToStr(OriginLineObject.Radius / ExpandValue);
    end;
  end;
end;

procedure TForm1.UpdateOriginShapeObjectList;
var
  TempChangedIndex, TempOriginIndex: Integer;
  OriginPointObject, ChangedPointObject: TPointShape;
  OriginLineObject, ChangedLineObject: TLineShape;
begin
  TempChangedIndex:=0;
  TempOriginIndex:=0;

  if ChangedShapeObjectList.Count=0 then Exit;

  for TempChangedIndex:=0 to (ChangedShapeObjectList.Count-1) do
  begin
    TempOriginIndex:=ChangedShapeObjectList[TempChangedIndex].OriginIndex;
    if OriginShapeObjectList[TempOriginIndex].Name=PointType then
    begin
      OriginPointObject:=OriginShapeObjectList[TempOriginIndex] as TPointShape;
      ChangedPointObject:=ChangedShapeObjectList[TempChangedIndex].ChangedShapeObject as TPointShape;
      OriginPointObject.Name:=ChangedPointObject.Name;
      OriginPointObject.Point:=ChangedPointObject.Point;
      OriginPointObject.Radius:=ChangedPointObject.Radius;
    end
    else if OriginShapeObjectList[TempOriginIndex].Name=LineType then
    begin
      OriginLineObject:=OriginShapeObjectList[TempOriginIndex] as TLineShape;
      ChangedLineObject:=ChangedShapeObjectList[TempChangedIndex].ChangedShapeObject as TLineShape;
      OriginLineObject.Name:=ChangedLineObject.Name;
      OriginLineObject.StartPoint:=ChangedLineObject.StartPoint;
      OriginLineObject.EndPoint:=ChangedLineObject.EndPoint;
      OriginLineObject.Radius:=ChangedLineObject.Radius;
    end;
  end;
end;

procedure TForm1.StringGrid1SelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
var
  ShapeName: String;
  InDataRegion: Boolean;
begin
  ShapeName:='';
  if (aCol<2) and (aRow<1) then
    InDataRegion:=False
  else
    InDataRegion:=True;

  if (EditButton.Caption=InEditButtonName) and InDataRegion then
  begin
    ShapeName:=StringGrid1.Cells[1, aRow];

    if ShapeName='P' then
      if (aCol=1) or (aCol>4) then
        StringGrid1.Options:=StringGrid1.Options-[goEditing]
      else
        StringGrid1.Options:=StringGrid1.Options+[goEditing]
    else
    if ShapeName='L' then
      if (aCol=1) or (aCol>6) then
        StringGrid1.Options:=StringGrid1.Options-[goEditing]
      else
        StringGrid1.Options:=StringGrid1.Options+[goEditing];
  end;
end;

end.

