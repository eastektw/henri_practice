unit form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, Types, Contnrs, shape_data_type, check_form, math;


type

  { TChangedShapeObjectList }

  TChangedShapeObjectList = class(TObjectList)
    private
      NOriginIndexes:Array of Integer;

      function GetOriginIndex(Index: Integer): Integer;
      procedure SetOriginIndex(Index: Integer; AValue: Integer);
    public
      property OriginIndex[Index:Integer]:Integer read GetOriginIndex write SetOriginIndex;
      procedure AddChangedShapeObject(AShapeObject: TShape; AOriginIndex: Integer);
      procedure RemoveOriginIndex(AOriginIndex: Integer);
      procedure ExtraClear;
      procedure ExtraFree;
      function FindOriginIndex(AOriginIndex: Integer): Boolean;
      constructor Create;
  end;

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

  public
    OriginShapeObjectList:TObjectList;
    ChangedShapeObjectList:TChangedShapeObjectList;

    //procedure
    procedure ShapeObjectListToStringGrid(AObjectList:TObjectList; AStringGrid:TStringGrid);
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
var
  ColIndex, EqualWidth:Integer;
begin
  EqualWidth:=Width div StringGrid1.ColCount;
  for ColIndex:=0 to (StringGrid1.ColCount-1) do
    StringGrid1.ColWidths[ColIndex]:=EqualWidth;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  OriginShapeObjectList.Free;
  ChangedShapeObjectList.ExtraFree;
end;

procedure TForm1.LoadFileButtonClick(Sender: TObject);
var
  FileName:String;
begin
  if OpenDialog1.Execute then
  begin
    OriginShapeObjectList:=TObjectList.Create;
    ChangedShapeObjectList:=TChangedShapeObjectList.Create;
    FileName:=OpenDialog1.FileName;
    try
      ReadFileIntoObjecList(FileName, OriginShapeObjectList, ExpandValue);
      ShapeObjectListToStringGrid(OriginShapeObjectList, StringGrid1);
      EditButton.Enabled:=True;
      SaveButton.Enabled:=True;
    except
      OriginShapeObjectList.Free;
      ChangedShapeObjectList.ExtraFree;
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
    ChangedShapeObjectList.ExtraClear;
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
      WriteObjectListIntoFile(FileName, OriginShapeObjectList, ExpandValue);
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
  if (aCol>0) and (aRow>0) then
    InDataRegion:=True
  else
    InDataRegion:=False;

  if (EditButton.Caption=InEditButtonName) and InDataRegion then
  begin
    OriginValue:=ValueInOriginObjectList(aCol, aRow);
    CurrentValue:=StringGrid1.Cells[aCol, aRow];
    if OriginValue<>CurrentValue then
      StringGrid1.Canvas.Brush.Color:=clYellow;
  end
  else if (EditButton.Caption=OutEditButtonName) and InDataRegion then
    StringGrid1.Canvas.Brush.Color:=clWhite;

  StringGrid1.Canvas.FillRect(aRect);
  LeftGap:=5;
  TopGap:=((aRect.Bottom-aRect.Top)-StringGrid1.Canvas.TextHeight(StringGrid1.Cells[ACol,ARow])) div 2;
  StringGrid1.Canvas.TextOut(aRect.Left+LeftGap, aRect.Top+TopGap, StringGrid1.Cells[ACol,ARow]);
end;

procedure TForm1.StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
const
  UpperBound=100.0;
  lowerBound=(-100.0);
var
  OriginIndex:Integer;
  InChangedList:Boolean;
  StrIsNum: Boolean;
  DataNum: Double;
  OriginValue, NumString: String;
begin
  OriginIndex:=ARow-1;
  InChangedList:=ChangedShapeObjectList.FindOriginIndex(OriginIndex);
  StrIsNum:=TryStrToFloat(Value, DataNum);
  OriginValue:=ValueInOriginObjectList(ACol, ARow);
  NumString:='';

  if not StrIsNum then
  begin
    NumString:=Copy(Value, 0, Length(Value)-1);
    if Length(Value)=0 then NumString:='0';
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
    ShapeObjectListToStringGrid(OriginShapeObjectList, StringGrid1);
    StringGrid1.Invalidate;
    Timer1.Enabled:=False;
  end;
end;

procedure TForm1.ShapeObjectListToStringGrid(AObjectList: TObjectList;
  AStringGrid: TStringGrid);
var
  TempShapeObject: TShape;
  TempPointObject: TPointShape;
  TempLineObject: TLineShape;
  ObjectListIndex, RowStartIndex:Integer;
  ShapeName:String;
begin
  ShapeName:='';
  AStringGrid.RowCount:=AObjectList.Count+1;
  AStringGrid.ColCount:=7;//因為整筆資料中，線的屬性最多，其屬性的資料總共有6筆，而又要在這筆資料前面加序號，所以最多會有7筆資料要填入
  RowStartIndex:=1;

  for ObjectListIndex:=0 to (AObjectList.Count-1) do
  begin
    AStringGrid.Cells[0, RowStartIndex+ObjectListIndex]:=IntToStr(ObjectListIndex+1);

    TempShapeObject:=AObjectList[ObjectListIndex] as TShape;
    if TempShapeObject.Name=PointType then ShapeName:='P'
    else if TempShapeObject.Name=LineType then ShapeName:='L'
    else continue;
    AStringGrid.Cells[1, RowStartIndex+ObjectListIndex]:=ShapeName;

    if AObjectList[ObjectListIndex] is TPointShape then
    begin
      TempPointObject:=AObjectList[ObjectListIndex] as TPointShape;
      AStringGrid.Cells[2, RowStartIndex+ObjectListIndex]:=FloatToStr(TempPointObject.Point.X / ExpandValue);
      AStringGrid.Cells[3, RowStartIndex+ObjectListIndex]:=FloatToStr(TempPointObject.Point.Y / ExpandValue);
      AStringGrid.Cells[4, RowStartIndex+ObjectListIndex]:=FloatToStr(TempPointObject.Radius / ExpandValue);
    end
    else if AObjectList[ObjectListIndex] is TLineShape then
    begin
      TempLineObject:=AObjectList[ObjectListIndex] as TLineShape;
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
  TempShapeType: TShapeType;
  TempPoint1, TempPoint2: TPoint;
  TempRadius: Integer;
  NumData:Double;
begin
  if (ACol<2) or (ARow<1) then Exit;
  if not TryStrToFloat(AValue, NumData) then Exit;

  if OriginShapeObjectList[ARow-1] is TPointShape then
  begin
    InPointObject:=OriginShapeObjectList[ARow-1] as TPointShape;
    TempShapeType:=InPointObject.Name;
    TempPoint1:=InPointObject.Point;
    TempRadius:=InPointObject.Radius;
    if ACol=2 then TempPoint1.x:=trunc(NumData*ExpandValue)
    else if ACol=3 then TempPoint1.y:=trunc(NumData*ExpandValue)
    else if ACol=4 then TempRadius:=trunc(NumData*ExpandValue);
    OutPointObject:=TPointShape.create(TempShapeType, TempPoint1, TempRadius);
    ChangedShapeObjectList.AddChangedShapeObject(OutPointObject, ARow-1);
  end
  else if OriginShapeObjectList[ARow-1] is TLineShape then
  begin
    InLineObject:=OriginShapeObjectList[ARow-1] as TLineShape;
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
    ChangedShapeObjectList.AddChangedShapeObject(OutLineObject, ARow-1);
  end;
end;

procedure TForm1.ModifyChangedObjectList(const AValue: String; ACol, ARow: Integer);
var
  TempPointObject: TPointShape;
  TempLineObject: TLineShape;
  TempPoint1, TempPoint2: TPoint;
  Index: Integer;
  OriginIndex: Integer;
  ChangedIndex: Integer;
  TestNum: Double;
  NumData: Integer;
begin
  if (ACol<2) or (ARow<1) then Exit;
  if not TryStrToFloat(AValue, TestNum) then Exit;

  OriginIndex:=ARow-1;
  NumData:=trunc(StrToFloat(AValue)*ExpandValue);

  for Index:=0 to (ChangedShapeObjectList.Count-1) do
    if ChangedShapeObjectList.OriginIndex[Index]=OriginIndex then ChangedIndex:=Index;

  if ChangedShapeObjectList[ChangedIndex] is TPointShape then
  begin
    TempPointObject:=ChangedShapeObjectList[ChangedIndex] as TPointShape;
    TempPoint1:=Point(TempPointObject.Point.x, TempPointObject.Point.y);
    if ACol=2 then TempPoint1.x:=NumData
    else if ACol=3 then TempPoint1.y:=NumData
    else if ACol=4 then TempPointObject.Radius:=NumData;
    TempPointObject.Point:=TempPoint1;
  end
  else if ChangedShapeObjectList[ChangedIndex] is TLineShape then
  begin
    TempLineObject:=ChangedShapeObjectList[ChangedIndex] as TLineShape;
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
  OriginIndex: Integer;
  ChangedIndex:Integer;
  TempShapeObject1, TempShapeObject2: TShape;
begin
  OriginIndex:=ARow-1;
  for Index:=0 to (ChangedShapeObjectList.Count-1) do
      if ChangedShapeObjectList.OriginIndex[Index]=OriginIndex then ChangedIndex:=Index;
  TempShapeObject1:=OriginShapeObjectList[OriginIndex] as TShape;
  TempShapeObject2:=ChangedShapeObjectList[ChangedIndex] as TShape;
  if TwoShapeObjectSame(TempShapeObject1, TempShapeObject2) then
  begin
    ChangedShapeObjectList.Delete(ChangedIndex);
    ChangedShapeObjectList.RemoveOriginIndex(OriginIndex);
  end;
end;

procedure TForm1.PrintChangedObjectList;
var
  ChangedIndex, OriginIndex: Integer;
  OriginPointObject: TPointShape;
  OriginLineObject: TLineShape;
begin
  Form2.DefaultCheckStringGrid;
  Form2.CheckStringGrid.RowCount:=ChangedShapeObjectList.Count+1;
  for ChangedIndex:=0 to (ChangedShapeObjectList.Count-1) do
  begin
    OriginIndex:=ChangedShapeObjectList.OriginIndex[ChangedIndex];
    if OriginShapeObjectList[OriginIndex] is TPointShape then
    begin
      OriginPointObject:=OriginShapeObjectList[OriginIndex] as TPointShape;
      Form2.CheckStringGrid.Cells[0, ChangedIndex+1]:=IntToStr(OriginIndex+1);
      Form2.CheckStringGrid.Cells[1, ChangedIndex+1]:='P';
      Form2.CheckStringGrid.Cells[2, ChangedIndex+1]:=FloatToStr(OriginPointObject.Point.x / ExpandValue);
      Form2.CheckStringGrid.Cells[3, ChangedIndex+1]:=FloatToStr(OriginPointObject.Point.y / ExpandValue);
      Form2.CheckStringGrid.Cells[4, ChangedIndex+1]:=FloatToStr(OriginPointObject.Radius / ExpandValue);
    end
    else if OriginShapeObjectList[OriginIndex] is TLineShape then
    begin
      OriginLineObject:=OriginShapeObjectList[OriginIndex] as TLineShape;
      Form2.CheckStringGrid.Cells[0, ChangedIndex+1]:=IntToStr(OriginIndex+1);
      Form2.CheckStringGrid.Cells[1, ChangedIndex+1]:='L';
      Form2.CheckStringGrid.Cells[2, ChangedIndex+1]:=FloatToStr(OriginLineObject.StartPoint.x / ExpandValue);
      Form2.CheckStringGrid.Cells[3, ChangedIndex+1]:=FloatToStr(OriginLineObject.StartPoint.y / ExpandValue);
      Form2.CheckStringGrid.Cells[4, ChangedIndex+1]:=FloatToStr(OriginLineObject.EndPoint.x / ExpandValue);
      Form2.CheckStringGrid.Cells[5, ChangedIndex+1]:=FloatToStr(OriginLineObject.EndPoint.y / ExpandValue);
      Form2.CheckStringGrid.Cells[6, ChangedIndex+1]:=FloatToStr(OriginLineObject.Radius / ExpandValue);
    end;
  end;
end;

procedure TForm1.UpdateOriginShapeObjectList;
var
  ChangedIndex, OriginIndex: Integer;
  OriginPointObject, ChangedPointObject: TPointShape;
  OriginLineObject, ChangedLineObject: TLineShape;
begin
  if ChangedShapeObjectList.Count=0 then Exit;

  for ChangedIndex:=0 to (ChangedShapeObjectList.Count-1) do
  begin
    OriginIndex:=ChangedShapeObjectList.OriginIndex[ChangedIndex];
    if OriginShapeObjectList[OriginIndex] is TPointShape then
    begin
      OriginPointObject:=OriginShapeObjectList[OriginIndex] as TPointShape;
      ChangedPointObject:=ChangedShapeObjectList[ChangedIndex] as TPointShape;
      OriginPointObject.Name:=ChangedPointObject.Name;
      OriginPointObject.Point:=ChangedPointObject.Point;
      OriginPointObject.Radius:=ChangedPointObject.Radius;
    end
    else if OriginShapeObjectList[OriginIndex] is TLineShape then
    begin
      OriginLineObject:=OriginShapeObjectList[OriginIndex] as TLineShape;
      ChangedLineObject:=ChangedShapeObjectList[ChangedIndex] as TLineShape;
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

{ TChangedShapeObjectList }

function TChangedShapeObjectList.GetOriginIndex(Index: Integer): Integer;
begin
  Result:=NOriginIndexes[Index];
end;

procedure TChangedShapeObjectList.SetOriginIndex(Index: Integer; AValue: Integer
  );
begin
  NOriginIndexes[Index]:=AValue;
end;

procedure TChangedShapeObjectList.AddChangedShapeObject(AShapeObject: TShape;
  AOriginIndex: Integer);
begin
  Add(AShapeObject);
  SetLength(NOriginIndexes, Length(NOriginIndexes)+1);
  NOriginIndexes[Length(NOriginIndexes)-1]:=AOriginIndex;
end;

procedure TChangedShapeObjectList.RemoveOriginIndex(AOriginIndex: Integer);
var
  MoveBackward: Boolean;
  Index: Integer;
begin
  MoveBackward:=False;
  for Index:=0 to (Length(NOriginIndexes)-1) do
  begin
    if MoveBackward then
       NOriginIndexes[Index-1]:=NOriginIndexes[Index];
    if NOriginIndexes[Index]=AOriginIndex then MoveBackward:=True;
  end;
  if MoveBackward then SetLength(NOriginIndexes, Length(NOriginIndexes)-1);
end;

procedure TChangedShapeObjectList.ExtraClear;
begin
  Clear;
  SetLength(NOriginIndexes, 0);
end;

procedure TChangedShapeObjectList.ExtraFree;
begin
  Free;
  SetLength(NOriginIndexes, 0);
end;

function TChangedShapeObjectList.FindOriginIndex(AOriginIndex: Integer
  ): Boolean;
var
  Index:Integer;
begin
  Result:=False;
  for Index:=0 to (Length(NOriginIndexes)-1) do
  begin
    if NOriginIndexes[Index]=AOriginIndex then
    begin
      Result:=True;
      Exit;
    end;
  end;
end;

constructor TChangedShapeObjectList.Create;
begin
  Inherited Create;
  SetLength(NOriginIndexes, 0);
end;

end.

