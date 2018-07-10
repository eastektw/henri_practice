unit form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, Types, Contnrs, shape_data_type, check_form, tools, math;


type
  { TForm1 }

  TForm1 = class(TForm)
    OpenDialog1: TOpenDialog;
    Timer1: TTimer;
    DataPresentPanel: TPanel;
    OperatedButtonPanel: TPanel;
    LoadFileButtonPanel: TPanel;
    StringGrid1: TStringGrid;
    EditButton: TButton;
    SaveButton: TButton;
    LoadFileButton: TButton;

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
    OriginShapeObjectList:TObjectList;
  public
    procedure AppendCheckStringGrid(ARow: Integer);
    procedure CheckCheckStringGrid(ARow: Integer);
    procedure OriginShapeObjectListToStringGrid(AOriginShapeObjectList:TObjectList; AStringGrid:TStringGrid);
    procedure UpdateOriginShapeObjectList;
    procedure SortCheckStringGrid;
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
procedure TForm1.FormCreate(Sender: TObject);
begin
  OriginShapeObjectList:=TObjectList.Create;
end;

procedure TForm1.FormResize(Sender: TObject);
begin
  EqualizeColWidth(Self, StringGrid1);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  OriginShapeObjectList.Free;
end;

procedure TForm1.LoadFileButtonClick(Sender: TObject);
var
  FileName:String;
begin
  if OpenDialog1.Execute then
  begin
    FileName:=OpenDialog1.FileName;
    try
      ReadFileIntoObjectList(FileName, OriginShapeObjectList, ExpandValue);
      OriginShapeObjectListToStringGrid(OriginShapeObjectList, StringGrid1);
      EditButton.Enabled:=True;
      SaveButton.Enabled:=True;
    except
      OriginShapeObjectList.Clear;
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
    LoadFileButton.Enabled:=True;
    SaveButton.Enabled:=True;
    EditButton.Caption:=OutEditButtonName;
    StringGrid1.Options:=StringGrid1.Options-[GoEditing];
    Form2.Hide;
    UpdateOriginShapeObjectList;
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
      WriteShapeObjectListIntoFile(FileName, OriginShapeObjectList, ExpandValue);
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

  if InDataRegion and (OriginShapeObjectList.Count<>0) then
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

procedure TForm1.StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
const
  UpperBound=100.0;
  lowerBound=(-100.0);
var
  TempOriginIndex:Integer;
  TempShapeObject: TShape;
  StrIsNum: Boolean;
  DataNum: Double;
  OriginValue, NumStr: String;
begin
  OriginValue:='';
  TempOriginIndex:=StrToInt(StringGrid1.Cells[0, ARow])-1;
  TempShapeObject:=OriginShapeObjectList[TempOriginIndex] as TShape;
  StrIsNum:=TryStrToFloat(Value, DataNum);
  OriginValue:=ValueInOriginObjectList(ACol, ARow);

  if not StrIsNum then
  begin
    NumStr:=Copy(Value, 0, Length(Value)-1);
    if Length(Value)=0 then
      NumStr:='0';
    StringGrid1.Cells[ACol, ARow]:=NumStr;
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

    if (DecimalPlacesOf(Value)>7) then
    begin
      ShowMessage('Cannot enter more than 7 decimal places');
      StringGrid1.Cells[ACol, ARow]:=FloatToStr(trunc(DataNum*ExpandValue)/ExpandValue);
    end;

    if (Value<>OriginValue) then
    begin
      if not TempShapeObject.Modified then
      begin
        TempShapeObject.Modified:=True;
        AppendCheckStringGrid(ARow);
        SortCheckStringGrid;
      end
    end
    else
    begin
      if TempShapeObject.Modified then
        CheckCheckStringGrid(ARow);
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

procedure TForm1.AppendCheckStringGrid(ARow: Integer);
var
  LastRowIndex: Integer;
  OriginIndex: Integer;
  TempShapeObject: TShape;
  TempPointObject: TPointShape;
  TempLineObject: TLineShape;
begin
  Form2.CheckStringGrid.RowCount:=Form2.CheckStringGrid.RowCount+1;
  LastRowIndex:=Form2.CheckStringGrid.RowCount-1;
  OriginIndex:=ARow-1;
  TempShapeObject:=OriginShapeObjectList[OriginIndex] as TShape;

  if TempShapeObject.Name=PointType then
  begin
    TempPointObject:=TempShapeObject as TPointShape;
    Form2.CheckStringGrid.Cells[0, LastRowIndex]:=IntToStr(ARow);
    Form2.CheckStringGrid.Cells[1, LastRowIndex]:='P';
    Form2.CheckStringGrid.Cells[2, LastRowIndex]:=FloatToStr(TempPointObject.Point.x / ExpandValue);
    Form2.CheckStringGrid.Cells[3, LastRowIndex]:=FloatToStr(TempPointObject.Point.y / ExpandValue);
    Form2.CheckStringGrid.Cells[4, LastRowIndex]:=FloatToStr(TempPointObject.Radius / ExpandValue);
  end
  else
  if TempShapeObject.Name=LineType then
  begin
    TempLineObject:= TempShapeObject as TLineShape;
    Form2.CheckStringGrid.Cells[0, LastRowIndex]:=IntToStr(ARow);
    Form2.CheckStringGrid.Cells[1, LastRowIndex]:='L';
    Form2.CheckStringGrid.Cells[2, LastRowIndex]:=FloatToStr(TempLineObject.StartPoint.x / ExpandValue);
    Form2.CheckStringGrid.Cells[3, LastRowIndex]:=FloatToStr(TempLineObject.StartPoint.y / ExpandValue);
    Form2.CheckStringGrid.Cells[4, LastRowIndex]:=FloatToStr(TempLineObject.EndPoint.x / ExpandValue);
    Form2.CheckStringGrid.Cells[5, LastRowIndex]:=FloatToStr(TempLineObject.EndPoint.y / ExpandValue);
    Form2.CheckStringGrid.Cells[6, LastRowIndex]:=FloatToStr(TempLineObject.Radius / ExpandValue);
  end;
end;

procedure TForm1.CheckCheckStringGrid(ARow: Integer);
var
  OriginStringList, ChangeStringList: TStrings;
  CheckRowIndex, RowIndex: Integer;
  OriginIndex: Integer;
begin
  for RowIndex:=0 to (Form2.CheckStringGrid.RowCount-1) do
  begin
    if Form2.CheckStringGrid.Cells[0, RowIndex]=IntToStr(ARow) then
    begin
      CheckRowIndex:=RowIndex;
      Break;
    end;
  end;

  ChangeStringList:=StringGrid1.Rows[ARow];
  OriginStringList:=Form2.CheckStringGrid.Rows[CheckRowIndex];
  OriginIndex:=ARow-1;
  if CompareTwoStringList(OriginStringList, ChangeStringList) then
  begin
    DeleteStringGridRowAt(CheckRowIndex, Form2.CheckStringGrid);
    TShape(OriginShapeObjectList[OriginIndex]).Modified:=False;
  end;
end;

procedure TForm1.OriginShapeObjectListToStringGrid(
  AOriginShapeObjectList: TObjectList; AStringGrid: TStringGrid);
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
  OriginIndex: Integer;
  TempShapeObject: TShape;
  TempPointObject: TPointShape;
  TempLineObject: TLineShape;
begin
  OriginIndex:=ARow-1;
  TempShapeObject:=OriginShapeObjectList[OriginIndex] as TShape;

  if (ACol<StringGrid1.ColCount) and (ARow<StringGrid1.RowCount) then
  begin
    if TempShapeObject.Name=PointType then
    begin
      TempPointObject:=TempShapeObject as TPointShape;
      if ACol=1 then Result:='P'
      else if ACol=2 then Result:=FloatToStr(TempPointObject.Point.x / ExpandValue)
      else if ACol=3 then Result:=FloatToStr(TempPointObject.Point.y / ExpandValue)
      else if ACol=4 then Result:=FloatToStr(TempPointObject.Radius / ExpandValue);
    end
    else if TempShapeObject.Name=LineType then
    begin
      TempLineObject:=TempShapeObject as TLineShape;
      if ACol=1 then Result:='L'
      else if ACol=2 then Result:=FloatToStr(TempLineObject.StartPoint.x / ExpandValue)
      else if ACol=3 then Result:=FloatToStr(TempLineObject.StartPoint.y / ExpandValue)
      else if ACol=4 then Result:=FloatToStr(TempLineObject.EndPoint.x / ExpandValue)
      else if ACol=5 then Result:=FloatToStr(TempLineObject.EndPoint.y / ExpandValue)
      else if ACol=6 then Result:=FloatToStr(TempLineObject.Radius / ExpandValue);
    end;
  end;
end;

procedure TForm1.UpdateOriginShapeObjectList;
var
  RowIndex: Integer;
  TempOriginIndex: Integer;
  TempShapeName: String;
  TempPoint1, TempPoint2: TPoint;
  TempRadius: Integer;
  TempRowStrings: TStrings;
  TempPointShapeObject: TPointShape;
  TempLineShapeObject: TLineShape;
begin
  if (Form2.CheckStringGrid.RowCount<=1) then Exit;

  for RowIndex:=1 to (Form2.CheckStringGrid.RowCount-1) do
  begin
    TempOriginIndex:=StrToInt(Form2.CheckStringGrid.Cells[0, RowIndex])-1;
    TempRowStrings:=StringGrid1.Rows[TempOriginIndex+1];
    TempShapeName:=TempRowStrings[1];
    if TempShapeName='P' then
    begin
      TempPoint1.x:=trunc(StrToFloat(TempRowStrings[2])*ExpandValue);
      TempPoint1.y:=trunc(StrToFloat(TempRowStrings[3])*ExpandValue);
      TempRadius:=trunc(StrToFloat(TempRowStrings[4])*ExpandValue);
      TempPointShapeObject:=OriginShapeObjectList[TempOriginIndex] as TPointShape;
      TempPointShapeObject.Point:=TempPoint1;
      TempPointShapeObject.Radius:=TempRadius;
      TempPointShapeObject.Modified:=False;
    end
    else
    if TempShapeName='L' then
    begin
      TempPoint1.x:=trunc(StrToFloat(TempRowStrings[2])*ExpandValue);
      TempPoint1.y:=trunc(StrToFloat(TempRowStrings[3])*ExpandValue);
      TempPoint2.x:=trunc(StrToFloat(TempRowStrings[4])*ExpandValue);
      TempPoint2.y:=trunc(StrToFloat(TempRowStrings[5])*ExpandValue);
      TempRadius:=trunc(StrToFloat(TempRowStrings[6])*ExpandValue);
      TempLineShapeObject:=OriginShapeObjectList[TempOriginIndex] as TLineShape;
      TempLineShapeObject.StartPoint:=TempPoint1;
      TempLineShapeObject.EndPoint:=TempPoint2;
      TempLineShapeObject.Radius:=TempRadius;
      TempLineShapeObject.Modified:=False;
    end;
  end;
end;

procedure TForm1.SortCheckStringGrid;
var
  RowIndex, SubRowIndex: Integer;
  PreIndex, Nowindex: Integer;
  PreStrings: TStringList;
begin
  if Form2.CheckStringGrid.RowCount<3 then Exit;
  PreStrings:=TStringList.Create;

  Try
    for RowIndex:=(Form2.CheckStringGrid.RowCount-1) downto 2 do
    begin
      for SubRowIndex:=2 to RowIndex do
      begin
        PreIndex:=StrToInt(Form2.CheckStringGrid.Cells[0, SubRowIndex-1]);
        NowIndex:=StrToInt(Form2.CheckStringGrid.Cells[0, SubRowIndex]);
        if PreIndex>Nowindex then
        begin
          ValueCopyStringListTo(PreStrings, Form2.CheckStringGrid.Rows[SubRowIndex-1]);
          ValueCopyStringListTo(Form2.CheckStringGrid.Rows[SubRowIndex-1], Form2.CheckStringGrid.Rows[SubRowIndex]);
          ValueCopyStringListTo(Form2.CheckStringGrid.Rows[SubRowIndex], PreStrings);
        end;
      end;
    end;
  finally
    PreStrings.Free;
  end;
end;

end.

