unit shape_data_type;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Dialogs, tools;

type
  { TShapeType }

  TShapeType = (None, PointType, LineType);

  { TShape }

  TShape=class
    private
      NName:TShapeType;

      function GetName:TShapeType;
      procedure SetName(AName:TShapeType);
    public
      property Name:TShapeType read GetName write SetName;

      constructor create(AName:TShapeType);
  end;

  { TPointShape }

  TPointShape=class(TShape){change name}
    private
      NPoint:TPoint;{change to use TPoint}
      NRadius:Integer;

      function GetPoint:TPoint;
      procedure SetPoint(APoint:TPoint);
      function GetRadius:Integer;
      procedure SetRadius(ARadius:Integer);
    public
      property Point:TPoint read GetPoint write SetPoint;
      property Radius:Integer read GetRadius write SetRadius;

      constructor create(AName:TShapeType; APoint:TPoint; ARadius:Integer);overload;
  end;

  { TLineShape }

  TLineShape=class(TShape)
    private
      NStartPoint:TPoint;
      NEndPoint:TPoint;
      NRadius:Integer;

      function GetStartPoint:TPoint;
      procedure SetStartPoint(APoint:TPoint);
      function GetEndPoint:TPoint;
      procedure SetEndPoint(APoint:TPoint);
      function GetRadius:Integer;
      procedure SetRadius(ARadius:Integer);
    public
      property StartPoint:TPoint read GetStartPoint write SetStartPoint;
      property EndPoint:TPoint read GetEndPoint write SetEndPoint;
      property Radius:Integer read GetRadius write SetRadius;

      constructor create(AName:TShapeType; AStartPoint, AEndPoint:TPoint; ARadius:Integer);
  end;

  procedure ReadFileIntoObjecList(AFileName:String; AObjectList:TObjectList; const ExpandValue: Integer);
  procedure WriteObjectListIntoFile(AFileName:String; AObjectList:TObjectList; const ExpandValue: Integer);
  procedure ClearEmptyStringElement(AStringList:TStringList);
  procedure CloneShapeObjectListTo(ATargetShapeObjectList, ASourceShapeObjectList: TObjectList);
  function IsStringListNum(AStringList:TStringList):Boolean;

var
  ShapeObjectList:TObjectList;

implementation

procedure ReadFileIntoObjecList(AFileName: String; AObjectList: TObjectList; const ExpandValue: Integer);
const
  PointName='#P';
  LineName='#L';
var
  FileHandle:TextFile;
  TempString:String;
  TempStringList:TStringList;
  ShapeObject:TShape;
  ShapeName:TShapeType;
  ShapeRadius:LongInt;
  Point1, Point2:TPoint;
  Index:integer;
begin
  try
    AssignFile(FileHandle, AFileName);
    Reset(FileHandle);
    AObjectList.Clear;
    try
      while not EOF(FileHandle) do
      begin
        ReadLn(FileHandle, TempString);
        ShapeName:=None;
        TempStringList:=TStringList.Create;
        try
          if Length(TempString)<>0 then
          begin
            SplitString(TempStringList, TempString, ' ');
            ClearEmptyStringElement(TempStringList);
            if TempStringList[0]=PointName then ShapeName:=PointType
            else if TempStringList[0]=LineName then ShapeName:=LineType
            else ShapeName:=None;

            TempStringList.Delete(0);
            ShapeRadius:=Trunc(StrToFloat(TempStringList[TempStringList.count-1])*ExpandValue);

            if (TempStringList.Count>=2) and IsStringListNum(TempStringList) then
            begin
              Point1.X:=Trunc(StrToFloat(TempStringList[0])*ExpandValue);
              Point1.Y:=Trunc(StrToFloat(TempStringList[1])*ExpandValue);

              if (TempStringList.Count=3) and (ShapeName=PointType) then
                ShapeObject:=TPointShape.create(ShapeName, Point1, ShapeRadius)
              else if (TempStringList.Count=5) and (ShapeName=LineType) then
              begin
                Point2.X:=Trunc(StrToFloat(TempStringList[2])*ExpandValue);
                Point2.Y:=Trunc(StrToFloat(TempStringList[3])*ExpandValue);
                ShapeObject:=TLineShape.create(ShapeName, Point1, Point2, ShapeRadius);
              end;
            end;

            AObjectList.Add(ShapeObject);
          end;
        finally
          TempStringList.Free;
        end;
      end;
    finally
      CloseFile(FileHandle);
    end;
  except
    ShowMessage('Error, cannot open the file name you assign');
  end;
end;

procedure CloneShapeObjectListTo(ATargetShapeObjectList,
  ASourceShapeObjectList: TObjectList);
var
  TempPointObject: TPointShape;
  TempLineObject: TLineShape;
  TempShapeName: TShapeType;
  TempPoint1, TempPoint2: TPoint;
  TempRadius: Integer;
  Index: Integer;
begin
  ATargetShapeObjectList.Clear;

  for Index:=0 to (ASourceShapeObjectList.Count-1) do
  begin
    if ASourceShapeObjectList[Index] is TPointShape then
    begin
      TempShapeName:=PointType;
      TempPoint1:=TPoint.Create(TPointShape(ASourceShapeObjectList[Index]).Point.x, TPointShape(ASourceShapeObjectList[Index]).Point.y);
      TempRadius:=TPointShape(ASourceShapeObjectList[Index]).Radius;
      TempPointObject:=TPointShape.create(TempShapeName, TempPoint1, TempRadius);
      ATargetShapeObjectList.Add(TempPointObject);
    end
    else
    if ASourceShapeObjectList[Index] is TLineShape then
    begin
      TempShapeName:=LineType;
      TempPoint1:=TPoint.Create(TLineShape(ASourceShapeObjectList[Index]).StartPoint.x, TLineShape(ASourceShapeObjectList[Index]).StartPoint.y);
      TempPoint2:=TPoint.Create(TLineShape(ASourceShapeObjectList[Index]).EndPoint.y, TLineShape(ASourceShapeObjectList[Index]).EndPoint.y);
      TempRadius:=TLineShape(ASourceShapeObjectList[Index]).Radius;
      TempLineObject:=TLineShape.create(TempShapeName, TempPoint1, TempPoint2, TempRadius);
      ATargetShapeObjectList.Add(TempLineObject);
    end;
  end;
end;

function IsStringListNum(AStringList: TStringList): Boolean;
var
  Index:Integer;
  TempNum:Extended;
begin
  try
    for Index:=0 to (AStringList.Count-1) do
    begin
      TempNum:=StrToFloat(AStringList[Index]);
    end;
    Result:=True;
  except
    Result:=False;
  end;
end;

procedure WriteObjectListIntoFile(AFileName: String; AObjectList: TObjectList;
  const ExpandValue: Integer);
const
  PointName='#P';
  LineName='#L';
var
  FileHandler:TextFile;
  Index:Integer;
  TempPointObject:TPointShape;
  TempLineObject:TLineShape;
begin
  try
    AssignFile(FileHandler, AFileName);
  except
    Exit;
  end;

  try
    Rewrite(FileHandler);
    for Index:=0 to (AObjectList.Count-1) do
    begin
      if AObjectList[Index] is TPointShape then
      begin
        TempPointObject:=TPointShape(AObjectList[Index]);
        Write(FileHandler, PointName + ' ');
        Write(FileHandler, FloatToStr(TempPointObject.Point.x / ExpandValue) + ' ');
        Write(FileHandler, FloatToStr(TempPointObject.Point.y / ExpandValue) + ' ');
        Write(FileHandler, FloatToStr(TempPointObject.Radius / ExpandValue));
        WriteLn(FileHandler);
      end
      else if AObjectList[Index] is TLineShape then
      begin
        TempLineObject:=TLineShape(AObjectList[Index]);
        Write(FileHandler, LineName + ' ');
        Write(FileHandler, FloatToStr(TempLineObject.StartPoint.x / ExpandValue) + ' ');
        Write(FileHandler, FloatToStr(TempLineObject.StartPoint.y / ExpandValue) + ' ');
        Write(FileHandler, FloatToStr(TempLineObject.EndPoint.x / ExpandValue) + ' ');
        Write(FileHandler, FloatToStr(TempLineObject.EndPoint.y / ExpandValue) + ' ');
        Write(FileHandler, FloatToStr(TempPointObject.Radius / ExpandValue));
        WriteLn(FileHandler);
      end;
    end;
  finally
    CloseFile(FileHandler);
  end;
end;

procedure ClearEmptyStringElement(AStringList: TStringList);
var
  Index:Integer;
begin
  for Index:=(AStringList.count-1) downto 0 do
  begin
    if AStringList[Index]='' then AStringList.Delete(Index);
  end;
end;

{ TLineShape }

function TLineShape.GetStartPoint: TPoint;
begin
  Result:=NStartPoint;
end;

procedure TLineShape.SetStartPoint(APoint: TPoint);
begin
  NStartPoint:=APoint;
end;

function TLineShape.GetEndPoint: TPoint;
begin
  Result:=NEndPoint;
end;

procedure TLineShape.SetEndPoint(APoint: TPoint);
begin
  NEndPoint:=APoint;
end;

function TLineShape.GetRadius: Integer;
begin
  Result:=NRadius;
end;

procedure TLineShape.SetRadius(ARadius: Integer);
begin
  NRadius:=ARadius;
end;

constructor TLineShape.create(AName: TShapeType; AStartPoint,
  AEndPoint: TPoint; ARadius: Integer);
begin
  Inherited create(AName);
  SetStartPoint(AStartPoint);
  SetEndPoint(AEndPoint);
  SetRadius(ARadius);
end;

{ TPointShape }

function TPointShape.GetPoint: TPoint;
begin
  Result:=NPoint;
end;

procedure TPointShape.SetPoint(APoint: TPoint);
begin
  NPoint:=APoint;
end;

function TPointShape.GetRadius: Integer;
begin
  Result:=NRadius;
end;

procedure TPointShape.SetRadius(ARadius: Integer);
begin
  NRadius:=ARadius;
end;

constructor TPointShape.create(AName: TShapeType; APoint: TPoint;
  ARadius: Integer);
begin
  Inherited create(AName);
  SetPoint(APoint);
  SetRadius(ARadius);
end;

{ TShape }

function TShape.GetName: TShapeType;
begin
  Result:=NName;
end;

procedure TShape.SetName(AName: TShapeType);
begin
  NName:=AName;
end;

constructor TShape.create(AName: TShapeType);
begin
  SetName(AName);
end;

end.

