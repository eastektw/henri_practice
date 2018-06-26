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
  TPointShape=class(TShape)
    private
      NPoint:TPoint;
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
  function TwoShapeObjectSame(AFirstShapeObject, ASecondShapeObject: TShape
  ): Boolean;

implementation

function TwoShapeObjectSame(AFirstShapeObject, ASecondShapeObject: TShape
  ): Boolean;
var
   FirstPointObject, SecondPointObject: TPointShape;
   FirstLineObject, SecondLineObject: TLineShape;
begin
  Result:=False;
  if AFirstShapeObject.ClassType<>ASecondShapeObject.ClassType then
    Result:=False
  else
  begin
    if AFirstShapeObject.Name<>ASecondShapeObject.Name then
    begin
      Result:=False;
      Exit;
    end;

    if AFirstShapeObject is TPointShape then
    begin
      FirstPointObject:= AFirstShapeObject as TPointShape;
      SecondPointObject:=ASecondShapeObject as TPointShape;
      if PointsEqual(FirstPointObject.Point, SecondPointObject.Point) and
      (FirstPointObject.Radius=SecondPointObject.Radius) then
        Result:=True;
    end
    else if AFirstShapeObject is TLineShape then
    begin
      FirstLineObject:=AFirstShapeObject as TLineShape;
      SecondLineObject:=ASecondShapeObject as TLineShape;
      if PointsEqual(FirstLineObject.StartPoint, SecondLineObject.StartPoint) and
      PointsEqual(FirstLineObject.EndPoint, SecondLineObject.EndPoint) and
      (FirstLineObject.Radius=SecondLineObject.Radius) then
        Result:=True;
    end;
  end;
end;

procedure ReadFileIntoObjecList(AFileName: String; AObjectList: TObjectList; const ExpandValue: Integer);
var
  FileHandler:TextFile;
  TempString:String;
  TempStringList:TStringList;
  ShapeName:String;
  ShapeType:TShapeType;
  ShapeRadius:Integer;
  Point1, Point2:TPoint;
  ShapeObject:TShape;
begin
  if not FileExists(AFileName) then
  begin
    ShowMessage('cannot find the file.');
    Exit;
  end;

  AssignFile(FileHandler, AFileName);
  TempString:='';
  TempStringList:=TStringList.Create;
  ShapeName:='';
  ShapeType:=None;
  ShapeRadius:=0;

  Reset(FileHandler);
  try
    try
      while not EOF(FileHandler) do
      begin
        ReadLn(FileHandler, TempString);

        if Length(TempString)=0 then
          continue;

        SplitString(TempStringList, TempString, ' ');
        ClearEmptyStringElement(TempStringList);

        ShapeName:=TempStringList[0];
        if ShapeName='#P' then
          ShapeType:=PointType
        else if ShapeName='#L' then
          ShapeType:=LineType
        else
          ShapeType:=None;

        RemoveStringFrom(TempStringList, ShapeName);
        if not IsStringListNum(TempStringList) then
           Exit;

        if (TempStringList.Count=3) and (ShapeType=PointType) then
        begin
          Point1.X:=Trunc(StrToFloat(TempStringList[0])*ExpandValue);
          Point1.Y:=Trunc(StrToFloat(TempStringList[1])*ExpandValue);
          ShapeRadius:=Trunc(StrToFloat(TempStringList[2])*ExpandValue);
          ShapeObject:=TPointShape.create(ShapeType, Point1, ShapeRadius);
          AObjectList.Add(ShapeObject);
        end
        else if (TempStringList.Count=5) and (ShapeType=LineType) then
        begin
          Point1.X:=Trunc(StrToFloat(TempStringList[0])*ExpandValue);
          Point1.Y:=Trunc(StrToFloat(TempStringList[1])*ExpandValue);
          Point2.X:=Trunc(StrToFloat(TempStringList[2])*ExpandValue);
          Point2.Y:=Trunc(StrToFloat(TempStringList[3])*ExpandValue);
          ShapeRadius:=Trunc(StrToFloat(TempStringList[4])*ExpandValue);
          ShapeObject:=TLineShape.create(ShapeType, Point1, Point2, ShapeRadius);
          AObjectList.Add(ShapeObject);
        end;
      end;
    except
      TempStringList.Free;
    end;
  finally
    CloseFile(FileHandler);
  end;
end;

procedure CloneShapeObjectListTo(ATargetShapeObjectList,
  ASourceShapeObjectList: TObjectList);
var
  TempShapeType: TShapeType;
  TempPoint1, TempPoint2: TPoint;
  SourcePointObject, TargetPointObject: TPointShape;
  SourceLineObject, TargetLineObject: TLineShape;
  TempRadius: Integer;
  Index: Integer;
begin
  ATargetShapeObjectList.Clear;
  TempShapeType:=None;
  TempPoint1:=Point(0,0);
  TempPoint2:=Point(0,0);
  TempRadius:=0;

  for Index:=0 to (ASourceShapeObjectList.Count-1) do
  begin
    if ASourceShapeObjectList[Index] is TPointShape then
    begin
      SourcePointObject:=ASourceShapeObjectList[Index] as TPointShape;

      TempShapeType:=PointType;
      TempPoint1:=Point(SourcePointObject.Point.x, SourcePointObject.Point.y);
      TempRadius:=SourcePointObject.Radius;
      TargetPointObject:=TPointShape.create(TempShapeType, TempPoint1, TempRadius);
      ATargetShapeObjectList.Add(TargetPointObject);
    end
    else
    if ASourceShapeObjectList[Index] is TLineShape then
    begin
      SourceLineObject:=ASourceShapeObjectList[Index] as TLineShape;

      TempShapeType:=LineType;
      TempPoint1:= Point(SourceLineObject.StartPoint.x, SourceLineObject.StartPoint.y);
      TempPoint2:= Point(SourceLineObject.EndPoint.x, SourceLineObject.EndPoint.y);
      TempRadius:=SourceLineObject.Radius;
      TargetLineObject:=TLineShape.create(TempShapeType, TempPoint1, TempPoint2, TempRadius);
      ATargetShapeObjectList.Add(TargetLineObject);
    end;
  end;
end;

function IsStringListNum(AStringList: TStringList): Boolean;
var
  Index:Integer;
  TestNum: Extended;
begin
  Result:= True;

  if AStringList.Count=0 then
  begin
    Result:=False;
    Exit;
  end;

  for Index:=0 to (AStringList.Count-1) do
  begin
    if not TryStrToFloat(AStringList[Index], TestNum) then
    begin
       result:= False;
       Exit;
    end;
  end;
end;

procedure WriteObjectListIntoFile(AFileName: String; AObjectList: TObjectList;
  const ExpandValue: Integer);
var
  FileHandler:TextFile;
  Index:Integer;
  TempPointObject:TPointShape;
  TempLineObject:TLineShape;
  PointName, LineName:String;
begin
  if FileExists(AFileName) then
  begin
     ShowMessage('File has been existed, please specify the other file name.');
     Exit;
  end;

  AssignFile(FileHandler, AFileName);
  PointName:='#P';
  LineName:='#L';
  Rewrite(FileHandler);

  try
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
  NStartPoint:=AStartPoint;
  NEndPoint:=AEndPoint;
  NRadius:=ARadius;
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
  NPoint:=APoint;
  NRadius:=ARadius;
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
  NName:=AName;
end;

end.

