unit shape_data_type;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Dialogs, math, tools;

type
  { TShapeType }
  TShapeType = (None, PointType, LineType);

  { TShape }
  TShape=class(TObject)
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

      constructor create(AName:TShapeType; APoint:TPoint; ARadius:Integer);
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

  { TChangedShapeObject }

  TChangedShapeObject=class(TObject)
    private
      FChangedShapeObject:TShape;
      FOriginIndex:Integer;
      //isDelete: Boolean;

      function GetChangedShapeObject: TShape;
      procedure SetChangedShapeObject(AShapeObject: TShape);
      function GetOriginIndex: Integer;
      procedure SetOriginIndex(AOriginIndex: Integer);
    public
      property ChangedShapeObject:TShape read GetChangedShapeObject write SetChangedShapeObject;
      property OriginIndex:Integer read GetOriginIndex write SetOriginIndex;

      constructor create(AShapeObject: TShape; AOriginIndex: Integer);
  end;

{ container type }
type

  { TChangedShapeObjectList }

  TChangedShapeObjectList=class(TObjectList)
    private
      function GetChangedShapeObject(Index: Integer):TChangedShapeObject;
      procedure SetChangedShapeObject(Index: Integer; AChangedShapeObject: TChangedShapeObject);
    public
      function AddChangedShapeObject(AChangedShapeObject: TChangedShapeObject): Integer;
      function HasOriginIndex(AOriginIndex: Integer): Boolean;
      Procedure DeleteChangedShapeObject(Index: Integer);
      procedure InsertChangedShapeObject(Index: Integer; AChangedShapeObject: TChangedShapeObject);
      procedure SortByOriginIndex;
      procedure Free;
      procedure Clear;
      property ChangedShapeObjects[Index: Integer]: TChangedShapeObject read GetChangedShapeObject write SetChangedShapeObject; default;
  end;

  { TOriginShapeObjectList }

  TOriginShapeObjectList=class(TObjectList)
    private
      function GetOriginShapeObject(Index: Integer):TShape;
      procedure SetOriginShapeObject(Index: Integer; AOriginShapeObject: TShape);
    public
      function AddOriginShapeObject(AOriginShapeObject: TShape): Integer;
      procedure InsertOriginShapeObject(Index: Integer; AOriginShapeObject: TShape);
      property OriginShapeObjects[Index: Integer]: TShape read GetOriginShapeObject write SetOriginShapeObject; default;
  end;

procedure CreateShapeObjectToOriginShapeObjectList(AOriginShapeObjectList: TOriginShapeObjectList; AShapeType: TShapeType; ANumStringList: TStringList; const ExpandValue: Integer);
procedure ReadFileIntoOriginShapeObjecList(AFileName:String; AOriginShapeObjectList:TOriginShapeObjectList; const ExpandValue: Integer);
procedure WriteOriginShapeObjectListIntoFile(AFileName:String; AOriginObjectList:TOriginShapeObjectList; const ExpandValue: Integer);
procedure ClearEmptyStringElement(AStringList:TStringList);
function IsStringListNum(AStringList:TStringList):Boolean;
function TwoShapeObjectSame(AFirstShapeObject, ASecondShapeObject: TShape
): Boolean;
function CompareChangedShapeObjectOriginIndex(Item1, Item2: Pointer): Integer;

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
      Result:=False
    else
    begin
      if AFirstShapeObject.Name=PointType then
      begin
        FirstPointObject:=AFirstShapeObject as TPointShape;
        SecondPointObject:=ASecondShapeObject as TPointShape;
        if PointsEqual(FirstPointObject.Point, SecondPointObject.Point) and
        (FirstPointObject.Radius=SecondPointObject.Radius) then
          Result:=True;
      end
      else if AFirstShapeObject.Name=LineType then
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
end;

function CompareChangedShapeObjectOriginIndex(Item1, Item2: Pointer): Integer;
var
  OriginIndex1, OriginIndex2: Integer;
begin
  OriginIndex1:=TChangedShapeObject(Item1).OriginIndex;
  OriginIndex2:=TChangedShapeObject(Item2).OriginIndex;
  Result:=CompareValue(OriginIndex1, OriginIndex2);
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

procedure CreateShapeObjectToOriginShapeObjectList(
  AOriginShapeObjectList: TOriginShapeObjectList; AShapeType: TShapeType;
  ANumStringList: TStringList; const ExpandValue: Integer);
var
   Point1, Point2:TPoint;
   ShapeRadius:Integer;
   ShapeObject: TShape;
begin
  ShapeRadius:=0;

  if IsStringListNum(ANumStringList) then
  begin
    if (ANumStringList.Count=3) and (AShapeType=PointType) then
    begin
      Point1.X:=Trunc(StrToFloat(ANumStringList[0])*ExpandValue);
      Point1.Y:=Trunc(StrToFloat(ANumStringList[1])*ExpandValue);
      ShapeRadius:=Trunc(StrToFloat(ANumStringList[2])*ExpandValue);
      ShapeObject:=TPointShape.create(PointType, Point1, ShapeRadius);
      AOriginShapeObjectList.AddOriginShapeObject(ShapeObject);
    end
    else if (ANumStringList.Count=5) and (AShapeType=LineType) then
    begin
      Point1.X:=Trunc(StrToFloat(ANumStringList[0])*ExpandValue);
      Point1.Y:=Trunc(StrToFloat(ANumStringList[1])*ExpandValue);
      Point2.X:=Trunc(StrToFloat(ANumStringList[2])*ExpandValue);
      Point2.Y:=Trunc(StrToFloat(ANumStringList[3])*ExpandValue);
      ShapeRadius:=Trunc(StrToFloat(ANumStringList[4])*ExpandValue);
      ShapeObject:=TLineShape.create(AShapeType, Point1, Point2, ShapeRadius);
      AOriginShapeObjectList.AddOriginShapeObject(ShapeObject);
    end
    else
    begin
    end;
  end;
end;

procedure ReadFileIntoOriginShapeObjecList(AFileName: String;
  AOriginShapeObjectList: TOriginShapeObjectList; const ExpandValue: Integer);
var
  FileHandler:TextFile;
  TempString:String;
  TempStringList:TStringList;
  ShapeName:String;
  ShapeType:TShapeType;
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

  Reset(FileHandler);
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
      if IsStringListNum(TempStringList) then
         CreateShapeObjectToOriginShapeObjectList(AOriginShapeObjectList, ShapeType, TempStringList, ExpandValue);
    end;
  finally
    TempStringList.Free;
    CloseFile(FileHandler);
  end;
end;

procedure WriteOriginShapeObjectListIntoFile(AFileName: String;
  AOriginObjectList: TOriginShapeObjectList; const ExpandValue: Integer);
var
  FileHandler:TextFile;
  Index:Integer;
  TempPointObject:TPointShape;
  TempLineObject:TLineShape;
  PointName, LineName:String;
begin
  if FileExists(AFileName) then
  begin
     ShowMessage('File has been existed, please specify another file name.');
     Exit;
  end;

  AssignFile(FileHandler, AFileName);
  PointName:='#P';
  LineName:='#L';

  Rewrite(FileHandler);
  try
    for Index:=0 to (AOriginObjectList.Count-1) do
    begin
      if AOriginObjectList[Index].Name=PointType then
      begin
        TempPointObject:=AOriginObjectList[Index] as TPointShape;
        Write(FileHandler, PointName + ' ');
        Write(FileHandler, FloatToStr(TempPointObject.Point.x / ExpandValue) + ' ');
        Write(FileHandler, FloatToStr(TempPointObject.Point.y / ExpandValue) + ' ');
        Write(FileHandler, FloatToStr(TempPointObject.Radius / ExpandValue));
        WriteLn(FileHandler);
      end
      else if AOriginObjectList[Index].Name=LineType then
      begin
        TempLineObject:=AOriginObjectList[Index] as TLineShape;
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

{ TOriginShapeObjectList }

function TOriginShapeObjectList.GetOriginShapeObject(Index: Integer): TShape;
begin
  Result:=TShape(Self.GetItem(Index))
end;

procedure TOriginShapeObjectList.SetOriginShapeObject(Index: Integer;
  AOriginShapeObject: TShape);
begin
  Self.SetItem(Index, AOriginShapeObject);
end;

function TOriginShapeObjectList.AddOriginShapeObject(AOriginShapeObject: TShape
  ): Integer;
begin
  Result:=Self.Add(AOriginShapeObject);
end;

procedure TOriginShapeObjectList.InsertOriginShapeObject(Index: Integer;
  AOriginShapeObject: TShape);
begin
  Self.Insert(Index, AOriginShapeObject);
end;

{ TChangedShapeObjectList }

function TChangedShapeObjectList.GetChangedShapeObject(Index: Integer
  ): TChangedShapeObject;
begin
  Result:=TChangedShapeObject(GetItem(Index));
end;

procedure TChangedShapeObjectList.SetChangedShapeObject(Index: Integer;
  AChangedShapeObject: TChangedShapeObject);
begin
  Self.SetItem(Index, AChangedShapeObject);
end;

function TChangedShapeObjectList.AddChangedShapeObject(
  AChangedShapeObject: TChangedShapeObject): Integer;
begin
  Result:=Self.Add(AChangedShapeObject);
end;

function TChangedShapeObjectList.HasOriginIndex(AOriginIndex: Integer): Boolean;
var
  Index:Integer;
begin
  Result:=False;
  for Index:=0 to (Self.Count-1) do
  begin
    if ChangedShapeObjects[Index].OriginIndex=AOriginIndex then
    begin
      Result:=True;
      Exit;
    end;
  end;
end;

procedure TChangedShapeObjectList.DeleteChangedShapeObject(Index: Integer);
begin
  ChangedShapeObjects[Index].ChangedShapeObject.Free;
  Delete(Index);
end;

procedure TChangedShapeObjectList.InsertChangedShapeObject(Index: Integer;
  AChangedShapeObject: TChangedShapeObject);
begin
  Self.Insert(Index, AChangedShapeObject);
end;

procedure TChangedShapeObjectList.SortByOriginIndex;
begin
  Self.Sort(@CompareChangedShapeObjectOriginIndex);
end;

procedure TChangedShapeObjectList.Free;
begin
  Self.Clear;
  inherited Free;
end;

procedure TChangedShapeObjectList.Clear;
var
  Index: Integer;
begin
  for Index:=(Self.Count-1) downto 0 do
    DeleteChangedShapeObject(Index);
end;

{ TChangedShapeObject }

function TChangedShapeObject.GetChangedShapeObject: TShape;
begin
  Result:=FChangedShapeObject;
end;

procedure TChangedShapeObject.SetChangedShapeObject(AShapeObject: TShape);
begin
  FChangedShapeObject:=AShapeObject;
end;

function TChangedShapeObject.GetOriginIndex: Integer;
begin
  Result:=FOriginIndex;
end;

procedure TChangedShapeObject.SetOriginIndex(AOriginIndex: Integer);
begin
  FOriginIndex:=AOriginIndex;
end;

constructor TChangedShapeObject.create(AShapeObject: TShape;
  AOriginIndex: Integer);
begin
  FChangedShapeObject:=AShapeObject;
  FOriginIndex:=AOriginIndex;
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

