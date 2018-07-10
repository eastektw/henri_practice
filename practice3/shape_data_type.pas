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
      NModified:Boolean;

      function GetName:TShapeType;
      procedure SetName(AName:TShapeType);
      function GetModified:Boolean;
      procedure SetModified(AModified:Boolean=False);
    public
      property Name:TShapeType read GetName write SetName;
      property Modified: Boolean read GetModified write SetModified;

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


procedure ClearEmptyStringElement(AStringList:TStringList);
procedure CreateShapeObjectToObjectList(AObjectList: TObjectList; AShapeType: TShapeType; ANumStringList: TStringList; const ExpandValue: Integer);
procedure ReadFileIntoObjectList(AFileName:String; AObjectList:TObjectList; const ExpandValue: Integer);
procedure WriteShapeObjectListIntoFile(AFileName:String; AShapeObjectList:TObjectList; const ExpandValue: Integer);
function IsStringListNum(AStringList:TStringList):Boolean;

implementation

procedure ClearEmptyStringElement(AStringList: TStringList);
var
  Index:Integer;
begin
  for Index:=(AStringList.count-1) downto 0 do
  begin
    if AStringList[Index]='' then AStringList.Delete(Index);
  end;
end;

procedure CreateShapeObjectToObjectList(
  AObjectList: TObjectList; AShapeType: TShapeType;
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
      AObjectList.Add(ShapeObject);
    end
    else if (ANumStringList.Count=5) and (AShapeType=LineType) then
    begin
      Point1.X:=Trunc(StrToFloat(ANumStringList[0])*ExpandValue);
      Point1.Y:=Trunc(StrToFloat(ANumStringList[1])*ExpandValue);
      Point2.X:=Trunc(StrToFloat(ANumStringList[2])*ExpandValue);
      Point2.Y:=Trunc(StrToFloat(ANumStringList[3])*ExpandValue);
      ShapeRadius:=Trunc(StrToFloat(ANumStringList[4])*ExpandValue);
      ShapeObject:=TLineShape.create(AShapeType, Point1, Point2, ShapeRadius);
      AObjectList.Add(ShapeObject);
    end
    else
    begin
    end;
  end;
end;

procedure ReadFileIntoObjectList(AFileName: String;
  AObjectList: TObjectList; const ExpandValue: Integer);
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

  AObjectList.Clear;
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
         CreateShapeObjectToObjectList(AObjectList, ShapeType, TempStringList, ExpandValue);
    end;
  finally
    TempStringList.Free;
    CloseFile(FileHandler);
  end;
end;

procedure WriteShapeObjectListIntoFile(AFileName: String;
  AShapeObjectList: TObjectList; const ExpandValue: Integer);
const
  PointName='#P';
  LineName='#L';
var
  FileHandler:TextFile;
  Index:Integer;
  TempShapeObject: TShape;
  TempPointObject:TPointShape;
  TempLineObject:TLineShape;
begin
  if FileExists(AFileName) then
  begin
     ShowMessage('File has been existed, please specify another file name.');
     Exit;
  end;

  AssignFile(FileHandler, AFileName);

  Rewrite(FileHandler);
  try
    for Index:=0 to (AShapeObjectList.Count-1) do
    begin
      TempShapeObject:=AShapeObjectList[Index] as TShape;
      if TempShapeObject.Name=PointType then
      begin
        TempPointObject:=AShapeObjectList[Index] as TPointShape;
        Write(FileHandler, PointName + ' ');
        Write(FileHandler, FloatToStr(TempPointObject.Point.x / ExpandValue) + ' ');
        Write(FileHandler, FloatToStr(TempPointObject.Point.y / ExpandValue) + ' ');
        Write(FileHandler, FloatToStr(TempPointObject.Radius / ExpandValue));
        WriteLn(FileHandler);
      end
      else if TempShapeObject.Name=LineType then
      begin
        TempLineObject:=AShapeObjectList[Index] as TLineShape;
        Write(FileHandler, LineName + ' ');
        Write(FileHandler, FloatToStr(TempLineObject.StartPoint.x / ExpandValue) + ' ');
        Write(FileHandler, FloatToStr(TempLineObject.StartPoint.y / ExpandValue) + ' ');
        Write(FileHandler, FloatToStr(TempLineObject.EndPoint.x / ExpandValue) + ' ');
        Write(FileHandler, FloatToStr(TempLineObject.EndPoint.y / ExpandValue) + ' ');
        Write(FileHandler, FloatToStr(TempLineObject.Radius / ExpandValue));
        WriteLn(FileHandler);
      end;
    end;
  finally
    CloseFile(FileHandler);
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

function TShape.GetModified: Boolean;
begin
  Result:=NModified;
end;

procedure TShape.SetModified(AModified: Boolean);
begin
  NModified:=AModified;
end;

constructor TShape.create(AName: TShapeType);
begin
  NName:=AName;
end;

end.

