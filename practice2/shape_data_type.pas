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

  function ReadFileIntoObjecList(AFileName:String; AObjectList:TObjectList; const ExpandValue: Integer):Boolean;
  function IsStringListNum(AStringList:TStringList):Boolean;
  procedure ClearEmptyStringElement(AStringList:TStringList);

implementation

function ReadFileIntoObjecList(AFileName: String; AObjectList: TObjectList; const ExpandValue: Integer): Boolean;
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
      Result:=True;
    finally
      CloseFile(FileHandle);
    end;
  except
    ShowMessage('Error, cannot open the file name you assign');
    Result:=False;
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

