unit shape_data_type;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Contnrs, Dialogs, tools;

type

  { TPointCoordinate }

  TPointCoordinate=record
    X:Integer;
    Y:Integer;
  end;

  { TShape }

  TShape=class
    private
      NName:String;

      function GetName:String;
      procedure SetName(AName:String);
    public
      property Name:String read GetName write SetName;

      constructor create(AName:String);
  end;

  { TPoint }

  TPoint=class(TShape)
    private
      NPoint:TPointCoordinate;
      NRadius:Integer;

      function GetPoint:TPointCoordinate;
      procedure SetPoint(APoint:TPointCoordinate);
      function GetRadius:Integer;
      procedure SetRadius(ARadius:Integer);
    public
      property Point:TPointCoordinate read GetPoint write SetPoint;
      property Radius:Integer read GetRadius write SetRadius;

      constructor create(AName:String; APoint:TPointCoordinate; ARadius:Integer);overload;
  end;

  { TLine }

  TLine=class(TShape)
    private
      NPoint:TPointCoordinate;
      NPoint2:TPointCoordinate;
      NRadius:Integer;

      function GetPoint:TPointCoordinate;
      procedure SetPoint(APoint:TPointCoordinate);
      function GetPoint2:TPointCoordinate;
      procedure SetPoint2(APoint:TPointCoordinate);
      function GetRadius:Integer;
      procedure SetRadius(ARadius:Integer);
    public
      property Point:TPointCoordinate read GetPoint write SetPoint;
      property Point2:TPointCoordinate read GetPoint2 write SetPoint2;
      property Radius:Integer read GetRadius write SetRadius;

      constructor create(AName:String; APoint, APoint2:TPointCoordinate; ARadius:Integer);
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
  Name:String;
  TempStringList:TStringList;
  ShapeObject:TShape;
  Radius:LongInt;
  Point1, Point2:TPointCoordinate;
  Index:integer;
begin
  try
    AssignFile(FileHandle, AFileName);
    Reset(FileHandle);

    while not EOF(FileHandle) do
    begin
      ReadLn(FileHandle, TempString);
      Name:='';
      TempStringList:=TStringList.Create;

      try
        if Length(TempString)<>0 then
        begin
          SplitString(TempStringList, TempString, ' ');
          ClearEmptyStringElement(TempStringList);
          Name:=TempStringList[0];
          TempStringList.Delete(0);

          //for Index:=0 to TempStringList.count-1 do ShowMessage(TempStringList[Index]);
          Radius:=Trunc(StrToFloat(TempStringList[TempStringList.count-1])*ExpandValue);

          if (TempStringList.Count>=2) and IsStringListNum(TempStringList) then
          begin
            Point1.X:=Trunc(StrToFloat(TempStringList[0])*ExpandValue);
            Point1.Y:=Trunc(StrToFloat(TempStringList[1])*ExpandValue);

            if (TempStringList.Count=3) and (Name=PointName) then
              ShapeObject:=TPoint.create(Name, Point1, Radius)
            else if (TempStringList.Count=5) and (Name=LineName) then
            begin
              Point2.X:=Trunc(StrToFloat(TempStringList[2])*ExpandValue);
              Point2.Y:=Trunc(StrToFloat(TempStringList[3])*ExpandValue);

              ShapeObject:=TLine.create(Name, Point1, Point2, Radius);
            end;
          end;

          AObjectList.Add(ShapeObject);
        end;
      finally
        TempStringList.Free;
      end;
    end;
    Result:=True;
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

{ TLine }

function TLine.GetPoint: TPointCoordinate;
begin
  Result:=NPoint;
end;

procedure TLine.SetPoint(APoint: TPointCoordinate);
begin
  NPoint:=APoint;
end;

function TLine.GetPoint2: TPointCoordinate;
begin
  Result:=NPoint2;
end;

procedure TLine.SetPoint2(APoint: TPointCoordinate);
begin
  NPoint2:=APoint;
end;

function TLine.GetRadius: Integer;
begin
  Result:=NRadius;
end;

procedure TLine.SetRadius(ARadius: Integer);
begin
  NRadius:=ARadius;
end;

constructor TLine.create(AName: String; APoint, APoint2: TPointCoordinate;
  ARadius: Integer);
begin
  inherited create(AName);
  SetPoint(APoint);
  SetPoint2(APoint2);
  SetRadius(ARadius);
end;

{ TPoint }

function TPoint.GetPoint: TPointCoordinate;
begin
  Result:=NPoint;
end;

procedure TPoint.SetPoint(APoint: TPointCoordinate);
begin
  NPoint:=APoint;
end;

function TPoint.GetRadius: Integer;
begin
  Result:=NRadius;
end;

procedure TPoint.SetRadius(ARadius: Integer);
begin
  NRadius:=ARadius;
end;

constructor TPoint.create(AName: String; APoint: TPointCoordinate;
  ARadius: Integer);
begin
  Inherited create(AName);
  SetPoint(APoint);
  SetRadius(ARadius);
end;

{ TShape }

function TShape.GetName: String;
begin
  Result:=NName;
end;

procedure TShape.SetName(AName: String);
begin
  NName:=AName;
end;

constructor TShape.create(AName: String);
begin
  SetName(AName);
end;

end.

