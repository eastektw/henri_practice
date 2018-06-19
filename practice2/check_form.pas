unit check_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Grids, StdCtrls, Types;

type

  { TForm2 }

  TForm2 = class(TForm)
    CheckButton: TButton;
    StringGridPanel: TPanel;
    ButtonPanel: TPanel;
    CheckStringGrid: TStringGrid;
    procedure CheckStringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
    procedure FormResize(Sender: TObject);
  private

  public
    procedure SortCheckStringGridByCol(ACol: Integer);
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormResize(Sender: TObject);
var
  ColIndex, EqualWidth:Integer;
begin
  EqualWidth:=Width div CheckStringGrid.ColCount;
  for ColIndex:=0 to (CheckStringGrid.ColCount-1) do
    CheckStringGrid.ColWidths[ColIndex]:=EqualWidth;
end;

procedure TForm2.SortCheckStringGridByCol(ACol: Integer);
var
  Strings: TStrings;
  RowIndex: Integer;
  TestNum: Extended;
begin
  Strings:=CheckStringGrid.Cols[ACol];
  for RowIndex:=0 to (Strings.Count-1) do
  begin
    if not TryStrToInt(Strings[RowIndex], TestNum) then
    begin
      ShowMessage('Cannot sort string');
      Exit;
    end;
  end;
end;

procedure TForm2.CheckStringGridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  CheckStringGrid.Canvas.Brush.Color:=clGreen;
end;

end.

