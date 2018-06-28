unit check_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids;

type

  { TForm2 }

  TForm2 = class(TForm)
    CheckStringGrid: TStringGrid;
    StringGridPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private

  public
    procedure EqualizeColWidth(AStringGrid: TStringGrid);
    procedure StringGridToEmptyShapeObjectFormat(AStringGrid: TStringGrid);
    procedure DefaultCheckStringGrid;
  end;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  DefaultCheckStringGrid;
end;

procedure TForm2.FormResize(Sender: TObject);
begin
  EqualizeColWidth(CheckStringGrid);
end;

procedure TForm2.EqualizeColWidth(AStringGrid: TStringGrid);
var
  EqualWidth:Integer;
begin
  EqualWidth:=Self.Width div CheckStringGrid.ColCount;
  AStringGrid.DefaultColWidth:=EqualWidth;
end;

procedure TForm2.StringGridToEmptyShapeObjectFormat(AStringGrid: TStringGrid);
begin
  AStringGrid.RowCount:=1;
  AStringGrid.ColCount:=7;
end;

procedure TForm2.DefaultCheckStringGrid;
begin
  EqualizeColWidth(CheckStringGrid);
  StringGridToEmptyShapeObjectFormat(CheckStringGrid);
end;

end.

