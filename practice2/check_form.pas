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
    procedure DefaultCheckStringGrid;
  end;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.FormCreate(Sender: TObject);
begin
  CheckStringGrid.RowCount:=1;
  CheckStringGrid.ColCount:=7;
end;

procedure TForm2.FormResize(Sender: TObject);
var
  ColIndex, EqualWidth:Integer;
begin
  EqualWidth:=Width div CheckStringGrid.ColCount;
  for ColIndex:=0 to (CheckStringGrid.ColCount-1) do
    CheckStringGrid.ColWidths[ColIndex]:=EqualWidth;
end;

procedure TForm2.DefaultCheckStringGrid;
begin
  FormCreate(Self);
  FormResize(Self);
end;

end.

