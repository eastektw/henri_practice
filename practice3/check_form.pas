unit check_form;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Grids, tools;

type

  { TForm2 }

  TForm2 = class(TForm)
    CheckStringGrid: TStringGrid;
    StringGridPanel: TPanel;
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private

  public
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
  EqualizeColWidth(Self, CheckStringGrid);
end;

procedure TForm2.StringGridToEmptyShapeObjectFormat(AStringGrid: TStringGrid);
begin
  AStringGrid.RowCount:=1;
  AStringGrid.ColCount:=7;
end;

procedure TForm2.DefaultCheckStringGrid;
begin
  EqualizeColWidth(Self, CheckStringGrid);
  StringGridToEmptyShapeObjectFormat(CheckStringGrid);
end;

end.

