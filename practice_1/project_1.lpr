program project_1;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, practice_1_stringgrid, practice_1_array, practice_1_TList,
  practice_1_tools, practice_1_linklist
  { you can add units after this };

{$R *.res}

begin
  RequireDerivedFormResource:=True;
  Application.Initialize;
  Application.CreateForm(TPractice1, Practice1);
  Application.Run;
end.

