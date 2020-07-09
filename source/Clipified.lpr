program Clipified;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UClipifiedForm, UClipboardListener, UGlobals, UTextTools, UOptions;

{$R *.res}

begin
    RequireDerivedFormResource:=True;
    Application.Initialize;
    Application.CreateForm(TClipifiedForm, ClipifiedForm);
    Application.Run;
end.

