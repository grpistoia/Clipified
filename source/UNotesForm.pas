unit UNotesForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, Forms, ExtCtrls, StdCtrls, Controls, Buttons;

type

  { TNotesForm }

  TNotesForm = class(TForm)
    OkButton: TBitBtn;
    GroupBox: TGroupBox;
    NotesMemo: TMemo;
    CancelButton: TBitBtn;
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    function  GetNotes(): string;
    procedure SetNotes(NewNotes: string);
  public
    { public declarations }
    property Notes: string read GetNotes write SetNotes;
  end;

implementation

{$R *.lfm}

{ TNotesForm }

procedure TNotesForm.FormKeyPress(Sender: TObject; var Key: char);
begin
    case Key of
        {#13: begin                               ENTER is OK because I do support many lines
                // bye
                ModalResult := mrOK;
             end;}
        #27: begin
                // bye
                ModalResult := mrCancel;
             end;
    end;
end;

function  TNotesForm.GetNotes(): string;
begin
    // Kept in one place
    result := NotesMemo.Lines.Text;
end;

procedure TNotesForm.SetNotes(NewNotes: string);
begin
    // Keep in one place..
    NotesMemo.Lines.Clear;
    NotesMemo.Lines.Text := NewNotes;
end;

end.

