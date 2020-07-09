unit UOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, EditBtn, StdCtrls, Buttons, UGlobals;

type

  { TOptions }

  TOptions = class(TForm)
    ButtonTestBackup: TButton;
    CancelButton: TBitBtn;
    CheckBoxShowBalloons: TCheckBox;
    CheckBoxTrayIcon: TCheckBox;
    CheckBoxAutoTrim: TCheckBox;
    CheckBoxMinimal: TCheckBox;
    EditBackupSourceDir: TDirectoryEdit;
    EditBackupTargetFile: TFileNameEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelHint: TLabel;
    EditBackupTime: TTimeEdit;
    MemoCopyright: TMemo;
    OkButton: TBitBtn;
    procedure ButtonTestBackupClick(Sender: TObject);
    procedure FolderOrZipFileChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure OkButtonClick(Sender: TObject);
  private
    { private declarations }
    procedure EnableTestBackupButton;
  public
    procedure ZipFileStart(Sender : TObject; Const AFileName : String);
  end;

implementation

{$R *.lfm}

{ TOptions }

procedure TOptions.FormCreate(Sender: TObject);
begin
    // Behaviour stuff
    CheckBoxMinimal.Checked       := UGlobals.UseMinimalWindowSize;
    CheckBoxTrayIcon.Checked      := UGlobals.ShowTrayIcon;
    CheckBoxShowBalloons.Checked  := UGlobals.ShowBalloonHints;
    CheckBoxAutoTrim.Checked      := UGlobals.AutomaticTrimNL;

    // Backup stuff
    EditBackupSourceDir.Text      := UGlobals.BackupSourceDir;
    EditBackupTargetFile.Text     := UGlobals.BackupTargetFile;
    EditBackupTime.Time           := UGlobals.BackupTime;

    // To be consistent..
    EnableTestBackupButton;

    // Legal stuff.
    MemoCopyright.Clear;
    MemoCopyright.Lines.Add(COPYRIGHT);

end;

procedure TOptions.OkButtonClick(Sender: TObject);
begin
    // Behaviour stuff
    UGlobals.UseMinimalWindowSize  := CheckBoxMinimal.Checked;
    UGlobals.ShowTrayIcon          := CheckBoxTrayIcon.Checked;
    UGlobals.ShowBalloonHints      := CheckBoxShowBalloons.Checked;
    UGlobals.AutomaticTrimNL       := CheckBoxAutoTrim.Checked;

    // Backup stuff
    UGlobals.BackupSourceDir       := EditBackupSourceDir.Text;
    UGlobals.BackupTargetFile      := EditBackupTargetFile.Text;
    UGlobals.BackupTime            := EditBackupTime.Time;
end;

procedure TOptions.EnableTestBackupButton;
begin
    ButtonTestBackup.Enabled := ValidateBackupParam(EditBackupSourceDir.Text, EditBackupTargetFile.Text);
end;

procedure TOptions.ButtonTestBackupClick(Sender: TObject);
begin
    LabelHint.Caption := 'Starting..';
    try
        { Whatever is written in the editboxes }
        BackupFile(EditBackupSourceDir.Text, EditBackupTargetFile.Text, @ZipFileStart);
    finally
        LabelHint.Caption := 'Zip completed.'; // completed
    end;
end;

procedure TOptions.ZipFileStart(Sender : TObject; Const AFileName : String);
begin
    LabelHint.Caption := 'Zipping ' + ExtractFileName(AFileName)+ '..';
    LabelHint.Repaint;
    Sleep(200);
end;

procedure TOptions.FolderOrZipFileChange(Sender: TObject);
begin
    EnableTestBackupButton;
end;

end.

