unit UGlobals;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
    ExtCtrls, Clipbrd, LCLIntf, LCLType, Menus, DbCtrls, Types, StrUtils,
    Zipper, DateUtils;

const

    { NAME AND STUFF }
    APP_NAME         = 'Clipified';  { Used also for Registry and file }
    APP_VERSION      = 'v8.0';
    APP_DESC         = APP_NAME + ' ' + APP_VERSION;
    APP_RELEASE_TEXT = '07/07/2020';

    { Expiration after this }
    EXPIRY_MONTHS    = 24;

    { Generic message }
    COPYRIGHT     = ' ' + APP_DESC + LineEnding +
                    ' Author: Gustavo R. Pistoia' + LineEnding +
                    ' Date  : ' + APP_RELEASE_TEXT + LineEnding +
                    '   This software was designed on my own personal time, outside working hours, with my own computer' + LineEnding +
                    '   and materials, without help or intructions from anyone or any organisation.' + LineEnding +
                    '   This software comes with no WARRANTY; without even the implied warranty of merchantability or fitness for a particular purpose.' + LineEnding +
                    '   This application will always belong to the author and can never be sold, used, modified or installed without consent of the author.' + LineEnding +
                    '   This application continues to belong to the author regardless in which organisation or computer runs.' + LineEnding +
                    '   The author has no obligation to give away the software, provide an upgrade, or any other obligation.' + LineEnding +
                    '   By using the software you accept the terms and conditions written here or what the author may decide without previous notice.';

    {  ITEMS TO KEEP AND DISPLAY }

    MAX_ITEMS_IN_CLIPBOARD_TO_REMEMBER = 200;
    MAX_TEXT_DISPLAY_LENGTH            = 100;

    { STYLE WHEN DOUBLE CLICK }
    MAIN_WINDOW_THIN_STYLE         = bsNone;
    MAIN_WINDOW_MOVE_STYLE         = {$IFDEF UNIX} bsSingle {$ELSE} bsSizeToolWin {$ENDIF};


    { HOW LONG BEFORE A BREAK }
    BREAK_ALERT_DISABLED   : TDateTime   = 0; // Is not an object, so zero means NO
    BREAK_ALERT_IN_30_MIN  : TDateTime   = OneMinute * 30;
    BREAK_ALERT_IN_1_HOUR  : TDateTime   = OneHour;
    BREAK_ALERT_IN_2_HOURS : TDateTime   = OneHour   * 2;
    BREAK_ALERT_IN_4_HOURS : TDateTime   = OneHour   * 4;

    { How long is it? }
    WARN_TIME_BEFORE_BREAK   : TDateTime = OneMinute * 4;
    TIME_SHOW_WORKED_TIME    : TDateTime = OneMinute * 1;
    TIME_CONSIDERED_INACTIVE : TDateTime = OneMinute * 3;
    TIME_CONSIDERED_A_BREAK  : TDateTime = OneMinute * 5;
    POSTPONE_TIME_FOR_BREAK  : TDateTime = OneMinute * 15;

    { messages }
    MESSAGE_CLIPBOARD_STORED      = 'Clipboard saved';
    MESSAGE_CLIPBOARD_RESTORED    = 'Clipboard restored';
    MESSAGE_CLIPBOARD_FILE_LOADED = 'File loaded';

    { pause }
    PAUSE_REMINDER_TIME_IS_UP     = 'Time is up! Have some rest.';
    PAUSE_REMINDER_TIME_ALMOST_UP = 'Take a break!!!';

    { behaviour }
    WINDOW_START_TOP  = {$IFDEF UNIX}  30 {$ELSE}   0  {$ENDIF};
    WINDOW_START_LEFT = {$IFDEF UNIX} 900 {$ELSE} 780  {$ENDIF};
    TEXT_WARNING_COLOR = clRed;
    TEXT_INFO_COLOR    = clBlue;
    TEXT_NORMAL_COLOR  = clInfoText;

    { how often to refresh.. big objects best to take a break }
    TIMER_REFRESH_SHORT =  250;
    TIMER_REFRESH_LONG  = 2000;

    { how long to display an important message }
    TIMER_SHOW_MESSAGE_SHORT = 3000;
    TIMER_SHOW_MESSAGE_LONG  = 6000;

    { registry }
    REGISTRY_RELATIVE_PATH         = 'Software\' + APP_NAME;
    REGISTRY_WINDOWS_TOP_POSITION  = 'TopPositition';
    REGISTRY_WINDOWS_LEFT_POSITION = 'LeftPositition';
    REGISTRY_SAVED_NOTES           = 'SavedNotes';
    REGISTRY_LAST_BREAK            = 'LastBreak';
    // kept for compatibility  (before I only saved text.. now nore..)
    REGISTRY_CLIPBOARD_ITEM_NUMBER = 'ClipboardItem#';  {+ number of item dynamically}
    REGISTRY_CLIPBOARD_ITEM_TYPE   = 'ClipboardType#';  {+ number of item dynamically}

    { Other parameters }
    // Desktop behaviour
    REGISTRY_MINIMAL_WINDOW_SIZE   = 'MinimalWindowSize';
    REGISTRY_SHOW_TRAY_ICON        = 'ShowTrayIcon';
    REGISTRY_SHOW_BALLOON_HINTS    = 'ShowBalloonHints';
    // Automatically trim entries...
    REGISTRY_AUTOMATIC_TRIM        = 'AutomaticTrim';
    // Backup stuff
    REGISTRY_BACKUP_SOURCE_DIR     = 'BackupSourceDir';
    REGISTRY_BACKUP_TARGET_FILE    = 'BackupTargetFile';
    REGISTRY_BACKUP_TIME           = 'BackupTime';

    { EDIT IMAGE FORM }
    EDITIMAGEFORM_DEFAULT_ZOOM   = 2;
    EDITIMAGEFORM_DEFAULT_EXTRA  = 4;
    EDITIMAGEFORM_KEY_FOR_ZOOM   = ssCtrl;
    EDITIMAGEFORM_KEY_FOR_BORDER = ssAlt;
    EDITIMAGEFORM_CURSOR_LINE_COLOUR = clRed;
    EDITIMAGEFORM_IMAGE_BORDER_COLOUR = clNavy;
    EDITIMAGEFORM_PEN_MODE_FOR_CURSOR = {$IFDEF UNIX} pmNotXor; {$ELSE} pmXor; {$ENDIF} // pmNotXor: Gray | pmXor: Works in Windows
    EDITIMAGEFORM_CONFIRMATION_MESSAGE = 'Selection complete. Copy to clipboard or retry?';

    { ZOOM FORM }
    ZOOMFORM_KEY_FOR_REFRESH = ssCtrl;

var
    // Desktop behaviour
    UseMinimalWindowSize  : boolean = true;
    ShowTrayIcon          : boolean = true;
    ShowBalloonHints      : boolean = true;
    // Automatically trim entries...
    AutomaticTrimNL       : boolean = false;
    // Backup stuff
    BackupSourceDir       : string;
    BackupTargetFile      : string;
    BackupTime            : TTime;

type

    { Registry type }
    TRegistryType = ( rtText, rtPicture );

    { Type of action        .. In order of priority to be kept }
    TContentActionGroup = ( cagFilePath, cagURL, cagEmails, cagNumberId, cagPicture, cagNone );

    { Generic functions }
    function WidthHeightToText(Width, Height: integer): string;
    function SizeToText(cx, cy: integer): string;
    function CursorToText(Point: TPoint): string;
    function ShiftStateToText(ShiftState: TShiftStateEnum): string;

    { For Screenshot and Zooming }
    procedure TakeScreenshotFromCursor(var ApplyOnBitmap: TBitmap);

    { Make backup.. }
    function  ValidateBackupParam(TheSourceDir, TheTargetFile: string) : boolean;
    procedure BackupFile(TheSourceDir, TheTargetFile: string; TheStartFile : TOnStartFileEvent);

const

    // Text to use in Registry
    RegistryTypeNames : array[TRegistryType] of string = ( 'Text', 'Picture' );

    // This is the text to show in the Menu
    ContentActionGroupNames : array[TContentActionGroup] of string  = ( 'File/Folder', 'Website', 'Email', 'Number', 'Picture', 'Generic Content' );
    ContentActionToKeep     : array[TContentActionGroup] of Integer = (  25,            25,        10,      40,       5,         0 );

implementation

    function WidthHeightToText(Width, Height: integer): string;
    begin
        result := IntToStr(Width) + 'x' + IntToStr(Height);
    end;

    function SizeToText(cx, cy: integer): string;
    begin
        result := 'Size: (' + WidthHeightToText(cx, cy) + ')';
    end;

    function CursorToText(Point: TPoint): string;
    begin
        result := 'Cursor: (' + IntToStr(Point.X) + ', ' + IntToStr(Point.Y) + ')';
    end;

    function ShiftStateToText(ShiftState: TShiftStateEnum): string;
    begin
        case ShiftState of
            ssShift     : result := 'Shift';
            ssAlt       : result := 'Alt';
            ssCtrl      : result := 'Ctrl';
            ssLeft      : result := 'Left';
            ssRight     : result := 'Right';
            ssMiddle    : result := 'Middle';
            ssDouble    : result := 'Double';
            ssMeta      : result := 'Meta';
            ssSuper     : result := 'Super';
            ssHyper     : result := 'Hyper';
            ssAltGr     : result := 'AltGr';
            ssCaps      : result := 'Caps';
            ssNum       : result := 'Num';
            ssScroll    : result := 'Scroll';
            ssTriple    : result := 'Triple';
            ssQuad      : result := 'Quad';
            ssExtra1    : result := 'Extra1';
            ssExtra2    : result := 'Extra2';
            otherwise result := 'Unknown';
        end;
    end;

    procedure TakeScreenshotFromCursor(var ApplyOnBitmap: TBitmap);
    var currentPosition: TPoint;
        currentMonitor: TMonitor;
        monitorCanvas: TCanvas;
        deviceContext: LCLType.HDC;
    begin

        try

            // Default..
            currentPosition := Point(Application.MainForm.Left, Application.MainForm.Top);
            // Where are you?
            GetCursorPos(currentPosition);
            // Convert..
            currentMonitor := Screen.MonitorFromPoint(currentPosition);

            // If for some reason is NULL.. I create it..
            if (ApplyOnBitmap = NIL) then ApplyOnBitmap := TBitmap.Create;

            // Make it bigger
            ApplyOnBitmap.SetSize(currentMonitor.Width, currentMonitor.Height);

            // the screen device context
            deviceContext := LCLIntf.GetDC(0);
            try

                // If you have one monitor.. in Linux doesn't work.. use normal style
                if (Screen.MonitorCount >= 2) then begin

                    // This is to access the monitor
                    monitorCanvas := TCanvas.Create();
                    try

                        // Now assign it so I can use it
                        monitorCanvas.Handle := deviceContext;

                        // hope works...
                        BitBlt( // the target canvas
                                ApplyOnBitmap.Canvas.Handle,
                                // the rectangle to copy into
                                0, 0, currentMonitor.Width, currentMonitor.Height,
                                // the source DC
                                monitorCanvas.Handle,
                                // the source start X,Y
                                currentMonitor.Left, currentMonitor.Top,
                                // config
                                SRCCOPY //or CAPTUREBLT
                              );

                    finally
                        // delete canvas created
                        FreeAndNil(monitorCanvas);
                    end;

                end else begin

                        // This is very safe
                    ApplyOnBitmap.LoadFromDevice(deviceContext);  // I dont use the monitor.. just the DC

                end;

            finally
                // release
                LCLIntf.ReleaseDC(0 , deviceContext);
            end;

        except
            // release memory
            FreeAndNil(ApplyOnBitmap);

        end;
    end;

    function ValidateBackupParam(TheSourceDir, TheTargetFile: AnsiString) : boolean;
    begin
        result := DirectoryExists(TheSourceDir) and
                  (
                      FileExists(TheTargetFile)
                      or
                      DirectoryExists(ExtractFilePath(TheTargetFile))
                  )
    end;

    procedure BackupFile(TheSourceDir, TheTargetFile: string; TheStartFile : TOnStartFileEvent);
    var zipfile  : TZipper;
        folders  : TStringList;
        tempText : string;
    begin
        // Create and destroy later..
        zipFile := TZipper.Create();
        try
            // Feedback..
            zipFile.OnStartFile := TheStartFile;
            // Changes..
            tempText := TheTargetFile;
            tempText := AnsiReplaceText(tempText, '%yyyy', FormatDateTime('yyyy', Now()));
            tempText := AnsiReplaceText(tempText, '%yy', FormatDateTime('yy', Now()));
            tempText := AnsiReplaceText(tempText, '%mmm', FormatDateTime('Mmm', Now()));
            tempText := AnsiReplaceText(tempText, '%mm', FormatDateTime('mm', Now()));
            tempText := AnsiReplaceText(tempText, '%ddd', FormatDateTime('DDD', Now()));
            tempText := AnsiReplaceText(tempText, '%dd', FormatDateTime('dd', Now()));
            // Final file...
            zipFile.FileName    := tempText;
            // Folders..
            folders := FindAllFiles(TheSourceDir);
            try
                // Not so many parameters.. zip it all
                zipFile.Entries.AddFileEntries(folders);
                // Now zip...
                zipFile.ZipAllFiles;
            finally
                // This is a list..
                FreeAndNil(folders);
            end;
        finally
            // The main zipper
            FreeAndNil(zipFile);
        end;
    end;

end.

