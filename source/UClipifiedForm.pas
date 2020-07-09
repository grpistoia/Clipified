unit UClipifiedForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Clipbrd, LCLIntf, LCLType, Menus, ExtDlgs, DbCtrls, ColorBox,
  Calendar, Math, Registry, LazFileUtils, DateUtils, FPExprPars,
  UGlobals, UClipboardContent, UClipboardListener, UEditImageForm, UEvaluatorForm,
  UZoomForm, UNotesForm, UTextTools, UOptions;

type

  { This is for the popup that changes behaviour }
  TPopupDynamicContentType = (pdctSortedByType {First because is the default}, pdctFullList {Only for button, special});

  { TClipifiedForm }
  TClipifiedForm = class(TForm)
    BtnOpenClipboard: TButton;
    ColorBox1: TColorBox;
    ComboBox1: TComboBox;
    MainPopupS4: TMenuItem;
    MainPopupByType: TMenuItem;
    ClipboardPopup: TPopupMenu;
    ClipboardDynamicItems: TMenuItem;
    MainPopupNotes: TMenuItem;
    MainPopupTextTools: TMenuItem;
    MainPopupS5: TMenuItem;
    RefreshEvent: TTimer;
    GenericPanel: TPanel;
    LblInfo: TLabel;
    MainPopup: TPopupMenu;
    MainPopupExit: TMenuItem;
    MainPopupS2: TMenuItem;
    MainPopupObliviate: TMenuItem;
    MainPopupS1: TMenuItem;
    MainPopupS3: TMenuItem;
    MainPopupCalc: TMenuItem;
    AppTrayIcon: TTrayIcon;
    MainPopupCalendar: TMenuItem;
    MainPopupScreenshot: TMenuItem;
    MainPopupOptions: TMenuItem;
    MainPopupBreak: TMenuItem;
    MainPopupBreakInfo: TMenuItem;
    MainPopupBreak30min: TMenuItem;
    MainPopupBreak1hour: TMenuItem;
    MainPopupBreak2hours: TMenuItem;
    MainPopupBreak4hours: TMenuItem;
    MainPopupBreakOff: TMenuItem;
    MainPopupEvaluator: TMenuItem;
    MainPopupZoom: TMenuItem;
    procedure BtnOpenClipboardClick(Sender: TObject);
    procedure ClipboardDynamicItemsClick(Sender: TObject);
    procedure ClipboardPopupOpen(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure MainPopupNotesClick(Sender: TObject);
    procedure MainPopupOptionsClick(Sender: TObject);
    procedure MainPopupTextToolsClick(Sender: TObject);
    procedure RefreshEventTimer(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of String);
    procedure GenericDoubleClick(Sender: TObject);
    procedure MainPopupBreak1hourClick(Sender: TObject);
    procedure MainPopupBreak2hoursClick(Sender: TObject);
    procedure MainPopupBreak30minClick(Sender: TObject);
    procedure MainPopupBreak4hoursClick(Sender: TObject);
    procedure MainPopupBreakOffClick(Sender: TObject);
    procedure MainPopupCalcClick(Sender: TObject);
    procedure MainPopupCalendarClick(Sender: TObject);
    procedure MainPopupClose(Sender: TObject);
    procedure MainPopupOpen(Sender: TObject);
    procedure MainPopupScreenshotClick(Sender: TObject);
    procedure MainPopupObliviateClick(Sender: TObject);
    procedure MainPopupExitClick(Sender: TObject);
    procedure MainPopupEvaluatorClick(Sender: TObject);
    procedure MainPopupZoomClick(Sender: TObject);
  private
    { message ones }
    procedure ShowMessage(Message: string);
    procedure ShowInfo(Message: string);
    procedure ShowWarning(Message: string);
    { idle }
    procedure OnAppIdleEnd(Sender: TObject);
    { handy ones }
    procedure MakeWindowStayOnTop;
    procedure MakeWindowOverlapable;
    { show the button text.. }
    procedure UpdateButtonOpenClipboard();
    { dynamic menus }
    procedure BuildClipboardContentMenues(contentType: TPopupDynamicContentType; ParentPopup: TPopupMenu; ParentItem: TMenuItem);
    procedure MainPopupByTypeItemClick(Sender: TObject);
    { registry and file }
    procedure LoadFromRegistry();
    procedure SaveInRegistry(ForceRewrite : Boolean);
    procedure SaveClipboardFile(TextToSave: string; ForceRewrite : Boolean);
    { Apply changes from Options }
    procedure ApplyWindowStyle;
  private
    { looking at the Clipboard }
    FClipboardListener: TClipboardListener;
    procedure AnalyseClipboard;
  private
    { Created by hand.. for speed and position.. }
    FCalendarDialog   : TCalendarDialog;
    FCalculatorDialog : TCalculatorDialog;
    FEvaluatorForm    : TEvaluatorForm;
    FNotesForm        : TNotesForm;
  protected
    { sometimes wait for a while before clearing messages }
    FMessageDelayTime : Integer;
    { Saved data.. }
    FSavedNotes  : string;
    { related with taking a break }
    FLastActivitySeen: TDateTime;
    FLastBreakTaken  : TDateTime;
    FAccumulatedWork : TDateTime;
    FPauseChosen     : TDateTime;
    FLastMousePosition: TPoint;
  public
    { Zip progress.. }
    procedure MakeBackupIfNeeded;
    procedure ZipFileStart(Sender : TObject; Const AFileName : String);
  end;

var
  ClipifiedForm: TClipifiedForm;

implementation

{$R *.lfm}

{ TClipifiedForm }

procedure TClipifiedForm.FormCreate(Sender: TObject);
begin
    // Put the title .. override
    Application.Title := APP_NAME; // LEAVE AS IT IS... OTHERWISE REGISTRY AND OTHER CHANGES...

    // Icon
    AppTrayIcon.BalloonTitle := Self.Caption;
    AppTrayIcon.Icon := Application.Icon;

    // Windows title..
    Self.Caption := APP_DESC;

    // starting point (default)
    Self.Top := WINDOW_START_TOP;
    Self.Left := WINDOW_START_LEFT;
    Self.Height := GenericPanel.Top + 1 + BtnOpenClipboard.Height + 1 {$IFDEF UNIX} - 2 {$ENDIF};
    Self.BorderStyle := MAIN_WINDOW_THIN_STYLE;

    // There is always a crazy Windows corrupted function or someone changing the screen for a small one
    if (Self.Top  <= 0) or (Self.Top  > Screen.Height - Self.Height) then Self.Top := WINDOW_START_TOP;
    if (Self.Left <= 0) or (Self.Left > Screen.Width  - Self.Width)  then Self.Left := WINDOW_START_LEFT;

    // Border style basically
    ApplyWindowStyle;

    // components start text
    ShowMessage('Loading..');

    // logic to take a break..     (the LAST BREAK TAKEN gets loaded form registry)
    FLastActivitySeen := Now();
    FLastBreakTaken   := Now();
    FAccumulatedWork  := 0;
    FPauseChosen      := BREAK_ALERT_IN_4_HOURS;

    // to ensure is set to something..
    FLastMousePosition := Point(0, 0);

    // Listener
    FClipboardListener := TClipboardListener.Create;

    // Reload config  (after I applied the defaults)
    LoadFromRegistry();

    // Process the one you got
    // AnalyseClipboard();    Wait for full load to analyse it..

    // Managed to connect the clipboard...
    if (FClipboardListener.Supported) then ShowMessage('Clipboard Hooked..') else ShowMessage(Caption);

    // Set the idle..
    Application.OnIdleEnd := @OnAppIdleEnd;

end;

procedure TClipifiedForm.FormDestroy(Sender: TObject);
begin
    // Delete..
    AppTrayIcon.Hide;
    // Stop listening..
    FreeAndNil(FClipboardListener);
    // kill this?
    FreeAndNil(FCalendarDialog);
    // kill this?
    FreeAndNil(FCalculatorDialog);
    // kill this?
    FreeAndNil(FEvaluatorForm);
    // kill this?
    FreeAndNil(FNotesForm);
end;

procedure TClipifiedForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    // die bastard...
    if (CloseAction <> caMinimize) then Application.Terminate;
end;

procedure TClipifiedForm.FormActivate(Sender: TObject);
begin
    // Show info
    ShowInfo(APP_DESC);
    // Because menu will appear, or combo.. allow those to be in front
    MakeWindowStayOnTop;
end;

procedure TClipifiedForm.ShowMessage(Message: string);
begin
    // This one is not that important may be..
    if (FMessageDelayTime <= 0) then begin
        // a bit of wait..
        FMessageDelayTime  := TIMER_SHOW_MESSAGE_SHORT;
        // Not showing.. do now..
        LblInfo.Font.Color := TEXT_NORMAL_COLOR;
        LblInfo.Caption := Message;
    end;
end;

procedure TClipifiedForm.ShowInfo(Message: string);
begin
    // a bit of wait..
    FMessageDelayTime  := TIMER_SHOW_MESSAGE_LONG;
    // Windows leave it dirty
    LblInfo.Invalidate;
    // The label...
    LblInfo.Font.Color := TEXT_INFO_COLOR;
    LblInfo.Caption := Message;
    // Windows leave it dirty
    LblInfo.Invalidate;
    // Pain now.. because it could be used inside a loop
    LblInfo.Repaint;
    // Only in Linux doesn't paint
    {$IFDEF UNIX} Application.ProcessMessages; {$ENDIF}
end;

procedure TClipifiedForm.ShowWarning(Message: string);
begin
    // a bit of wait..
    FMessageDelayTime  := TIMER_SHOW_MESSAGE_LONG;
    // Windows leave it dirty
    LblInfo.Invalidate;
    // The label...
    LblInfo.Font.Color := TEXT_WARNING_COLOR;
    LblInfo.Caption := Message;
    // Windows leave it dirty
    LblInfo.Invalidate;
    // Pain now.. because it could be used inside a loop
    LblInfo.Repaint;
    // Only in Linux doesn't paint
    {$IFDEF UNIX} Application.ProcessMessages; {$ENDIF}
end;

procedure TClipifiedForm.OnAppIdleEnd(Sender: TObject);
var releaseDate: TDate;
    expirationDate: TDate;
    currentMousePosition: TPoint;
    breakText: string;
    rightNow: TDateTime;
    pivotDate: TDateTime;
    bigBreak: boolean;
    // for the reset
    HourNow,  MinuteNow,  SecondNow,  MilliSecondNow: word;
begin

    { THIS IS TO MANAGE EXPIRATION DATE... }

    // Expired?
    TryStrToDate(UGlobals.APP_RELEASE_TEXT, releaseDate, 'DD/MM/YYYY', '/');
    expirationDate := IncMonth(releaseDate, UGlobals.EXPIRY_MONTHS);
    if (Date() > expirationDate ) then begin

        // Menu dead
        FreeAndNil(MainPopup);
        FreeAndNil(ClipboardPopup);

        // Kill the whole thing
        BtnOpenClipboard.Hide;

        // Expired
        ShowWarning('Software Expired');

        // Do not continue below...
        EXIT;

    end else if (Date() >= expirationDate - 5) then begin

        // Expired
        ShowMessage('Software Expires soon');

    end;

    // where is the mouse now?
    currentMousePosition := Point(0, 0);
    GetCursorPos(currentMousePosition);

    // Minor speed improvement
    rightNow := Now();

    // Get HH:MM
    DecodeTime(rightNow, HourNow, MinuteNow, SecondNow, MilliSecondNow);
    if (HourNow = 00) and (MinuteNow = 00) then FAccumulatedWork := 0;

    // This means.. I am already showing a dialog
    if (FPauseChosen <> BREAK_ALERT_DISABLED) then begin

        // Did you move.. you did something..
        if (PointsEqual(FLastMousePosition, currentMousePosition)) then begin

            // The last activity was a while ago?
            if ((rightNow - FLastActivitySeen) >= TIME_CONSIDERED_INACTIVE) then begin

                // Works slightly different
                bigBreak := ((rightNow - FLastActivitySeen) >= TIME_CONSIDERED_A_BREAK);

                // The last activity was a while ago?
                if (bigBreak) then begin

                    // Accumulate the difference.. next line will avoid this each loop
                    FAccumulatedWork := FAccumulatedWork + Max(0.0, FLastActivitySeen - FLastBreakTaken);

                    // I consider this second as a break
                    FLastBreakTaken := rightNow;

                    // build text
                    breakText := 'Break:';

                end else begin

                    // build text
                    breakText := 'Inactivity:';

                end;

                pivotDate := FLastActivitySeen;
                // Show a message.. less than 24 hrs?
                if (DaysBetween(rightNow, pivotDate) >= 1.0 ) then breakText := breakText + ' ' + IntToStr(Round(DaysBetween(rightNow, pivotDate)))+ ' day(s)';
                pivotDate := IncDay(pivotDate, DaysBetween(rightNow, pivotDate));
                if (HoursBetween(rightNow, pivotDate) >= 1.0 ) then breakText := breakText + ' ' + IntToStr(Round(HoursBetween(rightNow, pivotDate))) + ' hr';
                pivotDate := IncHour(pivotDate, HoursBetween(rightNow, pivotDate));
                // dont show for days
                if (DaysBetween(rightNow, FLastActivitySeen) < 1.0 ) then begin
                   // min appart..
                   if (MinutesBetween(rightNow, pivotDate) >= 1.0 )
                       then breakText := breakText + ' ' + IntToStr(Round(MinutesBetween(rightNow, pivotDate))) + ' min'
                       else breakText := breakText + ' ' + IntToStr(Round(SecondsBetween(rightNow, pivotDate))) + ' sec';
                end;

                // Show
                if (bigBreak) then ShowInfo(breakText) else ShowMessage(breakText);

            end else if ((rightNow - FLastActivitySeen) >= TIME_SHOW_WORKED_TIME) then begiN

                ShowInfo('Hours worked: ' + FormatDateTime('hh:nn', (FAccumulatedWork + Max(0.0, FLastActivitySeen - FLastBreakTaken))));

            end;

        end else begin

            // keep for next time
            FLastMousePosition := currentMousePosition;
            // I just did something..
            FLastActivitySeen := Now();

        end;

    end;

    // Idle.. make backup..
    MakeBackupIfNeeded;

    // DONT CHANGE STAY ON TOP HERE.. BECAUSE.. I AM DOING OTHER THINGS BASED ON MENUS..

end;

procedure TClipifiedForm.MakeWindowStayOnTop;
begin
    // Some double call
    if (not Application.Terminated) then begin
        // Force it
        Self.FormStyle := fsSystemStayOnTop;
        // StayOnTop hack!
        SetWindowPos(Self.Handle, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOSIZE or SWP_NOMOVE or SWP_NOACTIVATE);
        Self.SetZOrder(false);
    end;
end;

procedure TClipifiedForm.MakeWindowOverlapable;
begin
    // Some double call
    if (not Application.Terminated) then begin
        // Make it back to what it was..
        Self.FormStyle := fsNormal;
    end;
end;

procedure TClipifiedForm.ApplyWindowStyle;
begin
    // Swap between windows styles..
    if (UGlobals.UseMinimalWindowSize) then begin
        // change style
        Self.BorderStyle := MAIN_WINDOW_THIN_STYLE;
        // Fix for Linux
        MakeWindowStayOnTop;
    end else begin
        // change style
        Self.BorderStyle := MAIN_WINDOW_MOVE_STYLE;
        // Fix for Linux
        MakeWindowOverlapable;
    end;

    // Change...
    AppTrayIcon.Visible      := UGlobals.ShowTrayIcon;
    AppTrayIcon.BalloonFlags := bfNone;

    // UGlobals.AutomaticTrimNL .. automated..
end;

procedure TClipifiedForm.GenericDoubleClick(Sender: TObject);
begin
    // I did something..
    FLastActivitySeen := Now();
    // This one is just a swap..
    UGlobals.UseMinimalWindowSize := NOT UGlobals.UseMinimalWindowSize;
    // Make the adjustments..
    ApplyWindowStyle;
end;

procedure TClipifiedForm.MainPopupBreak30minClick(Sender: TObject);
begin
    FAccumulatedWork  := FAccumulatedWork + Max(0.0, FLastActivitySeen - FLastBreakTaken);
    FLastActivitySeen := Now();
    FLastBreakTaken   := Now();
    FPauseChosen      := BREAK_ALERT_IN_30_MIN;
end;

procedure TClipifiedForm.MainPopupBreak1hourClick(Sender: TObject);
begin
    FAccumulatedWork  := FAccumulatedWork + Max(0.0, FLastActivitySeen - FLastBreakTaken);
    FLastActivitySeen := Now();
    FLastBreakTaken   := Now();
    FPauseChosen      := BREAK_ALERT_IN_1_HOUR;
end;

procedure TClipifiedForm.MainPopupBreak2hoursClick(Sender: TObject);
begin
    FAccumulatedWork  := FAccumulatedWork + Max(0.0, FLastActivitySeen - FLastBreakTaken);
    FLastActivitySeen := Now();
    FLastBreakTaken   := Now();
    FPauseChosen      := BREAK_ALERT_IN_2_HOURS;
end;

procedure TClipifiedForm.MainPopupBreak4hoursClick(Sender: TObject);
begin
    FAccumulatedWork  := FAccumulatedWork + Max(0.0, FLastActivitySeen - FLastBreakTaken);
    FLastActivitySeen := Now();
    FLastBreakTaken   := Now();
    FPauseChosen      := BREAK_ALERT_IN_4_HOURS;
end;

procedure TClipifiedForm.MainPopupBreakOffClick(Sender: TObject);
begin
    FAccumulatedWork  := FAccumulatedWork + Max(0.0, FLastActivitySeen - FLastBreakTaken);
    FLastActivitySeen := Now();
    FLastBreakTaken   := Now();
    FPauseChosen      := BREAK_ALERT_DISABLED;
end;

procedure TClipifiedForm.MainPopupCalcClick(Sender: TObject);
begin
    // I should be always UP
    MakeWindowStayOnTop;

    // Create the first time with defaults..
    if (FCalculatorDialog = NIL) then begin
        FCalculatorDialog := TCalculatorDialog.Create(Self);
        FCalculatorDialog.DialogPosition  := poDesigned;
        FCalculatorDialog.Top   := Self.Top + Self.Height + 5;
        FCalculatorDialog.Left  := Self.Left;
        FCalculatorDialog.Value := 0.0;
    end;

    // Keep the value in case.. I have used it..
    // FCalculatorDialog.Value := ...

    // If pressed OK.. Copy to clipboard
    if (FCalculatorDialog.Execute) then Clipboard.AsText := FloatToStr(FCalculatorDialog.Value);
end;

procedure TClipifiedForm.MainPopupCalendarClick(Sender: TObject);
begin
    // I should be always UP
    MakeWindowStayOnTop;

    // Create the first time with defaults..
    if (FCalendarDialog = NIL) then begin
        FCalendarDialog := TCalendarDialog.Create(Self);
        //FCalendarDialog.CancelCaption:= ;
        FCalendarDialog.OKCaption := '&Copy';
        FCalendarDialog.DialogPosition  := poDesigned;
        FCalendarDialog.DisplaySettings := [dsShowHeadings, dsShowDayNames, dsShowWeekNumbers, dsStartMonday];
        FCalendarDialog.Top  := Self.Top + Self.Height + 5;
        FCalendarDialog.Left := Self.Left;
        FCalendarDialog.Date := Date();
    end;

    // Keep the date.. in case I have used it..
    // FCalendarDialog.Date := ...

    // If pressed OK.. Copy to clipboard
    if (FCalendarDialog.Execute) then Clipboard.AsText := FormatDateTime('dd-Mmm-yyyy hh DDD ', FCalendarDialog.Date);
end;

procedure TClipifiedForm.MainPopupEvaluatorClick(Sender: TObject);
var originalText : string;
begin
    // I should be always UP
    MakeWindowStayOnTop;

    // create it the first time
    if (FEvaluatorForm = NIL) then begin
        // build it
        FEvaluatorForm := TEvaluatorForm.Create(Self);
        FEvaluatorForm.Top := Self.Top + Self.Height + 5;
        FEvaluatorForm.Left := Self.Left;
    end;

    // Save...
    originalText := FEvaluatorForm.Expression;

    // Save?
    if (FEvaluatorForm.ShowModal <> mrOK) then begin
        // Restore
        FEvaluatorForm.Expression := originalText;
    end;

end;

procedure TClipifiedForm.MainPopupNotesClick(Sender: TObject);
begin
    // I should be always UP
    MakeWindowStayOnTop;

    // Create the first time with defaults..
    if (FNotesForm = NIL) then begin
        FNotesForm := TNotesForm.Create(Self);
        FNotesForm.Top   := Self.Top + Self.Height + 5;
        FNotesForm.Left  := Self.Left;
    end;

    // For notes.. I ALWAYS want to restore what it was in case I pressed Cancel
    FNotesForm.Notes := FSavedNotes;

    // Save?
    if (FNotesForm.ShowModal = mrOK) then begin
        // Replace
        FSavedNotes := FNotesForm.Notes;
        // Save...
        SaveInRegistry(false);
    end;

end;

procedure TClipifiedForm.MainPopupOptionsClick(Sender: TObject);
var options : TOptions;
begin
    options := TOptions.Create(Self);
    try
        // If applied settings...
        if (options.ShowModal = mrOK) then begin
            // Apply changes
            ApplyWindowStyle;
            // For future..
            SaveInRegistry(true); // To clean previous config completely..
        end;
    finally
        FreeAndNil(options);
    end;
end;

procedure TClipifiedForm.MainPopupTextToolsClick(Sender: TObject);
var textTools: TTextTools;
begin
    // I should be always UP
    MakeWindowStayOnTop;

    // On the fly..
    textTools := TTextTools.Create(Self);
    try
        // Move it where I just like it
        textTools.Top := Self.Top + Self.Height + 5;
        textTools.Left := Self.Left;
        // show it..
        textTools.ShowModal;
    finally
        FreeAndNil(textTools);
    end;
end;

procedure TClipifiedForm.MainPopupScreenshotClick(Sender: TObject);
var textMonitors: string;
    editimage: TEditImageForm;
    monitorCopy : TBitmap;
begin

    // Ensure is up..
    MakeWindowStayOnTop;

    // Before starting.. I dont have it..
    monitorCopy := NIL;

    try

        // Message (some return 0!)
        if (Screen.MonitorCount > 1) then textMonitors := IntToStr(Screen.MonitorCount) + ' monitors. ' else textMonitors := '';

        // Give time to click a menu
        ShowInfo(textMonitors + 'Place mouse..');
        Sleep(1000);

        // Hide after..
        MakeWindowOverlapable;

        ShowInfo(textMonitors + '4 seconds..');
        Sleep(1000);
        ShowInfo(textMonitors + '3 seconds..');
        Sleep(1000);
        ShowInfo(textMonitors + '2 seconds..');
        Sleep(1000);
        ShowInfo(textMonitors + '1 second..');
        Sleep(1000);
        ShowInfo('Taking screenshot..');
        Sleep(500);

        // Make it invisible (another gets focus.. and all that)
        Self.Visible := false;

        {$IFDEF UNIX}

        // Allow erase..
        Application.ProcessMessages;

        // Make all repaint
        Screen.UpdateScreen;

        {$ENDIF}

        // Wait until Windows complete
        Sleep(50);

        // Now do the snapshot...
        TakeScreenshotFromCursor(monitorCopy);


    finally
        // Show it again but you may have lost the position on the Taskbar
        Self.Visible := true;
        Self.Invalidate;
        Self.Repaint;

    end;

    // Becomes priority now..
    MakeWindowStayOnTop;

    // I prefer to release all resource before this..
    if (monitorCopy <> NIL) then begin

        ShowInfo('Select area to crop..');

        // create this thingy..
        editimage := TEditImageForm.Create(Self);
        try

            // Copy the image before destroying it all
            editimage.ApplyImage(monitorCopy);

            // release memory
            FreeAndNil(monitorCopy);

            // Got assigned.. then show.. otherwise something failed...
            editimage.ShowModal;

        finally
            // release memory (Just in case crashed before..
            FreeAndNil(monitorCopy);
            // bye
            FreeAndNil(editimage)
        end;

    end;

end;

procedure TClipifiedForm.ClipboardDynamicItemsClick(Sender: TObject);
var index : Integer;
begin
    // Which one selected?
    index := TMenuItem(Sender).Tag;
    // must be selected but just in case
    if (index >= 0) and (index <= ClipboardHistory.Count - 1) then begin
        // Hopefull set the clipboard
        ClipboardHistory.Items[ index ].ApplyToClipboard;
    end;
end;

procedure TClipifiedForm.BuildClipboardContentMenues(contentType: TPopupDynamicContentType; ParentPopup: TPopupMenu; ParentItem: TMenuItem);
var iterator: Integer;
    content : TClipboardContent;
    childrenMenu : TMenuItem;
    groups : Integer;
    subParentMenu : TMenuItem;
begin
    // I did something..
    FLastActivitySeen := Now();

    // Depending the type..
    if (contentType = pdctFullList) then begin

        // Free it...
        ParentPopup.Items.Clear;
        // Create them on the fly...
        for iterator := 0 to Min(MAX_ITEMS_IN_CLIPBOARD_TO_REMEMBER, ClipboardHistory.Count) - 1 do begin
            // get each item and check..
            content := ClipboardHistory.Items[iterator];
            // Create a menu on the fly..
            childrenMenu := TMenuItem.Create(Self);
            childrenMenu.Caption := IntToStr(ParentPopup.Items.Count + 1) + ') ' + content.ToStringShorten;
            childrenMenu.Tag     := iterator; // index in ClipboardHistory
            childrenMenu.OnClick := @ClipboardDynamicItemsClick;
            // add the item..
            ParentPopup.Items.Add(childrenMenu);
        end;

    end;

    if (contentType = pdctSortedByType) then begin

        // These are dynamic..
        if (ParentPopup <> NIL) then ParentPopup.Items.Clear;
        if (ParentItem  <> NIL) then ParentItem.Clear;

        // Iterate through the full list..
        for groups := 0 to Length(ContentActionGroupNames) - 1 do begin
            // if this is none.. just skip
            if (TContentActionGroup(groups) = cagNone) then CONTINUE;
            // No parent
            subParentMenu := NIL;
            // Create them on the fly...
            for iterator := 0 to ClipboardHistory.Count - 1 do begin
                // get each item and check..
                content := ClipboardHistory.Items[ iterator ];
                // If this one does something..
                if (content.GetActionGroup = TContentActionGroup(groups)) then begin
                    // Create parent if none
                    if (subParentMenu = NIL) then begin
                        subParentMenu := TMenuItem.Create(Self);
                        subParentMenu.Caption := ContentActionGroupNames[content.GetActionGroup];
                        // These are dynamic..
                        if (ParentItem  <> NIL) then ParentItem.Add(subParentMenu);
                        if (ParentPopup <> NIL) then ParentPopup.Items.Add(subParentMenu);
                    end;
                    // Create a menu on the fly..
                    childrenMenu := TMenuItem.Create(Self);
                    childrenMenu.Caption := IntToStr(subParentMenu.Count + 1) + ') ' + content.ToStringShorten;
                    childrenMenu.Tag     := iterator; // index in ClipboardHistory
                    childrenMenu.OnClick := @MainPopupByTypeItemClick;
                    // add the item..
                    subParentMenu.Add(childrenMenu);
                end;
            end;
        end;

        // These are dynamic..
        if (ParentPopup <> NIL) then ParentPopup.Items.Enabled := (MainPopupByType.Count > 0);
        if (ParentItem  <> NIL) then ParentItem.Enabled := (MainPopupByType.Count > 0);

    end;

end;

procedure TClipifiedForm.ClipboardPopupOpen(Sender: TObject);
begin
    // Depending who calls
    BuildClipboardContentMenues(TPopupDynamicContentType(ClipboardPopup.Tag), ClipboardPopup, NIL);
end;

procedure TClipifiedForm.BtnOpenClipboardClick(Sender: TObject);
begin
    // Set depending on who...
    ClipboardPopup.Tag := Int64(pdctFullList); // LIST BEHAVIOUR
    // Force it to show..
    ClipboardPopup.PopUp;
    // Set depending on who...
    ClipboardPopup.Tag := Int64(pdctSortedByType); // DEFAULT BEHAVIOUR
end;

procedure TClipifiedForm.MainPopupOpen(Sender: TObject);
begin

    // I did something..
    FLastActivitySeen := Now();

    // change the style...
    MakeWindowOverlapable;

    // DO NOT ENABLE THE WHOLE MainPopup.. because it is disabled when the App expires

    // Always enabled.. otherwise is irritating..
    // MainPopupTextTools.Enabled := Clipboard.HasFormat(PredefinedClipboardFormat(pcfText));

    // Not sure what Windows does..
    {MainPopupCalendar.Enabled  := (FCalendarDialog   = NIL) or (FCalendarDialog.IsVisible = false);
    MainPopupCalc.Enabled      := (FCalculatorDialog = NIL) or (FCalculatorDialog.IsVisible = false);}
    MainPopupEvaluator.Enabled := (FEvaluatorForm    = NIL) or (FEvaluatorForm.IsVisible = false);
    MainPopupNotes.Enabled     := (FNotesForm        = NIL) or (FNotesForm.IsVisible = false);

    // Flag the menu..
    if (FPauseChosen = BREAK_ALERT_DISABLED)
        then MainPopupBreakInfo.Caption := 'Select a reminder:'
        else MainPopupBreakInfo.Caption := IntToStr(Max(0, MinutesBetween(Now(), (FLastBreakTaken + FPauseChosen)))) + ' min remaining';
    MainPopupBreak30min.Checked  := (FPauseChosen = BREAK_ALERT_IN_30_MIN);
    MainPopupBreak1hour.Checked  := (FPauseChosen = BREAK_ALERT_IN_1_HOUR);
    MainPopupBreak2hours.Checked := (FPauseChosen = BREAK_ALERT_IN_2_HOURS);
    MainPopupBreak4hours.Checked := (FPauseChosen = BREAK_ALERT_IN_4_HOURS);
    MainPopupBreakOff.Checked    := (FPauseChosen = BREAK_ALERT_DISABLED);

    // Build the menu per item..
    BuildClipboardContentMenues(pdctSortedByType, NIL, MainPopupByType);

end;

procedure TClipifiedForm.MainPopupClose(Sender: TObject);
begin
    // change the style...
    MakeWindowStayOnTop;
end;

procedure TClipifiedForm.MainPopupByTypeItemClick(Sender: TObject);
var index : Integer;
begin
    // Which one selected?
    index := TMenuItem(Sender).Tag;
    // must be selected but just in case
    if (index >= 0) and (index <= ClipboardHistory.Count - 1) then begin
        // Hopefull set the clipboard
        ClipboardHistory.Items[ index ].DoAction;
    end;
end;

procedure TClipifiedForm.MainPopupObliviateClick(Sender: TObject);
begin
    // Ask
    if (MessageDlg('Are you sure you want to delete all history forever?', mtConfirmation, [mbOK, mbCancel], 0) = mrOK) then begin

        // Delete it all
        ClipboardHistory.Clear;

        // Blow the clipboard so it does not appear again
        Clipboard.AsText := '';

        // Show the button thingy..
        UpdateButtonOpenClipboard();

        // Save registry
        SaveInRegistry(true);

        // Delete the file..
        SaveClipboardFile('Emptied by user.', true);

    end;

end;

procedure TClipifiedForm.MainPopupExitClick(Sender: TObject);
begin
    // die bastard...
    Self.Close;
end;

procedure TClipifiedForm.MainPopupZoomClick(Sender: TObject);
var zoom : TZoomForm;
begin
    // I should be always UP
    MakeWindowStayOnTop;

    // On the fly..
    zoom := TZoomForm.Create(Self);
    try
        // Move it where I just like it
        zoom.Top := Self.Top + Self.Height + 5;
        zoom.Left := Self.Left;
        // show it..
        zoom.ShowModal;
    finally
        FreeAndNil(zoom);
    end;
end;

procedure TClipifiedForm.FormDropFiles(Sender: TObject; const FileNames: array of String);
var iterator   : Integer;
    theFile    : string;
    textStrings: TStringList;
    textBuffer : string;
begin
    try
        // loop
        for iterator := 0 to Length(FileNames) - 1 do begin

            // who are you?
            theFile := FileNames[iterator];

            // If this is a file.. otherwise.. stop..
            if (not DirectoryExists(theFile)) and (not FileExists(theFile)) then begin
                // Message
                ShowWarning('Unreachable: ' + ExtractFileNameOnly(theFile));
                // next?
                CONTINUE;
            end;

            // Message
            ShowInfo(ExtractFileNameOnly(theFile));

            // First.. I add it to the list.. so becomes part of the history
            ClipboardHistory.Insert( ClipboardHistory.CreateContentFromText(theFile) );

            // Apply on clipboard
            ClipboardHistory.NewestItem.ApplyToClipboard;

            // Refresh the clipboard
            UpdateButtonOpenClipboard();

            // Is text .. I copy to the clipboard
            if  (FileExists(theFile)) and (FileIsText(theFile)) then begin

                // one go...
                textStrings := TStringList.Create;
                try
                    // load....
                    textStrings.LoadFromFile(theFile);

                    // cached
                    textBuffer := textStrings.Text;

                    // Load in list so there is not message..
                    ClipboardHistory.Insert(ClipboardHistory.CreateContentFromText(textBuffer));

                    // Copy to the clipboard.. magic!  (which in time copies to the history and so...)
                    ClipboardHistory.NewestItem.ApplyToClipboard;

                    // Refresh the clipboard
                    UpdateButtonOpenClipboard();

                    // Message (before the other one by timer)
                    ShowInfo(MESSAGE_CLIPBOARD_FILE_LOADED);

                finally
                    // release..
                    FreeAndNil(textStrings);

                end;

            end;

        end;

        // Probably did changes..
        SaveInRegistry(false);

    except

        // Message
        ShowWarning('Can''t load file');

    end;

end;

procedure TClipifiedForm.UpdateButtonOpenClipboard();
begin
    // Sync..
    if (ClipboardHistory.IsEmpty())
        then BtnOpenClipboard.Caption := ''
        else BtnOpenClipboard.Caption := ' ' + ClipboardHistory.NewestItem.ToStringOneLine;
end;

procedure TClipifiedForm.LoadFromRegistry();
var
    registry : TRegistry;
begin
    // Sometimes the file is broken or something, ignore.. mainly debug...
    try

        // Create the object
        registry := TRegistry.Create();
        try
            registry.RootKey := HKEY_CURRENT_USER;
            if (registry.OpenKey(REGISTRY_RELATIVE_PATH, true)) then begin

                // if saved.. reload..
                if (registry.ValueExists(REGISTRY_WINDOWS_TOP_POSITION))  then Self.Top  := registry.ReadInteger(REGISTRY_WINDOWS_TOP_POSITION);
                if (registry.ValueExists(REGISTRY_WINDOWS_LEFT_POSITION)) then Self.Left := registry.ReadInteger(REGISTRY_WINDOWS_LEFT_POSITION);

                // Desktop behaviour
                if (registry.ValueExists(REGISTRY_MINIMAL_WINDOW_SIZE))  then UGlobals.UseMinimalWindowSize := StrToBool(registry.ReadString(REGISTRY_MINIMAL_WINDOW_SIZE));
                if (registry.ValueExists(REGISTRY_SHOW_TRAY_ICON))       then UGlobals.ShowTrayIcon         := StrToBool(registry.ReadString(REGISTRY_SHOW_TRAY_ICON));
                if (registry.ValueExists(REGISTRY_SHOW_BALLOON_HINTS))   then UGlobals.ShowBalloonHints     := StrToBool(registry.ReadString(REGISTRY_SHOW_BALLOON_HINTS));
                // Automatically trim
                if (registry.ValueExists(REGISTRY_AUTOMATIC_TRIM))       then UGlobals.AutomaticTrimNL      := StrToBool(registry.ReadString(REGISTRY_AUTOMATIC_TRIM));
                // Backup stuff
                if (registry.ValueExists(REGISTRY_BACKUP_SOURCE_DIR))    then UGlobals.BackupSourceDir      := registry.ReadString(REGISTRY_BACKUP_SOURCE_DIR);
                if (registry.ValueExists(REGISTRY_BACKUP_TARGET_FILE))   then UGlobals.BackupTargetFile     := registry.ReadString(REGISTRY_BACKUP_TARGET_FILE);
                if (registry.ValueExists(REGISTRY_BACKUP_TIME))          then UGlobals.BackupTime           := StrToTime(registry.ReadString(REGISTRY_BACKUP_TIME));

                // Text entered by the user
                if (registry.ValueExists(REGISTRY_SAVED_NOTES)) then Self.FSavedNotes := registry.ReadString(REGISTRY_SAVED_NOTES);

                // Save the last time I had a break.. so I restart and I dont loose it..
                if (registry.ValueExists(REGISTRY_LAST_BREAK)) then Self.FLastBreakTaken := registry.ReadDateTime(REGISTRY_LAST_BREAK);
                // If saved long ago.. forget it
                if (Self.FLastBreakTaken < (Now() - OneHour)) then Self.FLastBreakTaken := Now();

                { try to load the old items..}
                ClipboardHistory.LoadContentFromRegistry(registry);

                // Sync..
                UpdateButtonOpenClipboard();

            end;

            // just in ..
            registry.CloseKey;

        finally
            // let go!
            FreeAndNil(registry);

        end;

    except
        ShowMessage('Registry error!');

    end;
end;

procedure TClipifiedForm.SaveInRegistry(ForceRewrite : Boolean);
var
    registry : TRegistry;
begin
    // Create the object
    registry := TRegistry.Create();
    try

        registry.RootKey := HKEY_CURRENT_USER;

        // blow away
        if (ForceRewrite) then registry.DeleteKey(REGISTRY_RELATIVE_PATH);

        // Save each
        if (registry.OpenKey(REGISTRY_RELATIVE_PATH, true)) then begin

            // starting point
            registry.WriteInteger(REGISTRY_WINDOWS_TOP_POSITION,  Self.Top);
            registry.WriteInteger(REGISTRY_WINDOWS_LEFT_POSITION, Self.Left);

            // Desktop behaviour
            registry.WriteString(REGISTRY_MINIMAL_WINDOW_SIZE,  BoolToStr(UGlobals.UseMinimalWindowSize));
            registry.WriteString(REGISTRY_SHOW_TRAY_ICON,       BoolToStr(UGlobals.ShowTrayIcon));
            registry.WriteString(REGISTRY_SHOW_BALLOON_HINTS,   BoolToStr(UGlobals.ShowBalloonHints));
            // Automatically trim
            registry.WriteString(REGISTRY_AUTOMATIC_TRIM,       BoolToStr(UGlobals.AutomaticTrimNL));
            // Backup stuff
            registry.WriteString(REGISTRY_BACKUP_SOURCE_DIR,    UGlobals.BackupSourceDir);
            registry.WriteString(REGISTRY_BACKUP_TARGET_FILE,   UGlobals.BackupTargetFile);
            registry.WriteString(REGISTRY_BACKUP_TIME,          TimeToStr(UGlobals.BackupTime));

            // Text entered by the user
            registry.WriteString(REGISTRY_SAVED_NOTES, Self.FSavedNotes);

            // Save the last time I had a break.. so I restart and I dont loose it..
            registry.WriteDateTime(REGISTRY_LAST_BREAK, FLastBreakTaken);

            { try to load the old items.. }
            ClipboardHistory.SaveContentToRegistry(registry);

        end;

        // just in ..
        registry.CloseKey;

    finally
        // let go!
        FreeAndNil(registry);
    end;
end;

procedure TClipifiedForm.SaveClipboardFile(TextToSave : string; ForceRewrite : Boolean);
var
    fileName : string;
    backupFile : TextFile;
begin
    try
        { Although it is slow.. the truth is that is compared after the clipboard is copied and is different, so time is not important }
        fileName := (GetTempDir() + APP_NAME + '.txt');

        // Assign and reopen
        AssignFile(backupFile, fileName);

        try
            // Usually I keep.. or.. I may go crazy
            if (not FileExists(fileName) or ForceRewrite) then Rewrite(backupFile) else Append(backupFile);

            // Save this separators each time..
            WriteLn(backupFile, '----------' + FormatDateTime('dd-Mmm-yyyy hh:nn:ss', Now()) + '----------');
            WriteLn(backupFile, '');
            WriteLn(backupFile, TextToSave);
            WriteLn(backupFile, '');
            Flush(backupFile);

        finally
            // no need any more..
            CloseFile(backupFile);
        end;

    except
        // no much to do
        ShowMessage('Can''t save file');
    end;

end;

procedure TClipifiedForm.RefreshEventTimer(Sender: TObject);
var screenSaverActive : LongBool;
begin
    // The same processing for timer or if in Windows I get notified
    if (FClipboardListener = NIL) or (FClipboardListener.ClipboardChangeFound) or (not FClipboardListener.Supported) then AnalyseClipboard;

    // got to zero... release...
    if (FMessageDelayTime > 0) then begin

        // each tick reduce the wait...
        FMessageDelayTime := Max(0, FMessageDelayTime - RefreshEvent.Interval);

    end else begin

        // This only returns previous state if the form is not active
        //if ((GetKeyState(VK_CAPITAL) << 0001) <> 0) then keysInfo := keysInfo + '[CAPS]' else keysInfo := keysInfo + '[low]';

        // In Windows seems to stay
        if (AppTrayIcon.ShowIcon) and (AppTrayIcon.BalloonHint <> '') then begin
            AppTrayIcon.BalloonHint := '';
            AppTrayIcon.Hide;
            AppTrayIcon.Show;
        end;

        // Clear old message
        if (not ClipboardHistory.IsEmpty) then ShowMessage( ClipboardHistory.NewestItem.ToStringExtraInfo );

        { THIS IS HERE BECAUSE I CAN PAUSE THE TIMER BUT NOT APPLICATION.IDLE }

        // default.. avoid the message if the function fails (Linux or error)
        screenSaverActive := false; // {$IFDEF UNIX} false; {$ENDIF} {$IFDEF WINDOWS} true; {$ENDIF};
        // Do the magic
        SystemParametersInfo(SPI_GETSCREENSAVERRUNNING, 0, @screenSaverActive, 0);
        // If the screensaver is active.. reset the timer, otherwise .. increase and popup message
        if (screenSaverActive) then begin

            // Keep this variable as the last very second it was not working..
            FAccumulatedWork := FAccumulatedWork + Max(0.0, FLastActivitySeen - FLastBreakTaken);
            FLastBreakTaken  := Now();

        end else begin

            // Means.. disabled
            if (FPauseChosen <> BREAK_ALERT_DISABLED) then begin

                // Decide if I annoy you..
                if ((FLastBreakTaken + FPauseChosen) < Now()) then begin

                    // Put this into the future so it does not
                    FLastBreakTaken := Now() + 100; // Windows seems to continue after MessageDlg, so I block a loop

                    // annoy me!
                    if (MessageDlg(PAUSE_REMINDER_TIME_IS_UP, mtInformation, [mbCancel], 0) = mrCancel) then begin

                        // The time of the break finished
                        FLastBreakTaken := Now();

                    end;

                end else if ((FLastBreakTaken + FPauseChosen - WARN_TIME_BEFORE_BREAK) < Now()) then begin

                    // Show the message
                    ShowWarning(PAUSE_REMINDER_TIME_ALMOST_UP);

                end;

            end;

        end;

    end;

end;

procedure TClipifiedForm.AnalyseClipboard;
var currentClipboard      : TClipboardContent;
    hasSomethingNewToShow : Boolean;
begin

    try
        // Due to timer I found it coming before initialization
        if (ClipboardHistory = NIL) then EXIT;

        // I flag (Windows specific) that I done it
        if (FClipboardListener <> NIL) then FClipboardListener.ClipboardChangeProcessed;

        // Anything goes wrong.. release it always..
        try

            // Load each time in this method..
            currentClipboard := ClipboardHistory.CreateContentFromClipboard;

            // Nothing loaded, or this was already
            if (currentClipboard <> NIL) then begin

                // BIG items.. tend to end up locking the clipboard.. so for those I wait a bit.
                RefreshEvent.Interval := currentClipboard.refreshRate;

                // has something before (most times)
                hasSomethingNewToShow := (ClipboardHistory.HasItems) and (not ClipboardHistory.NewestItem.Equals( currentClipboard ));

                // notification if required...
                if (ClipboardHistory.IsEmpty or hasSomethingNewToShow) then begin

                    // Is a number?
                    if (UGlobals.AutomaticTrimNL) then currentClipboard.TrimAndReapply;

                    // Remove the notifications and I cannot save..
                    ClipboardHistory.DeleteTopUselessContent;

                    // Delete if existed before
                    if (ClipboardHistory.Contains(currentClipboard)) then begin

                        { EXISTED.. DO NOT CHANGE COLOR BECAUSE I DID IN THE OTHER MENU}

                        // from list wherever it is..
                        ClipboardHistory.Delete( ClipboardHistory.IndexOf(currentClipboard) );

                        // add in list at the top
                        ClipboardHistory.Insert(currentClipboard);

                        // Just to be sure if I crash I can survive..
                        SaveInRegistry(false);

                        // Notified
                        ShowInfo(MESSAGE_CLIPBOARD_RESTORED);

                    end else begin

                        { REALLY NEW CONTENT... }

                        // if now happens to have too many... still.. must kill the least important..
                        if (ClipboardHistory.IsFull) then ClipboardHistory.Delete( ClipboardHistory.MostIrrelevantObject );

                        // add in list
                        ClipboardHistory.Insert(currentClipboard);

                        // Just to be sure if I crash I can survive..
                        SaveInRegistry(false);

                        // Show..
                        ShowWarning(MESSAGE_CLIPBOARD_STORED);

                        // Make it obvious
                        if (UGlobals.ShowBalloonHints) then begin
                            AppTrayIcon.BalloonTitle   := APP_DESC;
                            AppTrayIcon.BalloonHint    := currentClipboard.ToStringShorten;
                            AppTrayIcon.BalloonFlags   := bfInfo;
                            AppTrayIcon.BalloonTimeout := TIMER_SHOW_MESSAGE_LONG;
                            AppTrayIcon.ShowIcon       := true;
                            AppTrayIcon.ShowBalloonHint;
                        end;

                        // append to the file
                        SaveClipboardFile(currentClipboard.ToString, false);
                    end;

                    // I added it.. so.. I dont want to delete it..
                    currentClipboard := NIL;

                    // Update the button..
                    UpdateButtonOpenClipboard();

                    // Just did something
                    FLastActivitySeen := Now();

                end;

            end;

        finally
            // I always delete if I get here.. because.. it may be the copy not the original..
            FreeAndNil(currentClipboard);

        end;

    except
        // Since this is in a timer, I will destroy any exception because otherwise they will compound
        SetClipboard(NIL);

    end;
end;

procedure TClipifiedForm.MakeBackupIfNeeded;
var HourUser, MinuteUser, SecondUser, MilliSecondUser: word;
    HourNow,  MinuteNow,  SecondNow,  MilliSecondNow: word;
begin
    // Nothing.. stop
    if (UGlobals.BackupSourceDir = '') and (UGlobals.BackupTargetFile = '') then EXIT;
    // Not valid.. say it straight away
    if (UGlobals.ValidateBackupParam(UGlobals.BackupSourceDir, UGlobals.BackupTargetFile)) then begin
        // Get HH:MM
        DecodeTime(Now(), HourNow, MinuteNow, SecondNow, MilliSecondNow);
        // Get HH:MM
        DecodeTime(UGlobals.BackupTime, HourUser, MinuteUser, SecondUser, MilliSecondUser);
        // Right time?
        if (HourNow = HourUser) and (MinuteNow = MinuteUser) then begin
            // Message..
            ClipifiedForm.ShowInfo('Zip starting..');
            // Go
            UGlobals.BackupFile(UGlobals.BackupSourceDir, UGlobals.BackupTargetFile, @ZipFileStart);
            // Message..
            ClipifiedForm.ShowInfo('Zip completed..');
        end;
    end else begin
        ClipifiedForm.ShowWarning('Zip parameters invalid.');
    end;
end;

procedure TClipifiedForm.ZipFileStart(Sender : TObject; Const AFileName : String);
begin
    ShowInfo('Zipping ' + ExtractFileName(AFileName)+ '..');
end;

end.

