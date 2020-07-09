unit UClipboardContent;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Registry, Graphics, Math, LResources, ExtCtrls, Forms,
  Clipbrd, LCLType, DateUtils, Dialogs, StrUtils, LCLIntf,
  UGlobals, fpexprpars;

type

    { Representing the clipboard content }

    TClipboardContent = class(TObject)
    protected
        FCreation: TDateTime;
        function CreationAsTextIfNeeded: string;
    public
        constructor Create();
        function  Equals(Obj: TObject) : Boolean; override;
        function  ToString : AnsiString; override;
        function  ToStringExtraInfo : AnsiString; virtual;
        function  ToStringShorten : AnsiString; {FINAL}
        function  ToStringOneLine : AnsiString; {FINAL}
        function  UselessContent : Boolean; virtual;
    public
        procedure ApplyToClipboard; virtual; abstract;
        function  GetActionGroup: TContentActionGroup; virtual;
        procedure DoAction; virtual;
        procedure TrimAndReapply; virtual; // only in very few...
        function  refreshRate: Cardinal; virtual; // some clipboard content may be a bit slow
    public
        procedure SaveToRegistry(Registry: TRegistry; Index: Integer); virtual; abstract;
    end;

    { Representing an object I cannot store... }

    TClipboardUnhandledContent = class(TClipboardContent)
    protected
        FMessage: String;
    public
        constructor CreateEmptyClipboardMessage;
        constructor CreateObjectTooBigMessage;
        constructor CreateUnknownObject;
        function    Equals(Obj: TObject) : Boolean; override;
        function    ToString : AnsiString; override;
        function    UselessContent : Boolean; override;
    public
        procedure   ApplyToClipboard; override;
        function    refreshRate: Cardinal; override;
    public
        procedure   SaveToRegistry(Registry: TRegistry; Index: Integer); override;
    end;

    { Representing text... }

    TClipboardContentText = class(TClipboardContent)
    protected
        FText: String;
    public
        constructor Create(Text: String);
        function    Equals(Obj: TObject) : Boolean; override;
        function    ToString : AnsiString; override;
        function    ToStringExtraInfo : AnsiString; override;
    public
        procedure   ApplyToClipboard; override;
    public
        procedure   SaveToRegistry(Registry: TRegistry; Index: Integer); override;
    end;

    { Extra goodies }

    TClipboardContentFilePath = class(TClipboardContentText)
    public
        function  GetActionGroup: TContentActionGroup; override;
        procedure DoAction; override;
        procedure TrimAndReapply; override;
    end;

    TClipboardContentURL = class(TClipboardContentText)
    public
        function    GetActionGroup: TContentActionGroup; override;
        procedure   DoAction; override;
        procedure   TrimAndReapply; override;
    end;

    TClipboardContentEmails = class(TClipboardContentText)
    public
        function    GetActionGroup: TContentActionGroup; override;
        procedure   DoAction; override;
        procedure   TrimAndReapply; override;
    end;

    TClipboardContentNumberID = class(TClipboardContentText)
    public
        function  ToString : AnsiString; override;
        function  GetActionGroup: TContentActionGroup; override;
        procedure TrimAndReapply; override;
    end;

    { Representing a picture... }

    TClipboardContentPicture = class(TClipboardContent)
    private
        FPicture: TPicture;
        FFileName: string;
    public
        constructor Create(Picture: TPicture);
        constructor CreateBitmapFromClipboard();
        constructor CreatePixmapFromClipboard();
        constructor CreatePictureFromClipboard();
        destructor  Destroy; override;
        function    Equals(Obj: TObject) : Boolean; override;
        function    ToString : AnsiString; override;
        function    UselessContent : Boolean; override;
    public
        procedure   ApplyToClipboard; override;
        function    GetActionGroup: TContentActionGroup; override;
        function    refreshRate: Cardinal; override;
    public
        procedure   SaveToRegistry(Registry: TRegistry; Index: Integer); override;
        class function CreatePictureFromRegistry(Registry: TRegistry; KeyName: string): TPicture; static;
    end;

    { Representing the history of the clipboard }

    TClipboardHistory = class(TObject)
    private
        FCount: Integer;
        FItems: Array[0 .. MAX_ITEMS_IN_CLIPBOARD_TO_REMEMBER - 1] of TClipboardContent;
    private
        procedure   DeleteFromRegistry(Registry: TRegistry; Index: Integer);
        function    LoadItemFromRegistry(Registry: TRegistry; Index: Integer) : TClipboardContent;
        procedure   SaveItemToRegistry(Registry: TRegistry; Index: Integer; Item: TClipboardContent);
        function    GetItem(SearchIndex: Integer): TClipboardContent; inline;
        function    GetCapacity: Integer; inline;
        function    GetNewestItem(): TClipboardContent; inline;
        function    GetMostIrrelevantObject: TClipboardContent; inline;
    protected
        class function BuildKeyNameForItemType(Index: Integer): string; static; inline;
        class function BuildKeyNameForItemNumber(Index: Integer): string; static; inline;
    public
        constructor Create;
        destructor  Destroy; override;
    public
        property    Count: Integer read FCount;
        property    Capacity: Integer read GetCapacity;
        property    Items[SearchIndex: Integer]: TClipboardContent read GetItem;
        property    NewestItem: TClipboardContent read GetNewestItem;
        property    MostIrrelevantObject: TClipboardContent read GetMostIrrelevantObject;
    public
        function    IndexOf(SearchItem: TClipboardContent): Integer;
        function    Contains(SearchItem: TClipboardContent): Boolean;
        function    HasItems: Boolean;
        function    IsEmpty: Boolean;
        function    IsFull: Boolean;
        procedure   Insert(NewItem: TClipboardContent);
        procedure   Append(NewItem: TClipboardContent);
        procedure   Delete(OldItem: TClipboardContent);
        procedure   Delete(OldIndex: Integer);
        procedure   Clear;
        procedure   DeleteTopUselessContent;
    public
        procedure   LoadContentFromRegistry(Registry: TRegistry);
        procedure   SaveContentToRegistry(Registry: TRegistry);
    public
        class function CreateContentFromClipboard(): TClipboardContent; static;
        class function CreateContentFromText(OriginalText: string): TClipboardContentText; static;
    end;

var

  { Representing the history of the clipboard }

  ClipboardHistory : TClipboardHistory;

implementation

    { Representing the clipboard content }

    constructor TClipboardContent.Create();
    begin
        inherited Create;
        Self.FCreation := Now();
    end;

    function TClipboardContent.Equals(Obj: TObject) : Boolean;
    begin
        {$IfOpt D+}
        raise EAbstractError.Create('You must override this method.');
        {$EndIf}
        // It's me!
        result := (Self = Obj);
    end;

    function TClipboardContent.ToString : AnsiString;
    begin
        {$IfOpt D+}
        raise EAbstractError.Create('You must override this method.');
        {$EndIf}
        result := '';
    end;

    function TClipboardContent.CreationAsTextIfNeeded: string;
    begin
        // recent.. dont show..
        if (FCreation > (Now() - OneMinute * 2)) then begin
            // nothing..
            result := '';
        end else begin
            // yesterday or before.. show day
            if (FCreation < Date())
               then result := ' on ' + FormatDateTime('dd-Mmm hh:mm', FCreation)
               else result := ' at ' + FormatDateTime('hh:mm', FCreation);
        end;
    end;

    function TClipboardContent.ToStringShorten: AnsiString;
    begin
        result := ToString;
        if (Length(result) > MAX_TEXT_DISPLAY_LENGTH)
           then result := Copy(Trim(result), 1, MAX_TEXT_DISPLAY_LENGTH) + '...';
    end;

    function TClipboardContent.ToStringOneLine : AnsiString;
    begin
        // Trim after I replace the ENTER for ¶
        result := Trim(AnsiReplaceText(ToStringShorten, LineEnding, '¶'));
    end;

    function TClipboardContent.ToStringExtraInfo : AnsiString;
    begin
        result := ContentActionGroupNames[GetActionGroup()];
    end;

    function  TClipboardContent.GetActionGroup: TContentActionGroup;
    begin
        result := cagNone; // Don't show
    end;

    procedure TClipboardContent.DoAction;
    begin
        // Shouldn't need anything special.. but for the children... just do as normal selection
        ApplyToClipboard;
    end;

    procedure TClipboardContent.TrimAndReapply;
    begin
        // By default will do NOTHING.. not touching the Clipboard or anything.. only children can decide
    end;

    function  TClipboardContent.refreshRate: cardinal;
    begin
        result := TIMER_REFRESH_SHORT;
    end;

    function  TClipboardContent.UselessContent : Boolean;
    begin
        result := false; // By default is all .. awesome!
    end;

    { Representing text... }

    constructor TClipboardContentText.Create(Text: String);
    begin
        inherited Create();
        Self.FText := Text;
    end;

    function TClipboardContentText.Equals(Obj: TObject) : Boolean;
    begin
        // It's me!
        if (Self = Obj)
            then result := true
            else result := (Obj <> NIL) and (Self.ClassType = Obj.ClassType) and (SameText(FText , TClipboardContentText(Obj).FText));
    end;

    function TClipboardContentText.ToString : AnsiString;
    begin
        result := Self.FText;
    end;

    function TClipboardContentText.ToStringExtraInfo : AnsiString;
    begin
        if (GetActionGroup() = cagNone)
           then result := IntToStr(Length(Self.FText)) + ' chars' + CreationAsTextIfNeeded
           else result := inherited;
    end;

    procedure TClipboardContentText.ApplyToClipboard;
    begin
        Clipboard.AsText := Self.FText;
    end;

    procedure TClipboardContentText.SaveToRegistry(Registry: TRegistry; Index: Integer);
    var fullText: string;
    begin
        fullText := Self.ToString;
        if (Length(fullText) < Power(2, 16) { 65K +/-}) then begin
            // save the FText
            Registry.WriteString(TClipboardHistory.BuildKeyNameForItemNumber(Index), fullText);
            // save the type
            //Registry.WriteString(TClipboardHistory.BuildKeyNameForItemType(Index), RegistryTypeNames[rtText]); optional
        end;
    end;

    { Representing an object I cannot store... }

    constructor TClipboardUnhandledContent.CreateEmptyClipboardMessage;
    begin
        inherited Create();
        Self.FMessage := '{Empty Clipboard}';
    end;

    constructor TClipboardUnhandledContent.CreateObjectTooBigMessage;
    begin
        inherited Create();
        Self.FMessage := '{Object too big}';
    end;

    constructor TClipboardUnhandledContent.CreateUnknownObject;
    begin
        inherited Create();
        Self.FMessage := '{Unknown object}';
    end;

    function TClipboardUnhandledContent.Equals(Obj: TObject) : Boolean;
    begin
        // It's me!
        if (Self = Obj)
           then result := true
           else result := (Obj <> nil) and (Self.ClassType = Obj.ClassType) and (SameText(FMessage , TClipboardUnhandledContent(Obj).FMessage));
    end;

    function TClipboardUnhandledContent.ToString : AnsiString;
    begin
        result := Self.FMessage;
    end;

    procedure TClipboardUnhandledContent.ApplyToClipboard;
    begin
        // Can do nothing.. should never happen..
    end;

    function  TClipboardUnhandledContent.refreshRate: Cardinal;
    begin
        result := TIMER_REFRESH_LONG;
    end;

    procedure TClipboardUnhandledContent.SaveToRegistry(Registry: TRegistry; Index: Integer);
    begin
        // Can do nothing.. should never happen..
    end;

    function TClipboardUnhandledContent.UselessContent : Boolean;
    begin
        result := true; // you guessed.. KILL ME!
    end;

    { File or Path }

    function TClipboardContentFilePath.GetActionGroup: TContentActionGroup;
    begin
        result := cagFilePath;
    end;

    procedure TClipboardContentFilePath.DoAction;
    begin
        // Just paste back
        OpenDocument(FText);
        // After removing spaces.. put it in the clipboard again..
        ApplyToClipboard;
    end;

    procedure TClipboardContentFilePath.TrimAndReapply;
    begin
        // Only if different..
        if (Self.FText <> Trim(Self.FText)) then begin
            // Chop..
            Self.FText := Trim(Self.FText);
            // After removing spaces.. put it in the clipboard again..
            ApplyToClipboard;
        end;
    end;

    { URL }

    function TClipboardContentURL.GetActionGroup: TContentActionGroup;
    begin
        result := cagURL;
    end;

    procedure TClipboardContentURL.DoAction;
    begin
        // And open the URL
        OpenURL(FText);
        // After removing spaces.. put it in the clipboard again..
        ApplyToClipboard;
    end;

    procedure TClipboardContentURL.TrimAndReapply;
    begin
        // Only if different..
        if (Self.FText <> Trim(Self.FText)) then begin
            // Chop..
            Self.FText := Trim(Self.FText);
            // After removing spaces.. put it in the clipboard again..
            ApplyToClipboard;
        end;
    end;

    { Email content }

    function TClipboardContentEmails.GetActionGroup: TContentActionGroup;
    begin
        result := cagEmails;
    end;

    procedure TClipboardContentEmails.DoAction;
    begin
        // And open the URL
        OpenURL('mailto:' + FText + '?subject=' + 'Email from ' + APP_DESC + '&body=' + ClipboardHistory.NewestItem.ToString);
    end;

    procedure TClipboardContentEmails.TrimAndReapply;
    begin
        // Only if different..
        if (Self.FText <> Trim(Self.FText)) then begin
            // Chop..
            Self.FText := Trim(Self.FText);
            // After removing spaces.. put it in the clipboard again..
            ApplyToClipboard;
        end;
    end;

    { Numeric ID }

    procedure TClipboardContentNumberID.TrimAndReapply;
    begin
        // Only if different..
        if (Self.FText <> Trim(Self.FText)) then begin
            // Chop..
            Self.FText := Trim(Self.FText);
            // Trim
            Self.FText := AnsiReplaceText(FText, LineEnding, '');
            // Force a change..
            ApplyToClipboard;
        end;
    end;

    function TClipboardContentNumberID.ToString : AnsiString;
    begin
        result := inherited + CreationAsTextIfNeeded;
    end;

    function TClipboardContentNumberID.GetActionGroup: TContentActionGroup;
    begin
        result := cagNumberId;
    end;

    { Representing a picture... }

    constructor TClipboardContentPicture.Create(Picture: TPicture);
    begin
        inherited Create();
        FPicture := Picture;
    end;

    constructor TClipboardContentPicture.CreateBitmapFromClipboard();
    begin
        inherited Create();
        FPicture := TPicture.Create;
        FPicture.Bitmap.Assign(Clipboard);
    end;

    constructor TClipboardContentPicture.CreatePixmapFromClipboard();
    begin
        inherited Create();
        FPicture := TPicture.Create;
        FPicture.Pixmap.Assign(Clipboard);
    end;

    constructor TClipboardContentPicture.CreatePictureFromClipboard();
    begin
        inherited Create();
        FPicture := TPicture.Create;
        FPicture.Assign(Clipboard);
    end;

    destructor TClipboardContentPicture.Destroy;
    begin
        FreeAndNil(FPicture);
        inherited Destroy;
    end;

    function TClipboardContentPicture.Equals(Obj: TObject) : Boolean;
    var other : TClipboardContentPicture;
    begin
        // Doesnt work.. may not be implemented..
        // if (FPicture.Equals(other)) then begin result := true; EXIT; end;

        // It's me!
        if (Self = Obj)
           then result := true
           else begin
               // default
               result := false;

                // compare all
                if (Obj <> nil) and (Self.ClassType = Obj.ClassType) then begin

                    // cached..
                    other := TClipboardContentPicture(Obj);

                    // they dont even match size... nop
                    // if the With and Height is the same.. I assume is the same.. for performance reasons
                    // Unlikely they put exactly the same image exactly the same size, and if they did, I keep one anyway
                    result := (FPicture.Width = other.FPicture.Width) and (FPicture.Height = other.FPicture.Height);
                end;
           end;
    end;

    function TClipboardContentPicture.ToString : AnsiString;
    begin
        result := 'Image ' + WidthHeightToText(FPicture.Width, FPicture.Height) + CreationAsTextIfNeeded;
    end;

    procedure TClipboardContentPicture.ApplyToClipboard;
    begin
        Clipboard.Assign(FPicture);
    end;

    function TClipboardContentPicture.GetActionGroup: TContentActionGroup;
    begin
        result := cagPicture;
    end;

    function TClipboardContentPicture.refreshRate: Cardinal;
    begin
        result := TIMER_REFRESH_LONG;
    end;

    function TClipboardContentPicture.UselessContent : Boolean;
    begin
        result := (FPicture = NIL) or (FPicture.Width = 0) or (FPicture.Height = 0);
    end;


    procedure TClipboardContentPicture.SaveToRegistry(Registry: TRegistry; Index: Integer);
    begin
        // if I already saved it.. just ignore the saving file part
        if (FFileName = '') or (not FileExists(FFileName)) then begin
            // Decide a name.. once set.. i wont retry
            FFileName := (GetTempDir() + APP_NAME + '_' + FormatDateTime('yyyyMmddhhmmss', FCreation)+ '.bmp');
            // Build it
            FPicture.SaveToFile(FFileName);
        end;
        // save the image first.. in case it fails..
        Registry.WriteString(TClipboardHistory.BuildKeyNameForItemNumber(Index), FFileName);
        // save the type after if all worked
        Registry.WriteString(TClipboardHistory.BuildKeyNameForItemType(Index), RegistryTypeNames[rtPicture]);

    end;

    class function TClipboardContentPicture.CreatePictureFromRegistry(Registry: TRegistry; KeyName: string): TPicture;
    var tempFileName: string;
    begin
        // default
        result := NIL;
        // Hope works..
        tempFileName := Registry.ReadString(KeyName);
        // Do you exist?
        if (FileExists(tempFileName)) then begin
            // Create
            result := TPicture.Create;
            // Load
            result.LoadFromFile( tempFileName );
        end
    end;

    { Representing the history of the clipboard }

    constructor TClipboardHistory.Create;
    begin
        inherited Create;
    end;

    destructor TClipboardHistory.Destroy;
    begin
        Clear;
        inherited Destroy;
    end;

    function TClipboardHistory.GetCapacity: Integer;
    begin
        result := MAX_ITEMS_IN_CLIPBOARD_TO_REMEMBER;
    end;

    function TClipboardHistory.GetItem(SearchIndex: Integer): TClipboardContent;
    begin
        result := FItems[SearchIndex];
    end;

    function TClipboardHistory.IndexOf(SearchItem: TClipboardContent): Integer;
    var iterator: Integer;
    begin
        result := -1;
        for iterator := 0 to FCount - 1 do begin
            if (FItems[iterator].Equals(SearchItem)) then begin
                result := iterator;
                EXIT;
            end;
        end;
    end;

    function TClipboardHistory.Contains(SearchItem: TClipboardContent): Boolean;
    var iterator: Integer;
    begin
        result := false;
        for iterator := 0 to FCount - 1 do begin
            result := FItems[iterator].Equals(SearchItem);
            if (result) then EXIT;
        end;
    end;

    function TClipboardHistory.HasItems: Boolean;
    begin
        result := (FCount > 0);
    end;

    function TClipboardHistory.IsEmpty: Boolean;
    begin
        result := (FCount = 0);
    end;

    function TClipboardHistory.IsFull: Boolean;
    begin
        result := (Count >= Capacity);
    end;

    procedure TClipboardHistory.Insert(NewItem: TClipboardContent);
    var iterator: Integer;
    begin
        if (not Contains(NewItem)) then begin
            for iterator := FCount - 1 downto 0 do begin
                FItems[iterator + 1] := FItems[iterator];
            end;
            FCount := FCount + 1;
            FItems[0] := NewItem;
        end;
    end;

    procedure TClipboardHistory.Append(NewItem: TClipboardContent);
    begin
        if (not Contains(NewItem)) then begin
            FItems[FCount] := NewItem;
            FCount := FCount + 1;
        end;
    end;

    procedure TClipboardHistory.Delete(OldIndex: Integer);
    var iterator: Integer;
        oldItem: TClipboardContent;
        oldActionGroup : TContentActionGroup;
    begin
        // Take it out before moving..
        oldItem := FItems[OldIndex];
        // Remove first.. so next time doesnt failt any more
        if (OldIndex >= 0) and (OldIndex <= FCount -1) then begin
            for iterator := OldIndex to (FCount - 1) - 1 do begin
                FItems[iterator] := FItems[iterator + 1];
            end;
            FItems[FCount - 1] := NIL;
        end;
        FCount := FCount - 1;

        // Default...
        oldActionGroup := cagNone;
        try
            // Type
            oldActionGroup := OldItem.GetActionGroup;
            // Blow this one
            FreeAndNil(OldItem);
        except
            // Forget it anyway.. cant save it..
            MessageDlg('Error deleting ' + ContentActionGroupNames[oldActionGroup], mtConfirmation, [mbClose], 0);
        end;
    end;

    procedure TClipboardHistory.Delete(OldItem: TClipboardContent);
    begin
        if (Contains(OldItem)) then Delete( IndexOf(OldItem) );
    end;

    procedure TClipboardHistory.Clear;
    var iterator: Integer;
    begin
        for iterator := 0 to FCount - 1 do FreeAndNil(FItems[iterator]);
        FCount := 0;
    end;

    procedure TClipboardHistory.DeleteFromRegistry(Registry: TRegistry; Index: Integer);
    begin
        // delete anything.. no questions asked..
        Registry.DeleteValue(BuildKeyNameForItemType(Index));
        Registry.DeleteValue(BuildKeyNameForItemNumber(Index));
    end;

    function TClipboardHistory.LoadItemFromRegistry(Registry: TRegistry; Index: Integer) : TClipboardContent;
    var tempPicture: TPicture;
    begin
        // none loaded
        result := nil;

        // try compatible one..
        if (Registry.ValueExists(BuildKeyNameForItemType(Index))) and (Registry.ReadString(BuildKeyNameForItemType(Index)) = RegistryTypeNames[rtText]) then begin
            // load the object
            result := CreateContentFromText(Registry.ReadString(BuildKeyNameForItemNumber(Index)));
            // stop
            EXIT;
        end;

        // is it old text?
        if (not Registry.ValueExists(BuildKeyNameForItemType(Index))) and (Registry.ValueExists(BuildKeyNameForItemNumber(Index))) then begin
            // load the object
            result := CreateContentFromText(Registry.ReadString(BuildKeyNameForItemNumber(Index)));
            // stop
            EXIT;
        end;

        // is it old text?
        if (Registry.ValueExists(BuildKeyNameForItemType(Index))) and (Registry.ReadString(BuildKeyNameForItemType(Index)) = RegistryTypeNames[rtPicture]) then begin
            // load a picture...
            tempPicture := TClipboardContentPicture.CreatePictureFromRegistry(Registry, BuildKeyNameForItemNumber(Index));
            // load the object
            if (tempPicture <> nil) then result := TClipboardContentPicture.Create(tempPicture);
            // stop
            EXIT;
        end;
    end;

    procedure TClipboardHistory.SaveItemToRegistry(Registry: TRegistry; Index: Integer; Item: TClipboardContent);
    begin
        try
            // blow anything prior
            DeleteFromRegistry(Registry, Index);
            // save now.. new format only
            Item.SaveToRegistry(Registry, Index);
        except
            // Delete it.. if fails
            DeleteFromRegistry(Registry, Index);
            // Desperate measure..
            Delete(Index);
        end;
    end;

    class function TClipboardHistory.CreateContentFromText(OriginalText: string): TClipboardContentText;
    var ID: Longint;
        trimmedText: string;
    begin
        // do it once..
        trimmedText := Trim(OriginalText);

        // is it a number? (void massive comparison)
        if (Length(trimmedText) < 50) and (TryStrToInt(trimmedText, ID)) then begin
            // create it
            result := TClipboardContentNumberID.Create(OriginalText);
            // Done
            EXIT;
        end;

        // seems like.. URL?
        if (Length(trimmedText) < 1024) and (AnsiStartsStr('http://', Lowercase(trimmedText)) or AnsiStartsStr('https://', Lowercase(trimmedText))) then begin
            // create it
            result := TClipboardContentURL.Create(OriginalText);
            // Done
            EXIT;
        end;

        // seems like.. URL?
        if (Length(trimmedText) < 120) and (AnsiContainsStr(Lowercase(trimmedText), '@')) then begin
            // create it
            result := TClipboardContentEmails.Create(OriginalText);
            // Done
            EXIT;
        end;

        // is it a path? (void massive comparison)
        if (Length(trimmedText) < 1024) and (DirectoryExists(trimmedText) or FileExists(trimmedText)) then begin
            // create it
            result := TClipboardContentFilePath.Create(OriginalText);
            // Done
            EXIT;
        end;

        // Here I used what I got. not a trimmed version
        result := TClipboardContentText.Create(OriginalText);
    end;

    class function TClipboardHistory.CreateContentFromClipboard(): TClipboardContent;
    var cachedText : string;
    begin
        { If something goes wrong.. or I missed an IF.. }
        result := NIL;

        { Strange stuff.. }
        if (Clipboard.HasFormat(PredefinedClipboardFormat(pcfObject))) or
           (Clipboard.HasFormat(PredefinedClipboardFormat(pcfCustomData))) then begin
            // Flag as unknown
            result := TClipboardUnhandledContent.CreateUnknownObject;
            // Stop
            EXIT;
        end;

        { Uknown format }
        if (Clipboard.FormatCount = 0) then begin
            // Must be empty..
            result := TClipboardUnhandledContent.CreateEmptyClipboardMessage;
            // Stop
            EXIT;
        end;

        { Some sort of text.. }
        if (Clipboard.HasFormat(PredefinedClipboardFormat(pcfText))) then begin

            // cached..
            cachedText := Clipboard.AsText;

            // Too big?
            if (Length(cachedText) > 400000) then begin
                // Just use the default one to stop further processing
                result := TClipboardUnhandledContent.CreateObjectTooBigMessage;
                // Stop
                EXIT;
            end;

            // Nothing..
            if (Length(Trim(cachedText)) = 0) then begin
                // Just use the default one to stop further processing
                result := TClipboardUnhandledContent.CreateEmptyClipboardMessage;
                // Stop
                EXIT;
            end;

            // Use this default text handling.. but may be is an URL or so..
            result := ClipboardHistory.CreateContentFromText(cachedText);
            // Stop
            EXIT;

        end;

        { Images }

        if (Clipboard.HasFormat(PredefinedClipboardFormat(pcfBitmap))) then begin
            // Try to load as it is..
            result := TClipboardContentPicture.CreateBitmapFromClipboard();
            // Stop
            EXIT;
        end;

        if (Clipboard.HasFormat(PredefinedClipboardFormat(pcfPixmap)))  then begin
            // Try to load as it is..
            result := TClipboardContentPicture.CreatePixmapFromClipboard();
            // Stop
            EXIT;
        end;

        if (Clipboard.HasFormat(PredefinedClipboardFormat(pcfPicture))) then begin
            // Try to load as it is..
            result := TClipboardContentPicture.CreatePictureFromClipboard();
            // Stop
            EXIT;
        end;

        { I have to assume I cannot handle the beast }
        result := TClipboardUnhandledContent.CreateUnknownObject;

    end;

    function TClipboardHistory.GetNewestItem(): TClipboardContent;
    begin
        if (FCount = 0)
           then result := NIL
           else result := FItems[0];
    end;

    function TClipboardHistory.GetMostIrrelevantObject: TClipboardContent;
    var iterator : Integer;
        tempContent : TClipboardContent;
        tempAction : TContentActionGroup;
        counterActions : array[TContentActionGroup] of Integer = ( 0, 0, 0, 0, 0, 0 );
    begin
        // nothing.. return nothing..
        if (FCount = 0) then result := NIL;
        // Not at full capacity.. dont judge them
        if (FCount <> Capacity) then result := NIL;

        // Default.. the last one..
        result := FItems[FCount - 1];

        // the ones it has
        for iterator := 0 to FCount - 1 do begin
            // get the item
            tempContent := FItems[iterator];
            // which action is
            tempAction := tempContent.GetActionGroup;
            // Keep track
            counterActions[tempAction] := counterActions[tempAction] + 1;
        end;

        // Look backwards and try to kill one..
        for iterator := FCount - 1 downto 0 do begin
            // get the item
            tempContent := FItems[iterator];
            // which action is
            tempAction := tempContent.GetActionGroup;
            // now.. decide if to kill or not..
            if (counterActions[tempAction] > ContentActionToKeep[tempAction]) then begin
                // Too much.. sorry..
                result := tempContent;
                // End the whole thing
                EXIT;
            end;
        end;
    end;

    procedure TClipboardHistory.DeleteTopUselessContent;
    var tempItem : TClipboardContent;
    begin
        // No items?
        if (IsEmpty) then EXIT;
        // I assume I have one..
        tempItem := NewestItem;
        // If this is crap.. flush..
        if (tempItem.UselessContent) then Delete(tempItem);
    end;

    { item level }

    class function TClipboardHistory.BuildKeyNameForItemType(Index: Integer): string;
    begin
        // Append the number to the type
        result := REGISTRY_CLIPBOARD_ITEM_TYPE + Format('%.3d', [Index]);
    end;

    class function TClipboardHistory.BuildKeyNameForItemNumber(Index: Integer): string;
    begin
        // Append the number to the item
        result := REGISTRY_CLIPBOARD_ITEM_NUMBER + Format('%.3d', [Index]);
    end;

    { Registry }

    procedure TClipboardHistory.LoadContentFromRegistry(Registry: TRegistry);
    var iterator : Integer;
        newItem : TClipboardContent;
    begin
        // I got to assume it was no called before.
        for iterator := 0 to ClipboardHistory.Capacity - 1 do begin
            // none by default
            newItem := LoadItemFromRegistry(Registry, iterator);
            // found one.. add it..
            if (newItem <> nil) then Append(newItem) // loads in order..
        end;
    end;

    procedure TClipboardHistory.SaveContentToRegistry(Registry: TRegistry);
    var iterator : Integer;
    begin
        // the ones it has
        for iterator := 0 to FCount - 1 do begin
            // save dynamically
            SaveItemToRegistry( Registry, iterator, FItems[iterator] );
        end;
        // the remaining must be deleted if any..
        for iterator := FCount to Capacity - 1 do begin
            // remove garbage
            DeleteFromRegistry( Registry, iterator );
        end;
    end;

initialization

    { Representing the history of the clipboard }

    ClipboardHistory := TClipboardHistory.Create;

    //RegisterClass(TImage); Not used any more

finalization

    FreeAndNil(ClipboardHistory);

end.

