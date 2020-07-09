unit UTextTools;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, SynHighlighterDiff, SynHighlighterJava,
    SynEdit, SynEditTypes, SynHighlighterSQL, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    StdCtrls, Buttons, ComCtrls, Clipbrd, Menus, SynEditKeyCmds, SynCompletion,
    LCLType, StrUtils;

type

    { TTextTools }

    TTextTools = class(TForm)
        CancelButton: TBitBtn;
        Images: TImageList;
        MenuItemConfigShowSpaceTab: TMenuItem;
        MenuItemConfigTrimTrailingSpaces: TMenuItem;
        MenuItemConfigTabAsSpaces: TMenuItem;
        MenuItemSquareBrackets: TMenuItem;
        MenuItemBrackets: TMenuItem;
        MenuItemParenthesis: TMenuItem;
        MenuItemDoubleQuote: TMenuItem;
        MenuItemSimpleQuote: TMenuItem;
        PopupMenuConfig: TPopupMenu;
        PopupMenuQuotes: TPopupMenu;
        MemoText: TSynEdit;
        SQLFormat: TSynSQLSyn;
        JavaFormat: TSynJavaSyn;
        ToolBar: TToolBar;
        ToolButton1: TToolButton;
        ToolButton2: TToolButton;
        ToolButtonModeTxt: TToolButton;
        ToolButtonModeSQL: TToolButton;
        ToolButtonModeJava: TToolButton;
        ToolButtonConfig: TToolButton;
        ToolButtonQuoteType: TToolButton;
        ToolButtonS1: TToolButton;
        ToolButtonS3: TToolButton;
        ToolButtonS5: TToolButton;
        ToolButtonS4: TToolButton;
        ToolButtonS2: TToolButton;
        ToolButtonTrim: TToolButton;
        ToolButtonRemoveQuotes: TToolButton;
        ToolButtonAddQuotes: TToolButton;
        ToolButtonLowercase: TToolButton;
        ToolButtonUppercase: TToolButton;
        ToolButtonSort: TToolButton;
        ToolButtonUnindent: TToolButton;
        ToolButtonIndent: TToolButton;
        ToolButtonUndo: TToolButton;
        EditSourceChar: TEdit;
        EditTargetChar: TEdit;
        OkButton: TBitBtn;
        SwapButton: TBitBtn;
        Panel: TPanel;
        RadioGroupTo: TRadioGroup;
        RBTargetChar: TRadioButton;
        RBSourceNewline: TRadioButton;
        RBTargetNewline: TRadioButton;
        RBSourceTab: TRadioButton;
        RBSourceChar: TRadioButton;
        RadioGroupFrom: TRadioGroup;
        RBTargetTab: TRadioButton;
        procedure EditSourceCharEnter(Sender: TObject);
        procedure EditTargetCharEnter(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure FormKeyPress(Sender: TObject; var Key: char);
        procedure MemoTextProcessCommand(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
        procedure MenuItemClick(Sender: TObject);
        procedure MenuItemConfigShowSpaceTabClick(Sender: TObject);
        procedure MenuItemConfigTabAsSpacesClick(Sender: TObject);
        procedure MenuItemConfigTrimTrailingSpacesClick(Sender: TObject);
        procedure OkButtonClick(Sender: TObject);
        procedure PopupMenuConfigPopup(Sender: TObject);
        procedure ToolButtonHighlighMode(Sender: TObject);
        procedure ToolButtonUndoClick(Sender: TObject);
        procedure ToolButtonUnindentClick(Sender: TObject);
        procedure ToolButtonIndentClick(Sender: TObject);
        procedure ToolButtonSortClick(Sender: TObject);
        procedure ToolButtonUppercaseClick(Sender: TObject);
        procedure ToolButtonLowercaseClick(Sender: TObject);
        procedure ToolButtonAddQuotesClick(Sender: TObject);
        procedure ToolButtonRemoveQuotesClick(Sender: TObject);
        procedure ToolButtonTrimClick(Sender: TObject);
        procedure SwapButtonClick(Sender: TObject);
    private
        { private declarations }
        function QuoteToUse(isStart: boolean): char;
        function QuoteUp(sourceText : string): string;
        function Unquote(sourceText : string): string;
        procedure ChangeLanguage(buttonChosen : TToolButton);
    public
        { public declarations }
    end;

implementation

{$R *.lfm}

{ Here are the codes.. to force }

const
    ecManualSwap      = ecUserDefinedFirst + 1;
    ecManualTrim      = ecUserDefinedFirst + 4;
    ecManualSort      = ecUserDefinedFirst + 5;
    ecManualLowercase = ecUserDefinedFirst + 6;
    ecManualUppercase = ecUserDefinedFirst + 7;
    ecManualQuoteUp   = ecUserDefinedFirst + 8;
    ecManualUnquote   = ecUserDefinedFirst + 9;

{ TTextTools }

procedure TTextTools.FormCreate(Sender: TObject);
var textToUse     : string;
    lowercaseText : string;
begin
    // From clipboard
    textToUse     := Clipboard.AsText;
    lowercaseText := Lowercase(textToUse);
    // Default mode
    ChangeLanguage(ToolButtonModeTxt);
    if (AnsiContainsStr(lowercaseText, 'class')) or (AnsiContainsStr(lowercaseText, 'public')) then ChangeLanguage(ToolButtonModeJava);
    if (AnsiContainsStr(lowercaseText, 'select')) or (AnsiContainsStr(lowercaseText, 'procedure')) then ChangeLanguage(ToolButtonModeSQL);
    // Paste the content..
    MemoText.Lines.Text := textToUse;
    // Move cursor
    MemoText.CaretXY := Point(1,1);
end;

procedure TTextTools.EditSourceCharEnter(Sender: TObject);
begin
    RBSourceChar.Checked := true;
end;

procedure TTextTools.EditTargetCharEnter(Sender: TObject);
begin
    RBTargetChar.Checked := true;
end;

procedure TTextTools.FormKeyPress(Sender: TObject; var Key: char);
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

procedure TTextTools.MemoTextProcessCommand(Sender: TObject; var Command: TSynEditorCommand; var AChar: TUTF8Char; Data: pointer);
var loopy        : integer;
    // full text
    allStartPos  : TPoint;
    allEndPos    : TPoint;
    // The selected text
    selStartPos  : TPoint;
    selEndPos    : TPoint;
    // for block selection
    loopsCount   : integer;
    loopFrom     : TPoint;
    loopTo       : TPoint;
    // cache text
    iterator     : integer;
    tempValue    : integer;
    tempText     : string;
    // swapping text
    sourceStr    : string;
    targetStr    : string;
    // temp work
    tempList     : TStringList;
begin
    // Changes based on flag...
    if (eoTabsToSpaces in MemoText.Options) then begin
        MemoText.BlockIndent    := 2; {spaces}
        MemoText.BlockTabIndent := 0; { no tabs }
    end else begin
        MemoText.BlockIndent    := 0; { no spaces }
        MemoText.BlockTabIndent := 1; { 1 tab }
    end;

    // Stop if not me...
    if (Command < ecUserDefinedFirst) or (Command > ecUserDefinedLast) then EXIT;

    // All text..
    allStartPos := Point(1,1);
    allEndPos   := Point(Length(MemoText.Lines[MemoText.Lines.Count-1])+1, MemoText.Lines.Count);

    // Check which one..
    case Command of

        {   THESE DO NO WORK ON BLOCK MODE..  }

        ecManualSort      : begin
                                tempList := TStringList.Create;
                                try
                                    // Get the text..  NOTICE I SORT THE WHOLE THING.. OTHERWISE IS WEIRD...
                                    tempList.Text := MemoText.TextBetweenPoints[allStartPos, allEndPos];
                                    // Process..
                                    tempList.Sort;
                                    // Replace the text in one go..
                                    MemoText.TextBetweenPoints[allStartPos, allEndPos] := tempList.Text;
                                finally
                                    FreeAndNil(tempList);
                                end;
                            end;

        {   THESE PART IS SIMULATED AS BLOCK.. LINE BY LINE.. }

        otherwise begin

            // All text..
            selStartPos := allStartPos;
            selEndPos   := allEndPos;

            // Has any selection?
            if (MemoText.SelAvail) then begin
                // Get the block
                selStartPos   := MemoText.BlockBegin;
                selEndPos     := MemoText.BlockEnd;
            end;

            // Sometimes is upside down
            if (selStartPos.X  > selEndPos.X) then begin
                tempValue     := selStartPos.X;
                selStartPos.X := selEndPos.X;
                selEndPos.X   := tempValue;
            end;
            if (selStartPos.Y  > selEndPos.Y) then begin
                tempValue     := selStartPos.Y;
                selStartPos.Y := selEndPos.Y;
                selEndPos.Y   := tempValue;
            end;

            // Default
            loopFrom := selStartPos;
            loopTo   := selEndPos;

            // By default.. once.. except in block
            loopsCount := 1;
            // I iterate the block ones that I can ..
            if (MemoText.SelectionMode = smColumn) then loopsCount := selEndPos.Y - selStartPos.Y + 1;

            // Loop and fix each one
            for loopy := 0 to loopsCount - 1 do begin

                // In block.. simulate many changes..
                if (loopsCount > 1) then begin
                    // Simulate..
                    loopFrom := Point(selStartPos.X, selStartPos.Y + loopy);
                    loopTo   := Point(selEndPos.X,   selStartPos.Y + loopy);
                end;

                // Check which one..
                case Command of
                    ecManualSwap      : begin
                                            // default
                                            sourceStr := '';
                                            targetStr := '';

                                            // source
                                            if (RBSourceNewline.Checked) then sourceStr := LineEnding;
                                            if (RBSourceTab.Checked)     then sourceStr := #9;
                                            if (RBSourceChar.Checked)    then sourceStr := EditSourceChar.Text;
                                            // target
                                            if (RBTargetNewline.Checked) then targetStr := LineEnding;
                                            if (RBTargetTab.Checked)     then targetStr := #9;
                                            if (RBTargetChar.Checked)    then targetStr := EditTargetChar.Text;

                                            // Get the text..
                                            tempText := MemoText.TextBetweenPoints[loopFrom, loopTo];

                                            // Replaced.. including lines...
                                            tempText := StringReplace(tempText, sourceStr, targetStr, [rfReplaceAll]);

                                            // Replace the text in one go..
                                            MemoText.TextBetweenPoints[loopFrom, loopTo] := tempText;
                                        end;

                    ecManualTrim      : begin
                                            tempList := TStringList.Create;
                                            try
                                                // Get the text..
                                                tempList.Text := MemoText.TextBetweenPoints[loopFrom, loopTo];
                                                // Process..
                                                for iterator := 0 to tempList.Count - 1 do begin
                                                    tempList.Strings[iterator] := TrimRight(tempList.Strings[iterator]);
                                                end;
                                                // Replace the text in one go..
                                                MemoText.TextBetweenPoints[loopFrom, loopTo] := tempList.Text;
                                            finally
                                                FreeAndNil(tempList);
                                            end;
                                        end;

                    ecManualLowercase : begin
                                            // Replace the text in one go..
                                            MemoText.TextBetweenPoints[loopFrom, loopTo] := Lowercase(MemoText.TextBetweenPoints[loopFrom, loopTo]);
                                        end;

                    ecManualUppercase : begin
                                            // Replace the text in one go..
                                            MemoText.TextBetweenPoints[loopFrom, loopTo] := Uppercase(MemoText.TextBetweenPoints[loopFrom, loopTo]);
                                        end;

                    ecManualQuoteUp   : begin
                                            // Replace the text in one go..
                                            MemoText.TextBetweenPoints[loopFrom, loopTo] := QuoteUp(MemoText.TextBetweenPoints[loopFrom, loopTo]);
                                        end;

                    ecManualUnquote   : begin
                                            // Replace the text in one go..
                                            MemoText.TextBetweenPoints[loopFrom, loopTo] := Unquote(MemoText.TextBetweenPoints[loopFrom, loopTo]);
                                        end;

                end;

            end;

        end;

    end;

end;

procedure TTextTools.MenuItemClick(Sender: TObject);
begin
    TMenuItem(Sender).Checked := true;
end;

procedure TTextTools.MenuItemConfigTrimTrailingSpacesClick(Sender: TObject);
begin
    if (eoTrimTrailingSpaces in MemoText.Options)
        then MemoText.Options := MemoText.Options - [ eoTrimTrailingSpaces ]
        else MemoText.Options := MemoText.Options + [ eoTrimTrailingSpaces ]
end;

procedure TTextTools.MenuItemConfigTabAsSpacesClick(Sender: TObject);
begin
    if (eoTabsToSpaces in MemoText.Options)
        then MemoText.Options := MemoText.Options - [ eoTabsToSpaces ]
        else MemoText.Options := MemoText.Options + [ eoTabsToSpaces ]
end;

procedure TTextTools.MenuItemConfigShowSpaceTabClick(Sender: TObject);
begin
    if (vscSpace in MemoText.VisibleSpecialChars)
        then MemoText.VisibleSpecialChars := MemoText.VisibleSpecialChars - [ vscSpace, vscTabAtFirst ]
        else MemoText.VisibleSpecialChars := MemoText.VisibleSpecialChars + [ vscSpace, vscTabAtFirst ]
end;

procedure TTextTools.OkButtonClick(Sender: TObject);
begin
    // Simplified
    with MemoText do begin
        // Default.. of course.. is ENTER
        Lines.LineBreak := LineEnding;
        // Change clipboard
        Clipboard.AsText := Lines.Text;
    end;
    // Bye
    ModalResult := mrOK;
end;

procedure TTextTools.ToolButtonUndoClick(Sender: TObject);
begin
    MemoText.CommandProcessor(ecUndo, '', nil);
end;

procedure TTextTools.ToolButtonUnindentClick(Sender: TObject);
begin
    MemoText.CommandProcessor(ecBlockUnindent, '', nil);
end;

procedure TTextTools.ToolButtonIndentClick(Sender: TObject);
begin
    MemoText.CommandProcessor(ecBlockIndent, '', nil);
end;

procedure TTextTools.ToolButtonSortClick(Sender: TObject);
begin
    MemoText.CommandProcessor(ecManualSort, '', nil);
end;

procedure TTextTools.ToolButtonUppercaseClick(Sender: TObject);
begin
    MemoText.CommandProcessor(ecManualUppercase, '', nil);
end;

procedure TTextTools.ToolButtonLowercaseClick(Sender: TObject);
begin
    MemoText.CommandProcessor(ecManualLowercase, '', nil);
end;

procedure TTextTools.ToolButtonAddQuotesClick(Sender: TObject);
begin
    MemoText.CommandProcessor(ecManualQuoteUp, '', nil);
end;

procedure TTextTools.ToolButtonRemoveQuotesClick(Sender: TObject);
begin
    MemoText.CommandProcessor(ecManualUnquote, '', nil);
end;

procedure TTextTools.ToolButtonTrimClick(Sender: TObject);
begin
    MemoText.CommandProcessor(ecManualTrim, '', nil);
end;

procedure TTextTools.SwapButtonClick(Sender: TObject);
begin
    MemoText.CommandProcessor(ecManualSwap, '', nil);
end;

function TTextTools.QuoteToUse(isStart: boolean): char;
begin
    result := '#';
    if (MenuItemSimpleQuote.Checked)    then result := '''';
    if (MenuItemDoubleQuote.Checked)    then result := '"';
    if (MenuItemParenthesis.Checked)    then if (isStart) then result := '(' else result := ')';
    if (MenuItemBrackets.Checked)       then if (isStart) then result := '{' else result := '}';
    if (MenuItemSquareBrackets.Checked) then if (isStart) then result := '[' else result := ']';
end;

function TTextTools.QuoteUp(sourceText : string): string;
const
    WORD_SEPARATORS : array[1..9] of char = ( #9, #13, #10, ' ', ',', '.', ':', ';', '=');
var
    iterator   : integer;
    charIndex  : integer;
    symbol     : char;
    isSpecial  : boolean;
    addedQuote : boolean;
    builtText  : string;
begin
    // If start with letter.. it will add...
    addedQuote := false;
    // Only add if not the special one
    isSpecial := false;

    // build the whole thing
    for iterator := 1 to Length(sourceText) do begin
        // Get each character
        symbol := sourceText[iterator];
        // Check if is any special one
        for charIndex := 1 to Length(WORD_SEPARATORS) do begin
            // Search each special character
            isSpecial := (symbol = WORD_SEPARATORS[charIndex]);
            // Must stop?
            if (isSpecial) then BREAK;
        end;
        // Add this beauty..
        if (not addedQuote) and (not isSpecial) then begin
            // Add the quote..
            builtText := builtText + QuoteToUse(true);
            // Swap..
            addedQuote := true;
        end;
        // Add this beauty..
        if (addedQuote) and (isSpecial) then begin
            // Add the quote..
            builtText := builtText + QuoteToUse(false);
            // Swap..
            addedQuote := false;
        end;
        // Keep adding one at the time..
        builtText := builtText + symbol;
    end;

    // Ended with no extra char..
    if (addedQuote) and (not isSpecial) then begin
        // Add the quote..
        builtText := builtText + QuoteToUse(false);
    end;

    // Completed the change..
    result := builtText;
end;

function TTextTools.Unquote(sourceText : string): string;
var
    builtText : string;
begin
    // Start point..
    builtText := sourceText;

    // Full Replace..
    builtText := StringReplace(builtText, QuoteToUse(true), '',  [rfReplaceAll]);

    // Only if they differ.. otherwise is pointless..
    if (QuoteToUse(true) <> QuoteToUse(false)) then builtText := StringReplace(builtText, QuoteToUse(false), '', [rfReplaceAll]);

    // Completed the change..
    result := builtText;
end;

procedure TTextTools.PopupMenuConfigPopup(Sender: TObject);
begin
    MenuItemConfigShowSpaceTab.Checked       := (vscSpace in MemoText.VisibleSpecialChars);
    MenuItemConfigTrimTrailingSpaces.Checked := (eoTrimTrailingSpaces in MemoText.Options);
    MenuItemConfigTabAsSpaces.Checked        := (eoTabsToSpaces in MemoText.Options);
end;

procedure TTextTools.ToolButtonHighlighMode(Sender: TObject);
begin
    ChangeLanguage(TToolButton(Sender));
end;

procedure TTextTools.ChangeLanguage(buttonChosen : TToolButton);
begin
    // Show in the button...
    ToolButtonModeTxt.Down  := (ToolButtonModeTxt  = buttonChosen);
    ToolButtonModeSQL.Down  := (ToolButtonModeSQL  = buttonChosen);
    ToolButtonModeJava.Down := (ToolButtonModeJava = buttonChosen);
    // Change mode..
    if (ToolButtonModeTxt  = buttonChosen) then MemoText.Highlighter := NIL;
    if (ToolButtonModeSQL  = buttonChosen) then MemoText.Highlighter := SQLFormat;
    if (ToolButtonModeJava = buttonChosen) then MemoText.Highlighter := JavaFormat;
end;

end.

