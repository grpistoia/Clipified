unit UEditImageForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Types, Clipbrd, StdCtrls, Math, LCLIntf,
  UGlobals;

type

  { TEditImageForm }

  TEditImageForm = class(TForm)
    BevelBorder: TBevel;
    DesktopCopy: TImage;
    GroupBoxPreview: TGroupBox;
    GroupBoxOriginal: TGroupBox;
    LblImageSize: TLabel;
    LblCursorPos: TLabel;
    LbExtralnfo: TLabel;
    PreviewArea: TImage;
    ScrollBoxes: TScrollBox;
    procedure DesktopCopyMouseEnter(Sender: TObject);
    procedure DesktopCopyMouseLeave(Sender: TObject);
    procedure DesktopCopyMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure DesktopCopyMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure DesktopCopyMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private
    { private declarations }
    FSelectionStarted : Boolean;
    FCurrentPosition  : TPoint;
    FCurrentSelection : TRect;
    { detail work for cursor }
    procedure DoDrawCursorLines(TheCanvas: TCanvas; NewPosition: TPoint); inline;
    { I draw cursor lines in the full image and in the preview }
    procedure DrawSelectionBox(TheCanvas: TCanvas; NewSelection: TRect);
    procedure DrawErasableCursorLines(TheCanvas: TCanvas; NewPosition: TPoint);
    procedure DrawIndelibleCursorLines(TheCanvas: TCanvas; NewPosition: TPoint; PenWidth: Integer);
    { detail work for selection }
    procedure CropSelection(Source: TImage; CroppingRect: TRect; Target: TPicture; AddBorder: Boolean);
    procedure CropPreview(MousePosition: TPoint; ExtraZoom: Boolean);
    { Manages the movement of the mouse and final selection }
    procedure HandleSelection(Shift: TShiftState; CursorPos: TPoint; OriginalSelection: TRect; CloseForm: Boolean);
  private
    { code reasons }
    FDesktopCopyBitmap : TBitmap;
  public
    { public declarations }
    procedure ApplyImage(NewImage: TBitmap);
  end;

implementation

{$R *.lfm}

{ TEditImageForm }

procedure TEditImageForm.FormCreate(Sender: TObject);
begin
    // Hope !!!
    Self.DoubleBuffered := true;
    // Make it easier... less scrolling...
    Self.Width  := Screen.Width * 80 div 100;
    Self.Height := Screen.Height * 70 div 100;
    // Try making it bigger...
    Self.Position := poScreenCenter;
end;

procedure TEditImageForm.FormKeyPress(Sender: TObject; var Key: char);
begin
    case Key of
        #13: begin
                // I used it
                Key := #0;
                // mmm...
                if (not FSelectionStarted)
                    then DesktopCopyMouseDown(Sender, mbLeft, GetKeyShiftState, FCurrentPosition.X, FCurrentPosition.Y)
                    else DesktopCopyMouseUp(Sender, mbLeft, GetKeyShiftState, FCurrentPosition.X, FCurrentPosition.Y)
             end;
        #27: begin
                 // bye
                 ModalResult := mrClose;
             end;
    end;
end;

procedure TEditImageForm.ApplyImage(NewImage: TBitmap);
begin
    try
        // Startup settings
        FSelectionStarted := false;
        FCurrentPosition  := Point(100, 100);
        FCurrentSelection := Rect(0, 0, FCurrentPosition.X, FCurrentPosition.Y);

        // should be sufficient...
        DesktopCopy.Picture.Bitmap.Width := Screen.Width;
        DesktopCopy.Picture.Bitmap.Height := Screen.Height;
        DesktopCopy.Picture.Bitmap.Assign(NewImage);

        // to make the code easier to read
        FDesktopCopyBitmap := DesktopCopy.Picture.Bitmap;

        // Show info...
        LblImageSize.Caption := SizeToText(NewImage.Width, NewImage.Height);

        // simulate a selection..
        HandleSelection([], FCurrentPosition, FCurrentSelection, false);

    except
        // silent..
        DesktopCopy.Picture.Bitmap.Clear;
    end;
end;

procedure TEditImageForm.DoDrawCursorLines(TheCanvas: TCanvas; NewPosition: TPoint);
begin
    // Draw cursors...
    if (NewPosition.X >= 0) and (NewPosition.X <= FDesktopCopyBitmap.Width)  then TheCanvas.Line(NewPosition.X, 0, NewPosition.X, FDesktopCopyBitmap.Height);
    if (NewPosition.Y >= 0) and (NewPosition.Y <= FDesktopCopyBitmap.Height) then TheCanvas.Line(0, NewPosition.Y, FDesktopCopyBitmap.Width, NewPosition.Y);
end;

procedure TEditImageForm.DrawSelectionBox(TheCanvas: TCanvas; NewSelection: TRect);
begin
    // Pen
    TheCanvas.Pen.Color   := EDITIMAGEFORM_CURSOR_LINE_COLOUR;
    TheCanvas.Pen.Width   := 1;
    TheCanvas.Pen.Style   := psSolid;
    TheCanvas.Pen.Mode    := EDITIMAGEFORM_PEN_MODE_FOR_CURSOR;
    // Brush
    TheCanvas.Brush.Color := EDITIMAGEFORM_CURSOR_LINE_COLOUR;
    TheCanvas.Brush.Style := bsClear;
    // Draw box
    TheCanvas.Rectangle(NewSelection);
end;

procedure TEditImageForm.DrawErasableCursorLines(TheCanvas: TCanvas; NewPosition: TPoint);
begin
    // Pen
    TheCanvas.Pen.Color   := EDITIMAGEFORM_CURSOR_LINE_COLOUR;
    TheCanvas.Pen.Width   := 1;
    TheCanvas.Pen.Style   := psSolid;
    TheCanvas.Pen.Mode    := EDITIMAGEFORM_PEN_MODE_FOR_CURSOR;
    // Brush
    TheCanvas.Brush.Color := EDITIMAGEFORM_CURSOR_LINE_COLOUR;
    TheCanvas.Brush.Style := bsClear;
    // Do it..
    DoDrawCursorLines(TheCanvas, NewPosition);
end;

procedure TEditImageForm.DrawIndelibleCursorLines(TheCanvas: TCanvas; NewPosition: TPoint; PenWidth: Integer);
begin
    // Pen
    TheCanvas.Pen.Color   := EDITIMAGEFORM_CURSOR_LINE_COLOUR;
    TheCanvas.Pen.Width   := PenWidth; // normally 1, or 3, 5.. to make it even on each side
    TheCanvas.Pen.Style   := psSolid;
    TheCanvas.Pen.Mode    := EDITIMAGEFORM_PEN_MODE_FOR_CURSOR;
    // Brush
    TheCanvas.Brush.Color := EDITIMAGEFORM_CURSOR_LINE_COLOUR;
    TheCanvas.Brush.Style := bsClear;
    // Do it..
    DoDrawCursorLines(TheCanvas, NewPosition);
end;

procedure TEditImageForm.DesktopCopyMouseEnter(Sender: TObject);
begin
    if (FSelectionStarted)
        then DrawSelectionBox(FDesktopCopyBitmap.Canvas, FCurrentSelection)
        else DrawErasableCursorLines(FDesktopCopyBitmap.Canvas, FCurrentPosition);
end;

procedure TEditImageForm.DesktopCopyMouseLeave(Sender: TObject);
begin
    if (FSelectionStarted)
        then DrawSelectionBox(FDesktopCopyBitmap.Canvas, FCurrentSelection)
        else DrawErasableCursorLines(FDesktopCopyBitmap.Canvas, FCurrentPosition);
end;

procedure TEditImageForm.DesktopCopyMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
    // Erase
    DrawErasableCursorLines(FDesktopCopyBitmap.Canvas, FCurrentPosition);

    // starts here
    FCurrentSelection := Rect(X, Y, X, Y);

    // border..
    FSelectionStarted := true;

    // Draw
    DrawSelectionBox(FDesktopCopyBitmap.Canvas, FCurrentSelection);

end;

procedure TEditImageForm.DesktopCopyMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
    // if not selecting yet.. preview from here...
    if (FSelectionStarted) then begin

        // Erase..
        DrawSelectionBox(FDesktopCopyBitmap.Canvas, FCurrentSelection);

        // update the target really...
        FCurrentPosition := Point(X, Y);

        // update the target really...
        FCurrentSelection := Rect(FCurrentSelection.Left, FCurrentSelection.Top, X, Y);

        // create the preview..
        HandleSelection(Shift, FCurrentPosition, FCurrentSelection, false);

        // Draw
        DrawSelectionBox(FDesktopCopyBitmap.Canvas, FCurrentSelection);

    end else begin

        // Draw the movement
        DrawErasableCursorLines(FDesktopCopyBitmap.Canvas, FCurrentPosition);

        // update the target really...
        FCurrentPosition := Point(X, Y);

        // simulate it...
        FCurrentSelection := Rect(X-20, Y-20, X+20, Y+20);

        // simulate it
        HandleSelection(Shift, FCurrentPosition, FCurrentSelection, false);

        // Draw the movement
        DrawErasableCursorLines(FDesktopCopyBitmap.Canvas, FCurrentPosition);

    end;
end;

procedure TEditImageForm.DesktopCopyMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var happyWithSeletion: Boolean;
begin
    // Still shows selection to use.. but ask if he likes it..
    happyWithSeletion := (MessageDlg(EDITIMAGEFORM_CONFIRMATION_MESSAGE, mtConfirmation, [mbOK, mbRetry], 0) = mrOK);

    // Erase the selection..
    DrawSelectionBox(FDesktopCopyBitmap.Canvas, FCurrentSelection);

    // no more border
    FSelectionStarted := false;

    // update the target really...
    FCurrentPosition := Point(X, Y);

    // Update..
    FCurrentSelection := Rect(FCurrentSelection.Left, FCurrentSelection.Top, X, Y);

    // create the preview
    HandleSelection(Shift, FCurrentPosition, FCurrentSelection, happyWithSeletion);

    { RESET MODE }

    // the user moved the mouse for sure.. not sure where it is.. don't draw
    GetCursorPos(FCurrentPosition);
    FCurrentPosition := DesktopCopy.ScreenToControl(FCurrentPosition);

    // Draw the movement
    DrawErasableCursorLines(FDesktopCopyBitmap.Canvas, FCurrentPosition);

    // Die!
    if (happyWithSeletion) then Self.Close;

end;

procedure TEditImageForm.CropPreview(MousePosition: TPoint; ExtraZoom: Boolean);
var
    zoomFactor : Integer;
    targetSize : TSize;
    targetRect : TRect;
    copyRect   : TRect;
    zoomSize   : TSize;
begin

    // how smart..
    if (ExtraZoom) then zoomFactor := EDITIMAGEFORM_DEFAULT_EXTRA else zoomFactor := EDITIMAGEFORM_DEFAULT_ZOOM;

    // this is the size of the preview.. I wont change it.. but crop half of that from source
    targetSize := Size(PreviewArea.Width, PreviewArea.Height);
    targetRect := Rect(0, 0, targetSize.cx, targetSize.cy);

    // Clean it...
    with PreviewArea.Picture do begin
        Bitmap.SetSize(targetSize.cx, targetSize.cy);
        Bitmap.Canvas.Brush.Style := bsSolid;
        Bitmap.Canvas.Brush.Color := EDITIMAGEFORM_IMAGE_BORDER_COLOUR;
        Bitmap.Canvas.FillRect(targetRect);
    end;

    // basic copy area with zoom
    copyRect := Rect(MousePosition.X, MousePosition.Y, MousePosition.X, MousePosition.Y);

    // reduce the drawing area for zooming...
    zoomSize := Size(targetSize.cx div zoomFactor, targetSize.cx div zoomFactor);

    // Now.. divide because I am inflating on each side..
    zoomSize := Size(zoomSize.cx div 2, zoomSize.cx div 2);
    InflateRect(copyRect, zoomSize.cx, zoomSize.cy);

    // Dont do that ugly unwanted zooming on borders...
    if (copyRect.Right > FDesktopCopyBitmap.Canvas.Width) then OffsetRect(copyRect, - (copyRect.Right - FDesktopCopyBitmap.Canvas.Width), 0);
    if (copyRect.Bottom > FDesktopCopyBitmap.Canvas.Height) then OffsetRect(copyRect, 0, - (copyRect.Bottom - FDesktopCopyBitmap.Canvas.Height));
    if (copyRect.Left < 0) then OffsetRect(copyRect, - copyRect.Left, 0);
    if (copyRect.Top  < 0) then OffsetRect(copyRect, 0, - copyRect.Top);

    // Finally copy...
    PreviewArea.Canvas.CopyRect(targetRect, FDesktopCopyBitmap.Canvas, copyRect);

    // Draw the lines...
    DrawIndelibleCursorLines(PreviewArea.Canvas, Point(targetSize.cx div 2, targetSize.cy div 2), 1 + zoomFactor);

end;

procedure TEditImageForm.CropSelection(Source: TImage; CroppingRect: TRect; Target: TPicture; AddBorder: Boolean);
const
    DEFAULT_BORDER = 1;
var
    borderChoice : Integer;
    imageSize    : TSize;
    imageRect    : TRect;
    fullRect     : TRect;
    fullSize     : TSize;
begin
    // Only apply if needed
    if (AddBorder) then borderChoice := DEFAULT_BORDER else borderChoice := 0;

    // the targetCanvas size is same as sourceImage..
    imageSize := Size(CroppingRect);

    // The image inside
    imageRect  := Rect(borderChoice, borderChoice, imageSize.cx + borderChoice, imageSize.cy + borderChoice);

    // Start in zero + the border left/top + width + the border right/bottom
    fullRect := Rect(0, 0, borderChoice + imageSize.cx + borderChoice, borderChoice + imageSize.cy + borderChoice);

    // Final area to use
    fullSize := Size(fullRect);

    // create the space
    Target.Bitmap.SetSize(fullSize.cx, fullSize.cy);

    // Do all the drawing operations
    with Target.Bitmap.Canvas do begin

        // drawing settings
        Pen.Style   := psSolid;
        Pen.Color   := EDITIMAGEFORM_IMAGE_BORDER_COLOUR;
        Brush.Style := bsSolid;
        Brush.Color := EDITIMAGEFORM_IMAGE_BORDER_COLOUR;
        FillRect(fullRect);

        // Paste the Source image area..
        CopyRect(imageRect, Source.Canvas, CroppingRect);

    end;

end;

procedure TEditImageForm.HandleSelection(Shift: TShiftState; CursorPos: TPoint; OriginalSelection: TRect; CloseForm: Boolean);
var
    newRect : TRect;
    newSize : TSize;
    workingPicture : TPicture;
begin

    // Message
    LblCursorPos.Caption := CursorToText(CursorPos);

    // Bitmap copy doesn't work backwards
    newRect := Rect(OriginalSelection.Left, OriginalSelection.Top, OriginalSelection.Right, OriginalSelection.Bottom);
    // swap if it is wrong
    if (newRect.Left > newRect.Right)  then newRect := Rect(newRect.Right, newRect.Top, newRect.Left, newRect.Bottom);
    if (newRect.Top  > newRect.Bottom) then newRect := Rect(newRect.Left, newRect.Bottom, newRect.Right, newRect.Top);

    // restrict inside the image (and 1+)
    newRect := Rect(Max(0, newRect.Left), Max(0, newRect.Top), Min(newRect.Right, FDesktopCopyBitmap.Width), Min(newRect.Bottom, FDesktopCopyBitmap.Height));

    // ready to get the size
    newSize := Size(newRect);

    // Modifiers
    if (FSelectionStarted) or (Shift <> []) then begin

        LbExtralnfo.Caption := '';

        if (EDITIMAGEFORM_KEY_FOR_BORDER in Shift)
            then LbExtralnfo.Caption := LbExtralnfo.Caption + 'Border ON.' + LineEnding
            else LbExtralnfo.Caption := LbExtralnfo.Caption + 'Press [' + ShiftStateToText(EDITIMAGEFORM_KEY_FOR_BORDER) + '] for Border.' + LineEnding;

        LbExtralnfo.Caption := LbExtralnfo.Caption + LineEnding;

        if (EDITIMAGEFORM_KEY_FOR_ZOOM in Shift)
            then LbExtralnfo.Caption := LbExtralnfo.Caption + 'Extra Zoom ON.' + LineEnding
            else LbExtralnfo.Caption := LbExtralnfo.Caption + 'Press [' + ShiftStateToText(EDITIMAGEFORM_KEY_FOR_ZOOM) + '] for Extra Zoom.' + LineEnding;

        LbExtralnfo.Caption := LbExtralnfo.Caption + SizeToText(newSize.cx, newSize.cy) + LineEnding

    end else begin

        LbExtralnfo.Caption := 'Select area to crop.';

    end;

    // ensure selectd more...
    if (newSize.cx <> 0.0) and (newSize.cy <> 0.0) then begin

        // the end?
        if (CloseForm) then begin

            // Temporary one...
            workingPicture := TPicture.Create;
            try
                // Do the dirty work..
                CropSelection(DesktopCopy, newRect, workingPicture, (EDITIMAGEFORM_KEY_FOR_BORDER in Shift));

                // Copy to clipboard
                Clipboard.Assign(workingPicture);

            finally
                // Bye!
                FreeAndNil(workingPicture);
            end;

        end else begin

            // Do the dirty work..  (size doesn't change)
            CropPreview(CursorPos, (EDITIMAGEFORM_KEY_FOR_ZOOM in Shift));

        end;

    end;

end;

end.

