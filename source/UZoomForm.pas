unit UZoomForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, LCLIntf, LCLType, Clipbrd,
  UGlobals;

type

  { TZoomForm }

  TZoomForm = class(TForm)
    ImageZoom: TImage;
    TimerZoom: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormResize(Sender: TObject);
    procedure TimerZoomTick(Sender: TObject);
  private
    { private declarations }
    FLastMousePosition : TPoint;
    FRefreshDesktopImage : Boolean;
    FDeskCopy : TBitmap; // for performance..
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TZoomForm }

procedure TZoomForm.FormCreate(Sender: TObject);
begin
    // Move it
    Self.Top := Application.MainForm.Top + 50;
    Self.Left := Application.MainForm.Left;

    // This is to erase the menu
    Application.ProcessMessages;

    // Make all repaint
    Screen.UpdateScreen;

    // Let screen paint..
    Sleep(100);

    // I DECIDED I DONT NEED TO REFRESH THE WHOLE SCREEN ALL THE TIME FOR VERY MINOR CHANGES.. SUCH AS.. THE CLOCK AREA.
    TakeScreenshotFromCursor(FDeskCopy);

end;

procedure TZoomForm.FormDestroy(Sender: TObject);
begin
    // Bye..
    FreeAndNil(FDeskCopy);
end;

procedure TZoomForm.FormResize(Sender: TObject);
begin
    // Allow me to make it bigger if I want to
    ImageZoom.Picture.Bitmap.Width  := self.Width;
    ImageZoom.Picture.Bitmap.Height := self.Height;
end;

procedure TZoomForm.FormKeyPress(Sender: TObject; var Key: char);
begin
    case Key of
        #13: begin
                 // Copy to clipboard
                 Clipboard.Assign(ImageZoom.Picture);
                 // bye
                 ModalResult := mrOK;
             end;
        #27: begin
                 // bye
                 ModalResult := mrClose;
             end;
    end;
end;

procedure TZoomForm.TimerZoomTick(Sender: TObject);
var isMouseInside : Boolean;
    mousePosition : TPoint;
    sourceRect : TRect;
    targetRect : TRect;
begin
    try
        // To the image..
        targetRect := Rect(0, 0, ImageZoom.Width, ImageZoom.Height); // full extend

        // Copy..
        mousePosition := Point(0, 0);
        GetCursorPos(mousePosition);

        // Are you inside?
        isMouseInside := (PtInRect(targetRect, ScreenToClient(mousePosition)));
        Self.AlphaBlend := isMouseInside;

        // Mouse moving inside.. make it invisible and clear.. otherwise draw..
        if (isMouseInside) then begin
            // clear so I can see behind..
            ImageZoom.Canvas.Clear;

        end else begin

            // Once I pressed the refresh key.. it will remain.. (otherwise is confusing)
            FRefreshDesktopImage := FRefreshDesktopImage or (ZOOMFORM_KEY_FOR_REFRESH in GetKeyShiftState);

            // same.. dont do it again.. otherwise becomes sluggish..
            if (FRefreshDesktopImage or (not PointsEqual(FLastMousePosition, mousePosition))) then begin

                // Keep it..
                FLastMousePosition := mousePosition;

                // refresh...
                if (FRefreshDesktopImage) then TakeScreenshotFromCursor(FDeskCopy);

                // If I refreshed and failed.. stop
                if (FDeskCopy = NIL) then EXIT;

                // Move around the rectangle to make it work
                sourceRect := Rect(mousePosition.X, mousePosition.Y, mousePosition.X, mousePosition.Y);

                // half on each side.. and half so the image zooms..
                InflateRect(sourceRect, ImageZoom.Width div 2 div 2, ImageZoom.Height div 2 div 2);

                // Dont do that ugly unwanted zooming on borders...
                if (sourceRect.Right > FDeskCopy.Width) then OffsetRect(sourceRect, - (sourceRect.Right - FDeskCopy.Width), 0);
                if (sourceRect.Bottom > FDeskCopy.Height) then OffsetRect(sourceRect, 0, - (sourceRect.Bottom - FDeskCopy.Height));
                if (sourceRect.Left < 0) then OffsetRect(sourceRect, - sourceRect.Left, 0);
                if (sourceRect.Top  < 0) then OffsetRect(sourceRect, 0, - sourceRect.Top);

                // Copy the thingy..
                ImageZoom.Canvas.CopyRect(targetRect, FDeskCopy.Canvas, sourceRect);

            end;

        end;

    except
        // Kill me
        Close();

    end;

end;

end.

