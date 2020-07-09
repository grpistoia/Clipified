unit UClipboardListener;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS} Windows, Messages, SysUtils, LCLIntf, {$ENDIF}
  Classes;

type
  { TClipboardListener }

  TClipboardListener = class(TObject)
  private
    {$IFDEF WINDOWS}
    WindowsHandle: HWND;
    AddClipboardFormatListener: function(Wnd: HWND): BOOL; stdcall;
    RemoveClipboardFormatListener: function(Wnd: HWND): BOOL; stdcall;
    procedure WindowProc(var Message: TMessage);
    {$ENDIF}
  private
    FHasClipboardChange : Boolean;
    FWasEverNotified    : Boolean;
    function    GetSupported: Boolean;
  public
    constructor Create;
    destructor  Destroy; override;
  public
    function    ClipboardChangeFound : Boolean;
    procedure   ClipboardChangeProcessed;
  public
    property    Supported: Boolean read GetSupported;
  end;

implementation

{ TClipboardListener }

constructor TClipboardListener.Create;
{$IFDEF WINDOWS}
var
    HUser32: HMODULE;
{$ENDIF}
begin
  inherited;
  // Default (Linux)
  FHasClipboardChange := true;        // the very first time.. reload...
  // Not until proven
  FWasEverNotified := false;
  // Only Windows..
  {$IFDEF WINDOWS}
  // Connect the whole Listener
  HUser32 := GetModuleHandle(user32);
  Pointer(Self.AddClipboardFormatListener) := GetProcAddress(HUser32, 'AddClipboardFormatListener');
  Pointer(Self.RemoveClipboardFormatListener) := GetProcAddress(HUser32, 'RemoveClipboardFormatListener');
  Self.WindowsHandle := LCLIntf.AllocateHWnd(@WindowProc);
  // I dont abort.. just return null
  // if not AddClipboardFormatListener(WindowsHandle) then RaiseLastOSError;
  {$ENDIF}
end;

destructor TClipboardListener.Destroy;
begin
  {$IFDEF WINDOWS}
  if (Self.WindowsHandle <> 0) then begin
      RemoveClipboardFormatListener(WindowsHandle);
      LCLIntf.DeallocateHWnd(WindowsHandle);
  end;
  {$ENDIF}
  inherited;
end;

function TClipboardListener.GetSupported: Boolean;
begin
  {$IFDEF WINDOWS}
  Result := Assigned(AddClipboardFormatListener) and Assigned(RemoveClipboardFormatListener) and FWasEverNotified;
  {$ELSE}
  Result := false;
  {$ENDIF}
end;

{$IFDEF WINDOWS}
procedure TClipboardListener.WindowProc(var Message: TMessage);
begin
    if (Message.msg = WM_CLIPBOARDUPDATE) then
    begin
      // Managed
      Message.Result := 0;
      // Now I automatically know..
      FHasClipboardChange := true;
      // This will remain as YES
      FWasEverNotified := true;
    end;
end;
{$ENDIF}

function TClipboardListener.ClipboardChangeFound : Boolean;
begin
    result := FHasClipboardChange;
end;

procedure TClipboardListener.ClipboardChangeProcessed;
begin
    FHasClipboardChange := false;
end;

end.
