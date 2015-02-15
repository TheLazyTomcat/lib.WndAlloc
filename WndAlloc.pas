{==============================================================================}
{                                                                              }
{   WndAlloc                                                                   }
{                                                                              }
{   ©František Milt 2015-02-15                                                 }
{                                                                              }
{   Version 1.0                                                                }
{                                                                              }
{==============================================================================}
unit WndAlloc;

interface

uses
  Windows, Classes;

Function AllocateHWND(Method: TWndMethod): HWND;
procedure DeallocateHWND(Wnd: HWND);

implementation

uses
  Messages, SysUtils, SyncObjs;

{$IF not Defined(FPC) and (RTLVersion <= 15) and not Defined(x64)}
Function GetWindowLongPtr(hWnd: HWND; nIndex: LongInt): Pointer;
begin
Result := Pointer(GetWindowLong(hWnd,nIndex));
end;

Function SetWindowLongPtr(hWnd: HWND; nIndex: LongInt; dwNewLong: Pointer): Pointer;
begin
Result := Pointer(SetWindowLong(hWnd,nIndex,LongInt(dwNewLong)));
end;
{$IFEND}

//------------------------------------------------------------------------------

const
  MaxMediators = 512;
  
  UtilityWindowClassName = 'TUtilityWindow';

type
{$IFDEF x64}
  PtrUInt = UInt64;
{$ELSE}
  PtrUInt = LongWord;
{$ENDIF}

  TMediator = packed record
    POP_Reg:          Byte;
    PUSH_MethodData:  Byte;
    MethodData:       Pointer;
    PUSH_MethodCode:  Byte;
    MethodCode:       Pointer;
    PUSH_Reg:         Byte;
    JMP_Handler:      Byte;
    HandlerOffset:    Pointer;
  end;
  PMediator = ^TMediator;

  TMediators = Array[0..Pred(MaxMediators)] of TMediator;
  PMediators = ^TMediators;

  TUtilityWindowsManager = class(TObject)
  private
    fSynchronizer:  TCriticalSection;
    fMediators:     PMediators;
    fMediatorCount: Integer;
  protected
    Function NewMediator(Method: TMethod): PMediator; virtual;
    procedure RemoveMediator(Mediator: PMediator); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    Function AllocateHWND(Method: TWndMethod): HWND; virtual;
    procedure DeallocateHWND(Wnd: HWND); virtual;
  end;

var
  UtilityWindowManager: TUtilityWindowsManager;


Function AllocateHWND(Method: TWndMethod): HWND;
begin
Result := UtilityWindowManager.AllocateHWND(Method);
end;

//------------------------------------------------------------------------------

procedure DeallocateHWND(Wnd: HWND);
begin
UtilityWindowManager.DeallocateHWND(Wnd);
end;

//==============================================================================

Function WndHandler(MethodCode, MethodData: Pointer; Window: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  WndProc:  TMethod;
  Msg:      TMessage;
begin
WndProc.Code := MethodCode;
WndProc.Data := MethodData;
If Assigned(TWndMethod(WndProc)) then
  begin
    Msg.msg := Message;
    Msg.wParam := wParam;
    Msg.lParam := lParam;
    TWndMethod(WndProc)(Msg);
    Result := Msg.Result
  end
else Result := DefWindowProc(Window,Message,wParam,lParam);
end;

//==============================================================================

Function TUtilityWindowsManager.NewMediator(Method: TMethod): PMediator;
var
  i:  Integer;
begin
For i := 0 to Pred(MaxMediators) do
  If not Assigned(fMediators^[i].HandlerOffset) then
    begin
      fMediators^[i].POP_Reg := $58;
      fMediators^[i].PUSH_MethodData := $68;
      fMediators^[i].MethodData := Method.Data;
      fMediators^[i].PUSH_MethodCode := $68;
      fMediators^[i].MethodCode := Method.Code;
      fMediators^[i].PUSH_Reg := $50;
      fMediators^[i].JMP_Handler := $E9;
      fMediators^[i].HandlerOffset := {%H-}Pointer(PtrUInt(@WndHandler) - (PtrUInt(Addr(fMediators^[i].HandlerOffset)) + SizeOf(Pointer)));
      Result := Addr(fMediators^[i]);
      Break;
    end;
end;

//------------------------------------------------------------------------------

procedure TUtilityWindowsManager.RemoveMediator(Mediator: PMediator);
begin
If {%H-}(PtrUInt(Mediator) >= PtrUInt(fMediators)) and
   {%H-}(PtrUInt(Mediator) < (PtrUInt(fMediators) + SizeOf(TMediators))) then
  Mediator^.HandlerOffset := nil;
end;

//------------------------------------------------------------------------------

constructor TUtilityWindowsManager.Create;
begin
inherited;
fSynchronizer := TCriticalSection.Create;
fMediators := VirtualAlloc(nil,SizeOf(TMediators),MEM_COMMIT,PAGE_EXECUTE_READWRITE);
fMediatorCount := 0;
end;

//------------------------------------------------------------------------------

destructor TUtilityWindowsManager.Destroy;
begin
VirtualFree(fMediators,SizeOf(TMediators),MEM_RELEASE);
fSynchronizer.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TUtilityWindowsManager.AllocateHWND(Method: TWndMethod): HWND;
var
  Registered:         Boolean;
  TempClass:          TWndClass;
  UtilityWindowClass: TWndClass;
begin
Result := 0;
fSynchronizer.Enter;
try
  If fMediatorCount < MaxMediators then
    begin
      ZeroMemory(@UtilityWindowClass,SizeOf(TWndClass));
      Registered := Windows.GetClassInfo(hInstance,UtilityWindowClassName,{%H-}TempClass);
      If not Registered or (TempClass.lpfnWndProc <> @DefWindowProc) then
        begin
          If Registered then Windows.UnregisterClass(UtilityWindowClassName,hInstance);
          UtilityWindowClass.lpszClassName := UtilityWindowClassName;
          UtilityWindowClass.hInstance := hInstance;
          UtilityWindowClass.lpfnWndProc := @DefWindowProc;
          If Windows.RegisterClass(UtilityWindowClass) = 0 then
            raise Exception.CreateFmt('TUtilityWindowsManager.AllocateHWND: Unable to register hidden window class. %s',[SysErrorMessage(GetLastError)]);
        end;
      Result := CreateWindowEx(WS_EX_TOOLWINDOW,UtilityWindowClassName,'',WS_POPUP,0,0,0,0,0,0,hInstance,nil);
      If Result = 0 then
        raise Exception.CreateFmt('TUtilityWindowsManager.AllocateHWND: Unable to create hidden window. %s',[SysErrorMessage(GetLastError)]);
    {$IFDEF FPC}
      SetWindowLongPtr(Result,GWL_WNDPROC,{%H-}PtrUInt(NewMediator(TMethod(Method))));
    {$ELSE}
      SetWindowLongPtr(Result,GWL_WNDPROC,NewMediator(TMethod(Method)));
    {$ENDIF}
      Inc(fMediatorCount);
    end
  else raise Exception.Create('TUtilityWindowsManager.AllocateHWND: Unable to create new mediator.');
finally
  fSynchronizer.Leave;
end;
end;

//------------------------------------------------------------------------------

procedure TUtilityWindowsManager.DeallocateHWND(Wnd: HWND);
var
  Mediator: PMediator;
begin
fSynchronizer.Enter;
try
{$IFDEF FPC}
  Mediator := {%H-}Pointer(GetWindowLongPtr(Wnd,GWL_WNDPROC));
{$ELSE}
  Mediator := GetWindowLongPtr(Wnd,GWL_WNDPROC);
{$ENDIF}
  DestroyWindow(Wnd);
  RemoveMediator(Mediator);    
  Dec(fMediatorCount);
  If fMediatorCount <= 0 then
    Windows.UnregisterClass(UtilityWindowClassName,hInstance);
finally
  fSynchronizer.Leave;
end;
end;

//==============================================================================

initialization
  UtilityWindowManager := TUtilityWindowsManager.Create;

finalization
  UtilityWindowManager.Free;

end.

