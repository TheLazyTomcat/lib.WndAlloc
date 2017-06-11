{-------------------------------------------------------------------------------

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

-------------------------------------------------------------------------------}
{===============================================================================

  WndAlloc

  ©František Milt 2017-06-11

  Version 1.1

  Dependencies:
    AuxTypes - github.com/ncs-sniper/Lib.AuxTypes

===============================================================================}
{-------------------------------------------------------------------------------

  Implementation notes

  When the system calls window handler, it expects it to be normal function.
  But we want it to be method of an object. Methods have hidden (or implicit)
  first parameter (Self). This pose a problem - handling method and handling
  function have different signature and of course, system cannot know the Self
  parameter. It is not possible to assign method to a window handler function.

  Two possible solutions are implemented in this library. One requiring the use
  of ASM, the other purely in pascal working with WinAPI.


  ASM solution

    When a window allocation is requested, the manager creates the window (using
    WinAPI) and along with it a new mediator (small piece of code, only few
    instructions long) bound to that particular window (every window has it own
    unique mediator). Information about method that should handle messages of
    the created window are stored in the mediator and also the mediator is
    assigned as a window handler function.
    When the window requires handler function to process a message, it calls
    the mediator as it is assigned as the handler function, mediator changes
    parameter list passed from the system (it adds information about handling
    method) and then passes these altered parameters to a universal function.
    This universal function (common for all windows) in turn calls proper method
    by using information it got from the mediator.

    The call chain for a particular window looks like this:

      - window needs handler to process a message, handler (unique mediator) is
        called
      - mediator adds information about handler method to the parameters list
        and passes execution to the universal function
      - universal function processes information it got from mediator and calls
        proper method according to them
      - handler method does its job


    This solution is sligtly faster than the pascal one, but it is sort of a
    hack.


  PurePascal solution

    When new window gets created, information about the handler method are
    stored in memory (on heap) and pointer to these information are in turn
    stored directly in the window using WinAPI. An universal function (common
    for all windows) is assigned as a handler function.
    When window calls this function, it again uses WinAPI to get back pointer
    (which is stored in the window) to handler method information  and then
    calls proper method accordingly.
    This solution is cleaner, but since the universal function has to obtain
    pointer to calling info from the window every time it is executed, it is
    slower.


-------------------------------------------------------------------------------}
unit WndAlloc;

{$IF defined(CPUX86_64) or defined(CPUX64)}
  {$DEFINE x64}
{$ELSEIF defined(CPU386)}
  {$DEFINE x86}
{$ELSE}
  {$DEFINE PurePascal}
{$IFEND}

{$IF not(defined(WINDOWS) or defined(MSWINDOWS))}
  {$MESSAGE FATAL 'Unsupported operating system.'}
{$IFEND}

{$IFDEF FPC}
  {$MODE ObjFPC}{$H+}
  {$IFNDEF PurePascal}
    {$ASMMODE Intel}
  {$ENDIF}
{$ENDIF}

{
  ImplicitManager

  When defined, an internally managed utility window manager is created at
  the unit initialization and freed at finalization. This allows the use of
  functions AllocateHWND and DeallocateHWND.
  It is here mainly for cases where implicit manager is not desired, so it can
  be disabled.

  Defined by default.
}
{$DEFINE ImplicitManager}

interface

uses
  Windows, Classes, SyncObjs;

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TUtilityWindowManager                            }
{------------------------------------------------------------------------------}
{==============================================================================}

const
  def_MaxWindows  = 512;
  def_WindowClass = 'TUtilityWindow';

{==============================================================================}
{   TUtilityWindowManager - class declaration                                  }
{==============================================================================}

type
  TUtilityWindowManager = class(TObject)
  private
    fWindowClassName: String;
    fMaxWindows:      Integer;
    fSynchronizer:    TCriticalSection;
    fHelpers:         Pointer;
    fWindowCount:     Integer;
  protected
    procedure RegisterWindowClass; virtual;
    procedure UnregisterWindowClass; virtual;
    Function NewHelper(Method: TMethod): Pointer; virtual;
    procedure RemoveHelper(Helper: Pointer); virtual;
  public
    constructor Create(WindowClassName: String = def_WindowClass; MaxWindows: Integer = def_MaxWindows);
    destructor Destroy; override;
    Function AllocateHWND(Method: TWndMethod): HWND; virtual;
    procedure DeallocateHWND(Wnd: HWND); virtual;
  published
    property WindowClassName: String read fWindowClassName;
    property MaxWindows: Integer read fMaxWindows;
    property WindowCount: Integer read fWindowCount;
  end;

{==============================================================================}
{   Functions of implicit manager                                              }
{==============================================================================}

{$IFDEF ImplicitManager}
  Function AllocateHWND(Method: TWndMethod): HWND;
  procedure DeallocateHWND(Wnd: HWND);
{$ENDIF}

implementation

uses
  Messages, SysUtils, AuxTypes;

{==============================================================================}
{   Auxiliary functions, types, constants, ...                                 }
{==============================================================================}

const
  GWLP_WNDPROC  = -4;
{$IFDEF PurePascal}
  GWLP_USERDATA = -21;
{$ENDIF}

{$IF not Declared(LONG_PTR)}
type
  LONG_PTR = Pointer;
{$IFEND}

//------------------------------------------------------------------------------

{$IF not(Declared(GetWindowLongPtr) and Declared(SetWindowLongPtr))}

{
  Following code is supposed to work only in 32 bits, 64bit OS provides these
  functions in WinAPI.
}
{$IF SizeOf(Pointer) <> 4}
  {$MESSAGE FATAL 'Unsupported platform.'}
{$IFEND}

Function GetWindowLongPtr(hWnd: HWND; nIndex: Int32): Pointer;
begin
Result := Pointer(GetWindowLong(hWnd,nIndex));
end;

//------------------------------------------------------------------------------

Function SetWindowLongPtr(hWnd: HWND; nIndex: Int32; dwNewLong: Pointer): Pointer;
begin
Result := Pointer(SetWindowLong(hWnd,nIndex,Int32(dwNewLong)));
end;

{$IFEND}

{==============================================================================}
{------------------------------------------------------------------------------}
{                             TUtilityWindowManager                            }
{------------------------------------------------------------------------------}
{==============================================================================}

{==============================================================================}
{   TUtilityWindowManager - auxiliary functions, types, ...                    }
{==============================================================================}

{$IFDEF PurePascal}

{------------------------------------------------------------------------------}
{   TUtilityWindowManager - pascal solution                                    }
{------------------------------------------------------------------------------}

type
  TMethodItem = packed record
    Method:   TMethod;
    Assigned: PtrUInt;
  end;

  PMethodItem = ^TMethodItem;

//------------------------------------------------------------------------------

Function WndHandler(Window: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  Msg:        TMessage;
  MethodInfo: Pointer;
begin
MethodInfo := Addr({%H-}PMethodItem(GetWindowLongPtr(Window,GWLP_USERDATA))^.Method);
If Assigned(TWndMethod(TMethod(MethodInfo^))) then
  begin
    Msg.msg := Message;
    Msg.wParam := wParam;
    Msg.lParam := lParam;
    TWndMethod(TMethod(MethodInfo^))(Msg);
    Result := Msg.Result
  end
else Result := DefWindowProc(Window,Message,wParam,lParam);
end;

{$ELSE PurePascal}
{------------------------------------------------------------------------------}
{   TUtilityWindowManager - ASM solution                                       }
{------------------------------------------------------------------------------}

{$IFDEF x64}

{
  In win64, the parameters for handler are passed in registers RCX, RDX, R8
  and R9 respectively, the stack is more-or-less empty (contains only return
  address).
  Our new, fifth parameter (pointer to the handler method) will not fit into
  registers, so we have to pass it on the stack. But stack cleanup is on the
  caller, which, in this case, is system. We have no control over cleanup, and
  the caller don't know we are adding parameter to the stack

  The solution - instead of transfering execution directly to handler function,
  we introduce simple procedure "stub" that will handle creation of new stack
  frame and cleanup (gets the stack to a state is was upon entry to the
  mediator). The process is as follows:

    - system calls the mediator
      - new parameter is moved to a temporary location (register R10)
      - execution jumps to the "stub" (via JMP instruction, not CALL)
        - stack is aligned
        - shadow space is created (4 old parameters are pushed in there)
        - our new, fifth parameter is pushed onto the stack from R10
        - handler function is called (CALL instruction)
        - handler returns execution to the "stub"
        - stack is cleared (fifth parameter, shadow space, alignment)
    - execution is returned to the system
}
type
  TMediator = packed record
    MOV_MethodInfo:   array[0..1] of Byte;
    MethodInfo:       Pointer;
    MOV_HandlerAddr:  array[0..1] of Byte;
    HandlerAddr:      Pointer;
    JMP_Reg:          array[0..2] of Byte;
    _padding:         Byte;
    MethodCode:       Pointer;
    MethodData:       Pointer;
  end;

const
  def_Mediator: TMediator = (
    MOV_MethodInfo:   ($49,$BA);      //  MOV   R10,  qword
    MethodInfo:       nil;
    MOV_HandlerAddr:  ($48,$B8);      //  MOV   RAX,  qword
    HandlerAddr:      nil;
    JMP_Reg:          ($48,$FF,$E0);  //  JMP   RAX
    _padding:         $00;
    MethodCode:       nil;
    MethodData:       nil);

{$ELSE}
//------------------------------------------------------------------------------

{
  An stdcall convention is used, therefore parameters for handler function are
  passed on the stack right-to-left (last parameter is pushed first).
  In addition, return address is pushed on the stack, so the stack after the
  call looks like this:

    ESP + 16    lParam  (LPARAM)
    ESP + 12    wParam  (WPARAM)
    ESP + 8     Message (UINT)
    ESP + 4     Window  (HWND)
    ESP         RET_ADDR

  We need to add one parameter - pointer to the handling method. Simplest thing
  is to push it as a last parameter (since the arguments are in reversed order,
  it will be actually first in the parameters list).
  First, return address is poped into EAX register, then the new parameter is
  pushed, and the return address is pushed again from EAX. Stack now looks like
  this:

    ESP + 20    lParam      (LPARAM)
    ESP + 16    wParam      (WPARAM)
    ESP + 12    Message     (UINT)
    ESP + 8     Window      (HWND)
    ESP + 4     MethodInfo  (Pointer)
    ESP         RET_ADDR

  After that, the actual handler is called. Or, more precisely, execution is
  transfered to it using JMP instruction.

  Stack cleanup is up to the called function. It expects 5 parameters, so it
  automatically cleans everything including what we have added. There is no need
  to do manual cleanup.
}
type
  TMediator = packed record
    POP_ReturnAddr:   Byte;
    PUSH_MethodInfo:  Byte;
    MethodInfo:       Pointer;
    PUSH_ReturnAddr:  Byte;
    MOV_HandlerAddr:  Byte;
    HandlerAddr:      Pointer;
    JMP_Reg:          array[0..1] of Byte;
    _padding:         array[0..1] of Byte;
    MethodCode:       Pointer;
    MethodData:       Pointer;
   end;

const
  def_Mediator: TMediator = (
    POP_ReturnAddr:   $58;        //  POP   EAX
    PUSH_MethodInfo:  $68;        //  PUSH  dword
    MethodInfo:       nil;
    PUSH_ReturnAddr:  $50;        //  PUSH  EAX
    MOV_HandlerAddr:  $B8;        //  MOV   EAX,  dword
    HandlerAddr:      nil;
    JMP_Reg:          ($FF,$E0);  //  JMP   EAX
    _padding:         ($00,$00);
    MethodCode:       nil;
    MethodData:       nil);

{$ENDIF}

type
  PMediator = ^TMediator;

//------------------------------------------------------------------------------

{$IFDEF x64}
Function WndHandler(Window: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM; MethodInfo: Pointer): LRESULT;
{$ELSE}
Function WndHandler(MethodInfo: Pointer; Window: HWND; Message: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ENDIF}
var
  Msg:  TMessage;
begin
If Assigned(TWndMethod(TMethod(MethodInfo^))) then
  begin
    Msg.msg := Message;
    Msg.wParam := wParam;
    Msg.lParam := lParam;
    TWndMethod(TMethod(MethodInfo^))(Msg);
    Result := Msg.Result
  end
else Result := DefWindowProc(Window,Message,wParam,lParam);
end;

//------------------------------------------------------------------------------

{$IFDEF x64}
procedure HandlerCaller; assembler;
asm
  SUB   RSP,  8
  PUSH  R10
  PUSH  R9
  PUSH  R8
  PUSH  RDX
  PUSH  RCX
  CALL  WndHandler
  ADD   RSP,  48
end;
{$ENDIF}

{$ENDIF PurePascal}

{==============================================================================}
{   TUtilityWindowManager - class implementation                               }
{==============================================================================}

{------------------------------------------------------------------------------}
{   TUtilityWindowManager - protected methods                                  }
{------------------------------------------------------------------------------}

procedure TUtilityWindowManager.RegisterWindowClass;
var
  Registered:   Boolean;
  TempClass:    TWndClass;
  WindowClass:  TWndClass;
begin
ZeroMemory(@WindowClass,SizeOf(TWndClass));
Registered := Windows.GetClassInfo(hInstance,PChar(fWindowClassName),{%H-}TempClass);
If not Registered or (TempClass.lpfnWndProc <> @DefWindowProc) then
  begin
    If Registered then
      Windows.UnregisterClass(PChar(fWindowClassName),hInstance);
    WindowClass.lpszClassName := PChar(fWindowClassName);
    WindowClass.hInstance := hInstance;
    WindowClass.lpfnWndProc := @DefWindowProc;
    If Windows.RegisterClass(WindowClass) = 0 then
      raise Exception.CreateFmt('TUtilityWindowManager.RegisterWindowClass: ' +
        'Unable to register utility window class. (%s)',[SysErrorMessage(GetLastError)]);
  end;
end;

//------------------------------------------------------------------------------

procedure TUtilityWindowManager.UnregisterWindowClass;
begin
Windows.UnregisterClass(PChar(fWindowClassName),hInstance);
end;

//------------------------------------------------------------------------------

Function TUtilityWindowManager.NewHelper(Method: TMethod): Pointer;
var
  i:    Integer;
{$IFDEF PurePascal}
  Temp: PMethodItem;
begin
For i := 0 to Pred(fMaxWindows) do
  begin
    Temp := {%H-}PMethodItem({%H-}PtrUInt(fHelpers) + PtrUInt(i * SizeOf(TMethodItem)));
    If Temp^.Assigned = 0 then
      begin
        Temp^.Method := Method;
        Temp^.Assigned := PtrUInt(-1);
        Result := Pointer(Temp);
        Exit;
      end;
  end;
{$ELSE}
  Temp: PMediator;
begin
For i := 0 to Pred(fMaxWindows) do
  begin
    Temp := {%H-}PMediator({%H-}PtrUInt(fHelpers) + PtrUInt(i * SizeOf(TMediator)));
    If not Assigned(Temp^.MethodInfo) then
      begin
        Temp^ := def_Mediator;
        Temp^.MethodInfo := Addr(Temp^.MethodCode);
      {$IFDEF x64}
        Temp^.HandlerAddr := @HandlerCaller;
      {$ELSE}
        Temp^.HandlerAddr := @WndHandler;
      {$ENDIF}
        Temp^.MethodCode := Method.Code;
        Temp^.MethodData := Method.Data;
        Result := Pointer(Temp);
        Exit;
      end;
  end;
{$ENDIF}
raise Exception.Create('TUtilityWindowManager.NewHelper: Out of resources.');
end;

//------------------------------------------------------------------------------

procedure TUtilityWindowManager.RemoveHelper(Helper: Pointer);
begin
{$IFDEF PurePascal}
If ({%H-}PtrUInt(Helper) >= {%H-}PtrUInt(fHelpers)) and
   ({%H-}PtrUInt(Helper) <= ({%H-}PtrUInt(fHelpers) +
    PtrUInt(Pred(fMaxWindows) * SizeOf(TMethodItem)))) then
  PMethodItem(Helper)^.Assigned := 0;
{$ELSE}
If ({%H-}PtrUInt(Helper) >= {%H-}PtrUInt(fHelpers)) and
   ({%H-}PtrUInt(Helper) <= ({%H-}PtrUInt(fHelpers) +
    PtrUInt(Pred(fMaxWindows) * SizeOf(TMediator)))) then
  PMediator(Helper)^.MethodInfo := nil;
{$ENDIF}
end;

{------------------------------------------------------------------------------}
{   TUtilityWindowManager - public methods                                     }
{------------------------------------------------------------------------------}

constructor TUtilityWindowManager.Create(WindowClassName: String = def_WindowClass; MaxWindows: Integer = def_MaxWindows);
begin
inherited Create;
fWindowClassName := WindowClassName;
fMaxWindows := MaxWindows;
fSynchronizer := TCriticalSection.Create;
{$IFDEF PurePascal}
fHelpers := AllocMem(fMaxWindows * SizeOf(TMethodItem));
{$ELSE}
fHelpers := VirtualAlloc(nil,fMaxWindows * SizeOf(TMediator),MEM_COMMIT,PAGE_EXECUTE_READWRITE);
{$ENDIF}
fWindowCount := 0;
RegisterWindowClass;
end;

//------------------------------------------------------------------------------

destructor TUtilityWindowManager.Destroy;
begin
UnregisterWindowClass;
{$IFDEF PurePascal}
FreeMem(fHelpers,fMaxWindows * SizeOf(TMethodItem));
{$ELSE}
VirtualFree(fHelpers,fMaxWindows * SizeOf(TMediator),MEM_RELEASE);
{$ENDIF}
fSynchronizer.Free;
inherited;
end;

//------------------------------------------------------------------------------

Function TUtilityWindowManager.AllocateHWND(Method: TWndMethod): HWND;
begin
Result := 0;
fSynchronizer.Enter;
try
  If fWindowCount < MaxWindows then
    begin
      Result := CreateWindowEx(WS_EX_TOOLWINDOW,PChar(fWindowClassName),'',
                               WS_POPUP,0,0,0,0,0,0,hInstance,nil);
      If Result = 0 then
        raise Exception.CreateFmt('TUtilityWindowManager.AllocateHWND: ' +
          'Unable to create utility window. %s',[SysErrorMessage(GetLastError)]);
    {$IFDEF PurePascal}
      SetWindowLongPtr(Result,GWLP_WNDPROC,{%H-}LONG_PTR(@WndHandler));
      SetWindowLongPtr(Result,GWLP_USERDATA,{%H-}LONG_PTR(NewHelper(TMethod(Method))));
    {$ELSE}
      SetWindowLongPtr(Result,GWLP_WNDPROC,{%H-}LONG_PTR(NewHelper(TMethod(Method))));
    {$ENDIF}
      Inc(fWindowCount);
    end
  else raise Exception.Create('TUtilityWindowManager.AllocateHWND: Unable to create new mediator.');
finally
  fSynchronizer.Leave;
end;
end;

//------------------------------------------------------------------------------

procedure TUtilityWindowManager.DeallocateHWND(Wnd: HWND);
var
  Helper: Pointer;
begin
fSynchronizer.Enter;
try
{$IFDEF PurePascal}
  Helper := {%H-}Pointer(GetWindowLongPtr(Wnd,GWLP_USERDATA));
{$ELSE}
  Helper := {%H-}Pointer(GetWindowLongPtr(Wnd,GWLP_WNDPROC));
{$ENDIF}
  DestroyWindow(Wnd);
  RemoveHelper(Helper);
  Dec(fWindowCount);
finally
  fSynchronizer.Leave;
end;
end;

{==============================================================================}
{   Implementation of implicit manager                                         }
{==============================================================================}

{$IFDEF ImplicitManager}
var
  UtilityWindowManager: TUtilityWindowManager;
  
//------------------------------------------------------------------------------

Function AllocateHWND(Method: TWndMethod): HWND;
begin
Result := UtilityWindowManager.AllocateHWND(Method);
end;

//------------------------------------------------------------------------------

procedure DeallocateHWND(Wnd: HWND);
begin
UtilityWindowManager.DeallocateHWND(Wnd);
end;

//------------------------------------------------------------------------------

initialization
  UtilityWindowManager := TUtilityWindowManager.Create;

finalization
  UtilityWindowManager.Free;

{$ENDIF}

end.

