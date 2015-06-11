// unit Magnetic
//
//  TMagnetic object is a Delphi version equivalent of Visual Basic "cMagneticWnd" class
//   written by Emil Weiss.
//  The forms adapting this object snap to each other, and the form defined as Parent
//    form drags its child forms snapped to that, at beging dragged by user.
//  The original "cMagneticWnd" class directly implants hooking code to sub class
//   window message procedure of form, so we do not need to put extra code to sub class
//   at programming Visual Basic application with that.
//  But straight conversioned Delphi object using above method does not work, (There should
//   be some modifications to adjust some differences between Visual Basic and Delphi)
//   and I could not find out solution.
//  So, I decided to use custom message handler which indirectly calls "zSubclass_Proc" of
//   TMagnetic object, for Delphi version.
//  And we should put extra code per each Form unit to define custom message handler to use
//   TMagnetic object.
//
//  Rewritten by Silhwan Hyun   ( 04 Dec 2008 )
//
// Repack by Alex_pac Тольятти 2012
// http://jqbook.narod.ru/delphi/magnetic.htm


unit Magnetic;

interface

uses
  Windows, SysUtils, Messages;

type
  TWnd_Info = record
    h_wnd      : HWND;
    hWndParent : HWND;
    Glue       : Boolean;
  end;
  PWnd_Info = ^TWnd_Info;

  //TSubClass_Proc = function(lng_hWnd: HWND; uMsg: Integer; var Msg: TMessage; var bHandled: Boolean) : boolean;

  TMagnetic = class
   private
    FSnapWidth    : integer;
    m_uWndInfo    : array of TWnd_Info;
    m_rcWnd       : array of TRect;
    m_lWndCount   : Integer;
    m_ptAnchor    : TPoint;
    m_ptOffset    : TPoint;
    m_ptCurr      : TPoint;
    m_ptLast      : TPoint;

    function  GetSnapWidth: Integer;
    procedure SetSnapWidth(const Value: Integer);
    procedure pvSizeRect(Handle: HWND; var rcWnd: TRect; lfEdge: Integer);
    procedure pvMoveRect(Handle: HWND; var rcWnd: TRect);
    procedure pvCheckGlueing;
    function  pvWndsConnected(rcWnd1: TRect; rcWnd2: TRect): Boolean;
    function  pvWndGetInfoIndex(Handle: HWND): Integer;
    function  pvWndParentGetInfoIndex(hWndParent: HWND): Integer;
    procedure zSubclass_Proc(lng_hWnd: HWND; uMsg, wParam, lParam: Integer;
      var lReturn: Integer;  var bHandled: Boolean);
  public
    constructor Create;
    Destructor Destroy; Override;
    function  AddWindow(Handle: HWND; hWndParent: HWND): Boolean;
    function  RemoveWindow(Handle: HWND): Boolean;
    procedure CheckGlueing;
    property  SnapWidth: Integer read GetSnapWidth write SetSnapWidth;
  end;

var
  MagneticWnd: TMagnetic;

implementation

const
  LB_RECT = 16;

//
function Subclass_Proc(lng_hWnd: HWND;uMsg: Integer; var Msg: TMessage; var bHandled: Boolean) : Boolean;
begin
  Result := Assigned(MagneticWnd);
  if Result then
    MagneticWnd.zSubclass_Proc(lng_hWnd, uMsg, Msg.wParam, Msg.lParam, Msg.Result, bHandled);
end;

// procedure to subclass ChildForms window procedure for magnetic effect.
function SubFormWindowProc(Wnd: HWND; Msg, wParam, lParam: Integer): Integer; stdcall;
var
  Handled: Boolean;
  Message_: TMessage;
  OrgWndProc: Integer;
begin
  Result := 0;
  OrgWndProc := GetWindowLong(Wnd, GWL_USERDATA);
  if (OrgWndProc = 0) then
    exit;

  if not Assigned(MagneticWnd) then
  begin
    Result := CallWindowProc(Pointer(OrgWndProc), Wnd, Msg, wParam, lParam);
    exit;
  end;

  Message_.WParam := wParam;
  Message_.LParam := lParam;
  Message_.Result := 0;

  if (Msg = WM_SYSCOMMAND) or
     (Msg = WM_ENTERSIZEMOVE) or
     (Msg = WM_EXITSIZEMOVE) or
     (Msg = WM_WINDOWPOSCHANGED) or
     (Msg = WM_COMMAND) then
  begin
    Result := CallWindowProc(Pointer(OrgWndProc), Wnd, Msg, wParam, lParam);
    Subclass_Proc(Wnd, Msg, Message_, Handled);
  end else
  if (Msg = WM_MOVING) or (Msg = WM_SIZING) then
  begin
    Subclass_Proc(Wnd, Msg, Message_, Handled);
    if Handled then
    begin
      Result := Message_.Result;
      exit;
    end else
      Result := CallWindowProc(Pointer(OrgWndProc), Wnd, Msg, wParam, lParam);
  end else
  if (Msg = WM_DESTROY) then
  begin
    if Assigned(MagneticWnd) then
      MagneticWnd.RemoveWindow(Wnd);

    Result := CallWindowProc(Pointer(OrgWndProc), Wnd, Msg, wParam, lParam);
  end else
    Result := CallWindowProc(Pointer(OrgWndProc), Wnd, Msg, wParam, lParam);
end;

{ TMagnetic }

constructor TMagnetic.create;
begin
 // Default snap width
  SnapWidth := 15;
  // Initialize registered number of window
  m_lWndCount := 0;
end;

destructor TMagnetic.Destroy;
begin
   MagneticWnd := nil;
   SetLength(m_uWndInfo, 0);  // not sure this is needed
   SetLength(m_rcWnd, 0);     // not sure this is needed
   inherited;
end;

function TMagnetic.GetSnapWidth: Integer;
begin
  Result := FSnapWidth;
end;

procedure TMagnetic.SetSnapWidth(const Value: Integer);
begin
  FSnapWidth := Value;
end;

procedure TMagnetic.zSubclass_Proc(lng_hWnd: HWND;
                                   uMsg, wParam, lParam: Integer;
                                   var lReturn: Integer;
                                   var bHandled: Boolean);
{
Parameters:
   lng_hWnd - The window handle
   uMsg     - The message number
   wParam   - Message related data
   lParam   - Message related data
   lReturn  - Set this variable as per your intentions and requirements, see the MSDN
              documentation or each individual message value.
   bHandled - Set this variable to True in a 'before' callback to prevent the message being
              subsequently processed by the default handler... and if set, an 'after' callback
}

{
Notes:
   If you really know what you're doing, it's possible to change the values of the
   lng_hWnd, uMsg, wParam and lParam parameters in a 'before' callback so that different
   values get passed to the default handler.. and optionaly, the 'after' callback
}
var
  rcWnd: TRect;
  i: Integer;
  PWindowPos: ^TWindowPos;
begin
  bHandled := False;

  case uMsg of
    // Size/Move starting
    WM_ENTERSIZEMOVE:
      begin
        // Get Desktop area (as first rectangle)
        SystemParametersInfo(SPI_GETWORKAREA, 0, @m_rcWnd[0], 0);

        // Get rectangles of all handled windows
        for i := 1 to m_lWndCount do
        begin
          // Window maximized ?
          if (IsZoomed(m_uWndInfo[i].h_wnd)) then
            CopyMemory(@m_rcWnd[i], @m_rcWnd[0], LB_RECT) // Take work are rectangle
          else
            GetWindowRect((m_uWndInfo[i].h_wnd), m_rcWnd[i]); // Get window rectangle

          // Is it our current window ?
          if (m_uWndInfo[i].h_wnd = lng_hWnd) then
          begin
            // Get anchor-offset
            GetCursorPos(m_ptAnchor);
            GetCursorPos(m_ptLast);
            m_ptOffset.x := m_rcWnd[i].Left - m_ptLast.x;
            m_ptOffset.y := m_rcWnd[i].Top - m_ptLast.y;
          end;
        end;
      end;
    // Sizing
    WM_SIZING:
      begin
        CopyMemory(@rcWnd, pointer(lParam), LB_RECT);
        pvSizeRect(lng_hWnd, rcWnd, wParam);
        CopyMemory(pointer(lParam), @rcWnd, LB_RECT);

        bHandled := True;
        lReturn := 1;
      end;
    // Moving
    WM_MOVING:
      begin
        CopyMemory(@rcWnd, pointer(lParam), LB_RECT);
        pvMoveRect(lng_hWnd, rcWnd);
        CopyMemory(pointer(lParam), @rcWnd, LB_RECT);

        bHandled := True;
        lReturn := 1;
      end;
    // Size/Move finishing
    WM_EXITSIZEMOVE:
      begin
        pvCheckGlueing;
      end;
    // at after Shown or Hidden window
    WM_WINDOWPOSCHANGED:  // ************** Added
      begin
        PWindowPos := pointer(lParam);
        if ((PWindowPos^.flags and SWP_SHOWWINDOW) = SWP_SHOWWINDOW) or
           ((PWindowPos^.flags and SWP_HIDEWINDOW) = SWP_HIDEWINDOW) then
          pvCheckGlueing;
      end;
    // Special case: *menu* call
    WM_SYSCOMMAND:
      begin
        if (wParam = SC_MINIMIZE) or (wParam = SC_RESTORE) then
          pvCheckGlueing;
      end;
    // Special case: *control* call
    WM_COMMAND:
      begin
        pvCheckGlueing;
      end;
  end;
end;

function TMagnetic.AddWindow(Handle: HWND; hWndParent: HWND): Boolean;
var
  i: Integer;
  OrgWndProc: Integer;
begin
  Result := False;  // assume failure

  // Already in collection ?
  for i := 1 to m_lWndCount do
    if (Handle = m_uWndInfo[i].h_wnd) then
      Exit;

  // Validate windows
  if IsWindow(Handle) and (IsWindow(hWndParent) or (hWndParent = 0)) then  //********* Changed
  begin
    // Increase count
    inc(m_lWndCount);

    // Resize arrays
    SetLength(m_uWndInfo, m_lWndCount+1);
    SetLength(m_rcWnd, m_lWndCount+1);

    // Add info
    m_uWndInfo[m_lWndCount].h_wnd := Handle;
    if hWndParent = Handle then      // Parent window is Self window ?      //******** Added
      m_uWndInfo[m_lWndCount].hWndParent := 0  // Then same to "no parent"  //******** Added
    else
      m_uWndInfo[m_lWndCount].hWndParent := hWndParent;

    // Check glueing for first time
    pvCheckGlueing;

    //FuncPointer := Subclass_Proc;

    // ## RUS переопределяем процедуру обработки Windows Событий

    // Subclassing sub form, the original Window Proc is saved in its own 32-bit value space.
    OrgWndProc := GetWindowLong(Handle, GWL_WNDPROC);
    SetWindowLong(Handle, GWL_USERDATA, OrgWndProc);  // Save Original Window Proc
    SetWindowLong(Handle, GWL_WNDPROC, Integer(@SubFormWindowProc));

    // Success
    Result := True;
  end;
end;

function TMagnetic.RemoveWindow(Handle: HWND): Boolean;
var
  lc1 : Integer;
  lc2 : Integer;
  OrgWndProc: Integer;
begin
  Result := False;  // assume failure

  for lc1 := 1 To m_lWndCount do
    if (Handle = m_uWndInfo[lc1].h_wnd) then
    begin
      // Move down
      for lc2 := lc1 to (m_lWndCount - 1) do
          m_uWndInfo[lc2] := m_uWndInfo[lc2 + 1];

      // Resize arrays
        dec(m_lWndCount);
        SetLength(m_uWndInfo, m_lWndCount+1);
        SetLength(m_rcWnd, m_lWndCount+1);

      // Remove parent relationships
      for lc2 := 1 to m_lWndCount do
        if (m_uWndInfo[lc2].hWndParent = Handle) then
          m_uWndInfo[lc2].hWndParent := 0;

      // verify connections
      pvCheckGlueing;

      //# RUS Отключаем процедуру обработки событий

      OrgWndProc := GetWindowLong(Handle, GWL_USERDATA);
      if (OrgWndProc > 0) then
      begin
        SetWindowLong(Handle, GWL_WNDPROC, OrgWndProc);
        SetWindowLong(Handle, GWL_USERDATA, 0);
      end;

      // Success
      Result := True;
      Break;
    end;
end;

procedure TMagnetic.CheckGlueing;
begin
  // Check ALL windows for possible new *connections*.
  pvCheckGlueing;
end;

procedure TMagnetic.pvSizeRect(Handle: HWND; var rcWnd: TRect; lfEdge: integer);
var
  rcTmp: TRect;
  i:    integer;
begin
  // Get a copy
  CopyMemory(@rcTmp, @rcWnd, LB_RECT);

  // Check all windows
  for i := 0 to m_lWndCount do
    with m_rcWnd[i] do
    begin
      // Avoid hidden window
      if i <> 0 then  // m_rcWnd[0] has the window rect of Desktop area
        if not IsWindowVisible(m_uWndInfo[i].h_wnd) then   // **************** Added
           continue;

      // Avoid current window
      if (m_uWndInfo[i].h_wnd <> Handle) then
      begin
        // X magnetism
        if (rcWnd.Top < Bottom + SnapWidth) and (rcWnd.Bottom > Top - SnapWidth) then
          case lfEdge of

            WMSZ_LEFT, WMSZ_TOPLEFT, WMSZ_BOTTOMLEFT:
              begin
                if Abs(rcTmp.Left - Left) < SnapWidth then
                  rcWnd.Left := Left;

                if Abs(rcTmp.Left - Right) < SnapWidth then
                  rcWnd.Left := Right;
              end;

            WMSZ_RIGHT, WMSZ_TOPRIGHT, WMSZ_BOTTOMRIGHT:
              begin
                if Abs(rcTmp.Right - Left) < SnapWidth then
                  rcWnd.Right := Left;

                if Abs(rcTmp.Right - Right) < SnapWidth then
                  rcWnd.Right := Right;
              end;
          end;

        // Y magnetism
        if (rcWnd.Left < Right + SnapWidth) and (rcWnd.Right > Left - SnapWidth) then
          case lfEdge of

            WMSZ_TOP, WMSZ_TOPLEFT, WMSZ_TOPRIGHT:
              begin
                if Abs(rcTmp.Top - Top) < SnapWidth then
                  rcWnd.Top := Top;

                if Abs(rcTmp.Top - Bottom) < SnapWidth then
                  rcWnd.Top := Bottom;
              end;

            WMSZ_BOTTOM, WMSZ_BOTTOMLEFT, WMSZ_BOTTOMRIGHT:
              begin
                if Abs(rcTmp.Bottom - Top) < SnapWidth then
                  rcWnd.Bottom := Top;

                if Abs(rcTmp.Bottom - Bottom) < SnapWidth then
                  rcWnd.Bottom := Bottom;
              end;
          end;
      end;
    end;
end;

procedure TMagnetic.pvMoveRect(Handle: HWND; var rcWnd: TRect);
var
  lc1:   integer;
  lc2:   integer;
  lWId:  integer;
  rcTmp: TRect;
  lOffx: integer;
  lOffy: integer;
  hDWP:  integer;
begin
  // Get current cursor position
  GetCursorPos(m_ptCurr);

  // Check magnetism for current window
  // 'Move' current window
  OffsetRect(rcWnd, (m_ptCurr.x - rcWnd.Left) + m_ptOffset.x, 0);
  OffsetRect(rcWnd, 0, (m_ptCurr.y - rcWnd.Top) + m_ptOffset.y);

  lOffx := 0;
  lOffy := 0;

  // Check all windows
  for lc1 := 0 to m_lWndCount do
  begin
    // Avoid hidden window
    if lC1 <> 0 then  // m_rcWnd[0] has the window rect of Desktop area
      if not IsWindowVisible(m_uWndInfo[lc1].h_wnd) then   // **************** Added
         continue;

    // Avoid current window
    if (m_uWndInfo[lc1].h_wnd <> Handle) then
    begin
      // Avoid child windows
      if (m_uWndInfo[lc1].Glue = False) or (m_uWndInfo[lc1].hWndParent <> Handle) then
        with m_rcWnd[lc1] do
        begin
          // X magnetism
          if (rcWnd.Top < Bottom + SnapWidth) and (rcWnd.Bottom > Top - SnapWidth) then
          begin
            if Abs(rcWnd.Left - Left) < SnapWidth then
              lOffx := Left - rcWnd.Left;

            if Abs(rcWnd.Left - Right) < SnapWidth then
              lOffx := Right - rcWnd.Left;

            if Abs(rcWnd.Right - Left) < SnapWidth then
              lOffx := Left - rcWnd.Right;

            if Abs(rcWnd.Right - Right) < SnapWidth then
              lOffx := Right - rcWnd.Right;
          end;

          // Y magnetism
          if (rcWnd.Left < Right + SnapWidth) and (rcWnd.Right > Left - SnapWidth) then
          begin
            if Abs(rcWnd.Top - Top) < SnapWidth then
              lOffy := Top - rcWnd.Top;

            if Abs(rcWnd.Top - Bottom) < SnapWidth then
              lOffy := Bottom - rcWnd.Top;

            if Abs(rcWnd.Bottom - Top) < SnapWidth then
              lOffy := Top - rcWnd.Bottom;

            if Abs(rcWnd.Bottom - Bottom) < SnapWidth then
              lOffy := Bottom - rcWnd.Bottom;
          end;
        end;
    end;
  end;

  // Check magnetism for child windows
  for lc1 := 1 to m_lWndCount do
  begin
    // Avoid hidden window
    if not IsWindowVisible(m_uWndInfo[lc1].h_wnd) then   // **************** Added
       continue;

    // Child and connected window ?
    if (m_uWndInfo[lc1].Glue) and (m_uWndInfo[lc1].hWndParent = Handle) then
    begin
      // 'Move' child window
      CopyMemory(@rcTmp, @m_rcWnd[lc1], LB_RECT);
      OffsetRect(rcTmp, m_ptCurr.x - m_ptAnchor.x, 0);
      OffsetRect(rcTmp, 0, m_ptCurr.y - m_ptAnchor.y);

      for lc2 := 0 to m_lWndCount do
        if (lc1 <> lc2) then
        begin
          // Avoid hidden window
          if not IsWindowVisible(m_uWndInfo[lc2].h_wnd) then   // **************** Added
             continue;

          // Avoid child windows
          if (m_uWndInfo[lc2].Glue = False) and (m_uWndInfo[lc2].h_wnd <> Handle) then
            with m_rcWnd[lc2] do
            begin
              // X magnetism
              if (rcTmp.Top < Bottom + SnapWidth) and
                 (rcTmp.Bottom > Top - SnapWidth) then
              begin
                if Abs(rcTmp.Left - Left) < SnapWidth then
                  lOffx := Left - rcTmp.Left;

                if Abs(rcTmp.Left - Right) < SnapWidth then
                  lOffx := Right - rcTmp.Left;

                if Abs(rcTmp.Right - Left) < SnapWidth then
                  lOffx := Left - rcTmp.Right;

                if Abs(rcTmp.Right - Right) < SnapWidth then
                  lOffx := Right - rcTmp.Right;
              end;

              // Y magnetism
              if (rcTmp.Left < Right + SnapWidth) and
                 (rcTmp.Right > Left - SnapWidth) then
              begin
                if Abs(rcTmp.Top - Top) < SnapWidth then
                  lOffy := Top - rcTmp.Top;

                if Abs(rcTmp.Top - Bottom) < SnapWidth then
                  lOffy := Bottom - rcTmp.Top;

                if Abs(rcTmp.Bottom - Top) < SnapWidth then
                  lOffy := Top - rcTmp.Bottom;

                if Abs(rcTmp.Bottom - Bottom) < SnapWidth then
                  lOffy := Bottom - rcTmp.Bottom;
              end;
            end;
        end;
    end;
  end;

  // Apply offsets
  OffsetRect(rcWnd, lOffx, lOffy);

  // Glueing (move child windows, if any)
  hDWP := BeginDeferWindowPos(1);

  for lc1 := 1 to m_lWndCount do
  begin
    // Avoid hidden window
    if not IsWindowVisible(m_uWndInfo[lc1].h_wnd) then   // **************** Added
      continue;

    with m_uWndInfo[lc1] do
      // Is parent our current window ?
      if (hWndParent = Handle) and (Glue) then
      begin
        // Move 'child' window
        lWId := pvWndGetInfoIndex(Handle);
        with m_rcWnd[lc1] do
          DeferWindowPos(hDWP, m_uWndInfo[lc1].h_wnd, 0,
                         Left - (m_rcWnd[lWId].Left - rcWnd.Left),
                         Top - (m_rcWnd[lWId].Top - rcWnd.Top),
                         0{width}, 0{height},  // No size change
                         SWP_NOACTIVATE or SWP_NOSIZE or SWP_NOZORDER);
      end;
  end;


  EndDeferWindowPos(hDWP);
  // Store last cursor position
  m_ptLast := m_ptCurr;
end;

procedure TMagnetic.pvCheckGlueing;
var
  lcMain: integer;
  lc1:    integer;
  lc2:    integer;
  lWId:   integer;
begin
  // Get all windows rectangles / Reset glueing
  for lc1 := 1 to m_lWndCount do
  begin
    GetWindowRect(m_uWndInfo[lc1].h_wnd, m_rcWnd[lc1]);
    m_uWndInfo[lc1].Glue := False;
  end;

  // Check direct connection
  for lc1 := 1 to m_lWndCount do
  begin
    if not IsWindowVisible(m_uWndInfo[lc1].h_wnd) then   // **************** Added
       continue;

    if (m_uWndInfo[lc1].hWndParent <> 0) then
    begin
      // Get parent window info index
      lWId := pvWndParentGetInfoIndex(m_uWndInfo[lc1].hWndParent);
      // Connected ?
      m_uWndInfo[lc1].Glue := pvWndsConnected(m_rcWnd[lWId], m_rcWnd[lc1]);
    end;
  end;

  // Check indirect connection
  for lcMain := 1 to m_lWndCount do  // to check the windows snapped far lower level
    for lc1 := 1 to m_lWndCount do
    begin
      // Avoid hidden window
      if not IsWindowVisible(m_uWndInfo[lc1].h_wnd) then   // **************** Added
         continue;

      if (m_uWndInfo[lc1].Glue) then
        for lc2 := 1 to m_lWndCount do
        begin
          // Avoid hidden window
          if not IsWindowVisible(m_uWndInfo[lc2].h_wnd) then   // **************** Added
             continue;

          if (lc1 <> lc2) then
            if (m_uWndInfo[lc1].hWndParent = m_uWndInfo[lc2].hWndParent) then
              // Connected ?
              if (m_uWndInfo[lc2].Glue = False) then
                m_uWndInfo[lc2].Glue := pvWndsConnected(m_rcWnd[lc1], m_rcWnd[lc2]);
        end;  // end of for lc2
    end;   // end of for lc1
end;

function TMagnetic.pvWndsConnected(rcWnd1: TRect; rcWnd2: TRect): Boolean;
var
  rcUnion: TRect;
begin
  // Calc. union rectangle of windows
  UnionRect(rcUnion, rcWnd1, rcWnd2);

  // Bounding glue-rectangle
  Result := ((rcUnion.Right  - rcUnion.Left) <= (rcWnd1.Right  - rcWnd1.Left  ) + (rcWnd2.Right  - rcWnd2.Left)) and
            ((rcUnion.Bottom - rcUnion.Top ) <= (rcWnd1.Bottom - rcWnd1.Top   ) + (rcWnd2.Bottom - rcWnd2.Top )) and
            ((rcWnd1.Left    = rcWnd2.Left ) or (rcWnd1.Left   = rcWnd2.Right ) or
             (rcWnd1.Right   = rcWnd2.Left ) or (rcWnd1.Right  = rcWnd2.Right ) or
             (rcWnd1.Top     = rcWnd2.Top  ) or (rcWnd1.Top    = rcWnd2.Bottom) or
             (rcWnd1.Bottom  = rcWnd2.Top  ) or (rcWnd1.Bottom = rcWnd2.Bottom));
end;

function TMagnetic.pvWndGetInfoIndex(Handle: HWND): integer;
var
  i: integer;
begin
  Result := -1;   // assume no matched item

  for i := 1 to m_lWndCount do
    if (m_uWndInfo[i].h_wnd = Handle) then
    begin
      Result := i;
      Break;
    end;
end;

function TMagnetic.pvWndParentGetInfoIndex(hWndParent: HWND): integer;
var
  i: integer;
begin
  Result := -1;   // assume no matched item

  for i := 1 to m_lWndCount do
    if (m_uWndInfo[i].h_wnd = hWndParent) then
    begin
      Result := i;
      Break;
    end;
end;

initialization

// запуск юнита
MagneticWnd:=TMagnetic.Create;

finalization

MagneticWnd.Free;

end.

