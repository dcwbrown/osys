MODULE WinGui;
IMPORT SYSTEM, H := WinHost;

CONST KeyBufSize = 16;


TYPE
  HostBitmap* = POINTER TO HostBitmapDesc;
  HostBitmapDesc* = RECORD
    width*:   INTEGER;
    height*:  INTEGER;
    address*: INTEGER;    (* Address of bitmap data                                  *)
    hdib:     INTEGER;    (* Device independant bitmap handle                        *)
    context:  INTEGER;    (* Device context that currently contains the bitmap       *)
    oldh:     INTEGER     (* Old handle for restoration following context operations *)
  END;

HostWindow* = POINTER TO HostWindowDesc;
  HostWindowDesc = RECORD
    hwnd:     INTEGER;
    bmp*:     HostBitmap;
    x*:       INTEGER;
    y*:       INTEGER;
    width*:   INTEGER;
    height*:  INTEGER;
    DPI*:     INTEGER;
    highch:   INTEGER;  (* Leading/high surrogate codepoint of a pair *)
  END;

  HostMessage* = RECORD-
    hwnd:     INTEGER;
    message:  SYSTEM.CARD32;
    pad:      SYSTEM.CARD32;
    wParam:   INTEGER;
    lParam:   INTEGER;
    time:     SYSTEM.CARD32;
    x, y:     SYSTEM.CARD32;
    lPrivate: SYSTEM.CARD32;
  END;

VAR
  Width*:  INTEGER;
  Height*: INTEGER;

  Window*: HostWindow;

  Keys:    ARRAY KeyBufSize OF BYTE;  (* Scancodes *)
  KeyFull: BOOLEAN;
  KeyIn:   INTEGER;
  KeyOut:  INTEGER;

  VKtoScn: INTEGER;

  MouseState*: INTEGER;  (* 3/keys, 12/x, 12/y  *)
  WmQuit*:     BOOLEAN;

PROCEDURE AddKey(key: BYTE);
BEGIN
  IF ~KeyFull THEN
    Keys[KeyIn] := key;
    KeyIn       := (KeyIn + 1) MOD KeyBufSize;
    KeyFull     := KeyIn = KeyOut
  END
END AddKey;

PROCEDURE InvalidateRect* (x, y, width, height: INTEGER);
VAR rect: RECORD- left, top, right, bottom: SYSTEM.INT32 END;
BEGIN
  rect.left := x;  rect.right  := x + width;
  rect.top  := y;  rect.bottom := y + height;
  ASSERT(H.InvalidateRect(Window.hwnd, SYSTEM.ADR(rect), 0) # 0)
END InvalidateRect;

PROCEDURE Invalidate*;  (* Invalidate entire window *)
BEGIN  InvalidateRect(0, 0, Window.width, Window.height) END Invalidate;


PROCEDURE Close*;
VAR res: INTEGER;
BEGIN
  IF Window # NIL THEN
   res := H.PostMessageW(Window.hwnd, 16 (* WM_CLOSE *), 0, 0)
 END
END Close;


(* ----------------------- Windows bitmap management ------------------------ *)

PROCEDURE EnsureBitmap(w, h: INTEGER; VAR bitmap: HostBitmap);
TYPE
  BITMAPINFO = RECORD-
    size:          SYSTEM.CARD32;
    width:         SYSTEM.CARD32;
    height:        SYSTEM.CARD32;
    planes:        SYSTEM.CARD16;
    bitCount:      SYSTEM.CARD16;
    compression:   SYSTEM.CARD32;
    sizeImage:     SYSTEM.CARD32;
    xPelsPerMeter: SYSTEM.CARD32;
    yPelsPerMeter: SYSTEM.CARD32;
    clrUsed:       SYSTEM.CARD32;
    clrImportant:  SYSTEM.CARD32;
    colour0:       SYSTEM.CARD32;
    colour1:       SYSTEM.CARD32;
  END;
VAR
  res: INTEGER;
  bmi: BITMAPINFO;
BEGIN
  IF bitmap = NIL THEN NEW(bitmap); H.ZeroFill(bitmap^) END;

  IF (bitmap.width < w) OR (bitmap.height < h) THEN
    IF bitmap.hdib # 0 THEN
      res := H.SelectObject(bitmap.context, bitmap.oldh);
      res := H.DeleteObject(bitmap.hdib)
    END;
    bitmap.width   := 0;  bitmap.height := 0;
    bitmap.address := 0;  bitmap.hdib   := 0;
    bitmap.oldh    := 0;

    H.ZeroFill(bmi);
    bmi.size     := SYSTEM.SIZE(BITMAPINFO) - 8;  (* Size excludes colours *)
    bmi.width    := w;
    bmi.height   := h;   (* Positive height => y=0 at bottom *)
    bmi.planes   := 1;
    bmi.bitCount := 1;   (* 1 bit per pixel *)
    bmi.colour0  := 0FF000000H; (*0FFFFFFFFH;*)
    bmi.colour1  := 0FFFFFFFFH; (*0FF000000H;*)
    bitmap.hdib  := H.CreateDIBSection(0, SYSTEM.ADR(bmi), 0, SYSTEM.ADR(bitmap.address), 0, 0);
    (*
    H.ws("Created DIB section: hdib "); H.wh(bitmap.hdib);
    H.ws("H, bitmap address ");         H.wh(bitmap.address); H.wsn("H.");
    *)

    ASSERT(bitmap.hdib    # 0);
    ASSERT(bitmap.address # 0);
    bitmap.width  := w;
    bitmap.height := h;

    IF bitmap.context = 0 THEN  (* If no context to reuse *)
      bitmap.context := H.CreateCompatibleDC(0);  ASSERT(bitmap.context # 0);
      (*
      H.ws("Created compatible DC: w ");  H.wi(w);
      H.ws(", h ");                       H.wi(h);
      H.ws(", hdc ");                     H.wh(bitmap.context); H.wsn("H.");
      *)
    END;

    bitmap.oldh := H.SelectObject(bitmap.context, bitmap.hdib)
  END
END EnsureBitmap;


(* -------------------------- Host window handlers -------------------------- *)

PROCEDURE WmPaint(hwnd: INTEGER);
TYPE
  PAINTSTRUCT = RECORD-
    hdc:         INTEGER;
    fErase:      SYSTEM.CARD32;
    left:        SYSTEM.CARD32;
    top:         SYSTEM.CARD32;
    right:       SYSTEM.CARD32;
    bottom:      SYSTEM.CARD32;
    fRestore:    SYSTEM.CARD32;
    fIncUpdate:  SYSTEM.CARD32;
    rgbReserved: ARRAY 36 OF BYTE
  END;
VAR
  res:    INTEGER;
  ps:     PAINTSTRUCT;
  x, y:   INTEGER;
  width:  INTEGER;
  height: INTEGER;
  hdc:    INTEGER;
BEGIN
  ASSERT(Window.hwnd = hwnd);
  EnsureBitmap(Window.width, Window.height, Window.bmp);
  res    := H.BeginPaint(hwnd, SYSTEM.ADR(ps));
  x      := ps.left;
  y      := ps.top;
  width  := ps.right  - ps.left + 1;
  height := ps.bottom - ps.top  + 1;
  (*
  H.ws("WM_PAINT handler: Paint(x ");   H.wi(x);
  H.ws(", y ");       H.wi(y);
  H.ws(", width ");   H.wi(width);
  H.ws(", height ");  H.wi(height);  H.wsn(".");
  *)
  res := H.BitBlt(ps.hdc, x, y, width, height, Window.bmp.context, x, y, 0CC0020H);  (* SRCCPY *)
  IF res = 0 THEN H.AssertWinErr(H.GetLastError()) END;
  res := H.EndPaint(hwnd, SYSTEM.ADR(ps));
END WmPaint;


PROCEDURE WmSize(wi, h: INTEGER);
BEGIN
  IF (wi # Width) OR (h # Height) THEN
    H.ws("size := "); H.wi(wi); H.wc(","); H.wi(h); H.wsn(".")
  END
END WmSize;


PROCEDURE WmMouse(hwnd, msg, x, y: INTEGER; flags: SET);
BEGIN
  IF x > 32767 THEN x := x - 10000H END; (* Sign extend 16 bit value *)
  IF y > 32767 THEN y := y - 10000H END; (* Sign extend 16 bit value *)
  y := Window.height - 1 - y;
  ASSERT(Window.hwnd = hwnd);
  IF    msg = 201H (* WM_LBUTTONDOWN *) THEN H.SetCapture(hwnd)
  ELSIF msg = 202H (* WM_LBUTTONUP   *) THEN H.ReleaseCapture
  END;
  (* Windows button flags:
       MK_LBUTTON  0x0001  {0}   The left mouse button is down.
       MK_MBUTTON  0x0010  {16}  The middle mouse button is down.
       MK_RBUTTON  0x0002  {1}   The right mouse button is down.
     Oberon button flags:
       {0}  MR
       {1}  MM
       {2}  ML
  *)
  MouseState := y MOD 1000H * 1000H + x MOD 1000H;
  IF 1 IN flags THEN INC(MouseState, 1000000H) END; (* MR *)
  IF 4 IN flags THEN INC(MouseState, 2000000H) END; (* MM *)
  IF 0 IN flags THEN INC(MouseState, 4000000H) END; (* ML *)
END WmMouse;


(* ---------------- Windows message names for debug logging ----------------- *)

PROCEDURE WriteWindowsMessageName*(msg: INTEGER);
VAR
  str:  ARRAY 1024 OF CHAR;
  name: ARRAY 1024 OF SYSTEM.CARD16;
  res:  INTEGER;
BEGIN str := "";
  IF    msg = 000H THEN str := "_NULL"
  ELSIF msg = 001H THEN str := "_CREATE"
  ELSIF msg = 002H THEN str := "_DESTROY"
  ELSIF msg = 003H THEN str := "_MOVE"
  ELSIF msg = 005H THEN str := "_SIZE"
  ELSIF msg = 006H THEN str := "_ACTIVATE"
  ELSIF msg = 007H THEN str := "_SETFOCUS"
  ELSIF msg = 008H THEN str := "_KILLFOCUS"
  ELSIF msg = 00AH THEN str := "_ENABLE"
  ELSIF msg = 00BH THEN str := "_SETREDRAW"
  ELSIF msg = 00CH THEN str := "_SETTEXT"
  ELSIF msg = 00DH THEN str := "_GETTEXT"
  ELSIF msg = 00EH THEN str := "_GETTEXTLENGTH"
  ELSIF msg = 00FH THEN str := "_PAINT"
  ELSIF msg = 010H THEN str := "_CLOSE"
  ELSIF msg = 011H THEN str := "_QUERYENDSESSION"
  ELSIF msg = 012H THEN str := "_QUIT"
  ELSIF msg = 013H THEN str := "_QUERYOPEN"
  ELSIF msg = 014H THEN str := "_ERASEBKGND"
  ELSIF msg = 015H THEN str := "_SYSCOLORCHANGE"
  ELSIF msg = 016H THEN str := "_ENDSESSION"
  ELSIF msg = 017H THEN str := "_SYSTEMERROR"
  ELSIF msg = 018H THEN str := "_SHOWWINDOW"
  ELSIF msg = 019H THEN str := "_CTLCOLOR"
  ELSIF msg = 01AH THEN str := "_WININICHANGE"
  ELSIF msg = 01BH THEN str := "_DEVMODECHANGE"
  ELSIF msg = 01CH THEN str := "_ACTIVATEAPP"
  ELSIF msg = 01DH THEN str := "_FONTCHANGE"
  ELSIF msg = 01EH THEN str := "_TIMECHANGE"
  ELSIF msg = 01FH THEN str := "_CANCELMODE"
  ELSIF msg = 020H THEN str := "_SETCURSOR"
  ELSIF msg = 021H THEN str := "_MOUSEACTIVATE"
  ELSIF msg = 022H THEN str := "_CHILDACTIVATE"
  ELSIF msg = 023H THEN str := "_QUEUESYNC"
  ELSIF msg = 024H THEN str := "_GETMINMAXINFO"
  ELSIF msg = 026H THEN str := "_PAINTICON"
  ELSIF msg = 027H THEN str := "_ICONERASEBKGND"
  ELSIF msg = 028H THEN str := "_NEXTDLGCTL"
  ELSIF msg = 02AH THEN str := "_SPOOLERSTATUS"
  ELSIF msg = 02BH THEN str := "_DRAWITEM"
  ELSIF msg = 02CH THEN str := "_MEASUREITEM"
  ELSIF msg = 02DH THEN str := "_DELETEITEM"
  ELSIF msg = 02EH THEN str := "_VKEYTOITEM"
  ELSIF msg = 02FH THEN str := "_CHARTOITEM"
  ELSIF msg = 030H THEN str := "_SETFONT"
  ELSIF msg = 031H THEN str := "_GETFONT"
  ELSIF msg = 032H THEN str := "_SETHOTKEY"
  ELSIF msg = 037H THEN str := "_QUERYDRAGICON"
  ELSIF msg = 039H THEN str := "_COMPAREITEM"
  ELSIF msg = 03DH THEN str := "_GETOBJECT"
  ELSIF msg = 041H THEN str := "_COMPACTING"
  ELSIF msg = 044H THEN str := "_COMMNOTIFY"
  ELSIF msg = 046H THEN str := "_WINDOWPOSCHANGING"
  ELSIF msg = 047H THEN str := "_WINDOWPOSCHANGED"
  ELSIF msg = 048H THEN str := "_POWER"
  ELSIF msg = 04AH THEN str := "_COPYDATA"
  ELSIF msg = 04BH THEN str := "_CANCELJOURNAL"
  ELSIF msg = 04EH THEN str := "_NOTIFY"
  ELSIF msg = 050H THEN str := "_INPUTLANGCHANGEREQUEST"
  ELSIF msg = 051H THEN str := "_INPUTLANGCHANGE"
  ELSIF msg = 052H THEN str := "_TCARD"
  ELSIF msg = 053H THEN str := "_HELP"
  ELSIF msg = 054H THEN str := "_USERCHANGED"
  ELSIF msg = 055H THEN str := "_NOTIFYFORMAT"
  ELSIF msg = 07BH THEN str := "_CONTEXTMENU"
  ELSIF msg = 07CH THEN str := "_STYLECHANGING"
  ELSIF msg = 07DH THEN str := "_STYLECHANGED"
  ELSIF msg = 07EH THEN str := "_DISPLAYCHANGE"
  ELSIF msg = 07FH THEN str := "_GETICON"
  ELSIF msg = 080H THEN str := "_SETICON"
  ELSIF msg = 081H THEN str := "_NCCREATE"
  ELSIF msg = 082H THEN str := "_NCDESTROY"
  ELSIF msg = 083H THEN str := "_NCCALCSIZE"
  ELSIF msg = 084H THEN str := "_NCHITTEST"
  ELSIF msg = 085H THEN str := "_NCPAINT"
  ELSIF msg = 086H THEN str := "_NCACTIVATE"
  ELSIF msg = 087H THEN str := "_GETDLGCODE"
  ELSIF msg = 090H THEN str := "_UAHDESTROYWINDOW"
  ELSIF msg = 091H THEN str := "_UAHDRAWMENU"
  ELSIF msg = 092H THEN str := "_UAHDRAWMENUITEM"
  ELSIF msg = 093H THEN str := "_UAHINITMENU"
  ELSIF msg = 094H THEN str := "_UAHMEASUREMENUITEM"
  ELSIF msg = 095H THEN str := "_UAHNCPAINTMENUPOPUP"
  ELSIF msg = 0A0H THEN str := "_NCMOUSEMOVE"
  ELSIF msg = 0A1H THEN str := "_NCLBUTTONDOWN"
  ELSIF msg = 0A2H THEN str := "_NCLBUTTONUP"
  ELSIF msg = 0A3H THEN str := "_NCLBUTTONDBLCLK"
  ELSIF msg = 0A4H THEN str := "_NCRBUTTONDOWN"
  ELSIF msg = 0A5H THEN str := "_NCRBUTTONUP"
  ELSIF msg = 0A6H THEN str := "_NCRBUTTONDBLCLK"
  ELSIF msg = 0A7H THEN str := "_NCMBUTTONDOWN"
  ELSIF msg = 0A8H THEN str := "_NCMBUTTONUP"
  ELSIF msg = 0A9H THEN str := "_NCMBUTTONDBLCLK"
  ELSIF msg = 100H THEN str := "_KEYDOWN"
  ELSIF msg = 101H THEN str := "_KEYUP"
  ELSIF msg = 102H THEN str := "_CHAR"
  ELSIF msg = 103H THEN str := "_DEADCHAR"
  ELSIF msg = 104H THEN str := "_SYSKEYDOWN"
  ELSIF msg = 105H THEN str := "_SYSKEYUP"
  ELSIF msg = 106H THEN str := "_SYSCHAR"
  ELSIF msg = 107H THEN str := "_SYSDEADCHAR"
  ELSIF msg = 108H THEN str := "_KEYLAST"
  ELSIF msg = 109H THEN str := "_UNICHAR"
  ELSIF msg = 110H THEN str := "_INITDIALOG"
  ELSIF msg = 111H THEN str := "_COMMAND"
  ELSIF msg = 112H THEN str := "_SYSCOMMAND"
  ELSIF msg = 113H THEN str := "_TIMER"
  ELSIF msg = 114H THEN str := "_HSCROLL"
  ELSIF msg = 115H THEN str := "_VSCROLL"
  ELSIF msg = 116H THEN str := "_INITMENU"
  ELSIF msg = 117H THEN str := "_INITMENUPOPUP"
  ELSIF msg = 119H THEN str := "_GESTURE"
  ELSIF msg = 11AH THEN str := "_GESTURENOTIFY"
  ELSIF msg = 11FH THEN str := "_MENUSELECT"
  ELSIF msg = 120H THEN str := "_MENUCHAR"
  ELSIF msg = 121H THEN str := "_ENTERIDLE"
  ELSIF msg = 122H THEN str := "_MENURBUTTONUP"
  ELSIF msg = 123H THEN str := "_MENUDRAG"
  ELSIF msg = 124H THEN str := "_MENUGETOBJECT"
  ELSIF msg = 125H THEN str := "_UNINITMENUPOPUP"
  ELSIF msg = 126H THEN str := "_MENUCOMMAND"
  ELSIF msg = 132H THEN str := "_CTLCOLORMSGBOX"
  ELSIF msg = 133H THEN str := "_CTLCOLOREDIT"
  ELSIF msg = 134H THEN str := "_CTLCOLORLISTBOX"
  ELSIF msg = 135H THEN str := "_CTLCOLORBTN"
  ELSIF msg = 136H THEN str := "_CTLCOLORDLG"
  ELSIF msg = 137H THEN str := "_CTLCOLORSCROLLBAR"
  ELSIF msg = 138H THEN str := "_CTLCOLORSTATIC"
  ELSIF msg = 200H THEN str := "_MOUSEMOVE"
  ELSIF msg = 201H THEN str := "_LBUTTONDOWN"
  ELSIF msg = 202H THEN str := "_LBUTTONUP"
  ELSIF msg = 203H THEN str := "_LBUTTONDBLCLK"
  ELSIF msg = 204H THEN str := "_RBUTTONDOWN"
  ELSIF msg = 205H THEN str := "_RBUTTONUP"
  ELSIF msg = 206H THEN str := "_RBUTTONDBLCLK"
  ELSIF msg = 207H THEN str := "_MBUTTONDOWN"
  ELSIF msg = 208H THEN str := "_MBUTTONUP"
  ELSIF msg = 209H THEN str := "_MBUTTONDBLCLK"
  ELSIF msg = 20AH THEN str := "_MOUSEWHEEL"
  ELSIF msg = 210H THEN str := "_PARENTNOTIFY"
  ELSIF msg = 211H THEN str := "_ENTERMENULOOP"
  ELSIF msg = 212H THEN str := "_EXITMENULOOP"
  ELSIF msg = 213H THEN str := "_NEXTMENU"
  ELSIF msg = 214H THEN str := "_SIZING"
  ELSIF msg = 215H THEN str := "_CAPTURECHANGED"
  ELSIF msg = 216H THEN str := "_MOVING"
  ELSIF msg = 218H THEN str := "_POWERBROADCAST"
  ELSIF msg = 219H THEN str := "_DEVICECHANGE"
  ELSIF msg = 220H THEN str := "_MDICREATE"
  ELSIF msg = 221H THEN str := "_MDIDESTROY"
  ELSIF msg = 222H THEN str := "_MDIACTIVATE"
  ELSIF msg = 223H THEN str := "_MDIRESTORE"
  ELSIF msg = 224H THEN str := "_MDINEXT"
  ELSIF msg = 225H THEN str := "_MDIMAXIMIZE"
  ELSIF msg = 226H THEN str := "_MDITILE"
  ELSIF msg = 227H THEN str := "_MDICASCADE"
  ELSIF msg = 228H THEN str := "_MDIICONARRANGE"
  ELSIF msg = 229H THEN str := "_MDIGETACTIVE"
  ELSIF msg = 230H THEN str := "_MDISETMENU"
  ELSIF msg = 231H THEN str := "_ENTERSIZEMOVE"
  ELSIF msg = 232H THEN str := "_EXITSIZEMOVE"
  ELSIF msg = 233H THEN str := "_DROPFILES"
  ELSIF msg = 234H THEN str := "_MDIREFRESHMENU"
  ELSIF msg = 281H THEN str := "_IME_SETCONTEXT"
  ELSIF msg = 282H THEN str := "_IME_NOTIFY"
  ELSIF msg = 283H THEN str := "_IME_CONTROL"
  ELSIF msg = 284H THEN str := "_IME_COMPOSITIONFULL"
  ELSIF msg = 285H THEN str := "_IME_SELECT"
  ELSIF msg = 286H THEN str := "_IME_CHAR"
  ELSIF msg = 290H THEN str := "_IME_KEYDOWN"
  ELSIF msg = 291H THEN str := "_IME_KEYUP"
  ELSIF msg = 2A1H THEN str := "_MOUSEHOVER"
  ELSIF msg = 2A3H THEN str := "_MOUSELEAVE"
  ELSIF msg = 300H THEN str := "_CUT"
  ELSIF msg = 301H THEN str := "_COPY"
  ELSIF msg = 302H THEN str := "_PASTE"
  ELSIF msg = 303H THEN str := "_CLEAR"
  ELSIF msg = 304H THEN str := "_UNDO"
  ELSIF msg = 305H THEN str := "_RENDERFORMAT"
  ELSIF msg = 306H THEN str := "_RENDERALLFORMATS"
  ELSIF msg = 307H THEN str := "_DESTROYCLIPBOARD"
  ELSIF msg = 308H THEN str := "_DRAWCLIPBOARD"
  ELSIF msg = 309H THEN str := "_PAINTCLIPBOARD"
  ELSIF msg = 30AH THEN str := "_VSCROLLCLIPBOARD"
  ELSIF msg = 30BH THEN str := "_SIZECLIPBOARD"
  ELSIF msg = 30CH THEN str := "_ASKCBFORMATNAME"
  ELSIF msg = 30DH THEN str := "_CHANGECBCHAIN"
  ELSIF msg = 30EH THEN str := "_HSCROLLCLIPBOARD"
  ELSIF msg = 30FH THEN str := "_QUERYNEWPALETTE"
  ELSIF msg = 310H THEN str := "_PALETTEISCHANGING"
  ELSIF msg = 311H THEN str := "_PALETTECHANGED"
  ELSIF msg = 312H THEN str := "_HOTKEY"
  ELSIF msg = 317H THEN str := "_PRINT"
  ELSIF msg = 318H THEN str := "_PRINTCLIENT"
  ELSIF msg = 31FH THEN str := "_DWMNCRENDERINGCHANGED"
  ELSIF msg = 358H THEN str := "_HANDHELDFIRST"
  ELSIF msg = 35FH THEN str := "_HANDHELDLAST"
  ELSIF msg = 360H THEN str := "_AFXFIRST"
  ELSIF msg = 37FH THEN str := "_AFXLAST"
  ELSIF msg = 380H THEN str := "_PENWINFIRST"
  ELSIF msg = 38FH THEN str := "_PENWINLAST"
  ELSIF msg = 390H THEN str := "_COALESCE_FIRST"
  ELSIF msg = 39FH THEN str := "_COALESCE_LAST"
  ELSIF msg = 400H THEN str := "USER"
  ELSIF (msg >= 0C000H) & (msg <= 0FFFFH) THEN
    (* registered cross-application name *)
    name[0] := 23H;  (* '#' *)
    IF H.GetClipboardFormatNameW(msg, SYSTEM.ADR(name)+2, LEN(name)-1) > 0 THEN
      res := H.Utf16ToUtf8(name, str)
    END
  END;
  IF str = "" THEN H.ws("WM "); H.wh(msg); H.wc("H") ELSE H.ws("WM"); H.ws(str) END
END WriteWindowsMessageName;


(* ------- Host window message handling callback procedure - wndproc -------- *)

PROCEDURE- WndProc(hwnd, msg, wp, lp: INTEGER): INTEGER;
VAR res: INTEGER;  scan: BYTE;
BEGIN
  (*
  H.ws("WndProc: hwnd "); H.wh(hwnd);
  H.ws("H, msg "); WriteWindowsMessageName(msg); H.wsn(".");
  *)

  IF Window.hwnd = 0 THEN Window.hwnd := hwnd ELSE ASSERT(Window.hwnd = hwnd) END;
  res := 0;
  IF     msg =   02H  (* WM_DESTROY       *) THEN H.PostQuitMessage(0)
  ELSIF  msg =   0FH  (* WM_PAINT         *) THEN WmPaint(hwnd)
  ELSIF  msg =   14H  (* WM_ERASEBKGND    *) THEN
  ELSIF  msg =   05H  (* WM_SIZE          *) THEN WmSize(lp MOD 10000H, lp DIV 10000H MOD 10000H)
  ELSIF  msg =  100H  (* WM_KEYDOWN       *) THEN SYSTEM.GET(VKtoScn + wp, scan); AddKey(scan)
  ELSIF  msg =  101H  (* WM_KEYUP         *) THEN SYSTEM.GET(VKtoScn + wp, scan); AddKey(0F0H); AddKey(scan)
  ELSIF  msg =  102H  (* WM_CHAR          *) THEN (* Ignore WM_CHAR *)
  ELSIF (msg >= 200H) (* WM_MOUSEMOVE     *)
     &  (msg <= 209H) (* WM_MBUTTONDBLCLK *) THEN WmMouse(hwnd, msg,
                                                          lp MOD 10000H, lp DIV 10000H MOD 10000H,
                                                          SYSTEM.VAL(SET, wp))
  ELSE res := H.DefWindowProcW(hwnd, msg, wp, lp);
  END;
RETURN res END WndProc;


(* -------------------------- Host window creation -------------------------- *)

PROCEDURE CreateWindow*(x, y, width, height: INTEGER);
TYPE
  wndclassexw = RECORD-
    cbsize:        SYSTEM.CARD32;
    style:         SYSTEM.CARD32;
    wndproc:       INTEGER;
    cbClsExtra:    SYSTEM.INT32;
    cbWndExtra:    SYSTEM.INT32;
    hInstance:     INTEGER;
    hIcon:         INTEGER;
    hCursor:       INTEGER;
    hbrBackground: INTEGER;
    lpszMenuName:  INTEGER;
    className:     INTEGER;
    hIconSm:       INTEGER
  END;

VAR
  class:      wndclassexw;
  classAtom:  INTEGER;
  classname:  ARRAY 16  OF SYSTEM.CARD16;
  windowname: ARRAY 256 OF SYSTEM.CARD16;
  i:          INTEGER;
BEGIN
  i := H.Utf8ToUtf16("Oberon", classname);
  i := H.Utf8ToUtf16("Oberon", windowname);
  H.ZeroFill(class);
  class.cbsize    := SYSTEM.SIZE(wndclassexw);
  class.style     := 3;  (* CS_HREDRAW | CS_VREDRAW *)
  class.wndproc   := SYSTEM.ADR(WndProc);
  class.className := SYSTEM.ADR(classname);
  class.hIcon     := 0; (*MakeIcon();*)
  classAtom       := H.RegisterClassExW(SYSTEM.ADR(class));
  ASSERT(classAtom # 0);

  NEW(Window);
  Window.hwnd      := 0;
  Window.bmp       := NIL;
  Window.x         := x;
  Window.y         := y;
  Window.width     := width;
  Window.height    := height;

  (*
  H.ws("CreateWindowExW: x "); H.wi(x);
  H.ws(", y ");                H.wi(y);
  H.ws(", width ");            H.wi(width);
  H.ws(", height ");           H.wi(height); H.wsn(".");
  H.wsn("classname:");  H.DumpMem(2, SYSTEM.ADR(classname),  SYSTEM.ADR(classname),  16);
  H.wsn("windowname:"); H.DumpMem(2, SYSTEM.ADR(windowname), SYSTEM.ADR(windowname), 16);
  *)

  Window.hwnd := H.CreateWindowExW(
    0,                       (* Extended window style *)
    SYSTEM.ADR(classname),
    SYSTEM.ADR(windowname),
    80000000H,               (* WS_POPUP *)
    x, y, width, height,     (* Initial position *)
    0, 0, 0, 0               (* hWndParent, hMenu, hInstance, lpParam *)
  );
  ASSERT(Window.hwnd # 0);
  (*
  H.ws("Created window. hwnd ");  H.wh(Window.hwnd);  H.wsn("H.");
  *)

  H.SetHWnd(Window.hwnd);  (* Make sure kernel error message boxes stop the message pump *)

  EnsureBitmap(width, height, Window.bmp);

  Window.DPI := H.GetDpiForWindow(Window.hwnd);
  (*H.ws("GetDpiForWindow -> ");  H.wi(Window.DPI);  H.wsn(".");*)

  H.ShowCursor(0);
  H.ShowWindow(Window.hwnd, 1);

  (*H.wsn("CreateWindow complete.");*)
END CreateWindow;


(* ------------------------ Host window message loop ------------------------ *)


PROCEDURE ProcessOneMessage* (): INTEGER;  (* 0 - none available, 1 - processed *)
VAR
  msg: HostMessage;
  res: INTEGER;
BEGIN
  res := H.PeekMessageW(SYSTEM.ADR(msg), 0,0,0,1); (* Get and remove message if available *)
  IF res # 0  THEN
    IF msg.message = 12H THEN
      WmQuit := TRUE;
      MouseState := MouseState MOD 1000000H;  (* All mouse buttons up *)
      KeyIn := KeyOut; KeyFull := FALSE;      (* Clear keybard *)
      res := 0
    ELSE
      (*
      IF    msg.message =   2H THEN H.wsn("* WM_DESTROY *")
      ELSIF msg.message =   6H THEN H.wsn("* WM_ACTIVATE *")
      ELSIF msg.message =   7H THEN H.wsn("* WM_SETFOCUS *")
      ELSIF msg.message =   8H THEN H.wsn("* WM_KILLFOCUS *")
      ELSIF msg.message =  10H THEN H.wsn("* WM_CLOSE *")
      ELSIF msg.message =  11H THEN H.wsn("* WM_QUERYENDSESSION *")
      ELSIF msg.message =  16H THEN H.wsn("* WM_ENDSESSION *")
      ELSIF msg.message =  48H THEN H.wsn("* WM_POWER *")
      ELSIF msg.message =  4EH THEN H.wsn("* WM_NOTIFY *")
      ELSIF msg.message =  60H THEN H.wsn("* WM_60 *")
      ELSIF msg.message = 101H THEN H.wsn("* WM_KEYUP *")
      ELSIF msg.message = 104H THEN H.wsn("* WM_SYSKEYDOWN *")
      ELSIF msg.message = 105H THEN H.wsn("* WM_SYSKEYUP *")
      ELSIF msg.message = 106H THEN H.wsn("* WM_SYSCHAR *")
      ELSIF msg.message = 109H THEN H.wsn("* WM_UNICHAR *")
      ELSIF msg.message = 113H THEN H.wsn("* WM_TIMER *")
      ELSE H.ws("Windows message "); H.wh(msg.message); H.wsn("H.");
      END;
      *)
      res := H.TranslateMessage(SYSTEM.ADR(msg));
      res := H.DispatchMessageW(SYSTEM.ADR(msg));
      res := 1;
    END
  END
RETURN res END ProcessOneMessage;


PROCEDURE WaitMsgOrTime*(time: INTEGER);  (* Waits for time (ms) OR message in queue *)
BEGIN H.MsgWaitForMultipleObjects(0, 0, 0, time, 01DFFH) END WaitMsgOrTime;

PROCEDURE Drain;
VAR res: INTEGER;
BEGIN
  REPEAT res := ProcessOneMessage() UNTIL res = 0;
END Drain;


(* ----------------- keyboard and mouse hardware emulation ------------------ *)

PROCEDURE KeyReady*(): BOOLEAN;
BEGIN Drain;
RETURN (KeyIn # KeyOut) OR KeyFull END KeyReady;

PROCEDURE GetKey*(VAR key: BYTE);
BEGIN ASSERT(KeyReady());
  key     := Keys[KeyOut];
  KeyOut  := (KeyOut + 1) MOD KeyBufSize;
  KeyFull := FALSE
END GetKey;

PROCEDURE Mouse*(): INTEGER;
BEGIN Drain;
RETURN MouseState END Mouse;

(* ------------------------- Display initialisation ------------------------- *)

BEGIN
  WmQuit := FALSE;
  ASSERT(H.SetProcessDpiAwarenessContext(-3) # 0);  (* DPI_AWARENESS_CONTEXT_PER_MONITOR_AWARE *)
  VKtoScn := SYSTEM.ADR($
    00 00 00 00 00 00 00 00  66 09 00 00 00 5A 00 00
    12 14 11 00 58 00 00 00  00 00 00 76 00 00 00 00
    29 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    45 16 1E 26 25 2E 36 3D  3E 46 00 00 00 00 00 00
    00 1C 32 21 23 24 2B 34  33 43 3B 42 4B 3A 31 44
    4D 15 2D 1B 2C 3C 2A 1D  22 35 1A 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 4C 55 41 4E 49 4A
    0E 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 54 61 5B 52 5D
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00$);
  Width  := 1536; (* 1024;*) (*1800*)
  Height := 1152; (*768;*)  (*1280*)
  CreateWindow(512, 64, Width, Height);
  (*H.wsn("WinGui initialisation complete.");*)
END WinGui.
