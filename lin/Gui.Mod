MODULE Gui;
IMPORT SYSTEM, H := Host, Texts;

CONST
  Width*      = 1536;
  Height*     = 1152;
  Stride*     = Width DIV 8;
  BitReverse* = FALSE;
  KeyBufSize  = 16;

  AFUNIX     = 1;
  AFINET     = 2;
  AFINET6    = 10;
  SOCKSTREAM = 1;
  SOCKDGRAM  = 2;
  SOCKRAW    = 3;


TYPE
  Event = RECORD
    code:  BYTE;
    error: BYTE;
    data:  ARRAY 30 OF BYTE
  END;

  ErrorEvent = RECORD
    code, error: BYTE;
    seqno:       SYSTEM.CARD16;
    bad:         SYSTEM.CARD32;
    minoropcode: SYSTEM.CARD16;
    majoropcode: BYTE;
    unused:      ARRAY 21 OF BYTE;
  END;

  KeyEvent = RECORD
    eventcode:   BYTE;
    keyCode:     BYTE;
    seq:         SYSTEM.CARD16;
    timestamp:   SYSTEM.CARD32;
    rootWindow:  SYSTEM.CARD32;
    eventWindow: SYSTEM.CARD32;
    childWindow: SYSTEM.CARD32;
    rootX:       SYSTEM.INT16;
    rootY:       SYSTEM.INT16;
    childX:      SYSTEM.INT16;
    childY:      SYSTEM.INT16;
    keySet:      SYSTEM.CARD16;
    sameScreen:  BYTE;
    pad:         BYTE;
  END;

  MouseEvent = RECORD-
    code:       BYTE;           (* 4: press, 5: release, 6: Move *)
    detail:     BYTE;           (* button for press, release *)
    seq:        SYSTEM.CARD16;
    time:       SYSTEM.CARD32;
    rootwin:    SYSTEM.CARD32;
    eventwin:   SYSTEM.CARD32;
    childwin:   SYSTEM.CARD32;
    rootX:      SYSTEM.INT16;
    rootY:      SYSTEM.INT16;
    eventX:     SYSTEM.INT16;
    eventY:     SYSTEM.INT16;
    butmask:    SYSTEM.CARD16;
    samescreen: BYTE;
    unused:     BYTE;
  END;

VAR
  BitmapAddress*: INTEGER;

  W:            Texts.Writer;
  Srv:          INTEGER;
  Buf:          ARRAY 16384 OF BYTE;
  Out:          INTEGER;
  NextId:       INTEGER;
  IdIncr:       INTEGER;
  MaxReqLen:    INTEGER;
  ScreenWidth:  INTEGER;
  ScreenHeight: INTEGER;
  RootWindow:   INTEGER;
  RootDepth:    INTEGER;
  Window:       INTEGER;  (* Window id          *)
  Context:      INTEGER;  (* Graphic context id *)
(*Bits:         ARRAY 8294400 OF SYSTEM.CARD32;  (* Up to 3840 x 2160 *) *)
  Bits:         ARRAY Width * Height DIV 8 OF BYTE;
  Background:   BYTE;

  Keys:         ARRAY KeyBufSize OF BYTE;  (* Scancodes *)
  KeyFull:      BOOLEAN;
  KeyIn:        INTEGER;
  KeyOut:       INTEGER;

  KCtoScan:     INTEGER;  (* Address of table, index in [8..255] *)

  MouseState*:  INTEGER;  (* 1/in-window, 3/keys, 12/x, 12/y  *)
  Shutdown*:    BOOLEAN;

  Dirtybottom:  INTEGER;  (* aka y      -  y increases from bottom to top *)
  Dirtytop:     INTEGER;  (* aka y + h  -  y increases from bottom to top *)


PROCEDURE #0  sysread   (fd,     adr,     len:      INTEGER): INTEGER;
PROCEDURE #1  syswrite  (fd,     adr,     len:      INTEGER): INTEGER;
PROCEDURE #7  syspoll   (fds,    nfds,    timeout:  INTEGER): INTEGER;
PROCEDURE #41 syssocket (domain, type,    protocol: INTEGER): INTEGER;
PROCEDURE #42 sysconnect(fd,     sockadr, socklen:  INTEGER): INTEGER;


(* -------------------------------- Logging --------------------------------- *)

(*
PROCEDURE wn; BEGIN Texts.WriteLn(W); Texts.Append(Texts.Log, W.buf) END wn;
PROCEDURE wc (c: CHAR);          BEGIN Texts.Write(W, c)           END wc;
PROCEDURE ws (s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s)     END ws;
PROCEDURE wsn(s: ARRAY OF CHAR); BEGIN Texts.WriteString(W, s); wn END wsn;
PROCEDURE wh (i: INTEGER);       BEGIN Texts.WriteHex(W, i)        END wh;
PROCEDURE wi (i: INTEGER);       BEGIN Texts.WriteInt(W, i, 1)     END wi;
PROCEDURE wir(i, n: INTEGER);    BEGIN Texts.WriteInt(W, i, n)     END wir;
*)

PROCEDURE wn;                          BEGIN H.wn            END wn;
PROCEDURE wc (c: CHAR);                BEGIN H.wc(c)         END wc;
PROCEDURE ws (s: ARRAY OF CHAR);       BEGIN H.ws(s)         END ws;
PROCEDURE wsn(s: ARRAY OF CHAR);       BEGIN H.wsn(s)        END wsn;
PROCEDURE wi (i: INTEGER);             BEGIN H.wi(i)         END wi;
PROCEDURE wir(i, w: INTEGER; c: CHAR); BEGIN H.wir(i, w, c)  END wir;
PROCEDURE wh (i: INTEGER);             BEGIN H.wh(i)         END wh;
PROCEDURE whr(i, w: INTEGER; c: CHAR); BEGIN H.whr(i, w, c)  END whr;

PROCEDURE WriteEventCode(code: INTEGER);
BEGIN
   IF    code = 2  THEN ws("KeyPress")
   ELSIF code = 3  THEN ws("KeyRelease")
   ELSIF code = 4  THEN ws("ButtonPress")
   ELSIF code = 5  THEN ws("ButtonRelease")
   ELSIF code = 6  THEN ws("MotionNotify")
   ELSIF code = 7  THEN ws("EnterNotify")
   ELSIF code = 8  THEN ws("LeaveNotify")
   ELSIF code = 9  THEN ws("FocusIn")
   ELSIF code = 10 THEN ws("FocusOut")
   ELSIF code = 11 THEN ws("KeymapNotify")
   ELSIF code = 12 THEN ws("Expose")
   ELSIF code = 13 THEN ws("GraphicsExposure")
   ELSIF code = 14 THEN ws("NoExposure")
   ELSIF code = 15 THEN ws("VisibilityNotify")
   ELSIF code = 16 THEN ws("CreateNotify")
   ELSIF code = 17 THEN ws("DestroyNotify")
   ELSIF code = 18 THEN ws("UnmapNotify")
   ELSIF code = 19 THEN ws("MapNotify")
   ELSIF code = 20 THEN ws("MapRequest")
   ELSIF code = 21 THEN ws("ReparentNotify")
   ELSIF code = 22 THEN ws("ConfigureNotify")
   ELSIF code = 23 THEN ws("ConfigureRequest")
   ELSIF code = 24 THEN ws("GravityNotify")
   ELSIF code = 25 THEN ws("ResizeRequest")
   ELSIF code = 26 THEN ws("CirculateNotify")
   ELSIF code = 27 THEN ws("CirculateRequest")
   ELSIF code = 28 THEN ws("PropertyNotify")
   ELSIF code = 29 THEN ws("SelectionClear")
   ELSIF code = 30 THEN ws("SelectionRequest")
   ELSIF code = 31 THEN ws("SelectionNotify")
   ELSIF code = 32 THEN ws("ColormapNotify")
   ELSIF code = 33 THEN ws("ClientMessage")
   ELSIF code = 34 THEN ws("MappingNotify")
   ELSE wi(code)
   END
END WriteEventCode;


PROCEDURE WriteError(ev: Event);
VAR
  err: POINTER TO ErrorEvent;
BEGIN
  IF    ev.error =  1 THEN ws("Request")
  ELSIF ev.error =  2 THEN ws("Value")
  ELSIF ev.error =  3 THEN ws("Window")
  ELSIF ev.error =  4 THEN ws("Pixmap")
  ELSIF ev.error =  5 THEN ws("Atom")
  ELSIF ev.error =  6 THEN ws("Cursor")
  ELSIF ev.error =  7 THEN ws("Font")
  ELSIF ev.error =  8 THEN ws("Match")
  ELSIF ev.error =  9 THEN ws("Drawable")
  ELSIF ev.error = 10 THEN ws("Access")
  ELSIF ev.error = 11 THEN ws("Alloc")
  ELSIF ev.error = 12 THEN ws("Colormap")
  ELSIF ev.error = 13 THEN ws("GContext")
  ELSIF ev.error = 14 THEN ws("IDChoice")
  ELSIF ev.error = 15 THEN ws("Name")
  ELSIF ev.error = 16 THEN ws("Length")
  ELSIF ev.error = 17 THEN ws("Implementation")
  ELSE wi(ev.error)
  END;
  SYSTEM.PUT(SYSTEM.ADR(err), SYSTEM.ADR(ev));
  ws(" opcode ");    wi(err.majoropcode); wc(":"); wi(err.minoropcode);
  ws(" bad value "); wh(err.bad); wc("H")
END WriteError;


(* ----------------------------- Buffered send ------------------------------ *)

PROCEDURE SendUnbuffered(buf: ARRAY OF BYTE; offset, len: INTEGER);
VAR rv: INTEGER;
BEGIN  ASSERT(len <= LEN(buf));
  IF len > 0 THEN
    rv := syswrite(Srv, SYSTEM.ADR(buf) + offset, len);
    IF    rv < 0   THEN H.Trap(-1, "Write to X11 server failed.")
    ELSIF rv < Out THEN H.Trap(-1, "Incomplete write to X11 server.")
    END
  END
END SendUnbuffered;

PROCEDURE SendFlush;
BEGIN IF Out > 0 THEN SendUnbuffered(Buf, 0, Out);  Out := 0 END END SendFlush;

PROCEDURE SendBuf(buf: ARRAY OF BYTE; offset, len: INTEGER);
BEGIN
  IF len > 0 THEN
    IF Out + len > LEN(Buf) THEN SendFlush END;
    IF Out + len > LEN(Buf) THEN SendUnbuffered(buf, offset, len);
    ELSE
      SYSTEM.COPY(SYSTEM.ADR(buf) + offset, SYSTEM.ADR(Buf[Out]), len);
      INC(Out, len)
    END
  END
END SendBuf;

PROCEDURE Send(buf: ARRAY OF BYTE); BEGIN SendBuf(buf, 0, LEN(buf)) END Send;

PROCEDURE SendByte (i: INTEGER); BEGIN SendBuf(i, 0, 1) END SendByte;
PROCEDURE SendWord (i: INTEGER); BEGIN SendBuf(i, 0, 2) END SendWord;
PROCEDURE SendDWord(i: INTEGER); BEGIN SendBuf(i, 0, 4) END SendDWord;

PROCEDURE SendPad(buflen: INTEGER);  (* Send padding to bring buflen up to a multiple of 4 *)
VAR i: INTEGER;
BEGIN
  IF buflen MOD 4 # 0 THEN i := 0; SendBuf(i, 0, 4 - buflen MOD 4) END
END SendPad;


PROCEDURE SendHeader(b1, b2, w: INTEGER);
BEGIN SendByte(b1); SendByte(b2); SendWord(w) END SendHeader;

(* ------------------------------- Reception -------------------------------- *)

PROCEDURE ReceiveBuf(VAR buf: ARRAY OF BYTE; offset, len: INTEGER): BOOLEAN;
VAR rv: INTEGER;
BEGIN
  SendFlush;
  IF len < 0 THEN len := LEN(buf) - offset END;
  ASSERT(len > 0);
  ASSERT(offset + len <= LEN(buf));
  REPEAT
    rv := sysread(Srv, SYSTEM.ADR(buf[offset]), len);
    IF rv > 0 THEN INC(offset, rv); DEC(len, rv) END
  UNTIL (len <= 0) OR (rv < 0)
RETURN rv > 0 END ReceiveBuf;

PROCEDURE MustReceiveBuf(VAR buf: ARRAY OF BYTE; offset, len: INTEGER);
BEGIN
  IF ~ReceiveBuf(buf, offset, len) THEN
    H.Trap(0, "X server dropped connection.")
  END
END MustReceiveBuf;

PROCEDURE Receive(VAR buf: ARRAY OF BYTE): BOOLEAN;
RETURN ReceiveBuf(buf, 0, LEN(buf)) END Receive;

PROCEDURE MustReceive(VAR buf: ARRAY OF BYTE);
BEGIN
  IF ~ReceiveBuf(buf, 0, LEN(buf)) THEN
    H.Trap(0, "X server dropped connection.")
  END
END MustReceive;


(* ------------------------ Server connection setup ------------------------- *)

PROCEDURE ConnectToX11Server(VAR fd: INTEGER);
CONST x11sockname = "/tmp/.X11-unix/X0";
VAR  rv, namelen: INTEGER;  sockadr:  ARRAY 200 OF BYTE;
BEGIN
  fd := syssocket(AFUNIX, SOCKSTREAM, 0);  ASSERT(fd >= 0);
  SYSTEM.PUT(SYSTEM.ADR(sockadr), SYSTEM.VAL(SYSTEM.INT16, AFUNIX));
  namelen := H.Length(x11sockname);  ASSERT(namelen + 2 <= LEN(sockadr));
  SYSTEM.COPY(SYSTEM.ADR(x11sockname), SYSTEM.ADR(sockadr[2]), namelen);
  rv := sysconnect(fd, SYSTEM.ADR(sockadr), namelen + 2);  ASSERT(rv = 0);
END ConnectToX11Server;


PROCEDURE IAND(a, b: INTEGER): INTEGER;
RETURN SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET, a) * SYSTEM.VAL(SET, b)) END IAND;


PROCEDURE IXOR(a, b: INTEGER): INTEGER;
RETURN SYSTEM.VAL(INTEGER, SYSTEM.VAL(SET, a) / SYSTEM.VAL(SET, b)) END IXOR;


PROCEDURE X11ConnectionSetup;
VAR
  initmsg: RECORD
    byteorder, dummy:     BYTE;
    majorver,  minorver:  SYSTEM.CARD16;
    apnamelen, apdatalen: SYSTEM.CARD16;
    unused:               SYSTEM.CARD16;
    protocol:             ARRAY 100 OF BYTE;
  END;
  initinfo: RECORD
    result,       dummy:                                   BYTE;
    majorVer,     minorVer,    additional:                 SYSTEM.CARD16;
    releaseNo,    resIdBase,   resIdMask,  motionBufSize:  SYSTEM.CARD32;
    vendorLen,    maxReqLen:                               SYSTEM.CARD16;
    screenCount,  formatCount, byteOrder,  bmpBitOrder:    BYTE;
    scanlineUnit, scanlinePad, minKeycode, maxKeyCode:     BYTE;
    pad:                                                   SYSTEM.CARD32;
  END;
  format: RECORD
    depth, bitsPerPixel, scanlinePad: BYTE;
    unused: ARRAY 5 OF BYTE;
  END;
  screen: RECORD
    rootWindow, defaultColourMap, whitePixel, blackPixel, inputMasks: SYSTEM.CARD32;
    widthInPixels, heightInPixels, widthInMM, heightInMM:             SYSTEM.CARD16;
    minInstalledMaps, maxInstalledMaps:                               SYSTEM.CARD16;
    rootVisual:                                                       SYSTEM.CARD32;
    backingStores, saveUnders, rootDepth, depthCount:                 BYTE;
  END;
  depth: RECORD
    depth, pad: BYTE;
    vtCount:    SYSTEM.CARD16;
    pad2:       SYSTEM.CARD32;
  END;
  visualType: RECORD
    visualId:                          SYSTEM.CARD32;
    class, bitsPerRgbValue:            BYTE;
    colourMapEntries:                  SYSTEM.CARD16;
    redMask, greenMask, blueMask, pad: SYSTEM.CARD32;
  END;
  vendor:  ARRAY 200 OF CHAR;
  i, j, k: INTEGER;
BEGIN
  H.ZeroFill(initmsg);  initmsg.byteorder := ORD("l");  initmsg.majorver := 11;
  SendUnbuffered(initmsg, 0, 12);
  IF ~Receive(initinfo)  THEN H.Trap(-1, "X server dropped connection.") END;
  IF initinfo.result # 1 THEN H.Trap(-1, "X server refused connection.") END;
  NextId := initinfo.resIdBase;
  IdIncr := IAND(initinfo.resIdMask, -initinfo.resIdMask);
  MaxReqLen := initinfo.maxReqLen;
  i := (initinfo.vendorLen + 3) DIV 4 * 4;
  IF ~ReceiveBuf(vendor, 0, i) THEN H.Trap(-1, "X server dropped connection.") END;
  vendor[initinfo.vendorLen] := 0X;
  FOR i := 1 TO initinfo.formatCount DO MustReceive(format) END;
  FOR i := 1 TO initinfo.screenCount DO MustReceive(screen);
    IF i = 1 THEN
      ScreenWidth := screen.widthInPixels;  ScreenHeight := screen.heightInPixels;
      RootWindow  := screen.rootWindow;     RootDepth    := screen.rootDepth
    END;
    FOR j := 1 TO screen.depthCount DO MustReceive(depth);
      FOR k := 1 TO depth.vtCount DO MustReceive(visualType) END;
    END;
  END
END X11ConnectionSetup;


(* ---------------------------- Window painting ----------------------------- *)

PROCEDURE PaintRow(y: INTEGER);
VAR bytecount: INTEGER;
BEGIN
  ASSERT(Width MOD 8 = 0);
  bytecount := Width DIV 8;
  SendByte(72);    (* command *)
  SendByte(0);     (* format 0: 1 bit per pixel *)
  SendWord((bytecount + 3) DIV 4 + 6);
  SendDWord(Window);  SendDWord(Context);
  SendWord(Width);    SendWord(1);
  SendWord(0);        SendWord(Height - 1 - y);
  SendByte(0);        SendByte(1);        (* leftpad, depth *)
  SendWord(0);                            (* padding *)
  SendBuf(Bits, y * (Width DIV 8), (bytecount + 3) DIV 4 * 4)
END PaintRow;


PROCEDURE PaintWindow;
VAR y: INTEGER;
BEGIN ASSERT(Dirtybottom < Dirtytop);
  (*ws("PaintWindow "); wi(Dirtybottom); ws(" up to "); wi(Dirtytop); wsn(".");*)
  FOR y := Dirtytop - 1 TO Dirtybottom BY -1 DO PaintRow(y) END;
  SendFlush;  Dirtybottom := Height;  Dirtytop := 0
END PaintWindow;


(* ----------------------------- Notifications ------------------------------ *)

PROCEDURE MapNotify(event: Event);
BEGIN wsn("MapNotify.");
END MapNotify;


PROCEDURE ReparentNotify(event: Event);
BEGIN wsn("ReparentNotify.");
END ReparentNotify;


PROCEDURE ConfigureNotify(event: Event);
BEGIN wsn("ConfigureNotify.");
END ConfigureNotify;


(* ---------------------- Dirty rectangle accumulation ---------------------- *)

PROCEDURE Dirty*(x, y, w, h: INTEGER);  (* For X11 only track rows dirtied *)
BEGIN
  ASSERT(h >= 0);
  IF y     < Dirtybottom THEN Dirtybottom := y     END;
  IF y + h > Dirtytop    THEN Dirtytop    := y + h END
END Dirty;

PROCEDURE Quit*; BEGIN Shutdown := TRUE END Quit;


(* ------------------------ Keyboard event handling ------------------------- *)

PROCEDURE AddKey(key: BYTE);
BEGIN
  IF ~KeyFull THEN
    Keys[KeyIn] := key;
    KeyIn       := (KeyIn + 1) MOD KeyBufSize;
    KeyFull     := KeyIn = KeyOut
  END
END AddKey;


PROCEDURE ScanCode(ev: Event): INTEGER;
VAR key: POINTER- TO KeyEvent;  scan: INTEGER;
BEGIN
  SYSTEM.PUT(SYSTEM.ADR(key), SYSTEM.ADR(ev));
  IF key.keyCode = 9 THEN H.Exit END;  (* Debugging: exit on esc key *)
  SYSTEM.GET(KCtoScan + key.keyCode - 8, scan);
RETURN scan END ScanCode;


(* -------------------------- Mouse event handling -------------------------- *)

PROCEDURE HandleMouse(event: Event);
VAR
  ev:      POINTER- TO MouseEvent;
  x, y:    INTEGER;
  buttons: SET;
BEGIN
  SYSTEM.PUT(SYSTEM.ADR(ev), SYSTEM.ADR(event));
  x := ev.eventX;
  y := Height - 1 - ev.eventY;  (* Oberon Y values increase upward *)

  (* Handle tracking / capture for window dragging *)

  (* X11 button flags:
       0100H  {8}  left
       0200H  {9}  middle
       0400H  {10} right
       0800H  {11} ? scroll up
       1000H  {12} ? scroll down
   Oberon button flags:
       {0}  MR
       {1}  MM
       {2}  ML
  *)
  buttons := SYSTEM.VAL(SET, ORD(ev.butmask));
  IF    ev.code = 4 THEN INCL(buttons, ev.detail + 7)
  ELSIF ev.code = 5 THEN EXCL(buttons, ev.detail + 7)
  END;
  MouseState := 8000000H + y MOD 1000H * 1000H + x MOD 1000H;
  IF 10 IN buttons THEN INC(MouseState, 1000000H) END; (* MR *)
  IF  9 IN buttons THEN INC(MouseState, 2000000H) END; (* MM *)
  IF  8 IN buttons THEN INC(MouseState, 4000000H) END; (* ML *)
END HandleMouse;


(* ----------------------------- Event handling ----------------------------- *)

PROCEDURE HandleEvent;
VAR
  event:  Event;
  evcode: INTEGER;
  scan:   INTEGER;
BEGIN
  IF Receive(event) THEN
    evcode := event.code MOD 80H;
    (*H.ws("Event "); H.wi(evcode); H.wsn(".");*)
    IF    evcode =  0 THEN ws("Error event, error: "); WriteError(event); wsn(".")
    ELSIF evcode =  2 THEN scan := ScanCode(event); IF scan # 0 THEN AddKey(scan) END  (* KeyPress   *)
    ELSIF evcode =  3 THEN scan := ScanCode(event); IF scan # 0 THEN AddKey(0F0H); AddKey(scan) END  (* KeyRelease *)
    ELSIF evcode =  4 THEN HandleMouse(event)  (* ButtonPress   *)
    ELSIF evcode =  5 THEN HandleMouse(event)  (* ButtonRelease *)
    ELSIF evcode =  6 THEN HandleMouse(event)  (* MotionNotify  *)
    ELSIF evcode =  7 THEN wsn("EnterNotify.")
    ELSIF evcode = 12 THEN Dirtybottom := 0;  Dirtytop := Height;  PaintWindow (* Expose *)
    ELSIF evcode = 19 THEN MapNotify(event)
    ELSIF evcode = 21 THEN ReparentNotify(event)
    ELSIF evcode = 22 THEN ConfigureNotify(event)
    ELSE ws("Event code ");  WriteEventCode(event.code);  wsn(".")
    END
  ELSE
    (* Connection closed. *)
    Shutdown := TRUE;
    MouseState := MouseState MOD 1000000H;  (* All mouse buttons up *)
    KeyIn := KeyOut; KeyFull := FALSE;      (* Clear keybard *)
  END
END HandleEvent;


PROCEDURE PollEvent(timeout: INTEGER): BOOLEAN;
VAR
  res: INTEGER;
  pollfd: RECORD
    fd:      SYSTEM.INT32;
    events:  SYSTEM.CARD16;
    revents: SYSTEM.CARD16;
  END;
BEGIN
  SendFlush;
  pollfd.fd      := Srv;
  pollfd.events  := 1;   (* POLLIN *)
  pollfd.revents := 0;
RETURN syspoll(SYSTEM.ADR(pollfd), 1, timeout) # 0 END PollEvent;


PROCEDURE ProcessOneMessage* (): INTEGER;  (* 0 - none available, 1 - processed *)
VAR res: INTEGER;
BEGIN res := 0;
  IF    PollEvent(0)         THEN res := 1;  HandleEvent
  ELSIF Dirtybottom < Height THEN res := 1;  PaintWindow
  END
RETURN res END ProcessOneMessage;


PROCEDURE WaitMsgOrTime*(time: INTEGER);  (* Waits for time (ms) OR message in queue *)
BEGIN IF Shutdown OR (Dirtybottom < Height) OR PollEvent(time) THEN END END WaitMsgOrTime;

PROCEDURE Drain;
BEGIN REPEAT UNTIL ProcessOneMessage() = 0 END Drain;


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
BEGIN Drain;  (*ws("Gui.Mouse -> "); wh(ORD(MouseState)); wsn("H.");*)
RETURN MouseState END Mouse;

PROCEDURE PositionWindow*(x, y: INTEGER);
BEGIN (* TODO Move window *)
END PositionWindow;

PROCEDURE Minimise*;
BEGIN (* TODO Minimize *)
END Minimise;

PROCEDURE GetScreenMousePos*(VAR x, y: INTEGER);
BEGIN
  x := 0;  y :=0
END GetScreenMousePos;


(* ---------------------------- Window creation ----------------------------- *)

PROCEDURE CreateWindow;
CONST
  (* Event masks *)
  KeyPress             = 0000001H;
  KeyRelease           = 0000002H;
  ButtonPress          = 0000004H;
  ButtonRelease        = 0000008H;
  EnterWindow          = 0000010H;
  LeaveWindow          = 0000020H;
  PointerMotion        = 0000040H;
  PointerMotionHint    = 0000080H;
  Button1Motion        = 0000100H;
  Button2Motion        = 0000200H;
  Button3Motion        = 0000400H;
  Button4Motion        = 0000800H;
  Button5Motion        = 0001000H;
  ButtonMotion         = 0002000H;
  KeymapState          = 0004000H;
  Exposure             = 0008000H;
  VisibilityChange     = 0010000H;
  StructureNotify      = 0020000H;
  ResizeRedirect       = 0040000H;
  SubstructureNotify   = 0080000H;
  SubstructureRedirect = 0100000H;
  FocusChange          = 0200000H;
  PropertyChange       = 0400000H;
  ColormapChange       = 0800000H;
  OwnerGrabButton      = 1000000H;
VAR
  request: RECORD
    opcode, depth:                BYTE;
    rlength:                      SYSTEM.CARD16;
    winID, parentID:              SYSTEM.CARD32;
    x, y, w, h, borwidth, class:  SYSTEM.CARD16;
    visualID, valMask:            SYSTEM.CARD32;
    ovrRedir, evntMask:           SYSTEM.CARD32;
  END;
BEGIN
  Window           := NextId;  INC(NextId, IdIncr);
  request.opcode   := 1;
  request.depth    := RootDepth;
  request.rlength  := 10;
  request.winID    := Window;
  request.parentID := RootWindow;
  request.x        := 100;
  request.y        := 100;
  request.w        := Width;
  request.h        := Height;
  request.borwidth := 0;
  request.class    := 1;      (* InputOutput *)
  request.visualID := 0;      (* CopyFromParent, *)
  request.valMask  := 0A00H;  (* override-redirect, event-mask *)
  request.ovrRedir := 0;      (* off *)
  (* request.evntMask := 28015H; (* structure notify, exposure, enter window, button press, key press *) *)
  (* request.evntMask := 1FFFF7FH; (* ALl but PointerMotionHint (as we want all pointer motion) *) *)
  request.evntMask := KeyPress      + KeyRelease
                    + ButtonPress   + ButtonRelease
                    + PointerMotion
                    + EnterWindow   + LeaveWindow
                    + Exposure;
  Send(request)
END CreateWindow;


PROCEDURE ChangeProperty(window, property, type, format, mode, len: INTEGER;
                         data: ARRAY OF BYTE);
(* mode:   0 - Replace, 1 - Prepend, 2 - Append *)
(* format: 8, 16 or 32                          *)
BEGIN
  SendHeader(18, mode, 6 + (len+3) DIV 4);
  SendDWord(window);  SendDWord(property);  SendDWord(type);  SendDWord(format MOD 256);
  SendDWord(len DIV (format DIV 8));
  SendBuf(data, 0, len);  SendPad(len)
END ChangeProperty;

PROCEDURE GetInternAtom(name: ARRAY OF CHAR): INTEGER;
VAR len: INTEGER;
  result: RECORD
    reply:  BYTE;
    pad:    BYTE;
    seq:    SYSTEM.CARD16;
    len:    SYSTEM.CARD32;
    atom:   SYSTEM.CARD32;
    unused: ARRAY 20 OF BYTE;
  END;
BEGIN len := H.Length(name);
  SendHeader(16, 1, 2 + (len + 3) DIV 4);
  SendWord(len);  SendWord(0);
  SendBuf(name, 0, len);  SendPad(len);
  MustReceive(result);
RETURN result.atom END GetInternAtom;

PROCEDURE SetWindowProperties;
VAR
  property: INTEGER;
  hints: RECORD
    flags:       SYSTEM.CARD32;
    functions:   SYSTEM.CARD32;
    decorations: SYSTEM.CARD32;
    inputMode:   SYSTEM.INT32;
    status:      SYSTEM.CARD32;
    END;
BEGIN
  H.ZeroFill(hints);  hints.flags := 2;  hints.decorations := 0;
  property := GetInternAtom("_MOTIF_WM_HINTS");
  ChangeProperty(Window, property, property, 32, 0, (* PropModeReplace *) 40, hints);
  ChangeProperty(Window, 39, 39 (* WM_NAME *), 8, 0, 6, "Oberon");
END SetWindowProperties;


PROCEDURE MapWindow;
BEGIN SendHeader(8, 0, 2);  SendDWord(Window) END MapWindow;


PROCEDURE CreateGraphicsContext;
BEGIN
  Context := NextId;  INC(NextId, IdIncr);
  SendHeader(55, 0, 6);
  SendDWord(Context);
  SendDWord(Window);
  SendDWord(4 + 8);     (* Provide foreground and background colours *)
  SendDWord(0FFFFFFH);  (* Foreground *)
  SendDWord(0);         (* Background *)
END CreateGraphicsContext;


PROCEDURE QueryExtension(name: ARRAY OF CHAR; VAR majorOpcode, firstEvent, firstError: INTEGER);
VAR
  i, l: INTEGER;
  result: RECORD
    reply, pad1: BYTE;
    seq:         SYSTEM.CARD16;
    replylen:    SYSTEM.CARD32;
    present:     BYTE;
    majoropcode: BYTE;
    firstevent:  BYTE;
    firsterror:  BYTE;
    pad2:        ARRAY 20 OF BYTE;
  END;
BEGIN
  l := H.Length(name);
  SendHeader(98, 0, 2 + (l + 3) DIV 4);  SendWord(l);  SendWord(0);
  SendBuf(name, 0, l);
  SendPad(l);

  MustReceive(result);
  IF (result.reply = 1) & (result.present # 0) THEN
    majorOpcode := result.majoropcode;  firstEvent  := result.firstevent;
    firstError := result.firsterror;
    (*
    ws("QueryExtension('"); ws(name); ws("') -> major opcode "); wi(majorOpcode);
    ws(", first event "); wi(firstEvent); ws(", first error "); wi(firstError); wsn(".");
    *)
  ELSE
    majorOpcode := 0;  firstEvent  := 0;  firstError := 0
  END
END QueryExtension;


PROCEDURE EnableBigRequests;
VAR
  opcode: INTEGER;
  result: RECORD
    reply, pad1: BYTE;
    seq:         SYSTEM.CARD16;
    replylen:    SYSTEM.CARD32;
    maxreqlen:   SYSTEM.CARD32;
    pad2:        ARRAY 20 OF BYTE;
  END;
  eventnum: INTEGER;
  errornum: INTEGER;
BEGIN
  QueryExtension("BIG-REQUESTS", opcode, eventnum, errornum);
  IF opcode # 0 THEN
    SendHeader(opcode, 0, 1);  MustReceive(result);
    MaxReqLen := result.maxreqlen
  END
END EnableBigRequests;


BEGIN
  Texts.OpenWriter(W);
  KCtoScan := SYSTEM.ADR($   00 76 16 1E 26 25 2E 36
    3D 3E 46 45 4E 55 66 0D  15 1D 24 2D 2C 35 3C 43
    44 4D 54 5B 5A 14 1C 1B  23 2B 34 33 3B 42 4B 4C
    52 0E 12 5D 1A 22 21 2A  32 31 3A 41 49 4A 59 7C
    11 29 58 05 06 04 0C 03  0B 83 0A 01 09 77 7E 00
    00 00 7B 00 00 00 79 00  00 00 00 00 00 00 61 78
    07 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 71  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00
    00 00 00 00 00 00 00 00  00 00 00 00 00 00 00 00$);
  Out           := 0;
  Background    := 33H;
  Dirtybottom   := 0;
  Dirtytop      := Height;
  BitmapAddress := SYSTEM.ADR(Bits);
  (*Tracking    := FALSE;*)
  Shutdown      := FALSE;
  H.ZeroFill(Bits);

  ConnectToX11Server(Srv);
  X11ConnectionSetup;
  EnableBigRequests;
  CreateWindow;
  SetWindowProperties;
  MapWindow;
  CreateGraphicsContext;
  Drain;
  wsn("Gui loaded.");
END Gui.
