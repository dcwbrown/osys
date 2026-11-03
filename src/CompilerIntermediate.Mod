MODULE CompilerIntermediate;  (* D.Brown 2026.11.03 *)

IMPORT
  CS := CompilerScanner,
  CB := CompilerBase;

CONST
  WordSize* = 8;

TYPE
  Item* = RECORD
    type*:  CB.Type;
    mode*:  INTEGER;
    a*:     INTEGER;
    b*:     INTEGER;
    rdo*:   BOOLEAN;  (* Read only *)
  END;

VAR
  pc*: INTEGER;

PROCEDURE MakeItem*      (VAR x: Item;  y: CB.Object;  curlev: INTEGER);  END MakeItem;
PROCEDURE MakeConstItem* (VAR x: Item;  typ: CB.Type;  val: INTEGER);     END MakeConstItem;
PROCEDURE MakeRealItem*  (VAR x: Item;  val: REAL);                       END MakeRealItem;
PROCEDURE MakeStringItem*(VAR x: Item;  len: INTEGER);                    END MakeStringItem;  (*copies string from S-buffer to OGenerator-string array*)

PROCEDURE BuildTD* (T: CB.Type;  VAR dc: INTEGER);                         END BuildTD;
PROCEDURE TypeTest*(VAR x: Item;  T: CB.Type;  varpar, isguard: BOOLEAN);  END TypeTest;

PROCEDURE Index*(VAR x, y: Item);             END Index;  (* x := x[y] *)
PROCEDURE DeRef*(VAR x: Item);                END DeRef;  (* x := x^   *)
PROCEDURE Field*(VAR x: Item;  y: CB.Object); END Field;  (* x := x.y  *)

PROCEDURE ValueParam*    (VAR x: Item);                   END ValueParam;
PROCEDURE VarParam*      (VAR x: Item;  ftype: CB.Type);  END VarParam;
PROCEDURE OpenArrayParam*(VAR x: Item);                   END OpenArrayParam;
PROCEDURE StringParam*   (VAR x: Item);                   END StringParam;
PROCEDURE StrToChar*     (VAR x: Item);                   END StrToChar;

PROCEDURE Not*      (VAR x: Item);  END Not;
PROCEDURE Neg*      (VAR x: Item);  END Neg;
PROCEDURE Abs*      (VAR x: Item);  END Abs;
PROCEDURE Odd*      (VAR x: Item);  END Odd;
PROCEDURE Floor*    (VAR x: Item);  END Floor;
PROCEDURE Float*    (VAR x: Item);  END Float;
PROCEDURE Ord*      (VAR x: Item);  END Ord;
PROCEDURE Len*      (VAR x: Item);  END Len;
PROCEDURE Adr*      (VAR x: Item);  END Adr;
PROCEDURE Register* (VAR x: Item);  END Register;
PROCEDURE Condition*(VAR x: Item);  END Condition;
PROCEDURE H*        (VAR x: Item);  END H;
PROCEDURE Singleton*(VAR x: Item);  END Singleton;
PROCEDURE Assert*   (VAR x: Item);  END Assert;
PROCEDURE New*      (VAR x: Item);  END New;
PROCEDURE Led*      (VAR x: Item);  END Led;
PROCEDURE LDPSR*    (VAR x: Item);  END LDPSR;

PROCEDURE Here*     (): INTEGER;                 RETURN 0 END Here;
PROCEDURE CFJump*   (VAR x: Item);               END CFJump;
PROCEDURE FJump*    (VAR L: INTEGER);            END FJump;
PROCEDURE BJump*    (VAR L: INTEGER);            END BJump;
PROCEDURE CBJump*   (VAR x: Item;  L: INTEGER);  END CBJump;
PROCEDURE Fixup*    (VAR x: Item);               END Fixup;
PROCEDURE FixLink*  (L: INTEGER);                END FixLink;
PROCEDURE FixOne*   (at: INTEGER);               END FixOne;

PROCEDURE For0*(VAR x, y: Item);                         END For0;
PROCEDURE For1*(VAR x, y, z, w: Item;  VAR L: INTEGER);  END For1;
PROCEDURE For2*(VAR x, y, w: Item);                      END For2;

PROCEDURE ADC*        (VAR x, y: Item);  END ADC;
PROCEDURE SBC*        (VAR x, y: Item);  END SBC;
PROCEDURE UML*        (VAR x, y: Item);  END UML;
PROCEDURE Bit*        (VAR x, y: Item);  END Bit;
PROCEDURE Set*        (VAR x, y: Item);  END Set;
PROCEDURE In*         (VAR x, y: Item);  END In;
PROCEDURE MulOp*      (VAR x, y: Item);  END MulOp;
PROCEDURE Pack*       (VAR x, y: Item);  END Pack;
PROCEDURE Unpk*       (VAR x, y: Item);  END Unpk;
PROCEDURE Get*        (VAR x, y: Item);  END Get;
PROCEDURE Put*        (VAR x, y: Item);  END Put;
PROCEDURE LDREG*      (VAR x, y: Item);  END LDREG;
PROCEDURE Store*      (VAR x, y: Item);  END Store;
PROCEDURE StoreStruct*(VAR x, y: Item);  END StoreStruct;
PROCEDURE CopyString* (VAR x, y: Item);  END CopyString;

PROCEDURE Copy*(VAR x, y, z: Item);  END Copy;

PROCEDURE AddOp*    (op: INTEGER;        VAR x, y: Item);  END AddOp;   (* x := x +- y *)
PROCEDURE DivOp*    (op: INTEGER;        VAR x, y: Item);  END DivOp;   (* x := x op y *)
PROCEDURE RealOp*   (op: INTEGER;        VAR x, y: Item);  END RealOp;  (* x := x op y *)
PROCEDURE SetOp*    (op: INTEGER;        VAR x, y: Item);  END SetOp;   (* x := x op y *)
PROCEDURE Shift*    (fct: INTEGER;       VAR x, y: Item);  END Shift;
PROCEDURE Increment*(upordown: INTEGER;  VAR x, y: Item);  END Increment;
PROCEDURE Include*  (inorex: INTEGER;    VAR x, y: Item);  END Include;  (* x := x | or &~ 1<<y *)

PROCEDURE And1*(VAR x: Item);     END And1;  (* x := x & *)
PROCEDURE And2*(VAR x, y: Item);  END And2;
PROCEDURE Or1* (VAR x: Item);     END Or1;   (* x := x OR *)
PROCEDURE Or2* (VAR x, y: Item);  END Or2;


PROCEDURE IntRelation*   (op: INTEGER;  VAR x, y: Item);  END IntRelation;    (* x := x < y *)
PROCEDURE RealRelation*  (op: INTEGER;  VAR x, y: Item);  END RealRelation;   (* x := x < y *)
PROCEDURE StringRelation*(op: INTEGER;  VAR x, y: Item);  END StringRelation; (* x := x < y *)

PROCEDURE CheckRegs*;  END CheckRegs;

PROCEDURE PrepCall*(VAR x: Item;  r: INTEGER);         END PrepCall;
PROCEDURE Call*    (VAR x: Item;  r: INTEGER);         END Call;
PROCEDURE Enter*   (parblksize, locblksize: INTEGER);  END Enter;
PROCEDURE Return*  (form: INTEGER;  VAR x: Item;  size: INTEGER);  END Return;

PROCEDURE Open*       (v: INTEGER);                            END Open;
PROCEDURE SetDataSize*(dc: INTEGER);                           END SetDataSize;
PROCEDURE Header*;                                             END Header;
PROCEDURE Close*(VAR modid: CS.Ident;  key, nofent: INTEGER);  END Close;

BEGIN
END CompilerIntermediate.
