NAME           SOS2EXAM Illustrate use of SOS2 set
ROWS
 N  OBJ
 E  CNVX
 E  CVOL
 E  CCST
 E  BALN
COLUMNS
    SELL1     OBJ       -20
    SELL1     BALN      1
    SELL2     OBJ       -14
    SELL2     BALN      1
    SELL3     OBJ       -13
    SELL3     BALN      1
    COST      OBJ       1
    COST      CCST      -1
 S2 SET2      'MARKER'  'SOSORG'
    W0        CNVX      1
    W0100     CNVX      1
    W0100     CCST      1500
    W0100     CVOL      100
    W1100     CNVX      1
    W1100     CVOL      1100
    W1100     CCST      15500
    W3100     CNVX      1
    W3100     CVOL      3100
    W3100     CCST      41500
    W6100     CNVX      1
    W6100     CVOL      6100
    W6100     CCST      77500
 S2 SET2      'MARKER'  'SOSEND'
    VOL       CVOL      -1
    VOL       BALN      -1
RHS
    RHS1 CNVX 1
BOUNDS
 UP BND1      SELL1     300
 UP BND1      SELL2     900
 UP BND1      SELL3     2000
ENDATA