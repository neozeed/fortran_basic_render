C       ray trace port from basic      
C       really sad attempot
C       chatgpt fixed it for me becasue im dum

      INCLUDE  'FGRAPH.FI'
      INCLUDE  'FGRAPH.FD'
      
      INTEGER M,N,GC,COLS
      INTEGER MAXX,MAXY
      INTEGER x1,x2,xx
      REAL U,V,W
      REAL X,Y,Z
      REAL E,F,G
      REAL P
      REAL I
      REAL D,T
      REAL POWER
      INTEGER LOOKUP(16)
      INTEGER RC

C        PARAMETER($MAXRESMODE   =-3)    ! graphics mode with highest resolution
C        PARAMETER($MAXCOLORMODE =-2)    ! graphics mode with most colors
C        PARAMETER($DEFAULTMODE  =-1)    ! restore screen to original mode
C        PARAMETER($TEXTBW40      =0)    ! 40 x 25 text, 16 grey
C        PARAMETER($TEXTC40       =1)    ! 40 x 25 text, 16/8 color
C        PARAMETER($TEXTBW80      =2)    ! 80 x 25 text, 16 grey
C        PARAMETER($TEXTC80       =3)    ! 80 x 25 text, 16/8 color
C        PARAMETER($MRES4COLOR    =4)    ! 320 x 200, 4 color
C        PARAMETER($MRESNOCOLOR   =5)    ! 320 x 200, 4 grey
C        PARAMETER($HRESBW        =6)    ! 640 x 200, BW
C        PARAMETER($TEXTMONO      =7)    ! 80 x 25 text, BW
C        PARAMETER($HERCMONO      =8)    ! 720 x 348, BW for HGC
C        PARAMETER($MRES16COLOR   =13)   ! 320 x 200, 16 color
C        PARAMETER($HRES16COLOR   =14)   ! 640 x 200, 16 color
C        PARAMETER($ERESNOCOLOR   =15)   ! 640 x 350, BW
C        PARAMETER($ERESCOLOR     =16)   ! 640 x 350, 4 or 16 color
C        PARAMETER($VRES2COLOR    =17)   ! 640 x 480, BW
C        PARAMETER($VRES16COLOR   =18)   ! 640 x 480, 16 color
C        PARAMETER($MRES256COLOR  =19)   ! 320 x 200, 256 color
C        PARAMETER($ORESCOLOR     =64)   ! 640 x 400, 1 of 16 colors (Olivetti)

      
      MODE = $MRES4COLOR
      COLS = 4
      MAXX = 320
      MAXY = 200
      SCRY = MAXY-1
      IF( setvideomode( MODE ) .EQ. 0 )
     +    STOP 'Error:  cannot set VGA graphics mode'


      DATA LOOKUP /0,8,2,10,12,4,14,6,3,11,1,9,15,7,13,5/

      DO 10 N = 0,MAXY-1
      DO 20 M = 0,MAXX-1
     
      X = 0
      Y = -.1
      Z = 3

C     -1 to 1 scaled position X
      U=(M-159.2) / 160
C     -1 to 1 scaled position Y
      V=(N-127.5) / 160
C     Reciprocal distance from centre.
      W = 1 / SQRT(U * U + V * V + 1)
C     REM Normalise U and V
      U = U * W
      V = V * W
C     left or right half of screen
      I = SIGN(1.0, U)
      G = 1

C     Walk towards the opposite edge.
230   E = X - I
C     Walk towards the top/bottom.
      F = Y - I
      P = U * E + V * F - W * Z

      D = P * P - E * E - F * F - Z * Z + 1

      IF (D .LE. 0) GOTO 420
      T = -P - SQRT(D)
      IF (T .LE. 0) GOTO 420
      X = X + T * U
      Y = Y + T * V
      Z = Z - T * W
      E = X - I
      F = Y - I
      G = Z
      P = 2 * (U * E + V * F - W * G)
      U = U - P * E
      V = V - P * F
      W = W + P * G
      I = -I
      GOTO 230

C     Ground Checkerboard
420   IF (V .GT.0) GOTO 450
      P = (Y + 2) / V
C     V = -V * (MOD(INT(X - U * P) + INT(Z - W * P), 2)) / 2.0+0.3+0.2
      x1 = INT(X - U * P)
      x2 = INT(Z - W * P)
      xx = IAND(x1 + x2, 1)
      V = -V * (xx / 2.0+0.3) +0.2
C     Select the dithering pattern in the lookup table
450   POWER = V ** 0.4
      GC = ABS((COLS - 1) - (INT(((COLS - 1) * 16) * POWER + 
     1    LOOKUP((MOD(M,4) + (MOD(N,4) * 4))+1)/3) / 16))
      RC = SETCOLOR(GC)
      RC = SETPIXEL(M,SCRY-N)
20    CONTINUE
10    CONTINUE
      PAUSE
      RC = SETVIDEOMODE( $DEFAULTMODE )
      END
