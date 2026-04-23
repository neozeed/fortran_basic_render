      INTEGER M,N,GC,COLS
      INTEGER MAXX,MAXY
      INTEGER x1,x2,xx
      REAL U,V,W
      REAL X,Y,Z
      REAL E,F,G
      COMPLEX P
      REAL I
      REAL D,T
      REAL POWER
      INTEGER LOOKUP(16)
      
      COLS = 2
      MAXX = 320
      MAXY = 200

      DATA LOOKUP /0,8,2,10,12,4,14,6,3,11,1,9,15,7,13,5/

      DO 10 N = 0,MAXY-1
      DO 20 M = 0,MAXX-1
     
      X = 0
      Y = -.1
      Z = 3

C     -1 to 1 scaled position X
      U=(M-159.2)
C     -1 to 1 scaled position Y
      V=(N-127.5)
C     Reciprocal distance from centre.
      W = 1 / SQRT(U * U + V * V + 1)
C     REM Normalise U and V
      U = U * W
      V = V * W
C     left or right half of screen
      I = SIGN(1,U)
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
      xx = INT(x1 + x2 .AND. 1)
      V = -V * (xx / 2.0+0.3) +0.2
C     Select the dithering pattern in the lookup table
450   POWER = V ** 0.4
      GC = ABS((COLS - 1) - (INT(((COLS - 1) * 16) * POWER + 
     1    LOOKUP(MOD(M,4) + (MOD(N,4) * 4))) / 16))
      PRINT *, "X:", M,"Y:", N, "D:", GC
20    CONTINUE
10    CONTINUE
      END
