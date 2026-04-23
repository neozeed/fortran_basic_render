      integer m,n,gc,cols
      integer maxx,maxy
      real u,v,w
      real x,y,z
      real e,f,g
      complex p
      real i
      real d,t
      real power
      INTEGER lookup(16)
      
      cols = 16
      maxx = 320
      maxy = 200

      DATA lookup /0,8,2,10,12,4,14,6,3,11,1,9,15,7,13,5/

      do 10 n = 0,maxy-1
      do 20 m = 0,maxx-1
     
      x = 0
      y = -1
      z = 3
      u=(m-159.2)
      v=(n-127.5)
      w = 1 / SQRT(u * u + v * v + 1)
      U = U * W
      V = V * W
      i = SIGN(1,U)
      g = 1
230   E = X - i
      F = Y - i
      P = U * E + V * F - W * z
      D = P * P - E * E - F * F - z * z + 1
      IF (D .LE. 0) GOTO 420
      T = -P - SQRT(D)
      IF (T .LE. 0) GOTO 420
      X = X + T * U
      Y = Y + T * V
      z = z - T * W
      E = X - i
      F = Y - i
      g = z
      P = 2 * (U * E + V * F - W * g)
      U = U - P * E
      V = V - P * F
      W = W + P * g
      i = -i
      goto 230
420   P = (Y + 2) / V
      V = -V * (MOD(INT(X - U * P) + INT(Z - W * P), 2)) / 2.0+0.3+0.2
      power = V ** 0.4
      gc = ABS((cols - 1) - (INT(((cols - 1) * 16) * power + 
     1lookup(MOD(m,4) + 1 + (MOD(n,4) * 4))) / 16))
      print *, "Integers:", m, n, "Real:", gc
20    continue
10    continue
      END
