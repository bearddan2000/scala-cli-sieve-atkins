object ScalaMain {
def SieveOfAtkin(limit: Int): Unit =
  {
      // 2 and 3 are known to be prime
      if (limit > 2)
          print("[OUTPUT] " + 2 + " ");

      if (limit > 3)
          print(3 + " ");

      // Initialise the sieve array with
      // false values
      var sieve = new Array[Boolean](limit);;

      for (i <- 0 until limit-1)
          sieve(i) = false;

      /* Mark siev[n] is true if one of the
      following is true:
      a) n = (4*x*x)+(y*y) has odd number
         of solutions, i.e., there exist
         odd number of distinct pairs
         (x, y) that satisfy the equation
         and    n % 12 = 1 or n % 12 = 5.
      b) n = (3*x*x)+(y*y) has odd number
         of solutions and n % 12 = 7
      c) n = (3*x*x)-(y*y) has odd number
         of solutions, x > y and n % 12 = 11 */

         var x:Int = 1;
      while (x * x < limit) {
        var y:Int = 1;
          while (y * y < limit) {

              // Main part of Sieve of Atkin
              var n:Int = (4 * x * x) + (y * y);
              if (n <= limit && (n % 12 == 1 || n % 12 == 5))

                  sieve(n) ^= true;

              n = (3 * x * x) + (y * y);
              if (n <= limit && n % 12 == 7)
                  sieve(n) ^= true;

              n = (3 * x * x) - (y * y);
              if (x > y && n <= limit && n % 12 == 11)
                  sieve(n) ^= true;
              y += 1;
          }
          x += 1;
      }

      // Mark all multiples of squares as
      // non-prime
      var r:Int = 5;
      while (r * r < limit) {
          if (sieve(r)) {
              for (i <- r * r until limit-1 by r * r)
                  sieve(i) = false;
          }
          r += 1;
      }

      // Print primes using sieve[]
      for (a <- 5 until limit)
          if (sieve(a))
              print(a + " ");

      println("");
  }
  def main(args: Array[String]): Unit = {
    var input:Int = 20;
    println("[INPUT] " + input);
    SieveOfAtkin(input);
  }
}
