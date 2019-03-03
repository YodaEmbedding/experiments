// https://www.codewars.com/kata/find-the-parity-outlier

// Attempt 1:
// Probably faster

using System.Linq;

public class Kata {
  public static int Find(int[] xs) {
    var findOdd = IsEven(xs[0]) && IsEven(xs[1]);
    return xs.First(x => findOdd ^ IsEven(x));
  }

  private static bool IsEven(int x) => x % 2 == 0;
}

// Attempt 2:
// Probably slower, but "fancier"

using System.Linq;

public class Kata {
  public static int Find(int[] xs) {
    var evens = xs.Where(IsEven);
    var odds  = xs.Where(x => !IsEven(x));
    return evens.Skip(1).Any() ? odds.First() : evens.First();
  }

  private static bool IsEven(int x) => x % 2 == 0;
}
