// https://www.codewars.com/kata/reverse-every-other-word-in-the-string

object Solution {
  def reverse(s: String): String = {
    (s.split(" ").zipWithIndex
      map {case (x, i) => if (i % 2 == 0) x else x.reverse}
      mkString " ")
  }
}
