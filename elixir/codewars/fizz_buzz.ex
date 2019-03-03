# https://www.codewars.com/kata/fizz-buzz

defmodule FizzBuzz do
  def fizzbuzz(n) do
    1..n
    |> Enum.map(fn
      x when rem(x, 15) == 0 -> "FizzBuzz"
      x when rem(x, 3)  == 0 -> "Fizz"
      x when rem(x, 5)  == 0 -> "Buzz"
      x -> x
      end)
  end
end
