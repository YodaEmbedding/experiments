# https://www.codewars.com/kata/fizz-buzz

def fizzbuzz(n)
  (1..n).map {|x|
    x % 15 == 0 ? 'FizzBuzz' :
    x % 3  == 0 ? 'Fizz' :
    x % 5  == 0 ? 'Buzz' :
    x}
end
