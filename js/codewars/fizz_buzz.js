// https://www.codewars.com/kata/fizz-buzz

function fizzbuzz(n) {
  let nums = Array(n + 1).fill(1).map((x, i) => i).slice(1);
  return nums.map(x =>
    x % 15 == 0 ? "FizzBuzz" :
    x % 3  == 0 ? "Fizz" :
    x % 5  == 0 ? "Buzz" :
    x);
}
