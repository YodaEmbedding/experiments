object ScalaTutorial {
    def factorial(x: Int): Int = {
        if (x > 1) x * factorial(x - 1) else 1
    }

    def getSum(args: Int*): Int = {
        var sum = 0
        for (x <- args)
            sum += x
        sum
    }

    def numToWord(x: Int): String = x match {
        case 0 => "zero"
        case 1 => "one"
        case 2 => "two"
        case 3 => "three"
        case 4 => "four"
        case _ => x.toString
    }

    def main(args: Array[String]): Unit = {
        val evenList = for {
            i <- 1 to 20 if (i % 2) == 0
        } yield i

        println((0 until 10).map(numToWord).mkString(", "))
        for (i <- 0 until 10) {
            print(numToWord(i))
        }
        println

        val fav = 3;

        println(factorial(5))
        println(getSum(3, 4, 6, 7))
    }
}
