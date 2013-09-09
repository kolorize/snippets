object wss {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  def factorial(n: Int): Int = {
    def loop(acc: Int, n: Int): Int =
      if (n == 0) acc
      else loop(acc * n, n - 1)
    loop(1, n)
  }                                               //> factorial: (n: Int)Int

  factorial(4)                                    //> res0: Int = 24

  //def sumInts(a: Int, b: Int): Int =
  //if (a > b) 0 else a + sumInts(a + 1, b)

  //sumInts(1, 10)

  def cube(x: Int): Int = x * x * x               //> cube: (x: Int)Int
  def sumCubes(a: Int, b: Int): Int =
    if (a > b) 0 else cube(a) + sumCubes(a + 1, b)//> sumCubes: (a: Int, b: Int)Int

  sumCubes(1, 20)                                 //> res1: Int = 44100

  def sum(f: Int => Int, a: Int, b: Int): Int = {
    if (a > b) 0
    else f(a) + sum(f, a + 1, b)
  }                                               //> sum: (f: Int => Int, a: Int, b: Int)Int
  def id(x: Int): Int = x                         //> id: (x: Int)Int

  def sumInts(a: Int, b: Int) = sum(id, a, b)     //> sumInts: (a: Int, b: Int)Int
  def sumCubs(a: Int, b: Int) = sum(cube, a, b)   //> sumCubs: (a: Int, b: Int)Int

  sumInts(1, 5)                                   //> res2: Int = 15
  sumCubs(2, 3)                                   //> res3: Int = 35
  sum(cube, 1, 2)                                 //> res4: Int = 9
  sum(x => x * x, 1, 2)                           //> res5: Int = 5

  // tail recursion
  def sumNew(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }
    loop(a, 0)
  }                                               //> sumNew: (f: Int => Int)(a: Int, b: Int)Int
  sumNew(x => x)(1, 2)                            //> res6: Int = 3

  //function that returns function
  def sumNew2(f: Int => Int): (Int, Int) => Int = {
    def sumF(a: Int, b: Int): Int = {
      if (a > b) 0
      else f(a) + sumF(a + 1, b)
    }
    sumF
  }                                               //> sumNew2: (f: Int => Int)(Int, Int) => Int

  sumNew(x => x)(1, 2)                            //> res7: Int = 3

  def sumer = sumNew2(x => x)                     //> sumer: => (Int, Int) => Int
  sumer(2, 2)                                     //> res8: Int = 2

  //wrote shorter
  def sum2(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 0 else f(a) + sum2(f)(a + 1, b)    //> sum2: (f: Int => Int)(a: Int, b: Int)Int

  sum2(x => x * 2)(1, 5)                          //> res9: Int = 30

  // sum and product in one function
  def product(f: Int => Int)(a: Int, b: Int): Int =
    if (a > b) 1
    else f(a) * product(f)(a + 1, b)              //> product: (f: Int => Int)(a: Int, b: Int)Int

  product(x => x * x)(3, 4)                       //> res10: Int = 144

  def fact(n: Int) = product(x => x)(1, n)        //> fact: (n: Int)Int
  fact(5)                                         //> res11: Int = 120

  //mapReduce
  def mapReduce(f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b: Int): Int =
    if (a > b) zero
    else combine(f(a), mapReduce(f, combine, zero)(a + 1, b))
                                                  //> mapReduce: (f: Int => Int, combine: (Int, Int) => Int, zero: Int)(a: Int, b
                                                  //| : Int)Int

  def product2(f: Int => Int)(a: Int, b: Int): Int = mapReduce(f, (x, y) => x * y, 1)(a, b)
                                                  //> product2: (f: Int => Int)(a: Int, b: Int)Int
  def fact2(n: Int) = product2(x => x)(1, n)      //> fact2: (n: Int)Int
  fact2(3)                                        //> res12: Int = 6

  // fixed Point

  import math.abs

  val tolerance = 0.0001                          //> tolerance  : Double = 1.0E-4
  def isCloseEnough(x: Double, y: Double) =
    abs((x - y) / x) / x < tolerance              //> isCloseEnough: (x: Double, y: Double)Boolean

  def fixedPoint(f: Double => Double)(firstGuess: Double) = {
    def iterate(guess: Double): Double = {
      val next = f(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }                                               //> fixedPoint: (f: Double => Double)(firstGuess: Double)Double
  
  fixedPoint(x=> 1 + x/2)(1)                      //> res13: Double = 1.999755859375
  
  
  //square by fixedPoint
 def sqrt(x: Double) = fixedPoint(y=> (y+ x/y)/2)(1.0)
                                                  //> sqrt: (x: Double)Double
 sqrt(5)                                          //> res14: Double = 2.236067977499978
 
 def averageDamp(f:Double => Double) (x: Double) = (x+f(x))/2
                                                  //> averageDamp: (f: Double => Double)(x: Double)Double
                                 
 def sqrt2(x: Double) = fixedPoint(averageDamp(y=>  x/y))(1)
                                                  //> sqrt2: (x: Double)Double
 sqrt(2)                                          //> res15: Double = 1.4142135623746899
}
