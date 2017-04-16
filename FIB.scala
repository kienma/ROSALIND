

class FIB {
  // computes term n of the fibonacci sequence by tail recursion 
  // starts with 1 as the first two terms 
  def TR_Fibonacci(n:BigInt):BigInt={
    if (n == 1 || n == 2){
      return 1 
    }
    if (n == 3){
      return 2 
    }
    var fibo_seq:Vector[BigInt]=Vector(1,1,2)
    while (fibo_seq.length < n) {
      val Fn = fibo_seq.max + fibo_seq(fibo_seq.length-2)
      fibo_seq = fibo_seq :+ Fn
    }
    return fibo_seq(n.toInt-1)
  }
  
  // computes the number of rabbit pairs present after n months, each rabbit producing k pairs 
  def rabbit_population(n:BigInt, k:BigInt):BigInt = {
    if (n == 1 || n == 2){
      return 1 
    }
    if (n == 3){
      return 1+k
    }
    var R:Vector[BigInt]=Vector(0,1,1) // total number of reproductively mature rabbits 
    var F:Vector[BigInt]=Vector(0,0,k) // values of offspring pairs 
    var T:Vector[BigInt]=Vector(1,1,k+1) // values of total pairs 
    while (T.length < n){
      val r = R.max // T_n-1 
      val f = r*k 
      F = F :+ f
      val t:BigInt = T.max + F.max
      R = R :+ T.max
      T = T :+ t 
    }
    return T(T.length-1)
  }
  
}
