
import scala.math._
class math {
  // computes large products a bit more quickly
  def product(a:BigInt,b:BigInt):BigInt={
    val b1=b%10
    val b2=floor(b.toDouble/10).toInt
    val x1=a*b1
    val x2=a*b2*10
    val product:BigInt=x1+x2
    return product
  }
}
