

class IPRB(data:Vector[String]) {
  
  def dataRead(data:Vector[String]):Vector[Double]={
    var DATA:Vector[Double]=Vector()
    for (i <- 0 to data.length-1){
      val x:Double=data(i).toDouble
      DATA=DATA:+x
    }
    return DATA
  }
  // probability of homozygous recessive offspring
  def P_homorecessive(data:Vector[String]):BigDecimal={
    val DATA = dataRead(data)
    var pop:Vector[Double]=Vector() // population data
    for (i <- 0 to 2){
      pop=pop:+DATA(i)
    }
    val p1=pop.sum
    val p2=p1-1
    val n=DATA(1) // number of heterozygous individuals
    val m=DATA(2) // number of homozygous recessive individuals
    val P:BigDecimal=(n/p1)*((n-1)/p2)+(n/p1)*(m/p2)+(m/p1)*((m-1)/p2)*(0.25)
    return P // returns the complement 
  }
  // probability of offspring in population having a dominant allele
  def P_dominant_allele(data:Vector[String]):BigDecimal={
    val P = P_homorecessive(data)
    return 1-P
  }
  
}
