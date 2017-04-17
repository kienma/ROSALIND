

class GC(sequences:Vector[String]) {
    
  val D = new DNA(sequences(1))
  
  def GCContent(seq:String):Double={
    val seq_len:Double = seq.length
    val C = D.letterCount("C", seq).toDouble
    val G = D.letterCount("G", seq).toDouble
    return 100*(C+G)/seq_len
  }
  
  def max_GC_ID(sequences:Vector[String]):String={
    var GCcont:Vector[Double]=Vector()
    for (i <- Vector.range(1,sequences.length).filter(x => x%2 != 0)){
      val GC = GCContent(sequences(i))
      GCcont = GCcont :+ -1.0
      GCcont = GCcont :+ GC
    }
    println(GCcont)
    val max = GCcont.indexOf(GCcont.max)
    println(sequences(max-1).substring(1,sequences(max-1).length)) // 
    println(GCcont.max)
    return sequences(max-1)
  }
  
}
