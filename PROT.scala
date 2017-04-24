

class PROT(mRNA:String, codonTable:Vector[String]) {
  
  def codonRead(triplet:String,codonTable:Vector[String]):String={
    val I = codonTable.indexOf(triplet)
    return codonTable(I+1)
  }
  
  def translate(mRNA:String):String={
    var prot: String = "" 
    var I1 = 0
    var I2 = 3 
    var triplet = mRNA.substring(I1,I2) 
    var read = codonRead(triplet,codonTable)
    while (read != "Stop"){
      prot = prot ++ read
      I1 += 3
      I2 += 3
      triplet = mRNA.substring(I1,I2)
      read = codonRead(triplet,codonTable)
    }
    return prot
  }
  
}
