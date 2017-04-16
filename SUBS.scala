

class SUBS(motif:String, sequence:String) {
  // finds the sites of a motif within a sequence
  def motifSites(motif: String, sequence:String):Vector[BigInt]={
    val motif_length = motif.length
    val seqL = sequence.length
    var index1 = 0
    var index2 = motif_length
    var sites:Vector[BigInt]=Vector()
    while (index2 != seqL){
      val sub = sequence.substring(index1, index2)
      if (sub == motif){
        val site:BigInt = index1+1
        sites = sites :+ site
      }
      index1 += 1 
      index2 += 1 
    }
    return sites
  }
  
}
