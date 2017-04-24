

class PRTM(peptide:String,MData:Vector[String]) {
  // MData is Monoisotopic mass data
  def protein_mass(peptide:String,MData:Vector[String]):Double={
    val L = peptide.length-1 //length of the string
    var mass:Double=0.000 //mass of the protein
    for (i <- 0 to L){
      val AA = peptide(i).toString
      val I = MData.indexOf(AA)+1
      mass += MData(I).toDouble
    }
    return mass // returns mass of the peptide chain
  }
}
