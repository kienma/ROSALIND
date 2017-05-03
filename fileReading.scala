import scala.io.Source

class fileReading {
  
  // reads the txt file as a sequence  
  def txt_sequence1(file:String):String={
    val sequence = scala.io.Source.fromFile(file).mkString.capitalize
    val final_sequence = sequence.filter(i => i != '\n')
    return final_sequence
  }
  
  // reads txt file for many lines 
  def txt_sequences(file:String):Vector[String]={
    val raw = scala.io.Source.fromFile(file).mkString.capitalize
    val RawLines = raw.split('\n')
    var lines:Vector[String] = Vector()
    for (i <- 0 to RawLines.length-1){
      val fin_line = RawLines(i).filter(i => i != '\n') // processed line, without the indent 
      lines = lines :+ fin_line
    }
    return lines
  }

  // assembles full sequences from FASTA file fragments
  def assembly(frags:Vector[String]):Vector[String]={
    val l = frags.length
    var index:Vector[Int] = Vector()
    for (i <- 0 to frags.length-1){
      val f = frags(i)
      if (f.contains('>') == true){
        index = index :+ i
      }
    }
    var assemble:Vector[String] = Vector()
    for (i <- 0 to index.length-2){
      val a = index(i)+1
      val b = index(i+1)-1
      var sequence:String = "" // assembled sequence
      for (j <- a to b){
        sequence = sequence ++ frags(j)
      }
      assemble = assemble :+ frags(index(i))
      assemble = assemble :+ sequence
    }
    var f_assemble:String = ""
    for (i <- index(index.length-1)+1 to frags.length-1){
      f_assemble = f_assemble ++ frags(i)
    }
    assemble = assemble :+ frags(index(index.length-1))
    assemble = assemble :+ f_assemble
    return assemble
  }
  
  // reads a codon table document
  def codonTable_read(file:String):Vector[String]={
    val raw = scala.io.Source.fromFile(file).mkString
    val p1 = raw.replace('\n', ' ')
    val p2 = p1.split(' ')
    var L1:Vector[String]=Vector()
    for (i <- 0 to p2.length-1){
      val line = p2(i)
      L1 = L1 :+ line
    }
    val L2 = L1.filter(i => i != "")
    return L2
  }
  // general importing function 
  def generalImport(file:String):Vector[String]={
    val raw = scala.io.Source.fromFile(file).mkString
    val p1 = raw.replace('\n', ' ')
    val p2 = p1.split(' ')
    var L1:Vector[String]=Vector()
    for (i <- 0 to p2.length-1){
      val line = p2(i)
      L1 = L1 :+ line
    }
    val L2 = L1.filter(i => i != "")
    return L2
  }
  // reading FASTA file
  def FASTA_read(file:String):Vector[String]={
    val parse = generalImport(file)
    val assemble = assembly(parse)
    return assemble
  }
}
