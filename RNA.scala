class RNA (seq: String){
  // transcribes DNA into RNA 
  def transcribe(sequence:String):String={ 
    return sequence.replace('T', 'U')
  }
}
