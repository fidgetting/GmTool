package types

class TraitDef(val name: String,
    val names:  Seq[String],
    val determ: Seq[(Double, String)]) {
  
  def apply(): String = {
    val d = Workspace.Double()
    var curr = 0
    
    while(d > determ(curr)._1)
      curr = curr + 1
    determ(curr)._2
  }
  
  override def toString =
    name + ":\n" +
    determ.foldLeft("")(
        (accum, curr) => accum + "  " + curr._1 + " => " + curr._2 + "\n")
}

object TraitDef {
  
  def apply(node: scala.xml.Node): TraitDef = {
    var total = 0.0
    
    def calDeterm(incF: scala.xml.Node => Double): Seq[(Double, String)] =
      for(curr <- (node \\ "trait").reverse) yield {
          total = incF(node)
          (total, (curr \\ "@value").text)
      }
    
    val name  = (node \ "@name").text
    val names = for(curr <- node \\ "trait") yield (curr \\ "@value").text
    
    val inc = 100.0 / names.length.toDouble 
    val determ =
      if((node \\ "@dist").text == "flat") calDeterm((node) => total + inc)
      else calDeterm((node) => total + (node \\ "@chance").text.toDouble / 100.0)
    
    new TraitDef(name, names, determ)
  }
}

