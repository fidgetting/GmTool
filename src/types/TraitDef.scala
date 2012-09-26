package types

class TraitDef(val name: String,
    val names: List[String],
    val determ: List[(Double, String)]) {
  
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
    
    val name = (node \ "@name").text
    val names =
      (node \\ "trait").foldLeft(List[String]())(
          (accum, curr) => (curr \\ "@value").text :: accum)
    
    val inc = 100.0 / names.length.toDouble
    val determ =
      if((node \\ "@dist").text == "flat")
        (node \\ "trait").foldLeft(List[(Double, String)]())(
            (accum, curr) => {
              total = total + inc
              accum :+ (total, (curr \\ "@value").text)
            })
      else
        (node \\ "trait").foldLeft(List[(Double, String)]())(
            (accum, curr) => {
              total = total + (curr \\ "@chance").text.toDouble / 100.0
              accum :+ (total, (curr \\ "@value").text)
            })
    
    new TraitDef(name, names, determ)
  }
}

