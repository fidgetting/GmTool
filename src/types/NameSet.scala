package types

abstract class Gender
case class Male   extends Gender
case class Female extends Gender

class NameSet(val name: String,
    val male:   Map[String, Seq[String]] ,
    val female: Map[String, Seq[String]] ) {
  
  def apply(ethnicity: String, gender: Gender): String = gender match {
    case Male()   => male  (ethnicity)(Workspace.Int(male  (ethnicity).length))
    case Female() => female(ethnicity)(Workspace.Int(female(ethnicity).length))
  }
}

object NameSet {
  
  def apply(node: scala.xml.Node): NameSet = {
    def isMale(str: scala.xml.NodeSeq): Boolean =
      str.text == "m" || str.text == "mf"
    def isFemale(str: scala.xml.NodeSeq): Boolean =
      str.text == "f" || str.text == "mf"
    
    new NameSet(
        (node \ "@name").text,
        (for(elem <- node \\ "ethnicity")
          yield (elem \ "@name").text ->
            (for(curr <- elem \\ "name" if isMale(curr \ "@gender"))
              yield (curr \"@name").text)) toMap ,
        (for(elem <- node \\ "ethnicity")
          yield (elem \ "@name").text ->
            (for(curr <- elem \\ "name" if isFemale(curr \ "@gender"))
              yield (curr \"@name").text)) toMap )
  }
}
