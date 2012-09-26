package types

abstract class Gender
case class Male   extends Gender
case class Female extends Gender

class NameSet(val name: String,
    val male: Map[String, List[String]],
    val female: Map[String, List[String]]) {
  
  def apply(ethnicity: String, gender: Gender): String = gender match {
    case Male()   => male  (ethnicity)(Workspace.Int(male  (ethnicity).length))
    case Female() => female(ethnicity)(Workspace.Int(female(ethnicity).length))
  }
}

object NameSet {
  
  def apply(node: scala.xml.Node): NameSet =
    new NameSet(
        (node \ "@name").text,
        (node \\ "ethnicity").foldLeft(Map[String, List[String]]())(
          (accum, curr) => accum + (
            (curr \ "@name").text ->
               (curr \\ "name").filter(
                 (name) => (name \ "@gender").text == "m" || (name \ "@gender") == "mf").map(
                   (name) => (name \ "@name").text).toList)),
        (node \\ "ethnicity").foldLeft(Map[String, List[String]]())(
          (accum, curr) => accum + (
            (curr \ "@name").text ->
              (curr \\ "name").filter(
                (name) => (name \ "@gender").text == "f" || (name \ "@gender") == "mf").map(
                  (name) => (name \ "@name").text).toList)))
}
