package types

class NPC(val first: String, val last: String, val ethnicity: String,
    val traits: Map[String, String], var notes: Seq[String] = Nil) {
  
  def toXml() =
    <npc firstName={first} lastName={last} ethnicity={ethnicity}>
      { for(elem <- traits) yield <trait name={elem._1} value={elem._2}/> }
      { for(elem <- notes)  yield <note>{elem}</note> }
    </npc>
  
  def toCSV() =
    first + ", " + last + ", " + ethnicity + ", " + 
    (for(elem <- traits) yield elem._2 + ", ") +
    (for(elem <- notes) yield "::" + elem) + ",\n"
  
  def addNote(str: String) =
    notes = notes :+ str
}

object NPC {
  
  def apply(first: String, last: String, ethnicity: String, 
      traits: Map[String, String]): NPC =
    new NPC(first, last, ethnicity, traits)
  
  def apply(node: scala.xml.Node): NPC =
    new NPC((node \ "@firstName").text,
        (node \ "@lastname").text,
        (node \ "@ethnicity").text,
        (for(curr <- node \\ "trait")
          yield (curr \ "@name").text -> (curr \ "@value").text) toMap,
        (for(curr <- node \\ "note")
          yield curr.text))
}
