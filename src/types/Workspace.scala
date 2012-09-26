package types

class Workspace(val first: NameSet, val last: NameSet,
    val traits: Map[String, TraitDef],
    val worlds: Map[String, World]) {
  
  def apply(world: String,
      givenEthnicity: String = null,
      givenFirst: String = null,
      givenLast: String  = null,
      givenTraits: Map[String, String] = null) = {
    val gender = if(Workspace.Boolean) Male() else Female()
    
    val ethnicity  = if(givenEthnicity == null) worlds(world).races()    else givenEthnicity
    val firstName  = if(givenFirst     == null) first(ethnicity, gender) else givenFirst
    val lastName   = if(givenLast      == null) last (ethnicity, gender) else givenLast
    val passTraits = worlds(world).traits.foldLeft(Map[String, String]())(
        (accum, curr) => accum + (curr -> (
          if(givenTraits != null && givenTraits.contains(curr)) givenTraits(curr)
          else traits(curr)())))
    
    NPC(firstName, lastName, ethnicity, passTraits)
  }
  
}

object Workspace {
  
  val _ran = new scala.util.Random()
  
  def Double(): Double =
    _ran.nextDouble()
  
  def Int(n: Int): Int =
    _ran.nextInt(n)
    
  def Boolean(): Boolean =
    _ran.nextBoolean()
  
  def apply(serialized: scala.xml.Node): Workspace =
    new Workspace(
          NameSet((serialized \ "names" \ "first")(0)),
          NameSet((serialized \ "names" \ "last" )(0)),
          (serialized \ "possible_traits" \\ "trait_def").
            foldLeft(Map[String, TraitDef]())(
              (accum, node) => accum + ((node \ "@name").text -> TraitDef(node))),
          (serialized \\ "world").
            foldLeft(Map[String, World]())(
              (accum, node) => accum + ((node \ "@name").text -> World(node))))
  
  def apply(str: String): Workspace =
    Workspace(scala.xml.XML.loadFile(str))
}
