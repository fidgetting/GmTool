package types

class World(val name: String, val races: TraitDef, val traits: Seq[String]) {
  
  override def toString =
    name + "{\n" +
    races +
    "TRAITS\n" +
    traits.foldLeft("")((accum, curr) => accum + "  " + curr + "\n") +
    "}"
}

object World {
  
  def apply(node: scala.xml.Node): World =
    new World(
        (node \ "@name").text,
        TraitDef((node \ "trait_def")(0)),
        for(curr <- (node \ "traits" \ "trait"))
          yield (curr \ "@name").text)
}
