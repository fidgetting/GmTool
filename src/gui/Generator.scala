package gui

import swing._
import swing.event._
import javax.jws

object Generator extends SwingApplication {
  
  val works = types.Workspace("workspace.xml")
  val world = "Deadlands"
  
  val checkNone   = new RadioButton { text = "Either" }
  val checkMale   = new RadioButton { text = "Male" }
  val checkFemale = new RadioButton { text = "Female" }
  val male_female = new ButtonGroup(checkNone, checkMale, checkFemale)
  
  val ethnicityList = new ComboBox("None" :: works.worlds(world).races.names)
  val generate = new Button { text = "Generate" }
  
  val headers = ("name" :: "ethnicity" :: works.worlds(world).traits).toSeq
  val rowData = Array.tabulate[Any](40, headers.length) {(a, b) => ""}
  val viewTable = new Table(rowData, headers) {
    autoResizeMode = Table.AutoResizeMode.Off
  }
 
  
  def top = new MainFrame {
    title = "Generate NPC"
      
    contents = new GridPanel(3, 1) {
      contents += new FlowPanel(new Label("Gender: ")) { contents ++= male_female.buttons }
      contents += new FlowPanel(ethnicityList)
      contents += new FlowPanel(generate)
      border = Swing.EmptyBorder(5)
    }
    
  }
  
  def view = new MainFrame {
    title = "View Generated NPC's"
    
    contents = new ScrollPane(viewTable) {
      border = Swing.EmptyBorder(5)
    }
    
  }
  
  listenTo(generate)
  reactions += {
    case ButtonClicked(generate) =>
      for(i <- 0 to 9) {
        val npc = works(world)
        
        viewTable(i, 0) = npc.first + " " + npc.last
        viewTable(i, 1) = npc.ethnicity
        
        for(j <- 0 until works.worlds(world).traits.length)
          viewTable(i, j + 2) = npc.traits(works.worlds(world).traits(j))
      }
  }
  
  override def startup(args: Array[String]) {
    val t = top
    val v = view
    
    if(t.size == new Dimension(0, 0)) t.pack()
    if(v.size == new Dimension(0, 0)) v.pack()
    
    t.visible = true
    v.visible = true
  }
}