package example
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, html}

@JSExport
object ScalaJSExample {

  def toRadians(degrees: Double) ={
    (degrees * math.Pi)/180
  }

  def drawLines(crossings: Int, canvas: html.Canvas) = {
    val context = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    val centerX = Math.floor(canvas.width / 2) 
    val centerY = Math.floor(canvas.height / 2)
    val radius = Math.floor(canvas.width / 2)

    val begin = 0
    val interval = 360 / ( 2 * crossings)
    val arcSize = toRadians(interval)

    context.strokeStyle = "black"
    context.lineWidth = 2

    drawArc(context, centerX, centerY, radius, 2*Math.PI, 0)

    (0 to crossings*2-1).map(_*arcSize).foreach{startingAngle =>
     // drawWithArrowhead(centerX+radius*Math.cos(startingAngle),centerY-radius*Math.sin(startingAngle), centerX, centerY, canvas);
    }

    drawWithArrowhead(centerX+radius*Math.cos(2*arcSize),centerY-radius*Math.sin(2*arcSize), centerX+radius*Math.cos(7*arcSize),centerY-radius*Math.sin(7*arcSize), canvas);

  }

  private def drawArc(context: CanvasRenderingContext2D, centerX: Double, centerY: Double, radius: Double, arcSize: Double, startingAngle: Double) = {
    context.arc(centerX, centerY, radius, startingAngle, startingAngle + arcSize, false)
    context.stroke
  }

  def drawWithArrowhead(x1: Double, y1: Double, x2: Double, y2: Double, canvas: html.Canvas) = {
    val ctx = canvas.getContext("2d")
      .asInstanceOf[ dom.CanvasRenderingContext2D ]

    ctx.strokeStyle = "blue"
    ctx.fillStyle = "blue"
    ctx.lineWidth = 1

    ctx.beginPath
    ctx.moveTo(x1, y1)
    ctx.lineTo(x2, y2)
    ctx.stroke
    var startRadians = Math.atan((y2 - y1) / (x2 - x1))
    if (x2 > x1) {
      startRadians += -90 * Math.PI / 180
    } else
      startRadians += 90 * Math.PI / 180
    drawArrowhead(canvas, x1, y1, startRadians)

  }

  def drawArrowhead(canvas: html.Canvas, x: Double, y:Double, radians: Double) = {
    val ctx = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    ctx.save
    ctx.beginPath
    ctx.translate(x, y)
    ctx.rotate(radians)
    ctx.moveTo(0, 0)
    ctx.lineTo(5, 20)
    ctx.lineTo(-5, 20)
    ctx.closePath
    ctx.restore
    ctx.fill

  }

  def calcAngle(x1: Double, y1:Double, x2: Double, y2: Double): Double = {
    if(x2 > x1){
      if(y2 > y1){
        Math.atan((y2 - y1) / (x2 - x1))
      }else{
        if(y2 == y1){
          0
        }else{
          2 * math.Pi + Math.atan((y2 - y1) / (x2 - x1))
        }
      }
    }else{
      if(x2 == x1){
        if(y2 == y1){
          0
        }else{
          if(y2 > y1){
            math.Pi / 2
          }else{
            1.5 * math.Pi
          }
        }
      }else{
        if(y2 == y1){
          math.Pi * 1
        }else{
          if(y2 > y1){
            math.Pi + Math.atan((y2 - y1) / (x2 - x1))
          }else{
            math.Pi + Math.atan((y2 - y1) / (x2 - x1))
          }
        }
      }
    }
  }

  @JSExport
  def main(canvas: html.Canvas): Unit = {
    drawLines(4,canvas)
  }
}
