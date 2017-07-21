package example
import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom
import org.scalajs.dom.html

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
    var startingAngle = begin

    while (startingAngle < 360) {
      context.beginPath
      context.moveTo(centerX, centerY)
      context.arc(centerX, centerY, radius, toRadians(startingAngle), startingAngle + arcSize, false)
      context.closePath
      context.stroke
      startingAngle = startingAngle + interval
    }
  }

  @JSExport
  def main(canvas: html.Canvas): Unit = {
    drawLines(3,canvas)
  }
}
