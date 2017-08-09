package example
import example.ScalaJSExample.pairings

import scala.scalajs.js.annotation.JSExport
import org.scalajs.dom.{CanvasRenderingContext2D, html}
import org.scalajs.dom
import org.scalajs.dom.html.Div
import org.scalajs.dom.raw.{Node, NodeList}

import scalatags.JsDom.all._

@JSExport
object ScalaJSExample{

  def toRadians(degrees: Double) ={
    (degrees * math.Pi)/180
  }

  @JSExport
  def drawLines(canvasWithPairing:(html.Canvas, K.Pairing)) = {
    val (canvas, pairing) = canvasWithPairing
    val context = canvas.getContext("2d")
      .asInstanceOf[dom.CanvasRenderingContext2D]

    val centerX = Math.floor(canvas.width / 2)
    println(centerX)
    val centerY = Math.floor(canvas.height / 2)
    println(centerY)
    val radius = Math.floor(canvas.width / 2)

    val begin = 0
    val crossings = pairing.size
    val interval = 360 / ( 2 * crossings)
    val arcSize = toRadians(interval)

    context.strokeStyle = "black"
    context.lineWidth = 2

    drawArc(context, centerX, centerY , radius, 2*Math.PI, 0)

    (0 to crossings*2-1).map(_*arcSize).foreach{startingAngle =>

    }

    pairing.foreach{p =>
      drawWithArrowhead(centerX+radius*Math.cos(p._2*arcSize),centerY-radius*Math.sin(p._2*arcSize), centerX+radius*Math.cos(p._1*arcSize),centerY-radius*Math.sin(p._1*arcSize), canvas);
    }
  }

  private def drawArc(context: CanvasRenderingContext2D, centerX: Double, centerY: Double, radius: Double, arcSize: Double, startingAngle: Double) = {
    context.arc(centerX, centerY, radius, startingAngle, startingAngle + arcSize, true)
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

  @JSExport
  def pairings(n:Int) = {
    K.configurations(n)
  }

  def drawCanvas(target: html.Div, idNo: Int): Unit = {
    def renderCanvas: Div = {
      div(id := "div" + idNo, width := 400, height := 400, margin := 20,
        canvas(id := "canvas" + idNo, attr("width") := 300, attr("height") := 300, attr("margin") := 50, attr("class") := "circleCanvas")
      ).render
    }

    target.appendChild(renderCanvas)


  }

  def canvases(target: html.Div): Seq[html.Canvas] = {
    val canvases: NodeList =target.getElementsByClassName("circleCanvas")
    (0 to canvases.length-1).map{ i =>
      canvases(i).asInstanceOf[html.Canvas]
    }
  }

  @JSExport
  def main(target: html.Div): Unit = {
    val ps = pairings(3)

    for(it <- 1 to ps.size) yield drawCanvas(target,it)

    (canvases(target) zip ps.map(_._1).filter(K.reduce(_).isEmpty)).foreach(drawLines);
  }
}
