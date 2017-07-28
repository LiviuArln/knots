object MyApp {
import org.scalacheck._
import Gen._


def moreThanC:Gen[String] = frequency((10,lazyC), (2,lazyB), (1, lazyEnd)).map("c"+_)
def lazyC = lzy{moreThanC}
def lazyB = lzy{moreThanC.map("b"+_)}
def lazyEnd = lzy{const("e")}
def moreThanA = frequency((10,lazyB), (1,lazyC)).map("a"+_)


def sample = listOfN(10,moreThanA).sample.get.foreach(println)

case class Graph(order:Int, pairs:Set[(Int,Int)]) {
  def edgesFrom(vertex:Int) = pairs.groupBy{_._1}.get(vertex).map(_.toList)
}

def pairGen(order:Int) = for {
    origin <- chooseNum(0,order-1)
    destination <-chooseNum(0,order-1)
} yield (origin, destination)

def pairsGen(order:Int) = buildableOfN[Set[(Int,Int)],(Int,Int)](order*order/10, {pairGen(order)})

def graphGen = for {
    o <- chooseNum(2,10)
    pairs <- pairsGen(o)
} yield Graph(o,pairs)

def from(vertex:Int) = for {
    edges <- graphGen.map(_.edgesFrom(vertex))
    if edges.isDefined
    index <- chooseNum(0, edges.get.length-1)
    pair <- edges.get(index)
} yield pair._2



def pathInGraph =   graphGen.flatMap { g =>
    fromEdge(0,g)
}

def fromEdge(vertex:Int, g:Graph):Gen[String] = lzy {
    fromEdges(g.edgesFrom(vertex)).flatMap { pair =>
        println(pair._2)
        if(pair._2 != -1) fromEdge(pair._2,g)
        else ""
    }.map { "" + vertex + _ }
}


def fromEdges(edges: Option[List[(Int,Int)]]) = lzy { edges.map { es =>
    oneOf(es)
}.getOrElse(const((0,-1))) }





def test = {
    try {
        pathInGraph.sample
    } catch {
        case e:Throwable => println(e.getMessage)
    }
    
}

}