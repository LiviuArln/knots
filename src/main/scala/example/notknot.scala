package example

import scala.annotation.tailrec

object K {
    type Pairing = Set[(Int,Int)]
    type PairingGroups = Set[(Pairing, Int)]

    def configurations(kk:Int) = cleanParings((0 to kk*2-1).toSet[Int])

    def pairing(l : Set[Int]):Set[Pairing] = {
        def pairingAux(remaining:Set[Int]): Set[Pairing] = {
            if(remaining.size == 2) Set(Set(remaining.head -> remaining.last))
            else for {
                origin <- remaining
                destination <- remaining - origin
                subPairing <- pairingAux(remaining - origin - destination)
            } yield subPairing + (origin -> destination)
        }

        pairingAux(l)
    } 

    def cleanParings(l:Set[Int]) = { 
        val size = l.size

        @tailrec 
        def cleaner(toClean:Set[Pairing], cleaned:PairingGroups) : PairingGroups = 
            if(toClean.isEmpty) cleaned 
            else {
                val h = toClean.head
                val eqv = equivalentPairings(h)
                cleaner(toClean -- eqv, cleaned + (h -> eqv.size))
            }

        def equivalentPairings(pairing:Pairing) = {
            val rotationDeltas = (0 to size - 1).toSet[Int]
            val rotations = rotationDeltas.map(rotation(pairing)) 
            val mirrors = rotations.map(mirror)
            rotations ++ mirrors
        }

        def rotation(pairing:Pairing)(d:Int) = pairing.map { p => 
            ((p._1 + d) % size, (p._2 + d) % size )
        }

        def mirror(pairing:Pairing) = pairing.map { p => 
            (size - 1 - p._1, size - 1 - p._2 )
        }

        cleaner(pairing(l),Set.empty)
    }

    case class Crossing(over:Set[Char], under:Set[Char])

    def cuttings(i:Int) = Set(('a'+i).toChar, ('b'+i).toChar)

    def toLoop(pairing : Pairing) = pairing.map { case (over,under) =>
        Crossing(cuttings(over),cuttings(under))
    }

    def defaultPairs(size:Int) = {
        val origins = (0 to size - 1)
        val destinations = origins.map{ i => (i + 1) % size }
        val forwardDefaults = origins zip destinations
        val backwardDefaults = destinations zip origins
        forwardDefaults.toSet ++ backwardDefaults.toSet
    }

    def paths(pairing : Pairing) = {
        val size = pairing.size * 2
        val reverse = pairing.map{ case (o,d) => d -> o }
        val allPairs = pairing ++ reverse ++ defaultPairs(size)
        val adjacency = allPairs.groupBy(_._1).map {
            case (from, toSet) => (from, toSet.map(_._2))
        }
        allPairs
    }


    def reduce(pairing : Pairing) : Option[Pairing] = {
        val size = pairing.size * 2

        def circleOnlyCut(pair: (Int,Int)) = {
            val gp = gapPoints(pair)
            (pairing - pair).forall { case (a, b) => 
                xor(gp.contains(a), gp.contains(b))
            }
        }

        def gapPoints(pair: (Int,Int)) = {
            val (x,y) = pair
            val gap = x - y
            val gapStart = if(gap > 0) y else x
            (1 to Math.abs(gap) - 1).map{ i => (gapStart + i) % size } 
        }

        def xor(a:Boolean, b:Boolean) = (a && b) || (!a && !b)

        def extractPoint(smallerPairing : Pairing, toExtract : Int) = {
            def shift(toShift : Int) = toShift - (if(toShift > toExtract) 1 else 0)
            
            smallerPairing.map { case (x,y) =>
                shift(x) -> shift(y)
            }
        }

        def extractPair(smallerPairing : Pairing, pair : (Int, Int)) = {
            val (x,y) = pair
            if(x > y) extractPoint(extractPoint(smallerPairing, x), y)
            else extractPoint(extractPoint(smallerPairing, y), x)
        }

        def flipGapPoints(smallerPairing:Pairing, gp: Seq[Int]) = {
            def flip(n:Int) = gp.size - n + 2 * gp.head - 1

            smallerPairing.map { case (a,b) => 
                if(gp.contains(a)) flip(a) -> flip(b)
                else a -> b
            }
        }

        if(size == 2) None
        else pairing.find(circleOnlyCut).map { cut =>
            val gp = gapPoints(cut)
            extractPair(flipGapPoints(pairing - cut, gp), cut)
        }
    }

    def test1 = reduce(Set((0,2),(1,3),(9,4),(8,6),(7,5)))

    def test2 = reduce(Set((0,1),(2,3),(9,4),(8,6),(7,5)))

    def test3 = reduce(Set((0,5),(1,6),(2,7),(3,8),(4,9)))
}