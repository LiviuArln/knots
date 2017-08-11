package example

import scala.annotation.tailrec

object K {
    val BASE = 0
    type PairingSet = Set[(Int,Int)]
    type PairingGroups = Set[(PairingSet, Int)]

    implicit class Pair(pair:(Int,Int)) {
        val (x,y) = pair
        
        def isAscending = x < y

        def descending = if (isAscending) pair.swap else pair

        def tupleMap[S](map: Int => S) = map(x) -> map(y)
    }

    implicit class Pairing(pairs:PairingSet) {
        val size = pairs.size * 2

        def simplify : Pairing = {
            if(isReduceable) pairs.find(isCircleOnlyCut).map { cut =>
                val gp = gapPoints(cut)
                val reduced = (pairs - cut).flipGapPoints(gp).extractPair(cut.descending)
                reduced.simplify
            }.get else this
        }

        def isReduceable = {
            size != 2 && (hasCircleOnlyCut || hasConsecutivePairs)
        }

        def hasCircleOnlyCut = pairs.exists(isCircleOnlyCut)

        def isCircleOnlyCut(pair: (Int,Int)) = {
            val gp = gapPoints(pair)
            val xort = (xor _).tupled
            (pairs - pair).map(tupleMap(gp.contains)).forall(xort)
        }

        def findPairEndByStart(start:Int) = pairs.find(_._1 == start).map(_._2)

        def next(i:Int) = jump(1,i)

        def jump(step: Int, from: Int) =  (from + step) % size

        def consecutivePairs = {
            for {
                i <-  (0 to size - 1)
                end1 <- findPairEndByStart(i)
                end2 <- findPairEndByStart(next(i))
                if next(end1) == end2 || next(end2) == end1
            } yield i
        }

        def hasGap(g:Int)(p:(Int,Int)) = {
            val (x,y) = p.descending
            (x - y) == g
        }

        def hasEvenGap(p:(Int,Int)) = {
            val (x,y) = p.descending
            (x - y) % 2 == 0
        }

        def isImpossible = pairs.find(hasEvenGap).isDefined

        def hasConsecutivePairs = !consecutivePairs.isEmpty

        def gapPoints(pair: (Int,Int)) = {
            val (x,y) = pair
            val gap = x - y
            val gapStart = if(gap > 0) y else x
            (1 to Math.abs(gap) - 1).map{ i => (gapStart + i) % size } 
        }

        def shift(lastFixedPoint:Int)(toShift : Int) = toShift - (if(toShift > lastFixedPoint) 1 else 0)    

        def extractPoint(toExtract : Int) = pairs.map(tupleMap(shift(toExtract)))

        val extractPair: ((Int,Int)) => PairingSet = { case (x, y) => extractPoint(x).extractPoint(y) }

        def gapFlip(gp: Seq[Int])(n:Int) = if(gp.contains(n)) gp.size - n + 2 * gp.head - 1 else n

        def flipGapPoints(gp: Seq[Int]) = pairs.map(tupleMap(gapFlip(gp)))

        def rotate(delta:Int)(point:Int) = (point + delta) % size

        def rotation(d:Int) = pairs.map(tupleMap(rotate(d)))

        def mirror = pairs.map(tupleMap(size - 1 - _))

        def equivalentPairings = {
            val rotationDeltas = (0 to size - 1).toSet[Int]
            val rotations = rotationDeltas.map(rotation) 
            val mirrors = rotations.map(_.mirror)
            rotations ++ mirrors
        }

        def toLoop = pairs.map { case (over,under) =>
            Crossing(cuttings(over),cuttings(under))
        }

        override def toString = pairs.toString
    }

    

    def xor(a:Boolean, b:Boolean) = (a && b) || (!a && !b)
    
    def tupleMap[S](map: Int => S)(tuple:(Int,Int)) = tuple.tupleMap(map)

    def configurations(kk:Int) = cleanParings((0 to kk*2-1).toSet[Int])

    def pairing(l : Set[Int]):Set[PairingSet] = {
        def pairingAux(remaining:Set[Int]): Set[PairingSet] = {
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
        def cleaner(toClean:Set[PairingSet], cleaned:PairingGroups) : PairingGroups = 
            if(toClean.isEmpty) cleaned 
            else {
                val h = toClean.head
                val eqv = h.equivalentPairings
                cleaner(toClean -- eqv, cleaned + (h -> eqv.size))
            }

        cleaner(pairing(l),Set.empty)
    }

    case class Crossing(over:Set[Char], under:Set[Char])

    def cuttings(i:Int) = Set(('a'+i).toChar, ('b'+i).toChar)

    

    def defaultPairs(size:Int) = {
        val origins = (0 to size - 1)
        val destinations = origins.map{ i => (i + 1) % size }
        val forwardDefaults = origins zip destinations
        val backwardDefaults = destinations zip origins
        forwardDefaults.toSet ++ backwardDefaults.toSet
    }

    def paths(pairing : PairingSet) = {
        val size = pairing.size * 2
        val reverse = pairing.map(_.swap)
        val allPairs = pairing ++ reverse ++ defaultPairs(size)
        val adjacency = allPairs.groupBy(_._1).map {
            case (from, toSet) => (from, toSet.map(_._2))
        }
        allPairs
    }


    

    def test1 = Set((0,2),(1,3),(9,4),(8,6),(7,5)).simplify

    def test2 = Set((0,1),(2,3),(9,4),(8,6),(7,5)).simplify

    def test3 = Set((0,5),(1,6),(2,7),(3,8),(4,9)).simplify

    def test4 = Set((0,1),(2,3),(4,5),(6,7),(8,9)).simplify
}