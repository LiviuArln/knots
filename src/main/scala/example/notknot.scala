
import scala.annotation.tailrec

object K {
    type Pairing = Set[(Int,Int)]
    type PairingGroup = Set[(Pairing, Int)]

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
        def cleaner(toClean:Set[Pairing], cleaned:PairingGroup) : PairingGroup = 
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

    case class Crossing(over:Set[Char], under:Set[Char]) {

    }

    def toLoop(pairing : Pairing) = {
        def cuttings(i:Int) = Set(('a'+i).toChar, ('b'+i+1).toChar)

        pairing.map { case (over,under) =>
            Crossing(cuttings(over),cuttings(under))
        }
    }



}