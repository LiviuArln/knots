object K {
    type Pairing = Set[(Int,Int)]
    type PairingGroup = Set[(Pairing, Int)]

    def configurations(kk:Int) = cleanParings((0 to kk*2-1).toSet[Int]).foreach(println)


    def pairing(l:Set[Int]) = {
        pairing2(l).map { _._1 }
    }

    def pairing2(l : Set[Int]) : Set[(Pairing, Set[Int])] = l.flatMap { e1 =>
        (l - e1).map{ e2 =>
            (Set((e1, e2)), l - e1 - e2)
        }
    }.flatMap { p1 =>
        p1._2.headOption.map { _ =>
            pairing2(p1._2).map { p2 =>
                (p1._1 ++ p2._1, p2._2)
            }
        }.getOrElse(Set(p1))
    }


    

    

    def cleanParings(l:Set[Int]) = { 
        def cleaner(remaining:Set[Pairing], cleaned:PairingGroup) : PairingGroup = remaining.headOption.map { h =>
            val rm = rotationsAndMirors(h)
            cleaner(remaining -- rm, cleaned + ((h, rm.size)))
        }.getOrElse(cleaned)

        def rotationsAndMirors(pairing:Pairing) = {
            val size = pairing.size * 2
            val rotations = (0 to size - 1).toSet[Int].map { d => rotation(pairing, d, size) } 
            val mirrors = rotations.map(mirror(_,size))
            rotations ++ mirrors
        }

        def rotation(pairing:Pairing, d:Int, size:Int) = pairing.map { p => 
            ((p._1 + d) % size, (p._2 + d) % size )
        }

        def mirror(pairing:Pairing, size:Int) = pairing.map { p => 
            (size - 1 - p._1, size - 1 - p._2 )
        }
        cleaner(pairing(l),Set.empty)
    }



}