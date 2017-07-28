object K {
    def k(kk:Int) = (0 to kk*2-1).toList

    def rotations(l:List[Int]) = {
        val s = l.size
        l :: (1 to s-1).toList.map { i =>
            l.map { x => (x + i) % s }
        }
    }

    def pairing(l:List[Int]) = {
        pairing2(l).map { _._1 }
    }

    def pairing2(l:List[Int]):List[(List[(Int, Int)], List[Int])] = l.flatMap { e1 =>
        (l diff e1::Nil).map{ e2 =>
            ((e1, e2) :: Nil, l diff e1::e2::Nil)
        }
    }.flatMap { p1 =>
        if(p1._2.isEmpty) p1 :: Nil
        else pairing2(p1._2).map { p2 =>
          (p1._1 ::: p2._1, p2._2)
        }
    }

    def zzz(x:List[(List[(Int, Int)], List[Int])]) = ???
}