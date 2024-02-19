val l = (5000000 to 5012000 by 20)
  .zipWithIndex
  .map(t => (t._1.toDouble, t._2.toDouble))
  .toList

val next: (Seq[(Double, Double)], Double, Int, Int) => Seq[(Double, Double)] = {
  (xs, base, st, end) => {
//    println(s"base: ${base} start: ${base + st} end: ${base + end}")
    xs.dropWhile(_._1 < base + st).takeWhile(_._1 <= base + end)
  }
}

val base = next.curried(l).apply(l.head._1)
val b0 = base(0)(20)
val b4 = base(3980)(4020)
val b8 = base(7980)(8020)
val b12 = base(11980)(12000)
val b2 = base(1000)(3000)
val b6 = base(5000)(7000)
val b10 = base(9000)(11000)


