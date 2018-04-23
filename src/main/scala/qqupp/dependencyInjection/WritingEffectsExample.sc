import cats.data.Writer
import cats.implicits.catsKernelStdMonoidForString

val l = List(2,3)

val pureFun = (x: Int) => x * x

val p1 = l.map( pureFun ).map( pureFun )
// p1: List[Int] = List(16, 81)

val p2 = l.map( pureFun andThen pureFun )
// p2: List[Int] = List(16, 81)



val impureFun = (x: Int) => {println(x); x * x}

val i1 = l.map( impureFun ).map( impureFun )
// 2
// 3
// 4
// 9
// i1: List[Int] = List(16, 81)

val i2 = l.map( impureFun andThen impureFun )
// 2
// 4
// 3
// 9
// i2: List[Int] = List(16, 81)




val monadicFun = (x: Int) => Writer[String,Int](s"$x\n",x * x)

val m1 = l.map(monadicFun).map( _.flatMap(monadicFun))

m1.foreach( x => print(x.written))
val mr1 = m1.map( _.value)
// 2
// 4
// 3
// 9
// mr1: List[cats.Id[Int]] = List(16, 81)

val m2 = l.map(monadicFun andThen( _.flatMap(monadicFun)))
m2.foreach( x => print(x.written))
val mr2 = m2.map( _.value)
// 2
// 4
// 3
// 9
// mr2: List[cats.Id[Int]] = List(16, 81)



