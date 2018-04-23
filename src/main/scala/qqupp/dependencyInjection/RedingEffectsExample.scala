package qqupp.dependencyInjection

import cats.Id

import scala.io.Source
import cats.data.Reader

/**
  * Effects don't compose.
  * Impure functions don't compose.
  *
  * Created on 23/04/18.
  */
object ReadingEffectsExample extends App {

  val file= Source.fromResource("test.txt")

  val fileIterator  = file.getLines()


  val pureFunction = (x: Int) => x * x

  val impureFunction = (x: Int) => {
    val v = fileIterator.next().toInt
    x * v
  }

  val l = List(1,2,3)


  println("Pure function test")
  val pureResult_1 = l.map( pureFunction ).map( pureFunction )
  val pureResult_2 = l.map( pureFunction andThen pureFunction )
  println(s"Res 1 =?= Res 2 : ${pureResult_1 == pureResult_2} = $pureResult_1 = $pureResult_2")

  println("")

  println("Impure function test")
  val impureResult_1 = l.map( impureFunction ).map( impureFunction )
  val impureResult_2 = l.map( impureFunction andThen impureFunction )
  println(s"Res 1 =?= Res 2 : ${impureResult_1 == impureResult_2} = $impureResult_1 = $impureResult_2")


  val monadicFunction = (x: Int) => Reader[Int,Int](v => x * v)


  println("")
  println("Monadic function test")
  val monadicResult_1 = l.map( monadicFunction ).map( _.flatMap(monadicFunction) )
  val monadicResult_2 = l.map( monadicFunction andThen( _.flatMap(monadicFunction)) )
  val fileIterator2  = file.getLines()
  val effectfullResult_1 = monadicResult_1.map(_.run(fileIterator2.next().toInt))
  val effectfullResult_2 = monadicResult_2.map(_.run(fileIterator2.next().toInt))
  println(s"Res 1 =?= Res 2 : ${effectfullResult_1 == effectfullResult_2} = $effectfullResult_1 = $effectfullResult_2")




}
