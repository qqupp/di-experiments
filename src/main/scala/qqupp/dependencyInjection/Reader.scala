package qqupp.dependencyInjection

import HelloTypes._
import cats._
import cats.implicits._

/**
  * Created on 29/04/18.
  */

object HelloPartialApplication000 {

  // an example with partial application
  def ping(hostname: Host)(gateway: Gateway) = s"connecting to $gateway and ping $hostname"

  val myProgram = ping("www.sky.com")(_)

  myProgram("testing gateway 127.0.0.1")
  // connecting to testing gateway 127.0.0.1 and ping www.sky.com

  myProgram("production bastion 10.0.0.2")
  // connecting to production bastion 10.0.0.2 and ping www.sky.com
}

object HelloReaderMonad101 {

  // the reader it's very similar to a container for
  // a function to apply a configuration and return an arbitrary value of type T
  case class Reader[C, T](run: C => T)

  // an example almost like partial application
  def ping(hostname: Host) = Reader[Gateway, String] {
    (g: Gateway) => s"connecting to $g and ping $hostname"
  }

  // you can construct your program
  val myProgram = ping("www.sky.com")


  // and run it with different configurations

  myProgram.run("testing gateway 127.0.0.1")
  // connecting to testing gateway 127.0.0.1 and ping www.sky.com

  myProgram.run("production bastion 10.0.0.2")
  // connecting to production bastion 10.0.0.2 and ping www.sky.com


}

object HelloReaderMonad102 {

  // at this point we just need to define the way
  // we compose Readers and transform
  case class Reader[C, T](run: C => T) {
    def map[T2](f: T => T2): Reader[C, T2] =
      Reader((e: C) => f(run(e)))

    def flatMap[T2](f: T => Reader[C, T2]) =
      Reader((e: C) => f(run(e)).run(e))
  }

  def ping(hostname: Host) = Reader[Gateway, String] {
    (g: Gateway) => s"connecting to $g and ping $hostname"
  }

  def grantAccess(g: Gateway, pswd: String) = if (g.startsWith("testing")) true else false

  def checkSecurity(password: String) = Reader[Gateway, Boolean] {
    (g: Gateway) => grantAccess(g, password)
  }

  // you can compose your program
  val myProgram = for {
    allowed      <-  checkSecurity("password_1234")
    ping_result  <-  if (allowed) ping("www.sky.com") else ping("localhost")
  } yield ping_result


  myProgram.run("testing gateway 127.0.0.1")
  // connecting to testing gateway 127.0.0.1 and ping www.sky.com


  myProgram.run("production bastion 10.0.0.2")
  // connecting to production bastion 10.0.0.2 and ping localhost

}


object HelloMonad103 {

  def functionComposition[A, B, C](f: A => B, g: B => C): A => C =
    (x: A) => g(f(x))

  def monadComposition[M[_] : Monad, A, B, C](f: A => M[B], g: B => M[C]): A => M[C] =
    (x: A) => f(x).flatMap(g)

}


object HelloTypes {
  type Gateway = String
  type Host = String

}



