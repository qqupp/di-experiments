/**
  * Created on 12/05/18.
  */
object ReaderIsAMonad {
  import Utils._

  case class Reader[E, T](run: E => T)
  def unit[T]: T =>  Reader[String, T] = (x: T) => Reader(_ => x )
  def flatMap[E, T1, T2]: (Reader[E,T1]) => (T1 => Reader[E, T2]) => Reader[E, T2] =
    (r) => (f) => Reader(e => ( f ( r.run(e) ) ).run(e))

  def compose[E, A, B, C](f: A => Reader[E, B], g: B => Reader[E, C]): A => Reader[E, C] =
    (x: A) => flatMap(f(x))(g)

  object Proof {
    // now lets help the type inference making fix some types...
    type E = Int
    type R[X] = Reader[E, X]
    type A = Int
    type B = Int
    type C = Int
    type D = Int
    val c = 10
    type F = A => R[A]
    type G = A => R[A]

    // and chose a particular instance for the polimorphic functions
    val unit: A => R[A] =
      (x: A) => Reader[E, A](_ => x)

    val flatMap:(R[A]) => (A => R[B]) => R[B] =
      ((r: R[A]) => (f: F) => Reader[E, B]((e: E )=> ( f ( r.run(e) ) ).run(e)))

    val compose: (F, G) => ( A => R[A]) =
      (f: F, g: G) => (x: A) => flatMap(f(x))(g)

    // a monadic function
    val h: G = (a: A) => Reader[E, C]((e: E) => c)


    /*
      Left Identity
     */
    compose(unit, h)                                          =?= h
    ((f: F, g: G) => (x: A) => flatMap (f(x))(g)) (unit, h)   =?= h
    ((x: A) => flatMap (unit(x)) (h))                                                   =?= h
    // subst unit
    ((x: A) => flatMap (((x1: A) => Reader[E,A](_ => x1))(x)) (h))                      =?= h
    ((x: A) => flatMap (Reader[E,A](_ => x)) (h))                                       =?= h
    // subst flatMap def
    ((x: A) => ((r: R[A]) => (f: F) => Reader[E, A]((e: E ) => ( f ( r.run(e) ) ).run(e))) (Reader[E,A](_ => x)) (h))             =?= h
    ((x: A) => ((f: F) => Reader[E, A]((e: E ) => ( f ( (Reader[E,A](_ => x)).run(e) ) ).run(e))) (h))                            =?= h
    ((x: A) => Reader[E, A]((e: E ) => ( (h) ( (Reader[E,A](_ => x)).run(e) ) ).run(e)) )                                         =?= h
    ((x: A) => Reader[E, A]((e: E ) => ( (h) (x) ).run(e)))                                                                       =?= h
    // subst h definition
    ((x: A) => Reader[E, A]((e: E ) => ( ((A: A) => Reader((e: E) => c)) (x) ).run(e)))                                           =?= h
    ((x: A) => Reader[E, A]((e: E ) => ( Reader((e: E) => c) ).run(e)))                                                           =?= h
    ((x: A) => Reader[E, A]((e: E ) => c))                                                                                        =?= h

    compose(unit, h) === ((x: A) => Reader[E, A]((e: E ) => c)) === h
    QED


    /*
      Right identity
     */
    compose(h, unit) =?= h
    ((f: G, g: F) => (x: A) => flatMap(f(x))(g)) (h, unit)     =?= h
    ((x: A) => flatMap((h)(x))(unit))                          =?= h

    // subst flatMap
    ((x: A) => ((r: R[A]) => (f: F) => Reader[E, A]((e: E )=> ( f ( r.run(e) ) ).run(e))) (h(x)) (unit))                        =?= h
    ((x: A) => ((f: F) => Reader[E, A]((e: E )=> ( f ( (h(x)).run(e) ) ).run(e)))  (unit))                                      =?= h
    // subst h
    ((x: A) => ((f: F) => Reader[E, A]((e: E )=> ( f ( ( Reader[E, A]((e: E) => c)).run(e) ) ).run(e)))  (unit))                =?= h
    ((x: A) => ((f: F) => Reader[E, A]((e: E )=> ( f ( c ) ).run(e)))  (unit))                                                  =?= h
    // subst f
    ((x: A) => (Reader[E, A]((e: E )=> ( unit ( c ) ).run(e))))                                                                 =?= h
    // subst unit
    ((x: A) => (Reader[E, A]((e: E )=> ( ((x: A) => Reader[E, A](_ => x)) ( c ) ).run(e))))                                     =?= h
    ((x: A) => (Reader[E, A]((e: E )=> (Reader[E, A](_ => c)).run(e))))                                                         =?= h
    ((x: A) => Reader[E, A]((e: E )=> c))                                                                                       =?= h

    compose(h, unit) === ((x: A) => Reader[E, A]((e: E )=> c)) === h
    QED

    def f1: A => R[A] = (a1: A) => Reader((e: E) => a1 + 1)
    def g1: A => R[A] = (a2: A) => Reader((e: E) => a2 * 2)
    def k1: A => R[A] = (a3: A) => Reader((e: E) => a3 ^ 3)

    type FF = A => R[A]
    /*
      Associative
     */
    compose(compose(f1, g1), k1)   =?=  compose(f1, compose(g1, k1))
    ((f: FF, g: FF) => (x: A) => flatMap(f(x))(g)) (compose(f1, g1), k1)                                      =?=  compose(f1, compose(g1, k1))
    ((x: A) => flatMap((compose(f1, g1))(x))(k1))                                                             =?=  compose(f1, compose(g1, k1))
    ((x1: A) => flatMap((compose(f1, g1))(x1))(k1))                                                           =?=  compose(f1, compose(g1, k1))
    ((x1: A) => flatMap((((f: FF, g: FF) => (x: A) => flatMap(f(x))(g))(f1, g1))(x1))(k1))                    =?=  compose(f1, compose(g1, k1))
    ((x1: A) => flatMap( ((x: A) => flatMap(f1(x))(g1))(x1) )(k1))                                            =?=  compose(f1, compose(g1, k1))
    ((x1: A) => flatMap( flatMap(f1(x1))(g1) )(k1))                                                           =?=  compose(f1, compose(g1, k1))
    ((x1: A) => ((r: R[A]) => (f: F) => Reader[E, A]((e: E )=> ( f ( r.run(e) ) ).run(e))) ( flatMap(f1(x1))(g1) ) (k1))                                               =?=  compose(f1, compose(g1, k1))
    ((x1: A) => ((f: F) => Reader[E, A]((e: E )=> ( f ( ( flatMap(f1(x1))(g1) ).run(e) ) ).run(e)))  (k1))                                                             =?=  compose(f1, compose(g1, k1))
    ((x1: A) => Reader[E, A]((e: E )=> ( (k1) ( ( flatMap(f1(x1))(g1) ).run(e) ) ).run(e))  )                                                                          =?=  compose(f1, compose(g1, k1))
    ((x1: A) => Reader[E, A]((e1: E )=> ( (k1) ( ( flatMap(f1(x1))(g1)).run(e1) ) ).run(e1))  )                                                                        =?=  compose(f1, compose(g1, k1))
    ((x1: A) => Reader[E, A]((e1: E )=> ( (k1) ( (  ((r: R[A]) => (f: F) => Reader[E, A]((e: E )=> ( f ( r.run(e) ) ).run(e))) (f1(x1)) (g1)).run(e1) ) ).run(e1))  )  =?=  compose(f1, compose(g1, k1))
    ((x1: A) => Reader[E, A]((e1: E )=> ( (k1) ( (  ( (f: F) => Reader[E, A]((e: E )=> ( f ( (f1(x1)).run(e) ) ).run(e)))  (g1)).run(e1) ) ).run(e1))  )               =?=  compose(f1, compose(g1, k1))
    ((x1: A) => Reader[E, A]((e1: E )=> ( (k1) ( Reader[E, A]( (e: E ) => ( (g1) ( (f1(x1)).run(e) ) ).run(e))  .run(e1) ) ).run(e1))  )                               =?=  compose(f1, compose(g1, k1))
    ((x1: A) => Reader[E, A]((e1: E )=> ( (k1) ( ( (e: E ) => ( (g1) ( (f1(x1)).run(e) ) ).run(e)) (e1) ) ).run(e1))  )                                                =?=  compose(f1, compose(g1, k1))
    ((x1: A) => Reader[E, A]((e1: E )=> ( (k1) ( ( ( (g1) ( (f1(x1)).run(e1) ) ).run(e1))  ) ).run(e1))  )                                                             =?=  compose(f1, compose(g1, k1))

    val lhsProof: A => R[A] =
      (x1: A) => Reader[E, A]{ (e1: E ) =>
        k1(
          g1(
            f1(
              x1
            ).run(e1)
          ).run(e1)
        ).run(e1)
      }

    compose(compose(f1, g1), k1) === lhsProof

    compose(f1, compose(g1, k1))      =?=  lhsProof
    ((f: FF, g: FF) => (x: A) => flatMap(f(x))(g)) (f1, compose(g1, k1))                                 =?=  lhsProof
    ( (x1: A) => ((flatMap (f1(x1))) (compose(g1, k1))) )                                                =?=  lhsProof
    ( (x1: A) => flatMap (f1(x1)) (((f: F, g: G) => (x: A) => flatMap(f(x))(g))(g1, k1)) )               =?=  lhsProof
    ( (x1: A) => flatMap (f1(x1)) ((x: A) => flatMap (g1(x)) (k1)) )                                     =?=  lhsProof
    ( (x1: A) => flatMap (f1(x1)) ((x: A) => ((r2: R[A]) => (f2: F) => Reader[E, A]((e2: E )=> ( f2 ( r2.run(e2) ) ).run(e2))) (g1(x)) (k1)) )          =?=  lhsProof
    ( (x1: A) => flatMap (f1(x1)) ((x3: A) => Reader[E, A]((e2: E )=> ( (k1) ( (g1(x3)).run(e2) ) ).run(e2))  ) )                                       =?=  lhsProof
    ( (x1: A) => ((r: R[A]) => (f: F) => Reader[E, A]((e: E )=> ( f ( r.run(e) ) ).run(e))) (f1(x1)) ((x3: A) => Reader[E, A]((e2: E )=> ( (k1) ( (g1(x3)).run(e2) ) ).run(e2))  ) )          =?=  lhsProof
    
    ((x1: A) => 
      ((r: R[A]) => (f: F) => Reader[E, A]((e: E )=> ( f ( r.run(e) ) ).run(e)))  (f1(x1))  ((x3: A) => Reader[E, A]((e2: E )=> ( (k1) ( (g1(x3)).run(e2) ) ).run(e2))  )
      )          =?=  lhsProof
    
    ((x1: A) =>
      Reader[E, A]((e: E )=> ( ((x3: A) => Reader[E, A]((e2: E )=> ( (k1) ( (g1(x3)).run(e2) ) ).run(e2))  ) ( (f1(x1)).run(e) ) ).run(e))    
      )          =?=  lhsProof

    ((x1: A) =>
      Reader[E, A](
        (e: E ) =>
          (  (Reader[E, A]((e2: E )=> ( (k1) ( (g1(( (f1(x1)).run(e) ))).run(e2) ) ).run(e2))  )  ).run(e)
      )
      )          =?=  lhsProof

    ((x1: A) =>
      Reader[E, A](
        (e: E ) =>
          Reader[E, A](
            (e2: E )=> ( (k1) ( (g1(( (f1(x1)).run(e) ))).run(e2) ) ).run(e2)).run(e)
      )
      )          =?=  lhsProof



    ((x1: A) =>
      Reader[E, A](
        (e: E ) =>
          Reader[E, A](
            (e2: E )=>
              k1( (g1(( (f1(x1)).run(e) ))).run(e2) ).run(e2))
            .run(e)
      )
      )          =?=  lhsProof


    ((x1: A) =>
      Reader[E, A](
        (e: E ) =>
              k1( (g1(( (f1(x1)).run(e) ))).run(e) ).run(e))

      ) =?=  lhsProof


    // Assoc

    {
      (x1: A) =>
        Reader[E, A]{(e: E ) =>
          k1(
            g1(
              f1(
                x1
              ).run(e)
            ).run(e)
          ).run(e)
        }

    }  ===  lhsProof === compose(compose(f1, g1), k1) ===  compose(f1, compose(g1, k1)) === {

      (x1: A) =>
        Reader[E, A] { (e1: E) =>
          k1(
            g1(
              f1(
                x1
              ).run(e1)
            ).run(e1)
          ).run(e1)
        }
    }

    QED

  }
}

object Utils {
  object QED { val proof = "Quod Erat Demostrandum" }
  implicit class ProofEQUtil[V1,V2](val llhs: V1 => V2) extends  AnyVal {
    // are 2 functions the same functions?
    def =?=(rhs: V1 => V2): Boolean = false // false for simplicity, comparing 2 functions is semidecidable we dont' have enough time to check :/
    def ===(rhs: V1 => V2): V1 => V2 = rhs // equality correct by construction
  }
}