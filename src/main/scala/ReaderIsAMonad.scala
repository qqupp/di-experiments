/**
  * Created on 12/05/18.
  */
object ReaderIsAMonad {
  import Utils._

  case class Reader_[E, T](run: E => T)
  def unit[T]: T =>  Reader_[String, T] = (x: T) => Reader_(_ => x )
  def flatMap[E, T1, T2]: (Reader_[E,T1]) => (T1 => Reader_[E, T2]) => Reader_[E, T2] =
    (r) => (f) => Reader_(e => ( f ( r.run(e) ) ).run(e))


  def compose[E, A, B, C](f: A => Reader_[E, B], g: B => Reader_[E, C]): A => Reader_[E, C] =
    (x: A) => flatMap(f(x))(g)

  object Proof {
    // fix some types
    type E = Int
    type R[X] = Reader[X]
    type A = Int
    type B = Int
    type C = Int
    type D = Int
    type F = A => R[B]
    type G = B => R[C]

    case class Reader[X](run: E => X)

    val unit: A => R[A] =
      (x: A) => Reader[A](_ => x)

    val flatMap:(R[A]) => (A => R[B]) => R[B] =
      ((r: R[A]) => (f: F) => Reader[B]((e: E )=> ( f ( r.run(e) ) ).run(e)))

    val compose: (F, G) => ( A => R[A]) =
      (f: F, g: G) => (x: A) => flatMap(f(x))(g)


    def c(a: A, e: E): C = ???

    val h: G = (a: A) => Reader[C]((e_1: E) => c(a,e_1))

    /*
      In order to have a proof that reader is a monad we need to satisfy the monad laws.

      - left identity
          compose(unit, h)  =  h

      - right identity
          compose(unit, h)  =  h

      - associative property
          compose(compose(f1, g1), k1)  =  compose(f1, compose(g1, k1))
     */

    /*
      Left Identity
     */
    compose(unit, h) =?= h
    ((f: F, g: G) => (x: A) => flatMap (f(x))(g)) (unit, h)                                                      =?= h
    ((x: A) => flatMap (unit(x)) (h))                                                                            =?= h
    // subst unit
    ((x: A) => flatMap (((x1: A) => Reader[A](_ => x1))(x)) (h))                                                 =?= h
    ((x: A) => flatMap (Reader[A](_ => x)) (h))                                                                  =?= h
    // subst flatMap def
    ((x: A) => ((r: R[A]) => (f: F) => Reader(( e: E ) => ( f ( r.run(e) ) ).run(e))) (Reader[A](_ => x)) (h))   =?= h
    ((x: A) => ((f: F) => Reader((e: E ) => ( f ( (Reader[A](_ => x)).run(e) ) ).run(e))) (h))                   =?= h
    ((x: A) => Reader((e: E ) => ( (h) ( (Reader[A](_ => x)).run(e) ) ).run(e)) )                                =?= h
    ((x: A) => Reader((e: E ) => ( (h) (x) ).run(e)))                                                            =?= h
    // subst h definition
    ((x: A) => Reader((e: E ) => ( ((a: A) => Reader[C]((e_1: E) => c(a,e_1))) (x) ).run(e)))                    =?= h
    ((x: A) => Reader((e: E ) => Reader((e_1: E) => c(x,e_1)).run(e)))                                           =?= h
    ((x: A) => Reader((e: E ) => c(x, e)))                                                                       =?= h

    compose(unit, h) === ((x: A) => Reader[C]((e: E ) => c(x, e))) === h
    QED


    /*
      Right identity
     */
    compose(h, unit) =?= h
    ((f: G, g: F) => (x: A) => flatMap(f(x))(g)) (h, unit)                                                        =?= h
    ((x: A) => flatMap((h)(x))(unit))                                                                             =?= h

    // subst flatMap
    ((x: A) => ((r: R[A]) => (f: F) => Reader((e: E )=> ( f ( r.run(e) ) ).run(e))) (h(x)) (unit))                =?= h
    ((x: A) => ((f: F) => Reader((e: E )=> ( f ( (h(x)).run(e) ) ).run(e)))  (unit))                              =?= h
    // subst h
    ((x: A) => ((f: F) => Reader((e: E )=> ( f ( ( Reader[C]((e_1: E) => c(x, e_1))).run(e) ) ).run(e)))  (unit)) =?= h
    ((x: A) => ((f: F) => Reader((e: E )=> ( f ( c(x, e) ) ).run(e)))  (unit))                                    =?= h
    // subst f
    ((x: A) => (Reader((e: E )=> ( unit ( c(x, e) ) ).run(e))))                                                   =?= h
    // subst unit
    ((x: A) => (Reader((e: E )=> ( ((x: A) => Reader(_ => x)) ( c(x, e) ) ).run(e))))                             =?= h
    ((x: A) => (Reader((e: E )=> (Reader(_ => c(x, e))).run(e))))                                                 =?= h
    ((x: A) => Reader((e: E )=> c(x, e)))                                                                         =?= h

    compose(h, unit) === ((x: A) => Reader((e: E )=> c(x, e))) === h
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
    ((x1: A) => ((r: R[A]) => (f: F) => Reader((e: E )=> ( f ( r.run(e) ) ).run(e))) ( flatMap(f1(x1))(g1) ) (k1))                                       =?=  compose(f1, compose(g1, k1))
    ((x1: A) => ((f: F) => Reader((e: E )=> ( f ( ( flatMap(f1(x1))(g1) ).run(e) ) ).run(e)))  (k1))                                                     =?=  compose(f1, compose(g1, k1))
    ((x1: A) => Reader((e: E )=> ( (k1) ( ( flatMap(f1(x1))(g1) ).run(e) ) ).run(e))  )                                                                  =?=  compose(f1, compose(g1, k1))
    ((x1: A) => Reader((e1: E )=> ( (k1) ( ( flatMap(f1(x1))(g1)).run(e1) ) ).run(e1))  )                                                                =?=  compose(f1, compose(g1, k1))
    ((x1: A) => Reader((e1: E )=> ( (k1) ( (  ((r: R[A]) => (f: F) => Reader((e: E )=> ( f ( r.run(e) ) ).run(e))) (f1(x1)) (g1)).run(e1) ) ).run(e1)))  =?=  compose(f1, compose(g1, k1))
    ((x1: A) => Reader((e1: E )=> ( (k1) ( (  ( (f: F) => Reader((e: E )=> ( f ( (f1(x1)).run(e) ) ).run(e)))  (g1)).run(e1) ) ).run(e1))  )             =?=  compose(f1, compose(g1, k1))
    ((x1: A) => Reader((e1: E )=> ( (k1) ( Reader( (e: E ) => ( (g1) ( (f1(x1)).run(e) ) ).run(e))  .run(e1) ) ).run(e1))  )                             =?=  compose(f1, compose(g1, k1))
    ((x1: A) => Reader((e1: E )=> ( (k1) ( ( (e: E ) => ( (g1) ( (f1(x1)).run(e) ) ).run(e)) (e1) ) ).run(e1))  )                                        =?=  compose(f1, compose(g1, k1))
    ((x1: A) => Reader((e1: E )=> ( (k1) ( ( ( (g1) ( (f1(x1)).run(e1) ) ).run(e1))  ) ).run(e1))  )                                                     =?=  compose(f1, compose(g1, k1))

    val lhsProof: A => R[A] =
      (x1: A) => Reader{ (e1: E ) =>
        k1(
          g1(
            f1(
              x1
            ).run(e1)
          ).run(e1)
        ).run(e1)
      }


    compose(compose(f1, g1), k1) === lhsProof


    compose(f1, compose(g1, k1))                                                                         =?=  lhsProof
    ((f: FF, g: FF) => (x: A) => flatMap(f(x))(g)) (f1, compose(g1, k1))                                 =?=  lhsProof
    ( (x1: A) => ((flatMap (f1(x1))) (compose(g1, k1))) )                                                =?=  lhsProof
    ( (x1: A) => flatMap (f1(x1)) (((f: F, g: G) => (x: A) => flatMap(f(x))(g))(g1, k1)) )               =?=  lhsProof
    ( (x1: A) => flatMap (f1(x1)) ((x: A) => flatMap (g1(x)) (k1)) )                                     =?=  lhsProof
    ( (x1: A) => flatMap (f1(x1)) ((x: A) => ((r2: R[A]) => (f2: F) => Reader((e2: E )=> ( f2 ( r2.run(e2) ) ).run(e2))) (g1(x)) (k1)) )                                 =?=  lhsProof
    ( (x1: A) => flatMap (f1(x1)) ((x3: A) => Reader((e2: E )=> ( (k1) ( (g1(x3)).run(e2) ) ).run(e2))  ) )                                                              =?=  lhsProof
    ( (x1: A) => ((r: R[A]) => (f: F) => Reader((e: E )=> ( f ( r.run(e) ) ).run(e))) (f1(x1)) ((x3: A) => Reader((e2: E )=> ( (k1) ( (g1(x3)).run(e2) ) ).run(e2)) ))   =?=  lhsProof
    
    ((x1: A) => 
      ((r: R[A]) => (f: F) => Reader((e: E )=> ( f ( r.run(e) ) ).run(e)))  (f1(x1))  ((x3: A) => Reader((e2: E )=> ( (k1) ( (g1(x3)).run(e2) ) ).run(e2))  )
      )          =?=  lhsProof
    
    ((x1: A) =>
      Reader((e: E )=> ( ((x3: A) => Reader((e2: E )=> ( (k1) ( (g1(x3)).run(e2) ) ).run(e2))  ) ( (f1(x1)).run(e) ) ).run(e))
      )          =?=  lhsProof

    ((x1: A) =>
      Reader(
        (e: E ) =>
          (  (Reader((e2: E )=> ( (k1) ( (g1(( (f1(x1)).run(e) ))).run(e2) ) ).run(e2))  )  ).run(e)
      )
      )          =?=  lhsProof

    ((x1: A) =>
      Reader(
        (e: E ) =>
          Reader(
            (e2: E )=> ( (k1) ( (g1(( (f1(x1)).run(e) ))).run(e2) ) ).run(e2)).run(e)
      )
      )          =?=  lhsProof



    ((x1: A) =>
      Reader(
        (e: E ) =>
          Reader(
            (e2: E )=>
              k1( (g1(( (f1(x1)).run(e) ))).run(e2) ).run(e2))
            .run(e)
      )
      )          =?=  lhsProof


    ((x1: A) =>
      Reader(
        (e: E ) =>
              k1( (g1(( (f1(x1)).run(e) ))).run(e) ).run(e))

      ) =?=  lhsProof


    // Assoc

    {
      (x1: A) =>
        Reader{(e: E ) =>
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
        Reader{ (e1: E) =>
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