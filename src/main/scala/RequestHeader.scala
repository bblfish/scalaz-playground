import collection.mutable
import java.net.URI
import scalaz.concurrent.Promise
import scalaz._
import Scalaz._
import language.implicitConversions
import language.higherKinds

/**
 * This is work emerging from a thread on the scalaz mailing list
 * https://groups.google.com/d/topic/scalaz/0v0mv_2-WDw/discussion
 *
 * To make it easier to work out what is needed we start with a simplification of what a certicate is.
 * Usually the only fields we are interested in are the following.
 * We are of course interested in the signature of the CA cert during verification, and that is something
 * we can try to model later
 * @param cn the common name part of the Distinguished Name (an LDAP construct)
 * @param pubKey we imagine that a public key is only one number - it is two for RSA
 * @param subjectAltNames a list of URI alternative names.
 */

case class Cert(cn: String, pubKey: BigInt, subjectAltNames: List[URI] )

/**
 * We imagine a document class, that is super restricted at this point
 * (One would have to generalise it to allow it to describe any object)
 */
case class Doc(primaryTopic: URI, key: BigInt)

/**
 * The web is just a place to find documents
 */
object Web extends mutable.HashMap[URI,Doc]

/**
 * This would be part of an server side Play 2.0 Http request header interface, that would allow
 * one to fetch the client certificates.
 */
trait RequestHeader {
  type Certificates = Cert

  def cert: Promise[Claim[_,Certificates]]

}

sealed trait Level
object Truth extends Level
object PubKeyVerified extends Level


/**
 * A claim is a monad (see object Claim) that has a Level of verification and is about some
 * set of statements S. (we think of objects as serialised statements)
 * @tparam L
 * @tparam S
 */
trait Claim[L <: Level, S] {
  protected val statements: S

  val level: L

  def verify[L2 <: Level, S2](implicit verify: Verificator[L, S, L2, S2]): Claim[L2, S2] = {
    verify(this)
  }

  /**
   * we can only extract the statement if it is true
   * (Thanks to Lars Hupel for this idea)
   * @param ev
   * @return
   */
  def extract(implicit ev: L =:= Truth.type) = statements

}

trait Verificator[L<: Level,S,L2<:Level,S2] {
  def apply(s: Claim[L,S]): Claim[L2,S2]
}

//extends java's Principal idea, with typed principals
case class Principal[T](id: T) extends java.security.Principal{
  def getName = id.toString
}

object Claim {

  implicit val level: Truth.type = Truth
  implicit val pkLevel: PubKeyVerified.type = PubKeyVerified

  implicit class iri(u: URI) {
    def doc = new URI(u.toString.split("#")(0))
  }

  /**
   * this seems to be missing from the scalaz Unapply
   * thanks to Lars Hupel for this
   *
   * @param TC0
   * @tparam TC
   * @tparam A0
   * @tparam B0
   * @return
   */
  implicit def unapplyClaim[TC[_[_]], A0 <: Level, B0](implicit TC0: TC[({type λ[α] = Claim[A0, α]})#λ]): Unapply[TC, Claim[A0, B0]] {
    type M[X] = Claim[A0, X]
    type A = B0
  } = new Unapply[TC, Claim[A0, B0]] {
    type M[X] = Claim[A0, X]
    type A = B0
    def TC = TC0
    def apply(ma: Claim[A0, B0]) = ma
  }

// we would like to have a verificator that can move something from one type of Monad (say a non verified one)
// to another type of monad (a verified one). With a pubkey verification we can only extract the public key as a principal
// But the problem here is that the map and flatMap method of monads must map to exactly the same type of monads, so that
// we can't change the level
/*
  implicit val pkToTruth = new Verificator[PubKeyVerified.type,Cert,Truth.type,Principal[BigInt]] {
    def apply(s: Claim[PubKeyVerified.type, Cert]) = s.flatMap(c => ClaimMonad(Truth).point(Principal(c.pubKey)))
  }
*/
/*
    implicit val webIDVerif = new Verificator[PubKeyVerified.type,Cert,Truth.type,List[Principal[URI]]] {
      def apply(s: Claim[PubKeyVerified.type, Cert]) = s.flatMap(c => ClaimMonad(Truth).point(webidVerif(c.subjectAltNames, c.pubKey)))
      def webidVerif(sans: List[URI], key: BigInt ) = {
        for (san <- sans;
             doc <- Web.get(san.doc)
             if (doc.primaryTopic == san && key == doc.key)
        ) yield Principal(san)
      }
    }

*/

  implicit def ClaimMonad[L <: Level](implicit lvl: L): Monad[({type f[+a] = Claim[L, a]})#f] =
    new Monad[({type f[+a] = Claim[L, a]})#f] {

      def point[A](a: => A) = new Claim[L,A] {
        protected val statements : A = a

        val level: L = lvl
      }

      def bind[A, B](fa: Claim[L,A])(f: A => Claim[L,B]) = f(fa.statements)
    }

}

object Test {

    import Claim._
    import System.out._

    implicitly[Truth.type]
    implicitly[PubKeyVerified.type]


    Web.put(new URI("http://bblfish.net/"),Doc(new URI("http://bblfish.net/#hjs"),BigInt("9876543210")))

    //for a certificate whose signature was signed by a trusted Certificate Authority - We can then accept
    //everything in the Cert as true.
    val truthMonad = ClaimMonad(Truth)
    val tmCert = truthMonad.point(Cert("Henry Story",BigInt(123456789),List(new java.net.URI("http://bblfish.net/#hjs"))))
    val cn = tmCert.map(_.cn)
    println(cn.extract) //we can access the common name

    //next an example with a certificate who signature we don't trust or don't know about - we only know that the
    //public key was correct
//    val pkVerifiedMonad = ClaimMonad(PubKeyVerified)
//    val pkCert = pkVerifiedMonad.point(Cert("Henry Story",BigInt("9876543210"),List(new java.net.URI("http://bblfish.net/#hjs"))))
//    val pk = pkCert.verify(pkToTruth).extract
}