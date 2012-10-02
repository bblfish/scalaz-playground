/*
 *    Copyright 2012 Henry Story, http://bblfish.net/
 *
 *    Licensed under the Apache License, Version 2.0 (the "License");
 *    you may not use this file except in compliance with the License.
 *    You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 *    Unless required by applicable law or agreed to in writing, software
 *    distributed under the License is distributed on an "AS IS" BASIS,
 *    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *    See the License for the specific language governing permissions and
 *    limitations under the License.
 */

package play.req.v3

import language.higherKinds
import java.net.URI
import collection.mutable
import concurrent.{ExecutionContext, Future}
import ExecutionContext.Implicits.global
import scala.Some
import scalaz._
import Scalaz._

/**
 *
 * To make it easier to work out what is needed we start with a simplification of what a certicate is.
 * Usually the only fields we are interested in are the following.
 * We are of course interested in the signature of the CA cert during verification, and that is something
 * we can try to model later
 * @param cn the common name part of the Distinguished Name (an LDAP construct)
 * @param pubKey we imagine that a public key is only one number - it is two for RSA
 * @param subjectAltNames a list of URI alternative names.
 */

case class Cert(cn: String,
                pubKey: PubKey,
                subjectAltNames: List[URI] )


/**
 * We imagine a document class, that is super restricted at this point
 * (One would have to generalise it to allow it to describe any object)
 */
case class Doc(primaryTopic: URI, key: PubKey)

trait PubKey
case class RSA(mod: BigInt, exp: BigInt) extends PubKey

/**
 * The web is just a place to find documents. As we want to demonstrate behavior in different
 * possible worlds we give the Web a name for each world.
 */
case class Web(world: String) {
  val resources = mutable.HashMap[URI,Doc]()

  // fetching things on the web takes time and can fail, so we return a Future Validation
  def get(url: URI): Future[Validation[Exception,Doc]] =
    resources.get(url) match {
      case Some(doc) => Future(Success(doc))
      case None => Future(Failure(new Exception("resource "+url+" not found")))
    }
}

/**
 * We enlarge on Java's notion of a Principal by typing it, so that we
 * can distinguish principals of keys, principals of URIs and principals of Distinguished Names (DN)
 * the type will then allow us to know the relation between a principal and the referred to item
 *
 * eg:
 * <http://bblfish.net/#hjs> a foaf:Person;
 *        foaf:name "Henry Story";
 *        cert:key 0987654321 ...
 *  shoud it be +T?
 */
case class Principal[+T](id: T) extends java.security.Principal {
  def getName = id.toString
}

/**
 * This would be part of an server side Play 2.0 Http request header interface, that would allow
 * one to fetch the client certificates.
 */
trait RequestHeader {

  type Certs = Array[Cert]  //in java cert chains are sent around in an Array

  /**
   * We return a Future that is either a Certificate verified by a CA (right)
   * or a claim of a Certificate ( when it has not been verified by a CA )
   * The Claim is useful if one wants to for example
   *   1. create a service that shows the user the certificate received
   *   2. use the public key as an identifier ( no need for a CA )
   *   3. use the cert for WebID ( http://webid.info/ ) authentication
   * @return
   */
  def cert: Future[\/[Claim[Certs],Certs]]

}

trait Claim[+S] {
  protected val statements: S

  def verify[V](implicit fn: S=> V ): V
}

object Claim {
  implicit val ClaimMonad: Monad[Claim] with Traverse[Claim] =
    new Monad[Claim] with Traverse[Claim] {

      def traverseImpl[G[_] : Applicative, A, B](fa: Claim[A])(f: A => G[B]): G[Claim[B]] =
        f(fa.statements).map(a => this.point(a))

      def point[A](a: => A) = new Claim[A]{
        protected val statements : A = a;
        def verify[V](implicit fn: A=> V ) = fn(statements)
      }

      def bind[A, B](fa: Claim[A])(f: (A) => Claim[B]) = f(fa.statements)
    }
}

object Verisign extends Cert("Verisign",RSA(314983423,65),List())

object TestDynamic {
  import Claim._
  import ExecutionContext.Implicits.global


  //First president Obama's certificate is signed by Verisign
  //note: that is already pretty unlikely. The white house cannot be dependent for the security
  //   of the president on a private company.  It would be more likely that the key is in DNSSEC,
  //   and that one should use DANE http://tools.ietf.org/html/rfc6698
  val caSignedCertChain = Array(
    Cert("Obama",RSA(139248359,65),List(new URI("https://whitehouse.gov/2008/president#obama"))),
    Verisign
  )

  //my cert, I don't add the CA to the chain, cause nobody knows my server :'-(
  val myCertChain = Array(Cert("Henry",RSA(98765432,65),List(new URI("http://bblfish.net/#hjs"))))

  // A request received when Barack Obama connects
  val obamaRequest = new RequestHeader {
    def cert = Future(caSignedCertChain.right)
  }

  // A request received when I connect
  val myRequest = new RequestHeader {
    def cert = Future(ClaimMonad.point(myCertChain).left)
  }

  /**
   * we extract the principals from a request header as a pair of List:
   * @param header the request
   * @return  A Future (for getting the cert) that contains a Pair of Lists of principals. The pair consists of
   *          1. Principals that take time to be verified (WebID)
   *          2. Principals that are already verified in the reception of the certificate (eg. if the cert was
   *          signed then everything is already verified, or the public key which is already verified
   *  Here we always put the WebIDs in the list that take time to be verified.
   *  We could also assume them to be verified.
   */
  def extractPrincipals(header: RequestHeader)(implicit web: Web): Future[(List[Future[Validation[Exception,Principal[Any]]]],List[Principal[Any]])] = {
    val re = for (certs <- header.cert) yield {
      certs.fold(
        claim => Pair(claim.verify(webIDverify),
                      List[Principal[Any]](claim.verify(pubkeyVerify))),
        chain => {
          val cert = chain(0) // can a chain be empty? (as an array it can, but should it perhaps be non null list? )
          Pair(webIDverify(chain),
            Principal(cert.pubKey) :: Principal(cert.cn) :: Nil)
        }
      )
    }
    re
  }

  /**
   *  verification of Subject Alternative Names (SAN) in an x509 certificate
   *  @return a list of Future principals (verifying takes time)
   */
//
// It is easier to work out how things work with map and flatmap. This does not work - see below
//
//  def webIDverify(x509: Array[Cert]): List[Future[Validation[Exception,Principal[Any]]]] =
//    for {
//      san <- x509.head.subjectAltNames  //should probably not assume List has even 1 element
//      docValidation <- Web.get(san)
//      doc <- docValidation
//    } yield {
//      if (doc.primaryTopic == san && doc.key == x509.head.pubKey) {
//         Success(Principal(san))
//      } else Failure(new Exception("could not verify identity of "+san))
//    }

  def webIDverify(x509: Array[Cert])(implicit web: Web): List[Future[Validation[Exception,Principal[Any]]]] =
    x509.head.subjectAltNames.map { san =>
       val profile = new URI(san.toString.split("#")(0))
       web.get(profile).map { docValidation =>
         docValidation.flatMap { doc =>
           if (doc.primaryTopic == san && doc.key == x509.head.pubKey) {
             Success(Principal(san))
           } else new Exception("public key " + x509.head.pubKey +
             " did not match any in doc for "+san).failure[Principal[Any]]
         }
       }
    }

  // a public key in a cert is already verified
  def pubkeyVerify(x509: Array[Cert]): Principal[PubKey] = Principal(x509(0).pubKey)

  //what should one have if someone connects but we does not send a certificate?
  //a failed promise?

  def main(args: Array[String]) {


    // this is happening asynchronously so we must print all info on one line
    def printPrincipal(name: String,
                       futurePrincipals: Future[(List[Future[Validation[Exception,Principal[Any]]]],List[Principal[Any]])] )
                       (implicit web: Web)
    {
      val id = name + " in "+ web.world
      futurePrincipals.onComplete { received =>
        if (received.isSuccess) {
          val p = received.get
          println(id+" non web verified "+p._2)
          for (futurePrincipalVal <- p._1) {
            futurePrincipalVal.onComplete { valReceived =>
              if (valReceived.isSuccess) {
                val principalVal = valReceived.get
                println(id+" web verified "+principalVal)
              } else {
                println(id+" future failed "+valReceived.failed.get)
              }
            }
          }
        } else {
          println(id+" did not receive a certificate")
        }
      }
    }

    // we run our tests in two parallele universes ( one with the web in state web1 the other with the
    // web in state web2

    {
      implicit val web1 = Web("Web1")
      // we fill the web up with information (ok only a few minimal documents...)

      web1.resources.put(new URI("https://whitehouse.gov/2008/president"),
        Doc(new URI("https://whitehouse.gov/2008/president#obama"), RSA(139248359,65)))

      //Here I don't have enough money to get a Verisign signed certificate for myself, so I have my
      //server sign the certificate. That key could be in DNSsec too, follwing the DANE protocol
      //http://tools.ietf.org/html/rfc6698 - in which case the WebID lookup would still be useful
      //to get further information about a user, and to verifiy the validity
      web1.resources.put(new URI("http://bblfish.net/"), Doc(new URI("http://bblfish.net/#hjs"), RSA(98765432,65)))

      println("1. Obama has a cert signed by a well trusted CA. " + caSignedCertChain(0))
      println("2. Henry has a cert that is not verified by a known CA. "+myCertChain)
      println()
      println()
      printPrincipal("Obama", extractPrincipals(obamaRequest))
      printPrincipal("Henry", extractPrincipals(myRequest))
    }

    {
      implicit val web2 = Web("Web2")
      //Here I lost my certificate, and made a new one. What happens if someone uses the old one? Wait and see...
      web2.resources.put(new URI("http://bblfish.net/"), Doc(new URI("http://bblfish.net/#hjs"), RSA(131313131,65)))
      //obama here does not have a WebID profile document.

      printPrincipal("Obama", extractPrincipals(obamaRequest))
      printPrincipal("Henry", extractPrincipals(myRequest))
    }
  }
}