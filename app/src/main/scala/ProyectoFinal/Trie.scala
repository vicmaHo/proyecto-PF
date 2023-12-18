package ProyectoFinal
import Oraculo._

abstract class Trie{
    
    def raiz( t : Trie ) : Char = {
        t match {
            case Nodo( c ,_ ,_ ) => c
            case Hoja ( c ,_ ) => c
        }
    } 
    
    
    def cabezas( t : Trie ) : Seq [Char ] = {
        t match {
            case Nodo( _, _, lt ) => lt.map( t=>raiz( t ) )
            case Hoja ( c ,_ ) => Seq [Char] ( c )
        }
    }
    def arbolDeSufijos(sufijos: Seq[String]): Trie = {
      sufijos.foldLeft(Nodo(' ', false, List.empty[Trie]): Trie) { (trie, sufijo) =>
        adicionar(trie, sufijo)
      }
    }
   def adicionar(t: Trie, sufijo: String): Trie = sufijo.toList match {
    case Nil => t
    case x :: xs =>
      val nuevoHijo = Nodo(x, true, List.empty[Trie])
      t match {
        case Hoja(c, marcada) =>
          Nodo(c, marcada, List(adicionar(nuevoHijo, xs.mkString)))
        case Nodo(c, marcada, hijos) =>
          val hijoExistente = hijos.find(h => raiz(h) == x)
          val nuevosHijos = hijoExistente match {
            case Some(h) =>
              hijos.updated(hijos.indexOf(h), adicionar(h, xs.mkString))
            case None =>
              hijos :+ adicionar(nuevoHijo, xs.mkString)
          }
          Nodo(c, marcada, nuevosHijos)
      }
  }

   def generarPosibilidades(t: Trie): Seq[String] = {
    def reconstruyendoPosibilidades(t: Trie, prefijo: String): Seq[String] = t match {
        case Hoja(_, _) => Seq(prefijo)
        case Nodo(_, _, hijos) =>
          if (hijos.isEmpty) {
            // Nodo sin hijos, devuelve solo el prefijo actual
            Seq(prefijo)
          } else {
            hijos.flatMap(h => reconstruyendoPosibilidades(h, prefijo + raiz(h)))
          }
      }

      reconstruyendoPosibilidades(t, "")
    }

def pertenece(s: String, t: Trie): Boolean = {
       t match {
          case Nodo( c , m , lt ) => {
              if ( s.isEmpty ) {
                  m
              } else {
                  val ( hijos , resto ) = lt.partition( t => raiz( t ) == s.head )
                  if ( hijos.isEmpty ) {
                      false
                  } else {
                      pertenece(  s.tail,hijos.head  )
                  }
              }
          }
          case Hoja ( c , m ) => {
              if ( s.isEmpty ) {
                  m
              } else {
                  false
              }
          }
      }
  }
}
case class Hoja ( car : Char , marcada : Boolean ) extends Trie
case class Nodo ( car :Char , marcada : Boolean , hijos : List [ Trie ] ) extends Trie

