package ProyectoFinal
import ImplAlgoritmos._
import Oraculo._
import common.task
import scala.collection.parallel.CollectionConverters._

object ImplAlgoritmosParallel {
  

    def reconstruirCadenaIngenuoPar(umbral: Int)(n: Int, oraculo: Oraculo): Seq[Char] = {

        // por definir umbral en tamanos menores a 5-6
        
        val combinaciones = generarCombinaciones(n)
        val combinacionesFiltradas = combinaciones.par.filter(oraculo(_) == true)
        val combinacion = combinacionesFiltradas.head
        val cadenaEncontrada = combinacion.toSeq
        cadenaEncontrada

       
    }

}
