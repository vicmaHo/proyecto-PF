/**
  * Taller 3 - Programación Funcional
  * Autores: <Estudiantes>
  * Profesor: Carlos A Delgado
  */
package ProyectoFinal

import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import Oraculo._
import ImplAlgoritmos._
import ImplAlgoritmosParallel._
//import BenchMark._
import ProyectoFinal.taller4.BenchMark

object App{

    
    def main(args: Array[String]): Unit = {

    
      

    val n = 7

    //val cadenaAleatoria = crearADN(n)
    // val combin = generarCombinaciones(n)
    // val (bloqu1, bloqu2) = separarCombinacion(combin)
    // println("bloque1: " + bloqu1)
    // println("bloque2: " + bloqu2)
   //val oraculo = oraculoFunc(cadenaAleatoria)
    // println("Tamaño de la cadena: " + n)
  // println("Cadena Aleatoria: " + cadenaAleatoria)
    //println("Resultados para mejorada paralela: "+ reconstruirCadenaMejoradoParallel(5)(n,oraculo) )

    for (i <- 2 to 11) {
        println(BenchMark.desempenoDeFunciones(i))
     }
     }
  
}
