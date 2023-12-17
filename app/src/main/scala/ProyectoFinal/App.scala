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

    val n = 6
    val cadenaAleatoria = crearADN(n)
    println("Tamaño de la cadena: " + n)
    println("Cadena Aleatoria: " + cadenaAleatoria)
    println(BenchMark.desempenoDeFunciones(n))
  }
  
}
