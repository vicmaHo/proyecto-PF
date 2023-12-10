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

object App{
    val obj = new ImplAlgoritmos()
    
    def main(args: Array[String]): Unit = {

    // Pruebas de la función oraculoFunc
    val oraculo = oraculoFunc("agga")
    //println(obj.reconstruirCadenaIngenuo(4, oraculo))
    //val combinaciones = obj.generarCombinaciones(4)

    println(obj.reconstruirCadenaIngenuo(4, oraculo))
  }
  
}
