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
    val n = 11
    val cadenaAleatoria = obj.crearADN(n)
    println("Cadena Aleatoria: " +cadenaAleatoria)
    println("Tamanio de la cadena: " +n)
    //println(math.pow(4, 500))
    //val prueba = "asda"
    //println(prueba.length)

    val oraculo = oraculoFunc(cadenaAleatoria)
    val timeF1 = withWarmer(new Warmer.Default) measure {
            obj.reconstruirCadenaIngenuo(cadenaAleatoria.length, oraculo)
    }
    val timeF2 = withWarmer(new Warmer.Default) measure {
            obj.reconstruirCadenaMejorado(cadenaAleatoria.length, oraculo)
    }
    val timeF3 = withWarmer(new Warmer.Default) measure {
        obj.reconstruirCadenaTurbo(cadenaAleatoria.length, oraculo)
    }

    val promedio = timeF1.value / timeF2.value
    val promedio2 = timeF2.value / timeF3.value
    
    
    //println(obj.reconstruirCadenaIngenuo(4, oraculo))
    //val combinaciones = obj.generarCombinaciones(100)
    //println(combinaciones)
    println("Respuesta de la reconstruccion ingenua: " +obj.reconstruirCadenaIngenuo(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion mejorada: " +obj.reconstruirCadenaMejorado(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion turbo: " + obj.reconstruirCadenaTurbo(cadenaAleatoria.length, oraculo))
    println()
    println("Tiempo de la solucion ingenua: " +timeF1.value)
    println("Tiempo de la solucion mejorada: " +timeF2.value)
    println("Es realmente mejor la merojada: " + promedio)
    println()
    println("Tiempo de la solucion turbo: " +timeF3.value)
    println("Es realmente mejor la turbo: " + promedio2)
  }
  
}
