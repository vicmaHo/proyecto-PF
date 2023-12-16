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
    val n = 9


    val cadenaAleatoria = obj.crearADN(n)
    println("Cadena Aleatoria: " +cadenaAleatoria)
    println("Tamanio de la cadena: " +n)
  
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
    val timeF4 = withWarmer(new Warmer.Default) measure {
        obj.reconstruirCadenaTurboMejorada(cadenaAleatoria.length, oraculo)
    }
    val timeF5 = withWarmer(new Warmer.Default) measure {
        obj.reconstruirCadenaTurboAcelerada(cadenaAleatoria.length, oraculo)
    }

    val promedio = timeF1.value / timeF2.value
    val promedio2 = timeF2.value / timeF3.value
    val promedio3 = timeF3.value / timeF4.value
    val promedio4 = timeF4.value / timeF5.value
    
    
    println("Respuesta de la reconstruccion ingenua: " +obj.reconstruirCadenaIngenuo(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion mejorada: " +obj.reconstruirCadenaMejorado(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion turbo: " + obj.reconstruirCadenaTurbo(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion turbo mejorada: " + obj.reconstruirCadenaTurboMejorada(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion turbo acelerada: " + obj.reconstruirCadenaTurboAcelerada(cadenaAleatoria.length, oraculo))
    println()
    println("Tiempo de la solucion ingenua: " +timeF1.value)
    println("Tiempo de la solucion mejorada: " +timeF2.value)
    println("Es realmente mejor la merojada: " + promedio)
    println()
    println("Tiempo de la solucion turbo: " +timeF3.value)
    println("Es realmente mejor la turbo: " + promedio2)
    println()
    println("Tiempo de la solucion turbo mejorada: " +timeF4.value)
    println("Es realmente mejor la turbo merojada: " + promedio3)
    println()
    println("Tiempo de la solucion turbo acelerada: " +timeF5.value)
    println("Es realmente mejor la turbo acelerada: " + promedio4)
  }
  
}
