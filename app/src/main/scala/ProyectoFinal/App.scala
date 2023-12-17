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

object App{

    def main(args: Array[String]): Unit = {

    // Pruebas de la función oraculoFunc
    val n = 9


    val cadenaAleatoria = crearADN(n)
    println("Cadena Aleatoria: " +cadenaAleatoria)
    println("Tamanio de la cadena: " +n)
  
    val oraculo = oraculoFunc(cadenaAleatoria)
    // val vacio = new Nodo(' ', false, List.empty[Trie])
    // val arbol = vacio.arbolDeSufijos(obj.generarCombinaciones(n))
    // println(arbol)
    
    val timeF1 = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaIngenuo(cadenaAleatoria.length, oraculo)
    }
    val timeF2 = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaMejorado(cadenaAleatoria.length, oraculo)
    }
    val timeF3 = withWarmer(new Warmer.Default) measure {
      reconstruirCadenaTurbo(cadenaAleatoria.length, oraculo)
    }
    val timeF4 = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboMejorada(cadenaAleatoria.length, oraculo)
    }
    val timeF5 = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboAcelerada(cadenaAleatoria.length, oraculo)
    }

    val timeF6 = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaIngenuoParallel(5)(cadenaAleatoria.length, oraculo)
    }

    val timeF7 = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaMejoradoParallel(5)(cadenaAleatoria.length, oraculo)
    }

    val timeF8 = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboParallel(5)(cadenaAleatoria.length, oraculo)
    }

    val timeF9 = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboMejoradaParallel(5)(cadenaAleatoria.length, oraculo)
    }

    val time10 = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaTurboAceleradaParallel(5)(cadenaAleatoria.length, oraculo)
    }
    val promedio = timeF1.value / timeF2.value
    val promedio2 = timeF2.value / timeF3.value
    val promedio3 = timeF3.value / timeF4.value
    val promedio4 = timeF4.value / timeF5.value
    
    
    println("Respuesta de la reconstruccion ingenua: " +reconstruirCadenaIngenuo(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion mejorada: " +reconstruirCadenaMejorado(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion turbo: " + reconstruirCadenaTurbo(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion turbo mejorada: " + reconstruirCadenaTurboMejorada(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion turbo acelerada: " + reconstruirCadenaTurboAcelerada(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion ingenua parallel: " + reconstruirCadenaIngenuoParallel(5)(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion mejorada parallel: " + reconstruirCadenaMejoradoParallel(5)(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion turbo parallel: " + reconstruirCadenaTurboParallel(5)(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion turbo mejorada parallel: " + reconstruirCadenaTurboMejoradaParallel(5)(cadenaAleatoria.length, oraculo))
    println("Respuesta de la reconstruccion turbo acelerada parallel: " + reconstruirCadenaTurboAceleradaParallel(5)(cadenaAleatoria.length, oraculo))

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

    println("Tiempo de la solucion ingenua parallel: " +timeF6.value)
    println("Tiempo de la solucion mejorada parallel: " +timeF7.value)
    println("Tiempo de la solucion turbo parallel: " +timeF8.value)
    println("Tiempo de la solucion turbo mejorada parallel: " +timeF9.value)
    println("Tiempo de la solucion turbo acelerada parallel: " +time10.value)
  }
  
}
