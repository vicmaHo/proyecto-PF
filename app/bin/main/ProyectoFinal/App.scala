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
import scala.util.Random
import ImplAlgoritmosParallel._

object App{

	def saludo() = "Proyecto Final"

	val alfabeto = Seq('a', 'c', 'g', 't')

	def main(args: Array[String]): Unit = {
		// Pruebas de la función oraculoFunc
    val n = 8
    val cadenaAleatoria = crearADN(n)
    println("Cadena Aleatoria: " +cadenaAleatoria)
    println("Tamanio de la cadena: " +n)

    

    val oraculo = oraculoFunc(cadenaAleatoria)

    val timeF1 = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaIngenuo(cadenaAleatoria.length, oraculo)
    }

    val timeF2 = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaIngenuoPar(10)(cadenaAleatoria.length, oraculo)

    }

    reconstruirCadenaTurbo(cadenaAleatoria.length, oraculo)


    // val promedio = timeF1.value / timeF2.value
    // val promedio2 = timeF2.value / timeF3.value
    
    // println("resultado de la reconstruccion ingenua: " + (Resultados = timeF1).r)
    println("tiempo ingenua: " + timeF1.value)
    // println("resulado de la reconstruccoin ingenua paralela: " + reconstruirCadenaIngenuoPar(10)(cadenaAleatoria.length, oraculo))
    println("tiempo ingenua paralela: " + timeF2.value)
	}
 }
