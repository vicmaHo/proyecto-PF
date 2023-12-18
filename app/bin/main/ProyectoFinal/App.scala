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
import ProyectoFinal.taller4.BenchMark._

object App{

	def saludo() = "Proyecto Final"

	val alfabeto = Seq('a', 'c', 'g', 't')

	def main(args: Array[String]): Unit = {

		//Corriendo las pruebas
		for (i <- 2 to 11) {
            println(desempenoDeFuncionesParalelas(i))
        }
		//println(desempenoDeFunciones(10))

    }
}
