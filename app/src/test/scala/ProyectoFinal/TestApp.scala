/**
 * Plantilla para pruebas
* @author Carlos Delgado
* @version 1.0
* @note 22 de Noviembre de 2023 
 */
package ProyectoFinal

import org.scalatest.funsuite.AnyFunSuite
import org.junit.runner.RunWith
import org.scalatestplus.junit.JUnitRunner
import ImplAlgoritmosParallel._
import ImplAlgoritmos._
import Oraculo._
import BenchMark._

@RunWith(classOf[JUnitRunner])
class TestApp extends AnyFunSuite{
    val cadena = "agga"
    val cadena2 = "aaaa"
    val oraculo = oraculoFunc(cadena)
    val oraculo2 = oraculoFunc(cadena2)
    test("Prueba #1 de resultados de las diferentes implementaciones para reconstruir las cadenas"){
        assert(reconstruirCadenaIngenuo(cadena.length, oraculo) == Seq('a', 'g', 'g', 'a'))
        assert(reconstruirCadenaIngenuoParallel(5)(cadena.length, oraculo) == Seq('a', 'g', 'g', 'a'))
        assert(reconstruirCadenaMejorado(cadena.length, oraculo) == Seq('a', 'g', 'g', 'a'))
        assert(reconstruirCadenaMejoradoParallel(5)(cadena.length, oraculo) == Seq('a', 'g', 'g', 'a'))
        assert(reconstruirCadenaTurbo(cadena.length, oraculo) == Seq('a', 'g', 'g', 'a'))
        assert(reconstruirCadenaTurboParallel(5)(cadena.length, oraculo) == Seq('a', 'g', 'g', 'a'))
        assert(reconstruirCadenaTurboMejorada(cadena.length, oraculo) == Seq('a', 'g', 'g', 'a'))
        assert(reconstruirCadenaTurboMejoradaParallel(5)(cadena.length, oraculo) == Seq('a', 'g', 'g', 'a'))
        assert(reconstruirCadenaTurboAcelerada(cadena.length, oraculo) == Seq('a', 'g', 'g', 'a'))
       assert(reconstruirCadenaTurboAceleradaParallel(5)(cadena.length, oraculo) == Seq('a', 'g', 'g', 'a'))
    }

    test("Prueba #2 de resultados de las diferentes implementaciones para reconstruir las cadenas"){
        assert(reconstruirCadenaIngenuo(cadena2.length, oraculo2) == Seq('a', 'a', 'a', 'a'))
        assert(reconstruirCadenaIngenuoParallel(5)(cadena2.length, oraculo2) == Seq('a', 'a', 'a', 'a'))
        assert(reconstruirCadenaMejorado(cadena2.length, oraculo2) == Seq('a', 'a', 'a', 'a'))
        assert(reconstruirCadenaMejoradoParallel(5)(cadena2.length, oraculo2) == Seq('a', 'a', 'a', 'a'))
        assert(reconstruirCadenaTurbo(cadena2.length, oraculo2) == Seq('a', 'a', 'a', 'a'))
        assert(reconstruirCadenaTurboParallel(5)(cadena2.length, oraculo2) == Seq('a', 'a', 'a', 'a'))
        assert(reconstruirCadenaTurboMejorada(cadena2.length, oraculo2) == Seq('a', 'a', 'a', 'a'))
        assert(reconstruirCadenaTurboMejoradaParallel(5)(cadena2.length, oraculo2) == Seq('a', 'a', 'a', 'a'))
        assert(reconstruirCadenaTurboAcelerada(cadena2.length, oraculo2) == Seq('a', 'a', 'a', 'a'))
        assert(reconstruirCadenaTurboAceleradaParallel(5)(cadena2.length, oraculo2) == Seq('a', 'a', 'a', 'a'))
    }

    test("Prueba de la reconstruccion de cadenas ingenua vs ingenua paralela") { 
        println("Comparación de algoritmos")
        val prueba = for {
            i <- 2 to 10
            cadena = crearADN(i)
            oraculo = oraculoFunc(cadena)
        } yield (compararAlgoritmos(reconstruirCadenaIngenuo,reconstruirCadenaIngenuoParallel(5))(i, oraculo),i)
        println(prueba)
    }

    test("Prueba de la reconstruccion de cadenas mejorada vs mejorada paralela") { 
        println("Comparación de algoritmos")
        val prueba = for {
            i <- 2 to 10
            cadena = crearADN(i)
            oraculo = oraculoFunc(cadena)
        } yield (compararAlgoritmos(reconstruirCadenaMejorado,reconstruirCadenaMejoradoParallel(5))(i, oraculo),i)
        println(prueba)
    }

    test("Prueba de la reconstruccion de cadenas turbo vs turbo paralela") { 
        println("Comparación de algoritmos")
        val prueba = for {
            i <- 2 to 10
            cadena = crearADN(i)
            oraculo = oraculoFunc(cadena)
        } yield (compararAlgoritmos(reconstruirCadenaTurbo,reconstruirCadenaTurboParallel(5))(i, oraculo), i)
        println(prueba)
    }

    test("Prueba de la reconstruccion de cadenas turbo mejorada vs turbo mejorada paralela") { 
        println("Comparación de algoritmos")
        val prueba = for {
            i <- 2 to 10
            cadena = crearADN(i)
            oraculo = oraculoFunc(cadena)
        } yield (compararAlgoritmos(reconstruirCadenaTurboMejorada,reconstruirCadenaTurboMejoradaParallel(5))(i, oraculo), i)
        println(prueba)
    }

    test("Prueba de la reconstruccion de cadenas turbo acelerada vs turbo acelerada paralela") { 
        println("Comparación de algoritmos")
        val prueba = for {
            i <- 2 to 10
            cadena = crearADN(i)
            oraculo = oraculoFunc(cadena)
        } yield (compararAlgoritmos(reconstruirCadenaTurboAcelerada,reconstruirCadenaTurboAceleradaParallel(5))(i, oraculo),i)
        println(prueba)
    }
    
    test("Prueba con todos los algoritmos con un tamaño especifico de cadena"){
        println("Desempeño de algoritmos con un tamaño especifico de cadena  ")
        for (i <- 2 to 10) {
            println(desempenoDeFunciones(i))
        }
    }

    test("Prueba de todos los algoritmos secuenciales"){
        println("Desempeño de algoritmos secuenciales")
        for(i <- 2  to 10){
            println(desempenoDeFuncionesSecuenciales(i))
        }
    }

    test("Prueba de todos los algoritmos paralelos"){
        println("Desempeño de algoritmos paralelos")
        for(i <- 2  to 10){
            println(desempenoDeFuncionesParalelas(i))
        }
    }
}
