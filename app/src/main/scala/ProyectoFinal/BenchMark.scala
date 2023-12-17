package ProyectoFinal

package taller4
import org.scalameter.measure
import org.scalameter.withWarmer
import org.scalameter.Warmer
import org.scalameter.Quantity
import Oraculo._
import  ImplAlgoritmos._
import ImplAlgoritmosParallel._

class Benchmark {

    def compararAlgoritmos(Funcion1:(Int,Oraculo) => Seq[Char], Funcion2:(Int,Oraculo) => Seq[Char])(n: Int,oraculo: Oraculo): (Double, Double, Double) = {
        val timeF1 = withWarmer(new Warmer.Default) measure {
            Funcion1(n, oraculo)
        }
        val timeF2 = withWarmer(new Warmer.Default) measure {
            Funcion2(n,oraculo)
        }

        val promedio = timeF1.value / timeF2.value
        (timeF1.value, timeF2.value, promedio)

    }

    def desempenoDeFunciones(tamanoCadena: Int): Vector[Double] = {
    println("Tamanio: " + tamanoCadena)
    val cadenaAleatoria = crearADN(tamanoCadena)
    val oraculo = oraculoFunc(cadenaAleatoria)
    
    val tiemposIngenua = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaIngenuo(cadenaAleatoria.length, oraculo)
    }
      tiemposIngenua(i) = time.value
    }

    val tiemposIngenuaPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaIngenuoParallel(5)(cadenaAleatoria.length, oraculo)
      }
      tiemposIngenuaPar(i) = time.value
    }

    val tiempoMejorado = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaMejorado(cadenaAleatoria.length, oraculo)
        }
        tiempoMejorado(i) = time.value
    }

    val tiempoMejoradoPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaMejoradoParallel(5)(cadenaAleatoria.length, oraculo)
        }
        tiempoMejoradoPar(i) = time.value
    }

    val tiempoTurbo = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaTurbo(cadenaAleatoria.length, oraculo)
        }
        tiempoTurbo(i) = time.value
    }

    val tiempoTurboPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaTurboParallel(5)(cadenaAleatoria.length, oraculo)
        }
        tiempoTurboPar(i) = time.value
    }
    
    val tiempoTurboMejorada = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaTurboMejorada(cadenaAleatoria.length, oraculo)
        }
        tiempoTurboMejorada(i) = time.value
    }

    val tiempoTurboMejoradaPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaTurboMejoradaParallel(5)(cadenaAleatoria.length, oraculo)
        }
        tiempoTurboMejoradaPar(i) = time.value
    }

    val tiempoTurboAcelerada = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaTurboAcelerada(cadenaAleatoria.length, oraculo)
        }
        tiempoTurboAcelerada(i) = time.value
    }
    val tiempoTurboAceleradaPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaTurboAceleradaParallel(5)(cadenaAleatoria.length, oraculo)
        }
        tiempoTurboAceleradaPar(i) = time.value
    }
    Vector(tiemposIngenua.sum / 100, tiemposIngenuaPar.sum / 100, tiempoMejorado.sum / 100,
     tiempoMejoradoPar.sum / 100, tiempoTurbo.sum / 100, tiempoTurboPar.sum / 100,
        tiempoTurboMejorada.sum / 100, tiempoTurboMejoradaPar.sum / 100,
     tiempoTurboAcelerada.sum / 100, tiempoTurboAceleradaPar.sum / 100)
  }

  def desempenoDeFuncionesSecuenciales(tamanoCadena: Int): Vector[Double] = {
    println("Tamanio: " + tamanoCadena)
    
    val cadenaAleatoria = crearADN(tamanoCadena)
    val oraculo = oraculoFunc(cadenaAleatoria)

    val tiempoIngenua = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaIngenuo(cadenaAleatoria.length, oraculo)
      }
      tiempoIngenua(i) = time.value
    }

    val tiempoMejorado = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaMejorado(cadenaAleatoria.length, oraculo)
        }
        tiempoMejorado(i) = time.value
    }

    val tiempoTurbo = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaTurbo(cadenaAleatoria.length, oraculo)
        }
        tiempoTurbo(i) = time.value
    }
    val tiempoTurboMejorada = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaTurboMejorada(cadenaAleatoria.length, oraculo)
        }
        tiempoTurboMejorada(i) = time.value
    }
    val tiempoTurboAcelerada = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaTurboAcelerada(cadenaAleatoria.length, oraculo)
        }
        tiempoTurboAcelerada(i) = time.value
    }

    Vector(tiempoIngenua.sum / 100,  tiempoMejorado.sum / 100, tiempoTurbo.sum / 100,
     tiempoTurboMejorada.sum / 100, tiempoTurboAcelerada.sum / 100)
  }

  def desempenoDeFuncionesParalelas(tamanoCadena: Int): Vector[Double] = {
    println("Tamanio: " + tamanoCadena)
        val cadenaAleatoria = crearADN(tamanoCadena)
    val oraculo = oraculoFunc(cadenaAleatoria)

    val tiempoIngenuaPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
      val time = withWarmer(new Warmer.Default) measure {
        reconstruirCadenaIngenuoParallel(5)(cadenaAleatoria.length, oraculo)
      }
      tiempoIngenuaPar(i) = time.value
    }

    val tiempoMejoradoPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaMejoradoParallel(5)(cadenaAleatoria.length, oraculo)
        }
        tiempoMejoradoPar(i) = time.value
    }

    val tiempoTurboPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaTurboParallel(5)(cadenaAleatoria.length, oraculo)
        }
        tiempoTurboPar(i) = time.value
    }
    val tiempoTurboMejoradaPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaTurboMejoradaParallel(5)(cadenaAleatoria.length, oraculo)
        }
        tiempoTurboMejoradaPar(i) = time.value
    }
    val tiempoTurboAceleradaPar = (1 to 100).map(_ => 0.0).toArray
    for (i <- 0 until 100) {
        val time = withWarmer(new Warmer.Default) measure {
            reconstruirCadenaTurboAceleradaParallel(5)(cadenaAleatoria.length, oraculo)
        }
        tiempoTurboAceleradaPar(i) = time.value
    }

    Vector(tiempoIngenuaPar.sum / 100,  tiempoMejoradoPar.sum / 100, tiempoTurboPar.sum / 100,
     tiempoTurboMejoradaPar.sum / 100, tiempoTurboAceleradaPar.sum / 100)
  }


}