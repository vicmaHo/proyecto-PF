package ProyectoFinal
import Oraculo._

class ImplAlgoritmos {
    val alfabeto =Seq('a','c','g','t')

    def generarCombinaciones(tamano: Int): Seq[String] = {
		if (tamano == 0) {
		Seq("")
		} else {
		for {
			caracter <- alfabeto
			combinacion <- generarCombinaciones(tamano - 1)
		} yield caracter + combinacion
		}
  	}
    
    def reconstruirCadenaIngenuo(n: Int, oraculo: Oraculo): Seq[Char] = {
        val combinaciones = generarCombinaciones(n)
        val combinacionesFiltradas = combinaciones.filter(oraculo(_) == true)
        val combinacion = combinacionesFiltradas.head
        val cadenaEncontrada = combinacion.toSeq
        cadenaEncontrada
        

        // def reconstruirCadenaAux(cadena: Seq[Char], combinaciones: Seq[String]): Seq[Char] = {
        //     if (oraculo(cadena) == true) {
        //         cadena
        //     } else {
        //         reconstruirCadenaAux(combinaciones.head.toSeq, combinaciones.tail)
        //     }
        // }
        // reconstruirCadenaAux(combinaciones.head.toSeq, combinaciones.tail)
       
    }




}
