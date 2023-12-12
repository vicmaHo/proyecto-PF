package ProyectoFinal
import Oraculo._
import scala.util.Random

class ImplAlgoritmos {
    val alfabeto =Seq('a','c','g','t')

    def crearADN(tamano: Int): String = {
		val ADN: String = (for {
				i <- 1 to tamano
			} yield alfabeto(Random.nextInt(alfabeto.length))).mkString
		ADN
	}

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

        def reconstruirCadenaAux(cadena: Seq[Char], combinaciones: Seq[String]): Seq[Char] = {
            if (oraculo(cadena) == true) {
                cadena
            } else {
                reconstruirCadenaAux(combinaciones.head.toSeq, combinaciones.tail)
            }
        }
        reconstruirCadenaAux(combinaciones.head.toSeq, combinaciones.tail)
       
    }

    def reconstruirCadenaMejorado (n : Int , oraculo : Oraculo ): Seq[Char]= {
        def reconstruirCadenaMejoradoAux(cadena: Seq[Char], combinaciones: Seq[String], acumulador: Seq[String], n:Int): Seq[String] = {
            if (n == 0) {
                acumulador
            } else {
                val combinaciones  = generarCombinaciones(n)
                
                val cadenaEncontrada = for {
                cadena <- combinaciones
                if oraculo(cadena) == true
                } yield cadena
            
                val acumulacion = acumulador ++ cadenaEncontrada
                reconstruirCadenaMejoradoAux(combinaciones.head.toSeq, combinaciones.tail, acumulacion,n-1)
                }
        }

        if(n%2 == 0){
            val subCadenasCorrectas = reconstruirCadenaMejoradoAux( Seq(), Seq(), Seq(), n/2)

             val posiblesCombinaciones =  for {
                 subCadena1 <- subCadenasCorrectas
                 subCadena2 <- subCadenasCorrectas
                 if (oraculo(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            posiblesCombinaciones.mkString
        }else{
            val subCadenasCorrectas1 = reconstruirCadenaMejoradoAux( Seq(), Seq(), Seq(), n/2)
            val subCadenasCorrectas2 = reconstruirCadenaMejoradoAux( Seq(), Seq(), Seq(), n-(n/2))
            val posiblesCombinaciones = for {
                subCadena1 <- subCadenasCorrectas1
                subCadena2 <- subCadenasCorrectas2
                if (oraculo(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            posiblesCombinaciones.mkString

        }
    }

    def reconstruirCadenaTurbo (n : Int , oraculo : Oraculo ) : Seq [Char]={
         def reconstruirCadenaTurboAux(cadena: Seq[Char], combinaciones: Seq[String], acumulador: Seq[String], n:Int): Seq[String] = {
            if (n == 0 || n == 1) {
                acumulador
            } else {
                val combinaciones  = generarCombinaciones(n)
                
                val cadenaEncontrada = for {
                cadena <- combinaciones
                if oraculo(cadena) == true
                } yield cadena
            
                val acumulacion = acumulador ++ cadenaEncontrada
                reconstruirCadenaTurboAux(combinaciones.head.toSeq, combinaciones.tail, acumulacion,n-2)
                }
        }

        if(n%2 == 0){
            val subCadenasCorrectas = reconstruirCadenaTurboAux( Seq(), Seq(), Seq(), n/2)

             val posiblesCombinaciones =  for {
                 subCadena1 <- subCadenasCorrectas
                 subCadena2 <- subCadenasCorrectas
                 if (oraculo(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            posiblesCombinaciones.mkString
        }else{
            val subCadenasCorrectas1 = reconstruirCadenaTurboAux( Seq(), Seq(), Seq(), n/2)
            val subCadenasCorrectas2 = reconstruirCadenaTurboAux( Seq(), Seq(), Seq(), n-(n/2))
            val posiblesCombinaciones = for {
                subCadena1 <- subCadenasCorrectas1
                subCadena2 <- subCadenasCorrectas2
                if (oraculo(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            posiblesCombinaciones.mkString

        }

    }




}
