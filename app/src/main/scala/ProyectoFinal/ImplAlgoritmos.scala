package ProyectoFinal
import Oraculo._
import scala.util.Random


object  ImplAlgoritmos {
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
        val cadenaEncontrada = for {
            cadena <- combinaciones
            if oraculo(cadena) == true
        } yield cadena.toSeq

        cadenaEncontrada.head
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
            if (((n == 0 || n == 1) && acumulador.length > 0) || n < 0 ) {
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
    def reconstruirCadenaTurboMejorada (n : Int , oraculo : Oraculo ) : Seq [Char]= {
        def reconstruirCadenaTurboMejoradaAux(cadena: Seq[Char], combinaciones: Seq[String], acumulador: Seq[String], n:Int,baseInicial:Int,potencia:Int): Seq[String] = {
            if ( baseInicial >= n && acumulador.length > 0) {
                acumulador
            } else {
                val combinaciones  = generarCombinaciones(n)
                
                val cadenaEncontrada = for {
                cadena <- combinaciones
                if oraculo(cadena) == true
                } yield cadena
            
                val acumulacion = acumulador ++ cadenaEncontrada
                val base = math.pow(2,potencia).toInt
                reconstruirCadenaTurboMejoradaAux(combinaciones.head.toSeq, combinaciones.tail, acumulacion,n-base,base,potencia+1)
                }
        }

        if(n%2 == 0){
            val subCadenasCorrectas = reconstruirCadenaTurboMejoradaAux( Seq(), Seq(), Seq(), n/2,2,1)

             val posiblesCombinaciones =  for {
                 subCadena1 <- subCadenasCorrectas
                 subCadena2 <- subCadenasCorrectas
                 if (oraculo(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            posiblesCombinaciones.mkString
        }else{
            val subCadenasCorrectas1 = reconstruirCadenaTurboMejoradaAux( Seq(), Seq(), Seq(), n/2,2,1)
            val subCadenasCorrectas2 = reconstruirCadenaTurboMejoradaAux( Seq(), Seq(), Seq(), n-(n/2),2,1)
            val posiblesCombinaciones = for {
                subCadena1 <- subCadenasCorrectas1
                subCadena2 <- subCadenasCorrectas2
                if (oraculo(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            posiblesCombinaciones.mkString

        }

    }
    def reconstruirCadenaTurboAcelerada (n : Int , oraculo : Oraculo ) : Seq [Char]= {
        def reconstruirCadenaTurboAceleradaAux(cadena: Seq[Char], combinaciones: Seq[String], acumulador: Seq[String], n:Int,baseInicial:Int,potencia:Int): Seq[String] = {
            if ( baseInicial >= n && acumulador.length > 0) {
                acumulador
            } else {
                val combinaciones  = generarCombinaciones(n)
                
                val cadenaEncontrada = for {
                cadena <- combinaciones
                if oraculo(cadena) == true
                } yield cadena
            
                val acumulacion = acumulador ++ cadenaEncontrada
                val base = math.pow(2,potencia).toInt
                reconstruirCadenaTurboAceleradaAux(combinaciones.head.toSeq, combinaciones.tail, acumulacion,n-base,base,potencia+1)
                }
        }

        if(n%2 == 0){
            val subCadenasCorrectas = (reconstruirCadenaTurboAceleradaAux( Seq(), Seq(), Seq(), n/2,2,1))

             val posiblesCombinaciones =  for {
                 subCadena1 <- subCadenasCorrectas
                 subCadena2 <- subCadenasCorrectas
                 if  ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            val vacio = new Nodo(' ', false, Nil)
            val arbolDePosibilidades = vacio.arbolDeSufijos(posiblesCombinaciones)
            val posibilidades = vacio.generarPosibilidades(arbolDePosibilidades)
            val cadenaEncontrada = for {
                 posibilidad <- posibilidades
                 if oraculo(posibilidad) == true
             } yield posibilidad

            cadenaEncontrada.mkString

                    
        }else{
            val subCadenasCorrectas1 = reconstruirCadenaTurboAceleradaAux( Seq(), Seq(), Seq(), n/2,2,1)
            val subCadenasCorrectas2 = reconstruirCadenaTurboAceleradaAux( Seq(), Seq(), Seq(), n-(n/2),2,1)
            val posiblesCombinaciones = for {
                subCadena1 <- subCadenasCorrectas1
                subCadena2 <- subCadenasCorrectas2
                 if  ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            val vacio = new Nodo(' ', false, Nil)
            val arbolDePosibilidades = vacio.arbolDeSufijos(posiblesCombinaciones)
            val posibilidades = vacio.generarPosibilidades(arbolDePosibilidades)
            val cadenaEncontrada = for {
                 posibilidad <- posibilidades
                 if oraculo(posibilidad) == true
             } yield posibilidad

            cadenaEncontrada.mkString


        }

    }


}
