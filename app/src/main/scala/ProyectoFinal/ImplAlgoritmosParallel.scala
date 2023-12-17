package ProyectoFinal
import common._
import scala.collection.parallel.CollectionConverters._
import Oraculo._
import  ImplAlgoritmos._

object  ImplAlgoritmosParallel {
     def reconstruirCadenaIngenuoParallel(umbral: Int)(n: Int, oraculo: Oraculo): Seq[Char] = {

        // por definir umbral en tamanos menores a 5-6
        
        val combinaciones = generarCombinaciones(n)
        val combinacionesFiltradas = combinaciones.par.filter(oraculo(_) == true)
        val combinacion = combinacionesFiltradas.head
        val cadenaEncontrada = combinacion.toSeq
        cadenaEncontrada       
    }
      def reconstruirCadenaMejoradoParallel(umbral: Int) (n : Int , oraculo : Oraculo ): Seq[Char]= {
        def reconstruirCadenaMejoradoParallelAux(cadena: Seq[Char], combinaciones: Seq[String], acumulador: Seq[String], n:Int): Seq[String] = {
            if (n == 0) {
                acumulador
            } else {
                val combinaciones  = generarCombinaciones(n)
                
                val cadenaEncontrada = for {
                cadena <- combinaciones
                if oraculo(cadena) == true
                } yield cadena
            
                val acumulacion = acumulador ++ cadenaEncontrada
                reconstruirCadenaMejoradoParallelAux(combinaciones.head.toSeq, combinaciones.tail, acumulacion,n-1)
                }
        }

        if(n%2 == 0){
            val subCadenasCorrectas = task(reconstruirCadenaMejoradoParallelAux( Seq(), Seq(), Seq(), n/2))

             val posiblesCombinaciones =  for {
                 subCadena1 <- subCadenasCorrectas.join()
                 subCadena2 <- subCadenasCorrectas.join()
                 if (oraculo(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            posiblesCombinaciones.mkString
        }else{
            val (subCadenasCorrectas1, subCadenasCorrectas2) = parallel(reconstruirCadenaMejoradoParallelAux( Seq(), Seq(), Seq(), n/2),
            reconstruirCadenaMejoradoParallelAux( Seq(), Seq(), Seq(), n-(n/2)))
            val posiblesCombinaciones = for {
                subCadena1 <- subCadenasCorrectas1
                subCadena2 <- subCadenasCorrectas2
                if (oraculo(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            posiblesCombinaciones.mkString

        }
    }

      def reconstruirCadenaTurboParallel(umbral: Int)(n : Int , oraculo : Oraculo ) : Seq [Char]={
         def reconstruirCadenaTurboParallelAux(cadena: Seq[Char], combinaciones: Seq[String], acumulador: Seq[String], n:Int): Seq[String] = {
            if (((n == 0 || n == 1) && acumulador.length > 0) || n < 0 ) {
                acumulador
            } else {
                val combinaciones  = generarCombinaciones(n)
                
                val cadenaEncontrada = for {
                cadena <- combinaciones
                if oraculo(cadena) == true
                } yield cadena
            
                val acumulacion = acumulador ++ cadenaEncontrada
                reconstruirCadenaTurboParallelAux(combinaciones.head.toSeq, combinaciones.tail, acumulacion,n-2)
                }
        }

        if(n%2 == 0){
            val subCadenasCorrectas = task(reconstruirCadenaTurboParallelAux( Seq(), Seq(), Seq(), n/2))

             val posiblesCombinaciones =  for {
                 subCadena1 <- subCadenasCorrectas.join()
                 subCadena2 <- subCadenasCorrectas.join()
                 if (oraculo(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            posiblesCombinaciones.mkString
        }else{
            val (subCadenasCorrectas1,subCadenasCorrectas2) = parallel(reconstruirCadenaTurboParallelAux( Seq(), Seq(), Seq(), n/2)
             ,reconstruirCadenaTurboParallelAux( Seq(), Seq(), Seq(), n-(n/2)))
            val posiblesCombinaciones = for {
                subCadena1 <- subCadenasCorrectas1
                subCadena2 <- subCadenasCorrectas2
                if (oraculo(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            posiblesCombinaciones.mkString

        }

    }
    def reconstruirCadenaTurboMejoradaParallel (umbral: Int)(n : Int , oraculo : Oraculo ) : Seq [Char]= {
        def reconstruirCadenaTurboMejoradaParallelAux(cadena: Seq[Char], combinaciones: Seq[String], acumulador: Seq[String], n:Int,baseInicial:Int,potencia:Int): Seq[String] = {
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
                reconstruirCadenaTurboMejoradaParallelAux(combinaciones.head.toSeq, combinaciones.tail, acumulacion,n-base,base,potencia+1)
                }
        }

        if(n%2 == 0){
            val subCadenasCorrectas = task(reconstruirCadenaTurboMejoradaParallelAux( Seq(), Seq(), Seq(), n/2,2,1))

             val posiblesCombinaciones =  for {
                 subCadena1 <- subCadenasCorrectas.join()
                 subCadena2 <- subCadenasCorrectas.join()
                 if (oraculo(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            posiblesCombinaciones.mkString
        }else{
            val (subCadenasCorrectas1,subCadenasCorrectas2) = parallel(reconstruirCadenaTurboMejoradaParallelAux( Seq(), Seq(), Seq(), n/2,2,1)
            ,reconstruirCadenaTurboMejoradaParallelAux( Seq(), Seq(), Seq(), n-(n/2),2,1))
            val posiblesCombinaciones = for {
                subCadena1 <- subCadenasCorrectas1
                subCadena2 <- subCadenasCorrectas2
                if (oraculo(subCadena1 + subCadena2) == true) && ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            posiblesCombinaciones.mkString

        }

    }
    def reconstruirCadenaTurboAceleradaParallel(umbral: Int)(n : Int , oraculo : Oraculo ) : Seq [Char]= {
        def reconstruirCadenaTurboAceleradaParallelAux(cadena: Seq[Char], combinaciones: Seq[String], acumulador: Seq[String], n:Int,baseInicial:Int,potencia:Int): Seq[String] = {
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
                reconstruirCadenaTurboAceleradaParallelAux(combinaciones.head.toSeq, combinaciones.tail, acumulacion,n-base,base,potencia+1)
                }
        }

        if(n%2 == 0){
            val subCadenasCorrectas = (reconstruirCadenaTurboAceleradaParallelAux( Seq(), Seq(), Seq(), n/2,2,1))

             val posiblesCombinaciones =  for {
                 subCadena1 <- subCadenasCorrectas
                 subCadena2 <- subCadenasCorrectas
                 if  ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            val vacio = new Nodo(' ', false, Nil)
            val arbolDePosibilidades = task(vacio.arbolDeSufijos(posiblesCombinaciones))
            val posibilidades = vacio.generarPosibilidades(arbolDePosibilidades.join())
            val cadenaEncontrada = for {
                 posibilidad <- posibilidades
                 if oraculo(posibilidad) == true
             } yield posibilidad

            cadenaEncontrada.mkString

                    
        }else{
            val (subCadenasCorrectas1,subCadenasCorrectas2) = parallel(reconstruirCadenaTurboAceleradaParallelAux( Seq(), Seq(), Seq(), n/2,2,1)
            , reconstruirCadenaTurboAceleradaParallelAux( Seq(), Seq(), Seq(), n-(n/2),2,1))
            val posiblesCombinaciones = for {
                subCadena1 <- subCadenasCorrectas1
                subCadena2 <- subCadenasCorrectas2
                 if  ((subCadena1 + subCadena2).length == n)
            } yield subCadena1 + subCadena2
            
            val vacio = new Nodo(' ', false, Nil)
            val arbolDePosibilidades = task(vacio.arbolDeSufijos(posiblesCombinaciones))
            val posibilidades = vacio.generarPosibilidades(arbolDePosibilidades.join())
            val cadenaEncontrada = for {
                 posibilidad <- posibilidades
                 if oraculo(posibilidad) == true
             } yield posibilidad

            cadenaEncontrada.mkString


        }

    }

}