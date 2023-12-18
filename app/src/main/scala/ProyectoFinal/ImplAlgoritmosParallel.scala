package ProyectoFinal
import common._
import scala.collection.parallel.CollectionConverters._
import Oraculo._
import  ImplAlgoritmos._

object  ImplAlgoritmosParallel {



    // def reconstruirCadenaIngenuoParallel1(umbral: Int)(n: Int, oraculo: Oraculo): Seq[Char] = {

    //     // por definir umbral en tamanos menores a 5-6
        
    //     val combinaciones = generarCombinaciones(n)
    //     val combinacionesFiltradas = combinaciones.par.filter(oraculo(_) == true)
    //     val combinacion = combinacionesFiltradas.head
    //     val cadenaEncontrada = combinacion.toSeq
    //     cadenaEncontrada       
    // }

    def reconstruirCadenaIngenuoParallel(umbral: Int)(n: Int, oraculo: Oraculo): Seq[Char] = {

        val combinaciones = generarCombinaciones(n)

        def tareaRecorrido(bloqueCombinaciones: Seq[String]) : Seq[Char] = {
            val combinacionesFiltradas = bloqueCombinaciones.par.filter(oraculo(_) == true)
            if (combinacionesFiltradas.isEmpty) {
                Seq.empty[Char] 
            } else {
                val combinacion = combinacionesFiltradas.head
                combinacion.toSeq
            }  
        }

        def separarCombinaciones(listaCombinaciones: Seq[String]): (Seq[String], Seq[String], Seq[String], Seq[String]) = {
            val mitad = listaCombinaciones.size / 2
            val cuarto1 = mitad / 2
            val cuarto2 = cuarto1 * 2
            val cuarto3 = cuarto1 * 3

            (
                listaCombinaciones.slice(0, cuarto1),
                listaCombinaciones.slice(cuarto1, cuarto2),
                listaCombinaciones.slice(cuarto2, cuarto3),
                listaCombinaciones.slice(cuarto3, listaCombinaciones.size)
            )
        }

        val (bloque1, bloque2, bloque3, bloque4) = separarCombinaciones(combinaciones)

        val (resultado1, resultado2, resultado3, resultado4) = parallel(tareaRecorrido(bloque1), tareaRecorrido(bloque2), tareaRecorrido(bloque3), tareaRecorrido(bloque4))


        def encontrarResultadoNoVacio(resultados: Seq[Seq[Char]]): Seq[Char] = {
            resultados.find(!_.isEmpty).getOrElse(Seq.empty[Char])
        }

        val resultadoCorrecto: Seq[Char] = encontrarResultadoNoVacio(Seq(resultado1, resultado2, resultado3, resultado4))

        resultadoCorrecto

    }
      def reconstruirCadenaMejoradoParallel(umbral: Int) (n : Int , oraculo : Oraculo ): Seq[Char]= {
        def reconstruirCadenaMejoradoParallelAux(cadena: Seq[Char], combinaciones: Seq[String], acumulador: Seq[String], n:Int): Seq[String] = {
            if (n == 0) {
                acumulador
            } else {
                val combinaciones  = generarCombinaciones(n)    
                

                
                def separarCombinacionesInterno(listaCombinaciones: Seq[String]): (Seq[String], Seq[String]) = {
                    val mitad = listaCombinaciones.size / 2
                    (
                        listaCombinaciones.slice(0, mitad),
                        listaCombinaciones.slice(mitad, listaCombinaciones.size),
                    )
                }

                def tareaRecorrido(bloqueCombinaciones: Seq[String]) : Seq[String] = {
                    val cadenaEncontrada = for {
                    cadena <- bloqueCombinaciones
                    if oraculo(cadena) == true
                    } yield cadena

                    cadenaEncontrada
                }

                val (bloque1, bloque2) = separarCombinacionesInterno(combinaciones)
                val (cadenaEncontrada1, cadenaEncontrada2) = parallel(tareaRecorrido(bloque1), tareaRecorrido(bloque2))
                val acumulacion = acumulador ++ (cadenaEncontrada1 ++ cadenaEncontrada2)
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