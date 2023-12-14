package ProyectoFinal

object Oraculo {

    val alfabeto = Seq('a', 'c', 'g', 't')
    type Oraculo = Seq[Char] => Boolean


   def oraculoFunc(cadena: Seq[Char]): Oraculo = {
		(subCadena: Seq[Char]) => cadena.mkString.contains(subCadena.mkString)
	}

}
