package ProyectoFinal

object  Oraculo{
    type Oraculo = Seq[Char] => Boolean


    def oraculoFunc(cadena: Seq[Char]): Oraculo = {
        (subcadena: Seq[Char]) => cadena.mkString.contains(subcadena.mkString)
    }

}