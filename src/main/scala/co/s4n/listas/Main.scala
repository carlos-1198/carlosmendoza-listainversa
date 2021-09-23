package co.s4n.listas

import scala.io.Source

object Main extends App{
  def deListALista(lst:List[Int]):Lista = lst match {
    case Nil => Vacia()
    case i :: lstp => Cons(i, deListALista(lstp))
  }

  def leerArchivo(src:String):Lista =
    deListALista(Source.fromFile(src).getLines().toList.map(_.toInt))

  def concatenar(lst1:Lista, lst2:Lista):Lista = ???

  def imprimirLista(lst:Lista):String = {
    def iImprimirLista(lst: Lista, posicion: Int): String = {
      if (Vacia()) ""
      else iImprimirLista(lst, pos + 1)
    }
    iImprimirLista(lst,0)
  }

  def invertirLista(lst:Lista):Lista = ???

  /*
  def longitud(lst:Lista):Int = lst match {
    case Vacia()      => 0
    case Cons(i, lst) => 1 + longitud
  }
   */
  //val lista = leerArchivo(args(0))
  println(leerArchivo(s"data/numeros1.txt"))
  println(longitud(leerArchivo(s"data/numeros1.txt")))
}

