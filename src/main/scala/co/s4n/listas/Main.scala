package co.s4n.listas
import scala.io.Source

object Main extends App{

  def deListALista(lst:List[Int]):Lista = lst match {
    case Nil => Vacia()
    case i :: lstp => Cons(i, deListALista(lstp))
  }

  def leerArchivo(src:String):Lista =
    deListALista(Source.fromFile(src).getLines().toList.map(_.toInt))

  def concatenar(lst1:Lista, lst2:Lista):Lista = (lst1,lst2) match {
    case (Vacia(), Vacia())              => Vacia()
    case (Vacia(), lst2)                 => lst2
    case (lst1, Vacia())                 => lst1
    case (Cons(i, lst1), Cons(j,lst2))   => Cons(j, concatenar(Cons(i,lst1),lst2))
  }

  def concatenar2(lst1:Lista, lst2:Lista):Lista = lst1 match {
    case Vacia()        => lst2
    case Cons(i, lst1)  => Cons(i, concatenar(lst1,lst2))
  }

  def imprimirLista(lst:Lista):String = {
    def iImprimirLista(a: Int, lst: Lista): String = lst match {
      case Vacia() => ""
      case Cons(i, Vacia()) => i + "]"
      case Cons(i, lst) => if (a==1) "[" + i + ", " + iImprimirLista(a+1, lst) else i + ", " + iImprimirLista(a+1,lst)
    }
    iImprimirLista(1,lst)
  }
  def imprimirListaReverse(lst:Lista):String = lst match {
    case Vacia()          => ""
    case Cons(i, lst)     => imprimirLista(lst) + "" +i
  }

  def invertirLista(lst:Lista):Lista = lst match {
    case Vacia() => Vacia()
    case Cons(i, lst) => Cons(i, invertirLista(lst))
  }

  def buscarLista(a:Int, lst:Lista):Boolean = lst match {
    case Vacia()      => false
    case Cons(i, lst) => if (i == a ) true else buscarLista(a,lst)
  }

  def longitud(lst:Lista):Int = lst match {
    case Vacia()      => 0
    case Cons(i, lst) => 1 + longitud(lst)
  }

  //val lista = leerArchivo(args(0))
  val tester = leerArchivo(s"data/numeros1.txt")
  val tester2 = leerArchivo(s"data/numeros2.txt")
  val mirrorTester = invertirLista(tester)
  println(buscarLista(6,tester))
  //println(longitud(tester))
  println(imprimirLista(tester))
  //println(imprimirListaReverse(tester))

  val concaTester = concatenar(tester,tester2)
  val concaTester2 = concatenar2(tester2,tester)
  println(imprimirLista(concaTester))
  println(imprimirLista(concaTester2))
}

