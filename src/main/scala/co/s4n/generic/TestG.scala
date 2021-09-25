package co.s4n.generic

object TestG extends App {
  def addInicioLista[A](a: A, lst: ListaG[A]) = Cons(a, lst)

  def addFinalLista[A](a:A, lst:ListaG[A]):ListaG[A] = lst match {
    case Vacia        => Cons(a,Vacia)
    case Cons(i, lst) => Cons(i, addFinalLista(a, lst))
  }

  def buscarLista[A](a:A, list: ListaG[A]):Boolean = list match {
    case Vacia => false
    case Cons(i, lst) => if (i == a) true else buscarLista(a, lst)
  }

  def concatenar[A](list1:ListaG[A], list2:ListaG[A]):ListaG[A] = list1 match {
    case Vacia        => list2
    case Cons(i, lst) => Cons(i, concatenar(lst,list2))
  }

  def cabeza[A](lst:ListaG[A]):A = lst match {
    case Cons(i, _)  => i
  }

  def ultimo[A](lst:ListaG[A]):A = lst match {
    case Cons(i, Vacia) => i
    case Cons(i, lst)   => ultimo(lst)
  }

  def cola[A](lst:ListaG[A]):ListaG[A] =  lst match {
    case Vacia => Vacia
    case Cons(i, lst) => lst
  }
 // muestra todos menos el ultimo
  def init[A](lst:ListaG[A]):ListaG[A] = lst match {
      case Vacia          => Vacia
      case Cons(i, Vacia) => Vacia
      case Cons(j, lst)   => Cons(j, init(lst))
  }

  //drop recibe un n y "borra los primeros n elementos" y devuelve el resto
  def drop[A](lst:ListaG[A], a:Int):ListaG[A] = (lst,a) match {
    case (Vacia, a)        => Vacia
    case (Cons(i, lst), a) => if(a > 0) drop(lst, a-1) else Cons(i, lst)
  }
  //split(3, 1,2,3,4,5) = 1,2,3   4,5
  //SPLIT(0, 1,2,3,4,5) = VACIA 1,2,3,4,5
  def split[A](n:Int, lst:ListaG[A]):(ListaG[A], ListaG[A]) = (n, lst) match{
    case (0, lst)   => (Vacia, lst)
    case (n, Cons(i, lst))   => {
      val a = split(n-1, lst)
      (Cons(i, a._1), a._2)
    }
  }


/*
  def zip[A,B] (lst1:ListaG[A], lst2:ListaG[B]):List[(A,B)] = (lst1,lst2) match {
    case (Vacia, Vacia)                 => Vacia
    case (Cons(j,list1), Cons(i,list2)) => Cons((j,i), zip(list1,list2))
  }
 */

  def dropWhile[A](lst:ListaG[A], p:A => Boolean):ListaG[A] = lst match {
    case Vacia        => Vacia
    case Cons(i, lst) => if(p(i)) dropWhile(lst, p) else Cons(i, lst)
  }

  def and(lst:ListaG[Boolean]):Boolean = lst match{
    case Vacia        => true
    case Cons(i, lst) => if(!i) false else and(lst)
  }

  def or(lst:ListaG[Boolean]):Boolean = lst match {
    case Vacia  => false
    case Cons(i,lst) => if(i) true else or(lst)
  }

  val numberList = ListaG(1, 2, 3, 4, 5)
  val numberList2 = ListaG(7, 8, 9, 10)
  val booleanList = ListaG(false, false, false)
  println(init(numberList))
  println(split(2,numberList))
  println(dropWhile(numberList2, (x:Int)=> x<9))
  println(and(booleanList))
  println(or(booleanList))
  //println(split(2, numberList))
  /*
 println(drop(numberList2,1))
 println(cabeza(numberList2))
 println(ultimo(numberList))
 println(cola(numberList2))
  */

  /*
  println(addFinalLista(9,numberList))
  println(addInicioLista(8, numberList))
  println(buscarLista(true,booleanList))
  println(concatenar(numberList,numberList2))
  println(numberList)
   */
}
