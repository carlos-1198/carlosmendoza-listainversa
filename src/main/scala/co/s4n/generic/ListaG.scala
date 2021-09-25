package co.s4n.generic

sealed trait ListaG[+A]

case object Vacia extends ListaG[Nothing]
case class Cons[A](h:A, t:ListaG[A]) extends ListaG[A]

object ListaG{
  def apply[A](as:A*):ListaG[A] =
    if (as.isEmpty) Vacia
    else Cons(as.head, apply(as.tail: _*))
}