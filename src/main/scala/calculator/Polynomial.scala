package calculator

object Polynomial extends PolynomialInterface:
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] =
    Signal(b()*b() - 4*a()*c())

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] =
    Signal({
      if(delta() < 0) Set()
      else {
        val sol1 = (-b() + math.sqrt(delta())) / (2 * a())
        val sol2 = (-b() - math.sqrt(delta())) / (2 * a())
        if(sol1 == sol2) Set(sol1)
        else Set(sol1, sol2)
      }
    })

