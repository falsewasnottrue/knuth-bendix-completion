package orders

import models.Term

trait TermOrder {
  def compare(t1: Term, t2: Term): Int
}
s