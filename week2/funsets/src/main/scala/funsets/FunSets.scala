package funsets

object FunSets {
  /**
   * We represent a set by its characteristic function, i.e.
   * its `contains` predicate.
   */
  type Set = Int => Boolean

  /**
   * Indicates whether a set contains a given element.
   */
  def contains(s: Set, elem: Int): Boolean = s(elem)
                                                  //> contains: (s: funsets.FunSets.Set, elem: Int)Boolean

  /**
   * Returns the set of the one given element.
   */
  def singletonSet(elem: Int): Set = x => x == elem
                                                  //> singletonSet: (elem: Int)funsets.FunSets.Set

  /**
   * Returns the union of the two given sets,
   * the sets of all elements that are in either `s` or `t`.
   */
  def union(s: Set, t: Set): Set = x => contains(s,x) || contains(t,x)
                                                  //> union: (s: funsets.FunSets.Set, t: funsets.FunSets.Set)funsets.FunSets.Set

  /**
   * Returns the intersection of the two given sets,
   * the set of all elements that are both in `s` and `t`.
   */
  def intersect(s: Set, t: Set): Set = x => contains(s,x) && contains(t,x)
                                                  //> intersect: (s: funsets.FunSets.Set, t: funsets.FunSets.Set)funsets.FunSets.S
                                                  //| et

  /**
   * Returns the difference of the two given sets,
   * the set of all elements of `s` that are not in `t`.
   */
    def diff(s: Set, t: Set): Set = x => contains(s, x) && !contains(t, x)
                                                  //> diff: (s: funsets.FunSets.Set, t: funsets.FunSets.Set)funsets.FunSets.Set

  /**
   * Returns the subset of `s` for which `p` holds.
   */
    def filter(s: Set, p: Int => Boolean): Set = x => contains(s, x) && p(x)
                                                  //> filter: (s: funsets.FunSets.Set, p: Int => Boolean)funsets.FunSets.Set


  /**
   * The bounds for `forall` and `exists` are +/- 1000.
   */
  val bound = 1000                                //> bound  : Int = 1000

  /**
   * Returns whether all bounded integers within `s` satisfy `p`.
   */
    def forall(s: Set, p: Int => Boolean): Boolean = {
    def iter(a: Int): Boolean = {
       if (a > bound) true
       else if (contains(s, a) && !contains(p, a)) false
       else iter(a + 1)
    }
      iter(-bound)
  }                                               //> forall: (s: funsets.FunSets.Set, p: Int => Boolean)Boolean

  /**
   * Returns whether there exists a bounded integer within `s`
   * that satisfies `p`.
   */
    def exists(s: Set, p: Int => Boolean): Boolean = !forall(s, x => !p(x))
                                                  //> exists: (s: funsets.FunSets.Set, p: Int => Boolean)Boolean

  /**
   * Returns a set transformed by applying `f` to each element of `s`.
   */
    def map(s: Set, f: Int => Int): Set = x => s(f(x))
                                                  //> map: (s: funsets.FunSets.Set, f: Int => Int)funsets.FunSets.Set

  /**
   * Displays the contents of a set
   */
  def toString(s: Set): String = {
    val xs = for (i <- -bound to bound if contains(s, i)) yield i
    xs.mkString("{", ",", "}")
  }                                               //> toString: (s: funsets.FunSets.Set)String

  /**
   * Prints the contents of a set on the console.
   */
  def printSet(s: Set) {
    println(toString(s))
  }                                               //> printSet: (s: funsets.FunSets.Set)Unit

}
