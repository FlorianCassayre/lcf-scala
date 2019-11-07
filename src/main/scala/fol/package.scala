
package object fol {

  // Types

  // Formulas
  // Any combination of boolean operators (does not have to be a tautology!)
  sealed abstract class Formula
  final class Var[I] extends Formula
  final class True extends Formula
  final class False extends Formula
  final class Not[P <: Formula] extends Formula
  final class And[P <: Formula, Q <: Formula] extends Formula
  final class Or[P <: Formula, Q <: Formula] extends Formula
  final class Implies[P <: Formula, Q <: Formula] extends Formula
  final class Iff[P <: Formula, Q <: Formula] extends Formula

  // Theorem
  // Something that is always true
  sealed class Theorem[F <: Formula] private ()

  private object Theorem {
    def apply[F <: Formula](theorems: Theorem[_]*): Theorem[F] = {
      require(!theorems.contains(null)) // Check if `null` values were not introduced
      new Theorem[F]()
    }
  }

  // Implicit conversions

  sealed class TheoremImplies[P <: Formula, Q <: Formula] private (pq: Theorem[P ->: Q]) {
    def apply(p: Theorem[P]): Theorem[Q] = modusPonens(pq, p) // Shorthand for modus ponens
  }
  private object TheoremImplies {
    def apply[P <: Formula, Q <: Formula](theorem: Theorem[P ->: Q]): TheoremImplies[P, Q] =
      new TheoremImplies[P, Q](theorem)
  }
  implicit def toTheoremImplies[P <: Formula, Q <: Formula](theorem: Theorem[P ->: Q]): TheoremImplies[P, Q] =
    TheoremImplies[P, Q](theorem)


  sealed class TheoremIff[P <: Formula, Q <: Formula] private(pq: Theorem[P <-> Q]) {
    def apply(q: Theorem[P]): Theorem[Q] =
      modusPonens(modusPonens(iffToImplies1[P, Q], pq), q)
  }
  private object TheoremIff {
    def apply[P <: Formula, Q <: Formula](theorem: Theorem[P <-> Q]): TheoremIff[P, Q] =
      new TheoremIff[P, Q](theorem)
  }
  implicit def toTheoremIff[P <: Formula, Q <: Formula](theorem: Theorem[P <-> Q]): TheoremIff[P, Q] =
    TheoremIff[P, Q](theorem)


  // Shorthands

  type ~[P <: Formula] = Not[P]
  type /\[P <: Formula, Q <: Formula] = And[P, Q]
  type \/[P <: Formula, Q <: Formula] = Or[P, Q]
  type ->:[P <: Formula, Q <: Formula] = Implies[P, Q] // ':' for right associativity
  type <->[P <: Formula, Q <: Formula] = Iff[P, Q]


  // Rules

  /** Q given P => Q and P */
  def modusPonens[P <: Formula, Q <: Formula](pq: Theorem[P ->: Q], p: Theorem[P]): Theorem[Q] = Theorem(pq, p)

  // Axioms

  /** P => Q => P */
  def addImplies[P <: Formula, Q <: Formula]: Theorem[P ->: Q ->: P] = Theorem()

  /** (P => Q => R) => (P => Q) => P => R */
  def impliesDistr[P <: Formula, Q <: Formula, R <: Formula]: Theorem[(P ->: Q ->: R) ->: (P ->: Q) ->: P ->: R] = Theorem()

  /** ((P => False) => False) => P */
  def doubleNegation[P <: Formula]: Theorem[((P ->: False) ->: False) ->: P] = Theorem()

  /** (P => Q) => (Q => P) => (P <=> Q) */
  def impliesToIff[P <: Formula, Q <: Formula]: Theorem[(P ->: Q) ->: (Q ->: P) ->: (P <-> Q)] = Theorem()

  /** (P <=> Q) => P => Q */
  def iffToImplies1[P <: Formula, Q <: Formula]: Theorem[(P <-> Q) ->: P ->: Q] = Theorem()

  /** (P <=> Q) => Q => P */
  def iffToImplies2[P <: Formula, Q <: Formula]: Theorem[(P <-> Q) ->: Q ->: P] = Theorem()

  /** True <=> (False => False) */
  def trueIff: Theorem[True <-> (False ->: False)] = Theorem()

  /** ~P <=> (P => False) */
  def notIff[P <: Formula]: Theorem[~[P] <-> (P ->: False)] = Theorem()

  /** (P /\ Q) <=> ((P => Q => False) => False) */
  def andIff[P <: Formula, Q <: Formula]: Theorem[(P /\ Q) <-> ((P ->: Q ->: False) ->: False)] = Theorem()

  /** (P \/ Q) => ~(~P /\ ~Q) */
  def orIff[P <: Formula, Q <: Formula]: Theorem[(P \/ Q) ->: ~[~[P] /\ ~[Q]]] = Theorem()


  val truth: Theorem[True] = Theorem()


}

// Axioms and theorems shamelessly taken from: https://lara.epfl.ch/w/fv19/labs04