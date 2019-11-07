package fol

object Theorems {

  // Theorems

  /** P => P */
  def impliesRefl[P <: Formula]: Theorem[P ->: P] =
    impliesDistr[P, P ->: P, P](addImplies[P, P ->: P])(addImplies[P, P])

  /** P <=> Q given P => Q and Q => P */
  def impliesIff[P <: Formula, Q <: Formula](pq: Theorem[P ->: Q], qp: Theorem[Q ->: P]): Theorem[P <-> Q] =
    impliesToIff[P, Q](pq)(qp)

  /** P <=> P */
  def iffRefl[P <: Formula]: Theorem[P <-> P] = {
    val pp = impliesRefl[P]
    impliesIff(pp, pp)
  }

  /** Q <=> P given P <=> Q */
  def iffSym[P <: Formula, Q <: Formula](pq: Theorem[P <-> Q]): Theorem[Q <-> P] = {
    impliesToIff[Q, P](iffToImplies2[P, Q](pq))(iffToImplies1[P, Q](pq))
  }

  // etc.

}
