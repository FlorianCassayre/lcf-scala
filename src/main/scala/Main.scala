import fol._ // Types, rules, axioms
import fol.Theorems._ // Useful theorems

object Main extends App {

  // Formulas are represented as *types*, and theorems as *instances*

  // Some variables
  type P = Var["P"]
  type Q = Var["Q"]
  type R = Var["R"]

  type MyFormula = ~[P] /\ False // A custom formula alias

  // P <-> P
  val pIffp: Theorem[P <-> P] = iffRefl[P] // A theorem

  // False -> False
  val fImpliesf: Theorem[False ->: False] = trueIff(truth) // Implicit modus ponens application

  // ~False
  val nfalse: Theorem[~[False]] = iffSym(notIff[False])(fImpliesf) // Build proofs upon previous work


  // `null` values are normally prohibited (as well as exceptions and other constructs that bypass type checking)
  // While they can lead to arbitrary incorrect theorems that don't get caught during compilation, they will result in runtime exceptions

  // val myIncorrectTheorem1: Theorem[False] = modusPonens[True, False](truth, truth) // Caught at compilation time (raises type checking error)
  // val myIncorrectTheorem2: Theorem[False] = modusPonens[True, False](null, truth) // Caught at runtime


}
