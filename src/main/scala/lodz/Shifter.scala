package lodz

// my hypothesis is that IARZAXK is rotated. The clue suggests it's rotated +/- 6 in the alphabet, but because
// we're lazy, let's ignore the clue and see if we can find it.
// To do that, we first start with defining a "signature" for a given word that is an invariant under rotation
// through a ring alphabet.  Then let's load the words file, and see if there is a word in it that has the same
// signature as the clue word. If it does, and it's unique, that's the solution.

object Shifter extends App {
  val clueWord = "IARZAXK"
  val words = {
    val dictFile = "/usr/share/dict/words"
    val source = io.Source.fromFile(dictFile)
    try source.getLines().map(s => s.filter(c => Character.isAlphabetic(c)).toUpperCase()).toIndexedSeq
    finally source.close()
  }

  val sig = signature(clueWord)
  println(words.find { w => signature(w) == sig }.getOrElse("No match found"))

  // shifts a word some number of characters through a cyclical alphabet (so shift("A", 1) == "B", shift("A", -1) == "Z")
  def shift(word: String, count: Int): String = {
    assert(count >= -26 && count <= 26, count)
    assert(word.forall(c => Character.isAlphabetic(c)), s"$word contains non-alphabetic character")
    if (count >= 0) word.toUpperCase.map(c => (((c - 'A' + count) % 26) + 'A').toChar).mkString
    else word.toUpperCase.map(c => (((c - 'A' + 26 + count) % 26) + 'A').toChar).mkString
  }

  // computes the delta between two characters
  // e.g. deltaOne('A', 'Z') == -1, deltaOne('A', 'B') == 1
  def deltaOne(a: Char, b: Char): Int = {
    assert(a >= 'A' && a <= 'Z', s"$a")
    assert(b >= 'A' && b <= 'Z', s"$b")
    val d = b - a
    if (d < 0) d + 26 else d
  }

  // generates a signature for a word, which is invariant against rotation.
  def signature(str: String): String = {
    val s = str.toUpperCase()
    s.zip(s.tail).map { f =>
      val t = deltaOne(f._1, f._2).toHexString.toUpperCase().takeRight(2)
      if (t.length < 2) s"0$t" else t
    }.mkString
  }
}
