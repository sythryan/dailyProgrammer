trait myMethods {
  import scala.collection.mutable.Map

  private[this] def mapLetterAmounts(letters: String): scala.collection.mutable.Map[Char, Int] = {
    val baseAmounts = collection.mutable.Map(('a' to 'z' map {e: Char => (e, 0)}).toMap.toSeq: _*)
    letters map {e => baseAmounts(e) += 1}
    baseAmounts
  }

  private[this] def score(word: String, letterAmounts: scala.collection.mutable.Map[Char, Int]): Int = {
    def innerCheck(word: String, acc: Int): Int = word.length match {
      case 0 => acc
      case _ => letterAmounts(word.head) match {
        case 0 => innerCheck(word.tail, acc)
        case _ => 
          letterAmounts(word.head) -= 1 
          innerCheck(word.tail, acc + 1)
      }
    }  
    innerCheck(word, 0)
  }

  def AugThirteenth(words: String, letters: String): String = {
    def inner(words: List[String], acc: String): String = words.length match {
      case e if (e > 0) => words.head match {
          case e if (score(e, mapLetterAmounts(letters.filter(_ != ' '))) > acc.length) => inner(words.tail, words.head)
          case _ => inner(words.tail, acc) 
      }
      case _ => acc
    }
    inner(words.split(" ").toList, "")
  }
}

object Wednesday extends App  with myMethods {

  println(AugThirteenth("das day mad den foot ball down touch pass play","z a d f o n"))
}