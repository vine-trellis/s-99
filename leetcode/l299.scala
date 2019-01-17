object Solution {
    def getHint(secret: String, guess: String): String = {
        def getHintHelper[A](s: List[A], g: List[A], a: Int, b: Int): (Int, Int) = (s, g) match {
            case (Nil, Nil) => (a, b)
            case (sx :: sxs, gx :: gxs) if (sx == gx) => getHintHelper(sxs, gxs, a + 1, b)
            case (sx :: sxs, gx :: gxs) => getHintHelper(sxs, gxs, a, b + 1)
        }
        val ans = getHintHelper(secret.toList, guess.toList, 0, 0)
        s"${ans._1}A${ans._2}B"
    }
    def main(args: Array[String]): Unit = {
        getHint("1807", "7810")
    }
}