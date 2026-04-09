object Main {
// BLOQUE 1 — Pattern Matching (base del parcial)
    type Post = (String, String, String, String, Int)  // (subreddit, title, selftext, formattedDate, score)

    def getScore(p: Post): Int = p match {
        case (_, _, _, _, score) => score
    }
    def isHighScore(p: Post): Boolean = 
        getScore(p) > 100
    def summarizePost(p: Post): String = p match {
        case (subreddit, title, _, _, score) => s"[$subreddit] $title ($score)"
    }

// BLOQUE 2 — map / filter / flatMap
    def titles(posts: List[Post]): List[String] =
        posts.map { case (_, title, _, _, _) => title }
    def highScorePosts(posts: List[Post]): List[Post] =
        post.filter { case (_, _, _, _, score) =>
            score > 100
        }
    def titlesWithHighScore(posts: List[Post]): List[String] =
        posts 
            .filter { case (_, _, _, _, score) => score > 100 }
            .map { case (_, title, _, _, _) => title }
        // titles(highScorePosts(posts))
    def wordsFromPosts(posts: List[Post]): List[String] =
        posts.flatMap { case (_, title, selftext, _, _) => 
            (title + " " + selftext).split("\\W+")
        }

// BLOQUE 3 — groupBy
    def groupBySubreddit(posts: List[Post]): Map[String, List[Post]]
        posts
            .groupBy(_._1) { case (subreddit, _, _, _, _) }
    def countPostsBySubreddit(posts: List[Post]): Map[String, Int]
    
    def titlesBySubreddit(posts: List[Post]): Map[String, List[String]]

// BLOQUE 4 — fold (EL MÁS IMPORTANTE)
    def sumScores(posts: List[Post]): Int

    def countPosts(posts: List[Post]): Int

    def maxScore(posts: List[Post]): Int

    def totalScoreBySubreddit(posts: List[Post]): Map[String, Int]

// BLOQUE 5 — Composición (CLAVE)
    def topTitles(posts: List[Post]): List[String]

    def averageScore(posts: List[Post]): Double

    def report(posts: List[Post]): String

// BLOQUE 6 — Nivel PARCIAL (tipo árbol / ADT)
    sealed trait Expr
    case class Num(n: Int) extends Expr
    case class Add(a: Expr, b: Expr) extends Expr
    case class Mul(a: Expr, b: Expr) extends Expr

    def eval(e: Expr): Int

    def mapExpr(e: Expr)(f: Int => Int): Expr

    def foldExpr[A](e: Expr)(
        num: Int => A,
        add: (A, A) => A,
        mul: (A, A) => A
    ): A
}