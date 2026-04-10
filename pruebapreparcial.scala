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
        posts.filter { case (_, _, _, _, score) =>
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
    def groupBySubreddit(posts: List[Post]): Map[String, List[Post]] = 
        posts.groupBy { case (subreddit, _, _, _, _) => subreddit}
        // posts.groupBy(_._1)

    def countPostsBySubreddit(posts: List[Post]): Map[String, Int] =
        posts
            .groupBy(_._1)
            .map { case (subreddit, posts) => (subreddit, posts.length)}

    def titlesBySubreddit(posts: List[Post]): Map[String, List[String]] = 
        posts
            .groupBy(_._1)
            .map { case (sub, ps) => (sub, ps.map { case (_, title, _, _, _) => title})}
            // .map { case (sub, ps) => (sub, ps.map(_._2))}

// BLOQUE 4 — fold (EL MÁS IMPORTANTE)
    def sumScores(posts: List[Post]): Int = 
        posts.foldLeft(0) {
            case (acc, (_, _, _, _, score)) => score + acc
        }
    def countPosts(posts: List[Post]): Int =
        posts.foldLeft(0) ((acc, _) => acc + 1)
    def maxScore(posts: List[Post]): Int =
        posts.foldLeft(0) {
            case (acc, (_, _, _, _, score)) => 
                if (score > acc)
                    score
                else
                    acc
        }

    def totalScoreBySubreddit(posts: List[Post]): Map[String, Int] =
        posts
            .groupBy(_._1)
            .map {
                case (sub, ps) => 
                (sub, ps.foldLeft(0) { case (acc, (_, _, _, _, score)) => acc + score })
            }
            //.map { case (sub, ps) =>
            //  val total = ps.foldLeft(0)((acc, p) => acc + p._5)
            //  (sub, total) }

// BLOQUE 5 — Composición (CLAVE)
    def topTitles(posts: List[Post]): List[String] =
        posts.sortBy(-_._5).take(5).map { case (_, title, _, _, _) => title }
        // sortBy(-_._5) ordena los score en orden desc
    def averageScore(posts: List[Post]): Double =
        posts
            .foldLeft(0) { case (acc, (_, _, _, _, score)) => score + acc} / posts.length
            // .foldLeft(0) ((acc, ps) => getScore(ps) + acc) / posts.length
        // if (posts.isEmpty) 0.0
        // else posts.foldLeft(0.0)((acc, p) => acc + getScore(p).toDouble) / posts.length
    def report(posts: List[Post]): String =
        posts
            .groupBy(_._1)
            .map { case (sub, ps) => 
                val count = ps.length
                val total = ps.foldLeft(0)( (acc, post) => acc + post._5 )
                
                s"""Subreddit: $sub
                Cantidad: $count
                Score total: $total"""
            }
            .mkString("\n")
        

// BLOQUE 6 — Nivel PARCIAL (tipo árbol / ADT)
    sealed trait Expr
    case class Num(n: Int) extends Expr
    case class Add(a: Expr, b: Expr) extends Expr
    case class Mul(a: Expr, b: Expr) extends Expr

    def eval(e: Expr): Int = e match {
        case Num(n) => n
        case Add(a, b) => eval(a) + eval(b)
        case Mul(a, b) => eval(a) * eval(b)
    }


    def mapExpr(e: Expr)(f: Int => Int): Expr = e match {
        case Num(n)    => n
        case Add(a, b) => Add(mapExpr(a)(f), mapExpr(b)(f))
        case Mul(a, b) => Mul(mapExpr(a)(f), mapExpr(b)(f))
    }

    def foldExpr[A](e: Expr)(
        num: Int => A,
        add: (A, A) => A,
        mul: (A, A) => A
    ): A = e match {

        case Num(n) => num(n)

        case Add(a, b) =>
            add(foldExpr(a)(num, add, mul), foldExpr(b)(num, add, mul))

        case Mul(a, b) =>
            mul(foldExpr(a)(num, add, mul), foldExpr(b)(num, add, mul))
    }
}