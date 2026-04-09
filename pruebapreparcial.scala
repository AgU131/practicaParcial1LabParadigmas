object Main {
// BLOQUE 1 — Pattern Matching (base del parcial)
    type Post = (String, String, String, String, Int)  // (subreddit, title, selftext, formattedDate, score)

    def getScore(p: Post): Int = p.match {
        
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
}


