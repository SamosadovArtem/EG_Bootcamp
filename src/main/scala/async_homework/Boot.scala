package async_homework

import java.net.URL
import java.util.concurrent.Executors

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

/**
 * Application:
 * - takes a web-page URL from arguments (args array)
 * - loads the web-page body, extracts HTTP links from it
 * - for all the found links, tries to fetch a server name header if there is one
 * - prints all the encountered unique server name values in alphabetical order
 *
 * Each link processing should be done in parallel.
 * Validation of arguments is not needed.
 *
 * Try to test it on http://google.com!
 */
object Boot extends App {
  private implicit val ec: ExecutionContext = ExecutionContext.fromExecutor(Executors.newCachedThreadPool())

  def process(links: List[String]) = {
    for {
      pageBodies <- Future.sequence(
        links.map(fetchPageBody).map(v => v.recover {
          case err => println(s"Error during parsing body, $err"); ""
        })
      )
      allUrls <- Future.sequence(pageBodies.map(findLinkUrls).map(v => v.recover {
        case err => println(s"Error during finding links URLs, $err"); List()
      })).map(_.flatten)
      serverNames <- Future.sequence(
        allUrls.map(fetchServerName).map(v => v.recover {
          case err => println(s"Error during fetching server name, $err"); None
        })
      )
    } yield {
      serverNames
        .filter(_.isDefined)
        .map(_.get)
        .sortBy(_.toUpperCase)
        .foreach(println)
      ()
    }
  }

  args.toList match {
    case Nil =>
      println("Put at least one args")
    case _ => process(args.toList)
  }

  private def fetchPageBody(url: String): Future[String] = {
    println(f"Fetching $url")
    Future {
      val source = Source.fromURL(url)
      try {
        source.mkString
      } finally {
        source.close()
      }
    }
  }

  private def fetchServerName(url: String): Future[Option[String]] = {
    println(s"Fetching server name header for $url")
    Future {
      Option(new URL(url).openConnection().getHeaderField("Server"))
    }
  }


  private def findLinkUrls(html: String): Future[List[String]] = Future {
    val linkPattern = """href="(http[^"]+)"""".r
    linkPattern.findAllMatchIn(html).map(m => m.group(1)).toList
  }

  /*
  OUTPUT for http://google.com:

  Fetching http://google.com
  Fetching server name header for http://www.google.by/imghp?hl=be&tab=wi
  Fetching server name header for http://www.youtube.com/?gl=BY&tab=w1
  Fetching server name header for https://news.google.com/?tab=wn
  Fetching server name header for https://mail.google.com/mail/?tab=wm
  Fetching server name header for https://drive.google.com/?tab=wo
  Fetching server name header for https://www.google.com/calendar?tab=wc
  Fetching server name header for http://translate.google.by/?hl=be&tab=wT
  Fetching server name header for http://www.google.by/history/optout?hl=be
  Fetching server name header for https://accounts.google.com/ServiceLogin?hl=be&passive=true&continue=http://www.google.com/&ec=GAZAAQ
  Fetching server name header for http://www.google.com/setprefs?sig=0_Hd-aBZjv3jnCgT2ZN4xnUgnQ7NE%3D&amp;hl=ru&amp;source=homepage&amp;sa=X&amp;ved=0ahUKEwilpYOCsObsAhUQCxoKHUgyDDgQ2ZgBCAU
  Fetching server name header for http://www.google.com/setprefdomain?prefdom=BY&amp;prev=http://www.google.by/&amp;sig=K_26Q2h9oeiSuVNCv8WSxuAI2bkIg%3D
  ESF
  ESF
  GSE
  GSE
  GSE
  GSE
  gws
  gws
  gws
  HTTP server (unknown)
  YouTube Frontend Proxy

   */
}
