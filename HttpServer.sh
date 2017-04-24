#!/bin/sh
exec scala -savecompiled "$0" $@
!#
import java.io._
import java.net.{ServerSocket, Socket}
import java.util.Date
import java.util.concurrent.Executors

import scala.io.Source
import scala.sys.process.BasicIO
import scala.util.Try
import scala.util.matching.Regex
import scala.xml.{NodeSeq, Xhtml}
import scala.collection._

object Boot extends Loggable {

  def main(args: Array[String]): Unit = {
    val port = args.headOption
      .flatMap(arg => Try(arg.toInt).toOption)
      .getOrElse(8080)

    val serverSocket = new ServerSocket(port)
    info(s"Listening for connections on port $port")

    val pool = Executors.newCachedThreadPool()
    while(true) pool.execute(HttpServer(serverSocket.accept))
  }
}

class HttpServer(socket: Socket) extends Runnable with Loggable {
  import HttpServer._

  def run(): Unit = {
    val source = Source.fromInputStream(socket.getInputStream, Encoding)
    try {
      val line = source.getLines.next

      val request = line.split("\\s+").toList
      info(request.mkString("::"))
      routes(request)
    } finally {
      source.close()
    }
  }

  def routes(list: List[String]): Unit = list match {
    case "GET" :: "/stats" :: _ =>
      new Stats().createPage()
    case "GET" :: HttpServer.PathImageByIdParser(id) :: _ =>
      new Image(id.toInt).createPage()
    case _ =>
      NotFound.createPage()
  }

  trait Page {

    def createPage(): Unit

    def response(status: Status, contentType: String = "text/html", content: Array[Byte]): Unit = {
      val out = new BufferedOutputStream(socket.getOutputStream)

      val header =
        s"""
           |HTTP/1.1 ${status.code} ${status.text}
           |Server: Scala HTTP Server 1.0
           |Date: ${new Date()}
           |Content-type: $contentType
           |Content-length: ${content.length}
      """.stripMargin + LineSep + LineSep

      try {
        out.write(header.getBytes(Encoding))
        out.flush()

        out.write(content)
        out.flush()
      } catch {
        case e: IOException =>
          error(e.getMessage)
          new ServerError(e).createPage()
      } finally {
        out.close()
      }
    }

    def generateHtml(status: Status, title: String, body: NodeSeq): Unit = {
      response(
        status = status,
        content = Xhtml.toXhtml(
          <HTML>
            <HEAD><TITLE>{ title }</TITLE></HEAD>
            <BODY>{ body }</BODY>
          </HTML>
        ).getBytes(Encoding))
    }
  }

  class Stats extends Page {
    def createPage(): Unit = {
      
      val info: NodeSeq = HttpServer.StatsInfo.map {
        case (_, imgInfo) =>
          <LI>id: { imgInfo.id }, fileName: { imgInfo.fileName }, views: { imgInfo.views }</LI>
      }.toSeq
      
      val body = <DIV>
        <H2>Stats</H2>
        <UL>{ info }</UL>
      </DIV>
      
      generateHtml(
        status = Status(200, "Main page"),
        title = "Main page",
        body = body)
    }
  }

  class Image(id: Int) extends Page {
    def createPage(): Unit = {
      HttpServer.StatsInfo.get(id) match {
        case Some(imgInfo) =>
          val content = getImage(imgInfo.fileName)
            .map { file =>
              val outBytes = new ByteArrayOutputStream()
              BasicIO.transferFully(new FileInputStream(file), outBytes)
              outBytes.toByteArray
            }.getOrElse(Array.emptyByteArray)

          updateView(id)

          response(
            status = Status(200, "Image"),
            content = content,
            contentType = "image/jpeg")
        case None =>
          NotFound.createPage()
      }
    }

    def getImage(img: String, imagesPath: String = HttpServer.ImagePath): Option[File] = {
      val file = new File(imagesPath, img)
      if(file.exists && file.isFile) {
        Some(file)
      }
      else {
        error("Image file not found")
        None
      }
    }

    def updateView(key: Int): Unit =
      HttpServer.StatsInfo.get(key) match {
        case Some(imgInfo) =>
          HttpServer.StatsInfo.update(key, imgInfo.copy(views = imgInfo.views + 1))
        case None => error("Image not found in StatsInfo map")
      }
  }

  object NotFound extends Page {
    
    def createPage(): Unit = {
      generateHtml(
        status = Status(404, "Page Not Found"),
        title = "Page Not Found",
        body = <P>404 Page Not Found</P>)
    }
  }

  class ServerError(e: Exception) extends Page {
    
    def createPage(): Unit = {
      generateHtml(
        status = Status(500, "Internal Server Error"),
        title = "Internal Server Error",
        body = <DIV>
          <H2>500: Internal Server Error</H2>
          <P>{ e.getMessage }</P>
        </DIV>
      )
    }
  }

}

object HttpServer extends Loggable {

  val Encoding: String = "UTF-8"
  val LineSep: String = System.getProperty("line.separator")
  val ImagePath: String = "./images"

  val PathImageByIdParser: Regex = "/image/([0-9]+)".r

  val ImgIdParser: Regex = "cat([0-9]+).jpg".r

  val StatsInfo: concurrent.TrieMap[Int, ImgInfo] = {
    val imagesInfoMap = new File(ImagePath) match {
      case imageDir if imageDir.exists && imageDir.isDirectory =>
        imageDir.listFiles
          .filter(file => file.isFile && file.getName.endsWith(".jpg"))
          .flatMap(img => {
            img.getName match {
              case ImgIdParser(id) =>
                Option(id.toInt -> ImgInfo(id.toInt, img.getName, 0))
              case _ =>
                None
            }
          }).toSeq
      case _ =>
        error("Images path not found...")
        Seq.empty[(Int, ImgInfo)]
    }
    concurrent.TrieMap(imagesInfoMap: _*)
  }

  def apply(socket: Socket): HttpServer =
    new HttpServer(socket)
}

trait Loggable {

  @inline
  def info(msg: => String): Unit = println("INFO: " + msg)

  @inline
  def error(msg: => String): Unit = println("ERROR: " + msg)

  @inline
  def debug(msg: => String): Unit = println("DEBUG: " + msg)
}

case class Status(code: Int, text: String)
case class ImgInfo(id: Int, fileName: String, views: Int)

Boot.main(args)