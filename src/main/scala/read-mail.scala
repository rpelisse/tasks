import javax.mail._
import javax.mail.internet._
import java.io._
import java.util._

val is = new FileInputStream(new File(args(0)))
val s = Session.getDefaultInstance(new Properties())

val msg = new MimeMessage(s, is)
println("Subject:" + msg.getSubject())
msg.getContent() match {
  case m: Multipart => println(m.getBodyPart(0).getContent())
  case _ => println(msg.getContent().toString())
}
