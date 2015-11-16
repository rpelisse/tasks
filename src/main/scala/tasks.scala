import javax.mail._
import javax.mail.internet._
import java.io._
import java.text.SimpleDateFormat
import java.util._

import com.google.api.client.auth.oauth2.Credential
import com.google.api.client.extensions.java6.auth.oauth2.AuthorizationCodeInstalledApp
import com.google.api.client.extensions.jetty.auth.oauth2.LocalServerReceiver
import com.google.api.client.googleapis.auth.oauth2.GoogleAuthorizationCodeFlow
import com.google.api.client.googleapis.auth.oauth2.GoogleClientSecrets
import com.google.api.client.googleapis.javanet.GoogleNetHttpTransport
import com.google.api.client.http.HttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import com.google.api.client.json.JsonFactory
import com.google.api.client.util.store.FileDataStoreFactory
import com.google.api.client.util.DateTime

import com.google.api.services.tasks.Tasks
import com.google.api.services.tasks.TasksScopes
import com.google.api.services.tasks.model._

import com.beust.jcommander.JCommander
import com.beust.jcommander.Parameter
import com.beust.jcommander.ParameterException

import javax.mail._
import javax.mail.internet._
import java.io._
import java.util._

import scala.collection.JavaConversions._

val debug = false

val phone_call_task = "☎"
val mail_task = "✉"
val print_task = "⎙"


def today() = {
  new DateTime(System.currentTimeMillis())
}

def readEmail(f:String) = {

  val msg = readMailFile(f)
  msg.getContent() match {
    case m: Multipart => m.getBodyPart(0).getContent().toString()
    case _ => msg.getContent().toString()
  }
}

def readMailFile(f:String) = {
  val is = new FileInputStream(new File(f))
  val s = Session.getDefaultInstance(new Properties())
  new MimeMessage(s, is)
}

def readSubject(f:String) = {
  readMailFile(f).getSubject
}

def getDesc(description:String, email:String, bugUrl:String): String = {
  if ( ! "".equals(bugUrl))
    return bugUrl

  if ( ! "".equals(email) )
    return readEmail(email)
  description
}

def getTitle(title:String, email:String, bugUrl:String):String = {
  if ( ! "".equals(bugUrl) )
    return bugUrl.substring(bugUrl.lastIndexOf('/') + 1)

  if ( ! "".equals(title) )
    return title

  if ( ! "".equals(email) ) {
    val subject = readSubject(email)
    if ( "".equals(subject) ) return "Mail based task"
    return subject
  }

  throw new IllegalArgumentException("No title provided, neither to extrapolate one.")
}

def getDueDate(dueDate:String) = {
  if ( ! "".equals(dueDate) )
    new DateTime(new java.text.SimpleDateFormat("dd/MM/yyyy").parse(dueDate))
  else
    today
}

def connectAndGetService() = {

  val APPLICATION_NAME = "Google Tasks API Java Quickstart"
  val DATA_STORE_DIR = new File(System.getProperty("user.home"), ".credentials/tasks-java-quickstart")
  val DATA_STORE_FACTORY = new FileDataStoreFactory(DATA_STORE_DIR)
  val JSON_FACTORY = JacksonFactory.getDefaultInstance()
  val HTTP_TRANSPORT = GoogleNetHttpTransport.newTrustedTransport()
  val SCOPES = Arrays.asList(TasksScopes.TASKS)

  val clientSecrets = GoogleClientSecrets.load(JSON_FACTORY, new InputStreamReader(new FileInputStream("client_secret.json")))
  val flow = new GoogleAuthorizationCodeFlow.Builder( HTTP_TRANSPORT, JSON_FACTORY, clientSecrets, SCOPES)
                  .setDataStoreFactory(DATA_STORE_FACTORY)
                  .setAccessType("offline")
                  .build()
  val credential = new AuthorizationCodeInstalledApp(flow, new LocalServerReceiver()).authorize("user")
  new Tasks.Builder(HTTP_TRANSPORT, JSON_FACTORY, credential).setApplicationName(APPLICATION_NAME).build()
}

def emptyStringIfNull(s: String):String = { if (s == null) "" else s }

def searchAndQuit(search: String):Unit = {
  if ( ! "".equals(search) ) {
    val tasks = service.tasks.list("@default").execute()

    for (task <- tasks.getItems )
      if ( task.getTitle().toLowerCase().contains(search.toLowerCase()) )
        println(taskDisplay(task))
    System.exit(0)
  }
}

def isToday(due:DateTime) = {
  isSameDay(due, Calendar.getInstance().getTime())
}

def isSameDay(due: DateTime, day: Date) = {
  //println("Is " +  due.toStringRfc3339.substring(0,10) + " equals to " + new SimpleDateFormat("y-M-dd").format(day) + " ?")
  due.toStringRfc3339.substring(0,10).equals(new SimpleDateFormat("y-M-dd").format(day))
}

def taskDisplay(task: com.google.api.services.tasks.model.Task) = {
  "[" + task.getId() + "] " + task.getTitle + "\nDue on: " + task.getDue() + "\n" + emptyStringIfNull(task.getNotes)
}

def listAndQuit(isListRequested: Boolean):Unit = {
  if ( isListRequested ) {
    val tasks = service.tasks.list("@default").execute()
    val dueDate = today()
    println("Today (" + dueDate + ") tasks:")
    for (task <- tasks.getItems )
      if ( task.getDue() != null && isToday(task.getDue()))
        println(taskDisplay(task))

    System.exit(0)
  }
}

def bumpDueDate(days:Int, id:String):Unit = {
  if ( days > 0 && id != null && ! "".equals(id) ) {
    val NB_SECONDS_BY_DAY = 86400L * 1000
    val task = service.tasks.get("@default", id).execute();
    task.setDue(new DateTime(task.getDue().getValue() + (days * NB_SECONDS_BY_DAY)))
    val result = service.tasks.update("@default", task.getId(), task).execute();
    println("Task '" + result.getTitle() + " has been bumped by " + days + " days:" + result.getDue())
    System.exit(0)
  }
}

def editTitle(id:String, newTitle:String):Unit = {
  if ( id != null && ! "".equals(id) && newTitle != null && ! "".equals(newTitle) ) {
    val task = service.tasks.get("@default", id).execute();
    task.setTitle(newTitle)
    val result = service.tasks.update("@default", task.getId(), task).execute();
    println("Task new title is '" + result.getTitle())
    System.exit(0)
  }
}

object Args {
  @Parameter(names = Array("-t", "--task-title"), description = "Task title", required = false)
  var title: String = ""

  @Parameter(names = Array("-d", "--task-description"), description = "Task description", required = false)
  var description: String = ""

  @Parameter(names = Array("-e", "--email-as-description"), description = "Task description", required = false)
  var email: String = ""

  @Parameter(names = Array("-D", "--due-date"), description = "Task description", required = false)
  var dueDate: String = ""

  @Parameter(names = Array("-b", "--bug-url"), description = "A Bug entry URL", required = false)
  var bugUrl: String = ""

  @Parameter(names = Array("-s", "--search-tasks"), description = "Search a task title containing the provided string", required = false)
  var search: String = ""

  @Parameter(names= Array("-l" , "--list-today-tasks"), description = "List today's tasks" , required = false )
  var list = false

  @Parameter(names= Array("-B", "--bump-task"), description = "Bump due date", required = false)
  var bump: Int = 0

  @Parameter(names= Array("-i", "--task-id"), description = "Task ID", required = false)
  var id: String = ""

  @Parameter(names= Array("-E", "--edit-task-title"), description = "Edit task title, requires task id", required = false)
  var newTitle: String = ""

}

new JCommander(Args, args.toArray: _*)

val service = connectAndGetService()

editTitle(Args.id, Args.newTitle)
bumpDueDate(Args.bump, Args.id)
listAndQuit(Args.list)
searchAndQuit(Args.search)


val title = getTitle(Args.title, Args.email, Args.bugUrl)
val desc = getDesc(Args.description, Args.email, Args.bugUrl)
val dueDate = getDueDate(Args.dueDate)

if ( debug )
  println("title:" + title + ", desc:" + desc)

val task = new com.google.api.services.tasks.model.Task()
task.setTitle(title)
task.setNotes(desc)
task.setDue(dueDate)

val result = service.tasks.insert("@default", task).execute()
println("Task '" + result.getTitle() + "' has been created and added")