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

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props

import scala.concurrent.Await
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._

val debug = true
implicit val timeout = Timeout(65 seconds)
val TASK_SYMBOL = scala.collection.Map("c" -> "☎", "m" -> "✉", "p" -> "⎙", "M" -> "♫")

def buildTask(title:String, desc: String, date: DateTime = today(), symbol: String = "") = {
  val task = new com.google.api.services.tasks.model.Task()
  task.setTitle(addSymbolToTitle(title, symbol))
  task.setNotes(desc)
  task.setDue(date)
  task
}

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

def bugIdFromBugUrl(bugUrl: String) = bugUrl.substring(bugUrl.lastIndexOf('/') + 1)

def descForBugUrl(bugUrl: String) = {
  val content = scala.io.Source.fromURL("https://issues.jboss.org/rest/api/latest/issue/" + bugIdFromBugUrl(bugUrl) + "?fields=summary").mkString
  val r = """^.*\"summary\":\"([^"]*)\".*$""".r
  val res = Option(content) collect { case r(group) => group }
  res.get
}

def getDesc(description:String, email:String, bugUrl:String): String = {
  if ( ! "".equals(bugUrl))
    return descForBugUrl(bugUrl) + "\n\n" + bugUrl

  if ( ! "".equals(email) )
    return readEmail(email)
  description
}

def parseTaskLine(line: String, sep: String = ";") = {
   val arr = line.split(sep)
   if (arr.length == 1 )
     (arr(0),"")
   else if ( arr.length == 2)
     (arr(0),arr(1))
   else
     ("","")
}

def getTitle(title:String, email:String, bugUrl:String):String = {
  if ( ! "".equals(bugUrl) )
    return bugIdFromBugUrl(bugUrl)

  if ( ! "".equals(title) )
    return title

  if ( ! "".equals(email) ) {
    val subject = readSubject(email)
    if ( "".equals(subject) ) return "Mail based task"
    return subject
  }
  title
}

def getDueDate(dueDate:String) = {
  if ( ! "".equals(dueDate) )
    new DateTime(new java.text.SimpleDateFormat("dd/MM/yyyy").parse(dueDate))
  else
    today
}

def getSymbol(symbolLetter:String): String = {
  if ( TASK_SYMBOL.keySet.contains(symbolLetter) ) {
    val symbol = TASK_SYMBOL.get(symbolLetter)
    if ( symbol != None ) {
      return symbol.get
    }
  }
  return ""
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
  //println("Is " +  due.toStringRfc3339.substring(0,10) + " equals to " + new SimpleDateFormat("y-MM-dd").format(day) + " ?")
  due.toStringRfc3339.substring(0,10).equals(new SimpleDateFormat("y-MM-dd").format(day))
}

def dateDisplay(date: DateTime) = {
  new SimpleDateFormat("dd/MM/YYYY").format(new Date(date.getValue()))
}

def taskDisplay(task: com.google.api.services.tasks.model.Task) = {
  "[" + task.getId() + "] " + task.getTitle + "\nDue on: " + dateDisplay(task.getDue()) + "\n" + emptyStringIfNull(task.getNotes)
}

def notNullNorEmpty(value: String) = {
  (value != null &&  ! "".equals(value) )
}

def addSymbolToTitle(title: String, symbol: String): String = {
  if ( notNullNorEmpty(symbol) ) return title + " " + symbol
  title
}

// Features methods

def listAndQuit(isListRequested: Boolean):Unit = {
  if ( isListRequested ) {
    val tasks = service.tasks.list("@default").execute()
    val dueDate = today()
    println("Today (" + dueDate + ") tasks:")
    println
    var taskNumber = 1
    for (task <- tasks.getItems )
      if ( task.getDue() != null && isToday(task.getDue())) {
        println(taskNumber + ") " + taskDisplay(task))
        taskNumber = taskNumber + 1
      }

    System.exit(0)
  }
}

def bumpDueDate(days:Int, id:String):Unit = {
  if ( days > 0 && id != null && ! "".equals(id) ) {
    val NB_SECONDS_BY_DAY = 86400L * 1000
    val task = service.tasks.get("@default", id).execute()
    task.setDue(new DateTime(task.getDue().getValue() + (days * NB_SECONDS_BY_DAY)))
    val result = service.tasks.update("@default", task.getId(), task).execute();
    println("Task '" + result.getTitle() + " has been bumped by " + days + " days:" + dateDisplay(result.getDue()))
    System.exit(0)
  }
}

def addTask(task: com.google.api.services.tasks.model.Task) = {
  "Task '" + service.tasks.insert("@default", task).execute().getTitle() + "' has been created and added"
}

def editTask(id:String, newTask: com.google.api.services.tasks.model.Task, symbol: String = ""):Unit = {
  if ( notNullNorEmpty(id) ) {
    val task = service.tasks.get("@default", id).execute()
    println("NewTask Title:" + newTask.getTitle())
    if ( notNullNorEmpty(newTask.getTitle().filterNot("☎✉⎙♫".toSet).replaceAll(" ","")) ) task.setTitle(newTask.getTitle())
    if ( notNullNorEmpty(newTask.getNotes())  ) task.setNotes(newTask.getNotes())

    if ( newTask.getDue() != null ) task.setDue(newTask.getDue())
    val result = service.tasks.update("@default", task.getId(), task).execute()
    println(taskDisplay(task))
    System.exit(0)
  }
}

def taskDone(id:String) = {
  if ( notNullNorEmpty(id) ) {
    service.tasks.delete("@default", id).execute()
    println("Task [" + id + "] has been removed.")
    System.exit(0)
  }
}

class TaskCreatorActor extends Actor {
  def receive = {
    case task: com.google.api.services.tasks.model.Task => sender ! addTask(task)
    case _ => println("Not a valid instance of Task")
  }
}

def sendTaskToActor(lines: Iterator[String], taskActors: akka.actor.ActorRef) = {
  val queue = new scala.collection.mutable.Queue[scala.concurrent.Future[Any]]
  for ( line <- lines ) {
      val (title, desc) = parseTaskLine(line)
      if ( notNullNorEmpty(title) )
        queue += taskActors ? buildTask( title, desc)
  }
  queue
}

def waitForTasksToBeCreated(queue: scala.collection.mutable.Queue[scala.concurrent.Future[Any]]) = {
  for ( future <- queue ) println(Await.result(future, timeout.duration).asInstanceOf[String])
}


def bulkTasksAdd(tasksFile:String) = {
  if ( notNullNorEmpty(tasksFile) ) {
    println("Loading task from file:" + tasksFile)
    val system = ActorSystem("ActorSystem")
    waitForTasksToBeCreated(sendTaskToActor(scala.io.Source.fromFile(tasksFile).getLines(),
      system.actorOf(Props(new TaskCreatorActor()), name = "taks-actors")))
    system.shutdown
    System.exit(0)
  }
}

object Args {

  // Parameters for new tasks
  @Parameter(names = Array("-t", "--task-title"), description = "Task title", required = false)
  var title: String = ""

  @Parameter(names = Array("-d", "--task-description"), description = "Task description", required = false)
  var description: String = ""

    // optional param for creation
  @Parameter(names = Array("-T", "--task-type"), description = "Type of the task (☎,✉,⎙)", required = false)
  var symbol : String = ""

  @Parameter(names = Array("-D", "--due-date"), description = "Task description", required = false)
  var dueDate: String = ""

  // Shortcut to task creation
  @Parameter(names = Array("-e", "--email-as-description"), description = "Task description", required = false)
  var email: String = ""

  @Parameter(names = Array("-b", "--bug-url"), description = "A Bug entry URL", required = false)
  var bugUrl: String = ""

  // Other features
  @Parameter(names = Array("-s", "--search-tasks"), description = "Search a task title containing the provided string", required = false)
  var search: String = ""

  @Parameter(names= Array("-l" , "--list-today-tasks"), description = "List today's tasks" , required = false )
  var list = false

  // Features using the extra -i parameter
  @Parameter(names= Array("-B", "--bump-task"), description = "Bump due date", required = false)
  var bump: Int = 0

  @Parameter(names= Array("-i", "--task-id"), description = "Task ID", required = false)
  var id: String = ""

  // Features using id as passed value to main paramter AND reusing title, desc, ...
  @Parameter(names= Array("-E", "--edit-task-title"), description = "Edit task title, requires task id", required = false)
  var taskToEdit: String = ""

  @Parameter(names= Array("-F", "--task-finished"), description = "Mark task as done, requires task id as value", required = false)
  var taskToFinishId: String = ""

  @Parameter(names= Array("-A", "--bulk-add"), description = "Add tasks in bulk, using a simple 'one-line' by task name", required = false)
  var bulkAdd: String = ""
}

new JCommander(Args, args.toArray: _*)

val service = connectAndGetService()

bulkTasksAdd(Args.bulkAdd)
taskDone(Args.taskToFinishId)
bumpDueDate(Args.bump, Args.id)
listAndQuit(Args.list)
searchAndQuit(Args.search)

val symbol = getSymbol(Args.symbol)
val task = buildTask( getTitle(Args.title, Args.email, Args.bugUrl), getDesc(Args.description, Args.email, Args.bugUrl), getDueDate(Args.dueDate), symbol)

editTask(Args.taskToEdit, task , symbol)

addTask(task)
