import javax.mail._
import javax.mail.internet._
import java.io._
import java.text.SimpleDateFormat
import java.util._
import java.lang.management.ManagementFactory

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

import java.net.ServerSocket
import java.net.Socket
import java.net.InetAddress
import java.io.DataInputStream

import scala.collection.JavaConversions._
import scala.sys
import scala.Console
import scala.concurrent.Await
import scala.concurrent.duration._

import akka.actor.Actor
import akka.actor.ActorSystem
import akka.actor.Props
import akka.pattern.ask
import akka.util.Timeout

val debug = true
implicit val timeout = Timeout(65 seconds)
val TASK_SYMBOL = scala.collection.Map[String,String](
  "c" -> "☎",   // call
  "m" -> "✉",   // mail
  "p" -> "⎙",   // print
  "M" -> "♫",   // music
  "w" -> "ʬ",   // set of related small taks or just a small task
  "t" -> "ʘ",   // figure out next step, faire le point
  "W" -> "Ɯ",   // Task
  "€" -> "€",   // Money task
  "x" -> "ɸ"    // find a use for thi symbol someday
  )
val TASK_SYMBOL_STRING = "☎✉⎙♫ʬʘɸƜ€"

val TASK_CLIENT_SECRET_FILE_ENV_VAR_NAME = "TASKS_CLIENT_SECRET"
val TASK_SERVER_BIND_ADDR_ENV_VAR_NAME = "TASKS_SERVER_BIND_ADDR"
val TASK_SERVER_PORT_ENV_VAR_NAME = "TASKS_SERVER_PORT"
val TASK_SERVER_PID_FILE_ENV_VAR_NAME = "TASKSD_PIDFILE"

val ONE_DAY__IN_MILLIS = 86400000

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

def tomorrow() = {
  new DateTime(System.currentTimeMillis() + ONE_DAY__IN_MILLIS)
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

def buildRestUrlFromBugUrl(bugUrl:String) = { bugUrl.replaceFirst("/browse/","/rest/api/latest/issue/").replaceFirst("$","?fields=summary") }

def descForBugUrl(bugUrl: String) = {
  val content = scala.io.Source.fromURL(buildRestUrlFromBugUrl(bugUrl)).mkString
  val r = """^.*\"summary\":\"([^"]*)\".*$""".r
  val res = Option(content) collect { case r(group) => group }
  res.get
}

def descForPrUrl(prUrl: String) = {
  prUrl
}

def prIdFromPrUrl(prUrl: String) = {

  val url = prUrl.replaceFirst("/github.com/","/api.github.com/repos/").replaceFirst("pull","pulls")
  val content = scala.io.Source.fromURL(prUrl.replaceFirst("/github.com/","/api.github.com/repos/").replaceFirst("pull","pulls")).mkString
  val res = scala.util.parsing.json.JSON.parseFull(content) match {
    case Some(map: scala.collection.immutable.HashMap[String, Any]) => { "PR" +
      map("number").toString + " - " + map("title").toString  }
    case _ => println("other")
  }
  res.toString
}

def getDesc(description:String, email:String, bugUrl:String, prUrl: String): String = {

  if ( ! "".equals(prUrl))
    return descForPrUrl(prUrl) + "\n\n" + prUrl

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

def getTitle(title:String, email:String, bugUrl:String, prUrl: String):String = {
  if ( ! "".equals(prUrl) )
     return prIdFromPrUrl(prUrl)
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

  val clientSecretFilename = sys.env(TASK_CLIENT_SECRET_FILE_ENV_VAR_NAME)
  if ( ! new java.io.File(clientSecretFilename).exists() )  throw new IllegalStateException("Certificate file does not exists: " + clientSecretFilename)
  val clientSecrets = GoogleClientSecrets.load(JSON_FACTORY, new InputStreamReader(new FileInputStream( clientSecretFilename )))
  val flow = new GoogleAuthorizationCodeFlow.Builder( HTTP_TRANSPORT, JSON_FACTORY, clientSecrets, SCOPES)
                  .setDataStoreFactory(DATA_STORE_FACTORY)
                  .setAccessType("offline")
                  .build()
  val credential = new AuthorizationCodeInstalledApp(flow, new LocalServerReceiver()).authorize("user")
  new Tasks.Builder(HTTP_TRANSPORT, JSON_FACTORY, credential).setApplicationName(APPLICATION_NAME).build()
}

def emptyStringIfNull(s: String):String = { if (s == null) "" else s }

def searchAndQuit(search: String, done: () => Unit ):Unit = {
  if ( ! "".equals(search) ) {
    val tasks = service.tasks.list("@default").execute()

    for (task <- tasks.getItems )
      if ( task.getTitle().toLowerCase().contains(search.toLowerCase()) )
        Console.out.println(taskDisplay(task))
    done()
  }
}

def isToday(due:DateTime) = {
  isSameDay(due, Calendar.getInstance().getTime())
}

def isSameDay(due: DateTime, day: Date) = {
  //Console.out.println("Is " +  due.toStringRfc3339.substring(0,10) + " equals to " + new SimpleDateFormat("y-MM-dd").format(day) + " ?")
  due.toStringRfc3339.substring(0,10).equals(new SimpleDateFormat("y-MM-dd").format(day))
}

def dateDisplay(date: DateTime) = {
  new SimpleDateFormat("dd/MM/YYYY").format(new Date(date.getValue()))
}

def taskNotesDisplay(task: com.google.api.services.tasks.model.Task, noNotesDisplay: Boolean) = {
  if ( noNotesDisplay ) "" else { "\n" + emptyStringIfNull(task.getNotes) }
}

def taskDisplay(task: com.google.api.services.tasks.model.Task, noNotesDisplay: Boolean = false) = {
  "[" + task.getId() + "] " + task.getTitle + "\nDue on: " + dateDisplay(task.getDue()) + taskNotesDisplay(task, noNotesDisplay)
}

def notNullNorEmpty(value: String) = {
  (value != null &&  ! "".equals(value) )
}

def addSymbolToTitle(title: String, symbol: String): String = {
  if ( notNullNorEmpty(symbol) ) return title + " " + symbol
  title
}

// Features methods

def listTasksForDayAndQuit(Args: Args, dueDate: DateTime, done: () => Unit):Unit = {
  if ( Args.list ) {
    val tasks = service.tasks.list("@default").execute()
    Console.out.println("Today (" + dueDate + ") tasks:")
    Console.out.println
    var taskNumber = 1
    for (task <- tasks.getItems ) if ( task.getDue() != null && isToday(task.getDue())) {
        Console.out.println(taskNumber + ") " + taskDisplay(task, Args.noNotesDisplay))
        taskNumber = taskNumber + 1
    }
    done()
  }
}

def listTasksAndQuit(Args: Args, done: () => Unit):Unit = {
  if ( Args.list )
    listTasksForDayAndQuit(Args, today(), done)
  if ( Args.listTomorrow )
    listTasksForDayAndQuit(Args, tomorrow(), done)
}

def bumpDueDate(days:Int, id:String, done: () => Unit):Unit = {
  if ( days > 0 && id != null && ! "".equals(id) ) {
    val NB_SECONDS_BY_DAY = 86400L * 1000
    val task = service.tasks.get("@default", id).execute()
    task.setDue(new DateTime(task.getDue().getValue() + (days * NB_SECONDS_BY_DAY)))
    val result = service.tasks.update("@default", task.getId(), task).execute();
    Console.out.println("Task '" + result.getTitle() + " has been bumped by " + days + " days:" + dateDisplay(result.getDue()))
    done()
  }
}

def addTask(task: com.google.api.services.tasks.model.Task, done: () => Unit) = {
  "Task '" + service.tasks.insert("@default", task).execute().getTitle() + "' has been created and added"
}

def editTask(id:String, newTask: com.google.api.services.tasks.model.Task, symbol: String = "", done: () => Unit):Unit = {
  if ( notNullNorEmpty(id) ) {
    val task = service.tasks.get("@default", id).execute()
    Console.out.println("New Title:" + newTask.getTitle())
    if ( notNullNorEmpty(newTask.getTitle().filterNot(TASK_SYMBOL_STRING.toSet).replaceAll(" ","")) ) task.setTitle(newTask.getTitle())
    if ( notNullNorEmpty(newTask.getNotes())  ) task.setNotes(newTask.getNotes())

    if ( newTask.getDue() != null ) task.setDue(newTask.getDue())
    val result = service.tasks.update("@default", task.getId(), task).execute()
    Console.out.println(taskDisplay(task))
    done()
  }
}

def taskDone(id:String, done: () => Unit) = {
  if ( notNullNorEmpty(id) ) {
    service.tasks.delete("@default", id).execute()
    Console.out.println("Task [" + id + "] has been removed.")
    done()
  }
}

class TaskCreatorActor extends Actor {
  def receive = {
    case task: com.google.api.services.tasks.model.Task => sender ! addTask(task, () => {})
    case _ => Console.out.println("Not a valid instance of Task")
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
  for ( future <- queue ) Console.out.println(Await.result(future, timeout.duration).asInstanceOf[String])
}


def bulkTasksAdd(tasksFile:String, done: () => Unit) = {
  if ( notNullNorEmpty(tasksFile) ) {
    Console.out.println("Loading task from file:" + tasksFile)
    val system = ActorSystem("BulkTaskActors")
    waitForTasksToBeCreated(sendTaskToActor(scala.io.Source.fromFile(tasksFile).getLines(), system.actorOf(Props(new TaskCreatorActor()), name = "task-actor")))
    system.shutdown
    done()
  }
}

class Args {

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

  @Parameter(names = Array("-p", "--pull-request"), description = "A PR URL entry", required = false)
  var prUrl: String = ""

  // Other features
  @Parameter(names = Array("-s", "--search-tasks"), description = "Search a task title containing the provided string", required = false)
  var search: String = ""

  @Parameter(names= Array("-l" , "--list-today-tasks"), description = "List today's tasks" , required = false )
  var list = false

  @Parameter(names= Array("-ll" , "--list-tomorrow-tasks"), description = "List tomorow's tasks" , required = false )
  var listTomorrow = false

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

  @Parameter(names= Array("-N", "--no-notes"), description = "Do not show notes when printing task out", required= false)
  var noNotesDisplay = false
}

def processRequest(Args: Args, done:() => Unit) = {

  bulkTasksAdd(Args.bulkAdd, done)
  taskDone(Args.taskToFinishId, done)
  bumpDueDate(Args.bump, Args.id, done)
  listTasksAndQuit(Args, done)
  searchAndQuit(Args.search, done)

  val symbol = getSymbol(Args.symbol)
  val task = buildTask( getTitle(Args.title, Args.email, Args.bugUrl, Args.prUrl), getDesc(Args.description, Args.email, Args.bugUrl, Args.prUrl), getDueDate(Args.dueDate), symbol)

  editTask(Args.taskToEdit, task , symbol, done)

  addTask(task, done)
}

class TaskRequestActor extends Actor {
  def receive = {
    case socket: java.net.Socket => sender ! launchProcessRequest(socket)
    case _ => Console.out.println("Not a valid java.net.Socket ! ")
  }
}

def launchProcessRequest(socket: Socket) = {
  val input = new DataInputStream(socket.getInputStream())

  val s: String = input.readLine
  val out = Console.out
  println(">>> Processing command: " + s )
  Console.setOut(socket.getOutputStream())
  val args = parseCommandLine(s.split(" "))
  try  {
      processRequest(args, () => { throw new IllegalStateException("done") } )
  } catch {
    case done: IllegalStateException =>  {} //
    case _: Throwable => Console.out.println("something went wrong:")
  }
  Console.setOut(out)
  input.close
  socket.close
}

def parseCommandLine(args: Array[java.lang.String]) = {
  val arguments = new Args
  new JCommander(arguments, args.toArray: _*)
  arguments
}

def writeInFile(filename: String, content: String) = { new PrintWriter(filename) { write(content); close } }

val service = connectAndGetService()
if ( args.length == 0  ) {
  val pid = ManagementFactory.getRuntimeMXBean().getName().split("@")(0)
  println("Server Mode Started (PID:" + pid + ")")
  writeInFile(sys.env(TASK_SERVER_PID_FILE_ENV_VAR_NAME), pid)

  val system = ActorSystem("TaskRequestActors")
  val actor = system.actorOf(Props(new TaskRequestActor()), name = "task-request-actor")
  try {
    val server = new ServerSocket(sys.env(TASK_SERVER_PORT_ENV_VAR_NAME).toInt, 1, InetAddress.getByName(sys.env(TASK_SERVER_BIND_ADDR_ENV_VAR_NAME)))

    while (true) {
      actor ? server.accept
    }
  } finally {
    system.shutdown
  }

} else
    processRequest(parseCommandLine(args), () => System.exit(0) )
