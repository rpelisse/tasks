import javax.mail._
import javax.mail.internet._
import java.io._
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

def readEmail(f:String) = {

  val is = new FileInputStream(new File(f))
  val s = Session.getDefaultInstance(new Properties())

  val msg = new MimeMessage(s, is)
  msg.getContent() match {
    case m: Multipart => m.getBodyPart(0).getContent().toString()
    case _ => msg.getContent().toString()
  }
}

def readSubject(f:String) = {

  val is = new FileInputStream(new File(f))
  val s = Session.getDefaultInstance(new Properties())

  val msg = new MimeMessage(s, is)
  msg.getSubject
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

  if ( ! "".equals(email) )
    return readSubject(email)

  throw new IllegalArgumentException("No title provided, neither to extrapolate one.")
}

def getDueDate(dueDate:String) = {
  if ( ! "".equals(dueDate) )
    new DateTime(new java.text.SimpleDateFormat("dd/MM/yyyy").parse(dueDate))
  else
    new DateTime(System.currentTimeMillis())
}

def connectAndGetService() = {

  val APPLICATION_NAME = "Google Tasks API Java Quickstart"
  val DATA_STORE_DIR = new File(System.getProperty("user.home"), ".credentials/tasks-java-quickstart")
  val DATA_STORE_FACTORY = new FileDataStoreFactory(DATA_STORE_DIR)
  val JSON_FACTORY = JacksonFactory.getDefaultInstance()
  val HTTP_TRANSPORT = GoogleNetHttpTransport.newTrustedTransport()
  val SCOPES = Arrays.asList(TasksScopes.TASKS)

  val in = new Object().getClass().getResourceAsStream("/client_secret.json")
  val clientSecrets = GoogleClientSecrets.load(JSON_FACTORY, new InputStreamReader(in))
  val flow = new GoogleAuthorizationCodeFlow.Builder( HTTP_TRANSPORT, JSON_FACTORY, clientSecrets, SCOPES)
                  .setDataStoreFactory(DATA_STORE_FACTORY)
                  .setAccessType("offline")
                  .build()
  val credential = new AuthorizationCodeInstalledApp(flow, new LocalServerReceiver()).authorize("user")
  new Tasks.Builder(HTTP_TRANSPORT, JSON_FACTORY, credential).setApplicationName(APPLICATION_NAME).build()
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
}

new JCommander(Args, args.toArray: _*)

val title = getTitle(Args.title, Args.email, Args.bugUrl)
val desc = getDesc(Args.description, Args.email, Args.bugUrl)
val dueDate = getDueDate(Args.dueDate)

val task = new com.google.api.services.tasks.model.Task()
task.setTitle(title)
task.setNotes(desc)
task.setDue(dueDate)

println(task)
/*
val service = connectAndGetService()
val result = service.tasks.insert("@default", task).execute()
println("Task '" + result.getTitle() + "' has been created and added")
*/
