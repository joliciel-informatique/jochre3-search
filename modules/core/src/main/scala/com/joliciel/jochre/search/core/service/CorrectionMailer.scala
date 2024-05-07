package com.joliciel.jochre.search.core.service

import com.joliciel.jochre.search.core.DocReference
import com.typesafe.config.ConfigFactory
import jakarta.mail._
import jakarta.mail.internet._
import org.slf4j.LoggerFactory

import java.time.format.DateTimeFormatter
import java.time.{ZoneOffset, ZonedDateTime}
import java.util.Properties

object CorrectionMailer {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.search")
  private val mailConfig = config.getConfig("mail")
  private val dateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME

  def mailCorrection(correction: DbMetadataCorrection, docRefs: Seq[DocReference]) = try {

    val mailProps = new Properties
    mailProps.put("mail.smtp.port", mailConfig.getString("smtp.port"))
    mailProps.put("mail.smtp.auth", mailConfig.getString("smtp.auth"))
    mailProps.put("mail.smtp.starttls.enable", mailConfig.getString("smtp.starttls.enable"))

    val mailSession = Session.getDefaultInstance(mailProps, null)
    val message = new MimeMessage(mailSession)
    if (mailConfig.hasPath("to-name"))
      message.addRecipient(
        Message.RecipientType.TO,
        new InternetAddress(mailConfig.getString("to"), mailConfig.getString("to-name"))
      )
    else message.addRecipient(Message.RecipientType.TO, new InternetAddress(mailConfig.getString("to")))
    if (mailConfig.hasPath("cc"))
      if (mailConfig.hasPath("cc-name"))
        message.addRecipient(
          Message.RecipientType.CC,
          new InternetAddress(mailConfig.getString("cc"), mailConfig.getString("cc-name"))
        )
      else message.addRecipient(Message.RecipientType.CC, new InternetAddress(mailConfig.getString("cc")))

    if (mailConfig.hasPath("from-name"))
      message.setFrom(new InternetAddress(mailConfig.getString("from"), mailConfig.getString("from-name")))

    val subject = f"New Jochre correction for ${docRefs.head.ref}, ${correction.field.entryName}"
    message.setSubject(subject, "UTF-8")

    val undoCommandUrl =
      config.getString("corrections.undo-command-url").replace("${CORRECTION_ID}", f"${correction.id.id}")
    val body =
      f"""
         |<html>
         |<body>
         |<b>New correction on Jochre</b>
         |<br>
         |<ul>
         |<li><b>User:</b> ${correction.username}</li>
         |<li><b>IP:</b> ${correction.ipAddress}</li>
         |<li><b>Date:</b> ${ZonedDateTime.ofInstant(correction.created, ZoneOffset.UTC).format(dateTimeFormatter)}</li>
         |<li><b>Field:</b> ${correction.field.entryName}</li>
         |<li><b>Previous value:</b> ${correction.oldValue}</li>
         |<li><b>New value:</b> ${correction.newValue}</li>
         |<li>Apply to the following documents:
         |<ul>
         |${docRefs.map(docRef => f"<li>${docRef.ref}</li>").mkString("\n")}
         |</ul>
         |<br>
         |<br>To undo this correction, click here: $undoCommandUrl
         |</body>
         |</html>
         |""".stripMargin
    message.setText(body, "UTF-8", "html")

    if (log.isDebugEnabled) log.debug("Sending e-mail to " + mailConfig.getString("to") + " for " + subject)
    val transport = mailSession.getTransport("smtp")

    transport.connect(mailConfig.getString("smtp.host"), mailConfig.getString("from"), mailConfig.getString("password"))
    transport.sendMessage(message, message.getAllRecipients)
    transport.close()
  } catch {
    case exception: Exception =>
      log.error(f"Unable to send email", exception)
      throw exception
  }
}
