package com.wix.sms.bulksms.testkit

import java.util.concurrent.atomic.AtomicReference
import java.util.{List => JList}

import akka.http.scaladsl.model._
import com.google.api.client.http.UrlEncodedParser
import com.wix.e2e.http.RequestHandler
import com.wix.e2e.http.client.extractors.HttpMessageExtractors._
import com.wix.e2e.http.server.WebServerFactory.aMockWebServerWith
import com.wix.sms.bulksms.model.{StatusCodes, _}
import com.wix.sms.bulksms.{BulksmsHelper, Credentials}
import com.wix.sms.model.Sender

import scala.collection.JavaConversions._
import scala.collection.mutable

class BulksmsDriver(port: Int) {
  private val delegatingHandler: RequestHandler = { case r: HttpRequest => handler.get().apply(r) }
  private val notFoundHandler: RequestHandler = { case _: HttpRequest => HttpResponse(status = 404) }

  private val handler = new AtomicReference(notFoundHandler)

  private val probe = aMockWebServerWith(delegatingHandler).onPort(port).build
  private val responseParser = new ResponseParser

  def startProbe() {
    probe.start()
  }

  def stopProbe() {
    probe.stop()
  }

  def resetProbe() {
    handler.set(notFoundHandler)
  }

  def aSendFor(credentials: Credentials, sender: Sender, destPhone: String, text: String, routingGroup: String): SendCtx = {
    new SendCtx(
      credentials = credentials,
      sender = sender,
      destPhone = destPhone,
      text = text,
      routingGroup = routingGroup)
  }

  private def prependHandler(handle: RequestHandler) =
    handler.set(handle orElse handler.get())

  class SendCtx(credentials: Credentials, sender: Sender, destPhone: String, text: String, routingGroup: String) {
    private val expectedParams = BulksmsHelper.createRequestParams(
      sender = sender,
      destPhone = destPhone,
      text = text,
      credentials = credentials,
      routingGroup = routingGroup
    )

    def returns(msgId: String): Unit = {
      val response = new Response(
        code = StatusCodes.inProgress,
        description = "IN_PROGRESS",
        batchId = Some(msgId)
      )

      val responseText = responseParser.stringify(response)
      returnText(responseText)
    }

    def failsWith(code: String, description: String): Unit = {
      val response = new Response(
        code = code,
        description = description
      )

      val responseText = responseParser.stringify(response)
      returnText(responseText)
    }

    private def returnText(responseText: String): Unit = {
      prependHandler({
        case HttpRequest(
        HttpMethods.POST,
        Uri.Path("/submission/send_sms/2/2.0"),
        headers,
        entity,
        _) if isStubbedRequestEntity(entity) => HttpResponse(
          status = 200,
          entity = HttpEntity(ContentTypes.`text/plain(UTF-8)`, responseText)
        )
      })
    }

    private def isStubbedRequestEntity(entity: HttpEntity): Boolean = {
      val requestParams = urlDecode(entity.extractAsString)

      requestParams == expectedParams
    }

    private def urlDecode(str: String): Map[String, String] = {
      val params = mutable.LinkedHashMap[String, JList[String]]()
      UrlEncodedParser.parse(str, mutableMapAsJavaMap(params))
      params.mapValues( _.head ).toMap
    }
  }
}
