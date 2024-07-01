package com.joliciel.jochre.search.api.index

import io.circe.generic.auto._
import io.circe.syntax.EncoderOps
import sttp.client3._
import sttp.client3.impl.zio.RIOMonadAsyncError
import sttp.client3.testing.SttpBackendStub
import sttp.model.{HeaderNames, StatusCode}
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe._
import sttp.tapir.server.stub.TapirStubInterpreter
import sttp.tapir.ztapir._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestEnvironment, assertTrue}
import zio.{Scope, ZIO, ZLayer}

object ZioTapirCirceSecureLayerTest extends JUnitRunnableSpec {
  case class Payload(one: String, two: String)

  trait Printer {
    def print(user: String, s1: String, s2: String): String
  }

  object PrinterImpl extends Printer {
    override def print(user: String, s1: String, s2: String): String = f"User $user. One $s1. Two $s2."
  }

  val printerLayer = ZLayer.succeed[Printer](PrinterImpl)

  sealed trait HttpError

  object HttpError {
    case class Unauthorized(message: String) extends HttpError
  }

  val myEndpoint: _root_.sttp.tapir.ztapir.ZServerEndpoint[Printer, Any] =
    endpoint
      .securityIn(auth.bearer[String]())
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[HttpError.Unauthorized](
            StatusCode.Unauthorized,
            jsonBody[HttpError.Unauthorized].description("Invalid token or missing permissions")
          )
        )
      )
      .zServerSecurityLogic[Printer, String](bearer => ZIO.succeed(bearer))
      .post
      .in(jsonBody[Payload])
      .out(stringBody)
      .serverLogic[Printer] { user => payload =>
        for {
          printer <- ZIO.service[Printer]
        } yield {
          printer.print(user, payload.one, payload.two)
        }
      }

  val stub = TapirStubInterpreter(SttpBackendStub(new RIOMonadAsyncError[Printer]))
    .whenServerEndpoint(myEndpoint)
    .thenRunLogic()
    .backend()

  val body = Payload("1", "2").asJson.noSpaces

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ZioTapirCirceSecureLayerTest")(
    test("work") {
      for {
        response <- basicRequest
          .contentType("application/json")
          .body(body)
          .header(HeaderNames.Authorization, "Bearer Joe")
          .post(uri"http://test.com/test-request")
          .send(stub)
      } yield {
        assertTrue(response.body.getOrElse("") == "User Joe. One 1. Two 2.")
      }
    }
  ).provideLayer(printerLayer)
}
