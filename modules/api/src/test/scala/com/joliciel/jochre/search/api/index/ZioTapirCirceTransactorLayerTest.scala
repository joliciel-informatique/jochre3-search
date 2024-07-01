package com.joliciel.jochre.search.api.index

import com.joliciel.jochre.search.core.service.DatabaseTestBase
import doobie.Transactor
import io.circe.generic.auto._
import io.circe.syntax.EncoderOps
import sttp.client3._
import sttp.client3.impl.zio.RIOMonadAsyncError
import sttp.client3.testing.SttpBackendStub
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe._
import sttp.tapir.server.stub.TapirStubInterpreter
import sttp.tapir.ztapir._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestEnvironment, assertTrue}
import zio.{Scope, Task, ZIO, ZLayer}

object ZioTapirCirceTransactorLayerTest extends JUnitRunnableSpec with DatabaseTestBase {
  case class Payload(one: String, two: String)

  trait Printer {
    def print(s1: String, s2: String): String
  }

  object PrinterImpl extends Printer {
    override def print(s1: String, s2: String): String = f"One $s1. Two $s2."
  }

  val printerLayer = ZLayer.succeed[Printer](PrinterImpl)

  type MyRequirements = Printer with Transactor[Task]

  val myEndpoint: _root_.sttp.tapir.ztapir.ZServerEndpoint[MyRequirements, Any] =
    endpoint.post.in(jsonBody[Payload]).out(stringBody).zServerLogic[MyRequirements] { payload =>
      for {
        printer <- ZIO.service[Printer]
      } yield {
        printer.print(payload.one, payload.two)
      }
    }

  val stub = TapirStubInterpreter(SttpBackendStub(new RIOMonadAsyncError[MyRequirements]))
    .whenServerEndpoint(myEndpoint)
    .thenRunLogic()
    .backend()

  val body = Payload("1", "2").asJson.noSpaces

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ZioTapirCirceTransactorLayerTest")(
    test("work") {
      for {
        response <- basicRequest
          .contentType("application/json")
          .body(body)
          .post(uri"http://test.com/test-request")
          .send(stub)
      } yield {
        assertTrue(response.body.getOrElse("") == "One 1. Two 2.")
      }
    }
  ).provideLayer(printerLayer ++ transactorLayer)
}
