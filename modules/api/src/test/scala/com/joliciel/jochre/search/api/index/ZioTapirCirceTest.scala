package com.joliciel.jochre.search.api.index

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
import zio.{Scope, ZIO}

object ZioTapirCirceTest extends JUnitRunnableSpec {
  case class Payload(one: String, two: String)

  val myEndpoint =
    endpoint.post.in(jsonBody[Payload]).out(stringBody).zServerLogic(r => ZIO.succeed(s"One ${r.one}. Two ${r.two}."))

  val stub = TapirStubInterpreter(SttpBackendStub(new RIOMonadAsyncError[Any]))
    .whenServerEndpoint(myEndpoint)
    .thenRunLogic()
    .backend()

  val body = Payload("1", "2").asJson.noSpaces

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ZioTapirCirceTest")(
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
  )
}
