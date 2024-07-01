package com.joliciel.jochre.search.api.index

import com.joliciel.jochre.search.api.Types.Requirements
import com.joliciel.jochre.search.core.config.AppConfig
import com.joliciel.jochre.search.core.service.{DatabaseTestBase, PreferenceService, SearchService, WithTestIndex}
import com.joliciel.jochre.search.yiddish.YiddishFilters
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
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}
import zio.{Scope, Task, ZIO, ZLayer}

object ZioTapirCirceAllRequirementsTest extends JUnitRunnableSpec with DatabaseTestBase with WithTestIndex {
  case class Payload(one: String, two: String)

  val myEndpoint: _root_.sttp.tapir.ztapir.ZServerEndpoint[Requirements, Any] =
    endpoint.post.in(jsonBody[Payload]).out(stringBody).zServerLogic[Requirements] { payload =>
      ZIO.succeed(f"One ${payload.one}. Two ${payload.two}.")
    }

  val stub = TapirStubInterpreter(SttpBackendStub(new RIOMonadAsyncError[Requirements]))
    .whenServerEndpoint(myEndpoint)
    .thenRunLogic()
    .backend()

  val body = Payload("1", "2").asJson.noSpaces

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ZioTapirCirceAllRequirementsTest")(
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
  ).provideLayer(
    (transactorLayer ++ searchRepoLayer ++ suggestionRepoLayer ++ preferenceRepoLayer ++ indexLayer ++ YiddishFilters.live) >>> AppConfig.live ++ SearchService.live ++ PreferenceService.live ++ ZLayer
      .service[Transactor[Task]]
  ) @@ TestAspect.sequential
}
