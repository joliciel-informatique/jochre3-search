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
import sttp.model.{HeaderNames, StatusCode}
import sttp.tapir.generic.auto._
import sttp.tapir.json.circe._
import sttp.tapir.server.stub.TapirStubInterpreter
import sttp.tapir.ztapir._
import zio.test.junit.JUnitRunnableSpec
import zio.test.{Spec, TestAspect, TestEnvironment, assertTrue}
import zio.{Scope, Task, ZIO, ZLayer}

object ZioTapirCirceSecureAllRequirementsTest extends JUnitRunnableSpec with DatabaseTestBase with WithTestIndex {
  case class Payload(one: String, two: String)

  sealed trait HttpError

  object HttpError {
    case class Unauthorized(message: String) extends HttpError
  }

  val myEndpoint: _root_.sttp.tapir.ztapir.ZServerEndpoint[Requirements, Any] =
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
      .zServerSecurityLogic[Requirements, String](bearer => ZIO.succeed(bearer))
      .post
      .in(jsonBody[Payload])
      .out(stringBody)
      .serverLogic[Requirements] { _ => _ =>
        ZIO.succeed("Hello")
      }

  val stub = TapirStubInterpreter(SttpBackendStub(new RIOMonadAsyncError[Requirements]))
    .whenServerEndpoint(myEndpoint)
    .thenRunLogic()
    .backend()

  val body = Payload("1", "2").asJson.noSpaces

  override def spec: Spec[TestEnvironment with Scope, Any] = suite("ZioTapirCirceSecureAllRequirementsTest")(
    test("work") {
      for {
        response <- basicRequest
          .contentType("application/json")
          .body(body)
          .header(HeaderNames.Authorization, "Bearer Joe")
          .post(uri"http://test.com/test-request")
          .send(stub)
      } yield {
        assertTrue(response.body.getOrElse("") == "Hello")
      }
    }
  ).provideLayer(
    (transactorLayer ++ searchRepoLayer ++ suggestionRepoLayer ++ preferenceRepoLayer ++ indexLayer ++ YiddishFilters.live) >>> AppConfig.live ++ SearchService.live ++ PreferenceService.live ++ ZLayer
      .service[Transactor[Task]]
  ) @@ TestAspect.sequential
}
